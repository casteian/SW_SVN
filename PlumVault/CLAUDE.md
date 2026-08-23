# PlumVault — SolidWorks + SVN integration add-in

VB.NET SolidWorks add-in ("PlumVault") that integrates SolidWorks CAD files with an SVN
repository for the Gryphon Racing team: file locking, commit/get-latest, naming-convention
enforcement (GRC27/CFD27), external-reference handling, and a task-pane UI.

See `WORK_PLAN.md` for the current in-progress session state — read that first when picking
this project back up. This file is the durable "how the codebase works" reference.

## Build

```bash
"C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\MSBuild.exe" PlumVaultProj.vbproj /t:Build /p:Configuration=Debug /p:RegisterForComInterop=false /v:minimal /nologo
```

`/p:RegisterForComInterop=false` is required unless running as admin — the normal build
tries to unregister/re-register the COM interop DLL, which needs elevation and fails with
`MSB3392` otherwise. This flag skips that step; it does **not** deploy the add-in for
SolidWorks to pick up automatically. There is no way to launch/test this in-session — a real
SolidWorks install is required, so nothing here has been verified against a live session by
Claude. Always say so explicitly rather than claiming a fix "works."

## Architecture

- **`svnModule.vb`** (~13,000+ lines) — the central Module holding almost all business
  logic: SVN process invocation (`runSvnProcess`, `runSvnProcessBackgroundNoUi`), lock/
  commit/unlock, the assembly-edit-protection guard, close-safety checks, external-reference
  and drawing-dependency handling, naming-convention validation, and the entry points called
  from event handlers (`...Public` suffixed functions are the public API surface other files
  call into).
- **`SwAddin.vb`** — SolidWorks add-in COM entry point (`ISwAddin`). Top-level SolidWorks
  application events (`ActiveDocChangeNotify`, `CommandOpenPreNotify` for Ctrl+S
  interception) and the main-window close guard (`SolidWorksCloseGuardWindowHook`, hooked to
  the SolidWorks frame's `WM_CLOSE` — this is the actual "closing the whole application"
  signal, distinct from closing one document).
- **`EventHandling.vb`** — per-document event handler classes: `PartEventHandler`,
  `AssemblyEventHandler`, `DrawingEventHandler`, `DocView` (model-view level). These wire raw
  SolidWorks COM events to `svnModule` functions. `AssemblyEventHandler` is the largest and
  most delicate — it's where the assembly-edit-protection guard's event wiring lives
  (`ModifyNotify`, `AddItemNotify`, `ComponentMoveNotify2`, `RegenNotify`/`RegenPostNotify`,
  `BeginInContextEditNotify`/`EndInContextEditNotify`, etc). Also has the Ctrl+W /
  document-window close guard (`SolidWorksCtrlWCloseGuardKeyboardHook`,
  `SolidWorksDocumentCloseGuardWindowHook`) — the *document-level* close guard, distinct from
  the app-level one in SwAddin.vb. **`SolidWorksCtrlWCloseGuardKeyboardHook` is a raw
  `SetWindowsHookEx(WH_KEYBOARD, ...)` low-level hook** (needed because SOLIDWORKS' own
  accelerator table likely consumes Ctrl+W before it ever reaches a subclassed window's
  `WM_KEYDOWN`, unlike the small-X/Window-menu-close paths, which route through
  `SolidWorksDocumentCloseGuardWindowHook`'s normal `WndProc` override). **Never run the full
  safety check (which can show a modal review dialog) synchronously inside that raw hook
  callback** — it must return quickly, and a modal dialog there fights the same message pump
  the hook is currently suspended inside. The keyboard hook now always eats Ctrl+W immediately
  and defers the real check to `svnModule.queueDeferredCtrlWCloseCheckPublic` (`BeginInvoke`,
  runs on the normal UI thread), replaying the close itself via `CloseDoc` if that deferred
  check comes back clean. This was root-caused this session: showing the dialog synchronously
  in the hook was the likely reason the review table never appeared for Ctrl+W specifically.
- **`UserControl1.vb` / `UserControl1.Designer.vb`** — the task-pane UI: tree view, the
  toolstrip buttons (Get Locks/Commit/Unlock/Get Latest/Releases in the main `ToolStrip1`,
  Save As/Re-ID/Move in the separate `FileActionToolStrip`), and several `Timer`s (live
  status check, DPI-aware layout, cache-age display, a one-shot lock-snapshot warmup).
  Runtime layout code (`positionFileActionsAboveRepositoryPath`,
  `positionOnlineCheckboxBesideVersion`) recalculates several control positions at startup —
  **the Designer.vb coordinates for those controls are not the final rendered layout**; if a
  positioning bug shows up, check the runtime repositioning functions in UserControl1.vb
  before touching Designer.vb.
- **`SVNStatus.vb`** — status data structures (`SVNStatus`, `SVNStatus.filePpty` — the
  per-file lock/modification/up-to-date state parsed from `svn status` output).
- **Review-table forms** — `CloseLockReviewForm.vb`, `ExternalReferenceImportForm.vb`,
  `VirtualComponentExternalizeForm.vb`, `LegacyImportForm.vb`, `CadRelocationReviewForm.vb`.
  All follow the same UI pattern: a `DataGridView` with row-level Check/Status/Explanation
  columns, colored rows (pending/valid/invalid), `AutoSizeRowsMode=AllCells` +
  `WrapMode=True` on the Explanation column so long messages wrap instead of clipping, and a
  form `MinimumSize` that must actually fit the sum of the grid's own column minimums (a bug
  that recurred across multiple of these forms — always check the arithmetic when touching
  one). `CadRelocationReviewForm` is the most capable one: callback-driven
  (`Func(Of String, CadRelocationCheckResult)`), used for Re-ID, Move, and (new-in-this-
  session) first-time Save via a `CadRelocationMode.NewSave` mode.

## Working with "codex" in parallel

The user runs a second AI ("codex") on this same codebase, sometimes concurrently. Files can
change on disk between turns without any action from Claude. When a file shows up as changed
outside of an edit Claude just made:

- **Do not revert it.** Treat it as deliberate. Re-read the file to see its actual current
  state before editing it.
- Codex has, at various points, substantially rewritten pieces Claude also touched (the
  Save As/Re-ID/Move UI, the rebuild-suppression tracker, `canTreatAssemblySaveFlagAsGuardGenerated`).
  Some of that rewriting has **reverted fixes** made earlier in the same conversation (this
  happened at least once — a generalized "read-only+unlocked file's dirty flag is spurious"
  check was removed, causing a real regression that had to be re-diagnosed and re-added
  later). When something reported as fixed comes back, check whether it was actually
  reverted before assuming a new bug.
- Before adding a new UI mechanism (a new form, a new table), grep for related existing
  types first (`CadRelocation`, `CadRenameMove`, etc.) — codex may have already built
  something more complete than a from-scratch version would be.

## Core design invariants

- **The local SVN lock token is authoritative.** Files should be OS read-only by default and
  become writable when the user holds the lock, but SOLIDWORKS and writable-state transitions
  can leave the OS attribute temporarily out of sync. Save/commit and close-safety decisions
  therefore use the working copy's `K` token, not the read-only attribute. An existing managed
  file without that token is never treated as a committable user edit; its in-memory dirty flag
  may be discarded at close. New/unversioned files remain first-commit candidates.
- **Existing managed Ctrl+S saves only the active document, then auto-commits it.** Native
  SOLIDWORKS Save can expand an assembly/drawing save to referenced documents whose internal
  save flags are dirty. `CommandOpenPreNotify` therefore cancels ordinary Save for an existing
  managed CAD file and queues a silent `ModelDoc2.Save3` without
  `swSaveAsOptions_SaveReferenced`; the controlled path then explicitly queues one automatic
  commit and retains the existing lock with `--no-unlock`. Save As, non-SVN files, and the
  first-save naming/location review retain their separate flows.
- **SolidWorks has no public API to clear `ModelDoc2.GetSaveFlag()`.** Once SolidWorks marks
  a document dirty (including for internal, non-user-authored reasons — e.g. a rebuild
  picking up an already-committed dependency), there is no way to reset that bit
  programmatically. This is the recurring root cause behind "unrelated files show up as
  modified" bugs (Ctrl+S "Save Modified Documents", the close-time lock review, etc.) — those
  can be *explained to the user* and the guard logic can *avoid reacting to* a flag known to
  be spurious, but the native SolidWorks dialogs that read `GetSaveFlag()` directly cannot be
  suppressed or filtered from outside SolidWorks.
- **GRC27/CFD27 naming convention** is enforced via the same regex duplicated in a few
  places: `^(GRC|CFD)27_(BR|DT|AE|FR|EL|ST|SU|WT|MI)_[A-Z]{0,3}\d+_R\d+\.(SLDPRT|SLDASM|SLDDRW)$`
  (case-insensitive). Canonical copy is `isValidGrc27FileName` in svnModule.vb; each
  review-table form has its own inline copy (`isValidGrcFileName`) rather than a shared
  cross-module call.
- **CAD edit-protection guard**: cancellable pre-events block edits (including feature/sketch
  edits and deletes) when the exact part or assembly owner is not locked. Post-events for
  actions SOLIDWORKS cannot cancel safely are warning-only: **never call `EditUndo2` from a
  guard callback**. Native Undo during an active feature/assembly transaction caused process
  crashes and could remove unrelated designer work. Structural assembly edits (add/delete,
  move, dimension change, new mate) require the owning assembly lock, *unless*
  the edit targets a separately file-backed child that *does* have its own lock and is being
  edited in-context. In-context detection uses `BeginInContextEditNotify`/
  `EndInContextEditNotify` (not just `AssemblyDoc.GetEditTarget()`, which can already be
  `Nothing` again by the time the corresponding `ModifyNotify` fires). Suppressing/unsuppressing
  a component (`ComponentStateChangeNotify`/`Notify2`, `EventHandling.vb`'s
  `ComponentStateChange`) is guarded the same as add/delete/move — it changes what's actually
  persisted and computed in the assembly, unlike hide/show and transparency, which remain
  **explicitly exempt** as pure local viewing state (a user locked out of the top assembly
  still needs to hide siblings while working on one part they do hold the lock on). This was
  corrected after initially exempting suppress/unsuppress too; the user clarified suppression
  is a real edit, not viewing state, and it should be blocked like any other. A feature can be
  suppressed directly under an expanded, separately file-backed nested assembly without first
  entering Edit Assembly. In that case the nested assembly that owns the feature requires the
  lock—not the top-level assembly. Capture that owner in `CommandOpenPreNotify`; the subsequent
  component-state/Modify events can be broadcast on every ancestor and no longer identify the
  persisted edit owner reliably.
- **Rebuild vs. real edit**: `RegenNotify`/`RegenPostNotify` bracket a genuine SolidWorks
  rebuild, tracked per assembly path with a 30-minute staleness expiry (self-healing if a
  `RegenPostNotify` is ever lost) — used to stop a rebuild that only picked up an
  already-correctly-updated child from being mistaken for an unlocked structural edit.
- **Writable-state reconciliation is interaction-scoped.** Acquiring locks clears the
  on-disk read-only bit for every successfully locked file, but an already-open SOLIDWORKS
  document is switched out of its internal read-only state only when it is the active
  document, an actively edited in-context child, or the exact locked nested assembly whose
  feature is about to be suppressed/unsuppressed. Edit Part/Edit Assembly/Edit Feature/Edit
  Sketch never obtains an SVN lock automatically: the user must use Get Locks. Once that exact
  target's live local lock token is verified, its open document is switched writable
  synchronously before the native edit command so SOLIDWORKS does not show its own read-only
  Yes/No prompt. During a valid in-context edit of a locked child, the exact unlocked hosting
  assembly may be made temporarily writable solely so every native exit route can complete
  without SOLIDWORKS' repeated parent-read-only prompt. The parent is never saved and its
  original internal/on-disk read-only state is restored after `EndInContextEditNotify`.
  A dirty commit target is likewise switched synchronously immediately before
  its required `Save3`. Bulk `SetReadOnlyState(False)` calls can trigger rebuild/false-dirty
  cascades across sibling documents; do not restore broad every-locked-open-document
  reconciliation.
- **The visibly selected SVN-tree CAD row is the single-file action target.** It is authoritative
  for Get Locks, Commit, and other single-row file actions whether selected by a direct task-pane
  click or by graphical-selection synchronization from SOLIDWORKS. Never display one selected
  row and silently fall back to ActiveDoc/the top assembly for the actual operation.
- **Verified application close is controlled.** Once the retained-lock table and the open-
  document/SVN dirty checks have passed, `blockCloseIfOpenDocsUnsafe` calls
  `queueVerifiedSafeApplicationClose`, which defers (via `BeginInvoke`, outside the WM_CLOSE
  callback) to `continueVerifiedSafeApplicationClose` → `closeAllVerifiedDocumentsWithoutSaving`
  — an explicit per-document `CloseDoc(title)` loop (drawings, then assemblies, then parts, up
  to 8 passes) — and only then `ExitApp`. (Older `CloseAllDocuments(True)` wording describes a
  prior implementation; the current one is this per-document loop, functionally the same intent
  — nothing is handed back to native SOLIDWORKS close with a dirty flag still set.) This
  intentionally discards only dirty flags already classified as noncommittable/SVN-clean and
  is meant to avoid a second misleading native "Save modified documents" prompt after lock
  release. **As of the last session this was not fully confirmed live** — the user saw the
  native prompt appear after using the table's "Close SOLIDWORKS" button at least once; see
  WORK_PLAN.md. `closeAllVerifiedDocumentsWithoutSaving` closes documents by `GetTitle()`
  string via `CloseDoc`, which is a plausible failure point if two open documents ever share a
  title (unverified). Any scan error fails closed; never let a controlled close proceed before
  the safety checks succeed.
- **Commit/unlock completion refreshes local status before recoloring the tree.** Updating only
  `statusCacheByNormalizedPath` is insufficient because the visible task-pane rows are rendered
  from `statusOfAllOpenModels`; otherwise a released lock remains displayed as `[Locked by you]`
  until the user manually clicks Refresh.
- **Locking is per-file, independent; committing is atomic and bundled.** `svnlock` runs one
  `svn.exe` call *per file* (`bEach:=True`) so one file already locked by a teammate doesn't
  cause every other file in the same request to be reported as failed — this was a real,
  confirmed bug (fixed). Commits deliberately stay bundled into one `svn commit` call
  (`bEach:=False`) since a multi-file commit is supposed to be one atomic revision — do not
  "fix" that the same way.
