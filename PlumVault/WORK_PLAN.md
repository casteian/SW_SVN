# Work plan / session handoff

Last updated: 2026-08-22, end of a very long single session. **Nothing described below has
been committed to git, and nothing has been tested inside a live SolidWorks session** — this
whole session was code-only, verified solely by `MSBuild` compiling cleanly after every
change. Read `CLAUDE.md` first for the architecture/conventions; this file is the "what's
actually in flight right now" state.

## Repo state right now

```
git status
```
should show these modified (all uncommitted):
`CloseLockReviewForm.vb`, `EventHandling.vb`, `ExternalReferenceImportForm.vb`,
`LegacyImportForm.vb`, `PlumVaultProj.vbproj`, `SVNAddInUtils.vb`, `SwAddin.vb`,
`UserControl1.Designer.vb`, `UserControl1.vb`, `VirtualComponentExternalizeForm.vb`,
`svnModule.vb` — plus one **untracked** new file, `CadRelocationReviewForm.vb` (this one
predates Claude's involvement this session; it's codex's).

The user also has "codex" (a separate AI) working on this same codebase, sometimes in the
same window of time as Claude. See the "Working with codex" section in `CLAUDE.md` — some of
what's listed below may have already been further changed/refined/reverted since this was
written. **Re-read before trusting a description here as current fact.**

First thing to do in a new session: `git status` and `git diff --stat` to see whether
anything changed since this was written, then skim the diffs for anything unexpected.

## What was done this session (chronological, high confidence = compiles clean, none tested live)

1. **STEP/Parasolid external-reference blocking fixed.** Assemblies referencing
   non-native-format temp imports (e.g. a vendor STEP file for a bearing/differential) no
   longer hard-block with "temporary or unresolved external SOLIDWORKS file" — they route
   through the normal external-reference review table instead, name cleaned of the embedded
   neutral-format extension (`.stp.SLDPRT` → proposes `...` without the `.stp`).
2. **Full-working-copy lock-review dialog no longer fires on single-document close** — only
   on closing SolidWorks entirely (the app-level `WM_CLOSE` guard in `SwAddin.vb`). Removed a
   redundant `FileCloseNotify`-based trigger that fired it on every document close.
3. **Rebuild-triggered false "assembly edit" fixed** via `RegenNotify`/`RegenPostNotify`
   bracketing (later refined by codex into per-assembly-path tracking with a 30-minute
   staleness expiry — see `assemblyRebuildPaths` in svnModule.vb).
4. **Save As button** added to the task pane (later evolved by codex into the
   Save As / Re-ID / Move trio in `FileActionToolStrip`).
5. **Stability hardening**:
   - `runSvnProcess`/`runSvnProcessBackgroundNoUi` switched from synchronous
     `ReadToEnd()`-before-`WaitForExit()` (a real deadlock/hang risk if svn.exe stalls) to
     async output capture (`BeginOutputReadLine`/`BeginErrorReadLine`), so the existing
     timeout/kill logic can actually fire.
   - All four `Process` objects created in svnModule.vb wrapped in `Using` blocks (were
     leaking handles across a long session — a plausible cause of "critical memory low").
   - `runTortoiseProcexeWithMonitor` hardened against a real race (`HasExited`/
     `Responding`/`Kill` can each throw if TortoiseProc exits between the check and the
     call) — previously fully unguarded, could crash into the SolidWorks callback stack.
6. **Table-sizing fixes** across `CloseLockReviewForm`, `ExternalReferenceImportForm`,
   `VirtualComponentExternalizeForm`, `LegacyImportForm`: form `MinimumSize` raised to
   actually fit the sum of the grid's own column minimums (they didn't — guaranteed clipping
   at anything but the exact default size), `MinimumWidth` added to every fixed column,
   `AutoSizeRowsMode=AllCells` + `WrapMode=True` on long-text columns.
7. **Drawing dependency discovery fixed.** `getComponentsOfAssemblyOptionalUpdateTree`'s
   drawing branch previously only ever found one same-basename part/assembly (broken for
   any drawing referencing multiple/differently-named files). Now uses
   `IModelDocExtension.GetDependencies` via a new `getDrawingReferencedFilePaths` helper
   (Friend, in UserControl1.vb), and reports (rather than silently drops) referenced files
   that aren't currently open.
8. **`svnlock` per-file success/failure fixed.** Previously bundled every path into one
   `svn lock a b c` call; a single conflicting file (already locked by a teammate) made the
   whole batch report as failed, even files with no conflict. Now one `svn.exe` call per
   file (`bEach:=True`), with a clear combined message listing exactly which files failed
   and why.
9. **Drawing-open freshness check added**: `queueDrawingReferenceFreshnessCheckPublic` /
   `checkDrawingReferencesFreshnessPublic` in svnModule.vb — once per drawing per session,
   Online-mode-gated, warns if a referenced file has a newer revision on the server than the
   local working copy ("opened the drawing alone, someone else already committed a change to
   the geometry it shows").
10. **Generalized "read-only+unlocked = spurious dirty flag" check** in
    `canTreatAssemblySaveFlagAsGuardGenerated` (svnModule.vb). **This was added, then
    removed by a later codex refactor (a real regression), then re-added** after diagnosing
    that its removal was the direct cause of the Ctrl+S "shows unrelated files as modified"
    bug reported later in the session. If this logic is ever missing again, that's very
    likely the cause of that exact symptom recurring.
11. **In-context "Edit Part" false-positive fixed.** Root cause: `AssemblyDoc.GetEditTarget()`
    can already be `Nothing` again by the time the edit's `ModifyNotify` arrives (confirmed
    via the user's exact repro: lock a part, "Open Part in Position", edit it, switch to the
    assembly, "Edit Part" in-context, exit — falsely flagged as an unlocked assembly edit).
    Fixed with `BeginInContextEditNotify`/`EndInContextEditNotify` (verified via reflection
    against the installed SolidWorks interop DLL — `AssemblyDocClass` genuinely exposes
    both), tracked with a 15-second grace window after editing ends.
12. **Hide/show and transparency changes exempted from the assembly-edit guard** — same
    reasoning as suppress/unsuppress (already user-approved earlier): local viewing state,
    not real edits, and blocking them got in the way of a legitimate workflow (hiding
    neighbors to take a measurement while locked out of the assembly).
13. **Close-time full-scan caching for speed.** `blockCloseForOwnedLocks`'s
    `scanWholeWorkingCopy:=True` path (a full `svn status -v` over the entire working copy,
    the actual cause of "closing SolidWorks takes forever") now reuses a cached scan
    (`ownedLocksWholeCopySnapshot`, 2-minute TTL) instead of rescanning on every close
    attempt. Invalidated centrally inside `updateStatusCacheForKnownPaths` (whenever called
    with `forceLock6:=...`, which every Get Locks/Commit/Unlock path already does) so a
    future lock-changing code path can't accidentally skip invalidation. A one-shot timer in
    UserControl1.vb (`ownedLocksWarmupTimer`, ~4s after startup) pre-warms it. Falls back to
    the original always-correct synchronous scan whenever no valid cache exists.
14. **New-document (including drawing) first save now uses the review table**, not the old
    `InputBox` + native `SaveFileDialog`. Added `CadRelocationMode.NewSave` to codex's
    `CadRelocationReviewForm.vb` (title/header/continue-button text branch on the new mode;
    `nameTextBox.ReadOnly` already correctly evaluates `False` for it without changes), and a
    new `checkNewDocumentSaveDestination` checker function in svnModule.vb. Both call sites
    (`handleSolidWorksSaveCommandPreNotifyPublic`, `performSaveAsButtonActionPublic`) no
    longer call `promptForValidGrc27FileName` first — the table is now the single place a
    name is entered and validated.
15. **UI layout overlap fixed** — twice, because the first fix targeted the wrong layer.
    Static `Designer.vb` coordinates for `FileActionToolStrip`/`localRepoPath`/`ToolStrip1`
    turned out to be overridden at runtime by `positionFileActionsAboveRepositoryPath()` in
    UserControl1.vb. Extended *that* function to also reposition `versionLabel` (was fixed at
    design-time X=282,Y=70, which collided with the now-full-width repo-path box) below the
    repo-path row, and to push `TreeView1.Top` down if it would otherwise start above the
    icon toolstrip's actual bottom edge.
16. **Ctrl+S / close-time native dialog cascade — root-caused and reduced at its trigger.**
    Cause: calling `SetReadOnlyState(False)` on the assembly just locked (needed to make it
    writable) can trigger a SolidWorks rebuild that cascades a spurious `GetSaveFlag=True`
    onto sibling documents never touched by the user — a limitation already acknowledged in
    a comment elsewhere in this codebase, and SolidWorks provides no public API to clear
    `GetSaveFlag` once set. The native "Save Modified Documents" / close-time dialogs read
    that flag directly and cannot be filtered or suppressed from outside SolidWorks.

    The trigger is now reduced by keeping live-document writable-state reconciliation scoped
    to the active document and any actively edited in-context child. All requested SVN locks
    are still acquired and every locked path is made writable on disk; other already-open
    SOLIDWORKS documents are switched from their internal read-only state only when the user
    activates or edits them. This scoping applies to status refreshes, background server
    status application, and—after a follow-up audit—the actual asynchronous Get Locks
    completion path.

    A proposed Ctrl+S advisory warning was removed because it added another modal click,
    claimed more certainty than `GetSaveFlag` + read-only can provide, and told users it was
    safe to select files that PlumVault's save pre-event would then correctly block. The
    cascade cannot be guaranteed gone without live testing, but the known bulk transition
    that caused it is no longer performed.

    A live repro later showed the remaining direct cause: native SOLIDWORKS Ctrl+S displayed
    "Save Modified Documents" and selected dirty referenced parts even when only the top
    assembly was locked. Existing managed Ctrl+S is now intercepted and replaced with a
    queued silent `ModelDoc2.Save3` for the active document only. The call deliberately omits
    `swSaveAsOptions_SaveReferenced`, then explicitly queues one automatic commit for that
    active path. Save As, files outside the configured SVN working copy, and first-save naming
    keep their existing flows. This is the primary functional fix for the screenshot repro;
    the interaction-scoped writable transition above remains useful prevention.

    Close classification was also aligned with product policy: an existing managed document
    without this working copy's `K` lock token is not a committable edit, so an in-memory dirty
    flag alone does not produce PlumVault's "not safe to close" warning. If the old native
    Save All already wrote a child to disk, its real SVN `M` status is still intentionally
    reported and must be reverted once; the new Ctrl+S path prevents creating that state.

    Live testing then exposed a final close-only native prompt after the user released clean
    retained locks in the table. PlumVault's checks had passed, but returning to native app
    close let SOLIDWORKS ask to save stale in-memory dirty flags. App close is now completed
    through a queued verified-close path: after every existing lock/dirty/SVN check passes,
    `CloseAllDocuments(True)` closes the already-vetted documents without saving and `ExitApp`
    completes shutdown. Document-level close is unchanged. A COM failure while enumerating
    open documents now blocks this controlled shutdown rather than failing open.

## Explicitly NOT done / still open

- **The close-prompt wording** ("Yes = I want to go back to get locks / push / revert...").
  Gave an honest opinion when asked (directionally fine, but the wording bundles three
  different next steps into one option and could be clearer for an SVN novice) — did **not**
  change it, since the user asked for an opinion, not an implementation.
- **A pre-existing false dirty flag still cannot be cleared**, per #16 above. The known bulk
  writable-transition trigger has been reduced, but once SOLIDWORKS has set `GetSaveFlag`
  there is still no supported reset API. Do not attempt to "just suppress the native dialog"
  without a safe, verified mechanism.
- **Nothing has been run inside SolidWorks.** Every single item above needs a live pass
  before it can be trusted. If the user reports a fix "isn't working," the first move should
  be checking whether it actually made it into the current file (see "Working with codex")
  before assuming the logic itself is wrong.

## Suggested next steps

1. `git status` / `git diff --stat` — confirm nothing changed underneath since this was
   written, and skim for anything unexpected from codex.
2. Rebuild once to confirm the baseline still compiles:
   ```bash
   "C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\MSBuild.exe" PlumVaultProj.vbproj /t:Build /p:Configuration=Debug /p:RegisterForComInterop=false /v:minimal /nologo
   ```
3. Ask the user whether they've had a chance to load this into SolidWorks yet, and if so,
   prioritize whatever they report over anything in this file — a live report always
   supersedes an untested assumption here.
4. If nothing's been tested yet, the highest-value things to walk through first (most
   complex / most recently touched):
   - The in-context "Edit Part" repro from item 11 (lock a part, edit in a separate window,
     switch to the assembly, "Edit Part" in-context, edit again, exit — should no longer
     falsely flag).
   - The new-document/new-drawing Save flow from item 14 (create a new part/drawing, Ctrl+S,
     confirm the table appears instead of the old InputBox+SaveFileDialog).
   - The layout fix from item 15 at a couple of different task-pane widths/DPI settings.
   - The close-speed caching from item 13 — close SolidWorks twice in a row within a couple
     of minutes and confirm the second close is noticeably faster, and that acquiring/
     releasing a lock in between still forces a fresh scan (i.e. correctness wasn't traded
     for speed).
5. Nothing here has been committed. Don't commit or push without the user asking explicitly.
