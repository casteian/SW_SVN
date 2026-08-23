# Work plan / session handoff

Last updated: 2026-08-23, end of a very long single session (the previous entry, dated
2026-08-22, was the session before this one — fully superseded by everything below, including
several of its own "explicitly not done" items, which have since been resolved and re-broken).
Read `CLAUDE.md` first for architecture/conventions; this file is the "what's actually in
flight right now" state.

**Codex has continued working on this codebase in parallel, including after this session's
Claude work landed** — the user explicitly flagged mid-session that "significant upgrades"
happened that Claude did not observe turn-by-turn. Confirmed by direct comparison: codex went
further than Claude's own crash fix (see item 6 below) and removed `EditUndo2` from the
*assembly* guard too, not just the part guard Claude fixed it in. CLAUDE.md's "CAD
edit-protection guard" section now also describes cancellable pre-events for feature/sketch
edits and a nested-assembly suppression-ownership case that Claude did not personally implement
or verify — read that section as authoritative over any half-remembered description here.
**Re-verify current code before trusting any specific function/line description below.**

## Repo state right now

```
git status
```
should show these modified (all uncommitted, unless committed/pushed after this was written):
`CLAUDE.md`, `CadRelocationReviewForm.vb`, `CloseLockReviewForm.vb`, `EventHandling.vb`,
`SwAddin.vb`, `UserControl1.vb`, `svnModule.vb`. Two commits from earlier in this session
(`cad530a More Updates`, `cd07813 Fix silent app-close block and add duplicate GRC27 name
check`) are already pushed to `origin/master`; everything since is uncommitted.

First thing to do in a new session: `git status` / `git diff --stat`, skim for anything
unexpected from codex, and rebuild once to confirm the baseline still compiles:
```bash
"C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\MSBuild.exe" PlumVaultProj.vbproj /t:Build /p:Configuration=Debug /p:RegisterForComInterop=false /v:minimal /nologo
```
(Confirmed clean as of the last edit in this session.)

## What was done this session (chronological; "user-tested/reported live" items are real
SolidWorks repros the user hit and described — everything else is build-verified only)

1. **Reconciliation narrowing.** `getActiveInteractionLockedPaths`/
   `reconcileWriteAccessForActiveDocumentPublic` — scoped the broad "flip every locked+open
   document writable" sweep down to just the active document + any live in-context-edit child,
   to reduce how often `SetReadOnlyState(False)` fires (each flip is a chance to trigger the
   spurious-sibling-dirty-flag cascade this whole session's early discussion was about). A
   follow-up attempt to also poll for externally-acquired locks on a timer caused visible UI
   flicker — **user-reported live, reverted same session.**
2. **In-context edit lock enforcement.** `enforceInContextEditRequiresLock`, fired from
   `noteInContextEditBeganPublic` (`BeginInContextEditNotify`). Detects an in-context edit
   starting on a child with no current lock and backs out via `AssemblyDoc.EditAssembly()`
   (confirmed to exist via reflection against the installed interop DLL before use), plus
   auto-kicks off `getLocksOfPathsAsync` for that file so the user doesn't need a second manual
   Get Locks click. **User-tested live, confirmed working.**
3. **Deep code review** (8-angle parallel review of the full uncommitted diff at that point)
   surfaced and Claude fixed:
   - `allowActiveChildEditContext` (a parameter codex had added) defaulted `False` and was
     never passed `True` by `ComponentMoveNotify2`/`ComponentReorganizeNotify`/`AddItemNotify`/
     delete/rename pre-notify — the documented "locked child edited in-context is exempt from
     the parent's lock requirement" invariant silently didn't apply to those five paths. Fixed.
   - `assemblyIsEditingExternalPhysicalChild`'s primary detection path (via
     `AssemblyDoc.GetEditTarget()`) returned "exempt" without ever checking the child actually
     had a lock — a **false negative**: being in Edit Component mode on *any* unlocked child
     (not just your own) silently bypassed the whole guard. Fixed to require
     `externalChildPathHasRequiredLockFast`, matching the guard's other two detection paths.
   - `controlledDocumentCloseInProgress` was a single global boolean; closing one document
     could silently block an unrelated document's close for the whole multi-turn unwind
     window. Rescoped to per-path via the existing `assemblyGuardControlledCloseQueuedPaths`
     set (the whole-app close guard's own separate global check was left alone — that one
     *should* be global).
   - `CloseLockReviewForm` column `MinimumWidth`s summed to more than the form's
     `MinimumSize` width (a recurring pattern across these review-table forms) — bumped
     `MinimumSize`.
   - A pre-existing, unrelated build break (`Environment.NewLine` ambiguous between `System`
     and `SolidWorks.Interop.sldworks`, from a very recent external edit) — fixed via `vbCrLf`.
   - Flagged but **deliberately not fixed**: `ensureLiveLockedCommitPathsWritable` calls
     `SetReadOnlyState(False)` directly/synchronously during Commit, reintroducing the same
     bulk-writable-cascade risk item 1 was mitigating. Needs a real redesign (route through the
     deferred queue), not a blind patch.
4. **Revert/Discard false-failure bug** — **user-reproduced live** ("SOLIDWORKS still reports
   unsaved changes after Discard" shown even though the discard fully succeeded):
   `performCloseReviewRevertNow` required `canTreatAssemblySaveFlagAsGuardGenerated` to agree
   the post-reload `GetSaveFlag()` was ignorable, but that helper is scoped for a *different*
   case (guard-generated false-dirty) and returns "not ignorable" whenever you still hold the
   lock, which Discard deliberately retains. Fixed: a fresh SVN check moments earlier already
   proves the file clean, so a lingering `GetSaveFlag()=True` there is logged, not treated as
   failure.
5. **Multi-layer assembly / window-jump investigation** — traced (not assumed) the save flow,
   auto-commit bundling, event-handler re-attachment on window switch, and the multi-level
   commit-safety check; all held up under tracing. Also traced codex's separate
   `handleInContextEditCommandPreNotify` auto-lock-and-replay mechanism and confirmed it
   already handles window-jump-mid-flight and multi-level nesting deliberately.
6. **Standalone part had no edit guard at all** — **user-reproduced live** (opened a part
   directly, it wasn't read-only despite no lock, was freely editable). Added
   `handlePartOwnedEditPostPublic` on `PartDoc.ModifyNotify`/`RegenNotify`/`RegenPostNotify`,
   reusing the assembly guard's lock-check/rebuild-tracking machinery. **First version called
   `EditUndo2` and crashed SolidWorks** on a second in-context geometry edit after an earlier
   undo on a reopened part — **user-reported live** (`sldProcMon.exe` access violation,
   referenced 0x...D0). Fixed by removing the destructive undo entirely; warn-only now
   (detects, tells the user to Get Locks or self-undo, never touches the document). Also added
   `reconcileReadOnlyForUnlockedActiveDocumentPublic` (restores read-only on window activation,
   but only for an unlocked *and clean* document, to avoid the false-dirty-flag risk).
   **Codex has since also removed `EditUndo2` from the assembly guard** — confirmed by reading
   current code (`CLAUDE.md` now says "never call `EditUndo2` from a guard callback," no
   qualifier). Claude's interim assembly-guard mitigation (gate the undo behind
   `ISldWorks.CommandInProgress`, confirmed via reflection) is superseded by codex's fuller
   removal — don't be surprised that check isn't in the code anymore.
7. **Ctrl+W ran its full safety check (including a possible modal dialog) synchronously inside
   a raw `WH_KEYBOARD` hook callback** — a known-unsafe pattern, and the likely cause of both
   "review table doesn't appear for Ctrl+W" and possibly the separately-reported "X buttons
   stop responding after a while" (a wedged low-level hook can degrade input broadly). Fixed:
   the hook now always eats Ctrl+W immediately and defers the actual check to
   `svnModule.queueDeferredCtrlWCloseCheckPublic` (`BeginInvoke`, safe UI-thread context),
   replaying the close via `CloseDoc` if the deferred check comes back clean. Confirmed via
   reading `SolidWorksDocumentCloseGuardWindowHook.WndProc` that the small-X/Window-menu-close
   paths already run in a normal, safe `WndProc` context and were not touched.
8. **Component suppression required no lock** — **user-flagged as wrong** ("suppressing is
   different from hiding" — the exemption written earlier this session as "user-confirmed
   intentional" was itself wrong). `ComponentStateChangeNotify`/`Notify2` now route through the
   same guard as add/delete/move for **both** suppress and unsuppress (unsuppress included on
   the reasoning that it's the same persisted state in reverse — flag to the user if they only
   meant one direction). `CLAUDE.md` corrected to match, including a further nested-assembly
   suppression-ownership addition that appears to be codex's, not personally verified by Claude.
9. **Robustness/self-healing additions**, in response to the user's "protections stop working
   after a while / after reopening" and "little X and big X stop working after a while"
   reports (**neither fully root-caused** — see below):
   - `assemblyGuardControlledCloseQueuedPaths` and `assemblyGuardQueuedPaths` both now
     self-heal: any entry left stuck (e.g. by an exit path that fails to clean up) auto-clears
     after 2 minutes instead of silently disabling that protection for the rest of the session.
     This is a defensive addition for one *plausible* mechanism (the failure mode would be
     completely silent — no error, protection just quietly stops firing) — not a confirmed fix.
10. **UI/workflow fixes**, mostly from one large user bug-report batch:
    - Duplicate-filename check added to new-document Save (GRC27 names must be unique
      repo-wide, not just within the destination folder).
    - `SVNStatus.updateStatusLocally`'s merge logic only copied `lock6`/`released` from a fresh
      local scan into the existing status array, never `addDelChg1` — so a file that just went
      from unversioned to committed kept showing "Not in SVN" until a full Sync (which replaces
      the array wholesale) instead of a plain Refresh. **User-reported live**, root-caused and
      fixed.
    - First-commit-after-insert prompt: inserting a component into a never-saved assembly now
      offers to save/commit it immediately (once per assembly per session) — directly
      requested by the user to avoid an "alert flurry" on a later in-context edit attempt.
    - Move/Re-ID destination check now gives one specific reason instead of one generic message
      covering 7 unrelated causes, and auto-`svn add`s a destination folder that exists on disk
      but isn't versioned yet (e.g. just created via Browse) instead of hard-blocking — mirrors
      what new-document Save already did, per the user's explicit "make sure new folders get
      committed too" ask.
    - `CadRelocationReviewForm` (Move/Re-ID/Save New): selection color set `BackColor` but
      never `SelectionForeColor`, which read back `Color.Empty` and rendered as near-invisible
      text once a row was selected — **user-reported live from a screenshot**. Fixed with an
      explicit `Color.Black`. Also widened the File column (realistic GRC27 names were
      truncating at the old 180px default).
    - Confirmation message added after a new file's Save succeeds (previously silent until the
      background commit's own message, much later).

## Explicitly NOT done / still open — in the order the user raised them

- **The native "Save modified documents" dialog still appeared after the close-review table**,
  even after Commit/Unlock inside it — **user-reported live**, this session's last exchange
  before handoff. Claude traced the whole chain (`blockCloseForOwnedLocks` → `ContinueClose` →
  `blockCloseIfOpenDocsUnsafeOnly` → `queueVerifiedSafeApplicationClose` →
  `continueVerifiedSafeApplicationClose` → `closeAllVerifiedDocumentsWithoutSaving` →
  `ExitApp`) and it reads as correctly wired to prevent exactly this. **Unresolved** — the last
  question asked of the user, not yet answered: did they click the table's "Close SOLIDWORKS —
  no further saves" button specifically, or dismiss the table some other way? That's the fork
  point in the logic — only that button queues the controlled close-everything-then-exit
  sequence. If they did click it and still saw the native prompt, check
  `closeAllVerifiedDocumentsWithoutSaving`'s reliance on closing documents by `GetTitle()`
  string via `CloseDoc` — a plausible failure if two open documents ever share a title,
  unconfirmed. (Note: this function used to be described as calling `CloseAllDocuments(True)`,
  per the previous session's WORK_PLAN entry and pre-correction `CLAUDE.md` text — the actual
  code no longer does that; it's a per-document `CloseDoc` loop now. Confirm which is really in
  the file before assuming either description.)
- **"Little X and big X not working after a while."** Not root-caused. The Ctrl+W low-level-hook
  fix (item 7) is a plausible *partial* explanation (a wedged hook degrading input generally)
  but this was speculative when written, not confirmed against this specific symptom.
- **"Protections work on first open, but not on subsequent trials after some operations /
  closing and reopening SolidWorks."** Not root-caused. Item 9's self-healing staleness fixes
  are a defensive mitigation for one plausible mechanism (a leaked queue entry), not a confirmed
  fix — a leaked in-memory flag shouldn't normally survive an actual SolidWorks *process*
  restart, which makes this report harder to explain than same-session degradation would be.
  Worth a live test isolating "does it degrade within one session" vs. "only after a restart"
  before guessing further.
- **Whether to remove Unlock/Revert entirely.** The user raised this ("its often prone to
  failures and needs svn cleanup") but no decision was reached — removing "Unlock" outright
  seems too broad (a simple, low-risk `svn unlock`, no filesystem mutation) whereas
  "Revert/Discard" is the more complex, more failure-prone one (matches item 4's bug and the
  general `svn cleanup`-needing pattern). Needs the user's explicit scope decision — this is a
  feature-removal call, not a bug fix, and shouldn't be made unilaterally.
- **Ctrl+S doesn't show the GRC27 naming prompt on a new drawing.** User later said this might
  just be their flaky keyboard, not a real repro — deprioritized, not investigated.
- **Efficiency findings from the original 8-angle review**, never acted on: Commit spawns the
  same per-file `svn.exe` lock check three separate times; Discard/Revert spawns `svn.exe` four
  times sequentially with the whole grid locked and no progress feedback (tied to item 4's bug
  report, may be worth revisiting alongside it).
- **Three separate copies of the same in-context-edit-unwind retry loop**
  (`continueUserApprovedDocumentCloseWithoutSave`, `continueVerifiedSafeApplicationClose`,
  `continueCloseReviewRevert`) — real duplication, flagged, never consolidated. Touches the most
  delicate close/revert paths in the codebase; deliberately not attempted without live testing
  between changes.
- **Nothing in this session has been run inside a live SolidWorks session by Claude** — every
  fix above is build-verified only, except where marked "user-tested/reported live" (real
  repros the user hit and described back — how most of the above was found), and even those
  fixes have not yet been re-tested live as of this handoff.

## Suggested next steps

1. `git status` / `git diff --stat` — confirm nothing changed underneath since this was
   written (likely, given codex's continued parallel work), skim for anything unexpected.
2. Rebuild to confirm baseline (command above).
3. Ask the user which open item to prioritize — the native "Save modified documents" dialog and
   the two "stops working after a while" reports are the highest-value/highest-uncertainty
   items and would most benefit from the user's next live test, specifically answering the
   fork-point question in the first "still open" bullet.
4. Do not further generalize/consolidate the guard system's undo behavior, retry loops, or
   close-sequencing without a live test between each change — this session's two real crash
   reports both came from exactly that class of change (an API call that looks safe in
   isolation but interacts badly with SolidWorks' own interactive/transactional state).
5. Nothing here has been committed since `cd07813`. Don't commit or push without the user
   asking explicitly.
