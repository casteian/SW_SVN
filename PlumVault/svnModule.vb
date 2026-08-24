Imports SolidWorks.Interop.sldworks
Imports SolidWorks.Interop.swconst
Imports System.Collections.Generic
Imports System.Configuration
Imports System.IO
Imports System.Linq
Imports System.Runtime.Remoting.Messaging
Imports System.Windows.Forms.LinkLabel
Imports System.Windows.Forms.VisualStyles.VisualStyleElement
Imports System.Xml
Imports System.Threading.Tasks
Imports System.Diagnostics
Imports System.Text

Public Module svnModule
    Private Class ExternalReferenceInfo
        Public Property oldPath As String
        Public Property newPath As String
        Public Property fileName As String
    End Class

    Private Class SyncStatusChunkResult
        Public Entries As New List(Of SVNStatus.filePpty)()
        Public ErrorMessage As String = ""
        Public TimingLog As String = ""
    End Class

    Private Class DrawingFreshnessChunkResult
        Public OutOfDatePaths As New List(Of String)()
        Public ErrorMessage As String = ""
        Public HasOutOfDateViews As Boolean = False
    End Class

    Private Class AsyncLocalSvnState
        Public StatusChar As Char = ChrW(0)
        Public HasLocalLockToken As Boolean = False
    End Class

    Dim myUserControl As UserControl1
    Dim iSwApp As SldWorks
    Dim statusOfAllOpenModels As SVNStatus
    Private liveAssemblyChangeCheckInProgress As Boolean = False
    Private pendingExternalRefCommitPaths As New List(Of String)
    Private pendingExternalRefSkipNameCheckPaths As New List(Of String)
    Private closeGuardMessageShowing As Boolean = False
    Private lockReviewMessageShowing As Boolean = False
    Private controlledApplicationCloseQueued As Boolean = False
    Private controlledApplicationExitInProgress As Boolean = False
    Private controlledApplicationNativeCloseCallInProgress As Boolean = False
    Private applicationCloseRequestedAfterDocumentClose As Boolean = False
    Private applicationExitStateWatchdog As System.Windows.Forms.Timer = Nothing
    Private controlledDocumentCloseNativeCallInProgress As Boolean = False
    Private lastCloseGuardPromptTime As DateTime = DateTime.MinValue
    Private unsafeForceCloseApprovedUntil As DateTime = DateTime.MinValue
    Private unsafeForceCloseApprovedPath As String = ""
    Private documentLockReviewApprovedUntil As DateTime = DateTime.MinValue
    Private documentLockReviewApprovedPath As String = ""
    Private documentLockReviewApprovedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    Private applicationLockReviewApprovedUntil As DateTime = DateTime.MinValue
    Private applicationLockReviewApprovedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    Private statusCacheByNormalizedPath As New Dictionary(Of String, SVNStatus.filePpty)(StringComparer.OrdinalIgnoreCase)
    Private statusCacheLastWriteUtc As DateTime = DateTime.MinValue
    Private statusCacheLastServerAwareUtc As DateTime = DateTime.MinValue
    Private asyncGetLocksInProgress As Boolean = False
    Private ReadOnly asyncGetLocksStateSync As New Object()
    Private asyncGetLocksRequestedPaths() As String = Nothing
    Private pendingCloseReviewLockPath As String = ""
    Private asyncCommitInProgress As Boolean = False
    Private asyncCleanupInProgress As Boolean = False
    Private closeReviewRevertInProgress As Boolean = False

    'The close-review table remains open while its TortoiseSVN commit runs. This event is
    'raised on the SOLIDWORKS UI thread so that table can refresh the affected row in place.
    Public Event CloseReviewCommitCompleted(ByVal committedPaths() As String,
                                            ByVal success As Boolean,
                                            ByVal errorMessage As String)
    Public Event CloseReviewLockCompleted(ByVal lockedPath As String,
                                          ByVal success As Boolean,
                                          ByVal errorMessage As String)
    Public Event CloseReviewRevertCompleted(ByVal revertedPath As String,
                                            ByVal success As Boolean,
                                            ByVal errorMessage As String)
    Private cachedConfiguredRepoPathForWorkingCopyRoot As String = ""
    Private cachedResolvedWorkingCopyRoot As String = ""

    'Native SOLIDWORKS mutation gate.
    'SOLIDWORKS COM calls that change live document state must never overlap reference
    'relinking, save completion, tree refreshes, or lock/read-only reconciliation.
    Private ReadOnly solidWorksNativeMutationSync As New Object()
    Private solidWorksNativeMutationInProgress As Boolean = False
    Private solidWorksNativeMutationDescription As String = ""

    'Lightweight diagnostic log. A native SOLIDWORKS crash cannot be caught by VB.NET,
    'so the final completed phase is written here for post-crash diagnosis.
    Private ReadOnly operationLogSync As New Object()
    Private operationLogFilePath As String = ""

    'Automatic save -> SVN commit state.
    'All of this runs on the SOLIDWORKS UI thread except the actual svn.exe commit process.
    Private internalSolidWorksSaveDepth As Integer = 0
    Private newDocumentTeamSaveWorkflowInProgress As Boolean = False
    Private managedActiveDocumentSaveQueued As Boolean = False
    Private managedActiveDocumentSaveQueuedUtc As DateTime = DateTime.MinValue
    Private pendingAutomaticSaveCommitPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    Private ReadOnly automaticSaveStateSync As New Object()
    Private postFirstCommitLockPendingPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    Private postFirstCommitLockRetryPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    Private postFirstCommitLockRetryTimer As System.Windows.Forms.Timer = Nothing
    Private postFirstCommitLockRetryStartedUtc As DateTime = DateTime.MinValue
    Private automaticSaveCommitPreparing As Boolean = False
    Private legacyImportInProgress As Boolean = False
    Private cadRelocationInProgress As Boolean = False

    'Assembly edit protection state. The guard is event-driven; it does not poll and
    'does not contact the SVN server while the user is modelling.
    Private assemblyGuardUndoInProgress As Boolean = False

    'A Rebuild picks up an already-correctly-updated (locked/committed) child and can raise
    'ModifyNotify on every ancestor assembly purely from that recompute, with no structural
    'edit of the ancestor itself. RegenNotify/RegenPostNotify bracket the real rebuild so that
    'recompute-only dirtying is never mistaken for an edit requiring the ancestor's own lock.
    Private Class AssemblyRebuildTracker
        Public Property Depth As Integer = 0
        Public Property LastBeginUtc As DateTime = DateTime.MinValue
        Public Property WasDirtyAtOuterBegin As Boolean = False
        Public Property SawGenericModifyNotify As Boolean = False
    End Class

    'If RegenPostNotify is ever lost (SOLIDWORKS errors out mid-rebuild, an exception, etc.)
    'the matching RegenNotify would otherwise leave that assembly's edit guard silently
    'suppressed forever. A generous staleness window makes that self-healing instead of a
    'permanent, invisible loss of lock protection for the rest of the session.
    Private Const REBUILD_SUPPRESSION_STALE_MINUTES As Double = 30.0
    Private ReadOnly assemblyRebuildPaths As New Dictionary(Of String, AssemblyRebuildTracker)(StringComparer.OrdinalIgnoreCase)
    Private Const COMPLETED_REBUILD_MODIFY_GRACE_SECONDS As Double = 10.0
    Private ReadOnly completedAssemblyRebuildModifyUtcByPath As New Dictionary(Of String, DateTime)(StringComparer.OrdinalIgnoreCase)

    'A drawing opened alone (its referenced part/assembly not also opened) can silently show
    'stale views if someone else committed a newer revision of that geometry since this
    'working copy last updated. Track only checks currently running: closing and reopening a
    'drawing must run a fresh check because a teammate may have committed in between.
    Private ReadOnly drawingFreshnessChecksInProgress As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    'Value is when the entry was queued, so a leaked entry (any exit path that ever fails to
    'remove its own key) self-heals after ASSEMBLY_GUARD_QUEUE_STALE_MINUTES instead of
    'silently disabling the assembly-edit guard for that one assembly for the rest of the
    'session - the queue check (assemblyGuardQueuedPaths.Contains) is a silent no-op skip with
    'no user-facing symptom, so a leak here would look exactly like "protection stopped working."
    Private ReadOnly assemblyGuardQueuedPaths As New Dictionary(Of String, DateTime)(StringComparer.OrdinalIgnoreCase)
    Private Const ASSEMBLY_GUARD_QUEUE_STALE_MINUTES As Double = 2.0
    Private ReadOnly assemblyGuardSync As New Object()
    Private lastAssemblyGuardMessageUtc As DateTime = DateTime.MinValue
    Private lastAssemblyGuardMessagePath As String = ""

    'A dimension selected through an assembly can belong to a separately file-backed child
    'even when IAssemblyDoc.GetEditTarget temporarily returns Nothing while the dimension
    'dialog is opening. Remember that selection briefly so the assembly guard checks the
    'document that actually owns the dimension instead of blocking the parent assembly.
    Private Class AssemblySelectionContext
        Public Property ChildPath As String = ""
        Public Property CapturedUtc As DateTime = DateTime.MinValue
        Public Property IsDimensionSelection As Boolean = False
    End Class

    Private ReadOnly assemblySelectionContextByAssemblyPath As New Dictionary(Of String, AssemblySelectionContext)(StringComparer.OrdinalIgnoreCase)

    Private Class RecentNestedFeatureOwner
        Public Property OwnerPath As String = ""
        Public Property CapturedUtc As DateTime = DateTime.MinValue
    End Class

    'Selecting a nested assembly feature is the last reliable point at which SOLIDWORKS still
    'identifies its owner. Suppress/unsuppress can replace that selection with a generated
    'component before ComponentStateChangeNotify arrives.
    Private ReadOnly recentNestedFeatureOwnerByEventAssembly As New Dictionary(Of String, RecentNestedFeatureOwner)(StringComparer.OrdinalIgnoreCase)

    Private Class PendingAssemblySuppressionCommand
        Public Property ActiveAssemblyPath As String = ""
        Public Property OwnerAssemblyPath As String = ""
        Public Property Command As Integer = 0
        Public Property OpenedUtc As DateTime = DateTime.MinValue
        Public Property ClosedUtc As DateTime = DateTime.MinValue
        Public Property ExpiryQueued As Boolean = False
    End Class

    'Suppressing a feature in an expanded subassembly is allowed by SOLIDWORKS without first
    'entering Edit Assembly. The later ComponentStateChange/Modify events are then raised on
    'both the true subassembly owner and one or more ancestors. Capture the selected feature's
    'owner at the cancellable command boundary so those ancestor bookkeeping events cannot be
    'mistaken for an unlocked edit of the top-level assembly.
    Private pendingAssemblySuppressionState As PendingAssemblySuppressionCommand = Nothing

    'BeginInContextEditNotify/EndInContextEditNotify are SOLIDWORKS' own purpose-built signal
    'for "the user is editing this specific child in-context from the assembly window" - more
    'reliable than GetEditTarget at the moment ModifyNotify actually fires, which can already
    'be Nothing again by the time control has returned to the assembly (e.g. right after
    'exiting "Edit Part" in-context editing).
    Private Class InContextEditSession
        Public Property ChildPath As String = ""
        Public Property LockPath As String = ""
        Public Property BeganUtc As DateTime = DateTime.MinValue
        Public Property EndedUtc As DateTime = DateTime.MinValue 'MinValue while still actively editing.
        Public Property AssemblyWasDirtyBeforeEdit As Boolean = True
        Public Property ParentWasReadOnly As Boolean = False
        Public Property ParentOriginalAttributes As FileAttributes = FileAttributes.Normal
        Public Property ParentHadOriginalAttributes As Boolean = False
        Public Property ParentTemporarilyWritable As Boolean = False
    End Class

    Private ReadOnly inContextEditSessionByAssemblyPath As New Dictionary(Of String, InContextEditSession)(StringComparer.OrdinalIgnoreCase)

    Private Class InContextEditDirtyBaseline
        Public Property WasDirty As Boolean = True
        Public Property CapturedUtc As DateTime = DateTime.MinValue
    End Class

    'CommandOpenPreNotify runs before SOLIDWORKS dirties the parent as bookkeeping for Edit
    'Part. Carry that clean/dirty baseline into BeginInContextEditNotify so close handling can
    'distinguish a child-driven parent SaveFlag from a genuine unsaved assembly edit.
    Private ReadOnly pendingInContextDirtyBaselineByAssemblyPath As New Dictionary(Of String, InContextEditDirtyBaseline)(StringComparer.OrdinalIgnoreCase)

    'If Edit Component is clicked while a manual Get Locks request for that exact child is
    'still running, cancel the premature native command. Once that existing request proves
    'the lock is locally owned and the child is writable, replay the original command. This
    'never starts a lock automatically and never replays after a failed/conflicting lock.
    'Only stable paths cross the asynchronous boundary so stale Component2 RCWs are never retained.
    Private Class PendingInContextAutoEdit
        Public Property AssemblyPath As String = ""
        Public Property ChildPath As String = ""
        Public Property RequestedUtc As DateTime = DateTime.MinValue
        Public Property RequestedCommand As Integer = 0
    End Class

    Private pendingInContextAutoEditRequest As PendingInContextAutoEdit = Nothing
    Private inContextAutoEditReplayInProgress As Boolean = False
    Private inContextExitTransitionInProgress As Boolean = False
    Private ReadOnly inContextExitTransitionQueuedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

    'Display-only commands can also raise the assembly's generic ModifyNotify. Remember the
    'purpose-built visibility/appearance event briefly so the generic post-event does not
    'undo a harmless viewing change. Explicit structural events remain independently guarded.
    Private ReadOnly assemblyDisplayOnlyChangeUtcByPath As New Dictionary(Of String, DateTime)(StringComparer.OrdinalIgnoreCase)
    Private Const DISPLAY_ONLY_MODIFY_GRACE_MILLISECONDS As Double = 500.0

    'SOLIDWORKS can leave an assembly SaveFlag set after an unauthorized edit was undone, a
    'rebuild, or a legitimate in-context child edit, even though the assembly file itself is
    'still SVN-clean. Track only those event-proven cases; any later assembly-owned edit clears
    'the candidate, and close handling also re-verifies local SVN cleanliness.
    Private ReadOnly assemblyGuardFalseDirtyCandidatePaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

    'When SOLIDWORKS leaves an assembly dirty after PlumVault has already undone a blocked
    'assembly edit, swallow the original close message and close that verified-clean document
    'one UI turn later with ISldWorks.QuitDoc. Only stable file paths cross the deferred boundary.
    Private ReadOnly assemblyGuardControlledCloseQueuedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

    'Tracks how long ANY entry has been continuously outstanding in
    'assemblyGuardControlledCloseQueuedPaths, so the whole-application close guard
    '(blockCloseIfOpenDocsUnsafe) can self-heal instead of silently blocking the big-X close
    'forever if some exit path is ever found to leave a stale entry behind. MinValue means empty.
    Private assemblyGuardControlledCloseQueuedPathsSinceUtc As DateTime = DateTime.MinValue
    Private Const CONTROLLED_CLOSE_QUEUE_STALE_MINUTES As Double = 2.0
    Private lastControlledCloseQueueBlockedMessageUtc As DateTime = DateTime.MinValue

    'Callers must already hold assemblyGuardSync.
    Private Sub addControlledCloseQueuedPathLocked(ByVal normalizedPath As String)
        If assemblyGuardControlledCloseQueuedPaths.Count = 0 Then
            assemblyGuardControlledCloseQueuedPathsSinceUtc = DateTime.UtcNow
        End If
        assemblyGuardControlledCloseQueuedPaths.Add(normalizedPath)
    End Sub

    'Callers must already hold assemblyGuardSync.
    Private Sub removeControlledCloseQueuedPathLocked(ByVal normalizedPath As String)
        assemblyGuardControlledCloseQueuedPaths.Remove(normalizedPath)
        If assemblyGuardControlledCloseQueuedPaths.Count = 0 Then
            assemblyGuardControlledCloseQueuedPathsSinceUtc = DateTime.MinValue
        End If
    End Sub

    'True only while at least one entry is outstanding AND it hasn't been stuck longer than the
    'stale threshold. A stale set self-heals (cleared, logged) rather than permanently blocking
    'the application close guard with no way for the user to recover short of restarting SOLIDWORKS.
    Private Function hasFreshControlledCloseQueuedPaths() As Boolean
        SyncLock assemblyGuardSync
            If assemblyGuardControlledCloseQueuedPaths.Count = 0 Then Return False

            If assemblyGuardControlledCloseQueuedPathsSinceUtc <> DateTime.MinValue AndAlso
               (DateTime.UtcNow - assemblyGuardControlledCloseQueuedPathsSinceUtc).TotalMinutes > CONTROLLED_CLOSE_QUEUE_STALE_MINUTES Then

                writeOperationLog(
                    "controlledCloseQueuedPaths stale after " & CONTROLLED_CLOSE_QUEUE_STALE_MINUTES.ToString() &
                    " minute(s), clearing: " & String.Join(" | ", assemblyGuardControlledCloseQueuedPaths.ToArray())
                )
                assemblyGuardControlledCloseQueuedPaths.Clear()
                assemblyGuardControlledCloseQueuedPathsSinceUtc = DateTime.MinValue
                Return False
            End If

            Return True
        End SyncLock
    End Function

    Private Const SW_COMMAND_SAVE As Integer = 2
    Private Const SW_COMMAND_INSERT_COMPONENTS As Integer = 13
    Private Const SW_COMMAND_DELETE As Integer = 16
    Private Const SW_COMMAND_SUPPRESS As Integer = 14
    Private Const SW_COMMAND_UNSUPPRESS As Integer = 15
    Private Const SW_COMMAND_UNSUPPRESS_WITH_DEPENDENTS As Integer = 50
    Private Const SW_COMMAND_CHANGE_SUPPRESSION_STATE As Integer = 150
    Private Const SW_COMMAND_SAVE_AS As Integer = 620
    'Values verified against the installed SolidWorks.Interop.swcommands.dll. Edit Component
    'has two UI command IDs; Edit Feature is intercepted separately for deep tree selections.
    Private Const SW_COMMAND_EDIT_COMPONENT As Integer = 119
    Private Const SW_COMMAND_EDIT_FEATURE As Integer = 623
    Private Const SW_COMMAND_EDIT_PART As Integer = 965
    Private Const SW_COMMAND_SKETCH As Integer = 45
    Private Const SW_COMMAND_EDIT_SKETCH As Integer = 859
    Private Const SW_COMMAND_MAKE_EDIT_SKETCH As Integer = 3419
    Private Const SW_COMMAND_INSERT_FEATURE_FOLDER As Integer = 1829
    Private Const SW_COMMAND_CREATE_EMPTY_FEATURE_FOLDER As Integer = 1906
    Private Const SW_COMMAND_EDIT_FEATURE_FOLDER As Integer = 2390
    Private Const SW_COMMAND_MAKE_SUPPRESSED As Integer = 1204
    Private Const SW_COMMAND_SUPPRESS_ALL_CONFIGS As Integer = 1417
    Private Const SW_COMMAND_SUPPRESS_SELECTED_CONFIGS As Integer = 1419
    Private Const SW_COMMAND_UNSUPPRESS_ALL_CONFIGS As Integer = 1421
    Private Const SW_COMMAND_UNSUPPRESS_SELECTED_CONFIGS As Integer = 1422
    Private Const SW_COMMAND_UNSUPPRESS_DEPENDENT_ALL_CONFIGS As Integer = 1424
    Private Const SW_COMMAND_UNSUPPRESS_DEPENDENT_SELECTED_CONFIGS As Integer = 1425
    Private Const SW_COMMAND_SUPPRESS_FEATURE As Integer = 2498
    Private Const SW_COMMAND_UNSUPPRESS_FEATURE As Integer = 2499

    Public sSVNPath As String '= "C:\Program Files\TortoiseSVN\bin\svn.exe"
    Public sTortPath As String '= "C:\Users\benne\Documents\SVN\TortoiseProc.exe"
    Public sInstallDirectory As String

    Friend Sub svnModuleInitialize(
                                  mySwAppPass As SldWorks,
                                  myUserControlPass As UserControl1,
                                  statusOfAllOpenModelsPass As SVNStatus)
        sInstallDirectory = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)

        sSVNPath = "C:\Program Files\TortoiseSVN\bin\svn.exe"
        'Debug.Print(sSVNPath)
        If Not My.Computer.FileSystem.FileExists(sSVNPath) Then
            sSVNPath = sInstallDirectory & "\bin\svn.exe"
            If Not My.Computer.FileSystem.FileExists(sSVNPath) Then
                sSVNPath = sInstallDirectory & "\svn.exe" 'Try a slightly different path
                If Not My.Computer.FileSystem.FileExists(sSVNPath) Then
                    iSwApp.SendMsgToUser2("Error: " & sInstallDirectory & "\bin\svn.exe" & "does not exist.",
                                    swMessageBoxIcon_e.swMbStop, swMessageBoxBtn_e.swMbOk)
                    setOnlineModeEnabledOnControl(myUserControlPass, False)
                End If
            End If
        End If

        sTortPath = "C:\Program Files\TortoiseSVN\bin\TortoiseProc.exe"
        If Not My.Computer.FileSystem.FileExists(sTortPath) Then
            sTortPath = sInstallDirectory & "\bin\TortoiseProc.exe"  'System.Environment.CurrentDirectory & "\TortoiseProc.exe"
            If Not My.Computer.FileSystem.FileExists(sTortPath) Then
                sTortPath = sInstallDirectory & "\TortoiseProc.exe" 'Try a slightly different path
                If Not My.Computer.FileSystem.FileExists(sTortPath) Then
                    iSwApp.SendMsgToUser2("Error: " & sInstallDirectory & "\bin\TortoiseProc.exe" & "does not exist.",
                                       swMessageBoxIcon_e.swMbStop, swMessageBoxBtn_e.swMbOk)
                    setOnlineModeEnabledOnControl(myUserControlPass, False)
                End If
            End If
        End If

        myUserControl = myUserControlPass
        iSwApp = mySwAppPass
        statusOfAllOpenModels = statusOfAllOpenModelsPass

        Try
            operationLogFilePath = Path.Combine(
                System.Environment.GetFolderPath(System.Environment.SpecialFolder.LocalApplicationData),
                "PlumVault",
                "Logs",
                "PlumVault.log"
            )
        Catch
            operationLogFilePath = ""
        End Try

        writeOperationLog("PlumVault initialized.")
    End Sub

    Private Sub writeOperationLog(ByVal message As String)
        If String.IsNullOrWhiteSpace(message) Then Exit Sub

        Try
            If String.IsNullOrWhiteSpace(operationLogFilePath) Then
                operationLogFilePath = Path.Combine(
                    System.Environment.GetFolderPath(System.Environment.SpecialFolder.LocalApplicationData),
                    "PlumVault",
                    "Logs",
                    "PlumVault.log"
                )
            End If

            Dim logFolder As String = Path.GetDirectoryName(operationLogFilePath)
            If Not String.IsNullOrWhiteSpace(logFolder) Then Directory.CreateDirectory(logFolder)

            Dim line As String =
                DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff") &
                " | T" & System.Threading.Thread.CurrentThread.ManagedThreadId.ToString() &
                " | " & message & System.Environment.NewLine

            SyncLock operationLogSync
                Try
                    If File.Exists(operationLogFilePath) Then
                        Dim logInfo As New FileInfo(operationLogFilePath)
                        If logInfo.Length > 4L * 1024L * 1024L Then
                            Dim archivePath As String = operationLogFilePath & ".old"
                            Try
                                If File.Exists(archivePath) Then File.Delete(archivePath)
                            Catch
                            End Try
                            Try
                                File.Move(operationLogFilePath, archivePath)
                            Catch
                            End Try
                        End If
                    End If
                Catch
                End Try

                File.AppendAllText(operationLogFilePath, line)
            End SyncLock
        Catch
            'Logging must never interfere with CAD work.
        End Try
    End Sub

    Public Sub logOperationPublic(ByVal message As String)
        writeOperationLog(message)
    End Sub

    Private Function tryBeginSolidWorksNativeMutation(ByVal description As String) As Boolean
        SyncLock solidWorksNativeMutationSync
            If solidWorksNativeMutationInProgress Then
                writeOperationLog(
                    "Native mutation deferred: " & description &
                    "; busy with: " & solidWorksNativeMutationDescription
                )
                Return False
            End If

            solidWorksNativeMutationInProgress = True
            solidWorksNativeMutationDescription = If(description, "")
        End SyncLock

        writeOperationLog("Native mutation begin: " & description)
        Return True
    End Function

    Private Sub endSolidWorksNativeMutation(ByVal description As String)
        SyncLock solidWorksNativeMutationSync
            solidWorksNativeMutationInProgress = False
            solidWorksNativeMutationDescription = ""
        End SyncLock

        writeOperationLog("Native mutation end: " & description)
    End Sub

    Public Function tryBeginSolidWorksNativeMutationPublic(ByVal description As String) As Boolean
        Return tryBeginSolidWorksNativeMutation(description)
    End Function

    Public Sub endSolidWorksNativeMutationPublic(ByVal description As String)
        endSolidWorksNativeMutation(description)
    End Sub

    Public Function canRunDeferredSolidWorksUiMutationPublic(Optional ByVal allowCloseReview As Boolean = False) As Boolean
        SyncLock solidWorksNativeMutationSync
            If solidWorksNativeMutationInProgress Then Return False
        End SyncLock

        If automaticSaveCommitPreparing Then Return False
        If legacyImportInProgress Then Return False
        If assemblyGuardUndoInProgress Then Return False
        If closeGuardMessageShowing Then Return False
        If lockReviewMessageShowing AndAlso Not allowCloseReview Then Return False
        If asyncCommitInProgress Then Return False

        Return True
    End Function

    Private Sub queueDeferredFeatureTreeRefresh(ByVal assemblyPath As String)
        If String.IsNullOrWhiteSpace(assemblyPath) Then Exit Sub
        If myUserControl Is Nothing Then Exit Sub

        Try
            myUserControl.queueFeatureTreeRefreshForPathsPublic(
                New String() {assemblyPath}
            )
            writeOperationLog("Queued FeatureManager refresh: " & assemblyPath)
        Catch ex As Exception
            writeOperationLog("Could not queue FeatureManager refresh: " & ex.Message)
        End Try
    End Sub


    '==========================================================================
    ' SOLIDWORKS SAVE -> SVN COMMIT
    '==========================================================================

    Private Function automaticSaveEventsSuppressed() As Boolean
        Return internalSolidWorksSaveDepth > 0 OrElse
               automaticSaveCommitPreparing OrElse
               legacyImportInProgress OrElse
               cadRelocationInProgress OrElse
               closeReviewRevertInProgress
    End Function


    '==========================================================================
    ' ASSEMBLY EDIT PROTECTION
    '==========================================================================

    Private Function assemblyEditGuardSuppressed(ByVal assemblyDocument As ModelDoc2,
                                                   Optional ByVal ignoreActiveRebuild As Boolean = False) As Boolean
        If automaticSaveEventsSuppressed() OrElse
           assemblyGuardUndoInProgress OrElse
           controlledApplicationNativeCloseCallInProgress OrElse
           controlledDocumentCloseNativeCallInProgress OrElse
           inContextExitTransitionInProgress Then Return True

        'Purpose-built user-edit events (move/add/delete/dimension) remain authoritative even
        'when SOLIDWORKS happens to wrap that edit in a RegenNotify bracket. Only the generic
        'ModifyNotify path may borrow rebuild suppression.
        If ignoreActiveRebuild Then Return False

        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Return False

        SyncLock assemblyGuardSync
            Dim tracker As AssemblyRebuildTracker = Nothing
            If Not assemblyRebuildPaths.TryGetValue(assemblyKey, tracker) OrElse tracker Is Nothing Then Return False

            If (DateTime.UtcNow - tracker.LastBeginUtc).TotalMinutes > REBUILD_SUPPRESSION_STALE_MINUTES Then
                'A matching RegenPostNotify never arrived. Do not let a lost event permanently
                'disable this assembly's lock protection - expire it and resume normal guarding.
                assemblyRebuildPaths.Remove(assemblyKey)
                Return False
            End If

            Return True
        End SyncLock
    End Function

    Public Sub beginAssemblyRebuildPublic(ByVal assemblyDocument As ModelDoc2)
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub

        SyncLock assemblyGuardSync
            Dim tracker As AssemblyRebuildTracker = Nothing

            If Not assemblyRebuildPaths.TryGetValue(assemblyKey, tracker) OrElse tracker Is Nothing Then
                tracker = New AssemblyRebuildTracker()
                assemblyRebuildPaths(assemblyKey) = tracker
            End If

            If tracker.Depth = 0 Then
                completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)

                Try
                    tracker.WasDirtyAtOuterBegin = assemblyDocument.GetSaveFlag()
                Catch
                    'Unknown pre-rebuild state is treated as dirty so close protection remains conservative.
                    tracker.WasDirtyAtOuterBegin = True
                End Try

                tracker.SawGenericModifyNotify = False
            End If

            tracker.Depth += 1
            tracker.LastBeginUtc = DateTime.UtcNow
        End SyncLock
    End Sub

    Public Sub endAssemblyRebuildPublic(ByVal assemblyDocument As ModelDoc2)
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub
        Dim completedCleanStartRebuild As Boolean = False
        Dim needsPostRebuildModifyAllowance As Boolean = False

        SyncLock assemblyGuardSync
            Dim tracker As AssemblyRebuildTracker = Nothing
            If Not assemblyRebuildPaths.TryGetValue(assemblyKey, tracker) OrElse tracker Is Nothing Then Exit Sub

            If tracker.Depth <= 1 Then
                completedCleanStartRebuild = Not tracker.WasDirtyAtOuterBegin
                needsPostRebuildModifyAllowance = completedCleanStartRebuild AndAlso Not tracker.SawGenericModifyNotify
                assemblyRebuildPaths.Remove(assemblyKey)

                If needsPostRebuildModifyAllowance Then
                    completedAssemblyRebuildModifyUtcByPath(assemblyKey) = DateTime.UtcNow
                Else
                    completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)
                End If
            Else
                tracker.Depth -= 1
            End If
        End SyncLock

        If needsPostRebuildModifyAllowance Then
            queueAssemblyRebuildGenericModifyAllowanceExpiry(assemblyKey)
        End If

        If Not completedCleanStartRebuild Then Exit Sub

        'Preserve the original rebuild workflow without reviving the unsafe blanket rule
        'that treated every read-only dirty document as harmless. This candidate is created
        'only when a previously clean, unlocked/read-only assembly became dirty inside an
        'actual RegenNotify/RegenPostNotify bracket. Close still rechecks local SVN cleanliness.
        Try
            Dim assemblyPath As String = assemblyDocument.GetPathName()
            If String.IsNullOrWhiteSpace(assemblyPath) Then Exit Sub
            If Not isPathInsideLocalRepo(assemblyPath) OrElse Not File.Exists(assemblyPath) Then Exit Sub
            If assemblyHasRequiredLockFast(assemblyDocument) Then Exit Sub
            If (File.GetAttributes(assemblyPath) And FileAttributes.ReadOnly) <> FileAttributes.ReadOnly Then Exit Sub
            If Not assemblyDocument.GetSaveFlag() Then Exit Sub

            markAssemblyGuardFalseDirtyCandidate(assemblyDocument)
        Catch
        End Try
    End Sub

    Private Sub queueAssemblyRebuildGenericModifyAllowanceExpiry(ByVal assemblyKey As String)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub

        Dim firstTurn As New MethodInvoker(
            Sub()
                Try
                    If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then
                        SyncLock assemblyGuardSync
                            completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)
                        End SyncLock
                        Exit Sub
                    End If

                    'Allow the native RegenPostNotify call stack and one subsequent SOLIDWORKS
                    'UI turn to deliver its paired ModifyNotify. A later human edit must never
                    'inherit a ten-second rebuild exception merely because no ModifyNotify came.
                    myUserControl.BeginInvoke(
                        New MethodInvoker(
                            Sub()
                                SyncLock assemblyGuardSync
                                    completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)
                                End SyncLock
                            End Sub
                        )
                    )
                Catch
                    SyncLock assemblyGuardSync
                        completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)
                    End SyncLock
                End Try
            End Sub
        )

        Try
            If myUserControl IsNot Nothing AndAlso
               Not myUserControl.IsDisposed AndAlso
               myUserControl.IsHandleCreated Then
                myUserControl.BeginInvoke(firstTurn)
            Else
                SyncLock assemblyGuardSync
                    completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)
                End SyncLock
            End If
        Catch
            SyncLock assemblyGuardSync
                completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)
            End SyncLock
        End Try
    End Sub

    Private Function consumeAssemblyRebuildGenericModifyAllowance(ByVal assemblyDocument As ModelDoc2) As Boolean
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Return False

        SyncLock assemblyGuardSync
            Dim activeTracker As AssemblyRebuildTracker = Nothing

            If assemblyRebuildPaths.TryGetValue(assemblyKey, activeTracker) AndAlso activeTracker IsNot Nothing Then
                activeTracker.SawGenericModifyNotify = True
                Return True
            End If

            Dim completedUtc As DateTime = DateTime.MinValue
            If Not completedAssemblyRebuildModifyUtcByPath.TryGetValue(assemblyKey, completedUtc) Then Return False

            completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)

            Dim ageSeconds As Double = (DateTime.UtcNow - completedUtc).TotalSeconds
            Return ageSeconds >= 0 AndAlso ageSeconds <= COMPLETED_REBUILD_MODIFY_GRACE_SECONDS
        End SyncLock
    End Function

    Private Sub clearAssemblyRebuildGenericModifyAllowance(ByVal assemblyDocument As ModelDoc2)
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub

        SyncLock assemblyGuardSync
            completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)
        End SyncLock
    End Sub

    Private Function getAssemblyEditTargetDocumentSafe(ByVal assemblyDocument As ModelDoc2) As ModelDoc2
        If assemblyDocument Is Nothing Then Return Nothing

        Try
            Dim swAssembly As AssemblyDoc = TryCast(assemblyDocument, AssemblyDoc)
            If swAssembly Is Nothing Then Return Nothing

            Dim editTarget As ModelDoc2 = TryCast(swAssembly.GetEditTarget(), ModelDoc2)
            If editTarget Is Nothing Then Return Nothing

            'At the top level of Edit Assembly, some SOLIDWORKS releases return the
            'assembly itself from GetEditTarget. That is not an in-context child edit.
            'Treating it as one makes every close state machine call EditAssembly forever
            'because the owner and target can never separate (ownerPath|ownerPath).
            If Object.ReferenceEquals(editTarget, assemblyDocument) Then Return Nothing

            Try
                If iSwApp IsNot Nothing AndAlso
                   iSwApp.IsSame(assemblyDocument, editTarget) = swObjectEquality.swObjectSame Then
                    Return Nothing
                End If
            Catch
            End Try

            Dim ownerPath As String = ""
            Dim targetPath As String = ""

            Try
                ownerPath = assemblyDocument.GetPathName()
            Catch
                ownerPath = ""
            End Try

            Try
                targetPath = editTarget.GetPathName()
            Catch
                targetPath = ""
            End Try

            If Not String.IsNullOrWhiteSpace(ownerPath) AndAlso
               Not String.IsNullOrWhiteSpace(targetPath) AndAlso
               pathsAreSame(ownerPath, targetPath) Then
                Return Nothing
            End If

            Return editTarget
        Catch
            Return Nothing
        End Try
    End Function

    Private Function getAssemblyPathKeySafe(ByVal assemblyDocument As ModelDoc2) As String
        If assemblyDocument Is Nothing Then Return ""

        Dim assemblyPath As String = ""

        Try
            assemblyPath = assemblyDocument.GetPathName()
        Catch
            assemblyPath = ""
        End Try

        If String.IsNullOrWhiteSpace(assemblyPath) Then
            Try
                assemblyPath = assemblyDocument.GetTitle()
            Catch
                assemblyPath = ""
            End Try
        End If

        If String.IsNullOrWhiteSpace(assemblyPath) Then Return ""

        Try
            Return Path.GetFullPath(assemblyPath)
        Catch
            Return assemblyPath
        End Try
    End Function

    Private Function getSelectedExternalPhysicalChildPathSafe(ByVal assemblyDocument As ModelDoc2) As String
        If assemblyDocument Is Nothing Then Return ""

        Dim assemblyPath As String = ""

        Try
            assemblyPath = assemblyDocument.GetPathName()
        Catch
            assemblyPath = ""
        End Try

        Dim selectionManager As SelectionMgr = Nothing

        Try
            selectionManager = TryCast(assemblyDocument.SelectionManager, SelectionMgr)
        Catch
            selectionManager = Nothing
        End Try

        If selectionManager Is Nothing Then Return ""

        Dim selectedCount As Integer = 0

        Try
            selectedCount = CInt(selectionManager.GetSelectedObjectCount2(-1))
        Catch
            selectedCount = 0
        End Try

        For index As Integer = 1 To selectedCount
            Dim selectedComponent As Component2 = Nothing

            Try
                selectedComponent = TryCast(selectionManager.GetSelectedObjectsComponent4(index, -1), Component2)
            Catch
                selectedComponent = Nothing
            End Try

            If selectedComponent Is Nothing Then Continue For

            Dim isVirtual As Boolean = False

            Try
                isVirtual = selectedComponent.IsVirtual
            Catch
                isVirtual = False
            End Try

            If isVirtual Then Continue For

            Dim childPath As String = ""

            Try
                childPath = selectedComponent.GetPathName()
            Catch
                childPath = ""
            End Try

            If String.IsNullOrWhiteSpace(childPath) Then Continue For
            If isSolidWorksTempOrVirtualPath(childPath) Then Continue For
            If Not isCadFilePath(childPath) Then Continue For
            If Not String.IsNullOrWhiteSpace(assemblyPath) AndAlso pathsAreSame(assemblyPath, childPath) Then Continue For

            Try
                Return Path.GetFullPath(childPath)
            Catch
                Return childPath
            End Try
        Next

        Return ""
    End Function

    Private Function selectionContainsDimensionSafe(ByVal assemblyDocument As ModelDoc2) As Boolean
        If assemblyDocument Is Nothing Then Return False

        Dim selectionManager As SelectionMgr = Nothing

        Try
            selectionManager = TryCast(assemblyDocument.SelectionManager, SelectionMgr)
        Catch
            selectionManager = Nothing
        End Try

        If selectionManager Is Nothing Then Return False

        Dim selectedCount As Integer = 0

        Try
            selectedCount = CInt(selectionManager.GetSelectedObjectCount2(-1))
        Catch
            selectedCount = 0
        End Try

        For index As Integer = 1 To selectedCount
            Try
                If CInt(selectionManager.GetSelectedObjectType3(index, -1)) = CInt(swSelectType_e.swSelDIMENSIONS) Then
                    Return True
                End If
            Catch
            End Try
        Next

        Return False
    End Function

    Private Function externalChildPathHasRequiredLockFast(ByVal childPath As String) As Boolean
        If String.IsNullOrWhiteSpace(childPath) Then Return False
        If Not isPathInsideLocalRepo(childPath) Then Return False

        Try
            Dim cached As SVNStatus.filePpty = Nothing

            If tryFindCachedStatusProperty(childPath, cached) Then
                Return cached.lock6 = "K" OrElse
                       cached.addDelChg1 = "?" OrElse
                       cached.addDelChg1 = "A"
            End If
        Catch
        End Try

        Dim hasLocalChanges As Boolean = False
        Dim hasLocalLockToken As Boolean = False
        Dim workingCopyState As Char = " "c
        Dim statusError As String = ""

        If tryGetLocalSvnChangeState(
            childPath,
            hasLocalChanges,
            statusError,
            hasLocalLockToken,
            workingCopyState) Then

            Return hasLocalLockToken OrElse
                   workingCopyState = "?"c OrElse
                   workingCopyState = "A"c
        End If

        Return False
    End Function

    Private Function cachedServerStatusProvesLockUnavailable(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Try
            Dim cached As SVNStatus.filePpty = Nothing
            If Not tryFindCachedStatusProperty(filePath, cached) Then Return False

            'SVN status -u lock column: O = other working copy, T = stolen token,
            'B = broken token. These server-aware states override a stale local K token.
            Return cached.lock6 = "O" OrElse cached.lock6 = "T" OrElse cached.lock6 = "B"
        Catch
            Return False
        End Try
    End Function

    Public Sub noteAssemblySelectionContextPublic(ByVal assemblyDocument As ModelDoc2)
        If assemblyDocument Is Nothing Then Exit Sub

        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub

        Dim childPath As String = getSelectedExternalPhysicalChildPathSafe(assemblyDocument)
        Dim isDimensionSelection As Boolean = selectionContainsDimensionSafe(assemblyDocument)

        SyncLock assemblyGuardSync
            'Only retain the fallback for a selected child-owned dimension. Ordinary selected
            'components must never bypass assembly protection for moves, mates, suppression,
            'display-state changes, or other assembly-owned edits.
            If String.IsNullOrWhiteSpace(childPath) OrElse Not isDimensionSelection Then
                assemblySelectionContextByAssemblyPath.Remove(assemblyKey)
                Exit Sub
            End If

            assemblySelectionContextByAssemblyPath(assemblyKey) = New AssemblySelectionContext With {
                .ChildPath = childPath,
                .CapturedUtc = DateTime.UtcNow,
                .IsDimensionSelection = True
            }
        End SyncLock
    End Sub

    Private Function getRecentSelectedExternalChildPath(ByVal assemblyDocument As ModelDoc2) As String
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Return ""

        SyncLock assemblyGuardSync
            Dim context As AssemblySelectionContext = Nothing

            If Not assemblySelectionContextByAssemblyPath.TryGetValue(assemblyKey, context) Then Return ""
            If context Is Nothing OrElse Not context.IsDimensionSelection Then Return ""

            'The dimension Modify dialog can remain open while the user enters a value.
            'This is selection-scoped rather than a general child-selection bypass.
            If (DateTime.UtcNow - context.CapturedUtc).TotalMinutes > 3.0 Then
                assemblySelectionContextByAssemblyPath.Remove(assemblyKey)
                Return ""
            End If

            Return context.ChildPath
        End SyncLock
    End Function

    Private Sub clearAssemblySelectionContext(ByVal assemblyDocument As ModelDoc2)
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub

        SyncLock assemblyGuardSync
            assemblySelectionContextByAssemblyPath.Remove(assemblyKey)
        End SyncLock
    End Sub

    Private Sub rememberInContextEditDirtyBaseline(ByVal assemblyDocument As ModelDoc2,
                                                    ByVal assemblyKey As String)
        If assemblyDocument Is Nothing OrElse String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub

        Dim wasDirty As Boolean = True

        Try
            wasDirty = assemblyDocument.GetSaveFlag()
        Catch
            wasDirty = True
        End Try

        SyncLock assemblyGuardSync
            pendingInContextDirtyBaselineByAssemblyPath(assemblyKey) =
                New InContextEditDirtyBaseline With {
                    .WasDirty = wasDirty,
                    .CapturedUtc = DateTime.UtcNow
                }
        End SyncLock
    End Sub

    Private Function consumeInContextEditDirtyBaseline(ByVal assemblyDocument As ModelDoc2,
                                                        ByVal assemblyKey As String) As Boolean
        Dim wasDirty As Boolean = True

        Try
            wasDirty = assemblyDocument.GetSaveFlag()
        Catch
            wasDirty = True
        End Try

        SyncLock assemblyGuardSync
            Dim baseline As InContextEditDirtyBaseline = Nothing

            If pendingInContextDirtyBaselineByAssemblyPath.TryGetValue(assemblyKey, baseline) Then
                pendingInContextDirtyBaselineByAssemblyPath.Remove(assemblyKey)

                If baseline IsNot Nothing AndAlso
                   (DateTime.UtcNow - baseline.CapturedUtc).TotalSeconds <= 60.0 Then
                    Return baseline.WasDirty
                End If
            End If
        End SyncLock

        Return wasDirty
    End Function

    Public Sub noteInContextEditBeganPublic(ByVal assemblyDocument As ModelDoc2, ByVal editedDocument As ModelDoc2)
        Try
            Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
            If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub
            If editedDocument Is Nothing Then Exit Sub

            Dim childPath As String = ""
            Try
                childPath = editedDocument.GetPathName()
            Catch
                childPath = ""
            End Try

            If String.IsNullOrWhiteSpace(childPath) Then Exit Sub

            Dim lockPath As String = getInContextEffectiveLockPath(editedDocument, childPath)
            If String.IsNullOrWhiteSpace(lockPath) Then lockPath = childPath

            Dim assemblyWasDirtyBeforeEdit As Boolean =
                consumeInContextEditDirtyBaseline(assemblyDocument, assemblyKey)

            Dim editSession As New InContextEditSession With {
                .ChildPath = childPath,
                .LockPath = lockPath,
                .BeganUtc = DateTime.UtcNow,
                .EndedUtc = DateTime.MinValue,
                .AssemblyWasDirtyBeforeEdit = assemblyWasDirtyBeforeEdit
            }

            SyncLock assemblyGuardSync
                inContextEditSessionByAssemblyPath(assemblyKey) = editSession
            End SyncLock

            'Keep only the hosting parent temporarily writable while a properly locked child is
            'being edited in context. Register the session first because SetReadOnlyState(False)
            'can itself emit ModifyNotify; that notification must be classified as child-edit
            'bookkeeping rather than a top-assembly edit. The parent is never saved and is
            'restored after EndInContextEditNotify.
            If inContextEditTargetHasRequiredLock(editedDocument, lockPath) Then
                prepareInContextParentForCleanExit(assemblyDocument, assemblyKey, editSession)
            End If

            'Create the candidate at Begin as well as End. If SOLIDWORKS loses the matching End
            'event or the user closes while still editing the child, the parent bookkeeping
            'SaveFlag is still classified correctly. Any real assembly-owned edit clears it.
            If Not assemblyWasDirtyBeforeEdit Then
                markAssemblyGuardFalseDirtyCandidate(assemblyDocument)
            End If

            'The child being edited in-context is never ActiveDoc while the parent assembly
            'window has focus, so it would otherwise wait on the next broad status refresh to
            'get flipped writable. Reconcile it immediately.
            reconcileWriteAccessForActiveDocumentPublic()

            enforceInContextEditRequiresLock(
                assemblyDocument,
                editedDocument,
                assemblyKey,
                childPath,
                lockPath
            )
        Catch
        End Try
    End Sub

    Private Sub prepareInContextParentForCleanExit(ByVal assemblyDocument As ModelDoc2,
                                                    ByVal assemblyPath As String,
                                                    ByVal editSession As InContextEditSession)
        If assemblyDocument Is Nothing OrElse editSession Is Nothing Then Exit Sub
        If String.IsNullOrWhiteSpace(assemblyPath) OrElse Not File.Exists(assemblyPath) Then Exit Sub
        If Not isPathInsideLocalRepo(assemblyPath) Then Exit Sub

        'A locked parent is already legitimately writable and must keep its current state.
        If userHasLocalSvnLockTokenForPath(assemblyPath, allowCachedToken:=False) Then Exit Sub

        Try
            editSession.ParentWasReadOnly = assemblyDocument.IsOpenedReadOnly()
        Catch
            editSession.ParentWasReadOnly = False
        End Try

        If Not editSession.ParentWasReadOnly Then Exit Sub

        Try
            editSession.ParentOriginalAttributes = File.GetAttributes(assemblyPath)
            editSession.ParentHadOriginalAttributes = True
        Catch
            editSession.ParentHadOriginalAttributes = False
        End Try

        Try
            If editSession.ParentHadOriginalAttributes Then
                File.SetAttributes(
                    assemblyPath,
                    editSession.ParentOriginalAttributes And Not FileAttributes.ReadOnly
                )
            End If

            If Not assemblyDocument.SetReadOnlyState(False) Then
                Throw New InvalidOperationException(
                    "SOLIDWORKS would not temporarily release the hosting assembly's read-only state."
                )
            End If

            'Only SOLIDWORKS' in-memory access state needs to remain writable for the eventual
            'context exit. Put the needs-lock file attribute back immediately so a SOLIDWORKS
            'crash during a long Edit Part session cannot leave the unlocked parent writable
            'on disk for the next launch.
            If editSession.ParentHadOriginalAttributes AndAlso File.Exists(assemblyPath) Then
                File.SetAttributes(assemblyPath, editSession.ParentOriginalAttributes)
            End If

            editSession.ParentTemporarilyWritable = True
            writeOperationLog(
                "Hosting assembly temporarily writable for locked child edit session: " & assemblyPath
            )
        Catch ex As Exception
            Try
                assemblyDocument.SetReadOnlyState(True)
            Catch
            End Try

            If editSession.ParentHadOriginalAttributes AndAlso File.Exists(assemblyPath) Then
                Try
                    File.SetAttributes(assemblyPath, editSession.ParentOriginalAttributes)
                Catch
                End Try
            End If

            editSession.ParentTemporarilyWritable = False
            writeOperationLog(
                "Could not prepare hosting assembly for a clean child-edit exit: " & ex.Message
            )
        End Try
    End Sub

    'BeginInContextEditNotify is a post-facto Notify - SOLIDWORKS has already entered edit
    'mode by the time it fires, unlike CommandOpenPreNotify for Ctrl+S which can cancel the
    'command outright. There is no cancellable pre-event for entering in-context edit, so this
    'cannot block the click itself. It can only detect an edit that started on a child with no
    'current SVN lock and immediately leave that edit context without saving either document.
    Private Sub enforceInContextEditRequiresLock(ByVal assemblyDocument As ModelDoc2,
                                                  ByVal editedDocument As ModelDoc2,
                                                  ByVal assemblyKey As String,
                                                  ByVal childPath As String,
                                                  ByVal lockPath As String)
        If assemblyEditGuardSuppressed(assemblyDocument) Then Exit Sub
        If inContextEditTargetHasRequiredLock(editedDocument, lockPath) Then Exit Sub

        'An already-loaded child can enter Edit Part without either CommandOpenPreNotify or
        'FileOpenPreNotify firing. Never leave that child editable while a pending network lock
        'might still fail because another user owns it. The normal command-pre route defers and
        'replays the edit automatically; this post-facto fallback exits promptly unless the K
        'token becomes authoritative before the deferred check runs.
        Dim lockWasPendingAtNotify As Boolean =
            asyncGetLocksInProgress AndAlso asyncGetLocksIncludesPath(lockPath)

        'If this edit raced an explicit Get Locks click, let that one authoritative request
        'finish. Keep the edit only when the local K token is then present; if the lock fails
        '(including another user owning it), the waiter exits the edit context safely.
        If lockWasPendingAtNotify AndAlso
           waitForCurrentGetLocksBeforeEnforcingInContextEdit(
               assemblyDocument,
                editedDocument,
                assemblyKey,
                childPath,
                lockPath
            ) Then Exit Sub

        Dim kickOutAction As New System.Windows.Forms.MethodInvoker(
            Sub()
                Try
                    'Re-verify against live state before acting - the lock may have been
                    'acquired, or the edit session may have already ended naturally, in the
                    'brief window between the notify firing and this deferred action running.
                    Dim currentSession As InContextEditSession = Nothing

                    SyncLock assemblyGuardSync
                        If Not inContextEditSessionByAssemblyPath.TryGetValue(assemblyKey, currentSession) Then Exit Sub
                        If currentSession Is Nothing Then Exit Sub
                        If currentSession.EndedUtc <> DateTime.MinValue Then Exit Sub
                        If Not pathsAreSame(currentSession.ChildPath, childPath) Then Exit Sub
                    End SyncLock

                    Dim currentChild As ModelDoc2 = getOpenModelByPathSafe(childPath)
                    If currentChild Is Nothing Then currentChild = editedDocument
                    If inContextEditTargetHasRequiredLock(currentChild, lockPath) Then Exit Sub

                    Dim currentAssembly As ModelDoc2 = getOpenModelByPathSafe(assemblyKey)
                    If currentAssembly Is Nothing Then currentAssembly = assemblyDocument
                    If currentAssembly Is Nothing Then Exit Sub

                    'Leave the unauthorized context without invoking Undo. This helper makes the
                    'parent writable only for the context transition and never saves it.
                    exitAssemblyInContextEditWithoutSavingParent(currentAssembly)

                    writeOperationLog("In-context edit backed out - child not locked: " & childPath)

                    Dim childName As String = childPath
                    Try
                        childName = Path.GetFileName(childPath)
                    Catch
                    End Try

                    'This is the fallback for edit entry points SOLIDWORKS does not expose through
                    'CommandOpenPreNotify. Never start a network lock request from an Edit command:
                    'selection can change while it runs, producing delayed replay and parent/child
                    'confusion. Back out safely and require the explicit Get Locks action instead.
                    If lockWasPendingAtNotify Then
                        iSwApp.SendMsgToUser2(
                            "Get Locks is still finishing for " & childName & "." & vbCrLf & vbCrLf &
                            "PlumVault left Edit Part/Edit Assembly so the file could not be changed before the lock result was known." & vbCrLf & vbCrLf &
                            "If the lock succeeds, select the item and start the edit again.",
                            swMessageBoxIcon_e.swMbInformation,
                            swMessageBoxBtn_e.swMbOk
                        )
                    Else
                        showManualLockRequired(lockPath, "start Edit Part or Edit Assembly")
                    End If
                    writeOperationLog(
                        "Post-notify in-context edit blocked; manual Get Locks required: " & childName
                    )
                Catch
                End Try
            End Sub
        )

        Try
            If myUserControl IsNot Nothing AndAlso
               Not myUserControl.IsDisposed AndAlso
               myUserControl.IsHandleCreated Then

                myUserControl.BeginInvoke(kickOutAction)
            End If
        Catch
        End Try
    End Sub

    Private Function assemblyHasActiveInContextEdit(ByVal assemblyDocument As ModelDoc2) As Boolean
        If assemblyDocument Is Nothing Then Return False

        Dim editTarget As ModelDoc2 = getAssemblyEditTargetDocumentSafe(assemblyDocument)
        If editTarget IsNot Nothing Then
            Try
                Dim ownerPath As String = assemblyDocument.GetPathName()
                Dim targetPath As String = editTarget.GetPathName()

                If Not String.IsNullOrWhiteSpace(targetPath) AndAlso
                   (String.IsNullOrWhiteSpace(ownerPath) OrElse Not pathsAreSame(ownerPath, targetPath)) Then
                    Dim selectedChildPath As String = getSelectedExternalPhysicalChildPathSafe(assemblyDocument)

                    'While editing a subassembly, the user can select a deeper child and invoke
                    'Edit Part again. That is a drill-down request, not the toggle that exits the
                    'current context; let the normal child-lock pipeline handle it.
                    If Not String.IsNullOrWhiteSpace(selectedChildPath) AndAlso
                       Not pathsAreSame(selectedChildPath, targetPath) Then Return False

                    Return True
                End If
            Catch
                Return True
            End Try
        End If

        'Command interception must use the live edit target. The tracked event session is
        'allowed to protect delayed ModifyNotify events, but a lost End notification must
        'never turn a later, unrelated Edit Part click into an "exit" command.
        Return False
    End Function

    Private Function queueReadOnlyParentInContextExit(ByVal assemblyDocument As ModelDoc2) As Boolean
        If assemblyDocument Is Nothing OrElse myUserControl Is Nothing Then Return False
        If inContextExitTransitionInProgress Then Return True

        Dim isReadOnly As Boolean = False

        Try
            isReadOnly = assemblyDocument.IsOpenedReadOnly()
        Catch
            isReadOnly = False
        End Try

        'A writable parent can use the native toggle without any intervention.
        If Not isReadOnly Then Return False

        Dim assemblyPath As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyPath) Then Return False

        SyncLock assemblyGuardSync
            If inContextExitTransitionQueuedPaths.Contains(assemblyPath) Then Return True
            inContextExitTransitionQueuedPaths.Add(assemblyPath)
        End SyncLock

        Try
            myUserControl.BeginInvoke(
                New MethodInvoker(
                    Sub()
                        Try
                            Dim currentAssembly As ModelDoc2 = getOpenModelByPathSafe(assemblyPath)
                            If currentAssembly Is Nothing Then currentAssembly = assemblyDocument
                            If currentAssembly Is Nothing OrElse Not assemblyHasActiveInContextEdit(currentAssembly) Then Exit Sub

                            exitAssemblyInContextEditWithoutSavingParent(currentAssembly)
                        Catch ex As Exception
                            Try
                                iSwApp.SendMsgToUser2(
                                    "Could not safely leave Edit Part/Edit Assembly mode." & vbCrLf & vbCrLf & ex.Message,
                                    swMessageBoxIcon_e.swMbWarning,
                                    swMessageBoxBtn_e.swMbOk
                                )
                            Catch
                            End Try
                        Finally
                            SyncLock assemblyGuardSync
                                inContextExitTransitionQueuedPaths.Remove(assemblyPath)
                            End SyncLock
                        End Try
                    End Sub
                )
            )
            Return True
        Catch
            SyncLock assemblyGuardSync
                inContextExitTransitionQueuedPaths.Remove(assemblyPath)
            End SyncLock
            Return False
        End Try
    End Function

    Private Sub exitAssemblyInContextEditWithoutSavingParent(ByVal assemblyDocument As ModelDoc2)
        If assemblyDocument Is Nothing Then Exit Sub

        Dim assemblyDoc As AssemblyDoc = TryCast(assemblyDocument, AssemblyDoc)
        If assemblyDoc Is Nothing Then Exit Sub

        Dim assemblyPath As String = getAssemblyPathKeySafe(assemblyDocument)
        Dim parentWasDirty As Boolean = True
        Dim parentWasReadOnly As Boolean = False
        Dim originalAttributes As FileAttributes = FileAttributes.Normal
        Dim haveOriginalAttributes As Boolean = False
        Dim parentHasLock As Boolean = False

        Try
            parentWasDirty = assemblyDocument.GetSaveFlag()
        Catch
            parentWasDirty = True
        End Try

        Try
            parentWasReadOnly = assemblyDocument.IsOpenedReadOnly()
        Catch
            parentWasReadOnly = False
        End Try

        If Not String.IsNullOrWhiteSpace(assemblyPath) AndAlso File.Exists(assemblyPath) Then
            Try
                originalAttributes = File.GetAttributes(assemblyPath)
                haveOriginalAttributes = True
            Catch
            End Try

            parentHasLock = userHasLocalSvnLockTokenForPath(assemblyPath, allowCachedToken:=False)
        End If

        inContextExitTransitionInProgress = True
        Dim restoreReadOnlyDeferred As Boolean = False

        Try
            If parentWasReadOnly Then
                If haveOriginalAttributes Then
                    File.SetAttributes(assemblyPath, originalAttributes And Not FileAttributes.ReadOnly)
                End If

                If Not assemblyDocument.SetReadOnlyState(False) Then
                    Throw New InvalidOperationException("SOLIDWORKS would not temporarily release the parent document's read-only state.")
                End If
            End If

            'This changes only the active editing context. PlumVault never saves the parent
            'during this transition. SOLIDWORKS completes the context change on its next UI
            'turn, so an unlocked parent must remain temporarily writable until that turn.
            assemblyDoc.EditAssembly()

            If parentWasReadOnly AndAlso Not parentHasLock Then
                Dim restoreAction As New MethodInvoker(
                    Sub()
                        restoreReadOnlyAfterInContextExit(
                            assemblyPath,
                            assemblyDocument,
                            originalAttributes,
                            haveOriginalAttributes,
                            0
                        )
                    End Sub
                )

                If myUserControl IsNot Nothing AndAlso
                   Not myUserControl.IsDisposed AndAlso
                   myUserControl.IsHandleCreated Then
                    myUserControl.BeginInvoke(restoreAction)
                    restoreReadOnlyDeferred = True
                End If
            End If
        Catch
            'If the native transition itself fails, restore the original access state before
            'letting the caller report the failure.
            If parentWasReadOnly AndAlso Not parentHasLock Then
                Try
                    assemblyDocument.SetReadOnlyState(True)
                Catch
                End Try

                If haveOriginalAttributes AndAlso File.Exists(assemblyPath) Then
                    Try
                        File.SetAttributes(assemblyPath, originalAttributes Or FileAttributes.ReadOnly)
                    Catch
                    End Try
                End If
            End If

            Throw
        Finally
            If Not restoreReadOnlyDeferred Then inContextExitTransitionInProgress = False
        End Try

        If Not parentWasDirty Then
            markAssemblyGuardFalseDirtyCandidate(assemblyDocument)
        End If

        writeOperationLog("Exited in-context edit without saving read-only parent: " & assemblyPath)
    End Sub

    Private Sub restoreReadOnlyAfterInContextExit(ByVal assemblyPath As String,
                                                   ByVal fallbackAssembly As ModelDoc2,
                                                   ByVal originalAttributes As FileAttributes,
                                                   ByVal haveOriginalAttributes As Boolean,
                                                   ByVal attempt As Integer,
                                                   Optional ByVal clearTransitionFlag As Boolean = True,
                                                   Optional ByVal restoreExactDiskAttributes As Boolean = False)
        Dim restorationFinished As Boolean = False
        Try
            Dim currentAssembly As ModelDoc2 = getOpenModelByPathSafe(assemblyPath)
            If currentAssembly Is Nothing Then currentAssembly = fallbackAssembly

            'EditAssembly is asynchronous. Restoring read-only on the very next UI turn can
            'race the native context exit and produce the repeated "top level is read-only"
            'prompt. Keep the temporary access only until GetEditTarget confirms the exit.
            If currentAssembly IsNot Nothing AndAlso
               assemblyHasActiveInContextEdit(currentAssembly) AndAlso
               attempt < 24 AndAlso
               myUserControl IsNot Nothing AndAlso
               Not myUserControl.IsDisposed AndAlso
               myUserControl.IsHandleCreated Then

                myUserControl.BeginInvoke(
                    New MethodInvoker(
                        Sub()
                            restoreReadOnlyAfterInContextExit(
                                assemblyPath,
                                fallbackAssembly,
                                originalAttributes,
                                haveOriginalAttributes,
                                attempt + 1,
                                clearTransitionFlag,
                                restoreExactDiskAttributes
                            )
                        End Sub
                    )
                )
                Exit Sub
            End If

            restorationFinished = True

            If currentAssembly IsNot Nothing Then
                Try
                    currentAssembly.SetReadOnlyState(True)
                Catch
                End Try
            End If

            If haveOriginalAttributes AndAlso File.Exists(assemblyPath) Then
                Try
                    If restoreExactDiskAttributes Then
                        File.SetAttributes(assemblyPath, originalAttributes)
                    Else
                        File.SetAttributes(assemblyPath, originalAttributes Or FileAttributes.ReadOnly)
                    End If
                Catch
                End Try
            End If

            writeOperationLog("Restored hosting assembly read-only state: " & assemblyPath)
        Catch ex As Exception
            restorationFinished = True
            writeOperationLog("Could not restore read-only after in-context exit: " & ex.Message)
        Finally
            If restorationFinished AndAlso clearTransitionFlag Then inContextExitTransitionInProgress = False
        End Try
    End Sub

    Public Function handleSolidWorksCommandOpenPreNotifyPublic(ByVal command As Integer,
                                                                 ByVal userCommand As Integer) As Integer
        If Not isAssemblySuppressionCommand(command) Then
            SyncLock assemblyGuardSync
                pendingAssemblySuppressionState = Nothing
            End SyncLock
        End If

        If command = SW_COMMAND_INSERT_COMPONENTS Then
            Dim insertResult As Integer = handleInsertComponentOnUnsavedAssemblyPreNotify()
            If insertResult <> 0 Then Return insertResult
        End If

        If command = SW_COMMAND_DELETE Then
            Dim deleteResult As Integer = blockSelectedCadDestructiveEditPrePublic(
                TryCast(iSwApp.ActiveDoc, ModelDoc2),
                "deleting the selected item"
            )
            If deleteResult <> 0 Then Return -1
        End If

        If isAssemblySuppressionCommand(command) Then
            Dim suppressionResult As Integer = handleAssemblySuppressionCommandPreNotify(command)
            If suppressionResult <> 0 Then Return suppressionResult
        End If

        If command = SW_COMMAND_INSERT_FEATURE_FOLDER OrElse
           command = SW_COMMAND_CREATE_EMPTY_FEATURE_FOLDER OrElse
           command = SW_COMMAND_EDIT_FEATURE_FOLDER Then
            Dim activeAssembly As ModelDoc2 = Nothing

            Try
                activeAssembly = TryCast(iSwApp.ActiveDoc, ModelDoc2)
                If activeAssembly IsNot Nothing AndAlso
                   activeAssembly.GetType() = swDocumentTypes_e.swDocASSEMBLY Then
                    Return blockAssemblyOwnedEditPrePublic(activeAssembly, "changing FeatureManager folders")
                End If
            Catch
            End Try
        End If

        Dim editResult As Integer = handleInContextEditCommandPreNotify(command)
        If editResult <> 0 Then Return editResult

        Return handleSolidWorksSaveCommandPreNotifyPublic(command, userCommand)
    End Function

    Public Function handleSolidWorksFileOpenPreNotifyPublic(ByVal fileName As String) As Integer
        If String.IsNullOrWhiteSpace(fileName) Then Return 0
        If Not asyncGetLocksInProgress OrElse Not asyncGetLocksIncludesPath(fileName) Then Return 0
        If iSwApp Is Nothing Then Return 0

        Dim activeAssembly As ModelDoc2 = Nothing

        Try
            activeAssembly = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeAssembly Is Nothing OrElse
               activeAssembly.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return 0
        Catch
            Return 0
        End Try

        'Some SOLIDWORKS tree/context-menu routes do not emit the Edit Part command pre-event.
        'They begin loading the selected child and then queue the native read-only warning. The
        'document-open pre-event is still cancellable, so pause only when this exact selected
        'child already has a user-started Get Locks request in flight. Replaying remains subject
        'to the same live K-token, active-document, and unchanged-selection checks as command
        'interception. A lock conflict therefore never opens or edits the child.
        Dim selectedChildPath As String = getSelectedExternalPhysicalChildPathSafe(activeAssembly)
        If Not pathsAreSame(selectedChildPath, fileName) Then Return 0

        Dim assemblyPath As String = getAssemblyPathKeySafe(activeAssembly)
        If String.IsNullOrWhiteSpace(assemblyPath) Then Return 0

        If rememberEditReplayForCurrentGetLocks(assemblyPath, fileName, SW_COMMAND_EDIT_PART) Then
            writeOperationLog(
                "Child file open deferred until current Get Locks completes: " & fileName
            )
        Else
            writeOperationLog(
                "Child file open cancelled while another deferred edit is already pending: " & fileName
            )
        End If

        'Per SOLIDWORKS FileOpenPreNotify semantics, any non-zero result cancels this premature
        'open. The verified edit replay opens the child normally after the lock succeeds.
        Return 1
    End Function

    Public Function handleSolidWorksCommandCloseNotifyPublic(ByVal command As Integer,
                                                               ByVal reason As Integer) As Integer
        If Not isAssemblySuppressionCommand(command) Then Return 0

        Dim openedUtc As DateTime = DateTime.MinValue

        SyncLock assemblyGuardSync
            If pendingAssemblySuppressionState IsNot Nothing Then
                pendingAssemblySuppressionState.ClosedUtc = DateTime.UtcNow
                If Not pendingAssemblySuppressionState.ExpiryQueued Then
                    pendingAssemblySuppressionState.ExpiryQueued = True
                    openedUtc = pendingAssemblySuppressionState.OpenedUtc
                End If
            End If
        End SyncLock

        If openedUtc <> DateTime.MinValue Then queuePendingAssemblySuppressionExpiry(openedUtc)

        Return 0
    End Function

    Private Function isAssemblySuppressionCommand(ByVal command As Integer) As Boolean
        Select Case command
            Case SW_COMMAND_SUPPRESS,
                 SW_COMMAND_UNSUPPRESS,
                 SW_COMMAND_UNSUPPRESS_WITH_DEPENDENTS,
                 SW_COMMAND_CHANGE_SUPPRESSION_STATE,
                 SW_COMMAND_MAKE_SUPPRESSED,
                 SW_COMMAND_SUPPRESS_ALL_CONFIGS,
                 SW_COMMAND_SUPPRESS_SELECTED_CONFIGS,
                 SW_COMMAND_UNSUPPRESS_ALL_CONFIGS,
                 SW_COMMAND_UNSUPPRESS_SELECTED_CONFIGS,
                 SW_COMMAND_UNSUPPRESS_DEPENDENT_ALL_CONFIGS,
                 SW_COMMAND_UNSUPPRESS_DEPENDENT_SELECTED_CONFIGS,
                 SW_COMMAND_SUPPRESS_FEATURE,
                 SW_COMMAND_UNSUPPRESS_FEATURE
                Return True
        End Select

        Return False
    End Function

    Private Function handleAssemblySuppressionCommandPreNotify(ByVal command As Integer) As Integer
        If iSwApp Is Nothing Then Return 0

        Dim activeAssembly As ModelDoc2 = Nothing

        Try
            activeAssembly = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeAssembly Is Nothing OrElse
               activeAssembly.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return 0
        Catch
            Return 0
        End Try

        'This is deliberately a live selection lookup. CommandOpenPreNotify is the last point
        'before SOLIDWORKS replaces MirrorComponent1 with one of its generated components and
        'starts broadcasting state changes on every ancestor assembly.
        Dim ownerDocument As ModelDoc2 = getLiveSelectedSuppressionOwnerDocument(activeAssembly)
        If ownerDocument Is Nothing Then
            ownerDocument = getRecentNestedFeatureOwnerDocument(activeAssembly)
        End If
        If ownerDocument Is Nothing Then
            ownerDocument = getSelectedAssemblyEditOwnerPublic(activeAssembly)
        End If
        If ownerDocument Is Nothing Then ownerDocument = activeAssembly

        Dim ownerIsPart As Boolean = False
        Try
            ownerIsPart = ownerDocument.GetType() = swDocumentTypes_e.swDocPART
        Catch
            ownerIsPart = False
        End Try

        Dim actionDescription As String = If(
            command = SW_COMMAND_UNSUPPRESS OrElse
            command = SW_COMMAND_UNSUPPRESS_WITH_DEPENDENTS OrElse
            command = SW_COMMAND_UNSUPPRESS_ALL_CONFIGS OrElse
            command = SW_COMMAND_UNSUPPRESS_SELECTED_CONFIGS OrElse
            command = SW_COMMAND_UNSUPPRESS_DEPENDENT_ALL_CONFIGS OrElse
            command = SW_COMMAND_UNSUPPRESS_DEPENDENT_SELECTED_CONFIGS OrElse
            command = SW_COMMAND_UNSUPPRESS_FEATURE,
            If(ownerIsPart, "unsuppressing the selected part feature", "unsuppressing an assembly feature or component"),
            If(ownerIsPart, "suppressing the selected part feature", "suppressing an assembly feature or component")
        )

        Dim blockResult As Integer = 0

        If ownerIsPart Then
            If Not assemblyHasRequiredLockFast(ownerDocument) Then
                showManualLockRequired(getAssemblyPathKeySafe(ownerDocument), actionDescription)
                blockResult = 1
            End If
        Else
            blockResult = blockAssemblyOwnedEditPrePublic(ownerDocument, actionDescription)
        End If

        If blockResult <> 0 Then
            SyncLock assemblyGuardSync
                pendingAssemblySuppressionState = Nothing
            End SyncLock
            Return blockResult
        End If

        Dim activePath As String = getAssemblyPathKeySafe(activeAssembly)
        Dim ownerPath As String = getAssemblyPathKeySafe(ownerDocument)

        If String.IsNullOrWhiteSpace(activePath) OrElse String.IsNullOrWhiteSpace(ownerPath) Then Return 0

        Dim openedUtc As DateTime = DateTime.UtcNow

        SyncLock assemblyGuardSync
            pendingAssemblySuppressionState = New PendingAssemblySuppressionCommand With {
                .ActiveAssemblyPath = activePath,
                .OwnerAssemblyPath = ownerPath,
                .Command = command,
                .OpenedUtc = openedUtc,
                .ClosedUtc = DateTime.MinValue,
                .ExpiryQueued = False
            }
        End SyncLock

        Dim writableFailure As String = ""
        If Not ensureOpenCadDocumentWritableNow(ownerPath, ownerDocument, writableFailure) Then
            SyncLock assemblyGuardSync
                pendingAssemblySuppressionState = Nothing
            End SyncLock

            Try
                iSwApp.SendMsgToUser2(
                    "Suppress/Unsuppress was paused because PlumVault could not give the locked file write access." & vbCrLf & vbCrLf &
                    Path.GetFileName(ownerPath) & vbCrLf & vbCrLf &
                    writableFailure & vbCrLf & vbCrLf &
                    "Click Sync and try again. The feature was not changed.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            Return 1
        End If

        writeOperationLog(
            "Suppression command owner captured: " & Path.GetFileName(ownerPath) &
            " from " & Path.GetFileName(activePath) & " (command " & command.ToString() & ")"
        )

        Return 0
    End Function

    'ModelDoc2.SetReadOnlyState(False) internally rebuilds the document, which on a
    'virtual/STEP-import-heavy assembly has been measured at 5+ minutes of blocked UI while a
    'normal part takes ~2 seconds. There is no safe way to predict this in advance (traversing
    'the component tree to "detect" such assemblies is itself what froze document open in an
    'earlier attempt), so the transition is measured whenever it actually runs and any file
    'that ever proves pathologically slow is remembered - in memory and on disk, so the
    'knowledge survives restarts. Pre-emptive/background transitions (Get Locks completion,
    'window-switch reconciliation) skip known-slow files entirely; the explicit edit/save
    'precheck still transitions them synchronously because there the user has explicitly
    'chosen to edit that exact document and the one-time wait is unavoidable.
    Private Const SLOW_WRITABLE_TRANSITION_THRESHOLD_MS As Long = 5000
    Private ReadOnly slowWritableTransitionSync As New Object()
    Private slowWritableTransitionPaths As HashSet(Of String) = Nothing

    Private Function getSlowWritableTransitionStorePath() As String
        Try
            Return Path.Combine(
                System.Environment.GetFolderPath(System.Environment.SpecialFolder.LocalApplicationData),
                "PlumVault",
                "PlumVault_SlowWritableTransitions.txt"
            )
        Catch
            Return ""
        End Try
    End Function

    Private Sub ensureSlowWritableTransitionPathsLoaded()
        'Callers hold slowWritableTransitionSync.
        If slowWritableTransitionPaths IsNot Nothing Then Exit Sub

        slowWritableTransitionPaths = New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            Dim storePath As String = getSlowWritableTransitionStorePath()
            If String.IsNullOrWhiteSpace(storePath) OrElse Not File.Exists(storePath) Then Exit Sub

            For Each line As String In File.ReadAllLines(storePath)
                If Not String.IsNullOrWhiteSpace(line) Then slowWritableTransitionPaths.Add(line.Trim())
            Next
        Catch
            'A missing/unreadable store only loses the cross-session memory; behavior falls
            'back to measuring again, never to anything unsafe.
        End Try
    End Sub

    Public Function isKnownSlowWritableTransitionPathPublic(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim key As String = normalizeFullPathSafe(filePath)

        SyncLock slowWritableTransitionSync
            ensureSlowWritableTransitionPathsLoaded()
            Return slowWritableTransitionPaths.Contains(key)
        End SyncLock
    End Function

    'Assemblies observed (during ordinary task-pane tree building, which already inspects
    'Component2.IsVirtual per child at zero added cost) to contain virtual/imported embedded
    'components. Their native writable transition is presumed pathological BEFORE it has ever
    'been paid once, unlike the measured store above which only learns after the fact.
    Private ReadOnly virtualContainingAssemblyPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

    Public Sub noteAssemblyContainsVirtualComponentsPublic(ByVal assemblyPath As String)
        If String.IsNullOrWhiteSpace(assemblyPath) Then Exit Sub

        Dim key As String = normalizeFullPathSafe(assemblyPath)

        SyncLock slowWritableTransitionSync
            If Not virtualContainingAssemblyPaths.Add(key) Then Exit Sub
        End SyncLock

        writeOperationLog("Assembly contains virtual/imported components; background writable transitions disabled: " & key)
    End Sub

    'True when a background (non-user-initiated) SetReadOnlyState transition must be skipped:
    'measured-slow files, assemblies known to embed virtual/imported components, and
    'imported neutral-format/temp/virtual file names. Explicit edit/save prechecks do NOT use
    'this - the user chose that exact document, so the transition runs there regardless.
    Public Function shouldSkipBackgroundWritableTransitionPublic(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        If isKnownSlowWritableTransitionPathPublic(filePath) Then Return True

        Dim key As String = normalizeFullPathSafe(filePath)

        SyncLock slowWritableTransitionSync
            If virtualContainingAssemblyPaths.Contains(key) Then Return True
        End SyncLock

        Try
            If isSolidWorksTempOrVirtualPath(filePath) Then Return True

            '"Name.stp.SLDPRT"-style imported vendor files carry dumb translated geometry
            'whose rebuild cost is unpredictable; never transition them in the background.
            Dim innerExt As String = Path.GetExtension(Path.GetFileNameWithoutExtension(filePath))
            If Not String.IsNullOrWhiteSpace(innerExt) Then
                For Each candidate As String In neutralCadImportExtensions
                    If String.Equals(innerExt, candidate, StringComparison.OrdinalIgnoreCase) Then Return True
                Next
            End If
        Catch
        End Try

        Return False
    End Function

    Public Sub noteWritableTransitionDurationPublic(ByVal filePath As String, ByVal elapsedMs As Long)
        If String.IsNullOrWhiteSpace(filePath) Then Exit Sub

        If elapsedMs < SLOW_WRITABLE_TRANSITION_THRESHOLD_MS Then
            'Self-un-poison: a one-off system hitch (antivirus scan, disk wake) can mark an
            'ordinary file slow forever, since the persisted store has no other removal path.
            'A later transition proving fast removes the stale entry so background
            'reconciliation resumes normally for that file.
            Dim fastKey As String = normalizeFullPathSafe(filePath)

            SyncLock slowWritableTransitionSync
                ensureSlowWritableTransitionPathsLoaded()
                If Not slowWritableTransitionPaths.Remove(fastKey) Then Exit Sub

                Try
                    Dim fastStorePath As String = getSlowWritableTransitionStorePath()
                    If Not String.IsNullOrWhiteSpace(fastStorePath) Then
                        Directory.CreateDirectory(Path.GetDirectoryName(fastStorePath))
                        File.WriteAllLines(fastStorePath, slowWritableTransitionPaths.ToArray())
                    End If
                Catch
                End Try
            End SyncLock

            writeOperationLog(
                "Slow writable transition record cleared after fast measurement (" &
                elapsedMs.ToString() & " ms): " & fastKey
            )
            Exit Sub
        End If

        Dim key As String = normalizeFullPathSafe(filePath)

        SyncLock slowWritableTransitionSync
            ensureSlowWritableTransitionPathsLoaded()
            If Not slowWritableTransitionPaths.Add(key) Then Exit Sub

            Try
                Dim storePath As String = getSlowWritableTransitionStorePath()
                If Not String.IsNullOrWhiteSpace(storePath) Then
                    Directory.CreateDirectory(Path.GetDirectoryName(storePath))
                    File.WriteAllLines(storePath, slowWritableTransitionPaths.ToArray())
                End If
            Catch
            End Try
        End SyncLock

        writeOperationLog(
            "Slow writable transition recorded (" & elapsedMs.ToString() & " ms); " &
            "future background transitions will skip: " & key
        )
    End Sub

    Private Function ensureOpenCadDocumentWritableNow(ByVal filePath As String,
                                                       ByVal openDocument As ModelDoc2,
                                                       ByRef failureReason As String) As Boolean
        failureReason = ""

        If String.IsNullOrWhiteSpace(filePath) Then
            failureReason = "The document path is unavailable."
            Return False
        End If

        Try
            If File.Exists(filePath) Then
                File.SetAttributes(filePath, File.GetAttributes(filePath) And Not FileAttributes.ReadOnly)

                If (File.GetAttributes(filePath) And FileAttributes.ReadOnly) <> 0 Then
                    failureReason = "The working-copy file remained read-only on disk."
                    Return False
                End If
            End If
        Catch ex As Exception
            failureReason = "The working-copy file could not be made writable: " & ex.Message
            Return False
        End Try

        If openDocument Is Nothing Then Return True

        Dim isReadOnly As Boolean

        Try
            isReadOnly = openDocument.IsOpenedReadOnly()
        Catch ex As Exception
            failureReason = "SOLIDWORKS could not report the document's write-access state: " & ex.Message
            Return False
        End Try

        If Not isReadOnly Then Return True

        Dim transitionWatch As Stopwatch = Stopwatch.StartNew()

        Try
            'This is intentionally synchronous and scoped to one explicit interaction target.
            'The user already owns its live SVN lock and is about to edit/save this exact file.
            'Broad writable reconciliation remains deferred because changing unrelated open
            'documents can trigger sibling rebuild and false-dirty cascades.
            openDocument.SetReadOnlyState(False)
        Catch ex As Exception
            noteWritableTransitionDurationPublic(filePath, transitionWatch.ElapsedMilliseconds)
            failureReason = "SOLIDWORKS could not release its internal read-only state: " & ex.Message
            Return False
        End Try

        'Remember pathologically slow transitions so background reconciliation never repeats
        'them for this file. This explicit path itself still runs when the user edits/saves.
        noteWritableTransitionDurationPublic(filePath, transitionWatch.ElapsedMilliseconds)

        Try
            If openDocument.IsOpenedReadOnly() Then
                failureReason = "SOLIDWORKS kept the document open for read-only access even though its SVN lock is held by you."
                Return False
            End If
        Catch ex As Exception
            failureReason = "SOLIDWORKS could not verify the writable state after changing it: " & ex.Message
            Return False
        End Try

        writeOperationLog("Immediate interaction write access completed: " & filePath)
        Return True
    End Function

    Public Function canRunQuietActiveServerStatusCheckPublic(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) OrElse Not File.Exists(filePath) Then Return False
        If Not isCadFilePath(filePath) OrElse Not isPathInsideLocalRepo(filePath) Then Return False

        SyncLock solidWorksNativeMutationSync
            If solidWorksNativeMutationInProgress Then Return False
        End SyncLock

        If automaticSaveEventsSuppressed() OrElse
           legacyImportInProgress OrElse
           cadRelocationInProgress OrElse
           closeReviewRevertInProgress OrElse
           closeGuardMessageShowing OrElse
           lockReviewMessageShowing OrElse
           asyncGetLocksInProgress OrElse
           asyncCommitInProgress OrElse
           asyncCleanupInProgress OrElse
           syncStatusInProgressOnControl() Then Return False

        'New/unversioned files have no repository lock to verify. Use only the existing cache
        'here; a quiet timer gate must never launch a local svn.exe process on the UI thread.
        Try
            Dim cached As SVNStatus.filePpty = Nothing
            If tryFindCachedStatusProperty(filePath, cached) AndAlso
               (cached.addDelChg1 = "?" OrElse cached.addDelChg1 = "A") Then Return False
        Catch
        End Try

        Return True
    End Function

    Public Function getActiveInteractionStatusPathPublic(ByVal activeDocument As ModelDoc2) As String
        If activeDocument Is Nothing Then Return ""

        Try
            If activeDocument.GetType() = swDocumentTypes_e.swDocASSEMBLY Then
                Dim editTarget As ModelDoc2 = getAssemblyEditTargetDocumentSafe(activeDocument)
                If editTarget IsNot Nothing Then
                    Dim editTargetPath As String = editTarget.GetPathName()
                    If Not String.IsNullOrWhiteSpace(editTargetPath) Then Return editTargetPath
                End If
            End If

            Return activeDocument.GetPathName()
        Catch
            Return ""
        End Try
    End Function

    Public Function canApplyQuietActiveServerStatusResultPublic(ByVal requestStartedUtc As DateTime) As Boolean
        'Never let a slow poll overwrite a newer explicit action. All Get Locks, Commit,
        'Unlock, Refresh, and Sync paths update this cache timestamp when their result lands.
        If statusCacheLastWriteUtc > requestStartedUtc Then Return False

        If asyncGetLocksInProgress OrElse asyncCommitInProgress OrElse
           asyncCleanupInProgress OrElse syncStatusInProgressOnControl() Then Return False

        Return True
    End Function

    Private Sub queuePendingAssemblySuppressionExpiry(ByVal openedUtc As DateTime)
        If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse
           Not myUserControl.IsHandleCreated Then Exit Sub

        Try
            'Suppression's state and ancestor Modify notifications normally complete in the
            'native command call. Two UI turns cover its trailing event without leaving a
            'seconds-long authorization window in which an unrelated edit could borrow it.
            myUserControl.BeginInvoke(
                New MethodInvoker(
                    Sub()
                        If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse
                           Not myUserControl.IsHandleCreated Then Exit Sub

                        myUserControl.BeginInvoke(
                            New MethodInvoker(
                                Sub()
                                    SyncLock assemblyGuardSync
                                        If pendingAssemblySuppressionState IsNot Nothing AndAlso
                                           pendingAssemblySuppressionState.OpenedUtc = openedUtc Then
                                            pendingAssemblySuppressionState = Nothing
                                        End If
                                    End SyncLock
                                End Sub
                            )
                        )
                    End Sub
                )
            )
        Catch
            SyncLock assemblyGuardSync
                If pendingAssemblySuppressionState IsNot Nothing AndAlso
                   pendingAssemblySuppressionState.OpenedUtc = openedUtc Then
                    pendingAssemblySuppressionState = Nothing
                End If
            End SyncLock
        End Try
    End Sub

    Private Function handleInsertComponentOnUnsavedAssemblyPreNotify() As Integer
        If iSwApp Is Nothing Then Return 0

        Dim activeAssembly As ModelDoc2 = Nothing

        Try
            activeAssembly = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeAssembly Is Nothing OrElse
               activeAssembly.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return 0

            If Not String.IsNullOrWhiteSpace(activeAssembly.GetPathName()) Then Return 0
        Catch
            'If SOLIDWORKS cannot verify the active document, preserve its native command.
            Return 0
        End Try

        'Cancel Insert Component before SOLIDWORKS mutates the assembly. Running first-save
        'after AddItemNotify was too late: the unsaved parent had no SVN identity and editing
        'the inserted child could produce a cascade of lock/status warnings.
        If newDocumentTeamSaveWorkflowInProgress Then Return -1

        If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse
           Not myUserControl.IsHandleCreated Then
            Try
                iSwApp.SendMsgToUser2(
                    "Save and commit this new assembly before inserting components.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try
            Return -1
        End If

        Try
            myUserControl.BeginInvoke(
                New MethodInvoker(
                    Sub()
                        If startNewDocumentFirstSaveFromCommitPublic() Then
                            Try
                                iSwApp.SendMsgToUser2(
                                    "Insert Component was paused while PlumVault starts the assembly's initial save and commit." & vbCrLf & vbCrLf &
                                    "After that workflow finishes, click Insert Component again.",
                                    swMessageBoxIcon_e.swMbInformation,
                                    swMessageBoxBtn_e.swMbOk
                                )
                            Catch
                            End Try
                        End If
                    End Sub
                )
            )
        Catch
            Try
                iSwApp.SendMsgToUser2(
                    "The initial save workflow could not be started. Save and commit this assembly before inserting components.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try
        End Try

        Return -1
    End Function

    Private Function handleInContextEditCommandPreNotify(ByVal command As Integer) As Integer
        If command <> SW_COMMAND_EDIT_COMPONENT AndAlso
           command <> SW_COMMAND_EDIT_PART AndAlso
           command <> SW_COMMAND_EDIT_FEATURE AndAlso
           command <> SW_COMMAND_SKETCH AndAlso
           command <> SW_COMMAND_EDIT_SKETCH AndAlso
           command <> SW_COMMAND_MAKE_EDIT_SKETCH Then Return 0
        If inContextAutoEditReplayInProgress Then Return 0
        If iSwApp Is Nothing Then Return 0

        Dim activeModel As ModelDoc2 = Nothing

        Try
            activeModel = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch
            Return 0
        End Try

        If activeModel Is Nothing Then Return 0

        Dim activeType As Integer

        Try
            activeType = activeModel.GetType()
        Catch
            Return 0
        End Try

        Dim isAssemblyContext As Boolean = (activeType = swDocumentTypes_e.swDocASSEMBLY)

        'The same Edit Component command is a toggle. When a read-only parent is already
        'hosting an in-context edit, letting the native exit command run produces SOLIDWORKS'
        '"parent is read-only" dialog. Cancel it and perform the bounded no-save transition.
        If isAssemblyContext AndAlso
           command <> SW_COMMAND_EDIT_FEATURE AndAlso
           assemblyHasActiveInContextEdit(activeModel) AndAlso
           queueReadOnlyParentInContextExit(activeModel) Then
            Return -1
        End If

        If Not isAssemblyContext AndAlso
           Not (activeType = swDocumentTypes_e.swDocPART AndAlso
                (command = SW_COMMAND_EDIT_FEATURE OrElse
                 command = SW_COMMAND_SKETCH OrElse
                 command = SW_COMMAND_EDIT_SKETCH OrElse
                 command = SW_COMMAND_MAKE_EDIT_SKETCH)) Then Return 0

        Dim childPath As String = ""
        Dim childDocument As ModelDoc2 = Nothing

        If isAssemblyContext Then
            If command = SW_COMMAND_EDIT_FEATURE OrElse
               command = SW_COMMAND_SKETCH OrElse
               command = SW_COMMAND_EDIT_SKETCH OrElse
               command = SW_COMMAND_MAKE_EDIT_SKETCH Then

                Dim selectedFeatureObject As Object = Nothing
                Try
                    Dim selectionManager As SelectionMgr = activeModel.SelectionManager
                    If selectionManager IsNot Nothing AndAlso selectionManager.GetSelectedObjectCount2(-1) > 0 Then
                        selectedFeatureObject = selectionManager.GetSelectedObject6(1, -1)
                    End If
                Catch
                    selectedFeatureObject = Nothing
                End Try

                childDocument = getFeatureEditTargetDocumentSafe(activeModel, selectedFeatureObject)
                If childDocument IsNot Nothing Then
                    Try
                        childPath = childDocument.GetPathName()
                    Catch
                        childPath = ""
                    End Try
                End If
            End If

            If String.IsNullOrWhiteSpace(childPath) Then
                'GetSelectedObjectsComponent4 resolves the owning Component2 even when the user
                'right-clicked a feature several levels down in the assembly tree.
                childPath = getSelectedInContextLockPathSafe(activeModel)
            End If

            If String.IsNullOrWhiteSpace(childPath) AndAlso command = SW_COMMAND_SKETCH Then
                childDocument = activeModel
                childPath = getAssemblyPathKeySafe(activeModel)
            End If
        Else
            childDocument = activeModel
            Try
                childPath = activeModel.GetPathName()
            Catch
                childPath = ""
            End Try
        End If

        If String.IsNullOrWhiteSpace(childPath) Then Return 0
        If Not isPathInsideLocalRepo(childPath) Then Return 0
        If isNewUnversionedOrAddedFile(childPath) Then Return 0

        writeOperationLog(
            "Edit command precheck: command=" & command.ToString() &
            "; active=" & getAssemblyPathKeySafe(activeModel) &
            "; target=" & childPath
        )

        If Not File.Exists(childPath) Then
            showInContextAutoEditFailure(
                childPath,
                "The selected component file is missing from the local working copy."
            )
            Return -1
        End If

        Dim assemblyPath As String = getAssemblyPathKeySafe(activeModel)
        If String.IsNullOrWhiteSpace(assemblyPath) Then Return 0

        'A very fast Edit Part/Edit Assembly click can arrive while the user's manual Get
        'Locks request is still running. Letting SOLIDWORKS continue now produces its native
        'read-only warning even when the lock succeeds a moment later. Hold only this exact
        'target and replay only after the completed SVN result proves that its K token is ours.
        If asyncGetLocksInProgress AndAlso asyncGetLocksIncludesPath(childPath) Then
            If rememberEditReplayForCurrentGetLocks(assemblyPath, childPath, command) Then
                writeOperationLog(
                    "Edit command deferred until current Get Locks completes: command=" &
                    command.ToString() & "; target=" & childPath
                )
            Else
                Try
                    iSwApp.SendMsgToUser2(
                        "Get Locks is still running for this file." & vbCrLf & vbCrLf &
                        "Wait for the lock result, then select the item and start the edit again.",
                        swMessageBoxIcon_e.swMbInformation,
                        swMessageBoxBtn_e.swMbOk
                    )
                Catch
                End Try
            End If

            Return -1
        End If

        'Do not trust the UI status cache here: the user may have unlocked through Explorer.
        'Edit access requires the working copy's live local K token.
        If cachedServerStatusProvesLockUnavailable(childPath) Then
            showManualLockRequired(childPath, "start this edit")
            Return -1
        End If

        Dim hasLock As Boolean = userHasLocalSvnLockTokenForPath(childPath, allowCachedToken:=False)
        If Not hasLock Then
            showManualLockRequired(childPath, "start this edit")
            Return -1
        End If

        If childDocument Is Nothing Then childDocument = getOpenModelByPathSafe(childPath)
        Dim writableFailure As String = ""

        'This exact file has a live local lock and is the immediate user interaction target.
        'Make its already-open SOLIDWORKS document writable synchronously so the native
        '"available for write access" Yes/No dialog never appears. This is deliberately not a
        'bulk reconciliation across sibling documents.
        If Not ensureOpenCadDocumentWritableNow(childPath, childDocument, writableFailure) Then
            showInContextAutoEditFailure(childPath, writableFailure, writeAccessWasObtained:=True)
            Return -1
        End If

        If isAssemblyContext AndAlso command <> SW_COMMAND_EDIT_FEATURE Then
            rememberInContextEditDirtyBaseline(activeModel, assemblyPath)
        End If

        Return 0
    End Function

    Public Function handleCadFeatureEditPrePublic(ByVal eventDocument As ModelDoc2,
                                                   ByVal actionDescription As String,
                                                   Optional ByVal editFeature As Object = Nothing) As Integer
        If eventDocument Is Nothing Then Return 0

        Dim eventType As Integer
        Try
            eventType = eventDocument.GetType()
        Catch
            Return 0
        End Try

        Dim targetPath As String = ""
        Dim targetDocument As ModelDoc2 = Nothing

        If eventType = swDocumentTypes_e.swDocASSEMBLY Then
            'Resolve the feature itself first. A sketch below a part and an assembly-owned
            'MirrorComponent feature can both expose a Component2 through SelectionManager,
            'but they require different locks. Matching the feature against its model document
            'keeps the exact part/middle-assembly owner authoritative in deep assemblies.
            targetDocument = getFeatureEditTargetDocumentSafe(eventDocument, editFeature)

            If targetDocument IsNot Nothing Then
                Try
                    targetPath = targetDocument.GetPathName()
                Catch
                    targetPath = ""
                End Try
            End If

            If String.IsNullOrWhiteSpace(targetPath) Then
                targetPath = getSelectedExternalPhysicalChildPathSafe(eventDocument)
                If Not String.IsNullOrWhiteSpace(targetPath) Then
                    targetDocument = getOpenModelByPathSafe(targetPath)
                End If
            End If

            If String.IsNullOrWhiteSpace(targetPath) Then
                targetDocument = getAssemblyEditTargetDocumentSafe(eventDocument)
                If targetDocument IsNot Nothing Then
                    Try
                        targetPath = targetDocument.GetPathName()
                    Catch
                        targetPath = ""
                    End Try
                End If
            End If

            'No physical child owns the selected feature, so this is an assembly-owned sketch
            'or feature and the event assembly itself is the file whose lock controls the edit.
            If String.IsNullOrWhiteSpace(targetPath) Then
                targetDocument = eventDocument
                targetPath = getAssemblyPathKeySafe(eventDocument)
            End If
        ElseIf eventType = swDocumentTypes_e.swDocPART Then
            targetDocument = eventDocument
            targetPath = getAssemblyPathKeySafe(eventDocument)
        Else
            Return 0
        End If

        If String.IsNullOrWhiteSpace(targetPath) Then Return 0
        If Not isPathInsideLocalRepo(targetPath) Then Return 0
        If isNewUnversionedOrAddedFile(targetPath) Then Return 0

        writeOperationLog(
            "Feature edit precheck: " & actionDescription &
            "; event=" & getAssemblyPathKeySafe(eventDocument) &
            "; target=" & targetPath
        )

        If cachedServerStatusProvesLockUnavailable(targetPath) OrElse
           Not userHasLocalSvnLockTokenForPath(targetPath, allowCachedToken:=False) Then
            showManualLockRequired(targetPath, actionDescription)
            Return 1
        End If

        Dim writableFailure As String = ""
        If Not ensureOpenCadDocumentWritableNow(targetPath, targetDocument, writableFailure) Then
            showInContextAutoEditFailure(targetPath, writableFailure, writeAccessWasObtained:=True)
            Return 1
        End If

        Return 0
    End Function

    Public Function blockSelectedCadDestructiveEditPrePublic(ByVal eventDocument As ModelDoc2,
                                                              ByVal actionDescription As String) As Integer
        If eventDocument Is Nothing Then Return 0

        Dim eventType As Integer
        Try
            eventType = eventDocument.GetType()
        Catch
            Return 0
        End Try

        Dim targetDocument As ModelDoc2 = Nothing
        Dim targetPath As String = ""

        If eventType = swDocumentTypes_e.swDocPART OrElse
           eventType = swDocumentTypes_e.swDocDRAWING Then
            'A drawing's views/annotations/dimensions belong to the drawing file itself, so
            'deleting them requires the drawing's own lock - same single-file rule as a part.
            targetDocument = eventDocument
        ElseIf eventType = swDocumentTypes_e.swDocASSEMBLY Then
            Dim selectedObject As Object = Nothing
            Dim selectedComponent As Component2 = Nothing
            Dim selectedType As Integer = -1

            Try
                Dim selectionManager As SelectionMgr = eventDocument.SelectionManager
                If selectionManager IsNot Nothing AndAlso selectionManager.GetSelectedObjectCount2(-1) > 0 Then
                    selectedObject = selectionManager.GetSelectedObject6(1, -1)
                    selectedComponent = selectionManager.GetSelectedObjectsComponent4(1, -1)
                    selectedType = selectionManager.GetSelectedObjectType3(1, -1)
                End If
            Catch
                selectedObject = Nothing
                selectedComponent = Nothing
                selectedType = -1
            End Try

            'Features beneath an expanded part/subassembly belong to that physical file.
            'A directly selected component belongs to its immediate parent assembly.
            targetDocument = getFeatureEditTargetDocumentSafe(eventDocument, selectedObject)
            If targetDocument Is Nothing AndAlso
               selectedComponent IsNot Nothing AndAlso
               selectedType >= 0 AndAlso
               selectedType <> swSelectType_e.swSelCOMPONENTS Then
                Try
                    targetDocument = TryCast(selectedComponent.GetModelDoc2(), ModelDoc2)
                Catch
                    targetDocument = Nothing
                End Try
            End If
            If targetDocument Is Nothing Then
                targetDocument = getSelectedAssemblyEditOwnerPublic(eventDocument)
            End If
            If targetDocument Is Nothing Then targetDocument = eventDocument
        Else
            Return 0
        End If

        targetPath = getAssemblyPathKeySafe(targetDocument)
        If String.IsNullOrWhiteSpace(targetPath) Then Return 0
        If Not isPathInsideLocalRepo(targetPath) Then Return 0
        If isNewUnversionedOrAddedFile(targetPath) Then Return 0

        writeOperationLog(
            "Destructive edit precheck: " & actionDescription &
            "; event=" & getAssemblyPathKeySafe(eventDocument) &
            "; target=" & targetPath
        )

        If cachedServerStatusProvesLockUnavailable(targetPath) OrElse
           Not userHasLocalSvnLockTokenForPath(targetPath, allowCachedToken:=False) Then
            showManualLockRequired(targetPath, actionDescription)
            Return 1
        End If

        Dim writableFailure As String = ""
        If Not ensureOpenCadDocumentWritableNow(targetPath, targetDocument, writableFailure) Then
            showInContextAutoEditFailure(targetPath, writableFailure, writeAccessWasObtained:=True)
            Return 1
        End If

        Return 0
    End Function

    Private Function getFeatureEditTargetDocumentSafe(ByVal eventAssembly As ModelDoc2,
                                                       ByVal editFeatureObject As Object) As ModelDoc2
        If eventAssembly Is Nothing Then Return Nothing

        Dim selectedFeature As Feature = TryCast(editFeatureObject, Feature)
        Dim selectedComponent As Component2 = Nothing

        Try
            Dim selectionManager As SelectionMgr = eventAssembly.SelectionManager
            If selectionManager IsNot Nothing AndAlso selectionManager.GetSelectedObjectCount2(-1) > 0 Then
                If selectedFeature Is Nothing Then
                    selectedFeature = TryCast(selectionManager.GetSelectedObject6(1, -1), Feature)
                End If
                selectedComponent = TryCast(selectionManager.GetSelectedObjectsComponent4(1, -1), Component2)
            End If
        Catch
        End Try

        If selectedFeature Is Nothing Then Return Nothing

        'An assembly-owned feature may be represented by a contextual proxy. Native identity
        'against loaded assemblies is strongest and correctly resolves the top-level case.
        Dim exactAssemblyOwner As ModelDoc2 = findLoadedAssemblyOwningSelectedFeature(selectedFeature)
        If exactAssemblyOwner IsNot Nothing Then Return exactAssemblyOwner

        If selectedComponent IsNot Nothing Then
            Dim selectedModel As ModelDoc2 = Nothing

            Try
                selectedModel = TryCast(selectedComponent.GetModelDoc2(), ModelDoc2)
            Catch
                selectedModel = Nothing
            End Try

            If selectedModel IsNot Nothing Then
                Try
                    Dim featureName As String = selectedFeature.Name
                    Dim candidate As Feature = selectedModel.FeatureByName(featureName)

                    If candidate IsNot Nothing Then
                        Dim selectedType As String = selectedFeature.GetTypeName2()
                        Dim candidateType As String = candidate.GetTypeName2()

                        If comObjectsHaveSameIdentity(candidate, selectedFeature) OrElse
                           String.Equals(candidateType, selectedType, StringComparison.OrdinalIgnoreCase) Then
                            Return selectedModel
                        End If
                    End If
                Catch
                End Try
            End If

            'The selected Component2 can be a generated child of an assembly feature (for
            'example MirrorComponent1). Walk upward only after proving the feature is not owned
            'by the selected part/subassembly itself.
            Dim componentAssemblyOwner As ModelDoc2 =
                findAssemblyFeatureOwnerInComponentChain(selectedComponent, selectedFeature)
            If componentAssemblyOwner IsNot Nothing Then Return componentAssemblyOwner

            If selectedModel IsNot Nothing Then Return selectedModel
        End If

        Return Nothing
    End Function

    Public Function getSelectedAssemblyEditOwnerPublic(ByVal eventAssembly As ModelDoc2,
                                                        Optional ByVal selectedDocumentOwnsEdit As Boolean = False) As ModelDoc2
        If eventAssembly Is Nothing Then Return Nothing

        Try
            'While SOLIDWORKS explicitly reports Editing Assembly/Edit Part, that live target
            'is the authoritative owner. Mirror features can select one generated component
            'while emitting several suppression events; selection alone then points at the
            'read-only top assembly even though the locked middle assembly owns the feature.
            Dim activeEditTarget As ModelDoc2 = getAssemblyEditTargetDocumentSafe(eventAssembly)
            If activeEditTarget IsNot Nothing AndAlso
               activeEditTarget.GetType() = swDocumentTypes_e.swDocASSEMBLY Then Return activeEditTarget

            Dim rememberedOwner As ModelDoc2 = getRecentNestedFeatureOwnerDocument(eventAssembly)
            If rememberedOwner IsNot Nothing Then Return rememberedOwner

            Dim selectionManager As SelectionMgr = eventAssembly.SelectionManager
            If selectionManager Is Nothing OrElse selectionManager.GetSelectedObjectCount2(-1) <= 0 Then Return eventAssembly

            Dim selectedComponent As Component2 = selectionManager.GetSelectedObjectsComponent4(1, -1)
            If selectedComponent Is Nothing Then Return eventAssembly

            Dim selectedType As Integer = -1
            Try
                selectedType = selectionManager.GetSelectedObjectType3(1, -1)
            Catch
                selectedType = -1
            End Try

            'A direct component selection changes suppression/position in its parent assembly.
            'A feature, sketch, dimension, face, or edge selected through that component is
            'owned by the component's own model document instead.
            Dim featureOwnsEdit As Boolean =
                selectedType >= 0 AndAlso selectedType <> swSelectType_e.swSelCOMPONENTS

            If selectedDocumentOwnsEdit OrElse featureOwnsEdit Then
                Dim selectedDocument As ModelDoc2 = TryCast(selectedComponent.GetModelDoc2(), ModelDoc2)
                If selectedDocument IsNot Nothing AndAlso
                   selectedDocument.GetType() = swDocumentTypes_e.swDocASSEMBLY Then Return selectedDocument
            End If

            Dim parentComponent As Component2 = selectedComponent.GetParent()
            If parentComponent Is Nothing Then Return eventAssembly

            Dim ownerDocument As ModelDoc2 = TryCast(parentComponent.GetModelDoc2(), ModelDoc2)
            If ownerDocument Is Nothing Then Return eventAssembly
            If ownerDocument.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return eventAssembly

            Return ownerDocument
        Catch
            Return eventAssembly
        End Try
    End Function

    Private Function getLiveSelectedSuppressionOwnerDocument(ByVal eventAssembly As ModelDoc2) As ModelDoc2
        If eventAssembly Is Nothing Then Return Nothing

        Try
            Dim selectionManager As SelectionMgr = eventAssembly.SelectionManager
            If selectionManager IsNot Nothing AndAlso selectionManager.GetSelectedObjectCount2(-1) > 0 Then
                Dim selectedObject As Object = selectionManager.GetSelectedObject6(1, -1)
                Dim selectedType As Integer = selectionManager.GetSelectedObjectType3(1, -1)

                'Suppressing a feature beneath an expanded physical part changes the part
                'file, not the assembly containing that occurrence. Reuse the same proven
                'feature-owner resolver as Edit Feature/Edit Sketch. A direct Component2
                'selection deliberately skips this branch because its suppression state is
                'persisted by the immediate parent assembly.
                If selectedType <> swSelectType_e.swSelCOMPONENTS AndAlso
                   TypeOf selectedObject Is Feature Then
                    Dim featureOwner As ModelDoc2 = getFeatureEditTargetDocumentSafe(
                        eventAssembly,
                        selectedObject
                    )

                    If featureOwner IsNot Nothing Then Return featureOwner

                    'A deep in-context feature can be exposed through a contextual proxy that
                    'does not compare equal to the native part feature. Only in this proven
                    'feature-selection branch, accept SOLIDWORKS' explicit Edit Part target.
                    Dim activeEditTarget As ModelDoc2 = getAssemblyEditTargetDocumentSafe(eventAssembly)
                    If activeEditTarget IsNot Nothing AndAlso
                       activeEditTarget.GetType() = swDocumentTypes_e.swDocPART Then Return activeEditTarget
                End If
            End If
        Catch ex As Exception
            writeOperationLog("Could not resolve selected suppression feature owner: " & ex.Message)
        End Try

        'Preserve the established assembly-feature/component behavior whenever a physical
        'part feature cannot be proven from the live selection.
        Return getLiveSelectedAssemblyFeatureOwner(eventAssembly)
    End Function

    Private Function getLiveSelectedAssemblyFeatureOwner(ByVal eventAssembly As ModelDoc2) As ModelDoc2
        If eventAssembly Is Nothing Then Return Nothing

        Try
            Dim selectionManager As SelectionMgr = eventAssembly.SelectionManager
            If selectionManager Is Nothing OrElse selectionManager.GetSelectedObjectCount2(-1) <= 0 Then Return Nothing

            Dim selectedObject As Object = selectionManager.GetSelectedObject6(1, -1)
            Dim selectedFeature As Feature = TryCast(selectedObject, Feature)
            Dim selectedComponent As Component2 = selectionManager.GetSelectedObjectsComponent4(1, -1)
            Dim selectedType As Integer = selectionManager.GetSelectedObjectType3(1, -1)

            If selectedFeature IsNot Nothing Then
                'A feature selected below an expanded subassembly can be represented by a
                'contextual COM proxy. Match its native identity against every loaded assembly
                'document first; unlike the later state-change event, this still identifies
                'MirrorComponent1's actual document even though Edit Assembly was never entered.
                Dim exactOwner As ModelDoc2 = findLoadedAssemblyOwningSelectedFeature(selectedFeature)
                If exactOwner IsNot Nothing Then Return exactOwner

                'Some SOLIDWORKS releases return a contextual feature proxy with a different
                'IUnknown. Walk the owning component chain and match name/type as a fallback.
                Dim componentOwner As ModelDoc2 = findAssemblyFeatureOwnerInComponentChain(
                    selectedComponent,
                    selectedFeature
                )
                If componentOwner IsNot Nothing Then Return componentOwner
            End If

            If selectedComponent Is Nothing Then Return Nothing

            If selectedType <> swSelectType_e.swSelCOMPONENTS Then
                Dim selectedDocument As ModelDoc2 = TryCast(selectedComponent.GetModelDoc2(), ModelDoc2)
                If isAssemblyDocumentSafe(selectedDocument) Then Return selectedDocument
            End If

            Dim parentComponent As Component2 = selectedComponent.GetParent()
            If parentComponent Is Nothing Then Return eventAssembly

            Dim parentDocument As ModelDoc2 = TryCast(parentComponent.GetModelDoc2(), ModelDoc2)
            If isAssemblyDocumentSafe(parentDocument) Then Return parentDocument
        Catch ex As Exception
            writeOperationLog("Could not resolve selected assembly feature owner: " & ex.Message)
        End Try

        Return Nothing
    End Function

    Private Function findLoadedAssemblyOwningSelectedFeature(ByVal selectedFeature As Feature) As ModelDoc2
        If selectedFeature Is Nothing OrElse iSwApp Is Nothing Then Return Nothing

        Dim featureName As String = ""

        Try
            featureName = selectedFeature.Name
        Catch
            featureName = ""
        End Try

        If String.IsNullOrWhiteSpace(featureName) Then Return Nothing

        Dim documents As Object() = Nothing

        Try
            documents = TryCast(iSwApp.GetDocuments(), Object())
        Catch
            documents = Nothing
        End Try

        If documents Is Nothing Then Return Nothing

        For Each documentObject As Object In documents
            Dim candidateDocument As ModelDoc2 = TryCast(documentObject, ModelDoc2)
            If Not isAssemblyDocumentSafe(candidateDocument) Then Continue For

            Try
                Dim candidateFeature As Feature = candidateDocument.FeatureByName(featureName)
                If candidateFeature IsNot Nothing AndAlso comObjectsHaveSameIdentity(candidateFeature, selectedFeature) Then
                    Return candidateDocument
                End If
            Catch
            End Try
        Next

        Return Nothing
    End Function

    Private Function findAssemblyFeatureOwnerInComponentChain(ByVal selectedComponent As Component2,
                                                               ByVal selectedFeature As Feature) As ModelDoc2
        If selectedComponent Is Nothing OrElse selectedFeature Is Nothing Then Return Nothing

        Dim featureName As String = ""
        Dim featureType As String = ""

        Try
            featureName = selectedFeature.Name
            featureType = selectedFeature.GetTypeName2()
        Catch
            Return Nothing
        End Try

        Dim currentComponent As Component2 = selectedComponent
        Dim depth As Integer = 0

        While currentComponent IsNot Nothing AndAlso depth < 64
            Try
                Dim candidateDocument As ModelDoc2 = TryCast(currentComponent.GetModelDoc2(), ModelDoc2)

                If isAssemblyDocumentSafe(candidateDocument) Then
                    Dim candidateFeature As Feature = candidateDocument.FeatureByName(featureName)

                    If candidateFeature IsNot Nothing Then
                        Dim candidateType As String = ""
                        Try
                            candidateType = candidateFeature.GetTypeName2()
                        Catch
                            candidateType = ""
                        End Try

                        If String.Equals(candidateType, featureType, StringComparison.OrdinalIgnoreCase) Then
                            Return candidateDocument
                        End If
                    End If
                End If

                currentComponent = currentComponent.GetParent()
            Catch
                Exit While
            End Try

            depth += 1
        End While

        Return Nothing
    End Function

    Private Function comObjectsHaveSameIdentity(ByVal leftObject As Object,
                                                 ByVal rightObject As Object) As Boolean
        If leftObject Is Nothing OrElse rightObject Is Nothing Then Return False

        Dim leftPointer As IntPtr = IntPtr.Zero
        Dim rightPointer As IntPtr = IntPtr.Zero

        Try
            leftPointer = System.Runtime.InteropServices.Marshal.GetIUnknownForObject(leftObject)
            rightPointer = System.Runtime.InteropServices.Marshal.GetIUnknownForObject(rightObject)
            Return leftPointer = rightPointer
        Catch
            Return Object.ReferenceEquals(leftObject, rightObject)
        Finally
            If leftPointer <> IntPtr.Zero Then
                System.Runtime.InteropServices.Marshal.Release(leftPointer)
            End If
            If rightPointer <> IntPtr.Zero Then
                System.Runtime.InteropServices.Marshal.Release(rightPointer)
            End If
        End Try
    End Function

    Private Function isAssemblyDocumentSafe(ByVal document As ModelDoc2) As Boolean
        If document Is Nothing Then Return False

        Try
            Return document.GetType() = swDocumentTypes_e.swDocASSEMBLY
        Catch
            Return False
        End Try
    End Function

    Public Sub noteSelectedAssemblyFeatureOwnerPublic(ByVal eventAssembly As ModelDoc2)
        If eventAssembly Is Nothing Then Exit Sub

        Dim eventAssemblyPath As String = getAssemblyPathKeySafe(eventAssembly)
        If String.IsNullOrWhiteSpace(eventAssemblyPath) Then Exit Sub

        Dim ownerPath As String = ""

        Try
            Dim ownerDocument As ModelDoc2 = getLiveSelectedAssemblyFeatureOwner(eventAssembly)
            If ownerDocument IsNot Nothing Then ownerPath = getAssemblyPathKeySafe(ownerDocument)
        Catch
            ownerPath = ""
        End Try

        SyncLock assemblyGuardSync
            If String.IsNullOrWhiteSpace(ownerPath) OrElse pathsAreSame(ownerPath, eventAssemblyPath) Then
                Dim existing As RecentNestedFeatureOwner = Nothing
                If recentNestedFeatureOwnerByEventAssembly.TryGetValue(eventAssemblyPath, existing) AndAlso
                   existing IsNot Nothing AndAlso
                   (DateTime.UtcNow - existing.CapturedUtc).TotalMilliseconds <= 750.0 Then
                    'Suppress/unsuppress commonly clears the feature selection or replaces it
                    'with a generated component immediately before raising its state event.
                    'Keep the just-captured owner through that native selection churn.
                    Exit Sub
                End If
                recentNestedFeatureOwnerByEventAssembly.Remove(eventAssemblyPath)
            Else
                recentNestedFeatureOwnerByEventAssembly(eventAssemblyPath) = New RecentNestedFeatureOwner With {
                    .OwnerPath = ownerPath,
                    .CapturedUtc = DateTime.UtcNow
                }
            End If
        End SyncLock
    End Sub

    Private Function getRecentNestedFeatureOwnerDocument(ByVal eventAssembly As ModelDoc2) As ModelDoc2
        Dim eventAssemblyPath As String = getAssemblyPathKeySafe(eventAssembly)
        If String.IsNullOrWhiteSpace(eventAssemblyPath) Then Return Nothing

        Dim ownerPath As String = ""

        SyncLock assemblyGuardSync
            Dim remembered As RecentNestedFeatureOwner = Nothing
            If Not recentNestedFeatureOwnerByEventAssembly.TryGetValue(eventAssemblyPath, remembered) OrElse remembered Is Nothing Then Return Nothing

            If (DateTime.UtcNow - remembered.CapturedUtc).TotalSeconds > 3.0 Then
                recentNestedFeatureOwnerByEventAssembly.Remove(eventAssemblyPath)
                Return Nothing
            End If

            ownerPath = remembered.OwnerPath
        End SyncLock

        Dim ownerDocument As ModelDoc2 = getOpenModelByPathSafe(ownerPath)
        If ownerDocument Is Nothing Then Return Nothing

        Try
            If ownerDocument.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return Nothing
        Catch
            Return Nothing
        End Try

        Return ownerDocument
    End Function

    Private Function resultContainsPath(ByVal paths() As String, ByVal targetPath As String) As Boolean
        If paths Is Nothing OrElse String.IsNullOrWhiteSpace(targetPath) Then Return False

        For Each candidate As String In paths
            If pathsAreSame(candidate, targetPath) Then Return True
        Next

        Return False
    End Function

    Private Sub rememberAsyncGetLocksPaths(ByVal filePaths() As String)
        Dim normalized As New List(Of String)()

        If filePaths IsNot Nothing Then
            For Each filePath As String In filePaths
                If String.IsNullOrWhiteSpace(filePath) Then Continue For
                Dim normalizedPath As String = normalizeFullPathSafe(filePath)
                If String.IsNullOrWhiteSpace(normalizedPath) Then Continue For
                normalized.Add(normalizedPath)
            Next
        End If

        SyncLock asyncGetLocksStateSync
            asyncGetLocksRequestedPaths = normalized.Distinct(StringComparer.OrdinalIgnoreCase).ToArray()
        End SyncLock
    End Sub

    Private Sub clearAsyncGetLocksPaths()
        SyncLock asyncGetLocksStateSync
            asyncGetLocksRequestedPaths = Nothing
        End SyncLock
    End Sub

    Private Function asyncGetLocksIncludesPath(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        Dim normalizedPath As String = normalizeFullPathSafe(filePath)

        SyncLock asyncGetLocksStateSync
            If asyncGetLocksRequestedPaths Is Nothing Then Return False

            For Each requestedPath As String In asyncGetLocksRequestedPaths
                If String.Equals(requestedPath, normalizedPath, StringComparison.OrdinalIgnoreCase) Then Return True
            Next
        End SyncLock

        Return False
    End Function

    Private Function rememberEditReplayForCurrentGetLocks(ByVal assemblyPath As String,
                                                           ByVal childPath As String,
                                                           ByVal requestedCommand As Integer) As Boolean
        If Not asyncGetLocksInProgress OrElse Not asyncGetLocksIncludesPath(childPath) Then Return False

        If pendingInContextAutoEditRequest IsNot Nothing Then
            Dim pendingAgeSeconds As Double =
                (DateTime.UtcNow - pendingInContextAutoEditRequest.RequestedUtc).TotalSeconds

            If pendingAgeSeconds >= 0 AndAlso pendingAgeSeconds <= 60.0 Then
                Return pathsAreSame(pendingInContextAutoEditRequest.AssemblyPath, assemblyPath) AndAlso
                    pathsAreSame(pendingInContextAutoEditRequest.ChildPath, childPath) AndAlso
                    pendingInContextAutoEditRequest.RequestedCommand = requestedCommand
            End If
        End If

        pendingInContextAutoEditRequest = New PendingInContextAutoEdit With {
            .AssemblyPath = assemblyPath,
            .ChildPath = childPath,
            .RequestedUtc = DateTime.UtcNow,
            .RequestedCommand = requestedCommand
        }

        Return True
    End Function

    Private Function buildInContextAutoEditFailureMessage(ByVal childPath As String,
                                                           ByVal detail As String,
                                                           Optional ByVal writeAccessWasObtained As Boolean = False) As String
        Dim childName As String = childPath

        Try
            childName = Path.GetFileName(childPath)
        Catch
        End Try

        Dim operationName As String =
            If(String.Equals(Path.GetExtension(childPath), ".SLDASM", StringComparison.OrdinalIgnoreCase),
               "Edit Assembly",
               "Edit Part")

        Dim message As String
        If writeAccessWasObtained Then
            message = "Could not start " & operationName & ":" & vbCrLf & vbCrLf & childName
        Else
            message = "Could not get write access for " & operationName & ":" & vbCrLf & vbCrLf & childName
        End If

        If Not String.IsNullOrWhiteSpace(detail) Then
            message &= vbCrLf & vbCrLf & detail.Trim()
        End If

        If writeAccessWasObtained Then
            message &= vbCrLf & vbCrLf &
                "The SVN lock is still held by you. Reselect the item and try " & operationName &
                " again. If SOLIDWORKS still shows it as read-only, click Sync and reopen that document."
        Else
            message &= vbCrLf & vbCrLf &
                "Click Sync to refresh the SVN lock and revision status. If the file is out of date, " &
                "use Get Latest, then select the component and try " & operationName & " again."
        End If

        Return message
    End Function

    Private Sub showInContextAutoEditFailure(ByVal childPath As String,
                                              ByVal detail As String,
                                              Optional ByVal writeAccessWasObtained As Boolean = False)
        Try
            iSwApp.SendMsgToUser2(
                buildInContextAutoEditFailureMessage(childPath, detail, writeAccessWasObtained),
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
        Catch
        End Try
    End Sub

    'The deferred writable-state timer must not skip a known-slow file that a pending
    'Edit Component replay is waiting on: reporting success without the transition would
    'replay the edit against a still-read-only document. Mirrors the immediate Get Locks
    'loop's auto-edit exemption.
    Public Function isPendingInContextAutoEditTargetPublic(ByVal filePath As String) As Boolean
        Try
            Dim request As PendingInContextAutoEdit = pendingInContextAutoEditRequest
            Return request IsNot Nothing AndAlso pathsAreSame(request.ChildPath, filePath)
        Catch
            Return False
        End Try
    End Function

    Public Sub noteDeferredWriteAccessResultPublic(ByVal filePath As String, ByVal succeeded As Boolean)
        Dim request As PendingInContextAutoEdit = pendingInContextAutoEditRequest
        If request Is Nothing OrElse Not pathsAreSame(request.ChildPath, filePath) Then Exit Sub

        If Not succeeded Then
            pendingInContextAutoEditRequest = Nothing
            writeOperationLog("Edit Component writable-state reconciliation failed: " & filePath)
            showInContextAutoEditFailure(
                filePath,
                "The SVN lock was obtained, but SOLIDWORKS did not finish making this open document writable.",
                writeAccessWasObtained:=True
            )
            Exit Sub
        End If

        'Always leave the timer/native-mutation callback before replaying the native command.
        Try
            myUserControl.BeginInvoke(New MethodInvoker(AddressOf resumePendingInContextAutoEdit))
        Catch
            pendingInContextAutoEditRequest = Nothing
        End Try
    End Sub

    Private Sub resumePendingInContextAutoEdit()
        Dim request As PendingInContextAutoEdit = pendingInContextAutoEditRequest
        If request Is Nothing Then Exit Sub

        Dim childIsAssembly As Boolean =
            String.Equals(Path.GetExtension(request.ChildPath), ".SLDASM", StringComparison.OrdinalIgnoreCase)
        Dim isFeatureEdit As Boolean = request.RequestedCommand = SW_COMMAND_EDIT_FEATURE
        Dim isSketchEdit As Boolean =
            request.RequestedCommand = SW_COMMAND_SKETCH OrElse
            request.RequestedCommand = SW_COMMAND_EDIT_SKETCH OrElse
            request.RequestedCommand = SW_COMMAND_MAKE_EDIT_SKETCH
        Dim operationName As String = If(isFeatureEdit,
                                         "Edit Feature",
                                         If(isSketchEdit,
                                            "Edit Sketch",
                                            If(childIsAssembly, "Edit Assembly", "Edit Part")))

        If (DateTime.UtcNow - request.RequestedUtc).TotalSeconds > 60.0 Then
            pendingInContextAutoEditRequest = Nothing
            iSwApp.SendMsgToUser2(
                "The SVN lock was obtained, but too much time passed to safely resume " & operationName & "." & vbCrLf & vbCrLf &
                "Select the item and click " & operationName & " again.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub
        End If

        Dim assemblyModel As ModelDoc2 = Nothing

        Try
            assemblyModel = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch
            assemblyModel = Nothing
        End Try

        If assemblyModel Is Nothing OrElse
           Not pathsAreSame(getAssemblyPathKeySafe(assemblyModel), request.AssemblyPath) Then
            pendingInContextAutoEditRequest = Nothing
            iSwApp.SendMsgToUser2(
                "The SVN lock was obtained. Because you changed document windows while it was running, " &
                "PlumVault did not force SolidWorks into " & operationName & " mode." & vbCrLf & vbCrLf &
                "Return to the original document, select the item, and click " & operationName & " again.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub
        End If

        Dim selectedChildPath As String = ""

        Try
            If assemblyModel.GetType() = swDocumentTypes_e.swDocASSEMBLY Then
                selectedChildPath = getSelectedInContextLockPathSafe(assemblyModel)
            ElseIf (isFeatureEdit OrElse isSketchEdit) AndAlso
                   assemblyModel.GetType() = swDocumentTypes_e.swDocPART Then
                selectedChildPath = assemblyModel.GetPathName()
            End If
        Catch
            selectedChildPath = ""
        End Try

        If Not pathsAreSame(selectedChildPath, request.ChildPath) Then
            pendingInContextAutoEditRequest = Nothing
            writeOperationLog(operationName & " replay skipped because selection changed; lock retained: " & request.ChildPath)
            Exit Sub
        End If

        Dim childDocument As ModelDoc2 = getOpenModelByPathSafe(request.ChildPath)
        If childDocument IsNot Nothing Then
            Try
                If childDocument.IsOpenedReadOnly() Then
                    pendingInContextAutoEditRequest = Nothing
                    showInContextAutoEditFailure(
                        request.ChildPath,
                        "The SVN lock was obtained, but SolidWorks still has the open child in read-only mode."
                    )
                    Exit Sub
                End If
            Catch
                pendingInContextAutoEditRequest = Nothing
                showInContextAutoEditFailure(
                    request.ChildPath,
                    "SolidWorks could not verify that the open child is writable."
                )
                Exit Sub
            End Try
        End If

        pendingInContextAutoEditRequest = Nothing
        inContextAutoEditReplayInProgress = True

        Try
            Dim replayCommand As Integer = request.RequestedCommand

            'Edit Part (965) cannot enter a selected subassembly and returns status -1. The
            'general Edit Component command delegates correctly to Edit Assembly for SLDASM.
            If childIsAssembly AndAlso replayCommand = SW_COMMAND_EDIT_PART Then
                replayCommand = SW_COMMAND_EDIT_COMPONENT
            End If

            If Not iSwApp.RunCommand(replayCommand, "") Then
                showInContextAutoEditFailure(
                    request.ChildPath,
                    "SOLIDWORKS declined the edit command after the lock and writable state were verified.",
                    writeAccessWasObtained:=True
                )
            Else
                writeOperationLog(operationName & " resumed after lock/write verification: " & request.ChildPath)
            End If
        Catch ex As Exception
            showInContextAutoEditFailure(
                request.ChildPath,
                "SOLIDWORKS could not resume " & operationName & ": " & ex.Message,
                writeAccessWasObtained:=True
            )
        Finally
            inContextAutoEditReplayInProgress = False
        End Try
    End Sub

    Public Sub noteInContextEditEndedPublic(ByVal assemblyDocument As ModelDoc2, ByVal editedDocument As ModelDoc2)
        Dim parentWasCleanBeforeChildEdit As Boolean = False
        Dim endedSession As InContextEditSession = Nothing
        Dim assemblyKey As String = ""

        Try
            assemblyKey = getAssemblyPathKeySafe(assemblyDocument)
            If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub

            SyncLock assemblyGuardSync
                Dim session As InContextEditSession = Nothing
                If Not inContextEditSessionByAssemblyPath.TryGetValue(assemblyKey, session) OrElse session Is Nothing Then Exit Sub

                Dim endedChildPath As String = ""
                If editedDocument IsNot Nothing Then
                    Try
                        endedChildPath = editedDocument.GetPathName()
                    Catch
                        endedChildPath = ""
                    End Try
                End If

                'A mismatched End event must not extend an older child's authorization.
                If Not String.IsNullOrWhiteSpace(endedChildPath) AndAlso
                   Not pathsAreSame(endedChildPath, session.ChildPath) Then
                    inContextEditSessionByAssemblyPath.Remove(assemblyKey)
                    Exit Sub
                End If

                'Keep the just-finished child path remembered briefly - the ModifyNotify for
                'the edit just made can arrive slightly after EndInContextEditNotify fires.
                session.EndedUtc = DateTime.UtcNow
                parentWasCleanBeforeChildEdit = Not session.AssemblyWasDirtyBeforeEdit
                endedSession = session
            End SyncLock

            If endedSession IsNot Nothing AndAlso endedSession.ParentTemporarilyWritable Then
                Dim restoreAction As New MethodInvoker(
                    Sub()
                        restoreReadOnlyAfterInContextExit(
                            assemblyKey,
                            assemblyDocument,
                            endedSession.ParentOriginalAttributes,
                            endedSession.ParentHadOriginalAttributes,
                            0,
                            clearTransitionFlag:=False,
                            restoreExactDiskAttributes:=True
                        )
                    End Sub
                )

                If myUserControl IsNot Nothing AndAlso
                   Not myUserControl.IsDisposed AndAlso
                   myUserControl.IsHandleCreated Then
                    myUserControl.BeginInvoke(restoreAction)
                Else
                    restoreAction.Invoke()
                End If
            End If

            If parentWasCleanBeforeChildEdit Then
                'ModifyNotify can arrive just after EndInContextEditNotify and set the parent
                'SaveFlag a moment later. Mark the event-proven candidate now; it has no effect
                'unless the parent later reports dirty AND remains locally SVN-clean. Any real
                'assembly-owned edit clears it before close.
                markAssemblyGuardFalseDirtyCandidate(assemblyDocument)
                writeOperationLog(
                    "Parent assembly eligible for child-edit SaveFlag classification: " & assemblyKey
                )
            End If
        Catch
        End Try
    End Sub

    Public Sub clearInContextEditSessionPublic(ByVal assemblyDocument As ModelDoc2)
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub

        SyncLock assemblyGuardSync
            inContextEditSessionByAssemblyPath.Remove(assemblyKey)
            pendingInContextDirtyBaselineByAssemblyPath.Remove(assemblyKey)
            assemblyDisplayOnlyChangeUtcByPath.Remove(assemblyKey)
            assemblyRebuildPaths.Remove(assemblyKey)
            completedAssemblyRebuildModifyUtcByPath.Remove(assemblyKey)
            inContextExitTransitionQueuedPaths.Remove(assemblyKey)
        End SyncLock
    End Sub

    Public Sub noteAssemblyDisplayOnlyChangePublic(ByVal assemblyDocument As ModelDoc2)
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Exit Sub

        SyncLock assemblyGuardSync
            assemblyDisplayOnlyChangeUtcByPath(assemblyKey) = DateTime.UtcNow
        End SyncLock
    End Sub

    Private Function hasRecentAssemblyDisplayOnlyChange(ByVal assemblyDocument As ModelDoc2) As Boolean
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Return False

        SyncLock assemblyGuardSync
            Dim notedUtc As DateTime = DateTime.MinValue
            If Not assemblyDisplayOnlyChangeUtcByPath.TryGetValue(assemblyKey, notedUtc) Then Return False

            If (DateTime.UtcNow - notedUtc).TotalMilliseconds <= DISPLAY_ONLY_MODIFY_GRACE_MILLISECONDS Then
                Return True
            End If

            assemblyDisplayOnlyChangeUtcByPath.Remove(assemblyKey)
            Return False
        End SyncLock
    End Function

    Private Function assemblyContainsPhysicalChildPathSafe(ByVal assemblyDocument As ModelDoc2,
                                                            ByVal childPath As String) As Boolean
        If assemblyDocument Is Nothing OrElse String.IsNullOrWhiteSpace(childPath) Then Return False

        Dim assemblyDoc As AssemblyDoc = TryCast(assemblyDocument, AssemblyDoc)
        If assemblyDoc Is Nothing Then Return False

        Dim componentsObject As Object = Nothing
        Try
            'False includes every loaded assembly depth, which lets a middle-assembly event be
            'matched to the same locked part being edited through a higher assembly window.
            componentsObject = assemblyDoc.GetComponents(False)
        Catch
            componentsObject = Nothing
        End Try

        Dim components As Array = TryCast(componentsObject, Array)
        If components Is Nothing Then Return False

        For Each componentObject As Object In components
            Dim component As Component2 = TryCast(componentObject, Component2)
            If component Is Nothing OrElse isComponentVirtualSafe(component) Then Continue For

            Dim componentPath As String = ""
            Try
                componentPath = component.GetPathName()
            Catch
                componentPath = ""
            End Try

            If Not String.IsNullOrWhiteSpace(componentPath) AndAlso pathsAreSame(componentPath, childPath) Then
                Return True
            End If
        Next

        Return False
    End Function

    Private Function getInContextEditChildPath(ByVal assemblyDocument As ModelDoc2,
                                                ByVal allowRecentlyEndedEdit As Boolean) As String
        Dim assemblyKey As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyKey) Then Return ""

        'Before BeginInContextEditNotify has propagated to every referenced assembly handler,
        'the active top-level assembly can already expose the authoritative edit target. Use
        'that live state to classify an immediate child-driven ModifyNotify on a middle owner.
        If allowRecentlyEndedEdit AndAlso iSwApp IsNot Nothing Then
            Try
                Dim activeAssembly As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)

                If activeAssembly IsNot Nothing AndAlso
                   activeAssembly.GetType() = swDocumentTypes_e.swDocASSEMBLY Then

                    Dim activeTarget As ModelDoc2 = getAssemblyEditTargetDocumentSafe(activeAssembly)
                    If activeTarget IsNot Nothing Then
                        Dim activeTargetPath As String = ""
                        Try
                            activeTargetPath = activeTarget.GetPathName()
                        Catch
                            activeTargetPath = ""
                        End Try

                        Dim activeLockPath As String = getInContextEffectiveLockPath(activeTarget, activeTargetPath)
                        If Not String.IsNullOrWhiteSpace(activeLockPath) AndAlso
                           assemblyContainsPhysicalChildPathSafe(assemblyDocument, activeLockPath) Then
                            Return activeLockPath
                        End If
                    End If
                End If
            Catch
            End Try
        End If

        Dim crossAssemblyLockPaths As New List(Of String)()

        SyncLock assemblyGuardSync
            Dim session As InContextEditSession = Nothing
            If Not inContextEditSessionByAssemblyPath.TryGetValue(assemblyKey, session) OrElse session Is Nothing Then
                If allowRecentlyEndedEdit Then
                    For Each candidate As InContextEditSession In inContextEditSessionByAssemblyPath.Values
                        If candidate Is Nothing OrElse candidate.EndedUtc <> DateTime.MinValue Then Continue For
                        If (DateTime.UtcNow - candidate.BeganUtc).TotalHours > 4.0 Then Continue For

                        Dim candidateLockPath As String = candidate.LockPath
                        If String.IsNullOrWhiteSpace(candidateLockPath) Then candidateLockPath = candidate.ChildPath
                        If Not String.IsNullOrWhiteSpace(candidateLockPath) Then crossAssemblyLockPaths.Add(candidateLockPath)
                    Next
                End If
            Else

                'GetEditTarget already handles a normal active edit before this helper is called.
                'Use the tracked active session only for the same generic ModifyNotify fallback
                'as an ended session, and only immediately after Begin. Explicit component/add
                'events must never borrow this broader fallback.
                If session.EndedUtc = DateTime.MinValue Then
                    Dim ageSeconds As Double = (DateTime.UtcNow - session.BeganUtc).TotalSeconds

                    'An active BeginInContextEditNotify session is process state, not a short-lived
                    'timing hint. A user can legitimately remain in Edit Part/Edit Assembly for
                    'hours; while that session is active, generic ModifyNotify events on any owner
                    'assembly belong to the separately file-backed child. Explicit structural
                    'events never request this fallback and therefore remain guarded normally.
                    If allowRecentlyEndedEdit AndAlso ageSeconds >= 0 AndAlso ageSeconds <= 4.0 * 60.0 * 60.0 Then
                        Return If(String.IsNullOrWhiteSpace(session.LockPath), session.ChildPath, session.LockPath)
                    End If

                    'Keep a legitimate long-running edit registered so its eventual End event
                    'can create the narrow two-second grace. Only purge a clearly orphaned entry.
                    If ageSeconds > 4.0 * 60.0 * 60.0 Then
                        inContextEditSessionByAssemblyPath.Remove(assemblyKey)
                    End If
                Else
                    'Only ModifyNotify is allowed to claim this one-shot grace. Explicit assembly
                    'events such as component move/add must never inherit a child edit that ended.
                    If allowRecentlyEndedEdit AndAlso
                       (DateTime.UtcNow - session.EndedUtc).TotalSeconds <= 2.0 Then
                        Dim childPath As String = If(
                            String.IsNullOrWhiteSpace(session.LockPath),
                            session.ChildPath,
                            session.LockPath
                        )
                        inContextEditSessionByAssemblyPath.Remove(assemblyKey)
                        Return childPath
                    End If

                    If (DateTime.UtcNow - session.EndedUtc).TotalSeconds > 2.0 Then
                        inContextEditSessionByAssemblyPath.Remove(assemblyKey)
                    End If
                End If
            End If
        End SyncLock

        'SOLIDWORKS can report the child-driven rebuild on an intermediate assembly instead
        'of the assembly that emitted BeginInContextEditNotify. This fallback applies only to
        'generic ModifyNotify and only when that assembly really contains the locked child.
        For Each lockPath As String In crossAssemblyLockPaths.Distinct(StringComparer.OrdinalIgnoreCase)
            If assemblyContainsPhysicalChildPathSafe(assemblyDocument, lockPath) Then Return lockPath
        Next

        Return ""
    End Function

    Private Sub markAssemblyGuardFalseDirtyCandidate(ByVal assemblyDocument As ModelDoc2)
        Dim assemblyPath As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyPath) Then Exit Sub

        SyncLock assemblyGuardSync
            assemblyGuardFalseDirtyCandidatePaths.Add(assemblyPath)
        End SyncLock
    End Sub

    Private Sub clearAssemblyGuardFalseDirtyCandidate(ByVal assemblyDocument As ModelDoc2)
        Dim assemblyPath As String = getAssemblyPathKeySafe(assemblyDocument)
        If String.IsNullOrWhiteSpace(assemblyPath) Then Exit Sub

        SyncLock assemblyGuardSync
            assemblyGuardFalseDirtyCandidatePaths.Remove(assemblyPath)
        End SyncLock
    End Sub

    Private Function isAssemblyGuardFalseDirtyCandidate(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim key As String = filePath

        Try
            key = Path.GetFullPath(filePath)
        Catch
        End Try

        SyncLock assemblyGuardSync
            Return assemblyGuardFalseDirtyCandidatePaths.Contains(key)
        End SyncLock
    End Function

    Private Function isSvnPathLocallyClean(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not File.Exists(filePath) Then Return False
        If Not isPathInsideLocalRepo(filePath) Then Return False

        Try
            Dim statusResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "status --non-interactive --depth empty """ & filePath & """"
            )

            Dim errorText As String = ""
            If statusResult.outputError IsNot Nothing Then errorText = statusResult.outputError.Trim()
            If errorText <> "" Then Return False

            Dim outputText As String = ""
            If statusResult.output IsNot Nothing Then outputText = statusResult.output

            If String.IsNullOrWhiteSpace(outputText) Then Return True

            Dim lines() As String = outputText.Split(
                New String() {vbCrLf, vbLf},
                StringSplitOptions.RemoveEmptyEntries
            )

            For Each line As String In lines
                If String.IsNullOrWhiteSpace(line) Then Continue For
                If line.StartsWith("Status against revision", StringComparison.OrdinalIgnoreCase) Then Continue For

                Dim workingCopyState As Char = If(line.Length >= 1, line(0), " "c)
                Dim propertyState As Char = If(line.Length >= 2, line(1), " "c)
                Dim treeConflictState As Char = If(line.Length >= 7, line(6), " "c)

                'A clean locked file can still produce a status row because column 6 is K.
                'Only working-copy, property, or conflict changes make the path unsafe.
                If workingCopyState <> " "c OrElse propertyState <> " "c OrElse treeConflictState <> " "c Then
                    Return False
                End If
            Next

            Return True
        Catch
            Return False
        End Try
    End Function

    Private Function canTreatAssemblySaveFlagAsGuardGenerated(ByVal document As ModelDoc2,
                                                               ByVal filePath As String) As Boolean
        If document Is Nothing Then Return False
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Try
            If document.GetType() = swDocumentTypes_e.swDocASSEMBLY Then
                If isAssemblyGuardFalseDirtyCandidate(filePath) AndAlso isSvnPathLocallyClean(filePath) Then
                    Return True
                End If
            End If
        Catch
            Return False
        End Try

        'Product policy: an existing managed file without this working copy's SVN lock token
        'is not a committable document. SOLIDWORKS can still mark it dirty in memory because of
        'dependency rebuilds, cancelled save dialogs, view updates, or even temporary read-only
        'experimentation, but PlumVault must neither save it nor report it as an owned change
        'that needs committing before close. The lock token is authoritative; relying on the OS
        'read-only bit here is unsafe because SOLIDWORKS or an earlier writable-state transition
        'can leave that bit out of sync. New/unversioned files remain protected because
        'userHasSvnLockOnDoc deliberately treats them as valid first-commit candidates.
        Try
            If Not isPathInsideLocalRepo(filePath) Then Return False
            If Not File.Exists(filePath) Then Return False
            If userHasSvnLockOnDoc(document) Then Return False

            Return True
        Catch
            Return False
        End Try
    End Function

    Private Function hasAnyOpenSolidWorksDocument() As Boolean
        If iSwApp Is Nothing Then Return False

        Dim documentCountQueryFailed As Boolean = False

        Try
            If iSwApp.GetDocumentCount() > 0 Then Return True
        Catch
            documentCountQueryFailed = True
        End Try

        Try
            Dim docsObj As Object = iSwApp.GetDocuments()
            If docsObj Is Nothing Then Return documentCountQueryFailed

            Dim docs As Object() = CType(docsObj, Object())
            Return docs IsNot Nothing AndAlso docs.Length > 0
        Catch
            'During controlled application close, an unreadable COM document collection must
            'not be treated as proof that every document is gone.  Assume one remains so the
            'watchdog can retry/recover instead of silently leaving the close state half-open.
            Return documentCountQueryFailed
        End Try
    End Function

    Private Function queueGuardGeneratedFalseDirtyDocumentClose(ByVal filePath As String) As Boolean
        If iSwApp Is Nothing OrElse myUserControl Is Nothing Then Return False
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim normalizedPath As String = filePath
        Try
            normalizedPath = Path.GetFullPath(filePath)
        Catch
        End Try

        SyncLock assemblyGuardSync
            If assemblyGuardControlledCloseQueuedPaths.Contains(normalizedPath) Then Return True
            addControlledCloseQueuedPathLocked(normalizedPath)
        End SyncLock

        Dim closeAction As New System.Windows.Forms.MethodInvoker(
            Sub()
                Try
                    Dim currentDoc As ModelDoc2 = getOpenModelByPathSafe(normalizedPath)
                    If currentDoc Is Nothing Then Exit Sub

                    'Re-verify at execution time. A real edit made after the guard warning must
                    'never be discarded by this controlled-close path.
                    If Not canTreatAssemblySaveFlagAsGuardGenerated(currentDoc, normalizedPath) Then Exit Sub

                    Dim documentName As String = ""
                    Try
                        documentName = Path.GetFileName(normalizedPath)
                    Catch
                        documentName = ""
                    End Try

                    If String.IsNullOrWhiteSpace(documentName) Then
                        Try
                            documentName = currentDoc.GetTitle()
                        Catch
                            documentName = normalizedPath
                        End Try
                    End If

                    'QuitDoc is the SOLIDWORKS API close-without-saving operation. The original
                    'native close message has already been swallowed, so SOLIDWORKS does not show
                    'its false Save Modified Documents prompt for this verified-clean assembly.
                    documentLockReviewApprovedPath = normalizedPath
                    documentLockReviewApprovedUntil = DateTime.Now.AddSeconds(10)
                    iSwApp.QuitDoc(documentName)

                    SyncLock assemblyGuardSync
                        assemblyGuardFalseDirtyCandidatePaths.Remove(normalizedPath)
                    End SyncLock
                Catch
                    'If SOLIDWORKS refuses the controlled close, leave the document open.
                Finally
                    SyncLock assemblyGuardSync
                        removeControlledCloseQueuedPathLocked(normalizedPath)
                    End SyncLock
                End Try
            End Sub
        )

        Try
            If Not myUserControl.IsDisposed AndAlso myUserControl.IsHandleCreated Then
                myUserControl.BeginInvoke(closeAction)
                Return True
            End If
        Catch
        End Try

        SyncLock assemblyGuardSync
            removeControlledCloseQueuedPathLocked(normalizedPath)
        End SyncLock

        Return False
    End Function

    Private Function queueUserApprovedDocumentCloseWithoutSave(ByVal filePath As String,
                                                                Optional ByVal allowWithoutCloseReview As Boolean = False) As Boolean
        If iSwApp Is Nothing OrElse myUserControl Is Nothing Then Return False
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim normalizedPath As String = normalizeFullPathSafe(filePath)
        If Not allowWithoutCloseReview AndAlso Not documentCloseReviewIsApproved(normalizedPath) Then Return False

        'The reviewed decision may be followed by several UI turns while SOLIDWORKS leaves a
        'nested Edit Part/Edit Assembly context. Remember only a proven-clean baseline so a
        'new edit made during that gap is not silently discarded. Files already dirty at the
        'decision point were explicitly covered by the user's close-without-saving choice.
        Dim mustRemainClean As Boolean = False
        Try
            Dim queuedDocument As ModelDoc2 = getOpenModelByPathSafe(normalizedPath)
            If queuedDocument IsNot Nothing Then mustRemainClean = Not queuedDocument.GetSaveFlag()
        Catch
            'A missing baseline preserves the existing reviewed-close behavior.
        End Try

        SyncLock assemblyGuardSync
            If assemblyGuardControlledCloseQueuedPaths.Contains(normalizedPath) Then Return True
            addControlledCloseQueuedPathLocked(normalizedPath)
        End SyncLock

        Dim closeAction As New MethodInvoker(
            Sub()
                continueUserApprovedDocumentCloseWithoutSave(normalizedPath, mustRemainClean, 0, "", 0)
            End Sub
        )

        Try
            If Not myUserControl.IsDisposed AndAlso myUserControl.IsHandleCreated Then
                myUserControl.BeginInvoke(closeAction)
                Return True
            End If
        Catch
        End Try

        finishUserApprovedDocumentClose(normalizedPath)

        Return False
    End Function

    Private Sub continueUserApprovedDocumentCloseWithoutSave(ByVal normalizedPath As String,
                                                               ByVal mustRemainClean As Boolean,
                                                               ByVal attempt As Integer,
                                                               ByVal previousContextSignature As String,
                                                               ByVal repeatedContextCount As Integer)
        Try
            Dim currentDoc As ModelDoc2 = getOpenModelByPathSafe(normalizedPath)
            If currentDoc Is Nothing Then
                finishUserApprovedDocumentClose(normalizedPath)
                Exit Sub
            End If

            Dim ownerToExit As ModelDoc2 = Nothing
            Dim contextSignature As String = ""

            If tryGetInContextOwnerBlockingClose(normalizedPath, ownerToExit, contextSignature) Then
                Dim sameContext As Boolean = String.Equals(
                    contextSignature,
                    previousContextSignature,
                    StringComparison.OrdinalIgnoreCase
                )
                Dim nextRepeatedCount As Integer = If(sameContext, repeatedContextCount + 1, 0)

                If attempt >= 64 OrElse nextRepeatedCount >= 24 Then
                    Throw New InvalidOperationException(
                        "SOLIDWORKS did not finish leaving Edit Part/Edit Assembly mode. " &
                        "PlumVault left the document open so no work is lost."
                    )
                End If

                If Not sameContext OrElse nextRepeatedCount Mod 4 = 0 Then
                    activateAssemblyForContextExit(ownerToExit, contextSignature)
                    exitAssemblyInContextEditWithoutSavingParent(ownerToExit)
                    writeOperationLog("Queued in-context unwind before reviewed close: " & contextSignature)
                End If

                If myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then
                    Throw New InvalidOperationException("The PlumVault task pane closed before SOLIDWORKS completed the document close.")
                End If

                myUserControl.BeginInvoke(New MethodInvoker(Sub() continueUserApprovedDocumentCloseWithoutSave(normalizedPath, mustRemainClean, attempt + 1, contextSignature, nextRepeatedCount)))
                Exit Sub
            End If

            If mustRemainClean Then
                Dim becameDirty As Boolean = False

                Try
                    becameDirty = currentDoc.GetSaveFlag()
                Catch
                    becameDirty = False
                End Try

                If becameDirty AndAlso Not isAssemblyGuardFalseDirtyCandidate(normalizedPath) Then
                    Throw New InvalidOperationException(
                        Path.GetFileName(normalizedPath) & " changed after the close decision. " &
                        "PlumVault left it open so the latest change can be reviewed. Close it again when ready."
                    )
                End If
            End If

            Dim documentName As String = ""

            Try
                documentName = Path.GetFileName(normalizedPath)
            Catch
                documentName = ""
            End Try

            If String.IsNullOrWhiteSpace(documentName) Then
                Try
                    documentName = currentDoc.GetTitle()
                Catch
                    documentName = ""
                End Try
            End If

            'CloseDoc is reached only after every owner/target edit relationship involving
            'this exact file has disappeared. Referenced documents may remain loaded invisibly.
            controlledDocumentCloseNativeCallInProgress = True
            Try
                iSwApp.CloseDoc(documentName)
            Finally
                controlledDocumentCloseNativeCallInProgress = False
            End Try

            Dim verifyCloseAction As New MethodInvoker(
                Sub()
                    Try
                        If cadPathStillHasVisibleDocumentContext(normalizedPath) Then
                            iSwApp.SendMsgToUser2(
                                "SOLIDWORKS could not close " & Path.GetFileName(normalizedPath) & "." & vbCrLf & vbCrLf &
                                "Its document window or in-context edit is still active. Review it and try again.",
                                swMessageBoxIcon_e.swMbWarning,
                                swMessageBoxBtn_e.swMbOk
                            )
                        End If
                    Finally
                        finishUserApprovedDocumentClose(normalizedPath)
                    End Try
                End Sub
            )

            If Not myUserControl.IsDisposed AndAlso myUserControl.IsHandleCreated Then
                myUserControl.BeginInvoke(verifyCloseAction)
            Else
                verifyCloseAction.Invoke()
            End If
        Catch ex As Exception
            writeOperationLog("Reviewed document close stopped: " & normalizedPath & " | " & ex.Message)

            Try
                iSwApp.SendMsgToUser2(
                    "SOLIDWORKS could not complete the reviewed document close." & vbCrLf & vbCrLf & ex.Message,
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            finishUserApprovedDocumentClose(normalizedPath)
        End Try
    End Sub

    Private Function tryGetInContextOwnerBlockingClose(ByVal normalizedPath As String,
                                                         ByRef ownerToExit As ModelDoc2,
                                                         ByRef contextSignature As String,
                                                         Optional ByVal matchAnyActiveContext As Boolean = False) As Boolean
        ownerToExit = Nothing
        contextSignature = ""
        If iSwApp Is Nothing Then Return False
        If Not matchAnyActiveContext AndAlso String.IsNullOrWhiteSpace(normalizedPath) Then Return False

        Dim documents As Object() = Nothing

        Try
            documents = TryCast(iSwApp.GetDocuments(), Object())
        Catch
            documents = Nothing
        End Try

        If documents Is Nothing Then Return False

        For Each documentObject As Object In documents
            Dim candidate As ModelDoc2 = TryCast(documentObject, ModelDoc2)
            If candidate Is Nothing Then Continue For

            Try
                If candidate.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
            Catch
                Continue For
            End Try

            Dim editTarget As ModelDoc2 = getAssemblyEditTargetDocumentSafe(candidate)
            If editTarget Is Nothing Then Continue For

            Dim ownerPath As String = getAssemblyPathKeySafe(candidate)
            Dim targetPath As String = ""

            Try
                targetPath = normalizeFullPathSafe(editTarget.GetPathName())
            Catch
                targetPath = ""
            End Try

            If matchAnyActiveContext OrElse
               (Not String.IsNullOrWhiteSpace(ownerPath) AndAlso pathsAreSame(ownerPath, normalizedPath)) OrElse
               (Not String.IsNullOrWhiteSpace(targetPath) AndAlso pathsAreSame(targetPath, normalizedPath)) Then
                ownerToExit = candidate
                contextSignature = ownerPath & "|" & targetPath
                Return True
            End If
        Next

        Return False
    End Function

    Private Sub finishUserApprovedDocumentClose(ByVal normalizedPath As String)
        controlledDocumentCloseNativeCallInProgress = False

        If pathsAreSame(documentLockReviewApprovedPath, normalizedPath) Then
            documentLockReviewApprovedUntil = DateTime.MinValue
            documentLockReviewApprovedPath = ""
            documentLockReviewApprovedPaths.Clear()
        End If

        SyncLock assemblyGuardSync
            removeControlledCloseQueuedPathLocked(normalizedPath)
            assemblyGuardFalseDirtyCandidatePaths.Remove(normalizedPath)
        End SyncLock

        resumeRequestedApplicationCloseAfterDocumentClose()
    End Sub

    Private Sub resumeRequestedApplicationCloseAfterDocumentClose()
        If Not applicationCloseRequestedAfterDocumentClose Then Exit Sub
        If hasFreshControlledCloseQueuedPaths() Then Exit Sub
        If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then Exit Sub

        applicationCloseRequestedAfterDocumentClose = False

        Try
            myUserControl.BeginInvoke(
                New MethodInvoker(
                    Sub()
                        Try
                            'Re-run every application-close safety check after the document
                            'close has actually finished. Normally this queues the verified
                            'application close and returns True.
                            If blockCloseIfOpenDocsUnsafe() Then Exit Sub

                            controlledApplicationNativeCloseCallInProgress = True
                            Try
                                iSwApp.ExitApp()
                            Finally
                                controlledApplicationNativeCloseCallInProgress = False
                            End Try
                        Catch ex As Exception
                            applicationCloseRequestedAfterDocumentClose = False
                            writeOperationLog("Deferred application close failed: " & ex.Message)
                        End Try
                    End Sub
                )
            )
        Catch
            applicationCloseRequestedAfterDocumentClose = False
        End Try
    End Sub

    Private Sub showManualLockRequired(ByVal filePath As String,
                                        ByVal actionDescription As String)
        Dim fileName As String = "the selected CAD file"

        Try
            If Not String.IsNullOrWhiteSpace(filePath) Then fileName = Path.GetFileName(filePath)
        Catch
        End Try

        Dim actionText As String = "make this change"
        If Not String.IsNullOrWhiteSpace(actionDescription) Then actionText = actionDescription.Trim()

        Try
            iSwApp.SendMsgToUser2(
                "Please select Get Locks first." & vbCrLf & vbCrLf &
                fileName & " is not locked by you, so PlumVault stopped this action before it changed the file." & vbCrLf & vbCrLf &
                "Requested action: " & actionText & vbCrLf & vbCrLf &
                "Select this file in the SVN tree, click Get Locks, then try again." & vbCrLf & vbCrLf &
                "If the SVN tree already shows this file as locked by you, click Sync to refresh, then retry.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
        Catch
        End Try
    End Sub

    Private Function waitForCurrentGetLocksBeforeEnforcingInContextEdit(ByVal assemblyDocument As ModelDoc2,
                                                                        ByVal editedDocument As ModelDoc2,
                                                                        ByVal assemblyKey As String,
                                                                        ByVal childPath As String,
                                                                        ByVal lockPath As String) As Boolean
        If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse
           Not myUserControl.IsHandleCreated Then Return False

        Dim startedUtc As DateTime = DateTime.UtcNow
        Dim waitTimer As System.Windows.Forms.Timer = Nothing
        waitTimer = New System.Windows.Forms.Timer() With {.Interval = 100}

        AddHandler waitTimer.Tick,
            Sub(sender As Object, e As EventArgs)
                Try
                    If asyncGetLocksInProgress AndAlso asyncGetLocksIncludesPath(lockPath) AndAlso
                       (DateTime.UtcNow - startedUtc).TotalSeconds < 120.0 Then Exit Sub

                    waitTimer.Stop()
                    waitTimer.Dispose()

                    Dim currentSession As InContextEditSession = Nothing
                    SyncLock assemblyGuardSync
                        If Not inContextEditSessionByAssemblyPath.TryGetValue(assemblyKey, currentSession) Then Exit Sub
                        If currentSession Is Nothing OrElse currentSession.EndedUtc <> DateTime.MinValue Then Exit Sub
                        If Not pathsAreSame(currentSession.ChildPath, childPath) Then Exit Sub
                    End SyncLock

                    Dim currentChild As ModelDoc2 = getOpenModelByPathSafe(childPath)
                    If currentChild Is Nothing Then currentChild = editedDocument

                    If inContextEditTargetHasRequiredLock(currentChild, lockPath) Then
                        Dim writableFailure As String = ""
                        Dim writableDocument As ModelDoc2 = currentChild
                        If Not pathsAreSame(childPath, lockPath) Then
                            writableDocument = getOpenModelByPathSafe(lockPath)
                            If writableDocument Is Nothing Then writableDocument = assemblyDocument
                        End If

                        If Not ensureOpenCadDocumentWritableNow(lockPath, writableDocument, writableFailure) Then
                            Dim failedAssembly As ModelDoc2 = getOpenModelByPathSafe(assemblyKey)
                            If failedAssembly Is Nothing Then failedAssembly = assemblyDocument
                            If failedAssembly IsNot Nothing Then exitAssemblyInContextEditWithoutSavingParent(failedAssembly)
                            showInContextAutoEditFailure(lockPath, writableFailure, writeAccessWasObtained:=True)
                            Exit Sub
                        End If

                        Dim currentAssembly As ModelDoc2 = getOpenModelByPathSafe(assemblyKey)
                        If currentAssembly Is Nothing Then currentAssembly = assemblyDocument
                        If currentAssembly IsNot Nothing Then
                            prepareInContextParentForCleanExit(currentAssembly, assemblyKey, currentSession)
                        End If

                        writeOperationLog(
                            "In-context edit retained after pending Get Locks succeeded: " & childPath
                        )
                        Exit Sub
                    End If

                    Dim assemblyToExit As ModelDoc2 = getOpenModelByPathSafe(assemblyKey)
                    If assemblyToExit Is Nothing Then assemblyToExit = assemblyDocument
                    If assemblyToExit IsNot Nothing Then exitAssemblyInContextEditWithoutSavingParent(assemblyToExit)

                    writeOperationLog(
                        "In-context edit exited after pending Get Locks failed or timed out: " & childPath
                    )
                Catch ex As Exception
                    Try
                        waitTimer.Stop()
                        waitTimer.Dispose()
                    Catch
                    End Try
                    writeOperationLog("Pending Edit Part lock verification failed: " & ex.Message)
                End Try
            End Sub

        writeOperationLog("In-context edit waiting for current Get Locks result: " & lockPath)
        waitTimer.Start()
        Return True
    End Function

    Private Function cadPathStillHasVisibleDocumentContext(ByVal filePath As String) As Boolean
        If iSwApp Is Nothing OrElse String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim normalizedPath As String = normalizeFullPathSafe(filePath)
        Dim loadedDocument As ModelDoc2 = getOpenModelByPathSafe(normalizedPath)

        If loadedDocument Is Nothing Then Return False

        'Visible is the important distinction: referenced models commonly remain loaded
        'after their own document window is gone. This works for parts, assemblies, and
        'drawings without relying on document titles that can contain "-in- <assembly>".
        Try
            If loadedDocument.Visible Then Return True
        Catch
        End Try

        Try
            Dim activeDocument As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDocument IsNot Nothing AndAlso
               pathsAreSame(activeDocument.GetPathName(), normalizedPath) Then Return True
        Catch
        End Try

        'An in-context child may report Visible=False because the assembly owns the native
        'window. Check every loaded assembly's edit target so a genuinely failed close is
        'not mistaken for a harmless hidden referenced model.
        Try
            Dim documentsObject As Object = iSwApp.GetDocuments()
            Dim documents As Object() = TryCast(documentsObject, Object())

            If documents IsNot Nothing Then
                For Each documentObject As Object In documents
                    Dim candidate As ModelDoc2 = TryCast(documentObject, ModelDoc2)
                    If candidate Is Nothing Then Continue For

                    Try
                        If candidate.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
                    Catch
                        Continue For
                    End Try

                    Dim editTarget As ModelDoc2 = getAssemblyEditTargetDocumentSafe(candidate)
                    If editTarget Is Nothing Then Continue For

                    Try
                        If pathsAreSame(editTarget.GetPathName(), normalizedPath) Then Return True
                    Catch
                    End Try
                Next
            End If
        Catch
        End Try

        Return False
    End Function

    Private Function assemblyIsEditingExternalPhysicalChild(ByVal assemblyDocument As ModelDoc2,
                                                             Optional ByVal allowLockedChildDimensionFallback As Boolean = False,
                                                             Optional ByVal allowRecentlyEndedInContextEdit As Boolean = False) As Boolean
        If assemblyDocument Is Nothing Then Return False

        Dim assemblyPath As String = ""

        Try
            assemblyPath = assemblyDocument.GetPathName()
        Catch
            assemblyPath = ""
        End Try

        Dim editTarget As ModelDoc2 = getAssemblyEditTargetDocumentSafe(assemblyDocument)

        If editTarget IsNot Nothing Then
            Dim targetPath As String = ""

            Try
                targetPath = editTarget.GetPathName()
            Catch
                targetPath = ""
            End Try

            If Not String.IsNullOrWhiteSpace(assemblyPath) AndAlso
               Not String.IsNullOrWhiteSpace(targetPath) AndAlso
               pathsAreSame(assemblyPath, targetPath) Then
                targetPath = ""
            End If

            If Not String.IsNullOrWhiteSpace(targetPath) Then
                'Virtual documents are stored by the physical assembly and therefore require
                'the assembly lock. They commonly have a temporary/AppData path or a title
                'containing a caret.
                Try
                    Dim ownerPath As String = getOwningPhysicalAssemblyPathForVirtualDocument(editTarget)
                    If Not String.IsNullOrWhiteSpace(ownerPath) Then targetPath = ""
                Catch
                End Try
            End If

            If Not String.IsNullOrWhiteSpace(targetPath) AndAlso isSolidWorksTempOrVirtualPath(targetPath) Then
                targetPath = ""
            End If

            If Not String.IsNullOrWhiteSpace(targetPath) Then
                Dim targetTitle As String = ""

                Try
                    targetTitle = editTarget.GetTitle()
                Catch
                    targetTitle = ""
                End Try

                If targetTitle.Contains("^") AndAlso Not isPathInsideLocalRepo(targetPath) Then
                    targetPath = ""
                End If
            End If

            'GetEditTarget proves SOLIDWORKS is in an in-context edit on this external child -
            'it does not by itself prove the child is locked by the current user. Without this
            'check, being in Edit Component mode on ANY unlocked child (including one another
            'user holds) would silently exempt the parent assembly from its own lock
            'requirement, bypassing the guard entirely rather than just avoiding a false block.
            If Not String.IsNullOrWhiteSpace(targetPath) AndAlso
               externalChildPathHasRequiredLockFast(targetPath) Then Return True
        End If

        'SOLIDWORKS can clear GetEditTarget by the time ModifyNotify actually arrives -
        'confirmed with "Edit Part" in-context editing from the assembly window: exiting
        'edit mode raises ModifyNotify slightly after GetEditTarget has already gone back to
        'Nothing, so the check above alone falsely reports "no active child edit" and blocks
        'a fully authorized edit to a locked child. BeginInContextEditNotify/EndInContextEditNotify
        'are a purpose-built, more reliable signal for exactly this case. The ended-edit
        'grace is one-shot and is requested only by generic ModifyNotify; explicit assembly
        'events must not be mistaken for the child edit after the user exits Edit Part.
        Dim recentInContextChildPath As String = getInContextEditChildPath(
            assemblyDocument,
            allowRecentlyEndedInContextEdit
        )

        If Not String.IsNullOrWhiteSpace(recentInContextChildPath) AndAlso
           externalChildPathHasRequiredLockFast(recentInContextChildPath) Then
            Return True
        End If

        If allowLockedChildDimensionFallback Then
            'SOLIDWORKS can temporarily clear GetEditTarget while the dimension Modify dialog
            'opens. Use only a dimension selection owned by a separately file-backed child.
            'A normal selected component is deliberately insufficient because moving that
            'component is an assembly-owned edit and still requires the assembly lock.
            noteAssemblySelectionContextPublic(assemblyDocument)

            Dim recentChildPath As String = getRecentSelectedExternalChildPath(assemblyDocument)

            If Not String.IsNullOrWhiteSpace(recentChildPath) AndAlso
               externalChildPathHasRequiredLockFast(recentChildPath) Then
                Return True
            End If
        End If

        Return False
    End Function

    Private Function assemblyHasRequiredLockFast(ByVal assemblyDocument As ModelDoc2) As Boolean
        If assemblyDocument Is Nothing Then Return False

        Dim assemblyPath As String = ""

        Try
            assemblyPath = assemblyDocument.GetPathName()
        Catch
            assemblyPath = ""
        End Try

        'New or unmanaged assemblies are allowed to be created normally. Their first
        'managed save is still handled by the existing naming/add/commit workflow.
        If String.IsNullOrWhiteSpace(assemblyPath) Then Return True
        If Not isPathInsideLocalRepo(assemblyPath) Then Return True

        'Normal modelling actions must not launch svn.exe when the task-pane cache already
        'knows the answer. New Part/AddItem/Modify notifications can arrive several times
        'inside one native SOLIDWORKS transaction; the previous live-first order could freeze
        'the UI while those callbacks synchronously repeated local status scans.
        Try
            Dim cached As SVNStatus = findStatusForFile(assemblyPath)
            If cached IsNot Nothing AndAlso cached.fp IsNot Nothing AndAlso cached.fp.Length > 0 Then
                Return cached.fp(0).lock6 = "K" OrElse
                       cached.fp(0).addDelChg1 = "?" OrElse
                       cached.fp(0).addDelChg1 = "A"
            End If
        Catch
        End Try

        'Unknown cache state gets one local working-copy query. It never contacts the server
        'and obtains both first-commit and K-token state in the same svn.exe invocation.
        Dim hasLocalChanges As Boolean = False
        Dim hasLocalLockToken As Boolean = False
        Dim workingCopyState As Char = " "c
        Dim statusError As String = ""

        If tryGetLocalSvnChangeState(
            assemblyPath,
            hasLocalChanges,
            statusError,
            hasLocalLockToken,
            workingCopyState) Then

            Return hasLocalLockToken OrElse
                   workingCopyState = "?"c OrElse
                   workingCopyState = "A"c
        End If

        Return False
    End Function

    Private Function assemblyOwnedEditMustBeBlocked(ByVal assemblyDocument As ModelDoc2,
                                                      Optional ByVal allowLockedChildDimensionFallback As Boolean = False,
                                                      Optional ByVal ignoreActiveRebuild As Boolean = False,
                                                      Optional ByVal allowActiveChildEditContext As Boolean = False) As Boolean
        If assemblyDocument Is Nothing Then Return False
        If assemblyEditGuardSuppressed(assemblyDocument, ignoreActiveRebuild) Then Return False

        Try
            If assemblyDocument.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return False
        Catch
            Return False
        End Try

        'Do not let the parent assembly's lack of a lock interfere while a separately
        'file-backed child is being edited in context.
        If allowActiveChildEditContext AndAlso
           assemblyIsEditingExternalPhysicalChild(assemblyDocument, allowLockedChildDimensionFallback) Then Return False

        Return Not assemblyHasRequiredLockFast(assemblyDocument)
    End Function

    Private Sub showAssemblyLockRequiredMessage(ByVal assemblyDocument As ModelDoc2,
                                                 ByVal actionDescription As String,
                                                 Optional ByVal editWasUndone As Boolean = False,
                                                 Optional ByVal editWasBlockedBeforeChange As Boolean = False)
        Dim assemblyPath As String = ""
        Dim assemblyName As String = "the assembly"

        Try
            assemblyPath = assemblyDocument.GetPathName()
            If Not String.IsNullOrWhiteSpace(assemblyPath) Then assemblyName = Path.GetFileName(assemblyPath)
        Catch
        End Try

        'Several post-notifications can describe one SOLIDWORKS action. Suppress only
        'duplicate messages from that same action, not later user attempts.
        If pathsAreSame(lastAssemblyGuardMessagePath, assemblyPath) AndAlso
           (DateTime.UtcNow - lastAssemblyGuardMessageUtc).TotalMilliseconds < 750.0 Then
            Exit Sub
        End If

        lastAssemblyGuardMessagePath = assemblyPath
        lastAssemblyGuardMessageUtc = DateTime.UtcNow

        Dim firstLine As String = "Please select Get Locks first."

        Dim actionText As String = ""
        If Not String.IsNullOrWhiteSpace(actionDescription) Then
            actionText = vbCrLf & vbCrLf & "Attempted action: " & actionDescription
        End If

        Dim resultText As String
        If editWasBlockedBeforeChange Then
            resultText = "PlumVault stopped this operation before it changed the file."
        Else
            resultText =
                "SOLIDWORKS reported an assembly change after it occurred. PlumVault did not use automatic Undo, " &
                "so the change remains local and cannot be committed through PlumVault without the assembly lock." &
                vbCrLf & vbCrLf &
                "If the change was unintended, use Ctrl+Z yourself."
        End If

        Try
            iSwApp.SendMsgToUser2(
                firstLine & vbCrLf & vbCrLf &
                assemblyName & " is not locked by you." & vbCrLf & vbCrLf &
                resultText & actionText & vbCrLf & vbCrLf &
                "You may still hide/show components, change transparency for inspection, or edit a separately file-backed child in context when that child has its own lock." & vbCrLf & vbCrLf &
                "If the SVN tree already shows this file as locked by you, click Sync to refresh, then retry.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
        Catch
        End Try
    End Sub

    'A standalone part opened directly (not in-context through an assembly) had no equivalent
    'of the assembly-edit-protection guard - the OS read-only attribute was the only thing
    'standing between "not locked" and "can edit", and nothing re-verified it once SOLIDWORKS
    'had the document open. The post-event is warning-only because mutating SOLIDWORKS' undo
    'stack from a feature transaction can crash the process.
    Private lastPartGuardMessagePath As String = ""
    Private lastPartGuardMessageUtc As DateTime = DateTime.MinValue
    Private partGuardMessageQueued As Boolean = False

    'IMPORTANT - deliberately WARN-ONLY, does NOT call EditUndo2 on the part.
    'A first version of this guard called EditUndo2(1) here, mirroring the assembly guard.
    'Live testing produced a SolidWorks crash (access violation) when a second in-context
    'geometry edit followed an earlier undo on the same reopened part - ModifyNotify can fire
    'repeatedly while a feature (e.g. a Boss-Extrude drag) is still mid-interaction, unlike the
    'assembly events this pattern was copied from, which fire once per completed action.
    'Calling Undo against SolidWorks' feature-edit state machine mid-drag is not safe. Detecting
    'and warning without mutating the document is the safe subset of this protection; do not
    'restore the EditUndo2 call without confirming SolidWorks' interactive-edit behavior first.
    Public Sub handlePartOwnedEditPostPublic(ByVal partDocument As ModelDoc2, ByVal actionDescription As String)
        If partDocument Is Nothing Then Exit Sub
        If assemblyEditGuardSuppressed(partDocument) Then Exit Sub
        If assemblyHasRequiredLockFast(partDocument) Then Exit Sub
        If consumeAssemblyRebuildGenericModifyAllowance(partDocument) Then Exit Sub

        Dim partPath As String = getAssemblyPathKeySafe(partDocument)
        If String.IsNullOrWhiteSpace(partPath) Then Exit Sub

        'Rate-limited: ModifyNotify can fire many times during one interactive edit, and this
        'must never touch the document, so a dialog per fire would be an unusable spam storm.
        If pathsAreSame(lastPartGuardMessagePath, partPath) AndAlso
           (DateTime.UtcNow - lastPartGuardMessageUtc).TotalSeconds < 15.0 Then
            Exit Sub
        End If

        lastPartGuardMessagePath = partPath
        lastPartGuardMessageUtc = DateTime.UtcNow

        Dim actionText As String = ""
        If Not String.IsNullOrWhiteSpace(actionDescription) Then
            actionText = vbCrLf & vbCrLf & "Attempted action: " & actionDescription
        End If

        If partGuardMessageQueued Then Exit Sub
        If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse
           Not myUserControl.IsHandleCreated Then Exit Sub

        partGuardMessageQueued = True

        'Never open a modal SOLIDWORKS message from ModifyNotify. Feature editing can emit this
        'event while its geometry transaction is still active; re-entering SOLIDWORKS from that
        'callback caused intermittent access violations after a part was closed and reopened.
        Try
            myUserControl.BeginInvoke(
                New MethodInvoker(
                    Sub()
                        Try
                            iSwApp.SendMsgToUser2(
                                "Please select Get Locks first." & vbCrLf & vbCrLf &
                                Path.GetFileName(partPath) & " is not locked by you." & vbCrLf & vbCrLf &
                                "This change remains local and cannot be committed through PlumVault without the file lock. " &
                                "PlumVault did not use automatic Undo. If the change was unintended, use Ctrl+Z yourself." &
                                actionText & vbCrLf & vbCrLf &
                                "If the SVN tree already shows this file as locked by you, click Sync to refresh, then retry.",
                                swMessageBoxIcon_e.swMbInformation,
                                swMessageBoxBtn_e.swMbOk
                            )
                        Catch
                        Finally
                            partGuardMessageQueued = False
                        End Try
                    End Sub
                )
            )
        Catch
            partGuardMessageQueued = False
        End Try
    End Sub

    Public Function blockAssemblyOwnedEditPrePublic(ByVal assemblyDocument As ModelDoc2,
                                                     ByVal actionDescription As String) As Integer
        Dim childEditContext As Boolean = assemblyIsEditingExternalPhysicalChild(assemblyDocument)

        If Not assemblyOwnedEditMustBeBlocked(assemblyDocument, ignoreActiveRebuild:=True, allowActiveChildEditContext:=True) Then
            'A genuine assembly-owned edit made while the assembly is locked supersedes any
            'earlier guard-generated false-dirty candidate. Child edits do not touch it.
            If Not childEditContext Then clearAssemblyGuardFalseDirtyCandidate(assemblyDocument)
            Return 0
        End If

        showAssemblyLockRequiredMessage(
            assemblyDocument,
            actionDescription,
            editWasBlockedBeforeChange:=True
        )
        Return 1
    End Function

    'Deferred entry point for the drawing-open freshness check. SOLIDWORKS dependency discovery
    'runs on its UI thread; only stable file paths cross to the background SVN worker.
    Public Sub queueDrawingReferenceFreshnessCheckPublic(ByVal drawingDocument As ModelDoc2)
        Try
            If myUserControl Is Nothing Then Return
            If myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then Return

            myUserControl.BeginInvoke(
                New System.Windows.Forms.MethodInvoker(Sub() checkDrawingReferencesFreshnessPublic(drawingDocument))
            )
        Catch
        End Try
    End Sub

    'Starts a non-blocking server check whenever a drawing is opened. Reopening the same drawing
    'later in the SOLIDWORKS session intentionally checks again.
    Public Sub checkDrawingReferencesFreshnessPublic(ByVal drawingDocument As ModelDoc2)
        Dim normalizedDrawingPath As String = ""
        Dim freshnessCheckRegistered As Boolean = False

        Try
            If drawingDocument Is Nothing Then Return
            If iSwApp Is Nothing OrElse myUserControl Is Nothing Then Return

            Try
                If drawingDocument.GetType() <> swDocumentTypes_e.swDocDRAWING Then Return
            Catch
                Return
            End Try

            Dim drawingPath As String = ""
            Try
                drawingPath = drawingDocument.GetPathName()
            Catch
                drawingPath = ""
            End Try

            If String.IsNullOrWhiteSpace(drawingPath) Then Return
            If Not isPathInsideLocalRepo(drawingPath) Then Return

            Dim hasOutOfDateViews As Boolean = drawingHasOutOfDateViews(drawingDocument)

            If Not isOnlineModeEnabled() Then
                If hasOutOfDateViews Then
                    Dim localOnlyResult As New DrawingFreshnessChunkResult()
                    localOnlyResult.HasOutOfDateViews = True
                    finishDrawingReferenceFreshnessCheck(drawingPath, localOnlyResult)
                End If
                Return
            End If

            normalizedDrawingPath = drawingPath
            Try
                normalizedDrawingPath = Path.GetFullPath(drawingPath)
            Catch
            End Try

            SyncLock assemblyGuardSync
                If drawingFreshnessChecksInProgress.Contains(normalizedDrawingPath) Then Return
                drawingFreshnessChecksInProgress.Add(normalizedDrawingPath)
                freshnessCheckRegistered = True
            End SyncLock

            Dim referencedPaths As List(Of String) = myUserControl.getDrawingReferencedFilePaths(drawingDocument)
            If referencedPaths Is Nothing OrElse referencedPaths.Count = 0 Then
                SyncLock assemblyGuardSync
                    drawingFreshnessChecksInProgress.Remove(normalizedDrawingPath)
                End SyncLock

                If hasOutOfDateViews Then
                    Dim localOnlyResult As New DrawingFreshnessChunkResult()
                    localOnlyResult.HasOutOfDateViews = True
                    finishDrawingReferenceFreshnessCheck(normalizedDrawingPath, localOnlyResult)
                End If
                Return
            End If

            Dim referencedInRepo As New List(Of String)
            Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
            For Each refPath As String In referencedPaths
                If isPathInsideLocalRepo(refPath) AndAlso File.Exists(refPath) AndAlso seen.Add(refPath) Then
                    referencedInRepo.Add(refPath)
                End If
            Next

            If referencedInRepo.Count = 0 Then
                SyncLock assemblyGuardSync
                    drawingFreshnessChecksInProgress.Remove(normalizedDrawingPath)
                End SyncLock

                If hasOutOfDateViews Then
                    Dim localOnlyResult As New DrawingFreshnessChunkResult()
                    localOnlyResult.HasOutOfDateViews = True
                    finishDrawingReferenceFreshnessCheck(normalizedDrawingPath, localOnlyResult)
                End If
                Return
            End If

            Dim referencedPathArr() As String = referencedInRepo.ToArray()
            Dim savedPathForBackground As String = ""

            Try
                savedPathForBackground = myUserControl.savedPATH
            Catch
                savedPathForBackground = ""
            End Try

            Task.Run(
                Sub()
                    Dim result As DrawingFreshnessChunkResult =
                        getDrawingOutOfDatePathsBackground(referencedPathArr, savedPathForBackground)
                    result.HasOutOfDateViews = hasOutOfDateViews

                    Try
                        If myUserControl IsNot Nothing AndAlso
                           Not myUserControl.IsDisposed AndAlso
                           myUserControl.IsHandleCreated Then

                            myUserControl.BeginInvoke(
                                New MethodInvoker(
                                    Sub() finishDrawingReferenceFreshnessCheck(
                                        normalizedDrawingPath,
                                        result
                                    )
                                )
                            )
                        Else
                            SyncLock assemblyGuardSync
                                drawingFreshnessChecksInProgress.Remove(normalizedDrawingPath)
                            End SyncLock
                        End If
                    Catch
                        SyncLock assemblyGuardSync
                            drawingFreshnessChecksInProgress.Remove(normalizedDrawingPath)
                        End SyncLock
                    End Try
                End Sub
            )
        Catch ex As Exception
            If freshnessCheckRegistered Then
                SyncLock assemblyGuardSync
                    drawingFreshnessChecksInProgress.Remove(normalizedDrawingPath)
                End SyncLock
            End If

            writeOperationLog("Could not start drawing reference freshness check: " & ex.Message)
        End Try
    End Sub

    Private Function drawingHasOutOfDateViews(ByVal drawingDocument As ModelDoc2) As Boolean
        Try
            Dim drawing As DrawingDoc = TryCast(drawingDocument, DrawingDoc)
            If drawing Is Nothing Then Return False

            Dim sheetsObject As Object = drawing.GetViews()
            Dim sheets As Object() = TryCast(sheetsObject, Object())
            If sheets Is Nothing Then Return False

            For Each sheetObject As Object In sheets
                Dim views As Object() = TryCast(sheetObject, Object())
                If views Is Nothing OrElse views.Length <= 1 Then Continue For

                'Index 0 is the sheet pseudo-view. Real model views start at index 1.
                For viewIndex As Integer = 1 To views.Length - 1
                    Dim drawingView As SolidWorks.Interop.sldworks.View =
                        TryCast(views(viewIndex), SolidWorks.Interop.sldworks.View)
                    If drawingView Is Nothing Then Continue For

                    Try
                        If drawingView.IsModelOutOfDate() Then Return True
                    Catch
                    End Try
                Next
            Next
        Catch
        End Try

        Return False
    End Function

    Private Function getDrawingOutOfDatePathsBackground(ByVal referencedPaths() As String,
                                                         ByVal savedPathForBackground As String) As DrawingFreshnessChunkResult
        Dim combined As New DrawingFreshnessChunkResult()
        If referencedPaths Is Nothing OrElse referencedPaths.Length = 0 Then Return combined

        Try
            Dim chunks As List(Of String()) = chunkFilePathsForBackground(referencedPaths, 16)
            Dim parallelGate As New System.Threading.SemaphoreSlim(Math.Min(3, Math.Max(1, chunks.Count)))
            Dim tasks As New List(Of Task(Of DrawingFreshnessChunkResult))()

            For Each chunk As String() In chunks
                Dim chunkCopy As String() = CType(chunk.Clone(), String())

                tasks.Add(
                    Task.Run(
                        Function()
                            parallelGate.Wait()
                            Try
                                Dim chunkResult As New DrawingFreshnessChunkResult()
                                Dim errorMessage As String = ""
                                Dim stalePaths() As String = getOutOfDatePathsForAsyncLock(
                                    chunkCopy,
                                    errorMessage,
                                    savedPathForBackground,
                                    timeoutMilliseconds:=20000
                                )

                                chunkResult.ErrorMessage = errorMessage
                                If stalePaths IsNot Nothing Then chunkResult.OutOfDatePaths.AddRange(stalePaths)
                                Return chunkResult
                            Finally
                                parallelGate.Release()
                            End Try
                        End Function
                    )
                )
            Next

            Task.WaitAll(tasks.ToArray())
            parallelGate.Dispose()

            Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

            For Each task As Task(Of DrawingFreshnessChunkResult) In tasks
                Dim chunkResult As DrawingFreshnessChunkResult = task.Result
                If chunkResult Is Nothing Then Continue For

                If String.IsNullOrWhiteSpace(combined.ErrorMessage) AndAlso
                   Not String.IsNullOrWhiteSpace(chunkResult.ErrorMessage) Then
                    combined.ErrorMessage = chunkResult.ErrorMessage
                End If

                For Each stalePath As String In chunkResult.OutOfDatePaths
                    If seen.Add(stalePath) Then combined.OutOfDatePaths.Add(stalePath)
                Next
            Next
        Catch ex As Exception
            combined.ErrorMessage = ex.Message
        End Try

        Return combined
    End Function

    Private Sub finishDrawingReferenceFreshnessCheck(ByVal drawingPath As String,
                                                      ByVal result As DrawingFreshnessChunkResult)
        SyncLock assemblyGuardSync
            drawingFreshnessChecksInProgress.Remove(drawingPath)
        End SyncLock

        If result Is Nothing Then Return

        If Not String.IsNullOrWhiteSpace(result.ErrorMessage) Then
            writeOperationLog("Drawing reference freshness check did not complete: " & result.ErrorMessage)
            'The local SOLIDWORKS IsModelOutOfDate check is still useful even when SVN is
            'temporarily unreachable. Do not suppress that independent warning.
            result.OutOfDatePaths.Clear()
        End If

        'Do not show a stale result after the user has already closed the drawing.
        Dim openDrawing As ModelDoc2 = getOpenModelByPathSafe(drawingPath)
        If openDrawing Is Nothing Then Return

        Try
            If openDrawing.GetType() <> swDocumentTypes_e.swDocDRAWING Then Return
        Catch
            Return
        End Try

        'The server check can take a few seconds. Re-read SOLIDWORKS' live view state so an
        'automatic view update that completed meanwhile does not produce a stale warning.
        result.HasOutOfDateViews = drawingHasOutOfDateViews(openDrawing)

        Dim hasServerStaleReferences As Boolean =
            result.OutOfDatePaths IsNot Nothing AndAlso result.OutOfDatePaths.Count > 0

        If Not hasServerStaleReferences AndAlso Not result.HasOutOfDateViews Then Return

        Dim message As String = "This drawing's views may not reflect current geometry." & vbCrLf & vbCrLf

        If result.HasOutOfDateViews Then
            message &= "SOLIDWORKS reports that one or more drawing views are out of date with the local referenced models." & vbCrLf & vbCrLf
        End If

        If hasServerStaleReferences Then
            message &= "These referenced file(s) also have a newer version on the SVN server:" & vbCrLf &
                stringArrToSingleStringWithNewLines(result.OutOfDatePaths.ToArray(), bTrimFileNames:=True, iLimit:=10) & vbCrLf & vbCrLf &
                "They are listed beneath the drawing in the SVN tree. Select them and click Get Latest first." & vbCrLf & vbCrLf
        End If

        message &= "Then use Update All Views or rebuild the drawing (Ctrl+Q). Get the drawing's own SVN lock before saving updated view data or annotations."

        iSwApp.SendMsgToUser2(
            message,
            swMessageBoxIcon_e.swMbWarning,
            swMessageBoxBtn_e.swMbOk
        )
    End Sub

    Public Sub handleAssemblyDimensionChangePostPublic(ByVal assemblyDocument As ModelDoc2,
                                                        ByVal displayDimension As Object)
        'Capture the selected owning component before SOLIDWORKS clears the temporary
        'dimension-selection context. No SVN/server operation occurs here.
        noteAssemblySelectionContextPublic(assemblyDocument)

        Try
            handleAssemblyOwnedEditPostPublic(
                assemblyDocument,
                "changing an assembly, mate, or child-part dimension",
                allowLockedChildDimensionFallback:=True,
                allowRecentlyEndedInContextEdit:=True,
                allowRebuildModifyFallback:=True,
                allowActiveChildEditContext:=True
            )
        Finally
            clearAssemblySelectionContext(assemblyDocument)
        End Try
    End Sub

    Private Function allowPendingAssemblySuppressionNotification(ByVal eventAssembly As ModelDoc2) As Boolean
        If eventAssembly Is Nothing Then Return False

        Dim ownerPath As String = ""
        Dim activeAssemblyPath As String = ""
        Dim openedUtcToExpire As DateTime = DateTime.MinValue
        Dim eventPath As String = getAssemblyPathKeySafe(eventAssembly)
        Dim nowUtc As DateTime = DateTime.UtcNow

        SyncLock assemblyGuardSync
            Dim pending As PendingAssemblySuppressionCommand = pendingAssemblySuppressionState
            If pending Is Nothing Then Return False

            'CommandCloseNotify normally brackets all native state events. Retain the captured
            'owner for one short trailing UI turn because ModifyNotify can arrive just after
            'CommandCloseNotify. The absolute cap self-heals if SOLIDWORKS loses the close event.
            If (nowUtc - pending.OpenedUtc).TotalSeconds > 10.0 OrElse
               (pending.ClosedUtc <> DateTime.MinValue AndAlso
                (nowUtc - pending.ClosedUtc).TotalMilliseconds > 750.0) Then
                pendingAssemblySuppressionState = Nothing
                Return False
            End If

            ownerPath = pending.OwnerAssemblyPath
            activeAssemblyPath = pending.ActiveAssemblyPath

            If Not pending.ExpiryQueued Then
                pending.ExpiryQueued = True
                openedUtcToExpire = pending.OpenedUtc
            End If
        End SyncLock

        If openedUtcToExpire <> DateTime.MinValue Then
            queuePendingAssemblySuppressionExpiry(openedUtcToExpire)
        End If

        If String.IsNullOrWhiteSpace(ownerPath) Then Return False

        Dim ownerDocument As ModelDoc2 = getOpenModelByPathSafe(ownerPath)
        If ownerDocument Is Nothing OrElse Not assemblyHasRequiredLockFast(ownerDocument) Then
            SyncLock assemblyGuardSync
                pendingAssemblySuppressionState = Nothing
            End SyncLock
            Return False
        End If

        If Not String.IsNullOrWhiteSpace(activeAssemblyPath) AndAlso
           Not pathsAreSame(activeAssemblyPath, ownerPath) Then
            Dim activeAssembly As ModelDoc2 = getOpenModelByPathSafe(activeAssemblyPath)
            If activeAssembly IsNot Nothing Then markAssemblyGuardFalseDirtyCandidate(activeAssembly)
        End If

        If pathsAreSame(eventPath, ownerPath) Then
            'This is the real persisted edit and it belongs to the locked subassembly.
            clearAssemblyGuardFalseDirtyCandidate(eventAssembly)
        Else
            'Ancestor assemblies can receive SaveFlag/Modify bookkeeping for the child's
            'recompute. Their files were not the target of the command; close handling will
            'still verify SVN cleanliness before treating this as false dirty.
            markAssemblyGuardFalseDirtyCandidate(eventAssembly)
        End If

        Return True
    End Function

    Public Function getAssemblyEditOwnerForComponentStatePublic(ByVal eventAssembly As ModelDoc2,
                                                                 ByVal componentName As String) As ModelDoc2
        If eventAssembly Is Nothing OrElse String.IsNullOrWhiteSpace(componentName) Then Return Nothing

        Try
            Dim assemblyDocument As AssemblyDoc = TryCast(eventAssembly, AssemblyDoc)
            If assemblyDocument Is Nothing Then Return Nothing

            Dim changedComponent As Component2 = assemblyDocument.GetComponentByName(componentName)
            If changedComponent Is Nothing Then Return Nothing

            'A top-level component in this event document is owned by the event assembly.
            'A nested component is owned by the model document of its immediate assembly
            'parent. This remains true for components generated by MirrorComponent1.
            Dim parentComponent As Component2 = changedComponent.GetParent()
            If parentComponent Is Nothing Then Return eventAssembly

            Dim ownerDocument As ModelDoc2 = TryCast(parentComponent.GetModelDoc2(), ModelDoc2)
            If isAssemblyDocumentSafe(ownerDocument) Then Return ownerDocument
        Catch ex As Exception
            writeOperationLog(
                "Could not resolve component-state owner for " & componentName & ": " & ex.Message
            )
        End Try

        Return Nothing
    End Function

    Public Function getAssemblyEditOwnersForMovedComponentsPublic(ByVal eventAssembly As ModelDoc2,
                                                                   ByVal componentsPayload As Object) As ModelDoc2()
        If eventAssembly Is Nothing Then Return Nothing

        Dim movedComponents As New List(Of Component2)()

        Try
            If TypeOf componentsPayload Is Component2 Then
                movedComponents.Add(DirectCast(componentsPayload, Component2))
            ElseIf componentsPayload IsNot Nothing AndAlso componentsPayload.GetType().IsArray Then
                For Each item As Object In DirectCast(componentsPayload, System.Array)
                    Dim component As Component2 = TryCast(item, Component2)
                    If component IsNot Nothing Then movedComponents.Add(component)
                Next
            End If
        Catch ex As Exception
            writeOperationLog("Could not read moved-component event payload: " & ex.Message)
        End Try

        Dim owners As New List(Of ModelDoc2)()
        Dim ownerPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each movedComponent As Component2 In movedComponents
            Dim owner As ModelDoc2 = eventAssembly

            Try
                Dim parentComponent As Component2 = movedComponent.GetParent()
                If parentComponent IsNot Nothing Then
                    Dim parentDocument As ModelDoc2 = TryCast(parentComponent.GetModelDoc2(), ModelDoc2)
                    If isAssemblyDocumentSafe(parentDocument) Then owner = parentDocument
                End If
            Catch ex As Exception
                writeOperationLog("Could not resolve moved-component owner: " & ex.Message)
            End Try

            Dim ownerKey As String = getAssemblyPathKeySafe(owner)
            If String.IsNullOrWhiteSpace(ownerKey) Then
                Try
                    ownerKey = owner.GetTitle()
                Catch
                    ownerKey = Guid.NewGuid().ToString()
                End Try
            End If

            If ownerPaths.Add(ownerKey) Then owners.Add(owner)
        Next

        If owners.Count = 0 Then owners.Add(eventAssembly)
        Return owners.ToArray()
    End Function

    Public Sub handleAssemblyOwnedEditPostPublic(ByVal assemblyDocument As ModelDoc2,
                                                  ByVal actionDescription As String,
                                                  Optional ByVal allowLockedChildDimensionFallback As Boolean = False,
                                                  Optional ByVal addedEntityType As Integer = 0,
                                                  Optional ByVal addedItemName As String = "",
                                                  Optional ByVal allowRecentlyEndedInContextEdit As Boolean = False,
                                                  Optional ByVal allowDisplayOnlyFallback As Boolean = False,
                                                  Optional ByVal allowRebuildModifyFallback As Boolean = False,
                                                  Optional ByVal allowActiveChildEditContext As Boolean = False,
                                                  Optional ByVal allowPendingSuppressionCommandFallback As Boolean = False,
                                                  Optional ByVal pendingSuppressionEventAssembly As ModelDoc2 = Nothing)
        If allowPendingSuppressionCommandFallback AndAlso
           allowPendingAssemblySuppressionNotification(
               If(pendingSuppressionEventAssembly, assemblyDocument)
           ) Then Exit Sub

        If allowRebuildModifyFallback Then
            If consumeAssemblyRebuildGenericModifyAllowance(assemblyDocument) Then Exit Sub
        Else
            'A purpose-built structural event is a real user action, not the delayed generic
            'notification from the preceding rebuild. It invalidates any unused allowance.
            clearAssemblyRebuildGenericModifyAllowance(assemblyDocument)
        End If

        If allowDisplayOnlyFallback AndAlso hasRecentAssemblyDisplayOnlyChange(assemblyDocument) Then Exit Sub

        Dim childEditContext As Boolean = False

        If allowActiveChildEditContext Then
            childEditContext = assemblyIsEditingExternalPhysicalChild(
                assemblyDocument,
                allowLockedChildDimensionFallback,
                allowRecentlyEndedInContextEdit
            )
        End If

        'Use the child-context result once. A recently ended session is intentionally
        'consumed so a later, unrelated assembly edit cannot borrow the same exception.
        If childEditContext Then Exit Sub

        If Not assemblyOwnedEditMustBeBlocked(
            assemblyDocument,
            allowLockedChildDimensionFallback,
            ignoreActiveRebuild:=Not allowRebuildModifyFallback,
            allowActiveChildEditContext:=allowActiveChildEditContext
        ) Then
            If Not childEditContext Then clearAssemblyGuardFalseDirtyCandidate(assemblyDocument)
            Exit Sub
        End If

        'This notification represents a real assembly-owned operation that SOLIDWORKS has
        'already applied and PlumVault intentionally does not undo. It must invalidate any
        'older child-rebuild/ancestor false-dirty candidate so close review cannot discard the
        'user's move, mate, suppression, or feature change as harmless bookkeeping.
        clearAssemblyGuardFalseDirtyCandidate(assemblyDocument)

        Dim assemblyPath As String = ""
        Dim queueKey As String = ""

        Try
            assemblyPath = assemblyDocument.GetPathName()
        Catch
            assemblyPath = ""
        End Try

        If Not String.IsNullOrWhiteSpace(assemblyPath) Then
            Try
                queueKey = Path.GetFullPath(assemblyPath)
            Catch
                queueKey = assemblyPath
            End Try
        Else
            Try
                queueKey = assemblyDocument.GetTitle()
            Catch
                queueKey = Guid.NewGuid().ToString()
            End Try
        End If

        SyncLock assemblyGuardSync
            Dim queuedSinceUtc As DateTime = DateTime.MinValue

            If assemblyGuardQueuedPaths.TryGetValue(queueKey, queuedSinceUtc) Then
                If (DateTime.UtcNow - queuedSinceUtc).TotalMinutes <= ASSEMBLY_GUARD_QUEUE_STALE_MINUTES Then
                    Exit Sub
                End If

                writeOperationLog("Assembly guard queue entry stale, clearing and re-queuing: " & queueKey)
            End If

            assemblyGuardQueuedPaths(queueKey) = DateTime.UtcNow
        End SyncLock

        Dim warningAction As New System.Windows.Forms.MethodInvoker(
            Sub()
                Try
                    Dim currentAssembly As ModelDoc2 = assemblyDocument

                    If Not String.IsNullOrWhiteSpace(assemblyPath) Then
                        Try
                            Dim reopened As ModelDoc2 = getOpenModelByPathSafe(assemblyPath)
                            If reopened IsNot Nothing Then currentAssembly = reopened
                        Catch
                        End Try
                    End If

                    If currentAssembly Is Nothing Then Exit Sub
                    If allowDisplayOnlyFallback AndAlso hasRecentAssemblyDisplayOnlyChange(currentAssembly) Then Exit Sub
                    If Not assemblyOwnedEditMustBeBlocked(
                        currentAssembly,
                        allowLockedChildDimensionFallback,
                        ignoreActiveRebuild:=Not allowRebuildModifyFallback,
                        allowActiveChildEditContext:=allowActiveChildEditContext
                    ) Then Exit Sub

                    'Post-notifications can arrive while SOLIDWORKS is still inside a feature,
                    'mate, move, or suppression transaction. Calling EditUndo2 from here has
                    'caused native crashes and can undo unrelated designer work. Cancellable
                    'pre-events block where available; this fallback only reports the exact
                    'unlocked owner and leaves any cleanup decision to the designer.
                    writeOperationLog(
                        "Assembly guard warning only; automatic Undo disabled: " &
                        actionDescription & " (" & addedItemName & ")"
                    )
                    showAssemblyLockRequiredMessage(currentAssembly, actionDescription)
                Catch
                    Try
                        showAssemblyLockRequiredMessage(assemblyDocument, actionDescription, editWasUndone:=False)
                    Catch
                    End Try
                Finally
                    SyncLock assemblyGuardSync
                        assemblyGuardQueuedPaths.Remove(queueKey)
                    End SyncLock
                End Try
            End Sub
        )

        Try
            If myUserControl IsNot Nothing AndAlso
               Not myUserControl.IsDisposed AndAlso
               myUserControl.IsHandleCreated Then
                myUserControl.BeginInvoke(warningAction)
                Exit Sub
            End If
        Catch
        End Try

        'The task pane is normally available. This fallback keeps the edit from being
        'left behind if the pane is temporarily unavailable.
        Try
            warningAction.Invoke()
        Catch
            SyncLock assemblyGuardSync
                assemblyGuardQueuedPaths.Remove(queueKey)
            End SyncLock
        End Try
    End Sub

    'Confirms a blocked "add" (component, mate, plane, sketch, or other feature) was actually
    'reverted rather than trusting a single Undo. Never issue additional blind Undo commands:
    'they can cross the blocked add and destroy unrelated work already on the user's undo stack.
    'A component may be removed directly only when its unique instance name is an exact match.
    Private Function ensureAddedItemRemoved(ByVal assemblyDocument As ModelDoc2,
                                            ByVal entityType As Integer,
                                            ByVal itemName As String) As Boolean
        If assemblyDocument Is Nothing Then Return True
        If String.IsNullOrWhiteSpace(itemName) Then Return True

        If Not addedItemStillPresent(assemblyDocument, entityType, itemName) Then Return True

        Return forceRemoveAddedItem(assemblyDocument, entityType, itemName)
    End Function

    Private Function addedItemStillPresent(ByVal assemblyDocument As ModelDoc2,
                                           ByVal entityType As Integer,
                                           ByVal itemName As String) As Boolean
        Try
            If entityType = CInt(swNotifyEntityType_e.swNotifyComponent) Then
                Return findComponentByAddNotifyName(assemblyDocument, itemName) IsNot Nothing
            End If

            'Mates, reference planes, sketches, and most other FeatureManager entries report as
            'swNotifyFeature. There is no name-indexed lookup API, so walk the feature list.
            Dim currentFeature As Object = assemblyDocument.FirstFeature()

            While currentFeature IsNot Nothing
                Dim feature As Feature = TryCast(currentFeature, Feature)
                If feature Is Nothing Then Exit While

                Dim featureName As String = ""
                Try
                    featureName = feature.Name
                Catch
                End Try

                If String.Equals(featureName, itemName, StringComparison.OrdinalIgnoreCase) Then Return True

                Try
                    currentFeature = feature.GetNextFeature()
                Catch
                    Exit While
                End Try
            End While

            Return False
        Catch
            Return False
        End Try
    End Function

    Private Function findComponentByAddNotifyName(ByVal assemblyDocument As ModelDoc2,
                                                   ByVal itemName As String) As Component2
        Try
            Dim assemblyDoc As AssemblyDoc = TryCast(assemblyDocument, AssemblyDoc)
            If assemblyDoc Is Nothing Then Return Nothing

            'The AddItemNotify contract supplies the added item's name. Assembly component
            'instance names are unique, so prefer the API's exact name lookup.
            Try
                Dim exactComponent As Component2 = assemblyDoc.GetComponentByName(itemName)
                If exactComponent IsNot Nothing Then Return exactComponent
            Catch
            End Try

            Dim componentsObject As Object = assemblyDoc.GetComponents(False)
            Dim components As Object() = TryCast(componentsObject, Object())
            If components Is Nothing Then Return Nothing

            For Each componentObject As Object In components
                Dim component As Component2 = TryCast(componentObject, Component2)
                If component Is Nothing Then Continue For

                Dim componentName As String = ""
                Try
                    componentName = component.Name2
                Catch
                    Try
                        componentName = component.Name
                    Catch
                    End Try
                End Try

                If String.IsNullOrWhiteSpace(componentName) Then Continue For

                If String.Equals(componentName, itemName, StringComparison.OrdinalIgnoreCase) Then
                    Return component
                End If
            Next
        Catch
        End Try

        Return Nothing
    End Function

    Private Function forceRemoveAddedItem(ByVal assemblyDocument As ModelDoc2,
                                          ByVal entityType As Integer,
                                          ByVal itemName As String) As Boolean
        Try
            If entityType <> CInt(swNotifyEntityType_e.swNotifyComponent) Then
                'Mates/planes/sketches/features do not have one generic, safe-for-every-type
                'deletion API. Report honestly rather than risk deleting the wrong feature.
                Return False
            End If

            Dim assemblyDoc As AssemblyDoc = TryCast(assemblyDocument, AssemblyDoc)
            If assemblyDoc Is Nothing Then Return False

            Dim component As Component2 = findComponentByAddNotifyName(assemblyDocument, itemName)
            If component Is Nothing Then Return True 'Already gone.

            Try
                assemblyDocument.ClearSelection2(True)
            Catch
            End Try

            Dim selected As Boolean = False
            Try
                selected = component.Select4(False, Nothing, False)
            Catch
                selected = False
            End Try

            Dim deleted As Boolean = False

            If selected Then
                Try
                    deleted = assemblyDoc.DeleteSelections(0)
                Catch
                    deleted = False
                End Try
            End If

            Try
                assemblyDocument.ClearSelection2(True)
            Catch
            End Try

            If Not deleted Then Return False
            Return Not addedItemStillPresent(assemblyDocument, entityType, itemName)
        Catch
            Return False
        End Try
    End Function

    Private Sub beginInternalSolidWorksSave()
        internalSolidWorksSaveDepth += 1
    End Sub

    Private Sub endInternalSolidWorksSave()
        If internalSolidWorksSaveDepth > 0 Then internalSolidWorksSaveDepth -= 1
    End Sub

    Private Function getCadExtensionForDocument(ByVal doc As ModelDoc2) As String
        If doc Is Nothing Then Return ""

        Try
            Select Case CInt(doc.GetType())
                Case swDocumentTypes_e.swDocPART
                    Return ".SLDPRT"
                Case swDocumentTypes_e.swDocASSEMBLY
                    Return ".SLDASM"
                Case swDocumentTypes_e.swDocDRAWING
                    Return ".SLDDRW"
            End Select
        Catch
        End Try

        Return ""
    End Function

    Private Function isCadDocument(ByVal doc As ModelDoc2) As Boolean
        Return Not String.IsNullOrWhiteSpace(getCadExtensionForDocument(doc))
    End Function

    Private Class SolidWorksDialogOwner
        Implements System.Windows.Forms.IWin32Window

        Private ReadOnly ownerHandle As IntPtr

        Public Sub New(ByVal handleValue As IntPtr)
            ownerHandle = handleValue
        End Sub

        Public ReadOnly Property Handle As IntPtr Implements System.Windows.Forms.IWin32Window.Handle
            Get
                Return ownerHandle
            End Get
        End Property
    End Class

    Private Function getSolidWorksDialogOwner() As System.Windows.Forms.IWin32Window
        If iSwApp Is Nothing Then Return Nothing

        Try
            Dim frameObject As Object = iSwApp.Frame()
            If frameObject Is Nothing Then Return Nothing

            Dim hwnd As IntPtr = New IntPtr(Convert.ToInt64(frameObject.GetHWnd()))
            If hwnd = IntPtr.Zero Then Return Nothing

            Return New SolidWorksDialogOwner(hwnd)
        Catch
            Return Nothing
        End Try
    End Function


    Public Function handleSolidWorksSaveCommandPreNotifyPublic(ByVal command As Integer,
                                                               ByVal userCommand As Integer) As Integer
        'First-save is routed through PlumVault's naming/location review. Ordinary Save for
        'an existing managed file is also intercepted, but for a different reason: native
        'SOLIDWORKS Ctrl+S can expand a dirty assembly/drawing save to every referenced model
        'whose in-memory save flag is set. PlumVault instead queues a silent Save3 of only the
        'document on which the user pressed Ctrl+S. Managed SVN Save As uses PlumVault's
        'checked review table; files outside the configured working copy stay native.
        If automaticSaveEventsSuppressed() Then Return 0
        If newDocumentTeamSaveWorkflowInProgress Then Return -1
        If command <> SW_COMMAND_SAVE AndAlso command <> SW_COMMAND_SAVE_AS Then Return 0
        If iSwApp Is Nothing Then Return 0

        Dim doc As ModelDoc2 = Nothing

        Try
            doc = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch
            doc = Nothing
        End Try

        If doc Is Nothing OrElse Not isCadDocument(doc) Then Return 0

        Dim currentPath As String = ""

        Try
            currentPath = doc.GetPathName()
        Catch
            currentPath = ""
        End Try

        If Not String.IsNullOrWhiteSpace(currentPath) Then
            If Not isCadFilePath(currentPath) Then Return 0
            If Not isPathInsideLocalRepo(currentPath) Then Return 0

            'Use the same checked name/location table regardless of whether Save As came
            'from PlumVault's toolbar, SOLIDWORKS' File menu, or its keyboard command. The
            'workflow is document-type agnostic, so drawings receive the same validation and
            'automatic SVN commit as parts and assemblies. Files outside SVN remain native.
            If command = SW_COMMAND_SAVE_AS Then
                If myUserControl Is Nothing OrElse Not myUserControl.IsHandleCreated Then
                    iSwApp.SendMsgToUser2(
                        "Save As could not start because the SVN task pane is not ready.",
                        swMessageBoxIcon_e.swMbStop,
                        swMessageBoxBtn_e.swMbOk
                    )
                    Return -1
                End If

                Try
                    myUserControl.BeginInvoke(
                        New MethodInvoker(Sub() performSaveAsButtonActionPublic())
                    )
                Catch ex As Exception
                    iSwApp.SendMsgToUser2(
                        "Save As could not start." & vbCrLf & vbCrLf & ex.Message,
                        swMessageBoxIcon_e.swMbStop,
                        swMessageBoxBtn_e.swMbOk
                    )
                End Try

                Return -1
            End If

            If command <> SW_COMMAND_SAVE Then Return 0

            If managedActiveDocumentSaveQueued Then
                'The queued isolated save normally starts within one UI turn. If the flag is
                'still set long after queuing (e.g. the BeginInvoke delegate was lost during a
                'task-pane teardown), a permanently stuck flag would silently turn every future
                'Ctrl+S into a no-op for the rest of the session. Self-heal like the other
                'guard queues: treat a stale flag as lost and requeue this save normally.
                If (DateTime.UtcNow - managedActiveDocumentSaveQueuedUtc).TotalSeconds < 15.0 Then Return -1
                managedActiveDocumentSaveQueued = False
                writeOperationLog("Stale queued managed save flag cleared; requeuing Ctrl+S save.")
            End If

            If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then
                'A native save of a managed assembly may save/rebuild referenced documents and
                'reintroduce the alert cascade this isolated-save route exists to prevent.
                'Fail closed when the UI dispatcher is unavailable.
                iSwApp.SendMsgToUser2(
                    "Save was cancelled because the SVN task pane is not ready." & vbCrLf & vbCrLf &
                    "Wait for the task pane to finish loading, then try Save again.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
                Return -1
            End If

            managedActiveDocumentSaveQueued = True
            managedActiveDocumentSaveQueuedUtc = DateTime.UtcNow

            Try
                Dim queuedPath As String = currentPath
                myUserControl.BeginInvoke(
                    New MethodInvoker(
                        Sub()
                            performManagedActiveDocumentOnlySave(queuedPath)
                        End Sub
                    )
                )
            Catch ex As Exception
                managedActiveDocumentSaveQueued = False
                Try
                    iSwApp.SendMsgToUser2(
                        "Save could not be queued safely, so the native save was cancelled." & vbCrLf & vbCrLf &
                        "Try Save again." & vbCrLf & vbCrLf & ex.Message,
                        swMessageBoxIcon_e.swMbWarning,
                        swMessageBoxBtn_e.swMbOk
                    )
                Catch
                End Try
                Return -1
            End Try

            'Cancel native Ctrl+S. The queued Save3 deliberately omits
            'swSaveAsOptions_SaveReferenced, so only this assembly/part/drawing is saved.
            Return -1
        End If

        Dim response As swMessageBoxResult_e = iSwApp.SendMsgToUser2(
            "Is this new CAD file for the Gryphon Racing SVN repository?" & vbCrLf & vbCrLf &
            "Yes = enter the required GRC27/CFD27 name, then choose the SVN folder." & vbCrLf &
            "No = normal SOLIDWORKS Save As for classwork or files outside SVN." & vbCrLf &
            "Cancel = stop the save.",
            swMessageBoxIcon_e.swMbQuestion,
            swMessageBoxBtn_e.swMbYesNoCancel
        )

        If response = swMessageBoxResult_e.swMbHitNo Then Return 0
        If response <> swMessageBoxResult_e.swMbHitYes Then Return -1

        Dim ext As String = getCadExtensionForDocument(doc)
        Dim titleNoExt As String = "NewFile"

        Try
            titleNoExt = Path.GetFileNameWithoutExtension(doc.GetTitle())
        Catch
            titleNoExt = "NewFile"
        End Try

        If String.IsNullOrWhiteSpace(titleNoExt) Then titleNoExt = "NewFile"

        'The review table (opened inside performNewDocumentSvnSave) is now the single place
        'a name is entered and validated - no separate InputBox step before it.
        Dim requestedName As String = titleNoExt & ext

        newDocumentTeamSaveWorkflowInProgress = True

        Try
            If myUserControl IsNot Nothing AndAlso myUserControl.IsHandleCreated Then
                myUserControl.BeginInvoke(
                    New MethodInvoker(Sub() performNewDocumentSvnSave(doc, requestedName))
                )
            Else
                newDocumentTeamSaveWorkflowInProgress = False
                iSwApp.SendMsgToUser2(
                    "Save could not start because the SVN task pane is not ready.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
            End If
        Catch ex As Exception
            newDocumentTeamSaveWorkflowInProgress = False
            iSwApp.SendMsgToUser2(
                "Save could not start." & vbCrLf & vbCrLf & ex.Message,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
        End Try

        'Cancel the original SOLIDWORKS command. The queued workflow displays the controlled
        'Save dialog with the compliant name already filled in.
        Return -1
    End Function

    Private Sub performManagedActiveDocumentOnlySave(ByVal requestedPath As String)
        Try
            If String.IsNullOrWhiteSpace(requestedPath) Then Exit Sub

            Dim doc As ModelDoc2 = getOpenModelByPathSafe(requestedPath)

            If doc Is Nothing Then
                iSwApp.SendMsgToUser2(
                    "Save did not run because the document was closed before PlumVault could save it.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
                Exit Sub
            End If

            Dim currentPath As String = ""
            Try
                currentPath = doc.GetPathName()
            Catch
                currentPath = ""
            End Try

            If String.IsNullOrWhiteSpace(currentPath) OrElse Not pathsAreSame(currentPath, requestedPath) Then
                iSwApp.SendMsgToUser2(
                    "The document name or location changed before Save ran." & vbCrLf & vbCrLf &
                    "Press Ctrl+S again to save the current file.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
                Exit Sub
            End If

            'Run the same Online, naming, and local-lock checks used by native document save
            'events. Internal-save suppression below prevents those events from duplicating
            'the checks and from queuing a second automatic commit.
            If handleSolidWorksFileSavePrePublic(doc, currentPath, isSaveAs:=False) <> 0 Then Exit Sub

            Dim isDirty As Boolean = True
            Try
                isDirty = doc.GetSaveFlag()
            Catch
                isDirty = True
            End Try

            'If only referenced children carry false dirty flags, Ctrl+S on a clean parent is
            'intentionally a no-op. Most importantly, it does not save those children.
            If Not isDirty Then Exit Sub

            Dim errors As Integer = 0
            Dim warnings As Integer = 0
            Dim saved As Boolean = False

            beginInternalSolidWorksSave()
            Try
                saved = doc.Save3(
                    CInt(swSaveAsOptions_e.swSaveAsOptions_Silent),
                    errors,
                    warnings
                )
            Finally
                endInternalSolidWorksSave()
            End Try

            If Not saved Then
                iSwApp.SendMsgToUser2(
                    "SOLIDWORKS could not save the active document." & vbCrLf & vbCrLf &
                    Path.GetFileName(currentPath) & vbCrLf & vbCrLf &
                    "Errors: " & errors.ToString() & "; warnings: " & warnings.ToString(),
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Exit Sub
            End If

            'FileSavePostNotify is suppressed for this controlled save, so queue exactly one
            'automatic commit explicitly. Existing files retain their SVN lock via --no-unlock.
            queueAutomaticSaveCommitPath(currentPath)

        Catch ex As Exception
            Try
                iSwApp.SendMsgToUser2(
                    "The active-document save did not complete." & vbCrLf & vbCrLf & ex.Message,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try
        Finally
            managedActiveDocumentSaveQueued = False
        End Try
    End Sub


    Public Function startNewDocumentFirstSaveFromCommitPublic() As Boolean
        If iSwApp Is Nothing Then Return False

        Dim activeDocument As ModelDoc2 = Nothing

        Try
            activeDocument = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch
            activeDocument = Nothing
        End Try

        If activeDocument Is Nothing OrElse Not isCadDocument(activeDocument) Then Return False

        Dim activePath As String = ""

        Try
            activePath = activeDocument.GetPathName()
        Catch
            activePath = ""
        End Try

        If Not String.IsNullOrWhiteSpace(activePath) Then Return False

        'Reuse the exact managed first-save workflow used by Save/Ctrl+S. The return value
        'is -1 when PlumVault consumed the command and queued the controlled Save As flow.
        Return handleSolidWorksSaveCommandPreNotifyPublic(SW_COMMAND_SAVE, 0) = -1
    End Function

    Public Sub performSaveAsButtonActionPublic()
        If iSwApp Is Nothing Then Return
        If myUserControl Is Nothing Then Return

        Dim doc As ModelDoc2 = Nothing

        Try
            doc = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch
            doc = Nothing
        End Try

        If doc Is Nothing OrElse Not isCadDocument(doc) Then
            iSwApp.SendMsgToUser2(
                "Open a SOLIDWORKS part, assembly, or drawing before using Save As.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            Return
        End If

        If newDocumentTeamSaveWorkflowInProgress Then
            iSwApp.SendMsgToUser2(
                "A Save is already in progress. Finish that first.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            Return
        End If

        Dim ext As String = getCadExtensionForDocument(doc)
        Dim titleNoExt As String = "NewFile"

        Try
            titleNoExt = Path.GetFileNameWithoutExtension(doc.GetTitle())
        Catch
            titleNoExt = "NewFile"
        End Try

        If String.IsNullOrWhiteSpace(titleNoExt) Then titleNoExt = "NewFile"

        'The review table (opened inside performNewDocumentSvnSave) is now the single place
        'a name is entered and validated - no separate InputBox step before it.
        Dim requestedName As String = titleNoExt & ext

        newDocumentTeamSaveWorkflowInProgress = True

        Try
            If myUserControl.IsHandleCreated Then
                myUserControl.BeginInvoke(
                    New MethodInvoker(Sub() performNewDocumentSvnSave(doc, requestedName))
                )
            Else
                newDocumentTeamSaveWorkflowInProgress = False
                iSwApp.SendMsgToUser2(
                    "Save As could not start because the SVN task pane is not ready.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
            End If
        Catch ex As Exception
            newDocumentTeamSaveWorkflowInProgress = False
            iSwApp.SendMsgToUser2(
                "Save As could not start." & vbCrLf & vbCrLf & ex.Message,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
        End Try
    End Sub

    Private Class CadReferenceGraph
        Public ReadOnly DirectDependenciesByReferrer As New Dictionary(Of String, HashSet(Of String))(StringComparer.OrdinalIgnoreCase)
        Public Property ScanSucceeded As Boolean = True
        Public Property ErrorMessage As String = ""
    End Class

    Public Sub performCadRelocationPublic(ByVal mode As CadRelocationMode)
        If iSwApp Is Nothing OrElse myUserControl Is Nothing Then Return

        Dim document As ModelDoc2 = Nothing
        Dim sourcePath As String = ""

        Try
            document = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If document IsNot Nothing Then sourcePath = document.GetPathName()
        Catch
            document = Nothing
            sourcePath = ""
        End Try

        If document Is Nothing OrElse String.IsNullOrWhiteSpace(sourcePath) OrElse
           Not File.Exists(sourcePath) OrElse Not isCadFilePath(sourcePath) OrElse
           Not isPathInsideLocalRepo(sourcePath) Then
            iSwApp.SendMsgToUser2(
                "Open a saved part, assembly, or drawing inside the SVN working copy first.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            Return
        End If

        If Not isOnlineModeEnabled() Then
            iSwApp.SendMsgToUser2(
                "Re-ID and Move require Online mode because the completed reference change is committed automatically.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            Return
        End If

        If asyncCommitInProgress OrElse cadRelocationInProgress Then
            iSwApp.SendMsgToUser2(
                "Another save, commit, Re-ID, or Move operation is still running. Finish it first.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
            Return
        End If

        Dim repoRoot As String = getResolvedSvnWorkingCopyRootPath()
        If String.IsNullOrWhiteSpace(repoRoot) OrElse Not Directory.Exists(repoRoot) Then Return

        Dim approvedDestination As String = ""

        Using review As New CadRelocationReviewForm(
            sourcePath,
            repoRoot,
            mode,
            Function(destinationPath As String) buildCadRelocationCheck(sourcePath, destinationPath)
        )
            Dim owner As System.Windows.Forms.IWin32Window = getSolidWorksDialogOwner()
            Dim result As DialogResult = If(owner Is Nothing, review.ShowDialog(), review.ShowDialog(owner))

            If result <> DialogResult.OK OrElse Not review.Approved Then Return
            approvedDestination = review.ApprovedDestinationPath
        End Using

        If String.IsNullOrWhiteSpace(approvedDestination) Then Return
        executeCadRelocation(sourcePath, approvedDestination, mode)
    End Sub

    Private Function buildCadRelocationCheck(ByVal sourcePath As String,
                                             ByVal destinationPath As String) As CadRelocationCheckResult
        Dim result As New CadRelocationCheckResult()
        Dim rows As List(Of CadRelocationReviewRow) = result.Rows

        sourcePath = normalizeFullPathSafe(sourcePath)
        destinationPath = normalizeFullPathSafe(destinationPath)

        Dim destinationFolder As String = ""
        Try
            destinationFolder = Path.GetDirectoryName(destinationPath)
        Catch
        End Try

        'Built as a single specific reason (checked in order) rather than one generic message
        'for every possible cause - "not ready" alone doesn't tell the user which of several
        'unrelated problems (same path, wrong extension, outside the working copy, folder
        'missing, folder not yet under version control, name collision, naming convention)
        'actually applies.
        Dim destinationNotReadyReason As String = ""

        If String.IsNullOrWhiteSpace(destinationPath) Then
            destinationNotReadyReason = "Choose a destination file name."
        ElseIf pathsAreSame(sourcePath, destinationPath) Then
            destinationNotReadyReason = "Choose a different name or folder than the source file."
        ElseIf Not String.Equals(Path.GetExtension(sourcePath), Path.GetExtension(destinationPath), StringComparison.OrdinalIgnoreCase) Then
            destinationNotReadyReason = "The destination must keep the same file extension (" & Path.GetExtension(sourcePath) & ")."
        ElseIf Not isPathInsideLocalRepo(destinationPath) Then
            destinationNotReadyReason = "Choose a folder inside the configured SVN working copy."
        ElseIf Not Directory.Exists(destinationFolder) Then
            destinationNotReadyReason = "The destination folder does not exist yet."
        ElseIf pathContainsSvnAdministrativeSegment(destinationPath) Then
            destinationNotReadyReason = "Choose a normal project folder, not an SVN .svn administration folder."
        ElseIf File.Exists(destinationPath) Then
            destinationNotReadyReason = "A file already exists at this destination."
        ElseIf Not isVendorPartPath(destinationPath) AndAlso
               Not shouldIgnoreGrc27NamingConventionForDebug() AndAlso
               Not isValidGrc27FileName(destinationPath) Then
            destinationNotReadyReason = "The file name must follow the GRC27/CFD27 naming convention (or be placed under Vendor Parts)."
        End If

        Dim destinationReady As Boolean = String.IsNullOrWhiteSpace(destinationNotReadyReason)

        'A destination folder that exists on disk but isn't versioned yet (e.g. just created
        'via Browse's "Make New Folder") is not blocked here - executeCadRelocation adds it to
        'SVN automatically right before the move, so it rides along in the same commit.
        Dim destinationFolderIsNewToSvn As Boolean =
            destinationReady AndAlso Not svnFolderIsVersioned(destinationFolder)

        rows.Add(New CadRelocationReviewRow With {
            .FilePath = destinationPath,
            .RoleText = "Destination",
            .StateText = If(destinationReady, "Available", "Not ready"),
            .CheckText = If(destinationReady, "Ready", "Fix"),
            .Explanation = If(destinationReady,
                              If(destinationFolderIsNewToSvn,
                                 "Name and extension are valid. This folder is not yet in SVN and will be added automatically.",
                                 "Existing versioned folder; name and extension are valid."),
                              destinationNotReadyReason),
            .IsReady = destinationReady
        })

        Dim sourceReady As Boolean = cadPathHasRequiredRelocationLock(sourcePath)
        rows.Add(createCadRelocationFileRow(sourcePath, "File being changed", sourceReady, True))

        If Not destinationReady OrElse Not sourceReady Then
            result.Summary = "Fix the highlighted row(s), then click Check again. No files have been changed."
            Return result
        End If

        Dim graph As CadReferenceGraph = buildCadReferenceGraph(getResolvedSvnWorkingCopyRootPath())

        If graph Is Nothing OrElse Not graph.ScanSucceeded Then
            rows.Add(New CadRelocationReviewRow With {
                .FilePath = getResolvedSvnWorkingCopyRootPath(),
                .RoleText = "Reference scan",
                .StateText = "Incomplete",
                .CheckText = "Blocked",
                .Explanation = If(graph Is Nothing, "The dependency scan did not start.", graph.ErrorMessage),
                .IsReady = False
            })
            result.Summary = "PlumVault could not prove that every assembly and drawing reference was found. No files have been changed."
            Return result
        End If

        Dim directReferrers As List(Of String) = getDirectReferrers(graph, sourcePath)
        Dim openPaths As List(Of String) = getOpenCadDocumentPaths()
        Dim openClosure As List(Of String) = getOpenReverseReferenceClosure(graph, sourcePath, openPaths)
        Dim directSet As New HashSet(Of String)(directReferrers, StringComparer.OrdinalIgnoreCase)
        Dim rowsSeen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase) From {sourcePath, destinationPath}
        Dim allReady As Boolean = destinationReady AndAlso sourceReady

        For Each referrer As String In directReferrers
            Dim ready As Boolean = cadPathHasRequiredRelocationLock(referrer)
            rows.Add(createCadRelocationFileRow(
                referrer,
                If(Path.GetExtension(referrer).Equals(".SLDDRW", StringComparison.OrdinalIgnoreCase),
                   "Drawing reference",
                   "Direct model reference"),
                ready,
                getOpenModelByPathSafe(referrer) IsNot Nothing
            ))
            rowsSeen.Add(referrer)
            If Not ready Then allReady = False
        Next

        For Each openPath As String In openClosure
            If rowsSeen.Contains(openPath) Then Continue For

            Dim doc As ModelDoc2 = getOpenModelByPathSafe(openPath)
            Dim dirty As Boolean = documentHasRealUnsavedChanges(doc, openPath)
            Dim ready As Boolean = Not dirty OrElse cadPathHasRequiredRelocationLock(openPath)

            rows.Add(New CadRelocationReviewRow With {
                .FilePath = openPath,
                .RoleText = "Open parent",
                .StateText = If(dirty, "Unsaved; will save", "Clean; will reopen"),
                .CheckText = If(ready, "Ready", "Lock needed"),
                .Explanation = If(ready,
                                  "Temporarily closes so nested references can be updated safely, then reopens.",
                                  "This open parent has unsaved changes and needs your lock before PlumVault can save and temporarily close it."),
                .IsReady = ready
            })
            rowsSeen.Add(openPath)
            If Not ready Then allReady = False
        Next

        If asyncCommitInProgress OrElse cadRelocationInProgress Then allReady = False

        result.DirectReferrerPaths = directReferrers.ToArray()
        result.OpenPathsToReopen = openClosure.ToArray()

        Try
            Dim active As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If active IsNot Nothing Then result.ActivePathBeforeOperation = normalizeFullPathSafe(active.GetPathName())
        Catch
        End Try

        result.CanProceed = allReady
        result.Summary = If(
            allReady,
            directReferrers.Count.ToString() & " direct reference(s) will be updated. " &
            openClosure.Count.ToString() & " open document(s) will be safely closed and reopened. The complete change will auto-commit.",
            "Fix the highlighted row(s), then click Check again. No files have been changed."
        )

        Return result
    End Function

    Private Function createCadRelocationFileRow(ByVal filePath As String,
                                                ByVal roleText As String,
                                                ByVal ready As Boolean,
                                                ByVal isOpen As Boolean) As CadRelocationReviewRow
        Dim firstStatus As Char = getFirstSvnStatusChar(filePath)
        Dim isNew As Boolean = firstStatus = "?"c OrElse firstStatus = "A"c

        Return New CadRelocationReviewRow With {
            .FilePath = filePath,
            .RoleText = roleText,
            .StateText = If(isNew, "New SVN file", If(isOpen, "Open; lock checked", "Closed; lock checked")),
            .CheckText = If(ready, "Ready", "Lock needed"),
            .Explanation = If(ready,
                              If(isNew, "New file; no existing SVN lock is required.", "You own the SVN lock."),
                              "Get Locks for this file, then click Check again."),
            .IsReady = ready
        }
    End Function

    Private Function cadPathHasRequiredRelocationLock(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) OrElse Not File.Exists(filePath) Then Return False
        If Not isCadFilePath(filePath) OrElse Not isPathInsideLocalRepo(filePath) Then Return False

        Dim firstStatus As Char = getFirstSvnStatusChar(filePath)
        If firstStatus = "?"c OrElse firstStatus = "A"c Then Return True
        If firstStatus = ChrW(0) Then Return False
        Return userHasLocalSvnLockTokenForPath(filePath, allowCachedToken:=False)
    End Function

    Private Function svnFolderIsVersioned(ByVal folderPath As String) As Boolean
        If String.IsNullOrWhiteSpace(folderPath) OrElse Not Directory.Exists(folderPath) Then Return False
        If Not isPathInsideLocalRepo(folderPath) Then Return False

        Try
            Dim info As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "info --show-item kind --non-interactive """ & folderPath.Replace("""", "") & """"
            )
            Return String.IsNullOrWhiteSpace(If(info.outputError, "")) AndAlso
                   String.Equals(If(info.output, "").Trim(), "dir", StringComparison.OrdinalIgnoreCase)
        Catch
            Return False
        End Try
    End Function

    Private Function pathContainsSvnAdministrativeSegment(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        Return filePath.IndexOf("\.svn\", StringComparison.OrdinalIgnoreCase) >= 0 OrElse
               filePath.EndsWith("\.svn", StringComparison.OrdinalIgnoreCase)
    End Function

    Private Function normalizeFullPathSafe(ByVal filePath As String) As String
        If String.IsNullOrWhiteSpace(filePath) Then Return ""
        Try
            Return Path.GetFullPath(filePath)
        Catch
            Return filePath
        End Try
    End Function

    Private Function buildCadReferenceGraph(ByVal repoRoot As String) As CadReferenceGraph
        Dim graph As New CadReferenceGraph()
        If String.IsNullOrWhiteSpace(repoRoot) OrElse Not Directory.Exists(repoRoot) Then Return graph

        Dim candidates As New List(Of String)()

        Try
            For Each candidate As String In Directory.EnumerateFiles(repoRoot, "*.*", SearchOption.AllDirectories)
                If pathContainsSvnAdministrativeSegment(candidate) Then Continue For
                If isCadFilePath(candidate) Then candidates.Add(normalizeFullPathSafe(candidate))
            Next
        Catch ex As Exception
            writeOperationLog("Reference scan was incomplete: " & ex.Message)
            graph.ScanSucceeded = False
            graph.ErrorMessage = "Could not enumerate every CAD file in the working copy: " & ex.Message
            Return graph
        End Try

        For Each candidate As String In candidates
            Dim dependencies As HashSet(Of String) = Nothing
            Dim dependencyError As String = ""

            If Not tryGetDirectCadDependencies(candidate, dependencies, dependencyError) Then
                graph.ScanSucceeded = False
                graph.ErrorMessage = "Could not inspect references in " & Path.GetFileName(candidate) & ": " & dependencyError
                Return graph
            End If

            graph.DirectDependenciesByReferrer(candidate) = dependencies
        Next

        Return graph
    End Function

    Private Function getDirectCadDependencies(ByVal documentPath As String) As HashSet(Of String)
        Dim output As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        Dim ignoredError As String = ""
        tryGetDirectCadDependencies(documentPath, output, ignoredError)
        Return output
    End Function

    Private Function tryGetDirectCadDependencies(ByVal documentPath As String,
                                                 ByRef output As HashSet(Of String),
                                                 ByRef errorMessage As String) As Boolean
        output = New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        errorMessage = ""

        Try
            Dim dependenciesObject As Object = iSwApp.GetDocumentDependencies2(documentPath, False, False, False)
            Dim dependencies As Array = TryCast(dependenciesObject, Array)
            If dependencies Is Nothing OrElse dependencies.Length < 2 Then Return True

            Dim lower As Integer = dependencies.GetLowerBound(0)
            Dim upper As Integer = dependencies.GetUpperBound(0)

            Dim index As Integer = lower + 1
            While index <= upper
                Dim value As String = Convert.ToString(dependencies.GetValue(index))

                If Not String.IsNullOrWhiteSpace(value) Then
                    If Not Path.IsPathRooted(value) Then
                        value = Path.Combine(Path.GetDirectoryName(documentPath), value)
                    End If

                    value = normalizeFullPathSafe(value)
                    If isCadFilePath(value) Then output.Add(value)
                End If

                index += 2
            End While
        Catch ex As Exception
            writeOperationLog("Could not read dependencies for " & documentPath & ": " & ex.Message)
            errorMessage = ex.Message
            Return False
        End Try

        Return True
    End Function

    Private Function getDirectReferrers(ByVal graph As CadReferenceGraph,
                                        ByVal targetPath As String) As List(Of String)
        Dim output As New List(Of String)()
        If graph Is Nothing Then Return output

        For Each pair As KeyValuePair(Of String, HashSet(Of String)) In graph.DirectDependenciesByReferrer
            If pair.Value IsNot Nothing AndAlso pair.Value.Contains(targetPath) Then output.Add(pair.Key)
        Next

        Return output.OrderBy(Function(p) p, StringComparer.OrdinalIgnoreCase).ToList()
    End Function

    Private Function getOpenCadDocumentPaths() As List(Of String)
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            Dim documents As Array = TryCast(iSwApp.GetDocuments(), Array)
            If documents Is Nothing Then Return output

            For Each value As Object In documents
                Dim doc As ModelDoc2 = TryCast(value, ModelDoc2)
                If doc Is Nothing Then Continue For

                Dim p As String = normalizeFullPathSafe(doc.GetPathName())
                If String.IsNullOrWhiteSpace(p) OrElse Not isCadFilePath(p) Then Continue For
                If seen.Add(p) Then output.Add(p)
            Next
        Catch
        End Try

        Return output
    End Function

    Private Function getOpenReverseReferenceClosure(ByVal graph As CadReferenceGraph,
                                                    ByVal sourcePath As String,
                                                    ByVal openPaths As IEnumerable(Of String)) As List(Of String)
        Dim distance As New Dictionary(Of String, Integer)(StringComparer.OrdinalIgnoreCase)
        distance(sourcePath) = 0

        Dim changed As Boolean = True
        While changed
            changed = False

            For Each pair As KeyValuePair(Of String, HashSet(Of String)) In graph.DirectDependenciesByReferrer
                If distance.ContainsKey(pair.Key) OrElse pair.Value Is Nothing Then Continue For

                Dim dependencyDistances As New List(Of Integer)()
                For Each dependency As String In pair.Value
                    If distance.ContainsKey(dependency) Then dependencyDistances.Add(distance(dependency))
                Next

                If dependencyDistances.Count > 0 Then
                    distance(pair.Key) = dependencyDistances.Max() + 1
                    changed = True
                End If
            Next
        End While

        Dim openSet As New HashSet(Of String)(If(openPaths, Enumerable.Empty(Of String)()), StringComparer.OrdinalIgnoreCase)

        Return distance.Keys.
            Where(Function(p) openSet.Contains(p)).
            OrderByDescending(Function(p) distance(p)).
            ThenBy(Function(p) p, StringComparer.OrdinalIgnoreCase).
            ToList()
    End Function

    Private Function documentHasRealUnsavedChanges(ByVal doc As ModelDoc2,
                                                   ByVal filePath As String) As Boolean
        If doc Is Nothing Then Return False

        Try
            If Not doc.GetSaveFlag() Then Return False
            Return Not canTreatAssemblySaveFlagAsGuardGenerated(doc, filePath)
        Catch
            Return True
        End Try
    End Function

    Private Sub executeCadRelocation(ByVal sourcePath As String,
                                     ByVal destinationPath As String,
                                     ByVal mode As CadRelocationMode)
        Dim finalCheck As CadRelocationCheckResult = buildCadRelocationCheck(sourcePath, destinationPath)

        If finalCheck Is Nothing OrElse Not finalCheck.CanProceed Then
            iSwApp.SendMsgToUser2(
                "The files changed after the review. Nothing was moved. Click Check again.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            Return
        End If

        Dim modifiedReferrers As New List(Of String)()
        Dim newlyAddedReferrers As New List(Of String)()
        Dim originalReferrerStatuses As New Dictionary(Of String, Char)(StringComparer.OrdinalIgnoreCase)
        Dim sourceMoved As Boolean = False
        Dim originalSourceStatus As Char = getFirstSvnStatusChar(sourcePath)
        Dim operationDescription As String = If(mode = CadRelocationMode.ReId, "Re-ID CAD file", "Move CAD file")

        If Not tryBeginSolidWorksNativeMutation(operationDescription) Then
            iSwApp.SendMsgToUser2("Another SOLIDWORKS file operation is still finishing. Try again in a moment.",
                                  swMessageBoxIcon_e.swMbInformation,
                                  swMessageBoxBtn_e.swMbOk)
            Return
        End If

        cadRelocationInProgress = True

        Try
            If Not commitPathsAllowedOnlyIfUpToDate(
                (New String() {sourcePath}).Concat(If(finalCheck.DirectReferrerPaths, New String() {})).ToArray()
            ) Then Return

            makeCadRelocationPathsWritable(
                (New String() {sourcePath}).
                    Concat(If(finalCheck.DirectReferrerPaths, New String() {})).
                    Concat(If(finalCheck.OpenPathsToReopen, New String() {})).
                    Distinct(StringComparer.OrdinalIgnoreCase).
                    ToArray()
            )

            If Not saveOpenDocumentsForCadRelocation(finalCheck.OpenPathsToReopen) Then Return
            If Not closeDocumentsForCadRelocation(finalCheck.OpenPathsToReopen) Then
                reopenCadRelocationDocuments(finalCheck.OpenPathsToReopen, sourcePath, destinationPath, finalCheck.ActivePathBeforeOperation)
                Return
            End If

            Dim destinationFolderForMove As String = ""
            Try
                destinationFolderForMove = Path.GetDirectoryName(destinationPath)
            Catch
            End Try

            Dim folderVersionError As String = ""
            If Not ensureCadRelocationDestinationFolderVersioned(destinationFolderForMove, folderVersionError) Then
                reopenCadRelocationDocuments(finalCheck.OpenPathsToReopen, sourcePath, destinationPath, finalCheck.ActivePathBeforeOperation)
                iSwApp.SendMsgToUser2("The file was not moved." & vbCrLf & vbCrLf &
                                      "Could not add the new destination folder to SVN: " & folderVersionError,
                                      swMessageBoxIcon_e.swMbStop,
                                      swMessageBoxBtn_e.swMbOk)
                Return
            End If

            Dim moveError As String = ""
            If Not moveCadWorkingCopyPath(sourcePath, destinationPath, moveError) Then
                reopenCadRelocationDocuments(finalCheck.OpenPathsToReopen, sourcePath, destinationPath, finalCheck.ActivePathBeforeOperation)
                iSwApp.SendMsgToUser2("The file was not moved." & vbCrLf & vbCrLf & moveError,
                                      swMessageBoxIcon_e.swMbStop,
                                      swMessageBoxBtn_e.swMbOk)
                Return
            End If
            sourceMoved = True

            For Each referrer As String In If(finalCheck.DirectReferrerPaths, New String() {})
                originalReferrerStatuses(referrer) = getFirstSvnStatusChar(referrer)

                Try
                    iSwApp.ReplaceReferencedDocument(referrer, sourcePath, destinationPath)
                Catch
                End Try

                Dim referenceVerified As Boolean = closedDocumentReferencesPath(referrer, destinationPath, sourcePath)

                'The on-disk dependency check is authoritative. Some SOLIDWORKS versions
                'return False after writing the reference when a secondary warning occurred.
                If Not referenceVerified Then
                    Throw New InvalidOperationException("SOLIDWORKS could not safely update references in " & Path.GetFileName(referrer) & ".")
                End If

                modifiedReferrers.Add(referrer)
            Next

            For Each referrer As String In modifiedReferrers
                If originalReferrerStatuses(referrer) <> "?"c Then Continue For

                Dim addResult As rawProcessReturn = runSvnProcess(
                    sSVNPath,
                    "add --non-interactive """ & referrer & """"
                )

                If Not String.IsNullOrWhiteSpace(If(addResult.outputError, "")) Then
                    Throw New InvalidOperationException("Could not add " & Path.GetFileName(referrer) & " to SVN: " & addResult.outputError)
                End If

                newlyAddedReferrers.Add(referrer)
            Next

            If (originalSourceStatus = "?"c OrElse originalSourceStatus = "A"c) AndAlso
               Not svnPropset(New String() {destinationPath}, "addin:release_state", "||EDIT||") Then
                Throw New InvalidOperationException("Could not set the SVN release-state property on the new destination file.")
            End If

            reopenCadRelocationDocuments(finalCheck.OpenPathsToReopen, sourcePath, destinationPath, finalCheck.ActivePathBeforeOperation)

            Dim commitPaths As New List(Of String) From {destinationPath}
            If originalSourceStatus <> "?"c AndAlso originalSourceStatus <> "A"c Then commitPaths.Add(sourcePath)
            commitPaths.AddRange(modifiedReferrers)
            startCadRelocationCommitBackground(
                commitPaths.Distinct(StringComparer.OrdinalIgnoreCase).ToArray(),
                If(mode = CadRelocationMode.ReId, "Re-ID CAD file", "Move CAD file") & ": " &
                Path.GetFileName(sourcePath) & " -> " & Path.GetFileName(destinationPath)
            )

            Try
                myUserControl.queueSvnTreeStructureRefreshPublic()
            Catch
            End Try

        Catch ex As Exception
            Dim rollbackErrors As New List(Of String)()

            For Each referrer As String In newlyAddedReferrers
                Try
                    runSvnProcess(sSVNPath, "revert --non-interactive """ & referrer & """")
                Catch
                End Try
            Next

            For Each referrer As String In modifiedReferrers.AsEnumerable().Reverse()
                Try
                    iSwApp.ReplaceReferencedDocument(referrer, destinationPath, sourcePath)

                    If Not closedDocumentReferencesPath(referrer, sourcePath, destinationPath) Then
                        rollbackErrors.Add(Path.GetFileName(referrer))
                    End If
                Catch
                    rollbackErrors.Add(Path.GetFileName(referrer))
                End Try
            Next

            If sourceMoved Then
                Dim rollbackMoveError As String = ""
                If Not rollbackCadWorkingCopyMove(sourcePath, destinationPath, originalSourceStatus, rollbackMoveError) Then
                    rollbackErrors.Add("file move: " & rollbackMoveError)
                End If
            End If

            reopenCadRelocationDocuments(finalCheck.OpenPathsToReopen, sourcePath, destinationPath, finalCheck.ActivePathBeforeOperation)

            Dim message As String = "Re-ID/Move stopped and was rolled back." & vbCrLf & vbCrLf & ex.Message
            If rollbackErrors.Count > 0 Then
                message &= vbCrLf & vbCrLf & "Manual attention is required for: " & String.Join(", ", rollbackErrors)
            End If

            iSwApp.SendMsgToUser2(message, swMessageBoxIcon_e.swMbStop, swMessageBoxBtn_e.swMbOk)
        Finally
            cadRelocationInProgress = False
            endSolidWorksNativeMutation(operationDescription)
        End Try
    End Sub

    Private Function saveOpenDocumentsForCadRelocation(ByVal paths() As String) As Boolean
        If paths Is Nothing Then Return True

        For Each p As String In paths
            Dim doc As ModelDoc2 = getOpenModelByPathSafe(p)
            If doc Is Nothing OrElse Not documentHasRealUnsavedChanges(doc, p) Then Continue For

            Dim errors As Integer = 0
            Dim warnings As Integer = 0
            Dim saved As Boolean = False

            beginInternalSolidWorksSave()
            Try
                saved = doc.Save3(swSaveAsOptions_e.swSaveAsOptions_Silent, errors, warnings)
            Finally
                endInternalSolidWorksSave()
            End Try

            If Not saved Then
                iSwApp.SendMsgToUser2(
                    "Could not save " & Path.GetFileName(p) & " before the reference-safe close." & vbCrLf & vbCrLf &
                    "Errors: " & errors.ToString() & "; warnings: " & warnings.ToString(),
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If
        Next

        Return True
    End Function

    Private Sub makeCadRelocationPathsWritable(ByVal paths() As String)
        If paths Is Nothing Then Exit Sub

        For Each p As String In paths
            If String.IsNullOrWhiteSpace(p) OrElse Not File.Exists(p) Then Continue For
            If Not cadPathHasRequiredRelocationLock(p) Then Continue For

            Try
                File.SetAttributes(p, File.GetAttributes(p) And Not FileAttributes.ReadOnly)
            Catch
            End Try

            Try
                Dim doc As ModelDoc2 = getOpenModelByPathSafe(p)
                If doc IsNot Nothing Then doc.SetReadOnlyState(False)
            Catch
            End Try
        Next
    End Sub

    Private Function closeDocumentsForCadRelocation(ByVal paths() As String) As Boolean
        If paths Is Nothing Then Return True

        For Each p As String In paths
            Dim doc As ModelDoc2 = getOpenModelByPathSafe(p)
            If doc Is Nothing Then Continue For

            Dim title As String = ""
            Try
                title = doc.GetTitle()
            Catch
                title = Path.GetFileName(p)
            End Try

            iSwApp.QuitDoc(title)

            If getOpenModelByPathSafe(p) IsNot Nothing Then
                iSwApp.SendMsgToUser2(
                    "SOLIDWORKS could not temporarily close " & Path.GetFileName(p) & ". Nothing was moved.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If
        Next

        Return True
    End Function

    'Move requires the destination folder to already be under version control - a raw
    '`svn move` fails outright into an unversioned folder - but a folder just created through
    'the Browse dialog's "Make New Folder" is not versioned yet. Auto-add any unversioned
    'ancestor folders (bounded by the repo root, --depth=empty so pre-existing unrelated files
    'inside them are never swept in) so a physically new folder does not require a separate
    'manual svn add before Move will accept it. Adding (not yet committing) is sufficient for
    'svn move to succeed; the folder add rides along in the same commit as the moved file.
    Private Function ensureCadRelocationDestinationFolderVersioned(ByVal destinationFolder As String,
                                                                    ByRef errorMessage As String) As Boolean
        errorMessage = ""
        If String.IsNullOrWhiteSpace(destinationFolder) Then Return False
        If svnFolderIsVersioned(destinationFolder) Then Return True

        Dim repoRoot As String = ""
        Try
            repoRoot = Path.GetFullPath(myUserControl.localRepoPath.Text.TrimEnd("\"c)).TrimEnd("\"c)
        Catch
            repoRoot = ""
        End Try

        If String.IsNullOrWhiteSpace(repoRoot) Then
            errorMessage = "Could not resolve the configured SVN working copy root."
            Return False
        End If

        Dim unversionedChain As New List(Of String)()
        Dim currentDir As String = destinationFolder

        Try
            While Not String.IsNullOrWhiteSpace(currentDir) AndAlso
                  currentDir.StartsWith(repoRoot, StringComparison.OrdinalIgnoreCase) AndAlso
                  Not svnFolderIsVersioned(currentDir)

                unversionedChain.Insert(0, currentDir)

                Dim parentInfo As DirectoryInfo = Directory.GetParent(currentDir)
                If parentInfo Is Nothing Then Exit While
                currentDir = parentInfo.FullName.TrimEnd("\"c)
            End While
        Catch ex As Exception
            errorMessage = ex.Message
            Return False
        End Try

        If unversionedChain.Count = 0 Then Return True

        Dim addResult As rawProcessReturn = runSvnProcess(
            sSVNPath,
            "add --non-interactive --depth=empty " & quoteFilePathArgs(unversionedChain.ToArray())
        )

        If Not String.IsNullOrWhiteSpace(If(addResult.outputError, "")) Then
            errorMessage = addResult.outputError.Trim()
            Return False
        End If

        writeOperationLog("Auto-added new destination folder(s) to SVN for Move: " & String.Join(" | ", unversionedChain.ToArray()))
        Return True
    End Function

    Private Function moveCadWorkingCopyPath(ByVal oldPath As String,
                                            ByVal newPath As String,
                                            ByRef errorMessage As String) As Boolean
        errorMessage = ""
        Dim statusChar As Char = getFirstSvnStatusChar(oldPath)

        Try
            If statusChar = "?"c Then
                File.Move(oldPath, newPath)
                Dim addResult As rawProcessReturn = runSvnProcess(sSVNPath, "add --non-interactive """ & newPath & """")
                If Not String.IsNullOrWhiteSpace(If(addResult.outputError, "")) Then Throw New IOException(addResult.outputError)
                Return True
            End If

            If statusChar = "A"c Then
                Dim revertResult As rawProcessReturn = runSvnProcess(sSVNPath, "revert --non-interactive """ & oldPath & """")
                If Not String.IsNullOrWhiteSpace(If(revertResult.outputError, "")) Then Throw New IOException(revertResult.outputError)
                File.Move(oldPath, newPath)
                Dim addResult As rawProcessReturn = runSvnProcess(sSVNPath, "add --non-interactive """ & newPath & """")
                If Not String.IsNullOrWhiteSpace(If(addResult.outputError, "")) Then Throw New IOException(addResult.outputError)
                Return True
            End If

            Dim moveResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "move --non-interactive """ & oldPath & """ """ & newPath & """"
            )

            If Not String.IsNullOrWhiteSpace(If(moveResult.outputError, "")) Then
                errorMessage = moveResult.outputError.Trim()
                Return False
            End If

            Return File.Exists(newPath) AndAlso Not File.Exists(oldPath)
        Catch ex As Exception
            errorMessage = ex.Message
            Return False
        End Try
    End Function

    Private Function closedDocumentReferencesPath(ByVal referrerPath As String,
                                                  ByVal requiredPath As String,
                                                  ByVal forbiddenPath As String) As Boolean
        Dim dependencies As HashSet(Of String) = getDirectCadDependencies(referrerPath)
        Return dependencies.Contains(normalizeFullPathSafe(requiredPath)) AndAlso
               Not dependencies.Contains(normalizeFullPathSafe(forbiddenPath))
    End Function

    Private Function rollbackCadWorkingCopyMove(ByVal originalPath As String,
                                                ByVal movedPath As String,
                                                ByVal originalStatus As Char,
                                                ByRef errorMessage As String) As Boolean
        errorMessage = ""

        Try
            If originalStatus = "?"c OrElse originalStatus = "A"c Then
                runSvnProcess(sSVNPath, "revert --non-interactive """ & movedPath & """")

                If File.Exists(originalPath) Then
                    Throw New IOException("Rollback destination already exists: " & originalPath)
                End If
                If File.Exists(movedPath) Then File.Move(movedPath, originalPath)

                If originalStatus = "A"c Then
                    Dim addResult As rawProcessReturn = runSvnProcess(sSVNPath, "add --non-interactive """ & originalPath & """")
                    If Not String.IsNullOrWhiteSpace(If(addResult.outputError, "")) Then Throw New IOException(addResult.outputError)
                End If

                Return File.Exists(originalPath) AndAlso Not File.Exists(movedPath)
            End If

            Dim revertDestination As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "revert --non-interactive """ & movedPath & """"
            )
            If Not String.IsNullOrWhiteSpace(If(revertDestination.outputError, "")) Then Throw New IOException(revertDestination.outputError)

            Dim revertSource As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "revert --non-interactive """ & originalPath & """"
            )
            If Not String.IsNullOrWhiteSpace(If(revertSource.outputError, "")) Then Throw New IOException(revertSource.outputError)

            If File.Exists(movedPath) Then
                File.SetAttributes(movedPath, FileAttributes.Normal)
                File.Delete(movedPath)
            End If

            Return File.Exists(originalPath) AndAlso Not File.Exists(movedPath)
        Catch ex As Exception
            errorMessage = ex.Message
            Return False
        End Try
    End Function

    Private Sub reopenCadRelocationDocuments(ByVal paths() As String,
                                             ByVal oldSourcePath As String,
                                             ByVal newSourcePath As String,
                                             ByVal activePathBefore As String)
        If paths Is Nothing Then Return

        Dim reopenPaths As New List(Of String)(paths)
        reopenPaths.Reverse()

        For Each originalPath As String In reopenPaths
            Dim p As String = If(pathsAreSame(originalPath, oldSourcePath) AndAlso File.Exists(newSourcePath), newSourcePath, originalPath)
            If Not File.Exists(p) OrElse getOpenModelByPathSafe(p) IsNot Nothing Then Continue For

            Dim errors As Integer = 0
            Dim warnings As Integer = 0
            iSwApp.OpenDoc6(p, getSolidWorksDocumentTypeForPath(p), swOpenDocOptions_e.swOpenDocOptions_Silent, "", errors, warnings)
        Next

        Dim pathToActivate As String = activePathBefore
        If pathsAreSame(activePathBefore, oldSourcePath) AndAlso File.Exists(newSourcePath) Then pathToActivate = newSourcePath

        Dim activeDoc As ModelDoc2 = getOpenModelByPathSafe(pathToActivate)
        If activeDoc IsNot Nothing Then
            Dim activateErrors As Integer = 0
            iSwApp.ActivateDoc3(activeDoc.GetTitle(), True, swRebuildOnActivation_e.swUserDecision, activateErrors)
        End If
    End Sub

    Private Function getSolidWorksDocumentTypeForPath(ByVal filePath As String) As Integer
        Select Case Path.GetExtension(filePath).ToUpperInvariant()
            Case ".SLDPRT"
                Return swDocumentTypes_e.swDocPART
            Case ".SLDASM"
                Return swDocumentTypes_e.swDocASSEMBLY
            Case ".SLDDRW"
                Return swDocumentTypes_e.swDocDRAWING
        End Select

        Return swDocumentTypes_e.swDocNONE
    End Function

    Private Sub startCadRelocationCommitBackground(ByVal commitPaths() As String,
                                                   ByVal commitMessage As String)
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return

        Dim pathsForBackground As String() = CType(commitPaths.Clone(), String())
        Dim savedPathForBackground As String = ""
        Try
            savedPathForBackground = myUserControl.savedPATH
        Catch
        End Try

        asyncCommitInProgress = True
        invalidateOwnedLocksWholeCopySnapshotPublic()

        Task.Run(
            Sub()
                Dim success As Boolean = False
                Dim errorMessage As String = ""

                Try
                    Dim result As rawProcessReturn = runSvnProcessBackgroundNoUi(
                        sSVNPath,
                        "commit --non-interactive -m """ & commitMessage.Replace("""", "'") & """ " & quoteFilePathArgs(pathsForBackground),
                        savedPathForBackground
                    )
                    errorMessage = If(result.outputError, "").Trim()
                    success = String.IsNullOrWhiteSpace(errorMessage)
                Catch ex As Exception
                    errorMessage = ex.Message
                End Try

                Try
                    myUserControl.BeginInvoke(
                        New MethodInvoker(
                            Sub()
                                asyncCommitInProgress = False
                                invalidateOwnedLocksWholeCopySnapshotPublic()

                                If success Then
                                    For Each p As String In pathsForBackground
                                        Try
                                            If File.Exists(p) Then File.SetAttributes(p, File.GetAttributes(p) Or FileAttributes.ReadOnly)
                                        Catch
                                        End Try
                                    Next

                                    updateStatusCacheForKnownPaths(pathsForBackground, forceAddDelChg1:=" ", forceLock6:=" ", forceUpToDate9:=" ")
                                    refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False, bRebuildTree:=True)
                                    iSwApp.SendMsgToUser2("Re-ID/Move completed and committed to SVN.",
                                                          swMessageBoxIcon_e.swMbInformation,
                                                          swMessageBoxBtn_e.swMbOk)
                                Else
                                    iSwApp.SendMsgToUser2(
                                        "The CAD files and references were updated locally, but automatic commit failed." & vbCrLf & vbCrLf &
                                        errorMessage & vbCrLf & vbCrLf &
                                        "Do not revert individual files. Resolve SVN and commit the displayed move as one change.",
                                        swMessageBoxIcon_e.swMbWarning,
                                        swMessageBoxBtn_e.swMbOk
                                    )
                                End If
                            End Sub
                        )
                    )
                Catch
                    asyncCommitInProgress = False
                End Try
            End Sub
        )
    End Sub

    'Checker callback for CadRelocationReviewForm in NewSave mode. Validates a proposed
    'first-save destination for a brand-new, never-saved document: same rules the old
    'SaveFileDialog loop enforced (naming convention, inside the working copy, no existing
    'collision, required lock), just surfaced as one reviewed row instead of a retry loop
    'of native message boxes.
    Private Function checkNewDocumentSaveDestination(ByVal destinationPath As String) As CadRelocationCheckResult
        Dim result As New CadRelocationCheckResult()
        Dim row As New CadRelocationReviewRow With {
            .FilePath = destinationPath,
            .RoleText = "New file"
        }

        If pathContainsSvnAdministrativeSegment(destinationPath) Then
            row.StateText = "SVN system folder"
            row.CheckText = "Blocked"
            row.Explanation = "Choose a normal project folder. Files cannot be saved inside an SVN .svn administration folder."
            result.Rows.Add(row)
            result.Summary = "Choose a normal project folder, then Check again."
            Return result
        End If

        If Not isPathInsideLocalRepo(destinationPath) Then
            row.StateText = "Outside working copy"
            row.CheckText = "Blocked"
            row.Explanation = "Choose a folder inside the configured SVN working copy."
            result.Rows.Add(row)
            result.Summary = "Choose a folder inside the SVN working copy, then Check again."
            Return result
        End If

        If Not isVendorPartPath(destinationPath) AndAlso
           Not shouldIgnoreGrc27NamingConventionForDebug() AndAlso
           Not isValidGrc27FileName(destinationPath) Then

            row.StateText = "Name not compliant"
            row.CheckText = "Blocked"
            row.Explanation = "The file name must follow the GRC27/CFD27 naming convention (or be placed under Vendor Parts)."
            result.Rows.Add(row)
            result.Summary = "Fix the file name, then Check again."
            Return result
        End If

        If File.Exists(destinationPath) OrElse Directory.Exists(destinationPath) Then
            row.StateText = "Already exists"
            row.CheckText = "Blocked"
            row.Explanation = "A file or folder already exists at this destination."
            result.Rows.Add(row)
            result.Summary = "Choose a different name or folder, then Check again."
            Return result
        End If

        'GRC27/CFD27 names are unique identifiers across the whole project, not just within a
        'folder - saving a second file with the same name in a different folder would silently
        'create two unrelated documents sharing one name.
        If Not isVendorPartPath(destinationPath) Then
            Dim duplicateNamePath As String = getExistingRepoCadPathForFileName(
                Path.GetFileName(destinationPath),
                excludeVendorParts:=True
            )

            If Not String.IsNullOrWhiteSpace(duplicateNamePath) AndAlso
               Not pathsAreSame(duplicateNamePath, destinationPath) Then

                row.StateText = "Name already used"
                row.CheckText = "Blocked"
                row.Explanation = "A file with this exact name already exists elsewhere in the working copy:" &
                    vbCrLf & duplicateNamePath & vbCrLf &
                    "GRC27/CFD27 names must be unique across the whole project."
                result.Rows.Add(row)
                result.Summary = "Choose a different name, then Check again."
                Return result
            End If

        End If

        If Not automaticSaveTargetHasRequiredLock(destinationPath) Then
            row.StateText = "Lock needed"
            row.CheckText = "Blocked"
            row.Explanation = "PlumVault could not confirm the required lock for this destination. See the message shown."
            result.Rows.Add(row)
            result.Summary = "Resolve the lock issue, then Check again."
            Return result
        End If

        row.StateText = "Ready"
        row.CheckText = "Ready"
        row.IsReady = True
        row.Explanation = "Valid. The file will be saved here and queued for commit; any needed destination folder is created and committed first."
        result.Rows.Add(row)
        result.CanProceed = True
        result.Summary = "Ready. The new file will be saved to this name and location."
        Return result
    End Function

    Private Sub performNewDocumentSvnSave(ByVal doc As ModelDoc2,
                                          ByVal compliantFileName As String)
        Try
            If doc Is Nothing Then Exit Sub
            If String.IsNullOrWhiteSpace(compliantFileName) Then Exit Sub

            'The toolbar Save As action also supports an already-saved document. Its source
            'must be locked just like native Save As; the internal SaveAs3 call suppresses
            'normal pre-save events, so enforce that invariant here as well.
            Dim existingSourcePath As String = ""
            Try
                existingSourcePath = doc.GetPathName()
            Catch
                existingSourcePath = ""
            End Try

            If Not String.IsNullOrWhiteSpace(existingSourcePath) AndAlso
               isCadFilePath(existingSourcePath) AndAlso
               isPathInsideLocalRepo(existingSourcePath) AndAlso
               Not automaticSaveTargetHasRequiredLock(existingSourcePath) Then
                Exit Sub
            End If

            If Not isOnlineModeEnabled() Then
                iSwApp.SendMsgToUser2(
                    "Save blocked for SVN CAD." & vbCrLf & vbCrLf &
                    "Online mode is disabled, so the plugin cannot complete the required first commit." & vbCrLf & vbCrLf &
                    "Enable Online, then save again.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
                Exit Sub
            End If

            Dim repoRoot As String = ""

            Try
                repoRoot = Path.GetFullPath(myUserControl.localRepoPath.Text.Trim()).TrimEnd("\"c)
            Catch
                repoRoot = ""
            End Try

            If String.IsNullOrWhiteSpace(repoRoot) OrElse Not Directory.Exists(repoRoot) Then
                iSwApp.SendMsgToUser2(
                    "Save blocked." & vbCrLf & vbCrLf &
                    "The configured local SVN working-copy folder is unavailable.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Exit Sub
            End If

            Dim selectedPath As String = ""
            Dim owner As System.Windows.Forms.IWin32Window = getSolidWorksDialogOwner()

            'A synthetic placeholder path only seeds the table's default name/folder fields -
            'nothing is read from or checked against this exact path existing on disk.
            Dim placeholderSourcePath As String = Path.Combine(repoRoot, compliantFileName)

            Try
                Using reviewForm As New CadRelocationReviewForm(
                    placeholderSourcePath,
                    repoRoot,
                    CadRelocationMode.NewSave,
                    Function(destinationPath As String) checkNewDocumentSaveDestination(destinationPath)
                )
                    Dim dialogResult As System.Windows.Forms.DialogResult

                    If owner Is Nothing Then
                        dialogResult = reviewForm.ShowDialog()
                    Else
                        dialogResult = reviewForm.ShowDialog(owner)
                    End If

                    If dialogResult <> System.Windows.Forms.DialogResult.OK OrElse Not reviewForm.Approved Then Exit Sub

                    selectedPath = reviewForm.ApprovedDestinationPath
                End Using
            Catch ex As Exception
                iSwApp.SendMsgToUser2(
                    "The Save review table could not be opened." & vbCrLf & vbCrLf & ex.Message,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Exit Sub
            End Try

            If String.IsNullOrWhiteSpace(selectedPath) Then Exit Sub

            'A user can create a brand-new folder from the Save dialog or Windows Explorer.
            'SVN versions directories separately, so create/add/commit the destination folder
            'before SOLIDWORKS writes the new CAD file into it.
            Dim selectedFolder As String = ""

            Try
                selectedFolder = Path.GetDirectoryName(selectedPath)
            Catch
                selectedFolder = ""
            End Try

            Dim folderPreparationError As String = ""
            Dim folderCommitMessage As String =
                If(isVendorPartPath(selectedPath),
                   "Create Vendor Parts folder for first CAD save",
                   "Create CAD folder for first save")

            If Not prepareSvnDestinationFolderAndCommitIfNeeded(
                selectedFolder,
                folderCommitMessage,
                folderPreparationError) Then

                iSwApp.SendMsgToUser2(
                    "The destination folder could not be prepared in SVN." & vbCrLf & vbCrLf &
                    folderPreparationError & vbCrLf & vbCrLf &
                    "The CAD file was not saved.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Exit Sub
            End If

            Dim errors As Integer = 0
            Dim warnings As Integer = 0
            Dim saveSucceeded As Boolean = False

            beginInternalSolidWorksSave()
            Try
                saveSucceeded = doc.Extension.SaveAs3(
                    selectedPath,
                    swSaveAsVersion_e.swSaveAsCurrentVersion,
                    swSaveAsOptions_e.swSaveAsOptions_Silent,
                    Nothing,
                    Nothing,
                    errors,
                    warnings
                )
            Finally
                endInternalSolidWorksSave()
            End Try

            If Not saveSucceeded Then
                iSwApp.SendMsgToUser2(
                    "SOLIDWORKS could not save the new CAD file." & vbCrLf & vbCrLf &
                    selectedPath & vbCrLf & vbCrLf &
                    "Errors: " & errors.ToString() & vbCrLf &
                    "Warnings: " & warnings.ToString(),
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Exit Sub
            End If

            iSwApp.SendMsgToUser2(
                "New file saved:" & vbCrLf &
                selectedPath & vbCrLf & vbCrLf &
                "It will be committed to SVN automatically.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )

            queueAutomaticSaveCommitPath(selectedPath)

        Catch ex As Exception
            iSwApp.SendMsgToUser2(
                "The new SVN CAD save did not complete." & vbCrLf & vbCrLf & ex.Message,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
        Finally
            newDocumentTeamSaveWorkflowInProgress = False
        End Try
    End Sub

    Public Function handleSolidWorksFileSavePrePublic(ByVal doc As ModelDoc2,
                                                      ByVal requestedFileName As String,
                                                      ByVal isSaveAs As Boolean) As Integer
        If automaticSaveEventsSuppressed() Then Return 0
        If doc Is Nothing Then Return 0

        Dim targetPath As String = requestedFileName
        Dim currentPath As String = ""

        Try
            currentPath = doc.GetPathName()
        Catch
            currentPath = ""
        End Try

        Dim virtualOwnerPath As String = getOwningPhysicalAssemblyPathForVirtualDocument(doc)

        'A Ctrl+S on an active virtual part/subassembly changes the physical owning
        'assembly file. Enforce the assembly lock and commit the assembly instead of
        'attempting SVN operations on a temporary virtual-component path.
        If Not String.IsNullOrWhiteSpace(virtualOwnerPath) Then
            If Not isSaveAs Then targetPath = virtualOwnerPath
            currentPath = virtualOwnerPath
        End If

        'FileSaveAsNotify2 is raised before the native Save As destination is finalized.
        'For an existing SVN document, verify the source lock before allowing Save As.
        'The final destination is handled by FileSavePostNotify and the guarded commit pipeline.
        If isSaveAs Then
            If String.IsNullOrWhiteSpace(currentPath) Then Return 0
            If Not isCadFilePath(currentPath) Then Return 0
            If Not isPathInsideLocalRepo(currentPath) Then Return 0

            If Not isOnlineModeEnabled() Then
                iSwApp.SendMsgToUser2(
                    "Save As blocked for SVN CAD." & vbCrLf & vbCrLf &
                    "Online mode is disabled, so the plugin cannot verify the source lock or complete the automatic commit.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
                Return 1
            End If

            If Not automaticSaveTargetHasRequiredLock(currentPath) Then Return 1
            Return 0
        End If

        If String.IsNullOrWhiteSpace(targetPath) Then targetPath = currentPath
        If String.IsNullOrWhiteSpace(targetPath) Then Return 0
        If Not isCadFilePath(targetPath) Then Return 0

        'Never affect files outside the configured team working copy.
        If Not isPathInsideLocalRepo(targetPath) Then Return 0

        If Not isOnlineModeEnabled() Then
            iSwApp.SendMsgToUser2(
                "Save blocked for SVN CAD." & vbCrLf & vbCrLf &
                "Online mode is disabled, so the plugin cannot verify the lock or complete the automatic commit." & vbCrLf & vbCrLf &
                "Enable Online, then save again.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            Return 1
        End If

        If Not isVendorPartPath(targetPath) AndAlso
           Not shouldIgnoreGrc27NamingConventionForDebug() AndAlso
           Not isValidGrc27FileName(targetPath) Then

            iSwApp.SendMsgToUser2(
                "Save blocked." & vbCrLf & vbCrLf &
                "This SVN CAD file does not follow the GRC27/CFD27 naming convention:" & vbCrLf &
                Path.GetFileName(targetPath) & vbCrLf & vbCrLf &
                "Use Save As and enter a compliant name.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return 1
        End If

        If Not automaticSaveTargetHasRequiredLock(targetPath) Then Return 1

        Return 0
    End Function

    Private Function automaticSaveTargetHasRequiredLock(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not isPathInsideLocalRepo(filePath) Then Return True

        'A path that does not exist yet is a valid first-save/first-commit target.
        If Not File.Exists(filePath) Then Return True

        'A new CAD file is briefly versioned-but-unlocked after its first commit while the
        'automatic Get Locks request runs. Permit its local continuation save, but the queued
        'commit remains deferred until that exact lock request succeeds or fails.
        If isPostFirstCommitLockPending(filePath) Then Return True

        Dim hasLocalChanges As Boolean = False
        Dim hasLocalLockToken As Boolean = False
        Dim statusChar As Char = " "c
        Dim statusError As String = ""

        If Not tryGetLocalSvnChangeState(
            filePath,
            hasLocalChanges,
            statusError,
            hasLocalLockToken,
            statusChar) Then
            iSwApp.SendMsgToUser2(
                "Save blocked." & vbCrLf & vbCrLf &
                "The plugin could not verify SVN status for:" & vbCrLf &
                filePath & vbCrLf & vbCrLf &
                statusError & vbCrLf & vbCrLf &
                "Run Cleanup/Sync and try again.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End If

        If statusChar = "?"c OrElse statusChar = "A"c OrElse hasLocalLockToken Then Return True

        'If the task pane still showed a green row, correct its cached token immediately. Do
        'not refresh/rebuild the SOLIDWORKS tree from inside the native save callback.
        Try
            updateStatusCacheForKnownPaths(New String() {filePath}, forceLock6:=" ")
        Catch
        End Try

        iSwApp.SendMsgToUser2(
            "Save blocked." & vbCrLf & vbCrLf &
            "You do not own the SVN lock for:" & vbCrLf &
            Path.GetFileName(filePath) & vbCrLf & vbCrLf &
            "Click Get Locks first. The plugin will not save or commit a versioned SVN file without your lock.",
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )

        Return False
    End Function

    Private Function userHasLocalSvnLockTokenForPath(ByVal filePath As String,
                                                     Optional ByVal allowCachedToken As Boolean = True) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        If allowCachedToken Then
            Try
                Dim cached As SVNStatus.filePpty = Nothing

                If tryFindCachedStatusProperty(filePath, cached) AndAlso cached.lock6 = "K" Then
                    Return True
                End If
            Catch
            End Try
        End If

        Try
            Dim statusResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "status --non-interactive """ & filePath & """"
            )

            If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
                Return False
            End If

            Dim statusText As String = ""

            If statusResult.output IsNot Nothing Then statusText = statusResult.output

            Dim lines() As String = statusText.Split(
                New String() {vbCrLf, vbLf},
                StringSplitOptions.RemoveEmptyEntries
            )

            For Each line As String In lines
                If String.IsNullOrWhiteSpace(line) Then Continue For

                'SVN status column 6 is the working-copy lock token.
                If line.Length >= 6 AndAlso line(5) = "K"c Then Return True
            Next
        Catch
        End Try

        Return False
    End Function

    Public Function handleSolidWorksFileSavePostPublic(ByVal doc As ModelDoc2,
                                                       ByVal saveType As Integer,
                                                       ByVal fileName As String) As Integer
        If automaticSaveEventsSuppressed() Then Return 0
        If doc Is Nothing Then Return 0

        'Use the event filename first. For exports such as PDF/STEP, doc.GetPathName still
        'points at the source CAD file and must not accidentally trigger a CAD commit.
        Dim savedPath As String = fileName

        If String.IsNullOrWhiteSpace(savedPath) Then
            Try
                savedPath = doc.GetPathName()
            Catch
                savedPath = ""
            End Try
        End If

        Dim virtualOwnerPath As String = getOwningPhysicalAssemblyPathForVirtualDocument(doc)

        If Not String.IsNullOrWhiteSpace(virtualOwnerPath) AndAlso
           (String.IsNullOrWhiteSpace(savedPath) OrElse isSolidWorksTempOrVirtualPath(savedPath) OrElse Not isPathInsideLocalRepo(savedPath)) Then
            savedPath = virtualOwnerPath
        End If

        If String.IsNullOrWhiteSpace(savedPath) Then Return 0
        If Not isCadFilePath(savedPath) Then Return 0
        If Not isPathInsideLocalRepo(savedPath) Then Return 0

        Try
            If doc.GetType() = swDocumentTypes_e.swDocASSEMBLY Then
                clearAssemblyGuardFalseDirtyCandidate(doc)
            End If
        Catch
        End Try

        If Not isOnlineModeEnabled() Then
            iSwApp.SendMsgToUser2(
                "The SOLIDWORKS save completed locally, but automatic SVN commit was not started because Online mode is disabled." & vbCrLf & vbCrLf &
                "Enable Online and commit this file before closing SOLIDWORKS.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            Return 0
        End If

        Try
            If myUserControl IsNot Nothing AndAlso myUserControl.IsHandleCreated Then
                myUserControl.BeginInvoke(
                    New MethodInvoker(Sub() queueAutomaticSaveCommitPath(savedPath))
                )
            Else
                queueAutomaticSaveCommitPath(savedPath)
            End If
        Catch
            queueAutomaticSaveCommitPath(savedPath)
        End Try

        Return 0
    End Function

    Private Sub queueAutomaticSaveCommitPath(ByVal filePath As String)
        If String.IsNullOrWhiteSpace(filePath) Then Exit Sub
        If Not File.Exists(filePath) Then Exit Sub
        If Not isCadFilePath(filePath) Then Exit Sub
        If Not isPathInsideLocalRepo(filePath) Then Exit Sub

        Dim normalizedPath As String = normalizeSvnPath(filePath)
        If String.IsNullOrWhiteSpace(normalizedPath) Then normalizedPath = filePath

        'HashSet coalesces repeated notifications while a commit is pending. Once the current
        'commit has started, a later Ctrl+S remains queued for one follow-up commit.
        pendingAutomaticSaveCommitPaths.Add(normalizedPath)
        processPendingAutomaticSaveCommits()
    End Sub

    Private Sub claimPendingAutomaticSaveCommitPathsForManualCommit(ByVal filePaths() As String)
        If filePaths Is Nothing Then Exit Sub

        For Each filePath As String In filePaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            Dim normalizedPath As String = normalizeSvnPath(filePath)
            If String.IsNullOrWhiteSpace(normalizedPath) Then normalizedPath = filePath

            If pendingAutomaticSaveCommitPaths.Remove(normalizedPath) Then
                writeOperationLog(
                    "Manual Commit claimed pending automatic-save commit: " & normalizedPath
                )
            End If
        Next
    End Sub

    Private Sub dropCleanAutomaticSaveCommitDuplicates(ByVal committedPaths() As String)
        If committedPaths Is Nothing OrElse pendingAutomaticSaveCommitPaths.Count = 0 Then Exit Sub

        For Each committedPath As String In committedPaths
            If String.IsNullOrWhiteSpace(committedPath) Then Continue For

            Dim normalizedPath As String = normalizeSvnPath(committedPath)
            If String.IsNullOrWhiteSpace(normalizedPath) Then normalizedPath = committedPath
            If Not pendingAutomaticSaveCommitPaths.Contains(normalizedPath) Then Continue For

            Dim hasLocalChanges As Boolean = False
            Dim hasLocalLockToken As Boolean = False
            Dim statusError As String = ""

            'A delayed FileSavePostNotify can arrive after Manual Commit claimed the original
            'queue entry. Drop it only when SVN proves the just-committed path is clean; a real
            'save made after the commit began remains modified and keeps its follow-up commit.
            If tryGetLocalSvnChangeState(
                normalizedPath,
                hasLocalChanges,
                statusError,
                hasLocalLockToken) AndAlso Not hasLocalChanges Then

                pendingAutomaticSaveCommitPaths.Remove(normalizedPath)
                writeOperationLog(
                    "Dropped clean automatic-save duplicate after Manual Commit: " & normalizedPath
                )
            End If
        Next
    End Sub

    Private Function isPostFirstCommitLockPending(ByVal filePath As String) As Boolean
        Dim normalizedPath As String = normalizeFullPathSafe(filePath)
        If String.IsNullOrWhiteSpace(normalizedPath) Then Return False

        SyncLock automaticSaveStateSync
            Return postFirstCommitLockPendingPaths.Contains(normalizedPath)
        End SyncLock
    End Function

    Private Sub markPostFirstCommitLockPending(ByVal filePaths() As String,
                                                ByVal isPending As Boolean)
        If filePaths Is Nothing Then Exit Sub

        SyncLock automaticSaveStateSync
            For Each filePath As String In filePaths
                Dim normalizedPath As String = normalizeFullPathSafe(filePath)
                If String.IsNullOrWhiteSpace(normalizedPath) Then Continue For

                If isPending Then
                    postFirstCommitLockPendingPaths.Add(normalizedPath)
                Else
                    postFirstCommitLockPendingPaths.Remove(normalizedPath)
                End If
            Next
        End SyncLock
    End Sub

    Private Sub finishPostFirstCommitLockTransition(ByVal attemptedPaths() As String)
        If attemptedPaths Is Nothing Then
            SyncLock automaticSaveStateSync
                postFirstCommitLockPendingPaths.Clear()
                postFirstCommitLockRetryPaths.Clear()
            End SyncLock

            stopPostFirstCommitLockRetryTimer()
        Else
            Dim transitionedPaths() As String = attemptedPaths.Where(
                Function(path) isPostFirstCommitLockPending(path)
            ).ToArray()

            markPostFirstCommitLockPending(transitionedPaths, False)

            'SaveAs and component externalization can emit a second FileSavePostNotify while
            'the first commit/lock transition is still running. If that duplicate is now clean,
            'remove it instead of starting an empty commit or showing "not locked" after the
            'first commit already succeeded. A genuine edit remains queued and is still checked.
            For Each path As String In transitionedPaths
                Dim normalizedPath As String = normalizeFullPathSafe(path)
                If Not pendingAutomaticSaveCommitPaths.Contains(normalizedPath) Then Continue For

                Dim hasLocalChanges As Boolean = False
                Dim hasLocalLockToken As Boolean = False
                Dim statusError As String = ""

                If tryGetLocalSvnChangeState(
                    normalizedPath,
                    hasLocalChanges,
                    statusError,
                    hasLocalLockToken) AndAlso Not hasLocalChanges Then

                    pendingAutomaticSaveCommitPaths.Remove(normalizedPath)
                    writeOperationLog(
                        "Dropped clean duplicate save notification after first commit: " & normalizedPath
                    )
                End If
            Next
        End If

        processPendingAutomaticSaveCommits()
    End Sub

    Private Sub stopPostFirstCommitLockRetryTimer()
        If postFirstCommitLockRetryTimer Is Nothing Then Exit Sub

        Try
            postFirstCommitLockRetryTimer.Stop()
            postFirstCommitLockRetryTimer.Dispose()
        Catch
        End Try

        postFirstCommitLockRetryTimer = Nothing
        postFirstCommitLockRetryStartedUtc = DateTime.MinValue
    End Sub

    Private Sub queuePostFirstCommitLockTransition(ByVal filePaths() As String)
        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        markPostFirstCommitLockPending(filePaths, True)

        SyncLock automaticSaveStateSync
            For Each filePath As String In filePaths
                Dim normalizedPath As String = normalizeFullPathSafe(filePath)
                If Not String.IsNullOrWhiteSpace(normalizedPath) Then
                    postFirstCommitLockRetryPaths.Add(normalizedPath)
                End If
            Next
        End SyncLock

        If postFirstCommitLockRetryStartedUtc = DateTime.MinValue Then
            postFirstCommitLockRetryStartedUtc = DateTime.UtcNow
        End If

        tryStartPostFirstCommitLockTransition()
    End Sub

    Private Sub ensurePostFirstCommitLockRetryTimer()
        If postFirstCommitLockRetryTimer IsNot Nothing Then
            postFirstCommitLockRetryTimer.Start()
            Exit Sub
        End If

        postFirstCommitLockRetryTimer = New System.Windows.Forms.Timer() With {.Interval = 200}
        AddHandler postFirstCommitLockRetryTimer.Tick,
            Sub(sender As Object, e As EventArgs)
                tryStartPostFirstCommitLockTransition()
            End Sub
        postFirstCommitLockRetryTimer.Start()
    End Sub

    Private Sub tryStartPostFirstCommitLockTransition()
        Dim pathsToLock() As String

        SyncLock automaticSaveStateSync
            pathsToLock = postFirstCommitLockRetryPaths.Where(
                Function(path) Not String.IsNullOrWhiteSpace(path) AndAlso File.Exists(path)
            ).ToArray()
        End SyncLock

        If pathsToLock.Length = 0 Then
            stopPostFirstCommitLockRetryTimer()
            Exit Sub
        End If

        If postFirstCommitLockRetryStartedUtc <> DateTime.MinValue AndAlso
           (DateTime.UtcNow - postFirstCommitLockRetryStartedUtc).TotalSeconds >= 120.0 Then

            SyncLock automaticSaveStateSync
                For Each path As String In pathsToLock
                    postFirstCommitLockRetryPaths.Remove(path)
                Next
            End SyncLock
            stopPostFirstCommitLockRetryTimer()

            Try
                iSwApp.SendMsgToUser2(
                    "The new CAD file was committed, but PlumVault could not start its follow-up Get Locks operation." &
                    vbCrLf & vbCrLf &
                    "Select the new file and click Get Locks before editing it.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            finishPostFirstCommitLockTransition(pathsToLock)
            Exit Sub
        End If

        If asyncGetLocksInProgress Then
            'If these exact paths are already in the running request, its normal completion
            'will clear the pending transition. Otherwise wait without showing a false warning.
            If pathsToLock.All(Function(path) asyncGetLocksIncludesPath(path)) Then
                SyncLock automaticSaveStateSync
                    For Each path As String In pathsToLock
                        postFirstCommitLockRetryPaths.Remove(path)
                    Next
                End SyncLock
                stopPostFirstCommitLockRetryTimer()
            Else
                ensurePostFirstCommitLockRetryTimer()
            End If
            Exit Sub
        End If

        If Not canRunDeferredSolidWorksUiMutationPublic() Then
            ensurePostFirstCommitLockRetryTimer()
            Exit Sub
        End If

        getLocksOfPathsAsync(
            pathsToLock,
            bBreakLocks:=False,
            bUseTortoise:=False,
            sMessage:="Auto-lock after automatic save commit"
        )

        Dim lockRequestStarted As Boolean =
            asyncGetLocksInProgress AndAlso pathsToLock.All(Function(path) asyncGetLocksIncludesPath(path))

        If lockRequestStarted Then
            SyncLock automaticSaveStateSync
                For Each path As String In pathsToLock
                    postFirstCommitLockRetryPaths.Remove(path)
                Next
            End SyncLock
            stopPostFirstCommitLockRetryTimer()
            writeOperationLog(
                "Post-first-commit Get Locks started: " & String.Join(" | ", pathsToLock)
            )
        Else
            ensurePostFirstCommitLockRetryTimer()
        End If
    End Sub

    Private Sub processPendingAutomaticSaveCommits()
        If asyncCommitInProgress Then Exit Sub
        If automaticSaveCommitPreparing Then Exit Sub
        If pendingAutomaticSaveCommitPaths.Count = 0 Then Exit Sub

        'Do not misclassify a just-committed CAD file as an established unlocked file while
        'its automatic first K-token request is still in flight. Once Get Locks completes,
        'finishPostFirstCommitLockTransition re-enters this queue with the authoritative result.
        If pendingAutomaticSaveCommitPaths.Any(
            Function(path) isPostFirstCommitLockPending(path)
        ) Then Exit Sub

        Dim pathsToCommit() As String = pendingAutomaticSaveCommitPaths.ToArray()
        pendingAutomaticSaveCommitPaths.Clear()

        automaticSaveCommitPreparing = True

        Dim commitStarted As Boolean = False

        Try
            commitStarted = prepareAndStartAutomaticSaveCommit(pathsToCommit)
        Catch ex As Exception
            commitStarted = False

            Try
                iSwApp.SendMsgToUser2(
                    "SOLIDWORKS saved the file locally, but automatic SVN commit preparation failed." & vbCrLf & vbCrLf &
                    ex.Message & vbCrLf & vbCrLf &
                    "Resolve the issue and commit before closing SOLIDWORKS.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try
        Finally
            automaticSaveCommitPreparing = False
        End Try

        If Not commitStarted AndAlso pendingAutomaticSaveCommitPaths.Count > 0 Then
            Try
                If myUserControl IsNot Nothing AndAlso myUserControl.IsHandleCreated Then
                    myUserControl.BeginInvoke(New MethodInvoker(Sub() processPendingAutomaticSaveCommits()))
                End If
            Catch
            End Try
        End If
    End Sub

    Private Function prepareAndStartAutomaticSaveCommit(ByVal requestedPaths() As String) As Boolean
        Dim commitPaths() As String = filterCommitPathsInsideRepoOnly(requestedPaths)

        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return False

        If Not prepareExternalReferencesForCommitPaths(commitPaths) Then Return False

        commitPaths = expandFirstCommitAssemblyDatasetPaths(commitPaths)
        commitPaths = expandAssemblyCommitPathsWithNewFirstCommitChildren(commitPaths)
        commitPaths = filterCommitPathsInsideRepoOnly(commitPaths)

        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return False

        If Not validateCadPathNamesBeforeCommit(commitPaths) Then Return False
        If Not validateNoDuplicateCadFileNamesForPaths(commitPaths) Then Return False
        If Not commitPathsAllowedOnlyIfUpToDate(commitPaths) Then Return False
        If Not commitAssemblyChildrenAllowedOnlyIfCachedUpToDate(commitPaths) Then Return False
        If Not automaticSaveCommitPathsHaveRequiredLocks(commitPaths) Then Return False

        'The initiating document has already been saved, but a first-commit assembly can add
        'other open new children. Persist any dirty expanded documents before svn.exe reads them.
        If Not saveOpenDocsForCommitPaths(commitPaths) Then Return False

        makeFirstCommitCandidatePathsWritable(commitPaths)

        commitPaths = expandCommitPathsWithAddedParentDirectories(commitPaths)
        commitPaths = filterCommitPathsInsideRepoOnly(commitPaths)

        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return False

        'Capture new CAD paths before svn add changes ? to A. A mixed commit can contain
        'an already-versioned locked assembly plus one or more brand-new children.
        Dim firstCommitCadPaths() As String = getFirstCommitCandidateCadPaths(commitPaths)
        Dim isInitialDataset As Boolean = allCommitPathsAreFirstCommitCandidates(commitPaths)

        runSvnByArgs(commitPaths, "add", bEach:=True)

        If Not svnPropset(commitPaths, "addin:release_state", "||EDIT||") Then
            iSwApp.SendMsgToUser2(
                "Automatic commit blocked." & vbCrLf & vbCrLf &
                "The plugin could not set the SVN release-state property.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End If

        Dim commitMessage As String

        If isInitialDataset Then
            commitMessage = "Initial CAD commit from SOLIDWORKS save"
        Else
            Dim savedNames As New List(Of String)()

            For Each p As String In commitPaths
                If String.IsNullOrWhiteSpace(p) OrElse Directory.Exists(p) Then Continue For
                savedNames.Add(Path.GetFileName(p))
            Next

            commitMessage = "Automatic SOLIDWORKS save"
            If savedNames.Count > 0 Then commitMessage &= ": " & String.Join(", ", savedNames.Distinct().Take(8))
        End If

        startAutomaticSaveCommitBackground(commitPaths, commitMessage, isInitialDataset, firstCommitCadPaths)
        Return True
    End Function

    Private Function automaticSaveCommitPathsHaveRequiredLocks(ByVal commitPaths() As String,
                                                                 Optional ByVal operationLabel As String = "Automatic commit",
                                                                 Optional ByVal retryInstruction As String = "Get Locks, save again, and the plugin will commit automatically.",
                                                                 Optional ByVal knownLiveLockedPaths As HashSet(Of String) = Nothing) As Boolean
        If commitPaths Is Nothing Then Return False

        Dim missingLocks As New List(Of String)()

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Directory.Exists(p) Then Continue For
            If Not File.Exists(p) Then Continue For
            If Not isCadFilePath(p) Then Continue For
            If isFirstCommitCandidatePath(p) Then Continue For

            Dim hasRequiredLock As Boolean = If(
                knownLiveLockedPaths Is Nothing,
                userHasLocalSvnLockTokenForPath(p, allowCachedToken:=False),
                knownLiveLockedPaths.Contains(normalizeFullPathSafe(p))
            )

            If Not hasRequiredLock Then
                missingLocks.Add(p)
            End If
        Next

        If missingLocks.Count = 0 Then Return True

        iSwApp.SendMsgToUser2(
            operationLabel & " blocked." & vbCrLf & vbCrLf &
            "These versioned CAD files are not locked by you:" & vbCrLf &
            stringArrToSingleStringWithNewLines(missingLocks.ToArray(), bTrimFileNames:=True, iLimit:=10) & vbCrLf &
            retryInstruction,
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )

        Return False
    End Function

    Private Sub startAutomaticSaveCommitBackground(ByVal commitPaths() As String,
                                                   ByVal commitMessage As String,
                                                   ByVal isInitialDataset As Boolean,
                                                   ByVal firstCommitCadPaths() As String)
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Exit Sub

        If asyncCommitInProgress Then
            For Each p As String In commitPaths
                If Not String.IsNullOrWhiteSpace(p) Then pendingAutomaticSaveCommitPaths.Add(p)
            Next
            Exit Sub
        End If

        Dim pathsForBackground() As String = CType(commitPaths.Clone(), String())
        Dim firstCommitPathsForCompletion() As String = Nothing

        If firstCommitCadPaths IsNot Nothing Then
            firstCommitPathsForCompletion = CType(firstCommitCadPaths.Clone(), String())
        End If

        Dim savedPathForBackground As String = ""

        Try
            savedPathForBackground = myUserControl.savedPATH
        Catch
            savedPathForBackground = ""
        End Try

        Dim safeMessage As String = If(commitMessage, "").Replace("""", "'")
        If String.IsNullOrWhiteSpace(safeMessage) Then safeMessage = "Automatic SOLIDWORKS save"

        asyncCommitInProgress = True

        'Do not alter TreeView node text during automatic Save/Ctrl+S commits.
        'The commit runs silently; failures are still shown to the user.
        Task.Run(
            Sub()
                Dim success As Boolean = False
                Dim errorMessage As String = ""

                Try
                    Dim noUnlockArg As String = If(isInitialDataset, "", "--no-unlock ")
                    Dim result As rawProcessReturn = runSvnProcessBackgroundNoUi(
                        sSVNPath,
                        "commit --non-interactive " & noUnlockArg &
                        "-m """ & safeMessage & """ " &
                        quoteFilePathArgs(pathsForBackground),
                        savedPathForBackground
                    )

                    If result.outputError IsNot Nothing AndAlso result.outputError.Trim() <> "" Then
                        errorMessage = result.outputError.Trim()
                    Else
                        success = True
                    End If
                Catch ex As Exception
                    success = False
                    errorMessage = ex.Message
                End Try

                Try
                    If myUserControl IsNot Nothing AndAlso myUserControl.IsHandleCreated Then
                        myUserControl.BeginInvoke(
                            New MethodInvoker(
                                Sub()
                                    finishAutomaticSaveCommitOnMainThread(
                                        pathsForBackground,
                                        success,
                                        errorMessage,
                                        isInitialDataset,
                                        firstCommitPathsForCompletion
                                    )
                                End Sub
                            )
                        )
                    Else
                        asyncCommitInProgress = False
                    End If
                Catch
                    asyncCommitInProgress = False
                End Try
            End Sub
        )
    End Sub

    Private Sub finishAutomaticSaveCommitOnMainThread(ByVal commitPaths() As String,
                                                       ByVal success As Boolean,
                                                       ByVal errorMessage As String,
                                                       ByVal isInitialDataset As Boolean,
                                                       ByVal firstCommitCadPaths() As String)
        asyncCommitInProgress = False

        Try
            myUserControl.markCommitPendingForFilePathsPublic(commitPaths, False)
        Catch
        End Try

        If Not success Then
            iSwApp.SendMsgToUser2(
                "SOLIDWORKS saved the file locally, but the automatic SVN commit did not complete." & vbCrLf & vbCrLf &
                errorMessage & vbCrLf & vbCrLf &
                "Your local save is still present. Resolve the SVN issue and commit before closing.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )

            processPendingAutomaticSaveCommits()
            Exit Sub
        End If

        Try
            myUserControl.markCommitResultForFilePathsPublic(commitPaths, True)
        Catch
        End Try

        Try
            If isInitialDataset Then
                updateStatusCacheForKnownPaths(
                    commitPaths,
                    forceAddDelChg1:=" ",
                    forceLock6:=" ",
                    forceUpToDate9:=" "
                )
            Else
                'Normal automatic saves use --no-unlock, so existing user-owned locks remain held.
                updateStatusCacheForKnownPaths(
                    commitPaths,
                    forceAddDelChg1:=" ",
                    forceLock6:="K",
                    forceUpToDate9:=" "
                )

                'New files in a mixed assembly commit are not locked yet. Correct their cache
                'entry before the asynchronous post-commit lock request runs.
                If firstCommitCadPaths IsNot Nothing AndAlso firstCommitCadPaths.Length > 0 Then
                    updateStatusCacheForKnownPaths(
                        firstCommitCadPaths,
                        forceAddDelChg1:=" ",
                        forceLock6:=" ",
                        forceUpToDate9:=" "
                    )
                End If
            End If

            'A newly-added CAD file changes the tree structure, so rebuild the current tree once.
            'Normal Ctrl+S commits only change SVN status and keep the faster recolor-only path.
            Dim shouldRebuildTreeAfterCommit As Boolean =
                firstCommitCadPaths IsNot Nothing AndAlso firstCommitCadPaths.Length > 0

            refreshActiveTreeAfterSvnAction(
                bUpdateLocalLockStatus:=False,
                bRebuildTree:=shouldRebuildTreeAfterCommit
            )
        Catch
        End Try

        If isInitialDataset Then
            iSwApp.SendMsgToUser2(
                "Initial commit completed." & vbCrLf & vbCrLf &
                "The new CAD dataset was added and pushed to SVN automatically." & vbCrLf & vbCrLf &
                "The plugin will now get locks so the new files remain writable.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
        Else
            'Keep existing files writable because --no-unlock preserves the user's SVN lock.
            Try
                Dim retainedLockPaths() As String = commitPaths.Where(
                    Function(p)
                        If String.IsNullOrWhiteSpace(p) OrElse Not File.Exists(p) Then Return False
                        Return firstCommitCadPaths Is Nothing OrElse
                               Not firstCommitCadPaths.Any(Function(newPath) pathsAreSame(newPath, p))
                    End Function
                ).ToArray()

                If retainedLockPaths.Length > 0 Then
                    myUserControl.forceWriteAccessForLockedFilePathsPublic(retainedLockPaths)
                End If
            Catch
            End Try
        End If

        'Pure first commits and mixed assembly commits both need locks on every newly-added CAD file.
        If firstCommitCadPaths IsNot Nothing AndAlso firstCommitCadPaths.Length > 0 Then
            queuePostFirstCommitLockTransition(firstCommitCadPaths)
        End If

        processPendingAutomaticSaveCommits()
    End Sub

    Private Function getOnlineCheckBoxFromControl(ByVal ctrl As Object) As System.Windows.Forms.CheckBox
        If ctrl Is Nothing Then Return Nothing

        Try
            Dim ctrlType As Type = ctrl.GetType()

            Dim fieldInfo As System.Reflection.FieldInfo = ctrlType.GetField(
                "onlineCheckBox",
                System.Reflection.BindingFlags.Instance Or System.Reflection.BindingFlags.Public Or System.Reflection.BindingFlags.NonPublic
            )

            If fieldInfo IsNot Nothing Then
                Dim fieldValue As Object = fieldInfo.GetValue(ctrl)
                Dim checkBox As System.Windows.Forms.CheckBox = TryCast(fieldValue, System.Windows.Forms.CheckBox)
                If checkBox IsNot Nothing Then Return checkBox
            End If

            Dim propInfo As System.Reflection.PropertyInfo = ctrlType.GetProperty(
                "onlineCheckBox",
                System.Reflection.BindingFlags.Instance Or System.Reflection.BindingFlags.Public Or System.Reflection.BindingFlags.NonPublic
            )

            If propInfo IsNot Nothing Then
                Dim propValue As Object = propInfo.GetValue(ctrl, Nothing)
                Dim checkBox As System.Windows.Forms.CheckBox = TryCast(propValue, System.Windows.Forms.CheckBox)
                If checkBox IsNot Nothing Then Return checkBox
            End If
        Catch
        End Try

        Return Nothing
    End Function

    Private Function isOnlineModeEnabled() As Boolean
        Dim checkBox As System.Windows.Forms.CheckBox = getOnlineCheckBoxFromControl(myUserControl)
        If checkBox Is Nothing Then Return False

        Try
            Return checkBox.Checked
        Catch
            Return False
        End Try
    End Function

    Private Sub setOnlineModeEnabled(ByVal enabled As Boolean)
        setOnlineModeEnabledOnControl(myUserControl, enabled)
    End Sub

    Private Sub setOnlineModeEnabledOnControl(ByVal ctrl As Object, ByVal enabled As Boolean)
        Dim checkBox As System.Windows.Forms.CheckBox = getOnlineCheckBoxFromControl(ctrl)
        If checkBox Is Nothing Then Exit Sub

        Try
            checkBox.Checked = enabled
        Catch
        End Try
    End Sub
    Private Function debugTimingEnabled() As Boolean
        Try
            If myUserControl Is Nothing Then Return False
            Return myUserControl.debugTimingEnabledPublic()
        Catch
            Return False
        End Try
    End Function

    Private Function syncStatusInProgressOnControl() As Boolean
        Try
            If myUserControl Is Nothing Then Return False
            Return myUserControl.syncStatusInProgressPublic()
        Catch
            Return False
        End Try
    End Function

    Private Sub showSvnTimingDebugWindow(ByVal title As String, ByVal debugNotes As List(Of String))
        Try
            If Not debugTimingEnabled() Then Exit Sub

            Dim msg As New System.Text.StringBuilder()
            msg.AppendLine(title)
            msg.AppendLine()

            If debugNotes IsNot Nothing Then
                For Each line As String In debugNotes
                    msg.AppendLine(line)
                Next
            End If

            System.Windows.Forms.MessageBox.Show(
                msg.ToString(),
                "SVN Timing Debug",
                System.Windows.Forms.MessageBoxButtons.OK,
                System.Windows.Forms.MessageBoxIcon.Information
            )
        Catch
        End Try
    End Sub

    Private Function countStringArrayItems(ByVal values() As String) As Integer
        If values Is Nothing Then Return 0

        Dim count As Integer = 0

        For Each value As String In values
            If Not String.IsNullOrWhiteSpace(value) Then count += 1
        Next

        Return count
    End Function

    Private Function compactNonBlankStringArray(ByVal values() As String) As String()
        If values Is Nothing Then Return Nothing

        Dim output As New List(Of String)()

        For Each value As String In values
            If String.IsNullOrWhiteSpace(value) Then Continue For
            output.Add(value)
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function distinctExistingCadFilePaths(ByVal inputPaths() As String) As String()
        Dim filteredPaths() As String = filterExistingCadFilePathsOnly(inputPaths)
        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each filePath As String In filteredPaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            Dim normalizedPath As String = filePath

            Try
                normalizedPath = Path.GetFullPath(filePath)
            Catch
            End Try

            If seen.Contains(normalizedPath) Then Continue For
            seen.Add(normalizedPath)
            output.Add(normalizedPath)
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function userAcceptsLossOfChangesPaths(ByVal filePaths() As String, Optional ByVal msg As String = "") As Boolean
        Dim filteredPaths() As String = distinctExistingCadFilePaths(filePaths)

        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No valid CAD file paths were selected.", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            Return False
        End If

        Dim userPickMsg As swMessageBoxResult_e
        userPickMsg = iSwApp.SendMsgToUser2(msg & vbCrLf &
                                            "WARNING: Changes to the selected files will be lost!" & vbCrLf &
                                            stringArrToSingleStringWithNewLines(filteredPaths, bTrimFileNames:=True, iLimit:=10),
                              Icon:=swMessageBoxIcon_e.swMbWarning, Buttons:=swMessageBoxBtn_e.swMbOkCancel)

        Return userPickMsg = swMessageBoxResult_e.swMbHitOk
    End Function

    Private Sub attachOpenDocsToStatusPaths(ByRef status As SVNStatus)
        If status Is Nothing Then Exit Sub
        If status.fp Is Nothing Then Exit Sub
        If iSwApp Is Nothing Then Exit Sub

        Try
            For i As Integer = 0 To UBound(status.fp)
                Dim filePath As String = status.fp(i).filename
                If String.IsNullOrWhiteSpace(filePath) Then Continue For

                Try
                    status.fp(i).modDoc = TryCast(iSwApp.GetOpenDocumentByName(filePath), ModelDoc2)
                Catch
                    status.fp(i).modDoc = Nothing
                End Try
            Next
        Catch
        End Try
    End Sub

    Private Function getCachedServerStatusForExactPaths(ByVal filePaths() As String,
                                                        Optional ByVal requireEveryPathCached As Boolean = True) As SVNStatus
        Dim filteredPaths() As String = distinctExistingCadFilePaths(filePaths)
        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then Return Nothing

        Dim entries As New List(Of SVNStatus.filePpty)()
        Dim missingCount As Integer = 0

        For Each filePath As String In filteredPaths
            Dim cached As SVNStatus.filePpty = Nothing
            Dim found As Boolean = False

            Try
                found = tryFindCachedStatusProperty(filePath, cached)
            Catch
                found = False
            End Try

            If Not found Then
                missingCount += 1
                If requireEveryPathCached Then Return Nothing
                Continue For
            End If

            'Only trust cached data for Get Latest if it came from a server-aware Sync.
            'Local-only status has upToDate9 = NoUpdate and cannot safely decide whether Get Latest is needed.
            If String.IsNullOrWhiteSpace(cached.upToDate9) OrElse String.Equals(cached.upToDate9, "NoUpdate", StringComparison.OrdinalIgnoreCase) Then
                missingCount += 1
                If requireEveryPathCached Then Return Nothing
                Continue For
            End If

            cached.filename = filePath
            cached.modDoc = Nothing
            Try
                cached.modDoc = TryCast(iSwApp.GetOpenDocumentByName(filePath), ModelDoc2)
            Catch
                cached.modDoc = Nothing
            End Try

            entries.Add(cached)
        Next

        If entries.Count = 0 Then Return Nothing

        Dim cachedStatus As New SVNStatus()
        cachedStatus.fp = entries.ToArray()
        Return cachedStatus
    End Function

    Private Function getLockedPathsFromStatus(ByVal status As SVNStatus) As String()
        If status Is Nothing OrElse status.fp Is Nothing Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            For i As Integer = 0 To UBound(status.fp)
                If status.fp(i).lock6 <> "K" Then Continue For
                Dim filePath As String = status.fp(i).filename
                If String.IsNullOrWhiteSpace(filePath) Then Continue For

                Try
                    filePath = Path.GetFullPath(filePath)
                Catch
                End Try

                If seen.Contains(filePath) Then Continue For
                seen.Add(filePath)
                output.Add(filePath)
            Next
        Catch
        End Try

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    'Full-sweep reconciliation (every locked+open document, on every status refresh) can flip
    'several documents from read-only to writable in the same pass, and each such transition is
    'the trigger that can cascade a spurious SOLIDWORKS rebuild/dirty-flag onto siblings. Narrow
    'the set to only the document(s) the user is actually interacting with right now - the
    'active document, plus any child currently being edited in-context (which is never itself
    'ActiveDoc while the parent assembly window has focus) - so a stale lock token on a document
    'the user hasn't touched yet doesn't get force-flipped writable until it is actually needed.
    Private Function getActiveInteractionPathsFromCandidates(ByVal candidatePaths() As String) As String()
        If candidatePaths Is Nothing OrElse candidatePaths.Length = 0 Then Return Nothing

        Dim candidateSet As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each candidatePath As String In candidatePaths
            If String.IsNullOrWhiteSpace(candidatePath) Then Continue For

            Dim normalizedCandidate As String = candidatePath
            Try
                normalizedCandidate = Path.GetFullPath(candidatePath)
            Catch
            End Try

            candidateSet.Add(normalizedCandidate)
        Next

        If candidateSet.Count = 0 Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            If iSwApp IsNot Nothing Then
                Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)

                If activeDoc IsNot Nothing Then
                    Dim activePath As String = ""

                    Try
                        activePath = Path.GetFullPath(activeDoc.GetPathName())
                    Catch
                        activePath = ""
                    End Try

                    If Not String.IsNullOrWhiteSpace(activePath) AndAlso
                       candidateSet.Contains(activePath) AndAlso seen.Add(activePath) Then

                        output.Add(activePath)
                    End If
                End If
            End If
        Catch
        End Try

        Try
            SyncLock assemblyGuardSync
                For Each session As InContextEditSession In inContextEditSessionByAssemblyPath.Values
                    If session Is Nothing Then Continue For
                    If session.EndedUtc <> DateTime.MinValue Then Continue For 'Only still-active sessions.

                    Dim childPath As String = session.ChildPath
                    If String.IsNullOrWhiteSpace(childPath) Then Continue For

                    Try
                        childPath = Path.GetFullPath(childPath)
                    Catch
                    End Try

                    If candidateSet.Contains(childPath) AndAlso seen.Add(childPath) Then
                        output.Add(childPath)
                    End If
                Next
            End SyncLock
        Catch
        End Try

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function getActiveInteractionLockedPaths(ByVal status As SVNStatus) As String()
        Return getActiveInteractionPathsFromCandidates(getLockedPathsFromStatus(status))
    End Function

    'Read-only enforcement for documents whose lock the user just RELEASED while they are
    'still open. Without this, the document stays internally writable and SOLIDWORKS will
    'happily accept drags/dimension edits that only the warn-only post guards can catch.
    'Restoring the internal read-only state makes SOLIDWORKS itself refuse those edits.
    '
    'Deliberately narrow, because broad SetReadOnlyState(True) sweeps during FileOpen/
    'ActiveDocChange are a documented source of native prompts, false save flags, and
    'unstable feature-edit state. This runs only on explicit Release Locks completion,
    'deferred to a clean UI turn, and every path must pass all gates:
    '  - the document is open, and SOLIDWORKS reports it currently writable
    '  - it is CLEAN (GetSaveFlag=False; Release Locks just reverted it) - a dirty document
    '    is never transitioned, so this can never create a discard/save question
    '  - it is not virtual/slow-flagged (unknown native rebuild cost) and not an in-flight
    '    Edit Component replay target
    'Failures are logged, never shown: the edit guards remain the warning backstop.
    Public Sub restoreInternalReadOnlyForReleasedPathsPublic(ByVal releasedPaths() As String)
        If releasedPaths Is Nothing OrElse releasedPaths.Length = 0 Then Exit Sub
        If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse
           Not myUserControl.IsHandleCreated Then Exit Sub

        Dim pathsCopy() As String = releasedPaths.Clone()

        Try
            myUserControl.BeginInvoke(
                New MethodInvoker(
                    Sub()
                        For Each releasedPath As String In pathsCopy
                            Try
                                If String.IsNullOrWhiteSpace(releasedPath) Then Continue For
                                If shouldSkipBackgroundWritableTransitionPublic(releasedPath) Then Continue For
                                If isPendingInContextAutoEditTargetPublic(releasedPath) Then Continue For

                                Dim openDocument As ModelDoc2 = getOpenModelByPathSafe(releasedPath)
                                If openDocument Is Nothing Then Continue For
                                If openDocument.IsOpenedReadOnly() Then Continue For
                                If openDocument.GetSaveFlag() Then
                                    writeOperationLog(
                                        "Released document kept writable (unsaved changes present): " & releasedPath
                                    )
                                    Continue For
                                End If

                                openDocument.SetReadOnlyState(True)

                                writeOperationLog(
                                    "Internal read-only restored after lock release: " & releasedPath &
                                    "; nowReadOnly=" & openDocument.IsOpenedReadOnly().ToString()
                                )
                            Catch ex As Exception
                                Try
                                    writeOperationLog(
                                        "Internal read-only restore failed (edit guards remain active): " &
                                        releasedPath & "; " & ex.Message
                                    )
                                Catch
                                End Try
                            End Try
                        Next
                    End Sub
                )
            )
        Catch
        End Try
    End Sub

    'Called on ActiveDocChangeNotify and right after an in-context edit begins, so a document
    'the user just switched/edited into is reconciled immediately rather than waiting for the
    'next broad status refresh to happen to catch it.
    Public Sub reconcileWriteAccessForActiveDocumentPublic()
        Try
            If myUserControl Is Nothing OrElse iSwApp Is Nothing Then Exit Sub

            Dim priorityLockedPaths() As String = getActiveInteractionLockedPaths(statusOfAllOpenModels)

            If priorityLockedPaths IsNot Nothing AndAlso priorityLockedPaths.Length > 0 Then
                myUserControl.forceWriteAccessForLockedFilePathsPublic(priorityLockedPaths)
            End If
        Catch
        End Try
    End Sub

    'Complements reconcileWriteAccessForActiveDocumentPublic above: restores read-only for the
    'Restores the on-disk read-only attribute for an unlocked managed file. Do not call
    'ModelDoc2.SetReadOnlyState(True) after a document is already open: live mode transitions
    'during FileOpen/ActiveDocChange have produced native read-only prompts, false save flags,
    'and unstable feature-edit state after close/reopen. The disk attribute governs the next
    'open; edit guards remain the protection for an already-open stale-writable document.
    Public Sub reconcileReadOnlyForUnlockedActiveDocumentPublic()
        Try
            If iSwApp Is Nothing Then Exit Sub

            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDoc Is Nothing Then Exit Sub
            If Not isCadDocument(activeDoc) Then Exit Sub

            Dim activePath As String = ""
            Try
                activePath = activeDoc.GetPathName()
            Catch
                activePath = ""
            End Try

            If String.IsNullOrWhiteSpace(activePath) Then Exit Sub
            If Not isPathInsideLocalRepo(activePath) Then Exit Sub
            If isNewUnversionedOrAddedFile(activePath) Then Exit Sub
            If assemblyHasRequiredLockFast(activeDoc) Then Exit Sub

            Dim isDirty As Boolean = True
            Try
                isDirty = activeDoc.GetSaveFlag()
            Catch
                isDirty = True
            End Try

            If isDirty Then Exit Sub

            Try
                If File.Exists(activePath) Then
                    File.SetAttributes(activePath, File.GetAttributes(activePath) Or FileAttributes.ReadOnly)
                End If
            Catch
            End Try

            writeOperationLog("Restored on-disk read-only for unlocked, clean active document: " & activePath)
        Catch
        End Try
    End Sub

    Private Function getLockedModifiedPathsFromStatus(ByVal status As SVNStatus) As String()
        If status Is Nothing OrElse status.fp Is Nothing Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            For i As Integer = 0 To UBound(status.fp)
                If status.fp(i).lock6 <> "K" Then Continue For
                If status.fp(i).addDelChg1 <> "M" Then Continue For

                Dim filePath As String = status.fp(i).filename
                If String.IsNullOrWhiteSpace(filePath) Then Continue For

                Try
                    filePath = Path.GetFullPath(filePath)
                Catch
                End Try

                If seen.Contains(filePath) Then Continue For
                seen.Add(filePath)
                output.Add(filePath)
            Next
        Catch
        End Try

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function getExistingCadFilePathsFromDocs(ByVal modDocArr() As ModelDoc2) As String()
        If modDocArr Is Nothing Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each doc As ModelDoc2 In modDocArr
            If doc Is Nothing Then Continue For

            Dim docPath As String = ""

            Try
                docPath = doc.GetPathName()
            Catch
                docPath = ""
            End Try

            If String.IsNullOrWhiteSpace(docPath) Then Continue For
            If Not File.Exists(docPath) Then Continue For
            If Not isCadFilePath(docPath) Then Continue For

            Try
                docPath = Path.GetFullPath(docPath)
            Catch
            End Try

            If seen.Contains(docPath) Then Continue For
            seen.Add(docPath)
            output.Add(docPath)
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function


    Public Function updateLockStatusPublic(Optional bRefreshAllTreeViews As Boolean = True,
                                           Optional ByVal filePathsToRefresh() As String = Nothing) As Boolean
        updateLockStatusPublic = statusOfAllOpenModels.updateStatusLocally(iSwApp, filePathsToRefresh)

        'Local lock/status refreshes may preserve the last server-aware upToDate9 values,
        'but they are not a new Sync and must not reset the displayed Sync age.
        rebuildStatusCacheFromStatus(statusOfAllOpenModels, markAsServerSync:=False)

        'If the local working copy still has K lock tokens, immediately reconcile open
        'SOLIDWORKS documents to writable. This repairs stale cache/read-only states without
        'requiring the unsafe unlock-then-relock workaround and without changing unlocked docs.
        'Scoped to the document(s) actually being interacted with (see
        'getActiveInteractionLockedPaths) rather than every locked+open document, so a stale
        'lock token on a document the user hasn't touched yet isn't force-flipped writable in
        'the same pass as others - each such flip is a chance to trigger a SOLIDWORKS rebuild
        'that can cascade a spurious dirty flag onto sibling documents.
        Try
            Dim priorityLockedPaths() As String = getActiveInteractionLockedPaths(statusOfAllOpenModels)

            If priorityLockedPaths IsNot Nothing AndAlso priorityLockedPaths.Length > 0 Then
                myUserControl.forceWriteAccessForLockedFilePathsPublic(priorityLockedPaths)
            End If
        Catch
        End Try

        If bRefreshAllTreeViews Then myUserControl.refreshAllTreeViewsVariable()
    End Function
    Public Function updateStatusOfAllModelsVariable(Optional bRefreshAllTreeViews As Boolean = False) As Boolean
        Dim bWhatToReturn As Boolean = False

        bWhatToReturn = statusOfAllOpenModels.updateFromSvnServer(bRefreshAllTreeViews)
        rebuildStatusCacheFromStatus(statusOfAllOpenModels, markAsServerSync:=bWhatToReturn)

        If bRefreshAllTreeViews And bWhatToReturn Then
            myUserControl.refreshAllTreeViewsVariable()
        End If
        Return bWhatToReturn
    End Function
    Public Function liveCheckForAssemblyServerChangesOnly(ByRef modDocArr() As ModelDoc2) As Boolean
        If liveAssemblyChangeCheckInProgress Then Return False
        If myUserControl Is Nothing Then Return False
        If iSwApp Is Nothing Then Return False
        If modDocArr Is Nothing Then Return False
        If modDocArr.Length = 0 Then Return False
        If Not isOnlineModeEnabled() Then Return False

        liveAssemblyChangeCheckInProgress = True

        Try
            Dim liveStatus As SVNStatus = getFileSVNStatus(
            bCheckServer:=True,
            modDocArr:=modDocArr,
            bUpdateStatusOfAllOpenModels:=False
        )

            If liveStatus Is Nothing Then Return False

            Dim outOfDateFiles As String() = liveStatus.sFilterUpToDate9("*")

            Return outOfDateFiles IsNot Nothing

        Catch
            Return False
        Finally
            liveAssemblyChangeCheckInProgress = False
        End Try
    End Function


    Private Function documentCloseReviewIsApproved(ByVal filePath As String) As Boolean
        If DateTime.Now >= documentLockReviewApprovedUntil Then Return False
        If String.IsNullOrWhiteSpace(filePath) OrElse String.IsNullOrWhiteSpace(documentLockReviewApprovedPath) Then Return False
        Return pathsAreSame(filePath, documentLockReviewApprovedPath)
    End Function

    Private Function documentCloseReviewCoveredPath(ByVal filePath As String) As Boolean
        If DateTime.Now >= documentLockReviewApprovedUntil Then Return False
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        Return documentLockReviewApprovedPaths.Contains(normalizeFullPathSafe(filePath))
    End Function

    Private Function applicationCloseReviewApprovedPath(ByVal filePath As String) As Boolean
        If DateTime.Now >= applicationLockReviewApprovedUntil Then Return False
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        Return applicationLockReviewApprovedPaths.Contains(normalizeFullPathSafe(filePath))
    End Function

    Public Function blockCloseIfOpenDocsUnsafe() As Boolean
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False

        'While the reviewed close is asynchronously unwinding nested edit contexts, a second
        'user click on the big X must remain blocked rather than racing the first close. Once
        'the verified documents are all gone, however, ExitApp can post WM_CLOSE after its COM
        'call has already returned. Let that final native close through; otherwise our own
        'controlledApplicationExitInProgress flag swallows it and makes the big X look dead.
        If controlledApplicationNativeCloseCallInProgress Then Return False
        If controlledApplicationExitInProgress Then
            If Not hasAnyOpenSolidWorksDocument() Then Return False
            Return True
        End If
        If controlledApplicationCloseQueued Then Return True

        If hasFreshControlledCloseQueuedPaths() Then
            applicationCloseRequestedAfterDocumentClose = True
            'Unlike the two silent checks above (a review table or the final ExitApp sequence is
            'already visibly in progress), nothing else is necessarily on screen here - a document
            'close is quietly unwinding in the background. Tell the user why the X did nothing
            'instead of leaving it looking broken, but only once per few seconds so repeatedly
            'clicking X during the normal brief window does not spam dialogs.
            If (DateTime.UtcNow - lastControlledCloseQueueBlockedMessageUtc).TotalSeconds >= 5.0 Then
                lastControlledCloseQueueBlockedMessageUtc = DateTime.UtcNow
                Try
                    iSwApp.SendMsgToUser2(
                        "PlumVault is still finishing the reviewed document close." & vbCrLf & vbCrLf &
                        "SOLIDWORKS will continue closing automatically as soon as that document close finishes.",
                        swMessageBoxIcon_e.swMbInformation,
                        swMessageBoxBtn_e.swMbOk
                    )
                Catch
                End Try
            End If
            Return True
        End If

        If closeMustWaitForActiveOperation() Then Return True

        'This safety check still runs when Online is off. In that mode all checks use
        'GetSaveFlag and local SVN status/lock tokens; server status is requested only
        'when Online is enabled. Retained locks and uncommitted edits remain relevant offline.
        Dim skipRepeatedLockReview As Boolean = DateTime.Now < applicationLockReviewApprovedUntil

        If Not skipRepeatedLockReview Then
            applicationLockReviewApprovedPaths.Clear()
        End If

        'Present actionable locked files first. A novice can commit or revert from one table
        'instead of dismissing a warning and hunting for the correct tree command.
        If Not skipRepeatedLockReview Then
            If blockCloseForOwnedLocks(
                isClosingSolidWorks:=True,
                closingDocumentPath:=""
            ) Then Return True
        End If

        'Still inspect every document that was not explicitly covered by the table. Reviewed
        'paths are skipped because Continue is now the final no-further-save decision; new or
        'unlocked unsafe documents remain protected by the legacy fail-safe.
        If blockCloseIfOpenDocsUnsafeOnly() Then Return True

        'All PlumVault safety checks have passed. Do not hand dirty-but-SVN-clean documents
        'back to native SOLIDWORKS close, because its stale GetSaveFlag values would produce a
        'misleading "Save modified documents" prompt after the user already committed/released
        'everything in the review table. Queue a controlled close outside this WM_CLOSE callback.
        If hasAnyOpenSolidWorksDocument() Then
            Return queueVerifiedSafeApplicationClose()
        End If

        Return False
    End Function

    Private Function closeAllVerifiedDocumentsWithoutSaving() As Boolean
        If iSwApp Is Nothing Then Return False

        'Every save/SVN/lock check and the user's final review decision have already passed.
        'Close documents explicitly so SOLIDWORKS cannot show its native bulk Save Modified
        'Documents dialog after the PlumVault table. Drawings and parent assemblies close before
        'their referenced models. Multiple passes handle documents that SOLIDWORKS reorders or
        'temporarily keeps alive while a parent is closing.
        controlledApplicationNativeCloseCallInProgress = True
        Try
            For pass As Integer = 1 To 8
                Dim beforeCount As Integer = 0

                Try
                    beforeCount = iSwApp.GetDocumentCount()
                Catch ex As Exception
                    writeOperationLog("Could not count documents during verified close: " & ex.Message)
                    Return False
                End Try

                If beforeCount <= 0 Then Return True

                Dim docsObject As Object = Nothing

                Try
                    docsObject = iSwApp.GetDocuments()
                Catch ex As Exception
                    writeOperationLog("Could not enumerate documents during verified close: " & ex.Message)
                    Return False
                End Try

                Dim docs As Object() = TryCast(docsObject, Object())
                If docs Is Nothing OrElse docs.Length = 0 Then Return Not hasAnyOpenSolidWorksDocument()

                Dim closeNames As New List(Of KeyValuePair(Of Integer, String))()

                For Each docObject As Object In docs
                    Dim doc As ModelDoc2 = TryCast(docObject, ModelDoc2)
                    If doc Is Nothing Then Continue For

                    Dim closeOrder As Integer = 3
                    Dim documentName As String = ""

                    Try
                        Select Case CInt(doc.GetType())
                            Case swDocumentTypes_e.swDocDRAWING
                                closeOrder = 0
                            Case swDocumentTypes_e.swDocASSEMBLY
                                closeOrder = 1
                            Case swDocumentTypes_e.swDocPART
                                closeOrder = 2
                        End Select
                    Catch
                    End Try

                    'The physical file name includes the extension and is less ambiguous than a
                    'display title. Unsaved documents have no path, so fall back to GetTitle.
                    Try
                        documentName = Path.GetFileName(doc.GetPathName())
                    Catch
                        documentName = ""
                    End Try

                    If String.IsNullOrWhiteSpace(documentName) Then
                        Try
                            documentName = doc.GetTitle()
                        Catch
                            documentName = ""
                        End Try
                    End If

                    If Not String.IsNullOrWhiteSpace(documentName) Then
                        closeNames.Add(New KeyValuePair(Of Integer, String)(closeOrder, documentName))
                    End If
                Next

                For Each closeEntry As KeyValuePair(Of Integer, String) In closeNames.OrderBy(Function(entry) entry.Key)
                    Try
                        iSwApp.CloseDoc(closeEntry.Value)
                    Catch ex As Exception
                        writeOperationLog("CloseDoc failed for " & closeEntry.Value & ": " & ex.Message)
                    End Try
                Next

                Dim afterCount As Integer = beforeCount

                Try
                    afterCount = iSwApp.GetDocumentCount()
                Catch
                End Try

                If afterCount <= 0 OrElse Not hasAnyOpenSolidWorksDocument() Then Return True
                If afterCount >= beforeCount Then Return False
            Next
        Finally
            controlledApplicationNativeCloseCallInProgress = False
        End Try

        Return Not hasAnyOpenSolidWorksDocument()
    End Function

    Private Function getDeferredCloseDocumentIdentity(ByVal document As ModelDoc2,
                                                        ByRef displayName As String,
                                                        ByRef physicalPath As String) As String
        displayName = ""
        physicalPath = ""
        If document Is Nothing Then Return ""

        Try
            physicalPath = normalizeFullPathSafe(document.GetPathName())
        Catch
            physicalPath = ""
        End Try

        If Not String.IsNullOrWhiteSpace(physicalPath) Then
            Try
                displayName = Path.GetFileName(physicalPath)
            Catch
                displayName = physicalPath
            End Try

            Return "PATH|" & physicalPath
        End If

        Try
            displayName = document.GetTitle()
        Catch
            displayName = ""
        End Try

        If String.IsNullOrWhiteSpace(displayName) Then Return ""
        Return "UNSAVED|" & displayName.Trim()
    End Function

    Private Function captureDeferredApplicationCloseState() As Dictionary(Of String, Boolean)
        Dim snapshot As New Dictionary(Of String, Boolean)(StringComparer.OrdinalIgnoreCase)
        If iSwApp Is Nothing Then Return snapshot

        Dim documents As Object() = Nothing
        Try
            documents = TryCast(iSwApp.GetDocuments(), Object())
        Catch
            documents = Nothing
        End Try

        If documents Is Nothing Then Return snapshot

        For Each documentObject As Object In documents
            Dim document As ModelDoc2 = TryCast(documentObject, ModelDoc2)
            If document Is Nothing Then Continue For

            Dim displayName As String = ""
            Dim physicalPath As String = ""
            Dim identity As String = getDeferredCloseDocumentIdentity(document, displayName, physicalPath)
            If String.IsNullOrWhiteSpace(identity) Then Continue For

            Dim wasDirty As Boolean = True
            Try
                wasDirty = document.GetSaveFlag()
            Catch
                'Unknown state is treated as already dirty so this extra race check does not
                'override the close review that already completed successfully.
                wasDirty = True
            End Try

            snapshot(identity) = wasDirty
        Next

        Return snapshot
    End Function

    Private Function deferredApplicationCloseStateIsStillSafe(
        ByVal snapshot As Dictionary(Of String, Boolean),
        ByRef changedDocumentName As String
    ) As Boolean
        changedDocumentName = ""
        If snapshot Is Nothing OrElse iSwApp Is Nothing Then Return True

        Dim documents As Object() = Nothing
        Try
            documents = TryCast(iSwApp.GetDocuments(), Object())
        Catch
            'The established close guard already fails closed when the document collection
            'cannot be read. Preserve that behavior at the final destructive boundary.
            changedDocumentName = "an open SOLIDWORKS document"
            Return False
        End Try

        If documents Is Nothing Then Return True

        For Each documentObject As Object In documents
            Dim document As ModelDoc2 = TryCast(documentObject, ModelDoc2)
            If document Is Nothing Then Continue For

            Dim displayName As String = ""
            Dim physicalPath As String = ""
            Dim identity As String = getDeferredCloseDocumentIdentity(document, displayName, physicalPath)
            If String.IsNullOrWhiteSpace(identity) Then
                changedDocumentName = "an unidentified SOLIDWORKS document"
                Return False
            End If

            Dim wasDirty As Boolean = False
            If Not snapshot.TryGetValue(identity, wasDirty) Then
                changedDocumentName = If(String.IsNullOrWhiteSpace(displayName), "a newly opened document", displayName)
                Return False
            End If

            If wasDirty Then Continue For

            Dim isDirtyNow As Boolean = False
            Try
                isDirtyNow = document.GetSaveFlag()
            Catch
                changedDocumentName = If(String.IsNullOrWhiteSpace(displayName), "an open SOLIDWORKS document", displayName)
                Return False
            End Try

            If Not isDirtyNow Then Continue For

            'Leaving a legitimate in-context child edit can set an unlocked ancestor's
            'SaveFlag without changing that assembly file. Event-proven candidates remain
            'covered by the existing close logic; real assembly-owned edits clear the marker.
            If Not String.IsNullOrWhiteSpace(physicalPath) AndAlso
               isAssemblyGuardFalseDirtyCandidate(physicalPath) Then Continue For

            changedDocumentName = If(String.IsNullOrWhiteSpace(displayName), identity, displayName)
            Return False
        Next

        Return True
    End Function

    Private Function queueVerifiedSafeApplicationClose() As Boolean
        If controlledApplicationCloseQueued OrElse controlledApplicationExitInProgress Then Return True
        If iSwApp Is Nothing OrElse myUserControl Is Nothing Then Return False

        Try
            If myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then Return False
        Catch
            Return False
        End Try

        Dim reviewedDocumentState As Dictionary(Of String, Boolean) = captureDeferredApplicationCloseState()
        controlledApplicationCloseQueued = True

        Dim closeAction As New System.Windows.Forms.MethodInvoker(
            Sub()
                continueVerifiedSafeApplicationClose(reviewedDocumentState, 0, "", 0)
            End Sub
        )

        Try
            myUserControl.BeginInvoke(closeAction)
            Return True
        Catch
            controlledApplicationCloseQueued = False
            Return False
        End Try
    End Function

    Private Sub startApplicationExitStateWatchdog()
        Try
            If applicationExitStateWatchdog IsNot Nothing Then
                applicationExitStateWatchdog.Stop()
                applicationExitStateWatchdog.Dispose()
            End If

            applicationExitStateWatchdog = New System.Windows.Forms.Timer()
            applicationExitStateWatchdog.Interval = 15000

            AddHandler applicationExitStateWatchdog.Tick,
                Sub(sender As Object, e As EventArgs)
                    Try
                        applicationExitStateWatchdog.Stop()
                        applicationExitStateWatchdog.Dispose()
                    Catch
                    Finally
                        applicationExitStateWatchdog = Nothing
                    End Try

                    'If this callback runs, SOLIDWORKS did not terminate after ExitApp. Do not
                    'leave the WM_CLOSE guard permanently swallowing every future big-X click.
                    If controlledApplicationExitInProgress OrElse controlledApplicationCloseQueued Then
                        controlledApplicationNativeCloseCallInProgress = False
                        controlledApplicationCloseQueued = False
                        controlledApplicationExitInProgress = False
                        writeOperationLog("Application-exit watchdog reset stale close state after ExitApp did not terminate SOLIDWORKS.")
                    End If
                End Sub

            applicationExitStateWatchdog.Start()
        Catch ex As Exception
            writeOperationLog("Could not start application-exit watchdog: " & ex.Message)
        End Try
    End Sub

    Private Sub continueVerifiedSafeApplicationClose(ByVal reviewedDocumentState As Dictionary(Of String, Boolean),
                                                       ByVal attempt As Integer,
                                                       ByVal previousContextSignature As String,
                                                       ByVal repeatedContextCount As Integer)
        Try
            controlledApplicationExitInProgress = True

            'EditAssembly is asynchronous inside SOLIDWORKS. Unwind exactly one owner/target
            'relationship per UI turn so a Top -> Mid -> Bottom edit chain is handled at any
            'depth without restoring a read-only parent before SOLIDWORKS leaves that level.
            Dim ownerToExit As ModelDoc2 = Nothing
            Dim contextSignature As String = ""

            If tryGetInContextOwnerBlockingClose(
                "",
                ownerToExit,
                contextSignature,
                matchAnyActiveContext:=True
            ) Then
                Dim sameContext As Boolean = String.Equals(
                    contextSignature,
                    previousContextSignature,
                    StringComparison.OrdinalIgnoreCase
                )
                Dim nextRepeatedCount As Integer = If(sameContext, repeatedContextCount + 1, 0)

                If attempt >= 64 OrElse nextRepeatedCount >= 24 Then
                    Throw New InvalidOperationException(
                        "SOLIDWORKS did not finish leaving Edit Part/Edit Assembly mode. " &
                        "PlumVault left the application open so no work is lost."
                    )
                End If

                If Not sameContext OrElse nextRepeatedCount Mod 4 = 0 Then
                    activateAssemblyForContextExit(ownerToExit, contextSignature)
                    exitAssemblyInContextEditWithoutSavingParent(ownerToExit)
                    writeOperationLog("Queued in-context unwind before verified application close: " & contextSignature)
                End If

                If myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then
                    Throw New InvalidOperationException("The PlumVault task pane closed before SOLIDWORKS completed the application close.")
                End If

                myUserControl.BeginInvoke(
                    New MethodInvoker(
                        Sub()
                            continueVerifiedSafeApplicationClose(
                                reviewedDocumentState,
                                attempt + 1,
                                contextSignature,
                                nextRepeatedCount
                            )
                        End Sub
                    )
                )
                Exit Sub
            End If

            Dim changedDocumentName As String = ""
            If Not deferredApplicationCloseStateIsStillSafe(reviewedDocumentState, changedDocumentName) Then
                Throw New InvalidOperationException(
                    changedDocumentName & " changed or opened after the close decision. " &
                    "PlumVault left SOLIDWORKS open so the latest state can be reviewed. Close it again when ready."
                )
            End If

            'The PlumVault checks/table have already established what must be committed,
            'retained, or discarded. Close every verified document without saving so
            'SOLIDWORKS cannot append a duplicate native save decision.
            Dim documentsClosed As Boolean = closeAllVerifiedDocumentsWithoutSaving()

            If Not documentsClosed AndAlso hasAnyOpenSolidWorksDocument() Then
                Throw New InvalidOperationException(
                    "SOLIDWORKS could not close every verified document automatically. " &
                    "Check the remaining document and close again."
                )
            End If

            'With all documents already closed there is no native save question left to ask.
            'Only this exact ExitApp call may pass the application WM_CLOSE hook.
            controlledApplicationNativeCloseCallInProgress = True
            Try
                iSwApp.ExitApp()
            Finally
                controlledApplicationNativeCloseCallInProgress = False
            End Try

            controlledApplicationCloseQueued = False
            'Keep controlledApplicationExitInProgress set while SOLIDWORKS drains shutdown.
            'If ExitApp returns but the process remains alive, release that state after a
            'bounded delay so subsequent big-X clicks are not swallowed forever.
            startApplicationExitStateWatchdog()

        Catch ex As Exception
            controlledApplicationNativeCloseCallInProgress = False
            controlledApplicationCloseQueued = False
            controlledApplicationExitInProgress = False

            Try
                iSwApp.SendMsgToUser2(
                    "SOLIDWORKS could not complete the verified close." & vbCrLf & vbCrLf &
                    ex.Message & vbCrLf & vbCrLf &
                    "The application was left open so no work is silently lost.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try
        End Try
    End Sub

    Private Function closeMustWaitForActiveOperation() As Boolean
        Dim operationDescription As String = ""

        If cadRelocationInProgress Then
            operationDescription = "Re-ID or Move"
        ElseIf newDocumentTeamSaveWorkflowInProgress OrElse automaticSaveCommitPreparing Then
            operationDescription = "Save and automatic commit"
        ElseIf legacyImportInProgress Then
            operationDescription = "legacy import"
        ElseIf asyncGetLocksInProgress Then
            operationDescription = "Get Locks"
        ElseIf asyncCommitInProgress Then
            operationDescription = "Commit"
        ElseIf asyncCleanupInProgress Then
            operationDescription = "SVN cleanup"
        Else
            SyncLock solidWorksNativeMutationSync
                If solidWorksNativeMutationInProgress Then
                    operationDescription = If(
                        String.IsNullOrWhiteSpace(solidWorksNativeMutationDescription),
                        "SOLIDWORKS file update",
                        solidWorksNativeMutationDescription
                    )
                End If
            End SyncLock
        End If

        If String.IsNullOrWhiteSpace(operationDescription) Then Return False

        iSwApp.SendMsgToUser2(
            "SOLIDWORKS will stay open until PlumVault finishes: " & operationDescription & "." & vbCrLf & vbCrLf &
            "Wait for the operation to finish, then close SOLIDWORKS again. This prevents a save, commit, or reference update from being interrupted.",
            swMessageBoxIcon_e.swMbInformation,
            swMessageBoxBtn_e.swMbOk
        )

        Return True
    End Function

    Private Function blockCloseIfOpenDocsUnsafeOnly() As Boolean
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False

        'Uses only GetSaveFlag and local SVN status while offline.
        'If the user just chose "No = close anyway", allow duplicate close events through briefly.
        If DateTime.Now < unsafeForceCloseApprovedUntil Then Return False

        'A duplicate close message can arrive while the modal warning is open.
        'Block it; allowing it through can close SOLIDWORKS behind the prompt.
        If closeGuardMessageShowing Then Return True

        Dim openPaths As New List(Of String)

        Try
            Dim docsObj As Object = iSwApp.GetDocuments()

            If docsObj Is Nothing Then Return False

            Dim docs As Object() = CType(docsObj, Object())

            For Each docObj As Object In docs
                Dim doc As ModelDoc2 = TryCast(docObj, ModelDoc2)
                If doc Is Nothing Then Continue For

                Dim docPath As String = ""

                Try
                    docPath = doc.GetPathName()
                Catch
                    Continue For
                End Try

                Dim title As String = ""

                Try
                    title = doc.GetTitle()
                Catch
                    title = "<unknown document>"
                End Try

                'The close-review table is the final PlumVault decision point. If the user
                'explicitly chose Continue for this locked file, do not ask the same question
                'again through the legacy Yes/No guard. Saved local SVN changes remain on disk;
                'unsaved in-memory changes are intentionally discarded by the controlled close.
                If applicationCloseReviewApprovedPath(docPath) Then Continue For

                Dim isDirty As Boolean = False

                Try
                    isDirty = doc.GetSaveFlag()
                Catch
                    isDirty = False
                End Try

                If String.IsNullOrWhiteSpace(docPath) Then
                    openPaths.Add("[UNSAVED_NEW_FILE] " & title)
                    Continue For
                End If

                If isDirty Then
                    If canTreatAssemblySaveFlagAsGuardGenerated(doc, docPath) Then
                        'The assembly guard already undid the blocked edit and SVN confirms the
                        'physical assembly file is locally clean. Continue to the retained-lock
                        'review instead of showing a false uncommitted-changes warning.
                        openPaths.Add(docPath)
                    ElseIf userHasSvnLockOnDoc(doc) OrElse isNewUnversionedOrAddedFile(docPath) Then
                        openPaths.Add("[UNSAVED_SOLIDWORKS_CHANGES] " & title)
                    Else
                        openPaths.Add("[UNSAVED_WITHOUT_LOCK] " & title)
                    End If
                    Continue For
                End If

                If Not isCadFilePath(docPath) Then Continue For
                If Not isPathInsideLocalRepo(docPath) Then Continue For

                openPaths.Add(docPath)
            Next

        Catch ex As Exception
            'The verified-close path may close documents without saving, so an incomplete open-
            'document scan must not silently fall through. Give the user an explicit escape
            'route, though, so a persistent COM/SVN verification error can never make the main
            'SOLIDWORKS window permanently unclosable.
            writeOperationLog("Could not verify every open document during application close: " & ex.Message)
            Return Not userApprovedApplicationCloseAfterVerificationFailure(
                "PlumVault could not verify every open SOLIDWORKS document."
            )
        End Try

        If openPaths.Count = 0 Then Return False

        Dim unsafeMsg As String = getUnsafeCloseStatusMessage(openPaths)

        If String.IsNullOrWhiteSpace(unsafeMsg) Then
            Return False
        End If

        Try
            closeGuardMessageShowing = True
            Return showUnsafeClosePrompt(unsafeMsg)

        Finally
            closeGuardMessageShowing = False
        End Try
    End Function

    Public Function blockCloseIfSingleDocUnsafe(ByVal closingDoc As ModelDoc2,
                                                Optional ByVal approveControlledDocumentClose As Boolean = False) As Boolean
        If controlledApplicationExitInProgress Then Return False
        If cadRelocationInProgress Then Return False
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False

        'Uses only GetSaveFlag and local SVN status while offline.
        'If the user just chose "No = close anyway", allow duplicate close events through briefly.
        If Not approveControlledDocumentClose AndAlso DateTime.Now < unsafeForceCloseApprovedUntil Then Return False

        'A duplicate close message can arrive while the modal warning is open.
        'Block it; allowing it through can destroy the document behind the prompt.
        If closeGuardMessageShowing Then Return True
        If closingDoc Is Nothing Then Return False

        Dim openPaths As New List(Of String)
        Dim closingPath As String = ""

        Try
            Dim docPath As String = ""
            Dim title As String = ""

            Try
                docPath = closingDoc.GetPathName()
                closingPath = docPath
            Catch
                docPath = ""
            End Try

            Try
                title = closingDoc.GetTitle()
            Catch
                title = "<unknown document>"
            End Try

            Dim isDirty As Boolean = False

            Try
                isDirty = closingDoc.GetSaveFlag()
            Catch
                isDirty = False
            End Try

            If String.IsNullOrWhiteSpace(docPath) Then
                openPaths.Add("[UNSAVED_NEW_FILE] " & title)

            ElseIf isDirty Then
                If canTreatAssemblySaveFlagAsGuardGenerated(closingDoc, docPath) Then
                    openPaths.Add(docPath)
                ElseIf userHasSvnLockOnDoc(closingDoc) OrElse isNewUnversionedOrAddedFile(docPath) Then
                    openPaths.Add("[UNSAVED_SOLIDWORKS_CHANGES] " & title)
                Else
                    openPaths.Add("[UNSAVED_WITHOUT_LOCK] " & title)
                End If

            ElseIf isCadFilePath(docPath) AndAlso isPathInsideLocalRepo(docPath) Then
                openPaths.Add(docPath)
            End If

        Catch ex As Exception
            'A mini-X close that cannot be verified must not fall through to SOLIDWORKS'
            'native close; that can discard a file the guard never classified and reintroduce
            'the duplicate Save/Don't Save dialog. Leave the document open and give one clear
            'recovery action instead.
            Try
                iSwApp.SendMsgToUser2(
                    "The file close was cancelled because PlumVault could not verify the document state." & vbCrLf & vbCrLf &
                    "Click Sync and try closing again." & vbCrLf & vbCrLf & ex.Message,
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            Return True
        End Try

        If openPaths.Count = 0 Then Return False

        Dim unsafeMsg As String = getUnsafeCloseStatusMessage(openPaths)

        If String.IsNullOrWhiteSpace(unsafeMsg) Then
            Return False
        End If

        Try
            closeGuardMessageShowing = True
            Return showUnsafeClosePrompt(
                unsafeMsg,
                approvedDocumentPath:=If(approveControlledDocumentClose, closingPath, "")
            )

        Finally
            closeGuardMessageShowing = False
        End Try
    End Function

    'Entry point for the low-level Ctrl+W keyboard hook (EventHandling.vb), which always eats
    'the keystroke immediately rather than deciding synchronously inside the raw hook callback.
    'Runs the real check on the UI thread's normal message pump instead, where showing the
    'review dialog is safe (matches how the small-X/Window-close paths already do it).
    Public Sub queueDeferredCtrlWCloseCheckPublic()
        Try
            If myUserControl Is Nothing OrElse myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then Exit Sub
            myUserControl.BeginInvoke(New System.Windows.Forms.MethodInvoker(AddressOf performDeferredCtrlWClose))
        Catch
        End Try
    End Sub

    Private Sub performDeferredCtrlWClose()
        Try
            If iSwApp Is Nothing Then Exit Sub

            Dim blocked As Boolean = False

            Try
                blocked = blockCloseIfActiveDocUnsafe()
            Catch ex As Exception
                blocked = True

                Try
                    iSwApp.SendMsgToUser2(
                        "The file close was cancelled because PlumVault could not verify its save and lock state." & vbCrLf & vbCrLf &
                        "Click Sync and try again. If this repeats, disable the PlumVault add-in before closing SOLIDWORKS." & vbCrLf & vbCrLf & ex.Message,
                        swMessageBoxIcon_e.swMbWarning,
                        swMessageBoxBtn_e.swMbOk
                    )
                Catch
                End Try
            End Try

            'Blocked covers every case that already fully handled itself: the review table,
            'a queued verified close, or a message explaining why closing was refused. Only the
            'genuinely-safe, nothing-to-do case reaches here, and Ctrl+W's own keystroke was
            'already eaten (unconditionally, at the hook) - so replay the close explicitly.
            If blocked Then Exit Sub

            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDoc Is Nothing Then Exit Sub

            Dim activePath As String = ""

            Try
                activePath = activeDoc.GetPathName()
            Catch
                activePath = ""
            End Try

            If Not String.IsNullOrWhiteSpace(activePath) Then
                If queueUserApprovedDocumentCloseWithoutSave(
                    activePath,
                    allowWithoutCloseReview:=True
                ) Then Exit Sub

                iSwApp.SendMsgToUser2(
                    "The verified Ctrl+W close could not be started." & vbCrLf & vbCrLf &
                    "The file was left open. Try closing it again.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
                Exit Sub
            End If

            Dim docName As String = ""
            Try
                docName = activeDoc.GetTitle()
            Catch
                docName = ""
            End Try

            If String.IsNullOrWhiteSpace(docName) Then Exit Sub

            controlledDocumentCloseNativeCallInProgress = True
            Try
                iSwApp.CloseDoc(docName)
            Finally
                controlledDocumentCloseNativeCallInProgress = False
            End Try
        Catch
        End Try
    End Sub

    Public Function blockCloseIfActiveDocUnsafe() As Boolean
        If controlledApplicationNativeCloseCallInProgress Then Return False
        If controlledApplicationExitInProgress Then
            If Not hasAnyOpenSolidWorksDocument() Then Return False
            Return True
        End If
        If controlledApplicationCloseQueued Then Return True
        If controlledDocumentCloseNativeCallInProgress Then Return False
        If cadRelocationInProgress Then Return False
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False

        'Local-only check. Must still run when the user has turned Online off.
        Dim activeDoc As ModelDoc2 = Nothing
        Dim activePath As String = ""

        Try
            activeDoc = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch
            activeDoc = Nothing
        End Try

        If activeDoc Is Nothing Then Return False

        Try
            activePath = activeDoc.GetPathName()
        Catch
            activePath = ""
        End Try

        'A controlled close already in flight for THIS document must stay blocked (a second
        'click on its own small X must not race the first). An unrelated document's controlled
        'close in flight must NOT block this one - each document's close is independent, unlike
        'the whole-application guard in blockCloseIfOpenDocsUnsafe, which intentionally treats
        'any in-flight controlled close as reason to hold the entire app-close.
        If Not String.IsNullOrWhiteSpace(activePath) AndAlso hasFreshControlledCloseQueuedPaths() Then
            Dim normalizedActivePath As String = normalizeFullPathSafe(activePath)

            SyncLock assemblyGuardSync
                If assemblyGuardControlledCloseQueuedPaths.Contains(normalizedActivePath) Then Return True
            End SyncLock
        End If

        'Use the same actionable review table as full application close, but scope it to the
        'document whose small X was clicked and its recursive CAD dependency closure. This
        'catches both changed files and clean retained locks without showing unrelated projects.
        If Not documentCloseReviewIsApproved(activePath) Then
            If blockCloseForOwnedLocks(
                isClosingSolidWorks:=False,
                closingDocumentPath:=activePath
            ) Then Return True
        End If

        'Continue closing in the table is an explicit no-further-save decision for every row
        'that the table actually displayed. A dirty active document without its own lock is
        'not an owned-lock row; if the table appeared only because one of its dependencies was
        'locked, run the ordinary unsafe-file guard before inheriting the table approval.
        If documentCloseReviewIsApproved(activePath) Then
            If Not documentCloseReviewCoveredPath(activePath) AndAlso
               blockCloseIfSingleDocUnsafe(activeDoc, approveControlledDocumentClose:=True) Then Return True

            'The unsafe-file prompt's explicit "close without saving" choice is consumed by
            'this controlled CloseDoc path.  Clear its short-lived approval so it cannot leak
            'into a later close of another document.
            consumeUnsafeDocumentCloseApproval(activePath)

            'Swallow the native window close and use CloseDoc so SOLIDWORKS cannot append its
            'own ambiguous Save/Don't Save prompt.
            If queueUserApprovedDocumentCloseWithoutSave(activePath) Then Return True

            Try
                iSwApp.SendMsgToUser2(
                    "The reviewed close could not be started." & vbCrLf & vbCrLf &
                    "The file was left open. Try closing it again.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            Return True
        End If

        Dim guardGeneratedFalseDirty As Boolean = False
        Try
            guardGeneratedFalseDirty = activeDoc.GetSaveFlag() AndAlso
                                       canTreatAssemblySaveFlagAsGuardGenerated(activeDoc, activePath)
        Catch
            guardGeneratedFalseDirty = False
        End Try

        Dim blockedByUnsafeChanges As Boolean = blockCloseIfSingleDocUnsafe(
            activeDoc,
            approveControlledDocumentClose:=True
        )
        If blockedByUnsafeChanges Then Return True

        If consumeUnsafeDocumentCloseApproval(activePath) Then
            'Never let the native close continue after the user chose "close without saving";
            'SOLIDWORKS would append its own Save/Don't Save prompt.  Use the same verified,
            'context-aware CloseDoc route as the close-review table instead.
            If queueUserApprovedDocumentCloseWithoutSave(
                activePath,
                allowWithoutCloseReview:=True
            ) Then Return True

            Try
                iSwApp.SendMsgToUser2(
                    "The close-without-saving request could not be started." & vbCrLf & vbCrLf &
                    "The file was left open. Try closing it again.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            Return True
        End If

        If guardGeneratedFalseDirty Then
            'SOLIDWORKS has no public API to clear GetSaveFlag. Swallow this native close
            'message and queue a verified close-without-save instead, preventing the false
            'Save Modified Documents dialog while preserving all genuine dirty-file checks.
            If queueGuardGeneratedFalseDirtyDocumentClose(activePath) Then Return True

            Try
                iSwApp.SendMsgToUser2(
                    "The file is SVN-clean, but SOLIDWORKS could not start the verified close." & vbCrLf & vbCrLf &
                    "The file was left open. Try closing it again.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            Return True
        End If

        Return False
    End Function

    'Full-app-close scans the ENTIRE working copy ("svn status -v" over the whole tree) to
    'find any lock the user holds anywhere - not just currently open documents. On a large
    'CAD repo this is genuinely slow, and it previously ran fresh on every single close
    'attempt, including "closed, saw the table, went back to fix something, closed again"
    'within the same minute with nothing having changed.
    '
    'This cache reuses a recent scan instead of repeating it, but only ever as a fast path:
    '- It is invalidated immediately by anything that could change what is locked (Get
    '  Locks, Commit, Unlock/Revert, both from the main toolstrip and from inside the
    '  close-review table itself).
    '- It expires on its own after a short TTL. The .svn working-copy database timestamp is
    '  also validated, so a lock acquired by TortoiseSVN or another client invalidates it.
    '- If no valid cached scan exists for any reason, it falls back to the original,
    '  always-correct synchronous scan - the check itself is never weakened or skipped,
    '  only its redundant repetition is.
    Private ownedLocksWholeCopySnapshot As List(Of CloseLockReviewItem) = Nothing
    Private ownedLocksWholeCopySnapshotUtc As DateTime = DateTime.MinValue
    Private ownedLocksWholeCopySnapshotWcDbPath As String = ""
    Private ownedLocksWholeCopySnapshotWcDbWriteUtc As DateTime = DateTime.MinValue
    Private ReadOnly ownedLocksWholeCopySnapshotSync As New Object()
    Private ownedLocksWholeCopyRefreshInProgress As Boolean = False
    Private ownedLocksWholeCopySnapshotGeneration As Integer = 0
    Private Const OWNED_LOCKS_SNAPSHOT_MAX_AGE_MINUTES As Double = 2.0

    Public Sub invalidateOwnedLocksWholeCopySnapshotPublic()
        SyncLock ownedLocksWholeCopySnapshotSync
            ownedLocksWholeCopySnapshot = Nothing
            ownedLocksWholeCopySnapshotUtc = DateTime.MinValue
            ownedLocksWholeCopySnapshotWcDbPath = ""
            ownedLocksWholeCopySnapshotWcDbWriteUtc = DateTime.MinValue
            ownedLocksWholeCopySnapshotGeneration += 1
        End SyncLock
    End Sub

    Private Function getWorkingCopyDatabaseWriteUtc(ByVal workingCopyRoot As String,
                                                     ByRef databasePath As String) As DateTime
        databasePath = ""
        If String.IsNullOrWhiteSpace(workingCopyRoot) Then Return DateTime.MinValue

        Try
            databasePath = Path.Combine(Path.GetFullPath(workingCopyRoot), ".svn", "wc.db")
            If Not File.Exists(databasePath) Then Return DateTime.MinValue
            Return File.GetLastWriteTimeUtc(databasePath)
        Catch
            databasePath = ""
            Return DateTime.MinValue
        End Try
    End Function

    'Runs the full scan and (re)populates the cache. Safe to call proactively (e.g. shortly
    'after the plugin connects, or right after any action that just changed lock state) so
    'the answer is already warm by the time the user actually tries to close.
    Public Sub refreshOwnedLocksWholeCopySnapshotPublic()
        Try
            If iSwApp Is Nothing OrElse myUserControl Is Nothing Then Return
            If String.IsNullOrWhiteSpace(sSVNPath) OrElse Not File.Exists(sSVNPath) Then Return

            Dim configuredWorkingCopyPath As String = ""

            Try
                configuredWorkingCopyPath = Path.GetFullPath(myUserControl.localRepoPath.Text.Trim()).TrimEnd("\"c)
            Catch
                configuredWorkingCopyPath = ""
            End Try

            If String.IsNullOrWhiteSpace(configuredWorkingCopyPath) Then Return

            Dim svnExecutable As String = sSVNPath
            Dim savedPathForBackground As String = ""
            Dim refreshGeneration As Integer = 0

            Try
                savedPathForBackground = myUserControl.savedPATH
            Catch
            End Try

            SyncLock ownedLocksWholeCopySnapshotSync
                If ownedLocksWholeCopyRefreshInProgress Then Return
                ownedLocksWholeCopyRefreshInProgress = True
                refreshGeneration = ownedLocksWholeCopySnapshotGeneration
            End SyncLock

            'Only svn.exe and plain managed parsing run here. Never marshal an
            'IModelDoc2/ISldWorks COM object to a worker thread.
            Task.Run(
                Sub()
                    Dim refreshed As List(Of CloseLockReviewItem) = Nothing
                    Dim refreshedWcDbPath As String = ""
                    Dim refreshedWcDbWriteUtc As DateTime = DateTime.MinValue
                    Dim preScanWcDbWriteUtc As DateTime = DateTime.MinValue

                    Try
                        Dim workingCopyRoot As String = configuredWorkingCopyPath
                        Dim infoResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
                            svnExecutable,
                            "info --show-item wc-root --non-interactive """ & configuredWorkingCopyPath & """",
                            savedPathForBackground
                        )

                        If String.IsNullOrWhiteSpace(If(infoResult.outputError, "")) AndAlso
                           Not String.IsNullOrWhiteSpace(If(infoResult.output, "")) Then
                            workingCopyRoot = normalizeFullPathSafe(infoResult.output.Trim().Split({vbCrLf, vbLf}, StringSplitOptions.RemoveEmptyEntries)(0).Trim(""""c))
                        End If

                        preScanWcDbWriteUtc = getWorkingCopyDatabaseWriteUtc(workingCopyRoot, refreshedWcDbPath)

                        Dim result As rawProcessReturn = runSvnProcessBackgroundNoUi(
                            svnExecutable,
                            "status -v --non-interactive """ & workingCopyRoot & """",
                            savedPathForBackground
                        )

                        If String.IsNullOrWhiteSpace(If(result.outputError, "")) Then
                            refreshed = parseOwnedLockReviewItems(
                                If(result.output, ""),
                                workingCopyRoot,
                                includeOpenDocumentSaveFlags:=False
                            )

                            Dim postScanWcDbPath As String = ""
                            refreshedWcDbWriteUtc = getWorkingCopyDatabaseWriteUtc(workingCopyRoot, postScanWcDbPath)

                            'If another SVN client changed the working copy during this scan,
                            'do not publish a result that may already be stale.
                            If Not String.Equals(refreshedWcDbPath, postScanWcDbPath, StringComparison.OrdinalIgnoreCase) OrElse
                               preScanWcDbWriteUtc <> refreshedWcDbWriteUtc Then
                                refreshed = Nothing
                            End If
                        End If
                    Catch
                        refreshed = Nothing
                    Finally
                        SyncLock ownedLocksWholeCopySnapshotSync
                            'An SVN action may have invalidated the cache while this older scan
                            'was still running. Never publish that stale result afterward.
                            If refreshed IsNot Nothing AndAlso refreshGeneration = ownedLocksWholeCopySnapshotGeneration Then
                                ownedLocksWholeCopySnapshot = cloneCloseLockReviewItems(refreshed)
                                ownedLocksWholeCopySnapshotUtc = DateTime.UtcNow
                                ownedLocksWholeCopySnapshotWcDbPath = refreshedWcDbPath
                                ownedLocksWholeCopySnapshotWcDbWriteUtc = refreshedWcDbWriteUtc
                            End If

                            ownedLocksWholeCopyRefreshInProgress = False
                        End SyncLock
                    End Try
                End Sub
            )
        Catch
            SyncLock ownedLocksWholeCopySnapshotSync
                ownedLocksWholeCopyRefreshInProgress = False
            End SyncLock
        End Try
    End Sub

    Private Function getOwnedLockReviewItemsForCloseCached() As List(Of CloseLockReviewItem)
        Dim cachedItems As List(Of CloseLockReviewItem) = Nothing
        Dim cachedWcDbPath As String = ""
        Dim cachedWcDbWriteUtc As DateTime = DateTime.MinValue

        SyncLock ownedLocksWholeCopySnapshotSync
            If ownedLocksWholeCopySnapshot IsNot Nothing AndAlso
               (DateTime.UtcNow - ownedLocksWholeCopySnapshotUtc).TotalMinutes < OWNED_LOCKS_SNAPSHOT_MAX_AGE_MINUTES Then
                cachedItems = cloneCloseLockReviewItems(ownedLocksWholeCopySnapshot)
                cachedWcDbPath = ownedLocksWholeCopySnapshotWcDbPath
                cachedWcDbWriteUtc = ownedLocksWholeCopySnapshotWcDbWriteUtc
            End If
        End SyncLock

        If cachedItems IsNot Nothing Then
            Try
                If String.IsNullOrWhiteSpace(cachedWcDbPath) OrElse
                   Not File.Exists(cachedWcDbPath) OrElse
                   File.GetLastWriteTimeUtc(cachedWcDbPath) <> cachedWcDbWriteUtc Then
                    cachedItems = Nothing
                End If
            Catch
                cachedItems = Nothing
            End Try
        End If

        If cachedItems IsNot Nothing Then
            'The expensive part is discovering owned locks across the whole working copy.
            'Recheck only those few candidate paths so newly-saved local modifications and
            'current SOLIDWORKS dirty flags can never be hidden by the two-minute discovery cache.
            Return refreshCachedOwnedLockCandidates(cachedItems)
        End If

        Dim freshRoot As String = getResolvedSvnWorkingCopyRootPath()
        Dim freshWcDbPath As String = ""
        Dim preScanWcDbWriteUtc As DateTime = getWorkingCopyDatabaseWriteUtc(freshRoot, freshWcDbPath)

        Dim freshItems As List(Of CloseLockReviewItem) = getOwnedLockReviewItems(
            candidatePaths:=Nothing,
            scanWholeWorkingCopy:=True,
            returnNothingOnFailure:=True
        )

        Dim postScanWcDbPath As String = ""
        Dim postScanWcDbWriteUtc As DateTime = getWorkingCopyDatabaseWriteUtc(freshRoot, postScanWcDbPath)

        SyncLock ownedLocksWholeCopySnapshotSync
            If freshItems IsNot Nothing AndAlso
               String.Equals(freshWcDbPath, postScanWcDbPath, StringComparison.OrdinalIgnoreCase) AndAlso
               preScanWcDbWriteUtc = postScanWcDbWriteUtc Then
                ownedLocksWholeCopySnapshot = cloneCloseLockReviewItems(freshItems)
                ownedLocksWholeCopySnapshotUtc = DateTime.UtcNow
                ownedLocksWholeCopySnapshotWcDbPath = postScanWcDbPath
                ownedLocksWholeCopySnapshotWcDbWriteUtc = postScanWcDbWriteUtc
            Else
                ownedLocksWholeCopySnapshot = Nothing
                ownedLocksWholeCopySnapshotUtc = DateTime.MinValue
                ownedLocksWholeCopySnapshotWcDbPath = ""
                ownedLocksWholeCopySnapshotWcDbWriteUtc = DateTime.MinValue
            End If
        End SyncLock

        Return freshItems
    End Function

    Private Function refreshCachedOwnedLockCandidates(ByVal cachedItems As IEnumerable(Of CloseLockReviewItem)) As List(Of CloseLockReviewItem)
        Dim output As New List(Of CloseLockReviewItem)()
        If cachedItems Is Nothing Then Return output

        Dim paths As String() = cachedItems.
            Where(Function(item) item IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(item.FilePath)).
            Select(Function(item) item.FilePath).
            Distinct(StringComparer.OrdinalIgnoreCase).
            ToArray()

        If paths.Length = 0 Then Return output

        'Keep the command comfortably below Windows' command-line limit even for a user
        'who retained an unusually large number of locks.
        Const chunkSize As Integer = 40

        For startIndex As Integer = 0 To paths.Length - 1 Step chunkSize
            Dim chunk As String() = paths.Skip(startIndex).Take(chunkSize).ToArray()
            Dim refreshedChunk As List(Of CloseLockReviewItem) = getOwnedLockReviewItems(
                chunk,
                scanWholeWorkingCopy:=False,
                returnNothingOnFailure:=True
            )
            If refreshedChunk Is Nothing Then Return Nothing
            output.AddRange(refreshedChunk)
        Next

        Return output.
            GroupBy(Function(item) item.FilePath, StringComparer.OrdinalIgnoreCase).
            Select(Function(group) group.First()).
            OrderBy(Function(item) Path.GetFileName(item.FilePath), StringComparer.OrdinalIgnoreCase).
            ThenBy(Function(item) item.FilePath, StringComparer.OrdinalIgnoreCase).
            ToList()
    End Function

    Private Function cloneCloseLockReviewItems(ByVal source As IEnumerable(Of CloseLockReviewItem)) As List(Of CloseLockReviewItem)
        Dim output As New List(Of CloseLockReviewItem)()
        If source Is Nothing Then Return output

        For Each item As CloseLockReviewItem In source
            If item Is Nothing Then Continue For

            output.Add(
                New CloseLockReviewItem With {
                    .FilePath = item.FilePath,
                    .IsSafeToUnlock = item.IsSafeToUnlock,
                    .StateText = item.StateText,
                    .IsStillLocked = item.IsStillLocked,
                    .RequiresLockBeforeCommit = item.RequiresLockBeforeCommit,
                    .CanGetLock = item.CanGetLock,
                    .ResultText = item.ResultText,
                    .CanCommit = item.CanCommit,
                    .CanRevert = item.CanRevert
                }
            )
        Next

        Return output
    End Function

    Private Function userApprovedApplicationCloseAfterVerificationFailure(ByVal failureSummary As String) As Boolean
        If iSwApp Is Nothing Then Return False

        Dim response As Integer = swMessageBoxResult_e.swMbHitCancel

        Try
            'A modal warning pumps Windows messages. Keep duplicate WM_CLOSE messages blocked
            'until this one decision has completed.
            lockReviewMessageShowing = True

            response = iSwApp.SendMsgToUser2(
                If(String.IsNullOrWhiteSpace(failureSummary),
                   "PlumVault could not finish the close-safety check.",
                   failureSummary) & vbCrLf & vbCrLf &
                "Choose an action:" & vbCrLf &
                "Yes = Keep SOLIDWORKS open, click Sync, and try closing again" & vbCrLf &
                "No = Close SOLIDWORKS now without any further saves" & vbCrLf & vbCrLf &
                "If you close anyway, unsaved in-memory changes will be discarded. " &
                "Saved local SVN changes remain on disk, and existing SVN locks are retained.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbYesNo
            )
        Catch ex As Exception
            writeOperationLog("Could not show the application close-verification choice: " & ex.Message)
            Return False
        Finally
            lockReviewMessageShowing = False
        End Try

        If response <> swMessageBoxResult_e.swMbHitNo Then Return False

        'Reuse the established controlled no-save shutdown. These short-lived approvals also
        'cover duplicate close messages pumped while the deferred close is being queued.
        unsafeForceCloseApprovedUntil = DateTime.Now.AddSeconds(10)
        unsafeForceCloseApprovedPath = ""
        applicationLockReviewApprovedUntil = DateTime.Now.AddSeconds(10)
        applicationLockReviewApprovedPaths.Clear()
        writeOperationLog("User explicitly chose to close SOLIDWORKS after close verification failed; no further saves will be attempted.")
        Return True
    End Function

    Private Function blockCloseForOwnedLocks(ByVal isClosingSolidWorks As Boolean,
                                                  ByVal closingDocumentPath As String) As Boolean
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False

        'getOwnedLockReviewItems runs plain local "svn status" (no -u), never the server.
        'Retained locks must still be caught when the user has turned Online off.

        'A second native close message can be pumped while the modal table is open.
        'Always block that duplicate instead of allowing the document/application to
        'close behind the user's review window.
        If lockReviewMessageShowing Then Return True

        Dim reviewItems As List(Of CloseLockReviewItem) = Nothing
        Dim unlockedEditScopePaths() As String = Nothing

        Try
            If isClosingSolidWorks Then
                reviewItems = getOwnedLockReviewItemsForCloseCached()
                unlockedEditScopePaths = getOpenSessionCadPathsForLockReview()
            Else
                Dim documentScopePaths() As String = getCadDependencyClosureForDocumentClose(closingDocumentPath)
                unlockedEditScopePaths = getOpenCadPathsWithinScope(documentScopePaths)

                reviewItems = getOwnedLockReviewItems(
                    candidatePaths:=documentScopePaths,
                    scanWholeWorkingCopy:=False,
                    returnNothingOnFailure:=True
                )
            End If

            If reviewItems IsNot Nothing Then
                Dim unlockedEditItems As List(Of CloseLockReviewItem) =
                    getUnlockedEditReviewItems(unlockedEditScopePaths)

                If unlockedEditItems Is Nothing Then
                    reviewItems = Nothing
                ElseIf unlockedEditItems.Count > 0 Then
                    Dim existingPaths As New HashSet(Of String)(
                        reviewItems.
                            Where(Function(item) item IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(item.FilePath)).
                            Select(Function(item) normalizeFullPathSafe(item.FilePath)),
                        StringComparer.OrdinalIgnoreCase
                    )

                    For Each unlockedItem As CloseLockReviewItem In unlockedEditItems
                        If unlockedItem Is Nothing Then Continue For
                        If existingPaths.Add(normalizeFullPathSafe(unlockedItem.FilePath)) Then
                            reviewItems.Add(unlockedItem)
                        End If
                    Next

                    reviewItems = reviewItems.
                        OrderBy(Function(item) Path.GetFileName(item.FilePath), StringComparer.OrdinalIgnoreCase).
                        ThenBy(Function(item) item.FilePath, StringComparer.OrdinalIgnoreCase).
                        ToList()
                End If
            End If
        Catch
            reviewItems = Nothing
        End Try

        If reviewItems Is Nothing Then
            If Not isClosingSolidWorks Then
                Try
                    iSwApp.SendMsgToUser2(
                        "PlumVault could not verify the SVN locks for this file and its references." & vbCrLf & vbCrLf &
                        "The close was cancelled. Click Sync, then try closing the file again.",
                        swMessageBoxIcon_e.swMbWarning,
                        swMessageBoxBtn_e.swMbOk
                    )
                Catch
                End Try

                Return True
            End If

            Return Not userApprovedApplicationCloseAfterVerificationFailure(
                "PlumVault could not verify whether SVN locks are still held in the working copy."
            )
        End If

        If reviewItems.Count = 0 Then Return False

        Try
            lockReviewMessageShowing = True

            Using reviewForm As New CloseLockReviewForm(reviewItems, isClosingSolidWorks)
                reviewForm.ShowDialog()

                If reviewForm.Decision = CloseLockReviewDecision.ContinueClose Then
                    If isClosingSolidWorks Then
                        applicationLockReviewApprovedUntil = DateTime.Now.AddSeconds(10)
                        applicationLockReviewApprovedPaths = New HashSet(Of String)(
                            reviewItems.
                                Where(Function(item) item IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(item.FilePath)).
                                Select(Function(item) normalizeFullPathSafe(item.FilePath)),
                            StringComparer.OrdinalIgnoreCase
                        )
                    Else
                        documentLockReviewApprovedPath = closingDocumentPath
                        documentLockReviewApprovedUntil = DateTime.Now.AddSeconds(10)
                        documentLockReviewApprovedPaths = New HashSet(Of String)(
                            reviewItems.
                                Where(Function(item) item IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(item.FilePath)).
                                Select(Function(item) normalizeFullPathSafe(item.FilePath)),
                            StringComparer.OrdinalIgnoreCase
                        )
                    End If

                    Return False
                End If
            End Using

            Return True
        Catch ex As Exception
            'The review table is a required close decision point whenever retained locks exist.
            'If its UI fails, leave SOLIDWORKS open rather than bypassing the lock review.
            Try
                iSwApp.SendMsgToUser2(
                    "The close was cancelled because the SVN lock review window could not be opened." & vbCrLf & vbCrLf &
                    "Click Sync and try again." & vbCrLf & vbCrLf & ex.Message,
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            Return True
        Finally
            lockReviewMessageShowing = False
        End Try
    End Function

    Private Function getCadDependencyClosureForDocumentClose(ByVal documentPath As String) As String()
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If String.IsNullOrWhiteSpace(documentPath) Then Return Nothing

        Dim normalizedDocumentPath As String = normalizeFullPathSafe(documentPath)

        If isCadFilePath(normalizedDocumentPath) AndAlso
           isPathInsideLocalRepo(normalizedDocumentPath) AndAlso
           seen.Add(normalizedDocumentPath) Then
            output.Add(normalizedDocumentPath)
        End If

        'Closing a part window does not close its referenced/derived source documents. Review
        'the part's own lock only; this also avoids a fragile COM dependency walk on the most
        'common mini-X/Ctrl+W path. Assembly and drawing closes still review their full closure.
        If String.Equals(Path.GetExtension(normalizedDocumentPath), ".SLDPRT", StringComparison.OrdinalIgnoreCase) Then
            Return output.ToArray()
        End If

        'One local SOLIDWORKS dependency query returns the complete recursive closure for
        'assemblies and drawings, including referenced files that are not currently open.
        'This keeps the mini-X review scoped to the document being closed while still finding
        'clean retained locks at any depth. It never scans unrelated working-copy folders and
        'does not contact the SVN server.
        Try
            Dim dependenciesObject As Object = iSwApp.GetDocumentDependencies2(
                normalizedDocumentPath,
                True,  'Traverseflag: include every nested assembly/drawing dependency
                True,  'Searchflag: resolve full paths where possible
                False  'AddReadOnlyInfo: preserve [name, resolved path] pairs
            )

            Dim dependencies As Array = TryCast(dependenciesObject, Array)

            If dependencies IsNot Nothing AndAlso dependencies.Length >= 2 Then
                Dim lowerBound As Integer = dependencies.GetLowerBound(0)
                Dim upperBound As Integer = dependencies.GetUpperBound(0)

                For entryIndex As Integer = lowerBound + 1 To upperBound Step 2
                    Dim dependencyPath As String = Convert.ToString(dependencies.GetValue(entryIndex))
                    If String.IsNullOrWhiteSpace(dependencyPath) Then Continue For

                    If Not Path.IsPathRooted(dependencyPath) Then
                        Try
                            dependencyPath = Path.Combine(Path.GetDirectoryName(normalizedDocumentPath), dependencyPath)
                        Catch
                        End Try
                    End If

                    dependencyPath = normalizeFullPathSafe(dependencyPath)

                    If Not isCadFilePath(dependencyPath) Then Continue For
                    If Not isPathInsideLocalRepo(dependencyPath) Then Continue For
                    If seen.Add(dependencyPath) Then output.Add(dependencyPath)
                Next
            End If
        Catch ex As Exception
            writeOperationLog(
                "Mini-X dependency lock review failed for " &
                Path.GetFileName(normalizedDocumentPath) & ": " & ex.Message
            )
            Throw New InvalidOperationException(
                "SOLIDWORKS could not enumerate the document dependencies safely.",
                ex
            )
        End Try

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function getOpenSessionCadPathsForLockReview() As String()
        If iSwApp Is Nothing Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            Dim docsObj As Object = iSwApp.GetDocuments()
            If docsObj Is Nothing Then Return Nothing

            Dim docs As Object() = CType(docsObj, Object())

            For Each docObj As Object In docs
                Dim doc As ModelDoc2 = TryCast(docObj, ModelDoc2)
                If doc Is Nothing Then Continue For

                Dim docPath As String = ""

                Try
                    docPath = doc.GetPathName()
                Catch
                    docPath = ""
                End Try

                If String.IsNullOrWhiteSpace(docPath) Then Continue For
                If Not isCadFilePath(docPath) Then Continue For
                If Not isPathInsideLocalRepo(docPath) Then Continue For

                Try
                    docPath = Path.GetFullPath(docPath)
                Catch
                End Try

                If seen.Add(docPath) Then output.Add(docPath)
            Next
        Catch
            Return Nothing
        End Try

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function getOpenCadPathsWithinScope(ByVal scopePaths() As String) As String()
        If scopePaths Is Nothing OrElse scopePaths.Length = 0 Then Return Nothing

        Dim openPaths() As String = getOpenSessionCadPathsForLockReview()
        If openPaths Is Nothing OrElse openPaths.Length = 0 Then Return Nothing

        Dim normalizedScope As New HashSet(Of String)(
            scopePaths.
                Where(Function(pathValue) Not String.IsNullOrWhiteSpace(pathValue)).
                Select(Function(pathValue) normalizeFullPathSafe(pathValue)),
            StringComparer.OrdinalIgnoreCase
        )

        Dim output() As String = openPaths.
            Where(Function(pathValue) normalizedScope.Contains(normalizeFullPathSafe(pathValue))).
            Distinct(StringComparer.OrdinalIgnoreCase).
            ToArray()

        If output.Length = 0 Then Return Nothing
        Return output
    End Function

    Private Class CloseReviewLocalPathState
        Public Property HasLocalLockToken As Boolean = False
        Public Property WorkingCopyState As Char = " "c
        Public Property PropertyState As Char = " "c
        Public Property TreeConflictState As Char = " "c

        Public ReadOnly Property HasLocalChanges As Boolean
            Get
                Return WorkingCopyState <> " "c OrElse
                       PropertyState <> " "c OrElse
                       TreeConflictState <> " "c
            End Get
        End Property
    End Class

    'The owned-lock review intentionally starts from K-token rows. This companion scan is
    'limited to the CAD paths currently being closed and catches the opposite case: a real
    'versioned document has unsaved/in-working-copy edits but no local lock token. It never
    'walks unrelated repository folders and never contacts the SVN server.
    Private Function getUnlockedEditReviewItems(ByVal candidatePaths() As String) As List(Of CloseLockReviewItem)
        Dim output As New List(Of CloseLockReviewItem)()
        Dim filteredPaths() As String = distinctExistingCadFilePaths(candidatePaths)

        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then Return output
        If String.IsNullOrWhiteSpace(sSVNPath) OrElse Not File.Exists(sSVNPath) Then Return Nothing

        Dim workingCopyRoot As String = getResolvedSvnWorkingCopyRootPath()
        If String.IsNullOrWhiteSpace(workingCopyRoot) Then Return Nothing

        Try
            workingCopyRoot = Path.GetFullPath(workingCopyRoot).TrimEnd("\"c)
        Catch
            workingCopyRoot = workingCopyRoot.TrimEnd("\"c)
        End Try

        Dim statesByPath As New Dictionary(Of String, CloseReviewLocalPathState)(StringComparer.OrdinalIgnoreCase)
        Const chunkSize As Integer = 16

        For startIndex As Integer = 0 To filteredPaths.Length - 1 Step chunkSize
            Dim chunk() As String = filteredPaths.Skip(startIndex).Take(chunkSize).ToArray()
            Dim statusResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "status -v --non-interactive " & formatFilePathArrForSvnProc(chunk)
            )
            Dim errorText As String = If(statusResult.outputError, "").Trim()

            If errorText <> "" Then
                writeOperationLog("Unlocked-edit close review status failed: " & errorText)
                Return Nothing
            End If

            Dim lines() As String = If(statusResult.output, "").Split(
                New String() {vbCrLf, vbLf},
                StringSplitOptions.RemoveEmptyEntries
            )

            For Each statusLine As String In lines
                If String.IsNullOrWhiteSpace(statusLine) Then Continue For

                Dim pathStart As Integer = statusLine.IndexOf(
                    workingCopyRoot,
                    StringComparison.OrdinalIgnoreCase
                )
                If pathStart < 0 Then Continue For

                Dim filePath As String = normalizeFullPathSafe(statusLine.Substring(pathStart).Trim())
                If String.IsNullOrWhiteSpace(filePath) OrElse Not isCadFilePath(filePath) Then Continue For

                statesByPath(filePath) = New CloseReviewLocalPathState With {
                    .HasLocalLockToken = statusLine.Length >= 6 AndAlso statusLine(5) = "K"c,
                    .WorkingCopyState = If(statusLine.Length >= 1, statusLine(0), " "c),
                    .PropertyState = If(statusLine.Length >= 2, statusLine(1), " "c),
                    .TreeConflictState = If(statusLine.Length >= 7, statusLine(6), " "c)
                }
            Next
        Next

        For Each candidatePath As String In filteredPaths
            Dim normalizedPath As String = normalizeFullPathSafe(candidatePath)
            Dim localState As CloseReviewLocalPathState = Nothing

            If Not statesByPath.TryGetValue(normalizedPath, localState) Then
                'A clean path can be absent with some SVN client/version combinations. Verify
                'only that exceptional path locally instead of turning the whole close into a
                'false failure or broadening the scan.
                Dim hasLocalChanges As Boolean = False
                Dim hasLocalLockToken As Boolean = False
                Dim workingCopyState As Char = " "c
                Dim stateError As String = ""

                If Not tryGetLocalSvnChangeState(
                    normalizedPath,
                    hasLocalChanges,
                    stateError,
                    hasLocalLockToken,
                    workingCopyState
                ) Then
                    writeOperationLog("Unlocked-edit close review fallback failed: " & stateError)
                    Return Nothing
                End If

                localState = New CloseReviewLocalPathState With {
                    .HasLocalLockToken = hasLocalLockToken,
                    .WorkingCopyState = workingCopyState
                }
            End If

            If localState.HasLocalLockToken Then Continue For

            'New/uncommitted CAD files do not have a repository lock to obtain yet. Their
            'existing first-commit workflow remains responsible for adding and committing them.
            If localState.WorkingCopyState = "?"c OrElse localState.WorkingCopyState = "A"c Then Continue For

            Dim hasUnsavedSolidWorksChanges As Boolean = False
            Dim openDocument As ModelDoc2 = getOpenModelByPathSafe(normalizedPath)

            If openDocument IsNot Nothing Then
                Try
                    hasUnsavedSolidWorksChanges = openDocument.GetSaveFlag()

                    If hasUnsavedSolidWorksChanges AndAlso Not localState.HasLocalChanges Then
                        Dim isEventProvenAssemblyFalseDirty As Boolean = False

                        Try
                            isEventProvenAssemblyFalseDirty =
                                openDocument.GetType() = swDocumentTypes_e.swDocASSEMBLY AndAlso
                                isAssemblyGuardFalseDirtyCandidate(normalizedPath)
                        Catch
                            isEventProvenAssemblyFalseDirty = False
                        End Try

                        If isEventProvenAssemblyFalseDirty Then
                            hasUnsavedSolidWorksChanges = False
                        End If
                    End If
                Catch
                    hasUnsavedSolidWorksChanges = False
                End Try
            End If

            If Not hasUnsavedSolidWorksChanges AndAlso Not localState.HasLocalChanges Then Continue For

            output.Add(
                New CloseLockReviewItem With {
                    .FilePath = normalizedPath,
                    .IsSafeToUnlock = False,
                    .IsStillLocked = False,
                    .RequiresLockBeforeCommit = True,
                    .CanGetLock = True,
                    .CanCommit = False,
                    .CanRevert = False,
                    .StateText = If(hasUnsavedSolidWorksChanges,
                                    "Unsaved edits; SVN lock required",
                                    "Local changes without SVN lock"),
                    .ResultText = "Edits were made without your lock. Select Get Lock, then Commit."
                }
            )
        Next

        Return output.
            OrderBy(Function(item) Path.GetFileName(item.FilePath), StringComparer.OrdinalIgnoreCase).
            ThenBy(Function(item) item.FilePath, StringComparer.OrdinalIgnoreCase).
            ToList()
    End Function

    Private Function getOwnedLockReviewItems(ByVal candidatePaths() As String,
                                             ByVal scanWholeWorkingCopy As Boolean,
                                             Optional ByVal returnNothingOnFailure As Boolean = False) As List(Of CloseLockReviewItem)
        If String.IsNullOrWhiteSpace(sSVNPath) OrElse Not File.Exists(sSVNPath) Then
            Return If(returnNothingOnFailure, Nothing, New List(Of CloseLockReviewItem)())
        End If

        Dim workingCopyRoot As String = getResolvedSvnWorkingCopyRootPath()
        If String.IsNullOrWhiteSpace(workingCopyRoot) Then
            Return If(returnNothingOnFailure, Nothing, New List(Of CloseLockReviewItem)())
        End If

        Try
            workingCopyRoot = Path.GetFullPath(workingCopyRoot).TrimEnd("\"c)
        Catch
            workingCopyRoot = workingCopyRoot.TrimEnd("\"c)
        End Try

        Dim statusArguments As String = "status -v --non-interactive "

        If scanWholeWorkingCopy Then
            statusArguments &= """" & workingCopyRoot & """"
        Else
            Dim filteredPaths() As String = distinctExistingCadFilePaths(candidatePaths)
            If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then Return New List(Of CloseLockReviewItem)()
            statusArguments &= formatFilePathArrForSvnProc(filteredPaths)
        End If

        Dim statusResult As rawProcessReturn = runSvnProcess(sSVNPath, statusArguments)
        Dim outputText As String = If(statusResult.output, "")
        Dim errorText As String = If(statusResult.outputError, "").Trim()

        If errorText <> "" OrElse String.IsNullOrWhiteSpace(outputText) Then
            Return If(returnNothingOnFailure, Nothing, New List(Of CloseLockReviewItem)())
        End If

        Return parseOwnedLockReviewItems(outputText, workingCopyRoot, includeOpenDocumentSaveFlags:=True)
    End Function

    Private Function parseOwnedLockReviewItems(ByVal outputText As String,
                                               ByVal workingCopyRoot As String,
                                               ByVal includeOpenDocumentSaveFlags As Boolean) As List(Of CloseLockReviewItem)
        Dim output As New List(Of CloseLockReviewItem)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If String.IsNullOrWhiteSpace(outputText) OrElse String.IsNullOrWhiteSpace(workingCopyRoot) Then Return output

        Dim lines() As String = outputText.Split(
            New String() {vbCrLf, vbLf},
            StringSplitOptions.RemoveEmptyEntries
        )

        For Each statusLine As String In lines
            If String.IsNullOrWhiteSpace(statusLine) Then Continue For
            If statusLine.Length < 7 Then Continue For

            'SVN status column 6 is K when this working copy owns the lock token.
            If statusLine(5) <> "K"c Then Continue For

            Dim pathStart As Integer = statusLine.IndexOf(
                workingCopyRoot,
                StringComparison.OrdinalIgnoreCase
            )

            If pathStart < 0 Then Continue For

            Dim filePath As String = statusLine.Substring(pathStart).Trim()
            If String.IsNullOrWhiteSpace(filePath) Then Continue For
            If Not isCadFilePath(filePath) Then Continue For

            Try
                filePath = Path.GetFullPath(filePath)
            Catch
            End Try

            If Not seen.Add(filePath) Then Continue For

            Dim workingCopyState As Char = statusLine(0)
            Dim propertyState As Char = statusLine(1)
            Dim treeConflictState As Char = statusLine(6)

            Dim hasUnsavedSolidWorksChanges As Boolean = False

            If includeOpenDocumentSaveFlags Then
                Try
                    Dim openDocument As ModelDoc2 = getOpenModelByPathSafe(filePath)
                    If openDocument IsNot Nothing Then
                        hasUnsavedSolidWorksChanges = openDocument.GetSaveFlag()

                        If hasUnsavedSolidWorksChanges AndAlso
                           canTreatAssemblySaveFlagAsGuardGenerated(openDocument, filePath) Then
                            hasUnsavedSolidWorksChanges = False
                        End If
                    End If
                Catch
                    hasUnsavedSolidWorksChanges = False
                End Try
            End If

            Dim safeToUnlock As Boolean =
                Not hasUnsavedSolidWorksChanges AndAlso
                workingCopyState = " "c AndAlso
                propertyState = " "c AndAlso
                treeConflictState = " "c

            Dim stateText As String = If(
                hasUnsavedSolidWorksChanges,
                "Unsaved SOLIDWORKS changes",
                If(safeToUnlock,
                   "Clean; nothing to commit",
                   getLockReviewUnsafeStateText(workingCopyState, propertyState, treeConflictState))
            )

            output.Add(
                New CloseLockReviewItem With {
                    .FilePath = filePath,
                    .IsSafeToUnlock = safeToUnlock,
                    .StateText = stateText,
                    .IsStillLocked = True,
                    .CanCommit = Not safeToUnlock AndAlso workingCopyState <> "!"c AndAlso workingCopyState <> "?"c,
                    .CanRevert = Not safeToUnlock,
                    .ResultText = If(safeToUnlock,
                                     "Lock retained",
                                     "Return to SOLIDWORKS and resolve changes")
                }
            )
        Next

        Return output.
            OrderBy(Function(item) Path.GetFileName(item.FilePath), StringComparer.OrdinalIgnoreCase).
            ThenBy(Function(item) item.FilePath, StringComparer.OrdinalIgnoreCase).
            ToList()
    End Function

    Private Function getLockReviewUnsafeStateText(ByVal workingCopyState As Char,
                                                  ByVal propertyState As Char,
                                                  ByVal treeConflictState As Char) As String
        Select Case workingCopyState
            Case "M"c
                Return "Local changes not committed"
            Case "A"c
                Return "File not committed yet"
            Case "D"c
                Return "Deletion not committed"
            Case "R"c
                Return "Replacement not committed"
            Case "C"c
                Return "SVN conflict requires attention"
            Case "!"c
                Return "File is missing locally"
            Case "?"c
                Return "File is not versioned"
        End Select

        If propertyState <> " "c Then Return "SVN property changes not committed"
        If treeConflictState <> " "c Then Return "SVN tree conflict requires attention"

        Return "Local SVN changes require attention"
    End Function

    Public Function lockPathFromCloseReviewPublic(ByVal filePath As String,
                                                   ByRef errorMessage As String) As Boolean
        errorMessage = ""

        If Not isOnlineModeEnabled() Then
            errorMessage = "PlumVault is offline. Reconnect before getting the lock."
            Return False
        End If

        If String.IsNullOrWhiteSpace(filePath) OrElse Not File.Exists(filePath) OrElse
           Not isCadFilePath(filePath) OrElse Not isPathInsideLocalRepo(filePath) Then
            errorMessage = "The selected path is not a managed local CAD file."
            Return False
        End If

        If asyncGetLocksInProgress Then
            errorMessage = "Another Get Locks request is already running. Finish it first."
            Return False
        End If

        If Not canRunDeferredSolidWorksUiMutationPublic(allowCloseReview:=True) Then
            errorMessage = "PlumVault is finishing another SOLIDWORKS document operation. Wait a moment and try again."
            Return False
        End If

        pendingCloseReviewLockPath = normalizeFullPathSafe(filePath)

        Try
            getLocksOfPathsAsync(
                New String() {filePath},
                bBreakLocks:=False,
                bUseTortoise:=False,
                sMessage:="Lock obtained from close review",
                allowCloseReview:=True
            )
        Catch ex As Exception
            pendingCloseReviewLockPath = ""
            errorMessage = "Get Lock could not be started: " & ex.Message
            Return False
        End Try

        If Not asyncGetLocksInProgress Then
            pendingCloseReviewLockPath = ""
            errorMessage = "Get Lock could not be started. Wait for any current operation and try again."
            Return False
        End If

        Return True
    End Function

    Public Function commitPathFromCloseReviewPublic(ByVal filePath As String,
                                                     ByRef errorMessage As String) As Boolean
        errorMessage = ""

        If Not isOnlineModeEnabled() Then
            errorMessage = "PlumVault is offline. Reconnect before committing."
            Return False
        End If

        If String.IsNullOrWhiteSpace(filePath) OrElse Not File.Exists(filePath) OrElse
           Not isCadFilePath(filePath) OrElse Not isPathInsideLocalRepo(filePath) Then
            errorMessage = "The selected path is not a managed local CAD file."
            Return False
        End If

        If asyncCommitInProgress Then
            errorMessage = "Another commit is already running. Finish it first."
            Return False
        End If

        'The table normally contains only locks owned by this working copy, but re-check live
        'at the click boundary in case the lock was released externally while the table stayed
        'open. This gives a direct explanation instead of launching a commit that fails later.
        If Not userHasLocalSvnLockTokenForPath(filePath, allowCachedToken:=False) Then
            errorMessage = "You do not have the SVN lock for this file, so it cannot be committed. Get Locks and reopen the close review."
            Return False
        End If

        tortCommitPathsAsync(
            New String() {filePath},
            suppressParentAssemblyNotice:=True
        )
        Return asyncCommitInProgress
    End Function

    Public Function refreshPathFromCloseReviewPublic(ByVal filePath As String,
                                                      ByRef querySucceeded As Boolean,
                                                      ByRef errorMessage As String) As CloseLockReviewItem
        querySucceeded = False
        errorMessage = ""

        If String.IsNullOrWhiteSpace(filePath) OrElse Not File.Exists(filePath) OrElse
           Not isCadFilePath(filePath) OrElse Not isPathInsideLocalRepo(filePath) Then
            errorMessage = "The selected path is not a managed local CAD file."
            Return Nothing
        End If

        Dim refreshed As List(Of CloseLockReviewItem) = getOwnedLockReviewItems(
            candidatePaths:=New String() {filePath},
            scanWholeWorkingCopy:=False,
            returnNothingOnFailure:=True
        )

        If refreshed Is Nothing Then
            errorMessage = "PlumVault could not refresh the file's local SVN status."
            Return Nothing
        End If

        querySucceeded = True
        Return refreshed.FirstOrDefault(Function(item) pathsAreSame(item.FilePath, filePath))
    End Function

    Public Function revertPathFromCloseReviewPublic(ByVal filePath As String,
                                                     ByRef errorMessage As String) As Boolean
        errorMessage = ""

        If iSwApp Is Nothing OrElse myUserControl Is Nothing Then
            errorMessage = "PlumVault is not connected to SOLIDWORKS."
            Return False
        End If

        If String.IsNullOrWhiteSpace(filePath) OrElse Not File.Exists(filePath) OrElse
           Not isCadFilePath(filePath) OrElse Not isPathInsideLocalRepo(filePath) Then
            errorMessage = "The selected path is not a managed local CAD file."
            Return False
        End If

        If closeReviewRevertInProgress Then
            errorMessage = "Another discard is still finishing."
            Return False
        End If

        Dim response As swMessageBoxResult_e = iSwApp.SendMsgToUser2(
            "Revert every change to " & Path.GetFileName(filePath) & " and return its SVN lock?" & vbCrLf & vbCrLf &
            "This permanently discards both unsaved SOLIDWORKS edits and saved local SVN changes. " &
            "After SOLIDWORKS reloads the vault version, PlumVault will return the lock.",
            swMessageBoxIcon_e.swMbWarning,
            swMessageBoxBtn_e.swMbYesNo
        )

        If response <> swMessageBoxResult_e.swMbHitYes Then
            errorMessage = "Revert cancelled; no changes were removed and the lock was retained."
            Return False
        End If

        Dim normalizedPath As String = normalizeFullPathSafe(filePath)
        Dim operationDescription As String = "Discard changes from close review"

        If Not tryBeginSolidWorksNativeMutation(operationDescription) Then
            errorMessage = "Another SOLIDWORKS file operation is still finishing. Try again in a moment."
            Return False
        End If

        closeReviewRevertInProgress = True

        Try
            If myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then
                Throw New InvalidOperationException("The PlumVault task pane is not available.")
            End If

            myUserControl.BeginInvoke(
                New System.Windows.Forms.MethodInvoker(
                    Sub()
                        continueCloseReviewRevert(normalizedPath, 0, "", 0)
                    End Sub
                )
            )
        Catch ex As Exception
            closeReviewRevertInProgress = False
            endSolidWorksNativeMutation(operationDescription)
            errorMessage = "Discard could not be started. " & ex.Message
            Return False
        End Try

        Return True
    End Function

    Private Sub continueCloseReviewRevert(ByVal normalizedPath As String,
                                          ByVal attempt As Integer,
                                          ByVal previousContextSignature As String,
                                          ByVal repeatedContextCount As Integer)
        Try
            'A selected row can be a child currently edited through any number of parent
            'assemblies. Leave one real owner/child relationship per UI turn before touching
            'the file. GetEditTarget self-results are filtered centrally and never enter here.
            Dim ownerToExit As ModelDoc2 = Nothing
            Dim contextSignature As String = ""

            If tryGetInContextOwnerBlockingClose(normalizedPath, ownerToExit, contextSignature) Then
                Dim sameContext As Boolean = String.Equals(
                    contextSignature,
                    previousContextSignature,
                    StringComparison.OrdinalIgnoreCase
                )
                Dim nextRepeatedCount As Integer = If(sameContext, repeatedContextCount + 1, 0)

                If attempt >= 64 OrElse nextRepeatedCount >= 24 Then
                    Throw New InvalidOperationException(
                        "SOLIDWORKS did not finish leaving Edit Part/Edit Assembly mode. " &
                        "Return to SOLIDWORKS, leave the active edit, and try Discard again."
                    )
                End If

                'Give SOLIDWORKS several message turns to complete EditAssembly before
                'reissuing it. Replaying on every turn can itself keep a deep edit transition
                'busy and is a common cause of the old repeated close error.
                If Not sameContext OrElse nextRepeatedCount Mod 4 = 0 Then
                    activateAssemblyForContextExit(ownerToExit, contextSignature)
                    exitAssemblyInContextEditWithoutSavingParent(ownerToExit)
                    writeOperationLog("Queued in-context unwind before close-review discard: " & contextSignature)
                End If

                If myUserControl.IsDisposed OrElse Not myUserControl.IsHandleCreated Then
                    Throw New InvalidOperationException("The PlumVault task pane closed before SOLIDWORKS completed the discard.")
                End If

                myUserControl.BeginInvoke(
                    New System.Windows.Forms.MethodInvoker(
                        Sub()
                            continueCloseReviewRevert(
                                normalizedPath,
                                attempt + 1,
                                contextSignature,
                                nextRepeatedCount
                            )
                        End Sub
                    )
                )
                Exit Sub
            End If

            Dim operationError As String = ""
            Dim success As Boolean = performCloseReviewRevertNow(normalizedPath, operationError)
            completeCloseReviewRevert(normalizedPath, success, operationError)

        Catch ex As Exception
            writeOperationLog("Close-review discard stopped: " & normalizedPath & " | " & ex.Message)
            completeCloseReviewRevert(normalizedPath, False, ex.Message)
        End Try
    End Sub

    Private Sub activateAssemblyForContextExit(ByRef ownerToExit As ModelDoc2,
                                               ByVal contextSignature As String)
        If ownerToExit Is Nothing OrElse iSwApp Is Nothing Then Exit Sub

        Dim ownerPath As String = getAssemblyPathKeySafe(ownerToExit)
        Dim ownerTitle As String = ""

        Try
            ownerTitle = ownerToExit.GetTitle()
        Catch
            ownerTitle = ""
        End Try

        If Not String.IsNullOrWhiteSpace(ownerTitle) Then
            Dim activationErrors As Integer = 0
            iSwApp.ActivateDoc3(
                ownerTitle,
                True,
                swRebuildOnActivation_e.swDontRebuildActiveDoc,
                activationErrors
            )

            If activationErrors <> 0 Then
                writeOperationLog(
                    "Activation returned status " & activationErrors.ToString() &
                    " while unwinding context: " & contextSignature
                )
            End If
        End If

        If Not String.IsNullOrWhiteSpace(ownerPath) Then
            Dim reboundOwner As ModelDoc2 = getOpenModelByPathSafe(ownerPath)
            If reboundOwner IsNot Nothing Then ownerToExit = reboundOwner
        End If
    End Sub

    Private Function tryGetLocalSvnChangeState(ByVal filePath As String,
                                               ByRef hasLocalChanges As Boolean,
                                               ByRef errorMessage As String,
                                               Optional ByRef hasLocalLockToken As Boolean = False,
                                               Optional ByRef workingCopyState As Char = " "c) As Boolean
        hasLocalChanges = False
        hasLocalLockToken = False
        workingCopyState = " "c
        errorMessage = ""

        Try
            Dim statusResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "status --non-interactive --depth empty """ & filePath & """"
            )

            Dim svnError As String = If(statusResult.outputError, "").Trim()
            If svnError <> "" Then
                errorMessage = "SVN status failed: " & svnError
                Return False
            End If

            Dim lines() As String = If(statusResult.output, "").Split(
                New String() {vbCrLf, vbLf},
                StringSplitOptions.RemoveEmptyEntries
            )

            For Each line As String In lines
                If String.IsNullOrWhiteSpace(line) Then Continue For

                If line.Length >= 6 AndAlso line(5) = "K"c Then hasLocalLockToken = True

                workingCopyState = If(line.Length >= 1, line(0), " "c)
                Dim propertyState As Char = If(line.Length >= 2, line(1), " "c)
                Dim treeConflictState As Char = If(line.Length >= 7, line(6), " "c)

                If workingCopyState <> " "c OrElse
                   propertyState <> " "c OrElse
                   treeConflictState <> " "c Then
                    hasLocalChanges = True
                    Exit For
                End If
            Next

            Return True
        Catch ex As Exception
            errorMessage = "SVN status failed: " & ex.Message
            Return False
        End Try
    End Function

    Private Function performCloseReviewRevertNow(ByVal filePath As String,
                                                  ByRef errorMessage As String) As Boolean
        errorMessage = ""

        Dim hasLocalChanges As Boolean = False
        Dim hadLock As Boolean = False
        If Not tryGetLocalSvnChangeState(filePath, hasLocalChanges, errorMessage, hadLock) Then Return False
        Dim openDocument As ModelDoc2 = getOpenModelByPathSafe(filePath)
        Dim documentType As Integer = swDocumentTypes_e.swDocNONE
        Dim documentWasDirty As Boolean = False
        Dim documentWasVisible As Boolean = False
        Dim activePathBefore As String = ""
        Dim drawingWasClosedForDiskRevert As Boolean = False
        Dim solidWorksLocksWereReleased As Boolean = False

        Try
            Dim activeDocument As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDocument IsNot Nothing Then activePathBefore = normalizeFullPathSafe(activeDocument.GetPathName())
        Catch
            activePathBefore = ""
        End Try

        If openDocument IsNot Nothing Then
            Try
                documentType = openDocument.GetType()
            Catch
                documentType = swDocumentTypes_e.swDocNONE
            End Try

            Try
                documentWasDirty = openDocument.GetSaveFlag()
            Catch
                documentWasDirty = False
            End Try

            Try
                documentWasVisible = openDocument.Visible
            Catch
                documentWasVisible = False
            End Try
        End If

        Try
            'Drawings use SOLIDWORKS' drawing-specific close/reopen API when only memory must be
            'discarded. When SVN must overwrite the drawing on disk, close it without saving,
            'revert the working-copy file, then reopen it; ForceReleaseLocks is unsupported for
            'drawings by the SOLIDWORKS API.
            If openDocument IsNot Nothing AndAlso documentType = swDocumentTypes_e.swDocDRAWING Then
                If hasLocalChanges Then
                    Dim drawingTitle As String = ""
                    Try
                        drawingTitle = Path.GetFileName(filePath)
                    Catch
                        drawingTitle = ""
                    End Try

                    If String.IsNullOrWhiteSpace(drawingTitle) Then
                        Try
                            drawingTitle = openDocument.GetTitle()
                        Catch
                            drawingTitle = ""
                        End Try
                    End If

                    controlledDocumentCloseNativeCallInProgress = True
                    Try
                        iSwApp.CloseDoc(drawingTitle)
                    Finally
                        controlledDocumentCloseNativeCallInProgress = False
                    End Try

                    If getOpenModelByPathSafe(filePath) IsNot Nothing Then
                        errorMessage = "SOLIDWORKS did not close the drawing, so no SVN changes were discarded."
                        Return False
                    End If

                    drawingWasClosedForDiskRevert = True
                    openDocument = Nothing
                ElseIf documentWasDirty Then
                    Dim closeOptions As Integer = CInt(swCloseReopenOption_e.swCloseReopenOption_DiscardChanges)
                    If Not hadLock Then closeOptions = closeOptions Or CInt(swCloseReopenOption_e.swCloseReopenOption_ReadOnly)

                    Dim reopenedDrawing As ModelDoc2 = Nothing
                    Dim closeReopenResult As Integer

                    controlledDocumentCloseNativeCallInProgress = True
                    Try
                        closeReopenResult = iSwApp.CloseAndReopen(openDocument, closeOptions, reopenedDrawing)
                    Finally
                        controlledDocumentCloseNativeCallInProgress = False
                    End Try

                    If closeReopenResult <> CInt(swCloseReopenError_e.swCloseReopenNoError) OrElse
                   reopenedDrawing Is Nothing Then
                        errorMessage = "SOLIDWORKS could not discard the drawing's unsaved changes (status " & closeReopenResult.ToString() & ")."
                        Return False
                    End If

                    openDocument = reopenedDrawing
                End If

            ElseIf openDocument IsNot Nothing AndAlso (documentWasDirty OrElse hasLocalChanges) Then
                'ReloadOrReplace requires a document window. An assembly-only referenced child
                'may be loaded invisibly, so expose it just for the verified reload and restore
                'its original visibility afterwards.
                If Not documentWasVisible Then
                    Try
                        openDocument.Visible = True
                    Catch
                    End Try
                End If

                If hasLocalChanges Then
                    Try
                        Dim released As Integer = openDocument.ForceReleaseLocks()
                        solidWorksLocksWereReleased = True
                        writeOperationLog("ForceReleaseLocks before close-review discard returned " & released.ToString() & ": " & filePath)
                    Catch ex As Exception
                        writeOperationLog("ForceReleaseLocks before close-review discard raised: " & filePath & " | " & ex.Message)
                    End Try
                End If
            End If

            Dim revertError As String = ""

            If hasLocalChanges Then
                Try
                    Dim revertResult As rawProcessReturn = runSvnProcess(
                    sSVNPath,
                    "revert --non-interactive """ & filePath & """"
                )
                    revertError = If(revertResult.outputError, "").Trim()
                Catch ex As Exception
                    revertError = ex.Message
                End Try
            End If

            'Reattach/reload even when SVN revert failed. The user explicitly chose Discard, and
            'leaving a ForceReleaseLocks document detached is less safe than reloading the file
            'that remains on disk and reporting the exact SVN error in the table.
            If drawingWasClosedForDiskRevert Then
                Dim openOptions As Integer = CInt(swOpenDocOptions_e.swOpenDocOptions_Silent) Or
                                         CInt(swOpenDocOptions_e.swOpenDocOptions_LoadModel)
                If Not hadLock Then openOptions = openOptions Or CInt(swOpenDocOptions_e.swOpenDocOptions_ReadOnly)

                Dim openErrors As Integer = 0
                Dim openWarnings As Integer = 0
                openDocument = iSwApp.OpenDoc6(
                filePath,
                swDocumentTypes_e.swDocDRAWING,
                openOptions,
                "",
                openErrors,
                openWarnings
            )

                If openDocument Is Nothing Then
                    errorMessage = "The drawing changes were discarded, but SOLIDWORKS could not reopen it (error " & openErrors.ToString() & ")."
                    If revertError <> "" Then errorMessage &= " SVN also reported: " & revertError
                    Return False
                End If

            ElseIf openDocument IsNot Nothing AndAlso
                   documentType <> swDocumentTypes_e.swDocDRAWING AndAlso
                   (documentWasDirty OrElse hasLocalChanges) Then
                Dim reloadResult As Integer

                Try
                    reloadResult = openDocument.ReloadOrReplace(
                    ReadOnly:=Not hadLock,
                    ReplaceFileName:=Nothing,
                    DiscardChanges:=True
                )
                Catch ex As Exception
                    errorMessage = "SOLIDWORKS could not reload the discarded file. " & ex.Message
                    If revertError <> "" Then errorMessage &= " SVN also reported: " & revertError
                    If solidWorksLocksWereReleased Then
                        errorMessage &= " The SOLIDWORKS file lock was released; return to the document and use File > Reload before continuing."
                    End If
                    Return False
                End Try

                If reloadResult <> CInt(swComponentReloadError_e.swReloadOkay) AndAlso
               reloadResult <> CInt(swComponentReloadError_e.swDocumentNotChanged) AndAlso
               reloadResult <> CInt(swComponentReloadError_e.swReadOnlyChanged) Then
                    errorMessage = "SOLIDWORKS could not reload the discarded file (status " & reloadResult.ToString() & ")."
                    If revertError <> "" Then errorMessage &= " SVN also reported: " & revertError
                    If solidWorksLocksWereReleased Then
                        errorMessage &= " Return to the document and use File > Reload before continuing."
                    End If
                    Return False
                End If

                solidWorksLocksWereReleased = False
            End If

            If revertError <> "" Then
                errorMessage = "SVN could not discard the saved local changes: " & revertError
                Return False
            End If

            Dim stillHasLocalChanges As Boolean = False
            If Not tryGetLocalSvnChangeState(filePath, stillHasLocalChanges, errorMessage) Then Return False
            If stillHasLocalChanges Then
                errorMessage = "SVN still reports local changes after Discard. Return to SOLIDWORKS and review the file."
                Return False
            End If

            Dim verifiedDocument As ModelDoc2 = getOpenModelByPathSafe(filePath)
            If verifiedDocument IsNot Nothing Then
                Try
                    'tryGetLocalSvnChangeState just above already proved the on-disk file is
                    'clean (no local SVN changes remain). GetSaveFlag() can still read True here
                    'purely because of the reload SOLIDWORKS just performed - a documented
                    'SOLIDWORKS limitation elsewhere in this add-in (no public API clears
                    'GetSaveFlag once set, e.g. after a rebuild). Since the file's actual content
                    'is already verified clean, that flag cannot represent a real unsaved change
                    'and must not block reporting the discard as successful.
                    If verifiedDocument.GetSaveFlag() Then
                        writeOperationLog(
                            "Close-review discard: SOLIDWORKS still shows a dirty flag after a " &
                            "verified-clean reload, treating as spurious: " & filePath
                        )
                    End If
                Catch ex As Exception
                    errorMessage = "PlumVault could not verify the reloaded SOLIDWORKS document: " & ex.Message
                    Return False
                End Try
            End If

            Try
                updateStatusCacheForKnownPaths(New String() {filePath}, forceAddDelChg1:=" ")
                invalidateOwnedLocksWholeCopySnapshotPublic()
            Catch
            End Try

            Return True
        Finally
            'Every early return above still restores the transient UI state used to make a
            'referenced child reloadable. Without this finally block, one failed SVN/reload
            'step could leave a formerly hidden child window visible or steal focus from the
            'assembly/drawing the user was working in.
            If openDocument IsNot Nothing AndAlso Not documentWasVisible AndAlso
               documentType <> swDocumentTypes_e.swDocDRAWING Then
                Try
                    openDocument.Visible = False
                Catch
                End Try
            End If

            If Not String.IsNullOrWhiteSpace(activePathBefore) AndAlso
               Not pathsAreSame(activePathBefore, filePath) Then
                Try
                    Dim previousActive As ModelDoc2 = getOpenModelByPathSafe(activePathBefore)
                    If previousActive IsNot Nothing Then
                        Dim activationErrors As Integer = 0
                        iSwApp.ActivateDoc3(
                            previousActive.GetTitle(),
                            True,
                            swRebuildOnActivation_e.swDontRebuildActiveDoc,
                            activationErrors
                        )
                    End If
                Catch
                End Try
            End If
        End Try
    End Function

    Private Sub completeCloseReviewRevert(ByVal filePath As String,
                                          ByVal success As Boolean,
                                          ByVal errorMessage As String)
        closeReviewRevertInProgress = False
        endSolidWorksNativeMutation("Discard changes from close review")

        Try
            RaiseEvent CloseReviewRevertCompleted(filePath, success, If(errorMessage, ""))
        Catch ex As Exception
            writeOperationLog("Close-review discard completion handler failed: " & ex.Message)
        End Try
    End Sub

    Public Function unlockPathFromCloseReviewPublic(ByVal filePath As String,
                                                     ByRef errorMessage As String) As Boolean
        errorMessage = ""

        If iSwApp Is Nothing OrElse myUserControl Is Nothing Then
            errorMessage = "PlumVault is not connected to SOLIDWORKS."
            Return False
        End If

        If Not isOnlineModeEnabled() Then
            errorMessage = "PlumVault is offline. Reconnect before releasing the lock."
            Return False
        End If

        If String.IsNullOrWhiteSpace(filePath) OrElse Not File.Exists(filePath) Then
            errorMessage = "The CAD file could not be found locally."
            Return False
        End If

        If Not isCadFilePath(filePath) OrElse Not isPathInsideLocalRepo(filePath) Then
            errorMessage = "The selected file is not a managed CAD file in this SVN working copy."
            Return False
        End If

        'Recheck the exact file immediately before unlock so a stale table can never
        'release a file that was edited after the review window opened.
        Dim currentItems As List(Of CloseLockReviewItem) = getOwnedLockReviewItems(
            candidatePaths:=New String() {filePath},
            scanWholeWorkingCopy:=False,
            returnNothingOnFailure:=True
        )

        If currentItems Is Nothing Then
            errorMessage = "PlumVault could not verify the file's current SVN lock and change state. Click Sync and try again."
            Return False
        End If

        Dim currentItem As CloseLockReviewItem = currentItems.
            FirstOrDefault(Function(item) String.Equals(
                normalizeSvnPath(item.FilePath),
                normalizeSvnPath(filePath),
                StringComparison.OrdinalIgnoreCase
            ))

        If currentItem Is Nothing Then
            errorMessage = "The file is no longer locked by this working copy."
            Return False
        End If

        If Not currentItem.IsSafeToUnlock Then
            errorMessage = currentItem.StateText & ". Commit or revert the file before unlocking it."
            Return False
        End If

        Dim unlockResult As rawProcessReturn = runSvnProcess(
            sSVNPath,
            "unlock --non-interactive """ & filePath & """"
        )

        Dim svnError As String = If(unlockResult.outputError, "").Trim()
        If svnError <> "" Then
            errorMessage = svnError
            Return False
        End If

        'Do not call ModelDoc2.SetReadOnlyState(True) here. SVN unlock already applies
        'the working-copy read-only state for needs-lock files, and forcing the open
        'SOLIDWORKS document into read-only mode can set a false document save/dirty flag.
        'That false flag caused the next close attempt to report uncommitted changes even
        'though the file was clean. Future saves remain protected by PlumVault's lock check.

        Try
            updateStatusCacheForKnownPaths(New String() {filePath}, forceLock6:=" ")
        Catch
        End Try

        'The lock-review form can be open while documents are closing. Never touch the
        'SOLIDWORKS tree/ActiveDoc when there are no open documents; the SVN unlock and cache
        'update above are already complete.
        If hasAnyOpenSolidWorksDocument() Then
            Try
                Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
                If activeDoc IsNot Nothing Then
                    updateLockStatusPublic(bRefreshAllTreeViews:=False)
                    refreshActiveTreeAfterSvnAction(
                        bUpdateLocalLockStatus:=False,
                        bRebuildTree:=False
                    )
                End If
            Catch
            End Try
        End If

        Return True
    End Function

    Private Function userHasSvnLockOnDoc(ByVal doc As ModelDoc2) As Boolean
        If doc Is Nothing Then Return False

        Dim docPath As String = ""

        Try
            docPath = doc.GetPathName()
        Catch
            docPath = ""
        End Try

        If String.IsNullOrWhiteSpace(docPath) Then Return False
        If Not isCadFilePath(docPath) Then Return False
        If Not isPathInsideLocalRepo(docPath) Then Return False

        'New/unversioned files do not have SVN locks yet, but they are valid to add/commit.
        If isNewUnversionedOrAddedFile(docPath) Then Return True

        Try
            Dim docsToCheck As ModelDoc2() = New ModelDoc2() {doc}

            Dim status As SVNStatus = getFileSVNStatus(
                bCheckServer:=False,
                modDocArr:=docsToCheck,
                bUpdateStatusOfAllOpenModels:=False
            )

            If status IsNot Nothing AndAlso status.fp IsNot Nothing AndAlso status.fp.Length > 0 Then
                If status.fp(0).lock6 = "K" Then Return True
            End If
        Catch
        End Try

        Try
            Dim cachedStatus As SVNStatus = findStatusForFile(docPath)

            If cachedStatus IsNot Nothing AndAlso cachedStatus.fp IsNot Nothing AndAlso cachedStatus.fp.Length > 0 Then
                If cachedStatus.fp(0).lock6 = "K" Then Return True
            End If
        Catch
        End Try

        Return False
    End Function

    Private Function consumeUnsafeDocumentCloseApproval(ByVal filePath As String) As Boolean
        If DateTime.Now >= unsafeForceCloseApprovedUntil Then Return False
        If String.IsNullOrWhiteSpace(filePath) OrElse String.IsNullOrWhiteSpace(unsafeForceCloseApprovedPath) Then Return False
        If Not pathsAreSame(filePath, unsafeForceCloseApprovedPath) Then Return False

        unsafeForceCloseApprovedUntil = DateTime.MinValue
        unsafeForceCloseApprovedPath = ""
        Return True
    End Function

    Private Function showUnsafeClosePrompt(ByVal unsafeMsg As String,
                                           Optional ByVal approvedDocumentPath As String = "") As Boolean
        Dim response As Integer = iSwApp.SendMsgToUser2(
            "One or more open CAD files are not safe to close yet." & vbCrLf & vbCrLf &
            unsafeMsg & vbCrLf &
            "Choose an action:" & vbCrLf &
            "Yes = Cancel close and return to SOLIDWORKS" & vbCrLf &
            "No = Close without saving these changes (they may be lost)",
            swMessageBoxIcon_e.swMbWarning,
            swMessageBoxBtn_e.swMbYesNo
        )

        If response = swMessageBoxResult_e.swMbHitYes Then
            iSwApp.SendMsgToUser2(
                "Close cancelled." & vbCrLf & vbCrLf &
                "Get Locks if needed, then Save/Commit your files, or use Unlock && Revert to discard the local work intentionally.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )

            Return True 'Block close
        End If

        If response = swMessageBoxResult_e.swMbHitNo Then
            'Allow duplicate close events through briefly.
            'This prevents the same force-close choice from prompting multiple times.
            unsafeForceCloseApprovedUntil = DateTime.Now.AddSeconds(10)
            unsafeForceCloseApprovedPath = If(
                String.IsNullOrWhiteSpace(approvedDocumentPath),
                "",
                normalizeFullPathSafe(approvedDocumentPath)
            )
            Return False 'Allow close anyway
        End If

        Return True 'Safety fallback: block close
    End Function


    Private Function getUnsafeCloseStatusMessage(openPaths As List(Of String)) As String
        If openPaths Is Nothing OrElse openPaths.Count = 0 Then Return ""

        Dim msg As String = ""
        Dim statusPaths As New List(Of String)()
        Dim seenStatusPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each filePath As String In openPaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            If filePath.StartsWith("[UNSAVED_SOLIDWORKS_CHANGES]") Then
                msg &= filePath.Replace("[UNSAVED_SOLIDWORKS_CHANGES] ", "") & vbCrLf &
                    "SOLIDWORKS has unsaved changes and you have the SVN lock, or this is a new file ready to be committed." & vbCrLf & vbCrLf
                Continue For
            End If

            If filePath.StartsWith("[UNSAVED_WITHOUT_LOCK]") Then
                msg &= filePath.Replace("[UNSAVED_WITHOUT_LOCK] ", "") & vbCrLf &
                    "SOLIDWORKS has unsaved changes, but you do NOT have the SVN lock. Get Locks before saving/committing, or discard the changes intentionally." & vbCrLf & vbCrLf
                Continue For
            End If

            If filePath.StartsWith("[UNSAVED_NEW_FILE]") Then
                msg &= filePath.Replace("[UNSAVED_NEW_FILE] ", "") & vbCrLf &
                    "New SOLIDWORKS file has not been saved yet." & vbCrLf & vbCrLf
                Continue For
            End If

            If Not File.Exists(filePath) Then Continue For

            Dim normalizedPath As String = normalizeFullPathSafe(filePath)
            If seenStatusPaths.Add(normalizedPath) Then statusPaths.Add(normalizedPath)
        Next

        'Only local working-copy state matters when deciding whether close can lose work.
        'Batch paths so a deep assembly does not launch one svn.exe process per document,
        'and never add -u here: a server round trip can hang shutdown and remote freshness
        'does not make a locally clean file unsafe to close.
        Const closeStatusChunkSize As Integer = 60

        For startIndex As Integer = 0 To statusPaths.Count - 1 Step closeStatusChunkSize
            Dim chunk As String() = statusPaths.Skip(startIndex).Take(closeStatusChunkSize).ToArray()

            Try
                Dim statusResult As rawProcessReturn = runSvnProcess(
                    sSVNPath,
                    "status --non-interactive " & formatFilePathArrForSvnProc(chunk)
                )

                Dim outputText As String = If(statusResult.output, "").Trim()
                Dim errorText As String = If(statusResult.outputError, "").Trim()

                If errorText <> "" Then
                    msg &= "Could not verify local SVN status for " & chunk.Length.ToString() & " open CAD file(s)." & vbCrLf &
                        "SVN status error: " & errorText & vbCrLf & vbCrLf
                    Continue For
                End If

                If String.IsNullOrWhiteSpace(outputText) Then Continue For

                Dim lines() As String = outputText.Split({vbCrLf, vbLf}, StringSplitOptions.RemoveEmptyEntries)

                For Each line As String In lines
                    If String.IsNullOrWhiteSpace(line) Then Continue For

                    Dim reason As String = getHumanReadableSvnCloseReason(line)
                    If reason = "" Then Continue For

                    Dim matchedPath As String = chunk.FirstOrDefault(
                        Function(candidate) line.IndexOf(candidate, StringComparison.OrdinalIgnoreCase) >= 0
                    )

                    Dim displayName As String = "CAD file"
                    If Not String.IsNullOrWhiteSpace(matchedPath) Then
                        displayName = Path.GetFileName(matchedPath)
                    ElseIf line.Length > 7 Then
                        Try
                            displayName = Path.GetFileName(line.Substring(7).Trim())
                        Catch
                        End Try
                    End If

                    msg &= displayName & vbCrLf & reason & vbCrLf & vbCrLf
                Next

            Catch
                msg &= "Could not verify local SVN status for open CAD files before close." & vbCrLf & vbCrLf
            End Try
        Next

        Return msg
    End Function

    Private Function getHumanReadableSvnCloseReason(statusLine As String) As String
        If String.IsNullOrWhiteSpace(statusLine) Then Return ""

        Dim wcStatus As Char = " "c
        Dim remoteStatus As Char = " "c

        If statusLine.Length >= 1 Then wcStatus = statusLine(0)
        If statusLine.Length >= 9 Then remoteStatus = statusLine(8)

        Select Case wcStatus
            Case "?"c
                Return "Saved inside the SVN folder but not added/committed yet. Click Commit first."
            Case "A"c
                Return "Scheduled for addition but not committed yet."
            Case "M"c
                Return "Modified locally and not committed."
            Case "D"c
                Return "Scheduled for deletion and not committed."
            Case "R"c
                Return "Scheduled for replacement and not committed."
            Case "C"c
                Return "SVN conflict detected."
            Case "!"c
                Return "Missing from disk but still tracked by SVN."
            Case "~"c
                Return "Obstructed or wrong item type in working copy."
        End Select

        If remoteStatus = "*"c Then
            Return "Out of date compared to SVN server. Use Get Latest before closing."
        End If

        Return ""
    End Function

    Private Function filterOutNewUnversionedOrAddedDocs(ByRef modDocArr() As ModelDoc2) As ModelDoc2()
        If modDocArr Is Nothing Then Return Nothing

        Dim filteredDocs As New List(Of ModelDoc2)

        For Each doc As ModelDoc2 In modDocArr
            If doc Is Nothing Then Continue For

            Dim docPath As String = ""

            Try
                docPath = doc.GetPathName()
            Catch
                Continue For
            End Try

            If String.IsNullOrWhiteSpace(docPath) Then Continue For

            If isNewUnversionedOrAddedFile(docPath) Then
                Continue For
            End If

            filteredDocs.Add(doc)
        Next

        Return filteredDocs.ToArray()
    End Function

    Private Function getFirstSvnStatusChar(filePath As String) As Char
        If String.IsNullOrWhiteSpace(filePath) Then Return ChrW(0)
        If Not File.Exists(filePath) Then Return ChrW(0)
        If Not isPathInsideLocalRepo(filePath) Then Return ChrW(0)

        Try
            Dim statusResult As rawProcessReturn = runSvnProcess(
            sSVNPath,
            "status --non-interactive """ & filePath & """"
        )

            If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
                Return ChrW(0)
            End If

            Dim statusText As String = ""

            If statusResult.output IsNot Nothing Then
                statusText = statusResult.output.Trim()
            End If

            If String.IsNullOrWhiteSpace(statusText) Then
                Return " "c 'Clean/versioned
            End If

            Return statusText(0)

        Catch
            Return ChrW(0)
        End Try
    End Function


    Private Function isNewUnversionedOrAddedFile(filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not File.Exists(filePath) Then Return False
        If Not isPathInsideLocalRepo(filePath) Then Return False

        Try
            Dim statusResult As rawProcessReturn = runSvnProcess(
            sSVNPath,
            "status --non-interactive """ & filePath & """"
        )

            If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
                Return False
            End If

            Dim statusText As String = ""

            If statusResult.output IsNot Nothing Then
                statusText = statusResult.output.Trim()
            End If

            If String.IsNullOrWhiteSpace(statusText) Then
                Return False
            End If

            Dim firstStatusChar As Char = statusText(0)

            '? = unversioned but inside working copy
            'A = scheduled for addition
            Return firstStatusChar = "?"c OrElse firstStatusChar = "A"c

        Catch
            Return False
        End Try
    End Function

    Private Sub keepNewUncommittedCadFilesWritable()
        If iSwApp Is Nothing Then Exit Sub

        Try
            Dim docsObj As Object = iSwApp.GetDocuments()
            If docsObj Is Nothing Then Exit Sub

            Dim docs As Object() = CType(docsObj, Object())

            For Each docObj As Object In docs
                Dim doc As ModelDoc2 = TryCast(docObj, ModelDoc2)
                If doc Is Nothing Then Continue For

                Dim docPath As String = ""

                Try
                    docPath = doc.GetPathName()
                Catch
                    Continue For
                End Try

                If String.IsNullOrWhiteSpace(docPath) Then Continue For
                If Not isCadFilePath(docPath) Then Continue For
                If Not isPathInsideLocalRepo(docPath) Then Continue For

                If isNewUnversionedOrAddedFile(docPath) Then
                    Try
                        File.SetAttributes(docPath, File.GetAttributes(docPath) And Not FileAttributes.ReadOnly)
                    Catch
                    End Try
                End If
            Next

        Catch
        End Try
    End Sub

    Private Function normalizeSvnPath(pathInput As String) As String
        If String.IsNullOrWhiteSpace(pathInput) Then Return ""

        Try
            If Not Path.IsPathRooted(pathInput) Then
                pathInput = Path.Combine(myUserControl.localRepoPath.Text.TrimEnd("\"c), pathInput)
            End If

            Return Path.GetFullPath(pathInput).TrimEnd("\"c).ToLowerInvariant()
        Catch
            Return pathInput.Replace("/", "\").TrimEnd("\"c).ToLowerInvariant()
        End Try
    End Function


    Private Function statusContainsServerAwareData(ByVal statusToCheck As SVNStatus) As Boolean
        Try
            If statusToCheck Is Nothing OrElse statusToCheck.fp Is Nothing Then Return False

            For i As Integer = 0 To UBound(statusToCheck.fp)
                Dim updateColumn As String = statusToCheck.fp(i).upToDate9

                If updateColumn IsNot Nothing AndAlso
                   Not String.Equals(updateColumn, "NoUpdate", StringComparison.OrdinalIgnoreCase) Then
                    Return True
                End If
            Next
        Catch
        End Try

        Return False
    End Function

    Private Function cacheEntryHasServerAwareData(ByVal entry As SVNStatus.filePpty) As Boolean
        Try
            Return entry.upToDate9 IsNot Nothing AndAlso
                   Not String.Equals(entry.upToDate9, "NoUpdate", StringComparison.OrdinalIgnoreCase)
        Catch
            Return False
        End Try
    End Function

    Private Sub notifyStatusCacheChanged()
        Try
            If myUserControl Is Nothing Then Exit Sub

            If myUserControl.IsHandleCreated Then
                myUserControl.BeginInvoke(New System.Windows.Forms.MethodInvoker(
                    Sub()
                        Try
                            myUserControl.updateCacheAgeIndicatorPublic()
                        Catch
                        End Try
                    End Sub
                ))
            End If
        Catch
            Try
                myUserControl.updateCacheAgeIndicatorPublic()
            Catch
            End Try
        End Try
    End Sub

    Public Function getStatusCacheAgeDisplayTextPublic() As String
        Try
            'The UI indicator represents the age of the last real server Sync only.
            'Get Locks, Commit, Unlock, Refresh and other local cache edits must not make it say "now".
            If statusCacheLastServerAwareUtc = DateTime.MinValue Then Return "not synced"

            Dim serverAge As TimeSpan = DateTime.UtcNow - statusCacheLastServerAwareUtc
            If serverAge.TotalSeconds < 0 Then serverAge = TimeSpan.Zero

            If serverAge.TotalSeconds < 60 Then
                Return "sync now"
            ElseIf serverAge.TotalMinutes < 60 Then
                Return "sync " & CInt(Math.Floor(serverAge.TotalMinutes)).ToString() & "m"
            ElseIf serverAge.TotalHours < 24 Then
                Return "sync " & CInt(Math.Floor(serverAge.TotalHours)).ToString() & "h"
            Else
                Return "sync " & CInt(Math.Floor(serverAge.TotalDays)).ToString() & "d"
            End If
        Catch
            Return "unknown"
        End Try
    End Function

    Private Sub markStatusCacheWritten(ByVal markAsServerSync As Boolean)
        statusCacheLastWriteUtc = DateTime.UtcNow

        If markAsServerSync Then
            statusCacheLastServerAwareUtc = statusCacheLastWriteUtc
        End If

        notifyStatusCacheChanged()
    End Sub

    Private Sub rebuildStatusCacheFromStatus(ByVal statusToCache As SVNStatus,
                                              Optional ByVal markAsServerSync As Boolean = False)
        Try
            If statusToCache Is Nothing OrElse statusToCache.fp Is Nothing Then Exit Sub

            'Only an explicit Sync replaces the bounded Sync cache and advances its age.
            'Other actions may obtain server-aware information for their selected files, but they
            'must merge those entries without erasing the last Sync result or changing its timestamp.
            If markAsServerSync Then
                statusCacheByNormalizedPath.Clear()
            End If

            For i As Integer = 0 To UBound(statusToCache.fp)
                Dim filePath As String = statusToCache.fp(i).filename
                If String.IsNullOrWhiteSpace(filePath) Then Continue For

                Dim normalizedPath As String = normalizeSvnPath(filePath)
                If String.IsNullOrWhiteSpace(normalizedPath) Then Continue For

                Dim entryToStore As SVNStatus.filePpty = statusToCache.fp(i)

                If statusCacheByNormalizedPath.ContainsKey(normalizedPath) Then
                    Dim previousEntry As SVNStatus.filePpty = statusCacheByNormalizedPath(normalizedPath)

                    'Local-only updates have NoUpdate in column 9. Preserve the last known server
                    'state for that path. A targeted Get Locks server check may update its own path,
                    'but it still does not become a new Sync or clear other cached branch entries.
                    If cacheEntryHasServerAwareData(previousEntry) AndAlso
                       (entryToStore.upToDate9 Is Nothing OrElse String.Equals(entryToStore.upToDate9, "NoUpdate", StringComparison.OrdinalIgnoreCase)) Then
                        entryToStore.upToDate9 = previousEntry.upToDate9
                    End If
                End If

                statusCacheByNormalizedPath(normalizedPath) = entryToStore
            Next

            markStatusCacheWritten(markAsServerSync)
        Catch
            Try
                If statusCacheByNormalizedPath Is Nothing Then
                    statusCacheByNormalizedPath = New Dictionary(Of String, SVNStatus.filePpty)(StringComparer.OrdinalIgnoreCase)
                End If
            Catch
            End Try
        End Try
    End Sub

    Private Sub updateStatusCacheForKnownPaths(ByVal filePaths() As String,
                                                Optional ByVal forceAddDelChg1 As String = Nothing,
                                                Optional ByVal forceLock6 As String = Nothing,
                                                Optional ByVal forceUpToDate9 As String = Nothing,
                                                Optional ByVal forceReleased As String = Nothing)
        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        Dim filteredPaths() As String = filterExistingCadFilePathsOnly(filePaths)
        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then Exit Sub

        Try
            For Each filePathInput As String In filteredPaths
                If String.IsNullOrWhiteSpace(filePathInput) Then Continue For

                Dim filePath As String = filePathInput

                Try
                    filePath = Path.GetFullPath(filePathInput)
                Catch
                End Try

                Dim normalizedPath As String = normalizeSvnPath(filePath)
                If String.IsNullOrWhiteSpace(normalizedPath) Then Continue For

                Dim entry As SVNStatus.filePpty

                If statusCacheByNormalizedPath.ContainsKey(normalizedPath) Then
                    entry = statusCacheByNormalizedPath(normalizedPath)
                Else
                    entry = New SVNStatus.filePpty()
                    entry.filename = filePath
                    entry.modDoc = Nothing
                    entry.bReconnect = False
                    entry.revertUpdate = getLatestType.none
                    entry.addDelChg1 = " "
                    entry.pptyMods2 = " "
                    entry.workingDirLock3 = " "
                    entry.addWithHist4 = " "
                    entry.switchWParent5 = " "
                    entry.lock6 = " "
                    entry.lockOwner = ""
                    entry.tree7 = " "
                    entry.upToDate9 = "NoUpdate"
                    entry.released = ""
                    entry.iTemp = 0
                End If

                entry.filename = filePath

                Try
                    entry.modDoc = TryCast(iSwApp.GetOpenDocumentByName(filePath), ModelDoc2)
                Catch
                    entry.modDoc = Nothing
                End Try

                If forceAddDelChg1 IsNot Nothing Then entry.addDelChg1 = forceAddDelChg1
                If forceLock6 IsNot Nothing Then entry.lock6 = forceLock6
                If forceUpToDate9 IsNot Nothing Then entry.upToDate9 = forceUpToDate9
                If forceReleased IsNot Nothing Then entry.released = forceReleased

                statusCacheByNormalizedPath(normalizedPath) = entry
            Next

            markStatusCacheWritten(False)

            'Every Get Locks / Unlock / Commit code path in the plugin already calls here
            'with forceLock6 to record the lock change it just made. Centralizing the
            'close-time snapshot invalidation on that same signal - instead of chasing down
            'every individual call site - means it cannot be missed by a future call path.
            If forceLock6 IsNot Nothing Then invalidateOwnedLocksWholeCopySnapshotPublic()
        Catch
        End Try
    End Sub

    Private Function tryFindCachedStatusProperty(ByVal filePath As String, ByRef foundStatus As SVNStatus.filePpty) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Try
            If statusCacheByNormalizedPath Is Nothing OrElse statusCacheByNormalizedPath.Count = 0 Then
                rebuildStatusCacheFromStatus(statusOfAllOpenModels, markAsServerSync:=False)
            End If

            Dim normalizedPath As String = normalizeSvnPath(filePath)
            If statusCacheByNormalizedPath.ContainsKey(normalizedPath) Then
                foundStatus = statusCacheByNormalizedPath(normalizedPath)
                Return True
            End If

            'Fallback for older tree nodes that may pass only the filename.
            Dim fileNameOnly As String = Path.GetFileName(filePath)
            If fileNameOnly <> "" Then
                For Each kvp As KeyValuePair(Of String, SVNStatus.filePpty) In statusCacheByNormalizedPath
                    If String.Equals(Path.GetFileName(kvp.Value.filename), fileNameOnly, StringComparison.OrdinalIgnoreCase) Then
                        foundStatus = kvp.Value
                        Return True
                    End If
                Next
            End If
        Catch
        End Try

        Return False
    End Function
    Private Function getSvnLockOwnersByPath(targetPath As String) As Dictionary(Of String, String)
        Dim lockOwners As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)

        If String.IsNullOrWhiteSpace(targetPath) Then Return lockOwners

        Dim xmlStatus As rawProcessReturn = runSvnProcess(
        sSVNPath,
        "status -u --xml --non-interactive """ & targetPath.TrimEnd("\"c) & """"
    )

        If xmlStatus.output Is Nothing Then Return lockOwners
        If xmlStatus.outputError IsNot Nothing AndAlso xmlStatus.outputError.Trim() <> "" Then Return lockOwners
        If xmlStatus.output.Trim() = "" Then Return lockOwners

        Dim doc As New XmlDocument()

        Try
            doc.LoadXml(xmlStatus.output)
        Catch
            Return lockOwners
        End Try

        Dim entries As XmlNodeList = doc.SelectNodes("/status/target/entry")

        For Each entry As XmlNode In entries
            If entry.Attributes Is Nothing Then Continue For
            If entry.Attributes("path") Is Nothing Then Continue For

            Dim entryPath As String = entry.Attributes("path").Value
            Dim ownerNode As XmlNode = entry.SelectSingleNode("repos-status/lock/owner")

            If ownerNode Is Nothing Then
                ownerNode = entry.SelectSingleNode("wc-status/lock/owner")
            End If

            If ownerNode Is Nothing Then Continue For

            Dim owner As String = ownerNode.InnerText.Trim()
            If owner = "" Then Continue For

            lockOwners(normalizeSvnPath(entryPath)) = owner
        Next

        Return lockOwners
    End Function


    Private Function getSvnLockOwnersForFilePaths(ByVal targetPaths() As String) As Dictionary(Of String, String)
        Dim lockOwners As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)

        If targetPaths Is Nothing OrElse targetPaths.Length = 0 Then Return lockOwners

        Dim filteredPaths() As String = filterExistingCadFilePathsOnly(targetPaths)
        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then Return lockOwners

        Dim currentChunk As New List(Of String)()
        Dim currentLength As Integer = 0
        Dim maxCommandLength As Integer = 28000

        For Each filePath As String In filteredPaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            Dim quotedPath As String = """" & filePath & """"
            Dim addLength As Integer = quotedPath.Length + 1

            If currentChunk.Count > 0 AndAlso currentLength + addLength > maxCommandLength Then
                mergeLockOwnerDictionaries(lockOwners, getSvnLockOwnersForFilePathChunk(currentChunk.ToArray()))
                currentChunk.Clear()
                currentLength = 0
            End If

            currentChunk.Add(filePath)
            currentLength += addLength
        Next

        If currentChunk.Count > 0 Then
            mergeLockOwnerDictionaries(lockOwners, getSvnLockOwnersForFilePathChunk(currentChunk.ToArray()))
        End If

        Return lockOwners
    End Function

    Private Sub mergeLockOwnerDictionaries(ByVal destination As Dictionary(Of String, String),
                                           ByVal source As Dictionary(Of String, String))
        If destination Is Nothing Then Exit Sub
        If source Is Nothing Then Exit Sub

        For Each kvp As KeyValuePair(Of String, String) In source
            destination(kvp.Key) = kvp.Value
        Next
    End Sub

    Private Function getSvnLockOwnersForFilePathChunk(ByVal targetPaths() As String) As Dictionary(Of String, String)
        Dim lockOwners As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)

        If targetPaths Is Nothing OrElse targetPaths.Length = 0 Then Return lockOwners

        Dim pathArgs As String = ""

        For Each filePath As String In targetPaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For
            pathArgs &= """" & filePath & """" & " "
        Next

        If pathArgs.Trim() = "" Then Return lockOwners

        Dim xmlStatus As rawProcessReturn = runSvnProcess(
        sSVNPath,
        "status -u --xml --non-interactive " & pathArgs.Trim()
    )

        If xmlStatus.output Is Nothing Then Return lockOwners
        If xmlStatus.outputError IsNot Nothing AndAlso xmlStatus.outputError.Trim() <> "" Then Return lockOwners
        If xmlStatus.output.Trim() = "" Then Return lockOwners

        Dim doc As New XmlDocument()

        Try
            doc.LoadXml(xmlStatus.output)
        Catch
            Return lockOwners
        End Try

        Dim entries As XmlNodeList = doc.SelectNodes("/status/target/entry")

        For Each entry As XmlNode In entries
            If entry.Attributes Is Nothing Then Continue For
            If entry.Attributes("path") Is Nothing Then Continue For

            Dim entryPath As String = entry.Attributes("path").Value
            Dim ownerNode As XmlNode = entry.SelectSingleNode("repos-status/lock/owner")

            If ownerNode Is Nothing Then
                ownerNode = entry.SelectSingleNode("wc-status/lock/owner")
            End If

            If ownerNode Is Nothing Then Continue For

            Dim owner As String = ownerNode.InnerText.Trim()
            If owner = "" Then Continue For

            lockOwners(normalizeSvnPath(entryPath)) = owner
        Next

        Return lockOwners
    End Function


    Private Function isUsableSvnStatusOutputLine(ByVal statusLine As String) As Boolean
        If String.IsNullOrWhiteSpace(statusLine) Then Return False

        Try
            If statusLine.StartsWith("Status against revision", StringComparison.OrdinalIgnoreCase) Then Return False

            'A targeted svn status -u call can legitimately return only one line.
            'This happens for added/renamed/new files, for example:
            'A       ?       C:\SVN test\part.SLDPRT
            'Do not treat that as "incomplete status" just because there is no second line.
            If myUserControl IsNot Nothing AndAlso myUserControl.localRepoPath IsNot Nothing Then
                Dim repoRoot As String = myUserControl.localRepoPath.Text
                If Not String.IsNullOrWhiteSpace(repoRoot) Then
                    If statusLine.IndexOf(repoRoot, StringComparison.OrdinalIgnoreCase) >= 0 Then
                        Return True
                    End If
                End If
            End If

            'Fallback: accept normal SVN status rows if they contain a CAD file name.
            Dim upperLine As String = statusLine.ToUpperInvariant()
            If upperLine.Contains(".SLDPRT") OrElse upperLine.Contains(".SLDASM") OrElse upperLine.Contains(".SLDDRW") Then
                Return True
            End If

        Catch
        End Try

        Return False
    End Function

    Public Function getFileSVNStatus(ByVal bCheckServer As Boolean,
                              Optional ByRef modDocArr() As ModelDoc2 = Nothing,
                              Optional ByRef bUpdateStatusOfAllOpenModels As Boolean = True,
                              Optional ByVal iRecursiveLevel As Integer = 0,
                              Optional ByRef sDirectFilePathArr() As String = Nothing) As SVNStatus
        'Pass sFilePath = Create from the file path
        'Pass modDocArr = create from the modDocArr
        'Pass Neither = create for entire repo
        'formatFilePathArrForProc(getFilePathsFromModDocArr(modDocArr), sDelimiter:=""" """)
        Dim modDocTemp As ModelDoc2
        Dim sOutputLines() As String
        Dim sOutputErrorLines() As String
        'Dim sLine2 As String
        Dim bSuccess As Boolean = False
        Dim sFilePathCat As String = ""
        Dim sFilePathTemp As String
        Dim iLineStep As Integer = 1
        Dim sModDocPathArr() As String = Nothing
        Dim sFileStartIndex As String
        Dim sCatMessage As String = ""
        Dim statusArguments As String
        Dim bCheckAllFiles As Boolean = False

        Dim statusProcessOutput As rawProcessReturn
        Dim sPropArr(,) As String
        Dim lockOwnersByPath As Dictionary(Of String, String) = Nothing

        'Dim iOutputUbound As Integer
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim k As Integer = 0
        Dim n As Integer = 0
        Dim m As Integer = 0
        Dim bExpectStatusAgainstRevision As Boolean = False
        Dim Index As Integer
        Dim response As Integer

        Dim entireSVNStatus As SVNStatus = New SVNStatus()
        Dim svnStatusOfPassedModDoc As SVNStatus = New SVNStatus()

        Dim sw As New Stopwatch
        'sw.Start()

        'SVNstartInfo.Arguments = "status " & If(bCheckServer, "-u ", "") & "-v --non-interactive E:\SolidworksBackup\svn " 'sFilePathCat 

        If Not verifyLocalRepoPath(, bCheckLocalFolder:=True, bCheckServer = False) Then Return Nothing 'Don't check server because we will in runSVNProcess

        If Not IsNothing(sDirectFilePathArr) Then
            sModDocPathArr = filterExistingCadFilePathsOnly(sDirectFilePathArr)
        ElseIf Not IsNothing(modDocArr) Then
            sModDocPathArr = getFilePathsFromModDocArr(modDocArr)
        End If

        'Speed fix:
        'Only scan the whole working copy when no specific files were supplied.
        'Targeted server status is used by Get Locks and Sync Status so large assemblies do not feel like full-repo scans.
        If IsNothing(sModDocPathArr) OrElse sModDocPathArr.Length = 0 Then bCheckAllFiles = True

        If bCheckAllFiles Then
            'Have to just check the whole file path, because otherwise, svn sends a separate server request for ech individual path sent
            'if you  format it, like ""C:/file1" "C:/file2"" (including the quotes, starting with double start and end) then it will only send one server request, however, the server has trouble finding the file names... 
            statusArguments = "status -v" & If(bCheckServer, "u", "") & " --non-interactive """ & myUserControl.localRepoPath.Text.TrimEnd("\\") & """" 'sFilePathCat 
            sPropArr = svnPropget("""" & myUserControl.localRepoPath.Text.TrimEnd("\\") & """")
        Else

            'Safety fix:
            'When checking targeted files against the server, keep the -u flag.
            'Without -u, normal Get Locks could miss the remote "*" out-of-date marker
            'and accidentally allow a user to lock/edit stale geometry.
            statusArguments = "status -v" & If(bCheckServer, "u", "") & " --non-interactive " & formatFilePathArrForSvnProc(sModDocPathArr) 'sFilePathCat 
            sPropArr = svnPropget(formatFilePathArrForSvnProc(sModDocPathArr))
        End If


        'iSwApp.SendMsgToUser(sSVNPath)
        statusProcessOutput = runSvnProcess(sSVNPath, statusArguments)
        If bCheckServer Then
            lockOwnersByPath = New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)

            'Speed fix:
            'Do one batched XML lock-owner query for targeted file lists instead of
            'spawning one svn.exe process per file. This keeps the safety check but
            'removes most of the delay from normal Get Locks / Sync Status.
            If sModDocPathArr IsNot Nothing AndAlso sModDocPathArr.Length > 0 Then
                lockOwnersByPath = getSvnLockOwnersForFilePaths(sModDocPathArr)
            Else
                lockOwnersByPath = getSvnLockOwnersByPath(myUserControl.localRepoPath.Text.TrimEnd("\"c))
            End If
        Else
            lockOwnersByPath = New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)
        End If

        sOutputLines = statusProcessOutput.output.Split(ControlChars.CrLf.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        sOutputErrorLines = statusProcessOutput.outputError.Split(ControlChars.CrLf.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)

        k = sOutputErrorLines.Length - 1
        'sOutputErrorLines = {""}


        entireSVNStatus = svnStatusOfPassedModDoc ' Be careful! This does not copy. This makes both point to the same memory! We will split/copy them later if theres no errors.
        'ReDim output.fp(UBound(sOutputLines))
        ReDim svnStatusOfPassedModDoc.fp(sOutputLines.Length - 1)

        'Error Checking
        If (sOutputErrorLines Is Nothing) Or (sOutputLines Is Nothing) Then
            iSwApp.SendMsgToUser("Error: SVN status output standard error is nothing. Must have not connected/read to SVN process")
            Return Nothing
        End If

        If sOutputErrorLines.Length <> 0 Then
            'We got some errors if length > 0
            For i = 0 To UBound(sOutputErrorLines)
                If sOutputErrorLines(i).Contains("E215004") Then
                    'Log in Failed!
                    If iRecursiveLevel <= 1 Then
                        Return Nothing
                    End If
                    'Open a log in, and then try again. 
                    iSwApp.SendMsgToUser(svnAddInUtils.catWithNewLine(sOutputErrorLines))

                    'https://tortoisesvn.net/docs/nightly/TortoiseSVN_en/tsvn-automation.html
                    runTortoiseProcexeWithMonitor("/command:repostatus /remote /path: """ & myUserControl.localRepoPath.Text & """") 'log in
                    Return getFileSVNStatus(bCheckServer, modDocArr, bUpdateStatusOfAllOpenModels, iRecursiveLevel:=(iRecursiveLevel + 1), sDirectFilePathArr:=sDirectFilePathArr)
                ElseIf sOutputErrorLines(i).Contains("E170013") Then
                    'Couldn't connect. Server is off or no internet connection
                    If iSwApp.SendMsgToUser2("SVN timed out while attempting to connect to the vault. " &
                      "Would you like to switch to offline? " & vbCrLf & vbCrLf & "Error Message Below" &
                      catWithNewLine(sOutputErrorLines),
                      swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbYesNo) = swMessageBoxResult_e.swMbHitYes Then
                        switchToOffline()
                    End If
                    Return Nothing
                ElseIf sOutputErrorLines(i).Contains("W155007:") Then
                    'Common error. File not saved into repository. Or folder is not connected to a repository.
                    sCatMessage &= vbCrLf &
                        sOutputErrorLines(i) & vbCrLf &
                        "Error W155007 the path is not associated with a repository. " &
                        "You may need to either checkout the repository to the folder with tortoiseSVN, " &
                        "or save the file inside an existing local repository And try again. "
                ElseIf statusProcessOutput.outputError.Contains("W155007:") Then

                    response = iSwApp.SendMsgToUser2("The files are not connected to an SVN Repository. " &
                                            "Would you like to select a new folder? " & vbCrLf &
                                            "Otherwise, Please use tortoiseSVN in Windows Explorer to CHECKOUT the repository, or ADD the files to the repository, and try again.",
                                            swMessageBoxIcon_e.swMbWarning,
                                            swMessageBoxBtn_e.swMbYesNo)
                    If response = swMessageBoxResult_e.swMbHitYes Then
                        If (myUserControl.pickFolder() = System.Windows.Forms.DialogResult.OK) Then
                            Return getFileSVNStatus(bCheckServer, modDocArr, bUpdateStatusOfAllOpenModels, iRecursiveLevel:=(iRecursiveLevel + 1), sDirectFilePathArr:=sDirectFilePathArr)
                        Else
                            Return Nothing
                        End If
                    ElseIf response = swMessageBoxResult_e.swMbHitNo Then
                        iSwApp.SendMsgToUser2("Please switch to offline with the checkbox under the folder.", swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)
                        Return Nothing
                    Else
                    End If
                Else
                    'Other Errors
                    sCatMessage &= vbCrLf &
                        sOutputErrorLines(i) & vbCrLf &
                        "Error: " & sOutputErrorLines(i)
                End If
            Next i
        End If

        If sOutputLines.Length = 0 Then
            If sCatMessage <> "" Then
                iSwApp.SendMsgToUser(sCatMessage)
                'Unknown other error. Continue running svnstatus function.
            Else
                iSwApp.SendMsgToUser(sCatMessage & vbCrLf & "Error: No Usable output lines returned from SVN. " &
                    "Possible Reasons: No connection to server.")
            End If
            Return Nothing
        End If

        If (bCheckServer) Then
            If sOutputLines(0).Length >= 23 AndAlso sOutputLines(0).Substring(0, 23) = "Status against revision" Then
                iSwApp.SendMsgToUser("Status Returned from SVN Server with No Items") 'If you change the string, change it other places in the code too!
                Return svnStatusOfPassedModDoc
            ElseIf (sOutputLines.Length = 1) Then
                'Targeted svn status -u can legitimately return one usable status row.
                'Example after rename/add: A       ?       C:\SVN test\part.SLDPRT
                'That is not an incomplete response; continue and parse it normally.
                If Not isUsableSvnStatusOutputLine(sOutputLines(0)) Then
                    iSwApp.SendMsgToUser("Error: Incomplete SVN Status. Could not Read Line 2. Line 1:" & sOutputLines(0))
                    Return svnStatusOfPassedModDoc
                End If
            End If
        End If

        ReDim svnStatusOfPassedModDoc.fp(UBound(sOutputLines))
        entireSVNStatus = svnStatusOfPassedModDoc.Clone

        For i = 0 To UBound(sOutputLines)
            Try
                If sOutputLines(i).Length >= 23 Then
                    If sOutputLines(i).Substring(0, 23) = "Status against revision" Then Continue For
                End If
            Catch e As Exception
                Continue For
            End Try

            If sOutputLines(i).Contains("~$") Then Continue For 'Temporary file!
            sFileStartIndex = Strings.InStr(sOutputLines(i), myUserControl.localRepoPath.Text, CompareMethod.Text) - 1
            If sFileStartIndex = -2 Then Continue For
            If sFileStartIndex = -1 Then Continue For
            sFilePathTemp = sOutputLines(i).Substring(sFileStartIndex, sOutputLines(i).Length - sFileStartIndex)

            modDocTemp = iSwApp.GetOpenDocumentByName(sFilePathTemp)

            'Important:
            'Do NOT skip files just because SolidWorks does not have a ModelDoc2 for them.
            'Suppressed/lightweight/path-only components can still have valid SVN paths.
            entireSVNStatus.addOutputLineToSVNStatus(sOutputLines(i), m, sFilePathTemp, modDocTemp, bCheckServer, vLookup(sFilePathTemp.Replace("\", "/"), sPropArr, 1))

            Dim lockOwnerTemp As String = ""
            If lockOwnersByPath IsNot Nothing Then
                lockOwnersByPath.TryGetValue(normalizeSvnPath(sFilePathTemp), lockOwnerTemp)
            End If

            entireSVNStatus.fp(m).lockOwner = lockOwnerTemp

            m = m + 1

            If Not IsNothing(sModDocPathArr) Then
                Index = svnAddInUtils.findIndexContains(sModDocPathArr, sFilePathTemp)
                If Index = -1 Then Continue For
                svnStatusOfPassedModDoc.addOutputLineToSVNStatus(sOutputLines(i), j, sFilePathTemp, modDocTemp, bCheckServer, vLookup(sFilePathTemp.Replace("\", "/"), sPropArr, returnColumn:=1))

                Dim lockOwnerTemp2 As String = ""
                If lockOwnersByPath IsNot Nothing Then
                    lockOwnersByPath.TryGetValue(normalizeSvnPath(sFilePathTemp), lockOwnerTemp2)
                End If

                svnStatusOfPassedModDoc.fp(j).lockOwner = lockOwnerTemp2

                j += 1
            End If
        Next i

        If j > 0 Then ReDim Preserve svnStatusOfPassedModDoc.fp(j - 1)
        If m > 0 Then ReDim Preserve entireSVNStatus.fp(m - 1)

        'sw.Stop()
        'Debug.WriteLine("getFileSVNStatus Time Taken: " + sw.Elapsed.TotalMilliseconds.ToString("#,##0.00 'milliseconds'"))

        If bUpdateStatusOfAllOpenModels Then
            statusOfAllOpenModels = entireSVNStatus.Clone
            rebuildStatusCacheFromStatus(statusOfAllOpenModels, markAsServerSync:=False)
        End If

        If IsNothing(modDocArr) Then
            'iSwApp.SendMsgToUser("Unknown error attempting to retrieve SVN Status from server")
            Return entireSVNStatus
        Else
            Return svnStatusOfPassedModDoc
        End If

    End Function

    Public Function syncServerStatusForFilePaths(ByVal sFilePathArr() As String) As Boolean
        If myUserControl Is Nothing Then Return False
        If iSwApp Is Nothing Then Return False
        If sFilePathArr Is Nothing Then Return False
        If sFilePathArr.Length = 0 Then Return False
        If Not isOnlineModeEnabled() Then Return False

        Dim filteredPaths() As String = filterExistingCadFilePathsOnly(sFilePathArr)

        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then Return False

        Try
            Dim serverStatus As SVNStatus = getFileSVNStatus(
                bCheckServer:=True,
                modDocArr:=Nothing,
                bUpdateStatusOfAllOpenModels:=False,
                sDirectFilePathArr:=filteredPaths
            )

            If serverStatus Is Nothing Then Return False

            statusOfAllOpenModels = serverStatus.Clone
            rebuildStatusCacheFromStatus(statusOfAllOpenModels, markAsServerSync:=True)

            Try
                myUserControl.statusOfAllOpenModels = statusOfAllOpenModels
            Catch
            End Try

            Return True

        Catch
            Return False
        End Try
    End Function

    Public Function getServerStatusForFilePathsBackgroundPublic(ByVal sFilePathArr() As String,
                                                               ByVal savedPathForBackground As String,
                                                               ByRef errorMessage As String,
                                                               Optional ByRef timingLog As String = "") As SVNStatus
        errorMessage = ""
        timingLog = ""

        If sFilePathArr Is Nothing OrElse sFilePathArr.Length = 0 Then
            errorMessage = "No file paths were supplied for Sync Status."
            Return Nothing
        End If

        Dim filteredPaths() As String = filterExistingCadFilePathsOnly(sFilePathArr)

        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then
            errorMessage = "No valid CAD file paths were supplied for Sync Status."
            Return Nothing
        End If

        Dim overallWatch As Stopwatch = Stopwatch.StartNew()
        Dim timingNotes As New List(Of String)()

        Try
            Dim allEntries As New List(Of SVNStatus.filePpty)()

            'Optimization:
            'Do not ask SVN for 35+ files in one huge serial call and do not fetch lock owners for every file.
            'Split into smaller chunks, run chunks in parallel, and only run the expensive lock-owner XML check
            'for files that the first status call says are actually locked by someone else.
            Dim chunks As List(Of String()) = chunkFilePathsForBackground(filteredPaths, 12)
            Dim maxParallelChunks As Integer = Math.Min(4, Math.Max(1, chunks.Count))
            Dim parallelGate As New System.Threading.SemaphoreSlim(maxParallelChunks)
            Dim tasks As New List(Of Task(Of SyncStatusChunkResult))()
            Dim chunkNumber As Integer = 0

            timingNotes.Add("Optimized Sync Status path")
            timingNotes.Add("Files checked: " & filteredPaths.Length.ToString())
            timingNotes.Add("Chunk size: 12")
            timingNotes.Add("Chunks: " & chunks.Count.ToString())
            timingNotes.Add("Max parallel chunks: " & maxParallelChunks.ToString())

            For Each chunk As String() In chunks
                chunkNumber += 1
                Dim chunkForTask As String() = CType(chunk.Clone(), String())
                Dim chunkIndexForTask As Integer = chunkNumber

                tasks.Add(Task.Run(Function()
                                       parallelGate.Wait()
                                       Try
                                           Return getServerStatusChunkOptimized(chunkForTask, savedPathForBackground, chunkIndexForTask)
                                       Finally
                                           Try
                                               parallelGate.Release()
                                           Catch
                                           End Try
                                       End Try
                                   End Function))
            Next

            Try
                Task.WaitAll(tasks.ToArray())
            Catch ex As Exception
                errorMessage = ex.Message
                timingNotes.Add("Parallel chunk wait failed: " & ex.Message)
                timingLog = String.Join(vbCrLf, timingNotes.ToArray())
                Return Nothing
            Finally
                Try
                    parallelGate.Dispose()
                Catch
                End Try
            End Try

            For Each taskResult As Task(Of SyncStatusChunkResult) In tasks
                Dim chunkResult As SyncStatusChunkResult = Nothing

                Try
                    chunkResult = taskResult.Result
                Catch ex As Exception
                    errorMessage = ex.Message
                    Continue For
                End Try

                If chunkResult Is Nothing Then Continue For

                If Not String.IsNullOrWhiteSpace(chunkResult.TimingLog) Then
                    timingNotes.Add(chunkResult.TimingLog)
                End If

                If Not String.IsNullOrWhiteSpace(chunkResult.ErrorMessage) Then
                    errorMessage = chunkResult.ErrorMessage
                    timingLog = String.Join(vbCrLf, timingNotes.ToArray())
                    Return Nothing
                End If

                If chunkResult.Entries IsNot Nothing AndAlso chunkResult.Entries.Count > 0 Then
                    allEntries.AddRange(chunkResult.Entries)
                End If
            Next

            Dim serverStatus As New SVNStatus()

            If allEntries.Count = 0 Then
                serverStatus.fp = Nothing
            Else
                serverStatus.fp = allEntries.ToArray()
            End If

            timingNotes.Add("Total optimized background status: " & overallWatch.ElapsedMilliseconds.ToString() & " ms")
            timingLog = String.Join(vbCrLf, timingNotes.ToArray())

            Return serverStatus

        Catch ex As Exception
            errorMessage = ex.Message
            Try
                timingNotes.Add("Total optimized background status before error: " & overallWatch.ElapsedMilliseconds.ToString() & " ms")
                timingLog = String.Join(vbCrLf, timingNotes.ToArray())
            Catch
            End Try
            Return Nothing
        End Try
    End Function

    Public Function getQuietActiveDocumentServerStatusBackgroundPublic(ByVal filePath As String,
                                                                        ByVal savedPathForBackground As String,
                                                                        ByRef errorMessage As String) As SVNStatus
        errorMessage = ""

        Dim filteredPaths() As String = filterExistingCadFilePathsOnly(New String() {filePath})
        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then Return Nothing

        Dim targetPath As String = filteredPaths(0)

        Try
            'Exactly one server request and no lock-owner/release-property follow-up calls.
            'Column 6 is enough for edit protection: K is ours, while O/T/B means the lock is
            'owned elsewhere, stolen, or broken. This runs only on a background worker.
            Dim args As String = "status -vu --non-interactive " &
                quoteFilePathArgs(New String() {targetPath})
            Dim statusResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
                sSVNPath,
                args,
                savedPathForBackground,
                15000
            )

            Dim errorText As String = ""
            If statusResult.outputError IsNot Nothing Then errorText = statusResult.outputError.Trim()
            If errorText <> "" Then
                errorMessage = errorText
                Return Nothing
            End If

            If String.IsNullOrWhiteSpace(statusResult.output) Then Return Nothing

            Dim lines() As String = statusResult.output.Split(
                New String() {vbCrLf, vbLf},
                StringSplitOptions.RemoveEmptyEntries
            )

            For Each line As String In lines
                If String.IsNullOrWhiteSpace(line) Then Continue For
                If line.StartsWith("Status against revision", StringComparison.OrdinalIgnoreCase) Then Continue For

                Dim matchedPath As String = findMatchingTargetPathInStatusLine(
                    line,
                    New String() {targetPath}
                )
                If String.IsNullOrWhiteSpace(matchedPath) Then Continue For

                Dim entry As New SVNStatus.filePpty()
                entry.filename = matchedPath
                entry.modDoc = Nothing
                entry.bReconnect = False
                entry.revertUpdate = getLatestType.none
                entry.addDelChg1 = getStatusColumn(line, 0)
                entry.pptyMods2 = getStatusColumn(line, 1)
                entry.workingDirLock3 = getStatusColumn(line, 2)
                entry.addWithHist4 = getStatusColumn(line, 3)
                entry.switchWParent5 = getStatusColumn(line, 4)
                entry.lock6 = getStatusColumn(line, 5)
                entry.lockOwner = ""
                entry.tree7 = getStatusColumn(line, 6)
                entry.upToDate9 = getStatusColumn(line, 8)
                entry.released = ""

                Dim output As New SVNStatus()
                ReDim output.fp(0)
                output.fp(0) = entry
                Return output
            Next

            Return Nothing
        Catch ex As Exception
            errorMessage = ex.Message
            Return Nothing
        End Try
    End Function

    Private Function getServerStatusChunkOptimized(ByVal chunk() As String,
                                                   ByVal savedPathForBackground As String,
                                                   ByVal chunkIndex As Integer) As SyncStatusChunkResult
        Dim result As New SyncStatusChunkResult()
        Dim chunkWatch As Stopwatch = Stopwatch.StartNew()
        Dim phaseStartMs As Long = 0
        Dim statusMs As Long = 0
        Dim ownerMs As Long = 0
        Dim releaseMs As Long = 0
        Dim parseMs As Long = 0

        Try
            If chunk Is Nothing OrElse chunk.Length = 0 Then Return result

            phaseStartMs = chunkWatch.ElapsedMilliseconds

            Dim args As String = "status -vu --non-interactive " & quoteFilePathArgs(chunk)
            Dim statusResult As rawProcessReturn = runSvnProcessBackgroundNoUi(sSVNPath, args, savedPathForBackground)

            statusMs = chunkWatch.ElapsedMilliseconds - phaseStartMs

            If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
                result.ErrorMessage = statusResult.outputError.Trim()
                result.TimingLog = "Chunk " & chunkIndex.ToString() & " failed during status -vu after " & statusMs.ToString() & " ms"
                Return result
            End If

            If statusResult.output Is Nothing Then
                result.TimingLog = "Chunk " & chunkIndex.ToString() & " returned no status output after " & statusMs.ToString() & " ms"
                Return result
            End If

            phaseStartMs = chunkWatch.ElapsedMilliseconds

            Dim parsedEntries As New List(Of SVNStatus.filePpty)()
            Dim pathsNeedingOwner As New List(Of String)()
            Dim releaseCandidatePaths As New List(Of String)()

            Dim lines() As String = statusResult.output.Split({vbCrLf, vbLf}, StringSplitOptions.RemoveEmptyEntries)

            For Each line As String In lines
                If String.IsNullOrWhiteSpace(line) Then Continue For
                If line.StartsWith("Status against revision", StringComparison.OrdinalIgnoreCase) Then Continue For

                Dim matchedPath As String = findMatchingTargetPathInStatusLine(line, chunk)
                If String.IsNullOrWhiteSpace(matchedPath) Then Continue For

                Dim fp As New SVNStatus.filePpty()
                fp.filename = matchedPath
                fp.modDoc = Nothing
                fp.bReconnect = False
                fp.revertUpdate = getLatestType.none
                fp.addDelChg1 = getStatusColumn(line, 0)
                fp.pptyMods2 = getStatusColumn(line, 1)
                fp.workingDirLock3 = getStatusColumn(line, 2)
                fp.addWithHist4 = getStatusColumn(line, 3)
                fp.switchWParent5 = getStatusColumn(line, 4)
                fp.lock6 = getStatusColumn(line, 5)
                fp.tree7 = getStatusColumn(line, 6)
                fp.upToDate9 = getStatusColumn(line, 8)
                fp.lockOwner = ""
                fp.released = ""

                'Optimization from the older fast build:
                'Only ask SVN for the remote lock owner when the status row says there is a remote lock.
                'For clean/unlocked files and files locked by you, owner lookup is wasted server work.
                If Not String.IsNullOrWhiteSpace(fp.lock6) AndAlso fp.lock6 <> " " AndAlso fp.lock6 <> "K" Then
                    pathsNeedingOwner.Add(matchedPath)
                End If

                'Release state only matters after out-of-date/local-change/lock states have been ruled out.
                'This keeps release propget targeted instead of hitting every possible status row.
                If fp.addDelChg1 = " " AndAlso fp.lock6 = " " AndAlso fp.upToDate9 <> "*" Then
                    releaseCandidatePaths.Add(matchedPath)
                End If

                parsedEntries.Add(fp)
            Next

            parseMs = chunkWatch.ElapsedMilliseconds - phaseStartMs

            Dim ownersByPath As Dictionary(Of String, String) = Nothing
            Dim releaseByPath As Dictionary(Of String, String) = Nothing

            If pathsNeedingOwner.Count > 0 Then
                phaseStartMs = chunkWatch.ElapsedMilliseconds
                ownersByPath = getSvnLockOwnersForPathsBackground(pathsNeedingOwner.ToArray(), savedPathForBackground)
                ownerMs = chunkWatch.ElapsedMilliseconds - phaseStartMs
            Else
                ownersByPath = New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)
                ownerMs = 0
            End If

            If releaseCandidatePaths.Count > 0 Then
                phaseStartMs = chunkWatch.ElapsedMilliseconds
                releaseByPath = getReleasePropertiesForPathsBackground(releaseCandidatePaths.ToArray(), savedPathForBackground)
                releaseMs = chunkWatch.ElapsedMilliseconds - phaseStartMs
            Else
                releaseByPath = New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)
                releaseMs = 0
            End If

            For i As Integer = 0 To parsedEntries.Count - 1
                Dim fp As SVNStatus.filePpty = parsedEntries(i)
                Dim normalizedPath As String = normalizeSvnPath(fp.filename)

                If ownersByPath IsNot Nothing Then
                    Dim owner As String = ""
                    If ownersByPath.TryGetValue(normalizedPath, owner) Then
                        fp.lockOwner = owner
                    End If
                End If

                If releaseByPath IsNot Nothing Then
                    Dim releaseState As String = ""
                    If releaseByPath.TryGetValue(normalizedPath, releaseState) Then
                        fp.released = releaseState
                    End If
                End If

                result.Entries.Add(fp)
            Next

            result.TimingLog =
                "Chunk " & chunkIndex.ToString() & " (" & chunk.Length.ToString() & " files): " &
                "status -vu " & statusMs.ToString() & " ms; " &
                "parse " & parseMs.ToString() & " ms; " &
                "owner check " & If(pathsNeedingOwner.Count > 0, ownerMs.ToString() & " ms for " & pathsNeedingOwner.Count.ToString() & " locked files", "skipped") & "; " &
                "release propget " & If(releaseCandidatePaths.Count > 0, releaseMs.ToString() & " ms for " & releaseCandidatePaths.Count.ToString() & " candidates", "skipped") & "; " &
                "total " & chunkWatch.ElapsedMilliseconds.ToString() & " ms"

            Return result

        Catch ex As Exception
            result.ErrorMessage = ex.Message
            result.TimingLog = "Chunk " & chunkIndex.ToString() & " failed after " & chunkWatch.ElapsedMilliseconds.ToString() & " ms: " & ex.Message
            Return result
        End Try
    End Function

    Public Sub applyServerStatusFromBackgroundPublic(ByVal serverStatus As SVNStatus)
        If serverStatus Is Nothing Then Exit Sub

        Try
            statusOfAllOpenModels = serverStatus.Clone
            rebuildStatusCacheFromStatus(statusOfAllOpenModels, markAsServerSync:=True)
        Catch
        End Try

        Try
            If myUserControl IsNot Nothing Then
                myUserControl.statusOfAllOpenModels = statusOfAllOpenModels

                'Scoped the same way as updateLockStatusPublic - see getActiveInteractionLockedPaths.
                Dim priorityLockedPaths() As String = getActiveInteractionLockedPaths(statusOfAllOpenModels)
                If priorityLockedPaths IsNot Nothing AndAlso priorityLockedPaths.Length > 0 Then
                    myUserControl.forceWriteAccessForLockedFilePathsPublic(priorityLockedPaths)
                End If
            End If
        Catch
        End Try
    End Sub

    Public Sub applyTargetedServerStatusFromBackgroundPublic(ByVal serverStatus As SVNStatus)
        If serverStatus Is Nothing OrElse serverStatus.fp Is Nothing Then Exit Sub

        'A quiet active-file poll must merge one authoritative row into the bounded cache.
        'Replacing statusOfAllOpenModels here would erase every sibling row and make the task
        'pane appear disconnected until the next full Sync.
        rebuildStatusCacheFromStatus(serverStatus, markAsServerSync:=False)
    End Sub

    Private Function chunkFilePathsForBackground(ByVal filePaths() As String,
                                                 Optional ByVal chunkSize As Integer = 12) As List(Of String())
        Dim chunks As New List(Of String())()

        If filePaths Is Nothing Then Return chunks
        If chunkSize <= 0 Then chunkSize = 12

        Dim current As New List(Of String)()

        For Each filePath As String In filePaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            current.Add(filePath)

            If current.Count >= chunkSize Then
                chunks.Add(current.ToArray())
                current.Clear()
            End If
        Next

        If current.Count > 0 Then chunks.Add(current.ToArray())

        Return chunks
    End Function

    Private Function getStatusColumn(ByVal statusLine As String, ByVal index As Integer) As String
        If statusLine Is Nothing Then Return " "
        If statusLine.Length <= index Then Return " "
        Return statusLine.Substring(index, 1)
    End Function

    Private Function findMatchingTargetPathInStatusLine(ByVal statusLine As String,
                                                        ByVal targetPaths() As String) As String
        If String.IsNullOrWhiteSpace(statusLine) Then Return ""
        If targetPaths Is Nothing Then Return ""

        Dim orderedPaths = targetPaths.
            Where(Function(p) Not String.IsNullOrWhiteSpace(p)).
            OrderByDescending(Function(p) p.Length)

        For Each targetPath As String In orderedPaths
            If statusLine.IndexOf(targetPath, StringComparison.OrdinalIgnoreCase) >= 0 Then
                Return targetPath
            End If
        Next

        Return ""
    End Function

    Private Function getSvnLockOwnersForPathsBackground(ByVal filePaths() As String,
                                                        ByVal savedPathForBackground As String) As Dictionary(Of String, String)
        Dim lockOwners As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Return lockOwners

        Try
            Dim xmlStatus As rawProcessReturn = runSvnProcessBackgroundNoUi(
                sSVNPath,
                "status -u --xml --non-interactive " & quoteFilePathArgs(filePaths),
                savedPathForBackground
            )

            If xmlStatus.output Is Nothing Then Return lockOwners
            If xmlStatus.outputError IsNot Nothing AndAlso xmlStatus.outputError.Trim() <> "" Then Return lockOwners
            If xmlStatus.output.Trim() = "" Then Return lockOwners

            Dim doc As New XmlDocument()
            doc.LoadXml(xmlStatus.output)

            Dim entries As XmlNodeList = doc.SelectNodes("/status/target/entry")

            For Each entry As XmlNode In entries
                If entry.Attributes Is Nothing Then Continue For
                If entry.Attributes("path") Is Nothing Then Continue For

                Dim entryPath As String = entry.Attributes("path").Value
                Dim ownerNode As XmlNode = entry.SelectSingleNode("repos-status/lock/owner")

                If ownerNode Is Nothing Then
                    ownerNode = entry.SelectSingleNode("wc-status/lock/owner")
                End If

                If ownerNode Is Nothing Then Continue For

                Dim owner As String = ownerNode.InnerText.Trim()
                If owner = "" Then Continue For

                lockOwners(normalizeSvnPath(entryPath)) = owner
            Next
        Catch
        End Try

        Return lockOwners
    End Function

    Private Function getReleasePropertiesForPathsBackground(ByVal filePaths() As String,
                                                            ByVal savedPathForBackground As String) As Dictionary(Of String, String)
        Dim releaseByPath As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Return releaseByPath

        Try
            Dim propResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
                sSVNPath,
                "propget addin:release_state --xml " & quoteFilePathArgs(filePaths),
                savedPathForBackground
            )

            If propResult.output Is Nothing Then Return releaseByPath
            If propResult.outputError IsNot Nothing AndAlso propResult.outputError.Trim() <> "" Then Return releaseByPath
            If propResult.output.Trim() = "" Then Return releaseByPath

            Dim doc As New XmlDocument()
            doc.LoadXml(propResult.output)

            Dim targets As XmlNodeList = doc.SelectNodes("/properties/target")

            For Each target As XmlNode In targets
                If target.Attributes Is Nothing Then Continue For
                If target.Attributes("path") Is Nothing Then Continue For

                Dim targetPath As String = target.Attributes("path").Value
                Dim propertyNode As XmlNode = target.SelectSingleNode("property")

                If propertyNode Is Nothing Then Continue For

                releaseByPath(normalizeSvnPath(targetPath)) = propertyNode.InnerText.Trim()
            Next
        Catch
        End Try

        Return releaseByPath
    End Function

    Function verifyCommandArgumentLength(input As String, Optional bVerbose As Boolean = False) As Boolean
        If input Is Nothing Then Return False
        If input.Length > (32768 - 1) Then
            If bVerbose = True Then
                iSwApp.SendMsgToUser2("Error: Too many arguments sent from the Add-In to TortoiseSVN, " +
                                  "likely caused by doing an action to too many components." +
                                  "You can do the action using TortoiseSVN in Windows Explorer," +
                                  "then back in the Add-in hit the Refresh command.",
                                    swMessageBoxIcon_e.swMbStop, swMessageBoxBtn_e.swMbOk)
            End If

            Return False 'Avoids error. https://stackoverflow.com/questions/9115279/commandline-argument-parameter-limitation

        Else
            Return True
        End If

    End Function
    Function runSvnProcess(filename As String, arguments As String) As rawProcessReturn

        Dim iWaitTime As Integer = 10000 'milliseconds to wait for the SVN process to finish

        Dim output As New rawProcessReturn()

        'Validate before changing UI state. Early returns here must not strand the wait cursor.
        If arguments Is Nothing Then Return output
        If Not verifyCommandArgumentLength(arguments) Then Return output

        System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.WaitCursor

        'Using guarantees Dispose() on every exit path (early Return, exception, normal
        'completion). Each Process wraps native handles plus, once redirected+read
        'asynchronously, anonymous pipe handles - leaving those undisposed across the
        'very large number of svn.exe calls this plugin makes in a session is a real,
        'gradual handle/memory leak, not just a cosmetic one.
        Try
            Using oSVNProcess As New Process()
                Dim SVNstartInfo As New ProcessStartInfo
                SVNstartInfo.Arguments = arguments
                SVNstartInfo.FileName = filename
                SVNstartInfo.UseShellExecute = False
                SVNstartInfo.RedirectStandardOutput = True
                SVNstartInfo.RedirectStandardError = True
                SVNstartInfo.CreateNoWindow = True
                SVNstartInfo.EnvironmentVariables.Remove("SVN_SSH") 'Fixes issue #47: SolidWorks Simulation breaking svn+ssh, so unable to contact repo
                SVNstartInfo.EnvironmentVariables("PATH") = myUserControl.savedPATH 'Fixes issue #47: SolidWorks Simulation breaking svn+ssh, so unable to contact repo

                oSVNProcess.StartInfo = SVNstartInfo

                'Read stdout/stderr asynchronously instead of blocking on ReadToEnd() before
                'WaitForExit(). A synchronous ReadToEnd() here can never return if svn.exe hangs
                '(a stalled network call, a stuck working-copy lock, etc.), which silently defeats
                'the timeout/kill loop below and can freeze SOLIDWORKS. Reading on the process's own
                'callback thread lets the timeout loop actually detect and kill a truly stuck process.
                Dim outputBuilder As New System.Text.StringBuilder()
                Dim errorBuilder As New System.Text.StringBuilder()

                AddHandler oSVNProcess.OutputDataReceived, Sub(sender As Object, e As DataReceivedEventArgs)
                                                               If e.Data IsNot Nothing Then outputBuilder.AppendLine(e.Data)
                                                           End Sub
                AddHandler oSVNProcess.ErrorDataReceived, Sub(sender As Object, e As DataReceivedEventArgs)
                                                              If e.Data IsNot Nothing Then errorBuilder.AppendLine(e.Data)
                                                          End Sub

                oSVNProcess.Start()
                oSVNProcess.BeginOutputReadLine()
                oSVNProcess.BeginErrorReadLine()

                Do While Not oSVNProcess.WaitForExit(iWaitTime)
                    'Do not kill the process before asking whether to keep waiting. Killing first made
                    'the "Yes, give it more time" option impossible and returned partial/empty output.
                    If iSwApp.SendMsgToUser2("SVN is taking longer than expected while connecting to the vault." & vbCrLf & vbCrLf &
                                      "Would you like to keep waiting?",
                                      swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbYesNo) = swMessageBoxResult_e.swMbHitNo Then
                        Try
                            oSVNProcess.Kill()
                            oSVNProcess.WaitForExit(5000)
                        Catch
                        End Try

                        iSwApp.SendMsgToUser("Switching to offline mode")
                        switchToOffline()
                        Return output
                    Else
                        'Wait in slightly larger intervals after each explicit user approval.
                        iWaitTime += 5000
                    End If
                Loop

                'A parameterless WaitForExit after asynchronous reads guarantees the final
                'OutputDataReceived/ErrorDataReceived callbacks have completed before parsing.
                oSVNProcess.WaitForExit()
                output.output = outputBuilder.ToString()
                output.outputError = errorBuilder.ToString()

            End Using
        Finally
            'Process.Start, event setup, and SVN itself can all throw. Always restore the cursor.
            System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Default
        End Try

        Return output
    End Function

    Private Function runSvnProcessBackgroundNoUi(ByVal filename As String,
                                                 ByVal arguments As String,
                                                 ByVal savedPathForBackground As String,
                                                 Optional ByVal timeoutMilliseconds As Integer = 120000) As rawProcessReturn
        Dim output As New rawProcessReturn()

        Try
            If String.IsNullOrWhiteSpace(filename) Then
                output.output = ""
                output.outputError = "SVN executable path is blank."
                Return output
            End If

            If arguments Is Nothing Then
                output.output = ""
                output.outputError = "SVN arguments are blank."
                Return output
            End If

            If Not verifyCommandArgumentLength(arguments) Then
                output.output = ""
                output.outputError = "SVN command was too long for Windows command-line limits."
                Return output
            End If

            Using p As New Process()
                Dim startInfo As New ProcessStartInfo()

                startInfo.FileName = filename
                startInfo.Arguments = arguments
                startInfo.UseShellExecute = False
                startInfo.RedirectStandardOutput = True
                startInfo.RedirectStandardError = True
                startInfo.CreateNoWindow = True

                Try
                    startInfo.EnvironmentVariables.Remove("SVN_SSH")
                Catch
                End Try

                Try
                    If Not String.IsNullOrWhiteSpace(savedPathForBackground) Then
                        startInfo.EnvironmentVariables("PATH") = savedPathForBackground
                    End If
                Catch
                End Try

                p.StartInfo = startInfo

                'Read asynchronously so a hung svn.exe (stalled network call, stuck lock, large
                'interleaved stdout/stderr) cannot block forever on a synchronous ReadToEnd()
                'before the WaitForExit timeout below ever gets a chance to kill it.
                Dim outputBuilder As New System.Text.StringBuilder()
                Dim errorBuilder As New System.Text.StringBuilder()

                AddHandler p.OutputDataReceived, Sub(sender As Object, e As DataReceivedEventArgs)
                                                     If e.Data IsNot Nothing Then outputBuilder.AppendLine(e.Data)
                                                 End Sub
                AddHandler p.ErrorDataReceived, Sub(sender As Object, e As DataReceivedEventArgs)
                                                    If e.Data IsNot Nothing Then errorBuilder.AppendLine(e.Data)
                                                End Sub

                p.Start()
                p.BeginOutputReadLine()
                p.BeginErrorReadLine()

                If timeoutMilliseconds < 1000 Then timeoutMilliseconds = 1000

                If Not p.WaitForExit(timeoutMilliseconds) Then
                    Try
                        p.Kill()
                        p.WaitForExit(5000)
                    Catch
                    End Try

                    output.outputError = "SVN command timed out while running in the background."
                    Return output
                End If

                'Flush the final asynchronous output callbacks before consuming the builders.
                p.WaitForExit()
                output.output = outputBuilder.ToString()
                output.outputError = errorBuilder.ToString()

                Return output
            End Using

        Catch ex As Exception
            output.output = ""
            output.outputError = ex.Message
            Return output
        End Try
    End Function
    Public Function editNewRev(modDocArr() As ModelDoc2) As Boolean
        Dim modDocPath As String
        Dim extension As String
        Dim existingRevision, inputRevision As String
        Dim bGotLockArr As Boolean()
        Dim i As Integer = 0
        Dim sFails As String = ""

        modDocArr = userFilePickerFromList(getMatchingDrawingForArray(modDocArr, iSwApp))
        If IsNothing(modDocArr) Then Return False

        getLocksOfDocs(modDocArr, bUseTortoise:=False, sMessage:="#UP REV EDIT#")

        bGotLockArr = ensureUserHasLocks(modDocArr, bRetry:=False)

        For Each modDoc In modDocArr
            If IsNothing(bGotLockArr(i)) Then Continue For
            If bGotLockArr(i) Then
                svnPropset(getFilePathsFromModDocArr({modDoc}), "addin:release_state", "||EDIT||")

                modDocPath = modDoc.GetPathName()
                If String.IsNullOrWhiteSpace(modDocPath) Then Continue For
                extension = Path.GetExtension(modDocPath).ToUpperInvariant()
                If extension = ".SLDPRT" OrElse extension = ".SLDASM" Then
                    existingRevision = GetSolidworksCustomProperty(modDoc, "Revision")
                    inputRevision = InputBox("Enter Revision:", "Revision", existingRevision)
                    SetSolidworksCustomProperty(modDoc, "Revision", inputRevision)
                End If
            Else
                sFails &= Path.GetFileName(modDoc.GetPathName()) & vbCrLf
            End If
            i += 1
        Next

        refreshActiveTreeAfterSvnAction()

        If Not bGotLockArr.All(Function(b) b) Then
            iSwApp.SendMsgToUser("Unable to Get locks on following Files: " & vbCrLf & sFails)
            Return False
        End If

        iSwApp.SendMsgToUser("Moved files from RELEASED to EDIT state, Set Revision, and Got Locks!")
        Return True
    End Function
    Sub myReleaseDoc(modDoc As ModelDoc2)
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Active Document not found") : Exit Sub
        Dim activeModDoc As ModelDoc2 = iSwApp.ActiveDoc
        Dim modelType As Integer = modDoc.GetType()
        Dim componentAndDrawingModDoc() As ModelDoc2
        Dim inputRevision As String = ""
        Dim bSuccess1 As Boolean
        Dim bSuccess2 As Boolean
        Dim bSuccess3() As Boolean

        componentAndDrawingModDoc = getMatchingComponentAndDrawing(modDoc, iSwApp)

        If componentAndDrawingModDoc(0) Is Nothing Then
            If componentAndDrawingModDoc(1) Is Nothing Then iSwApp.SendMsgToUser2("Error. Couldn't detect component and drawing. Exiting", swMessageBoxIcon_e.swMbStop, swMessageBoxBtn_e.swMbOk) : Exit Sub
            If Not (iSwApp.SendMsgToUser2("Part/Assembly not found. Do you want to continue releasing Drawing without its Part/Assembly?", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbYesNoCancel) = swMessageBoxResult_e.swMbHitYes) Then Exit Sub
            If Not ensureUserHasLocks({componentAndDrawingModDoc(1)}).All(Function(b) b) Then iSwApp.SendMsgToUser("Error. Couldn't get locks. Exiting") : Exit Sub

        ElseIf componentAndDrawingModDoc(1) Is Nothing Then
            If Not (iSwApp.SendMsgToUser2("Drawing not found. Do you want to continue releasing Component without its Drawing?", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbYesNoCancel) = swMessageBoxResult_e.swMbHitYes) Then Exit Sub
            If Not ensureUserHasLocks({componentAndDrawingModDoc(0)}).All(Function(b) b) Then iSwApp.SendMsgToUser("Error. Couldn't get locks. Exiting") : Exit Sub
        Else
            If activeModDoc Is Nothing Then iSwApp.SendMsgToUser("Couldn't find an active Doc.") : Exit Sub
            If StrComp(activeModDoc.GetPathName, componentAndDrawingModDoc(1).GetPathName, vbTextCompare) Then
                'Drawing exists, but is not open!
                iSwApp.SendMsgToUser("A drawing was found, but it is not the active document! Try again with the Drawing Active.")
                Exit Sub
            End If

            bSuccess3 = ensureUserHasLocks(componentAndDrawingModDoc)
            If bSuccess3(0) Then
                If bSuccess3(1) Then
                    'All Good
                Else
                    'couldnt get lock for drawing.
                    If Not (iSwApp.SendMsgToUser2("Couldn't get the lock for the Drawing File. Do you want to continue releasing the Part/Assembly without its Drawing?", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbYesNoCancel) = swMessageBoxResult_e.swMbHitYes) Then
                        Exit Sub
                    Else
                        componentAndDrawingModDoc(1) = Nothing
                    End If
                End If
            ElseIf bSuccess3(1) Then
                'couldnt get lock for part/asy, but did get it for drawing
                If Not (iSwApp.SendMsgToUser2("Couldn't get the lock for the Part/Assembly File. Do you want to continue releasing the Drawing without its Part/Assembly?", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbYesNoCancel) = swMessageBoxResult_e.swMbHitYes) Then
                    Exit Sub
                Else
                    componentAndDrawingModDoc(0) = Nothing
                End If
            Else
                iSwApp.SendMsgToUser("Error. Couldn't get locks for either part or drawing. Exiting")
                Exit Sub
            End If
        End If

        If componentAndDrawingModDoc(0) IsNot Nothing Then
            'UPDATE PART / ASY

            Dim existingRevision As String = GetSolidworksCustomProperty(componentAndDrawingModDoc(0), "Revision")
            inputRevision = InputBox("Enter Revision:", "Revision", existingRevision)
            If String.IsNullOrWhiteSpace(inputRevision) Then Exit Sub

            ' Set custom properties
            SetSolidworksCustomProperty(componentAndDrawingModDoc(0), "Revision", inputRevision)
            'SetSolidworksCustomProperty(componentAndDrawingModDoc(0), "State", "Released")

            svnPropset(getFilePathsFromModDocArr({componentAndDrawingModDoc(0)}), "addin:release_state", "||RELEASED||")
            svnPropset(getFilePathsFromModDocArr({componentAndDrawingModDoc(0)}), "addin:approved", """" & System.Environment.UserName & " " & DateTime.Now.ToString() & """") 'This also ensures that the file changes, preventing a bug / bad state where svn doesn't actually commit an unchanged file.
            componentAndDrawingModDoc(0).Rebuild(swRebuildOptions_e.swRebuildAll)

        End If

        If inputRevision = "" Then InputBox("Enter Revision:", "Revision", "")

        If componentAndDrawingModDoc(1) IsNot Nothing Then
            svnPropset(getFilePathsFromModDocArr({componentAndDrawingModDoc(1)}), "addin:release_state", "||RELEASED||")
            svnPropset(getFilePathsFromModDocArr({componentAndDrawingModDoc(1)}), "addin:approved", """" & System.Environment.UserName & " " & DateTime.Now.ToString() & """") 'This also ensures that the file changes, preventing a bug / bad state where svn doesn't actually commit an unchanged file
            componentAndDrawingModDoc(1).Rebuild(swRebuildOptions_e.swRebuildAll)
        End If

        If componentAndDrawingModDoc(0) IsNot Nothing Then
            If svnCommitDocs({componentAndDrawingModDoc(0)}, sCommitMessage:="#RELEASED# Revision: " & inputRevision) Then
                If iSwApp.SendMsgToUser2("Export Step?" & componentAndDrawingModDoc(0).GetTitle, swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNo) = swMessageBoxResult_e.swMbHitYes Then
                    bSuccess1 = createStep(componentAndDrawingModDoc(0), inputRevision)
                Else
                    bSuccess1 = True
                End If

            Else
                'commit failed, so rollback the propset back to edit
                svnPropset(getFilePathsFromModDocArr({componentAndDrawingModDoc(0)}), "addin:release_state", "||EDIT||")
                svnPropset(getFilePathsFromModDocArr({componentAndDrawingModDoc(0)}), "addin:approved", "unknown")
                bSuccess1 = False
                iSwApp.SendMsgToUser2("Failed to Commit " & componentAndDrawingModDoc(0).GetTitle, swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            End If
        End If

        If componentAndDrawingModDoc(1) IsNot Nothing Then
            If svnCommitDocs({componentAndDrawingModDoc(1)}, sCommitMessage:="#RELEASED# Revision: " & inputRevision) Then
                If iSwApp.SendMsgToUser2("Export PDF? " & componentAndDrawingModDoc(1).GetTitle, swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNo) = swMessageBoxResult_e.swMbHitYes Then
                    bSuccess2 = createPDF(componentAndDrawingModDoc(1))
                Else
                    bSuccess2 = True
                End If
            Else
                'commit failed, so rollback the propset back to edit
                svnPropset(getFilePathsFromModDocArr({componentAndDrawingModDoc(1)}), "addin:release_state", "||EDIT||")
                svnPropset(getFilePathsFromModDocArr({componentAndDrawingModDoc(1)}), "addin:approved", "unknown")
                bSuccess2 = False
                iSwApp.SendMsgToUser2("Failed to Commit " & componentAndDrawingModDoc(1).GetTitle, swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            End If
        End If

        Try
            Dim releaseCachePaths As New List(Of String)()

            If componentAndDrawingModDoc(0) IsNot Nothing Then releaseCachePaths.Add(componentAndDrawingModDoc(0).GetPathName())
            If componentAndDrawingModDoc(1) IsNot Nothing Then releaseCachePaths.Add(componentAndDrawingModDoc(1).GetPathName())

            If releaseCachePaths.Count > 0 Then
                If bSuccess1 OrElse bSuccess2 Then
                    updateStatusCacheForKnownPaths(releaseCachePaths.ToArray(), forceAddDelChg1:=" ", forceLock6:=" ", forceUpToDate9:=" ", forceReleased:="||RELEASED||")
                Else
                    updateStatusCacheForKnownPaths(releaseCachePaths.ToArray(), forceReleased:="||EDIT||")
                End If
            End If
        Catch
        End Try

        refreshActiveTreeAfterSvnAction()

        'Message User
        If bSuccess1 Then
            If bSuccess2 Then
                iSwApp.SendMsgToUser2("Release Complete! Committed, and STEP and PDF created.", swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)
            Else
                iSwApp.SendMsgToUser2("Release Complete! Committed, and STEP created.", swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)
            End If
        ElseIf bSuccess2 Then
            iSwApp.SendMsgToUser2("Release Complete! Committed, and PDF created.", swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)
        Else
            iSwApp.SendMsgToUser2("Release Failed.", swMessageBoxIcon_e.swMbStop, swMessageBoxBtn_e.swMbOk)
        End If
    End Sub
    Function createPDF(modDoc As ModelDoc2, Optional sInputRevision As String = "") As Boolean
        ' Save drawing as PDF
        Dim bSuccess As Boolean = False
        Dim errors As Integer = 0
        Dim warnings As Integer = 0
        Dim drawingPath As String = modDoc.GetPathName()
        Dim drawingBaseName As String = System.IO.Path.GetFileNameWithoutExtension(drawingPath)
        Dim drawingDirectory As String = System.IO.Path.GetDirectoryName(drawingPath)
        Dim pdfPath As String = System.IO.Path.Combine(drawingDirectory, drawingBaseName & sInputRevision & ".pdf")

        iSwApp.ActivateDoc3(getTitleClean(modDoc), True, swRebuildOnActivation_e.swRebuildActiveDoc, 0)

        beginInternalSolidWorksSave()
        Try
            bSuccess = modDoc.Extension.SaveAs3(pdfPath,
                                    swSaveAsVersion_e.swSaveAsCurrentVersion,
                                    swSaveAsOptions_e.swSaveAsOptions_Copy,
                                    Nothing, Nothing, errors, warnings)
        Finally
            endInternalSolidWorksSave()
        End Try
        If Not bSuccess Then
            iSwApp.SendMsgToUser2("Error: " & errors & vbCrLf & "Warnings: " & warnings & vbCrLf & "Lookup: swFileSaveError_e or swFileSaveWarning_e", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
        End If
        Return bSuccess
    End Function
    Function createStep(modDoc As ModelDoc2, Optional sInputRevision As String = "") As Boolean
        ' Save as STEP

        Dim modelPath As String = modDoc.GetPathName()
        Dim baseName As String = System.IO.Path.GetFileNameWithoutExtension(modelPath)
        Dim directory As String = System.IO.Path.GetDirectoryName(modelPath)
        Dim stepPath As String = System.IO.Path.Combine(directory, baseName & sInputRevision & ".step")
        Dim componentDoc As ModelDoc2
        Dim bSuccess As Boolean = False
        Dim errors As Integer = 0
        Dim warnings As Integer = 0

        iSwApp.ActivateDoc3(getTitleClean(modDoc), True, swRebuildOnActivation_e.swRebuildActiveDoc, 0)
        modDoc.ClearSelection2(True)
        componentDoc = iSwApp.ActiveDoc

        beginInternalSolidWorksSave()
        Try
            bSuccess = componentDoc.Extension.SaveAs3(stepPath,
                                           swSaveAsVersion_e.swSaveAsCurrentVersion,
                                           swSaveAsOptions_e.swSaveAsOptions_Copy + swSaveAsOptions_e.swSaveAsOptions_AvoidRebuildOnSave,
                                           Nothing, Nothing, errors, warnings)
        Finally
            endInternalSolidWorksSave()
        End Try
        If Not bSuccess Then
            iSwApp.SendMsgToUser2("Error: " & errors & vbCrLf & "Warnings: " & warnings & vbCrLf & "Lookup: swFileSaveError_e or swFileSaveWarning_e", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
        End If
        Return bSuccess
    End Function
    Function stringArrToSingleStringWithNewLines(inputStrings() As String, Optional bTrimFileNames As Boolean = False, Optional iLimit As Integer = 99999) As String
        Dim myReturnString As String = ""
        Dim i As Integer

        If inputStrings Is Nothing Then Return "< no file list available... feature coming in future versions >"



        For i = 0 To Math.Min(UBound(inputStrings), iLimit)
            If inputStrings(i) Is Nothing Then Continue For

            If bTrimFileNames Then
                myReturnString &= System.IO.Path.GetFileName(inputStrings(i)) & vbCrLf
            Else
                myReturnString &= inputStrings(i) & vbCrLf
            End If
        Next

        If iLimit < UBound(inputStrings) Then
            myReturnString &= "... And " & UBound(inputStrings) - iLimit & " more..."
        End If

        Return myReturnString
    End Function
    Function userAcceptsLossOfChanges(ByRef modDocArr() As ModelDoc2, Optional msg As String = "") As Boolean
        Dim userPickMsg As swMessageBoxResult_e
        userPickMsg = iSwApp.SendMsgToUser2(msg & vbCrLf &
                                            "WARNING: Changes to the selected files will be lost!" & vbCrLf &
                                            stringArrToSingleStringWithNewLines(getFilePathsFromModDocArr(modDocArr), bTrimFileNames:=True, iLimit:=10),
                              Icon:=swMessageBoxIcon_e.swMbWarning, Buttons:=swMessageBoxBtn_e.swMbOkCancel)

        If userPickMsg = swMessageBoxResult_e.swMbHitOk Then
            Return True
        Else
            Return False
        End If
    End Function

    Private Function getResolvedSvnWorkingCopyRootPath() As String
        If myUserControl Is Nothing OrElse myUserControl.localRepoPath Is Nothing Then Return ""

        Dim configuredPath As String = ""

        Try
            configuredPath = Path.GetFullPath(myUserControl.localRepoPath.Text.Trim()).TrimEnd("\"c)
        Catch
            configuredPath = ""
        End Try

        If String.IsNullOrWhiteSpace(configuredPath) Then Return ""

        If String.Equals(configuredPath,
                         cachedConfiguredRepoPathForWorkingCopyRoot,
                         StringComparison.OrdinalIgnoreCase) AndAlso
           Not String.IsNullOrWhiteSpace(cachedResolvedWorkingCopyRoot) Then
            Return cachedResolvedWorkingCopyRoot
        End If

        cachedConfiguredRepoPathForWorkingCopyRoot = configuredPath
        cachedResolvedWorkingCopyRoot = configuredPath

        Try
            If Directory.Exists(configuredPath) Then
                Dim infoResult As rawProcessReturn = runSvnProcess(
                    sSVNPath,
                    "info --show-item wc-root --non-interactive """ & configuredPath & """")

                Dim outputText As String = If(infoResult.output, "").Trim()

                If Not String.IsNullOrWhiteSpace(outputText) Then
                    Dim firstLine As String = outputText.
                        Split({vbCrLf, vbLf}, StringSplitOptions.RemoveEmptyEntries).
                        FirstOrDefault()

                    If Not String.IsNullOrWhiteSpace(firstLine) Then
                        Dim resolvedRoot As String = Path.GetFullPath(firstLine.Trim().Trim(""""c)).TrimEnd("\"c)
                        If Directory.Exists(resolvedRoot) Then cachedResolvedWorkingCopyRoot = resolvedRoot
                    End If
                End If
            End If
        Catch
            'Fallback remains the folder selected in PlumVault.
        End Try

        Return cachedResolvedWorkingCopyRoot
    End Function

    Private Function isPathInsideLocalRepo(filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Try
            Dim repoRoot As String = getResolvedSvnWorkingCopyRootPath()
            Dim fullPath As String = Path.GetFullPath(filePath).TrimEnd("\"c)

            If String.IsNullOrWhiteSpace(repoRoot) Then Return False
            If String.Equals(fullPath, repoRoot, StringComparison.OrdinalIgnoreCase) Then Return True

            Return fullPath.StartsWith(repoRoot & "\", StringComparison.OrdinalIgnoreCase)
        Catch
            Return False
        End Try
    End Function

    Private Function isSolidWorksTempOrVirtualPath(filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Try
            Dim fullPath As String = Path.GetFullPath(filePath)

            If fullPath.IndexOf("\AppData\Local\Temp\", StringComparison.OrdinalIgnoreCase) >= 0 Then Return True
            If fullPath.IndexOf("\swx", StringComparison.OrdinalIgnoreCase) >= 0 AndAlso
           fullPath.IndexOf("\Temp\", StringComparison.OrdinalIgnoreCase) >= 0 Then Return True

            If Path.GetFileName(fullPath).Contains("^") Then Return True

        Catch
            If filePath.IndexOf("\AppData\Local\Temp\", StringComparison.OrdinalIgnoreCase) >= 0 Then Return True
            If filePath.Contains("^") Then Return True
        End Try

        Return False
    End Function

    'SOLIDWORKS keeps an imported neutral-format part unsaved (temp path) with the neutral
    'extension embedded in its name, e.g. "Quaife LSD differential.stp.SLDPRT". These are
    'ordinary vendor components (bearings, differentials, etc.) supplied as STEP/Parasolid/IGES
    'and are not SOLIDWORKS-virtual components. They can be reviewed and copied into SVN like
    'any other external reference as long as the temp file still exists on disk right now.
    Private ReadOnly neutralCadImportExtensions As String() = {
        ".stp", ".step", ".igs", ".iges", ".x_t", ".x_b", ".sat", ".prt", ".catpart", ".catproduct"
    }

    'Strip both the outer SOLIDWORKS extension and, when present, an embedded neutral-format
    'import extension so a proposed SVN name is not left as e.g. "Quaife LSD differential.stp".
    Private Function getNeutralFormatBaseName(ByVal fileName As String) As String
        Dim withoutSwExt As String = Path.GetFileNameWithoutExtension(fileName)

        Try
            Dim innerExt As String = Path.GetExtension(withoutSwExt)

            If Not String.IsNullOrWhiteSpace(innerExt) Then
                For Each candidate As String In neutralCadImportExtensions
                    If String.Equals(innerExt, candidate, StringComparison.OrdinalIgnoreCase) Then
                        Return Path.GetFileNameWithoutExtension(withoutSwExt)
                    End If
                Next
            End If
        Catch
        End Try

        Return withoutSwExt
    End Function

    'A temp-path reference can go through the normal external-reference review/copy pipeline
    'instead of hard-blocking when it is not an unresolved SOLIDWORKS-virtual component
    '(caret-named with no discoverable owner) and its name matches a recognized neutral-format
    'import pattern (STEP/Parasolid/IGES/etc). Do NOT gate this on File.Exists: SOLIDWORKS can
    'report a live, still-open external reference at a temp path that a plain File.Exists check
    'fails to see (short-name scratch folders, a reference SOLIDWORKS is mid-refresh on, etc.),
    'and blocking the whole assembly here on that pre-check is worse than letting the actual
    'copy step (already wrapped in try/catch with a specific per-file error) be the one place
    'that surfaces a genuinely-missing source file.
    Private Function canRouteTempReferenceThroughReview(ByVal oldPath As String) As Boolean
        If String.IsNullOrWhiteSpace(oldPath) Then Return False
        If Path.GetFileName(oldPath).Contains("^") Then Return False

        Dim withoutSwExtension As String = Path.GetFileNameWithoutExtension(oldPath)
        Dim importedExtension As String = Path.GetExtension(withoutSwExtension)

        For Each candidate As String In neutralCadImportExtensions
            If String.Equals(importedExtension, candidate, StringComparison.OrdinalIgnoreCase) Then Return True
        Next

        Return False
    End Function

    'The review form and copy stage use the same source-availability rule. A normal source
    'must exist on disk. A recognized neutral-format temp source may instead be materialized
    'from the exact open SOLIDWORKS document that owns that reported path.
    Public Function canMaterializeExternalReferencePublic(ByVal sourcePath As String) As Boolean
        If String.IsNullOrWhiteSpace(sourcePath) Then Return False
        If File.Exists(sourcePath) Then Return True
        If Not canRouteTempReferenceThroughReview(sourcePath) Then Return False

        Return getOpenModelByPathSafe(sourcePath) IsNot Nothing
    End Function

    Private Function isComponentVirtualSafe(ByVal component As Component2) As Boolean
        If component Is Nothing Then Return False

        Try
            Return component.IsVirtual
        Catch
            Return False
        End Try
    End Function

    Private Function getPhysicalOwnerAssemblyPathForVirtualComponent(ByVal component As Component2,
                                                                     ByVal fallbackAssembly As ModelDoc2) As String
        If component Is Nothing Then Return ""

        Dim currentComponent As Component2 = Nothing

        Try
            currentComponent = component.GetParent()
        Catch
            currentComponent = Nothing
        End Try

        Dim guard As Integer = 0

        While currentComponent IsNot Nothing AndAlso guard < 100
            guard += 1

            If Not isComponentVirtualSafe(currentComponent) Then
                Try
                    Dim currentPath As String = currentComponent.GetPathName()

                    If Not String.IsNullOrWhiteSpace(currentPath) AndAlso
                       String.Equals(Path.GetExtension(currentPath), ".SLDASM", StringComparison.OrdinalIgnoreCase) AndAlso
                       File.Exists(currentPath) Then
                        Return Path.GetFullPath(currentPath)
                    End If
                Catch
                End Try
            End If

            Try
                currentComponent = currentComponent.GetParent()
            Catch
                currentComponent = Nothing
            End Try
        End While

        If fallbackAssembly IsNot Nothing Then
            Try
                If fallbackAssembly.GetType() = swDocumentTypes_e.swDocASSEMBLY Then
                    Dim fallbackPath As String = fallbackAssembly.GetPathName()

                    If Not String.IsNullOrWhiteSpace(fallbackPath) AndAlso File.Exists(fallbackPath) Then
                        Return Path.GetFullPath(fallbackPath)
                    End If
                End If
            Catch
            End Try
        End If

        Return ""
    End Function

    Private Function getOwningPhysicalAssemblyPathForVirtualDocument(ByVal possibleVirtualDocument As ModelDoc2) As String
        If possibleVirtualDocument Is Nothing OrElse iSwApp Is Nothing Then Return ""

        Dim possiblePath As String = ""
        Dim possibleTitle As String = ""

        Try
            possiblePath = possibleVirtualDocument.GetPathName()
        Catch
            possiblePath = ""
        End Try

        Try
            possibleTitle = possibleVirtualDocument.GetTitle()
        Catch
            possibleTitle = ""
        End Try

        'Normal physical CAD should take the fast path. Only temporary/internal
        'document paths need the assembly-component scan below.
        If Not String.IsNullOrWhiteSpace(possiblePath) AndAlso
           File.Exists(possiblePath) AndAlso
           Not isSolidWorksTempOrVirtualPath(possiblePath) Then
            Return ""
        End If

        Dim documentsObject As Object = Nothing

        Try
            documentsObject = iSwApp.GetDocuments()
        Catch
            documentsObject = Nothing
        End Try

        Dim documentsArray As Array = TryCast(documentsObject, Array)
        If documentsArray Is Nothing Then Return ""

        For Each documentObject As Object In documentsArray
            Dim assemblyModel As ModelDoc2 = TryCast(documentObject, ModelDoc2)
            If assemblyModel Is Nothing Then Continue For

            Try
                If assemblyModel.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
            Catch
                Continue For
            End Try

            Dim assemblyDocument As AssemblyDoc = TryCast(assemblyModel, AssemblyDoc)
            If assemblyDocument Is Nothing Then Continue For

            Dim componentsObject As Object = Nothing

            Try
                componentsObject = assemblyDocument.GetComponents(False)
            Catch
                componentsObject = Nothing
            End Try

            Dim componentsArray As Array = TryCast(componentsObject, Array)
            If componentsArray Is Nothing Then Continue For

            For Each componentObject As Object In componentsArray
                Dim component As Component2 = TryCast(componentObject, Component2)
                If component Is Nothing OrElse Not isComponentVirtualSafe(component) Then Continue For

                Dim componentDocument As ModelDoc2 = Nothing
                Dim matches As Boolean = False

                Try
                    componentDocument = TryCast(component.GetModelDoc2(), ModelDoc2)
                    matches = componentDocument IsNot Nothing AndAlso Object.ReferenceEquals(componentDocument, possibleVirtualDocument)
                Catch
                    componentDocument = Nothing
                    matches = False
                End Try

                If Not matches AndAlso componentDocument IsNot Nothing Then
                    Dim componentPath As String = ""
                    Dim componentTitle As String = ""

                    Try
                        componentPath = componentDocument.GetPathName()
                    Catch
                        componentPath = ""
                    End Try

                    Try
                        componentTitle = componentDocument.GetTitle()
                    Catch
                        componentTitle = ""
                    End Try

                    If Not String.IsNullOrWhiteSpace(possiblePath) AndAlso Not String.IsNullOrWhiteSpace(componentPath) Then
                        Try
                            matches = String.Equals(Path.GetFullPath(componentPath), Path.GetFullPath(possiblePath), StringComparison.OrdinalIgnoreCase)
                        Catch
                            matches = String.Equals(componentPath, possiblePath, StringComparison.OrdinalIgnoreCase)
                        End Try
                    ElseIf Not String.IsNullOrWhiteSpace(possibleTitle) AndAlso
                           possibleTitle.Contains("^") AndAlso
                           Not String.IsNullOrWhiteSpace(componentTitle) Then
                        matches = String.Equals(componentTitle, possibleTitle, StringComparison.OrdinalIgnoreCase)
                    End If
                End If

                If matches Then
                    Return getPhysicalOwnerAssemblyPathForVirtualComponent(component, assemblyModel)
                End If
            Next
        Next

        Return ""
    End Function

    Private Function getGrc27RootPath() As String
        Return Path.Combine(getResolvedSvnWorkingCopyRootPath(), "GRC27")
    End Function

    Private Function getVendorPartsRootPath() As String
        Return Path.Combine(getResolvedSvnWorkingCopyRootPath(), "Vendor Parts")
    End Function

    Private Function isPathInsideFolder(filePath As String, folderPath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If String.IsNullOrWhiteSpace(folderPath) Then Return False

        Try
            Dim root As String = Path.GetFullPath(folderPath).TrimEnd("\"c)
            Dim fullPath As String = Path.GetFullPath(filePath).TrimEnd("\"c)

            If String.Equals(fullPath, root, StringComparison.OrdinalIgnoreCase) Then Return True

            Return fullPath.StartsWith(root & "\", StringComparison.OrdinalIgnoreCase)
        Catch
            Return False
        End Try
    End Function

    Private Function pathContainsNamedFolderSegment(ByVal fileOrFolderPath As String,
                                                    ByVal rootFolder As String,
                                                    ByVal requiredFolderName As String) As Boolean
        If String.IsNullOrWhiteSpace(fileOrFolderPath) Then Return False
        If String.IsNullOrWhiteSpace(rootFolder) Then Return False
        If String.IsNullOrWhiteSpace(requiredFolderName) Then Return False

        Try
            Dim root As String = Path.GetFullPath(rootFolder).TrimEnd("\"c, "/"c)
            Dim fullPath As String = Path.GetFullPath(fileOrFolderPath).TrimEnd("\"c, "/"c)

            If Not String.Equals(fullPath, root, StringComparison.OrdinalIgnoreCase) AndAlso
               Not fullPath.StartsWith(root & Path.DirectorySeparatorChar, StringComparison.OrdinalIgnoreCase) Then
                Return False
            End If

            Dim relativePath As String = fullPath.Substring(root.Length).TrimStart("\"c, "/"c)
            If String.IsNullOrWhiteSpace(relativePath) Then Return False

            Dim segments() As String = relativePath.Split(New Char() {"\"c, "/"c}, StringSplitOptions.RemoveEmptyEntries)

            For Each segment As String In segments
                If String.Equals(segment, requiredFolderName, StringComparison.OrdinalIgnoreCase) Then Return True
            Next
        Catch
            Return False
        End Try

        Return False
    End Function

    Private Function isVendorPartPath(filePath As String) As Boolean
        Dim repoRoot As String = getResolvedSvnWorkingCopyRootPath()
        Return pathContainsNamedFolderSegment(filePath, repoRoot, "Vendor Parts")
    End Function

    Private Function isCadFilePath(filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim ext As String = Path.GetExtension(filePath).ToUpperInvariant()

        Return ext = ".SLDPRT" OrElse ext = ".SLDASM" OrElse ext = ".SLDDRW"
    End Function

    Public Function isPathInsideLocalRepoPublic(ByVal filePath As String) As Boolean
        Return isPathInsideLocalRepo(filePath)
    End Function

    Public Function shouldIncludeCadPathInSyncPublic(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not File.Exists(filePath) Then Return False
        If Not isCadFilePath(filePath) Then Return False
        If Not isPathInsideLocalRepo(filePath) Then Return False

        'Use the existing local/cache status when available. Do not launch an SVN command
        'for each tree node; normal Sync must remain fast and non-SVN references are skipped.
        Try
            Dim cached As SVNStatus.filePpty = Nothing

            If tryFindCachedStatusProperty(filePath, cached) Then
                If cached.addDelChg1 = "?" Then Return False
            End If
        Catch
        End Try

        Return True
    End Function

    Private Function isValidGrc27FileName(filePathOrName As String) As Boolean
        If String.IsNullOrWhiteSpace(filePathOrName) Then Return False

        Dim fileName As String = Path.GetFileName(filePathOrName)

        Return System.Text.RegularExpressions.Regex.IsMatch(
        fileName,
        "^(GRC|CFD)27_(BR|DT|AE|FR|EL|ST|SU|WT|MI)_[A-Z]{0,3}\d+_R\d+\.(SLDPRT|SLDASM|SLDDRW)$",
        System.Text.RegularExpressions.RegexOptions.IgnoreCase
    )
    End Function

    Private Function shouldIgnoreGrc27NamingConventionForDebug() As Boolean
        Try
            Return myUserControl IsNot Nothing AndAlso myUserControl.debugIgnoreNamingConventionEnabled()
        Catch
            Return False
        End Try
    End Function

    Private Function promptForValidGrc27FileName(originalPath As String) As String
        Dim ext As String = Path.GetExtension(originalPath)
        Dim originalNameNoExt As String = Path.GetFileNameWithoutExtension(originalPath)

        Do
            Dim inputName As String = InputBox(
            "This file does not follow the GRC27/CFD27 naming convention." & vbCrLf & vbCrLf &
            "Original file:" & vbCrLf &
            Path.GetFileName(originalPath) & vbCrLf & vbCrLf &
            "Required format:" & vbCrLf &
            "PREFIX_CODE_00000_R# or PREFIX_CODE_A0000_R# or PREFIX_CODE_AB0000_R# or PREFIX_CODE_ABC0000_R# (PREFIX = GRC27 or CFD27)" & vbCrLf & vbCrLf &
            "Allowed codes:" & vbCrLf &
            "BR, DT, AE, FR, EL, ST, SU, WT, MI" & vbCrLf & vbCrLf &
            "Enter the new file name without extension:",
            "GRC27/CFD27 File Naming Required",
            originalNameNoExt
        )

            If String.IsNullOrWhiteSpace(inputName) Then Return ""

            inputName = inputName.Trim()

            If Not inputName.EndsWith(ext, StringComparison.OrdinalIgnoreCase) Then
                inputName &= ext
            End If

            If isValidGrc27FileName(inputName) Then
                Return inputName
            End If

            iSwApp.SendMsgToUser2(
            "Invalid file name." & vbCrLf & vbCrLf &
            "Please use this format:" & vbCrLf &
            "PREFIX_CODE_00000_R# or PREFIX_CODE_A0000_R# or PREFIX_CODE_AB0000_R# or PREFIX_CODE_ABC0000_R# (PREFIX = GRC27 or CFD27)" & vbCrLf & vbCrLf &
            "Allowed codes:" & vbCrLf &
            "BR, DT, AE, FR, EL, ST, SU, WT, MI" & vbCrLf & vbCrLf &
            "Example:" & vbCrLf &
            "GRC27_AE_00001_R1" & ext & vbCrLf &
            "CFD27_AE_A0001_R1" & ext & vbCrLf &
            "GRC27_AE_AB0001_R1" & ext & vbCrLf &
            "CFD27_AE_ABC0001_R1" & ext,
            swMessageBoxIcon_e.swMbWarning,
            swMessageBoxBtn_e.swMbOk
)
        Loop
    End Function

    Private Sub addModelDocToCommitArrayIfMissing(ByRef modDocArr() As ModelDoc2, docToAdd As ModelDoc2)
        If docToAdd Is Nothing Then Exit Sub

        Dim docToAddPath As String = ""

        Try
            docToAddPath = docToAdd.GetPathName()
        Catch
            Exit Sub
        End Try

        If String.IsNullOrWhiteSpace(docToAddPath) Then Exit Sub

        If modDocArr Is Nothing Then
            ReDim modDocArr(0)
            modDocArr(0) = docToAdd
            Exit Sub
        End If

        For Each existingDoc As ModelDoc2 In modDocArr
            If existingDoc Is Nothing Then Continue For

            Dim existingPath As String = ""

            Try
                existingPath = existingDoc.GetPathName()
            Catch
                Continue For
            End Try

            If String.Equals(existingPath, docToAddPath, StringComparison.OrdinalIgnoreCase) Then
                Exit Sub
            End If
        Next

        Dim oldUpper As Integer = UBound(modDocArr)
        ReDim Preserve modDocArr(oldUpper + 1)
        modDocArr(oldUpper + 1) = docToAdd
    End Sub

    Private Sub deleteOldUncommittedCadFileIfSafe(oldPath As String, newPath As String)
        If String.IsNullOrWhiteSpace(oldPath) Then Exit Sub
        If String.IsNullOrWhiteSpace(newPath) Then Exit Sub

        Try
            If Not File.Exists(oldPath) Then Exit Sub

            If String.Equals(
            Path.GetFullPath(oldPath),
            Path.GetFullPath(newPath),
            StringComparison.OrdinalIgnoreCase
        ) Then
                Exit Sub
            End If

            'Only auto-delete files inside the local SVN working copy.
            If Not isPathInsideLocalRepo(oldPath) Then Exit Sub

            Dim statusResult As rawProcessReturn = runSvnProcess(
            sSVNPath,
            "status --non-interactive """ & oldPath & """"
        )

            Dim statusText As String = ""

            If statusResult.output IsNot Nothing Then
                statusText &= statusResult.output.Trim()
            End If

            If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
                Exit Sub
            End If

            'If blank, SVN thinks the file is already versioned and clean.
            'Do not auto-delete committed/versioned files.
            If String.IsNullOrWhiteSpace(statusText) Then Exit Sub

            Dim firstStatusChar As Char = statusText(0)

            'Safe cases:
            '?': unversioned junk file
            'A': scheduled for add but not committed yet
            If firstStatusChar = "?"c Then
                File.SetAttributes(oldPath, FileAttributes.Normal)
                File.Delete(oldPath)

            ElseIf firstStatusChar = "A"c Then
                runSvnProcess(sSVNPath, "revert """ & oldPath & """")
                If File.Exists(oldPath) Then
                    File.SetAttributes(oldPath, FileAttributes.Normal)
                    File.Delete(oldPath)
                End If
            End If

        Catch
            'Do not block commit if cleanup fails.
        End Try
    End Sub

    Private Function getOpenModelByPathSafe(filePath As String) As ModelDoc2
        If String.IsNullOrWhiteSpace(filePath) Then Return Nothing
        If iSwApp Is Nothing Then Return Nothing

        Try
            Dim doc As ModelDoc2 = TryCast(iSwApp.GetOpenDocumentByName(filePath), ModelDoc2)
            If doc IsNot Nothing Then Return doc
        Catch
        End Try

        Try
            Dim docsObj As Object = iSwApp.GetDocuments()
            If docsObj Is Nothing Then Return Nothing

            Dim docs As Object() = CType(docsObj, Object())

            For Each docObj As Object In docs
                Dim doc As ModelDoc2 = TryCast(docObj, ModelDoc2)
                If doc Is Nothing Then Continue For

                Dim p As String = ""

                Try
                    p = doc.GetPathName()
                Catch
                    Continue For
                End Try

                If String.Equals(p, filePath, StringComparison.OrdinalIgnoreCase) Then
                    Return doc
                End If
            Next

        Catch
        End Try

        Return Nothing
    End Function

    Private Function renameCadFileToGrc27Name(modDoc As ModelDoc2) As Boolean
        If modDoc Is Nothing Then Return False

        Dim oldPath As String = ""

        Try
            oldPath = modDoc.GetPathName()
        Catch
            Return False
        End Try

        If String.IsNullOrWhiteSpace(oldPath) Then Return False
        If Not isCadFilePath(oldPath) Then Return True

        If isVendorPartPath(oldPath) Then Return True
        If isValidGrc27FileName(oldPath) Then Return True

        Dim newFileName As String = promptForValidGrc27FileName(oldPath)

        If String.IsNullOrWhiteSpace(newFileName) Then Return False

        Dim folderPath As String = Path.GetDirectoryName(oldPath)
        Dim newPath As String = Path.Combine(folderPath, newFileName)

        If File.Exists(newPath) Then
            iSwApp.SendMsgToUser2(
            "Cannot rename file because this file already exists:" & vbCrLf & vbCrLf &
            newPath,
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )
            Return False
        End If

        Try
            'Save a copy using the new valid GRC27/CFD27 name.
            Dim errors As Integer = 0
            Dim warnings As Integer = 0

            Dim saveOk As Boolean = False

            beginInternalSolidWorksSave()
            Try
                saveOk = modDoc.Extension.SaveAs3(
                    newPath,
                    swSaveAsVersion_e.swSaveAsCurrentVersion,
                    swSaveAsOptions_e.swSaveAsOptions_Silent,
                    Nothing,
                    Nothing,
                    errors,
                    warnings
                )
            Finally
                endInternalSolidWorksSave()
            End Try

            If Not saveOk Then
                iSwApp.SendMsgToUser2(
                "Failed to rename/save file as:" & vbCrLf & vbCrLf &
                newPath & vbCrLf & vbCrLf &
                "SolidWorks errors: " & errors & vbCrLf &
                "Warnings: " & warnings,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
                Return False
            End If

            runSvnByArgs({newPath}, "add", bEach:=True)

            Try
                If File.Exists(newPath) Then
                    File.SetAttributes(newPath, File.GetAttributes(newPath) And Not FileAttributes.ReadOnly)
                End If
            Catch
            End Try

            deleteOldUncommittedCadFileIfSafe(oldPath, newPath)

            Try
                Dim reboundDoc As ModelDoc2 = getOpenModelByPathSafe(newPath)

                If reboundDoc IsNot Nothing Then
                    iSwApp.ActivateDoc3(
                        reboundDoc.GetTitle(),
                        True,
                        swRebuildOnActivation_e.swRebuildActiveDoc,
                        0
                    )

                    Try
                        reboundDoc.SetSaveFlag()
                    Catch
                    End Try
                End If
            Catch
            End Try

            Try
                If myUserControl IsNot Nothing Then
                    myUserControl.refreshCurrentTreeViewOnly()
                End If
            Catch
                Try
                    If myUserControl IsNot Nothing Then
                        myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
                    End If
                Catch
                End Try
            End Try

            iSwApp.SendMsgToUser2(
                "File renamed successfully." & vbCrLf & vbCrLf &
                "The new file was added to SVN locally and will be committed on Commit." & vbCrLf &
                "Commit the assembly and renamed child together.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )

            Return True

        Catch ex As Exception
            iSwApp.SendMsgToUser2(
            "Error while renaming file:" & vbCrLf & vbCrLf &
            oldPath & vbCrLf & vbCrLf &
            ex.Message,
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )
            Return False
        End Try
    End Function

    Private Function validateNoDuplicateCadFileNames(ByRef modDocArr() As ModelDoc2) As Boolean
        If modDocArr Is Nothing Then Return True

        Dim seenNames As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)
        Dim duplicateMsg As String = ""

        For Each doc As ModelDoc2 In modDocArr
            If doc Is Nothing Then Continue For

            Dim docPath As String = ""

            Try
                docPath = doc.GetPathName()
            Catch
                Continue For
            End Try

            If String.IsNullOrWhiteSpace(docPath) Then Continue For
            If Not isCadFilePath(docPath) Then Continue For

            Dim fileName As String = Path.GetFileName(docPath)

            If seenNames.ContainsKey(fileName) Then
                duplicateMsg &= fileName & vbCrLf &
                            "1) " & seenNames(fileName) & vbCrLf &
                            "2) " & docPath & vbCrLf & vbCrLf
            Else
                seenNames(fileName) = docPath
            End If
        Next

        If duplicateMsg <> "" Then
            iSwApp.SendMsgToUser2(
            "Commit blocked." & vbCrLf & vbCrLf &
            "Duplicate CAD file names were found in this commit/assembly." & vbCrLf &
            "Each CAD file must have a unique file name." & vbCrLf & vbCrLf &
            duplicateMsg &
            "Rename one of the duplicate files before committing.",
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )

            Return False
        End If

        Return True
    End Function

    Private Function shouldSkipNameCheckForPendingExternalRef(docPath As String) As Boolean
        If String.IsNullOrWhiteSpace(docPath) Then Return False
        If pendingExternalRefSkipNameCheckPaths Is Nothing Then Return False

        For Each pendingPath As String In pendingExternalRefSkipNameCheckPaths
            If String.IsNullOrWhiteSpace(pendingPath) Then Continue For

            Try
                If String.Equals(
                Path.GetFullPath(docPath),
                Path.GetFullPath(pendingPath),
                StringComparison.OrdinalIgnoreCase
            ) Then
                    Return True
                End If
            Catch
                If String.Equals(docPath, pendingPath, StringComparison.OrdinalIgnoreCase) Then
                    Return True
                End If
            End Try
        Next

        Return False
    End Function

    Private Function validateCadNamesBeforeCommit(ByRef modDocArr() As ModelDoc2) As Boolean
        If modDocArr Is Nothing Then Return True

        For Each doc As ModelDoc2 In modDocArr
            If doc Is Nothing Then Continue For

            Dim docPath As String = ""

            Try
                docPath = doc.GetPathName()
            Catch
                Continue For
            End Try

            If String.IsNullOrWhiteSpace(docPath) Then Continue For
            If Not isCadFilePath(docPath) Then Continue For

            'Debug override:
            'Used only for testing/import cleanup. It bypasses the GRC27/CFD27 naming convention prompt,
            'but still keeps duplicate checks, repo checks, add/commit behavior, etc.
            If shouldIgnoreGrc27NamingConventionForDebug() Then Continue For

            'External/vendor refs already handled during this commit should not be forced through normal naming.
            If shouldSkipNameCheckForPendingExternalRef(docPath) Then Continue For

            'Vendor parts are allowed to keep vendor naming, but only inside Vendor Parts.
            If isVendorPartPath(docPath) Then Continue For

            If Not isValidGrc27FileName(docPath) Then
                Dim result As swMessageBoxResult_e = iSwApp.SendMsgToUser2(
                "This CAD file does not follow the GRC27/CFD27 naming convention:" & vbCrLf & vbCrLf &
                Path.GetFileName(docPath) & vbCrLf & vbCrLf &
                "Normal CAD must use:" & vbCrLf &
                "PREFIX_CODE_00000_R# or PREFIX_CODE_A0000_R# or PREFIX_CODE_AB0000_R# or PREFIX_CODE_ABC0000_R# (PREFIX = GRC27 or CFD27)" & vbCrLf & vbCrLf &
                "Would you like to rename it now?",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbYesNo
            )

                If result <> swMessageBoxResult_e.swMbHitYes Then Return False

                If Not renameCadFileToGrc27Name(doc) Then Return False

                Try
                    Dim parentDoc As ModelDoc2 = iSwApp.ActiveDoc

                    If parentDoc IsNot Nothing Then
                        If parentDoc.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                            addModelDocToCommitArrayIfMissing(modDocArr, parentDoc)
                        End If
                    End If
                Catch
                End Try
            End If
        Next

        Return True
    End Function

    Private Function getExternalCadReferences(ByRef modDocArr() As ModelDoc2) As List(Of ExternalReferenceInfo)
        Dim externalRefs As New List(Of ExternalReferenceInfo)
        Dim seenPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If modDocArr Is Nothing Then Return externalRefs

        For Each doc As ModelDoc2 In modDocArr
            If doc Is Nothing Then Continue For

            Dim docPath As String = ""

            Try
                docPath = doc.GetPathName()
            Catch
                Continue For
            End Try

            If String.IsNullOrWhiteSpace(docPath) Then Continue For

            If Not String.IsNullOrWhiteSpace(getOwningPhysicalAssemblyPathForVirtualDocument(doc)) Then
                'Embedded virtual components are versioned through their owning assembly.
                Continue For
            End If

            If isSolidWorksTempOrVirtualPath(docPath) Then
                externalRefs.Add(New ExternalReferenceInfo With {
                    .oldPath = docPath,
                    .fileName = Path.GetFileName(docPath)
                                 })
                Continue For
            End If

            Dim ext As String = Path.GetExtension(docPath).ToUpperInvariant()

            If ext <> ".SLDPRT" AndAlso ext <> ".SLDASM" AndAlso ext <> ".SLDDRW" Then Continue For

            If Not isPathInsideLocalRepo(docPath) Then
                Dim normalized As String = ""
                Try
                    normalized = Path.GetFullPath(docPath)
                Catch
                    normalized = docPath
                End Try

                If Not seenPaths.Contains(normalized) Then
                    seenPaths.Add(normalized)

                    externalRefs.Add(New ExternalReferenceInfo With {
                    .oldPath = normalized,
                    .fileName = Path.GetFileName(normalized)
                })
                End If
            End If
        Next

        Return externalRefs
    End Function

    Private Function getExternalCadReferencesForCommitPathsFast(ByVal commitPaths() As String) As List(Of ExternalReferenceInfo)
        Dim externalRefs As New List(Of ExternalReferenceInfo)()
        Dim seenPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return externalRefs

        'Fast normal-Commit scan:
        'AssemblyDoc.GetComponents(False) gives component paths without recursively building a
        'ModelDoc2 list or resolving every lightweight component.  The previous implementation
        'called getComponentsOfAssemblyOptionalUpdateTree across the full assembly before every
        'commit, which was very expensive on full-car assemblies.
        For Each commitPath As String In commitPaths
            If String.IsNullOrWhiteSpace(commitPath) Then Continue For
            If Not String.Equals(Path.GetExtension(commitPath), ".SLDASM", StringComparison.OrdinalIgnoreCase) Then Continue For

            Dim assemblyModel As ModelDoc2 = getOpenModelByPathSafe(commitPath)
            If assemblyModel Is Nothing Then Continue For

            Try
                If assemblyModel.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
            Catch
                Continue For
            End Try

            Dim assemblyDoc As AssemblyDoc = Nothing
            Dim componentsObject As Object = Nothing

            Try
                assemblyDoc = CType(assemblyModel, AssemblyDoc)
                componentsObject = assemblyDoc.GetComponents(False)
            Catch
                assemblyDoc = Nothing
                componentsObject = Nothing
            End Try

            If componentsObject Is Nothing Then Continue For

            Dim components() As Object = Nothing

            Try
                components = CType(componentsObject, Object())
            Catch
                components = Nothing
            End Try

            If components Is Nothing Then Continue For

            For Each componentObject As Object In components
                Dim component As Component2 = TryCast(componentObject, Component2)
                If component Is Nothing Then Continue For

                Dim componentIsVirtual As Boolean = False

                Try
                    componentIsVirtual = component.IsVirtual
                Catch
                    componentIsVirtual = False
                End Try

                'A virtual component is stored inside its owning assembly. It is not an
                'external CAD reference and has no independent SVN target.
                If componentIsVirtual Then Continue For

                Dim componentPath As String = ""

                Try
                    componentPath = component.GetPathName()
                Catch
                    componentPath = ""
                End Try

                If String.IsNullOrWhiteSpace(componentPath) Then Continue For

                Dim normalizedPath As String = componentPath

                Try
                    normalizedPath = Path.GetFullPath(componentPath)
                Catch
                End Try

                If seenPaths.Contains(normalizedPath) Then Continue For

                If isSolidWorksTempOrVirtualPath(normalizedPath) Then
                    seenPaths.Add(normalizedPath)
                    externalRefs.Add(New ExternalReferenceInfo With {
                        .oldPath = normalizedPath,
                        .fileName = Path.GetFileName(normalizedPath)
                    })
                    Continue For
                End If

                If Not isCadFilePath(normalizedPath) Then Continue For
                If isPathInsideLocalRepo(normalizedPath) Then Continue For

                seenPaths.Add(normalizedPath)
                externalRefs.Add(New ExternalReferenceInfo With {
                    .oldPath = normalizedPath,
                    .fileName = Path.GetFileName(normalizedPath)
                })
            Next
        Next

        Return externalRefs
    End Function

    Private Function pickVaultDestinationFolder() As String
        Using fbd As New FolderBrowserDialog()
            fbd.Description = "Choose a folder inside the SVN working copy for the external CAD files."
            fbd.SelectedPath = myUserControl.localRepoPath.Text

            If fbd.ShowDialog() <> DialogResult.OK Then Return ""

            Dim selectedPath As String = fbd.SelectedPath

            If Not isPathInsideLocalRepo(selectedPath) Then
                iSwApp.SendMsgToUser2(
                "Selected folder is not inside the SVN working copy. Please choose a folder under:" & vbCrLf &
                myUserControl.localRepoPath.Text,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
                Return ""
            End If

            Return selectedPath
        End Using
    End Function

    Private Function getExistingVendorPathForFileName(fileName As String) As String
        If String.IsNullOrWhiteSpace(fileName) Then Return ""

        Dim vendorRoot As String = getVendorPartsRootPath()

        Try
            If Not Directory.Exists(vendorRoot) Then Return ""

            Dim matches() As String = Directory.GetFiles(vendorRoot, fileName, SearchOption.AllDirectories)

            If matches Is Nothing Then Return ""
            If matches.Length = 0 Then Return ""

            Return matches(0)
        Catch
            Return ""
        End Try
    End Function

    Private Function getExistingRepoCadPathForFileName(fileName As String, Optional excludeVendorParts As Boolean = True) As String
        If String.IsNullOrWhiteSpace(fileName) Then Return ""

        Dim repoRoot As String = ""

        Try
            repoRoot = myUserControl.localRepoPath.Text.TrimEnd("\"c)
        Catch
            repoRoot = ""
        End Try

        If String.IsNullOrWhiteSpace(repoRoot) Then Return ""

        Try
            If Not Directory.Exists(repoRoot) Then Return ""

            Dim matches() As String = Directory.GetFiles(repoRoot, fileName, SearchOption.AllDirectories)

            If matches Is Nothing OrElse matches.Length = 0 Then Return ""

            For Each matchPath As String In matches
                If String.IsNullOrWhiteSpace(matchPath) Then Continue For
                If Not File.Exists(matchPath) Then Continue For
                If Not isCadFilePath(matchPath) Then Continue For

                If excludeVendorParts AndAlso isVendorPartPath(matchPath) Then Continue For

                Try
                    Dim statusChar As Char = getFirstSvnStatusChar(matchPath)

                    'Use existing SVN-controlled files first. Clean/versioned files return blank -> " ".
                    'Modified/locked/versioned files can return M/K/etc. Those are still existing vault files.
                    If statusChar <> "?"c AndAlso statusChar <> ChrW(0) Then
                        Return matchPath
                    End If
                Catch
                End Try
            Next

            'Fallback: if exactly one physical match exists in the repo, use it rather than duplicating.
            For Each matchPath As String In matches
                If String.IsNullOrWhiteSpace(matchPath) Then Continue For
                If Not File.Exists(matchPath) Then Continue For
                If Not isCadFilePath(matchPath) Then Continue For
                If excludeVendorParts AndAlso isVendorPartPath(matchPath) Then Continue For
                Return matchPath
            Next
        Catch
        End Try

        Return ""
    End Function

    Private Function getSelectedInContextLockPathSafe(ByVal assemblyDocument As ModelDoc2) As String
        If assemblyDocument Is Nothing Then Return ""

        'Normal physical children own their own lock.
        Dim physicalChildPath As String = getSelectedExternalPhysicalChildPathSafe(assemblyDocument)
        If Not String.IsNullOrWhiteSpace(physicalChildPath) Then Return physicalChildPath

        'A virtual component has no independently versioned file. Its temporary AppData path
        'must never be sent through SVN lock validation; edits are stored in the nearest
        'physical owner assembly, which is also what the PlumVault tree maps the row to.
        Dim selectionManager As SelectionMgr = Nothing

        Try
            selectionManager = TryCast(assemblyDocument.SelectionManager, SelectionMgr)
        Catch
            selectionManager = Nothing
        End Try

        If selectionManager Is Nothing Then Return ""

        Dim selectedCount As Integer = 0
        Try
            selectedCount = CInt(selectionManager.GetSelectedObjectCount2(-1))
        Catch
            selectedCount = 0
        End Try

        For index As Integer = 1 To selectedCount
            Dim selectedComponent As Component2 = Nothing

            Try
                selectedComponent = TryCast(selectionManager.GetSelectedObjectsComponent4(index, -1), Component2)
            Catch
                selectedComponent = Nothing
            End Try

            If selectedComponent Is Nothing OrElse Not isComponentVirtualSafe(selectedComponent) Then Continue For

            Dim ownerPath As String = getPhysicalOwnerAssemblyPathForVirtualComponent(
                selectedComponent,
                assemblyDocument
            )

            If String.IsNullOrWhiteSpace(ownerPath) Then Continue For
            If Not isPathInsideLocalRepo(ownerPath) Then Continue For

            Return normalizeFullPathSafe(ownerPath)
        Next

        Return ""
    End Function

    Private Function getInContextEffectiveLockPath(ByVal editedDocument As ModelDoc2,
                                                    ByVal childPath As String) As String
        Dim ownerPath As String = ""

        Try
            ownerPath = getOwningPhysicalAssemblyPathForVirtualDocument(editedDocument)
        Catch
            ownerPath = ""
        End Try

        If Not String.IsNullOrWhiteSpace(ownerPath) AndAlso isPathInsideLocalRepo(ownerPath) Then
            Return normalizeFullPathSafe(ownerPath)
        End If

        Return normalizeFullPathSafe(childPath)
    End Function

    Private Function inContextEditTargetHasRequiredLock(ByVal editedDocument As ModelDoc2,
                                                         ByVal lockPath As String) As Boolean
        If String.IsNullOrWhiteSpace(lockPath) Then Return False

        Dim documentPath As String = ""
        Try
            If editedDocument IsNot Nothing Then documentPath = editedDocument.GetPathName()
        Catch
            documentPath = ""
        End Try

        If Not String.IsNullOrWhiteSpace(documentPath) AndAlso pathsAreSame(documentPath, lockPath) Then
            Return userHasSvnLockOnDoc(editedDocument)
        End If

        'Virtual children are authorized by the physical assembly that stores them. A brand-new
        'owner cannot have an SVN lock yet and remains valid until its first automatic commit.
        If isNewUnversionedOrAddedFile(lockPath) Then Return True
        Return userHasLocalSvnLockTokenForPath(lockPath, allowCachedToken:=False)
    End Function

    Private Function pathsAreSame(pathA As String, pathB As String) As Boolean
        If String.IsNullOrWhiteSpace(pathA) Then Return False
        If String.IsNullOrWhiteSpace(pathB) Then Return False

        Try
            Return String.Equals(
                Path.GetFullPath(pathA),
                Path.GetFullPath(pathB),
                StringComparison.OrdinalIgnoreCase
            )
        Catch
            Return String.Equals(pathA, pathB, StringComparison.OrdinalIgnoreCase)
        End Try
    End Function

    Private Function pathExistsAsFileOrDirectory(ByVal p As String) As Boolean
        If String.IsNullOrWhiteSpace(p) Then Return False

        Try
            Return File.Exists(p) OrElse Directory.Exists(p)
        Catch
            Return False
        End Try
    End Function

    Private Function filterCommitPathsInsideRepoOnly(ByVal inputPaths() As String) As String()
        If inputPaths Is Nothing Then Return Nothing

        Dim output As New List(Of String)

        For Each p As String In inputPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For

            Try
                If Not pathExistsAsFileOrDirectory(p) Then Continue For
                If Not isPathInsideLocalRepo(p) Then Continue For

                Dim alreadyIncluded As Boolean = output.Any(Function(existingPath) pathsAreSame(existingPath, p))

                If Not alreadyIncluded Then
                    output.Add(Path.GetFullPath(p))
                End If
            Catch
            End Try
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Sub addCommitPathIfMissing(ByVal p As String, ByVal output As List(Of String))
        If output Is Nothing Then Exit Sub
        If String.IsNullOrWhiteSpace(p) Then Exit Sub

        Try
            If Not pathExistsAsFileOrDirectory(p) Then Exit Sub
            If Not isPathInsideLocalRepo(p) Then Exit Sub

            'Files in commit lists should normally be CAD files. Directories are also allowed
            'because SVN requires newly-added parent folders to be included in the same commit
            'as their first child file. Without this, TortoiseSVN reports:
            '"parent is not known to exist in the repository and is not part of the commit".
            If File.Exists(p) AndAlso Not isCadFilePath(p) Then Exit Sub

            Dim fullPath As String = Path.GetFullPath(p)

            For Each existingPath As String In output
                If String.IsNullOrWhiteSpace(existingPath) Then Continue For

                Try
                    If String.Equals(Path.GetFullPath(existingPath), fullPath, StringComparison.OrdinalIgnoreCase) Then
                        Exit Sub
                    End If
                Catch
                    If String.Equals(existingPath, p, StringComparison.OrdinalIgnoreCase) Then Exit Sub
                End Try
            Next

            output.Add(fullPath)
        Catch
        End Try
    End Sub

    Private Function getFirstSvnStatusCharForPathDepthEmpty(ByVal targetPath As String) As Char
        If String.IsNullOrWhiteSpace(targetPath) Then Return ChrW(0)
        If Not pathExistsAsFileOrDirectory(targetPath) Then Return ChrW(0)
        If Not isPathInsideLocalRepo(targetPath) Then Return ChrW(0)

        Try
            Dim statusResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "status --depth empty --non-interactive """ & targetPath & """"
            )

            If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
                Return ChrW(0)
            End If

            Dim statusText As String = ""
            If statusResult.output IsNot Nothing Then statusText = statusResult.output.Trim()

            If String.IsNullOrWhiteSpace(statusText) Then Return " "c
            Return statusText(0)
        Catch
            Return ChrW(0)
        End Try
    End Function

    Private Function isAddedOrUnversionedDirectory(ByVal directoryPath As String) As Boolean
        If String.IsNullOrWhiteSpace(directoryPath) Then Return False
        If Not Directory.Exists(directoryPath) Then Return False
        If Not isPathInsideLocalRepo(directoryPath) Then Return False

        Try
            Dim statusChar As Char = getFirstSvnStatusCharForPathDepthEmpty(directoryPath)
            Return statusChar = "?"c OrElse statusChar = "A"c
        Catch
            Return False
        End Try
    End Function

    Private Sub addPendingDirectoryCommitPathIfNeeded(ByVal directoryPath As String)
        If String.IsNullOrWhiteSpace(directoryPath) Then Exit Sub
        If Not Directory.Exists(directoryPath) Then Exit Sub
        If Not isPathInsideLocalRepo(directoryPath) Then Exit Sub

        Try
            If Not isAddedOrUnversionedDirectory(directoryPath) Then Exit Sub

            If pendingExternalRefCommitPaths Is Nothing Then Exit Sub

            Dim fullDir As String = Path.GetFullPath(directoryPath)

            For Each existingPath As String In pendingExternalRefCommitPaths
                If String.IsNullOrWhiteSpace(existingPath) Then Continue For

                Try
                    If String.Equals(Path.GetFullPath(existingPath), fullDir, StringComparison.OrdinalIgnoreCase) Then Exit Sub
                Catch
                    If String.Equals(existingPath, directoryPath, StringComparison.OrdinalIgnoreCase) Then Exit Sub
                End Try
            Next

            pendingExternalRefCommitPaths.Add(fullDir)
        Catch
        End Try
    End Sub

    Private Function expandCommitPathsWithAddedParentDirectories(ByVal commitPaths() As String) As String()
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return commitPaths

        Dim output As New List(Of String)()

        For Each p As String In commitPaths
            addCommitPathIfMissing(p, output)
        Next

        Dim repoRoot As String = ""

        Try
            repoRoot = Path.GetFullPath(myUserControl.localRepoPath.Text.TrimEnd("\"c)).TrimEnd("\"c)
        Catch
            repoRoot = ""
        End Try

        If String.IsNullOrWhiteSpace(repoRoot) Then Return output.ToArray()

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Not File.Exists(p) Then Continue For
            If Not isPathInsideLocalRepo(p) Then Continue For

            Dim parentDirs As New List(Of String)()

            Try
                Dim currentDir As String = Path.GetDirectoryName(Path.GetFullPath(p))

                While Not String.IsNullOrWhiteSpace(currentDir) AndAlso
                      currentDir.StartsWith(repoRoot & "\", StringComparison.OrdinalIgnoreCase)

                    parentDirs.Add(currentDir)

                    Dim parentInfo As DirectoryInfo = Directory.GetParent(currentDir)
                    If parentInfo Is Nothing Then Exit While
                    currentDir = parentInfo.FullName.TrimEnd("\"c)
                End While
            Catch
            End Try

            parentDirs.Reverse()

            For Each dirPath As String In parentDirs
                If String.IsNullOrWhiteSpace(dirPath) Then Continue For

                Try
                    If Not Directory.Exists(dirPath) Then Continue For
                    If Not isPathInsideLocalRepo(dirPath) Then Continue For

                    Dim statusChar As Char = getFirstSvnStatusCharForPathDepthEmpty(dirPath)

                    If statusChar = "?"c Then
                        runSvnProcess(sSVNPath, "add --parents --depth empty """ & dirPath & """")
                        statusChar = getFirstSvnStatusCharForPathDepthEmpty(dirPath)
                    End If

                    If statusChar = "A"c Then
                        addCommitPathIfMissing(dirPath, output)
                    End If
                Catch
                End Try
            Next
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function getOpenAssemblyDependencyDocsForCommitPaths(ByVal commitPaths() As String) As ModelDoc2()
        If commitPaths Is Nothing Then Return Nothing

        Dim output As New List(Of ModelDoc2)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Not p.EndsWith(".SLDASM", StringComparison.OrdinalIgnoreCase) Then Continue For

            Dim assyDoc As ModelDoc2 = getOpenModelByPathSafe(p)
            If assyDoc Is Nothing Then Continue For

            Try
                If assyDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
            Catch
                Continue For
            End Try

            Try
                Dim assyPath As String = assyDoc.GetPathName()
                If Not String.IsNullOrWhiteSpace(assyPath) AndAlso Not seen.Contains(assyPath) Then
                    seen.Add(assyPath)
                    output.Add(assyDoc)
                End If
            Catch
                output.Add(assyDoc)
            End Try

            Try
                'Local SolidWorks traversal only. Do not resolve lightweight components here.
                Dim assyDocsForTraversal() As ModelDoc2 = New ModelDoc2() {assyDoc}
                Dim depDocs() As ModelDoc2 = myUserControl.getComponentsOfAssemblyOptionalUpdateTree(
                    assyDocsForTraversal,
                    bResolveLightweight:=False
                )

                If depDocs IsNot Nothing Then
                    For Each depDoc As ModelDoc2 In depDocs
                        If depDoc Is Nothing Then Continue For

                        Dim depPath As String = ""
                        Try
                            depPath = depDoc.GetPathName()
                        Catch
                            depPath = ""
                        End Try

                        If String.IsNullOrWhiteSpace(depPath) Then Continue For
                        If Not isCadFilePath(depPath) Then Continue For

                        If Not seen.Contains(depPath) Then
                            seen.Add(depPath)
                            output.Add(depDoc)
                        End If
                    Next
                End If
            Catch
            End Try
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function getOpenAssemblyDocsForCommitPaths(ByVal commitPaths() As String) As ModelDoc2()
        If commitPaths Is Nothing Then Return Nothing

        Dim output As New List(Of ModelDoc2)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Not p.EndsWith(".SLDASM", StringComparison.OrdinalIgnoreCase) Then Continue For

            Dim assyDoc As ModelDoc2 = getOpenModelByPathSafe(p)
            If assyDoc Is Nothing Then Continue For

            Try
                If assyDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
            Catch
                Continue For
            End Try

            Dim assyPath As String = ""
            Try
                assyPath = assyDoc.GetPathName()
            Catch
                assyPath = p
            End Try

            If String.IsNullOrWhiteSpace(assyPath) Then assyPath = p

            If Not seen.Contains(assyPath) Then
                seen.Add(assyPath)
                output.Add(assyDoc)
            End If
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function


    Private Function getVirtualComponentDocumentExtension(ByVal component As Component2) As String
        If component Is Nothing Then Return ""

        Try
            Dim componentDocument As ModelDoc2 = TryCast(component.GetModelDoc2(), ModelDoc2)

            If componentDocument IsNot Nothing Then
                Select Case componentDocument.GetType()
                    Case swDocumentTypes_e.swDocPART
                        Return ".SLDPRT"
                    Case swDocumentTypes_e.swDocASSEMBLY
                        Return ".SLDASM"
                End Select
            End If
        Catch
        End Try

        Try
            Dim componentPath As String = component.GetPathName()
            Dim ext As String = Path.GetExtension(componentPath).ToUpperInvariant()
            If ext = ".SLDPRT" OrElse ext = ".SLDASM" Then Return ext
        Catch
        End Try

        Return ""
    End Function

    Private Function getVirtualComponentDisplayName(ByVal component As Component2) As String
        If component Is Nothing Then Return "VirtualComponent"

        Dim displayName As String = ""

        Try
            displayName = component.Name2
        Catch
            displayName = ""
        End Try

        If String.IsNullOrWhiteSpace(displayName) Then
            Try
                Dim componentDocument As ModelDoc2 = TryCast(component.GetModelDoc2(), ModelDoc2)
                If componentDocument IsNot Nothing Then displayName = componentDocument.GetTitle()
            Catch
                displayName = ""
            End Try
        End If

        If String.IsNullOrWhiteSpace(displayName) Then displayName = "VirtualComponent"
        Return displayName
    End Function

    Private Function getVirtualComponentDepth(ByVal component As Component2) As Integer
        If component Is Nothing Then Return 0

        Dim depth As Integer = 0
        Dim current As Component2 = component

        While current IsNot Nothing AndAlso depth < 100
            depth += 1

            Try
                current = current.GetParent()
            Catch
                current = Nothing
            End Try
        End While

        Return depth
    End Function

    Private Function getVirtualComponentStableKey(ByVal component As Component2,
                                                   ByVal ownerAssemblyPath As String) As String
        If component Is Nothing Then Return ""

        Dim documentPath As String = ""
        Dim documentTitle As String = ""
        Dim componentName As String = ""

        Try
            componentName = component.Name2
        Catch
            componentName = ""
        End Try

        Try
            Dim componentDocument As ModelDoc2 = TryCast(component.GetModelDoc2(), ModelDoc2)

            If componentDocument IsNot Nothing Then
                Try
                    documentPath = componentDocument.GetPathName()
                Catch
                    documentPath = ""
                End Try

                Try
                    documentTitle = componentDocument.GetTitle()
                Catch
                    documentTitle = ""
                End Try
            End If
        Catch
        End Try

        Dim documentIdentity As String = documentPath.Trim().ToUpperInvariant() & "|" &
                                         documentTitle.Trim().ToUpperInvariant()

        'Multiple instances of the same virtual definition can have different component
        'instance names. Deduplicate by the embedded document identity whenever available;
        'fall back to the component name only when SOLIDWORKS exposes no document identity.
        If String.IsNullOrWhiteSpace(documentPath) AndAlso String.IsNullOrWhiteSpace(documentTitle) Then
            documentIdentity = componentName.Trim().ToUpperInvariant()
        End If

        Return normalizeSvnPath(ownerAssemblyPath) & "|" & documentIdentity
    End Function

    Private Function collectVirtualComponentsForCommitPaths(ByVal commitPaths() As String) As List(Of VirtualComponentExternalizeItem)
        Dim output As New List(Of VirtualComponentExternalizeItem)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Dim assemblyDocs() As ModelDoc2 = getOpenAssemblyDocsForCommitPaths(commitPaths)
        If assemblyDocs Is Nothing OrElse assemblyDocs.Length = 0 Then Return output

        For Each assemblyModel As ModelDoc2 In assemblyDocs
            If assemblyModel Is Nothing Then Continue For

            Dim assemblyPath As String = ""
            Dim assemblyDoc As AssemblyDoc = Nothing
            Dim componentsObject As Object = Nothing

            Try
                assemblyPath = assemblyModel.GetPathName()
                assemblyDoc = TryCast(assemblyModel, AssemblyDoc)
                If assemblyDoc IsNot Nothing Then componentsObject = assemblyDoc.GetComponents(False)
            Catch
                componentsObject = Nothing
            End Try

            Dim componentsArray As Array = TryCast(componentsObject, Array)
            If componentsArray Is Nothing Then Continue For

            For Each componentObject As Object In componentsArray
                Dim component As Component2 = TryCast(componentObject, Component2)
                If component Is Nothing OrElse Not isComponentVirtualSafe(component) Then Continue For

                Dim extension As String = getVirtualComponentDocumentExtension(component)
                If extension <> ".SLDPRT" AndAlso extension <> ".SLDASM" Then Continue For

                Dim ownerPath As String = getPhysicalOwnerAssemblyPathForVirtualComponent(component, assemblyModel)
                If String.IsNullOrWhiteSpace(ownerPath) Then ownerPath = assemblyPath
                If String.IsNullOrWhiteSpace(ownerPath) Then Continue For

                Dim stableKey As String = getVirtualComponentStableKey(component, ownerPath)
                If String.IsNullOrWhiteSpace(stableKey) Then Continue For
                If Not seen.Add(stableKey) Then Continue For

                Dim proposedName As String = getVirtualComponentDisplayName(component)

                output.Add(New VirtualComponentExternalizeItem With {
                    .Component = component,
                    .DisplayName = proposedName,
                    .OwnerAssemblyPath = ownerPath,
                    .DocumentExtension = extension,
                    .ComponentDepth = getVirtualComponentDepth(component),
                    .Handling = VirtualComponentHandlingType.SaveExternally,
                    .TargetType = VirtualComponentTargetType.GrcCad,
                    .ProposedId = proposedName,
                    .DestinationFolder = Path.GetDirectoryName(ownerPath)
                })
            Next
        Next

        Return output
    End Function

    Private Function buildVirtualComponentExternalizePlan(ByVal items As List(Of VirtualComponentExternalizeItem)) As VirtualComponentExternalizePlan
        Dim plan As New VirtualComponentExternalizePlan()
        plan.LocalRepoRootFolder = getResolvedSvnWorkingCopyRootPath()
        plan.VendorRootFolder = Path.Combine(plan.LocalRepoRootFolder, "Vendor Parts")

        If items IsNot Nothing Then
            For Each item As VirtualComponentExternalizeItem In items
                If item IsNot Nothing Then plan.Items.Add(item)
            Next
        End If

        Return plan
    End Function

    Private Function showVirtualComponentExternalizeTable(ByVal items As List(Of VirtualComponentExternalizeItem)) As VirtualComponentExternalizePlan
        Dim plan As VirtualComponentExternalizePlan = buildVirtualComponentExternalizePlan(items)

        Try
            Using form As New VirtualComponentExternalizeForm(plan)
                Dim owner As System.Windows.Forms.IWin32Window = getSolidWorksDialogOwner()
                Dim result As System.Windows.Forms.DialogResult

                If owner Is Nothing Then
                    result = form.ShowDialog()
                Else
                    result = form.ShowDialog(owner)
                End If

                If result <> System.Windows.Forms.DialogResult.OK Then Return Nothing
            End Using
        Catch ex As Exception
            iSwApp.SendMsgToUser2(
                "The virtual-component review table could not be opened." & vbCrLf & vbCrLf & ex.Message,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return Nothing
        End Try

        Return plan
    End Function

    Private Function getPhysicalOwnerAssemblyDocsForVirtualPlan(ByVal plan As VirtualComponentExternalizePlan) As ModelDoc2()
        If plan Is Nothing OrElse plan.Items Is Nothing Then Return Nothing

        Dim output As New List(Of ModelDoc2)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each item As VirtualComponentExternalizeItem In plan.Items
            If item Is Nothing Then Continue For
            If item.Handling <> VirtualComponentHandlingType.SaveExternally Then Continue For
            If String.IsNullOrWhiteSpace(item.OwnerAssemblyPath) Then Continue For

            Dim normalizedOwner As String = normalizeSvnPath(item.OwnerAssemblyPath)
            If String.IsNullOrWhiteSpace(normalizedOwner) OrElse Not seen.Add(normalizedOwner) Then Continue For

            Dim ownerDoc As ModelDoc2 = getOpenModelByPathSafe(item.OwnerAssemblyPath)
            If ownerDoc IsNot Nothing Then output.Add(ownerDoc)
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function externalizeVirtualComponentsFromPlan(ByVal plan As VirtualComponentExternalizePlan,
                                                           ByRef addedCommitPaths As List(Of String)) As Boolean
        If plan Is Nothing OrElse plan.Items Is Nothing Then Return False
        If addedCommitPaths Is Nothing Then addedCommitPaths = New List(Of String)()

        Dim externalItems As List(Of VirtualComponentExternalizeItem) = plan.Items.
            Where(Function(item As VirtualComponentExternalizeItem)
                      Return item IsNot Nothing AndAlso
                             item.Handling = VirtualComponentHandlingType.SaveExternally
                  End Function).
            OrderByDescending(Function(item As VirtualComponentExternalizeItem) item.ComponentDepth).
            ToList()

        If externalItems.Count = 0 Then Return True

        For Each item As VirtualComponentExternalizeItem In externalItems
            If Not item.IsChecked OrElse Not item.IsValid Then
                iSwApp.SendMsgToUser2(
                    "A virtual-component row was not fully validated:" & vbCrLf & vbCrLf & item.DisplayName,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If
        Next

        Dim preparedFolders As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each item As VirtualComponentExternalizeItem In externalItems
            Dim folderKey As String = normalizeSvnPath(item.DestinationFolder)
            If preparedFolders.Contains(folderKey) Then Continue For

            Dim folderError As String = ""
            Dim folderMessage As String =
                If(item.TargetType = VirtualComponentTargetType.VendorPart,
                   "Create Vendor Parts folder for virtual component",
                   "Create CAD folder for virtual component")

            If Not prepareSvnDestinationFolderAndCommitIfNeeded(item.DestinationFolder, folderMessage, folderError) Then
                iSwApp.SendMsgToUser2(
                    "The destination folder for a virtual component could not be prepared in SVN." & vbCrLf & vbCrLf &
                    item.DestinationFolder & vbCrLf & vbCrLf & folderError,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If

            preparedFolders.Add(folderKey)
        Next

        Dim ownerPathsToSave As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each item As VirtualComponentExternalizeItem In externalItems
            If File.Exists(item.DestinationPath) Then
                iSwApp.SendMsgToUser2(
                    "Virtual component export stopped because a file already exists at:" & vbCrLf & vbCrLf &
                    item.DestinationPath & vbCrLf & vbCrLf &
                    "Nothing was overwritten.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If

            Dim saveSucceeded As Boolean = False

            Try
                saveSucceeded = item.Component.SaveVirtualComponent(item.DestinationPath)
            Catch ex As Exception
                iSwApp.SendMsgToUser2(
                    "SOLIDWORKS could not save this virtual component externally:" & vbCrLf & vbCrLf &
                    item.DisplayName & vbCrLf & vbCrLf & ex.Message,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End Try

            If Not saveSucceeded OrElse Not File.Exists(item.DestinationPath) Then
                iSwApp.SendMsgToUser2(
                    "SOLIDWORKS did not complete the external save for:" & vbCrLf & vbCrLf &
                    item.DisplayName & vbCrLf & vbCrLf &
                    "Requested destination:" & vbCrLf & item.DestinationPath,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If

            Try
                File.SetAttributes(item.DestinationPath,
                                   File.GetAttributes(item.DestinationPath) And Not FileAttributes.ReadOnly)
            Catch
            End Try

            addCommitPathIfMissing(item.DestinationPath, addedCommitPaths)
            addCommitPathIfMissing(item.OwnerAssemblyPath, addedCommitPaths)
            ownerPathsToSave.Add(normalizeSvnPath(item.OwnerAssemblyPath))
        Next

        'Save each physical owner after all child externalizations. This persists the new
        'references without letting the normal Save event start a second automatic commit.
        For Each ownerPath As String In ownerPathsToSave
            Dim ownerDocument As ModelDoc2 = getOpenModelByPathSafe(ownerPath)
            If ownerDocument Is Nothing Then Continue For

            Dim errors As Integer = 0
            Dim warnings As Integer = 0
            Dim saveOk As Boolean = False

            beginInternalSolidWorksSave()
            Try
                saveOk = ownerDocument.Save3(swSaveAsOptions_e.swSaveAsOptions_Silent, errors, warnings)
            Finally
                endInternalSolidWorksSave()
            End Try

            If Not saveOk Then
                iSwApp.SendMsgToUser2(
                    "The virtual component was saved externally, but the physical owner assembly could not be saved:" & vbCrLf & vbCrLf &
                    ownerPath & vbCrLf & vbCrLf &
                    "SOLIDWORKS errors: " & errors.ToString() & vbCrLf &
                    "Warnings: " & warnings.ToString() & vbCrLf & vbCrLf &
                    "The commit was stopped so the reference change is not misreported as complete.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If
        Next

        Try
            If myUserControl IsNot Nothing Then myUserControl.refreshCurrentTreeViewOnly()
        Catch
        End Try

        Return True
    End Function

    Private Function prepareVirtualComponentsForManualCommit(ByRef commitPaths() As String) As Boolean
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return True

        Dim virtualItems As List(Of VirtualComponentExternalizeItem) = collectVirtualComponentsForCommitPaths(commitPaths)
        If virtualItems Is Nothing OrElse virtualItems.Count = 0 Then Return True

        Dim reviewedPlan As VirtualComponentExternalizePlan = showVirtualComponentExternalizeTable(virtualItems)
        If reviewedPlan Is Nothing Then Return False

        'Converting a virtual component to an external file changes the physical owner
        'assembly reference. Require the lock on each real owner, while preserving the
        'existing first-commit exemption for brand-new assemblies.
        Dim ownerDocs() As ModelDoc2 = getPhysicalOwnerAssemblyDocsForVirtualPlan(reviewedPlan)
        If ownerDocs IsNot Nothing AndAlso ownerDocs.Length > 0 Then
            If Not targetAssembliesMustBeLockedForReferenceChanges(ownerDocs) Then Return False
        End If

        Dim addedPaths As New List(Of String)()
        If Not externalizeVirtualComponentsFromPlan(reviewedPlan, addedPaths) Then Return False

        If addedPaths.Count > 0 Then
            Dim merged As New List(Of String)()

            For Each pathValue As String In commitPaths
                addCommitPathIfMissing(pathValue, merged)
            Next

            For Each pathValue As String In addedPaths
                addCommitPathIfMissing(pathValue, merged)
            Next

            commitPaths = merged.ToArray()
        End If

        Return True
    End Function

    Private Function prepareExternalReferencesForCommitPaths(ByRef commitPaths() As String) As Boolean
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return True

        Dim targetAssemblyDocs() As ModelDoc2 = getOpenAssemblyDocsForCommitPaths(commitPaths)
        If targetAssemblyDocs Is Nothing OrElse targetAssemblyDocs.Length = 0 Then Return True

        'Fast path-only component scan.  This avoids recursively creating ModelDoc2 objects for
        'every component in a large assembly just to discover whether an external/vendor path exists.
        Dim externalRefs As List(Of ExternalReferenceInfo) = getExternalCadReferencesForCommitPathsFast(commitPaths)
        If externalRefs Is Nothing OrElse externalRefs.Count = 0 Then Return True

        'Only assembly commits can change references. If external/vendor CAD must be copied/relinked,
        'the assembly itself has to be writable/locked, except for a brand-new first commit assembly.
        If Not targetAssembliesMustBeLockedForReferenceChanges(targetAssemblyDocs) Then Return False

        Dim noDependencyDocs() As ModelDoc2 = Nothing
        If Not prepareExternalReferencesForSvnActionInternal(noDependencyDocs, externalRefs) Then Return False

        Dim merged As New List(Of String)()

        For Each p As String In commitPaths
            addCommitPathIfMissing(p, merged)
        Next

        If pendingExternalRefCommitPaths IsNot Nothing AndAlso pendingExternalRefCommitPaths.Count > 0 Then
            For Each p As String In pendingExternalRefCommitPaths
                addCommitPathIfMissing(p, merged)
            Next
        End If

        If merged.Count = 0 Then Return False
        commitPaths = merged.ToArray()
        Return True
    End Function

    Private Function expandAssemblyCommitPathsWithNewFirstCommitChildren(ByVal commitPaths() As String) As String()
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return commitPaths

        Dim output As New List(Of String)()

        For Each p As String In commitPaths
            addCommitPathIfMissing(p, output)
        Next

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Not p.EndsWith(".SLDASM", StringComparison.OrdinalIgnoreCase) Then Continue For

            Dim assyDoc As ModelDoc2 = getOpenModelByPathSafe(p)
            If assyDoc Is Nothing Then Continue For

            Try
                If assyDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
            Catch
                Continue For
            End Try

            Try
                Dim assyDocsForTraversal() As ModelDoc2 = New ModelDoc2() {assyDoc}
                Dim depDocs() As ModelDoc2 = myUserControl.getComponentsOfAssemblyOptionalUpdateTree(
                    assyDocsForTraversal,
                    bResolveLightweight:=False
                )

                If depDocs IsNot Nothing Then
                    For Each depDoc As ModelDoc2 In depDocs
                        If depDoc Is Nothing Then Continue For

                        Dim depPath As String = ""
                        Try
                            depPath = depDoc.GetPathName()
                        Catch
                            depPath = ""
                        End Try

                        If String.IsNullOrWhiteSpace(depPath) Then Continue For
                        If Not isCadFilePath(depPath) Then Continue For
                        If Not isPathInsideLocalRepo(depPath) Then Continue For
                        If Not isFirstCommitCandidatePath(depPath) Then Continue For

                        If Not isVendorPartPath(depPath) AndAlso Not shouldIgnoreGrc27NamingConventionForDebug() Then
                            If Not isValidGrc27FileName(depPath) Then
                                If Not renameCadFileToGrc27Name(depDoc) Then
                                    Return Nothing
                                End If

                                Try
                                    depPath = depDoc.GetPathName()
                                Catch
                                    depPath = ""
                                End Try

                                If String.IsNullOrWhiteSpace(depPath) Then Return Nothing
                                If Not isPathInsideLocalRepo(depPath) Then Return Nothing
                                If Not isValidGrc27FileName(depPath) Then Return Nothing
                            End If
                        End If

                        addCommitPathIfMissing(depPath, output)
                    Next
                End If
            Catch
            End Try
        Next

        If output.Count = 0 Then Return commitPaths
        Return output.ToArray()
    End Function

    Private Function expandFirstCommitAssemblyDatasetPaths(ByVal commitPaths() As String) As String()
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return commitPaths

        Dim output As New List(Of String)()

        For Each p As String In commitPaths
            addCommitPathIfMissing(p, output)
        Next

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Not p.EndsWith(".SLDASM", StringComparison.OrdinalIgnoreCase) Then Continue For
            If Not isFirstCommitCandidatePath(p) Then Continue For

            Dim assyDoc As ModelDoc2 = getOpenModelByPathSafe(p)
            If assyDoc Is Nothing Then Continue For

            Try
                If assyDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
            Catch
                Continue For
            End Try

            Try
                Dim assyDocsForTraversal() As ModelDoc2 = New ModelDoc2() {assyDoc}
                Dim depDocs() As ModelDoc2 = myUserControl.getComponentsOfAssemblyOptionalUpdateTree(
                    assyDocsForTraversal,
                    bResolveLightweight:=False
                )

                If depDocs IsNot Nothing Then
                    For Each depDoc As ModelDoc2 In depDocs
                        If depDoc Is Nothing Then Continue For

                        Dim depPath As String = ""
                        Try
                            depPath = depDoc.GetPathName()
                        Catch
                            depPath = ""
                        End Try

                        If String.IsNullOrWhiteSpace(depPath) Then Continue For
                        If Not isFirstCommitCandidatePath(depPath) Then Continue For

                        addCommitPathIfMissing(depPath, output)
                    Next
                End If
            Catch
            End Try
        Next

        If output.Count = 0 Then Return commitPaths
        Return output.ToArray()
    End Function

    Private Function copyExternalRefsToVault(ByRef externalRefs As List(Of ExternalReferenceInfo), destinationFolder As String, Optional isVendorFlow As Boolean = False) As Boolean
        If externalRefs Is Nothing Then Return True
        If externalRefs.Count = 0 Then Return True
        If String.IsNullOrWhiteSpace(destinationFolder) Then Return False

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For

            Dim finalFileName As String = refInfo.fileName

            If Not isVendorFlow Then
                If Not isValidGrc27FileName(finalFileName) Then
                    finalFileName = promptForValidGrc27FileName(refInfo.oldPath)

                    If String.IsNullOrWhiteSpace(finalFileName) Then Return False
                End If
            End If

            'If the exact CAD already exists in the SVN working copy, do not duplicate it.
            'Relink the assembly to the existing vault file instead.
            If isVendorFlow Then
                Dim existingVendorPath As String = getExistingVendorPathForFileName(finalFileName)

                If Not String.IsNullOrWhiteSpace(existingVendorPath) AndAlso File.Exists(existingVendorPath) Then
                    refInfo.newPath = existingVendorPath
                    Continue For
                End If
            Else
                Dim existingGrcPath As String = getExistingRepoCadPathForFileName(finalFileName, excludeVendorParts:=True)

                If Not String.IsNullOrWhiteSpace(existingGrcPath) AndAlso File.Exists(existingGrcPath) Then
                    refInfo.newPath = existingGrcPath
                    Continue For
                End If
            End If

            Dim destPath As String = Path.Combine(destinationFolder, finalFileName)

            If File.Exists(destPath) Then
                'If the destination already exists, reuse it instead of creating a duplicate reference.
                'This is especially important for repeated vendor/GRC imports.
                If isPathInsideLocalRepo(destPath) Then
                    If isVendorFlow OrElse Not isVendorPartPath(destPath) Then
                        refInfo.newPath = destPath
                        Continue For
                    End If
                End If

                iSwApp.SendMsgToUser2(
                "A file with this name already exists in the selected SVN folder:" & vbCrLf & vbCrLf &
                destPath & vbCrLf & vbCrLf &
                "The assembly was not relinked because the existing file is not a valid destination for this flow.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
                Return False
            End If

            Try
                File.Copy(refInfo.oldPath, destPath, overwrite:=False)
            Catch ex As Exception
                iSwApp.SendMsgToUser2(
                "Failed to copy external CAD file into SVN folder:" & vbCrLf & vbCrLf &
                refInfo.oldPath & vbCrLf & vbCrLf &
                "Error:" & vbCrLf & ex.Message,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
                Return False
            End Try

            refInfo.newPath = destPath
        Next

        Return True
    End Function

    Private Function tryAssemblyReplaceComponent(ByVal assy As AssemblyDoc,
                                                 ByVal comp As Component2,
                                                 ByVal newPath As String) As Boolean
        If assy Is Nothing Then Return False
        If comp Is Nothing Then Return False
        If String.IsNullOrWhiteSpace(newPath) Then Return False
        If Not File.Exists(newPath) Then Return False

        'No-reload replacement path:
        'Use SolidWorks' selected-component replacement command. Component2.ReplaceReference
        'does not always update the active in-memory assembly, especially when the task pane
        'has focus or the external file is already loaded from Downloads/Desktop.
        Try
            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)

            If activeDoc IsNot Nothing Then
                Try
                    activeDoc.ClearSelection2(True)
                Catch
                End Try

                Dim compName As String = ""

                Try
                    compName = comp.Name2
                Catch
                    compName = ""
                End Try

                If Not String.IsNullOrWhiteSpace(compName) Then
                    Try
                        activeDoc.Extension.SelectByID2(compName, "COMPONENT", 0, 0, 0, False, 0, Nothing, 0)
                    Catch
                    End Try
                End If
            End If
        Catch
        End Try

        Try
            comp.Select4(False, Nothing, False)
        Catch
        End Try

        Try
            Dim assyObj As Object = assy

            'Replace the selected component instance with the SVN copy.
            'Arguments are intentionally late-bound to tolerate SolidWorks version differences.
            Dim replaceResult As Object = CallByName(assyObj, "ReplaceComponents", CallType.Method, newPath, "", True, True)

            If TypeOf replaceResult Is Boolean Then
                Return CBool(replaceResult)
            End If

            Return True
        Catch
            Try
                'Some SolidWorks versions expose ReplaceComponents2 instead.
                Dim assyObj As Object = assy
                Dim replaceResult2 As Object = CallByName(assyObj, "ReplaceComponents2", CallType.Method, newPath, "", True, True, False)

                If TypeOf replaceResult2 Is Boolean Then
                    Return CBool(replaceResult2)
                End If

                Return True
            Catch
                Return False
            End Try
        Finally
            Try
                Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
                If activeDoc IsNot Nothing Then activeDoc.ClearSelection2(True)
            Catch
            End Try
        End Try
    End Function

    Private Function getAssemblyComponentsSafe(ByVal assy As AssemblyDoc) As List(Of Component2)
        Dim output As New List(Of Component2)()
        If assy Is Nothing Then Return output

        Try
            Dim compsObj As Object = assy.GetComponents(False)
            If compsObj Is Nothing Then Return output

            Dim comps As Object() = CType(compsObj, Object())

            For Each compObj As Object In comps
                Dim comp As Component2 = TryCast(compObj, Component2)
                If comp IsNot Nothing Then output.Add(comp)
            Next
        Catch
        End Try

        Return output
    End Function

    Private Function getAssemblyComponentsUsingPath(ByVal assy As AssemblyDoc,
                                                    ByVal filePath As String) As List(Of Component2)
        Dim output As New List(Of Component2)()
        If assy Is Nothing Then Return output
        If String.IsNullOrWhiteSpace(filePath) Then Return output

        For Each comp As Component2 In getAssemblyComponentsSafe(assy)
            If comp Is Nothing Then Continue For

            Dim compPath As String = ""

            Try
                compPath = comp.GetPathName()
            Catch
                compPath = ""
            End Try

            If String.IsNullOrWhiteSpace(compPath) Then Continue For
            If pathsAreSame(compPath, filePath) Then output.Add(comp)
        Next

        Return output
    End Function

    Private Function externalReferenceIsRelinked(ByVal assy As AssemblyDoc,
                                                 ByVal refInfo As ExternalReferenceInfo) As Boolean
        If assy Is Nothing Then Return False
        If refInfo Is Nothing Then Return False
        If String.IsNullOrWhiteSpace(refInfo.oldPath) Then Return False
        If String.IsNullOrWhiteSpace(refInfo.newPath) Then Return False

        Dim oldStillReferenced As Boolean = False
        Dim newReferenced As Boolean = False

        For Each comp As Component2 In getAssemblyComponentsSafe(assy)
            If comp Is Nothing Then Continue For

            Dim compPath As String = ""

            Try
                compPath = comp.GetPathName()
            Catch
                compPath = ""
            End Try

            If String.IsNullOrWhiteSpace(compPath) Then Continue For

            If pathsAreSame(compPath, refInfo.oldPath) Then oldStillReferenced = True
            If pathsAreSame(compPath, refInfo.newPath) Then newReferenced = True
        Next

        Return (Not oldStillReferenced) AndAlso newReferenced
    End Function

    Private Function allExternalReferencesAreRelinked(ByVal assy As AssemblyDoc,
                                                       ByVal externalRefs As List(Of ExternalReferenceInfo)) As Boolean
        If externalRefs Is Nothing OrElse externalRefs.Count = 0 Then Return True

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For
            If Not externalReferenceIsRelinked(assy, refInfo) Then Return False
        Next

        Return True
    End Function

    Private Sub updateRelinkedComponentDisplayNames(ByVal assy As AssemblyDoc,
                                                     ByVal externalRefs As List(Of ExternalReferenceInfo))
        'Intentionally no direct Component2.Name2 writes.
        '
        'After ReplaceReferencedDocument / ReplaceReference, previously acquired Component2
        'RCWs can be stale even when the physical reference change succeeded. Writing Name2 at
        'that point can enter unstable native SOLIDWORKS code. PlumVault now enables the native
        '"Update component names when documents are replaced" preference during the relink and
        'then requests a deferred FeatureManager refresh after the relink/save stack has returned.
    End Sub

    Private Function saveRelinkedAssemblyWithoutRebuild(ByVal activeDoc As ModelDoc2,
                                                        ByRef saveErrors As Integer,
                                                        ByRef saveWarnings As Integer) As Boolean
        saveErrors = 0
        saveWarnings = 0

        If activeDoc Is Nothing Then Return False

        Try
            Dim saveOptions As Integer =
                CInt(swSaveAsOptions_e.swSaveAsOptions_Silent) Or
                CInt(swSaveAsOptions_e.swSaveAsOptions_AvoidRebuildOnSave)

            beginInternalSolidWorksSave()
            Try
                Return activeDoc.Save3(saveOptions, saveErrors, saveWarnings)
            Finally
                endInternalSolidWorksSave()
            End Try
        Catch
            Return False
        End Try
    End Function

    Private Function relinkExternalRefsToVaultCopies(ByRef externalRefs As List(Of ExternalReferenceInfo)) As Boolean
        If externalRefs Is Nothing Then Return True
        If externalRefs.Count = 0 Then Return True

        Dim activeDoc As ModelDoc2 = Nothing

        Try
            activeDoc = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch ex As Exception
            writeOperationLog("Relink blocked: could not read active document: " & ex.Message)
            Return False
        End Try

        If activeDoc Is Nothing Then Return False

        Try
            If activeDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return True
        Catch
            Return False
        End Try

        Dim activeAssemblyPath As String = ""

        Try
            activeAssemblyPath = activeDoc.GetPathName()
        Catch
            activeAssemblyPath = ""
        End Try

        If Not tryBeginSolidWorksNativeMutation("External reference relink") Then
            iSwApp.SendMsgToUser2(
                "PlumVault is finishing another SOLIDWORKS document operation." & vbCrLf & vbCrLf &
                "Wait a moment, then click Commit again.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End If

        Dim restoreUpdateComponentNamesPreference As Boolean = False
        Dim originalUpdateComponentNamesPreference As Boolean = False
        Dim operationSucceeded As Boolean = False

        Try
            writeOperationLog(
                "External relink started: " & activeAssemblyPath &
                "; references=" & externalRefs.Count.ToString()
            )

            'Use SOLIDWORKS' native replacement-name workflow instead of directly changing
            'Component2.Name2 on component RCWs after their references have changed.
            Try
                originalUpdateComponentNamesPreference =
                    iSwApp.GetUserPreferenceToggle(
                        CInt(swUserPreferenceToggle_e.swExtRefUpdateCompNames)
                    )
                restoreUpdateComponentNamesPreference = True

                If Not originalUpdateComponentNamesPreference Then
                    iSwApp.SetUserPreferenceToggle(
                        CInt(swUserPreferenceToggle_e.swExtRefUpdateCompNames),
                        True
                    )
                End If
            Catch ex As Exception
                writeOperationLog(
                    "Could not temporarily enable native component-name updates: " & ex.Message
                )
            End Try

            Dim assy As AssemblyDoc = CType(activeDoc, AssemblyDoc)

            For Each refInfo As ExternalReferenceInfo In externalRefs
                If refInfo Is Nothing Then Continue For
                If String.IsNullOrWhiteSpace(refInfo.oldPath) Then Continue For
                If String.IsNullOrWhiteSpace(refInfo.newPath) Then Continue For

                writeOperationLog(
                    "Relink item: " & refInfo.oldPath & " -> " & refInfo.newPath
                )

                If externalReferenceIsRelinked(assy, refInfo) Then Continue For

                'ISldWorks.ReplaceReferencedDocument cannot be used here: SOLIDWORKS requires
                'its referencing document to be closed, while this workflow intentionally edits
                'the live assembly. Use the supported live-component replacement paths below.
                Dim componentsUsingOldPath As List(Of Component2) =
                    getAssemblyComponentsUsingPath(assy, refInfo.oldPath)

                For Each comp As Component2 In componentsUsingOldPath
                    If comp Is Nothing Then Continue For

                    Try
                        comp.ReplaceReference(refInfo.newPath)
                    Catch ex As Exception
                        writeOperationLog(
                            "Component2.ReplaceReference failed: " & ex.Message
                        )
                    End Try
                Next

                componentsUsingOldPath = Nothing

                If externalReferenceIsRelinked(assy, refInfo) Then Continue For

                componentsUsingOldPath =
                    getAssemblyComponentsUsingPath(assy, refInfo.oldPath)

                For Each comp As Component2 In componentsUsingOldPath
                    If comp Is Nothing Then Continue For

                    Try
                        tryAssemblyReplaceComponent(assy, comp, refInfo.newPath)
                    Catch ex As Exception
                        writeOperationLog(
                            "ReplaceComponents fallback failed: " & ex.Message
                        )
                    End Try
                Next

                componentsUsingOldPath = Nothing
            Next

            'Discard all pre-relink document/component variables and reacquire the assembly by
            'stable file path before save/verification.
            If Not String.IsNullOrWhiteSpace(activeAssemblyPath) Then
                Try
                    Dim reacquiredDoc As ModelDoc2 =
                        TryCast(iSwApp.GetOpenDocumentByName(activeAssemblyPath), ModelDoc2)

                    If reacquiredDoc IsNot Nothing Then activeDoc = reacquiredDoc
                Catch ex As Exception
                    writeOperationLog(
                        "Could not reacquire assembly after relink: " & ex.Message
                    )
                End Try
            End If

            If activeDoc Is Nothing Then Return False

            Try
                assy = CType(activeDoc, AssemblyDoc)
            Catch
                Return False
            End Try

            Dim saveErrors As Integer = 0
            Dim saveWarnings As Integer = 0
            Dim fastSaveSucceeded As Boolean =
                saveRelinkedAssemblyWithoutRebuild(activeDoc, saveErrors, saveWarnings)

            writeOperationLog(
                "Relink fast save: success=" & fastSaveSucceeded.ToString() &
                "; errors=" & saveErrors.ToString() &
                "; warnings=" & saveWarnings.ToString()
            )

            If fastSaveSucceeded AndAlso
               allExternalReferencesAreRelinked(assy, externalRefs) Then

                operationSucceeded = True
                Return True
            End If

            Try
                activeDoc.ForceRebuild3(False)
                writeOperationLog("Relink recovery rebuild completed.")
            Catch ex As Exception
                writeOperationLog("Relink recovery rebuild failed: " & ex.Message)
            End Try

            Dim recoveryErrors As Integer = 0
            Dim recoveryWarnings As Integer = 0
            Dim recoverySaveSucceeded As Boolean = False

            Try
                beginInternalSolidWorksSave()
                Try
                    recoverySaveSucceeded = activeDoc.Save3(
                        swSaveAsOptions_e.swSaveAsOptions_Silent,
                        recoveryErrors,
                        recoveryWarnings
                    )
                Finally
                    endInternalSolidWorksSave()
                End Try
            Catch ex As Exception
                recoverySaveSucceeded = False
                writeOperationLog("Relink recovery save failed: " & ex.Message)
            End Try

            writeOperationLog(
                "Relink recovery save: success=" & recoverySaveSucceeded.ToString() &
                "; errors=" & recoveryErrors.ToString() &
                "; warnings=" & recoveryWarnings.ToString()
            )

            If recoverySaveSucceeded AndAlso
               allExternalReferencesAreRelinked(assy, externalRefs) Then

                operationSucceeded = True
                Return True
            End If

            Dim failedMsg As New StringBuilder()

            For Each refInfo As ExternalReferenceInfo In externalRefs
                If refInfo Is Nothing Then Continue For
                If externalReferenceIsRelinked(assy, refInfo) Then Continue For

                failedMsg.AppendLine(Path.GetFileName(refInfo.oldPath))
                failedMsg.AppendLine(refInfo.oldPath)
                failedMsg.AppendLine("→")
                failedMsg.AppendLine(refInfo.newPath)
                failedMsg.AppendLine()
            Next

            If failedMsg.Length = 0 Then
                failedMsg.AppendLine(
                    "The references appear updated, but SOLIDWORKS could not save the assembly reliably."
                )
                failedMsg.AppendLine(
                    "Fast save errors: " & saveErrors.ToString() &
                    "; warnings: " & saveWarnings.ToString()
                )
                failedMsg.AppendLine(
                    "Recovery save errors: " & recoveryErrors.ToString() &
                    "; warnings: " & recoveryWarnings.ToString()
                )
            End If

            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "SOLIDWORKS could not complete and save the external/vendor reference relink." &
                vbCrLf & vbCrLf &
                failedMsg.ToString(),
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )

            Return False

        Catch ex As Exception
            writeOperationLog("External relink exception: " & ex.ToString())

            Try
                iSwApp.SendMsgToUser2(
                    "Commit blocked." & vbCrLf & vbCrLf &
                    "PlumVault safely stopped the external-reference operation." & vbCrLf &
                    ex.Message,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            Return False

        Finally
            If restoreUpdateComponentNamesPreference Then
                Try
                    iSwApp.SetUserPreferenceToggle(
                        CInt(swUserPreferenceToggle_e.swExtRefUpdateCompNames),
                        originalUpdateComponentNamesPreference
                    )
                Catch ex As Exception
                    writeOperationLog(
                        "Could not restore component-name preference: " & ex.Message
                    )
                End Try
            End If

            endSolidWorksNativeMutation("External reference relink")

            If operationSucceeded AndAlso
               Not String.IsNullOrWhiteSpace(activeAssemblyPath) Then

                queueDeferredFeatureTreeRefresh(activeAssemblyPath)
            End If

            writeOperationLog(
                "External relink finished: success=" & operationSucceeded.ToString()
            )
        End Try
    End Function

    Private Function verifyExternalRefsNowPointToVaultCopies(ByRef externalRefs As List(Of ExternalReferenceInfo)) As Boolean
        If externalRefs Is Nothing OrElse externalRefs.Count = 0 Then Return True

        Dim activeDoc As ModelDoc2 = Nothing

        Try
            activeDoc = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch
            activeDoc = Nothing
        End Try

        If activeDoc Is Nothing Then Return True

        Try
            If activeDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return True
        Catch
            Return True
        End Try

        Dim assy As AssemblyDoc = CType(activeDoc, AssemblyDoc)
        Dim compsObj As Object = Nothing

        Try
            compsObj = assy.GetComponents(False)
        Catch
            compsObj = Nothing
        End Try

        If compsObj Is Nothing Then Return True

        Dim comps As Object() = CType(compsObj, Object())
        Dim badMsg As String = ""

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For
            If String.IsNullOrWhiteSpace(refInfo.oldPath) Then Continue For
            If String.IsNullOrWhiteSpace(refInfo.newPath) Then Continue For

            Dim oldStillReferenced As Boolean = False
            Dim newReferenced As Boolean = False

            For Each compObj As Object In comps
                Dim comp As Component2 = TryCast(compObj, Component2)
                If comp Is Nothing Then Continue For

                Dim compPath As String = ""

                Try
                    compPath = comp.GetPathName()
                Catch
                    compPath = ""
                End Try

                If String.IsNullOrWhiteSpace(compPath) Then Continue For

                If pathsAreSame(compPath, refInfo.oldPath) Then oldStillReferenced = True
                If pathsAreSame(compPath, refInfo.newPath) Then newReferenced = True
            Next

            If oldStillReferenced OrElse Not newReferenced Then
                badMsg &= Path.GetFileName(refInfo.oldPath) & vbCrLf &
                          "Current external path:" & vbCrLf & refInfo.oldPath & vbCrLf &
                          "Expected SVN path:" & vbCrLf & refInfo.newPath & vbCrLf & vbCrLf
            End If
        Next

        If badMsg <> "" Then
            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "The external/vendor CAD was copied or found in SVN, but SolidWorks is still not referencing the SVN copy." & vbCrLf & vbCrLf &
                "This must be fixed before commit so the assembly does not keep pointing to Downloads/Desktop/outside-SVN files." & vbCrLf & vbCrLf &
                badMsg &
                "The plugin will not reload the assembly automatically. Use SolidWorks File > Replace Components and select the SVN copy if this component cannot be programmatically replaced without reload.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End If

        Return True
    End Function

    Private Function verifyExternalRefsFixed(ByRef modDocArr() As ModelDoc2) As Boolean
        Dim remainingExternal As List(Of ExternalReferenceInfo) = getExternalCadReferences(modDocArr)

        If remainingExternal.Count = 0 Then Return True

        Dim msg As String = "External CAD references still remain. SVN action cancelled." & vbCrLf & vbCrLf

        For Each refInfo As ExternalReferenceInfo In remainingExternal
            msg &= refInfo.fileName & vbCrLf & refInfo.oldPath & vbCrLf & vbCrLf
        Next

        iSwApp.SendMsgToUser2(
        msg,
        swMessageBoxIcon_e.swMbStop,
        swMessageBoxBtn_e.swMbOk
    )

        Return False
    End Function

    Private Function activeAssemblyMustBeLockedForReferenceChanges() As Boolean
        Dim activeDoc As ModelDoc2 = iSwApp.ActiveDoc

        If activeDoc Is Nothing Then Return False

        Try
            If activeDoc.GetType <> swDocumentTypes_e.swDocASSEMBLY Then
                Return True
            End If
        Catch
            Return False
        End Try


        Dim activePath As String = ""

        Try
            activePath = activeDoc.GetPathName()
        Catch
            activePath = ""
        End Try

        'Brand-new assemblies saved inside the SVN working copy cannot be locked yet,
        'because they are not version controlled until the first commit.
        'Allow them through so Commit can svn add + commit them.
        If isNewUnversionedOrAddedFile(activePath) Then
            Return True
        End If
        Dim lockCheckDocs() As ModelDoc2 = {activeDoc}

        Try
            Dim hasLocks As Boolean() = ensureUserHasLocks(lockCheckDocs, bRetry:=False)

            If hasLocks IsNot Nothing AndAlso hasLocks.Length > 0 AndAlso hasLocks(0) Then
                Return True
            End If
        Catch
        End Try

        iSwApp.SendMsgToUser2(
        "Commit blocked." & vbCrLf & vbCrLf &
        "The active assembly must be locked by you before external or vendor CAD can be added." & vbCrLf & vbCrLf &
        "Why:" & vbCrLf &
        "Adding vendor/external CAD changes the assembly references, so the assembly must be writable and locked first." & vbCrLf & vbCrLf &
        "Please click Get Locks on the assembly, then try Commit again.",
        swMessageBoxIcon_e.swMbStop,
        swMessageBoxBtn_e.swMbOk
    )

        Return False
    End Function

    Private Function targetAssembliesMustBeLockedForReferenceChanges(ByVal assemblyDocs() As ModelDoc2) As Boolean
        If assemblyDocs Is Nothing OrElse assemblyDocs.Length = 0 Then Return True

        Dim docsThatNeedLocks As New List(Of ModelDoc2)()
        Dim namesThatNeedLocks As New List(Of String)()

        For Each doc As ModelDoc2 In assemblyDocs
            If doc Is Nothing Then Continue For

            Try
                If doc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
            Catch
                Continue For
            End Try

            Dim docPath As String = ""
            Dim displayName As String = "<assembly>"

            Try
                docPath = doc.GetPathName()
            Catch
                docPath = ""
            End Try

            Try
                If Not String.IsNullOrWhiteSpace(docPath) Then
                    displayName = Path.GetFileName(docPath)
                Else
                    displayName = doc.GetTitle()
                End If
            Catch
            End Try

            'Brand-new assemblies saved inside the SVN working copy cannot be locked yet,
            'because they are not version controlled until the first commit.
            'Allow them through so Commit can svn add + commit them.
            If isNewUnversionedOrAddedFile(docPath) Then Continue For

            docsThatNeedLocks.Add(doc)
            namesThatNeedLocks.Add(displayName)
        Next

        If docsThatNeedLocks.Count = 0 Then Return True

        Try
            Dim lockCheckDocs() As ModelDoc2 = docsThatNeedLocks.ToArray()
            Dim hasLocks As Boolean() = ensureUserHasLocks(lockCheckDocs, bRetry:=False)

            Dim missingLocks As New List(Of String)()

            For i As Integer = 0 To docsThatNeedLocks.Count - 1
                Dim lockedByYou As Boolean = False

                If hasLocks IsNot Nothing AndAlso i < hasLocks.Length Then
                    lockedByYou = hasLocks(i)
                End If

                If Not lockedByYou Then
                    If i < namesThatNeedLocks.Count Then
                        missingLocks.Add(namesThatNeedLocks(i))
                    Else
                        missingLocks.Add("<assembly>")
                    End If
                End If
            Next

            If missingLocks.Count = 0 Then Return True

            Dim msg As String = "Commit blocked." & vbCrLf & vbCrLf &
                "The assembly being committed must be locked by you before external or vendor CAD can be added." & vbCrLf & vbCrLf &
                "Why:" & vbCrLf &
                "Adding vendor/external CAD changes assembly references, so the target assembly must be writable and locked first." & vbCrLf & vbCrLf &
                "Assembly missing your lock:" & vbCrLf

            For Each missingName As String In missingLocks
                msg &= "- " & missingName & vbCrLf
            Next

            msg &= vbCrLf & "Please click Get Locks on the selected assembly, then try Commit again."

            iSwApp.SendMsgToUser2(
                msg,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )

            Return False

        Catch
            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "The plugin could not verify that the target assembly is locked by you before changing vendor/external references." & vbCrLf & vbCrLf &
                "Please click Get Locks on the selected assembly, then try Commit again.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )

            Return False
        End Try
    End Function

    Private Function getDefaultExternalReferenceDestinationFolder(ByVal modDocArr() As ModelDoc2) As String
        If modDocArr IsNot Nothing Then
            For Each doc As ModelDoc2 In modDocArr
                If doc Is Nothing Then Continue For

                Try
                    If doc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For
                Catch
                    Continue For
                End Try

                Dim assemblyPath As String = ""

                Try
                    assemblyPath = doc.GetPathName()
                Catch
                    assemblyPath = ""
                End Try

                If String.IsNullOrWhiteSpace(assemblyPath) Then Continue For
                If Not isPathInsideLocalRepo(assemblyPath) Then Continue For

                Try
                    Return Path.GetDirectoryName(assemblyPath)
                Catch
                End Try
            Next
        End If

        Try
            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)

            If activeDoc IsNot Nothing AndAlso activeDoc.GetType() = swDocumentTypes_e.swDocASSEMBLY Then
                Dim activePath As String = activeDoc.GetPathName()

                If Not String.IsNullOrWhiteSpace(activePath) AndAlso isPathInsideLocalRepo(activePath) Then
                    Return Path.GetDirectoryName(activePath)
                End If
            End If
        Catch
        End Try

        Try
            If myUserControl IsNot Nothing AndAlso myUserControl.localRepoPath IsNot Nothing Then
                Dim configuredPath As String = myUserControl.localRepoPath.Text
                If Not String.IsNullOrWhiteSpace(configuredPath) Then Return configuredPath
            End If
        Catch
        End Try

        Return getResolvedSvnWorkingCopyRootPath()
    End Function

    Private Function buildExternalReferenceImportPlan(ByVal externalRefs As List(Of ExternalReferenceInfo),
                                                       ByVal modDocArr() As ModelDoc2) As ExternalReferenceImportPlan
        Dim plan As New ExternalReferenceImportPlan()
        plan.LocalRepoRootFolder = getResolvedSvnWorkingCopyRootPath()
        plan.DefaultGrcDestinationFolder = getDefaultExternalReferenceDestinationFolder(modDocArr)
        plan.VendorRootFolder = getVendorPartsRootPath()

        If externalRefs Is Nothing Then Return plan

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For

            Dim item As New ExternalReferenceImportItem()
            item.SourcePath = refInfo.oldPath
            item.ProposedId = getNeutralFormatBaseName(refInfo.fileName)
            item.TargetType = ExternalReferenceImportTargetType.GrcCad
            item.DestinationFolder = plan.DefaultGrcDestinationFolder

            'A repeated standard part is pre-classified as Vendor Part when the same
            'filename already exists anywhere under a Vendor Parts folder. The table still
            'shows the row and tells the user that the existing canonical file will be reused.
            If String.Equals(Path.GetExtension(refInfo.oldPath), ".SLDPRT", StringComparison.OrdinalIgnoreCase) Then
                Dim existingVendorPath As String = getExistingVendorPathForFileName(refInfo.fileName)

                If Not String.IsNullOrWhiteSpace(existingVendorPath) AndAlso File.Exists(existingVendorPath) Then
                    item.TargetType = ExternalReferenceImportTargetType.VendorPart
                    item.DestinationFolder = Path.GetDirectoryName(existingVendorPath)
                End If
            End If

            plan.Items.Add(item)
        Next

        Return plan
    End Function

    Private Function copyExternalReferencesFromReviewedPlan(ByVal externalRefs As List(Of ExternalReferenceInfo),
                                                             ByVal plan As ExternalReferenceImportPlan) As Boolean
        If externalRefs Is Nothing OrElse externalRefs.Count = 0 Then Return True
        If plan Is Nothing OrElse plan.Items Is Nothing Then Return False

        Dim itemsBySource As New Dictionary(Of String, ExternalReferenceImportItem)(StringComparer.OrdinalIgnoreCase)

        For Each item As ExternalReferenceImportItem In plan.Items
            If item Is Nothing OrElse String.IsNullOrWhiteSpace(item.SourcePath) Then Continue For
            itemsBySource(normalizeSvnPath(item.SourcePath)) = item
        Next

        'Prepare every unique destination folder first. This prevents a half-copied import
        'when the user created one or more new folders in the table's Browse dialog.
        Dim preparedFolders As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each item As ExternalReferenceImportItem In plan.Items
            If item Is Nothing Then Continue For
            If Not String.IsNullOrWhiteSpace(item.ReuseExistingPath) Then Continue For
            If String.IsNullOrWhiteSpace(item.DestinationFolder) Then Return False

            Dim folderKey As String = normalizeSvnPath(item.DestinationFolder)
            If preparedFolders.Contains(folderKey) Then Continue For

            Dim folderError As String = ""

            If Not prepareSvnDestinationFolderAndCommitIfNeeded(
                item.DestinationFolder,
                "Create referenced CAD destination folder",
                folderError) Then

                iSwApp.SendMsgToUser2(
                    "Could not prepare the selected referenced-CAD destination in SVN:" & vbCrLf & vbCrLf &
                    item.DestinationFolder & vbCrLf & vbCrLf &
                    folderError,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If

            preparedFolders.Add(folderKey)
        Next

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For

            Dim item As ExternalReferenceImportItem = Nothing

            If Not itemsBySource.TryGetValue(normalizeSvnPath(refInfo.oldPath), item) OrElse item Is Nothing Then
                iSwApp.SendMsgToUser2(
                    "The reviewed external-reference mapping no longer matches the assembly reference list:" & vbCrLf & vbCrLf &
                    refInfo.oldPath,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If

            If Not item.IsChecked OrElse Not item.IsValid Then
                iSwApp.SendMsgToUser2(
                    "An external-reference row was not fully validated:" & vbCrLf & vbCrLf &
                    refInfo.oldPath,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If

            If Not String.IsNullOrWhiteSpace(item.ReuseExistingPath) Then
                If Not File.Exists(item.ReuseExistingPath) Then
                    iSwApp.SendMsgToUser2(
                        "The existing Vendor Parts file selected for reuse is no longer available:" & vbCrLf & vbCrLf &
                        item.ReuseExistingPath,
                        swMessageBoxIcon_e.swMbStop,
                        swMessageBoxBtn_e.swMbOk
                    )
                    Return False
                End If

                refInfo.newPath = item.ReuseExistingPath
                Continue For
            End If

            If String.IsNullOrWhiteSpace(item.DestinationPath) Then Return False

            If File.Exists(item.DestinationPath) Then
                iSwApp.SendMsgToUser2(
                    "A file appeared at the reviewed destination before PlumVault could copy the reference:" & vbCrLf & vbCrLf &
                    item.DestinationPath & vbCrLf & vbCrLf &
                    "Nothing was overwritten. Reopen the table and choose another ID or destination.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End If

            Try
                If File.Exists(refInfo.oldPath) Then
                    File.Copy(refInfo.oldPath, item.DestinationPath, overwrite:=False)
                Else
                    'Some live neutral-format imports have a SOLIDWORKS temp path that is not
                    'visible to File.Exists/File.Copy. Save a native copy from the exact open
                    'document; never do this fallback for an arbitrary missing path.
                    Dim sourceDocument As ModelDoc2 = getOpenModelByPathSafe(refInfo.oldPath)

                    If sourceDocument Is Nothing OrElse Not canRouteTempReferenceThroughReview(refInfo.oldPath) Then
                        Throw New FileNotFoundException(
                            "The temporary source is no longer open in SOLIDWORKS.",
                            refInfo.oldPath
                        )
                    End If

                    Dim saveErrors As Integer = 0
                    Dim saveWarnings As Integer = 0
                    Dim savedCopy As Boolean = False

                    beginInternalSolidWorksSave()
                    Try
                        savedCopy = sourceDocument.Extension.SaveAs3(
                            item.DestinationPath,
                            swSaveAsVersion_e.swSaveAsCurrentVersion,
                            swSaveAsOptions_e.swSaveAsOptions_Copy + swSaveAsOptions_e.swSaveAsOptions_AvoidRebuildOnSave,
                            Nothing,
                            Nothing,
                            saveErrors,
                            saveWarnings
                        )
                    Finally
                        endInternalSolidWorksSave()
                    End Try

                    If Not savedCopy OrElse Not File.Exists(item.DestinationPath) Then
                        Throw New IOException(
                            "SOLIDWORKS could not save the temporary import as a native CAD copy. " &
                            "Errors: " & saveErrors.ToString() & "; warnings: " & saveWarnings.ToString()
                        )
                    End If
                End If
            Catch ex As Exception
                iSwApp.SendMsgToUser2(
                    "Failed to copy referenced CAD into SVN:" & vbCrLf & vbCrLf &
                    refInfo.oldPath & vbCrLf & vbCrLf &
                    "Destination:" & vbCrLf & item.DestinationPath & vbCrLf & vbCrLf &
                    ex.Message,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End Try

            refInfo.newPath = item.DestinationPath
        Next

        Return True
    End Function

    Private Function showExternalReferenceImportTable(ByVal externalRefs As List(Of ExternalReferenceInfo),
                                                       ByVal modDocArr() As ModelDoc2) As ExternalReferenceImportPlan
        Dim plan As ExternalReferenceImportPlan = buildExternalReferenceImportPlan(externalRefs, modDocArr)

        Try
            Using form As New ExternalReferenceImportForm(plan)
                Dim owner As System.Windows.Forms.IWin32Window = getSolidWorksDialogOwner()
                Dim result As DialogResult

                If owner Is Nothing Then
                    result = form.ShowDialog()
                Else
                    result = form.ShowDialog(owner)
                End If

                If result <> DialogResult.OK Then Return Nothing
            End Using
        Catch ex As Exception
            iSwApp.SendMsgToUser2(
                "The referenced-CAD review table could not be opened." & vbCrLf & vbCrLf & ex.Message,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return Nothing
        End Try

        Return plan
    End Function

    Public Function prepareExternalReferencesForSvnAction(ByRef modDocArr() As ModelDoc2) As Boolean
        Return prepareExternalReferencesForSvnActionInternal(modDocArr, Nothing)
    End Function

    Private Function prepareExternalReferencesForSvnActionInternal(ByRef modDocArr() As ModelDoc2,
                                                                    ByVal precomputedExternalRefs As List(Of ExternalReferenceInfo)) As Boolean
        If precomputedExternalRefs Is Nothing Then
            If modDocArr Is Nothing Then Return True
            If modDocArr.Length = 0 Then Return True
        End If

        pendingExternalRefCommitPaths.Clear()
        pendingExternalRefSkipNameCheckPaths.Clear()

        Dim externalRefs As List(Of ExternalReferenceInfo) = precomputedExternalRefs

        If externalRefs Is Nothing Then
            externalRefs = getExternalCadReferences(modDocArr)
        End If

        If externalRefs Is Nothing OrElse externalRefs.Count = 0 Then Return True

        Dim virtualOrTempRefs As New List(Of ExternalReferenceInfo)

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If isSolidWorksTempOrVirtualPath(refInfo.oldPath) AndAlso
               Not canRouteTempReferenceThroughReview(refInfo.oldPath) Then
                virtualOrTempRefs.Add(refInfo)
            End If
        Next

        If virtualOrTempRefs.Count > 0 Then
            Dim virtualMsg As String =
        "This assembly contains a temporary or unresolved external SOLIDWORKS file." & vbCrLf & vbCrLf &
        "True virtual components are allowed and inherit the owning assembly's SVN state. " &
        "The file(s) below could not be confirmed as embedded virtual components and cannot be copied safely." & vbCrLf & vbCrLf &
        "Temporary/unresolved files:" & vbCrLf

            For Each refInfo As ExternalReferenceInfo In virtualOrTempRefs
                virtualMsg &= refInfo.fileName & vbCrLf & refInfo.oldPath & vbCrLf & vbCrLf
            Next

            virtualMsg &= "Resolve or save these temporary references to a stable file location, then retry." & vbCrLf & vbCrLf &
                  "SVN working copy:" & vbCrLf &
                  myUserControl.localRepoPath.Text

            iSwApp.SendMsgToUser2(
        virtualMsg,
        swMessageBoxIcon_e.swMbStop,
        swMessageBoxBtn_e.swMbOk
    )

            Return False
        End If

        'Every stable external reference is shown in one review table. Repeated vendor
        'parts are preclassified and clearly shown as reusing the existing canonical SVN file;
        'normal GRC/CFD rows may be re-IDed and placed independently.
        Dim reviewedPlan As ExternalReferenceImportPlan = showExternalReferenceImportTable(externalRefs, modDocArr)
        If reviewedPlan Is Nothing Then Return False

        If Not copyExternalReferencesFromReviewedPlan(externalRefs, reviewedPlan) Then Return False
        If Not relinkExternalRefsToVaultCopies(externalRefs) Then Return False
        If Not verifyExternalRefsNowPointToVaultCopies(externalRefs) Then Return False

        Dim copiedPaths As New List(Of String)
        Dim reusedExistingPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If reviewedPlan IsNot Nothing AndAlso reviewedPlan.Items IsNot Nothing Then
            For Each reviewedItem As ExternalReferenceImportItem In reviewedPlan.Items
                If reviewedItem Is Nothing Then Continue For
                If String.IsNullOrWhiteSpace(reviewedItem.ReuseExistingPath) Then Continue For
                reusedExistingPaths.Add(normalizeSvnPath(reviewedItem.ReuseExistingPath))
            Next
        End If

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For

            If Not String.IsNullOrWhiteSpace(refInfo.oldPath) Then
                pendingExternalRefSkipNameCheckPaths.Add(refInfo.oldPath)
            End If

            If Not String.IsNullOrWhiteSpace(refInfo.newPath) Then
                If Not reusedExistingPaths.Contains(normalizeSvnPath(refInfo.newPath)) Then
                    copiedPaths.Add(refInfo.newPath)
                End If

                pendingExternalRefSkipNameCheckPaths.Add(refInfo.newPath)
            End If
        Next

        pendingExternalRefCommitPaths.Clear()

        'Do NOT clear pendingExternalRefSkipNameCheckPaths here.
        'For vendor parts, SolidWorks may still report the old external file path
        'until the assembly/dependency list fully refreshes.
        'validateCadNamesBeforeCommit needs this list so vendor files are not
        'forced through normal GRC27 naming after they were already copied
        'into Vendor Parts and relinked.

        If copiedPaths.Count > 0 Then
            For Each copiedPath As String In copiedPaths
                If String.IsNullOrWhiteSpace(copiedPath) Then Continue For
                If Not File.Exists(copiedPath) Then Continue For
                If Not isPathInsideLocalRepo(copiedPath) Then Continue For

                runSvnProcess(sSVNPath, "add --parents """ & copiedPath & """")

                'If this is the first file under a new folder such as Vendor Parts,
                'the parent folder itself must be part of the same commit.
                'Otherwise SVN/Tortoise reports that the parent is not known to exist.
                Dim copiedParentFolder As String = ""
                Try
                    copiedParentFolder = Path.GetDirectoryName(copiedPath)
                Catch
                    copiedParentFolder = ""
                End Try

                If Not String.IsNullOrWhiteSpace(copiedParentFolder) Then
                    runSvnProcess(sSVNPath, "add --parents --depth empty """ & copiedParentFolder & """")
                    addPendingDirectoryCommitPathIfNeeded(copiedParentFolder)
                End If

                pendingExternalRefCommitPaths.Add(copiedPath)
            Next
        End If

        'The relink routine already persisted the assembly reference changes.
        'The normal commit save step will only save again if the document is still dirty.
        Return True
    End Function

    Function commitAllowedOnlyIfUpToDate(ByRef modDocArr() As ModelDoc2, Optional bIncludeDependents As Boolean = False) As Boolean
        If modDocArr Is Nothing Then Return False
        If modDocArr.Length = 0 Then Return False

        'Fast commit safety:
        'Do not walk/resolve the assembly again and do not contact the SVN server here.
        'Normal Commit and Commit With Dependents already provide the exact document paths that
        'are being committed.  Use the existing Sync cache for those paths and, when an assembly
        'is present, use the existing loaded-tree/cache guard for referenced geometry.
        Dim commitPaths() As String = Nothing

        Try
            commitPaths = getFilePathsFromModDocArr(modDocArr)
        Catch
            commitPaths = Nothing
        End Try

        commitPaths = filterCommitPathsInsideRepoOnly(commitPaths)

        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then
            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "No valid SVN working-copy CAD paths were available for the freshness check.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End If

        'This is cache-only.  It blocks files already known to be stale, but it never launches
        'a fresh svn status -u process during Commit.
        If Not commitPathsAllowedOnlyIfUpToDate(commitPaths) Then Return False

        Dim hasAssembly As Boolean = False

        Try
            For Each commitPath As String In commitPaths
                If String.IsNullOrWhiteSpace(commitPath) Then Continue For

                If String.Equals(Path.GetExtension(commitPath), ".SLDASM", StringComparison.OrdinalIgnoreCase) Then
                    hasAssembly = True
                    Exit For
                End If
            Next
        Catch
            hasAssembly = False
        End Try

        If hasAssembly Then
            'For assemblies, keep the stronger protection: every loaded child/related CAD path
            'must have usable server-aware Sync cache data and none may be marked out of date.
            Return commitAssemblyChildrenAllowedOnlyIfCachedUpToDate(commitPaths)
        End If

        Return True
    End Function

    Public Sub unlockPathsLockedOnly(ByVal selectedPaths() As String)
        Dim bSuccess As Boolean = True
        Dim status As SVNStatus = Nothing
        Dim debugWatch As Stopwatch = Nothing
        Dim debugNotes As New List(Of String)()
        Dim phaseStartMs As Long = 0

        If debugTimingEnabled() Then
            debugWatch = Stopwatch.StartNew()
        End If

        Dim filteredPaths() As String = distinctExistingCadFilePaths(selectedPaths)

        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No valid selected CAD file paths were found for Release Locks.", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If Not userAcceptsLossOfChangesPaths(filteredPaths, "Release Locks, and revert changes to vault version?") Then Exit Sub

        'saveAllOpenFiles writes EVERY open dirty document, not only the selected ones. Run it
        'inside the internal-save gate so PlumVault's own save guard does not treat each of
        'those writes as a user-initiated save: without this, one Release Locks click could
        'raise a modal "save blocked" dialog per open document and queue an automatic commit
        'for files the user never selected. The inner Try/Catch keeps the original retry.
        beginInternalSolidWorksSave()
        Try
            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                saveAllOpenFiles(bShowError:=True)
                If debugWatch IsNot Nothing Then debugNotes.Add("Save open files: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
                saveAllOpenFiles(bShowError:=True)
            End Try
        Finally
            endInternalSolidWorksSave()
        End Try

        If debugWatch IsNot Nothing Then debugNotes.Add("Selected path candidates: " & filteredPaths.Length.ToString())

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

            status = getFileSVNStatus(
                bCheckServer:=False,
                modDocArr:=Nothing,
                bUpdateStatusOfAllOpenModels:=False,
                sDirectFilePathArr:=filteredPaths
            )

            attachOpenDocsToStatusPaths(status)

            If debugWatch IsNot Nothing Then debugNotes.Add("Local SVN status for selected paths: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        Catch
            status = Nothing
        End Try

        If IsNothing(status) Then
            iSwApp.SendMsgToUser2("Release Locks failed. Could not read local SVN status.", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Dim lockedPaths() As String = Nothing
        Dim modifiedPaths() As String = Nothing

        Try
            lockedPaths = getLockedPathsFromStatus(status)
        Catch
            lockedPaths = Nothing
        End Try

        Try
            modifiedPaths = getLockedModifiedPathsFromStatus(status)
        Catch
            modifiedPaths = Nothing
        End Try

        If lockedPaths Is Nothing OrElse lockedPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No Selected Items were locked", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            If debugWatch IsNot Nothing Then
                debugNotes.Add("Locked files found: 0")
                debugNotes.Add("Total Release Locks time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
                showSvnTimingDebugWindow("Release Locks finished - nothing locked.", debugNotes)
            End If
            Exit Sub
        End If

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Locked files found: " & lockedPaths.Length.ToString())
            debugNotes.Add("Locked+modified files needing revert: " & countStringArrayItems(modifiedPaths).ToString())
        End If

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            bSuccess = runTortoiseProcexeWithMonitor("/command:unlock /path:" & formatFilePathArrForProc(lockedPaths) & " /closeonend:3")
            If debugWatch IsNot Nothing Then debugNotes.Add("TortoiseSVN unlock locked selected files: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        Catch
            bSuccess = False
        End Try

        If Not bSuccess Then
            'Do not fall through to the revert below. The unlock did not complete, so these
            'files are almost certainly still locked by this working copy, and discarding the
            'local changes now would destroy the user's work while they still hold the lock -
            'the worst possible outcome for this action. The status-cache update further down
            'would additionally have recoloured the tree as unlocked, a state that was never
            'true. Leave the files and the displayed state untouched and let the user retry.
            Try
                iSwApp.SendMsgToUser2(
                    "Releasing locks failed, so nothing was changed." & vbCrLf & vbCrLf &
                    "Your files are still locked and your local changes were kept." & vbCrLf & vbCrLf &
                    "Try Release Locks again. If it keeps failing, click Cleanup first, then retry.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            Exit Sub
        End If

        If modifiedPaths IsNot Nothing AndAlso modifiedPaths.Length > 0 Then
            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                status.releaseFileSystemAccessToRevertOrUpdateModels(iSwApp, New Integer() {-1})
                If debugWatch IsNot Nothing Then debugNotes.Add("Release SolidWorks file handles before revert: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
            End Try

            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                bSuccess = runTortoiseProcexeWithMonitor("/command:revert /path:" & formatFilePathArrForProc(modifiedPaths) & " /closeonend:3")
                If debugWatch IsNot Nothing Then debugNotes.Add("TortoiseSVN revert locked modified files: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

                If Not bSuccess Then iSwApp.SendMsgToUserv("Revert Files Failed.")
            Catch
            End Try

            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                status.reattachDocsToFileSystem(New Integer() {-1}, iSwApp)
                If debugWatch IsNot Nothing Then debugNotes.Add("Reattach docs after revert: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
            End Try
        End If

        Try
            updateStatusCacheForKnownPaths(lockedPaths, forceLock6:=" ")
            If modifiedPaths IsNot Nothing AndAlso modifiedPaths.Length > 0 Then
                updateStatusCacheForKnownPaths(modifiedPaths, forceAddDelChg1:=" ")
            End If
        Catch
        End Try

        'Read-only enforcement: the locks above are gone, so any of these documents still
        'open must go back to SOLIDWORKS' native read-only protection (gated + deferred).
        Try
            restoreInternalReadOnlyForReleasedPathsPublic(lockedPaths)
        Catch
        End Try

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            updateLockStatusPublic(bRefreshAllTreeViews:=False)
            refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)
            If debugWatch IsNot Nothing Then debugNotes.Add("Local status/tree refresh: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        Catch
        End Try

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Total Release Locks time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
            showSvnTimingDebugWindow("Release Locks finished.", debugNotes)
        End If
    End Sub

    Sub unlockDocs(Optional ByRef modDocArr() As ModelDoc2 = Nothing)
        Dim bSuccess As Boolean = True
        Dim status As SVNStatus = Nothing
        Dim debugWatch As Stopwatch = Nothing
        Dim debugNotes As New List(Of String)()
        Dim phaseStartMs As Long = 0

        If debugTimingEnabled() Then
            debugWatch = Stopwatch.StartNew()
        End If

        If Not userAcceptsLossOfChanges(modDocArr, "Release Locks, and revert changes to vault version?") Then Exit Sub

        'Same reasoning as unlockPathsLockedOnly above: gate the every-open-document save so it
        'cannot fire PlumVault's per-document save guard or queue automatic commits.
        beginInternalSolidWorksSave()
        Try
            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                saveAllOpenFiles(bShowError:=True)
                If debugWatch IsNot Nothing Then debugNotes.Add("Save open files: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
                saveAllOpenFiles(bShowError:=True)
            End Try
        Finally
            endInternalSolidWorksSave()
        End Try

        If IsNothing(modDocArr) Then
            If Not verifyLocalRepoPath() Then Exit Sub

            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                bSuccess = runTortoiseProcexeWithMonitor("/command:unlock /path:""" & myUserControl.localRepoPath.Text.TrimEnd("\"c) & """ /closeonend:3")
                If debugWatch IsNot Nothing Then debugNotes.Add("TortoiseSVN unlock whole working copy: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
                bSuccess = False
            End Try

            If Not bSuccess Then
                'Whole-working-copy Release Locks. Same reasoning as the selected-path variant:
                'a failed unlock means the locks are still held, so reverting every file in the
                'working copy here would discard the user's work while they still own the locks.
                Try
                    iSwApp.SendMsgToUser2(
                        "Releasing locks failed, so nothing was changed." & vbCrLf & vbCrLf &
                        "Your files are still locked and your local changes were kept." & vbCrLf & vbCrLf &
                        "Try Release Locks again. If it keeps failing, click Cleanup first, then retry.",
                        swMessageBoxIcon_e.swMbWarning,
                        swMessageBoxBtn_e.swMbOk
                    )
                Catch
                End Try

                Exit Sub
            End If

            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            myGetLatestOrRevert(modDocArr, getLatestType.revert)
            If debugWatch IsNot Nothing Then debugNotes.Add("Whole working-copy revert path: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

            If debugWatch IsNot Nothing Then
                debugNotes.Add("Total Release Locks time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
                showSvnTimingDebugWindow("Release Locks finished.", debugNotes)
            End If

            Exit Sub
        ElseIf UBound(modDocArr) = -1 Then
            Exit Sub
        End If

        Dim selectedPaths() As String = getExistingCadFilePathsFromDocs(modDocArr)

        If selectedPaths Is Nothing OrElse selectedPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No valid selected CAD file paths were found for Release Locks.", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If debugWatch IsNot Nothing Then debugNotes.Add("Selected files: " & selectedPaths.Length.ToString())

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

            'Speed fix: releasing your own locks does not need an SVN server status check.
            'The local working copy already knows whether you have a lock token (K).
            status = getFileSVNStatus(
                bCheckServer:=False,
                modDocArr:=modDocArr,
                bUpdateStatusOfAllOpenModels:=False
            )

            If debugWatch IsNot Nothing Then debugNotes.Add("Local SVN status for selected files: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        Catch
            status = Nothing
        End Try

        If IsNothing(status) Then
            iSwApp.SendMsgToUser2("Release Locks failed. Could not read local SVN status.", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Dim lockedPaths() As String = Nothing
        Dim modifiedPaths() As String = Nothing

        Try
            lockedPaths = getLockedPathsFromStatus(status)
        Catch
            lockedPaths = Nothing
        End Try

        Try
            'Only revert files you actually had locked.
            'This keeps Unlock && Revert With Dependents from checking/reverting every dependent.
            modifiedPaths = getLockedModifiedPathsFromStatus(status)
        Catch
            modifiedPaths = Nothing
        End Try

        If lockedPaths Is Nothing OrElse lockedPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No Selected Items were locked", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            If debugWatch IsNot Nothing Then
                debugNotes.Add("Locked files found: 0")
                debugNotes.Add("Total Release Locks time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
                showSvnTimingDebugWindow("Release Locks finished - nothing locked.", debugNotes)
            End If
            Exit Sub
        End If

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Locked files found: " & lockedPaths.Length.ToString())
            debugNotes.Add("Modified files needing revert: " & countStringArrayItems(modifiedPaths).ToString())
        End If

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            bSuccess = runTortoiseProcexeWithMonitor("/command:unlock /path:" & formatFilePathArrForProc(lockedPaths) & " /closeonend:3")
            If debugWatch IsNot Nothing Then debugNotes.Add("TortoiseSVN unlock selected files: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        Catch
            bSuccess = False
        End Try

        If Not bSuccess Then
            'Do not fall through to the revert below. The unlock did not complete, so these
            'files are almost certainly still locked by this working copy, and discarding the
            'local changes now would destroy the user's work while they still hold the lock -
            'the worst possible outcome for this action. The status-cache update further down
            'would additionally have recoloured the tree as unlocked, a state that was never
            'true. Leave the files and the displayed state untouched and let the user retry.
            Try
                iSwApp.SendMsgToUser2(
                    "Releasing locks failed, so nothing was changed." & vbCrLf & vbCrLf &
                    "Your files are still locked and your local changes were kept." & vbCrLf & vbCrLf &
                    "Try Release Locks again. If it keeps failing, click Cleanup first, then retry.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

            Exit Sub
        End If

        If modifiedPaths IsNot Nothing AndAlso modifiedPaths.Length > 0 Then
            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

                'Use the same local status object to detach/reconnect files before Tortoise overwrites them.
                status.releaseFileSystemAccessToRevertOrUpdateModels(iSwApp, New Integer() {-1})

                If debugWatch IsNot Nothing Then debugNotes.Add("Release SolidWorks file handles before revert: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
            End Try

            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                bSuccess = runTortoiseProcexeWithMonitor("/command:revert /path:" & formatFilePathArrForProc(modifiedPaths) & " /closeonend:3")
                If debugWatch IsNot Nothing Then debugNotes.Add("TortoiseSVN revert modified files: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

                If Not bSuccess Then iSwApp.SendMsgToUserv("Revert Files Failed.")
            Catch
            End Try

            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                status.reattachDocsToFileSystem(New Integer() {-1}, iSwApp)
                If debugWatch IsNot Nothing Then debugNotes.Add("Reattach docs after revert: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
            End Try
        End If

        Try
            updateStatusCacheForKnownPaths(lockedPaths, forceLock6:=" ")
            If modifiedPaths IsNot Nothing AndAlso modifiedPaths.Length > 0 Then
                updateStatusCacheForKnownPaths(modifiedPaths, forceAddDelChg1:=" ")
            End If
        Catch
        End Try

        'Read-only enforcement: the locks above are gone, so any of these documents still
        'open must go back to SOLIDWORKS' native read-only protection (gated + deferred).
        Try
            restoreInternalReadOnlyForReleasedPathsPublic(lockedPaths)
        Catch
        End Try

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            updateLockStatusPublic(bRefreshAllTreeViews:=False)
            refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)
            If debugWatch IsNot Nothing Then debugNotes.Add("Local status/tree refresh: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        Catch
        End Try

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Total Release Locks time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
            showSvnTimingDebugWindow("Release Locks finished.", debugNotes)
        End If
    End Sub

    Private Function isFirstCommitCandidatePath(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not File.Exists(filePath) Then Return False
        If Not isCadFilePath(filePath) Then Return False
        If Not isPathInsideLocalRepo(filePath) Then Return False

        Try
            Dim statusChar As Char = getFirstSvnStatusChar(filePath)

            '? = unversioned inside working copy
            'A = scheduled for add but never committed at this path
            Return statusChar = "?"c OrElse statusChar = "A"c
        Catch
            Return False
        End Try
    End Function

    Private Function allCommitPathsAreFirstCommitCandidates(ByVal commitPaths() As String) As Boolean
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return False

        Dim foundFirstCommitCad As Boolean = False

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For

            If Directory.Exists(p) Then
                Dim statusChar As Char = getFirstSvnStatusCharForPathDepthEmpty(p)

                'New parent folders are allowed in an automatic first commit.
                If statusChar = "A"c OrElse statusChar = "?"c Then Continue For

                'Already-versioned parent folders may be included only to support a child add.
                If statusChar = " "c Then Continue For

                Return False
            End If

            If Not isFirstCommitCandidatePath(p) Then
                Return False
            End If

            foundFirstCommitCad = True
        Next

        Return foundFirstCommitCad
    End Function

    Private Function getFirstCommitCandidateCadPaths(ByVal commitPaths() As String) As String()
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Not File.Exists(p) Then Continue For
            If Not isCadFilePath(p) Then Continue For
            If Not isFirstCommitCandidatePath(p) Then Continue For

            Dim normalizedPath As String = normalizeSvnPath(p)
            If String.IsNullOrWhiteSpace(normalizedPath) Then normalizedPath = p

            If seen.Add(normalizedPath) Then output.Add(normalizedPath)
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function autoCommitFirstDatasetPaths(ByVal commitPaths() As String, ByVal sCommitMessage As String) As Boolean
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return False

        Dim safeMessage As String = sCommitMessage

        If String.IsNullOrWhiteSpace(safeMessage) Then
            safeMessage = "Initial CAD commit from SolidWorks SVN add-in"
        End If

        safeMessage = safeMessage.Replace("""", "'")

        Try
            Dim processOutputArr() As rawProcessReturn = runSvnByArgs(
                commitPaths,
                "commit --non-interactive",
                "-m",
                """" & safeMessage & """",
                bEach:=False
            )

            If processOutputArr Is Nothing OrElse processOutputArr.Length = 0 Then Return False

            For Each processOutput As rawProcessReturn In processOutputArr
                If processOutput.outputError IsNot Nothing AndAlso processOutput.outputError.Trim() <> "" Then
                    iSwApp.SendMsgToUser2(
                        "Automatic first commit failed." & vbCrLf & vbCrLf &
                        processOutput.outputError.Trim() & vbCrLf & vbCrLf &
                        "The plugin will open the normal TortoiseSVN commit window instead.",
                        swMessageBoxIcon_e.swMbWarning,
                        swMessageBoxBtn_e.swMbOk
                    )
                    Return False
                End If
            Next

            iSwApp.SendMsgToUser2(
                "Initial commit completed." & vbCrLf & vbCrLf &
                "The new CAD dataset was added and pushed to SVN automatically.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )

            Return True

        Catch ex As Exception
            iSwApp.SendMsgToUser2(
                "Automatic first commit failed." & vbCrLf & vbCrLf &
                ex.Message & vbCrLf & vbCrLf &
                "The plugin will open the normal TortoiseSVN commit window instead.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End Try
    End Function

    Private Function tryMakeFirstCommitDocWritable(ByVal doc As ModelDoc2) As Boolean
        If doc Is Nothing Then Return False

        Dim docPath As String = ""

        Try
            docPath = doc.GetPathName()
        Catch
            docPath = ""
        End Try

        If String.IsNullOrWhiteSpace(docPath) Then Return False
        If Not isFirstCommitCandidatePath(docPath) Then Return False

        Try
            File.SetAttributes(docPath, File.GetAttributes(docPath) And Not FileAttributes.ReadOnly)
        Catch
        End Try

        Try
            doc.SetReadOnlyState(False)
        Catch
        End Try

        Return True
    End Function

    Private Sub makeFirstCommitCandidatesWritable(ByRef modDocArr() As ModelDoc2)
        If modDocArr Is Nothing Then Exit Sub

        For Each doc As ModelDoc2 In modDocArr
            Try
                tryMakeFirstCommitDocWritable(doc)
            Catch
            End Try
        Next
    End Sub

    Sub tortCommitDocs(ByRef modDocArr() As ModelDoc2, Optional sCommitMessage As String = "", Optional bIncludeDependents As Boolean = False)
        Dim bSuccess As Boolean = False
        Dim sErrorFiles As String = ""
        Dim i As Integer
        Dim j As Integer = 0
        Dim sModDocPathArr As String()

        Dim activeDoc As ModelDoc2 = iSwApp.ActiveDoc
        If activeDoc Is Nothing Then Exit Sub

        'If bRequiredDoc Is Nothing Then bRequiredDoc = svnAddInUtils.createBoolArray(UBound(modDocArr), True)

        If modDocArr Is Nothing Then
            iSwApp.SendMsgToUser("Active Document not found")
            Exit Sub
        ElseIf modDocArr.Length = 0 Then
            iSwApp.SendMsgToUser("Active Document not found")
            Exit Sub
        End If

        Dim docsForExternalRefCheck As ModelDoc2() = modDocArr

        If bIncludeDependents Then
            Try
                For Each docToCheck As ModelDoc2 In modDocArr
                    If docToCheck Is Nothing Then Continue For
                    If docToCheck.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                        docsForExternalRefCheck = myUserControl.getComponentsOfAssemblyOptionalUpdateTree(
                    modDocArr,
                    bResolveLightweight:=True
                )
                        Exit For
                    End If
                Next
            Catch
                docsForExternalRefCheck = modDocArr
            End Try
        End If

        'Reference changes must be protected by the selected/committed assembly, not necessarily the active/top-level assembly.
        If Not targetAssembliesMustBeLockedForReferenceChanges(modDocArr) Then Exit Sub

        If Not prepareExternalReferencesForSvnAction(docsForExternalRefCheck) Then Exit Sub

        'After external/vendor CAD is copied and relinked, rebuild the commit array only for the explicit
        'With Dependents path. Normal assembly commit stays assembly-file-only for speed.
        If bIncludeDependents Then
            Try
                For Each docToCheck As ModelDoc2 In modDocArr
                    If docToCheck Is Nothing Then Continue For

                    If docToCheck.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                        modDocArr = myUserControl.getComponentsOfAssemblyOptionalUpdateTree(
                    modDocArr,
                    bResolveLightweight:=True
                )
                        Exit For
                    End If
                Next
            Catch
            End Try
        End If

        If Not validateCadNamesBeforeCommit(modDocArr) Then Exit Sub

        Dim docsForDuplicateCheck As ModelDoc2() = modDocArr

        If bIncludeDependents Then
            Try
                For Each d As ModelDoc2 In modDocArr
                    If d IsNot Nothing AndAlso d.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                        docsForDuplicateCheck = myUserControl.getComponentsOfAssemblyOptionalUpdateTree(
                    modDocArr,
                    bResolveLightweight:=True
                )
                        Exit For
                    End If
                Next
            Catch
                docsForDuplicateCheck = modDocArr
            End Try
        End If

        If Not validateNoDuplicateCadFileNames(docsForDuplicateCheck) Then Exit Sub
        If Not commitAllowedOnlyIfUpToDate(modDocArr, bIncludeDependents:=bIncludeDependents) Then Exit Sub

        'First-commit CAD files are not lockable yet because SVN does not know them until add/commit.
        'Make them writable before the normal read-only filter runs.
        makeFirstCommitCandidatesWritable(modDocArr)

        'Filter out read-only files.
        'Exception: brand-new first-commit CAD cannot be locked yet, so keep it in the commit list
        'after forcing it writable. This is what allows initial datasets to commit without Get Locks.
        For i = 0 To UBound(modDocArr)
            If modDocArr(i) Is Nothing Then
                j += 1
                Continue For
            End If

            Dim currentCommitDocPath As String = ""

            Try
                currentCommitDocPath = modDocArr(i).GetPathName()
            Catch
                currentCommitDocPath = ""
            End Try

            If isFirstCommitCandidatePath(currentCommitDocPath) Then
                tryMakeFirstCommitDocWritable(modDocArr(i))

                If modDocArr(i).IsOpenedViewOnly() Then
                    modDocArr(i) = Nothing
                    j += 1
                End If

                Continue For
            End If

            If modDocArr(i).IsOpenedReadOnly() Or modDocArr(i).IsOpenedViewOnly() Then

                'If bRequiredDoc(i) Then
                '    sErrorFiles &= modDocArr(i).GetPathName & vbCrLf
                'End If
                modDocArr(i) = Nothing
                j += 1
            End If
        Next

        If j = i Then
            'If sErrorFiles <> "" Then
            iSwApp.SendMsgToUser("The file(s) are all Read-Only. You need write access to check in. " &
                                 "If you believe you have the file locked, you can try File > Reload")
            Exit Sub 'All Files were removed
        End If
        sModDocPathArr = filterCommitPathsInsideRepoOnly(getFilePathsFromModDocArr(modDocArr))

        If pendingExternalRefCommitPaths IsNot Nothing AndAlso pendingExternalRefCommitPaths.Count > 0 Then
            Dim mergedCommitPaths As New List(Of String)

            If sModDocPathArr IsNot Nothing Then
                mergedCommitPaths.AddRange(sModDocPathArr)
            End If

            For Each pendingPath As String In pendingExternalRefCommitPaths
                If String.IsNullOrWhiteSpace(pendingPath) Then Continue For
                If Not pathExistsAsFileOrDirectory(pendingPath) Then Continue For
                If Not isPathInsideLocalRepo(pendingPath) Then Continue For

                Dim alreadyIncluded As Boolean = mergedCommitPaths.Any(
            Function(existingPath)
                If String.IsNullOrWhiteSpace(existingPath) Then Return False
                Return String.Equals(
                    Path.GetFullPath(existingPath),
                    Path.GetFullPath(pendingPath),
                    StringComparison.OrdinalIgnoreCase
                )
            End Function
        )

                If Not alreadyIncluded Then
                    mergedCommitPaths.Add(pendingPath)
                End If
            Next

            sModDocPathArr = filterCommitPathsInsideRepoOnly(mergedCommitPaths.ToArray())
        End If

        If sModDocPathArr Is Nothing OrElse sModDocPathArr.Length = 0 Then
            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "No valid SVN working-copy CAD paths were available to commit." & vbCrLf & vbCrLf &
                "This usually means SolidWorks is still pointing to a file outside the SVN folder.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub
        End If

        sModDocPathArr = expandAssemblyCommitPathsWithNewFirstCommitChildren(sModDocPathArr)
        sModDocPathArr = filterCommitPathsInsideRepoOnly(sModDocPathArr)

        If sModDocPathArr Is Nothing OrElse sModDocPathArr.Length = 0 Then Exit Sub

        sModDocPathArr = expandCommitPathsWithAddedParentDirectories(sModDocPathArr)
        sModDocPathArr = filterCommitPathsInsideRepoOnly(sModDocPathArr)

        If sModDocPathArr Is Nothing OrElse sModDocPathArr.Length = 0 Then Exit Sub

        runSvnByArgs(sModDocPathArr, "add", bEach:=True)  'adds any not added.

        Dim bAutoFirstCommitDataset As Boolean = allCommitPathsAreFirstCommitCandidates(sModDocPathArr)

        svnPropset(sModDocPathArr, "addin:release_state", "||EDIT||")

        Dim saveResult As swMessageBoxResult_e

        beginInternalSolidWorksSave()
        Try
            saveResult = save3AndShowErrorMessages(modDocArr)
        Finally
            endInternalSolidWorksSave()
        End Try

        If saveResult <> swMessageBoxResult_e.swMbHitYes Then Exit Sub

        'Run the upload/commit portion in the background so SolidWorks stays usable.
        'All SolidWorks API work above this point has already finished on the main thread.
        startCommitProcessBackground(sModDocPathArr, sCommitMessage, bAutoFirstCommitDataset)
        Exit Sub
    End Sub
    Public Sub tortCommitPathsAsync(ByVal commitPaths() As String,
                                    Optional sCommitMessage As String = "",
                                    Optional suppressParentAssemblyNotice As Boolean = False)
        'Path-first commit used by the add-in tree.
        'This lets a user commit the selected child part without requiring the parent assembly
        'to be checked out, as long as the child itself is valid/current/writable.
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No CAD file paths were selected for Commit.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If asyncCommitInProgress Then
            iSwApp.SendMsgToUser2("A Commit operation is already running in the background.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Dim sModDocPathArr() As String = filterCommitPathsInsideRepoOnly(commitPaths)

        If sModDocPathArr Is Nothing OrElse sModDocPathArr.Length = 0 Then
            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "No valid SVN working-copy CAD paths were available to commit.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub
        End If

        'Assembly safety with almost no normal slowdown:
        'Only when the selected commit target is an open assembly, locally check for external CAD refs.
        'If external CAD is found, prompt vendor vs normal CAD, copy into the proper SVN folder, relink, and commit it too.
        If Not prepareExternalReferencesForCommitPaths(sModDocPathArr) Then Exit Sub

        'Manual Commit encourages virtual components to become normal external SVN files.
        'The review table defaults to Save externally in the physical owner assembly folder,
        'but a deliberate Keep embedded choice preserves the supported virtual workflow.
        If Not prepareVirtualComponentsForManualCommit(sModDocPathArr) Then Exit Sub

        'If this is a brand-new assembly dataset, include its brand-new referenced CAD files as well.
        'This is local-only and only runs for first-commit assemblies.
        sModDocPathArr = expandFirstCommitAssemblyDatasetPaths(sModDocPathArr)
        sModDocPathArr = expandAssemblyCommitPathsWithNewFirstCommitChildren(sModDocPathArr)
        sModDocPathArr = filterCommitPathsInsideRepoOnly(sModDocPathArr)

        If sModDocPathArr Is Nothing OrElse sModDocPathArr.Length = 0 Then
            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "No valid SVN working-copy CAD paths were available after preparing the commit.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub
        End If

        'One live local status pass is authoritative for every lock-dependent commit check below.
        'It also reconciles positive K tokens into the display cache so stale tree rows cannot
        'force a second Get Locks operation.
        Dim liveLockedCommitPaths As HashSet(Of String) = getLiveLockedManagedCadPaths(sModDocPathArr)
        refreshCachedLockTokensFromWorkingCopy(sModDocPathArr, liveLockedCommitPaths)

        If Not validateCadPathNamesBeforeCommit(sModDocPathArr) Then Exit Sub
        If Not validateNoDuplicateCadFileNamesForPaths(sModDocPathArr) Then Exit Sub
        If Not commitPathsAllowedOnlyIfUpToDate(sModDocPathArr) Then Exit Sub
        If Not commitAssemblyChildrenAllowedOnlyIfCachedUpToDate(sModDocPathArr) Then Exit Sub
        If Not automaticSaveCommitPathsHaveRequiredLocks(
            sModDocPathArr,
            operationLabel:="Commit",
            retryInstruction:="Click Sync to refresh status if these locks were changed outside PlumVault; otherwise use Get Locks and try Commit again.",
            knownLiveLockedPaths:=liveLockedCommitPaths
        ) Then Exit Sub
        If Not ensureLiveLockedCommitPathsWritable(sModDocPathArr, liveLockedCommitPaths) Then Exit Sub

        makeFirstCommitCandidatePathsWritable(sModDocPathArr)

        sModDocPathArr = expandCommitPathsWithAddedParentDirectories(sModDocPathArr)
        sModDocPathArr = filterCommitPathsInsideRepoOnly(sModDocPathArr)

        If sModDocPathArr Is Nothing OrElse sModDocPathArr.Length = 0 Then
            iSwApp.SendMsgToUser2("Commit blocked." & vbCrLf & vbCrLf &
                "No valid SVN working-copy paths were available after adding parent folders.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        runSvnByArgs(sModDocPathArr, "add", bEach:=True)

        Dim bAutoFirstCommitDataset As Boolean = allCommitPathsAreFirstCommitCandidates(sModDocPathArr)

        svnPropset(sModDocPathArr, "addin:release_state", "||EDIT||")

        If Not saveOpenDocsForCommitPaths(sModDocPathArr) Then Exit Sub

        startCommitProcessBackground(sModDocPathArr, sCommitMessage, bAutoFirstCommitDataset)

        'Show this informational note only after asyncCommitInProgress is set. SendMsgToUser2
        'pumps UI messages; showing it first allowed a queued save-triggered auto-commit to
        'start re-entrantly before this manual commit had claimed the same new child.
        If Not suppressParentAssemblyNotice Then
            warnIfActiveAssemblyDirtyButNotInCommit(sModDocPathArr)
        End If
    End Sub

    Private Function getLiveLockedManagedCadPaths(ByVal filePaths() As String) As HashSet(Of String)
        Dim liveLockedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        If filePaths Is Nothing Then Return liveLockedPaths

        For Each filePath As String In filePaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For
            If Directory.Exists(filePath) OrElse Not File.Exists(filePath) Then Continue For
            If Not isCadFilePath(filePath) OrElse Not isPathInsideLocalRepo(filePath) Then Continue For
            If isFirstCommitCandidatePath(filePath) Then Continue For

            If userHasLocalSvnLockTokenForPath(filePath, allowCachedToken:=False) Then
                liveLockedPaths.Add(normalizeFullPathSafe(filePath))
            End If
        Next

        Return liveLockedPaths
    End Function

    Private Sub refreshCachedLockTokensFromWorkingCopy(ByVal filePaths() As String,
                                                        Optional ByVal knownLiveLockedPaths As HashSet(Of String) = Nothing)
        If filePaths Is Nothing Then Exit Sub

        Dim liveLockedPaths As HashSet(Of String) = If(
            knownLiveLockedPaths,
            getLiveLockedManagedCadPaths(filePaths)
        )

        If liveLockedPaths.Count > 0 Then
            updateStatusCacheForKnownPaths(liveLockedPaths.ToArray(), forceLock6:="K")
        End If
    End Sub

    Private Function ensureLiveLockedCommitPathsWritable(ByVal filePaths() As String,
                                                          Optional ByVal knownLiveLockedPaths As HashSet(Of String) = Nothing) As Boolean
        If filePaths Is Nothing Then Return False

        For Each filePath As String In filePaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For
            If Directory.Exists(filePath) OrElse Not File.Exists(filePath) Then Continue For
            If Not isCadFilePath(filePath) OrElse Not isPathInsideLocalRepo(filePath) Then Continue For
            If isFirstCommitCandidatePath(filePath) Then Continue For
            Dim hasRequiredLock As Boolean = If(
                knownLiveLockedPaths Is Nothing,
                userHasLocalSvnLockTokenForPath(filePath, allowCachedToken:=False),
                knownLiveLockedPaths.Contains(normalizeFullPathSafe(filePath))
            )
            If Not hasRequiredLock Then Continue For

            Try
                File.SetAttributes(filePath, File.GetAttributes(filePath) And Not FileAttributes.ReadOnly)

                'This early phase verifies only the on-disk attribute. saveOpenDocsForCommitPaths
                'performs a synchronous live transition later, immediately before Save3 and only
                'for an open, dirty commit target. Keeping that transition at the save boundary
                'avoids broad writable-state changes across clean sibling documents.
                If (File.GetAttributes(filePath) And FileAttributes.ReadOnly) <> 0 Then
                    Throw New InvalidOperationException("The working-copy file remained read-only on disk.")
                End If
            Catch ex As Exception
                iSwApp.SendMsgToUser2(
                    "Commit blocked." & vbCrLf & vbCrLf &
                    "The SVN lock exists, but PlumVault could not make the working-copy file writable:" & vbCrLf &
                    Path.GetFileName(filePath) & vbCrLf & vbCrLf &
                    ex.Message & vbCrLf & vbCrLf &
                    "Click Sync to refresh status, then try Commit again.",
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End Try
        Next

        Return True
    End Function

    Private Function validateNoDuplicateCadFileNamesForPaths(ByVal filePaths() As String) As Boolean
        If filePaths Is Nothing Then Return True

        Dim seenNames As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)
        Dim duplicateMsg As String = ""

        For Each docPath As String In filePaths
            If String.IsNullOrWhiteSpace(docPath) Then Continue For
            If Not isCadFilePath(docPath) Then Continue For

            Dim fileName As String = Path.GetFileName(docPath)

            If seenNames.ContainsKey(fileName) Then
                duplicateMsg &= fileName & vbCrLf &
                            "1) " & seenNames(fileName) & vbCrLf &
                            "2) " & docPath & vbCrLf & vbCrLf
            Else
                seenNames(fileName) = docPath
            End If
        Next

        If duplicateMsg <> "" Then
            iSwApp.SendMsgToUser2(
            "Commit blocked." & vbCrLf & vbCrLf &
            "Duplicate CAD file names were found in this commit." & vbCrLf &
            "Each CAD file must have a unique file name." & vbCrLf & vbCrLf &
            duplicateMsg &
            "Rename one of the duplicate files before committing.",
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )

            Return False
        End If

        Return True
    End Function

    Private Function validateCadPathNamesBeforeCommit(ByRef filePaths() As String) As Boolean
        If filePaths Is Nothing Then Return True

        For i As Integer = 0 To UBound(filePaths)
            Dim docPath As String = filePaths(i)

            If String.IsNullOrWhiteSpace(docPath) Then Continue For
            If Not isCadFilePath(docPath) Then Continue For

            If shouldIgnoreGrc27NamingConventionForDebug() Then Continue For
            If shouldSkipNameCheckForPendingExternalRef(docPath) Then Continue For
            If isVendorPartPath(docPath) Then Continue For

            If Not isValidGrc27FileName(docPath) Then
                Dim openDoc As ModelDoc2 = getOpenModelByPathSafe(docPath)

                If openDoc IsNot Nothing Then
                    Dim result As swMessageBoxResult_e = iSwApp.SendMsgToUser2(
                        "This CAD file does not follow the GRC27/CFD27 naming convention:" & vbCrLf & vbCrLf &
                        Path.GetFileName(docPath) & vbCrLf & vbCrLf &
                        "Would you like to rename it now?",
                        swMessageBoxIcon_e.swMbWarning,
                        swMessageBoxBtn_e.swMbYesNo
                    )

                    If result <> swMessageBoxResult_e.swMbHitYes Then Return False
                    If Not renameCadFileToGrc27Name(openDoc) Then Return False

                    Try
                        filePaths(i) = openDoc.GetPathName()
                    Catch
                    End Try
                Else
                    iSwApp.SendMsgToUser2(
                        "Commit blocked." & vbCrLf & vbCrLf &
                        "This CAD file does not follow the GRC27/CFD27 naming convention:" & vbCrLf & vbCrLf &
                        Path.GetFileName(docPath) & vbCrLf & vbCrLf &
                        "Open the file and rename it, or enable Debug: ignore naming for testing.",
                        swMessageBoxIcon_e.swMbStop,
                        swMessageBoxBtn_e.swMbOk
                    )
                    Return False
                End If
            End If
        Next

        filePaths = filterCommitPathsInsideRepoOnly(filePaths)
        Return filePaths IsNot Nothing AndAlso filePaths.Length > 0
    End Function

    Private Function commitAssemblyChildrenAllowedOnlyIfCachedUpToDate(ByVal commitPaths() As String) As Boolean
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return False

        Dim hasAssembly As Boolean = False

        Try
            For Each commitPath As String In commitPaths
                If String.IsNullOrWhiteSpace(commitPath) Then Continue For

                If String.Equals(Path.GetExtension(commitPath), ".SLDASM", StringComparison.OrdinalIgnoreCase) Then
                    hasAssembly = True
                    Exit For
                End If
            Next
        Catch
            hasAssembly = False
        End Try

        If Not hasAssembly Then Return True

        'Fast cache-only assembly safety check.
        '
        'Important behavior:
        '  1. Never contact the SVN server from Commit.
        '  2. Check only paths already present in the lazily loaded tree.
        '  3. Block any loaded child that the most recent Sync cache positively marks as out of date.
        '  4. Do NOT block merely because a child has no cache entry.
        '
        'The previous implementation required usable server-aware cache data for every loaded child.
        'That created false commit blocks after adding/relinking a new vendor part because:
        '  - the new vendor file has no server revision yet, and
        '  - other lazily loaded children may not have been included in the last bounded branch Sync.
        '
        'SVN itself still prevents an out-of-date direct commit target from being committed, while this
        'guard preserves the useful early warning for any child that Sync has already proven is stale.
        Dim guardPaths() As String = Nothing

        Try
            If myUserControl IsNot Nothing Then
                guardPaths = myUserControl.getAssemblyCommitGuardPathsForPathsPublic(commitPaths)
            End If
        Catch
            guardPaths = Nothing
        End Try

        If guardPaths Is Nothing OrElse guardPaths.Length = 0 Then
            guardPaths = commitPaths
        End If

        Dim outOfDatePaths As New List(Of String)()
        Dim checkedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each guardPath As String In guardPaths
            If String.IsNullOrWhiteSpace(guardPath) Then Continue For
            If Not File.Exists(guardPath) Then Continue For
            If Not isCadFilePath(guardPath) Then Continue For

            'Old external paths can remain visible in the tree briefly after a vendor relink.
            'They are outside the working copy and are irrelevant to SVN freshness checking.
            If Not isPathInsideLocalRepo(guardPath) Then Continue For

            Dim normalizedGuardPath As String = normalizeSvnPath(guardPath)
            If String.IsNullOrWhiteSpace(normalizedGuardPath) Then Continue For
            If checkedPaths.Contains(normalizedGuardPath) Then Continue For
            checkedPaths.Add(normalizedGuardPath)

            'Newly copied vendor/external files are intentionally part of this first commit.
            'They have no server revision and therefore cannot have server-aware cache data yet.
            Dim isPendingExternalFirstCommit As Boolean = False

            Try
                If pendingExternalRefCommitPaths IsNot Nothing Then
                    For Each pendingPath As String In pendingExternalRefCommitPaths
                        If pathsAreSame(pendingPath, guardPath) Then
                            isPendingExternalFirstCommit = True
                            Exit For
                        End If
                    Next
                End If
            Catch
                isPendingExternalFirstCommit = False
            End Try

            If isPendingExternalFirstCommit Then Continue For

            Dim cached As SVNStatus.filePpty = Nothing
            Dim found As Boolean = False

            Try
                found = tryFindCachedStatusProperty(guardPath, cached)
            Catch
                found = False
            End Try

            'No cache entry is not proof that the file is stale.  Ignore it and preserve lazy Sync.
            If Not found Then Continue For

            'A/? files are first-commit candidates and cannot be stale against a server revision.
            If cached.addDelChg1 = "?" OrElse cached.addDelChg1 = "A" Then Continue For

            'Only a positive remote "*" marker from a server-aware Sync blocks the assembly commit.
            If cached.upToDate9 IsNot Nothing AndAlso
               Not String.Equals(cached.upToDate9, "NoUpdate", StringComparison.OrdinalIgnoreCase) AndAlso
               cached.upToDate9 = "*" Then
                outOfDatePaths.Add(guardPath)
            End If
        Next

        If outOfDatePaths.Count > 0 Then
            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "This assembly has one or more loaded children marked out of date by the last Sync cache." & vbCrLf & vbCrLf &
                "Out-of-date child/related files:" & vbCrLf &
                stringArrToSingleStringWithNewLines(outOfDatePaths.ToArray(), bTrimFileNames:=True, iLimit:=10) & vbCrLf &
                "Use Get Latest, verify the assembly, then commit again.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End If

        Return True
    End Function

    Private Function commitPathsAllowedOnlyIfUpToDate(ByVal commitPaths() As String) As Boolean
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return False

        'Fast path: never contact the SVN server from Commit.
        'If the last Sync cache already says a selected path is stale, block it immediately.
        'If a non-assembly file has no cache entry, let SVN enforce its own base-revision check
        'during the actual commit.  Assembly children are handled by the stricter cache guard below.
        Dim outOfDatePaths As New List(Of String)()
        Dim checkedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each commitPath As String In commitPaths
            If String.IsNullOrWhiteSpace(commitPath) Then Continue For
            If Directory.Exists(commitPath) Then Continue For
            If Not File.Exists(commitPath) Then Continue For
            If Not isCadFilePath(commitPath) Then Continue For
            If Not isPathInsideLocalRepo(commitPath) Then Continue For

            Dim normalizedPath As String = normalizeSvnPath(commitPath)
            If String.IsNullOrWhiteSpace(normalizedPath) Then Continue For
            If checkedPaths.Contains(normalizedPath) Then Continue For
            checkedPaths.Add(normalizedPath)

            Dim cached As SVNStatus.filePpty = Nothing
            Dim found As Boolean = False

            Try
                found = tryFindCachedStatusProperty(commitPath, cached)
            Catch
                found = False
            End Try

            If Not found Then Continue For
            If cached.addDelChg1 = "?" OrElse cached.addDelChg1 = "A" Then Continue For
            If cached.upToDate9 Is Nothing Then Continue For
            If String.Equals(cached.upToDate9, "NoUpdate", StringComparison.OrdinalIgnoreCase) Then Continue For

            If cached.upToDate9 = "*" Then
                outOfDatePaths.Add(commitPath)
            End If
        Next

        If outOfDatePaths.Count > 0 Then
            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "One or more selected files are marked out of date by the last Sync cache." & vbCrLf & vbCrLf &
                "Use Get Latest, confirm the geometry, then commit again." & vbCrLf & vbCrLf &
                "Out-of-date files:" & vbCrLf &
                stringArrToSingleStringWithNewLines(outOfDatePaths.ToArray(), bTrimFileNames:=True, iLimit:=10),
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End If

        Return True
    End Function

    Private Sub makeFirstCommitCandidatePathsWritable(ByVal commitPaths() As String)
        If commitPaths Is Nothing Then Exit Sub

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Not isFirstCommitCandidatePath(p) Then Continue For

            Try
                If File.Exists(p) Then
                    File.SetAttributes(p, File.GetAttributes(p) And Not FileAttributes.ReadOnly)
                End If
            Catch
            End Try

            Try
                Dim doc As ModelDoc2 = getOpenModelByPathSafe(p)
                If doc IsNot Nothing Then doc.SetReadOnlyState(False)
            Catch
            End Try
        Next
    End Sub

    Private Function saveOpenDocsForCommitPaths(ByVal commitPaths() As String) As Boolean
        If commitPaths Is Nothing Then Return True

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For

            Dim doc As ModelDoc2 = getOpenModelByPathSafe(p)
            If doc Is Nothing Then Continue For

            'The optimized vendor relink already saves the assembly once.
            'Do not save/evaluate a large clean assembly again immediately before Commit.
            Dim documentIsDirty As Boolean = True

            Try
                documentIsDirty = doc.GetSaveFlag()
            Catch
                documentIsDirty = True
            End Try

            If Not documentIsDirty Then Continue For

            Try
                Dim writableFailure As String = ""

                If Not ensureOpenCadDocumentWritableNow(p, doc, writableFailure) Then
                    iSwApp.SendMsgToUser2(
                        "Commit blocked." & vbCrLf & vbCrLf &
                        "The selected CAD file has unsaved changes, but SOLIDWORKS still has it open read-only:" & vbCrLf &
                        p & vbCrLf & vbCrLf &
                        writableFailure,
                        swMessageBoxIcon_e.swMbStop,
                        swMessageBoxBtn_e.swMbOk
                    )
                    Return False
                End If

                Dim errors As Integer = 0
                Dim warnings As Integer = 0

                Dim saveSucceeded As Boolean = False

                beginInternalSolidWorksSave()
                Try
                    saveSucceeded = doc.Save3(swSaveAsOptions_e.swSaveAsOptions_Silent, errors, warnings)
                Finally
                    endInternalSolidWorksSave()
                End Try

                If Not saveSucceeded Then
                    iSwApp.SendMsgToUser2(
                        "Commit blocked." & vbCrLf & vbCrLf &
                        "Could not save the selected CAD file before commit:" & vbCrLf &
                        p & vbCrLf & vbCrLf &
                        "SOLIDWORKS errors: " & errors.ToString() & "; warnings: " & warnings.ToString() & vbCrLf & vbCrLf &
                        "The SVN lock and writable state were verified. Review the document for an active command or unresolved rebuild, then try again.",
                        swMessageBoxIcon_e.swMbStop,
                        swMessageBoxBtn_e.swMbOk
                    )
                    Return False
                End If
            Catch ex As Exception
                iSwApp.SendMsgToUser2(
                    "Commit blocked." & vbCrLf & vbCrLf &
                    "Could not save the selected CAD file before commit:" & vbCrLf &
                    p & vbCrLf & vbCrLf &
                    ex.Message,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk
                )
                Return False
            End Try
        Next

        Return True
    End Function

    Private Sub warnIfActiveAssemblyDirtyButNotInCommit(ByVal commitPaths() As String)
        Try
            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDoc Is Nothing Then Exit Sub
            If activeDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Exit Sub
            If Not activeDoc.GetSaveFlag() Then Exit Sub

            Dim activePath As String = activeDoc.GetPathName()
            If String.IsNullOrWhiteSpace(activePath) Then Exit Sub

            For Each p As String In commitPaths
                If pathsAreSame(activePath, p) Then Exit Sub
            Next

            iSwApp.SendMsgToUser2(
                "Selected file commit started." & vbCrLf & vbCrLf &
                "The active parent assembly was not included in this commit. SOLIDWORKS currently marks that parent as modified; this can be normal after in-context child editing or a rebuild." & vbCrLf & vbCrLf &
                "Commit and lock the parent assembly only if you intentionally changed assembly-level mates, component positions, references, suppression, or display/configuration state.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
        Catch
        End Try
    End Sub

    Sub tortCommitDocsAsync(ByRef modDocArr() As ModelDoc2, Optional sCommitMessage As String = "", Optional bIncludeDependents As Boolean = False)
        'Fast normal commit path:
        'For normal Commit, do not run the heavier document/dependency workflow.
        'Convert the selected/open documents to file paths and use the path-first async commit.
        'The explicit Commit With Dependents command still uses the heavier synchronous preparation path.
        If bIncludeDependents Then
            tortCommitDocs(modDocArr, sCommitMessage, bIncludeDependents:=True)
            Exit Sub
        End If

        Dim commitPaths() As String = Nothing

        Try
            commitPaths = getFilePathsFromModDocArr(modDocArr)
        Catch
            commitPaths = Nothing
        End Try

        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No CAD file paths were selected for Commit.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        tortCommitPathsAsync(commitPaths, sCommitMessage)
    End Sub

    Private Sub startCommitProcessBackground(ByVal commitPaths() As String,
                                             ByVal sCommitMessage As String,
                                             ByVal bAutoFirstCommitDataset As Boolean)
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Exit Sub

        If asyncCommitInProgress Then
            iSwApp.SendMsgToUser2("A Commit operation is already running in the background.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        'The user's explicit Commit owns these paths. Remove any older save-triggered request
        'for the same files before a modal/Tortoise window can pump it as a second commit.
        claimPendingAutomaticSaveCommitPathsForManualCommit(commitPaths)

        Dim pathsForBackground() As String = CType(commitPaths.Clone(), String())
        Dim savedPathForBackground As String = ""
        Dim repoRootForBackground As String = ""
        Dim tortoiseArgs As String = ""
        Dim commitMessageForBackground As String = sCommitMessage

        Try
            savedPathForBackground = myUserControl.savedPATH
        Catch
            savedPathForBackground = ""
        End Try

        Try
            repoRootForBackground = myUserControl.localRepoPath.Text
        Catch
            repoRootForBackground = ""
        End Try

        Try
            tortoiseArgs = "/command:commit /path:" &
                formatFilePathArrForProc(pathsForBackground) &
                " /logmsg:""" & commitMessageForBackground.Replace("""", "'") & """" &
                " /closeonend:3"
        Catch
            tortoiseArgs = ""
        End Try

        asyncCommitInProgress = True

        Try
            myUserControl.markCommitPendingForFilePathsPublic(pathsForBackground, True, "Committing...")
        Catch
        End Try

        Task.Run(Sub()
                     Dim success As Boolean = False
                     Dim errorMessage As String = ""

                     Try
                         If bAutoFirstCommitDataset Then
                             success = autoCommitFirstDatasetPathsBackground(pathsForBackground, commitMessageForBackground, savedPathForBackground, errorMessage)
                         Else
                             success = runTortoiseProcBackgroundNoUi(tortoiseArgs, repoRootForBackground, pathsForBackground, savedPathForBackground, errorMessage)
                         End If

                     Catch ex As Exception
                         success = False
                         errorMessage = ex.Message
                     End Try

                     Try
                         If myUserControl IsNot Nothing AndAlso myUserControl.IsHandleCreated Then
                             myUserControl.BeginInvoke(
                                 New MethodInvoker(
                                     Sub()
                                         finishCommitProcessOnMainThread(
                                             pathsForBackground,
                                             success,
                                             errorMessage,
                                             bAutoFirstCommitDataset
                                         )
                                     End Sub
                                 )
                             )
                         Else
                             asyncCommitInProgress = False
                         End If
                     Catch
                         asyncCommitInProgress = False
                     End Try
                 End Sub)
    End Sub

    Private Sub finishCommitProcessOnMainThread(ByVal commitPaths() As String,
                                                ByVal success As Boolean,
                                                ByVal errorMessage As String,
                                                ByVal bAutoFirstCommitDataset As Boolean)
        asyncCommitInProgress = False

        Try
            myUserControl.markCommitPendingForFilePathsPublic(commitPaths, False)
        Catch
        End Try

        If Not success Then
            'Do not leave the selected nodes looking committed when the TortoiseSVN
            'dialog was cancelled or only some selected paths were committed.
            Try
                updateLockStatusPublic(bRefreshAllTreeViews:=False)
                refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)
            Catch
            End Try

            iSwApp.SendMsgToUser2("Commit did not complete." & vbCrLf & vbCrLf & errorMessage,
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk)

            Try
                RaiseEvent CloseReviewCommitCompleted(commitPaths, False, errorMessage)
            Catch
            End Try

            processPendingAutomaticSaveCommits()
            Exit Sub
        End If

        Try
            myUserControl.markCommitResultForFilePathsPublic(commitPaths, True)
        Catch
        End Try

        dropCleanAutomaticSaveCommitDuplicates(commitPaths)

        Try
            'A TortoiseSVN commit may retain or release locks depending on its dialog state.
            'Do not guess that every lock was released; refresh live status and reconcile the
            'active document's write access from the authoritative result.
            updateStatusCacheForKnownPaths(commitPaths, forceAddDelChg1:=" ", forceUpToDate9:=" ")
            'The task-pane tree is rendered from statusOfAllOpenModels, not only the keyed
            'cache. Refresh local working-copy state immediately so a lock released by Commit
            'does not remain green until the user manually clicks Refresh.
            updateLockStatusPublic(bRefreshAllTreeViews:=False)
            refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)
        Catch
        End Try

        Try
            If bAutoFirstCommitDataset Then
                iSwApp.SendMsgToUser2("Initial commit completed." & vbCrLf & vbCrLf &
                    "The new CAD dataset was added and pushed to SVN automatically." & vbCrLf & vbCrLf &
                    "The plugin will now automatically get locks again so the new files stay writable for editing.",
                    swMessageBoxIcon_e.swMbInformation,
                    swMessageBoxBtn_e.swMbOk)

                Try
                    getLocksOfPathsAsync(commitPaths, bBreakLocks:=False, bUseTortoise:=False, sMessage:="Auto-lock after initial commit")
                Catch
                End Try
            End If
        Catch
        End Try

        Try
            RaiseEvent CloseReviewCommitCompleted(commitPaths, True, "")
        Catch
        End Try

        processPendingAutomaticSaveCommits()
    End Sub

    Private Function autoCommitFirstDatasetPathsBackground(ByVal commitPaths() As String,
                                                           ByVal sCommitMessage As String,
                                                           ByVal savedPathForBackground As String,
                                                           ByRef errorMessage As String) As Boolean
        errorMessage = ""

        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then
            errorMessage = "No commit paths were supplied."
            Return False
        End If

        Dim safeMessage As String = sCommitMessage

        If String.IsNullOrWhiteSpace(safeMessage) Then
            safeMessage = "Initial CAD commit from SolidWorks SVN add-in"
        End If

        safeMessage = safeMessage.Replace("""", "'")

        Try
            Dim commitResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
                sSVNPath,
                "commit --non-interactive -m """ & safeMessage & """ " & quoteFilePathArgs(commitPaths),
                savedPathForBackground
            )

            If commitResult.outputError IsNot Nothing AndAlso commitResult.outputError.Trim() <> "" Then
                errorMessage = commitResult.outputError.Trim()
                Return False
            End If

            Return True
        Catch ex As Exception
            errorMessage = ex.Message
            Return False
        End Try
    End Function

    Private Function verifyCommitTargetsLocallyCleanBackground(ByVal commitPaths() As String,
                                                               ByVal workingDirectory As String,
                                                               ByRef remainingChangesMessage As String) As Boolean
        remainingChangesMessage = ""

        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return True

        Dim targets As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each rawPath As String In commitPaths
            If String.IsNullOrWhiteSpace(rawPath) Then Continue For

            Dim normalizedPath As String = rawPath
            Try
                normalizedPath = Path.GetFullPath(rawPath)
            Catch
            End Try

            If seen.Add(normalizedPath) Then targets.Add(normalizedPath)
        Next

        If targets.Count = 0 Then Return True

        Dim statusResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
            sSVNPath,
            "status --xml --ignore-externals --depth empty " & quoteFilePathArgs(targets.ToArray()),
            workingDirectory
        )

        Dim statusError As String = If(statusResult.outputError, "").Trim()
        If statusError <> "" Then
            remainingChangesMessage = "The commit dialog closed, but PlumVault could not verify the local SVN state:" & vbCrLf & statusError
            Return False
        End If

        Dim xmlText As String = If(statusResult.output, "").Trim()
        If xmlText = "" Then Return True

        Dim remaining As New List(Of String)()

        Try
            Dim xmlDocument As New XmlDocument()
            xmlDocument.LoadXml(xmlText)

            Dim entryNodes As XmlNodeList = xmlDocument.SelectNodes("//entry")

            If entryNodes IsNot Nothing Then
                For Each entryNode As XmlNode In entryNodes
                    Dim wcStatus As XmlNode = entryNode.SelectSingleNode("wc-status")
                    If wcStatus Is Nothing Then Continue For

                    Dim itemState As String = ""
                    Dim propertyState As String = ""

                    If wcStatus.Attributes IsNot Nothing Then
                        Dim itemAttribute As XmlAttribute = wcStatus.Attributes("item")
                        Dim propsAttribute As XmlAttribute = wcStatus.Attributes("props")

                        If itemAttribute IsNot Nothing Then itemState = itemAttribute.Value
                        If propsAttribute IsNot Nothing Then propertyState = propsAttribute.Value
                    End If

                    Dim itemDirty As Boolean =
                        Not String.IsNullOrWhiteSpace(itemState) AndAlso
                        Not String.Equals(itemState, "normal", StringComparison.OrdinalIgnoreCase) AndAlso
                        Not String.Equals(itemState, "none", StringComparison.OrdinalIgnoreCase) AndAlso
                        Not String.Equals(itemState, "external", StringComparison.OrdinalIgnoreCase) AndAlso
                        Not String.Equals(itemState, "ignored", StringComparison.OrdinalIgnoreCase)

                    Dim propsDirty As Boolean =
                        Not String.IsNullOrWhiteSpace(propertyState) AndAlso
                        Not String.Equals(propertyState, "normal", StringComparison.OrdinalIgnoreCase) AndAlso
                        Not String.Equals(propertyState, "none", StringComparison.OrdinalIgnoreCase)

                    If itemDirty OrElse propsDirty Then
                        Dim entryPath As String = "<selected path>"
                        If entryNode.Attributes IsNot Nothing AndAlso entryNode.Attributes("path") IsNot Nothing Then
                            entryPath = entryNode.Attributes("path").Value
                        End If

                        remaining.Add(Path.GetFileName(entryPath) & " [" & itemState & If(propsDirty, ", properties modified", "") & "]")
                    End If
                Next
            End If
        Catch ex As Exception
            remainingChangesMessage = "The commit dialog closed, but PlumVault could not parse the local SVN verification result:" & vbCrLf & ex.Message
            Return False
        End Try

        If remaining.Count = 0 Then Return True

        remainingChangesMessage =
            "The TortoiseSVN commit was cancelled, or one or more selected paths were left out of the commit." & vbCrLf & vbCrLf &
            "Local SVN changes still remain:" & vbCrLf &
            String.Join(vbCrLf, remaining.Take(12))

        If remaining.Count > 12 Then
            remainingChangesMessage &= vbCrLf & "...and " & (remaining.Count - 12).ToString() & " more."
        End If

        remainingChangesMessage &= vbCrLf & vbCrLf &
            "The tree was not marked committed. Commit again or revert the remaining changes before closing."

        Return False
    End Function

    Private Function runTortoiseProcBackgroundNoUi(ByVal arguments As String,
                                                   ByVal repoRoot As String,
                                                   ByVal commitPaths() As String,
                                                   ByVal verificationWorkingDirectory As String,
                                                   ByRef errorMessage As String) As Boolean
        errorMessage = ""

        Try
            If String.IsNullOrWhiteSpace(sTortPath) Then
                errorMessage = "TortoiseProc.exe path is blank."
                Return False
            End If

            If String.IsNullOrWhiteSpace(arguments) Then
                errorMessage = "TortoiseSVN arguments are blank."
                Return False
            End If

            If arguments.Length > (32768 - 1) Then
                errorMessage = "Too many files were sent to TortoiseSVN. Use Windows Explorer/TortoiseSVN for this large commit."
                Return False
            End If

            Using p As New Process()
                Dim startInfo As New ProcessStartInfo()
                startInfo.FileName = sTortPath
                startInfo.Arguments = arguments
                startInfo.UseShellExecute = True

                If Not String.IsNullOrWhiteSpace(repoRoot) Then
                    startInfo.WorkingDirectory = repoRoot
                End If

                p.StartInfo = startInfo
                p.Start()

                Do While Not p.HasExited
                    System.Threading.Thread.Sleep(50)
                Loop
            End Using

            'TortoiseProc can close normally when the user presses Cancel, so its process
            'exit alone is not proof of a commit. Verify only the selected paths with a
            'local, depth-empty svn status check. This does not contact the server and is
            'normally effectively instant.
            Dim localVerificationMessage As String = ""
            If Not verifyCommitTargetsLocallyCleanBackground(commitPaths, verificationWorkingDirectory, localVerificationMessage) Then
                errorMessage = localVerificationMessage
                Return False
            End If

            Return True
        Catch ex As Exception
            errorMessage = ex.Message
            Return False
        End Try
    End Function

    Public Sub externalSetReadWriteFromLockStatus()
        reconcileWriteAccessForActiveDocumentPublic()
        reconcileReadOnlyForUnlockedActiveDocumentPublic()
    End Sub

    Private Sub refreshActiveTreeAfterSvnAction(Optional ByVal bUpdateLocalLockStatus As Boolean = True,
                                             Optional ByVal bRebuildTree As Boolean = False)
        'Speed fix:
        'After normal SVN actions, do not rebuild every open tree.
        'Default behavior is now node/status recolor only. Use bRebuildTree:=True when geometry/tree structure changed.
        Try
            If bUpdateLocalLockStatus Then
                updateLockStatusPublic(bRefreshAllTreeViews:=False)
            End If
        Catch
        End Try

        Try
            If myUserControl IsNot Nothing Then
                If bRebuildTree Then
                    myUserControl.refreshCurrentTreeViewOnly()
                Else
                    myUserControl.recolorCurrentTreeFromStatusPublic()
                End If
            End If
        Catch
            Try
                If myUserControl IsNot Nothing Then
                    myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
                End If
            Catch
            End Try
        End Try

        Try
            reconcileWriteAccessForActiveDocumentPublic()
            reconcileReadOnlyForUnlockedActiveDocumentPublic()
        Catch
        End Try

        Try
            keepNewUncommittedCadFilesWritable()
        Catch
        End Try
    End Sub
    Public Sub myCommitAll()
        Dim bSuccess As Boolean
        'Dim OpenDocPathList() As String

        'Dim i As Integer
        'Dim index As Integer

        iSwApp.RunCommand(19, vbEmpty) 'Save All


        If Not verifyLocalRepoPath() Then Exit Sub
        bSuccess = runTortoiseProcexeWithMonitor("/command:commit /path:""" & myUserControl.localRepoPath.Text & """ /closeonend:3")
        If Not bSuccess Then iSwApp.SendMsgToUser("TortoiseSVN Process Failed.") : Exit Sub

        'Switch over files to read-only
        'OpenDocPathList = CType(getAllOpenDocs(True, True), String())
        'Dim OpenDocModels() As ModelDoc2 = getAllOpenDocs(bMustBeVisible:=True)

        'Dim sOpenDocPath() As String = getFilePathsFromModDoiSwApp.SendMsgToUser("Active Document not found") cArr(OpenDocModels)

        'Speed fix:
        'Commit All can touch the whole repo, but the task pane only needs the active tree refreshed afterward.
        refreshActiveTreeAfterSvnAction()

    End Sub
    Sub myRepoStatus()
        Dim bSuccess As Boolean
        Dim modDoc As ModelDoc2
        Dim modDocArr() As ModelDoc2

        If iSwApp.ActiveDoc Is Nothing Then
            iSwApp.SendMsgToUser("A File must be open")
            Exit Sub
            'bSuccess = runTortoiseProcexeWithMonitor("/command:repostatus /remote")
        Else
            modDoc = iSwApp.ActiveDoc
            modDocArr = myUserControl.getComponentsOfAssemblyOptionalUpdateTree(iSwApp.ActiveDoc)
            If IsNothing(modDocArr) Then Exit Sub
            bSuccess = runTortoiseProcexeWithMonitor("/command:repostatus /path:" &
                                                 formatModDocArrForTortoiseProc(modDocArr) &
                                                 " /remote")
        End If
        If Not bSuccess Then iSwApp.SendMsgToUser("Status Check Failed.")
    End Sub
    Sub myCleanup()
        If asyncCleanupInProgress Then
            iSwApp.SendMsgToUser2(
                "SVN cleanup is already running in the background." & vbCrLf & vbCrLf &
                "Wait for it to finish before starting another cleanup.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub
        End If

        If asyncGetLocksInProgress OrElse asyncCommitInProgress Then
            iSwApp.SendMsgToUser2(
                "Another SVN operation is already running." & vbCrLf & vbCrLf &
                "Wait for Get Locks / Commit to finish before running Cleanup.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub
        End If

        If syncStatusInProgressOnControl() Then
            iSwApp.SendMsgToUser2(
                "Sync Status is currently running." & vbCrLf & vbCrLf &
                "Wait for Sync to finish before running Cleanup.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub
        End If

        If Not verifyLocalRepoPath(bCheckServer:=False) Then Exit Sub

        Dim repoRootPath As String = ""
        Dim savedPathForBackground As String = ""

        Try
            repoRootPath = myUserControl.localRepoPath.Text.TrimEnd("\"c)
        Catch
            repoRootPath = ""
        End Try

        If String.IsNullOrWhiteSpace(repoRootPath) OrElse Not Directory.Exists(repoRootPath) Then
            iSwApp.SendMsgToUser2(
                "Cleanup blocked." & vbCrLf & vbCrLf &
                "The local SVN folder path is missing or invalid.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub
        End If

        Try
            savedPathForBackground = myUserControl.savedPATH
        Catch
            savedPathForBackground = ""
        End Try

        Dim openDocCount As Integer = 0

        Try
            If iSwApp IsNot Nothing Then openDocCount = iSwApp.GetDocumentCount()
        Catch
            openDocCount = 0
        End Try

        Dim cleanupMessage As String =
            "Run SVN cleanup in the background on:" & vbCrLf &
            repoRootPath & vbCrLf & vbCrLf &
            "This uses command-line svn cleanup, not the TortoiseSVN popup." & vbCrLf &
            "It should not revert, delete, or commit CAD changes." & vbCrLf & vbCrLf &
            "You can keep using SolidWorks while it runs. If cleanup fails because Windows/SolidWorks is holding a file handle, close open CAD files and run cleanup again."

        If openDocCount > 0 Then
            cleanupMessage &= vbCrLf & vbCrLf &
                "Note: SolidWorks currently has " & openDocCount.ToString() & " document(s) open. Cleanup may still work, but file-handle errors are more likely."
        End If

        If iSwApp.SendMsgToUser2(
            cleanupMessage & vbCrLf & vbCrLf & "Continue?",
            swMessageBoxIcon_e.swMbQuestion,
            swMessageBoxBtn_e.swMbYesNo
        ) <> swMessageBoxResult_e.swMbHitYes Then
            Exit Sub
        End If

        asyncCleanupInProgress = True

        Try
            iSwApp.SendMsgToUser2(
                "SVN cleanup started in the background." & vbCrLf & vbCrLf &
                "You can keep using SolidWorks. You will get a message when it finishes.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
        Catch
        End Try

        Task.Run(Sub()
                     Dim cleanupWatch As Stopwatch = Stopwatch.StartNew()
                     Dim cleanupResult As New rawProcessReturn()
                     Dim errorMessage As String = ""
                     Dim success As Boolean = False

                     Try
                         cleanupResult = runSvnProcessBackgroundNoUi(
                             sSVNPath,
                             "cleanup --non-interactive """ & repoRootPath & """",
                             savedPathForBackground
                         )

                         If cleanupResult.outputError IsNot Nothing AndAlso cleanupResult.outputError.Trim() <> "" Then
                             errorMessage = cleanupResult.outputError.Trim()
                         Else
                             success = True
                         End If
                     Catch ex As Exception
                         errorMessage = ex.Message
                     End Try

                     cleanupWatch.Stop()

                     Try
                         If myUserControl IsNot Nothing AndAlso myUserControl.IsHandleCreated Then
                             myUserControl.BeginInvoke(New System.Windows.Forms.MethodInvoker(
                                 Sub()
                                     finishAsyncCleanup(success, cleanupResult, errorMessage, cleanupWatch.ElapsedMilliseconds)
                                 End Sub
                             ))
                         Else
                             asyncCleanupInProgress = False
                         End If
                     Catch
                         asyncCleanupInProgress = False
                     End Try
                 End Sub)
    End Sub

    Private Sub finishAsyncCleanup(ByVal success As Boolean,
                                   ByVal cleanupResult As rawProcessReturn,
                                   ByVal errorMessage As String,
                                   ByVal elapsedMs As Long)
        asyncCleanupInProgress = False

        Dim debugNotes As New List(Of String)()
        debugNotes.Add("Cleanup path: " & myUserControl.localRepoPath.Text)
        debugNotes.Add("Elapsed: " & elapsedMs.ToString() & " ms")

        Try
            If cleanupResult.output IsNot Nothing AndAlso cleanupResult.output.Trim() <> "" Then
                debugNotes.Add("stdout:")
                debugNotes.Add(cleanupResult.output.Trim())
            End If

            If cleanupResult.outputError IsNot Nothing AndAlso cleanupResult.outputError.Trim() <> "" Then
                debugNotes.Add("stderr:")
                debugNotes.Add(cleanupResult.outputError.Trim())
            End If
        Catch
        End Try

        If success Then
            Try
                updateLockStatusPublic(bRefreshAllTreeViews:=False)
                refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)
            Catch
            End Try

            showSvnTimingDebugWindow("SVN cleanup finished.", debugNotes)

            iSwApp.SendMsgToUser2(
                "SVN cleanup finished successfully." & vbCrLf & vbCrLf &
                "Elapsed: " & elapsedMs.ToString() & " ms",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
        Else
            If String.IsNullOrWhiteSpace(errorMessage) Then errorMessage = "Unknown cleanup failure."

            showSvnTimingDebugWindow("SVN cleanup failed.", debugNotes)

            iSwApp.SendMsgToUser2(
                "SVN cleanup failed." & vbCrLf & vbCrLf &
                errorMessage & vbCrLf & vbCrLf &
                "If this mentions a locked file or access denied, close open CAD files and try again. If it still fails, close SolidWorks and run TortoiseSVN Cleanup from Windows Explorer.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
        End If
    End Sub

    Public Sub addtoRepoFunc(ByRef modDocArr() As ModelDoc2)

        If Not verifyLocalRepoPath() Then Exit Sub
        runTortoiseProcexeWithMonitor("/command:add /path:" & formatModDocArrForTortoiseProc(modDocArr) & " /closeonend:3")
        tortCommitDocs(modDocArr)

    End Sub

    Private Function filterExistingCadFilePathsOnly(ByVal inputPaths() As String) As String()
        If inputPaths Is Nothing Then Return Nothing

        Dim output As New List(Of String)

        For Each p As String In inputPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For

            Try
                If Not File.Exists(p) Then Continue For

                Dim ext As String = Path.GetExtension(p).ToUpperInvariant()

                If ext = ".SLDPRT" OrElse ext = ".SLDASM" OrElse ext = ".SLDDRW" Then
                    output.Add(p)
                End If
            Catch
            End Try
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Class AsyncGetLocksResult
        Public Property Success As Boolean = False
        Public Property Message As String = ""
        Public Property IsWarning As Boolean = False
        Public Property IsInfoOnly As Boolean = False
        Public Property AttemptedPaths As String() = Nothing
        Public Property LockedPaths As String() = Nothing
    End Class

    Public Sub getLocksOfPathsAsync(ByVal selectedPaths() As String,
                                    Optional bBreakLocks As Boolean = False,
                                    Optional bUseTortoise As Boolean = False,
                                    Optional sMessage As String = "",
                                    Optional allowCloseReview As Boolean = False)
        If Not canRunDeferredSolidWorksUiMutationPublic(allowCloseReview) Then
            iSwApp.SendMsgToUser2(
                "PlumVault is finishing another SOLIDWORKS document operation." & vbCrLf & vbCrLf &
                "Wait a moment, then click Get Locks again.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If asyncGetLocksInProgress Then
            iSwApp.SendMsgToUser2(
                "Get Locks is already running." & vbCrLf & vbCrLf &
                "Wait for the current lock request to finish before starting another one.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If bUseTortoise Then
            Dim pathDocs() As ModelDoc2 = getOpenDocsForPaths(selectedPaths)
            If pathDocs IsNot Nothing AndAlso pathDocs.Length > 0 Then
                getLocksOfDocs(pathDocs, bBreakLocks, bUseTortoise, sMessage)
            Else
                iSwApp.SendMsgToUser2("No open CAD documents were available for the Tortoise Get Locks path.",
                    swMessageBoxIcon_e.swMbInformation,
                    swMessageBoxBtn_e.swMbOk)
            End If
            Exit Sub
        End If

        Dim filteredPaths() As String = filterExistingCadFilePathsOnly(selectedPaths)

        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then
            iSwApp.SendMsgToUser2(
                "No valid CAD file paths were selected for Get Locks.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Dim repoRootPathForBackground As String = ""
        Dim savedPathForBackground As String = ""

        Try
            If myUserControl IsNot Nothing AndAlso myUserControl.localRepoPath IsNot Nothing Then
                repoRootPathForBackground = myUserControl.localRepoPath.Text
            End If
        Catch
            repoRootPathForBackground = ""
        End Try

        Try
            If myUserControl IsNot Nothing Then savedPathForBackground = myUserControl.savedPATH
        Catch
            savedPathForBackground = ""
        End Try

        rememberAsyncGetLocksPaths(filteredPaths)
        asyncGetLocksInProgress = True
        writeOperationLog(
            "Get Locks started: count=" & filteredPaths.Length.ToString() &
            "; paths=" & String.Join(" | ", filteredPaths)
        )

        Try
            myUserControl.markLockPendingForFilePathsPublic(filteredPaths, True, "Locking...")
        Catch
        End Try

        Task.Run(Sub()
                     Dim result As AsyncGetLocksResult = performGetLocksForPathsBackground(filteredPaths, bBreakLocks, sMessage, repoRootPathForBackground, savedPathForBackground)

                     Try
                         If myUserControl IsNot Nothing AndAlso Not myUserControl.IsDisposed AndAlso myUserControl.IsHandleCreated Then
                             myUserControl.BeginInvoke(New MethodInvoker(Sub() finishAsyncGetLocksOnMainThread(result)))
                         Else
                             asyncGetLocksInProgress = False
                             clearAsyncGetLocksPaths()
                             pendingInContextAutoEditRequest = Nothing
                         End If
                     Catch
                         asyncGetLocksInProgress = False
                         clearAsyncGetLocksPaths()
                         pendingInContextAutoEditRequest = Nothing
                     End Try
                 End Sub)
    End Sub

    Public Sub getLocksOfDocsAsync(ByRef modDocArr() As ModelDoc2,
                                   Optional bBreakLocks As Boolean = False,
                                   Optional bUseTortoise As Boolean = False,
                                   Optional sMessage As String = "")
        If bUseTortoise Then
            getLocksOfDocs(modDocArr, bBreakLocks, bUseTortoise, sMessage)
            Exit Sub
        End If

        Dim selectedPaths() As String = getCadFilePathsFromDocsForAsyncLock(modDocArr)
        getLocksOfPathsAsync(selectedPaths, bBreakLocks:=bBreakLocks, bUseTortoise:=False, sMessage:=sMessage)
    End Sub

    Private Function getCadFilePathsFromDocsForAsyncLock(ByRef modDocArr() As ModelDoc2) As String()
        If modDocArr Is Nothing Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each doc As ModelDoc2 In modDocArr
            If doc Is Nothing Then Continue For

            Dim docPath As String = ""

            Try
                docPath = doc.GetPathName()
            Catch
                docPath = ""
            End Try

            If String.IsNullOrWhiteSpace(docPath) Then Continue For
            If Not File.Exists(docPath) Then Continue For
            If Not isCadFilePath(docPath) Then Continue For

            Try
                docPath = Path.GetFullPath(docPath)
            Catch
            End Try

            If Not seen.Contains(docPath) Then
                seen.Add(docPath)
                output.Add(docPath)
            End If
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function getOpenDocsForPaths(ByVal filePaths() As String) As ModelDoc2()
        If filePaths Is Nothing Then Return Nothing

        Dim output As New List(Of ModelDoc2)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each filePath As String In filePaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            Dim normalizedPath As String = filePath
            Try
                normalizedPath = Path.GetFullPath(filePath)
            Catch
            End Try

            If seen.Contains(normalizedPath) Then Continue For
            seen.Add(normalizedPath)

            Dim doc As ModelDoc2 = getOpenModelByPathSafe(normalizedPath)
            If doc IsNot Nothing Then output.Add(doc)
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Function isFirstCommitCandidatePathForAsyncLock(ByVal filePath As String,
                                                            ByVal repoRootPathForBackground As String,
                                                            ByVal localState As AsyncLocalSvnState) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not File.Exists(filePath) Then Return False
        If Not isCadFilePath(filePath) Then Return False
        If Not isPathInsideRepoRootForBackground(filePath, repoRootPathForBackground) Then Return False

        If localState Is Nothing Then Return False
        Return localState.StatusChar = "?"c OrElse localState.StatusChar = "A"c
    End Function

    Private Function isPathInsideRepoRootForBackground(ByVal filePath As String,
                                                       ByVal repoRootPathForBackground As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If String.IsNullOrWhiteSpace(repoRootPathForBackground) Then Return False

        Try
            Dim repoRoot As String = Path.GetFullPath(repoRootPathForBackground).TrimEnd("\"c)
            Dim fullPath As String = Path.GetFullPath(filePath).TrimEnd("\"c)

            If String.Equals(fullPath, repoRoot, StringComparison.OrdinalIgnoreCase) Then Return True
            Return fullPath.StartsWith(repoRoot & "\", StringComparison.OrdinalIgnoreCase)
        Catch
            Return False
        End Try
    End Function

    Private Function getLocalSvnStateForAsyncLock(ByVal filePath As String,
                                                   ByVal savedPathForBackground As String) As AsyncLocalSvnState
        Dim state As New AsyncLocalSvnState()
        If String.IsNullOrWhiteSpace(filePath) Then Return state
        If Not File.Exists(filePath) Then Return state

        Try
            Dim statusResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
                sSVNPath,
                "status --non-interactive """ & filePath.Replace("""", "") & """",
                savedPathForBackground
            )

            If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
                Return state
            End If

            Dim statusText As String = If(statusResult.output, "")
            Dim lines() As String = statusText.Split(
                New String() {vbCrLf, vbLf},
                StringSplitOptions.RemoveEmptyEntries
            )

            If lines.Length = 0 Then
                state.StatusChar = " "c
                Return state
            End If

            For Each line As String In lines
                If String.IsNullOrWhiteSpace(line) Then Continue For
                If state.StatusChar = ChrW(0) Then state.StatusChar = line(0)
                If line.Length >= 6 AndAlso line(5) = "K"c Then state.HasLocalLockToken = True
            Next

        Catch
        End Try

        Return state
    End Function

    Private Function pathHasLocalSvnLockTokenBackground(ByVal filePath As String,
                                                            ByVal savedPathForBackground As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not File.Exists(filePath) Then Return False

        Try
            Dim statusResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
                sSVNPath,
                "status --non-interactive """ & filePath.Replace("""", "") & """",
                savedPathForBackground
            )

            If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
                Return False
            End If

            Dim statusText As String = ""
            If statusResult.output IsNot Nothing Then statusText = statusResult.output

            Dim lines() As String = statusText.Split(
                New String() {vbCrLf, vbLf},
                StringSplitOptions.RemoveEmptyEntries
            )

            For Each line As String In lines
                If String.IsNullOrWhiteSpace(line) Then Continue For
                If line.Length >= 6 AndAlso line(5) = "K"c Then Return True
            Next
        Catch
        End Try

        Return False
    End Function

    Private Function performGetLocksForPathsBackground(ByVal selectedPaths() As String,
                                                       ByVal bBreakLocks As Boolean,
                                                       ByVal sMessage As String,
                                                       ByVal repoRootPathForBackground As String,
                                                       ByVal savedPathForBackground As String) As AsyncGetLocksResult
        Dim result As New AsyncGetLocksResult()
        result.AttemptedPaths = selectedPaths
        Dim totalWatch As Stopwatch = Stopwatch.StartNew()
        Dim phaseWatch As Stopwatch = Stopwatch.StartNew()

        Try
            Dim filteredPaths() As String = filterExistingCadFilePathsOnly(selectedPaths)

            If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then
                result.Message = "No valid CAD file paths were selected for Get Locks."
                result.IsWarning = True
                Return result
            End If

            Dim lockablePaths As New List(Of String)()
            Dim firstCommitPaths As New List(Of String)()
            Dim alreadyLockedPaths As New List(Of String)()

            For Each filePath As String In filteredPaths
                'One local status command supplies both the first-commit state and the local
                'K-token state. The previous two-query sequence doubled svn.exe startup and
                'working-copy database work for every selected file.
                Dim localState As AsyncLocalSvnState =
                    getLocalSvnStateForAsyncLock(filePath, savedPathForBackground)

                If isFirstCommitCandidatePathForAsyncLock(filePath, repoRootPathForBackground, localState) Then
                    Try
                        File.SetAttributes(filePath, File.GetAttributes(filePath) And Not FileAttributes.ReadOnly)
                    Catch
                    End Try

                    firstCommitPaths.Add(filePath)
                ElseIf localState.HasLocalLockToken Then
                    'The working copy already owns the SVN lock token. This is the stale-cache /
                    'read-only recovery case: do not call svn lock again and do not tell the user
                    'they must unlock/relock. Reconcile the UI and SOLIDWORKS write state instead.
                    Try
                        File.SetAttributes(filePath, File.GetAttributes(filePath) And Not FileAttributes.ReadOnly)
                    Catch
                    End Try

                    alreadyLockedPaths.Add(filePath)
                Else
                    lockablePaths.Add(filePath)
                End If
            Next

            writeOperationLog(
                "Get Locks local classification: " & phaseWatch.ElapsedMilliseconds.ToString() &
                " ms; lockable=" & lockablePaths.Count.ToString() &
                "; alreadyOwned=" & alreadyLockedPaths.Count.ToString() &
                "; firstCommit=" & firstCommitPaths.Count.ToString()
            )

            If lockablePaths.Count = 0 Then
                If alreadyLockedPaths.Count > 0 Then
                    result.Success = True
                    result.LockedPaths = alreadyLockedPaths.ToArray()
                    result.IsInfoOnly = True
                    result.Message = "You already own the selected SVN lock." & vbCrLf & vbCrLf &
                        "PlumVault refreshed the tree and restored write access without unlocking or relocking the file."
                    Return result
                End If

                result.IsInfoOnly = True
                result.Message = "No SVN lock needed." & vbCrLf & vbCrLf &
                    "The selected file appears to be new and not committed yet." & vbCrLf &
                    "Click Commit instead. The plugin will add it to SVN during the first commit."
                Return result
            End If

            phaseWatch.Restart()
            Dim outOfDatePaths() As String = getOutOfDatePathsForAsyncLock(lockablePaths.ToArray(), result.Message, savedPathForBackground)
            writeOperationLog("Get Locks latest-revision check: " & phaseWatch.ElapsedMilliseconds.ToString() & " ms")
            If result.Message <> "" Then
                result.IsWarning = True
                Return result
            End If

            If outOfDatePaths IsNot Nothing AndAlso outOfDatePaths.Length > 0 Then
                result.IsWarning = True
                result.Message = "Lock cancelled because one or more selected files are out of date." & vbCrLf & vbCrLf &
                    "Use Get Latest first so you are working from the newest geometry, then click Get Locks again." & vbCrLf & vbCrLf &
                    "Out-of-date files:" & vbCrLf &
                    stringArrToSingleStringWithNewLines(outOfDatePaths, bTrimFileNames:=True, iLimit:=10)
                Return result
            End If

            phaseWatch.Restart()
            Dim releasedPaths() As String = getReleasedPathsForAsyncLock(lockablePaths.ToArray(), savedPathForBackground)
            writeOperationLog("Get Locks release-state check: " & phaseWatch.ElapsedMilliseconds.ToString() & " ms")
            If releasedPaths IsNot Nothing AndAlso releasedPaths.Length > 0 AndAlso sMessage <> "#UP REV EDIT#" Then
                Dim releasedSet As New HashSet(Of String)(releasedPaths.Select(Function(p) normalizeSvnPath(p)), StringComparer.OrdinalIgnoreCase)
                Dim remaining As New List(Of String)()

                For Each filePath As String In lockablePaths
                    If Not releasedSet.Contains(normalizeSvnPath(filePath)) Then
                        remaining.Add(filePath)
                    End If
                Next

                If remaining.Count = 0 Then
                    result.IsWarning = True
                    result.Message = "Unable to lock the selected file(s), since they are in RELEASED state." & vbCrLf & vbCrLf &
                        "Use 'EDIT New Revision' to get edit access." & vbCrLf & vbCrLf &
                        stringArrToSingleStringWithNewLines(releasedPaths, bTrimFileNames:=True, iLimit:=10)
                    Return result
                End If

                lockablePaths = remaining
            End If

            'Lock each path independently. Drawing dependency sets commonly contain a mix of
            'available files and files already held by teammates; one conflict must not roll
            'the entire request into a false total failure.
            Dim newlyLockedPaths As New List(Of String)()
            Dim lockFailureMessages As New List(Of String)()
            phaseWatch.Restart()

            For Each filePath As String In lockablePaths
                Dim lockResult As rawProcessReturn = runSvnLockForPathsBackground(
                    New String() {filePath},
                    bBreakLocks,
                    sMessage,
                    savedPathForBackground
                )

                Dim lockError As String = ""
                If lockResult.outputError IsNot Nothing Then lockError = lockResult.outputError.Trim()

                If String.IsNullOrWhiteSpace(lockError) Then
                    newlyLockedPaths.Add(filePath)
                    Continue For
                End If

                'One final local-token reconciliation handles the race where the cache said
                'unlocked but SVN reports that this same working copy already owns the lock.
                If pathHasLocalSvnLockTokenBackground(filePath, savedPathForBackground) Then
                    alreadyLockedPaths.Add(filePath)
                    Continue For
                End If

                lockFailureMessages.Add(Path.GetFileName(filePath) & ": " & lockError)
            Next

            writeOperationLog("Get Locks lock command(s): " & phaseWatch.ElapsedMilliseconds.ToString() & " ms")

            Dim allOwnedPaths As New List(Of String)()
            allOwnedPaths.AddRange(alreadyLockedPaths)
            allOwnedPaths.AddRange(newlyLockedPaths)

            If allOwnedPaths.Count = 0 Then
                result.IsWarning = True
                result.Message = "Locking failed." & vbCrLf & vbCrLf & String.Join(vbCrLf, lockFailureMessages.ToArray())
                Return result
            End If

            If newlyLockedPaths.Count > 0 Then
                phaseWatch.Restart()
                Dim propResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
                    sSVNPath,
                    "propset addin:release_state ""||EDIT||"" " & quoteFilePathArgs(newlyLockedPaths.ToArray()),
                    savedPathForBackground
                )

                If propResult.outputError IsNot Nothing AndAlso propResult.outputError.Trim() <> "" Then
                    result.IsWarning = True
                    result.Message = "Files were locked, but setting the SVN edit property failed." & vbCrLf & vbCrLf &
                        propResult.outputError.Trim()
                    result.LockedPaths = allOwnedPaths.Distinct(StringComparer.OrdinalIgnoreCase).ToArray()
                    Return result
                End If

                writeOperationLog("Get Locks edit-property update: " & phaseWatch.ElapsedMilliseconds.ToString() & " ms")
            End If

            For Each filePath As String In allOwnedPaths
                Try
                    File.SetAttributes(filePath, File.GetAttributes(filePath) And Not FileAttributes.ReadOnly)
                Catch
                End Try
            Next

            result.Success = True
            result.LockedPaths = allOwnedPaths.Distinct(StringComparer.OrdinalIgnoreCase).ToArray()
            writeOperationLog("Get Locks background total: " & totalWatch.ElapsedMilliseconds.ToString() & " ms")

            If lockFailureMessages.Count > 0 Then
                result.IsWarning = True
                result.Message = "Some files could not be locked; every other available file was locked normally." &
                    vbCrLf & vbCrLf & String.Join(vbCrLf, lockFailureMessages.ToArray())
            ElseIf releasedPaths IsNot Nothing AndAlso releasedPaths.Length > 0 Then
                result.IsInfoOnly = True
                result.Message = "Available files were locked. RELEASED files were left unchanged:" & vbCrLf & vbCrLf &
                    stringArrToSingleStringWithNewLines(releasedPaths, bTrimFileNames:=True, iLimit:=10)
            End If

            Return result

        Catch ex As Exception
            result.IsWarning = True
            result.Message = "Error running Get Locks in the background: " & ex.Message
            Return result
        End Try
    End Function

    Private Sub finishAsyncGetLocksOnMainThread(ByVal result As AsyncGetLocksResult)
        Dim autoEditTargetPath As String = ""
        Dim autoEditWasAttempted As Boolean = False
        Dim autoEditLockOwned As Boolean = False
        Dim autoEditWritableDeferred As Boolean = False
        Dim closeReviewLockTargetPath As String = pendingCloseReviewLockPath

        If pendingInContextAutoEditRequest IsNot Nothing AndAlso result IsNot Nothing Then
            autoEditTargetPath = pendingInContextAutoEditRequest.ChildPath
            autoEditWasAttempted = resultContainsPath(result.AttemptedPaths, autoEditTargetPath)
            autoEditLockOwned = resultContainsPath(result.LockedPaths, autoEditTargetPath)
        End If

        Try
            If result IsNot Nothing AndAlso result.AttemptedPaths IsNot Nothing Then
                myUserControl.markLockPendingForFilePathsPublic(
                    result.AttemptedPaths,
                    False
                )
            End If
        Catch ex As Exception
            writeOperationLog("Could not clear lock-pending UI: " & ex.Message)
        End Try

        'The SVN operation has finished before any live SOLIDWORKS document mutation is queued.
        asyncGetLocksInProgress = False
        clearAsyncGetLocksPaths()

        Dim autoEditWritableImmediately As Boolean = False

        Try
            If result IsNot Nothing AndAlso
               result.LockedPaths IsNot Nothing AndAlso
               result.LockedPaths.Length > 0 Then

                updateStatusCacheForKnownPaths(
                    result.LockedPaths,
                    forceLock6:="K",
                    forceReleased:="||EDIT||"
                )

                myUserControl.markLockResultForFilePathsPublic(
                    result.LockedPaths,
                    True,
                    "Locked by you"
                )

                'Keep all acquired SVN locks and clear every file's on-disk read-only bit,
                'but only change the live SOLIDWORKS read-only state for the document the
                'user is actively working in (plus an active in-context child). Flipping a
                'large dependent set at once is the rebuild/false-dirty cascade trigger.
                Dim immediateWritable As New List(Of String)()
                Dim activeInteractionPaths() As String =
                    getActiveInteractionPathsFromCandidates(result.LockedPaths)

                If activeInteractionPaths IsNot Nothing Then
                    immediateWritable.AddRange(activeInteractionPaths)
                End If

                'The intercepted Edit Component child is deliberately no longer an active
                'in-context document while the asynchronous lock runs. Add exactly that one
                'path back to writable reconciliation; never broaden this to all dependencies.
                If autoEditWasAttempted AndAlso autoEditLockOwned AndAlso
                   Not immediateWritable.Any(Function(p) pathsAreSame(p, autoEditTargetPath)) Then
                    immediateWritable.Add(autoEditTargetPath)
                End If

                Dim immediateWritablePaths() As String =
                    immediateWritable.Distinct(StringComparer.OrdinalIgnoreCase).ToArray()

                Dim deferredWritable As New List(Of String)()

                'This completion already runs on the SOLIDWORKS UI thread. Reconcile only the
                'active/exact targets synchronously so a native edit started immediately after
                'Get Locks cannot observe the old internal read-only state during the timer gap.
                'Any transient COM failure keeps the existing deferred retry as a fallback.
                For Each writablePath As String In immediateWritablePaths
                    Dim writableFailure As String = ""

                    'A file that previously took pathologically long in SetReadOnlyState is
                    'never transitioned pre-emptively again (the disk read-only attribute is
                    'already cleared by the lock itself). The explicit edit/save precheck
                    'still transitions it when the user actually works on it. The in-flight
                    'auto-edit target is exempt from the skip: that edit is about to replay
                    'and must observe a writable document now.
                    If shouldSkipBackgroundWritableTransitionPublic(writablePath) AndAlso
                       Not (autoEditWasAttempted AndAlso pathsAreSame(writablePath, autoEditTargetPath)) Then
                        writeOperationLog(
                            "Known-slow writable transition skipped after Get Locks: " & writablePath
                        )
                        Continue For
                    End If

                    Dim openDocument As ModelDoc2 = getOpenModelByPathSafe(writablePath)

                    If ensureOpenCadDocumentWritableNow(writablePath, openDocument, writableFailure) Then
                        If autoEditWasAttempted AndAlso pathsAreSame(writablePath, autoEditTargetPath) Then
                            autoEditWritableImmediately = True
                        End If
                    Else
                        deferredWritable.Add(writablePath)
                        If autoEditWasAttempted AndAlso pathsAreSame(writablePath, autoEditTargetPath) Then
                            autoEditWritableDeferred = True
                        End If
                        writeOperationLog(
                            "Immediate Get Locks writable-state reconciliation deferred: " &
                            writablePath & "; " & writableFailure
                        )
                    End If
                Next

                If deferredWritable.Count > 0 Then
                    myUserControl.forceWriteAccessForLockedFilePathsPublic(
                        deferredWritable.Distinct(StringComparer.OrdinalIgnoreCase).ToArray()
                    )
                End If

                writeOperationLog(
                    "Get Locks completed; live writable reconciliation scoped to active interaction: " &
                    If(immediateWritablePaths.Length = 0,
                       "<none; deferred until document activation>",
                       String.Join(" | ", immediateWritablePaths))
                )
            End If
        Catch ex As Exception
            writeOperationLog(
                "Get Locks main-thread completion error: " & ex.ToString()
            )
        End Try

        If result Is Nothing Then
            writeOperationLog("Get Locks finished with no result object.")

            If Not String.IsNullOrWhiteSpace(closeReviewLockTargetPath) Then
                pendingCloseReviewLockPath = ""
                Try
                    RaiseEvent CloseReviewLockCompleted(
                        closeReviewLockTargetPath,
                        False,
                        "The SVN lock operation ended without returning a result."
                    )
                Catch
                End Try
            End If

            If pendingInContextAutoEditRequest IsNot Nothing Then
                Dim failedPath As String = pendingInContextAutoEditRequest.ChildPath
                pendingInContextAutoEditRequest = Nothing
                showInContextAutoEditFailure(
                    failedPath,
                    "The SVN lock operation ended without returning a result."
                )
            End If

            finishPostFirstCommitLockTransition(Nothing)
            Exit Sub
        End If

        If autoEditWasAttempted Then
            If Not autoEditLockOwned Then
                Dim lockDetail As String = result.Message
                pendingInContextAutoEditRequest = Nothing
                result.Message = buildInContextAutoEditFailureMessage(autoEditTargetPath, lockDetail)
                result.IsWarning = True
                result.IsInfoOnly = False
            ElseIf result.IsInfoOnly AndAlso
                   result.Message.StartsWith("You already own", StringComparison.OrdinalIgnoreCase) Then
                'Successful Edit Part continues automatically; the generic stale-cache info
                'dialog would only add an unnecessary click before that continuation.
                result.Message = ""
                result.IsInfoOnly = False
            End If

            If autoEditLockOwned AndAlso Not autoEditWritableImmediately AndAlso
               Not autoEditWritableDeferred AndAlso pendingInContextAutoEditRequest IsNot Nothing Then
                pendingInContextAutoEditRequest = Nothing
                result.Message = buildInContextAutoEditFailureMessage(
                    autoEditTargetPath,
                    "The lock was obtained, but PlumVault could not complete the exact document write-access check.",
                    writeAccessWasObtained:=True
                )
                result.IsWarning = True
                result.IsInfoOnly = False
            End If
        End If

        If autoEditWasAttempted AndAlso autoEditLockOwned AndAlso autoEditWritableImmediately AndAlso
           pendingInContextAutoEditRequest IsNot Nothing Then
            Try
                'Leave the Get Locks completion callback before replaying the native edit.
                myUserControl.BeginInvoke(New MethodInvoker(AddressOf resumePendingInContextAutoEdit))
            Catch ex As Exception
                pendingInContextAutoEditRequest = Nothing
                writeOperationLog("Could not queue edit replay after Get Locks: " & ex.Message)
            End Try
        End If

        If Not result.Success Then
            writeOperationLog("Get Locks failed: " & result.Message)
        Else
            writeOperationLog("Get Locks SVN phase succeeded.")
        End If

        If Not String.IsNullOrWhiteSpace(closeReviewLockTargetPath) Then
            Dim closeReviewLockSucceeded As Boolean =
                resultContainsPath(result.LockedPaths, closeReviewLockTargetPath)
            Dim closeReviewLockMessage As String = If(result.Message, "").Trim()

            If Not closeReviewLockSucceeded AndAlso String.IsNullOrWhiteSpace(closeReviewLockMessage) Then
                closeReviewLockMessage =
                    "SVN did not obtain the lock. Another user may already hold it, or the file may be out of date."
            End If

            pendingCloseReviewLockPath = ""

            Try
                RaiseEvent CloseReviewLockCompleted(
                    closeReviewLockTargetPath,
                    closeReviewLockSucceeded,
                    closeReviewLockMessage
                )
            Catch ex As Exception
                writeOperationLog("Could not update the close-review lock row: " & ex.Message)
            End Try

            'The modal close table owns feedback for this request. Avoid a second generic
            'SOLIDWORKS alert on top of the row result, especially when another user owns it.
            result.Message = ""
            result.IsWarning = False
            result.IsInfoOnly = False
        End If

        If Not String.IsNullOrWhiteSpace(result.Message) Then
            Try
                Dim icon As swMessageBoxIcon_e =
                    If(
                        result.IsWarning,
                        swMessageBoxIcon_e.swMbWarning,
                        swMessageBoxIcon_e.swMbInformation
                    )

                iSwApp.SendMsgToUser2(
                    result.Message,
                    icon,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch ex As Exception
                writeOperationLog(
                    "Could not show Get Locks completion message: " & ex.Message
                )
            End Try
        End If

        finishPostFirstCommitLockTransition(result.AttemptedPaths)
    End Sub

    Private Function quoteFilePathArgs(ByVal filePaths() As String) As String
        If filePaths Is Nothing Then Return ""

        Dim args As New List(Of String)()

        For Each filePath As String In filePaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For
            args.Add("""" & filePath.Replace("""", "") & """")
        Next

        Return String.Join(" ", args)
    End Function

    Private Function runSvnLockForPathsBackground(ByVal filePaths() As String,
                                                  ByVal bBreakLocks As Boolean,
                                                  ByVal sMessage As String,
                                                  ByVal savedPathForBackground As String) As rawProcessReturn
        Dim args As String = "lock "

        If bBreakLocks Then args &= "--force "
        If Not String.IsNullOrWhiteSpace(sMessage) Then args &= "-m """ & sMessage.Replace("""", "'") & """ "

        args &= quoteFilePathArgs(filePaths)

        Return runSvnProcessBackgroundNoUi(sSVNPath, args, savedPathForBackground)
    End Function

    Private Function getOutOfDatePathsForAsyncLock(ByVal filePaths() As String,
                                                   ByRef errorMessage As String,
                                                   ByVal savedPathForBackground As String,
                                                   Optional ByVal timeoutMilliseconds As Integer = 120000) As String()
        errorMessage = ""

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Return Nothing

        'Keep very deep drawing/assembly dependency sets below Windows command-line limits.
        'The same helper is used by ordinary multi-select Get Locks.
        If filePaths.Length > 16 Then
            Dim combined As New List(Of String)()
            Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

            For Each chunk As String() In chunkFilePathsForBackground(filePaths, 16)
                Dim chunkError As String = ""
                Dim chunkOutOfDate() As String = getOutOfDatePathsForAsyncLock(
                    chunk,
                    chunkError,
                    savedPathForBackground,
                    timeoutMilliseconds
                )

                If Not String.IsNullOrWhiteSpace(chunkError) Then
                    errorMessage = chunkError
                    Return Nothing
                End If

                If chunkOutOfDate Is Nothing Then Continue For
                For Each stalePath As String In chunkOutOfDate
                    If seen.Add(stalePath) Then combined.Add(stalePath)
                Next
            Next

            If combined.Count = 0 Then Return Nothing
            Return combined.ToArray()
        End If

        Dim statusResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
            sSVNPath,
            "status -vu --non-interactive " & quoteFilePathArgs(filePaths),
            savedPathForBackground,
            timeoutMilliseconds
        )

        If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
            errorMessage = "Could not verify latest SVN status before locking." & vbCrLf & vbCrLf & statusResult.outputError.Trim()
            Return Nothing
        End If

        If statusResult.output Is Nothing OrElse statusResult.output.Trim() = "" Then Return Nothing

        Dim outOfDate As New List(Of String)()
        Dim lines() As String = statusResult.output.Split({vbCrLf, vbLf}, StringSplitOptions.RemoveEmptyEntries)

        For Each line As String In lines
            If String.IsNullOrWhiteSpace(line) Then Continue For
            If line.StartsWith("Status against revision", StringComparison.OrdinalIgnoreCase) Then Continue For

            If line.Length >= 9 AndAlso line(8) = "*"c Then
                Dim matchedPath As String = matchStatusLineToPath(line, filePaths)
                If matchedPath = "" Then matchedPath = line.Trim()
                outOfDate.Add(matchedPath)
            End If
        Next

        If outOfDate.Count = 0 Then Return Nothing
        Return outOfDate.ToArray()
    End Function

    Private Function matchStatusLineToPath(ByVal statusLine As String,
                                           ByVal filePaths() As String) As String
        If String.IsNullOrWhiteSpace(statusLine) OrElse filePaths Is Nothing Then Return ""

        For Each filePath As String In filePaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            Try
                If statusLine.EndsWith(filePath, StringComparison.OrdinalIgnoreCase) Then Return filePath
            Catch
            End Try
        Next

        Return ""
    End Function

    Private Function getReleasedPathsForAsyncLock(ByVal filePaths() As String,
                                                  ByVal savedPathForBackground As String) As String()
        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Return Nothing

        Dim propResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
            sSVNPath,
            "propget addin:release_state --xml " & quoteFilePathArgs(filePaths),
            savedPathForBackground
        )

        If propResult.output Is Nothing OrElse propResult.output.Trim() = "" Then Return Nothing
        If propResult.outputError IsNot Nothing AndAlso propResult.outputError.Trim() <> "" Then Return Nothing

        Dim released As New List(Of String)()
        Dim doc As New XmlDocument()

        Try
            doc.LoadXml(propResult.output)
        Catch
            Return Nothing
        End Try

        Dim targets As XmlNodeList = doc.SelectNodes("/properties/target")

        For Each target As XmlNode In targets
            If target.Attributes Is Nothing OrElse target.Attributes("path") Is Nothing Then Continue For

            Dim propNode As XmlNode = target.SelectSingleNode("property")
            If propNode Is Nothing Then Continue For

            If String.Equals(propNode.InnerText.Trim(), "||RELEASED||", StringComparison.OrdinalIgnoreCase) Then
                released.Add(target.Attributes("path").Value)
            End If
        Next

        If released.Count = 0 Then Return Nothing
        Return released.ToArray()
    End Function

    Public Sub getLocksOfDocs(ByRef modDocArr() As ModelDoc2, Optional bBreakLocks As Boolean = False, Optional bUseTortoise As Boolean = False, Optional sMessage As String = "")
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc()
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Active Document not found") : Exit Sub

        'New/uncommitted CAD cannot be SVN-locked yet. Keep it writable and tell the user to Commit.
        makeFirstCommitCandidatesWritable(modDocArr)

        modDocArr = filterOutNewUnversionedOrAddedDocs(modDocArr)

        If modDocArr Is Nothing OrElse modDocArr.Length = 0 Then
            iSwApp.SendMsgToUser2(
            "No SVN lock needed." & vbCrLf & vbCrLf &
            "The selected file appears to be new and not committed yet." & vbCrLf &
            "Click Commit instead. The plugin will add it to SVN during the first commit.",
            swMessageBoxIcon_e.swMbInformation,
            swMessageBoxBtn_e.swMbOk
        )
            Exit Sub
        End If

        Dim sDocPathsToCheckout() As String = Nothing
        Dim sPathsOfReleased() As String
        Dim status As SVNStatus
        Dim bSuccess As Boolean = False
        Dim sCatMessage As String = ""
        Dim sFilter As String
        Dim bEachSuccess() As Boolean = Nothing

        'Speed fix:
        'Normal Get Locks only checks/locks the files passed in.
        'Get Locks With Dependents already passes dependents in modDocArr, so no extra dependency walk is needed here.
        status = getFileSVNStatus(bCheckServer:=True, modDocArr:=modDocArr)
        If IsNothing(status) Then Exit Sub

        Dim outOfDateBeforeLock As String() = status.sFilterUpToDate9("*")

        If outOfDateBeforeLock IsNot Nothing Then
            Dim msg As String =
        "One or more selected files are out of date." & vbCrLf & vbCrLf &
        "You should update to the latest geometry before getting locks." & vbCrLf & vbCrLf &
        "Out-of-date files:" & vbCrLf &
        stringArrToSingleStringWithNewLines(outOfDateBeforeLock, bTrimFileNames:=True, iLimit:=10) & vbCrLf &
        "Would you like to update them now?"

            Dim result As swMessageBoxResult_e = iSwApp.SendMsgToUser2(
        msg,
        swMessageBoxIcon_e.swMbWarning,
        swMessageBoxBtn_e.swMbYesNo
    )

            If result = swMessageBoxResult_e.swMbHitYes Then
                myGetLatestOrRevert(modDocArr, getLatestType.update, bVerbose:=True)

                status = getFileSVNStatus(bCheckServer:=True, modDocArr:=modDocArr)
                If IsNothing(status) Then Exit Sub

                outOfDateBeforeLock = status.sFilterUpToDate9("*")
                If outOfDateBeforeLock IsNot Nothing Then
                    iSwApp.SendMsgToUser2(
                "The selected files are still out of date after update. Lock cancelled.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
                    Exit Sub
                End If
            Else
                iSwApp.SendMsgToUser2(
            "Lock cancelled. Update to latest geometry before locking.",
            swMessageBoxIcon_e.swMbInformation,
            swMessageBoxBtn_e.swMbOk
        )
                Exit Sub
            End If
        End If

        If bBreakLocks Then
            sFilter = "*K"
        Else
            sFilter = "K"
        End If

        sPathsOfReleased = status.sFilterReleased("||RELEASED||")
        If sPathsOfReleased IsNot Nothing Then
            'There's Released files in here...
            If sMessage <> "#UP REV EDIT#" Then
                iSwApp.SendMsgToUser("Unable to lock the following files, since they are in 'RELEASED' state. Use 'EDIT New Revision' command to get edit access " & vbCrLf & String.Join(vbCrLf, sPathsOfReleased))
                status = status.statusFilter(sFiltReleasedRemoved:="||RELEASED||") ' removes released files.
            End If
        End If

        If status Is Nothing Then Exit Sub

        sDocPathsToCheckout = status.sFilterUpToDate9(sFilter, bFilterNot:=True)
        sDocPathsToCheckout = filterExistingCadFilePathsOnly(sDocPathsToCheckout)

        sCatMessage = catWithNewLine(status.sFilterUpToDate9(sFilter))

        If sCatMessage <> "" Then
            iSwApp.SendMsgToUser("Local copy is out of date. Update from Vault and try again." & vbCrLf & sCatMessage)

            If sDocPathsToCheckout Is Nothing OrElse sDocPathsToCheckout.Length = 0 Then
                Exit Sub
            End If
        End If

        If sDocPathsToCheckout Is Nothing OrElse sDocPathsToCheckout.Length = 0 Then
            iSwApp.SendMsgToUser("No CAD files available to be locked.")
            Exit Sub
        End If

        If bUseTortoise Then
            bSuccess = runTortoiseProcexeWithMonitor("/command:lock /path:" & formatFilePathArrForProc(sDocPathsToCheckout) & " /closeonend:3")
            If Not bSuccess Then iSwApp.SendMsgToUser("Locking Failed.") : Exit Sub
            svnPropset(sDocPathsToCheckout, "addin:release_state", "||EDIT||")
        Else
            bEachSuccess = svnlock(sDocPathsToCheckout, sMessage, bBreakLocks)

            If bEachSuccess Is Nothing OrElse Not bEachSuccess.Any(Function(x) x) Then
                iSwApp.SendMsgToUser("Locking Failed.")
                Exit Sub
            End If

            svnPropset(boolFilter(sDocPathsToCheckout, bEachSuccess), "addin:release_state", "||EDIT||")
        End If

        Try
            If bUseTortoise Then
                updateStatusCacheForKnownPaths(sDocPathsToCheckout, forceLock6:="K", forceReleased:="||EDIT||")
            ElseIf bEachSuccess IsNot Nothing Then
                updateStatusCacheForKnownPaths(boolFilter(sDocPathsToCheckout, bEachSuccess), forceLock6:="K", forceReleased:="||EDIT||")
            End If
        Catch
        End Try

        'Speed fix:
        'Do not rebuild every open tree after a lock. Rebuild only the active tree.
        bSuccess = updateLockStatusPublic(bRefreshAllTreeViews:=False)
        If Not bSuccess Then Exit Sub

        Try
            myUserControl.recolorCurrentTreeFromStatusPublic()
        Catch
            myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
        End Try

        Try
            Dim lockedNow() As String = If(
                bUseTortoise,
                sDocPathsToCheckout,
                If(bEachSuccess Is Nothing, Nothing, boolFilter(sDocPathsToCheckout, bEachSuccess))
            )
            If lockedNow IsNot Nothing AndAlso lockedNow.Length > 0 Then
                myUserControl.forceWriteAccessForLockedFilePathsPublic(lockedNow)
            End If
        Catch
        End Try
        keepNewUncommittedCadFilesWritable()

    End Sub
    Function svnlock(sModDocPathArr() As String, Optional sMessage As String = "", Optional bBreakLocks As Boolean = False) As Boolean()
        If sModDocPathArr Is Nothing Then Return Nothing

        sModDocPathArr = filterExistingCadFilePathsOnly(sModDocPathArr)

        If sModDocPathArr Is Nothing OrElse sModDocPathArr.Length = 0 Then
            iSwApp.SendMsgToUser("Error: No valid CAD file paths were passed to SVN lock.")
            Return Nothing
        End If

        Dim bSuccess(UBound(sModDocPathArr)) As Boolean
        Dim processOutputArr() As rawProcessReturn
        Dim failureMessages As New List(Of String)

        Try
            Dim lockArgs As String = "lock "

            If bBreakLocks Then
                lockArgs &= "--force "
            End If

            'One svn.exe call per file (bEach:=True), not one call for every path at once.
            'A lock is independent per file - e.g. a drawing's dependencies where one part is
            'already locked by a teammate. Bundling every path into a single "svn lock a b c"
            'call meant that ONE conflicting file made svn.exe emit a warning, which the old
            'code treated as total failure and reported every file - including ones with no
            'conflict at all - as not locked.
            processOutputArr = runSvnByArgs(
                sModDocPathArr,
                lockArgs,
                "-m",
                """" & sMessage & """",
                bEach:=True
            )

            If processOutputArr Is Nothing OrElse processOutputArr.Length = 0 Then
                iSwApp.SendMsgToUser("Error: SVN lock returned no process output.")
                Return bSuccess
            End If

            For i As Integer = 0 To UBound(sModDocPathArr)
                If i > UBound(processOutputArr) Then Exit For

                Dim thisError As String = ""
                If processOutputArr(i).outputError IsNot Nothing Then
                    thisError = processOutputArr(i).outputError.Trim()
                End If

                If thisError = "" Then
                    bSuccess(i) = True
                Else
                    bSuccess(i) = False

                    Dim shortName As String = sModDocPathArr(i)
                    Try
                        shortName = Path.GetFileName(sModDocPathArr(i))
                    Catch
                    End Try

                    failureMessages.Add(shortName & ": " & thisError)
                End If
            Next

            If failureMessages.Count > 0 Then
                iSwApp.SendMsgToUser(
                    "Some files could not be locked (often because someone else already holds the lock)." &
                    vbCrLf & vbCrLf &
                    String.Join(vbCrLf, failureMessages) &
                    vbCrLf & vbCrLf &
                    "Every other file in this request was still locked normally."
                )
            End If

            Return bSuccess

        Catch ex As Exception
            iSwApp.SendMsgToUser("Error running SVN lock: " & ex.Message)
            Return bSuccess
        End Try
    End Function
    Function verifyLocalRepoPath(Optional bInteractive As Boolean = True, Optional bCheckLocalFolder As Boolean = True, Optional bCheckServer As Boolean = True) As Boolean

        Dim response As swMessageBoxResult_e
        Dim processOutput As rawProcessReturn
        Dim arguments As String
        Dim sLocalPath As String

        If IsNothing(myUserControl) Then Return False

        sLocalPath = myUserControl.localRepoPath.Text

        If Not isOnlineModeEnabled() Then Return False

        'Check the file exists on the computer
        If bCheckLocalFolder Then
            If Not My.Computer.FileSystem.DirectoryExists(sLocalPath) Then
                If Not bInteractive Then Return False
                response = iSwApp.SendMsgToUser2(
                "Local Folder Location " & vbCrLf & sLocalPath & vbCrLf &
                "was not found. Would you like to select a new folder? ",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbYesNo)
                If response = swMessageBoxResult_e.swMbHitYes Then

                    If (myUserControl.pickFolder() = System.Windows.Forms.DialogResult.OK) Then
                        Return verifyLocalRepoPath(bInteractive, bCheckLocalFolder, bCheckServer)
                    Else
                        Return False
                    End If
                ElseIf response = swMessageBoxResult_e.swMbHitNo Then
                    iSwApp.SendMsgToUser2("Switching to offline.", swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)
                    switchToOffline()
                    Return False
                End If
            End If
            If Not bCheckServer Then Return True
        End If

        'Check the path is actually connected to a repo
        arguments = "info " & "--non-interactive """ & sLocalPath.TrimEnd("\\") & """" 'sFilePathCat 

        processOutput = runSvnProcess(sSVNPath, arguments)
        If processOutput.outputError.Contains("W155007:") Then
            If Not bInteractive Then Return False
            response = iSwApp.SendMsgToUser2("The following directory is not connected to an SVN Repository. " &
                                "Would you like to download the entire vault to this folder? " & vbCrLf & sLocalPath,
                                swMessageBoxIcon_e.swMbWarning,
                                swMessageBoxBtn_e.swMbYesNo)
            If response = swMessageBoxResult_e.swMbHitYes Then
                '1. Checkout entire folder
                runTortoiseProcexeWithMonitor(" /command:checkout /path " & sLocalPath)

                Return verifyLocalRepoPath(bInteractive, bCheckLocalFolder, bCheckServer)
            End If

            response = iSwApp.SendMsgToUser2("The following directory is not connected to an SVN Repository. " &
                                "Would you like to select a new folder? " & vbCrLf & sLocalPath,
                                swMessageBoxIcon_e.swMbWarning,
                                swMessageBoxBtn_e.swMbYesNo)
            If response = swMessageBoxResult_e.swMbHitYes Then

                If (myUserControl.pickFolder() = System.Windows.Forms.DialogResult.OK) Then
                    Return verifyLocalRepoPath(bInteractive, bCheckLocalFolder, bCheckServer)
                Else
                    Return False
                End If
            ElseIf response = swMessageBoxResult_e.swMbHitNo Then
                iSwApp.SendMsgToUser2("Switching to offline.", swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)
                switchToOffline()
                Return False
            Else
                Return False
            End If
        Else
            Return True
        End If


        Return False ' code shouldn't get here...

    End Function
    'Public Sub sendFilePathsToClipboard(modDocArr As ModelDoc2())

    '    'Dim sModDocPathArr As String()
    '    'Dim sFileNames As String

    '    If modDocArr Is Nothing Then Exit Sub
    '    'If Not verifyLocalRepoPath() Then Return Nothing

    '    'Dim sDest As String = localRepoPath.Text & "\" & "fileList.txt"



    'End Sub

    Public Function getUrlfromPaths(sPaths As String()) As String()
        ' Run the SVN command and get XML output
        Dim rawXmlLines As svnModule.rawProcessReturn = runSvnProcess(sSVNPath, "info --xml " & formatFilePathArrForProc(sPaths, sDelimiter:=""" """) & """")
        Dim xmlOutput As String = String.Join(vbCrLf, rawXmlLines.output)

        ' Handle errors
        If rawXmlLines.outputError.Length > 0 Then
            iSwApp.SendMsgToUser(rawXmlLines.outputError)
            Return Nothing
        End If
        If String.IsNullOrWhiteSpace(xmlOutput) Then
            iSwApp.SendMsgToUser("Unable to get file info")
            Return Nothing
        End If

        ' Parse XML
        Dim doc As New XmlDocument()
        Try
            doc.LoadXml(xmlOutput)
        Catch ex As Exception
            iSwApp.SendMsgToUser("Invalid XML returned from SVN: " & ex.Message)
            Return Nothing
        End Try

        ' Get all <entry> nodes under <info>
        Dim entries As XmlNodeList = doc.SelectNodes("/info/entry")
        Dim resultList As New List(Of String)

        For Each entry As XmlNode In entries
            Dim urlNode As XmlNode = entry.SelectSingleNode("url")
            If urlNode IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(urlNode.InnerText) Then
                resultList.Add(urlNode.InnerText.Trim())
            End If
        Next

        Return resultList.ToArray()
    End Function

    Public Sub openFileNameInWebpage(sUrlInput As String, modDoc As ModelDoc2)
        'requires '%s' in the url, which will be replaced by the search string


        If modDoc Is Nothing Then
            iSwApp.SendMsgToUser("No active document found.")
            Exit Sub
        End If

        Dim title As String = Path.GetFileNameWithoutExtension(getTitleClean(modDoc))

        If String.IsNullOrWhiteSpace(title) Then
            iSwApp.SendMsgToUser("Document title is empty.")
            Exit Sub
        End If

        ' URL encode the title to be safe for use in the URL
        Dim encodedTitle As String = Uri.EscapeDataString(title)
        Dim url As String = $"" & sUrlInput.Replace("%s", encodedTitle)

        Try
            Process.Start(New ProcessStartInfo With {
                .FileName = url,
                .UseShellExecute = True
            })
        Catch ex As Exception
            MessageBox.Show("Failed to open browser: " & ex.Message)
        End Try

    End Sub

    Public Function runSvnByArgs(sModDocPathArr() As String, sArg1 As String, Optional sArg2 As String = "", Optional sArg3 As String = "", Optional bEach As Boolean = True) As rawProcessReturn()

        'sModDocPathArr()  = getFilePathsFromModDocArr(modDocArr)
        Dim arguments As String
        Dim processOutputArr(0) As rawProcessReturn
        If bEach Then ReDim processOutputArr(UBound(sModDocPathArr))
        Dim sFullPath As String = ""
        Dim iErr As Integer = 0
        Dim i As Integer = 0

        If IsNothing(sModDocPathArr) Then Return Nothing

        ' Pad spaces to separate arguments
        sArg1 &= " "
        If Not sArg2 = "" Then sArg2 &= " "
        If Not sArg3 = "" Then sArg3 &= " "
        arguments = sArg1 & sArg2 & sArg3

        If bEach Then
            For Each sPath As String In sModDocPathArr
                If sPath Is Nothing Then Continue For
                sPath = """" & sPath & """"
                processOutputArr(i) = runSvnProcess(sSVNPath, arguments & sPath)
                i += 1
            Next
        Else
            For Each sPath As String In sModDocPathArr
                If sPath Is Nothing Then Continue For
                sFullPath = sFullPath & """" & sPath & """ "
            Next
            processOutputArr(0) = runSvnProcess(sSVNPath, arguments & sFullPath)

        End If
        Return processOutputArr

        'for each processOutputArr in processOutputArr
        '    sOutputLines = processOutputArr.output.Split(ControlChars.CrLf.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        '    sOutputErrorLines = processOutputArr.outputError.Split(ControlChars.CrLf.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        ''Error Checking
        'If iErr > 10 Then Return Nothing 'Prevents user getting stuck with too many error messages
        'If (sOutputErrorLines Is Nothing) Or (sOutputLines Is Nothing) Then
        '    iSwApp.SendMsgToUser("Error: SVN propget (get property) output is nothing!")
        'End If

        'If sOutputErrorLines.Length <> 0 Then
        '    'We got some errors if length > 0
        '    'For i = 0 To UBound(sOutputErrorLines)
        '    '    If sOutputErrorLines(i).Contains("E215004") Then
        '    '        'Log in Failed!
        '    '    End If
        '    'Next
        '    iErr = iErr + 1
        '    iSwApp.SendMsgToUser("Error: " & sOutputErrorLines(0))
        'End If

    End Function
    Public Function svnCommitDocs(modDocArr As ModelDoc2(), sCommitMessage As String) As Boolean
        Dim processOutputArr() As rawProcessReturn
        Dim sOutputLines() As String
        Dim sOutputErrorLines() As String
        Dim iErr As Integer = 0
        Dim bSuccess As Boolean = True
        Dim sModDocPathArr As String() = getFilePathsFromModDocArr(modDocArr)

        keepNewUncommittedCadFilesWritable()

        Dim saveResult As swMessageBoxResult_e

        beginInternalSolidWorksSave()
        Try
            saveResult = save3AndShowErrorMessages(modDocArr)
        Finally
            endInternalSolidWorksSave()
        End Try

        If saveResult <> swMessageBoxResult_e.swMbHitYes Then Return False

        keepNewUncommittedCadFilesWritable()

        processOutputArr = runSvnByArgs(sModDocPathArr, "commit", "-m", """" & sCommitMessage & """", bEach:=False)

        For Each processOutput In processOutputArr
            sOutputLines = processOutput.output.Split(ControlChars.CrLf.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
            sOutputErrorLines = processOutput.outputError.Split(ControlChars.CrLf.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
            'Error Checking
            '            If iErr > 10 Then Return Nothing 'Prevents user getting stuck with too many error messages
            If (sOutputErrorLines Is Nothing) Or (sOutputLines Is Nothing) Then
                iSwApp.SendMsgToUser("Error: SVN commit output is nothing!")
                bSuccess = False

            ElseIf sOutputErrorLines.Length <> 0 Then
                'We got some errors if length > 0
                'For i = 0 To UBound(sOutputErrorLines)
                '    If sOutputErrorLines(i).Contains("E215004") Then
                '        'Log in Failed!
                '    End If
                'Next
                If iErr < 10 Then 'limits
                    iSwApp.SendMsgToUser("Error: " & String.Join(vbCrLf, sOutputErrorLines))
                    iErr += 1
                End If
                bSuccess = False
            End If
        Next
        Return bSuccess
    End Function
    Public Function svnPropset(sModDocPathArr() As String, sPropertyName As String, sPropertyValue As String) As Boolean
        Dim processOutputArr() As rawProcessReturn
        Dim sOutputLines() As String
        Dim sOutputErrorLines() As String
        Dim iErr As Integer = 0
        Dim bSuccess As Boolean = True

        If sModDocPathArr Is Nothing Then Return Nothing

        processOutputArr = runSvnByArgs(sModDocPathArr, "propset", sPropertyName, sPropertyValue, bEach:=True)

        For Each processOutput In processOutputArr
            If (processOutput.output Is Nothing) Or (processOutput.outputError Is Nothing) Then
                bSuccess = False
                Continue For
            End If

            sOutputLines = processOutput.output.Split(ControlChars.CrLf.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
            sOutputErrorLines = processOutput.outputError.Split(ControlChars.CrLf.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
            'Error Checking
            '            If iErr > 10 Then Return Nothing 'Prevents user getting stuck with too many error messages
            If (sOutputErrorLines Is Nothing) Or (sOutputLines Is Nothing) Then
                iSwApp.SendMsgToUser("Error: SVN propget output is nothing!")
                bSuccess = False

            ElseIf sOutputErrorLines.Length <> 0 Then
                'We got some errors if length > 0
                'For i = 0 To UBound(sOutputErrorLines)
                '    If sOutputErrorLines(i).Contains("E215004") Then
                '        'Log in Failed!
                '    End If
                'Next
                If iErr < 10 Then 'limits
                    iSwApp.SendMsgToUser("Error: " & String.Join(vbCrLf, sOutputErrorLines))
                    iErr += 1
                End If
                bSuccess = False
            End If
        Next
        Return bSuccess
    End Function

    Public Function svnPropget(Optional sFilename As String = "") As String(,)

        If sFilename = "" Then sFilename = """" & myUserControl.localRepoPath.Text.TrimEnd("\\") & """"

        Dim rawXmlLines As svnModule.rawProcessReturn = runSvnProcess(sSVNPath, "propget addin:release_state -R " & sFilename & " --xml")
        Dim xmlOutput As String = String.Join(vbCrLf, rawXmlLines.output)

        If rawXmlLines.outputError.Length > 0 Then
            iSwApp.SendMsgToUser(rawXmlLines.outputError)
            Return Nothing
        End If
        If xmlOutput Is Nothing Then Return Nothing
        If xmlOutput = "" Then Return Nothing

        ' Load the XML into an XmlDocument
        Dim doc As New XmlDocument()
        doc.LoadXml(xmlOutput)

        ' Get all <target> nodes
        Dim targets As XmlNodeList = doc.SelectNodes("/properties/target")

        ' Prepare a list to hold the path/property pairs
        Dim resultList As New List(Of String())

        ' Loop through each <target>
        For Each target As XmlNode In targets
            Dim path As String = target.Attributes("path")?.Value
            Dim propertyNode As XmlNode = target.SelectSingleNode("property")

            If path IsNot Nothing AndAlso propertyNode IsNot Nothing Then
                Dim propValue As String = propertyNode.InnerText
                resultList.Add(New String() {path, propValue})
            End If
        Next

        ' Convert the list to a 2D array
        Dim resultArray(resultList.Count - 1, 1) As String
        For i As Integer = 0 To resultList.Count - 1
            resultArray(i, 0) = resultList(i)(0) ' path
            resultArray(i, 1) = resultList(i)(1) ' property
        Next

        Return resultArray

    End Function
    Public Function ensureResolvedComponent(ByRef swcomp As Component2) As Boolean
        Dim suppChangeError As swSuppressionError_e
        Dim lightSuppressState As swComponentSuppressionState_e

        If swcomp Is Nothing Then Return False

        lightSuppressState = swcomp.GetSuppression2

        'Do NOT automatically unsuppress suppressed components.
        'Users may suppress components for performance or visibility.
        If lightSuppressState = swComponentSuppressionState_e.swComponentSuppressed Then
            Return False
        End If

        'Only resolve lightweight / fully-lightweight components when an explicit action asks for it.
        If lightSuppressState = swComponentSuppressionState_e.swComponentLightweight OrElse
            lightSuppressState = swComponentSuppressionState_e.swComponentFullyLightweight Then

            suppChangeError = swcomp.SetSuppression2(swComponentSuppressionState_e.swComponentResolved)

            If suppChangeError = swSuppressionError_e.swSuppressionChangeOk Then
                Return True
            Else
                Return False
            End If
        End If

        Return True
    End Function
    Public Function sGetDescription(modDoc As ModelDoc2) As String
        'https://help.solidworks.com/2023/english/api/sldworksapi/Get_Custom_Properties_of_Referenced_Part_Example_VBNET.htm
        Dim swModelDocExt As ModelDocExtension
        Dim swCustProp As CustomPropertyManager
        Dim val As String = ""
        Dim valout As String = ""
        Dim bool As Boolean
        If modDoc Is Nothing Then Return Nothing

        Try
            swModelDocExt = modDoc.Extension

            swCustProp = swModelDocExt.CustomPropertyManager("")
            bool = swCustProp.Get4("Property_Name", False, val, valout)

            'Debug.Print("Value:                    " & val)
            'Debug.Print("Evaluated value:          " & valout)
            'Debug.Print("Up-to-date data:          " & bool)

            Return valout
        Catch
            Return Nothing
        End Try
    End Function
    Public Sub subShowLog(sFilePath As String)
        Debug.Print(sFilePath)
        iSwApp.SendMsgToUser("Log is for VIEWING ONLY!" & vbCrLf & vbCrLf & "Advanced features inside the Log window that overwrite files (Revert, etc) will lockup svn. To use those advanced features, close SolidWorks, and use TortoiseSVN > Show Log in Windows Explorer.")
        runTortoiseProcexeWithMonitor("/command:log /path:""" & sFilePath & """")
    End Sub

    Public Sub myGetLatestOrRevertPaths(ByVal selectedPaths() As String,
                                        Optional ByVal myGetType As getLatestType = getLatestType.update,
                                        Optional ByVal bVerbose As Boolean = False)
        Dim i As Integer
        Dim j As Integer = 0
        Dim status As SVNStatus = Nothing
        Dim bSuccess As Boolean = True
        Dim sw As New Stopwatch
        Dim debugWatch As Stopwatch = Nothing
        Dim debugNotes As New List(Of String)()
        Dim phaseStartMs As Long = 0

        If debugTimingEnabled() Then
            debugWatch = Stopwatch.StartNew()
        End If

        Dim filteredPaths() As String = distinctExistingCadFilePaths(selectedPaths)

        If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No valid selected CAD file paths were found for Get Latest.", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

        If ((myGetType = getLatestType.both) OrElse (myGetType = getLatestType.update)) Then
            'Optimized selected Get Latest path:
            'Trust the last explicit Sync result. This avoids another slow server status call.
            'If the user has not synced this selection yet, ask them to Sync first.
            status = getCachedServerStatusForExactPaths(filteredPaths, requireEveryPathCached:=True)

            If debugWatch IsNot Nothing Then debugNotes.Add("Cached Sync status lookup: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

            If status Is Nothing OrElse status.fp Is Nothing Then
                iSwApp.SendMsgToUser2(
                    "Get Latest selected files needs a recent Sync result first." & vbCrLf & vbCrLf &
                    "Run Sync on the branch, select the out-of-date item(s) in the SVN tree, then click Get Latest." & vbCrLf & vbCrLf &
                    "Tip: Ctrl-click toggles multiple files. Shift-click selects a visible range.",
                    swMessageBoxIcon_e.swMbInformation,
                    swMessageBoxBtn_e.swMbOk
                )

                If debugWatch IsNot Nothing Then
                    debugNotes.Add("Selected paths: " & filteredPaths.Length.ToString())
                    debugNotes.Add("No usable cached server status. No update attempted.")
                    debugNotes.Add("Total selected Get Latest time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
                    showSvnTimingDebugWindow("Get Latest selected stopped - Sync needed.", debugNotes)
                End If

                Exit Sub
            End If
        Else
            status = getFileSVNStatus(
                bCheckServer:=False,
                modDocArr:=Nothing,
                bUpdateStatusOfAllOpenModels:=False,
                sDirectFilePathArr:=filteredPaths
            )

            If debugWatch IsNot Nothing Then debugNotes.Add("Local SVN status for selected paths: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        End If

        attachOpenDocsToStatusPaths(status)

        If IsNothing(status) Then Exit Sub
        If status.fp Is Nothing Then Exit Sub

        Dim sFileList(UBound(status.fp)) As String
        Dim selectedPathsRevertedForCache() As String = Nothing
        Dim selectedPathsUpdatedForCache() As String = Nothing

        If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

        For i = 0 To UBound(status.fp)
            If String.IsNullOrWhiteSpace(status.fp(i).filename) Then Continue For

            If (status.fp(i).upToDate9 = "*") AndAlso ((myGetType = getLatestType.update) OrElse (myGetType = getLatestType.both)) Then
                status.fp(i).revertUpdate = getLatestType.update
                sFileList(j) = status.fp(i).filename
                j += 1

            ElseIf (status.fp(i).addDelChg1 = "M") AndAlso ((myGetType = getLatestType.revert) OrElse (myGetType = getLatestType.both)) Then
                status.fp(i).revertUpdate = getLatestType.revert
                sFileList(j) = status.fp(i).filename
                j += 1
            End If

        Next

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Filter selected files needing update/revert: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            debugNotes.Add("Selected files checked: " & filteredPaths.Length.ToString())
            debugNotes.Add("Files needing action: " & j.ToString())
        End If

        If j = 0 Then
            If bVerbose Then
                iSwApp.SendMsgToUser2(
                    "Selected file(s) are not marked out-of-date by the last Sync." & vbCrLf & vbCrLf &
                    "Run Sync again if you expected an update.",
                    swMessageBoxIcon_e.swMbInformation,
                    swMessageBoxBtn_e.swMbOk
                )
            End If

            If debugWatch IsNot Nothing Then
                debugNotes.Add("Total selected Get Latest time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
                showSvnTimingDebugWindow("Get Latest selected finished - nothing to update.", debugNotes)
            End If

            Exit Sub
        End If

        Dim pathsNeedingAction() As String = compactNonBlankStringArray(sFileList)
        If Not userAcceptsLossOfChangesPaths(pathsNeedingAction, "Update/revert the following selected file(s) to vault version?") Then Exit Sub

        sw.Start()
        System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.WaitCursor

        Try
            Dim indexOfFilestoRevert As Integer() = status.indexFilterGetLatestType(getLatestType.revert, bIgnoreUpdate:=False)

            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            status.releaseFileSystemAccessToRevertOrUpdateModels(iSwApp, indexOfFilestoRevert)
            If debugWatch IsNot Nothing Then debugNotes.Add("Release SolidWorks file handles for selected revert: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

            sFileList = status.sFilterGetLatestType(getLatestType.revert, bIgnoreUpdate:=False)
            selectedPathsRevertedForCache = sFileList

            If (Not sFileList Is Nothing) AndAlso ((myGetType = getLatestType.revert) OrElse (myGetType = getLatestType.both)) Then
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                bSuccess = runTortoiseProcexeWithMonitor("/command:revert /path:" & formatFilePathArrForProc(sFileList) & " /closeonend:3")
                If debugWatch IsNot Nothing Then debugNotes.Add("TortoiseSVN selected revert call: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
                If Not bSuccess Then iSwApp.SendMsgToUserv("Revert Files Failed.")
            End If

            Dim indexOfFilestoUpdate As Integer() = status.indexFilterGetLatestType(getLatestType.update, bIgnoreUpdate:=False)

            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            status.releaseFileSystemAccessToRevertOrUpdateModels(iSwApp, indexOfFilestoUpdate)
            If debugWatch IsNot Nothing Then debugNotes.Add("Release SolidWorks file handles for selected update: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

            sFileList = status.sFilterGetLatestType(getLatestType.update, bIgnoreUpdate:=False)
            selectedPathsUpdatedForCache = sFileList

            If (Not sFileList Is Nothing) AndAlso ((myGetType = getLatestType.update) OrElse (myGetType = getLatestType.both)) Then
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                bSuccess = runTortoiseProcexeWithMonitor("/command:update /path:" & formatFilePathArrForProc(sFileList) & " /closeonend:3")
                If debugWatch IsNot Nothing Then debugNotes.Add("TortoiseSVN selected update call: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
                If Not bSuccess Then iSwApp.SendMsgToUserv("Updating Files Failed.")
            End If

            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            status.reattachDocsToFileSystem(indexOfFilestoRevert, iSwApp)
            status.reattachDocsToFileSystem(indexOfFilestoUpdate, iSwApp)
            If debugWatch IsNot Nothing Then debugNotes.Add("Reattach selected docs to filesystem: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

            Try
                If selectedPathsRevertedForCache IsNot Nothing Then
                    updateStatusCacheForKnownPaths(selectedPathsRevertedForCache, forceAddDelChg1:=" ")
                End If

                If selectedPathsUpdatedForCache IsNot Nothing Then
                    updateStatusCacheForKnownPaths(selectedPathsUpdatedForCache, forceAddDelChg1:=" ", forceUpToDate9:=" ")
                End If
            Catch
            End Try

            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                updateLockStatusPublic(bRefreshAllTreeViews:=False)
                refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)
                If debugWatch IsNot Nothing Then debugNotes.Add("Post-selected-action local status/tree refresh: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
            End Try

        Finally
            System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Default
        End Try

        sw.Stop()

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Total selected Get Latest time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
            showSvnTimingDebugWindow("Get Latest selected finished.", debugNotes)
        End If

        Debug.WriteLine("myGetLatestOrRevertPaths Time Taken: " + sw.Elapsed.TotalMilliseconds.ToString("#,##0.00 'milliseconds'"))
    End Sub

    Sub myGetLatestOrRevert(Optional ByRef modDocArr As ModelDoc2() = Nothing,
                        Optional ByRef myGetType As getLatestType = getLatestType.update,
                        Optional ByRef bVerbose As Boolean = False)
        Dim i As Integer
        Dim j As Integer = 0
        Dim status As SVNStatus
        Dim bSuccess As Boolean = True
        Dim sw As New Stopwatch
        Dim debugWatch As Stopwatch = Nothing
        Dim debugNotes As New List(Of String)()
        Dim phaseStartMs As Long = 0
        Dim needsServerCheck As Boolean = ((myGetType = getLatestType.both) OrElse (myGetType = getLatestType.update))

        If debugTimingEnabled() Then
            debugWatch = Stopwatch.StartNew()
        End If

        If ((myGetType = getLatestType.both) Or (myGetType = getLatestType.update)) Then
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            If Not userAcceptsLossOfChanges(modDocArr, "Update the following Files to latest vault version?") Then Exit Sub
            If debugWatch IsNot Nothing Then debugNotes.Add("User confirm / local-change safety check: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        End If

        If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

        'Speed fix:
        'Update/Get Latest must contact the server to know what is out of date.
        'Revert does not need a server check; local SVN status is enough and is much faster.
        If IsNothing(modDocArr) Then
            If needsServerCheck Then
                updateStatusOfAllModelsVariable(bRefreshAllTreeViews:=False)
            Else
                updateLockStatusPublic(bRefreshAllTreeViews:=False)
            End If

            status = statusOfAllOpenModels
        Else
            status = getFileSVNStatus(
                bCheckServer:=needsServerCheck,
                modDocArr:=modDocArr,
                bUpdateStatusOfAllOpenModels:=False
            )
        End If

        If debugWatch IsNot Nothing Then
            If needsServerCheck Then
                debugNotes.Add("SVN status pre-check (server): " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Else
                debugNotes.Add("SVN status pre-check (local only): " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            End If
        End If

        If IsNothing(status) Then Exit Sub
        If status.fp Is Nothing Then Exit Sub

        Dim sFileList(UBound(status.fp)) As String
        Dim pathsRevertedForCache() As String = Nothing
        Dim pathsUpdatedForCache() As String = Nothing

        If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

        For i = 0 To UBound(status.fp)
            If status.fp(i).modDoc Is Nothing Then Continue For

            If (status.fp(i).upToDate9 = "*") And ((myGetType = getLatestType.update) Or (myGetType = getLatestType.both)) Then
                status.fp(i).revertUpdate = getLatestType.update
                sFileList(j) = status.fp(i).filename
                j += 1

            ElseIf (status.fp(i).addDelChg1 = "M") And ((myGetType = getLatestType.revert) Or (myGetType = getLatestType.both)) Then
                status.fp(i).revertUpdate = getLatestType.revert
                sFileList(j) = status.fp(i).filename
                j += 1
            End If
        Next

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Filter files needing update/revert: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            debugNotes.Add("Files needing action: " & j.ToString())
        End If

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            reconcileWriteAccessForActiveDocumentPublic()
            reconcileReadOnlyForUnlockedActiveDocumentPublic()
            If debugWatch IsNot Nothing Then debugNotes.Add("Reconcile active document read/write state: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        Catch
        End Try

        If j = 0 Then
            If bVerbose Then iSwApp.SendMsgToUser("All Files Checked Are Up to Date!")

            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

                'Speed fix: no second server status call when nothing changed.
                updateLockStatusPublic(bRefreshAllTreeViews:=False)
                refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)

                If debugWatch IsNot Nothing Then debugNotes.Add("Post no-op local status/tree refresh: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
            End Try

            If debugWatch IsNot Nothing Then
                debugNotes.Add("Total Get Latest/Revert time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
                showSvnTimingDebugWindow("Get Latest/Revert finished - nothing to update.", debugNotes)
            End If

            Exit Sub
        End If

        sw.Start()
        System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.WaitCursor

        Try
            Dim indexOfFilestoRevert As Integer() = status.indexFilterGetLatestType(getLatestType.revert, bIgnoreUpdate:=False)

            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            status.releaseFileSystemAccessToRevertOrUpdateModels(iSwApp, indexOfFilestoRevert)
            If debugWatch IsNot Nothing Then debugNotes.Add("Release SolidWorks file handles for revert: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

            sFileList = status.sFilterGetLatestType(getLatestType.revert, bIgnoreUpdate:=False)
            pathsRevertedForCache = sFileList

            If (Not sFileList Is Nothing) And ((myGetType = getLatestType.revert) Or (myGetType = getLatestType.both)) Then
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                bSuccess = runTortoiseProcexeWithMonitor("/command:revert /path:" & formatFilePathArrForProc(sFileList) & " /closeonend:3")
                If debugWatch IsNot Nothing Then debugNotes.Add("TortoiseSVN revert call: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
                If Not bSuccess Then iSwApp.SendMsgToUserv("Revert Files Failed.")
            End If

            Dim indexOfFilestoUpdate As Integer() = status.indexFilterGetLatestType(getLatestType.update, bIgnoreUpdate:=False)

            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            status.releaseFileSystemAccessToRevertOrUpdateModels(iSwApp, indexOfFilestoUpdate)
            If debugWatch IsNot Nothing Then debugNotes.Add("Release SolidWorks file handles for update: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

            sFileList = status.sFilterGetLatestType(getLatestType.update, bIgnoreUpdate:=False)
            pathsUpdatedForCache = sFileList

            If (Not sFileList Is Nothing) And ((myGetType = getLatestType.update) Or (myGetType = getLatestType.both)) Then
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
                bSuccess = runTortoiseProcexeWithMonitor("/command:update /path:" & formatFilePathArrForProc(sFileList) & " /closeonend:3")
                If debugWatch IsNot Nothing Then debugNotes.Add("TortoiseSVN update call: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
                If Not bSuccess Then iSwApp.SendMsgToUserv("Updating Files Failed.")
            End If

            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            status.reattachDocsToFileSystem(indexOfFilestoRevert, iSwApp)
            status.reattachDocsToFileSystem(indexOfFilestoUpdate, iSwApp)
            If debugWatch IsNot Nothing Then debugNotes.Add("Reattach docs to filesystem: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")

            Try
                If pathsRevertedForCache IsNot Nothing Then
                    updateStatusCacheForKnownPaths(pathsRevertedForCache, forceAddDelChg1:=" ")
                End If

                If pathsUpdatedForCache IsNot Nothing Then
                    updateStatusCacheForKnownPaths(pathsUpdatedForCache, forceAddDelChg1:=" ", forceUpToDate9:=" ")
                End If
            Catch
            End Try

            Try
                If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

                'Speed fix: after Tortoise completes, do a local status/tree refresh only.
                'The expensive server re-check belongs under explicit Sync Status, not every Get Latest / Revert finish.
                updateLockStatusPublic(bRefreshAllTreeViews:=False)
                refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)

                If debugWatch IsNot Nothing Then debugNotes.Add("Post-action local status/tree refresh: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            Catch
            End Try

        Finally
            System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Default
        End Try

        sw.Stop()

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Total Get Latest/Revert time: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
            showSvnTimingDebugWindow("Get Latest/Revert finished.", debugNotes)
        End If

        Debug.WriteLine("myGetLatestOrRevert Time Taken: " + sw.Elapsed.TotalMilliseconds.ToString("#,##0.00 'milliseconds'"))
    End Sub

    Public Enum getLatestType
        undefined = -1
        none = 0
        revert = 1
        update = 2
        both = 3
    End Enum
    Private Function formatFilePathArrForSvnProc(ByVal sFilePathArr() As String) As String
        If sFilePathArr Is Nothing OrElse sFilePathArr.Length = 0 Then Return ""

        Dim output As New List(Of String)()

        For Each filePath As String In sFilePathArr
            If String.IsNullOrWhiteSpace(filePath) Then Continue For
            If filePath.Contains("~~") Then Continue For
            output.Add("""" & filePath & """")
        Next

        If output.Count = 0 Then Return ""
        Return String.Join(" ", output.ToArray())
    End Function

    Function formatFilePathArrForProc(ByRef sFilePathArr() As String, Optional sDelimiter As String = "*") As String
        'Use "*" delimiter for tortoiseProc.exe, and " " (space) for SVN.exe
        'Dim bSkipDelimiterForFirstOne As Boolean = True
        Dim sFilePathCat As String = ""


        For i = 0 To sFilePathArr.Length - 1
            If sFilePathArr(i) Is Nothing Then Continue For
            If sFilePathArr(i).Contains("~~") Then Continue For 'skip in-context parts/assemblies.

            'If bSkipDelimiterForFirstOne Then
            '    sFilePathCat &= sFilePathArr(i)
            '    bSkipDelimiterForFirstOne = False
            'Else
            sFilePathCat &= sDelimiter & sFilePathArr(i)
            'End If

        Next

        sFilePathCat = sFilePathCat.Trim(sDelimiter) 'removes first delimiter

        If sDelimiter = "*" Then             'for tortoiseproc
            sFilePathCat = """" & sFilePathCat & """"
        Else
            'sFilePathCat = sFilePathCat & """"
        End If

        Return sFilePathCat
    End Function
    Function formatModDocArrForTortoiseProc(ByRef modDocArr() As ModelDoc2) As String
        Dim sFilePathCat As String = """" '& modDocArr(0).GetPathName
        Dim sTempPathName As String
        Dim bSkipAsterixForFirstOne As Boolean = True

        For i = 0 To UBound(modDocArr)
            If modDocArr(i) Is Nothing Then Continue For
            sTempPathName = modDocArr(i).GetPathName
            If sTempPathName.Contains("~~") Then Continue For    'skip in-context parts/assemblies.

            If bSkipAsterixForFirstOne Then
                sFilePathCat &= sTempPathName
                bSkipAsterixForFirstOne = False
            Else
                sFilePathCat &= "*" & sTempPathName
            End If
        Next
        sFilePathCat &= """"
        Return sFilePathCat
    End Function

    Function runTortoiseProcexeWithMonitor(ByRef sArguments As String) As Boolean
        ' See https://tortoisesvn.net/docs/release/TortoiseSVN_en/tsvn-automation.html
        Using oTortProcess As New Process()
            Dim tortStartInfo As New ProcessStartInfo
            'Dim sw As New Stopwatch
            'sw.Start()

            tortStartInfo.FileName = sTortPath  'System.Environment.CurrentDirectory & "\\TortoiseProc.exe" 'AppDomain.CurrentDomain.BaseDirectory & 'sTortPath
            'iSwApp.SendMsgToUser(sTortPath)

            If sArguments.Length > (32768 - 1) Then
                iSwApp.SendMsgToUser2("Error: Too many arguments sent from the Add-In to TortoiseSVN, " +
                                      "likely caused by doing an action to too many components." +
                                      "You can do the action using TortoiseSVN in Windows Explorer," +
                                      "then back in the Add-in hit the Refresh command.",
                                        swMessageBoxIcon_e.swMbStop, swMessageBoxBtn_e.swMbOk)
                Return False 'Avoids error. https://stackoverflow.com/questions/9115279/commandline-argument-parameter-limitation
            End If

            tortStartInfo.Arguments = sArguments
            If Not verifyLocalRepoPath() Then Return Nothing
            tortStartInfo.WorkingDirectory = myUserControl.localRepoPath.Text
            oTortProcess.StartInfo = tortStartInfo
            oTortProcess.Start()

            'Monitor the process. Kill it if it stops responding.
            'HasExited/Responding/Kill can each throw if TortoiseProc exits in the instant between
            'the check and the call (a real race, not hypothetical). Never let that escape
            'uncaught into the SOLIDWORKS callback that triggered this action.
            Try
                Dim unresponsiveSinceUtc As DateTime = DateTime.MinValue
                Do While (Not oTortProcess.HasExited)
                    If oTortProcess.Responding Then
                        unresponsiveSinceUtc = DateTime.MinValue
                    ElseIf unresponsiveSinceUtc = DateTime.MinValue Then
                        unresponsiveSinceUtc = DateTime.UtcNow
                    End If

                    If unresponsiveSinceUtc <> DateTime.MinValue AndAlso
                       (DateTime.UtcNow - unresponsiveSinceUtc).TotalSeconds >= 30.0 Then
                        Try
                            oTortProcess.Kill()
                        Catch
                            'Already exited on its own - nothing left to kill.
                        End Try
                        iSwApp.SendMsgToUser("SVNTortoise Window Timed Out")
                        Return False
                    End If
                    System.Threading.Thread.Sleep(100)
                Loop
            Catch
                'The process ended on its own mid-check. Treat as a normal completion.
            End Try

            'sw.Stop()
            'System.Diagnostics.Debug.WriteLine("tortoiseProc Time Taken: " + sw.Elapsed.TotalMilliseconds.ToString("#,##0.00 'milliseconds'"))

            Return True
        End Using
    End Function

    Sub switchToOffline()
        setOnlineModeEnabled(False)

        clearMyTree("Offline. Click Checkbox at top of add-in to go online.")

    End Sub

    Public Sub clearMyTree(Optional ByVal message As String = "No Status Available for Any Open Files")

        myUserControl.allTreeViews = Nothing

        Dim msgTreeNode As TreeNode
        msgTreeNode = New TreeNode(message)

        myUserControl.TreeView1.Nodes.Clear()
        myUserControl.TreeView1.Nodes.Insert(0, msgTreeNode)
        myUserControl.TreeView1.Show()

    End Sub

    Public Function sGetFileNames(status As SVNStatus) As String()
        If status Is Nothing Then Return Nothing
        Dim returnsGetFileNames(UBound(status.fp)) As String
        Dim i, j As Integer
        If status.fp Is Nothing Then Return Nothing
        j = 0

        For i = 0 To UBound(status.fp)
            Try
                returnsGetFileNames(i - j) = status.fp(i).filename
            Catch
                j += 1
            End Try

        Next

        If j > 0 Then
            If i = j Then Return Nothing
            ReDim Preserve returnsGetFileNames(UBound(returnsGetFileNames) - j)
        End If

        Return returnsGetFileNames
    End Function
    'Public Function sGetFileNames(modDoc As ModelDoc2) As String()
    '    Dim returnsGetFileNames(UBound(status.fp)) As String

    '    If status.fp Is Nothing Then Return Nothing

    '    For i = 0 To UBound(status.fp)
    '        returnsGetFileNames(i) = status.fp(i).filename
    '    Next
    '    Return returnsGetFileNames
    'End Function


    Public Function findStatusForFile(ByRef sFileName As String) As SVNStatus
        Dim output As SVNStatus = New SVNStatus()
        Dim cachedFp As New SVNStatus.filePpty()

        If String.IsNullOrWhiteSpace(sFileName) Then Return Nothing

        'This helper is called while painting/coloring tree nodes. A visual lookup must never
        'silently launch a server-aware SVN status operation; that made initial display depend
        'on network timing and caused the task pane to churn until the user clicked Sync.
        'Unknown is a valid temporary visual state. Explicit Sync/local refresh populates the
        'cache and recolors the tree through their normal completion paths.
        If IsNothing(statusOfAllOpenModels) Then Return Nothing

        If tryFindCachedStatusProperty(sFileName, cachedFp) Then
            ReDim output.fp(0)
            output.fp(0) = cachedFp
            Return output
        End If

        'Fallback to the original contains-based scan for unusual legacy inputs.
        Try
            ReDim output.fp(0)

            For i As Integer = 0 To UBound(statusOfAllOpenModels.fp)
                If (Strings.InStr(statusOfAllOpenModels.fp(i).filename, sFileName, CompareMethod.Text) <> 0) Then
                    output.fp(0) = statusOfAllOpenModels.fp(i)
                    Return output
                End If
            Next
        Catch
        End Try

        Return Nothing
    End Function
    '==========================================================================
    ' COPY LEGACY DATA TO SVN
    '==========================================================================

    Public Sub showLegacyImportWizardPublic()
        If iSwApp Is Nothing OrElse myUserControl Is Nothing Then Exit Sub

        If asyncCommitInProgress Then
            iSwApp.SendMsgToUser2(
                "A Commit operation is already running." & vbCrLf & vbCrLf &
                "Wait for it to finish before starting a legacy import.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If legacyImportInProgress Then
            iSwApp.SendMsgToUser2(
                "A legacy import is already in progress.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Dim activeDoc As ModelDoc2 = Nothing

        Try
            activeDoc = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch
            activeDoc = Nothing
        End Try

        If activeDoc Is Nothing Then
            iSwApp.SendMsgToUser2(
                "Open the top-level legacy assembly before using Copy Legacy Data to SVN.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Try
            If CInt(activeDoc.GetType()) <> CInt(swDocumentTypes_e.swDocASSEMBLY) Then
                iSwApp.SendMsgToUser2(
                    "Copy Legacy Data to SVN must be started from an open top-level assembly.",
                    swMessageBoxIcon_e.swMbInformation,
                    swMessageBoxBtn_e.swMbOk)
                Exit Sub
            End If
        Catch
            iSwApp.SendMsgToUser2(
                "The active SOLIDWORKS document could not be verified as an assembly.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End Try

        Dim topAssemblyPath As String = ""

        Try
            topAssemblyPath = activeDoc.GetPathName()
        Catch
            topAssemblyPath = ""
        End Try

        If String.IsNullOrWhiteSpace(topAssemblyPath) Then
            iSwApp.SendMsgToUser2(
                "Save the legacy top-level assembly outside SVN before starting the import.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Dim repoRoot As String = getLocalRepoRootPathForLegacyImport()

        If String.IsNullOrWhiteSpace(repoRoot) OrElse Not Directory.Exists(repoRoot) Then
            iSwApp.SendMsgToUser2(
                "The local SVN working-copy folder is not valid." & vbCrLf & vbCrLf &
                "Pick any folder in your SVN working copy in PlumVault, then try again.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If isLegacySameOrChildPath(topAssemblyPath, repoRoot) Then
            iSwApp.SendMsgToUser2(
                "This command is for copying a legacy assembly from outside the SVN working copy." & vbCrLf & vbCrLf &
                "The active assembly is already inside SVN:" & vbCrLf &
                topAssemblyPath,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        'The destination prompt intentionally comes before the Pack and Go scan/table.
        Dim selectedGrcDestination As String = pickLegacyGrcDestinationFolderPublic("")
        If String.IsNullOrWhiteSpace(selectedGrcDestination) Then Exit Sub

        'Vendor parts always default to the canonical Vendor Parts folder at the
        'actual SVN working-copy root. The general vendor-path rule still accepts
        'any deeper location containing a folder segment named Vendor Parts.
        Dim automaticVendorDestination As String = Path.Combine(repoRoot, "Vendor Parts")

        Dim errorMessage As String = ""
        Dim plan As LegacyImportPlan = buildLegacyImportPlan(
            activeDoc,
            selectedGrcDestination,
            automaticVendorDestination,
            errorMessage)

        If plan Is Nothing Then
            If String.IsNullOrWhiteSpace(errorMessage) Then errorMessage = "The legacy assembly could not be scanned."

            iSwApp.SendMsgToUser2(
                errorMessage,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        'Prepare both destinations before the table opens. This supports folders
        'created in Windows Explorer and ensures empty folders are actually added
        'and committed to SVN before Pack and Go writes CAD files into them.
        If Not prepareSvnDestinationFolderAndCommitIfNeeded(
            selectedGrcDestination,
            "Create legacy CAD import destination",
            errorMessage) Then

            iSwApp.SendMsgToUser2(
                errorMessage,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If Not prepareSvnDestinationFolderAndCommitIfNeeded(
            automaticVendorDestination,
            "Create Vendor Parts folder",
            errorMessage) Then

            iSwApp.SendMsgToUser2(
                errorMessage,
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Using wizard As New LegacyImportForm(plan)
            wizard.ShowDialog(myUserControl)
        End Using
    End Sub

    Private Function buildLegacyImportPlan(ByVal topAssembly As ModelDoc2,
                                           ByVal selectedGrcDestination As String,
                                           ByVal automaticVendorDestination As String,
                                           ByRef errorMessage As String) As LegacyImportPlan
        errorMessage = ""
        If topAssembly Is Nothing Then
            errorMessage = "The top-level assembly is not available."
            Return Nothing
        End If

        Dim topPath As String = ""

        Try
            topPath = Path.GetFullPath(topAssembly.GetPathName())
        Catch
            topPath = ""
        End Try

        If String.IsNullOrWhiteSpace(topPath) Then
            errorMessage = "The top-level assembly must be saved before it can be imported."
            Return Nothing
        End If

        Dim sourceNames() As String = Nothing

        If Not tryGetLegacyPackAndGoDocumentNames(topAssembly, sourceNames, errorMessage) Then
            Return Nothing
        End If

        If sourceNames Is Nothing OrElse sourceNames.Length = 0 Then
            errorMessage = "Pack and Go did not return any SOLIDWORKS files."
            Return Nothing
        End If

        Dim plan As New LegacyImportPlan()
        plan.SourceTopAssemblyPath = topPath
        plan.PackAndGoSourceNames = CType(sourceNames.Clone(), String())
        plan.LocalRepoRootFolder = getLocalRepoRootPathForLegacyImport()
        plan.GrcRootFolder = plan.LocalRepoRootFolder
        plan.VendorRootFolder = automaticVendorDestination
        plan.ExistingRepoFileNames = getExistingRepoCadFileNamesForLegacyImport()
        plan.ExistingRepoModelIds = getExistingRepoModelIdsForLegacyImport()

        plan.GrcDestinationFolder = normalizeLegacyFolderPath(selectedGrcDestination)
        plan.VendorDestinationFolder = normalizeLegacyFolderPath(automaticVendorDestination)

        Dim virtualComponentKeys As HashSet(Of String) = getLegacyVirtualComponentKeys(topAssembly)
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        Dim items As New List(Of LegacyImportItem)()

        For Each rawSourcePath As String In sourceNames
            If String.IsNullOrWhiteSpace(rawSourcePath) Then Continue For

            Dim sourcePath As String = rawSourcePath

            Try
                If Not sourcePath.Contains("^") Then sourcePath = Path.GetFullPath(sourcePath)
            Catch
            End Try

            If seen.Contains(sourcePath) Then Continue For
            seen.Add(sourcePath)

            Dim extension As String = ""

            Try
                extension = Path.GetExtension(sourcePath).ToUpperInvariant()
            Catch
                extension = ""
            End Try

            Dim sourceType As LegacyImportSourceType
            Dim targetType As LegacyImportTargetType

            Select Case extension
                Case ".SLDASM"
                    sourceType = LegacyImportSourceType.Assembly
                    targetType = LegacyImportTargetType.Assembly
                Case ".SLDPRT"
                    sourceType = LegacyImportSourceType.Part
                    targetType = LegacyImportTargetType.Part
                Case ".SLDDRW"
                    sourceType = LegacyImportSourceType.Drawing
                    targetType = LegacyImportTargetType.Drawing
                Case Else
                    errorMessage =
                        "Pack and Go found an unsupported referenced file:" & vbCrLf & vbCrLf &
                        sourcePath & vbCrLf & vbCrLf &
                        "Only SLDASM, SLDPRT, and SLDDRW files are supported by the legacy import table."
                    Return Nothing
            End Select

            If isLegacySameOrChildPath(sourcePath, plan.LocalRepoRootFolder) Then
                errorMessage =
                    "The legacy assembly already references a file inside the SVN working copy:" & vbCrLf & vbCrLf &
                    sourcePath & vbCrLf & vbCrLf &
                    "For this first version of Legacy Import, remove or replace mixed SVN references before importing. " &
                    "This prevents Pack and Go from overwriting or duplicating an existing managed file."
                Return Nothing
            End If

            Dim proposedId As String = ""
            Dim originalName As String = ""

            Try
                originalName = Path.GetFileName(sourcePath)
            Catch
                originalName = sourcePath
            End Try

            If isValidGrc27FileName(originalName) Then
                proposedId = Path.GetFileNameWithoutExtension(originalName)
            End If

            items.Add(New LegacyImportItem With {
                .SourcePath = sourcePath,
                .SourceType = sourceType,
                .TargetType = targetType,
                .ProposedId = proposedId,
                .FinalFileName = "",
                .DestinationPath = "",
                .IsChecked = False,
                .IsValid = False,
                .ValidationMessage = "Not checked",
                .IsVirtualComponent = isLegacyVirtualSourcePath(sourcePath, virtualComponentKeys)
            })
        Next

        If items.Count = 0 Then
            errorMessage = "No supported SOLIDWORKS files were found."
            Return Nothing
        End If

        plan.Items = items.
            OrderByDescending(Function(item) pathsAreSame(item.SourcePath, topPath)).
            ThenBy(Function(item) CInt(item.SourceType)).
            ThenBy(Function(item) item.OriginalFileName, StringComparer.OrdinalIgnoreCase).
            ToList()

        Return plan
    End Function

    Private Function tryGetLegacyPackAndGoDocumentNames(ByVal topAssembly As ModelDoc2,
                                                         ByRef sourceNames() As String,
                                                         ByRef errorMessage As String) As Boolean
        sourceNames = Nothing
        errorMessage = ""

        Try
            Dim packAndGo As PackAndGo = topAssembly.Extension.GetPackAndGo()

            If packAndGo Is Nothing Then
                errorMessage = "SOLIDWORKS did not return a Pack and Go object."
                Return False
            End If

            'Include the full legacy assembly definition in the review table.
            packAndGo.IncludeDrawings = True
            packAndGo.IncludeSuppressed = True
            packAndGo.IncludeToolboxComponents = True

            Try
                packAndGo.IncludeSimulationResults = False
            Catch
            End Try

            Dim namesObject As Object = Nothing

            If Not packAndGo.GetDocumentNames(namesObject) Then
                errorMessage = "SOLIDWORKS Pack and Go could not return the assembly document list."
                Return False
            End If

            Dim namesArray As Array = TryCast(namesObject, Array)

            If namesArray Is Nothing OrElse namesArray.Length = 0 Then
                errorMessage = "SOLIDWORKS Pack and Go returned an empty assembly document list."
                Return False
            End If

            Dim output As New List(Of String)()

            For Each value As Object In namesArray
                Dim valueText As String = Convert.ToString(value)
                If Not String.IsNullOrWhiteSpace(valueText) Then output.Add(valueText)
            Next

            sourceNames = output.ToArray()
            Return sourceNames.Length > 0
        Catch ex As Exception
            errorMessage = "Could not scan the legacy assembly with SOLIDWORKS Pack and Go." & vbCrLf & vbCrLf & ex.Message
            Return False
        End Try
    End Function

    Private Function getLegacyVirtualComponentKeys(ByVal topAssembly As ModelDoc2) As HashSet(Of String)
        Dim output As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        If topAssembly Is Nothing Then Return output

        Try
            Dim assemblyDoc As AssemblyDoc = TryCast(topAssembly, AssemblyDoc)
            If assemblyDoc Is Nothing Then Return output

            Dim componentsObject As Object = assemblyDoc.GetComponents(False)
            Dim componentsArray As Array = TryCast(componentsObject, Array)
            If componentsArray Is Nothing Then Return output

            For Each componentObject As Object In componentsArray
                Dim component As Component2 = TryCast(componentObject, Component2)
                If component Is Nothing Then Continue For

                Dim isVirtual As Boolean = False

                Try
                    isVirtual = component.IsVirtual
                Catch
                    isVirtual = False
                End Try

                If Not isVirtual Then Continue For

                Try
                    addLegacyVirtualComponentKey(output, component.GetPathName())
                Catch
                End Try

                Try
                    addLegacyVirtualComponentKey(output, component.Name2)
                Catch
                End Try

                Try
                    Dim model As ModelDoc2 = TryCast(component.GetModelDoc2(), ModelDoc2)
                    If model IsNot Nothing Then
                        Try
                            addLegacyVirtualComponentKey(output, model.GetPathName())
                        Catch
                        End Try

                        Try
                            addLegacyVirtualComponentKey(output, model.GetTitle())
                        Catch
                        End Try
                    End If
                Catch
                End Try
            Next
        Catch
        End Try

        Return output
    End Function

    Private Sub addLegacyVirtualComponentKey(ByVal keys As HashSet(Of String),
                                             ByVal value As String)
        If keys Is Nothing OrElse String.IsNullOrWhiteSpace(value) Then Exit Sub

        Dim cleanValue As String = value.Trim().Trim(""""c)
        If String.IsNullOrWhiteSpace(cleanValue) Then Exit Sub

        keys.Add(cleanValue)

        Try
            keys.Add(normalizeLegacySourcePath(cleanValue))
        Catch
        End Try

        Try
            Dim fileName As String = Path.GetFileName(cleanValue)
            If Not String.IsNullOrWhiteSpace(fileName) Then keys.Add(fileName)
        Catch
        End Try
    End Sub

    Private Function isLegacyVirtualSourcePath(ByVal sourcePath As String,
                                               ByVal virtualComponentKeys As HashSet(Of String)) As Boolean
        If String.IsNullOrWhiteSpace(sourcePath) Then Return False

        If sourcePath.Contains("^") Then Return True

        Dim normalizedSource As String = normalizeLegacySourcePath(sourcePath)
        Dim sourceFileName As String = ""

        Try
            sourceFileName = Path.GetFileName(sourcePath)
        Catch
            sourceFileName = ""
        End Try

        If virtualComponentKeys IsNot Nothing Then
            If virtualComponentKeys.Contains(sourcePath) OrElse
               virtualComponentKeys.Contains(normalizedSource) OrElse
               (Not String.IsNullOrWhiteSpace(sourceFileName) AndAlso virtualComponentKeys.Contains(sourceFileName)) Then
                Return True
            End If
        End If

        'SOLIDWORKS commonly exposes embedded virtual components through a temporary
        'AppData path in the Pack and Go document list. Treat those as virtual so the
        'table explains the problem before SetDocumentSaveToNames is called.
        If normalizedSource.IndexOf("\AppData\Local\Temp\", StringComparison.OrdinalIgnoreCase) >= 0 Then
            Return True
        End If

        Return False
    End Function

    Public Function pickLegacyGrcDestinationFolderPublic(ByVal currentFolder As String) As String
        Dim repoRoot As String = getLocalRepoRootPathForLegacyImport()

        Return pickLegacyDestinationFolderInternal(
            currentFolder,
            repoRoot,
            "Choose where the imported assembly, parts, and drawings should be copied.",
            rejectVendorPartsFolder:=True)
    End Function

    Public Function pickLegacyVendorDestinationFolderPublic(ByVal currentFolder As String) As String
        'Retained for compatibility with older UI code. New imports automatically
        'use the canonical Vendor Parts folder at the working-copy root.
        Return Path.Combine(getLocalRepoRootPathForLegacyImport(), "Vendor Parts")
    End Function

    Private Function pickLegacyDestinationFolderInternal(ByVal currentFolder As String,
                                                          ByVal allowedRoot As String,
                                                          ByVal description As String,
                                                          Optional ByVal rejectVendorPartsFolder As Boolean = False) As String
        Dim repoRoot As String = getLocalRepoRootPathForLegacyImport()
        Dim normalizedAllowedRoot As String = normalizeLegacyFolderPath(allowedRoot)
        Dim normalizedRepoRoot As String = normalizeLegacyFolderPath(repoRoot)

        Using dialog As New FolderBrowserDialog()
            dialog.Description = description & vbCrLf & vbCrLf &
                                 "You may select an existing folder or click New Folder. " &
                                 "If the folder is new or unversioned, PlumVault will add and commit the empty folder before the import table opens." & vbCrLf & vbCrLf &
                                 "SVN working copy:" & vbCrLf & repoRoot
            dialog.ShowNewFolderButton = True

            Dim initialPath As String = getNearestExistingLegacyFolder(currentFolder)

            If String.IsNullOrWhiteSpace(initialPath) Then
                initialPath = getNearestExistingLegacyFolder(normalizedAllowedRoot)
            End If

            If String.IsNullOrWhiteSpace(initialPath) Then initialPath = normalizedRepoRoot
            If Directory.Exists(initialPath) Then dialog.SelectedPath = initialPath

            If dialog.ShowDialog(myUserControl) <> DialogResult.OK Then Return ""

            Dim selectedPath As String = normalizeLegacyFolderPath(dialog.SelectedPath)

            If Not isLegacySameOrChildPath(selectedPath, normalizedAllowedRoot) Then
                iSwApp.SendMsgToUser2(
                    "The selected folder is outside the SVN working copy." & vbCrLf & vbCrLf &
                    "Selected:" & vbCrLf & selectedPath & vbCrLf & vbCrLf &
                    "SVN working-copy root:" & vbCrLf & normalizedRepoRoot,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk)
                Return ""
            End If

            If rejectVendorPartsFolder AndAlso
               pathContainsNamedFolderSegment(selectedPath, normalizedRepoRoot, "Vendor Parts") Then

                iSwApp.SendMsgToUser2(
                    "The normal legacy-import destination cannot be inside a Vendor Parts folder." & vbCrLf & vbCrLf &
                    "Choose a normal design folder anywhere else inside:" & vbCrLf &
                    normalizedRepoRoot,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk)
                Return ""
            End If

            Return selectedPath
        End Using
    End Function

    Private Function normalizeLegacyFolderPath(ByVal folderPath As String) As String
        If String.IsNullOrWhiteSpace(folderPath) Then Return ""

        Try
            Return Path.GetFullPath(folderPath.Trim().Trim(""""c)).TrimEnd("\"c, "/"c)
        Catch
            Return folderPath.Trim().Trim(""""c).TrimEnd("\"c, "/"c)
        End Try
    End Function

    Private Function isLegacySameOrChildPath(ByVal candidatePath As String,
                                             ByVal requiredRoot As String) As Boolean
        Dim candidate As String = normalizeLegacyFolderPath(candidatePath)
        Dim root As String = normalizeLegacyFolderPath(requiredRoot)

        If String.IsNullOrWhiteSpace(candidate) OrElse String.IsNullOrWhiteSpace(root) Then Return False
        If String.Equals(candidate, root, StringComparison.OrdinalIgnoreCase) Then Return True

        Return candidate.StartsWith(root & Path.DirectorySeparatorChar,
                                    StringComparison.OrdinalIgnoreCase)
    End Function

    Private Function getNearestExistingLegacyFolder(ByVal requestedPath As String) As String
        Dim current As String = normalizeLegacyFolderPath(requestedPath)

        While Not String.IsNullOrWhiteSpace(current)
            If Directory.Exists(current) Then Return current

            Try
                Dim parent As DirectoryInfo = Directory.GetParent(current)
                If parent Is Nothing Then Exit While
                current = parent.FullName
            Catch
                Exit While
            End Try
        End While

        Return ""
    End Function

    Public Function validateLegacyImportItemPublic(ByVal item As LegacyImportItem,
                                                   ByVal plan As LegacyImportPlan) As LegacyImportValidationResult
        Dim result As New LegacyImportValidationResult With {
            .IsValid = False,
            .FinalFileName = "",
            .DestinationPath = "",
            .Message = "Validation failed."
        }

        If item Is Nothing OrElse plan Is Nothing Then
            result.Message = "The row or import plan is missing."
            Return result
        End If

        Dim sourceExtension As String = item.Extension

        If sourceExtension <> ".SLDASM" AndAlso sourceExtension <> ".SLDPRT" AndAlso sourceExtension <> ".SLDDRW" Then
            result.Message = "Unsupported SOLIDWORKS file extension."
            Return result
        End If

        If String.IsNullOrWhiteSpace(item.SourcePath) Then
            result.Message = "The original file path is blank."
            Return result
        End If

        Dim isVirtualComponent As Boolean = item.SourcePath.Contains("^")

        If Not isVirtualComponent AndAlso Not File.Exists(item.SourcePath) Then
            result.Message = "The original file is missing or cannot be accessed."
            Return result
        End If

        If item.IsVirtualComponent OrElse isVirtualComponent Then
            result.IsValid = True
            result.FinalFileName = item.OriginalFileName
            result.DestinationPath = ""
            result.Message =
                "Virtual component retained inside its owning assembly. It does not receive a separate SVN filename, lock, or commit."
            Return result
        End If

        'Hard type rule requested by the team:
        ' - Assembly remains Assembly
        ' - Drawing remains Drawing
        ' - Part can only be Part or Vendor Part
        Select Case item.SourceType
            Case LegacyImportSourceType.Assembly
                If item.TargetType <> LegacyImportTargetType.Assembly Then
                    result.Message = "An assembly cannot be changed to another type."
                    Return result
                End If
            Case LegacyImportSourceType.Drawing
                If item.TargetType <> LegacyImportTargetType.Drawing Then
                    result.Message = "A drawing cannot be changed to another type."
                    Return result
                End If
            Case LegacyImportSourceType.Part
                If item.TargetType <> LegacyImportTargetType.Part AndAlso item.TargetType <> LegacyImportTargetType.VendorPart Then
                    result.Message = "A part can only remain Part or be changed to Vendor Part."
                    Return result
                End If
        End Select

        Dim normalizedId As String = normalizeLegacyProposedId(item.ProposedId, sourceExtension)

        If String.IsNullOrWhiteSpace(normalizedId) Then
            If item.TargetType = LegacyImportTargetType.VendorPart Then
                result.Message = "Enter a vendor filename. The original descriptive filename may be used."
            Else
                result.Message = "Enter a GRC27 or CFD27 ID."
            End If
            Return result
        End If

        Dim finalFileName As String = normalizedId & sourceExtension

        If containsInvalidLegacyFileNameCharacters(finalFileName) Then
            result.Message = "The proposed filename contains a Windows-invalid character."
            Return result
        End If

        If isWindowsReservedLegacyFileName(normalizedId) Then
            result.Message = "The proposed filename is reserved by Windows."
            Return result
        End If

        If item.TargetType = LegacyImportTargetType.VendorPart Then
            If sourceExtension <> ".SLDPRT" Then
                result.Message = "Only source part files can be imported as Vendor Part."
                Return result
            End If

            If String.IsNullOrWhiteSpace(plan.VendorDestinationFolder) Then
                result.Message = "Choose a Vendor Parts destination folder."
                Return result
            End If

            Dim repoRoot As String = If(String.IsNullOrWhiteSpace(plan.LocalRepoRootFolder),
                                        getLocalRepoRootPathForLegacyImport(),
                                        plan.LocalRepoRootFolder)

            If Not isLegacySameOrChildPath(plan.VendorDestinationFolder, repoRoot) OrElse
               Not pathContainsNamedFolderSegment(plan.VendorDestinationFolder, repoRoot, "Vendor Parts") Then
                result.Message =
                    "Vendor parts may be saved anywhere inside the SVN working copy, but the path must contain a folder named Vendor Parts."
                Return result
            End If

            result.DestinationPath = Path.Combine(plan.VendorDestinationFolder, finalFileName)
        Else
            If Not isValidGrc27FileName(finalFileName) Then
                result.Message =
                    "Invalid GRC27/CFD27 ID. Required format: " &
                    "GRC27_CODE_00000_R# or CFD27_CODE_ABC0000_R#. " &
                    "Allowed codes: BR, DT, AE, FR, EL, ST, SU, WT, MI."
                Return result
            End If

            If String.IsNullOrWhiteSpace(plan.GrcDestinationFolder) Then
                result.Message = "Choose a GRC27 destination folder."
                Return result
            End If

            Dim repoRoot As String = If(String.IsNullOrWhiteSpace(plan.LocalRepoRootFolder),
                                        getLocalRepoRootPathForLegacyImport(),
                                        plan.LocalRepoRootFolder)

            If Not isLegacySameOrChildPath(plan.GrcDestinationFolder, repoRoot) Then
                result.Message = "The selected destination must be inside the SVN working copy: " & repoRoot
                Return result
            End If

            If pathContainsNamedFolderSegment(plan.GrcDestinationFolder, repoRoot, "Vendor Parts") Then
                result.Message = "Normal GRC27/CFD27 files cannot be saved inside a Vendor Parts folder."
                Return result
            End If

            result.DestinationPath = Path.Combine(plan.GrcDestinationFolder, finalFileName)
        End If

        result.FinalFileName = finalFileName

        If result.DestinationPath.Length >= 245 Then
            result.Message = "The final path is too long. Choose a shorter destination folder or ID."
            Return result
        End If

        If File.Exists(result.DestinationPath) OrElse Directory.Exists(result.DestinationPath) Then
            result.Message = "A file or folder already exists at the proposed destination."
            Return result
        End If

        If plan.ExistingRepoFileNames IsNot Nothing AndAlso plan.ExistingRepoFileNames.Contains(finalFileName) Then
            result.Message = "This filename already exists somewhere in the SVN working copy. Legacy Import will not overwrite or silently reuse it."
            Return result
        End If

        If item.TargetType = LegacyImportTargetType.Part OrElse item.TargetType = LegacyImportTargetType.Assembly Then
            If plan.ExistingRepoModelIds IsNot Nothing AndAlso plan.ExistingRepoModelIds.Contains(normalizedId) Then
                result.Message = "This GRC/CFD model ID is already used by a part or assembly in the SVN working copy."
                Return result
            End If
        End If

        Dim duplicate As LegacyImportItem = Nothing

        If plan.Items IsNot Nothing Then
            For Each otherItem As LegacyImportItem In plan.Items
                If otherItem Is Nothing OrElse Object.ReferenceEquals(otherItem, item) Then Continue For

                Dim otherId As String = normalizeLegacyProposedId(otherItem.ProposedId, otherItem.Extension)
                If String.IsNullOrWhiteSpace(otherId) Then Continue For

                Dim otherFinalName As String = otherId & otherItem.Extension

                If String.Equals(otherFinalName, finalFileName, StringComparison.OrdinalIgnoreCase) Then
                    duplicate = otherItem
                    Exit For
                End If

                Dim thisIsModel As Boolean = item.TargetType = LegacyImportTargetType.Part OrElse item.TargetType = LegacyImportTargetType.Assembly
                Dim otherIsModel As Boolean = otherItem.TargetType = LegacyImportTargetType.Part OrElse otherItem.TargetType = LegacyImportTargetType.Assembly

                If thisIsModel AndAlso otherIsModel AndAlso String.Equals(otherId, normalizedId, StringComparison.OrdinalIgnoreCase) Then
                    duplicate = otherItem
                    Exit For
                End If
            Next
        End If

        If duplicate IsNot Nothing Then
            result.Message = "Duplicate proposed filename. It is also assigned to: " & duplicate.OriginalFileName
            Return result
        End If

        result.IsValid = True

        Dim folderWillBeCreated As Boolean = False
        Try
            folderWillBeCreated = Not Directory.Exists(Path.GetDirectoryName(result.DestinationPath))
        Catch
            folderWillBeCreated = False
        End Try

        result.Message = If(item.TargetType = LegacyImportTargetType.VendorPart,
                            "Valid vendor part filename and destination.",
                            "Valid GRC27/CFD27 ID and destination.")

        If folderWillBeCreated Then
            result.Message &= " The destination folder will be created and committed automatically."
        End If

        Return result
    End Function

    Private Function expandLegacyCommitPathsWithAddedParentDirectories(ByVal commitPaths() As String,
                                                                         ByVal repoRoot As String) As String()
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return Nothing

        Dim normalizedRoot As String = normalizeLegacyFolderPath(repoRoot)
        Dim output As New List(Of String)()

        For Each pathValue As String In commitPaths
            If String.IsNullOrWhiteSpace(pathValue) Then Continue For
            If Not isLegacySameOrChildPath(pathValue, normalizedRoot) Then Continue For
            addCommitPathIfMissing(pathValue, output)

            Dim currentDirectory As String = ""

            Try
                If Directory.Exists(pathValue) Then
                    currentDirectory = Path.GetFullPath(pathValue)
                Else
                    currentDirectory = Path.GetDirectoryName(Path.GetFullPath(pathValue))
                End If
            Catch
                currentDirectory = ""
            End Try

            While Not String.IsNullOrWhiteSpace(currentDirectory) AndAlso
                  isLegacySameOrChildPath(currentDirectory, normalizedRoot) AndAlso
                  Not String.Equals(currentDirectory, normalizedRoot, StringComparison.OrdinalIgnoreCase)

                Dim statusChar As Char = getFirstLegacySvnStatusCharDepthEmpty(currentDirectory, normalizedRoot)

                If statusChar = "?"c Then
                    runSvnProcess(
                        sSVNPath,
                        "add --parents --depth empty --force --non-interactive """ & currentDirectory & """")
                    statusChar = getFirstLegacySvnStatusCharDepthEmpty(currentDirectory, normalizedRoot)
                End If

                If statusChar = "A"c Then addCommitPathIfMissing(currentDirectory, output)

                Try
                    Dim parent As DirectoryInfo = Directory.GetParent(currentDirectory)
                    If parent Is Nothing Then Exit While
                    currentDirectory = parent.FullName.TrimEnd("\"c)
                Catch
                    Exit While
                End Try
            End While
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Public Function executeLegacyImportPublic(ByVal plan As LegacyImportPlan,
                                              ByRef errorMessage As String) As Boolean
        errorMessage = ""

        If plan Is Nothing OrElse plan.Items Is Nothing OrElse plan.Items.Count = 0 Then
            errorMessage = "The legacy import plan is empty."
            Return False
        End If

        If asyncCommitInProgress Then
            errorMessage = "A Commit operation is already running. Wait for it to finish, then try again."
            Return False
        End If

        If legacyImportInProgress Then
            errorMessage = "A legacy import is already in progress."
            Return False
        End If

        'Refresh the repo filename/ID cache once immediately before the authoritative check.
        plan.ExistingRepoFileNames = getExistingRepoCadFileNamesForLegacyImport()
        plan.ExistingRepoModelIds = getExistingRepoModelIdsForLegacyImport()

        For Each item As LegacyImportItem In plan.Items
            Dim validation As LegacyImportValidationResult = validateLegacyImportItemPublic(item, plan)

            If validation Is Nothing OrElse Not validation.IsValid Then
                errorMessage = item.OriginalFileName & vbCrLf & vbCrLf & If(validation Is Nothing, "Validation failed.", validation.Message)
                Return False
            End If

            item.FinalFileName = validation.FinalFileName
            item.DestinationPath = validation.DestinationPath
            item.IsChecked = True
            item.IsValid = True
            item.ValidationMessage = validation.Message
        Next

        Dim topAssembly As ModelDoc2 = getOpenModelByPathSafe(plan.SourceTopAssemblyPath)

        If topAssembly Is Nothing Then
            errorMessage = "The source top-level assembly is no longer open. Reopen it and restart Legacy Import."
            Return False
        End If

        Dim currentSourceNames() As String = Nothing

        If Not tryGetLegacyPackAndGoDocumentNames(topAssembly, currentSourceNames, errorMessage) Then
            Return False
        End If

        If Not legacyPackAndGoListsMatch(plan.PackAndGoSourceNames, currentSourceNames) Then
            errorMessage =
                "The assembly file list changed after the import table was opened." & vbCrLf & vbCrLf &
                "Cancel and reopen Copy Legacy Data to SVN so the table includes the current assembly structure."
            Return False
        End If

        Dim itemBySource As New Dictionary(Of String, LegacyImportItem)(StringComparer.OrdinalIgnoreCase)

        For Each item As LegacyImportItem In plan.Items
            itemBySource(normalizeLegacySourcePath(item.SourcePath)) = item
        Next

        Dim saveToNames(currentSourceNames.Length - 1) As String

        For i As Integer = 0 To currentSourceNames.Length - 1
            Dim sourceKey As String = normalizeLegacySourcePath(currentSourceNames(i))

            If Not itemBySource.ContainsKey(sourceKey) Then
                errorMessage = "The Pack and Go list contains a file that is not represented in the import table:" & vbCrLf & currentSourceNames(i)
                Return False
            End If

            Dim mappedItem As LegacyImportItem = itemBySource(sourceKey)

            If mappedItem.IsVirtualComponent Then
                'A virtual component must remain represented in the Pack and Go array,
                'and SOLIDWORKS does not allow its filename to be changed. Preserve the
                'original Pack and Go entry so it remains embedded in its owning assembly.
                saveToNames(i) = currentSourceNames(i)
            Else
                saveToNames(i) = mappedItem.DestinationPath
            End If
        Next

        Dim outputFiles() As String = plan.Items.
            Select(Function(item) item.DestinationPath).
            Where(Function(pathValue) Not String.IsNullOrWhiteSpace(pathValue)).
            Distinct(StringComparer.OrdinalIgnoreCase).
            ToArray()

        Dim physicalOutputItemCount As Integer = plan.Items.Where(Function(item) item IsNot Nothing AndAlso Not item.IsVirtualComponent).Count()

        If outputFiles.Length <> physicalOutputItemCount Then
            errorMessage = "Two or more physical table rows resolve to the same final path."
            Return False
        End If

        For Each outputPath As String In outputFiles
            If File.Exists(outputPath) OrElse Directory.Exists(outputPath) Then
                errorMessage = "The destination became occupied after validation:" & vbCrLf & outputPath
                Return False
            End If
        Next

        Dim createdDirectories As New List(Of String)()

        Try
            ensureLegacyDirectoryExists(plan.GrcDestinationFolder, createdDirectories)

            If plan.Items.Any(Function(item) item.TargetType = LegacyImportTargetType.VendorPart) Then
                ensureLegacyDirectoryExists(plan.VendorDestinationFolder, createdDirectories)
            End If
        Catch ex As Exception
            errorMessage = "Could not create the selected destination folder." & vbCrLf & vbCrLf & ex.Message
            rollbackLegacyEmptyDirectories(createdDirectories)
            Return False
        End Try

        legacyImportInProgress = True
        Dim packAndGoCompleted As Boolean = False

        Try
            Dim packAndGo As PackAndGo = topAssembly.Extension.GetPackAndGo()

            If packAndGo Is Nothing Then
                errorMessage = "SOLIDWORKS did not return a Pack and Go object."
                Return False
            End If

            packAndGo.IncludeDrawings = True
            packAndGo.IncludeSuppressed = True
            packAndGo.IncludeToolboxComponents = True

            Try
                packAndGo.IncludeSimulationResults = False
            Catch
            End Try

            Dim duplicateDestination As String = saveToNames.
                Where(Function(value) Not String.IsNullOrWhiteSpace(value)).
                GroupBy(Function(value) normalizeLegacyFolderPath(value), StringComparer.OrdinalIgnoreCase).
                Where(Function(groupValue) groupValue.Count() > 1).
                Select(Function(groupValue) groupValue.Key).
                FirstOrDefault()

            If Not String.IsNullOrWhiteSpace(duplicateDestination) Then
                errorMessage = "Two Pack and Go rows resolve to the same destination:" & vbCrLf & duplicateDestination
                Return False
            End If

            Dim saveNamesObject As Object = saveToNames

            If Not packAndGo.SetDocumentSaveToNames(saveNamesObject) Then
                errorMessage =
                    "SOLIDWORKS rejected one or more proposed Pack and Go destination names." & vbCrLf & vbCrLf &
                    "The destination array must match the Pack and Go list exactly and physical destination filenames must be unique. " &
                    "Virtual-component entries keep their original filenames so they remain embedded in their owning assemblies."
                Return False
            End If

            Dim statusObject As Object = Nothing

            beginInternalSolidWorksSave()
            Try
                statusObject = topAssembly.Extension.SavePackAndGo(packAndGo)
            Finally
                endInternalSolidWorksSave()
            End Try

            Dim missingOutputs As New List(Of String)()

            For Each outputPath As String In outputFiles
                If Not File.Exists(outputPath) Then missingOutputs.Add(outputPath)
            Next

            If missingOutputs.Count > 0 Then
                errorMessage =
                    "SOLIDWORKS Pack and Go did not create every expected file." & vbCrLf & vbCrLf &
                    stringArrToSingleStringWithNewLines(missingOutputs.ToArray(), bTrimFileNames:=False, iLimit:=12)
                Return False
            End If

            packAndGoCompleted = True
        Catch ex As Exception
            errorMessage = "SOLIDWORKS Pack and Go failed." & vbCrLf & vbCrLf & ex.Message
            Return False
        Finally
            legacyImportInProgress = False

            If Not packAndGoCompleted Then
                rollbackLegacyPackAndGoOutputs(outputFiles, createdDirectories)
            End If
        End Try

        Dim addTargetsFile As String = ""
        Dim addResult As rawProcessReturn

        Try
            addTargetsFile = createLegacySvnTargetsFile(outputFiles, "add")
            addResult = runSvnProcess(
                sSVNPath,
                "add --parents --force --non-interactive --targets """ & addTargetsFile & """")
        Catch ex As Exception
            errorMessage = "Could not prepare or run SVN add." & vbCrLf & vbCrLf & ex.Message
            Return False
        Finally
            deleteLegacyTargetsFileQuietly(addTargetsFile)
        End Try

        If addResult.outputError IsNot Nothing AndAlso addResult.outputError.Trim() <> "" Then
            errorMessage =
                "Pack and Go completed, but SVN add failed." & vbCrLf & vbCrLf &
                addResult.outputError.Trim() & vbCrLf & vbCrLf &
                "The copied files were left in the SVN working copy so they can be recovered or cleaned up manually."
            Return False
        End If

        If Not svnPropset(outputFiles, "addin:release_state", "||EDIT||") Then
            errorMessage =
                "Pack and Go completed, but PlumVault could not set the SVN release-state property." & vbCrLf & vbCrLf &
                "The copied files were left added in the working copy and were not committed."
            Return False
        End If

        Dim commitSeedPaths() As String = outputFiles.
            Concat(createdDirectories).
            Where(Function(pathValue) Not String.IsNullOrWhiteSpace(pathValue)).
            Distinct(StringComparer.OrdinalIgnoreCase).
            ToArray()

        Dim commitPaths() As String = expandLegacyCommitPathsWithAddedParentDirectories(
            commitSeedPaths,
            plan.LocalRepoRootFolder)

        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then
            errorMessage = "No valid SVN commit paths remained after Pack and Go."
            Return False
        End If

        'Legacy imports intentionally finish unlocked. Imported CAD remains read-only
        'until a user explicitly chooses Get Locks from PlumVault.
        Dim topImportedPath As String = ""
        Dim topItem As LegacyImportItem = plan.Items.FirstOrDefault(Function(item) pathsAreSame(item.SourcePath, plan.SourceTopAssemblyPath))
        If topItem IsNot Nothing Then topImportedPath = topItem.DestinationPath

        Dim commitTargetsFile As String = ""

        Try
            commitTargetsFile = createLegacySvnTargetsFile(commitPaths, "commit")
        Catch ex As Exception
            errorMessage = "Could not prepare the SVN commit target list." & vbCrLf & vbCrLf & ex.Message
            Return False
        End Try

        startLegacyImportCommitBackground(
            commitTargetsFile,
            outputFiles,
            topImportedPath,
            "Legacy CAD import: " & Path.GetFileNameWithoutExtension(plan.SourceTopAssemblyPath))

        Return True
    End Function

    Private Sub startLegacyImportCommitBackground(ByVal commitTargetsFile As String,
                                                  ByVal importedCadPaths() As String,
                                                  ByVal topImportedAssemblyPath As String,
                                                  ByVal commitMessage As String)
        If String.IsNullOrWhiteSpace(commitTargetsFile) OrElse Not File.Exists(commitTargetsFile) Then Exit Sub

        Dim targetsFileForBackground As String = commitTargetsFile
        Dim cadPathsForCompletion() As String = If(importedCadPaths Is Nothing, Nothing, CType(importedCadPaths.Clone(), String()))
        Dim safeMessage As String = If(commitMessage, "Legacy CAD import").Replace("""", "'")
        Dim savedPathForBackground As String = ""

        Try
            savedPathForBackground = myUserControl.savedPATH
        Catch
            savedPathForBackground = ""
        End Try

        asyncCommitInProgress = True

        Try
            myUserControl.markCommitPendingForFilePathsPublic(cadPathsForCompletion, True, "Committing legacy import...")
        Catch
        End Try

        Task.Run(
            Sub()
                Dim success As Boolean = False
                Dim backgroundError As String = ""

                Try
                    Dim result As rawProcessReturn = runSvnProcessBackgroundNoUi(
                        sSVNPath,
                        "commit --non-interactive -m """ & safeMessage & """ --targets """ & targetsFileForBackground & """",
                        savedPathForBackground)

                    If result.outputError IsNot Nothing AndAlso result.outputError.Trim() <> "" Then
                        backgroundError = result.outputError.Trim()
                    Else
                        success = True
                    End If
                Catch ex As Exception
                    success = False
                    backgroundError = ex.Message
                Finally
                    deleteLegacyTargetsFileQuietly(targetsFileForBackground)
                End Try

                Try
                    If myUserControl IsNot Nothing AndAlso myUserControl.IsHandleCreated Then
                        myUserControl.BeginInvoke(
                            New MethodInvoker(
                                Sub()
                                    finishLegacyImportCommitOnMainThread(
                                        cadPathsForCompletion,
                                        topImportedAssemblyPath,
                                        success,
                                        backgroundError)
                                End Sub))
                    Else
                        asyncCommitInProgress = False
                    End If
                Catch
                    asyncCommitInProgress = False
                End Try
            End Sub)
    End Sub

    Private Sub finishLegacyImportCommitOnMainThread(ByVal importedCadPaths() As String,
                                                     ByVal topImportedAssemblyPath As String,
                                                     ByVal success As Boolean,
                                                     ByVal errorMessage As String)
        asyncCommitInProgress = False

        Try
            myUserControl.markCommitPendingForFilePathsPublic(importedCadPaths, False)
        Catch
        End Try

        If Not success Then
            iSwApp.SendMsgToUser2(
                "The legacy files were copied and added locally, but the SVN commit did not complete." & vbCrLf & vbCrLf &
                errorMessage & vbCrLf & vbCrLf &
                "SVN commits are atomic, so no partial repository commit was created. " &
                "The local added files remain in the working copy for recovery or cleanup.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk)
            processPendingAutomaticSaveCommits()
            Exit Sub
        End If

        Try
            For Each filePath As String In importedCadPaths
                If Not File.Exists(filePath) Then Continue For
                File.SetAttributes(filePath, File.GetAttributes(filePath) Or FileAttributes.ReadOnly)
            Next
        Catch
        End Try

        Try
            myUserControl.markCommitResultForFilePathsPublic(importedCadPaths, True)
        Catch
        End Try

        Try
            updateStatusCacheForKnownPaths(importedCadPaths, forceAddDelChg1:=" ", forceLock6:=" ", forceUpToDate9:=" ")
            refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)
        Catch
        End Try

        Dim message As String =
            "Legacy import committed successfully." & vbCrLf & vbCrLf &
            "Imported files: " & If(importedCadPaths Is Nothing, 0, importedCadPaths.Length).ToString()

        If Not String.IsNullOrWhiteSpace(topImportedAssemblyPath) Then
            message &= vbCrLf & vbCrLf & "Imported top assembly:" & vbCrLf & topImportedAssemblyPath
        End If

        message &= vbCrLf & vbCrLf &
            "Imported files remain unlocked and read-only. Use Get Locks only on the files you intend to edit."

        iSwApp.SendMsgToUser2(
            message,
            swMessageBoxIcon_e.swMbInformation,
            swMessageBoxBtn_e.swMbOk)

        processPendingAutomaticSaveCommits()
    End Sub

    Private Function getFirstLegacySvnStatusCharDepthEmpty(ByVal targetPath As String,
                                                           ByVal repoRoot As String) As Char
        If String.IsNullOrWhiteSpace(targetPath) Then Return ChrW(0)
        If Not Directory.Exists(targetPath) Then Return ChrW(0)
        If Not isLegacySameOrChildPath(targetPath, repoRoot) Then Return ChrW(0)

        Try
            Dim statusResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "status --depth empty --non-interactive """ & targetPath & """")

            If statusResult.outputError IsNot Nothing AndAlso statusResult.outputError.Trim() <> "" Then
                Return ChrW(0)
            End If

            Dim statusText As String = If(statusResult.output, "").Trim()
            If String.IsNullOrWhiteSpace(statusText) Then Return " "c

            Return statusText(0)
        Catch
            Return ChrW(0)
        End Try
    End Function

    Private Function getLegacyAddedDirectoryPaths(ByVal targetFolder As String,
                                                  ByVal repoRoot As String) As String()
        Dim output As New List(Of String)()
        Dim currentFolder As String = normalizeLegacyFolderPath(targetFolder)
        Dim normalizedRoot As String = normalizeLegacyFolderPath(repoRoot)

        While Not String.IsNullOrWhiteSpace(currentFolder) AndAlso
              isLegacySameOrChildPath(currentFolder, normalizedRoot) AndAlso
              Not String.Equals(currentFolder, normalizedRoot, StringComparison.OrdinalIgnoreCase)

            Dim statusChar As Char = getFirstLegacySvnStatusCharDepthEmpty(currentFolder, normalizedRoot)
            If statusChar = "A"c Then output.Add(currentFolder)

            Try
                Dim parent As DirectoryInfo = Directory.GetParent(currentFolder)
                If parent Is Nothing Then Exit While
                currentFolder = parent.FullName.TrimEnd("\"c)
            Catch
                Exit While
            End Try
        End While

        Return output.
            Distinct(StringComparer.OrdinalIgnoreCase).
            OrderBy(Function(pathValue) pathValue.Length).
            ToArray()
    End Function

    Private Function prepareSvnDestinationFolderAndCommitIfNeeded(ByVal folderPath As String,
                                                             ByVal commitMessage As String,
                                                             ByRef errorMessage As String) As Boolean
        errorMessage = ""

        Dim repoRoot As String = getLocalRepoRootPathForLegacyImport()
        Dim fullFolder As String = normalizeLegacyFolderPath(folderPath)

        If String.IsNullOrWhiteSpace(repoRoot) OrElse Not Directory.Exists(repoRoot) Then
            errorMessage = "The SVN working-copy root is not available."
            Return False
        End If

        If String.IsNullOrWhiteSpace(fullFolder) OrElse
           Not isLegacySameOrChildPath(fullFolder, repoRoot) Then
            errorMessage =
                "The selected folder must be inside the SVN working copy." & vbCrLf & vbCrLf &
                "Selected:" & vbCrLf & fullFolder & vbCrLf & vbCrLf &
                "SVN working-copy root:" & vbCrLf & repoRoot
            Return False
        End If

        Try
            If Not Directory.Exists(fullFolder) Then Directory.CreateDirectory(fullFolder)
        Catch ex As Exception
            errorMessage = "Could not create the destination folder." & vbCrLf & vbCrLf & ex.Message
            Return False
        End Try

        Dim initialStatus As Char = getFirstLegacySvnStatusCharDepthEmpty(fullFolder, repoRoot)

        If initialStatus = " "c Then Return True

        If initialStatus <> "?"c AndAlso initialStatus <> "A"c Then
            errorMessage =
                "PlumVault could not confirm the SVN status of the destination folder:" & vbCrLf & vbCrLf &
                fullFolder
            Return False
        End If

        If initialStatus = "?"c Then
            Dim addResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "add --parents --depth empty --force --non-interactive """ & fullFolder & """")

            If addResult.outputError IsNot Nothing AndAlso addResult.outputError.Trim() <> "" Then
                errorMessage =
                    "The folder was created, but SVN could not add it." & vbCrLf & vbCrLf &
                    addResult.outputError.Trim()
                Return False
            End If
        End If

        Dim addedDirectories() As String = getLegacyAddedDirectoryPaths(fullFolder, repoRoot)

        If addedDirectories Is Nothing OrElse addedDirectories.Length = 0 Then
            Dim finalStatus As Char = getFirstLegacySvnStatusCharDepthEmpty(fullFolder, repoRoot)
            If finalStatus = " "c Then Return True

            errorMessage =
                "The folder exists, but PlumVault could not confirm that it is versioned in SVN:" & vbCrLf & vbCrLf &
                fullFolder
            Return False
        End If

        Dim targetsFile As String = ""

        Try
            targetsFile = createLegacySvnTargetsFile(addedDirectories, "folders")

            Dim safeMessage As String = If(String.IsNullOrWhiteSpace(commitMessage),
                                           "Create legacy import folder",
                                           commitMessage.Trim()).Replace("""", "'")

            Dim commitResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "commit --depth empty --non-interactive -m """ & safeMessage & """ --targets """ & targetsFile & """")

            If commitResult.outputError IsNot Nothing AndAlso commitResult.outputError.Trim() <> "" Then
                errorMessage =
                    "The folder was added locally, but SVN could not commit it." & vbCrLf & vbCrLf &
                    commitResult.outputError.Trim() & vbCrLf & vbCrLf &
                    "The folder remains in the working copy and may still be scheduled for addition."
                Return False
            End If
        Catch ex As Exception
            errorMessage = "Could not commit the destination folder." & vbCrLf & vbCrLf & ex.Message
            Return False
        Finally
            deleteLegacyTargetsFileQuietly(targetsFile)
        End Try

        Dim verifiedStatus As Char = getFirstLegacySvnStatusCharDepthEmpty(fullFolder, repoRoot)

        If verifiedStatus = "?"c OrElse verifiedStatus = "A"c OrElse verifiedStatus = ChrW(0) Then
            errorMessage =
                "SVN did not confirm the destination folder as committed:" & vbCrLf & vbCrLf &
                fullFolder
            Return False
        End If

        Return True
    End Function

    Private Function getLocalRepoRootPathForLegacyImport() As String
        Return getResolvedSvnWorkingCopyRootPath()
    End Function

    Private Function getExistingRepoCadFileNamesForLegacyImport() As HashSet(Of String)
        Dim output As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        Dim repoRoot As String = getLocalRepoRootPathForLegacyImport()

        If String.IsNullOrWhiteSpace(repoRoot) OrElse Not Directory.Exists(repoRoot) Then Return output

        Try
            For Each filePath As String In Directory.EnumerateFiles(repoRoot, "*", SearchOption.AllDirectories)
                If Not isCadFilePath(filePath) Then Continue For
                output.Add(Path.GetFileName(filePath))
            Next
        Catch
        End Try

        Return output
    End Function

    Private Function getExistingRepoModelIdsForLegacyImport() As HashSet(Of String)
        Dim output As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        Dim repoRoot As String = getLocalRepoRootPathForLegacyImport()

        If String.IsNullOrWhiteSpace(repoRoot) OrElse Not Directory.Exists(repoRoot) Then Return output

        Try
            For Each filePath As String In Directory.EnumerateFiles(repoRoot, "*", SearchOption.AllDirectories)
                Dim extension As String = Path.GetExtension(filePath).ToUpperInvariant()
                If extension <> ".SLDPRT" AndAlso extension <> ".SLDASM" Then Continue For
                If pathContainsNamedFolderSegment(filePath, repoRoot, "Vendor Parts") Then Continue For
                output.Add(Path.GetFileNameWithoutExtension(filePath))
            Next
        Catch
        End Try

        Return output
    End Function

    Private Function createLegacySvnTargetsFile(ByVal paths() As String,
                                                ByVal purpose As String) As String
        If paths Is Nothing OrElse paths.Length = 0 Then Throw New IOException("No SVN target paths were supplied.")

        Dim cleanPaths As String() = paths.
            Where(Function(pathValue) Not String.IsNullOrWhiteSpace(pathValue)).
            Select(Function(pathValue) Path.GetFullPath(pathValue)).
            Distinct(StringComparer.OrdinalIgnoreCase).
            ToArray()

        If cleanPaths.Length = 0 Then Throw New IOException("No valid SVN target paths were supplied.")

        Dim safePurpose As String = If(String.IsNullOrWhiteSpace(purpose), "targets", purpose.Trim())
        Dim targetFile As String = Path.Combine(
            Path.GetTempPath(),
            "PlumVault_Legacy_" & safePurpose & "_" & Guid.NewGuid().ToString("N") & ".txt")

        File.WriteAllLines(targetFile, cleanPaths, New System.Text.UTF8Encoding(False))
        Return targetFile
    End Function

    Private Sub deleteLegacyTargetsFileQuietly(ByVal targetFile As String)
        If String.IsNullOrWhiteSpace(targetFile) Then Exit Sub

        Try
            If File.Exists(targetFile) Then File.Delete(targetFile)
        Catch
        End Try
    End Sub

    Private Function normalizeLegacyProposedId(ByVal proposedId As String,
                                               ByVal expectedExtension As String) As String
        If String.IsNullOrWhiteSpace(proposedId) Then Return ""

        Dim value As String = proposedId.Trim().Trim(""""c)
        Dim actualExtension As String = ""

        Try
            actualExtension = Path.GetExtension(value)
        Catch
            actualExtension = ""
        End Try

        If Not String.IsNullOrWhiteSpace(actualExtension) Then
            If Not String.Equals(actualExtension, expectedExtension, StringComparison.OrdinalIgnoreCase) Then Return ""
            value = Path.GetFileNameWithoutExtension(value)
        End If

        Return value.Trim()
    End Function

    Private Function containsInvalidLegacyFileNameCharacters(ByVal fileName As String) As Boolean
        If String.IsNullOrWhiteSpace(fileName) Then Return True

        Try
            Return fileName.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 OrElse fileName.Contains("\") OrElse fileName.Contains("/")
        Catch
            Return True
        End Try
    End Function

    Private Function isWindowsReservedLegacyFileName(ByVal baseName As String) As Boolean
        If String.IsNullOrWhiteSpace(baseName) Then Return True

        Dim normalized As String = baseName.Trim().TrimEnd("."c).ToUpperInvariant()
        Dim reserved As String() = {
            "CON", "PRN", "AUX", "NUL",
            "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9",
            "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9"
        }

        Return reserved.Contains(normalized, StringComparer.OrdinalIgnoreCase)
    End Function

    Private Function sanitizeLegacyFolderName(ByVal value As String) As String
        If String.IsNullOrWhiteSpace(value) Then Return "Legacy Assembly"

        Dim invalid As Char() = Path.GetInvalidFileNameChars()
        Dim chars As Char() = value.Trim().Select(Function(ch) If(invalid.Contains(ch), "_"c, ch)).ToArray()
        Dim output As String = New String(chars).Trim().TrimEnd("."c)

        If String.IsNullOrWhiteSpace(output) Then output = "Legacy Assembly"
        Return output
    End Function

    Private Function normalizeLegacySourcePath(ByVal sourcePath As String) As String
        If String.IsNullOrWhiteSpace(sourcePath) Then Return ""

        If sourcePath.Contains("^") Then Return sourcePath.Trim()

        Try
            Return Path.GetFullPath(sourcePath).Trim()
        Catch
            Return sourcePath.Trim()
        End Try
    End Function

    Private Function legacyPackAndGoListsMatch(ByVal originalNames() As String,
                                               ByVal currentNames() As String) As Boolean
        If originalNames Is Nothing OrElse currentNames Is Nothing Then Return False
        If originalNames.Length <> currentNames.Length Then Return False

        Dim originalSet As New HashSet(Of String)(originalNames.Select(Function(value) normalizeLegacySourcePath(value)), StringComparer.OrdinalIgnoreCase)
        Dim currentSet As New HashSet(Of String)(currentNames.Select(Function(value) normalizeLegacySourcePath(value)), StringComparer.OrdinalIgnoreCase)

        Return originalSet.SetEquals(currentSet)
    End Function

    Private Sub ensureLegacyDirectoryExists(ByVal folderPath As String,
                                            ByVal createdDirectories As List(Of String))
        If String.IsNullOrWhiteSpace(folderPath) Then Throw New IOException("Destination folder is blank.")

        Dim fullFolder As String = normalizeLegacyFolderPath(folderPath)
        Dim repoRoot As String = getLocalRepoRootPathForLegacyImport()

        If Not isLegacySameOrChildPath(fullFolder, repoRoot) Then
            Throw New IOException("Destination folder is outside the selected SVN working copy: " & fullFolder)
        End If

        If Directory.Exists(fullFolder) Then Exit Sub

        Dim missingFolders As New List(Of String)()
        Dim currentFolder As String = fullFolder

        While Not String.IsNullOrWhiteSpace(currentFolder) AndAlso
              isLegacySameOrChildPath(currentFolder, repoRoot) AndAlso
              Not Directory.Exists(currentFolder)

            missingFolders.Add(currentFolder)

            Dim parent As DirectoryInfo = Directory.GetParent(currentFolder)
            If parent Is Nothing Then Exit While
            currentFolder = parent.FullName
        End While

        Directory.CreateDirectory(fullFolder)

        If createdDirectories IsNot Nothing Then
            For Each createdFolder As String In missingFolders.OrderBy(Function(value) value.Length)
                If Not createdDirectories.Any(Function(existingFolder) String.Equals(existingFolder, createdFolder, StringComparison.OrdinalIgnoreCase)) Then
                    createdDirectories.Add(createdFolder)
                End If
            Next
        End If
    End Sub

    Private Sub rollbackLegacyPackAndGoOutputs(ByVal outputFiles() As String,
                                               ByVal createdDirectories As List(Of String))
        If outputFiles IsNot Nothing Then
            For Each outputPath As String In outputFiles
                Try
                    If File.Exists(outputPath) Then
                        File.SetAttributes(outputPath, FileAttributes.Normal)
                        File.Delete(outputPath)
                    End If
                Catch
                End Try
            Next
        End If

        rollbackLegacyEmptyDirectories(createdDirectories)
    End Sub

    Private Sub rollbackLegacyEmptyDirectories(ByVal createdDirectories As List(Of String))
        If createdDirectories Is Nothing Then Exit Sub

        For Each folderPath As String In createdDirectories.OrderByDescending(Function(value) value.Length)
            Try
                If Directory.Exists(folderPath) AndAlso Not Directory.EnumerateFileSystemEntries(folderPath).Any() Then
                    Directory.Delete(folderPath, False)
                End If
            Catch
            End Try
        Next
    End Sub


    Public Structure rawProcessReturn
        Public output As String
        Public outputError As String
    End Structure
    Public Structure lockStatus
        Public eDisposition As lockDisposition
        Public sFilePaths() As String
    End Structure
    Public Enum lockDisposition
        noSteal
        stealAndOverwrite
        stealAndDoNotOverwrite
        unknown
    End Enum

End Module
