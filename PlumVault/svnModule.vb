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

    Dim myUserControl As UserControl1
    Dim iSwApp As SldWorks
    Dim statusOfAllOpenModels As SVNStatus
    Private liveAssemblyChangeCheckInProgress As Boolean = False
    Private pendingExternalRefCommitPaths As New List(Of String)
    Private pendingExternalRefSkipNameCheckPaths As New List(Of String)
    Private closeGuardMessageShowing As Boolean = False
    Private lockReviewMessageShowing As Boolean = False
    Private lastCloseGuardPromptTime As DateTime = DateTime.MinValue
    Private unsafeForceCloseApprovedUntil As DateTime = DateTime.MinValue
    Private documentLockReviewApprovedUntil As DateTime = DateTime.MinValue
    Private documentLockReviewApprovedPath As String = ""
    Private applicationLockReviewApprovedUntil As DateTime = DateTime.MinValue
    Private statusCacheByNormalizedPath As New Dictionary(Of String, SVNStatus.filePpty)(StringComparer.OrdinalIgnoreCase)
    Private statusCacheLastWriteUtc As DateTime = DateTime.MinValue
    Private statusCacheLastServerAwareUtc As DateTime = DateTime.MinValue
    Private asyncGetLocksInProgress As Boolean = False
    Private asyncCommitInProgress As Boolean = False
    Private asyncCleanupInProgress As Boolean = False
    Private cachedConfiguredRepoPathForWorkingCopyRoot As String = ""
    Private cachedResolvedWorkingCopyRoot As String = ""

    'Automatic save -> SVN commit state.
    'All of this runs on the SOLIDWORKS UI thread except the actual svn.exe commit process.
    Private internalSolidWorksSaveDepth As Integer = 0
    Private newDocumentTeamSaveWorkflowInProgress As Boolean = False
    Private pendingAutomaticSaveCommitPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    Private automaticSaveCommitPreparing As Boolean = False
    Private legacyImportInProgress As Boolean = False

    'Assembly edit protection state. The guard is event-driven; it does not poll and
    'does not contact the SVN server while the user is modelling.
    Private assemblyGuardUndoInProgress As Boolean = False
    Private ReadOnly assemblyGuardQueuedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
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

    'When PlumVault immediately undoes an unauthorized assembly edit, SOLIDWORKS can leave
    'the assembly SaveFlag set even though the on-disk SVN file is still clean. Track only
    'those known guard-generated cases so the close workflow may proceed to the retained-lock
    'review table without weakening protection for genuine unsaved assembly edits.
    Private ReadOnly assemblyGuardFalseDirtyCandidatePaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

    Private Const SW_COMMAND_SAVE As Integer = 2
    Private Const SW_COMMAND_SAVE_AS As Integer = 620

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

    End Sub


    '==========================================================================
    ' SOLIDWORKS SAVE -> SVN COMMIT
    '==========================================================================

    Private Function automaticSaveEventsSuppressed() As Boolean
        Return internalSolidWorksSaveDepth > 0 OrElse automaticSaveCommitPreparing OrElse legacyImportInProgress
    End Function


    '==========================================================================
    ' ASSEMBLY EDIT PROTECTION
    '==========================================================================

    Private Function assemblyEditGuardSuppressed() As Boolean
        Return automaticSaveEventsSuppressed() OrElse assemblyGuardUndoInProgress
    End Function

    Private Function getAssemblyEditTargetDocumentSafe(ByVal assemblyDocument As ModelDoc2) As ModelDoc2
        If assemblyDocument Is Nothing Then Return Nothing

        Try
            Dim swAssembly As AssemblyDoc = TryCast(assemblyDocument, AssemblyDoc)
            If swAssembly Is Nothing Then Return Nothing

            Return TryCast(swAssembly.GetEditTarget(), ModelDoc2)
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
        If isNewUnversionedOrAddedFile(childPath) Then Return True

        Try
            Dim cached As SVNStatus.filePpty = Nothing

            If tryFindCachedStatusProperty(childPath, cached) Then
                If cached.lock6 = "K" Then Return True
            End If
        Catch
        End Try

        Return userHasLocalSvnLockTokenForPath(childPath)
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
            If document.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return False
        Catch
            Return False
        End Try

        If Not isAssemblyGuardFalseDirtyCandidate(filePath) Then Return False
        Return isSvnPathLocallyClean(filePath)
    End Function

    Private Function assemblyIsEditingExternalPhysicalChild(ByVal assemblyDocument As ModelDoc2,
                                                             Optional ByVal allowLockedChildDimensionFallback As Boolean = False) As Boolean
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

            If Not String.IsNullOrWhiteSpace(targetPath) Then Return True
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
        If isNewUnversionedOrAddedFile(assemblyPath) Then Return True

        'Fast cache path: normal modelling actions do not launch an SVN process.
        Try
            Dim cached As SVNStatus = findStatusForFile(assemblyPath)
            If cached IsNot Nothing AndAlso cached.fp IsNot Nothing AndAlso cached.fp.Length > 0 Then
                If cached.fp(0).lock6 = "K" Then Return True
                If cached.fp(0).lock6 = " " Then Return False
            End If
        Catch
        End Try

        'Fallback is local working-copy status only; no server call is made.
        Return userHasSvnLockOnDoc(assemblyDocument)
    End Function

    Private Function assemblyOwnedEditMustBeBlocked(ByVal assemblyDocument As ModelDoc2,
                                                      Optional ByVal allowLockedChildDimensionFallback As Boolean = False) As Boolean
        If assemblyDocument Is Nothing Then Return False
        If assemblyEditGuardSuppressed() Then Return False

        Try
            If assemblyDocument.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return False
        Catch
            Return False
        End Try

        'Do not let the parent assembly's lack of a lock interfere while a separately
        'file-backed child is being edited in context.
        If assemblyIsEditingExternalPhysicalChild(assemblyDocument, allowLockedChildDimensionFallback) Then Return False

        Return Not assemblyHasRequiredLockFast(assemblyDocument)
    End Function

    Private Sub showAssemblyLockRequiredMessage(ByVal assemblyDocument As ModelDoc2,
                                                ByVal actionDescription As String,
                                                Optional ByVal editWasUndone As Boolean = False)
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

        Dim firstLine As String = If(editWasUndone,
                                     "Assembly edit was undone.",
                                     "Assembly edit blocked.")

        Dim actionText As String = ""
        If Not String.IsNullOrWhiteSpace(actionDescription) Then
            actionText = vbCrLf & vbCrLf & "Attempted action: " & actionDescription
        End If

        Try
            iSwApp.SendMsgToUser2(
                firstLine & vbCrLf & vbCrLf &
                assemblyName & " is not locked by you." & vbCrLf &
                "Get Locks on the assembly before changing mates, component positions, inserted components, assembly configurations, display state, or virtual components." &
                actionText & vbCrLf & vbCrLf &
                "You may still edit a separately file-backed child part in context when that child has its own lock.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
        Catch
        End Try
    End Sub

    Public Function blockAssemblyOwnedEditPrePublic(ByVal assemblyDocument As ModelDoc2,
                                                     ByVal actionDescription As String) As Integer
        Dim childEditContext As Boolean = assemblyIsEditingExternalPhysicalChild(assemblyDocument)

        If Not assemblyOwnedEditMustBeBlocked(assemblyDocument) Then
            'A genuine assembly-owned edit made while the assembly is locked supersedes any
            'earlier guard-generated false-dirty candidate. Child edits do not touch it.
            If Not childEditContext Then clearAssemblyGuardFalseDirtyCandidate(assemblyDocument)
            Return 0
        End If

        showAssemblyLockRequiredMessage(assemblyDocument, actionDescription, editWasUndone:=False)
        Return 1
    End Function

    Public Sub handleAssemblyDimensionChangePostPublic(ByVal assemblyDocument As ModelDoc2,
                                                        ByVal displayDimension As Object)
        'Capture the selected owning component before SOLIDWORKS clears the temporary
        'dimension-selection context. No SVN/server operation occurs here.
        noteAssemblySelectionContextPublic(assemblyDocument)

        Try
            handleAssemblyOwnedEditPostPublic(
                assemblyDocument,
                "changing an assembly, mate, or child-part dimension",
                allowLockedChildDimensionFallback:=True
            )
        Finally
            clearAssemblySelectionContext(assemblyDocument)
        End Try
    End Sub

    Public Sub handleAssemblyOwnedEditPostPublic(ByVal assemblyDocument As ModelDoc2,
                                                  ByVal actionDescription As String,
                                                  Optional ByVal allowLockedChildDimensionFallback As Boolean = False)
        Dim childEditContext As Boolean = assemblyIsEditingExternalPhysicalChild(
            assemblyDocument,
            allowLockedChildDimensionFallback
        )

        If Not assemblyOwnedEditMustBeBlocked(assemblyDocument, allowLockedChildDimensionFallback) Then
            If Not childEditContext Then clearAssemblyGuardFalseDirtyCandidate(assemblyDocument)
            Exit Sub
        End If

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
            If assemblyGuardQueuedPaths.Contains(queueKey) Then Exit Sub
            assemblyGuardQueuedPaths.Add(queueKey)
        End SyncLock

        Dim undoAction As New System.Windows.Forms.MethodInvoker(
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
                    If Not assemblyOwnedEditMustBeBlocked(currentAssembly, allowLockedChildDimensionFallback) Then Exit Sub

                    assemblyGuardUndoInProgress = True
                    Try
                        currentAssembly.EditUndo2(1)
                        Try
                            currentAssembly.ForceRebuild3(False)
                        Catch
                        End Try
                    Finally
                        assemblyGuardUndoInProgress = False
                    End Try

                    'SOLIDWORKS sometimes leaves GetSaveFlag=True after an immediate Undo even
                    'though the assembly file on disk is unchanged. Mark this exact guard-generated
                    'case so close safety can verify local SVN cleanliness before showing the lock table.
                    markAssemblyGuardFalseDirtyCandidate(currentAssembly)

                    showAssemblyLockRequiredMessage(currentAssembly, actionDescription, editWasUndone:=True)

                    Try
                        refreshActiveTreeAfterSvnAction(bUpdateLocalLockStatus:=False)
                    Catch
                    End Try
                Catch
                    assemblyGuardUndoInProgress = False

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
                myUserControl.BeginInvoke(undoAction)
                Exit Sub
            End If
        Catch
        End Try

        'The task pane is normally available. This fallback keeps the edit from being
        'left behind if the pane is temporarily unavailable.
        Try
            undoAction.Invoke()
        Catch
            SyncLock assemblyGuardSync
                assemblyGuardQueuedPaths.Remove(queueKey)
            End SyncLock
        End Try
    End Sub

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

    Private Function getSaveDialogFilterForDocument(ByVal doc As ModelDoc2) As String
        Dim ext As String = getCadExtensionForDocument(doc)

        Select Case ext.ToUpperInvariant()
            Case ".SLDPRT"
                Return "SOLIDWORKS Part (*.SLDPRT)|*.SLDPRT"
            Case ".SLDASM"
                Return "SOLIDWORKS Assembly (*.SLDASM)|*.SLDASM"
            Case ".SLDDRW"
                Return "SOLIDWORKS Drawing (*.SLDDRW)|*.SLDDRW"
        End Select

        Return "SOLIDWORKS CAD Files (*.SLDPRT;*.SLDASM;*.SLDDRW)|*.SLDPRT;*.SLDASM;*.SLDDRW"
    End Function

    Public Function handleSolidWorksSaveCommandPreNotifyPublic(ByVal command As Integer,
                                                               ByVal userCommand As Integer) As Integer
        'Only intercept a brand-new, never-saved CAD document. Existing Save and Save As
        'commands continue through SOLIDWORKS and are handled by the document save events.
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

        If Not String.IsNullOrWhiteSpace(currentPath) Then Return 0

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

        Dim requestedName As String = promptForValidGrc27FileName(titleNoExt & ext)
        If String.IsNullOrWhiteSpace(requestedName) Then Return -1

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

    Private Sub performNewDocumentSvnSave(ByVal doc As ModelDoc2,
                                          ByVal compliantFileName As String)
        Try
            If doc Is Nothing Then Exit Sub
            If String.IsNullOrWhiteSpace(compliantFileName) Then Exit Sub

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

            Using dialog As New System.Windows.Forms.SaveFileDialog()
                dialog.Title = "Save New Gryphon Racing SVN CAD File"
                dialog.InitialDirectory = repoRoot
                dialog.FileName = compliantFileName
                dialog.Filter = getSaveDialogFilterForDocument(doc)
                dialog.FilterIndex = 1
                dialog.DefaultExt = getCadExtensionForDocument(doc).TrimStart("."c)
                dialog.AddExtension = True
                dialog.CheckPathExists = True
                dialog.OverwritePrompt = True
                dialog.RestoreDirectory = True
                dialog.ValidateNames = True

                Do
                    Dim dialogResult As System.Windows.Forms.DialogResult

                    If owner Is Nothing Then
                        dialogResult = dialog.ShowDialog()
                    Else
                        dialogResult = dialog.ShowDialog(owner)
                    End If

                    If dialogResult <> System.Windows.Forms.DialogResult.OK Then Exit Sub

                    selectedPath = dialog.FileName

                    If Not isPathInsideLocalRepo(selectedPath) Then
                        iSwApp.SendMsgToUser2(
                            "Choose a folder inside the configured SVN working copy." & vbCrLf & vbCrLf &
                            repoRoot,
                            swMessageBoxIcon_e.swMbWarning,
                            swMessageBoxBtn_e.swMbOk
                        )
                        dialog.InitialDirectory = repoRoot
                        dialog.FileName = compliantFileName
                        Continue Do
                    End If

                    If Not isVendorPartPath(selectedPath) AndAlso
                       Not shouldIgnoreGrc27NamingConventionForDebug() AndAlso
                       Not isValidGrc27FileName(selectedPath) Then

                        iSwApp.SendMsgToUser2(
                            "The file name must remain compliant with the GRC27/CFD27 naming convention." & vbCrLf & vbCrLf &
                            "Required name:" & vbCrLf & compliantFileName,
                            swMessageBoxIcon_e.swMbWarning,
                            swMessageBoxBtn_e.swMbOk
                        )
                        dialog.FileName = compliantFileName
                        Continue Do
                    End If

                    If Not automaticSaveTargetHasRequiredLock(selectedPath) Then
                        dialog.FileName = compliantFileName
                        Continue Do
                    End If

                    Exit Do
                Loop
            End Using

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

        Dim statusChar As Char = getFirstSvnStatusChar(filePath)

        If statusChar = "?"c OrElse statusChar = "A"c Then Return True

        If statusChar = ChrW(0) Then
            iSwApp.SendMsgToUser2(
                "Save blocked." & vbCrLf & vbCrLf &
                "The plugin could not verify SVN status for:" & vbCrLf &
                filePath & vbCrLf & vbCrLf &
                "Run Cleanup/Sync and try again.",
                swMessageBoxIcon_e.swMbStop,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End If

        If userHasLocalSvnLockTokenForPath(filePath) Then Return True

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

    Private Function userHasLocalSvnLockTokenForPath(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Try
            Dim cached As SVNStatus.filePpty = Nothing

            If tryFindCachedStatusProperty(filePath, cached) AndAlso cached.lock6 = "K" Then
                Return True
            End If
        Catch
        End Try

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

    Private Sub processPendingAutomaticSaveCommits()
        If asyncCommitInProgress Then Exit Sub
        If automaticSaveCommitPreparing Then Exit Sub
        If pendingAutomaticSaveCommitPaths.Count = 0 Then Exit Sub

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

    Private Function automaticSaveCommitPathsHaveRequiredLocks(ByVal commitPaths() As String) As Boolean
        If commitPaths Is Nothing Then Return False

        Dim missingLocks As New List(Of String)()

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Directory.Exists(p) Then Continue For
            If Not File.Exists(p) Then Continue For
            If Not isCadFilePath(p) Then Continue For
            If isFirstCommitCandidatePath(p) Then Continue For

            If Not userHasLocalSvnLockTokenForPath(p) Then
                missingLocks.Add(p)
            End If
        Next

        If missingLocks.Count = 0 Then Return True

        iSwApp.SendMsgToUser2(
            "Automatic commit blocked." & vbCrLf & vbCrLf &
            "These versioned CAD files are not locked by you:" & vbCrLf &
            stringArrToSingleStringWithNewLines(missingLocks.ToArray(), bTrimFileNames:=True, iLimit:=10) & vbCrLf &
            "Get Locks, save again, and the plugin will commit automatically.",
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
                For Each p As String In commitPaths
                    If String.IsNullOrWhiteSpace(p) OrElse Not File.Exists(p) Then Continue For
                    If firstCommitCadPaths IsNot Nothing AndAlso firstCommitCadPaths.Any(Function(newPath) pathsAreSame(newPath, p)) Then Continue For

                    File.SetAttributes(p, File.GetAttributes(p) And Not FileAttributes.ReadOnly)

                    Dim openDoc As ModelDoc2 = getOpenModelByPathSafe(p)
                    If openDoc IsNot Nothing Then openDoc.SetReadOnlyState(False)
                Next
            Catch
            End Try
        End If

        'Pure first commits and mixed assembly commits both need locks on every newly-added CAD file.
        If firstCommitCadPaths IsNot Nothing AndAlso firstCommitCadPaths.Length > 0 Then
            Try
                getLocksOfPathsAsync(
                    firstCommitCadPaths,
                    bBreakLocks:=False,
                    bUseTortoise:=False,
                    sMessage:="Auto-lock after automatic save commit"
                )
            Catch
            End Try
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


    Public Function updateLockStatusPublic(Optional bRefreshAllTreeViews As Boolean = True) As Boolean
        updateLockStatusPublic = statusOfAllOpenModels.updateStatusLocally(iSwApp)

        'Local lock/status refreshes may preserve the last server-aware upToDate9 values,
        'but they are not a new Sync and must not reset the displayed Sync age.
        rebuildStatusCacheFromStatus(statusOfAllOpenModels, markAsServerSync:=False)

        'If the local working copy still has K lock tokens, immediately reconcile open
        'SOLIDWORKS documents to writable. This repairs stale cache/read-only states without
        'requiring the unsafe unlock-then-relock workaround and without changing unlocked docs.
        Try
            Dim locallyLockedPaths() As String = getLockedPathsFromStatus(statusOfAllOpenModels)

            If locallyLockedPaths IsNot Nothing AndAlso locallyLockedPaths.Length > 0 Then
                myUserControl.forceWriteAccessForLockedFilePathsPublic(locallyLockedPaths)
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


    Public Function blockCloseIfOpenDocsUnsafe() As Boolean
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False
        If Not isOnlineModeEnabled() Then Return False

        If DateTime.Now < applicationLockReviewApprovedUntil Then Return False

        Dim blockedByUnsafeChanges As Boolean = blockCloseIfOpenDocsUnsafeOnly()
        If blockedByUnsafeChanges Then Return True

        'If the user explicitly chose the existing "close anyway" path for unsafe changes,
        'do not immediately place a second lock-review dialog in front of that decision.
        If DateTime.Now < unsafeForceCloseApprovedUntil Then Return False

        Return blockCloseForOwnedLocks(
            isClosingSolidWorks:=True,
            closingDocumentPath:=""
        )
    End Function

    Private Function blockCloseIfOpenDocsUnsafeOnly() As Boolean
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False
        If Not isOnlineModeEnabled() Then Return False

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

        Catch
            Return False
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

    Public Function blockCloseIfSingleDocUnsafe(ByVal closingDoc As ModelDoc2) As Boolean
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False
        If Not isOnlineModeEnabled() Then Return False

        'If the user just chose "No = close anyway", allow duplicate close events through briefly.
        If DateTime.Now < unsafeForceCloseApprovedUntil Then Return False

        'A duplicate close message can arrive while the modal warning is open.
        'Block it; allowing it through can destroy the document behind the prompt.
        If closeGuardMessageShowing Then Return True
        If closingDoc Is Nothing Then Return False

        Dim openPaths As New List(Of String)

        Try
            Dim docPath As String = ""
            Dim title As String = ""

            Try
                docPath = closingDoc.GetPathName()
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

        Catch
            Return False
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

    Public Function blockCloseIfActiveDocUnsafe() As Boolean
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False
        If Not isOnlineModeEnabled() Then Return False

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

        If DateTime.Now < documentLockReviewApprovedUntil AndAlso
           String.Equals(normalizeSvnPath(activePath),
                         normalizeSvnPath(documentLockReviewApprovedPath),
                         StringComparison.OrdinalIgnoreCase) Then
            Return False
        End If

        Dim blockedByUnsafeChanges As Boolean = blockCloseIfSingleDocUnsafe(activeDoc)
        If blockedByUnsafeChanges Then Return True

        'The pre-existing close-anyway choice is authoritative. Do not stack the
        'informational lock-review table on top of that exceptional path.
        If DateTime.Now < unsafeForceCloseApprovedUntil Then Return False

        Return blockCloseForOwnedLocks(
            isClosingSolidWorks:=False,
            closingDocumentPath:=activePath
        )
    End Function

    Private Function blockCloseForOwnedLocks(ByVal isClosingSolidWorks As Boolean,
                                                  ByVal closingDocumentPath As String) As Boolean
        If iSwApp Is Nothing Then Return False
        If myUserControl Is Nothing Then Return False
        If Not isOnlineModeEnabled() Then Return False

        'A second native close message can be pumped while the modal table is open.
        'Always block that duplicate instead of allowing the document/application to
        'close behind the user's review window.
        If lockReviewMessageShowing Then Return True

        Dim reviewItems As List(Of CloseLockReviewItem) = Nothing

        Try
            If isClosingSolidWorks Then
                reviewItems = getOwnedLockReviewItems(
                    candidatePaths:=Nothing,
                    scanWholeWorkingCopy:=True
                )
            Else
                reviewItems = getOwnedLockReviewItems(
                    candidatePaths:=getOpenSessionCadPathsForLockReview(),
                    scanWholeWorkingCopy:=False
                )
            End If
        Catch
            reviewItems = Nothing
        End Try

        If reviewItems Is Nothing OrElse reviewItems.Count = 0 Then Return False

        Try
            lockReviewMessageShowing = True

            Using reviewForm As New CloseLockReviewForm(reviewItems, isClosingSolidWorks)
                reviewForm.ShowDialog()

                If reviewForm.Decision = CloseLockReviewDecision.ContinueClose Then
                    If isClosingSolidWorks Then
                        applicationLockReviewApprovedUntil = DateTime.Now.AddSeconds(10)
                    Else
                        documentLockReviewApprovedPath = closingDocumentPath
                        documentLockReviewApprovedUntil = DateTime.Now.AddSeconds(10)
                    End If

                    Return False
                End If
            End Using

            Return True
        Catch
            'The lock table is advisory after the existing save/commit failsafe has
            'already passed. Never crash SOLIDWORKS if the review UI itself fails.
            Return False
        Finally
            lockReviewMessageShowing = False
        End Try
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

    Private Function getOwnedLockReviewItems(ByVal candidatePaths() As String,
                                             ByVal scanWholeWorkingCopy As Boolean) As List(Of CloseLockReviewItem)
        Dim output As New List(Of CloseLockReviewItem)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If String.IsNullOrWhiteSpace(sSVNPath) OrElse Not File.Exists(sSVNPath) Then Return output

        Dim workingCopyRoot As String = getResolvedSvnWorkingCopyRootPath()
        If String.IsNullOrWhiteSpace(workingCopyRoot) Then Return output

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
            If filteredPaths Is Nothing OrElse filteredPaths.Length = 0 Then Return output
            statusArguments &= formatFilePathArrForSvnProc(filteredPaths)
        End If

        Dim statusResult As rawProcessReturn = runSvnProcess(sSVNPath, statusArguments)
        Dim outputText As String = If(statusResult.output, "")
        Dim errorText As String = If(statusResult.outputError, "").Trim()

        If errorText <> "" OrElse String.IsNullOrWhiteSpace(outputText) Then Return output

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

            Dim safeToUnlock As Boolean =
                workingCopyState = " "c AndAlso
                propertyState = " "c AndAlso
                treeConflictState = " "c

            Dim stateText As String = If(
                safeToUnlock,
                "Saved and committed",
                getLockReviewUnsafeStateText(workingCopyState, propertyState, treeConflictState)
            )

            output.Add(
                New CloseLockReviewItem With {
                    .FilePath = filePath,
                    .IsSafeToUnlock = safeToUnlock,
                    .StateText = stateText,
                    .IsStillLocked = True,
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
            scanWholeWorkingCopy:=False
        )

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

        Try
            refreshActiveTreeAfterSvnAction(
                bUpdateLocalLockStatus:=False,
                bRebuildTree:=False
            )
        Catch
        End Try

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

    Private Function showUnsafeClosePrompt(ByVal unsafeMsg As String) As Boolean
        Dim response As Integer = iSwApp.SendMsgToUser2(
            "One or more open CAD files are not safe to close yet." & vbCrLf & vbCrLf &
            unsafeMsg & vbCrLf &
            "Choose an action:" & vbCrLf &
            "Yes = I want to go back to get locks / push / revert" & vbCrLf &
            "No = I want to close SolidWorks anyway",
            swMessageBoxIcon_e.swMbWarning,
            swMessageBoxBtn_e.swMbYesNo
        )

        If response = swMessageBoxResult_e.swMbHitYes Then
            iSwApp.SendMsgToUser2(
                "Close cancelled." & vbCrLf & vbCrLf &
                "Get Locks if needed, then Commit/push your files, or use Unlock && Revert to go back to the original version.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )

            Return True 'Block close
        End If

        If response = swMessageBoxResult_e.swMbHitNo Then
            'Allow duplicate close events through briefly.
            'This prevents the same force-close choice from prompting multiple times.
            unsafeForceCloseApprovedUntil = DateTime.Now.AddSeconds(10)
            Return False 'Allow close anyway
        End If

        Return True 'Safety fallback: block close
    End Function


    Private Function getUnsafeCloseStatusMessage(openPaths As List(Of String)) As String
        If openPaths Is Nothing OrElse openPaths.Count = 0 Then Return ""

        Dim msg As String = ""

        For Each filePath As String In openPaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            If filePath.StartsWith("[UNSAVED_SOLIDWORKS_CHANGES]") Then
                msg &= filePath.Replace("[UNSAVED_SOLIDWORKS_CHANGES] ", "") & vbCrLf &
                "SolidWorks has unsaved changes and you have the SVN lock, or this is a new file ready to be committed." & vbCrLf & vbCrLf
                Continue For
            End If

            If filePath.StartsWith("[UNSAVED_WITHOUT_LOCK]") Then
                msg &= filePath.Replace("[UNSAVED_WITHOUT_LOCK] ", "") & vbCrLf &
                "SolidWorks has unsaved changes, but you do NOT have the SVN lock. Click Get Locks before saving/committing, or revert/close to discard these changes." & vbCrLf & vbCrLf
                Continue For
            End If

            If filePath.StartsWith("[UNSAVED_NEW_FILE]") Then
                msg &= filePath.Replace("[UNSAVED_NEW_FILE] ", "") & vbCrLf &
           "New SolidWorks file has not been saved yet." & vbCrLf & vbCrLf
                Continue For
            End If

            If Not File.Exists(filePath) Then Continue For

            Try
                Dim statusResult As rawProcessReturn = runSvnProcess(
                sSVNPath,
                "status -u --non-interactive """ & filePath & """"
            )

                Dim outputText As String = ""
                If statusResult.output IsNot Nothing Then outputText = statusResult.output.Trim()

                Dim errorText As String = ""
                If statusResult.outputError IsNot Nothing Then errorText = statusResult.outputError.Trim()

                If errorText <> "" Then
                    msg &= Path.GetFileName(filePath) & vbCrLf &
                       "SVN status error: " & errorText & vbCrLf & vbCrLf
                    Continue For
                End If

                If String.IsNullOrWhiteSpace(outputText) Then Continue For

                Dim lines() As String = outputText.Split({vbCrLf, vbLf}, StringSplitOptions.RemoveEmptyEntries)

                For Each line As String In lines
                    If String.IsNullOrWhiteSpace(line) Then Continue For
                    If line.StartsWith("Status against revision", StringComparison.OrdinalIgnoreCase) Then Continue For

                    Dim reason As String = getHumanReadableSvnCloseReason(line)

                    If reason <> "" Then
                        msg &= Path.GetFileName(filePath) & vbCrLf &
                           reason & vbCrLf & vbCrLf
                        Exit For
                    End If
                Next

            Catch ex As Exception
                msg &= Path.GetFileName(filePath) & vbCrLf &
                   "Could not verify SVN status before close." & vbCrLf & vbCrLf
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

                Dim locallyOwnedPaths() As String = getLockedPathsFromStatus(statusOfAllOpenModels)
                If locallyOwnedPaths IsNot Nothing AndAlso locallyOwnedPaths.Length > 0 Then
                    myUserControl.forceWriteAccessForLockedFilePathsPublic(locallyOwnedPaths)
                End If
            End If
        Catch
        End Try
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

        Dim output As rawProcessReturn
        Dim oSVNProcess As New Process()
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

        System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.WaitCursor

        '============
        'sbOutputLines = New System.Text.StringBuilder()

        ' Set our event handler to asynchronously read the sort output.
        'AddHandler oSVNProcess.OutputDataReceived, AddressOf SortOutputHandler

        'iSwApp.SendMsgToUser(filename & vbCrLf & arguments)

        If arguments Is Nothing Then Return Nothing
        If Not verifyCommandArgumentLength(arguments) Then Return Nothing

        oSVNProcess.Start()

        'Using Sync
        Using ostreamreader As System.IO.StreamReader = oSVNProcess.StandardOutput
            output.output = ostreamreader.ReadToEnd()
        End Using
        Using ostreamreader As System.IO.StreamReader = oSVNProcess.StandardError
            output.outputError = ostreamreader.ReadToEnd()
        End Using

        'Using Sync
        'Dim returnLines(10000) As String
        'Dim returnError(10000) As String

        'Dim oneReturnLine As String
        'Dim oneErrorLine As String

        'Dim returnLines As New List(Of String)()
        'Dim returnError As New List(Of String)()
        '
        'Using ostreamreader As System.IO.StreamReader = oSVNProcess.StandardOutput
        '    oneReturnLine = ostreamreader.ReadLine()
        '    While Not IsNothing(oneReturnLine)
        '        returnLines.Add(oneReturnLine)
        '        oneReturnLine = ostreamreader.ReadLine() 'read next line
        '    End While
        'End Using
        'Using ostreamreader As System.IO.StreamReader = oSVNProcess.StandardError
        '    oneErrorLine = ostreamreader.ReadLine()
        '    While Not IsNothing(oneErrorLine)
        '        returnError.Add(oneErrorLine)
        '        oneErrorLine = ostreamreader.ReadLine() 'read next line
        '    End While
        'End Using

        'output.output = returnLines.ToArray
        'output.outputError = returnError.ToArray


        Do While Not oSVNProcess.WaitForExit(iWaitTime)
            'If the process doesn't finish after 10s then kill it and send error message to user
            oSVNProcess.Kill()
            If iSwApp.SendMsgToUser2("SVN timed out While attempting To connect To the vault. " &
                                  "Would you like to give it more time?",
                                  swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbYesNo) = swMessageBoxResult_e.swMbHitYes Then
                iSwApp.SendMsgToUser("Switching to offline mode")
                switchToOffline()
                System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Default
                Return Nothing
            Else
                iWaitTime += 5000
            End If
        Loop

        System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Default
        Return output
    End Function

    Private Function runSvnProcessBackgroundNoUi(ByVal filename As String,
                                                 ByVal arguments As String,
                                                 ByVal savedPathForBackground As String) As rawProcessReturn
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

            Dim p As New Process()
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
            p.Start()

            output.output = p.StandardOutput.ReadToEnd()
            output.outputError = p.StandardError.ReadToEnd()

            If Not p.WaitForExit(120000) Then
                Try
                    p.Kill()
                Catch
                End Try

                output.outputError = "SVN command timed out while running in the background."
            End If

            Return output

        Catch ex As Exception
            output.output = ""
            output.outputError = ex.Message
            Return output
        End Try
    End Function
    Sub myUnlockWithDependents(modDoc As ModelDoc2)

        'Dim modDoc() As ModelDoc2 = {iSwApp.ActiveDoc()}
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Active Document not found") : Exit Sub

        unlockDocs(myUserControl.getComponentsOfAssemblyOptionalUpdateTree(myUserControl.GetSelectedModDocList(iSwApp)))

    End Sub
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
                If iSwApp.SendMsgToUser2("Export PDF?" & componentAndDrawingModDoc(0).GetTitle, swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNo) = swMessageBoxResult_e.swMbHitYes Then
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
            Dim activeDoc As ModelDoc2 = iSwApp.ActiveDoc
            Dim activePath As String = ""

            If activeDoc IsNot Nothing Then
                activePath = activeDoc.GetPathName()
            End If

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

            'If active assembly was referencing the old file, point it to the new file.
            If activeDoc IsNot Nothing AndAlso
           activeDoc.GetType = swDocumentTypes_e.swDocASSEMBLY AndAlso
           Not String.IsNullOrWhiteSpace(activePath) Then

                Try
                    iSwApp.ReplaceReferencedDocument(activePath, oldPath, newPath)
                Catch
                End Try
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

        Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        If activeDoc Is Nothing Then Return False

        If activeDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then
            Return True
        End If

        Dim assy As AssemblyDoc = CType(activeDoc, AssemblyDoc)
        Dim activeAssemblyPath As String = ""

        Try
            activeAssemblyPath = activeDoc.GetPathName()
        Catch
            activeAssemblyPath = ""
        End Try

        'Fast progressive relink:
        '  1. Try the lightweight document-reference redirect.
        '  2. Verify the open assembly path immediately and stop if it worked.
        '  3. Try Component2.ReplaceReference only when needed.
        '  4. Use the selection-based ReplaceComponents command only as the last resort.
        'The old implementation ran all three methods for every file, which could repeatedly
        'evaluate mates and trigger rebuild/error sounds even after the first method succeeded.
        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For
            If String.IsNullOrWhiteSpace(refInfo.oldPath) Then Continue For
            If String.IsNullOrWhiteSpace(refInfo.newPath) Then Continue For

            If externalReferenceIsRelinked(assy, refInfo) Then Continue For

            'Stage 1: cheapest path-level redirect.
            If Not String.IsNullOrWhiteSpace(activeAssemblyPath) Then
                Try
                    iSwApp.ReplaceReferencedDocument(activeAssemblyPath, refInfo.oldPath, refInfo.newPath)
                Catch
                End Try
            End If

            If externalReferenceIsRelinked(assy, refInfo) Then Continue For

            'Stage 2: update only component instances that still use the old exact full path.
            Dim componentsUsingOldPath As List(Of Component2) =
                getAssemblyComponentsUsingPath(assy, refInfo.oldPath)

            For Each comp As Component2 In componentsUsingOldPath
                Try
                    comp.ReplaceReference(refInfo.newPath)
                Catch
                End Try
            Next

            If externalReferenceIsRelinked(assy, refInfo) Then Continue For

            'Stage 3: most disruptive/manual-style replacement, used only as a last fallback.
            componentsUsingOldPath = getAssemblyComponentsUsingPath(assy, refInfo.oldPath)

            For Each comp As Component2 In componentsUsingOldPath
                Try
                    tryAssemblyReplaceComponent(assy, comp, refInfo.newPath)
                Catch
                End Try
            Next
        Next

        'Persist all successful path changes once, without forcing a full assembly rebuild.
        Dim saveErrors As Integer = 0
        Dim saveWarnings As Integer = 0
        Dim fastSaveSucceeded As Boolean =
            saveRelinkedAssemblyWithoutRebuild(activeDoc, saveErrors, saveWarnings)

        If fastSaveSucceeded AndAlso allExternalReferencesAreRelinked(assy, externalRefs) Then
            Return True
        End If

        'Recovery only:
        'Some SolidWorks states do not expose the new in-memory component path until one rebuild.
        'Run at most one full rebuild/save, and only when the lightweight path did not fully settle.
        Try
            activeDoc.ForceRebuild3(False)
        Catch
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
        Catch
            recoverySaveSucceeded = False
        End Try

        If recoverySaveSucceeded AndAlso allExternalReferencesAreRelinked(assy, externalRefs) Then
            Return True
        End If

        Dim failedMsg As New System.Text.StringBuilder()

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
            failedMsg.AppendLine("The references appear updated, but SolidWorks could not save the assembly reliably.")
            failedMsg.AppendLine("Fast save errors: " & saveErrors.ToString() & "; warnings: " & saveWarnings.ToString())
            failedMsg.AppendLine("Recovery save errors: " & recoveryErrors.ToString() & "; warnings: " & recoveryWarnings.ToString())
        End If

        iSwApp.SendMsgToUser2(
            "Commit blocked." & vbCrLf & vbCrLf &
            "SolidWorks could not complete and save the external/vendor reference relink." & vbCrLf & vbCrLf &
            failedMsg.ToString(),
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )

        Return False
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
            item.ProposedId = Path.GetFileNameWithoutExtension(refInfo.fileName)
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
                File.Copy(refInfo.oldPath, item.DestinationPath, overwrite:=False)
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
            If isSolidWorksTempOrVirtualPath(refInfo.oldPath) Then
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

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            saveAllOpenFiles(bShowError:=True)
            If debugWatch IsNot Nothing Then debugNotes.Add("Save open files: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        Catch
            saveAllOpenFiles(bShowError:=True)
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

        If Not bSuccess Then iSwApp.SendMsgToUserv("Releasing Locks Failed.")

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

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds
            saveAllOpenFiles(bShowError:=True)
            If debugWatch IsNot Nothing Then debugNotes.Add("Save open files: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        Catch
            saveAllOpenFiles(bShowError:=True)
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

            If Not bSuccess Then iSwApp.SendMsgToUserv("Releasing Locks Failed.")

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

        If Not bSuccess Then iSwApp.SendMsgToUserv("Releasing Locks Failed.")

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
    Public Sub tortCommitPathsAsync(ByVal commitPaths() As String, Optional sCommitMessage As String = "")
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

        If Not validateCadPathNamesBeforeCommit(sModDocPathArr) Then Exit Sub
        If Not validateNoDuplicateCadFileNamesForPaths(sModDocPathArr) Then Exit Sub
        If Not commitPathsAllowedOnlyIfUpToDate(sModDocPathArr) Then Exit Sub
        If Not commitAssemblyChildrenAllowedOnlyIfCachedUpToDate(sModDocPathArr) Then Exit Sub

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

        warnIfActiveAssemblyDirtyButNotInCommit(sModDocPathArr)

        startCommitProcessBackground(sModDocPathArr, sCommitMessage, bAutoFirstCommitDataset)
    End Sub

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
                        "Make sure the file is locked by you and writable, then try again.",
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
                "Child file commit started." & vbCrLf & vbCrLf &
                "Note: the parent assembly still has unsaved changes. The child can be committed separately, but commit/check out the parent assembly only if you intentionally changed assembly-level mates, positions, references, or display/config state.",
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

                         If success Then
                             For Each commitPath As String In pathsForBackground
                                 Try
                                     If File.Exists(commitPath) Then
                                         File.SetAttributes(commitPath, File.GetAttributes(commitPath) Or FileAttributes.ReadOnly)
                                     End If
                                 Catch
                                 End Try
                             Next
                         End If

                     Catch ex As Exception
                         success = False
                         errorMessage = ex.Message
                     End Try

                     Try
                         If myUserControl IsNot Nothing AndAlso myUserControl.IsHandleCreated Then
                             myUserControl.BeginInvoke(New MethodInvoker(Sub() finishCommitProcessOnMainThread(pathsForBackground, success, errorMessage, bAutoFirstCommitDataset)))
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
            processPendingAutomaticSaveCommits()
            Exit Sub
        End If

        Try
            myUserControl.markCommitResultForFilePathsPublic(commitPaths, True)
        Catch
        End Try

        Try
            updateStatusCacheForKnownPaths(commitPaths, forceAddDelChg1:=" ", forceLock6:=" ", forceUpToDate9:=" ")
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

            Dim p As New Process()
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
        statusOfAllOpenModels.setReadWriteFromLockStatus()
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
            statusOfAllOpenModels.setReadWriteFromLockStatus()
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
                                    Optional sMessage As String = "")
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

        asyncGetLocksInProgress = True

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
                         End If
                     Catch
                         asyncGetLocksInProgress = False
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
                                                            ByVal savedPathForBackground As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not File.Exists(filePath) Then Return False
        If Not isCadFilePath(filePath) Then Return False
        If Not isPathInsideRepoRootForBackground(filePath, repoRootPathForBackground) Then Return False

        Try
            Dim statusChar As Char = getFirstSvnStatusCharForBackground(filePath, savedPathForBackground)
            Return statusChar = "?"c OrElse statusChar = "A"c
        Catch
            Return False
        End Try
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

    Private Function getFirstSvnStatusCharForBackground(ByVal filePath As String,
                                                        ByVal savedPathForBackground As String) As Char
        If String.IsNullOrWhiteSpace(filePath) Then Return ChrW(0)
        If Not File.Exists(filePath) Then Return ChrW(0)

        Try
            Dim statusResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
                sSVNPath,
                "status --non-interactive """ & filePath.Replace("""", "") & """",
                savedPathForBackground
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
                If isFirstCommitCandidatePathForAsyncLock(filePath, repoRootPathForBackground, savedPathForBackground) Then
                    Try
                        File.SetAttributes(filePath, File.GetAttributes(filePath) And Not FileAttributes.ReadOnly)
                    Catch
                    End Try

                    firstCommitPaths.Add(filePath)
                ElseIf pathHasLocalSvnLockTokenBackground(filePath, savedPathForBackground) Then
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

            Dim outOfDatePaths() As String = getOutOfDatePathsForAsyncLock(lockablePaths.ToArray(), result.Message, savedPathForBackground)
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

            Dim releasedPaths() As String = getReleasedPathsForAsyncLock(lockablePaths.ToArray(), savedPathForBackground)
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

            Dim lockResult As rawProcessReturn = runSvnLockForPathsBackground(lockablePaths.ToArray(), bBreakLocks, sMessage, savedPathForBackground)

            If lockResult.outputError IsNot Nothing AndAlso lockResult.outputError.Trim() <> "" Then
                'One final local-token reconciliation handles the race where the cache said
                'unlocked but SVN reports that this same working copy already owns the lock.
                Dim recoveredAlreadyLocked As New List(Of String)()

                For Each filePath As String In lockablePaths
                    If pathHasLocalSvnLockTokenBackground(filePath, savedPathForBackground) Then
                        recoveredAlreadyLocked.Add(filePath)
                    End If
                Next

                If recoveredAlreadyLocked.Count = lockablePaths.Count Then
                    alreadyLockedPaths.AddRange(recoveredAlreadyLocked)
                    result.Success = True
                    result.LockedPaths = alreadyLockedPaths.Distinct(StringComparer.OrdinalIgnoreCase).ToArray()
                    result.IsInfoOnly = True
                    result.Message = "You already own the selected SVN lock." & vbCrLf & vbCrLf &
                        "PlumVault reconciled the stale lock display and restored write access."
                    Return result
                End If

                result.IsWarning = True
                result.Message = "Locking failed." & vbCrLf & vbCrLf & lockResult.outputError.Trim()
                Return result
            End If

            Dim propResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
                sSVNPath,
                "propset addin:release_state ""||EDIT||"" " & quoteFilePathArgs(lockablePaths.ToArray()),
                savedPathForBackground
            )

            If propResult.outputError IsNot Nothing AndAlso propResult.outputError.Trim() <> "" Then
                result.IsWarning = True
                result.Message = "Files were locked, but setting the SVN edit property failed." & vbCrLf & vbCrLf &
                    propResult.outputError.Trim()
                result.LockedPaths = lockablePaths.ToArray()
                Return result
            End If

            For Each filePath As String In lockablePaths
                Try
                    File.SetAttributes(filePath, File.GetAttributes(filePath) And Not FileAttributes.ReadOnly)
                Catch
                End Try
            Next

            result.Success = True

            Dim allOwnedPaths As New List(Of String)()
            allOwnedPaths.AddRange(alreadyLockedPaths)
            allOwnedPaths.AddRange(lockablePaths)
            result.LockedPaths = allOwnedPaths.Distinct(StringComparer.OrdinalIgnoreCase).ToArray()
            Return result

        Catch ex As Exception
            result.IsWarning = True
            result.Message = "Error running Get Locks in the background: " & ex.Message
            Return result
        End Try
    End Function

    Private Sub finishAsyncGetLocksOnMainThread(ByVal result As AsyncGetLocksResult)
        Try
            If result IsNot Nothing AndAlso result.AttemptedPaths IsNot Nothing Then
                myUserControl.markLockPendingForFilePathsPublic(result.AttemptedPaths, False)
            End If
        Catch
        End Try

        Try
            If result IsNot Nothing AndAlso result.LockedPaths IsNot Nothing AndAlso result.LockedPaths.Length > 0 Then
                myUserControl.forceWriteAccessForLockedFilePathsPublic(result.LockedPaths)
                myUserControl.markLockResultForFilePathsPublic(result.LockedPaths, True, "Locked by you")
                updateStatusCacheForKnownPaths(result.LockedPaths, forceLock6:="K", forceReleased:="||EDIT||")
            End If
        Catch
        End Try

        asyncGetLocksInProgress = False

        If result Is Nothing Then Exit Sub

        If Not String.IsNullOrWhiteSpace(result.Message) Then
            Dim icon As swMessageBoxIcon_e = If(result.IsWarning, swMessageBoxIcon_e.swMbWarning, swMessageBoxIcon_e.swMbInformation)
            iSwApp.SendMsgToUser2(result.Message, icon, swMessageBoxBtn_e.swMbOk)
        End If
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
                                                   ByVal savedPathForBackground As String) As String()
        errorMessage = ""

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Return Nothing

        Dim statusResult As rawProcessReturn = runSvnProcessBackgroundNoUi(
            sSVNPath,
            "status -vu --non-interactive " & quoteFilePathArgs(filePaths),
            savedPathForBackground
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

        statusOfAllOpenModels.setReadWriteFromLockStatus()
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
        Dim sOutputError As String = ""

        Try
            Dim lockArgs As String = "lock "

            If bBreakLocks Then
                lockArgs &= "--force "
            End If

            processOutputArr = runSvnByArgs(
                sModDocPathArr,
                lockArgs,
                "-m",
                """" & sMessage & """",
                bEach:=False
            )

            If processOutputArr Is Nothing OrElse processOutputArr.Length = 0 Then
                iSwApp.SendMsgToUser("Error: SVN lock returned no process output.")
                Return bSuccess
            End If

            If processOutputArr(0).outputError IsNot Nothing Then
                sOutputError = processOutputArr(0).outputError.Trim()
            End If

            If sOutputError <> "" Then
                iSwApp.SendMsgToUser("Error: " & sOutputError)
                Return bSuccess
            End If

            For i As Integer = 0 To UBound(bSuccess)
                bSuccess(i) = True
            Next

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
            status.setReadWriteFromLockStatus()
            If debugWatch IsNot Nothing Then debugNotes.Add("Set read/write from lock status: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
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
        Dim oTortProcess As New Process()
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

        'Monitor the process. Kill it if it stops responding
        Dim nResponding As Integer = 0
        Do While (Not oTortProcess.HasExited)
            nResponding += Not (oTortProcess.Responding)
            If nResponding > 3000 Then 'Sort of milliseconds because of the sleep command. But not exactly.
                oTortProcess.Kill()
                iSwApp.SendMsgToUser("SVNTortoise Window Timed Out")
                Return False
            End If
            System.Threading.Thread.Sleep(1)
        Loop

        'sw.Stop()
        'System.Diagnostics.Debug.WriteLine("tortoiseProc Time Taken: " + sw.Elapsed.TotalMilliseconds.ToString("#,##0.00 'milliseconds'"))

        Return True
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
        Dim bSuccess As Boolean
        Dim output As SVNStatus = New SVNStatus()
        Dim cachedFp As New SVNStatus.filePpty()

        If String.IsNullOrWhiteSpace(sFileName) Then Return Nothing

        If IsNothing(statusOfAllOpenModels) Then
            bSuccess = updateStatusOfAllModelsVariable()
            If Not bSuccess Then Return Nothing
        End If

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
