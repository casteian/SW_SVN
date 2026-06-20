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
    Private lastCloseGuardPromptTime As DateTime = DateTime.MinValue
    Private unsafeForceCloseApprovedUntil As DateTime = DateTime.MinValue
    Private statusCacheByNormalizedPath As New Dictionary(Of String, SVNStatus.filePpty)(StringComparer.OrdinalIgnoreCase)
    Private statusCacheLastWriteUtc As DateTime = DateTime.MinValue
    Private statusCacheLastServerAwareUtc As DateTime = DateTime.MinValue
    Private asyncGetLocksInProgress As Boolean = False
    Private asyncCommitInProgress As Boolean = False
    Private asyncCleanupInProgress As Boolean = False

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
        rebuildStatusCacheFromStatus(statusOfAllOpenModels)
        If bRefreshAllTreeViews Then myUserControl.refreshAllTreeViewsVariable()
    End Function
    Public Function updateStatusOfAllModelsVariable(Optional bRefreshAllTreeViews As Boolean = False) As Boolean
        Dim bWhatToReturn As Boolean = False

        bWhatToReturn = statusOfAllOpenModels.updateFromSvnServer(bRefreshAllTreeViews)
        rebuildStatusCacheFromStatus(statusOfAllOpenModels)

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

        'If the user just chose "No = close anyway", allow duplicate close events through briefly.
        If DateTime.Now < unsafeForceCloseApprovedUntil Then Return False

        If closeGuardMessageShowing Then Return False

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
                    If userHasSvnLockOnDoc(doc) OrElse isNewUnversionedOrAddedFile(docPath) Then
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

        If closeGuardMessageShowing Then Return False
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
                If userHasSvnLockOnDoc(closingDoc) OrElse isNewUnversionedOrAddedFile(docPath) Then
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

        Dim activeDoc As ModelDoc2 = Nothing

        Try
            activeDoc = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch
            activeDoc = Nothing
        End Try

        If activeDoc Is Nothing Then Return False

        Return blockCloseIfSingleDocUnsafe(activeDoc)
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
            If statusCacheByNormalizedPath Is Nothing OrElse statusCacheByNormalizedPath.Count = 0 Then Return "none"
            If statusCacheLastWriteUtc = DateTime.MinValue Then Return "unknown"

            Dim age As TimeSpan = DateTime.UtcNow - statusCacheLastWriteUtc
            If age.TotalSeconds < 0 Then age = TimeSpan.Zero

            Dim ageText As String

            If age.TotalSeconds < 60 Then
                ageText = "now"
            ElseIf age.TotalMinutes < 60 Then
                ageText = CInt(Math.Floor(age.TotalMinutes)).ToString() & "m"
            ElseIf age.TotalHours < 24 Then
                ageText = CInt(Math.Floor(age.TotalHours)).ToString() & "h"
            Else
                ageText = CInt(Math.Floor(age.TotalDays)).ToString() & "d"
            End If

            If statusCacheLastServerAwareUtc = DateTime.MinValue Then
                Return ageText & " local"
            End If

            Dim serverAge As TimeSpan = DateTime.UtcNow - statusCacheLastServerAwareUtc
            If serverAge.TotalSeconds < 0 Then serverAge = TimeSpan.Zero

            Dim serverText As String
            If serverAge.TotalSeconds < 60 Then
                serverText = "sync now"
            ElseIf serverAge.TotalMinutes < 60 Then
                serverText = "sync " & CInt(Math.Floor(serverAge.TotalMinutes)).ToString() & "m"
            ElseIf serverAge.TotalHours < 24 Then
                serverText = "sync " & CInt(Math.Floor(serverAge.TotalHours)).ToString() & "h"
            Else
                serverText = "sync " & CInt(Math.Floor(serverAge.TotalDays)).ToString() & "d"
            End If

            If Math.Abs((statusCacheLastWriteUtc - statusCacheLastServerAwareUtc).TotalSeconds) < 2 Then
                Return serverText
            End If

            Return ageText & " / " & serverText
        Catch
            Return "unknown"
        End Try
    End Function

    Private Sub markStatusCacheWritten(ByVal serverAwareWrite As Boolean)
        statusCacheLastWriteUtc = DateTime.UtcNow

        If serverAwareWrite Then
            statusCacheLastServerAwareUtc = statusCacheLastWriteUtc
        End If

        notifyStatusCacheChanged()
    End Sub

    Private Sub rebuildStatusCacheFromStatus(ByVal statusToCache As SVNStatus)
        Try
            If statusToCache Is Nothing OrElse statusToCache.fp Is Nothing Then Exit Sub

            Dim serverAwareWrite As Boolean = statusContainsServerAwareData(statusToCache)

            'Server-aware Sync/status results replace the cache.
            'Local-only refreshes merge into the existing cache so they do not destroy the
            'last known up-to-date/out-of-date server column used by cached Get Latest.
            If serverAwareWrite Then
                statusCacheByNormalizedPath.Clear()
            End If

            For i As Integer = 0 To UBound(statusToCache.fp)
                Dim filePath As String = statusToCache.fp(i).filename
                If String.IsNullOrWhiteSpace(filePath) Then Continue For

                Dim normalizedPath As String = normalizeSvnPath(filePath)
                If String.IsNullOrWhiteSpace(normalizedPath) Then Continue For

                Dim entryToStore As SVNStatus.filePpty = statusToCache.fp(i)

                If Not serverAwareWrite AndAlso statusCacheByNormalizedPath.ContainsKey(normalizedPath) Then
                    Dim previousEntry As SVNStatus.filePpty = statusCacheByNormalizedPath(normalizedPath)

                    If cacheEntryHasServerAwareData(previousEntry) AndAlso
                       (entryToStore.upToDate9 Is Nothing OrElse String.Equals(entryToStore.upToDate9, "NoUpdate", StringComparison.OrdinalIgnoreCase)) Then
                        entryToStore.upToDate9 = previousEntry.upToDate9
                    End If
                End If

                statusCacheByNormalizedPath(normalizedPath) = entryToStore
            Next

            markStatusCacheWritten(serverAwareWrite)
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
                rebuildStatusCacheFromStatus(statusOfAllOpenModels)
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
            rebuildStatusCacheFromStatus(statusOfAllOpenModels)
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
            rebuildStatusCacheFromStatus(statusOfAllOpenModels)

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
            rebuildStatusCacheFromStatus(statusOfAllOpenModels)
        Catch
        End Try

        Try
            If myUserControl IsNot Nothing Then
                myUserControl.statusOfAllOpenModels = statusOfAllOpenModels
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
        bSuccess = modDoc.Extension.SaveAs3(pdfPath,
                                swSaveAsVersion_e.swSaveAsCurrentVersion,
                                swSaveAsOptions_e.swSaveAsOptions_Copy,
                                Nothing, Nothing, errors, warnings)
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
        bSuccess = componentDoc.Extension.SaveAs3(stepPath,
                                       swSaveAsVersion_e.swSaveAsCurrentVersion,
                                       swSaveAsOptions_e.swSaveAsOptions_Copy + swSaveAsOptions_e.swSaveAsOptions_AvoidRebuildOnSave,
                                       Nothing, Nothing, errors, warnings)
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

    Private Function isPathInsideLocalRepo(filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If myUserControl Is Nothing Then Return False

        Try
            Dim repoRoot As String = Path.GetFullPath(myUserControl.localRepoPath.Text).TrimEnd("\"c)
            Dim fullPath As String = Path.GetFullPath(filePath).TrimEnd("\"c)

            If String.Equals(fullPath, repoRoot, StringComparison.OrdinalIgnoreCase) Then
                Return True
            End If

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

    Private Function getGrc27RootPath() As String
        Return Path.Combine(myUserControl.localRepoPath.Text.TrimEnd("\"c), "GRC27")
    End Function

    Private Function getVendorPartsRootPath() As String
        Return Path.Combine(myUserControl.localRepoPath.Text.TrimEnd("\"c), "Vendor Parts")
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

    Private Function isVendorPartPath(filePath As String) As Boolean
        Return isPathInsideFolder(filePath, getVendorPartsRootPath())
    End Function

    Private Function isCadFilePath(filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim ext As String = Path.GetExtension(filePath).ToUpperInvariant()

        Return ext = ".SLDPRT" OrElse ext = ".SLDASM" OrElse ext = ".SLDDRW"
    End Function

    Private Function isValidGrc27FileName(filePathOrName As String) As Boolean
        If String.IsNullOrWhiteSpace(filePathOrName) Then Return False

        Dim fileName As String = Path.GetFileName(filePathOrName)

        Return System.Text.RegularExpressions.Regex.IsMatch(
        fileName,
        "^GRC27_(BR|DT|AE|FR|EL|ST|SU|WT|MI)_[A-Z]{0,3}\d+_R\d+\.(SLDPRT|SLDASM|SLDDRW)$",
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
            "This file does not follow the GRC27 naming convention." & vbCrLf & vbCrLf &
            "Original file:" & vbCrLf &
            Path.GetFileName(originalPath) & vbCrLf & vbCrLf &
            "Required format:" & vbCrLf &
            "GRC27_CODE_00000_R# or GRC27_CODE_A0000_R# or GRC27_CODE_AB0000_R# or GRC27_CODE_ABC0000_R#" & vbCrLf & vbCrLf &
            "Allowed codes:" & vbCrLf &
            "BR, DT, AE, FR, EL, ST, SU, WT, MI" & vbCrLf & vbCrLf &
            "Enter the new file name without extension:",
            "GRC27 File Naming Required",
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
            "GRC27_CODE_00000_R# or GRC27_CODE_A0000_R# or GRC27_CODE_AB0000_R# or GRC27_CODE_ABC0000_R#" & vbCrLf & vbCrLf &
            "Allowed codes:" & vbCrLf &
            "BR, DT, AE, FR, EL, ST, SU, WT, MI" & vbCrLf & vbCrLf &
            "Example:" & vbCrLf &
            "GRC27_AE_00001_R1" & ext & vbCrLf &
            "GRC27_AE_A0001_R1" & ext & vbCrLf &
            "GRC27_AE_AB0001_R1" & ext & vbCrLf &
            "GRC27_AE_ABC0001_R1" & ext,
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

            'Save a copy using the new GRC27 name.
            Dim errors As Integer = 0
            Dim warnings As Integer = 0

            Dim saveOk As Boolean = modDoc.Extension.SaveAs3(
            newPath,
            swSaveAsVersion_e.swSaveAsCurrentVersion,
            swSaveAsOptions_e.swSaveAsOptions_Silent,
            Nothing,
            Nothing,
            errors,
            warnings
        )

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
            'Used only for testing/import cleanup. It bypasses the GRC27 naming convention prompt,
            'but still keeps duplicate checks, repo checks, add/commit behavior, etc.
            If shouldIgnoreGrc27NamingConventionForDebug() Then Continue For

            'External/vendor refs already handled during this commit should not be forced through normal naming.
            If shouldSkipNameCheckForPendingExternalRef(docPath) Then Continue For

            'Vendor parts are allowed to keep vendor naming, but only inside Vendor Parts.
            If isVendorPartPath(docPath) Then Continue For

            If Not isValidGrc27FileName(docPath) Then
                Dim result As swMessageBoxResult_e = iSwApp.SendMsgToUser2(
                "This CAD file does not follow the GRC27 naming convention:" & vbCrLf & vbCrLf &
                Path.GetFileName(docPath) & vbCrLf & vbCrLf &
                "Normal CAD must use:" & vbCrLf &
                "GRC27_CODE_00000_R# or GRC27_CODE_A0000_R# or GRC27_CODE_AB0000_R# or GRC27_CODE_ABC0000_R#" & vbCrLf & vbCrLf &
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

    Private Function prepareExternalReferencesForCommitPaths(ByRef commitPaths() As String) As Boolean
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return True

        Dim docsForExternalRefCheck() As ModelDoc2 = getOpenAssemblyDependencyDocsForCommitPaths(commitPaths)
        If docsForExternalRefCheck Is Nothing OrElse docsForExternalRefCheck.Length = 0 Then Return True

        'Only assembly commits can change references. If external/vendor CAD must be copied/relinked,
        'the assembly itself has to be writable/locked, except for a brand-new first commit assembly.
        'Reference changes must be protected by the assembly being committed, not necessarily the active/top-level assembly.
        'This allows a locked subassembly (for example FINAL Swirl Pot Assembly) to process vendor/external children
        'without requiring the top-level car assembly to be checked out.
        If Not targetAssembliesMustBeLockedForReferenceChanges(getOpenAssemblyDocsForCommitPaths(commitPaths)) Then Return False
        If Not prepareExternalReferencesForSvnAction(docsForExternalRefCheck) Then Return False

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

    Private Function relinkExternalRefsToVaultCopies(ByRef externalRefs As List(Of ExternalReferenceInfo)) As Boolean
        If externalRefs Is Nothing Then Return True
        If externalRefs.Count = 0 Then Return True

        Dim activeDoc As ModelDoc2 = iSwApp.ActiveDoc
        If activeDoc Is Nothing Then Return False

        If activeDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then
            Return True
        End If

        Dim assy As AssemblyDoc = CType(activeDoc, AssemblyDoc)
        Dim okAll As Boolean = True
        Dim activeAssemblyPath As String = ""

        Try
            activeAssemblyPath = activeDoc.GetPathName()
        Catch
            activeAssemblyPath = ""
        End Try

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For
            If String.IsNullOrWhiteSpace(refInfo.oldPath) Then Continue For
            If String.IsNullOrWhiteSpace(refInfo.newPath) Then Continue For

            Dim replacedThisRef As Boolean = False

            Try
                'First try the document-level replacement. This catches references that SolidWorks
                'does not expose cleanly through Component2, and helps make the fix persist on save.
                If Not String.IsNullOrWhiteSpace(activeAssemblyPath) Then
                    Try
                        Dim replaceDocOk As Boolean = iSwApp.ReplaceReferencedDocument(activeAssemblyPath, refInfo.oldPath, refInfo.newPath)

                        If replaceDocOk Then
                            replacedThisRef = True
                        End If
                    Catch
                    End Try
                End If

                Dim compsObj As Object = assy.GetComponents(False)

                If compsObj IsNot Nothing Then
                    Dim comps As Object() = CType(compsObj, Object())

                    For Each compObj As Object In comps
                        Dim comp As Component2 = TryCast(compObj, Component2)
                        If comp Is Nothing Then Continue For

                        Dim compPath As String = ""

                        Try
                            compPath = comp.GetPathName()
                        Catch
                            Continue For
                        End Try

                        If String.IsNullOrWhiteSpace(compPath) Then Continue For

                        If pathsAreSame(compPath, refInfo.oldPath) Then
                            'Try both APIs.  Component2.ReplaceReference can report success while the
                            'open assembly still keeps the old path in memory.  The assembly-level
                            'ReplaceComponents command is the no-reload path that usually updates the
                            'active component instance immediately.
                            Try
                                Dim replaceOk As Boolean = comp.ReplaceReference(refInfo.newPath)

                                If replaceOk Then
                                    replacedThisRef = True
                                End If
                            Catch
                            End Try

                            Try
                                If tryAssemblyReplaceComponent(assy, comp, refInfo.newPath) Then
                                    replacedThisRef = True
                                End If
                            Catch
                            End Try
                        End If
                    Next
                End If

                If Not replacedThisRef Then
                    okAll = False
                    iSwApp.SendMsgToUser2(
                    "Failed to relink reference in the open assembly:" & vbCrLf & vbCrLf &
                    refInfo.oldPath & vbCrLf & "→" & vbCrLf & refInfo.newPath,
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
                End If

            Catch ex As Exception
                okAll = False
                iSwApp.SendMsgToUser2(
                "Error while relinking reference in the open assembly:" & vbCrLf & vbCrLf &
                refInfo.oldPath & vbCrLf & "→" & vbCrLf & refInfo.newPath & vbCrLf & vbCrLf &
                ex.Message,
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            End Try
        Next

        Try
            activeDoc.ForceRebuild3(False)
            activeDoc.Save3(swSaveAsOptions_e.swSaveAsOptions_Silent, 0, 0)
        Catch
        End Try

        Return okAll
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

    Public Function prepareExternalReferencesForSvnAction(ByRef modDocArr() As ModelDoc2) As Boolean
        If modDocArr Is Nothing Then Return True
        If modDocArr.Length = 0 Then Return True
        pendingExternalRefCommitPaths.Clear()
        pendingExternalRefSkipNameCheckPaths.Clear()

        Dim externalRefs As List(Of ExternalReferenceInfo) = getExternalCadReferences(modDocArr)

        If externalRefs.Count = 0 Then Return True

        Dim virtualOrTempRefs As New List(Of ExternalReferenceInfo)

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If isSolidWorksTempOrVirtualPath(refInfo.oldPath) Then
                virtualOrTempRefs.Add(refInfo)
            End If
        Next

        If virtualOrTempRefs.Count > 0 Then
            Dim virtualMsg As String =
        "This assembly contains virtual or temporary SolidWorks components." & vbCrLf & vbCrLf &
        "These cannot be automatically added to SVN safely." & vbCrLf & vbCrLf &
        "Virtual/temp files:" & vbCrLf

            For Each refInfo As ExternalReferenceInfo In virtualOrTempRefs
                virtualMsg &= refInfo.fileName & vbCrLf & refInfo.oldPath & vbCrLf & vbCrLf
            Next

            virtualMsg &= "Please save these as external files inside the SVN working copy first." & vbCrLf & vbCrLf &
                  "In SolidWorks: right click the component → Save Part(in External File)." & vbCrLf & vbCrLf &
                  "Save under:" & vbCrLf &
                  myUserControl.localRepoPath.Text

            iSwApp.SendMsgToUser2(
        virtualMsg,
        swMessageBoxIcon_e.swMbStop,
        swMessageBoxBtn_e.swMbOk
    )

            Return False
        End If

        'Second-time vendor behavior:
        'If the assembly still points to Downloads, but a file with the same name already exists
        'under Vendor Parts, silently relink to that existing vendor file instead of asking again.
        Dim refsAlreadyInVendor As New List(Of ExternalReferenceInfo)
        Dim refsStillExternal As New List(Of ExternalReferenceInfo)

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For

            Dim existingVendorPath As String = getExistingVendorPathForFileName(refInfo.fileName)

            If Not String.IsNullOrWhiteSpace(existingVendorPath) AndAlso File.Exists(existingVendorPath) Then
                refInfo.newPath = existingVendorPath
                refsAlreadyInVendor.Add(refInfo)
            Else
                refsStillExternal.Add(refInfo)
            End If
        Next

        If refsAlreadyInVendor.Count > 0 Then
            If Not relinkExternalRefsToVaultCopies(refsAlreadyInVendor) Then Return False
            If Not verifyExternalRefsNowPointToVaultCopies(refsAlreadyInVendor) Then Return False

            For Each refInfo As ExternalReferenceInfo In refsAlreadyInVendor
                If refInfo Is Nothing Then Continue For

                If Not String.IsNullOrWhiteSpace(refInfo.oldPath) Then
                    pendingExternalRefSkipNameCheckPaths.Add(refInfo.oldPath)
                End If

                If Not String.IsNullOrWhiteSpace(refInfo.newPath) Then
                    pendingExternalRefSkipNameCheckPaths.Add(refInfo.newPath)
                End If
            Next
        End If

        externalRefs = refsStillExternal

        If externalRefs.Count = 0 Then
            Dim activeDocExistingVendor As ModelDoc2 = iSwApp.ActiveDoc
            If activeDocExistingVendor IsNot Nothing Then
                Try
                    activeDocExistingVendor.ForceRebuild3(False)
                    activeDocExistingVendor.Save3(swSaveAsOptions_e.swSaveAsOptions_Silent, 0, 0)
                Catch
                End Try
            End If

            Return True
        End If

        Dim msg As String =
        "This assembly references CAD outside the SVN working copy." & vbCrLf & vbCrLf &
        "External files:" & vbCrLf

        For Each refInfo As ExternalReferenceInfo In externalRefs
            msg &= refInfo.fileName & vbCrLf & refInfo.oldPath & vbCrLf & vbCrLf
        Next

        msg &= "Would you like to copy these files into the SVN vault and relink the assembly?"

        Dim response As swMessageBoxResult_e = iSwApp.SendMsgToUser2(
        msg,
        swMessageBoxIcon_e.swMbWarning,
        swMessageBoxBtn_e.swMbYesNo
    )
        If response <> swMessageBoxResult_e.swMbHitYes Then Return False

        Dim vendorResponse As swMessageBoxResult_e = iSwApp.SendMsgToUser2(
    "Is this a vendor part?" & vbCrLf & vbCrLf &
    "Yes = save under Vendor Parts and keep the vendor file name." & vbCrLf &
    "No = this is normal GRC27 CAD. It cannot go under Vendor Parts and the GRC27 naming convention will be enforced.",
    swMessageBoxIcon_e.swMbQuestion,
    swMessageBoxBtn_e.swMbYesNo
)

        Dim isVendorFlow As Boolean = (vendorResponse = swMessageBoxResult_e.swMbHitYes)

        Dim destinationFolder As String = ""

        If isVendorFlow Then
            Dim vendorRoot As String = getVendorPartsRootPath()

            Try
                If Not Directory.Exists(vendorRoot) Then
                    Directory.CreateDirectory(vendorRoot)
                End If
            Catch ex As Exception
                iSwApp.SendMsgToUser2(
            "Could not create Vendor Parts folder:" & vbCrLf & vbCrLf &
            vendorRoot & vbCrLf & vbCrLf &
            ex.Message,
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )
                Return False
            End Try

            Using fbd As New FolderBrowserDialog()
                fbd.Description = "Choose a folder under Vendor Parts for the vendor CAD."
                fbd.SelectedPath = vendorRoot

                If fbd.ShowDialog() <> DialogResult.OK Then Return False

                destinationFolder = fbd.SelectedPath
            End Using

            If Not isPathInsideFolder(destinationFolder, vendorRoot) Then
                iSwApp.SendMsgToUser2(
            "Vendor CAD must be saved under:" & vbCrLf & vbCrLf &
            vendorRoot,
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )
                Return False
            End If

            runSvnProcess(sSVNPath, "add --parents --depth empty """ & vendorRoot & """")
            runSvnProcess(sSVNPath, "add --parents --depth empty """ & destinationFolder & """")

            addPendingDirectoryCommitPathIfNeeded(vendorRoot)
            addPendingDirectoryCommitPathIfNeeded(destinationFolder)
        Else
            destinationFolder = pickVaultDestinationFolder()
            If String.IsNullOrWhiteSpace(destinationFolder) Then Return False

            If isVendorPartPath(destinationFolder) Then
                iSwApp.SendMsgToUser2(
            "Normal GRC27 CAD cannot be saved inside Vendor Parts." & vbCrLf & vbCrLf &
            "Choose Yes on the vendor question if this is a standard/vendor part.",
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )
                Return False
            End If
        End If

        If Not copyExternalRefsToVault(externalRefs, destinationFolder, isVendorFlow) Then Return False
        If Not relinkExternalRefsToVaultCopies(externalRefs) Then Return False
        If Not verifyExternalRefsNowPointToVaultCopies(externalRefs) Then Return False

        Dim copiedPaths As New List(Of String)

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For

            If Not String.IsNullOrWhiteSpace(refInfo.oldPath) Then
                pendingExternalRefSkipNameCheckPaths.Add(refInfo.oldPath)
            End If

            If Not String.IsNullOrWhiteSpace(refInfo.newPath) Then
                copiedPaths.Add(refInfo.newPath)
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

        Dim activeDoc As ModelDoc2 = iSwApp.ActiveDoc
        If activeDoc IsNot Nothing Then
            Try
                activeDoc.ForceRebuild3(False)
                activeDoc.Save3(swSaveAsOptions_e.swSaveAsOptions_Silent, 0, 0)
            Catch
            End Try
        End If

        Return True
    End Function

    Function commitAllowedOnlyIfUpToDate(ByRef modDocArr() As ModelDoc2, Optional bIncludeDependents As Boolean = False) As Boolean
        If modDocArr Is Nothing Then Return False
        If modDocArr.Length = 0 Then Return False

        Dim modDocArrToCheckForLatest As ModelDoc2() = modDocArr

        If bIncludeDependents Then
            Try
                For Each docToCheck As ModelDoc2 In modDocArr
                    If docToCheck Is Nothing Then Continue For

                    If docToCheck.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                        modDocArrToCheckForLatest = myUserControl.getComponentsOfAssemblyOptionalUpdateTree(
                        modDocArr,
                        bResolveLightweight:=True
                    )
                        Exit For
                    End If
                Next
            Catch
                modDocArrToCheckForLatest = modDocArr
            End Try
        End If

        If modDocArrToCheckForLatest Is Nothing Then modDocArrToCheckForLatest = modDocArr

        Dim statusForCommitCheck As SVNStatus = getFileSVNStatus(
        bCheckServer:=True,
        modDocArr:=modDocArrToCheckForLatest
    )

        If statusForCommitCheck Is Nothing Then
            iSwApp.SendMsgToUser2(
            "Commit blocked. Could not verify latest SVN status.",
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )
            Return False
        End If

        Dim outOfDateFiles As String() = statusForCommitCheck.sFilterUpToDate9("*")

        If outOfDateFiles IsNot Nothing Then
            Dim msg As String =
            "Commit blocked." & vbCrLf & vbCrLf &
            "One or more files related to this commit are out of date." & vbCrLf & vbCrLf &
            "To commit assemblies safely, all referenced geometry must be up to date." & vbCrLf & vbCrLf &
            "Out-of-date files:" & vbCrLf &
            stringArrToSingleStringWithNewLines(outOfDateFiles, bTrimFileNames:=True, iLimit:=10) & vbCrLf &
            "Use Get Latest, confirm the assembly geometry, then commit again."

            iSwApp.SendMsgToUser2(
            msg,
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )

            Return False
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

        If save3AndShowErrorMessages(modDocArr) <> swMessageBoxResult_e.swMbHitYes Then Exit Sub

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
                        "This CAD file does not follow the GRC27 naming convention:" & vbCrLf & vbCrLf &
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
                        "This CAD file does not follow the GRC27 naming convention:" & vbCrLf & vbCrLf &
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

        'No extra SVN/server request here:
        'Use the already-loaded tree plus the last Sync cache. If the cache is missing,
        'block and ask for Sync because we cannot prove the children are current.
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
        Dim missingCachePaths As New List(Of String)()

        For Each guardPath As String In guardPaths
            If String.IsNullOrWhiteSpace(guardPath) Then Continue For
            If Not File.Exists(guardPath) Then Continue For
            If Not isCadFilePath(guardPath) Then Continue For

            Dim cached As SVNStatus.filePpty = Nothing
            Dim found As Boolean = False

            Try
                found = tryFindCachedStatusProperty(guardPath, cached)
            Catch
                found = False
            End Try

            If Not found OrElse cached.upToDate9 Is Nothing OrElse String.Equals(cached.upToDate9, "NoUpdate", StringComparison.OrdinalIgnoreCase) Then
                missingCachePaths.Add(guardPath)
                Continue For
            End If

            If cached.upToDate9 = "*" Then
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

        If missingCachePaths.Count > 0 Then
            iSwApp.SendMsgToUser2(
                "Commit blocked." & vbCrLf & vbCrLf &
                "This assembly commit needs a recent Sync cache for the assembly branch before it can verify child freshness." & vbCrLf & vbCrLf &
                "Run Sync on the assembly branch, then commit again." & vbCrLf & vbCrLf &
                "Files without usable cached server status:" & vbCrLf &
                stringArrToSingleStringWithNewLines(missingCachePaths.ToArray(), bTrimFileNames:=True, iLimit:=10),
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk
            )
            Return False
        End If

        Return True
    End Function

    Private Function commitPathsAllowedOnlyIfUpToDate(ByVal commitPaths() As String) As Boolean
        If commitPaths Is Nothing OrElse commitPaths.Length = 0 Then Return False

        'Brand-new CAD files saved inside the SVN working copy have no server revision yet.
        'Do not ask the server whether those paths are up to date; they will be svn add + committed.
        Dim pathsToCheck As New List(Of String)()

        For Each p As String In commitPaths
            If String.IsNullOrWhiteSpace(p) Then Continue For
            If Directory.Exists(p) Then Continue For
            If isFirstCommitCandidatePath(p) Then Continue For
            pathsToCheck.Add(p)
        Next

        If pathsToCheck.Count = 0 Then Return True

        Dim statusForCommitCheck As SVNStatus = getFileSVNStatus(
            bCheckServer:=True,
            modDocArr:=Nothing,
            bUpdateStatusOfAllOpenModels:=False,
            sDirectFilePathArr:=pathsToCheck.ToArray()
        )

        If statusForCommitCheck Is Nothing Then
            iSwApp.SendMsgToUser2(
            "Commit blocked. Could not verify latest SVN status.",
            swMessageBoxIcon_e.swMbStop,
            swMessageBoxBtn_e.swMbOk
        )
            Return False
        End If

        Dim outOfDateFiles As String() = statusForCommitCheck.sFilterUpToDate9("*")

        If outOfDateFiles IsNot Nothing Then
            Dim msg As String =
            "Commit blocked." & vbCrLf & vbCrLf &
            "The selected file is out of date." & vbCrLf & vbCrLf &
            "Use Get Latest, confirm the geometry, then commit again." & vbCrLf & vbCrLf &
            "Out-of-date files:" & vbCrLf &
            stringArrToSingleStringWithNewLines(outOfDateFiles, bTrimFileNames:=True, iLimit:=10)

            iSwApp.SendMsgToUser2(
            msg,
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

            Try
                Dim errors As Integer = 0
                Dim warnings As Integer = 0

                If Not doc.Save3(swSaveAsOptions_e.swSaveAsOptions_Silent, errors, warnings) Then
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
                             success = runTortoiseProcBackgroundNoUi(tortoiseArgs, repoRootForBackground, errorMessage)
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
            iSwApp.SendMsgToUser2("Commit did not complete." & vbCrLf & vbCrLf & errorMessage,
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk)
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

    Private Function runTortoiseProcBackgroundNoUi(ByVal arguments As String,
                                                   ByVal repoRoot As String,
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

            For Each filePath As String In filteredPaths
                If isFirstCommitCandidatePathForAsyncLock(filePath, repoRootPathForBackground, savedPathForBackground) Then
                    Try
                        File.SetAttributes(filePath, File.GetAttributes(filePath) And Not FileAttributes.ReadOnly)
                    Catch
                    End Try

                    firstCommitPaths.Add(filePath)
                Else
                    lockablePaths.Add(filePath)
                End If
            Next

            If lockablePaths.Count = 0 Then
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
            result.LockedPaths = lockablePaths.ToArray()
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

        If save3AndShowErrorMessages(modDocArr) <> swMessageBoxResult_e.swMbHitYes Then Return False

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
