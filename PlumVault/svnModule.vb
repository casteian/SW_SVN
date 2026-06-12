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

Public Module svnModule
    Private Class ExternalReferenceInfo
        Public Property oldPath As String
        Public Property newPath As String
        Public Property fileName As String
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
                    myUserControlPass.onlineCheckBox.Checked = False
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
                    myUserControlPass.onlineCheckBox.Checked = False
                End If
            End If
        End If

        myUserControl = myUserControlPass
        iSwApp = mySwAppPass
        statusOfAllOpenModels = statusOfAllOpenModelsPass

    End Sub
    Public Function updateLockStatusPublic(Optional bRefreshAllTreeViews As Boolean = True) As Boolean
        updateLockStatusPublic = statusOfAllOpenModels.updateStatusLocally(iSwApp)
        If bRefreshAllTreeViews Then myUserControl.refreshAllTreeViewsVariable()
    End Function
    Public Function updateStatusOfAllModelsVariable(Optional bRefreshAllTreeViews As Boolean = False) As Boolean
        Dim bWhatToReturn As Boolean = False

        bWhatToReturn = statusOfAllOpenModels.updateFromSvnServer(bRefreshAllTreeViews)

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
        If Not myUserControl.onlineCheckBox.Checked Then Return False

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
        If Not myUserControl.onlineCheckBox.Checked Then Return False

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

                If isDirty Then
                    openPaths.Add("[UNSAVED_SOLIDWORKS_CHANGES] " & title)
                    Continue For
                End If

                If String.IsNullOrWhiteSpace(docPath) Then
                    openPaths.Add("[UNSAVED_NEW_FILE] " & title)
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
        If Not myUserControl.onlineCheckBox.Checked Then Return False

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

            If isDirty Then
                openPaths.Add("[UNSAVED_SOLIDWORKS_CHANGES] " & title)

            ElseIf String.IsNullOrWhiteSpace(docPath) Then
                openPaths.Add("[UNSAVED_NEW_FILE] " & title)

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

    Private Function showUnsafeClosePrompt(ByVal unsafeMsg As String) As Boolean
        Dim response As Integer = iSwApp.SendMsgToUser2(
            "One or more open CAD files are not safe to close yet." & vbCrLf & vbCrLf &
            unsafeMsg & vbCrLf &
            "Choose an action:" & vbCrLf &
            "Yes = I want to go back to push/revert" & vbCrLf &
            "No = I want to close SolidWorks anyway",
            swMessageBoxIcon_e.swMbWarning,
            swMessageBoxBtn_e.swMbYesNo
        )

        If response = swMessageBoxResult_e.swMbHitYes Then
            iSwApp.SendMsgToUser2(
                "Close cancelled." & vbCrLf & vbCrLf &
                "Commit/push your files, or use Unlock && Revert to go back to the original version.",
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
                "SolidWorks has unsaved changes." & vbCrLf & vbCrLf
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

    Public Function getFileSVNStatus(ByVal bCheckServer As Boolean,
                              Optional ByRef modDocArr() As ModelDoc2 = Nothing,
                              Optional ByRef bUpdateStatusOfAllOpenModels As Boolean = True,
                              Optional ByVal iRecursiveLevel As Integer = 0) As SVNStatus
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
        If Not IsNothing(modDocArr) Then sModDocPathArr = getFilePathsFromModDocArr(modDocArr)

        If bCheckServer Or (IsNothing(modDocArr)) Then bCheckAllFiles = True

        If bCheckAllFiles Then
            'Have to just check the whole file path, because otherwise, svn sends a separate server request for ech individual path sent
            'if you  format it, like ""C:/file1" "C:/file2"" (including the quotes, starting with double start and end) then it will only send one server request, however, the server has trouble finding the file names... 
            statusArguments = "status -v" & If(bCheckServer, "u", "") & " --non-interactive """ & myUserControl.localRepoPath.Text.TrimEnd("\\") & """" 'sFilePathCat 
            sPropArr = svnPropget("""" & myUserControl.localRepoPath.Text.TrimEnd("\\") & """")
        Else

            statusArguments = "status -v --non-interactive " & formatFilePathArrForProc(sModDocPathArr, sDelimiter:=""" """) & """" 'sFilePathCat 
            sPropArr = svnPropget(formatFilePathArrForProc(sModDocPathArr, sDelimiter:=""" """) & """")
        End If


        'iSwApp.SendMsgToUser(sSVNPath)
        statusProcessOutput = runSvnProcess(sSVNPath, statusArguments)
        If bCheckServer Then
            lockOwnersByPath = New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)

            'Speed fix:
            'For targeted status checks, do not scan the whole repo just to get lock owners.
            'Normal Get Locks usually passes one or a few files, so query only those paths.
            If sModDocPathArr IsNot Nothing AndAlso sModDocPathArr.Length > 0 AndAlso sModDocPathArr.Length <= 50 Then
                For Each lockPath As String In sModDocPathArr
                    If String.IsNullOrWhiteSpace(lockPath) Then Continue For

                    Try
                        Dim ownersForPath As Dictionary(Of String, String) = getSvnLockOwnersByPath(lockPath)

                        For Each kvp As KeyValuePair(Of String, String) In ownersForPath
                            lockOwnersByPath(kvp.Key) = kvp.Value
                        Next
                    Catch
                    End Try
                Next
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
                    Return getFileSVNStatus(bCheckServer, modDocArr, bUpdateStatusOfAllOpenModels, iRecursiveLevel:=(iRecursiveLevel + 1))
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
                            Return getFileSVNStatus(bCheckServer, modDocArr, bUpdateStatusOfAllOpenModels, iRecursiveLevel:=(iRecursiveLevel + 1))
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
            If sOutputLines(0).Substring(0, 23) = "Status against revision" Then
                iSwApp.SendMsgToUser("Status Returned from SVN Server with No Items") 'If you change the string, change it other places in the code too!
                Return svnStatusOfPassedModDoc
            ElseIf (sOutputLines.Length = 1) Then
                'If we are checking the server, we should expect a line 2. If its not there then theres an error.
                iSwApp.SendMsgToUser("Error: Incomplete SVN Status. Could not Read Line 2. Line 1:" & sOutputLines(0))
                Return svnStatusOfPassedModDoc
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
        End If

        If IsNothing(modDocArr) Then
            'iSwApp.SendMsgToUser("Unknown error attempting to retrieve SVN Status from server")
            Return entireSVNStatus
        Else
            Return svnStatusOfPassedModDoc
        End If

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

        updateLockStatusPublic(bRefreshAllTreeViews:=True)
        myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
        statusOfAllOpenModels.setReadWriteFromLockStatus()

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

        myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
        updateLockStatusPublic(bRefreshAllTreeViews:=True)
        statusOfAllOpenModels.setReadWriteFromLockStatus()
        myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)

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
                    myUserControl.refreshAllTreeViewsVariable()
                    myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
                End If
            Catch
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

    Private Function copyExternalRefsToVault(ByRef externalRefs As List(Of ExternalReferenceInfo), destinationFolder As String, Optional isVendorFlow As Boolean = False) As Boolean
        If externalRefs Is Nothing Then Return True
        If externalRefs.Count = 0 Then Return True
        If String.IsNullOrWhiteSpace(destinationFolder) Then Return False

        For Each refInfo As ExternalReferenceInfo In externalRefs
            Dim finalFileName As String = refInfo.fileName

            If Not isVendorFlow Then
                If Not isValidGrc27FileName(finalFileName) Then
                    finalFileName = promptForValidGrc27FileName(refInfo.oldPath)

                    If String.IsNullOrWhiteSpace(finalFileName) Then Return False
                End If
            End If

            Dim destPath As String = Path.Combine(destinationFolder, finalFileName)

            If File.Exists(destPath) Then
                iSwApp.SendMsgToUser2(
                "A file with this name already exists in the selected SVN folder:" & vbCrLf & vbCrLf &
                destPath & vbCrLf & vbCrLf &
                "Please choose a different GRC27 file name.",
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

        For Each refInfo As ExternalReferenceInfo In externalRefs
            If refInfo Is Nothing Then Continue For
            If String.IsNullOrWhiteSpace(refInfo.oldPath) Then Continue For
            If String.IsNullOrWhiteSpace(refInfo.newPath) Then Continue For

            Dim replacedThisRef As Boolean = False

            Try
                Dim compsObj As Object = assy.GetComponents(False)

                If compsObj Is Nothing Then
                    okAll = False
                    Continue For
                End If

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

                    If String.Equals(
                    Path.GetFullPath(compPath),
                    Path.GetFullPath(refInfo.oldPath),
                    StringComparison.OrdinalIgnoreCase
                ) Then
                        Dim replaceOk As Boolean = comp.ReplaceReference(refInfo.newPath)

                        If replaceOk Then
                            replacedThisRef = True
                        End If
                    End If
                Next

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
    "Are these vendor/standard parts?" & vbCrLf & vbCrLf &
    "Yes = save under Vendor Parts and keep vendor file names." & vbCrLf &
    "No = normal GRC27 CAD; naming convention will be enforced.",
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
        pendingExternalRefSkipNameCheckPaths.Clear()

        If copiedPaths.Count > 0 Then
            For Each copiedPath As String In copiedPaths
                runSvnProcess(sSVNPath, "add --parents """ & copiedPath & """")
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

    Function commitAllowedOnlyIfUpToDate(ByRef modDocArr() As ModelDoc2) As Boolean
        If modDocArr Is Nothing Then Return False
        If modDocArr.Length = 0 Then Return False

        Dim modDocArrToCheckForLatest As ModelDoc2() = modDocArr

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

    Sub unlockDocs(Optional ByRef modDocArr() As ModelDoc2 = Nothing)
        Dim bSuccess As Boolean
        Dim Status As SVNStatus

        If Not userAcceptsLossOfChanges(modDocArr, "Release Locks, and revert changes to vault version?") Then Exit Sub
        saveAllOpenFiles(bShowError:=True)

        If IsNothing(modDocArr) Then
            If Not verifyLocalRepoPath() Then Exit Sub
            bSuccess = runTortoiseProcexeWithMonitor("/command:unlock /path:""" & myUserControl.localRepoPath.Text.TrimEnd("\\") & """ /closeonend:3")
        ElseIf UBound(modDocArr) = -1 Then
            Exit Sub
        Else
            Status = getFileSVNStatus(bCheckServer:=True, modDocArr)
            If IsNothing(Status) Then Exit Sub

            Dim sFilePaths() As String = sGetFileNames(Status.statusFilter(sFiltLock6:="K"))

            If IsNothing(sFilePaths) Then
                iSwApp.SendMsgToUser2("No Selected Items were locked", swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOk)
                Exit Sub
            End If

            bSuccess = runTortoiseProcexeWithMonitor("/command:unlock /path:" &
                                             formatFilePathArrForProc(
                                                sFilePaths) & " /closeonend:3")

            'for each moddoc in moddocarr
            '    'manually reattach to file system
            '    moddoc.reloadorreplace(readonly:=true, replacefilename:=false, discardchanges:=false)
            'next

        End If

        If Not bSuccess Then iSwApp.SendMsgToUserv("Releasing Locks Failed.")

        myGetLatestOrRevert(modDocArr, getLatestType.revert)

    End Sub
    Sub tortCommitDocs(ByRef modDocArr() As ModelDoc2, Optional sCommitMessage As String = "")
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

        If Not activeAssemblyMustBeLockedForReferenceChanges() Then Exit Sub

        If Not prepareExternalReferencesForSvnAction(docsForExternalRefCheck) Then Exit Sub

        'After external/vendor CAD is copied and relinked, rebuild the commit array.
        'This makes sure newly copied SVN files are included in the commit.
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

        If Not validateCadNamesBeforeCommit(modDocArr) Then Exit Sub

        Dim docsForDuplicateCheck As ModelDoc2() = modDocArr

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

        If Not validateNoDuplicateCadFileNames(docsForDuplicateCheck) Then Exit Sub
        If Not commitAllowedOnlyIfUpToDate(modDocArr) Then Exit Sub

        'Filter out read-only files
        For i = 0 To UBound(modDocArr)
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
        sModDocPathArr = getFilePathsFromModDocArr(modDocArr)

        If pendingExternalRefCommitPaths IsNot Nothing AndAlso pendingExternalRefCommitPaths.Count > 0 Then
            Dim mergedCommitPaths As New List(Of String)

            If sModDocPathArr IsNot Nothing Then
                mergedCommitPaths.AddRange(sModDocPathArr)
            End If

            For Each pendingPath As String In pendingExternalRefCommitPaths
                If String.IsNullOrWhiteSpace(pendingPath) Then Continue For
                If Not File.Exists(pendingPath) Then Continue For

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

            sModDocPathArr = mergedCommitPaths.ToArray()
        End If

        runSvnByArgs(sModDocPathArr, "add", bEach:=True)  'adds any not added. 
        svnPropset(sModDocPathArr, "addin:release_state", "||EDIT||")

        If save3AndShowErrorMessages(modDocArr) <> swMessageBoxResult_e.swMbHitYes Then Exit Sub

        bSuccess = runTortoiseProcexeWithMonitor("/command:commit /path:" &
                                         formatFilePathArrForProc(
                                            sModDocPathArr) & " /logmsg:""" & sCommitMessage & """" & " /closeonend:3")
        If Not bSuccess Then iSwApp.SendMsgToUser("Tortoise App Failed.") : Exit Sub

        'If Filter() files -> any In list have 'unknown' status before, then call server for updates.

        bSuccess = updateStatusOfAllModelsVariable(bRefreshAllTreeViews:=True)
        If Not bSuccess Then Exit Sub
        myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
        statusOfAllOpenModels.setReadWriteFromLockStatus()
    End Sub
    Public Sub externalSetReadWriteFromLockStatus()
        statusOfAllOpenModels.setReadWriteFromLockStatus()
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

        bSuccess = updateLockStatusPublic(bRefreshAllTreeViews:=True)
        If Not bSuccess Then Exit Sub
        myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
        statusOfAllOpenModels.setReadWriteFromLockStatus()
        keepNewUncommittedCadFilesWritable()

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
        'Dim bSuccessStatus As Boolean
        Dim bSuccessCleanup As Boolean

        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc

        If modDoc Is Nothing Then
        Else
            iSwApp.SendMsgToUser("This unfortunately can't be run with SolidWorks Files open. Close all open files, then in Windows Explorer, right click > TortoiseSVN > Cleanup")
            Exit Sub
        End If

        If Not verifyLocalRepoPath(bCheckServer:=False) Then Exit Sub

        If Not iSwApp.SendMsgToUser2("Unsaved changes will be discarded. Continue?",
                                swMessageBoxIcon_e.swMbWarning,
                                swMessageBoxBtn_e.swMbYesNo) = swMessageBoxResult_e.swMbHitYes Then
            Exit Sub
        End If

        iSwApp.SendMsgToUser2("Running cleanup from the Add-in has a poor success rate due to Windows only allowing " &
                              "a single program to edit a file at once. If this fails, close SolidWorks, 
                              and run cleanup from tortoiseSVN in Windows Explorer." & vbCrLf & vbCrLf &
                              "If you need to save files, but are unable without a lock, In Windows Explorer, Right click > Properties, and un-check Read-Only. " &
                              "This may create conflicts if another user is working on the file, so is typically not recommended.",
                                swMessageBoxIcon_e.swMbWarning,
                                swMessageBoxBtn_e.swMbOk)

        'bSuccessStatus = updateStatusOfAllModelsVariable(bRefreshAllTreeViews:=True)

        bSuccessCleanup = runTortoiseProcexeWithMonitor("/command:cleanup /cleanup /path:""" & myUserControl.localRepoPath.Text & """")

        If Not bSuccessCleanup Then
            iSwApp.SendMsgToUser("Cleanup Failed. This is often because the SVN server is attempting " &
                    "to open a file that SolidWorks is currently accessing. This occurs even when the file is read only. " &
                    "Try closing all open files and trying again. Or close SolidWorks and use ToroiseSVN to clean up. ")
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

    Public Sub getLocksOfDocs(ByRef modDocArr() As ModelDoc2, Optional bBreakLocks As Boolean = False, Optional bUseTortoise As Boolean = False, Optional sMessage As String = "")
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc()
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Active Document not found") : Exit Sub

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
        Dim bEachSuccess() As Boolean

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

        'Speed fix:
        'Do not rebuild every open tree after a lock. Rebuild only the active tree.
        bSuccess = updateLockStatusPublic(bRefreshAllTreeViews:=False)
        If Not bSuccess Then Exit Sub

        Try
            myUserControl.refreshCurrentTreeViewOnly()
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

        If Not myUserControl.onlineCheckBox.Checked Then Return False

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

    Sub myGetLatestOrRevert(Optional ByRef modDocArr As ModelDoc2() = Nothing,
                        Optional ByRef myGetType As getLatestType = getLatestType.update,
                            Optional ByRef bVerbose As Boolean = False)
        'Dim modDocTemp As ModelDoc2
        Dim i As Integer
        Dim j As Integer = 0
        Dim status As SVNStatus
        Dim bSuccess As Boolean
        Dim sw As New Stopwatch

        If ((myGetType = getLatestType.both) Or (myGetType = getLatestType.update)) Then
            If Not userAcceptsLossOfChanges(modDocArr, "Update the following Files to latest vault version?") Then Exit Sub
        End If

        'Update to use getFileSVNStatus so its only the
        If IsNothing(modDocArr) Then
            updateStatusOfAllModelsVariable()
            status = statusOfAllOpenModels
        Else
            status = getFileSVNStatus(bCheckServer:=True, modDocArr)
        End If
        If IsNothing(status) Then Exit Sub

        Dim sFileList(UBound(status.fp)) As String

        For i = 0 To UBound(status.fp)
            'modDocTemp = iSwApp.GetOpenDocumentByName(mySVNStatus.fp(i).filename)
            If status.fp(i).modDoc Is Nothing Then Continue For 'modDocTemp
            'modDocArr(m) = modDocTemp : m += 1
            If (status.fp(i).upToDate9 = "*") And ((myGetType = getLatestType.update) Or (myGetType = getLatestType.both)) Then
                ' File is out of date
                'sFileListToUpdate(j) = mySVNStatus.fp(i).filename : 
                status.fp(i).revertUpdate = getLatestType.update
                sFileList(j) = status.fp(i).filename
                'status.fp(i).bReconnect = True 'Should be set in releaseFileSystemAccessToRevertOrUpdateModels
                j += 1
            ElseIf (status.fp(i).addDelChg1 = "M") And (myGetType = getLatestType.revert) And (statusOfAllOpenModels.fp(i).lock6 <> "K") Then
                ' Local copy has been modified
                ' Note out of date files will go into FileListToUpdate and will be skipped over by revert.
                'sFileListToRevert(n) = mySVNStatus.fp(i).filename : n += 1
                status.fp(i).revertUpdate = getLatestType.revert
                sFileList(j) = status.fp(i).filename
                j += 1
            End If
        Next

        status.setReadWriteFromLockStatus()

        If j = 0 Then
            If bVerbose Then iSwApp.SendMsgToUser("All Files Checked Are Up to Date!")
            If updateStatusOfAllModelsVariable(bRefreshAllTreeViews:=True) Then
                myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
            End If
            Exit Sub
        End If

        sw.Start()
        System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.WaitCursor


        Dim indexOfFilestoRevert As Integer() = status.indexFilterGetLatestType(getLatestType.revert, bIgnoreUpdate:=False)
        status.releaseFileSystemAccessToRevertOrUpdateModels(iSwApp, indexOfFilestoRevert) 'This should be setting bReconnect to 
        sFileList = status.sFilterGetLatestType(getLatestType.revert, bIgnoreUpdate:=False)
        If (Not sFileList Is Nothing) And ((myGetType = getLatestType.revert) Or (myGetType = getLatestType.both)) Then
            bSuccess = runTortoiseProcexeWithMonitor("/command:revert /path:" &
                                          formatFilePathArrForProc(sFileList) & " /closeonend:3")
            If Not bSuccess Then iSwApp.SendMsgToUserv("Revert Files Failed.")
        End If

        Dim indexOfFilestoUpdate As Integer() = status.indexFilterGetLatestType(getLatestType.update, bIgnoreUpdate:=False)
        status.releaseFileSystemAccessToRevertOrUpdateModels(iSwApp, indexOfFilestoUpdate)
        sFileList = status.sFilterGetLatestType(getLatestType.update, bIgnoreUpdate:=False)
        If (Not sFileList Is Nothing) And ((myGetType = getLatestType.update) Or (myGetType = getLatestType.both)) Then
            bSuccess = runTortoiseProcexeWithMonitor("/command:update /path:" & formatFilePathArrForProc(sFileList) & " /closeonend:3")
            If Not bSuccess Then iSwApp.SendMsgToUserv("Updating Files Failed.")
        End If

        'What happens if user cancels any items in tortoise window??? Or if tortoiseSVN fails, such as needing clean up

        status.reattachDocsToFileSystem(indexOfFilestoRevert, iSwApp)
        status.reattachDocsToFileSystem(indexOfFilestoUpdate, iSwApp)

        If updateStatusOfAllModelsVariable(bRefreshAllTreeViews:=True) Then
            myUserControl.switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
        End If

        System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Default
        sw.Stop()
        Debug.WriteLine("myGetLatestOrRevert Time Taken: " + sw.Elapsed.TotalMilliseconds.ToString("#,##0.00 'milliseconds'"))
    End Sub
    Public Enum getLatestType
        undefined = -1
        none = 0
        revert = 1
        update = 2
        both = 3
    End Enum
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
        myUserControl.onlineCheckBox.Checked = False

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
        Dim i As Integer
        Dim bSuccess As Boolean
        Dim output As SVNStatus = New SVNStatus()

        If IsNothing(statusOfAllOpenModels) Then
            bSuccess = updateStatusOfAllModelsVariable()
            If Not bSuccess Then Return Nothing
        End If

        ReDim output.fp(0)

        For i = 0 To UBound(statusOfAllOpenModels.fp)
            If (Strings.InStr(statusOfAllOpenModels.fp(i).filename, sFileName, CompareMethod.Text) <> 0) Then
                Exit For
            End If
        Next
        If i = (UBound(statusOfAllOpenModels.fp) + 1) Then Return Nothing
        output.fp(0) = statusOfAllOpenModels.fp(i)
        Return output
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
