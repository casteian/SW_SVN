
Imports System.Runtime.InteropServices

Imports SolidWorks.Interop.sldworks
Imports SolidWorks.Interop.swconst

Imports System.Collections.Generic
Imports System.Windows.Forms
Imports System.Drawing
Imports System.IO
Imports System.CodeDom.Compiler
Imports System.Windows.Forms.Layout
Imports PlumVault.SVNStatus
Imports System.Linq
Imports System.Xml
Imports System.Security.Policy
'Imports System.Configuration

<ProgId("SVN_AddIn")>
Public Class UserControl1

    Public WithEvents iSwApp As SolidWorks.Interop.sldworks.SldWorks

    'Dim userAddin As SwAddin = New SwAddin() 'couldn't get access to swapp in here!

    'Public Const localRepoPath.text As String = "E:\SolidworksBackup\svn"
    'Public Const localRepoPath.text As String = "C:\Users\benne\Documents\SVN\cad1"

    Public statusOfAllOpenModels As SVNStatus = New SVNStatus
    Public allOpenDocs As ModelDoc2()
    Public savedPATH As String = Nothing 'Fixes issue #47: SolidWorks Simulation breaking svn+ssh, so unable to contact repo 

    'Dim modelDocList As New List(Of ModelDoc2)()
    Public allTreeViews As TreeView() = {New TreeView}
    'Public allTreeViews As New List(Of TreeView())

    Private WithEvents liveChangeCheckTimer As System.Windows.Forms.Timer
    Private refreshTreeNeedsUpdate As Boolean = False
    Private normalRefreshTreeBackColor As Color
    Private lastLiveCheckedActivePath As String = ""

    Private Sub setRefreshTreeButtonNormal()
        refreshTreeNeedsUpdate = False

        butRefresh.Text = "Refresh Tree"
        butRefresh.Size = New Size(220, 32)
        butRefresh.Font = New Font(butRefresh.Font.FontFamily, 10.0!, FontStyle.Bold)
        butRefresh.BackColor = normalRefreshTreeBackColor
        butRefresh.UseVisualStyleBackColor = True
    End Sub

    Private Sub setRefreshTreeButtonUpdateNeeded()
        refreshTreeNeedsUpdate = True

        butRefresh.Text = "Changes made - Update now"
        butRefresh.Size = New Size(220, 32)
        butRefresh.Font = New Font(butRefresh.Font.FontFamily, 9.0!, FontStyle.Bold)
        butRefresh.BackColor = Color.LightGreen
        butRefresh.UseVisualStyleBackColor = False
    End Sub

    Private Function getActiveAssemblyTreeForLiveCheck() As ModelDoc2()
        Dim activeModDoc As ModelDoc2 = iSwApp.ActiveDoc

        If activeModDoc Is Nothing Then Return Nothing

        Try
            If String.IsNullOrWhiteSpace(activeModDoc.GetPathName()) Then Return Nothing

            'Speed fix:
            'Do not walk the whole assembly every 30 seconds.
            'The timer/live check only needs the active document so SolidWorks stays snappy.
            Return New ModelDoc2() {activeModDoc}
        Catch
            Return Nothing
        End Try
    End Function

    Private Sub UserControl1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Dim docMenu As ContextMenuStrip
        Dim myrefreshItem, myCollapseItem As ToolStripMenuItem
        savedPATH = System.Environment.GetEnvironmentVariable("PATH") 'Fixes issue #47: SolidWorks Simulation breaking svn+ssh, so unable to contact repo 

        docMenu = New ContextMenuStrip()
        myrefreshItem = New ToolStripMenuItem("Refresh", My.Resources.PlumVault_128, AddressOf RefreshToolStripMenuItemEventHandler)
        myCollapseItem = New ToolStripMenuItem("Collapse", My.Resources.PlumVault_128, AddressOf collapseTreeViewHandler2)

        docMenu.Items.AddRange({myrefreshItem, myCollapseItem})

        Me.ContextMenuStrip = docMenu

        normalRefreshTreeBackColor = butRefresh.BackColor
        setRefreshTreeButtonNormal()

        liveChangeCheckTimer = New System.Windows.Forms.Timer()
        liveChangeCheckTimer.Interval = 30000 '30 seconds
        liveChangeCheckTimer.Start()


    End Sub

    Private Sub liveChangeCheckTimer_Tick(sender As Object, e As EventArgs) Handles liveChangeCheckTimer.Tick
        'Speed fix:
        'Do NOT run SVN server checks on a timer.
        'The old live check could call svn status -u against the repo and make SOLIDWORKS feel frozen.
        Dim activeModDoc As ModelDoc2 = iSwApp.ActiveDoc
        If activeModDoc Is Nothing Then Exit Sub

        Dim activePath As String = ""

        Try
            activePath = activeModDoc.GetPathName()
        Catch
            activePath = ""
        End Try

        If Not String.Equals(activePath, lastLiveCheckedActivePath, StringComparison.OrdinalIgnoreCase) Then
            lastLiveCheckedActivePath = activePath
            setRefreshTreeButtonNormal()
        End If
    End Sub

    Friend Sub myInitialize(ByRef swAppin As SldWorks)
        'Allows for swApp to be passed into this class.
        iSwApp = swAppin

        initializeSwModelFunctions(iSwApp)
        svnModuleInitialize(iSwApp, Me, statusOfAllOpenModels)

        localRepoPath.Text = My.Settings.localRepoPath
        versionLabel.Text = "Version: 2026.02.12"

        ToolStripSplitButFolder.DropDown.AutoClose = True

        If iSwApp.GetDocumentCount = 0 Then

            If verifyLocalRepoPath(bInteractive:=True, bCheckLocalFolder:=True, bCheckServer:=False) Then
                If iSwApp.SendMsgToUser2("Would you like to get latest CAD files from the SVN Server? (SVN Update)", swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNo) = swMessageBoxResult_e.swMbHitYes Then
                    runTortoiseProcexeWithMonitor("/command:update /path:""" & My.Settings.localRepoPath & """ /closeonend:3")
                End If
            End If
        Else
            refreshAddIn(bsaveLocalRepoPathSettings:=False)
        End If

    End Sub
    Friend Sub beforeClose()
        saveLocalRepoPathSettings()
    End Sub

    ' ### Get Locks
    Private Sub ToolStripDropDownGetLocks_ButtonClick(sender As Object, e As EventArgs) Handles ToolStripDropDownButGetLocks.ButtonClick
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Error: Active Document not found") : Exit Sub

        getLocksOfDocs(GetSelectedModDocList(iSwApp))
        updateStatusStrip()
    End Sub

    Private Sub dropDownGetLocksWithDependents_Click(sender As Object, e As EventArgs) Handles dropDownGetLocksWithDependents.Click
        Dim selectedDocs() As ModelDoc2
        Dim modDocArr() As ModelDoc2

        selectedDocs = GetSelectedModDocList(iSwApp)

        If selectedDocs Is Nothing Then
            iSwApp.SendMsgToUser("Error: Active Document not found")
            Exit Sub
        End If

        modDocArr = getComponentsOfAssemblyOptionalUpdateTree(selectedDocs, bResolveLightweight:=True)

        If modDocArr Is Nothing Then
            iSwApp.SendMsgToUser("Error: Active Document not found")
            Exit Sub
        End If

        If Not svnModule.prepareExternalReferencesForSvnAction(modDocArr) Then Exit Sub

        'Important: rebuild the dependency list after external references are relinked.
        selectedDocs = GetSelectedModDocList(iSwApp)
        modDocArr = getComponentsOfAssemblyOptionalUpdateTree(selectedDocs, bResolveLightweight:=True)

        If modDocArr Is Nothing Then
            iSwApp.SendMsgToUser("Error: Could not rebuild dependents after relinking external references.")
            Exit Sub
        End If

        getLocksOfDocs(modDocArr)
        updateStatusStrip()
    End Sub

    ' ### Commit
    Private Sub ToolStripDropDownButCommit_ButtonClick(sender As Object, e As EventArgs) Handles ToolStripDropDownButCommit.ButtonClick
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Error: Active Document not found") : Exit Sub
        tortCommitDocs(GetSelectedModDocList(iSwApp))
        updateStatusStrip()
    End Sub

    Private Sub dropDownCommitWithDependents_Click(sender As Object, e As EventArgs) Handles dropDownCommitWithDependents.Click
        Dim selectedDocs() As ModelDoc2
        Dim modDocArr() As ModelDoc2

        selectedDocs = GetSelectedModDocList(iSwApp)

        If selectedDocs Is Nothing Then
            iSwApp.SendMsgToUser("Error: Active Document not found")
            Exit Sub
        End If

        modDocArr = getComponentsOfAssemblyOptionalUpdateTree(selectedDocs, bResolveLightweight:=True)

        If modDocArr Is Nothing Then
            iSwApp.SendMsgToUser("Error: Active Document not found")
            Exit Sub
        End If

        If Not svnModule.prepareExternalReferencesForSvnAction(modDocArr) Then Exit Sub

        'Important: rebuild the dependency list after external references are relinked.
        selectedDocs = GetSelectedModDocList(iSwApp)
        modDocArr = getComponentsOfAssemblyOptionalUpdateTree(selectedDocs, bResolveLightweight:=True)

        If modDocArr Is Nothing Then
            iSwApp.SendMsgToUser("Error: Could not rebuild dependents after relinking external references.")
            Exit Sub
        End If

        tortCommitDocs(modDocArr)
        updateStatusStrip()
    End Sub
    Private Sub dropDownCommitAll_Click(sender As Object, e As EventArgs) Handles dropDownCommitAll.Click
        myCommitAll()
        updateStatusStrip()
    End Sub

    ' ### Unlock
    Private Sub ToolStripDropDownButUnlock_ButtonClick(sender As Object, e As EventArgs) Handles ToolStripDropDownButUnlock.ButtonClick
        unlockDocs(GetSelectedModDocList(iSwApp))
        updateStatusStrip()
    End Sub
    Private Sub dropDownUnlockWithDependents_Click(sender As Object, e As EventArgs) Handles dropDownUnlockWithDependents.Click
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Error: Active Document not found") : Exit Sub
        unlockDocs(getComponentsOfAssemblyOptionalUpdateTree(GetSelectedModDocList(iSwApp)))
        updateStatusStrip()
    End Sub
    Private Sub dropDownUnlockAll_Click(sender As Object, e As EventArgs) Handles dropDownUnlockAll.Click
        unlockDocs()
        updateStatusStrip()
    End Sub

    ' ### Get Latest
    Private Sub ToolStripDropDownButGetLatest_ButtonClick(sender As Object, e As EventArgs) Handles ToolStripDropDownButGetLatest.ButtonClick
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Error: Active Document not found") : Exit Sub

        myGetLatestOrRevert(GetSelectedModDocList(iSwApp),, bVerbose:=True)
        'myGetLatestOpenOnly()
        updateStatusStrip()
    End Sub
    Private Sub dropDownGetLatestAllOpenFiles_Click(sender As Object, e As EventArgs) Handles dropDownGetLatestAllOpenFiles.Click
        Dim modDocArr() As ModelDoc2 = getAllOpenDocs(bMustBeVisible:=False)

        saveAllOpenFiles(bShowError:=True)

        myGetLatestOrRevert(modDocArr,, bVerbose:=True)
        'myGetLatestOpenOnly()
        updateStatusStrip()
    End Sub
    Private Sub dropDownGetLatestAll_Click(sender As Object, e As EventArgs) Handles dropDownGetLatestAll.Click
        saveAllOpenFiles(bShowError:=True)
        myGetLatestOrRevert(,, bVerbose:=True)
        updateStatusStrip()
        'myGetLatestAllRepo()
    End Sub
    Private Sub butFindComponent_Click(sender As Object, e As EventArgs) Handles butFindComponent.Click
        Dim modDocArr As ModelDoc() = GetSelectedModDocList(iSwApp)

    End Sub

    ' ### Refresh
    Private Sub RefreshToolStripMenuItemEventHandler(sender As Object, e As EventArgs)
        performLightweightRefresh()
    End Sub
    Private Sub collapseTreeViewHandler2(sender As Object, e As EventArgs)
        TreeView1.CollapseAll()
        TreeView1.Nodes(0).Expand()
    End Sub

    Private Sub butRefresh_Click(sender As Object, e As EventArgs) Handles butRefresh.Click
        performLightweightRefresh()
    End Sub

    Private Sub performLightweightRefresh()
        'Speed fix:
        'Refresh Tree should refresh status/tree only.
        'It should NOT run Get Latest / SVN update, and it should NOT call refreshAddIn() again.

        If iSwApp.GetDocumentCount() = 0 Then
            If Me.onlineCheckBox.Checked Then
                If verifyLocalRepoPath(, bCheckLocalFolder:=True, bCheckServer:=True) Then
                    iSwApp.SendMsgToUser2("Couldn't find any open files to refresh the status for, but you are successfully communicating with SVN server. This button doesn't do anything if you don't have files open.",
                        swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)
                Else
                    iSwApp.SendMsgToUser2("Unable to contact a server and verify that your local path is a synced SVN folder.",
                        swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)
                End If
            Else
                verifyLocalRepoPath(, bCheckLocalFolder:=True, bCheckServer:=False)
                iSwApp.SendMsgToUser2("Couldn't find any open files to refresh the status for. Your 'online' checkbox is unchecked, so contact to the server was not attempted.",
                        swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)
            End If
            Exit Sub
        End If

        Try
            System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.WaitCursor

            statusOfAllOpenModels = New SVNStatus

            'Local-only refresh.
            'Do NOT call updateStatusOfAllModelsVariable here because that can hit the SVN server/repo
            'and rebuild trees. Server update belongs under Get Latest, not Refresh Tree.
            Try
                updateLockStatusPublic(bRefreshAllTreeViews:=False)
            Catch
            End Try

            refreshCurrentTreeViewOnly()

            Try
                statusOfAllOpenModels.setReadWriteFromLockStatus()
            Catch
            End Try

            Try
                externalSetReadWriteFromLockStatus()
            Catch
            End Try

            setRefreshTreeButtonNormal()

        Finally
            System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Default
        End Try
    End Sub

    ' ### Clean Up
    Private Sub butCleanup_Click(sender As Object, e As EventArgs) Handles butCleanup.Click
        iSwApp.SendMsgToUser("This unfortunately can't be run with SolidWorks Files open. Close all open files, then in Windows Explorer, right click > TortoiseSVN > Cleanup")
        'myCleanup()
    End Sub

    ' ### Folder
    Private Sub butPickFolder_Click(sender As Object, e As EventArgs) Handles butPickFolder.Click
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        Dim sSuggestedPath As String
        Dim eResponse As swMessageBoxResult_e

        If Not (modDoc Is Nothing) Then
            sSuggestedPath = modDoc.GetPathName
            Dim currentDir As DirectoryInfo = New FileInfo(sSuggestedPath).Directory

            If (ModifierKeys And Keys.Shift) = Keys.Shift Then
                sSuggestedPath = currentDir.FullName.TrimEnd("\\")
            Else
                sSuggestedPath = findSvnRoot(currentDir.FullName)
            End If

            eResponse = iSwApp.SendMsgToUser2("Would you like to use " & vbCrLf & sSuggestedPath, swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNoCancel)

            If eResponse = swMessageBoxResult_e.swMbHitYes Then
                sSuggestedPath = sSuggestedPath
                localRepoPath.Text = sSuggestedPath
                verifyLocalRepoPath()
            ElseIf eResponse = swMessageBoxResult_e.swMbHitCancel Then
                Exit Sub
            Else
                pickFolder()
            End If
        Else
            pickFolder()
        End If
    End Sub

    Private Sub boxCheck_Check(sender As Object, e As EventArgs) Handles onlineCheckBox.CheckedChanged
        If onlineCheckBox.Checked = False Then Exit Sub
        refreshAddIn()
    End Sub

    ' ### Parts Tree


    ' ### Status

    Private Sub StatusStrip2_ItemClicked(sender As Object, e As Windows.Forms.ToolStripItemClickedEventArgs)
        updateStatusStrip()
    End Sub
    Public Sub externalSetReadWriteFromLockStatus1()
        externalSetReadWriteFromLockStatus()
    End Sub
    Public Function refreshAddIn(Optional bsaveLocalRepoPathSettings As Boolean = True) As Boolean

        If Not verifyLocalRepoPath(, bCheckLocalFolder:=True, bCheckServer:=False) Then Return False     'Only need to check the local since updateStatusOfAllModelsVariable will check server. 

        Dim pathArr() As String = IO.Directory.GetDirectories(localRepoPath.Text, "*.*", IO.SearchOption.AllDirectories)
        'Dim sUserPreference As String

        'Set the referenced folder file to the local repository.
        'This will allow solidworks to find the files.
        'https://blogs.solidworks.com/tech/2014/06/search-path-order-for-opening-files-in-solidworks.html
        'http://help.solidworks.com/2012/English/api/swconst/SO_FileLocations.htm

        '=== Had to comment out, since it was adding 5000 file locations references to SolidWorks====
        ''Add all the subdirectories of the repo to the "reference files location" 
        '' This will let solidworks find the files!
        'For Each myPath In pathArr
        '    If myPath.Contains("\.svn") Then Continue For 'Skips the hidden folder that contains all the backup files

        '    sUserPreference = iSwApp.GetUserPreferenceStringValue(
        '        swUserPreferenceStringValue_e.swFileLocationsDocuments) 'Get existing preferences

        '    iSwApp.SetUserPreferenceStringValue(
        '        swUserPreferenceStringValue_e.swFileLocationsDocuments,
        '        sUserPreference & ";" & myPath)
        'Next

        'TODO: try to change setuserPreference to only be once in this file

        'Also: Prevent multiple files with the same name to be added to the vault!

        ''iSwApp.SetUserPreferenceStringValue(swUserPreferenceStringValue_e.swFileLocationsDocuments, "C:\Users\benne\Documents\SVN\fsae9\CAD\Subfolder")

        If updateStatusOfAllModelsVariable(bRefreshAllTreeViews:=True) Then
            switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
        End If
        saveLocalRepoPathSettings()
    End Function

    Public Sub saveLocalRepoPathSettings()
        My.Settings.localRepoPath = localRepoPath.Text
        My.Settings.Save()
    End Sub

    Public Function pickFolder() As DialogResult
        Dim folderDlg As FolderBrowserDialog = New FolderBrowserDialog()
        Dim result As DialogResult = folderDlg.ShowDialog()
        Dim sTempPath As String

        If (result = DialogResult.OK) Then
            sTempPath = folderDlg.SelectedPath
            'Environment.SpecialFolder root = folderDlg.RootFolder
            sTempPath = sTempPath.TrimEnd("\\")
            localRepoPath.Text = sTempPath
        End If

        Return result

        If verifyLocalRepoPath(bInteractive:=False) Then onlineCheckBox.Checked = True
        refreshAddIn()
    End Function

    Sub treeView1_NodeMouseClick(ByVal sender As Object,
    ByVal e As TreeNodeMouseClickEventArgs) _
    Handles TreeView1.NodeMouseClick

        'Dim sText As String = e.Node.Text
        'Dim modDoc As ModelDoc2
        Dim comp As Component2
        Dim activeModel As ModelDoc2 = iSwApp.ActiveDoc
        'Dim sText As String = localRepoPath.Text & "\" & e.Node.Text
        If activeModel Is Nothing Then Exit Sub

        If activeModel.GetType <> swDocumentTypes_e.swDocASSEMBLY Then Exit Sub

        'Debug.Assert(False, sText)
        'modDoc = activeModel.GetComponentByName(e.Node.Text)


        'Debug.Print(TypeOf e.Node.Tag)
        If TypeOf e.Node.Tag Is Component2 Then
            'If e.Node.Tag.GetType.ToString = "Component2" Then
            comp = e.Node.Tag
            comp.Select(False)
        End If

    End Sub

    Public Sub updateStatusStrip()

        'Exit Sub 'disabling for speed

        'Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        'If modDoc Is Nothing Then Exit Sub

        'Dim myCol As myColours = New myColours()
        'Dim status As SVNStatus = findStatusForFile(modDoc.GetPathName)
        'If IsNothing(status) Then Exit Sub

        'myCol.initialize()
        'If IsNothing(status) Then
        '    StatusStrip2.Text = ""
        '    StatusStrip2.BackColor = myCol.unknown
        'ElseIf status.fp(0).addDelChg1 = "?" Then
        '    StatusStrip2.Text = "File is not saved on the Vault"
        '    StatusStrip2.BackColor = myCol.notOnVault
        'ElseIf status.fp(0).lock6 = "K" Then
        '    StatusStrip2.Text = "Locked by you"
        '    StatusStrip2.BackColor = myCol.lockedByYou
        'ElseIf status.fp(0).lock6 = "O" Then
        '    StatusStrip2.Text = "Locked By someone Else"
        '    StatusStrip2.BackColor = myCol.lockedBySomeoneElse
        'ElseIf status.fp(0).lock6 = " " Then
        '    StatusStrip2.Text = "Available"
        '    StatusStrip2.BackColor = myCol.available
        'End If
    End Sub

    Sub NoCallbackSub()
    End Sub
    Sub FlyoutCommandItem1()
        iSwApp.SendMsgToUser("Flyout command 1")
    End Sub
    Function FlyoutEnable() As Integer
        Return 1
    End Function
    Function FlyoutDisable() As Integer
        Return 0
    End Function
    Sub FlyoutCallback()

    End Sub

    Public Sub switchTreeViewToCurrentModel(Optional bRetryWithRefresh As Boolean = True)

        If Not onlineCheckBox.Checked Then Exit Sub

        Dim treeNodeTemp As TreeNode
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc()
        If modDoc Is Nothing Then Exit Sub

        Dim treeNodeIndex As Integer = findStoredTreeView(modDoc.GetPathName, bRetryWithRefresh)
        If IsNothing(treeNodeIndex) Or IsNothing(allTreeViews) Then Exit Sub
        If Not onlineCheckBox.Checked Then Exit Sub

        Try
            treeNodeTemp = allTreeViews(treeNodeIndex).Nodes(0)
        Catch
            TreeView1.Nodes.Clear()
            Exit Sub
        End Try

        Dim clonedNode As TreeNode = CType(treeNodeTemp.Clone(), TreeNode)

        TreeView1.Nodes.Clear()
        TreeView1.Nodes.Insert(0, clonedNode)
        TreeView1.Nodes(0).Expand()
        'TreeView1.ExpandAll()
        TreeView1.Show()

    End Sub
    Function findStoredTreeView(pathName As String, Optional bRetryWithRefresh As Boolean = True) As Integer
        Dim i As Integer
        Dim bSuccess As Boolean
        'Dim bFound As Boolean = False

        If IsNothing(allTreeViews) Then
            bSuccess = updateStatusOfAllModelsVariable(bRefreshAllTreeViews:=True)
            If Not bSuccess Then iSwApp.SendMsgToUser("Status Update Failed.") : Return Nothing
            bRetryWithRefresh = False
        End If

        If allTreeViews.Length = 0 Then
            bSuccess = updateStatusOfAllModelsVariable(bRefreshAllTreeViews:=True)
            If Not bSuccess Then iSwApp.SendMsgToUser("Status Update Failed.") : Return Nothing
            bRetryWithRefresh = False
        End If


        'Try to find it using the existing allTreeViews object. This is the fastest
        For i = 0 To UBound(allTreeViews)
            If allTreeViews(i).Nodes.Count = 0 Then Continue For
            If (Strings.InStr(allTreeViews(i).Nodes(0).Text, System.IO.Path.GetFileName(pathName), CompareMethod.Text) <> 0) Then
                Return i
            End If
        Next

        If Not bRetryWithRefresh Then Return Nothing
        bSuccess = updateStatusOfAllModelsVariable(bRefreshAllTreeViews:=True)
        If Not bSuccess Then iSwApp.SendMsgToUser("Status Update Failed.") : Return Nothing

        For i = 0 To UBound(allTreeViews)
            If allTreeViews(i).Nodes.Count > 0 Then
                If (Strings.InStr(allTreeViews(i).Nodes(0).Text, System.IO.Path.GetFileName(pathName), CompareMethod.Text) <> 0) Then
                    Return i
                End If
            End If
        Next

        Return Nothing 'Couldn't find it!

    End Function
    Sub refreshAllTreeViewsVariable()
        Dim modDocArray As ModelDoc2() = getAllOpenDocs(bMustBeVisible:=True)

        If modDocArray Is Nothing Then
            ReDim allTreeViews(0)
            allTreeViews(0) = New TreeView
            Exit Sub
        End If

        If modDocArray.Length = 0 Then
            ReDim allTreeViews(0)
            allTreeViews(0) = New TreeView
            Exit Sub
        End If

        Dim i As Integer
        ReDim allTreeViews(UBound(modDocArray))

        For i = 0 To UBound(modDocArray)
            If modDocArray(i) Is Nothing Then Continue For
            allTreeViews(i) = New TreeView
            allTreeViews(i).Visible = False
            getComponentsOfAssemblyOptionalUpdateTree({modDocArray(i)}, i)
        Next
    End Sub

    Public Sub refreshCurrentTreeViewOnly()
        Dim activeDoc As ModelDoc2 = iSwApp.ActiveDoc

        If activeDoc Is Nothing Then Exit Sub

        Dim activePath As String = ""

        Try
            activePath = activeDoc.GetPathName()
        Catch
            activePath = ""
        End Try

        If String.IsNullOrWhiteSpace(activePath) Then Exit Sub

        If allTreeViews Is Nothing OrElse allTreeViews.Length = 0 Then
            ReDim allTreeViews(0)
            allTreeViews(0) = New TreeView
        End If

        Dim treeIndex As Integer = -1
        Dim activeFileName As String = System.IO.Path.GetFileName(activePath)

        For i As Integer = 0 To UBound(allTreeViews)
            If allTreeViews(i) Is Nothing Then Continue For
            If allTreeViews(i).Nodes.Count = 0 Then Continue For

            If Strings.InStr(allTreeViews(i).Nodes(0).Text, activeFileName, CompareMethod.Text) <> 0 Then
                treeIndex = i
                Exit For
            End If
        Next

        If treeIndex < 0 Then
            treeIndex = allTreeViews.Length
            ReDim Preserve allTreeViews(treeIndex)
            allTreeViews(treeIndex) = New TreeView
        End If

        If allTreeViews(treeIndex) Is Nothing Then
            allTreeViews(treeIndex) = New TreeView
        End If

        allTreeViews(treeIndex).Visible = False
        getComponentsOfAssemblyOptionalUpdateTree({activeDoc}, treeIndex)
        switchTreeViewToCurrentModel(bRetryWithRefresh:=False)
    End Sub

    Private Function getSafeModelPath(ByVal modDoc As ModelDoc2) As String
        If modDoc Is Nothing Then Return ""

        Try
            Return modDoc.GetPathName()
        Catch
            Return ""
        End Try
    End Function

    Private Function getSafeComponentPath(ByVal comp As Component2) As String
        If comp Is Nothing Then Return ""

        Try
            Return comp.GetPathName()
        Catch
            Return ""
        End Try
    End Function

    Private Function getSafeComponentSuppression(ByVal comp As Component2) As Integer
        If comp Is Nothing Then Return swComponentSuppressionState_e.swComponentResolved

        Try
            Return comp.GetSuppression2()
        Catch
            Return swComponentSuppressionState_e.swComponentResolved
        End Try
    End Function

    Private Function isComponentSuppressedState(ByVal suppressionState As Integer) As Boolean
        Return suppressionState = swComponentSuppressionState_e.swComponentSuppressed
    End Function

    Private Function isComponentLightweightState(ByVal suppressionState As Integer) As Boolean
        Return suppressionState = swComponentSuppressionState_e.swComponentLightweight OrElse
               suppressionState = swComponentSuppressionState_e.swComponentFullyLightweight
    End Function

    Private Function buildComponentNodeText(ByVal comp As Component2, ByVal modDoc As ModelDoc2) As String
        Dim compPath As String = getSafeComponentPath(comp)
        Dim nodeText As String = ""

        If Not String.IsNullOrWhiteSpace(compPath) Then
            nodeText = System.IO.Path.GetFileName(compPath)
        ElseIf modDoc IsNot Nothing Then
            Try
                nodeText = modDoc.GetTitle()
            Catch
                nodeText = "<unknown component>"
            End Try
        Else
            nodeText = "<unknown component>"
        End If

        Dim suppressionState As Integer = getSafeComponentSuppression(comp)

        If isComponentSuppressedState(suppressionState) Then
            nodeText &= " [Suppressed]"
        ElseIf isComponentLightweightState(suppressionState) Then
            nodeText &= " [Lightweight]"
        End If

        Return nodeText
    End Function

    Private Function modelDocListContainsPath(ByRef mdComponentList As List(Of ModelDoc2), ByVal filePath As String) As Boolean
        If mdComponentList Is Nothing Then Return False
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        For Each existingDoc As ModelDoc2 In mdComponentList
            If existingDoc Is Nothing Then Continue For

            Dim existingPath As String = getSafeModelPath(existingDoc)

            If String.IsNullOrWhiteSpace(existingPath) Then Continue For

            Try
                If String.Equals(System.IO.Path.GetFullPath(existingPath),
                                 System.IO.Path.GetFullPath(filePath),
                                 StringComparison.OrdinalIgnoreCase) Then
                    Return True
                End If
            Catch
                If String.Equals(existingPath, filePath, StringComparison.OrdinalIgnoreCase) Then
                    Return True
                End If
            End Try
        Next

        Return False
    End Function

    Private Sub addModelDocIfMissing(ByRef mdComponentList As List(Of ModelDoc2), ByVal modDoc As ModelDoc2, Optional ByVal bUniqueOnly As Boolean = True)
        If modDoc Is Nothing Then Exit Sub

        Dim docPath As String = getSafeModelPath(modDoc)

        If bUniqueOnly AndAlso modelDocListContainsPath(mdComponentList, docPath) Then Exit Sub

        mdComponentList.Add(modDoc)
    End Sub

    Private Function nodePathMatches(ByVal node As TreeNode, ByVal filePath As String) As Boolean
        If node Is Nothing Then Return False
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim nodePath As String = ""

        Try
            If TypeOf node.Tag Is Component2 Then
                nodePath = CType(node.Tag, Component2).GetPathName()
            ElseIf TypeOf node.Tag Is ModelDoc2 Then
                nodePath = CType(node.Tag, ModelDoc2).GetPathName()
            End If
        Catch
            nodePath = ""
        End Try

        If String.IsNullOrWhiteSpace(nodePath) Then Return False

        Try
            Return String.Equals(System.IO.Path.GetFullPath(nodePath),
                                 System.IO.Path.GetFullPath(filePath),
                                 StringComparison.OrdinalIgnoreCase)
        Catch
            Return String.Equals(nodePath, filePath, StringComparison.OrdinalIgnoreCase)
        End Try
    End Function

    Private Function treeContainsPath(ByVal rootNode As TreeNode, ByVal filePath As String) As Boolean
        If rootNode Is Nothing Then Return False

        If nodePathMatches(rootNode, filePath) Then Return True

        For Each child As TreeNode In rootNode.Nodes
            If treeContainsPath(child, filePath) Then Return True
        Next

        Return False
    End Function

    Private Sub addMissingComponentsFromFlatAssemblyList(ByVal swAssy As AssemblyDoc,
                                                         ByRef mdComponentList As List(Of ModelDoc2),
                                                         ByRef rootNode As TreeNode,
                                                         Optional ByVal bUniqueOnly As Boolean = True,
                                                         Optional ByVal bResolveLightweight As Boolean = False)

        If swAssy Is Nothing Then Exit Sub
        If rootNode Is Nothing Then Exit Sub

        Dim compObj As Object = Nothing

        Try
            compObj = swAssy.GetComponents(False)
        Catch
            compObj = Nothing
        End Try

        If compObj Is Nothing Then Exit Sub

        Dim compArr As Object() = Nothing

        Try
            compArr = CType(compObj, Object())
        Catch
            Exit Sub
        End Try

        For Each obj As Object In compArr
            Dim comp As Component2 = TryCast(obj, Component2)
            If comp Is Nothing Then Continue For

            Try
                If comp.IsEnvelope Then Continue For
            Catch
            End Try

            Dim compPath As String = getSafeComponentPath(comp)
            If String.IsNullOrWhiteSpace(compPath) Then Continue For

            If treeContainsPath(rootNode, compPath) Then Continue For

            Dim suppressionState As Integer = getSafeComponentSuppression(comp)
            Dim compDoc As ModelDoc2 = Nothing

            If Not isComponentSuppressedState(suppressionState) Then
                If bResolveLightweight AndAlso isComponentLightweightState(suppressionState) Then
                    Try
                        ensureResolvedComponent(comp)
                    Catch
                    End Try
                End If

                Try
                    compDoc = TryCast(comp.GetModelDoc2(), ModelDoc2)
                Catch
                    compDoc = Nothing
                End Try
            End If

            If compDoc IsNot Nothing Then
                addModelDocIfMissing(mdComponentList, compDoc, bUniqueOnly)
            End If

            Dim missingNode As New TreeNode(buildComponentNodeText(comp, compDoc))
            missingNode.Tag = comp
            setNodeColorFromStatus(missingNode)
            rootNode.Nodes.Add(missingNode)
        Next
    End Sub

    Public Function getComponentsOfAssemblyOptionalUpdateTree(
                                    ByRef modDoc As ModelDoc2,
                                    Optional ByVal allTreeViewsIndexToUpdate As Integer = -1,
                                    Optional ByVal bUniqueOnly As Boolean = True,
                                    Optional ByVal bResolveLightweight As Boolean = False) As ModelDoc2()

        If modDoc Is Nothing Then Return Nothing

        Dim modDocArr() As ModelDoc2 = {modDoc}

        Return getComponentsOfAssemblyOptionalUpdateTree(modDocArr, allTreeViewsIndexToUpdate, bUniqueOnly, bResolveLightweight)
    End Function

    Public Function getComponentsOfAssemblyOptionalUpdateTree(
                                    ByRef modDocArr() As ModelDoc2,
                                    Optional ByVal allTreeViewsIndexToUpdate As Integer = -1,
                                    Optional ByVal bUniqueOnly As Boolean = True,
                                    Optional ByVal bResolveLightweight As Boolean = False) As ModelDoc2()

        'Returns ModelDoc2() for normal/open/resolved files.
        'The tree can also show suppressed/path-only components by using Component2.GetPathName().
        'Important speed fix: when allTreeViewsIndexToUpdate is omitted, do NOT update the tree.

        If modDocArr Is Nothing Then Return Nothing

        Dim bUpdateTreeView As Boolean = (allTreeViewsIndexToUpdate >= 0 AndAlso Not IsNothing(allTreeViews))
        Dim sFileNameTemp As String
        Dim parentNode As TreeNode = Nothing
        Dim modelDocList As New List(Of ModelDoc2)()
        Dim swConfMgr As ConfigurationManager
        Dim swConf As Configuration
        Dim swRootComp As Component2
        Dim modDocTemp As ModelDoc2

        Dim i, j As Integer
        j = 0

        If (UBound(modDocArr) > 0) AndAlso bUpdateTreeView Then
            iSwApp.SendMsgToUser("Error: getComponentsOfAssemblyOptionalUpdateTree wasn't written to update tree views on multiple assemblies")
            Return Nothing
        End If

        For i = 0 To UBound(modDocArr)

            If IsNothing(modDocArr(i)) Then Continue For

            Try
                sFileNameTemp = System.IO.Path.GetFileName(modDocArr(i).GetPathName)
            Catch
                sFileNameTemp = modDocArr(i).GetTitle()
            End Try

            If bUpdateTreeView Then
                allTreeViews(allTreeViewsIndexToUpdate).Visible = False
                allTreeViews(allTreeViewsIndexToUpdate) = Nothing
                allTreeViews(allTreeViewsIndexToUpdate) = New TreeView

                parentNode = New TreeNode(sFileNameTemp)
                parentNode.Tag = modDocArr(i)
            End If

            If modDocArr(i).GetType = swDocumentTypes_e.swDocASSEMBLY Then

                'Do not resolve lightweight components during tree refresh.
                'Only explicit "With Dependents" actions pass bResolveLightweight:=True.
                If bResolveLightweight Then
                    Try
                        CType(modDocArr(i), AssemblyDoc).ResolveAllLightWeightComponents(WarnUser:=False)
                    Catch
                    End Try
                End If

                swConfMgr = modDocArr(i).ConfigurationManager
                swConf = swConfMgr.ActiveConfiguration
                swRootComp = swConf.GetRootComponent3(True)

                TraverseComponent(swRootComp, modelDocList, 1, parentNode, bUniqueOnly, bResolveLightweight)

                If bUpdateTreeView Then
                    addMissingComponentsFromFlatAssemblyList(CType(modDocArr(i), AssemblyDoc), modelDocList, parentNode, bUniqueOnly, bResolveLightweight)
                End If

                j += 1

            ElseIf modDocArr(i).GetType = swDocumentTypes_e.swDocDRAWING Then

                If bUpdateTreeView Then
                    setNodeColorFromStatus(parentNode)
                End If

                addModelDocIfMissing(modelDocList, modDocArr(i), bUniqueOnly)
                j += 1

                modDocTemp = iSwApp.GetOpenDocumentByName(System.IO.Path.ChangeExtension(modDocArr(i).GetPathName(), ".sldprt"))

                If Not (modDocTemp Is Nothing) Then
                    addModelDocIfMissing(modelDocList, modDocTemp, bUniqueOnly)
                    j += 1
                Else
                    modDocTemp = iSwApp.GetOpenDocumentByName(System.IO.Path.ChangeExtension(modDocArr(i).GetPathName(), ".sldasm"))
                    If Not (modDocTemp Is Nothing) Then
                        addModelDocIfMissing(modelDocList, modDocTemp, bUniqueOnly)
                        j += 1
                    End If
                End If

            Else
                If bUpdateTreeView Then
                    setNodeColorFromStatus(parentNode)
                    allTreeViews(allTreeViewsIndexToUpdate).Nodes.Add(parentNode)
                End If

                addModelDocIfMissing(modelDocList, modDocArr(i), bUniqueOnly)
                j += 1
            End If
        Next

        If j = 0 Then
            iSwApp.SendMsgToUser("Couldn't find model")
            Return Nothing
        End If

        Dim mdComponentArr() As ModelDoc2 = modelDocList.ToArray

        If bUpdateTreeView Then
            allTreeViews(allTreeViewsIndexToUpdate).Sort()
            If parentNode IsNot Nothing Then
                allTreeViews(allTreeViewsIndexToUpdate).Nodes.Add(parentNode)
            End If
        End If

        Return mdComponentArr
    End Function

    Sub TraverseComponent(
                         ByRef swComp As Component2,
                         ByRef mdComponentList As List(Of ModelDoc2),
                         ByVal nLevel As Long,
                         Optional ByRef rootNode As TreeNode = Nothing,
                         Optional ByVal bUniqueOnly As Boolean = True,
                         Optional ByVal bResolveLightweight As Boolean = False)

        'Keeps suppressed/lightweight components visible in the tree.
        'Suppressed components are not unsuppressed automatically.
        'If ModelDoc2 is unavailable, the tree still uses Component2.GetPathName().

        Dim bUC As Boolean = If(rootNode Is Nothing, False, True)
        Dim vChildComp As Object = Nothing
        Dim swChildComp As Component2
        Dim i As Long

        Dim modDocParent As ModelDoc2 = Nothing
        Dim modDocChild As ModelDoc2 = Nothing

        Dim parentNode As TreeNode = Nothing
        Dim childNode As TreeNode = Nothing

        If swComp Is Nothing Then Exit Sub

        Dim parentSuppression As Integer = getSafeComponentSuppression(swComp)

        If Not isComponentSuppressedState(parentSuppression) Then
            If bResolveLightweight AndAlso isComponentLightweightState(parentSuppression) Then
                Try
                    ensureResolvedComponent(swComp)
                Catch
                End Try
            End If

            Try
                modDocParent = TryCast(swComp.GetModelDoc2(), ModelDoc2)
            Catch
                modDocParent = Nothing
            End Try
        End If

        If modDocParent IsNot Nothing Then
            addModelDocIfMissing(mdComponentList, modDocParent, bUniqueOnly)
        End If

        If bUC Then
            parentNode = New TreeNode(buildComponentNodeText(swComp, modDocParent))
            parentNode.Tag = swComp
            setNodeColorFromStatus(parentNode)
        End If

        Try
            vChildComp = swComp.GetChildren()
        Catch
            vChildComp = Nothing
        End Try

        If vChildComp Is Nothing Then
            If bUC Then
                If nLevel = 1 Then
                    rootNode = parentNode
                ElseIf rootNode IsNot Nothing Then
                    rootNode.Nodes.Add(parentNode)
                End If
            End If

            Exit Sub
        End If

        For i = 0 To UBound(vChildComp)

            swChildComp = TryCast(vChildComp(i), Component2)
            If swChildComp Is Nothing Then Continue For

            Try
                If swChildComp.IsEnvelope Then Continue For
            Catch
            End Try

            Dim childPath As String = getSafeComponentPath(swChildComp)
            Dim childSuppression As Integer = getSafeComponentSuppression(swChildComp)

            modDocChild = Nothing

            If Not isComponentSuppressedState(childSuppression) Then
                If bResolveLightweight AndAlso isComponentLightweightState(childSuppression) Then
                    Try
                        ensureResolvedComponent(swChildComp)
                    Catch
                    End Try
                End If

                Try
                    modDocChild = TryCast(swChildComp.GetModelDoc2(), ModelDoc2)
                Catch
                    modDocChild = Nothing
                End Try
            End If

            If String.IsNullOrWhiteSpace(childPath) AndAlso modDocChild Is Nothing Then
                Continue For
            End If

            Dim childIsAssembly As Boolean = False

            If modDocChild IsNot Nothing Then
                Try
                    childIsAssembly = (modDocChild.GetType() = swDocumentTypes_e.swDocASSEMBLY)
                Catch
                    childIsAssembly = False
                End Try
            ElseIf Not String.IsNullOrWhiteSpace(childPath) Then
                childIsAssembly = String.Equals(System.IO.Path.GetExtension(childPath), ".SLDASM", StringComparison.OrdinalIgnoreCase)
            End If

            If childIsAssembly AndAlso modDocChild IsNot Nothing Then

                If bUniqueOnly AndAlso modelDocListContainsPath(mdComponentList, getSafeModelPath(modDocChild)) Then
                    If bUC Then
                        childNode = New TreeNode(buildComponentNodeText(swChildComp, modDocChild))
                        childNode.Tag = swChildComp
                        setNodeColorFromStatus(childNode)
                        parentNode.Nodes.Add(childNode)
                    End If

                    Continue For
                End If

                TraverseComponent(swChildComp, mdComponentList, nLevel + 1, parentNode, bUniqueOnly, bResolveLightweight)

            Else

                If modDocChild IsNot Nothing Then
                    addModelDocIfMissing(mdComponentList, modDocChild, bUniqueOnly)
                End If

                If bUC Then
                    childNode = New TreeNode(buildComponentNodeText(swChildComp, modDocChild))
                    childNode.Tag = swChildComp
                    setNodeColorFromStatus(childNode)
                    parentNode.Nodes.Add(childNode)
                End If

            End If

        Next i

        If bUC Then
            If nLevel = 1 Then
                rootNode = parentNode
            ElseIf rootNode IsNot Nothing Then
                rootNode.Nodes.Add(parentNode)
            End If
        End If

    End Sub
    Public Class myContextMenuClass

        Public Shared iSwApp2 As SldWorks
        Dim modDoc As ModelDoc2
        Dim modDocArr As ModelDoc2()
        Dim parentUserControl2 As UserControl1
        'Dim comp As Component2
        Public collapse As New ToolStripMenuItem("Collapse", My.Resources.PlumVault_128, AddressOf collapseTreeViewHandler)
        Public openLabel As New ToolStripMenuItem("Open", My.Resources.PlumVault_128, AddressOf openEventHandler)
        Public unlockLabel As New ToolStripMenuItem("Unlock", My.Resources.unlockIconOnly1, AddressOf unlockEventHandler)
        Public unlockWithDependentsLabel As New ToolStripMenuItem("Unlock With Dependents", My.Resources.unlockIconOnly1, AddressOf unlockWithDependentsEventHandler)
        Public commitLabel As New ToolStripMenuItem("Commit", My.Resources.Commit_Icon_Only, AddressOf commitEventHandler)
        Public commitWithDependentsLabel As New ToolStripMenuItem("Commit With Dependents", My.Resources.Commit_Icon_Only, AddressOf commitWithDependentsEventHandler)
        Public getLocksStealLabel As New ToolStripMenuItem("Get Lock (Steal Locks)", My.Resources.GetLocksIconOnly, AddressOf getLockStealLockEventHandler)
        Public getLockActiveDoc As New ToolStripMenuItem("Get Lock", My.Resources.GetLocksIconOnly, AddressOf getLockActiveDocEventHandler)
        Public getLockWithDependents As New ToolStripMenuItem("Get Lock With Dependents", My.Resources.GetLocksIconOnly, AddressOf getLocksActiveWithDependentsEventHandler)
        Public addToRepo As New ToolStripMenuItem("Add & Initial Commit", My.Resources.PlumVault_128, AddressOf addToRepoEventHandler)
        Public showLog As New ToolStripMenuItem("View SVN Log", My.Resources.PlumVault_128, AddressOf showLogEventHandler)
        Public upRevEdit As New ToolStripMenuItem("Up Rev to Edit", My.Resources.PlumVault_128, AddressOf upRevEditEventHandler)
        Public release As New ToolStripMenuItem("Approve & Release", My.Resources.PlumVault_128, AddressOf releaseEventHandler)
        Public Sub New(modDocInput As ModelDoc2, iSwAppInput As SldWorks, parentUserControl As UserControl1)
            modDoc = modDocInput 'compInput.GetModelDoc2
            'comp = compInput
            iSwApp2 = iSwAppInput
            parentUserControl2 = parentUserControl
        End Sub
        Sub upRevEditEventHandler(sender As Object, e As EventArgs)
            editNewRev({modDoc})
        End Sub
        Sub releaseEventHandler(sender As Object, e As EventArgs)
            myReleaseDoc(modDoc)
        End Sub
        Sub collapseTreeViewHandler(sender As Object, e As EventArgs)
            parentUserControl2.TreeView1.CollapseAll()
        End Sub
        Sub openEventHandler(sender As Object, e As EventArgs)
            iSwApp2.ActivateDoc3(modDoc.GetPathName, True, swRebuildOnActivation_e.swUserDecision, 0)
        End Sub
        Sub unlockEventHandler(sender As Object, e As EventArgs)
            unlockDocs({modDoc})
        End Sub
        Sub unlockWithDependentsEventHandler(sender As Object, e As EventArgs)
            myUnlockWithDependents(modDoc)
        End Sub
        Sub commitEventHandler(sender As Object, e As EventArgs)
            tortCommitDocs({modDoc})
        End Sub
        Public Sub commitWithDependentsEventHandler(sender As Object, e As EventArgs)
            modDocArr = parentUserControl2.GetSelectedModDocList(iSwApp2)
            tortCommitDocs(parentUserControl2.getComponentsOfAssemblyOptionalUpdateTree(modDocArr))
        End Sub
        Sub getLockStealLockEventHandler(sender As Object, e As EventArgs)
            If swMessageBoxResult_e.swMbHitOk =
            iSwApp2.SendMsgToUser2("File is Currently checked out by another user. You can steal their " &
                                   "Locks by clicking the checkbox in the next window. If both you and that user " &
                                   "attempt to check in their copies, a conflict can occur. Always communicate " &
                                   "your intention to break someone's lock with that user.",
                                    swMessageBoxIcon_e.swMbWarning, swMessageBoxBtn_e.swMbOkCancel) Then
                getLocksOfDocs({modDoc}, bBreakLocks:=True)
            End If
        End Sub
        Sub getLockActiveDocEventHandler(sender As Object, e As EventArgs)
            getLocksOfDocs(parentUserControl2.GetSelectedModDocList(iSwApp2))
        End Sub
        Sub getLocksActiveWithDependentsEventHandler(sender As Object, e As EventArgs)
            getLocksOfDocs(parentUserControl2.getComponentsOfAssemblyOptionalUpdateTree(parentUserControl2.GetSelectedModDocList(iSwApp2)))
        End Sub
        Sub addToRepoEventHandler(sender As Object, e As EventArgs)

            addtoRepoFunc(parentUserControl2.GetSelectedModDocList(iSwApp2))
        End Sub
        Sub showLogEventHandler(sender As Object, e As EventArgs)
            subShowLog(modDoc.GetPathName)
        End Sub
    End Class
    ' TODO

    ' make the treenode tag attach a custom class that contains component, modDoc, filepath, description, maybe all the svnstatus stuff too? 
    Function getModDocAttachedToNode(rootNode As TreeNode) As ModelDoc2
        Dim comp As Component2

        If rootNode Is Nothing Then Return Nothing
        If rootNode.Tag Is Nothing Then Return Nothing

        If TypeOf rootNode.Tag Is Component2 Then
            comp = CType(rootNode.Tag, Component2)

            Try
                Dim suppressionState As Integer = comp.GetSuppression2()

                'Do not unsuppress components just to color/build the tree.
                'Suppressed nodes should stay suppressed and use path-only SVN status.
                If suppressionState = swComponentSuppressionState_e.swComponentSuppressed Then
                    Return Nothing
                End If
            Catch
            End Try

            Try
                Return TryCast(comp.GetModelDoc2(), ModelDoc2)
            Catch
                Return Nothing
            End Try

        ElseIf TypeOf rootNode.Tag Is ModelDoc2 Then
            Return CType(rootNode.Tag, ModelDoc2)
        End If

        Return Nothing
    End Function

    Private Function stripStatusSuffix(nodeText As String) As String
        If String.IsNullOrWhiteSpace(nodeText) Then Return nodeText

        Dim lockedStart As Integer = nodeText.IndexOf(" [Locked", StringComparison.OrdinalIgnoreCase)
        Dim notCommittedStart As Integer = nodeText.IndexOf(" [Not committed", StringComparison.OrdinalIgnoreCase)

        Dim suffixStart As Integer = -1

        If lockedStart >= 0 Then suffixStart = lockedStart

        If notCommittedStart >= 0 Then
            If suffixStart = -1 OrElse notCommittedStart < suffixStart Then
                suffixStart = notCommittedStart
            End If
        End If

        If suffixStart >= 0 Then
            Return nodeText.Substring(0, suffixStart)
        End If

        Return nodeText
    End Function

    Sub setNodeColorFromStatus(
        ByRef rootNode As TreeNode)

        Dim myCol As myColours = New myColours()
        myCol.initialize()
        Dim status1 As SVNStatus
        Dim modDoc As ModelDoc2
        'Dim comp As Component2

        Dim bModelDocAttached As Boolean '= If(IsNothing(rootNode.Tag), False, True) ' True is modelDoc is attached to node
        Dim myContextMenu As myContextMenuClass

        Dim docMenu As ContextMenuStrip
        docMenu = New ContextMenuStrip()

        'If bCM Then
        '    rootNode.ContextMenuStrip.Items.Add(myContextMenu.openLabel)
        'End If

        modDoc = getModDocAttachedToNode(rootNode)

        Dim baseNodeText As String = stripStatusSuffix(rootNode.Text)
        rootNode.Text = baseNodeText

        If modDoc IsNot Nothing Then
            status1 = findStatusForFile(modDoc.GetPathName())
        Else
            Dim nodeFilePath As String = ""

            Try
                If TypeOf rootNode.Tag Is Component2 Then
                    nodeFilePath = CType(rootNode.Tag, Component2).GetPathName()
                ElseIf TypeOf rootNode.Tag Is ModelDoc2 Then
                    nodeFilePath = CType(rootNode.Tag, ModelDoc2).GetPathName()
                End If
            Catch
                nodeFilePath = ""
            End Try

            If Not String.IsNullOrWhiteSpace(nodeFilePath) Then
                status1 = findStatusForFile(nodeFilePath)
            Else
                status1 = findStatusForFile(baseNodeText)
            End If
        End If

        If modDoc Is Nothing Then
            bModelDocAttached = False
        Else
            bModelDocAttached = True
        End If

        myContextMenu = New myContextMenuClass(modDoc, iSwApp, Me) ' This gets overwritten immediately. It's just here to prevent pre-compile warnings
        If bModelDocAttached Then
            myContextMenu = New myContextMenuClass(modDoc, iSwApp, Me)
            docMenu.Items.AddRange({myContextMenu.openLabel, myContextMenu.collapse, myContextMenu.showLog})
            'modDoc = rootNode.Tag
        End If

        If status1 Is Nothing Then
            rootNode.BackColor = myCol.unknown
            rootNode.ToolTipText = "Unknown"

        ElseIf status1.fp(0).upToDate9 = "*" Then
            rootNode.BackColor = myCol.outOfDate
            rootNode.ToolTipText = "Your Copy is Out Of Date"
            'If bModelDocAttached Then docMenu.Items.AddRange({myContextMenu.getLocksStealLabel})

        ElseIf status1.fp(0).addDelChg1 = "M" OrElse
            status1.fp(0).addDelChg1 = "A" OrElse
            status1.fp(0).addDelChg1 = "?" Then

            rootNode.BackColor = myCol.localChangesNotCommitted
            rootNode.ToolTipText = "Local changes not committed"
            rootNode.Text &= " [Not committed]"

            If bModelDocAttached Then
                docMenu.Items.Add(myContextMenu.commitLabel)
                If modDoc.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                    docMenu.Items.Add(myContextMenu.commitWithDependentsLabel)
                End If
            End If

        ElseIf status1.fp(0).lock6 = "K" Then
            rootNode.BackColor = myCol.lockedByYou
            rootNode.ToolTipText = "Locked by you"
            rootNode.Text &= " [Locked by you]"

            If bModelDocAttached Then
                docMenu.Items.AddRange({myContextMenu.release})
                If modDoc.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                    docMenu.Items.AddRange(
                        {myContextMenu.commitLabel,
                        myContextMenu.commitWithDependentsLabel,
                        myContextMenu.unlockLabel,
                        myContextMenu.unlockWithDependentsLabel})
                Else
                    docMenu.Items.AddRange(
                        {myContextMenu.commitLabel,
                        myContextMenu.unlockLabel})
                End If
            End If


        ElseIf status1.fp(0).lock6 = "O" OrElse
            (Not String.IsNullOrWhiteSpace(status1.fp(0).lockOwner) AndAlso status1.fp(0).lock6 <> "K") Then
            rootNode.BackColor = myCol.lockedBySomeoneElse

            If Not String.IsNullOrWhiteSpace(status1.fp(0).lockOwner) Then
                rootNode.ToolTipText = "Locked by: " & status1.fp(0).lockOwner
                rootNode.Text &= " [Locked: " & status1.fp(0).lockOwner & "]"
            Else
                rootNode.ToolTipText = "Locked by someone else"
                rootNode.Text &= " [Locked]"
            End If
            If bModelDocAttached Then
                docMenu.Items.AddRange({myContextMenu.getLocksStealLabel})
                'If modDoc.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                If modDoc.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                    docMenu.Items.Add(myContextMenu.commitWithDependentsLabel)
                End If
            End If
            'If bCM Then rootNode.ContextMenuStrip.Items.Add(myContextMenu.getLocksStealLabel)
        ElseIf status1.fp(0).released = "||RELEASED||" Then
            rootNode.BackColor = myCol.released
            rootNode.ToolTipText = "Released"
            If bModelDocAttached Then
                docMenu.Items.AddRange({myContextMenu.upRevEdit})
            End If
        ElseIf status1.fp(0).addDelChg1 = "?" Then
            rootNode.BackColor = myCol.notOnVault
            rootNode.ToolTipText = "File is not saved the to the Vault"
            If bModelDocAttached Then
                docMenu.Items.Add(myContextMenu.addToRepo)
            End If

        ElseIf status1.fp(0).lock6 = " " Then
            rootNode.BackColor = myCol.available
            rootNode.ToolTipText = "Available"
            If bModelDocAttached Then
                docMenu.Items.Add(myContextMenu.getLockActiveDoc)
                If modDoc.GetType = swDocumentTypes_e.swDocASSEMBLY Then
                    docMenu.Items.AddRange({myContextMenu.commitWithDependentsLabel,
                                           myContextMenu.getLockWithDependents})
                End If
            End If
        Else
            rootNode.BackColor = myCol.unknown
            rootNode.ToolTipText = "Unknown"
            'If bModelDocAttached Then docMenu.Items.AddRange({myContextMenu.openLabel})

        End If


        rootNode.ContextMenuStrip = docMenu
    End Sub
    Public Sub TestMethod()
        'MsgBox("The strings in the flavorEnum are:")
        Dim i As String
        Dim j As Integer = 0
        For Each i In [Enum].GetNames(GetType(swSelectType_e))

            Debug.Print(j & " - " & i)
            j += 1
        Next
    End Sub

    Public Function GetSelectedModDocList(iSwApp As SolidWorks.Interop.sldworks.SldWorks) As SolidWorks.Interop.sldworks.ModelDoc2() 'SolidWorks.Interop.sldworks.Component2()

        'Returns the active doc if nothing is selected

        Dim swSelCompArr() As SolidWorks.Interop.sldworks.Component2
        Dim modDocArr() As SolidWorks.Interop.sldworks.ModelDoc2
        Dim swComp As SolidWorks.Interop.sldworks.Component2
        Dim obSelected As Object
        Dim i As Long
        'Dim tempObj As Object
        'swSelectType_e.swSelSHEETS
        Dim activeModDoc As ModelDoc2 = iSwApp.ActiveDoc
        If activeModDoc Is Nothing Then Return Nothing
        Dim swSelMgr As SolidWorks.Interop.sldworks.SelectionMgr = activeModDoc.SelectionManager
        Dim nSelCount As Long = swSelMgr.GetSelectedObjectCount2(-1)

        Dim myNames As String() = [Enum].GetNames(GetType(swSelectType_e))

        ReDim swSelCompArr(nSelCount - 1)
        ReDim modDocArr(0)

        If Not ((activeModDoc.GetType = swDocumentTypes_e.swDocPART) Or (activeModDoc.GetType = swDocumentTypes_e.swDocASSEMBLY)) Then
            'prevent selection manager (used later) from fatal errors on other files types
            Return {activeModDoc}
        End If

        For i = 1 To nSelCount
            ' need to grab all the components first before doing lightweight->resolve, otherwise the selection manager return 'nothing' for lightweight
            swSelCompArr(i - 1) = swSelMgr.GetSelectedObjectsComponent4(i, -1)
        Next

        For i = 1 To nSelCount

            swComp = swSelCompArr(i - 1)
            If ensureResolvedComponent(swComp) Then
                modDocArr(UBound(modDocArr)) = swComp.GetModelDoc2
            Else

                'unable to resolve component... maybe they had the top level selected? 
                obSelected = swSelMgr.GetSelectedObject6(i, -1)
                If obSelected Is Nothing Then Continue For

                Try
                    If obSelected.getPathName = activeModDoc.GetPathName Then 'check if they selected the top level
                        'They selected the top level... this was the only way I could pull it off
                        modDocArr(UBound(modDocArr)) = activeModDoc
                    Else
                        'couldn't get the component... not sure what they selected
                        Continue For
                    End If
                Catch ex As Exception
                    Continue For
                End Try


            End If

            ReDim Preserve modDocArr(UBound(modDocArr) + 1)
            'swSelCompArr(UBound(swSelCompArr)) = swComp
            'ReDim Preserve swSelCompArr(UBound(swSelCompArr) + 1)
        Next i

        If IsNothing(modDocArr(0)) Then
            'Return active doc if nothing is selected
            Return {activeModDoc}
        End If

        'Debug.Assert UBound(swSelCompArr) > 0
        'ReDim Preserve swSelCompArr(UBound(swSelCompArr) - 1)

        ReDim Preserve modDocArr(UBound(modDocArr) - 1)

        Return modDocArr

    End Function
    Class myColours
        Public lighterPurple As Drawing.Color
        Public localChangesNotCommitted As Drawing.Color
        Public darkerPurple As Drawing.Color
        Public lockedByYou As Drawing.Color
        Public lockedBySomeoneElse As Drawing.Color
        Public available As Drawing.Color
        Public unknown As Drawing.Color
        Public outOfDate As Drawing.Color
        Public notOnVault As Drawing.Color
        Public released As Drawing.Color
        Public Sub initialize()
            lighterPurple = Drawing.Color.FromArgb(208, 207, 229) 'used in icons
            darkerPurple = Drawing.Color.FromArgb(152, 150, 182) 'used in icons
            lockedByYou = Drawing.Color.FromArgb(159, 223, 159) 'Drawing.Color.Aquamarine
            localChangesNotCommitted = Drawing.Color.Orange
            lockedBySomeoneElse = Drawing.Color.FromArgb(255, 255, 153)
            available = Drawing.Color.White
            unknown = Drawing.Color.LightGray
            outOfDate = Drawing.Color.FromArgb(255, 129, 123)
            released = darkerPurple
            notOnVault = unknown
            'Drawing.Color.Bisque 'Drawing.Color.FromArgb(255, 77, 77) 'light red
        End Sub
    End Class

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles versionLabel.Click

    End Sub

    Private Sub ApproveReleaseToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ApproveReleaseToolStripMenuItem.Click
        Dim modDocArr() As ModelDoc2 = GetSelectedModDocList(iSwApp)

        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc

        If UBound(modDocArr) > 0 Then
            If iSwApp.SendMsgToUser2("Only one component can be released at a time. Would you like to release the assembly " & vbCrLf & modDoc.GetTitle & " ?",
                        swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbYesNoCancel) <> swMessageBoxResult_e.swMbHitOk Then
                Exit Sub
            End If
        Else
            modDoc = modDocArr(0)
        End If

        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Error: Document not found") : Exit Sub
        myReleaseDoc(modDoc)
    End Sub

    Private Sub EditNewRevisionToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles EditNewRevisionToolStripMenuItem.Click
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Error: Active Document not found") : Exit Sub
        editNewRev(GetSelectedModDocList(iSwApp))
    End Sub

    Private Sub ToolStripDropDownButReleases_ButtonClick(sender As Object, e As EventArgs) Handles ToolStripDropDownButReleases.ButtonClick
        ToolStripDropDownButReleases.ShowDropDown()
    End Sub
    Private Sub ToolStripSplitButFolder_ButtonClick(sender As Object, e As EventArgs) Handles ToolStripSplitButFolder.ButtonClick
        ToolStripSplitButFolder.ShowDropDown()
    End Sub

    Private Sub PickSVNFolderToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PickSVNFolderToolStripMenuItem.Click
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        If modDoc Is Nothing Then
            pickFolder()
            Exit Sub
        Else
            PickSVNFolderToolStripMenuItem.ShowDropDown()
        End If
    End Sub
    Private Sub ToolStripSplitButFolder_DropDownOpening(sender As Object, e As EventArgs) Handles ToolStripSplitButFolder.DropDownOpening

        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc

        ' Clear previous items if any
        PickSVNFolderToolStripMenuItem.DropDownItems.Clear()
        If IsNothing(modDoc) Then

        Else
            Dim docPath As String = modDoc.GetPathName
            Dim currentDir As DirectoryInfo = New FileInfo(docPath).Directory
            Dim svnRootPath As String = findSvnRoot(currentDir.FullName)

            ' Split the SVN root and current path into folder levels
            Dim svnRootUri As New Uri(svnRootPath & "\")
            Dim docUri As New Uri(currentDir.FullName & "\")

            ' Get relative folders from SVN root to document directory
            Dim relativeUri As Uri = svnRootUri.MakeRelativeUri(docUri)
            Dim relativePath As String = Uri.UnescapeDataString(relativeUri.ToString()).Replace("/", "\")
            Dim folders As List(Of String) = If(relativePath = "", New List(Of String)(), relativePath.Split("\"c).ToList())

            ' Build full paths from root up to 5 levels
            Dim fullPaths As New List(Of String)
            Dim currentPath As String = svnRootPath

            fullPaths.Add(currentPath) ' Include root
            For Each folder As String In folders
                If folder = "" Then Continue For
                currentPath = Path.Combine(currentPath, folder)
                fullPaths.Add(currentPath)
                If fullPaths.Count = 8 Then Exit For
            Next

            ' Add folder menu items
            For Each folderPath As String In fullPaths
                Dim item As New ToolStripMenuItem(folderPath)
                AddHandler item.Click,
            Sub(sender2 As Object, e2 As EventArgs)
                localRepoPath.Text = CType(sender2, ToolStripMenuItem).Text
                If verifyLocalRepoPath(bInteractive:=False) Then onlineCheckBox.Checked = True
                refreshAddIn()
            End Sub
                PickSVNFolderToolStripMenuItem.DropDownItems.Add(item)
            Next

            ' Add separator
            PickSVNFolderToolStripMenuItem.DropDownItems.Add(New ToolStripSeparator())

            ' Add "Open Folder Picker" menu item
            Dim openPickerItem As New ToolStripMenuItem("Open Folder Picker")
            AddHandler openPickerItem.Click, Sub() pickFolder()
            PickSVNFolderToolStripMenuItem.DropDownItems.Add(openPickerItem)
        End If

    End Sub

    Private Sub OpenFolderPickerToolStripMenuItem_Click(sender As Object, e As EventArgs)
        pickFolder()
        hideButton(ToolStripSplitButFolder)
    End Sub

    Private Sub SVNCleanupToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SVNCleanupToolStripMenuItem.Click
        myCleanup()
        hideButton(ToolStripSplitButFolder)
    End Sub
    Public Sub copyFileToClipboard(bWithDependents As Boolean, bTitleOnly As Boolean)
        Dim modDocArr As ModelDoc2()
        Dim sOutput As String() = {""}
        Dim eIncludeDrawings As swMessageBoxResult_e

        If bWithDependents Then
            modDocArr = getComponentsOfAssemblyOptionalUpdateTree(GetSelectedModDocList(iSwApp), bResolveLightweight:=True)
            If IsNothing(modDocArr) Then modDocArr = getComponentsOfAssemblyOptionalUpdateTree(iSwApp.ActiveDoc, bResolveLightweight:=True)
            eIncludeDrawings = iSwApp.SendMsgToUser2("Include drawings with names matching files?", swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNoCancel)
        Else
            modDocArr = GetSelectedModDocList(iSwApp)
            If IsNothing(modDocArr) Then modDocArr = {iSwApp.ActiveDoc}
            eIncludeDrawings = swMessageBoxResult_e.swMbHitNo
        End If

        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If

        Select Case eIncludeDrawings
            Case swMessageBoxResult_e.swMbHitYes
                sOutput = getMatchingDrawingForArrayPath(modDocArr, bTitleOnly)
            Case swMessageBoxResult_e.swMbHitNo
                sOutput = getFilePathsFromModDocArr(modDocArr, bTitleOnly)
            Case swMessageBoxResult_e.swMbHitCancel
                hideButton(ToolStripSplitButFolder)
                Exit Sub
        End Select

        CopyToClipboard(String.Join(vbCrLf, sOutput))

        hideButton(ToolStripSplitButFolder)

    End Sub
    Private Sub CopyFileNameToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyFileNameToolStripMenuItem.Click
        copyFileToClipboard(bWithDependents:=False, bTitleOnly:=True)
    End Sub
    Private Sub CopyFileNameWithDependentsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyFileNameWithDependentsToolStripMenuItem.Click
        copyFileToClipboard(bWithDependents:=True, bTitleOnly:=True)
    End Sub
    Private Sub CopyFullPathToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyFullPathToolStripMenuItem.Click
        copyFileToClipboard(bWithDependents:=False, bTitleOnly:=False)
    End Sub

    Private Sub CopyFilesPathsWithDependentsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyFilesPathsWithDependentsToolStripMenuItem.Click
        copyFileToClipboard(bWithDependents:=True, bTitleOnly:=False)
    End Sub

    Private Sub CopySvnUrlToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopySvnUrlToolStripMenuItem.Click
        'copy url to clipboard
        Dim modDocArr As ModelDoc2() = GetSelectedModDocList(iSwApp)
        If IsNothing(modDocArr) Then modDocArr = getComponentsOfAssemblyOptionalUpdateTree(iSwApp.ActiveDoc, bResolveLightweight:=True)
        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If

        Dim urls As String() = getUrlfromPaths(getFilePathsFromModDocArr(modDocArr))

        CopyToClipboard(String.Join(vbCrLf, urls))
        hideButton(ToolStripSplitButFolder)
    End Sub
    Private Sub CopySvnUrlWithDependentsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopySvnUrlWithDependentsToolStripMenuItem.Click
        'copy url to clipboard, with dependents
        Dim sOutput As String() = {""}
        Dim modDocArr As ModelDoc2() = getComponentsOfAssemblyOptionalUpdateTree(GetSelectedModDocList(iSwApp), bResolveLightweight:=True)
        If IsNothing(modDocArr) Then modDocArr = getComponentsOfAssemblyOptionalUpdateTree(iSwApp.ActiveDoc, bResolveLightweight:=True)
        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If

        Dim eIncludeDrawings As swMessageBoxResult_e = iSwApp.SendMsgToUser2("Include drawings with names matching files?", swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNoCancel)

        Select Case eIncludeDrawings
            Case swMessageBoxResult_e.swMbHitYes
                sOutput = getUrlfromPaths(getMatchingDrawingForArrayPath(modDocArr))
            Case swMessageBoxResult_e.swMbHitNo
                sOutput = getUrlfromPaths(getFilePathsFromModDocArr(modDocArr))
            Case swMessageBoxResult_e.swMbHitCancel
                hideButton(ToolStripSplitButFolder)
                CloseDropDown(CopySvnUrlToolStripMenuItem)
                Exit Sub
        End Select

        CopyToClipboard(String.Join(vbCrLf, sOutput))
        hideButton(ToolStripSplitButFolder)
    End Sub
    Private Sub CopyActiveFilesParentFolderToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyActiveFilesParentFolderToolStripMenuItem.Click

        Dim modDocArr As ModelDoc2() = GetSelectedModDocList(iSwApp)
        If IsNothing(modDocArr) Then modDocArr = {iSwApp.ActiveDoc}
        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If

        Dim currentDir As DirectoryInfo = New FileInfo(modDocArr(0).GetPathName).Directory

        CopyToClipboard(currentDir.ToString)
        hideButton(ToolStripSplitButFolder)

    End Sub

    Private Sub ShareWithColleagueToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ShareWithColleagueToolStripMenuItem.Click

        Dim stringArr As String()

        Dim modDocArr As ModelDoc2() = GetSelectedModDocList(iSwApp)
        If IsNothing(modDocArr) Then modDocArr = {iSwApp.ActiveDoc}

        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If
        If IsNothing(modDocArr(0)) Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If

        stringArr = getUrlfromPaths({modDocArr(0).GetPathName})

        If IsNothing(stringArr) Then
            iSwApp.SendMsgToUser("Couldn't find get URL(s)! Exiting.")
            Exit Sub
        End If

        Dim stringToClip As String = "CAD is available on svn" & vbCrLf & "My Local Path (yours may be different):" & vbCrLf

        stringToClip &= modDocArr(0).GetPathName & vbCrLf & vbCrLf & "or remote path: " & vbCrLf
        stringToClip &= stringArr(0)

        CopyToClipboard(stringToClip)
        hideButton(ToolStripSplitButFolder)
    End Sub

    Private Sub CreateSvnFilelistToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CreateSvnFilelistToolStripMenuItem.Click
        'filelist, file only
        If Not verifyLocalRepoPath() Then Exit Sub

        Dim sDest As String = localRepoPath.Text & "\" & "fileList.txt"
        Dim sFileNames As String
        Dim eIncludeDrawings As Integer = iSwApp.SendMsgToUser2("Include drawings with names matching files?", swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNoCancel)
        Dim bIncludeDrawings As Boolean = False
        Dim modDocArr As ModelDoc2()

        If eIncludeDrawings = swMessageBoxResult_e.swMbHitCancel Then Exit Sub
        If eIncludeDrawings = swMessageBoxResult_e.swMbHitYes Then bIncludeDrawings = True

        modDocArr = GetSelectedModDocList(iSwApp)

        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Couldn't find document! Exiting.")
            Exit Sub
        End If

        If bIncludeDrawings Then
            sFileNames = formatFilePathArrForProc(getMatchingDrawingForArrayPath(modDocArr), sDelimiter:=vbCrLf)
        Else
            sFileNames = formatFilePathArrForProc(getFilePathsFromModDocArr(modDocArr), sDelimiter:=vbCrLf)
        End If

        Try
            File.WriteAllText(sDest, sFileNames)
            iSwApp.SendMsgToUser("Wrote Filelist to " & vbCrLf & sDest)
        Catch ex As Exception
            iSwApp.SendMsgToUser("ERROR writing Filelist to " & vbCrLf & sDest)
        End Try
        hideButton(ToolStripSplitButFolder)
    End Sub

    Private Sub CreateSvnFilelistWithDependentsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CreateSvnFilelistWithDependentsToolStripMenuItem.Click
        'filelist with dependents
        If Not verifyLocalRepoPath() Then Exit Sub

        Dim sDest As String = localRepoPath.Text & "\" & "fileList.txt"
        Dim sFileNames As String
        Dim eMsgBoxResult As Integer = iSwApp.SendMsgToUser2("Include drawings with names matching files?", swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNoCancel)
        Dim bIncludeDrawings As Boolean = False
        Dim bIncludeDependents As Boolean = False
        Dim modDocArr As ModelDoc2()

        If eMsgBoxResult = swMessageBoxResult_e.swMbHitCancel Then Exit Sub
        If eMsgBoxResult = swMessageBoxResult_e.swMbHitYes Then bIncludeDrawings = True

        eMsgBoxResult = iSwApp.SendMsgToUser2("Include Dependents?", swMessageBoxIcon_e.swMbQuestion, swMessageBoxBtn_e.swMbYesNoCancel)
        If eMsgBoxResult = swMessageBoxResult_e.swMbHitCancel Then Exit Sub
        If eMsgBoxResult = swMessageBoxResult_e.swMbHitYes Then bIncludeDependents = True

        If bIncludeDependents Then
            modDocArr = getComponentsOfAssemblyOptionalUpdateTree(GetSelectedModDocList(iSwApp), bResolveLightweight:=True)
        Else
            modDocArr = GetSelectedModDocList(iSwApp)
        End If

        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Error Getting Files")
            Exit Sub
        End If

        If bIncludeDrawings Then
            sFileNames = formatFilePathArrForProc(getMatchingDrawingForArrayPath(modDocArr), sDelimiter:=vbCrLf)
        Else
            sFileNames = formatFilePathArrForProc(getFilePathsFromModDocArr(modDocArr), sDelimiter:=vbCrLf)
        End If

        Try
            File.WriteAllText(sDest, sFileNames)
            iSwApp.SendMsgToUser("Wrote Filelist to " & vbCrLf & sDest)
        Catch ex As Exception
            iSwApp.SendMsgToUser("ERROR writing Filelist to " & vbCrLf & sDest)
        End Try
        hideButton(ToolStripSplitButFolder)
    End Sub

    Private Sub OpenFileFromURLToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenFileFromURLToolStripMenuItem.Click

    End Sub

    Private Sub GoogleToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GoogleToolStripMenuItem.Click
        Dim modDocArr As ModelDoc2() = GetSelectedModDocList(iSwApp)
        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Error Getting Files")
            Exit Sub
        End If
        openFileNameInWebpage("https://www.google.com/search?q=%s", modDocArr(0))
    End Sub

    Private Sub McToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles McToolStripMenuItem.Click
        Dim modDocArr As ModelDoc2() = GetSelectedModDocList(iSwApp)
        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Error Getting Files")
            Exit Sub
        End If
        openFileNameInWebpage("https://www.mcmaster.com/%s", modDocArr(0))
    End Sub

    Private Sub DigikeyToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DigikeyToolStripMenuItem.Click
        Dim modDocArr As ModelDoc2() = GetSelectedModDocList(iSwApp)
        If IsNothing(modDocArr) Then
            iSwApp.SendMsgToUser("Error Getting Files")
            Exit Sub
        End If
        openFileNameInWebpage("https://www.digikey.com/en/products/result?keywords=%s", modDocArr(0))
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs)

    End Sub
End Class
