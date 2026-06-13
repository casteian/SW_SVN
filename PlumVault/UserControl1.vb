
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
    Private WithEvents solidWorksSelectionMirrorTimer As System.Windows.Forms.Timer
    Private WithEvents autoPreSyncLevel1Timer As System.Windows.Forms.Timer
    Private WithEvents butSyncStatus As Button
    Private WithEvents chkDebugIgnoreNaming As CheckBox
    Private syncProgressBar As ProgressBar
    Private syncProgressLabel As Label
    Private syncStatusContextMenu As ContextMenuStrip
    Private Const LAZY_LOAD_PLACEHOLDER_TEXT As String = "<load children>"
    Private syncStatusInProgress As Boolean = False
    Private pendingAutoPreSyncAssemblyPath As String = ""
    Private lastAutoPreSyncAssemblyPath As String = ""
    Private lastAutoPreSyncStartedAt As DateTime = DateTime.MinValue
    Private Const AUTO_PRESYNC_MIN_SECONDS_BETWEEN_RUNS As Integer = 120
    Private refreshTreeNeedsUpdate As Boolean = False
    Private normalRefreshTreeBackColor As Color
    Private lastLiveCheckedActivePath As String = ""

    'Bidirectional selection mirror:
    'Tree click -> selects/highlights the SOLIDWORKS component.
    'SOLIDWORKS selection -> highlights the matching loaded plugin-tree node.
    Private treeToSolidWorksSelectionInProgress As Boolean = False
    Private solidWorksToTreeSelectionInProgress As Boolean = False
    Private lastMirroredSolidWorksSelectionKey As String = ""

    'Darker selection highlight for the plugin tree.
    'The normal WinForms inactive TreeView selection is very faint grey when SOLIDWORKS has focus.
    Private ReadOnly pluginTreeSelectedBackColor As Color = Color.FromArgb(0, 95, 160)
    Private ReadOnly pluginTreeSelectedForeColor As Color = Color.White

    'Tracks whether the TreeView selection came from an actual user click.
    'WinForms/SolidWorks can leave the root node selected even when the user thinks
    'nothing is selected. Sync uses this so the default click stays Level-1-only.
    Private lastUserClickedTreeNodeForSync As TreeNode = Nothing

    Private Sub setRefreshTreeButtonNormal()
        refreshTreeNeedsUpdate = False

        If butRefresh Is Nothing Then Exit Sub

        If CStr(If(butRefresh.Tag, "")) = "CompactSvnActionButton" Then
            butRefresh.Text = "Refresh"
            butRefresh.Size = New Size(74, 24)
            butRefresh.Font = New Font(butRefresh.Font.FontFamily, 8.25!, FontStyle.Bold)
        Else
            butRefresh.Text = "Refresh Tree"
            butRefresh.Size = New Size(220, 32)
            butRefresh.Font = New Font(butRefresh.Font.FontFamily, 10.0!, FontStyle.Bold)
        End If

        butRefresh.BackColor = normalRefreshTreeBackColor
        butRefresh.UseVisualStyleBackColor = True
    End Sub
    Private Sub setRefreshTreeButtonUpdateNeeded()
        refreshTreeNeedsUpdate = True

        If butRefresh Is Nothing Then Exit Sub

        If CStr(If(butRefresh.Tag, "")) = "CompactSvnActionButton" Then
            butRefresh.Text = "Refresh*"
            butRefresh.Size = New Size(74, 24)
            butRefresh.Font = New Font(butRefresh.Font.FontFamily, 8.25!, FontStyle.Bold)
        Else
            butRefresh.Text = "Changes made - Update now"
            butRefresh.Size = New Size(220, 32)
            butRefresh.Font = New Font(butRefresh.Font.FontFamily, 9.0!, FontStyle.Bold)
        End If

        butRefresh.BackColor = Color.LightGreen
        butRefresh.UseVisualStyleBackColor = False
    End Sub
    Private Sub ensureSyncStatusButton()
        If butRefresh Is Nothing Then Exit Sub

        Dim parentControl As Control = butRefresh.Parent
        If parentControl Is Nothing Then parentControl = Me

        If butSyncStatus Is Nothing Then
            butSyncStatus = New Button()
            butSyncStatus.Name = "butSyncStatus"
            butSyncStatus.TabIndex = butRefresh.TabIndex + 1
            parentControl.Controls.Add(butSyncStatus)
        ElseIf butSyncStatus.Parent Is Nothing Then
            parentControl.Controls.Add(butSyncStatus)
        End If

        setCompactSvnActionButtonStyle(butRefresh, "Refresh")
        setCompactSvnActionButtonStyle(butSyncStatus, "Sync")
        setupSyncStatusContextMenu()
        ensureDebugIgnoreNamingCheckbox(parentControl)
        ensureSyncProgressControls(parentControl)

        positionRefreshAndSyncButtonsBesideCommit()
    End Sub

    Private Sub ensureDebugIgnoreNamingCheckbox(ByVal parentControl As Control)
        If parentControl Is Nothing Then parentControl = Me

        If chkDebugIgnoreNaming Is Nothing Then
            chkDebugIgnoreNaming = New CheckBox()
            chkDebugIgnoreNaming.Name = "chkDebugIgnoreNaming"
            chkDebugIgnoreNaming.Text = "Debug: ignore naming"
            chkDebugIgnoreNaming.AutoSize = True
            chkDebugIgnoreNaming.Font = New Font(Me.Font.FontFamily, 7.5!, FontStyle.Regular)
            chkDebugIgnoreNaming.BackColor = SystemColors.Control
            chkDebugIgnoreNaming.UseVisualStyleBackColor = True
            chkDebugIgnoreNaming.Checked = False
            chkDebugIgnoreNaming.Visible = True
            chkDebugIgnoreNaming.TabIndex = butRefresh.TabIndex + 2
            parentControl.Controls.Add(chkDebugIgnoreNaming)
        ElseIf chkDebugIgnoreNaming.Parent Is Nothing Then
            parentControl.Controls.Add(chkDebugIgnoreNaming)
        End If
    End Sub

    Private Sub ensureSyncProgressControls(ByVal parentControl As Control)
        If parentControl Is Nothing Then parentControl = Me

        If syncProgressBar Is Nothing Then
            syncProgressBar = New ProgressBar()
            syncProgressBar.Name = "syncProgressBar"
            syncProgressBar.Size = New Size(154, 10)
            syncProgressBar.Style = ProgressBarStyle.Marquee
            syncProgressBar.MarqueeAnimationSpeed = 0
            syncProgressBar.Visible = False
            syncProgressBar.TabIndex = butRefresh.TabIndex + 3
            parentControl.Controls.Add(syncProgressBar)
        ElseIf syncProgressBar.Parent Is Nothing Then
            parentControl.Controls.Add(syncProgressBar)
        End If

        If syncProgressLabel Is Nothing Then
            syncProgressLabel = New Label()
            syncProgressLabel.Name = "syncProgressLabel"
            syncProgressLabel.AutoSize = True
            syncProgressLabel.Font = New Font(Me.Font.FontFamily, 7.0!, FontStyle.Regular)
            syncProgressLabel.BackColor = SystemColors.Control
            syncProgressLabel.Text = "Sync pending..."
            syncProgressLabel.Visible = False
            syncProgressLabel.TabIndex = butRefresh.TabIndex + 4
            parentControl.Controls.Add(syncProgressLabel)
        ElseIf syncProgressLabel.Parent Is Nothing Then
            parentControl.Controls.Add(syncProgressLabel)
        End If
    End Sub

    Private Sub setSyncProgressVisible(ByVal visible As Boolean,
                                       Optional ByVal message As String = "",
                                       Optional ByVal fileCount As Integer = 0)
        Try
            If syncProgressBar Is Nothing OrElse syncProgressLabel Is Nothing Then Exit Sub

            If visible Then
                Dim msg As String = If(String.IsNullOrWhiteSpace(message), "Syncing", message)
                If fileCount > 0 Then msg &= " (" & fileCount.ToString() & " files)"

                syncProgressLabel.Text = msg
                syncProgressLabel.Visible = True
                syncProgressBar.Visible = True
                syncProgressBar.Style = ProgressBarStyle.Marquee
                syncProgressBar.MarqueeAnimationSpeed = 35
                syncProgressBar.BringToFront()
                syncProgressLabel.BringToFront()
            Else
                syncProgressBar.MarqueeAnimationSpeed = 0
                syncProgressBar.Visible = False
                syncProgressLabel.Visible = False
            End If
        Catch
        End Try
    End Sub

    Public Function debugIgnoreNamingConventionEnabled() As Boolean
        Try
            Return chkDebugIgnoreNaming IsNot Nothing AndAlso chkDebugIgnoreNaming.Checked
        Catch
            Return False
        End Try
    End Function

    Private Sub setupSyncStatusContextMenu()
        If butSyncStatus Is Nothing Then Exit Sub

        If syncStatusContextMenu Is Nothing Then
            syncStatusContextMenu = New ContextMenuStrip()

            Dim syncBranchItem As New ToolStripMenuItem("Sync Selected Branch", Nothing, AddressOf syncSelectedBranchMenuItem_Click)
            Dim syncWholeCarItem As New ToolStripMenuItem("Sync Whole Car Status (slow)", Nothing, AddressOf syncWholeCarMenuItem_Click)

            syncStatusContextMenu.Items.Add(syncBranchItem)
            syncStatusContextMenu.Items.Add(syncWholeCarItem)
        End If

        butSyncStatus.ContextMenuStrip = syncStatusContextMenu
        butSyncStatus.Text = "Sync"
        butSyncStatus.AutoEllipsis = True
        butSyncStatus.UseVisualStyleBackColor = True
    End Sub

    Private Sub syncSelectedBranchMenuItem_Click(sender As Object, e As EventArgs)
        performSyncStatus()
    End Sub

    Private Sub syncWholeCarMenuItem_Click(sender As Object, e As EventArgs)
        performSyncStatusWholeCar()
    End Sub

    Private Sub setCompactSvnActionButtonStyle(ByVal btn As Button, ByVal buttonText As String)
        If btn Is Nothing Then Exit Sub

        btn.Tag = "CompactSvnActionButton"
        btn.Text = buttonText
        btn.Size = New Size(74, 24)
        btn.Font = New Font(btn.Font.FontFamily, 8.25!, FontStyle.Bold)
        btn.BackColor = SystemColors.Control
        btn.UseVisualStyleBackColor = True
        btn.Anchor = AnchorStyles.Top Or AnchorStyles.Left
    End Sub

    Private Sub positionRefreshAndSyncButtonsBesideCommit()
        If butRefresh Is Nothing Then Exit Sub
        If butSyncStatus Is Nothing Then Exit Sub
        If ToolStripDropDownButCommit Is Nothing Then Exit Sub
        If ToolStripDropDownButCommit.Owner Is Nothing Then Exit Sub

        Dim parentControl As Control = butRefresh.Parent
        If parentControl Is Nothing Then parentControl = Me

        Dim ownerControl As Control = TryCast(ToolStripDropDownButCommit.Owner, Control)
        If ownerControl Is Nothing Then Exit Sub

        Try
            Dim commitBounds As Rectangle = ToolStripDropDownButCommit.Bounds
            Dim startScreen As Point = ownerControl.PointToScreen(New Point(commitBounds.Right + 8, commitBounds.Top + 2))
            Dim startPoint As Point = parentControl.PointToClient(startScreen)

            Dim gap As Integer = 4
            Dim minLeft As Integer = 4
            Dim maxLeft As Integer = Math.Max(minLeft, parentControl.ClientSize.Width - butRefresh.Width - gap)
            Dim x As Integer = Math.Max(minLeft, Math.Min(startPoint.X, maxLeft))
            Dim y As Integer = Math.Max(0, startPoint.Y)

            'Prefer putting both buttons beside Commit. If the task pane is too narrow,
            'stack Sync below Refresh so they do not get clipped at the bottom of the pane.
            If x + butRefresh.Width + gap + butSyncStatus.Width <= parentControl.ClientSize.Width - 2 Then
                butRefresh.Location = New Point(x, y)
                butSyncStatus.Location = New Point(x + butRefresh.Width + gap, y)
            Else
                butRefresh.Location = New Point(x, y)
                butSyncStatus.Location = New Point(x, y + butRefresh.Height + gap)
            End If

            If chkDebugIgnoreNaming IsNot Nothing Then
                chkDebugIgnoreNaming.Location = New Point(butRefresh.Left, Math.Max(butRefresh.Bottom, butSyncStatus.Bottom) + 2)
                chkDebugIgnoreNaming.BringToFront()
            End If

            If syncProgressLabel IsNot Nothing AndAlso syncProgressBar IsNot Nothing Then
                Dim progressTop As Integer = Math.Max(butRefresh.Bottom, butSyncStatus.Bottom) + 2
                If chkDebugIgnoreNaming IsNot Nothing Then progressTop = chkDebugIgnoreNaming.Bottom + 2
                syncProgressLabel.Location = New Point(butRefresh.Left, progressTop)
                syncProgressBar.Location = New Point(butRefresh.Left, syncProgressLabel.Bottom + 1)
                syncProgressBar.Width = Math.Max(120, Math.Min(180, parentControl.ClientSize.Width - butRefresh.Left - 8))
            End If

            butRefresh.BringToFront()
            butSyncStatus.BringToFront()
        Catch
            'Fallback: keep the buttons near their original area if the ToolStrip geometry is unavailable.
            Dim fallbackTop As Integer = Math.Max(0, butRefresh.Top)
            butRefresh.Location = New Point(Math.Max(4, butRefresh.Left), fallbackTop)
            butSyncStatus.Location = New Point(butRefresh.Right + 4, fallbackTop)

            If chkDebugIgnoreNaming IsNot Nothing Then
                chkDebugIgnoreNaming.Location = New Point(butRefresh.Left, Math.Max(butRefresh.Bottom, butSyncStatus.Bottom) + 2)
                chkDebugIgnoreNaming.BringToFront()
            End If

            If syncProgressLabel IsNot Nothing AndAlso syncProgressBar IsNot Nothing Then
                Dim progressTop As Integer = Math.Max(butRefresh.Bottom, butSyncStatus.Bottom) + 2
                If chkDebugIgnoreNaming IsNot Nothing Then progressTop = chkDebugIgnoreNaming.Bottom + 2
                syncProgressLabel.Location = New Point(butRefresh.Left, progressTop)
                syncProgressBar.Location = New Point(butRefresh.Left, syncProgressLabel.Bottom + 1)
                syncProgressBar.Width = Math.Max(120, Math.Min(180, parentControl.ClientSize.Width - butRefresh.Left - 8))
            End If
        End Try
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


    Private Sub configurePluginTreeSelectionHighlight()
        If TreeView1 Is Nothing Then Exit Sub

        Try
            'Keep selection visible when SOLIDWORKS takes focus.
            TreeView1.HideSelection = False

            'OwnerDrawText lets us replace the default inactive grey selection with a darker highlight.
            'Only the text area is owner-drawn; node icons/expanders remain standard TreeView behavior.
            TreeView1.DrawMode = TreeViewDrawMode.OwnerDrawText
        Catch
        End Try
    End Sub

    Private Sub TreeView1_DrawNode(sender As Object, e As DrawTreeNodeEventArgs) Handles TreeView1.DrawNode
        If e Is Nothing OrElse e.Node Is Nothing Then Exit Sub

        Try
            Dim isSelected As Boolean = ((e.State And TreeNodeStates.Selected) = TreeNodeStates.Selected)

            Dim backColorToUse As Color
            Dim foreColorToUse As Color

            If isSelected Then
                backColorToUse = pluginTreeSelectedBackColor
                foreColorToUse = pluginTreeSelectedForeColor
            Else
                backColorToUse = e.Node.BackColor
                If backColorToUse = Color.Empty Then backColorToUse = TreeView1.BackColor

                foreColorToUse = e.Node.ForeColor
                If foreColorToUse = Color.Empty Then foreColorToUse = TreeView1.ForeColor
            End If

            Using backBrush As New SolidBrush(backColorToUse)
                e.Graphics.FillRectangle(backBrush, e.Bounds)
            End Using

            TextRenderer.DrawText(
                e.Graphics,
                e.Node.Text,
                TreeView1.Font,
                e.Bounds,
                foreColorToUse,
                TextFormatFlags.GlyphOverhangPadding Or TextFormatFlags.VerticalCenter
            )

            If isSelected Then
                Try
                    ControlPaint.DrawFocusRectangle(e.Graphics, e.Bounds, pluginTreeSelectedForeColor, backColorToUse)
                Catch
                End Try
            End If

        Catch
            'Fallback to default drawing if anything unexpected happens.
            e.DrawDefault = True
        End Try
    End Sub

    Private Sub TreeView1_AfterSelect(sender As Object, e As TreeViewEventArgs) Handles TreeView1.AfterSelect
        Try
            TreeView1.Invalidate()
        Catch
        End Try
    End Sub

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
        ensureSyncStatusButton()

        configurePluginTreeSelectionHighlight()

        solidWorksSelectionMirrorTimer = New System.Windows.Forms.Timer()
        solidWorksSelectionMirrorTimer.Interval = 450
        solidWorksSelectionMirrorTimer.Start()

        liveChangeCheckTimer = New System.Windows.Forms.Timer()
        liveChangeCheckTimer.Interval = 30000 '30 seconds
        liveChangeCheckTimer.Start()

        'Auto Level 1 pre-sync timer intentionally not created.
        'Use manual Sync for fresh server status; persistent cache is applied instantly on tree build.


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
            'Auto Level 1 pre-sync intentionally disabled.
            'Persistent cache now provides immediate last-known status without starting SVN during open.
        End If
    End Sub

    Private Sub autoPreSyncLevel1Timer_Tick(sender As Object, e As EventArgs) Handles autoPreSyncLevel1Timer.Tick
        Try
            autoPreSyncLevel1Timer.Stop()
        Catch
        End Try

        startAutoPreSyncLevel1IfReady()
    End Sub

    Private Sub scheduleAutoPreSyncLevel1ForActiveAssembly(Optional ByVal reason As String = "")
        Try
            If iSwApp Is Nothing Then Exit Sub
            If Not onlineCheckBox.Checked Then Exit Sub
            If syncStatusInProgress Then Exit Sub

            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDoc Is Nothing Then Exit Sub
            If activeDoc.GetType <> swDocumentTypes_e.swDocASSEMBLY Then Exit Sub

            Dim activePath As String = ""
            Try
                activePath = activeDoc.GetPathName()
            Catch
                activePath = ""
            End Try

            If String.IsNullOrWhiteSpace(activePath) Then Exit Sub

            If String.Equals(activePath, lastAutoPreSyncAssemblyPath, StringComparison.OrdinalIgnoreCase) AndAlso
               (DateTime.Now - lastAutoPreSyncStartedAt).TotalSeconds < AUTO_PRESYNC_MIN_SECONDS_BETWEEN_RUNS Then
                Exit Sub
            End If

            pendingAutoPreSyncAssemblyPath = activePath

            If autoPreSyncLevel1Timer IsNot Nothing Then
                autoPreSyncLevel1Timer.Stop()
                autoPreSyncLevel1Timer.Start()
            End If
        Catch
        End Try
    End Sub

    Private Sub startAutoPreSyncLevel1IfReady()
        Try
            If iSwApp Is Nothing Then Exit Sub
            If Not onlineCheckBox.Checked Then Exit Sub
            If syncStatusInProgress Then Exit Sub

            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDoc Is Nothing Then Exit Sub
            If activeDoc.GetType <> swDocumentTypes_e.swDocASSEMBLY Then Exit Sub

            Dim activePath As String = ""
            Try
                activePath = activeDoc.GetPathName()
            Catch
                activePath = ""
            End Try

            If String.IsNullOrWhiteSpace(activePath) Then Exit Sub
            If String.IsNullOrWhiteSpace(pendingAutoPreSyncAssemblyPath) Then Exit Sub
            If Not String.Equals(activePath, pendingAutoPreSyncAssemblyPath, StringComparison.OrdinalIgnoreCase) Then Exit Sub

            If String.Equals(activePath, lastAutoPreSyncAssemblyPath, StringComparison.OrdinalIgnoreCase) AndAlso
               (DateTime.Now - lastAutoPreSyncStartedAt).TotalSeconds < AUTO_PRESYNC_MIN_SECONDS_BETWEEN_RUNS Then
                Exit Sub
            End If

            Dim rootNode As TreeNode = getRootTreeNodeForSync()
            If rootNode Is Nothing Then Exit Sub

            loadImmediateChildrenForNode(rootNode)

            Dim level1Paths() As String = collectImmediateChildCadPathsForSync(rootNode)
            If level1Paths Is Nothing OrElse level1Paths.Length = 0 Then Exit Sub

            lastAutoPreSyncAssemblyPath = activePath
            lastAutoPreSyncStartedAt = DateTime.Now

            startAsyncSyncStatus(level1Paths, "Pre-syncing Level 1...", True)
        Catch
        End Try
    End Sub

    Friend Sub myInitialize(ByRef swAppin As SldWorks)
        'Allows for swApp to be passed into this class.
        iSwApp = swAppin

        initializeSwModelFunctions(iSwApp)
        svnModuleInitialize(iSwApp, Me, statusOfAllOpenModels)

        localRepoPath.Text = My.Settings.localRepoPath
        versionLabel.Text = "Version: 2026.02.12"

        ToolStripSplitButFolder.DropDown.AutoClose = True

        ensureSyncStatusButton()

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

        'Fast normal Get Locks:
        'Tree selection wins. If a child part is selected in the add-in tree, lock that file only.
        'Do not let SOLIDWORKS edit-context make us accidentally target the parent assembly.
        Dim selectedTreePaths() As String = getSelectedTreeCadPathsForFileAction()
        If selectedTreePaths IsNot Nothing AndAlso selectedTreePaths.Length > 0 Then
            getLocksOfPathsAsync(selectedTreePaths)
        Else
            getLocksOfDocsAsync(GetSelectedModDocList(iSwApp))
        End If

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

        'Fast normal Commit:
        'Tree selection wins. This lets a user commit a locked child part without needing
        'the parent assembly checked out, unless the parent itself was actually changed.
        Dim selectedTreePaths() As String = getSelectedTreeCadPathsForFileAction()
        If selectedTreePaths IsNot Nothing AndAlso selectedTreePaths.Length > 0 Then
            tortCommitPathsAsync(selectedTreePaths)
        Else
            tortCommitDocsAsync(GetSelectedModDocList(iSwApp))
        End If

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
        Dim response As Integer = iSwApp.SendMsgToUser2(
            "You are about to run Get Latest on the whole SVN working copy / whole car." & vbCrLf & vbCrLf &
            "This can take a long time and is not recommended for large assemblies unless you really need the entire car updated." & vbCrLf & vbCrLf &
            "Continue?",
            swMessageBoxIcon_e.swMbWarning,
            swMessageBoxBtn_e.swMbYesNo
        )

        If response <> swMessageBoxResult_e.swMbHitYes Then Exit Sub

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

    Private Sub butSyncStatus_Click(sender As Object, e As EventArgs) Handles butSyncStatus.Click
        'Normal click syncs the selected branch only.
        'Shift+click is the explicit slow whole-car status sync.
        If (ModifierKeys And Keys.Shift) = Keys.Shift Then
            performSyncStatusWholeCar()
        Else
            performSyncStatus()
        End If
    End Sub

    Private Sub performSyncStatus()
        'Async Sync Status:
        'Collect/load tree paths on the SolidWorks/UI thread, then run SVN server checks in the background.
        'This keeps SolidWorks usable while SVN talks to the server.

        If iSwApp.GetDocumentCount() = 0 Then
            iSwApp.SendMsgToUser2("No open SolidWorks documents to sync status for.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If Not onlineCheckBox.Checked Then
            iSwApp.SendMsgToUser2("Online mode is off. Turn on Online before using Sync Status.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Dim selectedNode As TreeNode = getSelectedTreeNodeForSync()
        Dim syncPaths() As String = Nothing

        If selectedNode Is Nothing Then
            'No tree node selected:
            'Default to Level 1 only under the active/root assembly.
            Dim rootNode As TreeNode = getRootTreeNodeForSync()

            If rootNode IsNot Nothing Then
                loadImmediateChildrenForNode(rootNode)
                syncPaths = collectImmediateChildCadPathsForSync(rootNode)
            End If
        Else
            'Selected Level 0 -> sync selected node + Level 1.
            'Selected Level 1 -> sync selected node + Level 2.
            loadImmediateChildrenForNode(selectedNode)
            syncPaths = collectSelectedBranchCadPathsForSync(selectedNode)
        End If

        If syncPaths Is Nothing OrElse syncPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No CAD file paths were found in the selected tree branch to sync.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        startAsyncSyncStatus(syncPaths, "Syncing...")
    End Sub

    Private Sub performSyncStatusWholeCar()
        'Explicit slow operation. This recursively loads the visible active assembly tree
        'and server-checks every CAD path it can find. It does NOT download geometry.

        Dim response As Integer = iSwApp.SendMsgToUser2(
            "You are about to Sync Status for the whole visible car / active assembly tree." & vbCrLf & vbCrLf &
            "This recursively loads branches and checks many CAD files against the SVN server." & vbCrLf &
            "It can take a long time and is not recommended for large assemblies unless you really need the full-car status." & vbCrLf & vbCrLf &
            "This does NOT download geometry. Use Get Latest for that." & vbCrLf & vbCrLf &
            "Continue?",
            swMessageBoxIcon_e.swMbWarning,
            swMessageBoxBtn_e.swMbYesNo
        )

        If response <> swMessageBoxResult_e.swMbHitYes Then Exit Sub

        If iSwApp.GetDocumentCount() = 0 Then
            iSwApp.SendMsgToUser2("No open SolidWorks documents to sync status for.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If Not onlineCheckBox.Checked Then
            iSwApp.SendMsgToUser2("Online mode is off. Turn on Online before using Sync Status.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Try
            If TreeView1 IsNot Nothing AndAlso TreeView1.Nodes IsNot Nothing Then
                TreeView1.BeginUpdate()
                Try
                    For Each node As TreeNode In TreeView1.Nodes
                        loadEntireLazyTree(node)
                    Next
                Finally
                    TreeView1.EndUpdate()
                End Try
            End If
        Catch
        End Try

        Dim syncPaths() As String = collectCurrentTreeCadPaths()

        If syncPaths Is Nothing OrElse syncPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No CAD file paths were found in the current tree to sync.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        startAsyncSyncStatus(syncPaths, "Syncing whole car...")
    End Sub

    Private Sub startAsyncSyncStatus(ByVal syncPaths() As String,
                                     Optional ByVal pendingText As String = "Syncing...",
                                     Optional ByVal isAutoPreSync As Boolean = False)
        If syncPaths Is Nothing OrElse syncPaths.Length = 0 Then Exit Sub

        If syncStatusInProgress Then
            If Not isAutoPreSync Then
                iSwApp.SendMsgToUser2("A Sync Status operation is already running in the background.",
                    swMessageBoxIcon_e.swMbInformation,
                    swMessageBoxBtn_e.swMbOk)
            End If
            Exit Sub
        End If

        Dim pathsForBackground As String() = CType(syncPaths.Clone(), String())
        Dim savedPathForBackground As String = savedPATH
        Dim uiStartTimestamp As DateTime = DateTime.Now

        syncStatusInProgress = True
        markSyncPendingForFilePathsPublic(pathsForBackground, True, pendingText)
        setSyncProgressVisible(True, pendingText, pathsForBackground.Length)

        System.Threading.Tasks.Task.Run(Sub()
                                            Dim errorMessage As String = ""
                                            Dim serverStatus As SVNStatus = Nothing
                                            Dim timingLog As String = ""

                                            Try
                                                serverStatus = svnModule.getServerStatusForFilePathsBackgroundPublic(pathsForBackground, savedPathForBackground, errorMessage, timingLog)
                                            Catch ex As Exception
                                                errorMessage = ex.Message
                                            End Try

                                            Try
                                                If Me.IsHandleCreated Then
                                                    Me.BeginInvoke(New MethodInvoker(Sub() finishAsyncSyncStatus(pathsForBackground, serverStatus, errorMessage, timingLog, uiStartTimestamp, isAutoPreSync, pendingText)))
                                                Else
                                                    syncStatusInProgress = False
                                                End If
                                            Catch
                                                syncStatusInProgress = False
                                            End Try
                                        End Sub)
    End Sub

    Private Sub finishAsyncSyncStatus(ByVal syncPaths() As String,
                                      ByVal serverStatus As SVNStatus,
                                      ByVal errorMessage As String,
                                      ByVal timingLog As String,
                                      ByVal uiStartTimestamp As DateTime,
                                      Optional ByVal isAutoPreSync As Boolean = False,
                                      Optional ByVal syncLabel As String = "Syncing...")
        Dim finishSw As System.Diagnostics.Stopwatch = System.Diagnostics.Stopwatch.StartNew()
        Dim finishLines As New List(Of String)()

        Try
            markSyncPendingForFilePathsPublic(syncPaths, False)
        Catch
        End Try

        Try
            setSyncProgressVisible(False)
        Catch
        End Try

        syncStatusInProgress = False

        If Not String.IsNullOrWhiteSpace(errorMessage) Then
            Dim failedLog As String = buildSyncTimingLogWithContext(timingLog, isAutoPreSync, syncLabel) &
                                      vbCrLf & "UI finish: failed before cache/tree update"
            appendSyncTimingLog(failedLog)

            If (Not isAutoPreSync) OrElse debugIgnoreNamingConventionEnabled() Then
                iSwApp.SendMsgToUser2("Sync Status failed." & vbCrLf & vbCrLf & errorMessage,
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk)
            End If
            Exit Sub
        End If

        If serverStatus Is Nothing Then
            Dim failedLog As String = buildSyncTimingLogWithContext(timingLog, isAutoPreSync, syncLabel) &
                                      vbCrLf & "UI finish: failed because no SVN status was returned"
            appendSyncTimingLog(failedLog)

            If (Not isAutoPreSync) OrElse debugIgnoreNamingConventionEnabled() Then
                iSwApp.SendMsgToUser2("Sync Status failed. No SVN status was returned.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk)
            End If
            Exit Sub
        End If

        Dim applySw As System.Diagnostics.Stopwatch = System.Diagnostics.Stopwatch.StartNew()
        Try
            svnModule.applyServerStatusFromBackgroundPublic(serverStatus)
        Catch
        End Try
        applySw.Stop()
        finishLines.Add("UI apply cache: " & applySw.ElapsedMilliseconds.ToString() & " ms")

        Dim persistentSaveSw As System.Diagnostics.Stopwatch = System.Diagnostics.Stopwatch.StartNew()
        Try
            svnModule.savePersistentStatusCachePublic(serverStatus)
        Catch
        End Try
        persistentSaveSw.Stop()
        finishLines.Add("UI save persistent cache: " & persistentSaveSw.ElapsedMilliseconds.ToString() & " ms")

        Dim recolorSw As System.Diagnostics.Stopwatch = System.Diagnostics.Stopwatch.StartNew()
        Try
            recolorTreeNodesForFilePathsPublic(syncPaths)
        Catch
            Try
                recolorCurrentTreeFromStatus()
            Catch
            End Try
        End Try
        recolorSw.Stop()
        finishLines.Add("UI recolor synced nodes: " & recolorSw.ElapsedMilliseconds.ToString() & " ms")

        Try
            setRefreshTreeButtonNormal()
        Catch
        End Try

        finishSw.Stop()
        finishLines.Add("UI finish total: " & finishSw.ElapsedMilliseconds.ToString() & " ms")
        finishLines.Add("End-to-end visible sync time: " & CLng((DateTime.Now - uiStartTimestamp).TotalMilliseconds).ToString() & " ms")

        Dim fullTimingLog As String = buildSyncTimingLogWithContext(timingLog, isAutoPreSync, syncLabel)
        If Not String.IsNullOrWhiteSpace(fullTimingLog) Then fullTimingLog &= vbCrLf
        fullTimingLog &= String.Join(vbCrLf, finishLines.ToArray())

        appendSyncTimingLog(fullTimingLog)

        If debugIgnoreNamingConventionEnabled() Then
            Dim displayTiming As String = fullTimingLog
            If displayTiming.Length > 3000 Then
                displayTiming = displayTiming.Substring(0, 3000) & vbCrLf & "... timing log truncated. Full log is in %TEMP%\PlumVault_SyncTiming.log"
            End If

            Dim popupTitle As String = If(isAutoPreSync, "Auto Level 1 Pre-Sync Timing Log", "Sync Timing Log")
            iSwApp.SendMsgToUser2(popupTitle & vbCrLf & vbCrLf & displayTiming,
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
        End If
    End Sub

    Private Function buildSyncTimingLogWithContext(ByVal timingLog As String,
                                                   ByVal isAutoPreSync As Boolean,
                                                   ByVal syncLabel As String) As String
        Dim contextLines As New List(Of String)()

        If isAutoPreSync Then
            contextLines.Add("Sync source: AUTO Level 1 Pre-Sync")
        Else
            contextLines.Add("Sync source: Manual Sync")
        End If

        If Not String.IsNullOrWhiteSpace(syncLabel) Then
            contextLines.Add("Sync label: " & syncLabel)
        End If

        If Not String.IsNullOrWhiteSpace(pendingAutoPreSyncAssemblyPath) AndAlso isAutoPreSync Then
            contextLines.Add("Auto pre-sync assembly: " & pendingAutoPreSyncAssemblyPath)
        End If

        Dim contextText As String = String.Join(vbCrLf, contextLines.ToArray())

        If String.IsNullOrWhiteSpace(timingLog) Then Return contextText
        Return contextText & vbCrLf & timingLog
    End Function

    Private Sub applyPersistentStatusCacheForVisibleTree(Optional ByVal sourceLabel As String = "")
        Try
            If TreeView1 Is Nothing Then Exit Sub
            If TreeView1.Nodes Is Nothing OrElse TreeView1.Nodes.Count = 0 Then Exit Sub

            Dim cachePaths() As String = collectCurrentTreeCadPaths()
            If cachePaths Is Nothing OrElse cachePaths.Length = 0 Then Exit Sub

            Dim loadedCount As Integer = 0
            Dim newestAgeSeconds As Long = -1

            If svnModule.applyPersistentStatusCacheForFilePathsPublic(cachePaths, loadedCount, newestAgeSeconds) Then
                Try
                    recolorTreeNodesForFilePathsPublic(cachePaths)
                Catch
                    Try
                        recolorCurrentTreeFromStatus()
                    Catch
                    End Try
                End Try

                showPersistentCacheStatusLabel(loadedCount, newestAgeSeconds)

                Try
                    appendSyncTimingLog("Persistent cache applied" & vbCrLf &
                                        "Source: " & sourceLabel & vbCrLf &
                                        "Input paths: " & cachePaths.Length.ToString() & vbCrLf &
                                        "Cached entries applied: " & loadedCount.ToString() & vbCrLf &
                                        "Newest cache age seconds: " & newestAgeSeconds.ToString())
                Catch
                End Try
            End If
        Catch
        End Try
    End Sub

    Private Sub showPersistentCacheStatusLabel(ByVal loadedCount As Integer, ByVal newestAgeSeconds As Long)
        Try
            If loadedCount <= 0 Then Exit Sub
            If syncProgressLabel Is Nothing Then Exit Sub

            Dim ageText As String = formatCacheAgeForDisplay(newestAgeSeconds)
            syncProgressLabel.Text = "Cached status shown (" & loadedCount.ToString() & " files" & ageText & ")"
            syncProgressLabel.Visible = True

            If syncProgressBar IsNot Nothing Then
                syncProgressBar.MarqueeAnimationSpeed = 0
                syncProgressBar.Visible = False
            End If

            syncProgressLabel.BringToFront()
        Catch
        End Try
    End Sub

    Private Function formatCacheAgeForDisplay(ByVal newestAgeSeconds As Long) As String
        If newestAgeSeconds < 0 Then Return ""

        Try
            If newestAgeSeconds < 60 Then
                Return ", " & newestAgeSeconds.ToString() & " sec old"
            End If

            Dim minutes As Long = newestAgeSeconds \ 60
            If minutes < 60 Then
                Return ", " & minutes.ToString() & " min old"
            End If

            Dim hours As Long = minutes \ 60
            If hours < 48 Then
                Return ", " & hours.ToString() & " hr old"
            End If

            Dim days As Long = hours \ 24
            Return ", " & days.ToString() & " days old"
        Catch
            Return ""
        End Try
    End Function

    Private Sub appendSyncTimingLog(ByVal timingLog As String)
        If String.IsNullOrWhiteSpace(timingLog) Then Exit Sub

        Try
            Dim logText As String = vbCrLf & "========== PlumVault Sync Timing " & DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff") & " ==========" & vbCrLf & timingLog & vbCrLf

            Try
                System.Diagnostics.Debug.WriteLine(logText)
            Catch
            End Try

            Try
                File.AppendAllText(Path.Combine(Path.GetTempPath(), "PlumVault_SyncTiming.log"), logText)
            Catch
            End Try
        Catch
        End Try
    End Sub

    Private Function getSelectedTreeNodeForSync() As TreeNode
        Try
            If TreeView1 Is Nothing Then Return Nothing

            Dim selectedNode As TreeNode = TreeView1.SelectedNode
            If selectedNode Is Nothing Then Return Nothing

            'Important safety fix:
            'After a refresh/rebuild, the root node can remain selected automatically.
            'If the user simply clicks Sync, that used to behave like Level 0 was selected
            'and would sync the root assembly too. Treat an auto-selected root as
            '"nothing selected" so default Sync remains Level 1 only.
            If selectedNode.Parent Is Nothing Then
                If lastUserClickedTreeNodeForSync Is Nothing Then Return Nothing
                If Not Object.ReferenceEquals(selectedNode, lastUserClickedTreeNodeForSync) Then Return Nothing
            End If

            Return selectedNode

        Catch
        End Try

        Return Nothing
    End Function

    Private Function getRootTreeNodeForSync() As TreeNode
        Try
            If TreeView1 Is Nothing Then Return Nothing

            If TreeView1.Nodes IsNot Nothing AndAlso TreeView1.Nodes.Count > 0 Then
                Return TreeView1.Nodes(0)
            End If
        Catch
        End Try

        Return Nothing
    End Function

    Private Function collectSelectedBranchCadPathsForSync(ByVal selectedNode As TreeNode) As String()
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If selectedNode IsNot Nothing Then
            addTreeNodePathToSyncList(selectedNode, seen, output)

            For Each childNode As TreeNode In selectedNode.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For
                addTreeNodePathToSyncList(childNode, seen, output)
            Next
        End If

        'Never fall back to collectCurrentTreeCadPaths() here.
        'That fallback turns a failed/empty selected-branch sync into a whole loaded-tree sync,
        'which breaks the Level 0 / Level 1 / Level 2 controls.
        If output.Count = 0 Then Return Nothing

        Return output.ToArray()
    End Function
    Private Function collectImmediateChildCadPathsForSync(ByVal parentNode As TreeNode) As String()
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If parentNode IsNot Nothing Then
            For Each childNode As TreeNode In parentNode.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For
                addTreeNodePathToSyncList(childNode, seen, output)
            Next
        End If

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Sub addTreeNodePathToSyncList(ByVal node As TreeNode,
                                          ByVal seen As HashSet(Of String),
                                          ByVal output As List(Of String))
        If node Is Nothing Then Exit Sub
        If isLazyPlaceholderNode(node) Then Exit Sub

        Dim nodePath As String = getCadPathFromTreeNode(node)

        If Not isCadPathForSync(nodePath) Then Exit Sub

        Try
            nodePath = Path.GetFullPath(nodePath)
        Catch
        End Try

        If seen.Contains(nodePath) Then Exit Sub

        seen.Add(nodePath)
        output.Add(nodePath)
    End Sub

    Private Function collectCurrentTreeCadPaths() As String()
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            If TreeView1 IsNot Nothing AndAlso TreeView1.Nodes IsNot Nothing AndAlso TreeView1.Nodes.Count > 0 Then
                For Each node As TreeNode In TreeView1.Nodes
                    collectCadPathsFromTreeNode(node, seen, output)
                Next
            End If
        Catch
        End Try

        If output.Count = 0 Then
            Try
                Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)

                If activeDoc IsNot Nothing Then
                    Dim activePath As String = activeDoc.GetPathName()

                    If isCadPathForSync(activePath) AndAlso Not seen.Contains(activePath) Then
                        seen.Add(activePath)
                        output.Add(activePath)
                    End If
                End If
            Catch
            End Try
        End If

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Sub collectCadPathsFromTreeNode(ByVal node As TreeNode,
                                            ByVal seen As HashSet(Of String),
                                            ByVal output As List(Of String))
        If node Is Nothing Then Exit Sub

        Dim nodePath As String = getCadPathFromTreeNode(node)

        If isCadPathForSync(nodePath) Then
            Try
                nodePath = Path.GetFullPath(nodePath)
            Catch
            End Try

            If Not seen.Contains(nodePath) Then
                seen.Add(nodePath)
                output.Add(nodePath)
            End If
        End If

        For Each childNode As TreeNode In node.Nodes
            collectCadPathsFromTreeNode(childNode, seen, output)
        Next
    End Sub

    Private Function getCadPathFromTreeNode(ByVal node As TreeNode) As String
        If node Is Nothing Then Return ""

        Try
            If TypeOf node.Tag Is ModelDoc2 Then
                Return CType(node.Tag, ModelDoc2).GetPathName()
            End If

            If TypeOf node.Tag Is Component2 Then
                Return CType(node.Tag, Component2).GetPathName()
            End If
        Catch
        End Try

        Return ""
    End Function

    Private Function isCadPathForSync(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not File.Exists(filePath) Then Return False

        Dim ext As String = ""

        Try
            ext = Path.GetExtension(filePath).ToUpperInvariant()
        Catch
            Return False
        End Try

        Return ext = ".SLDPRT" OrElse ext = ".SLDASM" OrElse ext = ".SLDDRW"
    End Function

    Private Function getSelectedTreeCadPathForFileAction() As String
        'Fast action helper:
        'If the user selected a node in the add-in tree, normal Get Locks / Commit
        'should act on that exact file path, not whatever SOLIDWORKS currently thinks
        'the active/edited document is. This prevents child part commits from accidentally
        'trying to commit/check out the parent assembly.
        Try
            If TreeView1 Is Nothing Then Return ""
            If TreeView1.SelectedNode Is Nothing Then Return ""
            If isLazyPlaceholderNode(TreeView1.SelectedNode) Then Return ""

            Dim nodePath As String = getCadPathFromTreeNode(TreeView1.SelectedNode)

            If Not isCadPathForSync(nodePath) Then Return ""

            Try
                nodePath = Path.GetFullPath(nodePath)
            Catch
            End Try

            Return nodePath
        Catch
            Return ""
        End Try
    End Function

    Private Function getSelectedTreeCadPathsForFileAction() As String()
        Dim selectedPath As String = getSelectedTreeCadPathForFileAction()
        If String.IsNullOrWhiteSpace(selectedPath) Then Return Nothing
        Return New String() {selectedPath}
    End Function

    Private Sub recolorCurrentTreeFromStatus()
        Try
            If TreeView1 IsNot Nothing Then
                For Each node As TreeNode In TreeView1.Nodes
                    recolorTreeNodeRecursive(node)
                Next
            End If
        Catch
        End Try

        Try
            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDoc Is Nothing Then Exit Sub

            Dim activePath As String = activeDoc.GetPathName()
            If String.IsNullOrWhiteSpace(activePath) Then Exit Sub

            Dim treeIndex As Integer = findStoredTreeView(activePath, bRetryWithRefresh:=False)

            If treeIndex >= 0 AndAlso allTreeViews IsNot Nothing AndAlso treeIndex <= UBound(allTreeViews) Then
                If allTreeViews(treeIndex) IsNot Nothing Then
                    For Each node As TreeNode In allTreeViews(treeIndex).Nodes
                        recolorTreeNodeRecursive(node)
                    Next
                End If
            End If
        Catch
        End Try
    End Sub


    Public Sub recolorCurrentTreeFromStatusPublic()
        recolorCurrentTreeFromStatus()
    End Sub
    Private Sub recolorTreeNodeRecursive(ByVal node As TreeNode)
        If node Is Nothing Then Exit Sub

        setNodeColorFromStatus(node)

        For Each childNode As TreeNode In node.Nodes
            recolorTreeNodeRecursive(childNode)
        Next
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

        If Not verifyLocalRepoPath(, bCheckLocalFolder:=True, bCheckServer:=False) Then Return False

        'Speed fix:
        'Do not scan every subfolder or run server/all-tree refresh when the add-in refreshes.
        'Use the same lightweight path as the Refresh Tree button.
        If iSwApp IsNot Nothing AndAlso iSwApp.GetDocumentCount() > 0 Then
            performLightweightRefresh()
        End If

        If bsaveLocalRepoPathSettings Then
            saveLocalRepoPathSettings()
        End If

        Return True
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

        Try
            If e IsNot Nothing AndAlso e.Node IsNot Nothing Then
                TreeView1.SelectedNode = e.Node
                lastUserClickedTreeNodeForSync = e.Node
                selectSolidWorksObjectForTreeNode(e.Node)
            End If
        Catch
        End Try

    End Sub

    Private Sub solidWorksSelectionMirrorTimer_Tick(sender As Object, e As EventArgs) Handles solidWorksSelectionMirrorTimer.Tick
        mirrorSolidWorksSelectionToTree()
    End Sub

    Private Sub selectSolidWorksObjectForTreeNode(ByVal node As TreeNode)
        If node Is Nothing Then Exit Sub
        If iSwApp Is Nothing Then Exit Sub
        If treeToSolidWorksSelectionInProgress Then Exit Sub
        If solidWorksToTreeSelectionInProgress Then Exit Sub

        Try
            treeToSolidWorksSelectionInProgress = True

            Dim activeModel As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeModel Is Nothing Then Exit Sub

            If TypeOf node.Tag Is Component2 Then
                Dim comp As Component2 = CType(node.Tag, Component2)

                Try
                    activeModel.ClearSelection2(True)
                Catch
                End Try

                Try
                    comp.Select(False)
                Catch
                    Try
                        comp.Select4(False, Nothing, False)
                    Catch
                    End Try
                End Try

                lastMirroredSolidWorksSelectionKey = buildComponentSelectionKey(comp)
                Exit Sub
            End If

            If TypeOf node.Tag Is ModelDoc2 Then
                Dim nodeDoc As ModelDoc2 = CType(node.Tag, ModelDoc2)
                Dim nodePath As String = ""

                Try
                    nodePath = nodeDoc.GetPathName()
                Catch
                    nodePath = ""
                End Try

                'The top-level assembly document itself is not a Component2 instance, so there is
                'nothing useful to select in graphics. Clear the graphics selection but keep the
                'plugin tree node highlighted.
                Try
                    If Not String.IsNullOrWhiteSpace(nodePath) AndAlso
                       String.Equals(Path.GetFullPath(nodePath), Path.GetFullPath(activeModel.GetPathName()), StringComparison.OrdinalIgnoreCase) Then
                        activeModel.ClearSelection2(True)
                    End If
                Catch
                End Try

                lastMirroredSolidWorksSelectionKey = normalizePathForNodeMatch(nodePath)
            End If

        Catch
        Finally
            treeToSolidWorksSelectionInProgress = False
        End Try
    End Sub

    Private Sub mirrorSolidWorksSelectionToTree()
        If iSwApp Is Nothing Then Exit Sub
        If TreeView1 Is Nothing Then Exit Sub
        If treeToSolidWorksSelectionInProgress Then Exit Sub
        If solidWorksToTreeSelectionInProgress Then Exit Sub

        Try
            Dim selectedComp As Component2 = Nothing
            Dim selectedPath As String = ""
            Dim selectionKey As String = ""

            If Not tryGetPrimarySolidWorksSelectedComponent(selectedComp, selectedPath, selectionKey) Then Exit Sub
            If String.IsNullOrWhiteSpace(selectionKey) Then Exit Sub

            If String.Equals(selectionKey, lastMirroredSolidWorksSelectionKey, StringComparison.OrdinalIgnoreCase) Then
                Exit Sub
            End If

            Dim matchedNode As TreeNode = findTreeNodeForSolidWorksSelection(selectedComp, selectedPath)

            'If the selected component lives inside a lazy/unexpanded branch, load only
            'that parent chain and try again. This keeps deep children selectable from
            'the SOLIDWORKS graphics area or FeatureManager tree without expanding the
            'whole car.
            If matchedNode Is Nothing AndAlso selectedComp IsNot Nothing Then
                Try
                    If tryLoadLazyBranchForSolidWorksComponent(selectedComp) Then
                        matchedNode = findTreeNodeForSolidWorksSelection(selectedComp, selectedPath)
                    End If
                Catch
                End Try
            End If

            If matchedNode Is Nothing Then Exit Sub

            solidWorksToTreeSelectionInProgress = True

            Try
                TreeView1.SelectedNode = matchedNode
                matchedNode.EnsureVisible()
                lastUserClickedTreeNodeForSync = matchedNode
                lastMirroredSolidWorksSelectionKey = selectionKey
            Finally
                solidWorksToTreeSelectionInProgress = False
            End Try

        Catch
            solidWorksToTreeSelectionInProgress = False
        End Try
    End Sub

    Private Function tryGetPrimarySolidWorksSelectedComponent(ByRef selectedComp As Component2,
                                                             ByRef selectedPath As String,
                                                             ByRef selectionKey As String) As Boolean
        selectedComp = Nothing
        selectedPath = ""
        selectionKey = ""

        Try
            Dim activeModel As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeModel Is Nothing Then Return False

            If Not ((activeModel.GetType() = swDocumentTypes_e.swDocPART) OrElse
                    (activeModel.GetType() = swDocumentTypes_e.swDocASSEMBLY)) Then
                Return False
            End If

            Dim swSelMgr As SelectionMgr = activeModel.SelectionManager
            If swSelMgr Is Nothing Then Return False

            Dim nSelCount As Integer = 0

            Try
                nSelCount = swSelMgr.GetSelectedObjectCount2(-1)
            Catch
                nSelCount = 0
            End Try

            If nSelCount <= 0 Then Return False

            For i As Integer = 1 To nSelCount
                Dim comp As Component2 = Nothing

                Try
                    comp = swSelMgr.GetSelectedObjectsComponent4(i, -1)
                Catch
                    comp = Nothing
                End Try

                If comp IsNot Nothing Then
                    selectedComp = comp
                    selectedPath = getSafeComponentPath(comp)
                    selectionKey = buildComponentSelectionKey(comp)

                    If String.IsNullOrWhiteSpace(selectionKey) Then
                        selectionKey = normalizePathForNodeMatch(selectedPath)
                    End If

                    Return Not String.IsNullOrWhiteSpace(selectionKey)
                End If
            Next

            'If a part document is open by itself, selecting a face/sketch/body will not have a Component2.
            'In that case, mirror the selection to the root node for that part file if it is loaded in the plugin tree.
            If activeModel.GetType() = swDocumentTypes_e.swDocPART Then
                Try
                    selectedPath = activeModel.GetPathName()
                    selectionKey = normalizePathForNodeMatch(selectedPath)
                    Return Not String.IsNullOrWhiteSpace(selectionKey)
                Catch
                End Try
            End If

        Catch
        End Try

        Return False
    End Function

    Private Function buildComponentSelectionKey(ByVal comp As Component2) As String
        If comp Is Nothing Then Return ""

        Try
            Dim compName As String = comp.Name2
            Dim compPath As String = normalizePathForNodeMatch(getSafeComponentPath(comp))

            If Not String.IsNullOrWhiteSpace(compName) Then
                Return compName & "|" & compPath
            End If

            Return compPath
        Catch
            Return ""
        End Try
    End Function

    Private Function findTreeNodeForSolidWorksSelection(ByVal selectedComp As Component2,
                                                       ByVal selectedPath As String) As TreeNode
        Try
            If TreeView1 Is Nothing OrElse TreeView1.Nodes Is Nothing Then Return Nothing

            For Each rootNode As TreeNode In TreeView1.Nodes
                Dim exactCompNode As TreeNode = findTreeNodeByComponentInstance(rootNode, selectedComp)
                If exactCompNode IsNot Nothing Then Return exactCompNode
            Next

            For Each rootNode As TreeNode In TreeView1.Nodes
                Dim pathNode As TreeNode = findTreeNodeByCadPath(rootNode, selectedPath)
                If pathNode IsNot Nothing Then Return pathNode
            Next
        Catch
        End Try

        Return Nothing
    End Function

    Private Function tryLoadLazyBranchForSolidWorksComponent(ByVal selectedComp As Component2) As Boolean
        If selectedComp Is Nothing Then Return False
        If TreeView1 Is Nothing OrElse TreeView1.Nodes Is Nothing OrElse TreeView1.Nodes.Count = 0 Then Return False

        Try
            Dim componentChain As List(Of Component2) = getComponentParentChain(selectedComp)
            If componentChain Is Nothing OrElse componentChain.Count = 0 Then Return False

            'Try to anchor the chain on an already-loaded plugin node.
            'This may be a Level-1 assembly, a loaded subassembly, or the selected part itself.
            For chainIndex As Integer = 0 To componentChain.Count - 1
                Dim anchorComp As Component2 = componentChain(chainIndex)
                Dim anchorNode As TreeNode = Nothing

                For Each rootNode As TreeNode In TreeView1.Nodes
                    anchorNode = findTreeNodeByComponentInstance(rootNode, anchorComp)
                    If anchorNode IsNot Nothing Then Exit For
                Next

                If anchorNode IsNot Nothing Then
                    Return loadRemainingComponentChainFromNode(anchorNode, componentChain, chainIndex + 1)
                End If
            Next

            'If the first chain item is the invisible SOLIDWORKS root component, anchor on
            'the plugin root node and walk downward from there.
            Dim pluginRoot As TreeNode = getPluginTreeRootNodeForActiveDocument()
            If pluginRoot IsNot Nothing Then
                loadImmediateChildrenForNode(pluginRoot)
                pluginRoot.Expand()

                For chainIndex As Integer = 0 To componentChain.Count - 1
                    Dim firstVisibleNode As TreeNode = findDirectChildNodeForComponent(pluginRoot, componentChain(chainIndex))
                    If firstVisibleNode IsNot Nothing Then
                        Return loadRemainingComponentChainFromNode(firstVisibleNode, componentChain, chainIndex + 1)
                    End If
                Next
            End If

        Catch
        End Try

        Return False
    End Function

    Private Function getComponentParentChain(ByVal leafComp As Component2) As List(Of Component2)
        Dim chain As New List(Of Component2)()
        If leafComp Is Nothing Then Return chain

        Dim currentComp As Component2 = leafComp
        Dim safetyCounter As Integer = 0

        Do While currentComp IsNot Nothing AndAlso safetyCounter < 50
            chain.Insert(0, currentComp)
            safetyCounter += 1

            Try
                currentComp = TryCast(currentComp.GetParent(), Component2)
            Catch
                currentComp = Nothing
            End Try
        Loop

        Return chain
    End Function

    Private Function loadRemainingComponentChainFromNode(ByVal anchorNode As TreeNode,
                                                        ByVal componentChain As List(Of Component2),
                                                        ByVal nextChainIndex As Integer) As Boolean
        If anchorNode Is Nothing Then Return False
        If componentChain Is Nothing Then Return False

        Try
            Dim currentNode As TreeNode = anchorNode

            For i As Integer = nextChainIndex To componentChain.Count - 1
                If currentNode Is Nothing Then Return False

                loadImmediateChildrenForNode(currentNode)
                currentNode.Expand()

                Dim nextNode As TreeNode = findDirectChildNodeForComponent(currentNode, componentChain(i))

                If nextNode Is Nothing Then
                    'Fallback: search below the current loaded branch. This still only uses
                    'loaded nodes and does not force a whole-car load.
                    nextNode = findTreeNodeByComponentInstance(currentNode, componentChain(i))
                End If

                If nextNode Is Nothing Then Return False

                currentNode = nextNode
            Next

            Return True

        Catch
            Return False
        End Try
    End Function

    Private Function getPluginTreeRootNodeForActiveDocument() As TreeNode
        Try
            If TreeView1 Is Nothing OrElse TreeView1.Nodes Is Nothing OrElse TreeView1.Nodes.Count = 0 Then Return Nothing

            Dim activeModel As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeModel Is Nothing Then Return TreeView1.Nodes(0)

            Dim activePath As String = normalizePathForNodeMatch(activeModel.GetPathName())

            For Each rootNode As TreeNode In TreeView1.Nodes
                Dim rootPath As String = normalizePathForNodeMatch(getCadPathFromTreeNode(rootNode))

                If activePath <> "" AndAlso rootPath <> "" AndAlso
                   String.Equals(activePath, rootPath, StringComparison.OrdinalIgnoreCase) Then
                    Return rootNode
                End If
            Next

            Return TreeView1.Nodes(0)
        Catch
            Return Nothing
        End Try
    End Function

    Private Function findDirectChildNodeForComponent(ByVal parentNode As TreeNode, ByVal targetComp As Component2) As TreeNode
        If parentNode Is Nothing OrElse targetComp Is Nothing Then Return Nothing

        Try
            For Each childNode As TreeNode In parentNode.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For

                If treeNodeMatchesComponent(childNode, targetComp) Then
                    Return childNode
                End If
            Next
        Catch
        End Try

        Return Nothing
    End Function

    Private Function treeNodeMatchesComponent(ByVal node As TreeNode, ByVal targetComp As Component2) As Boolean
        If node Is Nothing OrElse targetComp Is Nothing Then Return False

        Try
            If Not TypeOf node.Tag Is Component2 Then Return False

            Dim nodeComp As Component2 = CType(node.Tag, Component2)

            Try
                If Object.ReferenceEquals(nodeComp, targetComp) Then Return True
            Catch
            End Try

            Dim nodeName As String = ""
            Dim targetName As String = ""

            Try
                nodeName = nodeComp.Name2
            Catch
                nodeName = ""
            End Try

            Try
                targetName = targetComp.Name2
            Catch
                targetName = ""
            End Try

            If Not String.IsNullOrWhiteSpace(nodeName) AndAlso
               String.Equals(nodeName, targetName, StringComparison.OrdinalIgnoreCase) Then
                Return True
            End If

            Dim nodePath As String = normalizePathForNodeMatch(getSafeComponentPath(nodeComp))
            Dim targetPath As String = normalizePathForNodeMatch(getSafeComponentPath(targetComp))

            If nodePath <> "" AndAlso targetPath <> "" AndAlso
               String.Equals(nodePath, targetPath, StringComparison.OrdinalIgnoreCase) Then
                Return True
            End If

        Catch
        End Try

        Return False
    End Function

    Private Function findTreeNodeByComponentInstance(ByVal node As TreeNode, ByVal selectedComp As Component2) As TreeNode
        If node Is Nothing OrElse selectedComp Is Nothing Then Return Nothing

        Try
            If TypeOf node.Tag Is Component2 Then
                Dim nodeComp As Component2 = CType(node.Tag, Component2)

                Try
                    If Object.ReferenceEquals(nodeComp, selectedComp) Then Return node
                Catch
                End Try

                Try
                    Dim nodeName As String = nodeComp.Name2
                    Dim selectedName As String = selectedComp.Name2

                    If Not String.IsNullOrWhiteSpace(nodeName) AndAlso
                       String.Equals(nodeName, selectedName, StringComparison.OrdinalIgnoreCase) Then
                        Return node
                    End If
                Catch
                End Try
            End If

            For Each childNode As TreeNode In node.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For

                Dim matchedChild As TreeNode = findTreeNodeByComponentInstance(childNode, selectedComp)
                If matchedChild IsNot Nothing Then Return matchedChild
            Next
        Catch
        End Try

        Return Nothing
    End Function

    Private Function findTreeNodeByCadPath(ByVal node As TreeNode, ByVal selectedPath As String) As TreeNode
        If node Is Nothing Then Return Nothing

        Dim normalizedSelectedPath As String = normalizePathForNodeMatch(selectedPath)
        If String.IsNullOrWhiteSpace(normalizedSelectedPath) Then Return Nothing

        Try
            Dim nodePath As String = normalizePathForNodeMatch(getCadPathFromTreeNode(node))

            If nodePath <> "" AndAlso
               String.Equals(nodePath, normalizedSelectedPath, StringComparison.OrdinalIgnoreCase) Then
                Return node
            End If

            For Each childNode As TreeNode In node.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For

                Dim matchedChild As TreeNode = findTreeNodeByCadPath(childNode, selectedPath)
                If matchedChild IsNot Nothing Then Return matchedChild
            Next
        Catch
        End Try

        Return Nothing
    End Function

    Private Sub TreeView1_BeforeExpand(sender As Object, e As TreeViewCancelEventArgs) Handles TreeView1.BeforeExpand
        Try
            loadImmediateChildrenForNode(e.Node)
        Catch
        End Try
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
        Dim activeFileName As String = System.IO.Path.GetFileName(pathName)

        If String.IsNullOrWhiteSpace(activeFileName) Then Return Nothing

        If allTreeViews IsNot Nothing AndAlso allTreeViews.Length > 0 Then
            For i As Integer = 0 To UBound(allTreeViews)
                If allTreeViews(i) Is Nothing Then Continue For
                If allTreeViews(i).Nodes.Count = 0 Then Continue For

                If Strings.InStr(allTreeViews(i).Nodes(0).Text, activeFileName, CompareMethod.Text) <> 0 Then
                    Return i
                End If
            Next
        End If

        If Not bRetryWithRefresh Then Return Nothing

        'Speed fix:
        'If the tree is missing, build only the active tree.
        'Do NOT run updateStatusOfAllModelsVariable(True), because that hits the server and rebuilds every tree.
        Try
            refreshCurrentTreeViewOnly()
        Catch
        End Try

        If allTreeViews IsNot Nothing AndAlso allTreeViews.Length > 0 Then
            For i As Integer = 0 To UBound(allTreeViews)
                If allTreeViews(i) Is Nothing Then Continue For
                If allTreeViews(i).Nodes.Count = 0 Then Continue For

                If Strings.InStr(allTreeViews(i).Nodes(0).Text, activeFileName, CompareMethod.Text) <> 0 Then
                    Return i
                End If
            Next
        End If

        Return Nothing
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
            getComponentsOfAssemblyOptionalUpdateTree({modDocArray(i)}, i, iTreeDepthLimit:=1)
        Next
    End Sub

    Public Sub refreshCurrentTreeViewOnly()
        Dim activeDoc As ModelDoc2 = iSwApp.ActiveDoc

        'Tree rebuilds can create/default-select a new root node.
        'Clear explicit sync selection so a plain Sync click remains Level-1-only.
        lastUserClickedTreeNodeForSync = Nothing

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
        getComponentsOfAssemblyOptionalUpdateTree({activeDoc}, treeIndex, iTreeDepthLimit:=1)
        switchTreeViewToCurrentModel(bRetryWithRefresh:=False)

        'Persistent metadata cache:
        'Show last-known status immediately after the shallow tree is built.
        'This does not contact SVN, does not download geometry, and does not resolve the full assembly.
        applyPersistentStatusCacheForVisibleTree("refreshCurrentTreeViewOnly")
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
                                    Optional ByVal bResolveLightweight As Boolean = False,
                                    Optional ByVal iTreeDepthLimit As Integer = -1) As ModelDoc2()

        If modDoc Is Nothing Then Return Nothing

        Dim modDocArr() As ModelDoc2 = {modDoc}

        Return getComponentsOfAssemblyOptionalUpdateTree(modDocArr, allTreeViewsIndexToUpdate, bUniqueOnly, bResolveLightweight, iTreeDepthLimit)
    End Function

    Public Function getComponentsOfAssemblyOptionalUpdateTree(
                                    ByRef modDocArr() As ModelDoc2,
                                    Optional ByVal allTreeViewsIndexToUpdate As Integer = -1,
                                    Optional ByVal bUniqueOnly As Boolean = True,
                                    Optional ByVal bResolveLightweight As Boolean = False,
                                    Optional ByVal iTreeDepthLimit As Integer = -1) As ModelDoc2()

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

                TraverseComponent(swRootComp, modelDocList, 1, parentNode, bUniqueOnly, bResolveLightweight, iTreeDepthLimit)

                If bUpdateTreeView AndAlso iTreeDepthLimit < 0 Then
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
                         Optional ByVal bResolveLightweight As Boolean = False,
                         Optional ByVal iTreeDepthLimit As Integer = -1)

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

                If bUC AndAlso iTreeDepthLimit >= 0 AndAlso nLevel >= iTreeDepthLimit Then
                    addModelDocIfMissing(mdComponentList, modDocChild, bUniqueOnly)

                    childNode = New TreeNode(buildComponentNodeText(swChildComp, modDocChild))
                    childNode.Tag = swChildComp
                    setNodeColorFromStatus(childNode)
                    addLazyPlaceholderIfNeeded(childNode)
                    parentNode.Nodes.Add(childNode)

                    Continue For
                End If

                If bUniqueOnly AndAlso modelDocListContainsPath(mdComponentList, getSafeModelPath(modDocChild)) Then
                    If bUC Then
                        childNode = New TreeNode(buildComponentNodeText(swChildComp, modDocChild))
                        childNode.Tag = swChildComp
                        setNodeColorFromStatus(childNode)
                        addLazyPlaceholderIfNeeded(childNode)
                        parentNode.Nodes.Add(childNode)
                    End If

                    Continue For
                End If

                TraverseComponent(swChildComp, mdComponentList, nLevel + 1, parentNode, bUniqueOnly, bResolveLightweight, iTreeDepthLimit)

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
    Private Function isLazyPlaceholderNode(ByVal node As TreeNode) As Boolean
        If node Is Nothing Then Return False
        Return String.Equals(node.Text, LAZY_LOAD_PLACEHOLDER_TEXT, StringComparison.OrdinalIgnoreCase)
    End Function

    Private Function hasLazyPlaceholder(ByVal node As TreeNode) As Boolean
        If node Is Nothing Then Return False
        If node.Nodes Is Nothing OrElse node.Nodes.Count = 0 Then Return False
        Return isLazyPlaceholderNode(node.Nodes(0))
    End Function

    Private Function isTreeNodeAssembly(ByVal node As TreeNode) As Boolean
        If node Is Nothing Then Return False

        Try
            If TypeOf node.Tag Is ModelDoc2 Then
                Return CType(node.Tag, ModelDoc2).GetType() = swDocumentTypes_e.swDocASSEMBLY
            End If

            If TypeOf node.Tag Is Component2 Then
                Dim comp As Component2 = CType(node.Tag, Component2)
                Dim compPath As String = getSafeComponentPath(comp)

                If Not String.IsNullOrWhiteSpace(compPath) Then
                    If String.Equals(Path.GetExtension(compPath), ".SLDASM", StringComparison.OrdinalIgnoreCase) Then
                        Return True
                    End If
                End If

                Dim compDoc As ModelDoc2 = TryCast(comp.GetModelDoc2(), ModelDoc2)
                If compDoc IsNot Nothing Then
                    Return compDoc.GetType() = swDocumentTypes_e.swDocASSEMBLY
                End If
            End If
        Catch
        End Try

        Return False
    End Function

    Private Sub addLazyPlaceholderIfNeeded(ByVal node As TreeNode)
        If node Is Nothing Then Exit Sub
        If Not isTreeNodeAssembly(node) Then Exit Sub
        If node.Nodes IsNot Nothing AndAlso node.Nodes.Count > 0 Then Exit Sub

        'Only add the placeholder if SolidWorks can actually provide children.
        'Suppressed/path-only assemblies can be shown, but cannot be expanded without resolving/opening them.
        If TypeOf node.Tag Is Component2 Then
            Try
                Dim comp As Component2 = CType(node.Tag, Component2)
                If isComponentSuppressedState(getSafeComponentSuppression(comp)) Then Exit Sub
                If comp.GetModelDoc2() Is Nothing Then Exit Sub
            Catch
                Exit Sub
            End Try
        End If

        node.Nodes.Add(New TreeNode(LAZY_LOAD_PLACEHOLDER_TEXT))
    End Sub

    Private Sub loadImmediateChildrenForNode(ByVal node As TreeNode)
        If node Is Nothing Then Exit Sub
        If Not isTreeNodeAssembly(node) Then Exit Sub

        If node.Nodes IsNot Nothing AndAlso node.Nodes.Count > 0 AndAlso Not hasLazyPlaceholder(node) Then
            Exit Sub
        End If

        Dim childObj As Object = Nothing

        Try
            If TypeOf node.Tag Is ModelDoc2 Then
                Dim asmDoc As AssemblyDoc = TryCast(node.Tag, AssemblyDoc)
                If asmDoc Is Nothing Then Exit Sub

                Dim modelDoc As ModelDoc2 = CType(node.Tag, ModelDoc2)
                Dim confMgr As ConfigurationManager = modelDoc.ConfigurationManager
                Dim conf As Configuration = confMgr.ActiveConfiguration
                Dim rootComp As Component2 = conf.GetRootComponent3(True)
                If rootComp Is Nothing Then Exit Sub

                childObj = rootComp.GetChildren()

            ElseIf TypeOf node.Tag Is Component2 Then
                Dim comp As Component2 = CType(node.Tag, Component2)

                If isComponentSuppressedState(getSafeComponentSuppression(comp)) Then Exit Sub

                childObj = comp.GetChildren()
            Else
                Exit Sub
            End If
        Catch
            childObj = Nothing
        End Try

        If childObj Is Nothing Then Exit Sub

        Dim childArr As Object() = Nothing

        Try
            childArr = CType(childObj, Object())
        Catch
            Exit Sub
        End Try

        node.Nodes.Clear()

        For Each child As Object In childArr
            Dim childComp As Component2 = TryCast(child, Component2)
            If childComp Is Nothing Then Continue For

            Try
                If childComp.IsEnvelope Then Continue For
            Catch
            End Try

            Dim childPath As String = getSafeComponentPath(childComp)
            Dim childSuppression As Integer = getSafeComponentSuppression(childComp)
            Dim childDoc As ModelDoc2 = Nothing

            If Not isComponentSuppressedState(childSuppression) Then
                Try
                    childDoc = TryCast(childComp.GetModelDoc2(), ModelDoc2)
                Catch
                    childDoc = Nothing
                End Try
            End If

            If String.IsNullOrWhiteSpace(childPath) AndAlso childDoc Is Nothing Then Continue For

            Dim childNode As New TreeNode(buildComponentNodeText(childComp, childDoc))
            childNode.Tag = childComp
            setNodeColorFromStatus(childNode)
            addLazyPlaceholderIfNeeded(childNode)
            node.Nodes.Add(childNode)
        Next

        Try
            node.TreeView.Sort()
        Catch
        End Try

        'When a lazy branch is expanded, apply cached status to the newly visible children instantly.
        Try
            applyPersistentStatusCacheForVisibleTree("loadImmediateChildrenForNode")
        Catch
        End Try
    End Sub

    Private Sub loadEntireLazyTree(ByVal node As TreeNode)
        If node Is Nothing Then Exit Sub

        loadImmediateChildrenForNode(node)

        For Each childNode As TreeNode In node.Nodes
            If isLazyPlaceholderNode(childNode) Then Continue For
            loadEntireLazyTree(childNode)
        Next
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
            tortCommitDocsAsync({modDoc})
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
                getLocksOfDocsAsync({modDoc}, bBreakLocks:=True)
            End If
        End Sub
        Sub getLockActiveDocEventHandler(sender As Object, e As EventArgs)
            'Context menu belongs to this node, so lock this node's file only.
            getLocksOfDocsAsync({modDoc})
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

        Dim suffixStart As Integer = -1
        Dim knownSuffixes As String() = {
            " [Locked",
            " [Not committed",
            " [Locking",
            " [Syncing",
            " [Committing",
            " [Pending"
        }

        For Each suffix As String In knownSuffixes
            Dim idx As Integer = nodeText.IndexOf(suffix, StringComparison.OrdinalIgnoreCase)
            If idx >= 0 Then
                If suffixStart = -1 OrElse idx < suffixStart Then suffixStart = idx
            End If
        Next

        If suffixStart >= 0 Then
            Return nodeText.Substring(0, suffixStart)
        End If

        Return nodeText
    End Function

    Public Sub markLockPendingForFilePathsPublic(ByVal filePaths() As String,
                                                  ByVal isPending As Boolean,
                                                  Optional ByVal pendingText As String = "Locking...")
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(Sub() markLockPendingForFilePathsPublic(filePaths, isPending, pendingText)))
                Exit Sub
            End If
        Catch
        End Try

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        Dim normalizedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each filePath As String In filePaths
            Dim normalizedPath As String = normalizePathForNodeMatch(filePath)
            If normalizedPath <> "" AndAlso Not normalizedPaths.Contains(normalizedPath) Then
                normalizedPaths.Add(normalizedPath)
            End If
        Next

        If normalizedPaths.Count = 0 Then Exit Sub

        Try
            If TreeView1 IsNot Nothing Then
                For Each node As TreeNode In TreeView1.Nodes
                    markLockPendingOnNodeRecursive(node, normalizedPaths, isPending, pendingText)
                Next
            End If
        Catch
        End Try

        'Do not recolor the whole tree here.
        'This method is used by async Get Locks, and a full recolor/status pass can make
        'SolidWorks feel frozen right when the background lock finishes.
    End Sub

    Private Function normalizePathForNodeMatch(ByVal filePath As String) As String
        If String.IsNullOrWhiteSpace(filePath) Then Return ""

        Try
            Return Path.GetFullPath(filePath).TrimEnd("\"c).ToLowerInvariant()
        Catch
            Return filePath.Replace("/", "\").TrimEnd("\"c).ToLowerInvariant()
        End Try
    End Function

    Private Sub markLockPendingOnNodeRecursive(ByVal node As TreeNode,
                                               ByVal normalizedPaths As HashSet(Of String),
                                               ByVal isPending As Boolean,
                                               ByVal pendingText As String)
        If node Is Nothing Then Exit Sub

        Dim nodePath As String = normalizePathForNodeMatch(getCadPathFromTreeNode(node))

        If nodePath <> "" AndAlso normalizedPaths.Contains(nodePath) Then
            If isPending Then
                Dim baseText As String = stripStatusSuffix(node.Text)
                node.Text = baseText & " [" & pendingText & "]"
                node.BackColor = Color.LightSkyBlue
                node.ToolTipText = "SVN Get Locks is running in the background. You can keep using SolidWorks."
            Else
                node.Text = stripStatusSuffix(node.Text)
            End If
        End If

        For Each childNode As TreeNode In node.Nodes
            markLockPendingOnNodeRecursive(childNode, normalizedPaths, isPending, pendingText)
        Next
    End Sub


    Public Sub markLockResultForFilePathsPublic(ByVal filePaths() As String,
                                                ByVal lockedByYou As Boolean,
                                                Optional ByVal resultText As String = "Locked by you")
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(Sub() markLockResultForFilePathsPublic(filePaths, lockedByYou, resultText)))
                Exit Sub
            End If
        Catch
        End Try

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        Dim normalizedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each filePath As String In filePaths
            Dim normalizedPath As String = normalizePathForNodeMatch(filePath)
            If normalizedPath <> "" AndAlso Not normalizedPaths.Contains(normalizedPath) Then
                normalizedPaths.Add(normalizedPath)
            End If
        Next

        If normalizedPaths.Count = 0 Then Exit Sub

        Try
            If TreeView1 IsNot Nothing Then
                For Each node As TreeNode In TreeView1.Nodes
                    markLockResultOnNodeRecursive(node, normalizedPaths, lockedByYou, resultText)
                Next
            End If
        Catch
        End Try
    End Sub

    Private Sub markLockResultOnNodeRecursive(ByVal node As TreeNode,
                                              ByVal normalizedPaths As HashSet(Of String),
                                              ByVal lockedByYou As Boolean,
                                              ByVal resultText As String)
        If node Is Nothing Then Exit Sub

        Dim nodePath As String = normalizePathForNodeMatch(getCadPathFromTreeNode(node))

        If nodePath <> "" AndAlso normalizedPaths.Contains(nodePath) Then
            Dim baseText As String = stripStatusSuffix(node.Text)

            If lockedByYou Then
                node.Text = baseText & " [" & resultText & "]"
                node.BackColor = Color.LightGreen
                node.ToolTipText = "SVN lock completed. This file should now be writable."
            Else
                node.Text = baseText
                node.ToolTipText = ""
            End If
        End If

        For Each childNode As TreeNode In node.Nodes
            markLockResultOnNodeRecursive(childNode, normalizedPaths, lockedByYou, resultText)
        Next
    End Sub


    Public Sub markSyncPendingForFilePathsPublic(ByVal filePaths() As String,
                                                 ByVal isPending As Boolean,
                                                 Optional ByVal pendingText As String = "Syncing...")
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(Sub() markSyncPendingForFilePathsPublic(filePaths, isPending, pendingText)))
                Exit Sub
            End If
        Catch
        End Try

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        Dim normalizedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each filePath As String In filePaths
            Dim normalizedPath As String = normalizePathForNodeMatch(filePath)
            If normalizedPath <> "" AndAlso Not normalizedPaths.Contains(normalizedPath) Then
                normalizedPaths.Add(normalizedPath)
            End If
        Next

        If normalizedPaths.Count = 0 Then Exit Sub

        Try
            If TreeView1 IsNot Nothing Then
                TreeView1.BeginUpdate()
                Try
                    For Each node As TreeNode In TreeView1.Nodes
                        markSyncPendingOnNodeRecursive(node, normalizedPaths, isPending, pendingText)
                    Next
                Finally
                    TreeView1.EndUpdate()
                End Try
            End If
        Catch
        End Try
    End Sub

    Private Sub markSyncPendingOnNodeRecursive(ByVal node As TreeNode,
                                               ByVal normalizedPaths As HashSet(Of String),
                                               ByVal isPending As Boolean,
                                               ByVal pendingText As String)
        If node Is Nothing Then Exit Sub

        Dim nodePath As String = normalizePathForNodeMatch(getCadPathFromTreeNode(node))

        If nodePath <> "" AndAlso normalizedPaths.Contains(nodePath) Then
            If isPending Then
                Dim baseText As String = stripStatusSuffix(node.Text)
                node.Text = baseText & " [" & pendingText & "]"
                node.BackColor = Color.LightSkyBlue
                node.ToolTipText = "SVN Sync Status is running in the background. You can keep using SolidWorks."
            Else
                node.Text = stripStatusSuffix(node.Text)
            End If
        End If

        For Each childNode As TreeNode In node.Nodes
            markSyncPendingOnNodeRecursive(childNode, normalizedPaths, isPending, pendingText)
        Next
    End Sub

    Public Sub recolorTreeNodesForFilePathsPublic(ByVal filePaths() As String)
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(Sub() recolorTreeNodesForFilePathsPublic(filePaths)))
                Exit Sub
            End If
        Catch
        End Try

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        Dim normalizedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each filePath As String In filePaths
            Dim normalizedPath As String = normalizePathForNodeMatch(filePath)
            If normalizedPath <> "" AndAlso Not normalizedPaths.Contains(normalizedPath) Then
                normalizedPaths.Add(normalizedPath)
            End If
        Next

        If normalizedPaths.Count = 0 Then Exit Sub

        Try
            If TreeView1 IsNot Nothing Then
                TreeView1.BeginUpdate()
                Try
                    For Each node As TreeNode In TreeView1.Nodes
                        recolorTreeNodeIfPathMatchesRecursive(node, normalizedPaths)
                    Next
                Finally
                    TreeView1.EndUpdate()
                End Try
            End If
        Catch
        End Try
    End Sub

    Private Sub recolorTreeNodeIfPathMatchesRecursive(ByVal node As TreeNode,
                                                      ByVal normalizedPaths As HashSet(Of String))
        If node Is Nothing Then Exit Sub

        Dim nodePath As String = normalizePathForNodeMatch(getCadPathFromTreeNode(node))

        If nodePath <> "" AndAlso normalizedPaths.Contains(nodePath) Then
            setNodeColorFromStatus(node)
        End If

        For Each childNode As TreeNode In node.Nodes
            recolorTreeNodeIfPathMatchesRecursive(childNode, normalizedPaths)
        Next
    End Sub

    Public Sub forceWriteAccessForLockedFilePathsPublic(ByVal filePaths() As String)
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(Sub() forceWriteAccessForLockedFilePathsPublic(filePaths)))
                Exit Sub
            End If
        Catch
        End Try

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        For Each filePath As String In filePaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            Try
                If File.Exists(filePath) Then
                    File.SetAttributes(filePath, File.GetAttributes(filePath) And Not FileAttributes.ReadOnly)
                End If
            Catch
            End Try

            Dim doc As ModelDoc2 = Nothing

            Try
                doc = TryCast(iSwApp.GetOpenDocumentByName(filePath), ModelDoc2)
            Catch
                doc = Nothing
            End Try

            If doc Is Nothing Then Continue For

            Try
                doc.SetReadOnlyState(False)
            Catch
            End Try

            'This prevents the SolidWorks "opened read-only but now writable" prompt when the user right-clicks Edit Part.
            'It is the programmatic equivalent of the user clicking File > Reload after getting the SVN lock.
            Try
                If doc.IsOpenedReadOnly() Then
                    doc.ReloadOrReplace(ReadOnly:=False, ReplaceFileName:=Nothing, DiscardChanges:=True)
                End If
            Catch
            End Try
        Next
    End Sub

    Public Sub setOpenDocsReadOnlyForFilePathsPublic(ByVal filePaths() As String)
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(Sub() setOpenDocsReadOnlyForFilePathsPublic(filePaths)))
                Exit Sub
            End If
        Catch
        End Try

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        For Each filePath As String In filePaths
            If String.IsNullOrWhiteSpace(filePath) Then Continue For

            Try
                If File.Exists(filePath) Then
                    File.SetAttributes(filePath, File.GetAttributes(filePath) Or FileAttributes.ReadOnly)
                End If
            Catch
            End Try

            Try
                Dim doc As ModelDoc2 = TryCast(iSwApp.GetOpenDocumentByName(filePath), ModelDoc2)
                If doc IsNot Nothing Then doc.SetReadOnlyState(True)
            Catch
            End Try
        Next
    End Sub

    Public Sub markCommitPendingForFilePathsPublic(ByVal filePaths() As String,
                                                   ByVal isPending As Boolean,
                                                   Optional ByVal pendingText As String = "Committing...")
        'Commit uses the same visual pending helper as Sync, but with a different label.
        markSyncPendingForFilePathsPublic(filePaths, isPending, pendingText)
    End Sub

    Public Sub markCommitResultForFilePathsPublic(ByVal filePaths() As String,
                                                  ByVal success As Boolean)
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(Sub() markCommitResultForFilePathsPublic(filePaths, success)))
                Exit Sub
            End If
        Catch
        End Try

        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        If success Then
            Try
                setOpenDocsReadOnlyForFilePathsPublic(filePaths)
            Catch
            End Try
        End If

        Dim normalizedPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each filePath As String In filePaths
            Dim normalizedPath As String = normalizePathForNodeMatch(filePath)
            If normalizedPath <> "" AndAlso Not normalizedPaths.Contains(normalizedPath) Then
                normalizedPaths.Add(normalizedPath)
            End If
        Next

        If normalizedPaths.Count = 0 Then Exit Sub

        Try
            If TreeView1 IsNot Nothing Then
                TreeView1.BeginUpdate()
                Try
                    For Each node As TreeNode In TreeView1.Nodes
                        markCommitResultOnNodeRecursive(node, normalizedPaths, success)
                    Next
                Finally
                    TreeView1.EndUpdate()
                End Try
            End If
        Catch
        End Try
    End Sub

    Private Sub markCommitResultOnNodeRecursive(ByVal node As TreeNode,
                                                ByVal normalizedPaths As HashSet(Of String),
                                                ByVal success As Boolean)
        If node Is Nothing Then Exit Sub

        Dim nodePath As String = normalizePathForNodeMatch(getCadPathFromTreeNode(node))

        If nodePath <> "" AndAlso normalizedPaths.Contains(nodePath) Then
            node.Text = stripStatusSuffix(node.Text)

            If success Then
                node.BackColor = SystemColors.Window
                node.ToolTipText = "Commit finished. Click Sync to verify latest server status if needed."
            Else
                node.BackColor = Color.LightSalmon
                node.ToolTipText = "Commit did not complete."
            End If
        End If

        For Each childNode As TreeNode In node.Nodes
            markCommitResultOnNodeRecursive(childNode, normalizedPaths, success)
        Next
    End Sub

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
