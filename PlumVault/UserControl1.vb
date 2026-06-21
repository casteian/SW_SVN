
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
    Private WithEvents graphicalSelectionSyncTimer As System.Windows.Forms.Timer
    Private WithEvents butSyncStatus As Button
    Private WithEvents chkDebugIgnoreNaming As CheckBox
    Private WithEvents onlineCheckBox As CheckBox
    Private syncProgressBar As ProgressBar
    Private syncProgressLabel As Label
    Private syncStatusContextMenu As ContextMenuStrip
    Private WithEvents syncDebugTimingMenuItem As ToolStripMenuItem
    Private syncDebugTimingEnabled As Boolean = False
    Private WithEvents butCleanupQuick As Button
    Private cacheAgeLabel As Label
    Private WithEvents cacheAgeTimer As System.Windows.Forms.Timer
    Private Const LAZY_LOAD_PLACEHOLDER_TEXT As String = "<load children>"
    Private syncStatusInProgress As Boolean = False
    Private refreshTreeNeedsUpdate As Boolean = False
    Private normalRefreshTreeBackColor As Color
    Private lastLiveCheckedActivePath As String = ""
    Private lastGraphicalSelectionPath As String = ""
    Private lastGraphicallyHighlightedTreeNode As TreeNode = Nothing
    Private ReadOnly treeSelectionBackColor As Color = Color.FromArgb(0, 82, 180)
    Private ReadOnly treeSelectionForeColor As Color = Color.White
    Private WithEvents treeStartDragHandle As Panel
    Private treeStartDragInProgress As Boolean = False
    Private treeStartDragMouseOffsetY As Integer = 0
    Private treeStartDefaultTop As Integer = -1
    Private userAdjustedTreeStart As Boolean = False
    Private batchSelectedTreeNodes As New List(Of TreeNode)()
    Private lastBatchAnchorTreeNode As TreeNode = Nothing

    Private Function normalTreeTextColor() As Color
        Return Color.Black
    End Function

    'Tracks whether the TreeView selection came from an actual user click.
    'WinForms/SolidWorks can leave the root node selected even when the user thinks
    'nothing is selected. Sync uses this so the default click stays Level-1-only.
    Private lastUserClickedTreeNodeForSync As TreeNode = Nothing

    Private Sub setRefreshTreeButtonNormal()
        refreshTreeNeedsUpdate = False

        If butRefresh Is Nothing Then Exit Sub

        If CStr(If(butRefresh.Tag, "")) = "CompactSvnActionButton" Then
            butRefresh.Text = "Refresh"
            butRefresh.Size = New Size(uiPx(86), uiPx(28))
            butRefresh.Font = readableUiFont(True, 8.75!)
        Else
            butRefresh.Text = "Refresh Tree"
            butRefresh.Size = New Size(uiPx(220), uiPx(32))
            butRefresh.Font = readableUiFont(True, 10.0!)
        End If

        butRefresh.BackColor = normalRefreshTreeBackColor
        butRefresh.UseVisualStyleBackColor = True
    End Sub
    Private Sub setRefreshTreeButtonUpdateNeeded()
        refreshTreeNeedsUpdate = True

        If butRefresh Is Nothing Then Exit Sub

        If CStr(If(butRefresh.Tag, "")) = "CompactSvnActionButton" Then
            butRefresh.Text = "Refresh*"
            butRefresh.Size = New Size(uiPx(86), uiPx(28))
            butRefresh.Font = readableUiFont(True, 8.75!)
        Else
            butRefresh.Text = "Changes made - Update now"
            butRefresh.Size = New Size(uiPx(220), uiPx(32))
            butRefresh.Font = readableUiFont(True, 9.0!)
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
        ensureCleanupQuickButton(parentControl)
        ensureCacheAgeLabel(parentControl)
        ensureSyncProgressControls(parentControl)

        positionRefreshAndSyncButtonsBesideCommit()
        updateCacheAgeIndicatorPublic()
        removeGetLatestAllMenuItem()
    End Sub

    Private Sub removeGetLatestAllMenuItem()
        Try
            If dropDownGetLatestAll IsNot Nothing Then
                dropDownGetLatestAll.Visible = False
                dropDownGetLatestAll.Enabled = False
            End If

            If ToolStripDropDownButGetLatest IsNot Nothing AndAlso dropDownGetLatestAll IsNot Nothing Then
                Try
                    ToolStripDropDownButGetLatest.DropDownItems.Remove(dropDownGetLatestAll)
                Catch
                End Try
            End If
        Catch
        End Try
    End Sub



    Private Sub ensureOnlineCheckbox()
        Dim parentControl As Control = Nothing

        Try
            If versionLabel IsNot Nothing Then parentControl = versionLabel.Parent
        Catch
            parentControl = Nothing
        End Try

        If parentControl Is Nothing Then parentControl = Me

        If onlineCheckBox Is Nothing Then
            Try
                onlineCheckBox = TryCast(parentControl.Controls.Find("onlineCheckBox", True).FirstOrDefault(), CheckBox)
            Catch
                onlineCheckBox = Nothing
            End Try
        End If

        If onlineCheckBox Is Nothing Then
            onlineCheckBox = New CheckBox()
            onlineCheckBox.Name = "onlineCheckBox"
            onlineCheckBox.Text = "Online"
            onlineCheckBox.Checked = True
            onlineCheckBox.Visible = True
            onlineCheckBox.AutoSize = True
            onlineCheckBox.BackColor = SystemColors.Control
            onlineCheckBox.UseVisualStyleBackColor = True
            onlineCheckBox.Font = readableUiFont(False, 8.5!)
            onlineCheckBox.Anchor = AnchorStyles.Top Or AnchorStyles.Left

            Try
                If butRefresh IsNot Nothing Then
                    onlineCheckBox.TabIndex = butRefresh.TabIndex + 5
                Else
                    onlineCheckBox.TabIndex = 50
                End If
            Catch
                onlineCheckBox.TabIndex = 50
            End Try

            parentControl.Controls.Add(onlineCheckBox)
        Else
            If onlineCheckBox.Parent Is Nothing Then
                parentControl.Controls.Add(onlineCheckBox)
            End If

            onlineCheckBox.Text = "Online"
            onlineCheckBox.Visible = True
            onlineCheckBox.AutoSize = True
            onlineCheckBox.BackColor = SystemColors.Control
            onlineCheckBox.UseVisualStyleBackColor = True
            onlineCheckBox.Font = readableUiFont(False, 8.5!)
            onlineCheckBox.Anchor = AnchorStyles.Top Or AnchorStyles.Left
        End If

        Try
            RemoveHandler onlineCheckBox.CheckedChanged, AddressOf boxCheck_Check
        Catch
        End Try

        Try
            AddHandler onlineCheckBox.CheckedChanged, AddressOf boxCheck_Check
        Catch
        End Try

        positionOnlineCheckboxBesideVersion()

        Try
            onlineCheckBox.BringToFront()
        Catch
        End Try
    End Sub

    Private Sub positionOnlineCheckboxBesideVersion()
        Try
            If onlineCheckBox Is Nothing Then Exit Sub
            If versionLabel Is Nothing Then Exit Sub

            Dim parentControl As Control = onlineCheckBox.Parent
            If parentControl Is Nothing Then parentControl = Me

            'Place Online significantly to the right of Version, but still inside the task pane.
            Dim desiredX As Integer = versionLabel.Right + uiPx(95)
            Dim minimumX As Integer = versionLabel.Left + uiPx(270)
            desiredX = Math.Max(desiredX, minimumX)

            Dim maxX As Integer = parentControl.ClientSize.Width - onlineCheckBox.Width - uiPx(8)
            If maxX < uiPx(4) Then maxX = uiPx(4)

            Dim finalX As Integer = Math.Min(desiredX, maxX)

            'If the pane is narrow, keep it at least a little right of the version label.
            If finalX < versionLabel.Right + uiPx(20) Then
                finalX = Math.Max(uiPx(4), maxX)
            End If

            Dim finalY As Integer = versionLabel.Top + CInt(Math.Round((versionLabel.Height - onlineCheckBox.Height) / 2.0))

            onlineCheckBox.Location = New Point(finalX, Math.Max(0, finalY))
            onlineCheckBox.BringToFront()
        Catch
        End Try
    End Sub
    Private Sub ensureDebugIgnoreNamingCheckbox(ByVal parentControl As Control)
        If parentControl Is Nothing Then parentControl = Me

        If chkDebugIgnoreNaming Is Nothing Then
            chkDebugIgnoreNaming = New CheckBox()
            chkDebugIgnoreNaming.Name = "chkDebugIgnoreNaming"
            chkDebugIgnoreNaming.Text = "Debug: ignore naming"
            chkDebugIgnoreNaming.AutoSize = True
            chkDebugIgnoreNaming.Font = readableUiFont(False, 8.5!)
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

    Private Sub ensureCleanupQuickButton(ByVal parentControl As Control)
        If parentControl Is Nothing Then parentControl = Me

        If butCleanupQuick Is Nothing Then
            butCleanupQuick = New Button()
            butCleanupQuick.Name = "butCleanupQuick"
            butCleanupQuick.TabIndex = If(butRefresh IsNot Nothing, butRefresh.TabIndex + 6, 60)
            parentControl.Controls.Add(butCleanupQuick)
        ElseIf butCleanupQuick.Parent Is Nothing Then
            parentControl.Controls.Add(butCleanupQuick)
        End If

        setCompactSvnActionButtonStyle(butCleanupQuick, "Cleanup")
        butCleanupQuick.Visible = True
        butCleanupQuick.Enabled = True
        butCleanupQuick.BringToFront()
    End Sub

    Private Sub ensureCacheAgeLabel(ByVal parentControl As Control)
        If parentControl Is Nothing Then parentControl = Me

        If cacheAgeLabel Is Nothing Then
            cacheAgeLabel = New Label()
            cacheAgeLabel.Name = "cacheAgeLabel"
            cacheAgeLabel.AutoSize = True
            cacheAgeLabel.Font = readableUiFont(False, 8.25!)
            cacheAgeLabel.BackColor = SystemColors.Control
            cacheAgeLabel.Text = "Cache: none"
            cacheAgeLabel.Visible = True
            cacheAgeLabel.TabIndex = If(butRefresh IsNot Nothing, butRefresh.TabIndex + 7, 61)
            parentControl.Controls.Add(cacheAgeLabel)
        ElseIf cacheAgeLabel.Parent Is Nothing Then
            parentControl.Controls.Add(cacheAgeLabel)
        End If

        cacheAgeLabel.Font = readableUiFont(False, 8.25!)
        cacheAgeLabel.AutoSize = True
        cacheAgeLabel.Visible = True

        If cacheAgeTimer Is Nothing Then
            cacheAgeTimer = New System.Windows.Forms.Timer()
            cacheAgeTimer.Interval = 15000
            cacheAgeTimer.Start()
        End If
    End Sub

    Private Sub butCleanupQuick_Click(sender As Object, e As EventArgs) Handles butCleanupQuick.Click
        myCleanup()
    End Sub

    Private Sub cacheAgeTimer_Tick(sender As Object, e As EventArgs) Handles cacheAgeTimer.Tick
        updateCacheAgeIndicatorPublic()
    End Sub

    Public Sub updateCacheAgeIndicatorPublic()
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(Sub() updateCacheAgeIndicatorPublic()))
                Exit Sub
            End If

            If cacheAgeLabel Is Nothing Then Exit Sub

            Dim cacheText As String = "none"

            Try
                cacheText = svnModule.getStatusCacheAgeDisplayTextPublic()
            Catch
                cacheText = "unknown"
            End Try

            cacheAgeLabel.Text = "Cache: " & cacheText
            cacheAgeLabel.BringToFront()
            positionRefreshAndSyncButtonsBesideCommit()
        Catch
        End Try
    End Sub

    Private Sub ensureSyncProgressControls(ByVal parentControl As Control)
        If parentControl Is Nothing Then parentControl = Me

        If syncProgressBar Is Nothing Then
            syncProgressBar = New ProgressBar()
            syncProgressBar.Name = "syncProgressBar"
            syncProgressBar.Size = New Size(uiPx(190), uiPx(12))
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
            syncProgressLabel.Font = readableUiFont(False, 8.25!)
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

    Private Function uiScaleFactor() As Single
        Try
            Using g As Graphics = Me.CreateGraphics()
                Return Math.Max(1.0F, g.DpiX / 96.0F)
            End Using
        Catch
            Return 1.0F
        End Try
    End Function

    Private Function uiPx(ByVal value96Dpi As Integer) As Integer
        Return Math.Max(1, CInt(Math.Round(value96Dpi * uiScaleFactor())))
    End Function

    Private Function readableUiFont(Optional ByVal bold As Boolean = False, Optional ByVal baseSize As Single = 9.0!) As Font
        Dim style As FontStyle = If(bold, FontStyle.Bold, FontStyle.Regular)
        Try
            Return New Font(SystemFonts.MessageBoxFont.FontFamily, Math.Max(baseSize, SystemFonts.MessageBoxFont.Size), style)
        Catch
            Return New Font(Me.Font.FontFamily, baseSize, style)
        End Try
    End Function

    Private Sub applyDpiFriendlyTaskPaneUi()
        Try
            Me.AutoScaleMode = AutoScaleMode.Dpi
            Me.Font = readableUiFont(False, 9.0!)
            applyDpiFriendlySizingRecursive(Me)

            If TreeView1 IsNot Nothing Then
                TreeView1.Font = readableUiFont(False, 9.0!)
                TreeView1.HideSelection = False
                TreeView1.FullRowSelect = False
                TreeView1.DrawMode = TreeViewDrawMode.OwnerDrawText
                TreeView1.ItemHeight = Math.Max(TreeView1.ItemHeight, uiPx(21))
            End If

            If butRefresh IsNot Nothing Then setCompactSvnActionButtonStyle(butRefresh, "Refresh")
            If butSyncStatus IsNot Nothing Then setCompactSvnActionButtonStyle(butSyncStatus, "Sync")
            If butCleanupQuick IsNot Nothing Then setCompactSvnActionButtonStyle(butCleanupQuick, "Cleanup")

            If cacheAgeLabel IsNot Nothing Then
                cacheAgeLabel.Font = readableUiFont(False, 8.25!)
                cacheAgeLabel.AutoSize = True
            End If

            If chkDebugIgnoreNaming IsNot Nothing Then
                chkDebugIgnoreNaming.Font = readableUiFont(False, 8.5!)
                chkDebugIgnoreNaming.AutoSize = True
            End If

            If syncProgressLabel IsNot Nothing Then
                syncProgressLabel.Font = readableUiFont(False, 8.25!)
                syncProgressLabel.AutoSize = True
            End If

            If syncProgressBar IsNot Nothing Then
                syncProgressBar.Height = uiPx(12)
                syncProgressBar.Width = uiPx(190)
            End If

            ensureTreeStartDragHandle()
            positionRefreshAndSyncButtonsBesideCommit()
            removeGetLatestAllMenuItem()
            ensureOnlineCheckbox()
            positionOnlineCheckboxBesideVersion()
        Catch
        End Try
    End Sub

    Private Sub applyDpiFriendlySizingRecursive(ByVal root As Control)
        If root Is Nothing Then Exit Sub

        Try
            If TypeOf root Is Button Then
                Dim btn As Button = CType(root, Button)
                btn.Font = readableUiFont(True, 8.75!)
                btn.MinimumSize = New Size(uiPx(86), uiPx(28))
                If btn.Width < btn.MinimumSize.Width Then btn.Width = btn.MinimumSize.Width
                If btn.Height < btn.MinimumSize.Height Then btn.Height = btn.MinimumSize.Height
                btn.AutoEllipsis = True
                btn.TextAlign = ContentAlignment.MiddleCenter
            ElseIf TypeOf root Is CheckBox Then
                CType(root, CheckBox).Font = readableUiFont(False, 8.5!)
                CType(root, CheckBox).AutoSize = True
            ElseIf TypeOf root Is Label Then
                CType(root, Label).Font = readableUiFont(False, 8.5!)
                CType(root, Label).AutoSize = True
            ElseIf TypeOf root Is TreeView Then
                CType(root, TreeView).Font = readableUiFont(False, 9.0!)
            ElseIf TypeOf root Is ToolStrip Then
                Dim ts As ToolStrip = CType(root, ToolStrip)
                ts.Font = readableUiFont(False, 9.0!)
                ts.ImageScalingSize = New Size(uiPx(24), uiPx(24))

                For Each item As ToolStripItem In ts.Items
                    item.Font = readableUiFont(False, 9.0!)
                    item.AutoSize = True
                Next
            End If

            For Each child As Control In root.Controls
                applyDpiFriendlySizingRecursive(child)
            Next
        Catch
        End Try
    End Sub

    Private Sub ensureTreeStartDragHandle()
        Try
            If TreeView1 Is Nothing Then Exit Sub

            Dim parentControl As Control = TreeView1.Parent
            If parentControl Is Nothing Then parentControl = Me

            If treeStartDefaultTop < 0 Then treeStartDefaultTop = TreeView1.Top

            If treeStartDragHandle Is Nothing Then
                treeStartDragHandle = New Panel()
                treeStartDragHandle.Name = "treeStartDragHandle"
                treeStartDragHandle.Height = uiPx(8)
                treeStartDragHandle.BackColor = SystemColors.ControlDark
                treeStartDragHandle.Cursor = Cursors.HSplit
                treeStartDragHandle.TabStop = False
                treeStartDragHandle.Visible = True
                treeStartDragHandle.BorderStyle = BorderStyle.None
                treeStartDragHandle.Anchor = AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Top
                treeStartDragHandle.Tag = "Drag this bar up/down to adjust where the SVN tree starts. Double-click to reset."
                parentControl.Controls.Add(treeStartDragHandle)
            ElseIf treeStartDragHandle.Parent Is Nothing Then
                parentControl.Controls.Add(treeStartDragHandle)
            End If

            positionTreeStartDragHandle()
            treeStartDragHandle.BringToFront()
        Catch
        End Try
    End Sub

    Private Sub positionTreeStartDragHandle()
        Try
            If TreeView1 Is Nothing Then Exit Sub
            If treeStartDragHandle Is Nothing Then Exit Sub

            Dim parentControl As Control = TreeView1.Parent
            If parentControl Is Nothing Then parentControl = Me

            Dim handleHeight As Integer = Math.Max(uiPx(6), treeStartDragHandle.Height)
            treeStartDragHandle.Height = handleHeight
            treeStartDragHandle.Left = TreeView1.Left
            treeStartDragHandle.Width = Math.Max(uiPx(40), TreeView1.Width)
            treeStartDragHandle.Top = Math.Max(0, TreeView1.Top - handleHeight)
            treeStartDragHandle.BringToFront()
        Catch
        End Try
    End Sub

    Private Function getMinimumTreeStartTop() As Integer
        Try
            If TreeView1 Is Nothing Then Return uiPx(120)

            Dim parentControl As Control = TreeView1.Parent
            If parentControl Is Nothing Then parentControl = Me

            Dim minTop As Integer = uiPx(120)
            Dim treeBottom As Integer = TreeView1.Bottom

            For Each sibling As Control In parentControl.Controls
                If sibling Is Nothing Then Continue For
                If Object.ReferenceEquals(sibling, TreeView1) Then Continue For
                If treeStartDragHandle IsNot Nothing AndAlso Object.ReferenceEquals(sibling, treeStartDragHandle) Then Continue For
                If Not sibling.Visible Then Continue For

                'Only protect the action/header controls above the tree. This keeps users from
                'dragging the tree start over Refresh/Sync/Commit/etc., while still allowing DPI fixes.
                If sibling.Bottom <= treeBottom AndAlso sibling.Top < TreeView1.Top Then
                    minTop = Math.Max(minTop, sibling.Bottom + uiPx(6))
                End If
            Next

            Return Math.Min(minTop, Math.Max(uiPx(20), treeBottom - uiPx(80)))
        Catch
            Return uiPx(120)
        End Try
    End Function

    Private Sub applyTreeStartTop(ByVal requestedTop As Integer)
        Try
            If TreeView1 Is Nothing Then Exit Sub

            Dim oldBottom As Integer = TreeView1.Bottom
            Dim minTop As Integer = getMinimumTreeStartTop()
            Dim maxTop As Integer = Math.Max(minTop, oldBottom - uiPx(80))
            Dim newTop As Integer = Math.Max(minTop, Math.Min(requestedTop, maxTop))

            TreeView1.SuspendLayout()
            TreeView1.Top = newTop
            TreeView1.Height = Math.Max(uiPx(80), oldBottom - newTop)
            TreeView1.ResumeLayout()

            userAdjustedTreeStart = True
            positionTreeStartDragHandle()
        Catch
            Try
                TreeView1.ResumeLayout()
            Catch
            End Try
        End Try
    End Sub

    Private Sub treeStartDragHandle_MouseDown(sender As Object, e As MouseEventArgs) Handles treeStartDragHandle.MouseDown
        If e.Button <> MouseButtons.Left Then Exit Sub

        Try
            treeStartDragInProgress = True
            treeStartDragMouseOffsetY = e.Y
            treeStartDragHandle.Capture = True
            treeStartDragHandle.BackColor = SystemColors.Highlight
        Catch
        End Try
    End Sub

    Private Sub treeStartDragHandle_MouseMove(sender As Object, e As MouseEventArgs) Handles treeStartDragHandle.MouseMove
        If Not treeStartDragInProgress Then Exit Sub

        Try
            Dim parentControl As Control = TreeView1.Parent
            If parentControl Is Nothing Then parentControl = Me

            Dim mouseParentPoint As Point = parentControl.PointToClient(Control.MousePosition)
            Dim newHandleTop As Integer = mouseParentPoint.Y - treeStartDragMouseOffsetY
            applyTreeStartTop(newHandleTop + treeStartDragHandle.Height)
        Catch
        End Try
    End Sub

    Private Sub treeStartDragHandle_MouseUp(sender As Object, e As MouseEventArgs) Handles treeStartDragHandle.MouseUp
        Try
            treeStartDragInProgress = False
            treeStartDragHandle.Capture = False
            treeStartDragHandle.BackColor = SystemColors.ControlDark
            positionTreeStartDragHandle()
        Catch
        End Try
    End Sub

    Private Sub treeStartDragHandle_DoubleClick(sender As Object, e As EventArgs) Handles treeStartDragHandle.DoubleClick
        Try
            If treeStartDefaultTop >= 0 Then
                userAdjustedTreeStart = False
                applyTreeStartTop(treeStartDefaultTop)
                userAdjustedTreeStart = False
            End If
        Catch
        End Try
    End Sub

    Private Sub setupSyncStatusContextMenu()
        If butSyncStatus Is Nothing Then Exit Sub

        If syncStatusContextMenu Is Nothing Then
            syncStatusContextMenu = New ContextMenuStrip()

            Dim syncBranchItem As New ToolStripMenuItem("Sync Selected Branch", Nothing, AddressOf syncSelectedBranchMenuItem_Click)
            Dim syncWholeCarItem As New ToolStripMenuItem("Sync Whole Car Status (slow)", Nothing, AddressOf syncWholeCarMenuItem_Click)

            syncDebugTimingMenuItem = New ToolStripMenuItem("Debug Timing Popups")
            syncDebugTimingMenuItem.CheckOnClick = True
            syncDebugTimingMenuItem.Checked = syncDebugTimingEnabled
            AddHandler syncDebugTimingMenuItem.CheckedChanged, AddressOf syncDebugTimingMenuItem_CheckedChanged

            syncStatusContextMenu.Items.Add(syncBranchItem)
            syncStatusContextMenu.Items.Add(syncWholeCarItem)
            syncStatusContextMenu.Items.Add(New ToolStripSeparator())
            syncStatusContextMenu.Items.Add(syncDebugTimingMenuItem)
        Else
            If syncDebugTimingMenuItem IsNot Nothing Then
                syncDebugTimingMenuItem.Checked = syncDebugTimingEnabled
            End If
        End If

        butSyncStatus.ContextMenuStrip = syncStatusContextMenu
        butSyncStatus.Text = "Sync"
        butSyncStatus.AutoEllipsis = True
        butSyncStatus.UseVisualStyleBackColor = True
    End Sub

    Private Sub syncDebugTimingMenuItem_CheckedChanged(sender As Object, e As EventArgs)
        Try
            If syncDebugTimingMenuItem IsNot Nothing Then
                syncDebugTimingEnabled = syncDebugTimingMenuItem.Checked
            End If
        Catch
            syncDebugTimingEnabled = False
        End Try
    End Sub

    Public Function debugTimingEnabledPublic() As Boolean
        Try
            Return syncDebugTimingEnabled
        Catch
            Return False
        End Try
    End Function

    Private Function syncDebugEnabled() As Boolean
        Return debugTimingEnabledPublic()
    End Function

    Public Function syncStatusInProgressPublic() As Boolean
        Try
            Return syncStatusInProgress
        Catch
            Return False
        End Try
    End Function

    Private Sub showSyncDebugWindow(ByVal title As String,
                                    ByVal syncPaths() As String,
                                    ByVal preSyncTimingLog As String,
                                    ByVal backgroundTimingLog As String,
                                    ByVal totalElapsedMs As Long,
                                    ByVal errorMessage As String)
        Try
            If Not syncDebugEnabled() Then Exit Sub

            Dim msg As New System.Text.StringBuilder()

            msg.AppendLine(title)
            msg.AppendLine()

            If syncPaths IsNot Nothing Then
                msg.AppendLine("Files queued: " & syncPaths.Length.ToString())
            Else
                msg.AppendLine("Files queued: 0")
            End If

            If totalElapsedMs >= 0 Then
                msg.AppendLine("Total elapsed after background start: " & totalElapsedMs.ToString() & " ms")
            End If

            If Not String.IsNullOrWhiteSpace(preSyncTimingLog) Then
                msg.AppendLine()
                msg.AppendLine("UI / tree collection timing:")
                msg.AppendLine(preSyncTimingLog)
            End If

            If Not String.IsNullOrWhiteSpace(backgroundTimingLog) Then
                msg.AppendLine()
                msg.AppendLine("SVN background timing:")
                msg.AppendLine(backgroundTimingLog)
            End If

            If Not String.IsNullOrWhiteSpace(errorMessage) Then
                msg.AppendLine()
                msg.AppendLine("Error:")
                msg.AppendLine(errorMessage)
            End If

            If syncPaths IsNot Nothing AndAlso syncPaths.Length > 0 Then
                msg.AppendLine()
                msg.AppendLine("First queued paths:")

                Dim maxPathsToShow As Integer = Math.Min(syncPaths.Length, 8)

                For i As Integer = 0 To maxPathsToShow - 1
                    msg.AppendLine("- " & syncPaths(i))
                Next

                If syncPaths.Length > maxPathsToShow Then
                    msg.AppendLine("... +" & (syncPaths.Length - maxPathsToShow).ToString() & " more")
                End If
            End If

            System.Windows.Forms.MessageBox.Show(
                msg.ToString(),
                "SVN Sync Debug",
                System.Windows.Forms.MessageBoxButtons.OK,
                System.Windows.Forms.MessageBoxIcon.Information
            )
        Catch
        End Try
    End Sub

    Private Function isTreeNodeBatchSelected(ByVal node As TreeNode) As Boolean
        If node Is Nothing Then Return False

        Try
            For Each selectedNode As TreeNode In batchSelectedTreeNodes
                If Object.ReferenceEquals(selectedNode, node) Then Return True
            Next
        Catch
        End Try

        Return False
    End Function

    Private Sub clearBatchTreeSelection(Optional ByVal invalidateTree As Boolean = True)
        Try
            batchSelectedTreeNodes.Clear()
            lastBatchAnchorTreeNode = Nothing
            If invalidateTree AndAlso TreeView1 IsNot Nothing Then TreeView1.Invalidate()
        Catch
        End Try
    End Sub

    Private Sub addBatchTreeNode(ByVal node As TreeNode)
        If node Is Nothing Then Exit Sub
        If isLazyPlaceholderNode(node) Then Exit Sub

        Try
            If Not isTreeNodeBatchSelected(node) Then batchSelectedTreeNodes.Add(node)
        Catch
        End Try
    End Sub

    Private Sub toggleBatchTreeNode(ByVal node As TreeNode)
        If node Is Nothing Then Exit Sub
        If isLazyPlaceholderNode(node) Then Exit Sub

        Try
            For i As Integer = batchSelectedTreeNodes.Count - 1 To 0 Step -1
                If Object.ReferenceEquals(batchSelectedTreeNodes(i), node) Then
                    batchSelectedTreeNodes.RemoveAt(i)
                    If TreeView1 IsNot Nothing Then TreeView1.Invalidate()
                    Exit Sub
                End If
            Next

            batchSelectedTreeNodes.Add(node)
            lastBatchAnchorTreeNode = node
            If TreeView1 IsNot Nothing Then TreeView1.Invalidate()
        Catch
        End Try
    End Sub

    Private Function getVisibleTreeNodesFlat() As List(Of TreeNode)
        Dim output As New List(Of TreeNode)()

        Try
            If TreeView1 Is Nothing OrElse TreeView1.Nodes Is Nothing Then Return output

            For Each node As TreeNode In TreeView1.Nodes
                addVisibleTreeNodeFlatRecursive(node, output)
            Next
        Catch
        End Try

        Return output
    End Function

    Private Sub addVisibleTreeNodeFlatRecursive(ByVal node As TreeNode, ByVal output As List(Of TreeNode))
        If node Is Nothing Then Exit Sub
        If output Is Nothing Then Exit Sub

        output.Add(node)

        Try
            If Not node.IsExpanded Then Exit Sub

            For Each childNode As TreeNode In node.Nodes
                addVisibleTreeNodeFlatRecursive(childNode, output)
            Next
        Catch
        End Try
    End Sub

    Private Sub selectBatchTreeRange(ByVal endNode As TreeNode)
        If endNode Is Nothing Then Exit Sub

        Try
            If lastBatchAnchorTreeNode Is Nothing Then lastBatchAnchorTreeNode = endNode

            Dim visibleNodes As List(Of TreeNode) = getVisibleTreeNodesFlat()
            Dim anchorIndex As Integer = -1
            Dim endIndex As Integer = -1

            For i As Integer = 0 To visibleNodes.Count - 1
                If Object.ReferenceEquals(visibleNodes(i), lastBatchAnchorTreeNode) Then anchorIndex = i
                If Object.ReferenceEquals(visibleNodes(i), endNode) Then endIndex = i
            Next

            If anchorIndex < 0 OrElse endIndex < 0 Then
                clearBatchTreeSelection(False)
                addBatchTreeNode(endNode)
                lastBatchAnchorTreeNode = endNode
                If TreeView1 IsNot Nothing Then TreeView1.Invalidate()
                Exit Sub
            End If

            Dim firstIndex As Integer = Math.Min(anchorIndex, endIndex)
            Dim lastIndex As Integer = Math.Max(anchorIndex, endIndex)

            clearBatchTreeSelection(False)

            For i As Integer = firstIndex To lastIndex
                addBatchTreeNode(visibleNodes(i))
            Next

            If TreeView1 IsNot Nothing Then TreeView1.Invalidate()
        Catch
        End Try
    End Sub

    Private Function getBatchSelectedTreeCadPathsForAction(Optional ByVal includeSingleSelectedNode As Boolean = True) As String()
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            If batchSelectedTreeNodes IsNot Nothing AndAlso batchSelectedTreeNodes.Count > 0 Then
                For Each node As TreeNode In batchSelectedTreeNodes
                    addTreeNodePathToBatchActionList(node, seen, output)
                Next
            ElseIf includeSingleSelectedNode AndAlso TreeView1 IsNot Nothing AndAlso TreeView1.SelectedNode IsNot Nothing Then
                'Only use the single tree selection for file actions if the user actually clicked it.
                'This avoids acting on an automatically-selected root node after refresh.
                If lastUserClickedTreeNodeForSync IsNot Nothing AndAlso Object.ReferenceEquals(TreeView1.SelectedNode, lastUserClickedTreeNodeForSync) Then
                    addTreeNodePathToBatchActionList(TreeView1.SelectedNode, seen, output)
                End If
            End If
        Catch
        End Try

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Sub addTreeNodePathToBatchActionList(ByVal node As TreeNode,
                                                 ByVal seen As HashSet(Of String),
                                                 ByVal output As List(Of String))
        If node Is Nothing Then Exit Sub
        If isLazyPlaceholderNode(node) Then Exit Sub
        If seen Is Nothing OrElse output Is Nothing Then Exit Sub

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

    Public Function getBatchSelectedTreeCadPathsForActionPublic(Optional ByVal includeSingleSelectedNode As Boolean = True) As String()
        Return getBatchSelectedTreeCadPathsForAction(includeSingleSelectedNode)
    End Function

    Public Function getAssemblyCommitGuardPathsForPathsPublic(ByVal selectedCommitPaths() As String) As String()
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            If selectedCommitPaths Is Nothing OrElse selectedCommitPaths.Length = 0 Then Return Nothing

            Dim selectedSet As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

            For Each selectedPath As String In selectedCommitPaths
                If String.IsNullOrWhiteSpace(selectedPath) Then Continue For

                Try
                    selectedPath = Path.GetFullPath(selectedPath)
                Catch
                End Try

                selectedSet.Add(selectedPath)
                addPathToGuardList(selectedPath, seen, output)
            Next

            If TreeView1 IsNot Nothing AndAlso TreeView1.Nodes IsNot Nothing Then
                For Each rootNode As TreeNode In TreeView1.Nodes
                    collectAssemblyCommitGuardPathsFromTree(rootNode, selectedSet, seen, output)
                Next
            End If
        Catch
        End Try

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Private Sub collectAssemblyCommitGuardPathsFromTree(ByVal node As TreeNode,
                                                        ByVal selectedSet As HashSet(Of String),
                                                        ByVal seen As HashSet(Of String),
                                                        ByVal output As List(Of String))
        If node Is Nothing Then Exit Sub
        If selectedSet Is Nothing OrElse seen Is Nothing OrElse output Is Nothing Then Exit Sub
        If isLazyPlaceholderNode(node) Then Exit Sub

        Dim nodePath As String = getCadPathFromTreeNode(node)

        Try
            If Not String.IsNullOrWhiteSpace(nodePath) Then nodePath = Path.GetFullPath(nodePath)
        Catch
        End Try

        If selectedSet.Contains(nodePath) AndAlso isTreeNodeAssembly(node) Then
            collectLoadedDescendantCadPathsForCommitGuard(node, seen, output)
            Exit Sub
        End If

        Try
            For Each childNode As TreeNode In node.Nodes
                collectAssemblyCommitGuardPathsFromTree(childNode, selectedSet, seen, output)
            Next
        Catch
        End Try
    End Sub

    Private Sub collectLoadedDescendantCadPathsForCommitGuard(ByVal node As TreeNode,
                                                              ByVal seen As HashSet(Of String),
                                                              ByVal output As List(Of String))
        If node Is Nothing Then Exit Sub
        If seen Is Nothing OrElse output Is Nothing Then Exit Sub
        If isLazyPlaceholderNode(node) Then Exit Sub

        Dim nodePath As String = getCadPathFromTreeNode(node)
        addPathToGuardList(nodePath, seen, output)

        Try
            For Each childNode As TreeNode In node.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For
                collectLoadedDescendantCadPathsForCommitGuard(childNode, seen, output)
            Next
        Catch
        End Try
    End Sub

    Private Sub addPathToGuardList(ByVal filePath As String,
                                   ByVal seen As HashSet(Of String),
                                   ByVal output As List(Of String))
        If seen Is Nothing OrElse output Is Nothing Then Exit Sub
        If Not isCadPathForSync(filePath) Then Exit Sub

        Try
            filePath = Path.GetFullPath(filePath)
        Catch
        End Try

        If seen.Contains(filePath) Then Exit Sub
        seen.Add(filePath)
        output.Add(filePath)
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
        btn.AutoSize = False
        btn.MinimumSize = New Size(uiPx(86), uiPx(28))
        btn.Size = btn.MinimumSize
        btn.Font = readableUiFont(True, 8.75!)
        btn.BackColor = SystemColors.Control
        btn.UseVisualStyleBackColor = True
        btn.Anchor = AnchorStyles.Top Or AnchorStyles.Left
        btn.AutoEllipsis = True
        btn.TextAlign = ContentAlignment.MiddleCenter
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

            Dim gap As Integer = uiPx(5)
            Dim minLeft As Integer = uiPx(4)
            Dim maxLeft As Integer = Math.Max(minLeft, parentControl.ClientSize.Width - butRefresh.Width - gap)
            Dim x As Integer = Math.Max(minLeft, Math.Min(startPoint.X, maxLeft))
            Dim y As Integer = Math.Max(0, startPoint.Y)

            'Prefer putting Refresh/Sync beside Commit. If the task pane is too narrow,
            'stack Sync below Refresh so they do not get clipped at the bottom of the pane.
            If x + butRefresh.Width + gap + butSyncStatus.Width <= parentControl.ClientSize.Width - 2 Then
                butRefresh.Location = New Point(x, y)
                butSyncStatus.Location = New Point(x + butRefresh.Width + gap, y)
            Else
                butRefresh.Location = New Point(x, y)
                butSyncStatus.Location = New Point(x, y + butRefresh.Height + gap)
            End If

            'New Cleanup button: prefer beside the Release dropdown so users can run SVN cleanup
            'without going through the folder menu and without closing SOLIDWORKS.
            If butCleanupQuick IsNot Nothing Then
                Dim cleanupPlaced As Boolean = False

                Try
                    If ToolStripDropDownButReleases IsNot Nothing AndAlso ToolStripDropDownButReleases.Owner IsNot Nothing Then
                        Dim releaseOwner As Control = TryCast(ToolStripDropDownButReleases.Owner, Control)
                        If releaseOwner IsNot Nothing Then
                            Dim releaseBounds As Rectangle = ToolStripDropDownButReleases.Bounds
                            Dim releaseScreen As Point = releaseOwner.PointToScreen(New Point(releaseBounds.Right + 8, releaseBounds.Top + 2))
                            Dim releasePoint As Point = parentControl.PointToClient(releaseScreen)

                            Dim cleanupX As Integer = Math.Max(minLeft, releasePoint.X)
                            Dim cleanupY As Integer = Math.Max(0, releasePoint.Y)

                            If cleanupX + butCleanupQuick.Width <= parentControl.ClientSize.Width - uiPx(2) Then
                                butCleanupQuick.Location = New Point(cleanupX, cleanupY)
                            Else
                                butCleanupQuick.Location = New Point(Math.Max(minLeft, parentControl.ClientSize.Width - butCleanupQuick.Width - uiPx(4)), cleanupY + butCleanupQuick.Height + gap)
                            End If

                            cleanupPlaced = True
                        End If
                    End If
                Catch
                    cleanupPlaced = False
                End Try

                If Not cleanupPlaced Then
                    butCleanupQuick.Location = New Point(butSyncStatus.Right + gap, butSyncStatus.Top)
                    If butCleanupQuick.Right > parentControl.ClientSize.Width - uiPx(2) Then
                        butCleanupQuick.Location = New Point(butRefresh.Left, Math.Max(butRefresh.Bottom, butSyncStatus.Bottom) + gap)
                    End If
                End If

                butCleanupQuick.BringToFront()
            End If

            Dim actionBottom As Integer = Math.Max(butRefresh.Bottom, butSyncStatus.Bottom)
            If butCleanupQuick IsNot Nothing Then actionBottom = Math.Max(actionBottom, butCleanupQuick.Bottom)

            If chkDebugIgnoreNaming IsNot Nothing Then
                chkDebugIgnoreNaming.Location = New Point(butRefresh.Left, actionBottom + 2)
                chkDebugIgnoreNaming.BringToFront()
            End If

            If cacheAgeLabel IsNot Nothing Then
                If chkDebugIgnoreNaming IsNot Nothing Then
                    cacheAgeLabel.Location = New Point(chkDebugIgnoreNaming.Right + uiPx(10), chkDebugIgnoreNaming.Top + Math.Max(0, CInt((chkDebugIgnoreNaming.Height - cacheAgeLabel.Height) / 2)))
                Else
                    cacheAgeLabel.Location = New Point(butRefresh.Left, actionBottom + 2)
                End If

                If cacheAgeLabel.Right > parentControl.ClientSize.Width - uiPx(4) Then
                    cacheAgeLabel.Location = New Point(butRefresh.Left, If(chkDebugIgnoreNaming IsNot Nothing, chkDebugIgnoreNaming.Bottom + 1, actionBottom + 2))
                End If

                cacheAgeLabel.BringToFront()
            End If

            If syncProgressLabel IsNot Nothing AndAlso syncProgressBar IsNot Nothing Then
                Dim progressTop As Integer = actionBottom + 2
                If chkDebugIgnoreNaming IsNot Nothing Then progressTop = Math.Max(progressTop, chkDebugIgnoreNaming.Bottom + 2)
                If cacheAgeLabel IsNot Nothing Then progressTop = Math.Max(progressTop, cacheAgeLabel.Bottom + 2)

                syncProgressLabel.Location = New Point(butRefresh.Left, progressTop)
                syncProgressBar.Location = New Point(butRefresh.Left, syncProgressLabel.Bottom + 1)
                syncProgressBar.Width = Math.Max(uiPx(140), Math.Min(uiPx(220), parentControl.ClientSize.Width - butRefresh.Left - uiPx(8)))
            End If

            butRefresh.BringToFront()
            butSyncStatus.BringToFront()
        Catch
            'Fallback: keep the buttons near their original area if the ToolStrip geometry is unavailable.
            Dim fallbackTop As Integer = Math.Max(0, butRefresh.Top)
            butRefresh.Location = New Point(Math.Max(4, butRefresh.Left), fallbackTop)
            butSyncStatus.Location = New Point(butRefresh.Right + 4, fallbackTop)

            If butCleanupQuick IsNot Nothing Then
                butCleanupQuick.Location = New Point(butSyncStatus.Right + 4, fallbackTop)
                If butCleanupQuick.Right > parentControl.ClientSize.Width - uiPx(2) Then
                    butCleanupQuick.Location = New Point(butRefresh.Left, Math.Max(butRefresh.Bottom, butSyncStatus.Bottom) + 4)
                End If
                butCleanupQuick.BringToFront()
            End If

            Dim actionBottom As Integer = Math.Max(butRefresh.Bottom, butSyncStatus.Bottom)
            If butCleanupQuick IsNot Nothing Then actionBottom = Math.Max(actionBottom, butCleanupQuick.Bottom)

            If chkDebugIgnoreNaming IsNot Nothing Then
                chkDebugIgnoreNaming.Location = New Point(butRefresh.Left, actionBottom + 2)
                chkDebugIgnoreNaming.BringToFront()
            End If

            If cacheAgeLabel IsNot Nothing Then
                If chkDebugIgnoreNaming IsNot Nothing Then
                    cacheAgeLabel.Location = New Point(chkDebugIgnoreNaming.Right + uiPx(10), chkDebugIgnoreNaming.Top)
                Else
                    cacheAgeLabel.Location = New Point(butRefresh.Left, actionBottom + 2)
                End If
                cacheAgeLabel.BringToFront()
            End If

            If syncProgressLabel IsNot Nothing AndAlso syncProgressBar IsNot Nothing Then
                Dim progressTop As Integer = actionBottom + 2
                If chkDebugIgnoreNaming IsNot Nothing Then progressTop = Math.Max(progressTop, chkDebugIgnoreNaming.Bottom + 2)
                If cacheAgeLabel IsNot Nothing Then progressTop = Math.Max(progressTop, cacheAgeLabel.Bottom + 2)

                syncProgressLabel.Location = New Point(butRefresh.Left, progressTop)
                syncProgressBar.Location = New Point(butRefresh.Left, syncProgressLabel.Bottom + 1)
                syncProgressBar.Width = Math.Max(uiPx(140), Math.Min(uiPx(220), parentControl.ClientSize.Width - butRefresh.Left - uiPx(8)))
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
        removeGetLatestAllMenuItem()
        ensureOnlineCheckbox()
        applyDpiFriendlyTaskPaneUi()

        liveChangeCheckTimer = New System.Windows.Forms.Timer()
        liveChangeCheckTimer.Interval = 30000 '30 seconds
        liveChangeCheckTimer.Start()

        graphicalSelectionSyncTimer = New System.Windows.Forms.Timer()
        graphicalSelectionSyncTimer.Interval = 500 'Keep the add-in tree aligned to graphical selections without SVN/server work.
        graphicalSelectionSyncTimer.Start()


    End Sub

    Private Sub UserControl1_Resize(sender As Object, e As EventArgs) Handles MyBase.Resize
        Try
            positionRefreshAndSyncButtonsBesideCommit()
            removeGetLatestAllMenuItem()
            positionOnlineCheckboxBesideVersion()
            ensureTreeStartDragHandle()
            If userAdjustedTreeStart Then positionTreeStartDragHandle()
        Catch
        End Try
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

    Private Sub graphicalSelectionSyncTimer_Tick(sender As Object, e As EventArgs) Handles graphicalSelectionSyncTimer.Tick
        'Client-side visual sync only:
        'If the user clicks a component in the SOLIDWORKS graphics/tree area, select the matching
        'node in the SVN task-pane tree. This does not call SVN and does not resolve components.
        Try
            If iSwApp Is Nothing Then Exit Sub
            If TreeView1 Is Nothing OrElse TreeView1.Nodes Is Nothing OrElse TreeView1.Nodes.Count = 0 Then Exit Sub

            Dim selectedPath As String = getCurrentGraphicalSelectionCadPath()

            If String.IsNullOrWhiteSpace(selectedPath) Then Exit Sub

            Try
                selectedPath = Path.GetFullPath(selectedPath)
            Catch
            End Try

            If String.Equals(selectedPath, lastGraphicalSelectionPath, StringComparison.OrdinalIgnoreCase) Then Exit Sub
            lastGraphicalSelectionPath = selectedPath

            selectTreeNodeByCadPath(selectedPath)
        Catch
        End Try
    End Sub

    Private Function getCurrentGraphicalSelectionCadPath() As String
        Try
            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDoc Is Nothing Then Return ""

            If activeDoc.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then
                Return activeDoc.GetPathName()
            End If

            Dim selMgr As SelectionMgr = activeDoc.SelectionManager
            If selMgr Is Nothing Then Return ""

            Dim selCount As Integer = selMgr.GetSelectedObjectCount2(-1)
            If selCount <= 0 Then Return ""

            For i As Integer = 1 To selCount
                Dim comp As Component2 = Nothing

                Try
                    comp = selMgr.GetSelectedObjectsComponent4(i, -1)
                Catch
                    comp = Nothing
                End Try

                If comp Is Nothing Then Continue For

                Dim compPath As String = ""

                Try
                    compPath = comp.GetPathName()
                Catch
                    compPath = ""
                End Try

                If Not String.IsNullOrWhiteSpace(compPath) AndAlso isCadPathForSync(compPath) Then
                    Return compPath
                End If
            Next
        Catch
        End Try

        Return ""
    End Function

    Private Sub selectTreeNodeByCadPath(ByVal filePath As String)
        If String.IsNullOrWhiteSpace(filePath) Then Exit Sub

        Dim normalizedTarget As String = normalizePathForNodeMatch(filePath)
        If String.IsNullOrWhiteSpace(normalizedTarget) Then Exit Sub

        Dim matchedNode As TreeNode = Nothing
        Dim visitedCount As Integer = 0

        Try
            TreeView1.BeginUpdate()

            For Each rootNode As TreeNode In TreeView1.Nodes
                matchedNode = findTreeNodeByCadPathRecursive(rootNode, normalizedTarget, visitedCount, 750)
                If matchedNode IsNot Nothing Then Exit For
            Next

            If matchedNode Is Nothing Then
                clearGraphicalTreeHighlight()
                Exit Sub
            End If

            expandParentsForTreeNode(matchedNode)
            TreeView1.SelectedNode = matchedNode
            matchedNode.EnsureVisible()
            applyGraphicalTreeHighlight(matchedNode)

            'Important:
            'A graphics-area click is only a visual/tree alignment helper.
            'Do not mark it as a deliberate Sync branch selection.
            'If the user wants Sync to target this branch, they can click the node in the SVN tree.
        Catch
        Finally
            Try
                TreeView1.EndUpdate()
            Catch
            End Try
        End Try
    End Sub

    Private Function findTreeNodeByCadPathRecursive(ByVal node As TreeNode,
                                                     ByVal normalizedTarget As String,
                                                     ByRef visitedCount As Integer,
                                                     ByVal maxVisitedNodes As Integer) As TreeNode
        If node Is Nothing Then Return Nothing

        visitedCount += 1
        If visitedCount > maxVisitedNodes Then Return Nothing

        Try
            Dim nodePath As String = normalizePathForNodeMatch(getCadPathFromTreeNode(node))
            If nodePath <> "" AndAlso String.Equals(nodePath, normalizedTarget, StringComparison.OrdinalIgnoreCase) Then
                Return node
            End If
        Catch
        End Try

        'If this is a lazy assembly node, load its immediate children only when the graphics
        'selection changed and we are actively trying to find that one selected file.
        'This avoids a server call and avoids resolving the whole car during normal idle time.
        Try
            If hasLazyPlaceholder(node) Then
                loadImmediateChildrenForNode(node)
            End If
        Catch
        End Try

        Try
            For Each childNode As TreeNode In node.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For

                Dim found As TreeNode = findTreeNodeByCadPathRecursive(childNode, normalizedTarget, visitedCount, maxVisitedNodes)
                If found IsNot Nothing Then Return found
            Next
        Catch
        End Try

        Return Nothing
    End Function

    Private Sub expandParentsForTreeNode(ByVal node As TreeNode)
        Try
            Dim parentNode As TreeNode = node.Parent

            While parentNode IsNot Nothing
                parentNode.Expand()
                parentNode = parentNode.Parent
            End While
        Catch
        End Try
    End Sub

    Private Sub clearGraphicalTreeHighlight()
        Try
            If lastGraphicallyHighlightedTreeNode Is Nothing Then Exit Sub

            Dim oldNode As TreeNode = lastGraphicallyHighlightedTreeNode
            lastGraphicallyHighlightedTreeNode = Nothing

            If oldNode.TreeView IsNot Nothing Then
                oldNode.Text = stripStatusSuffix(oldNode.Text)

                'Important: graphical selection draws white text while selected/highlighted.
                'When the user selects off, reset the stored node ForeColor back to normal black.
                oldNode.ForeColor = normalTreeTextColor()
                setNodeColorFromStatus(oldNode)
                oldNode.TreeView.Invalidate(oldNode.Bounds)
            End If
        Catch
            lastGraphicallyHighlightedTreeNode = Nothing
        End Try
    End Sub

    Private Sub applyGraphicalTreeHighlight(ByVal node As TreeNode)
        If node Is Nothing Then Exit Sub

        Try
            If lastGraphicallyHighlightedTreeNode IsNot Nothing AndAlso Not Object.ReferenceEquals(lastGraphicallyHighlightedTreeNode, node) Then
                clearGraphicalTreeHighlight()
            End If

            lastGraphicallyHighlightedTreeNode = node

            'Use owner-draw for the dark highlight instead of permanently changing ForeColor.
            'Permanently setting ForeColor to white causes the text to stay white after selecting off.
            If node.TreeView IsNot Nothing Then node.TreeView.Invalidate(node.Bounds)
        Catch
        End Try
    End Sub


    Private Sub TreeView1_DrawNode(ByVal sender As Object, ByVal e As DrawTreeNodeEventArgs) Handles TreeView1.DrawNode
        If e Is Nothing OrElse e.Node Is Nothing Then Exit Sub

        Try
            Dim tv As TreeView = TryCast(sender, TreeView)
            If tv Is Nothing Then
                e.DrawDefault = True
                Exit Sub
            End If

            Dim isSelected As Boolean = ((e.State And TreeNodeStates.Selected) = TreeNodeStates.Selected)
            Dim isGraphicalHighlight As Boolean = False
            Dim isBatchSelected As Boolean = False

            Try
                isGraphicalHighlight = lastGraphicallyHighlightedTreeNode IsNot Nothing AndAlso Object.ReferenceEquals(e.Node, lastGraphicallyHighlightedTreeNode)
            Catch
                isGraphicalHighlight = False
            End Try

            Try
                isBatchSelected = isTreeNodeBatchSelected(e.Node)
            Catch
                isBatchSelected = False
            End Try

            Dim backColor As Color = e.Node.BackColor
            Dim foreColor As Color = e.Node.ForeColor

            If isSelected OrElse isGraphicalHighlight OrElse isBatchSelected Then
                backColor = treeSelectionBackColor
                foreColor = treeSelectionForeColor
            Else
                'If a previous graphical/selected highlight left selection colors on the node,
                'draw it with normal tree colors once it is no longer selected/highlighted.
                If backColor = Color.Empty OrElse backColor = treeSelectionBackColor Then backColor = tv.BackColor
                If foreColor = Color.Empty OrElse foreColor = treeSelectionForeColor Then foreColor = tv.ForeColor
            End If

            Dim textBounds As Rectangle = e.Bounds
            If textBounds.Width < 1 Then textBounds.Width = Math.Max(1, tv.ClientSize.Width - textBounds.Left - uiPx(4))

            Using b As New SolidBrush(backColor)
                e.Graphics.FillRectangle(b, textBounds)
            End Using

            Dim flags As TextFormatFlags = TextFormatFlags.NoPrefix Or TextFormatFlags.VerticalCenter Or TextFormatFlags.SingleLine Or TextFormatFlags.NoPadding
            TextRenderer.DrawText(e.Graphics, e.Node.Text, tv.Font, textBounds, foreColor, backColor, flags)
        Catch
            e.DrawDefault = True
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
        removeGetLatestAllMenuItem()
        ensureOnlineCheckbox()
        applyDpiFriendlyTaskPaneUi()

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

        'Fast Get Locks behavior:
        'Normal click = exact single tree file.
        'Shift-click = every file currently batch-selected in the SVN tree.
        'Both paths go directly to the asynchronous path-first lock workflow, so this does not
        'resolve the assembly or invoke the slower "With Dependents" code.
        Dim selectedTreePaths() As String = Nothing
        Dim useBatchSelection As Boolean = (ModifierKeys And Keys.Shift) = Keys.Shift

        If useBatchSelection Then
            selectedTreePaths = getBatchSelectedTreeCadPathsForAction(includeSingleSelectedNode:=True)
        Else
            selectedTreePaths = getSelectedTreeCadPathsForFileAction()
        End If

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

        'Fast Commit behavior:
        'Normal click = exact single tree file.
        'Shift-click = every file currently batch-selected in the SVN tree.
        'Both paths use the asynchronous path-first commit workflow and do not expand dependents.
        Dim selectedTreePaths() As String = Nothing
        Dim useBatchSelection As Boolean = (ModifierKeys And Keys.Shift) = Keys.Shift

        If useBatchSelection Then
            selectedTreePaths = getBatchSelectedTreeCadPathsForAction(includeSingleSelectedNode:=True)
        Else
            selectedTreePaths = getSelectedTreeCadPathsForFileAction()
        End If

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
        Dim selectedTreePaths() As String = getBatchSelectedTreeCadPathsForAction(includeSingleSelectedNode:=True)

        If selectedTreePaths IsNot Nothing AndAlso selectedTreePaths.Length > 0 Then
            unlockPathsLockedOnly(selectedTreePaths)
        Else
            unlockDocs(GetSelectedModDocList(iSwApp))
        End If

        updateStatusStrip()
    End Sub
    Private Sub dropDownUnlockWithDependents_Click(sender As Object, e As EventArgs) Handles dropDownUnlockWithDependents.Click
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Error: Active Document not found") : Exit Sub

        'Optimized path: build the dependent candidate list, then the backend filters to files you actually locked.
        unlockDocs(getComponentsOfAssemblyOptionalUpdateTree(GetSelectedModDocList(iSwApp)))
        updateStatusStrip()
    End Sub
    Private Sub dropDownUnlockAll_Click(sender As Object, e As EventArgs) Handles dropDownUnlockAll.Click
        iSwApp.SendMsgToUser2(
            "Release Locks All has been disabled for safety." & vbCrLf & vbCrLf &
            "Select the file(s) in the SVN tree, or use Unlock && Revert > With Dependents. The backend will only unlock/revert files you actually have locked.",
            swMessageBoxIcon_e.swMbInformation,
            swMessageBoxBtn_e.swMbOk
        )
        updateStatusStrip()
    End Sub

    ' ### Get Latest
    Private Sub ToolStripDropDownButGetLatest_ButtonClick(sender As Object, e As EventArgs) Handles ToolStripDropDownButGetLatest.ButtonClick
        Dim selectedTreePaths() As String = getBatchSelectedTreeCadPathsForAction(includeSingleSelectedNode:=True)

        If selectedTreePaths IsNot Nothing AndAlso selectedTreePaths.Length > 0 Then
            myGetLatestOrRevertPaths(selectedTreePaths, getLatestType.update, bVerbose:=True)
        Else
            Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
            If modDoc Is Nothing Then iSwApp.SendMsgToUser("Error: Active Document not found") : Exit Sub

            myGetLatestOrRevert(GetSelectedModDocList(iSwApp),, bVerbose:=True)
        End If

        updateStatusStrip()
    End Sub
    Private Sub dropDownGetLatestAllOpenFiles_Click(sender As Object, e As EventArgs) Handles dropDownGetLatestAllOpenFiles.Click
        Dim modDocArr() As ModelDoc2 = getAllOpenDocs(bMustBeVisible:=False)

        saveAllOpenFiles(bShowError:=True)

        myGetLatestOrRevert(modDocArr,, bVerbose:=True)
        updateStatusStrip()
    End Sub
    Private Sub dropDownGetLatestAll_Click(sender As Object, e As EventArgs) Handles dropDownGetLatestAll.Click
        iSwApp.SendMsgToUser2(
            "Get Latest All has been disabled." & vbCrLf & vbCrLf &
            "Use Sync first, then select the specific out-of-date file(s) in the SVN tree and click Get Latest." & vbCrLf &
            "Tip: Ctrl-click toggles multiple tree files. Shift-click selects a visible range.",
            swMessageBoxIcon_e.swMbInformation,
            swMessageBoxBtn_e.swMbOk
        )
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

        Dim debugWatch As System.Diagnostics.Stopwatch = Nothing
        Dim debugNotes As New List(Of String)()

        If syncDebugEnabled() Then
            debugWatch = System.Diagnostics.Stopwatch.StartNew()
        End If

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
        Dim phaseStartMs As Long = 0

        If selectedNode Is Nothing Then
            'No tree node selected:
            'Default to Level 1 only under the active/root assembly.
            Dim rootNode As TreeNode = getRootTreeNodeForSync()

            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

            If rootNode IsNot Nothing Then
                loadOneExtraLazyLevelForSync(rootNode)
                syncPaths = collectImmediateChildCadPathsForSync(rootNode)
            End If

            If debugWatch IsNot Nothing Then
                debugNotes.Add("Default Level-1/Level-2 load/collect: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            End If
        Else
            'Selected Level 0 -> sync selected node + Level 1 + Level 2.
            'Selected Level 1 -> sync selected node + Level 2 + Level 3.
            'This deliberately loads only one extra lazy level below the old behavior, not the whole tree.
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

            loadOneExtraLazyLevelForSync(selectedNode)
            syncPaths = collectSelectedBranchCadPathsForSync(selectedNode)

            If debugWatch IsNot Nothing Then
                debugNotes.Add("Selected branch load/collect: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
                debugNotes.Add("Selected node: " & selectedNode.Text)
            End If
        End If

        If syncPaths Is Nothing OrElse syncPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No CAD file paths were found in the selected tree branch to sync.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Pre-background total: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
        End If

        startAsyncSyncStatus(syncPaths, "Syncing...", String.Join(vbCrLf, debugNotes.ToArray()))
    End Sub

    Private Sub performSyncStatusWholeCar()
        'Explicit slow operation. This recursively loads the visible active assembly tree
        'and server-checks every CAD path it can find. It does NOT download geometry.

        Dim debugWatch As System.Diagnostics.Stopwatch = Nothing
        Dim debugNotes As New List(Of String)()

        If syncDebugEnabled() Then
            debugWatch = System.Diagnostics.Stopwatch.StartNew()
        End If

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

        Dim phaseStartMs As Long = 0

        Try
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

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

            If debugWatch IsNot Nothing Then
                debugNotes.Add("Whole-car lazy tree load: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            End If
        Catch
        End Try

        If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

        Dim syncPaths() As String = collectCurrentTreeCadPaths()

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Whole-car path collection: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
        End If

        If syncPaths Is Nothing OrElse syncPaths.Length = 0 Then
            iSwApp.SendMsgToUser2("No CAD file paths were found in the current tree to sync.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If debugWatch IsNot Nothing Then
            debugNotes.Add("Pre-background total: " & debugWatch.ElapsedMilliseconds.ToString() & " ms")
        End If

        startAsyncSyncStatus(syncPaths, "Syncing whole car...", String.Join(vbCrLf, debugNotes.ToArray()))
    End Sub

    Private Sub startAsyncSyncStatus(ByVal syncPaths() As String,
                                     Optional ByVal pendingText As String = "Syncing...",
                                     Optional ByVal preSyncTimingLog As String = "")
        If syncPaths Is Nothing OrElse syncPaths.Length = 0 Then Exit Sub

        If syncStatusInProgress Then
            iSwApp.SendMsgToUser2("A Sync Status operation is already running in the background.",
                swMessageBoxIcon_e.swMbInformation,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Dim pathsForBackground As String() = CType(syncPaths.Clone(), String())
        Dim savedPathForBackground As String = savedPATH
        Dim overallWatch As System.Diagnostics.Stopwatch = Nothing

        If syncDebugEnabled() Then
            overallWatch = System.Diagnostics.Stopwatch.StartNew()
        End If

        syncStatusInProgress = True
        markSyncPendingForFilePathsPublic(pathsForBackground, True, pendingText)
        setSyncProgressVisible(True, pendingText, pathsForBackground.Length)

        System.Threading.Tasks.Task.Run(Sub()
                                            Dim errorMessage As String = ""
                                            Dim timingLog As String = ""
                                            Dim serverStatus As SVNStatus = Nothing
                                            Dim backgroundWatch As System.Diagnostics.Stopwatch = Nothing

                                            If syncDebugEnabled() Then
                                                backgroundWatch = System.Diagnostics.Stopwatch.StartNew()
                                            End If

                                            Try
                                                serverStatus = svnModule.getServerStatusForFilePathsBackgroundPublic(pathsForBackground, savedPathForBackground, errorMessage, timingLog)
                                            Catch ex As Exception
                                                errorMessage = ex.Message
                                            End Try

                                            If backgroundWatch IsNot Nothing Then
                                                Try
                                                    If Not String.IsNullOrWhiteSpace(timingLog) Then timingLog &= vbCrLf
                                                    timingLog &= "Background SVN status call: " & backgroundWatch.ElapsedMilliseconds.ToString() & " ms"
                                                Catch
                                                End Try
                                            End If

                                            Try
                                                If Me.IsHandleCreated Then
                                                    Dim totalElapsedMs As Long = -1

                                                    If overallWatch IsNot Nothing Then
                                                        totalElapsedMs = overallWatch.ElapsedMilliseconds
                                                    End If

                                                    Me.BeginInvoke(New MethodInvoker(Sub() finishAsyncSyncStatus(pathsForBackground, serverStatus, errorMessage, timingLog, totalElapsedMs, preSyncTimingLog)))
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
                                      Optional ByVal timingLog As String = "",
                                      Optional ByVal totalElapsedMs As Long = -1,
                                      Optional ByVal preSyncTimingLog As String = "")
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
            Try
                showSyncDebugWindow("Sync Status failed.", syncPaths, preSyncTimingLog, timingLog, totalElapsedMs, errorMessage)
            Catch
            End Try

            iSwApp.SendMsgToUser2("Sync Status failed." & vbCrLf & vbCrLf & errorMessage,
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        If serverStatus Is Nothing Then
            Try
                showSyncDebugWindow("Sync Status failed. No SVN status was returned.", syncPaths, preSyncTimingLog, timingLog, totalElapsedMs, "")
            Catch
            End Try

            iSwApp.SendMsgToUser2("Sync Status failed. No SVN status was returned.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk)
            Exit Sub
        End If

        Try
            svnModule.applyServerStatusFromBackgroundPublic(serverStatus)
        Catch
        End Try

        Try
            recolorTreeNodesForFilePathsPublic(syncPaths)
        Catch
            Try
                recolorCurrentTreeFromStatus()
            Catch
            End Try
        End Try

        Try
            setRefreshTreeButtonNormal()
        Catch
        End Try

        Try
            showSyncDebugWindow("Sync Status finished.", syncPaths, preSyncTimingLog, timingLog, totalElapsedMs, "")
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
            If lastGraphicallyHighlightedTreeNode IsNot Nothing AndAlso
               Object.ReferenceEquals(selectedNode, lastGraphicallyHighlightedTreeNode) AndAlso
               Not Object.ReferenceEquals(selectedNode, lastUserClickedTreeNodeForSync) Then
                Return Nothing
            End If

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
            'Selected branch sync is intentionally bounded:
            'selected node + loaded direct children + loaded grandchildren only.
            'It does not recurse forever, so it will not accidentally become whole-car sync.
            addTreeNodePathToSyncList(selectedNode, seen, output)

            For Each childNode As TreeNode In selectedNode.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For
                addTreeNodePathToSyncList(childNode, seen, output)

                For Each grandChildNode As TreeNode In childNode.Nodes
                    If isLazyPlaceholderNode(grandChildNode) Then Continue For
                    addTreeNodePathToSyncList(grandChildNode, seen, output)
                Next
            Next
        End If

        'Never fall back to collectCurrentTreeCadPaths() here.
        'That fallback turns a failed/empty selected-branch sync into a whole loaded-tree sync,
        'which breaks the controlled Level 0 / Level 1 / Level 2 / Level 3 sync behavior.
        If output.Count = 0 Then Return Nothing

        Return output.ToArray()
    End Function
    Private Function collectImmediateChildCadPathsForSync(ByVal parentNode As TreeNode) As String()
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If parentNode IsNot Nothing Then
            'Default/no-selection sync stays bounded to one level lower than before:
            'direct children + loaded grandchildren only. It does not include the root node
            'and it does not recurse through the whole car.
            For Each childNode As TreeNode In parentNode.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For
                addTreeNodePathToSyncList(childNode, seen, output)

                For Each grandChildNode As TreeNode In childNode.Nodes
                    If isLazyPlaceholderNode(grandChildNode) Then Continue For
                    addTreeNodePathToSyncList(grandChildNode, seen, output)
                Next
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
        myCleanup()
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

    Private Sub boxCheck_Check(sender As Object, e As EventArgs)
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
                clearGraphicalTreeHighlight()
                TreeView1.SelectedNode = e.Node
                lastUserClickedTreeNodeForSync = e.Node

                If (ModifierKeys And Keys.Control) = Keys.Control Then
                    toggleBatchTreeNode(e.Node)
                ElseIf (ModifierKeys And Keys.Shift) = Keys.Shift Then
                    selectBatchTreeRange(e.Node)
                Else
                    clearBatchTreeSelection()
                    lastBatchAnchorTreeNode = e.Node
                End If
            End If
        Catch
        End Try

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

        clearBatchTreeSelection(False)
        TreeView1.Nodes.Clear()
        TreeView1.Nodes.Insert(0, clonedNode)
        TreeView1.Nodes(0).Expand()
        'TreeView1.ExpandAll()
        TreeView1.Show()
        ensureTreeStartDragHandle()

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
    End Sub

    Private Sub loadOneExtraLazyLevelForSync(ByVal parentNode As TreeNode)
        If parentNode Is Nothing Then Exit Sub

        'This is intentionally not recursive.
        'It loads the selected/root node's immediate children, then loads one more level
        'under those children so normal Sync has cache data for one level lower.
        loadImmediateChildrenForNode(parentNode)

        For Each childNode As TreeNode In parentNode.Nodes
            If isLazyPlaceholderNode(childNode) Then Continue For
            loadImmediateChildrenForNode(childNode)
        Next
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

        'Reset normal text color every time status is reapplied.
        'Selected/highlighted nodes still draw white through TreeView1_DrawNode.
        rootNode.ForeColor = normalTreeTextColor()

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
