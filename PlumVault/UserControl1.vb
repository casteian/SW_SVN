
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

<ComVisible(True)>
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
    Private taskPaneClosing As Boolean = False
    Private WithEvents butSyncStatus As Button
    Private WithEvents chkDebugIgnoreNaming As CheckBox
    Private WithEvents onlineCheckBox As CheckBox
    Private syncProgressBar As ProgressBar
    Private syncProgressLabel As Label
    Private syncStatusContextMenu As ContextMenuStrip
    Private WithEvents syncDebugTimingMenuItem As ToolStripMenuItem
    Private syncDebugTimingEnabled As Boolean = False
    Private WithEvents butCleanupQuick As Button
    Private copyLegacyDataToSvnMenuItem As ToolStripMenuItem
    Private cacheAgeLabel As Label
    Private WithEvents cacheAgeTimer As System.Windows.Forms.Timer

    'Deferred native SOLIDWORKS UI work.
    'Never change live document read-only state or refresh FeatureManager inside an SVN
    'completion callback. Queue stable file paths and reacquire COM objects later.
    Private WithEvents deferredSolidWorksUiTimer As System.Windows.Forms.Timer
    Private ReadOnly pendingWriteAccessPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    Private ReadOnly pendingFeatureTreeRefreshPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    Private pendingSvnTreeStructureRefresh As Boolean = False
    Private deferredSolidWorksUiAttemptCount As Integer = 0
    Private Const MAX_DEFERRED_SOLIDWORKS_UI_ATTEMPTS As Integer = 8

    Private Const LAZY_LOAD_PLACEHOLDER_TEXT As String = "<load children>"
    Private syncStatusInProgress As Boolean = False
    Private refreshTreeNeedsUpdate As Boolean = False
    Private normalRefreshTreeBackColor As Color
    Private lastLiveCheckedActivePath As String = ""
    Private quietActiveStatusCheckInProgress As Boolean = False
    Private lastQuietActiveStatusCheckUtc As DateTime = DateTime.UtcNow
    Private Const QUIET_ACTIVE_STATUS_INTERVAL_MINUTES As Double = 3.0
    Private lastGraphicalSelectionPath As String = ""
    Private lastGraphicalSelectionComponentName As String = ""
    Private lastExplicitSvnTreeClickUtc As DateTime = DateTime.MinValue
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

    'Stable tree-action selection identity.
    'TreeNode and Component2 COM objects are replaced during tree rebuilds. File actions
    'must therefore remember the physical path the user clicked rather than requiring
    'reference equality with an older TreeNode instance.
    Private lastUserClickedTreePathForActions As String = ""
    Private lastUserClickedTreeTextForActions As String = ""

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


    Private Sub ensureCopyLegacyDataToSvnMenuItem()
        Try
            If ToolStripSplitButFolder Is Nothing Then Exit Sub

            If copyLegacyDataToSvnMenuItem Is Nothing Then
                For Each existingItem As ToolStripItem In ToolStripSplitButFolder.DropDownItems
                    If String.Equals(existingItem.Name, "copyLegacyDataToSvnMenuItem", StringComparison.OrdinalIgnoreCase) Then
                        copyLegacyDataToSvnMenuItem = TryCast(existingItem, ToolStripMenuItem)
                        Exit For
                    End If
                Next
            End If

            If copyLegacyDataToSvnMenuItem Is Nothing Then
                Dim separator As ToolStripSeparator = Nothing

                For Each existingItem As ToolStripItem In ToolStripSplitButFolder.DropDownItems
                    If String.Equals(existingItem.Name, "legacyImportSeparator", StringComparison.OrdinalIgnoreCase) Then
                        separator = TryCast(existingItem, ToolStripSeparator)
                        Exit For
                    End If
                Next

                If separator Is Nothing Then
                    separator = New ToolStripSeparator()
                    separator.Name = "legacyImportSeparator"
                    ToolStripSplitButFolder.DropDownItems.Add(separator)
                End If

                copyLegacyDataToSvnMenuItem = New ToolStripMenuItem()
                copyLegacyDataToSvnMenuItem.Name = "copyLegacyDataToSvnMenuItem"
                copyLegacyDataToSvnMenuItem.Text = "Copy Legacy Data to SVN..."
                copyLegacyDataToSvnMenuItem.ToolTipText = "Pack and Go an old assembly into SVN with controlled GRC27/CFD27 re-identification."
                ToolStripSplitButFolder.DropDownItems.Add(copyLegacyDataToSvnMenuItem)
            End If

            RemoveHandler copyLegacyDataToSvnMenuItem.Click, AddressOf copyLegacyDataToSvnMenuItem_Click
            AddHandler copyLegacyDataToSvnMenuItem.Click, AddressOf copyLegacyDataToSvnMenuItem_Click
        Catch
        End Try
    End Sub

    Private Sub copyLegacyDataToSvnMenuItem_Click(ByVal sender As Object, ByVal e As EventArgs)
        Try
            svnModule.showLegacyImportWizardPublic()
        Catch ex As Exception
            Try
                iSwApp.SendMsgToUser2(
                    "Copy Legacy Data to SVN could not start." & vbCrLf & vbCrLf & ex.Message,
                    swMessageBoxIcon_e.swMbStop,
                    swMessageBoxBtn_e.swMbOk)
            Catch
                MessageBox.Show("Copy Legacy Data to SVN could not start." & vbCrLf & vbCrLf & ex.Message,
                                "PlumVault",
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error)
            End Try
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
            positionFileActionsAboveRepositoryPath()

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

    Private Sub positionFileActionsAboveRepositoryPath()
        Try
            If FileActionToolStrip Is Nothing OrElse localRepoPath Is Nothing OrElse ToolStrip1 Is Nothing Then Exit Sub

            Dim edge As Integer = uiPx(3)
            Dim verticalGap As Integer = uiPx(5)
            Dim availableWidth As Integer = Math.Max(uiPx(150), Me.ClientSize.Width - (edge * 2))

            Me.SuspendLayout()

            'ToolStrip item fonts become DPI-sized at runtime, so fixed designer Y values are
            'not reliable. Size the action row from its actual preferred height and flow every
            'control below it; this prevents Save As/Re-ID/Move from covering the repo path.
            FileActionToolStrip.AutoSize = False
            FileActionToolStrip.Left = edge
            FileActionToolStrip.Top = edge
            FileActionToolStrip.Width = availableWidth
            FileActionToolStrip.Height = Math.Max(uiPx(32), FileActionToolStrip.PreferredSize.Height)

            localRepoPath.Left = edge
            localRepoPath.Top = FileActionToolStrip.Bottom + verticalGap
            localRepoPath.Width = availableWidth
            localRepoPath.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right

            'Version/Online used to sit at a fixed designer Y (70) chosen for the old, much
            'narrower repo-path box. Now that the box spans the full row width, that fixed
            'position lands on the same row and the same X range, so the two visually collide
            'whenever DPI/font scaling makes the box even slightly taller than the designer
            'assumed. Give Version its own row below the repo path instead of a guessed
            'coordinate; positionOnlineCheckboxBesideVersion (called right after this) already
            'places Online relative to versionLabel's actual position, so fixing this one
            'anchor point fixes both.
            If versionLabel IsNot Nothing Then
                versionLabel.Left = edge
                versionLabel.Top = localRepoPath.Bottom + verticalGap
            End If

            Dim toolStripTop As Integer = localRepoPath.Bottom + verticalGap

            If versionLabel IsNot Nothing Then
                toolStripTop = versionLabel.Bottom + verticalGap
            End If

            ToolStrip1.Left = edge
            ToolStrip1.Top = toolStripTop
            ToolStrip1.Width = Math.Min(availableWidth, uiPx(267))

            'TreeView1's designer position (Y 530) assumed the old, shorter top area. Give it
            'a safe gap below wherever the icon toolstrip actually ends now, instead of a
            'fixed value that can leave it starting above the last icon button.
            If TreeView1 IsNot Nothing Then
                Dim minimumTreeTop As Integer = ToolStrip1.Bottom + verticalGap
                If TreeView1.Top < minimumTreeTop Then TreeView1.Top = minimumTreeTop
            End If

            FileActionToolStrip.BringToFront()
        Catch
        Finally
            Try
                Me.ResumeLayout()
            Catch
            End Try
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

    'Tree nodes must not depend on long-lived SOLIDWORKS COM objects for ordinary
    'selection and file actions. TreeNode.Clone preserves Tag references, and a Component2
    'from a document that was closed and reopened can point to a released native object.
    'Store the resolved physical CAD path in TreeNode.Name so clicks/actions can remain
    'path-only and reacquire live SOLIDWORKS objects only when a graphical selection is needed.
    Private Sub setStableTreeNodeCadPath(ByVal node As TreeNode, ByVal filePath As String)
        If node Is Nothing Then Exit Sub

        Dim normalizedPath As String = normalizeTreeActionPath(filePath)

        If String.IsNullOrWhiteSpace(normalizedPath) Then
            node.Name = ""
        Else
            node.Name = normalizedPath
        End If
    End Sub

    Private Function getStableTreeNodeCadPath(ByVal node As TreeNode) As String
        If node Is Nothing Then Return ""

        Try
            Dim storedPath As String = normalizeTreeActionPath(node.Name)
            If Not String.IsNullOrWhiteSpace(storedPath) Then Return storedPath
        Catch
        End Try

        Return ""
    End Function

    Private Function treeNodeRepresentsVirtualComponent(ByVal node As TreeNode) As Boolean
        If node Is Nothing Then Return False

        Try
            Return node.Text.IndexOf("[Virtual]", StringComparison.OrdinalIgnoreCase) >= 0
        Catch
            Return False
        End Try
    End Function

    Private Function normalizeTreeActionPath(ByVal filePath As String) As String
        If String.IsNullOrWhiteSpace(filePath) Then Return ""

        Try
            Return Path.GetFullPath(filePath)
        Catch
            Return filePath.Trim()
        End Try
    End Function

    Private Function isPhysicalCadFilePath(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False
        If Not File.Exists(filePath) Then Return False

        Try
            Dim extension As String = Path.GetExtension(filePath).ToUpperInvariant()
            Return extension = ".SLDPRT" OrElse extension = ".SLDASM" OrElse extension = ".SLDDRW"
        Catch
            Return False
        End Try
    End Function

    Private Function tryResolveTreeNodeFileActionTarget(ByVal node As TreeNode,
                                                        ByRef targetPath As String,
                                                        ByRef resolvedFromVirtualComponent As Boolean,
                                                        ByRef failureReason As String) As Boolean
        targetPath = ""
        resolvedFromVirtualComponent = False
        failureReason = ""

        If node Is Nothing Then
            failureReason = "No tree item is selected."
            Return False
        End If

        If isLazyPlaceholderNode(node) Then
            failureReason = "The selected row is a lazy-loading placeholder, not a CAD file."
            Return False
        End If

        'Primary path: use the stable physical path captured when the live tree was built.
        'This avoids touching a stale Component2/ModelDoc2 RCW merely because the user clicked
        'a tree row after closing and reopening an assembly.
        Dim stableNodePath As String = getStableTreeNodeCadPath(node)

        If isPhysicalCadFilePath(stableNodePath) Then
            targetPath = stableNodePath
            resolvedFromVirtualComponent = treeNodeRepresentsVirtualComponent(node)
            Return True
        End If

        Try
            If TypeOf node.Tag Is Component2 Then
                Dim component As Component2 = CType(node.Tag, Component2)

                If isComponentVirtualSafe(component) Then
                    resolvedFromVirtualComponent = True
                    targetPath = getPhysicalOwnerAssemblyPathForTreeNode(node)

                    If String.IsNullOrWhiteSpace(targetPath) Then
                        failureReason = "The selected virtual component's physical owning assembly could not be resolved."
                        Return False
                    End If
                Else
                    'Critical targeting rule: a physical component resolves only to its own
                    'document path. Never fall back to the active or parent assembly.
                    targetPath = getSafeComponentPath(component)

                    If String.IsNullOrWhiteSpace(targetPath) Then
                        failureReason = "The selected physical component does not currently expose a file path."
                        Return False
                    End If
                End If

            ElseIf TypeOf node.Tag Is ModelDoc2 Then
                Dim model As ModelDoc2 = CType(node.Tag, ModelDoc2)
                Dim directPath As String = getSafeModelPath(model)

                If isPhysicalCadFilePath(directPath) AndAlso
                   directPath.IndexOf("\AppData\Local\Temp\", StringComparison.OrdinalIgnoreCase) < 0 AndAlso
                   Not Path.GetFileName(directPath).Contains("^") Then

                    targetPath = directPath
                Else
                    'A virtual model opened in its own tab has no independent SVN file.
                    'Only this positively identified virtual-model case may map to an assembly.
                    Dim ownerDocument As ModelDoc2 = getOwningPhysicalAssemblyDocumentForVirtualModel(model)

                    If ownerDocument Is Nothing Then
                        failureReason = "The selected document could not be resolved to a physical CAD file."
                        Return False
                    End If

                    resolvedFromVirtualComponent = True
                    targetPath = getSafeModelPath(ownerDocument)
                End If
            Else
                failureReason = "The selected tree item is not attached to a CAD document or component."
                Return False
            End If

        Catch ex As InvalidComObjectException
            failureReason = "The selected tree item became stale after a SOLIDWORKS refresh. Refresh the tree and select it again."
            Return False
        Catch ex As COMException
            failureReason = "SOLIDWORKS could not resolve the selected tree item safely. Refresh the tree and select it again."
            Return False
        Catch
            failureReason = "The selected tree item could not be resolved safely."
            Return False
        End Try

        targetPath = normalizeTreeActionPath(targetPath)

        If Not isPhysicalCadFilePath(targetPath) Then
            failureReason = "The selected tree item does not resolve to an existing physical CAD file."
            targetPath = ""
            Return False
        End If

        Return True
    End Function

    Private Sub rememberExplicitTreeActionSelection(ByVal node As TreeNode)
        lastUserClickedTreeNodeForSync = node
        lastUserClickedTreeTextForActions = ""
        lastUserClickedTreePathForActions = ""

        If node Is Nothing Then Exit Sub

        Try
            lastUserClickedTreeTextForActions = stripStatusSuffix(node.Text)
        Catch
            lastUserClickedTreeTextForActions = ""
        End Try

        Dim resolvedPath As String = ""
        Dim resolvedFromVirtual As Boolean = False
        Dim failureReason As String = ""

        If tryResolveTreeNodeFileActionTarget(node, resolvedPath, resolvedFromVirtual, failureReason) Then
            lastUserClickedTreePathForActions = normalizeTreeActionPath(resolvedPath)
        End If
    End Sub

    Private Function isCurrentTreeSelectionExplicitForFileAction() As Boolean
        Try
            If batchSelectedTreeNodes IsNot Nothing AndAlso batchSelectedTreeNodes.Count > 0 Then Return True
            If TreeView1 Is Nothing OrElse TreeView1.SelectedNode Is Nothing Then Return False

            Dim selectedNode As TreeNode = TreeView1.SelectedNode

            If lastUserClickedTreeNodeForSync IsNot Nothing AndAlso
               Object.ReferenceEquals(selectedNode, lastUserClickedTreeNodeForSync) Then
                Return True
            End If

            If String.IsNullOrWhiteSpace(lastUserClickedTreePathForActions) Then Return False

            Dim resolvedPath As String = ""
            Dim resolvedFromVirtual As Boolean = False
            Dim failureReason As String = ""

            If Not tryResolveTreeNodeFileActionTarget(selectedNode, resolvedPath, resolvedFromVirtual, failureReason) Then Return False

            Return String.Equals(
                normalizeTreeActionPath(resolvedPath),
                normalizeTreeActionPath(lastUserClickedTreePathForActions),
                StringComparison.OrdinalIgnoreCase
            )
        Catch
            Return False
        End Try
    End Function

    Private Function getCurrentExplicitTreeSelectionDiagnostic(ByRef selectedText As String,
                                                               ByRef resolvedPath As String,
                                                               ByRef resolvedFromVirtual As Boolean,
                                                               ByRef failureReason As String) As Boolean
        selectedText = ""
        resolvedPath = ""
        resolvedFromVirtual = False
        failureReason = ""

        If TreeView1 Is Nothing OrElse TreeView1.SelectedNode Is Nothing Then
            failureReason = "No tree item is selected."
            Return False
        End If

        Try
            selectedText = stripStatusSuffix(TreeView1.SelectedNode.Text)
        Catch
            selectedText = ""
        End Try

        Return tryResolveTreeNodeFileActionTarget(
            TreeView1.SelectedNode,
            resolvedPath,
            resolvedFromVirtual,
            failureReason
        )
    End Function

    Private Function getBatchSelectedTreeCadPathsForAction(Optional ByVal includeSingleSelectedNode As Boolean = True) As String()
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        Try
            If batchSelectedTreeNodes IsNot Nothing AndAlso batchSelectedTreeNodes.Count > 0 Then
                For Each node As TreeNode In batchSelectedTreeNodes
                    addTreeNodePathToBatchActionList(node, seen, output)
                Next
            ElseIf includeSingleSelectedNode AndAlso TreeView1 IsNot Nothing AndAlso
                   TreeView1.SelectedNode IsNot Nothing Then
                Dim selectedNode As TreeNode = TreeView1.SelectedNode
                Dim isGraphicallySynchronized As Boolean =
                    lastGraphicallyHighlightedTreeNode IsNot Nothing AndAlso
                    Object.ReferenceEquals(selectedNode, lastGraphicallyHighlightedTreeNode)

                'The visibly selected row is authoritative when it came from a task-pane click
                'or graphical synchronization. A root row selected automatically after a tree
                'rebuild is not a user target and falls back to the active/SW selection.
                If isCurrentTreeSelectionExplicitForFileAction() OrElse isGraphicallySynchronized Then
                    addTreeNodePathToBatchActionList(selectedNode, seen, output)
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

        Dim nodePath As String = ""
        Dim resolvedFromVirtual As Boolean = False
        Dim failureReason As String = ""

        If Not tryResolveTreeNodeFileActionTarget(node, nodePath, resolvedFromVirtual, failureReason) Then Exit Sub
        If Not isCadPathForSync(nodePath) Then Exit Sub

        nodePath = normalizeTreeActionPath(nodePath)

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

        taskPaneClosing = False

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
        ensureCopyLegacyDataToSvnMenuItem()
        ensureOnlineCheckbox()
        applyDpiFriendlyTaskPaneUi()

        'Broad "With Dependents" actions are intentionally not exposed.  Designers should
        'select the exact files they intend to change; internal dependency traversal remains
        'available for close safety, drawing/reference handling, and other integrity checks.
        dropDownGetLocksWithDependents.Visible = False
        dropDownCommitWithDependents.Visible = False
        dropDownCommitAll.Visible = False
        dropDownUnlockWithDependents.Visible = False
        CopyFileNameWithDependentsToolStripMenuItem.Visible = False
        CopyFilesPathsWithDependentsToolStripMenuItem.Visible = False
        CopySvnUrlWithDependentsToolStripMenuItem.Visible = False
        CreateSvnFilelistWithDependentsToolStripMenuItem.Visible = False
        ToolStripDropDownButUnlock.Visible = False

        'Remove broad actions from their parent menus as well as hiding them. This avoids
        'empty drop-down arrows and prevents indirect invocation of dependency expansion.
        ToolStripDropDownButGetLocks.DropDownItems.Remove(dropDownGetLocksWithDependents)
        ToolStripDropDownButCommit.DropDownItems.Remove(dropDownCommitWithDependents)
        ToolStripDropDownButCommit.DropDownItems.Remove(dropDownCommitAll)
        ToolStripDropDownButUnlock.DropDownItems.Remove(dropDownUnlockWithDependents)
        CopyFileNameToolStripMenuItem.DropDownItems.Remove(CopyFileNameWithDependentsToolStripMenuItem)
        CopyFullPathToolStripMenuItem.DropDownItems.Remove(CopyFilesPathsWithDependentsToolStripMenuItem)
        CopySvnUrlToolStripMenuItem.DropDownItems.Remove(CopySvnUrlWithDependentsToolStripMenuItem)
        CreateSvnFilelistToolStripMenuItem.DropDownItems.Remove(CreateSvnFilelistWithDependentsToolStripMenuItem)

        liveChangeCheckTimer = New System.Windows.Forms.Timer()
        liveChangeCheckTimer.Interval = 30000 '30 seconds
        liveChangeCheckTimer.Start()

        graphicalSelectionSyncTimer = New System.Windows.Forms.Timer()
        graphicalSelectionSyncTimer.Interval = 500 'Keep the add-in tree aligned to graphical selections without SVN/server work.
        graphicalSelectionSyncTimer.Start()

        deferredSolidWorksUiTimer = New System.Windows.Forms.Timer()
        deferredSolidWorksUiTimer.Interval = 350
        deferredSolidWorksUiTimer.Stop()

    End Sub

    Private Sub UserControl1_Resize(sender As Object, e As EventArgs) Handles MyBase.Resize
        Try
            positionFileActionsAboveRepositoryPath()
            positionRefreshAndSyncButtonsBesideCommit()
            removeGetLatestAllMenuItem()
            positionOnlineCheckboxBesideVersion()
            ensureTreeStartDragHandle()
            If userAdjustedTreeStart Then positionTreeStartDragHandle()
        Catch
        End Try
    End Sub

    Private Sub liveChangeCheckTimer_Tick(sender As Object, e As EventArgs) Handles liveChangeCheckTimer.Tick
        'Speed rule:
        'Never run a whole-tree/repository status check from this timer. The old live check did
        'that on the UI thread and made SOLIDWORKS feel frozen. The bounded helper below starts
        'at most one background status request for the exact active edit target every 3 minutes.
        '
        'Shutdown safety:
        'SOLIDWORKS can disconnect the add-in COM application object while a WinForms timer tick is
        'already queued. Never let that expected shutdown race escape as an unhandled .NET dialog.
        If taskPaneClosing Then Exit Sub
        If iSwApp Is Nothing Then Exit Sub

        Dim activeModDoc As ModelDoc2 = Nothing

        Try
            activeModDoc = TryCast(iSwApp.ActiveDoc, ModelDoc2)
        Catch ex As InvalidComObjectException
            stopTaskPaneTimers()
            Exit Sub
        Catch ex As COMException
            'SOLIDWORKS may be between documents, rebuilding, or shutting down.
            'Skip this harmless visual-only tick and try again on the next interval.
            Exit Sub
        Catch
            Exit Sub
        End Try

        If activeModDoc Is Nothing Then Exit Sub

        Dim activePath As String = ""

        Try
            activePath = activeModDoc.GetPathName()
        Catch ex As InvalidComObjectException
            Exit Sub
        Catch ex As COMException
            Exit Sub
        Catch
            activePath = ""
        End Try

        If Not String.Equals(activePath, lastLiveCheckedActivePath, StringComparison.OrdinalIgnoreCase) Then
            lastLiveCheckedActivePath = activePath
            setRefreshTreeButtonNormal()
        End If

        'The three-minute background "quiet" server status heartbeat was removed. It started a
        'background svn.exe every three minutes and, on every 30-second tick, asked SOLIDWORKS
        'for the active assembly's in-context edit target (a COM GetEditTarget call) purely to
        'decide what that poll should look at. Neither is needed: every edit/save/commit guard
        'already performs its own live local lock check for its exact target before acting.
        'This timer is now visual-only bookkeeping and makes no SVN or COM traversal calls.
    End Sub

    Private Sub tryStartQuietActiveDocumentStatusCheck(ByVal activePath As String)
        If quietActiveStatusCheckInProgress OrElse syncStatusInProgress Then Exit Sub
        If (DateTime.UtcNow - lastQuietActiveStatusCheckUtc).TotalMinutes < QUIET_ACTIVE_STATUS_INTERVAL_MINUTES Then Exit Sub
        If String.IsNullOrWhiteSpace(activePath) OrElse Not File.Exists(activePath) Then Exit Sub

        Try
            If onlineCheckBox Is Nothing OrElse Not onlineCheckBox.Checked Then Exit Sub
        Catch
            Exit Sub
        End Try

        If Not svnModule.canRunQuietActiveServerStatusCheckPublic(activePath) Then Exit Sub

        Dim pathForBackground As String = activePath
        Dim savedPathForBackground As String = savedPATH
        Dim requestStartedUtc As DateTime = DateTime.UtcNow
        quietActiveStatusCheckInProgress = True
        lastQuietActiveStatusCheckUtc = requestStartedUtc

        System.Threading.Tasks.Task.Run(
            Sub()
                Dim errorMessage As String = ""
                Dim serverStatus As SVNStatus = Nothing

                Try
                    serverStatus = svnModule.getQuietActiveDocumentServerStatusBackgroundPublic(
                        pathForBackground,
                        savedPathForBackground,
                        errorMessage
                    )
                Catch ex As Exception
                    errorMessage = ex.Message
                End Try

                Try
                    If Me.IsHandleCreated Then
                        Me.BeginInvoke(
                            New MethodInvoker(
                                Sub()
                                    finishQuietActiveDocumentStatusCheck(
                                        pathForBackground,
                                        serverStatus,
                                        errorMessage,
                                        requestStartedUtc
                                    )
                                End Sub
                            )
                        )
                    Else
                        quietActiveStatusCheckInProgress = False
                    End If
                Catch
                    quietActiveStatusCheckInProgress = False
                End Try
            End Sub
        )
    End Sub

    Private Sub finishQuietActiveDocumentStatusCheck(ByVal checkedPath As String,
                                                      ByVal serverStatus As SVNStatus,
                                                      ByVal errorMessage As String,
                                                      ByVal requestStartedUtc As DateTime)
        quietActiveStatusCheckInProgress = False

        'This poll is deliberately silent. A transient network/SVN failure must not interrupt
        'modelling; the next three-minute interval tries again. A successful result updates
        'only this path's cache, so the next edit guard can reject a stolen/broken lock without
        'performing any network work on the SOLIDWORKS event thread.
        If taskPaneClosing OrElse Not String.IsNullOrWhiteSpace(errorMessage) OrElse
           serverStatus Is Nothing Then Exit Sub

        If Not svnModule.canApplyQuietActiveServerStatusResultPublic(requestStartedUtc) Then Exit Sub

        Try
            svnModule.applyTargetedServerStatusFromBackgroundPublic(serverStatus)
            recolorTreeNodesForFilePathsPublic(New String() {checkedPath})
        Catch
        End Try
    End Sub

    Private Sub graphicalSelectionSyncTimer_Tick(sender As Object, e As EventArgs) Handles graphicalSelectionSyncTimer.Tick
        'Client-side visual sync only:
        'If the user clicks a component in the SOLIDWORKS graphics/tree area, select the matching
        'node in the SVN task-pane tree. This does not call SVN and does not resolve components.
        syncSvnTreeToCurrentSolidWorksSelectionPublic()
    End Sub

    Public Sub syncSvnTreeToCurrentSolidWorksSelectionPublic()
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(AddressOf syncSvnTreeToCurrentSolidWorksSelectionPublic))
                Exit Sub
            End If

            If iSwApp Is Nothing Then Exit Sub
            If TreeView1 Is Nothing OrElse TreeView1.Nodes Is Nothing OrElse TreeView1.Nodes.Count = 0 Then Exit Sub
            If (DateTime.UtcNow - lastExplicitSvnTreeClickUtc).TotalSeconds < 5.0 Then Exit Sub

            Dim selectedComponentName As String = ""
            Dim selectedPath As String = getCurrentGraphicalSelectionCadPath(selectedComponentName)

            If String.IsNullOrWhiteSpace(selectedPath) Then Exit Sub

            Try
                selectedPath = Path.GetFullPath(selectedPath)
            Catch
            End Try

            If String.Equals(selectedPath, lastGraphicalSelectionPath, StringComparison.OrdinalIgnoreCase) AndAlso
               String.Equals(selectedComponentName, lastGraphicalSelectionComponentName, StringComparison.OrdinalIgnoreCase) Then Exit Sub
            'Only remember a successful match. If a component was just imported and the SVN
            'tree has not refreshed yet, the next event/timer tick must be allowed to retry.
            If selectTreeNodeByCadPath(selectedPath, selectedComponentName) Then
                lastGraphicalSelectionPath = selectedPath
                lastGraphicalSelectionComponentName = selectedComponentName
            End If
        Catch
        End Try
    End Sub

    Private Function getCurrentGraphicalSelectionCadPath(Optional ByRef selectedComponentName As String = "") As String
        selectedComponentName = ""
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

                Try
                    selectedComponentName = comp.Name2
                Catch
                    selectedComponentName = ""
                End Try

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

    Private Function selectTreeNodeByCadPath(ByVal filePath As String,
                                             Optional ByVal selectedComponentName As String = "") As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim normalizedTarget As String = normalizePathForNodeMatch(filePath)
        If String.IsNullOrWhiteSpace(normalizedTarget) Then Return False

        Dim matchedNode As TreeNode = Nothing
        Dim visitedCount As Integer = 0
        Dim updateStarted As Boolean = False

        Try
            'Searching is read-only. Do not bracket every 500-ms retry in BeginUpdate/EndUpdate:
            'when the status/tree cache is not ready yet, a failed lookup used to call EndUpdate
            'anyway and force a full repaint forever until Sync created the matching node.
            For Each rootNode As TreeNode In TreeView1.Nodes
                'Large vehicle assemblies can easily exceed 750 visible/lazy nodes. This work
                'runs only when the selected path changes and performs no SVN/server calls.
                matchedNode = findTreeNodeByCadPathRecursive(
                    rootNode,
                    normalizedTarget,
                    selectedComponentName,
                    visitedCount,
                    10000
                )
                If matchedNode IsNot Nothing Then Exit For
            Next

            If matchedNode Is Nothing Then
                clearGraphicalTreeHighlight()
                Return False
            End If

            TreeView1.BeginUpdate()
            updateStarted = True
            expandParentsForTreeNode(matchedNode)
            TreeView1.SelectedNode = matchedNode
            matchedNode.EnsureVisible()
            applyGraphicalTreeHighlight(matchedNode)
            Return True

            'Important:
            'A graphics-area click is a visual/tree alignment helper. The visibly selected row
            'remains authoritative for file actions, while it is not marked as a deliberate
            'task-pane branch selection for Sync.
        Catch
        Finally
            If updateStarted Then
                Try
                    TreeView1.EndUpdate()
                Catch
                End Try
            End If
        End Try

        Return False
    End Function

    Private Function findTreeNodeByCadPathRecursive(ByVal node As TreeNode,
                                                     ByVal normalizedTarget As String,
                                                     ByVal selectedComponentName As String,
                                                     ByRef visitedCount As Integer,
                                                     ByVal maxVisitedNodes As Integer) As TreeNode
        If node Is Nothing Then Return Nothing

        visitedCount += 1
        If visitedCount > maxVisitedNodes Then Return Nothing

        Try
            'The same physical part can occur in several subassemblies. Prefer the exact
            'SOLIDWORKS component-instance name so the tree jumps to the selected occurrence,
            'then fall back to physical path for older/suppressed nodes.
            If Not String.IsNullOrWhiteSpace(selectedComponentName) AndAlso TypeOf node.Tag Is Component2 Then
                Dim nodeComponentName As String = ""

                Try
                    nodeComponentName = CType(node.Tag, Component2).Name2
                Catch
                    nodeComponentName = ""
                End Try

                If String.Equals(nodeComponentName, selectedComponentName, StringComparison.OrdinalIgnoreCase) Then
                    Return node
                End If
            End If

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

                Dim found As TreeNode = findTreeNodeByCadPathRecursive(
                    childNode,
                    normalizedTarget,
                    selectedComponentName,
                    visitedCount,
                    maxVisitedNodes
                )
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
        versionLabel.Text = "Version: 2026.08.24.1"

        ToolStripSplitButFolder.DropDown.AutoClose = True

        ensureSyncStatusButton()
        removeGetLatestAllMenuItem()
        ensureCopyLegacyDataToSvnMenuItem()
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
    Private Sub stopTaskPaneTimers()
        Try
            If liveChangeCheckTimer IsNot Nothing Then
                liveChangeCheckTimer.Stop()
                liveChangeCheckTimer.Enabled = False
            End If
        Catch
        End Try

        Try
            If graphicalSelectionSyncTimer IsNot Nothing Then
                graphicalSelectionSyncTimer.Stop()
                graphicalSelectionSyncTimer.Enabled = False
            End If
        Catch
        End Try

        Try
            If cacheAgeTimer IsNot Nothing Then
                cacheAgeTimer.Stop()
                cacheAgeTimer.Enabled = False
            End If
        Catch
        End Try

        Try
            If deferredSolidWorksUiTimer IsNot Nothing Then
                deferredSolidWorksUiTimer.Stop()
                deferredSolidWorksUiTimer.Enabled = False
            End If
        Catch
        End Try
    End Sub

    Private Sub disposeTaskPaneTimers()
        stopTaskPaneTimers()

        Try
            If liveChangeCheckTimer IsNot Nothing Then
                liveChangeCheckTimer.Dispose()
                liveChangeCheckTimer = Nothing
            End If
        Catch
            liveChangeCheckTimer = Nothing
        End Try

        Try
            If graphicalSelectionSyncTimer IsNot Nothing Then
                graphicalSelectionSyncTimer.Dispose()
                graphicalSelectionSyncTimer = Nothing
            End If
        Catch
            graphicalSelectionSyncTimer = Nothing
        End Try

        Try
            If cacheAgeTimer IsNot Nothing Then
                cacheAgeTimer.Dispose()
                cacheAgeTimer = Nothing
            End If
        Catch
            cacheAgeTimer = Nothing
        End Try

        Try
            If deferredSolidWorksUiTimer IsNot Nothing Then
                deferredSolidWorksUiTimer.Dispose()
                deferredSolidWorksUiTimer = Nothing
            End If
        Catch
            deferredSolidWorksUiTimer = Nothing
        End Try

        pendingWriteAccessPaths.Clear()
        pendingFeatureTreeRefreshPaths.Clear()
    End Sub

    Friend Sub beforeClose()
        'Mark shutdown first so an already queued timer message becomes a no-op.
        taskPaneClosing = True
        disposeTaskPaneTimers()

        Try
            saveLocalRepoPathSettings()
        Catch
        End Try

        'The SwAddin owns the root SOLIDWORKS COM reference. Drop this secondary reference only
        'after every task-pane timer has stopped so no callback can touch a disconnected RCW.
        iSwApp = Nothing
    End Sub

    ' ### Get Locks
    Private Sub ToolStripDropDownGetLocks_ButtonClick(sender As Object, e As EventArgs) Handles ToolStripDropDownButGetLocks.ButtonClick
        'Resolve a deliberate SVN-tree selection before consulting ActiveDoc or the
        'SOLIDWORKS graphics selection. A physical tree part must never silently become
        'the active parent assembly merely because a TreeNode object was rebuilt.
        Dim hasExplicitTreeSelection As Boolean = isCurrentTreeSelectionExplicitForFileAction()
        Dim selectedTreePaths() As String = getBatchSelectedTreeCadPathsForAction(includeSingleSelectedNode:=True)

        If selectedTreePaths IsNot Nothing AndAlso selectedTreePaths.Length > 0 Then
            Dim selectedText As String = ""
            Dim resolvedPath As String = ""
            Dim resolvedFromVirtual As Boolean = False
            Dim failureReason As String = ""

            getCurrentExplicitTreeSelectionDiagnostic(
                selectedText,
                resolvedPath,
                resolvedFromVirtual,
                failureReason
            )

            Try
                svnModule.logOperationPublic(
                    "Get Locks tree selection: text=" & selectedText &
                    "; virtualOwner=" & resolvedFromVirtual.ToString() &
                    "; resolvedTargets=" & String.Join(" | ", selectedTreePaths)
                )
            Catch
            End Try

            getLocksOfPathsAsync(selectedTreePaths)

        ElseIf hasExplicitTreeSelection Then
            Dim selectedText As String = ""
            Dim resolvedPath As String = ""
            Dim resolvedFromVirtual As Boolean = False
            Dim failureReason As String = ""

            getCurrentExplicitTreeSelectionDiagnostic(
                selectedText,
                resolvedPath,
                resolvedFromVirtual,
                failureReason
            )

            If String.IsNullOrWhiteSpace(failureReason) Then
                If Not String.IsNullOrWhiteSpace(resolvedPath) Then
                    failureReason = "The selected CAD file is not managed by the current SVN working copy."
                Else
                    failureReason = "The selected tree item could not be resolved to an SVN CAD file."
                End If
            End If

            Try
                svnModule.logOperationPublic(
                    "Get Locks blocked for explicit tree selection: text=" & selectedText &
                    "; resolvedPath=" & resolvedPath &
                    "; reason=" & failureReason
                )
            Catch
            End Try

            iSwApp.SendMsgToUser2(
                "PlumVault could not safely resolve the selected tree item for Get Locks." &
                vbCrLf & vbCrLf &
                If(String.IsNullOrWhiteSpace(selectedText), "Selected item", selectedText) &
                vbCrLf &
                failureReason &
                vbCrLf & vbCrLf &
                "The active assembly was not used as a fallback. Refresh the tree and select the file again.",
                swMessageBoxIcon_e.swMbWarning,
                swMessageBoxBtn_e.swMbOk
            )
            Exit Sub

        Else
            Dim activeDocument As ModelDoc2 = Nothing

            Try
                activeDocument = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            Catch
                activeDocument = Nothing
            End Try

            If activeDocument Is Nothing Then
                iSwApp.SendMsgToUser("Error: Active Document not found")
                Exit Sub
            End If

            Dim selectedDocuments() As ModelDoc2 = GetSelectedModDocList(iSwApp)

            If selectedDocuments Is Nothing OrElse selectedDocuments.Length = 0 Then
                iSwApp.SendMsgToUser("Error: No CAD document could be resolved for Get Locks")
                Exit Sub
            End If

            Try
                Dim fallbackPaths As New List(Of String)()

                For Each selectedDocument As ModelDoc2 In selectedDocuments
                    If selectedDocument Is Nothing Then Continue For

                    Dim selectedPath As String = getSafeModelPath(selectedDocument)
                    If Not String.IsNullOrWhiteSpace(selectedPath) Then fallbackPaths.Add(selectedPath)
                Next

                svnModule.logOperationPublic(
                    "Get Locks SOLIDWORKS-selection fallback: " & String.Join(" | ", fallbackPaths.ToArray())
                )
            Catch
            End Try

            getLocksOfDocsAsync(selectedDocuments)
        End If

        updateStatusStrip()
    End Sub

    Private Sub dropDownGetLocksWithDependents_Click(sender As Object, e As EventArgs) Handles dropDownGetLocksWithDependents.Click
        'Kept for designer/binary compatibility. Broad dependency actions are retired.
        ToolStripDropDownGetLocks_ButtonClick(sender, e)
    End Sub

    ' ### Commit
    Private Sub ToolStripDropDownButCommit_ButtonClick(sender As Object, e As EventArgs) Handles ToolStripDropDownButCommit.ButtonClick
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        If modDoc Is Nothing Then iSwApp.SendMsgToUser("Error: Active Document not found") : Exit Sub

        'Fast path-first Commit behavior:
        'Ctrl-click and Shift-click build the blue batch selection in the SVN tree.
        'When Commit is clicked afterward, always use that stored batch selection.
        'The user does NOT need to keep Ctrl or Shift held while clicking Commit.
        'If there is no batch selection, this helper falls back to the exact single tree node.
        Dim selectedTreePaths() As String = getBatchSelectedTreeCadPathsForAction(includeSingleSelectedNode:=True)

        If selectedTreePaths IsNot Nothing AndAlso selectedTreePaths.Length > 0 Then
            Try
                svnModule.logOperationPublic("Commit visible tree selection: " & String.Join(" | ", selectedTreePaths))
            Catch
            End Try
        End If

        'A brand-new document has no physical path yet, so the normal path-first Commit
        'workflow cannot act on it. When there is no explicit tree selection, Commit now
        'starts the same controlled naming, Save As, folder preparation, and automatic
        'first-commit workflow used by Save/Ctrl+S.
        If selectedTreePaths Is Nothing OrElse selectedTreePaths.Length = 0 Then
            Dim activePath As String = ""

            Try
                activePath = modDoc.GetPathName()
            Catch
                activePath = ""
            End Try

            If String.IsNullOrWhiteSpace(activePath) Then
                svnModule.startNewDocumentFirstSaveFromCommitPublic()
                updateStatusStrip()
                Exit Sub
            End If
        End If

        If selectedTreePaths IsNot Nothing AndAlso selectedTreePaths.Length > 0 Then
            tortCommitPathsAsync(selectedTreePaths)
        Else
            tortCommitDocsAsync(GetSelectedModDocList(iSwApp))
        End If

        updateStatusStrip()
    End Sub

    Private Sub dropDownCommitWithDependents_Click(sender As Object, e As EventArgs) Handles dropDownCommitWithDependents.Click
        ToolStripDropDownButCommit_ButtonClick(sender, e)
    End Sub
    Private Sub dropDownCommitAll_Click(sender As Object, e As EventArgs) Handles dropDownCommitAll.Click
        ToolStripDropDownButCommit_ButtonClick(sender, e)
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
        ToolStripDropDownButUnlock_ButtonClick(sender, e)
    End Sub
    Private Sub dropDownUnlockAll_Click(sender As Object, e As EventArgs) Handles dropDownUnlockAll.Click
        iSwApp.SendMsgToUser2(
            "Release Locks All has been disabled for safety." & vbCrLf & vbCrLf &
            "Select the exact file(s) in the SVN tree. The backend will only unlock/revert files you actually have locked.",
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

    Private Sub ToolStripButSaveAs_Click(sender As Object, e As EventArgs) Handles ToolStripButSaveAs.Click
        svnModule.performSaveAsButtonActionPublic()
    End Sub

    Private Sub ToolStripButReId_Click(sender As Object, e As EventArgs) Handles ToolStripButReId.Click
        svnModule.performCadRelocationPublic(CadRelocationMode.ReId)
    End Sub

    Private Sub ToolStripButMove_Click(sender As Object, e As EventArgs) Handles ToolStripButMove.Click
        svnModule.performCadRelocationPublic(CadRelocationMode.Move)
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
                'Original lazy-sync boundary: load the root's immediate children only,
                'then Sync those Level-1 files. Do not expand or Sync Level 2.
                loadImmediateChildrenForNode(rootNode)
                syncPaths = collectImmediateChildCadPathsForSync(rootNode)
            End If

            If debugWatch IsNot Nothing Then
                debugNotes.Add("Default Level-1 load/collect: " & (debugWatch.ElapsedMilliseconds - phaseStartMs).ToString() & " ms")
            End If
        Else
            'Original lazy-sync boundary:
            'Selected Level 0 -> selected file + Level 1 only.
            'Selected Level 1 -> selected file + Level 2 only.
            'No deeper descendants are loaded or server-checked by normal Sync.
            If debugWatch IsNot Nothing Then phaseStartMs = debugWatch.ElapsedMilliseconds

            loadImmediateChildrenForNode(selectedNode)
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

        Dim serverStatusApplied As Boolean = False
        Try
            svnModule.applyServerStatusFromBackgroundPublic(serverStatus)
            serverStatusApplied = True
        Catch
        End Try

        If serverStatusApplied Then
            'A successful manual Sync is newer and broader than the quiet active-document
            'check, so begin the three-minute interval again from this completion time.
            lastQuietActiveStatusCheckUtc = DateTime.UtcNow
        End If

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
            'Selected branch Sync is intentionally bounded to exactly one tree level:
            'the selected file plus its immediate children. It never includes grandchildren.
            addTreeNodePathToSyncList(selectedNode, seen, output)

            For Each childNode As TreeNode In selectedNode.Nodes
                If isLazyPlaceholderNode(childNode) Then Continue For
                addTreeNodePathToSyncList(childNode, seen, output)
            Next
        End If

        'Never fall back to collectCurrentTreeCadPaths() here.
        'That fallback turns a failed/empty selected-branch Sync into a whole loaded-tree Sync
        'and breaks the controlled lazy boundary.
        If output.Count = 0 Then Return Nothing

        Return output.ToArray()
    End Function
    Private Function collectImmediateChildCadPathsForSync(ByVal parentNode As TreeNode) As String()
        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If parentNode IsNot Nothing Then
            'Default/no-selection Sync includes the root's immediate children only.
            'It does not include the root itself and never includes grandchildren.
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

        Dim stableNodePath As String = getStableTreeNodeCadPath(node)
        If Not String.IsNullOrWhiteSpace(stableNodePath) Then Return stableNodePath

        Try
            If TypeOf node.Tag Is ModelDoc2 Then
                Dim model As ModelDoc2 = CType(node.Tag, ModelDoc2)

                'When a virtual component is opened in its own SOLIDWORKS window, the root
                'tree node is tagged with the virtual ModelDoc2 rather than Component2. Its
                'temporary/internal path is not an SVN file; resolve every action and status
                'display to the nearest physical owning assembly instead.
                Dim ownerDocument As ModelDoc2 = getOwningPhysicalAssemblyDocumentForVirtualModel(model)
                If ownerDocument IsNot Nothing Then Return getSafeModelPath(ownerDocument)

                Return model.GetPathName()
            End If

            If TypeOf node.Tag Is Component2 Then
                Dim component As Component2 = CType(node.Tag, Component2)

                'Virtual components do not have an independent SVN file. Their editable
                'content is stored in the nearest physical owning assembly, so every
                'tree action resolves to that assembly path.
                If isComponentVirtualSafe(component) Then
                    Return getPhysicalOwnerAssemblyPathForTreeNode(node)
                End If

                Return getSafeComponentPath(component)
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

        If ext <> ".SLDPRT" AndAlso ext <> ".SLDASM" AndAlso ext <> ".SLDDRW" Then Return False

        'External CAD remains visible in the tree, but it has no SVN server status to
        'synchronize. Skip it silently so one unmanaged reference never blocks Sync.
        Try
            Return svnModule.shouldIncludeCadPathInSyncPublic(filePath)
        Catch
            Return False
        End Try
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

            'Rebuild only the active shallow tree first. The replacement is constructed away
            'from the visible/cached tree and swapped in only after it succeeds, so a slow or
            'failed SOLIDWORKS component query cannot leave the task pane white.
            Try
                refreshCurrentTreeViewOnly()
            Catch
            End Try

            'Local-only, active-tree status refresh. The old implementation called
            'updateStatusLocally with no paths, which silently ran svn status and propget over
            'the entire working copy on the SOLIDWORKS UI thread. A large set of virtual/new
            'parts or imported references could therefore make SOLIDWORKS appear frozen.
            Dim activeTreePaths() As String = collectCurrentTreeCadPaths()

            Try
                If activeTreePaths IsNot Nothing AndAlso activeTreePaths.Length > 0 Then
                    updateLockStatusPublic(
                        bRefreshAllTreeViews:=False,
                        filePathsToRefresh:=activeTreePaths
                    )
                End If
            Catch
            End Try

            Try
                recolorCurrentTreeFromStatus()
            Catch
            End Try

            'Writable/read-only state is interaction-scoped. The two former calls below both
            'ran the legacy bulk SetReadOnlyState loop across every cached open document; they
            'were duplicates and could produce rebuild/false-dirty cascades during Refresh.
            Try
                svnModule.reconcileWriteAccessForActiveDocumentPublic()
            Catch
            End Try

            Try
                svnModule.reconcileReadOnlyForUnlockedActiveDocumentPublic()
            Catch
            End Try

            setRefreshTreeButtonNormal()

        Catch ex As Exception
            'Refresh is a routine button and must never let an exception escape into the
            'SOLIDWORKS message loop - that terminates the whole application. A brand-new,
            'never-saved assembly (GetPathName = "") with imported/virtual children is the
            'known stress case. Log the real error for diagnosis and tell the user plainly.
            Try
                svnModule.logOperationPublic("Refresh failed: " & ex.ToString())
            Catch
            End Try

            Try
                iSwApp.SendMsgToUser2(
                    "Refresh could not complete." & vbCrLf & vbCrLf &
                    "If the active document is new and unsaved, save and commit it first, then Refresh again.",
                    swMessageBoxIcon_e.swMbWarning,
                    swMessageBoxBtn_e.swMbOk
                )
            Catch
            End Try

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

            'A new, never-saved document has no path. FileInfo("") throws an unhandled
            'ArgumentException here, which crashes SOLIDWORKS from a plain button click.
            If String.IsNullOrWhiteSpace(sSuggestedPath) Then
                pickFolder()
                Exit Sub
            End If

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

        Dim clickedNode As TreeNode = Nothing

        Try
            If e IsNot Nothing AndAlso e.Node IsNot Nothing Then
                clickedNode = e.Node
                lastExplicitSvnTreeClickUtc = DateTime.UtcNow
                clearGraphicalTreeHighlight()
                TreeView1.SelectedNode = clickedNode
                rememberExplicitTreeActionSelection(clickedNode)

                If (ModifierKeys And Keys.Control) = Keys.Control Then
                    toggleBatchTreeNode(clickedNode)
                ElseIf (ModifierKeys And Keys.Shift) = Keys.Shift Then
                    selectBatchTreeRange(clickedNode)
                Else
                    clearBatchTreeSelection()
                    lastBatchAnchorTreeNode = clickedNode
                End If
            End If
        Catch
            Exit Sub
        End Try

        If clickedNode Is Nothing Then Exit Sub

        'Never call Select on the Component2 stored in TreeNode.Tag. TreeView cloning preserves
        'that RCW even after the source assembly has been closed, and using it after reopen can
        'crash inside native SOLIDWORKS before VB can catch an exception.
        'Graphical highlighting is cosmetic, so queue only a stable path and reacquire a live
        'component from the currently open assembly on the next UI turn.
        queueSafeGraphicalSelectionForTreeNode(clickedNode)
    End Sub

    Private Sub queueSafeGraphicalSelectionForTreeNode(ByVal node As TreeNode)
        If node Is Nothing Then Exit Sub
        If taskPaneClosing Then Exit Sub

        Dim selectedPath As String = getStableTreeNodeCadPath(node)
        If String.IsNullOrWhiteSpace(selectedPath) Then Exit Sub

        'A virtual row resolves to its physical owner assembly for SVN operations. It does not
        'have a separate physical component path that can be safely selected by this helper.
        If treeNodeRepresentsVirtualComponent(node) Then Exit Sub

        Try
            Me.BeginInvoke(New MethodInvoker(
                Sub()
                    safelySelectCurrentAssemblyComponentByPath(selectedPath)
                End Sub))
        Catch
        End Try
    End Sub

    Private Sub safelySelectCurrentAssemblyComponentByPath(ByVal selectedPath As String)
        If taskPaneClosing Then Exit Sub
        If iSwApp Is Nothing Then Exit Sub
        If String.IsNullOrWhiteSpace(selectedPath) Then Exit Sub

        Dim activeModel As ModelDoc2 = Nothing

        Try
            activeModel = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeModel Is Nothing Then Exit Sub
            If activeModel.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Exit Sub
        Catch
            Exit Sub
        End Try

        Dim activePath As String = getSafeModelPath(activeModel)
        Dim normalizedSelectedPath As String = normalizeTreeActionPath(selectedPath)
        Dim normalizedActivePath As String = normalizeTreeActionPath(activePath)

        'Clicking the top assembly row is a task-pane selection only. There is no component
        'selection to perform, and avoiding a native call here removes the reopen crash path.
        If Not String.IsNullOrWhiteSpace(normalizedActivePath) AndAlso
           String.Equals(normalizedSelectedPath, normalizedActivePath, StringComparison.OrdinalIgnoreCase) Then
            Exit Sub
        End If

        Dim currentComponent As Component2 = findCurrentAssemblyComponentByPath(activeModel, normalizedSelectedPath)
        If currentComponent Is Nothing Then Exit Sub

        Try
            'Use the current component's modern selection API so one selection is shared by
            'the graphics area and SOLIDWORKS FeatureManager tree. Clear the previous native
            'selection first; otherwise a stale face/feature selection can leave the component
            'highlighted graphically without becoming the active FeatureManager row.
            activeModel.ClearSelection2(True)
            Dim selectedInSolidWorks As Boolean = currentComponent.Select4(False, Nothing, False)

            If Not selectedInSolidWorks Then
                'Select4 is preferred, but retain the older API as a compatibility fallback
                'for lightweight components in older SOLIDWORKS releases.
                currentComponent.Select(False)
            End If

            Try
                Dim featureManager As FeatureManager = activeModel.FeatureManager
                If featureManager IsNot Nothing Then featureManager.UpdateFeatureTree()
            Catch
            End Try

            activeModel.GraphicsRedraw2()
        Catch
            'Graphical cross-highlighting is optional. Never escalate a failed native
            'selection into a file-operation or stability problem.
        End Try
    End Sub

    Private Function findCurrentAssemblyComponentByPath(ByVal activeModel As ModelDoc2,
                                                        ByVal selectedPath As String) As Component2
        If activeModel Is Nothing Then Return Nothing
        If String.IsNullOrWhiteSpace(selectedPath) Then Return Nothing

        Try
            If activeModel.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Return Nothing
        Catch
            Return Nothing
        End Try

        Dim assemblyDocument As AssemblyDoc = TryCast(activeModel, AssemblyDoc)
        If assemblyDocument Is Nothing Then Return Nothing

        Dim componentsObject As Object = Nothing

        Try
            componentsObject = assemblyDocument.GetComponents(False)
        Catch
            componentsObject = Nothing
        End Try

        Dim componentsArray As Array = TryCast(componentsObject, Array)
        If componentsArray Is Nothing Then Return Nothing

        Dim normalizedSelectedPath As String = normalizeTreeActionPath(selectedPath)

        For Each componentObject As Object In componentsArray
            Dim currentComponent As Component2 = TryCast(componentObject, Component2)
            If currentComponent Is Nothing Then Continue For
            If isComponentVirtualSafe(currentComponent) Then Continue For

            Dim currentPath As String = normalizeTreeActionPath(getSafeComponentPath(currentComponent))
            If String.IsNullOrWhiteSpace(currentPath) Then Continue For

            If String.Equals(currentPath, normalizedSelectedPath, StringComparison.OrdinalIgnoreCase) Then
                Return currentComponent
            End If
        Next

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

        'A stored TreeView may contain Component2/ModelDoc2 RCWs from a document instance that
        'was closed and later reopened. Rebuild the shallow current tree from the live ActiveDoc
        'before displaying it. refreshCurrentTreeViewOnly calls back here with False, so this
        'does not recurse and does not contact the SVN server.
        If bRetryWithRefresh Then
            Try
                refreshCurrentTreeViewOnly()
            Catch
                'Keep the last known-good tree visible if SOLIDWORKS cannot rebuild it.
            End Try
            Exit Sub
        End If

        Dim treeNodeTemp As TreeNode
        Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc()
        If modDoc Is Nothing Then Exit Sub

        Dim treeNodeIndex As Integer = findStoredTreeView(modDoc.GetPathName, bRetryWithRefresh)
        If allTreeViews Is Nothing OrElse
           treeNodeIndex < 0 OrElse
           treeNodeIndex >= allTreeViews.Length Then Exit Sub
        If Not onlineCheckBox.Checked Then Exit Sub

        Try
            treeNodeTemp = allTreeViews(treeNodeIndex).Nodes(0)
        Catch
            'Keep the last known-good tree visible if the replacement is unavailable.
            Exit Sub
        End Try

        Dim clonedNode As TreeNode = CType(treeNodeTemp.Clone(), TreeNode)

        TreeView1.BeginUpdate()
        Try
            clearBatchTreeSelection(False)
            TreeView1.Nodes.Clear()
            TreeView1.Nodes.Insert(0, clonedNode)
            TreeView1.Nodes(0).Expand()
            'TreeView1.ExpandAll()
            TreeView1.Show()
            ensureTreeStartDragHandle()
        Finally
            TreeView1.EndUpdate()
        End Try

    End Sub
    Function findStoredTreeView(pathName As String, Optional bRetryWithRefresh As Boolean = True) As Integer
        Dim normalizedPath As String = normalizeTreeActionPath(pathName)
        If String.IsNullOrWhiteSpace(normalizedPath) Then Return -1

        Dim storedIndex As Integer = findStoredTreeViewByExactPath(normalizedPath)
        If storedIndex >= 0 Then Return storedIndex

        If Not bRetryWithRefresh Then Return -1

        'Speed fix:
        'If the tree is missing, build only the active tree.
        'Do NOT run updateStatusOfAllModelsVariable(True), because that hits the server and rebuilds every tree.
        Try
            refreshCurrentTreeViewOnly()
        Catch
        End Try

        Return findStoredTreeViewByExactPath(normalizedPath)
    End Function

    Private Function findStoredTreeViewByExactPath(ByVal normalizedPath As String) As Integer
        If String.IsNullOrWhiteSpace(normalizedPath) Then Return -1
        If allTreeViews Is Nothing OrElse allTreeViews.Length = 0 Then Return -1

        For i As Integer = 0 To UBound(allTreeViews)
            If allTreeViews(i) Is Nothing Then Continue For
            If allTreeViews(i).Nodes.Count = 0 Then Continue For

            Dim rootPath As String = getStableTreeNodeCadPath(allTreeViews(i).Nodes(0))
            If String.Equals(rootPath, normalizedPath, StringComparison.OrdinalIgnoreCase) Then
                Return i
            End If
        Next

        Return -1
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
        'Clear the stale TreeNode object so a plain Sync click remains Level-1-only.
        'Do not clear lastUserClickedTreePathForActions: file actions compare the newly
        'rebuilt visible node by stable physical path, preventing a selected part from
        'falling back to the active parent assembly.
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

        Dim treeIndex As Integer = findStoredTreeView(activePath, bRetryWithRefresh:=False)

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

    Private Function isComponentVirtualSafe(ByVal comp As Component2) As Boolean
        If comp Is Nothing Then Return False

        Try
            Return comp.IsVirtual
        Catch
            Return False
        End Try
    End Function

    Private Function getPhysicalAssemblyPathFromComponent(ByVal comp As Component2) As String
        If comp Is Nothing Then Return ""
        If isComponentVirtualSafe(comp) Then Return ""

        Dim componentPath As String = getSafeComponentPath(comp)
        If String.IsNullOrWhiteSpace(componentPath) Then Return ""

        Try
            If Not String.Equals(Path.GetExtension(componentPath), ".SLDASM", StringComparison.OrdinalIgnoreCase) Then Return ""
            If Not File.Exists(componentPath) Then Return ""
            Return Path.GetFullPath(componentPath)
        Catch
            Return ""
        End Try
    End Function

    Private Function getPhysicalOwnerAssemblyPathForComponent(ByVal comp As Component2,
                                                               Optional ByVal fallbackDocument As ModelDoc2 = Nothing) As String
        If comp Is Nothing Then Return ""

        Dim currentComponent As Component2 = Nothing

        Try
            currentComponent = comp.GetParent()
        Catch
            currentComponent = Nothing
        End Try

        Dim guard As Integer = 0

        While currentComponent IsNot Nothing AndAlso guard < 100
            guard += 1

            Dim assemblyPath As String = getPhysicalAssemblyPathFromComponent(currentComponent)
            If Not String.IsNullOrWhiteSpace(assemblyPath) Then Return assemblyPath

            Try
                currentComponent = currentComponent.GetParent()
            Catch
                currentComponent = Nothing
            End Try
        End While

        'A top-level virtual component has no Component2 parent. In that case its
        'physical owner is the active top-level assembly.
        Dim candidates As New List(Of ModelDoc2)()
        If fallbackDocument IsNot Nothing Then candidates.Add(fallbackDocument)

        Try
            Dim activeDocument As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDocument IsNot Nothing AndAlso Not candidates.Contains(activeDocument) Then
                candidates.Add(activeDocument)
            End If
        Catch
        End Try

        For Each candidate As ModelDoc2 In candidates
            If candidate Is Nothing Then Continue For

            Try
                If candidate.GetType() <> swDocumentTypes_e.swDocASSEMBLY Then Continue For

                Dim candidatePath As String = candidate.GetPathName()
                If String.IsNullOrWhiteSpace(candidatePath) Then Continue For
                If Not File.Exists(candidatePath) Then Continue For

                Return Path.GetFullPath(candidatePath)
            Catch
            End Try
        Next

        Return ""
    End Function

    Private Function getPhysicalOwnerAssemblyPathForTreeNode(ByVal node As TreeNode) As String
        If node Is Nothing Then Return ""

        'The tree hierarchy is the most reliable owner map, including nested virtual
        'subassemblies. Walk upward until a real file-backed assembly is found.
        Dim parentNode As TreeNode = node.Parent
        Dim guard As Integer = 0

        While parentNode IsNot Nothing AndAlso guard < 100
            guard += 1

            Try
                If TypeOf parentNode.Tag Is ModelDoc2 Then
                    Dim parentDocument As ModelDoc2 = CType(parentNode.Tag, ModelDoc2)

                    If parentDocument.GetType() = swDocumentTypes_e.swDocASSEMBLY Then
                        Dim parentPath As String = parentDocument.GetPathName()

                        If Not String.IsNullOrWhiteSpace(parentPath) AndAlso File.Exists(parentPath) Then
                            Return Path.GetFullPath(parentPath)
                        End If
                    End If
                ElseIf TypeOf parentNode.Tag Is Component2 Then
                    Dim parentComponent As Component2 = CType(parentNode.Tag, Component2)
                    Dim parentPath As String = getPhysicalAssemblyPathFromComponent(parentComponent)

                    If Not String.IsNullOrWhiteSpace(parentPath) Then Return parentPath
                End If
            Catch
            End Try

            parentNode = parentNode.Parent
        End While

        Try
            If TypeOf node.Tag Is Component2 Then
                Return getPhysicalOwnerAssemblyPathForComponent(CType(node.Tag, Component2), TryCast(iSwApp.ActiveDoc, ModelDoc2))
            End If
        Catch
        End Try

        Return ""
    End Function

    Private Function getOpenDocumentByPathSafe(ByVal filePath As String) As ModelDoc2
        If String.IsNullOrWhiteSpace(filePath) Then Return Nothing

        Try
            Dim openDocument As ModelDoc2 = TryCast(iSwApp.GetOpenDocumentByName(filePath), ModelDoc2)
            If openDocument IsNot Nothing Then Return openDocument
        Catch
        End Try

        Try
            Dim activeDocument As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDocument Is Nothing Then Return Nothing

            Dim activePath As String = activeDocument.GetPathName()
            If String.IsNullOrWhiteSpace(activePath) Then Return Nothing

            If String.Equals(Path.GetFullPath(activePath), Path.GetFullPath(filePath), StringComparison.OrdinalIgnoreCase) Then
                Return activeDocument
            End If
        Catch
        End Try

        Return Nothing
    End Function

    Private Function getOwningPhysicalAssemblyDocumentForVirtualModel(ByVal possibleVirtualDocument As ModelDoc2) As ModelDoc2
        If possibleVirtualDocument Is Nothing OrElse iSwApp Is Nothing Then Return Nothing

        Dim possiblePath As String = getSafeModelPath(possibleVirtualDocument)
        Dim possibleTitle As String = ""

        Try
            possibleTitle = possibleVirtualDocument.GetTitle()
        Catch
            possibleTitle = ""
        End Try

        If Not String.IsNullOrWhiteSpace(possiblePath) AndAlso
           File.Exists(possiblePath) AndAlso
           possiblePath.IndexOf("\AppData\Local\Temp\", StringComparison.OrdinalIgnoreCase) < 0 AndAlso
           Not Path.GetFileName(possiblePath).Contains("^") Then
            Return Nothing
        End If

        Dim documentsObject As Object = Nothing

        Try
            documentsObject = iSwApp.GetDocuments()
        Catch
            documentsObject = Nothing
        End Try

        Dim documentsArray As Array = TryCast(documentsObject, Array)
        If documentsArray Is Nothing Then Return Nothing

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
                    Dim componentPath As String = getSafeModelPath(componentDocument)
                    Dim componentTitle As String = ""

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
                    Dim ownerPath As String = getPhysicalOwnerAssemblyPathForComponent(component, assemblyModel)
                    Return getOpenDocumentByPathSafe(ownerPath)
                End If
            Next
        Next

        Return Nothing
    End Function

    Private Function distinctModelDocsByPhysicalPath(ByVal inputDocs() As ModelDoc2) As ModelDoc2()
        If inputDocs Is Nothing Then Return Nothing

        Dim output As New List(Of ModelDoc2)()
        Dim seenPaths As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each inputDoc As ModelDoc2 In inputDocs
            If inputDoc Is Nothing Then Continue For

            Dim inputPath As String = ""

            Try
                inputPath = inputDoc.GetPathName()
            Catch
                inputPath = ""
            End Try

            If String.IsNullOrWhiteSpace(inputPath) Then
                If Not output.Contains(inputDoc) Then output.Add(inputDoc)
                Continue For
            End If

            Try
                inputPath = Path.GetFullPath(inputPath)
            Catch
            End Try

            If seenPaths.Contains(inputPath) Then Continue For

            seenPaths.Add(inputPath)
            output.Add(inputDoc)
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
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
        Dim isVirtual As Boolean = isComponentVirtualSafe(comp)

        If isVirtual Then
            'Use the SOLIDWORKS component name rather than its temporary/internal path.
            Try
                nodeText = comp.Name2
            Catch
                nodeText = ""
            End Try
        End If

        If String.IsNullOrWhiteSpace(nodeText) AndAlso Not String.IsNullOrWhiteSpace(compPath) Then
            nodeText = System.IO.Path.GetFileName(compPath)
        ElseIf String.IsNullOrWhiteSpace(nodeText) AndAlso modDoc IsNot Nothing Then
            Try
                nodeText = modDoc.GetTitle()
            Catch
                nodeText = "<unknown component>"
            End Try
        ElseIf String.IsNullOrWhiteSpace(nodeText) Then
            nodeText = "<unknown component>"
        End If

        If isVirtual Then nodeText &= " [Virtual]"

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

        Dim nodePath As String = getCadPathFromTreeNode(node)

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

            Dim componentIsVirtual As Boolean = isComponentVirtualSafe(comp)
            Dim compPath As String = getSafeComponentPath(comp)
            If String.IsNullOrWhiteSpace(compPath) AndAlso Not componentIsVirtual Then Continue For

            If Not componentIsVirtual AndAlso treeContainsPath(rootNode, compPath) Then Continue For

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

            If compDoc IsNot Nothing AndAlso Not componentIsVirtual Then
                addModelDocIfMissing(mdComponentList, compDoc, bUniqueOnly)
            End If

            Dim missingNode As New TreeNode(buildComponentNodeText(comp, compDoc))
            missingNode.Tag = comp
            Dim missingStablePath As String = getSafeComponentPath(comp)
            If componentIsVirtual Then
                missingStablePath = getPhysicalOwnerAssemblyPathForComponent(comp, TryCast(iSwApp.ActiveDoc, ModelDoc2))
            End If
            setStableTreeNodeCadPath(missingNode, missingStablePath)
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
        Dim replacementTree As TreeView = Nothing
        Dim modelDocList As New List(Of ModelDoc2)()
        Dim swConfMgr As ConfigurationManager
        Dim swConf As Configuration
        Dim swRootComp As Component2

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
                'Build off-screen and commit the replacement only after traversal succeeds.
                'Never destroy the cached tree before making COM calls into SOLIDWORKS.
                replacementTree = New TreeView
                replacementTree.Visible = False

                parentNode = New TreeNode(sFileNameTemp)
                parentNode.Tag = modDocArr(i)
                setStableTreeNodeCadPath(parentNode, getSafeModelPath(modDocArr(i)))
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

                TraverseComponent(swRootComp, modelDocList, 1, parentNode, bUniqueOnly, bResolveLightweight, iTreeDepthLimit, getSafeModelPath(modDocArr(i)))

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

                'A drawing can show views of any number of differently-named parts/assemblies
                'across its sheets (a same-named "Part.SLDPRT for Part.SLDDRW" convention is
                'common but not guaranteed - e.g. an assembly drawing with several detail
                'parts). Use SOLIDWORKS' own dependency walk instead of assuming a single
                'matching-named model, so multi-reference drawings are handled correctly.
                Dim drawingReferencedPaths As List(Of String) = getDrawingReferencedFilePaths(modDocArr(i))

                For Each referencedPath As String In drawingReferencedPaths
                    Dim referencedDoc As ModelDoc2 = Nothing

                    Try
                        referencedDoc = TryCast(iSwApp.GetOpenDocumentByName(referencedPath), ModelDoc2)
                    Catch
                        referencedDoc = Nothing
                    End Try

                    If referencedDoc IsNot Nothing Then
                        addModelDocIfMissing(modelDocList, referencedDoc, bUniqueOnly)
                        j += 1
                    End If

                    'Keep drawing dependencies visible and actionable even when the referenced
                    'model is not open. Path-first toolbar actions can Get Latest/Get Locks on
                    'these nodes without forcing SOLIDWORKS to load a large model hierarchy.
                    If bUpdateTreeView Then
                        Dim dependencyNode As New TreeNode(System.IO.Path.GetFileName(referencedPath))
                        dependencyNode.Tag = referencedDoc
                        setStableTreeNodeCadPath(dependencyNode, referencedPath)
                        setNodeColorFromStatus(dependencyNode)
                        parentNode.Nodes.Add(dependencyNode)
                    End If
                Next

            Else
                If bUpdateTreeView Then
                    setNodeColorFromStatus(parentNode)
                    replacementTree.Nodes.Add(parentNode)
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
            replacementTree.Sort()
            If parentNode IsNot Nothing AndAlso replacementTree.Nodes.Count = 0 Then
                replacementTree.Nodes.Add(parentNode)
            End If

            allTreeViews(allTreeViewsIndexToUpdate) = replacementTree
        End If

        Return mdComponentArr
    End Function

    Private Function isRecognizedCadExtensionPath(ByVal filePath As String) As Boolean
        If String.IsNullOrWhiteSpace(filePath) Then Return False

        Dim ext As String = ""
        Try
            ext = System.IO.Path.GetExtension(filePath).ToUpperInvariant()
        Catch
            Return False
        End Try

        Return ext = ".SLDPRT" OrElse ext = ".SLDASM" OrElse ext = ".SLDDRW"
    End Function

    'Returns every part/assembly/drawing file the given document depends on, using
    'SOLIDWORKS' own dependency walk (IModelDocExtension.GetDependencies) rather than
    'guessing from filenames. Works for a drawing referencing any number of differently
    'named models, not just one sharing the drawing's own base filename.
    Friend Function getDrawingReferencedFilePaths(ByVal drawingDocument As ModelDoc2) As List(Of String)
        Dim result As New List(Of String)
        If drawingDocument Is Nothing Then Return result

        Try
            Dim modelExtension As ModelDocExtension = drawingDocument.Extension
            If modelExtension Is Nothing Then Return result

            Dim dependenciesObject As Object = modelExtension.GetDependencies(
                True,  'Traverseflag - walk the full dependency graph, not just direct refs
                True,  'Searchflag - resolve to full paths where possible
                False, 'AddReadOnlyInfo - keep the returned array a plain list of paths
                False, 'ListBrokenRefs
                False  'AppendImportedPaths
            )

            Dim dependencyEntries As Object() = TryCast(dependenciesObject, Object())
            If dependencyEntries Is Nothing Then Return result

            Dim ownPath As String = ""
            Try
                ownPath = System.IO.Path.GetFullPath(drawingDocument.GetPathName())
            Catch
                ownPath = ""
            End Try

            Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

            'With AddReadOnlyInfo=False, GetDependencies returns repeating
            '[reference name, resolved path] pairs. Only consume the path entry. Treating
            'every string as a path can turn a display name such as "Part.SLDPRT" into a
            'bogus path rooted at the add-in process's current working directory.
            For entryIndex As Integer = 1 To dependencyEntries.Length - 1 Step 2
                Dim entryPath As String = TryCast(dependencyEntries(entryIndex), String)
                If String.IsNullOrWhiteSpace(entryPath) Then Continue For
                If Not isRecognizedCadExtensionPath(entryPath) Then Continue For
                If Not System.IO.Path.IsPathRooted(entryPath) Then Continue For

                Dim normalizedPath As String = entryPath
                Try
                    normalizedPath = System.IO.Path.GetFullPath(entryPath)
                Catch
                End Try

                If String.Equals(normalizedPath, ownPath, StringComparison.OrdinalIgnoreCase) Then Continue For
                If seen.Add(normalizedPath) Then result.Add(normalizedPath)
            Next
        Catch
        End Try

        Return result
    End Function

    Private Function documentArrayContainsDrawing(ByVal documents() As ModelDoc2) As Boolean
        If documents Is Nothing Then Return False

        For Each document As ModelDoc2 In documents
            If document Is Nothing Then Continue For

            Try
                If document.GetType() = swDocumentTypes_e.swDocDRAWING Then Return True
            Catch
            End Try
        Next

        Return False
    End Function

    'Path-first drawing workflow. Unlike ModelDoc2 arrays, this preserves dependencies that
    'are intentionally not open in SOLIDWORKS, so With Dependents remains complete without
    'loading every part in a large drawing/assembly hierarchy.
    Friend Function getCadFilePathsIncludingDrawingDependencies(ByVal documents() As ModelDoc2) As String()
        If documents Is Nothing Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each document As ModelDoc2 In documents
            If document Is Nothing Then Continue For

            Dim documentPath As String = getSafeModelPath(document)
            If Not String.IsNullOrWhiteSpace(documentPath) Then
                Try
                    documentPath = System.IO.Path.GetFullPath(documentPath)
                Catch
                End Try

                If svnModule.isPathInsideLocalRepoPublic(documentPath) AndAlso seen.Add(documentPath) Then
                    output.Add(documentPath)
                End If
            End If

            Try
                If document.GetType() <> swDocumentTypes_e.swDocDRAWING Then Continue For
            Catch
                Continue For
            End Try

            For Each dependencyPath As String In getDrawingReferencedFilePaths(document)
                If String.IsNullOrWhiteSpace(dependencyPath) Then Continue For

                Try
                    dependencyPath = System.IO.Path.GetFullPath(dependencyPath)
                Catch
                End Try

                If svnModule.isPathInsideLocalRepoPublic(dependencyPath) AndAlso seen.Add(dependencyPath) Then
                    output.Add(dependencyPath)
                End If
            Next
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Sub TraverseComponent(
                         ByRef swComp As Component2,
                         ByRef mdComponentList As List(Of ModelDoc2),
                         ByVal nLevel As Long,
                         Optional ByRef rootNode As TreeNode = Nothing,
                         Optional ByVal bUniqueOnly As Boolean = True,
                         Optional ByVal bResolveLightweight As Boolean = False,
                         Optional ByVal iTreeDepthLimit As Integer = -1,
                         Optional ByVal rootAssemblyPath As String = "")

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

        If modDocParent IsNot Nothing AndAlso Not isComponentVirtualSafe(swComp) Then
            addModelDocIfMissing(mdComponentList, modDocParent, bUniqueOnly)
        End If

        If bUC Then
            parentNode = New TreeNode(buildComponentNodeText(swComp, modDocParent))
            parentNode.Tag = swComp
            Dim parentStablePath As String = getSafeComponentPath(swComp)
            If isComponentVirtualSafe(swComp) Then
                parentStablePath = getPhysicalOwnerAssemblyPathForComponent(swComp, TryCast(iSwApp.ActiveDoc, ModelDoc2))
            End If
            setStableTreeNodeCadPath(parentNode, parentStablePath)
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

            Dim childIsVirtual As Boolean = isComponentVirtualSafe(swChildComp)

            'Free byproduct of the traversal this tree build is already doing: remember that
            'this root assembly embeds virtual/imported components, so background writable
            'transitions (the multi-minute SetReadOnlyState rebuild) never touch it - learned
            'BEFORE the cost is ever paid, unlike the measured-slow store.
            If childIsVirtual AndAlso Not String.IsNullOrWhiteSpace(rootAssemblyPath) Then
                Try
                    svnModule.noteAssemblyContainsVirtualComponentsPublic(rootAssemblyPath)
                Catch
                End Try
            End If
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
                    If Not childIsVirtual Then addModelDocIfMissing(mdComponentList, modDocChild, bUniqueOnly)

                    childNode = New TreeNode(buildComponentNodeText(swChildComp, modDocChild))
                    childNode.Tag = swChildComp
                    Dim childStablePath As String = getSafeComponentPath(swChildComp)
                    If childIsVirtual Then
                        childStablePath = getPhysicalOwnerAssemblyPathForComponent(swChildComp, TryCast(iSwApp.ActiveDoc, ModelDoc2))
                    End If
                    setStableTreeNodeCadPath(childNode, childStablePath)
                    setNodeColorFromStatus(childNode)
                    addLazyPlaceholderIfNeeded(childNode)
                    parentNode.Nodes.Add(childNode)

                    Continue For
                End If

                If Not childIsVirtual AndAlso bUniqueOnly AndAlso modelDocListContainsPath(mdComponentList, getSafeModelPath(modDocChild)) Then
                    If bUC Then
                        childNode = New TreeNode(buildComponentNodeText(swChildComp, modDocChild))
                        childNode.Tag = swChildComp
                        Dim duplicateStablePath As String = getSafeComponentPath(swChildComp)
                        If childIsVirtual Then
                            duplicateStablePath = getPhysicalOwnerAssemblyPathForComponent(swChildComp, TryCast(iSwApp.ActiveDoc, ModelDoc2))
                        End If
                        setStableTreeNodeCadPath(childNode, duplicateStablePath)
                        setNodeColorFromStatus(childNode)
                        addLazyPlaceholderIfNeeded(childNode)
                        parentNode.Nodes.Add(childNode)
                    End If

                    Continue For
                End If

                TraverseComponent(swChildComp, mdComponentList, nLevel + 1, parentNode, bUniqueOnly, bResolveLightweight, iTreeDepthLimit, rootAssemblyPath)

            Else

                If modDocChild IsNot Nothing AndAlso Not childIsVirtual Then
                    addModelDocIfMissing(mdComponentList, modDocChild, bUniqueOnly)
                End If

                If bUC Then
                    childNode = New TreeNode(buildComponentNodeText(swChildComp, modDocChild))
                    childNode.Tag = swChildComp
                    Dim leafStablePath As String = getSafeComponentPath(swChildComp)
                    If childIsVirtual Then
                        leafStablePath = getPhysicalOwnerAssemblyPathForComponent(swChildComp, TryCast(iSwApp.ActiveDoc, ModelDoc2))
                    End If
                    setStableTreeNodeCadPath(childNode, leafStablePath)
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

        Dim stablePath As String = getStableTreeNodeCadPath(node)
        If Not String.IsNullOrWhiteSpace(stablePath) Then
            Try
                If String.Equals(Path.GetExtension(stablePath), ".SLDASM", StringComparison.OrdinalIgnoreCase) Then Return True
            Catch
            End Try
        End If

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
            Dim stableNodePath As String = getStableTreeNodeCadPath(node)
            Dim activeModel As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            Dim activeModelPath As String = getSafeModelPath(activeModel)

            If Not String.IsNullOrWhiteSpace(stableNodePath) AndAlso
               activeModel IsNot Nothing AndAlso
               String.Equals(normalizeTreeActionPath(stableNodePath), normalizeTreeActionPath(activeModelPath), StringComparison.OrdinalIgnoreCase) AndAlso
               activeModel.GetType() = swDocumentTypes_e.swDocASSEMBLY Then

                Dim modelDoc As ModelDoc2 = activeModel
                Dim confMgr As ConfigurationManager = modelDoc.ConfigurationManager
                Dim conf As Configuration = confMgr.ActiveConfiguration
                Dim rootComp As Component2 = conf.GetRootComponent3(True)
                If rootComp Is Nothing Then Exit Sub

                childObj = rootComp.GetChildren()

            ElseIf Not String.IsNullOrWhiteSpace(stableNodePath) AndAlso
                   activeModel IsNot Nothing AndAlso
                   activeModel.GetType() = swDocumentTypes_e.swDocASSEMBLY Then

                'Reacquire the component from the currently open assembly by stable file path.
                'Never expand children through a Component2 copied from a stored TreeView.
                Dim comp As Component2 = findCurrentAssemblyComponentByPath(activeModel, stableNodePath)
                If comp Is Nothing Then Exit Sub
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

        'Suspend painting for the whole mutation. Clearing, re-adding node-by-node, and
        'sorting an owner-drawn TreeView without BeginUpdate repaints on every step - the
        'visible "tree flashing" during Sync/expand on component-heavy assemblies.
        Dim owningTree As TreeView = node.TreeView

        Try
            If owningTree IsNot Nothing Then owningTree.BeginUpdate()
        Catch
            owningTree = Nothing
        End Try

        Try

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
            Dim lazyChildStablePath As String = getSafeComponentPath(childComp)
            If isComponentVirtualSafe(childComp) Then
                lazyChildStablePath = getPhysicalOwnerAssemblyPathForComponent(childComp, TryCast(iSwApp.ActiveDoc, ModelDoc2))

                'Virtual components discovered through lazy expansion (deeper than the initial
                'depth-1 tree build) must also disable background writable transitions - and
                'here the exact OWNING assembly is already computed, which is the file whose
                'native transition would rebuild the embedded imported data.
                Try
                    svnModule.noteAssemblyContainsVirtualComponentsPublic(lazyChildStablePath)
                Catch
                End Try
            End If
            setStableTreeNodeCadPath(childNode, lazyChildStablePath)
            setNodeColorFromStatus(childNode)
            addLazyPlaceholderIfNeeded(childNode)
            node.Nodes.Add(childNode)
        Next

        Try
            node.TreeView.Sort()
        Catch
        End Try

        Finally
            Try
                If owningTree IsNot Nothing Then owningTree.EndUpdate()
            Catch
            End Try
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
            If modDoc IsNot Nothing Then unlockDocs({modDoc})
        End Sub
        Sub commitEventHandler(sender As Object, e As EventArgs)
            tortCommitDocsAsync({modDoc})
        End Sub
        Public Sub commitWithDependentsEventHandler(sender As Object, e As EventArgs)
            If modDoc IsNot Nothing Then tortCommitDocsAsync({modDoc})
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
            If modDoc IsNot Nothing Then getLocksOfDocsAsync({modDoc})
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

            If isComponentVirtualSafe(comp) Then
                'Context-menu actions on a virtual node operate on its physical owner.
                Return getOpenDocumentByPathSafe(getCadPathFromTreeNode(rootNode))
            End If

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
            Dim model As ModelDoc2 = CType(rootNode.Tag, ModelDoc2)
            Dim ownerDocument As ModelDoc2 = getOwningPhysicalAssemblyDocumentForVirtualModel(model)

            'A virtual component opened in position inherits its physical assembly's lock,
            'commit, and SVN status. Returning the owner also keeps context-menu actions from
            'sending the virtual AppData/internal path to SVN.
            If ownerDocument IsNot Nothing Then Return ownerDocument
            Return model
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
            " [Saving to SVN",
            " [Pending",
            " [Not in SVN]"
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

    Private Function normalizeDeferredSolidWorksPath(ByVal filePath As String) As String
        If String.IsNullOrWhiteSpace(filePath) Then Return ""

        Try
            Return Path.GetFullPath(filePath)
        Catch
            Return filePath.Trim()
        End Try
    End Function

    Private Sub ensureDeferredSolidWorksUiTimer()
        If deferredSolidWorksUiTimer Is Nothing Then
            deferredSolidWorksUiTimer = New System.Windows.Forms.Timer()
            deferredSolidWorksUiTimer.Interval = 350
        End If
    End Sub

    Private Sub startDeferredSolidWorksUiTimer()
        If taskPaneClosing Then Exit Sub

        ensureDeferredSolidWorksUiTimer()

        'Only restart the attempt budget for a genuinely new deferred batch.
        'reconcileWriteAccessForActiveDocumentPublic re-arms this timer on every document
        'activation, so resetting the counter unconditionally meant
        'MAX_DEFERRED_SOLIDWORKS_UI_ATTEMPTS was never reached while the user kept switching
        'windows: a document SOLIDWORKS refuses to switch writable stayed queued forever and
        'the 350 ms tick kept re-issuing its expensive native SetReadOnlyState call, which on
        'a virtual/imported-heavy assembly can block the SOLIDWORKS UI thread for minutes at a
        'time. It also meant the "did not safely switch to writable" warning below never fired.
        'The tick handler still clears this counter and stops the timer once the queues drain,
        'so an ordinary batch continues to get its full retry budget.
        If Not deferredSolidWorksUiTimer.Enabled Then
            deferredSolidWorksUiAttemptCount = 0
        End If

        Try
            deferredSolidWorksUiTimer.Stop()
            deferredSolidWorksUiTimer.Start()
        Catch
        End Try
    End Sub

    Public Sub queueFeatureTreeRefreshForPathsPublic(ByVal filePaths() As String)
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(
                    New MethodInvoker(
                        Sub() queueFeatureTreeRefreshForPathsPublic(filePaths)
                    )
                )
                Exit Sub
            End If
        Catch
        End Try

        If taskPaneClosing Then Exit Sub
        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        For Each filePath As String In filePaths
            Dim normalizedPath As String =
                normalizeDeferredSolidWorksPath(filePath)

            If normalizedPath <> "" Then
                pendingFeatureTreeRefreshPaths.Add(normalizedPath)
            End If
        Next

        If pendingFeatureTreeRefreshPaths.Count > 0 Then
            startDeferredSolidWorksUiTimer()
        End If
    End Sub

    Public Sub queueSvnTreeStructureRefreshPublic()
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(New MethodInvoker(AddressOf queueSvnTreeStructureRefreshPublic))
                Exit Sub
            End If
        Catch
        End Try

        If taskPaneClosing Then Exit Sub

        pendingSvnTreeStructureRefresh = True
        startDeferredSolidWorksUiTimer()
    End Sub

    Public Sub forceWriteAccessForLockedFilePathsPublic(ByVal filePaths() As String)
        Try
            If Me.InvokeRequired Then
                Me.BeginInvoke(
                    New MethodInvoker(
                        Sub() forceWriteAccessForLockedFilePathsPublic(filePaths)
                    )
                )
                Exit Sub
            End If
        Catch
        End Try

        If taskPaneClosing Then Exit Sub
        If filePaths Is Nothing OrElse filePaths.Length = 0 Then Exit Sub

        For Each filePath As String In filePaths
            Dim normalizedPath As String =
                normalizeDeferredSolidWorksPath(filePath)

            If normalizedPath = "" Then Continue For

            Try
                If File.Exists(normalizedPath) Then
                    File.SetAttributes(
                        normalizedPath,
                        File.GetAttributes(normalizedPath) And Not FileAttributes.ReadOnly
                    )
                End If
            Catch
            End Try

            'Sync/Get Locks completion and window activation all funnel here. Never even
            'enqueue a file whose native writable transition is known/predicted pathological
            '(virtual/STEP-heavy) - the disk attribute above is already cleared, and the
            'explicit edit/save precheck performs the live transition when actually needed.
            'An in-flight Edit Component replay target is still enqueued so its transition
            'and replay complete normally.
            Try
                If svnModule.shouldSkipBackgroundWritableTransitionPublic(normalizedPath) AndAlso
                   Not svnModule.isPendingInContextAutoEditTargetPublic(normalizedPath) Then
                    svnModule.logOperationPublic(
                        "Known-slow writable transition not queued: " & normalizedPath
                    )
                    Continue For
                End If
            Catch
            End Try

            pendingWriteAccessPaths.Add(normalizedPath)
        Next

        If pendingWriteAccessPaths.Count > 0 Then
            Try
                svnModule.logOperationPublic(
                    "Queued writable-state reconciliation: " &
                    String.Join(" | ", pendingWriteAccessPaths.ToArray())
                )
            Catch
            End Try

            startDeferredSolidWorksUiTimer()
        End If
    End Sub

    Private Sub deferredSolidWorksUiTimer_Tick(
        sender As Object,
        e As EventArgs
    ) Handles deferredSolidWorksUiTimer.Tick

        If taskPaneClosing Then
            Try
                deferredSolidWorksUiTimer.Stop()
            Catch
            End Try
            Exit Sub
        End If

        If iSwApp Is Nothing Then Exit Sub

        Try
            If Not svnModule.canRunDeferredSolidWorksUiMutationPublic() Then Exit Sub
        Catch
            Exit Sub
        End Try

        Dim acquiredMutationGate As Boolean = False

        Try
            acquiredMutationGate =
                svnModule.tryBeginSolidWorksNativeMutationPublic(
                    "Deferred write access / FeatureManager refresh"
                )
        Catch
            acquiredMutationGate = False
        End Try

        If Not acquiredMutationGate Then Exit Sub

        deferredSolidWorksUiAttemptCount += 1

        Try
            Dim writeSnapshot As String() =
                pendingWriteAccessPaths.ToArray()

            For Each filePath As String In writeSnapshot
                Dim removeFromQueue As Boolean = False
                Dim doc As ModelDoc2 = Nothing

                Try
                    If File.Exists(filePath) Then
                        File.SetAttributes(
                            filePath,
                            File.GetAttributes(filePath) And Not FileAttributes.ReadOnly
                        )
                    End If
                Catch
                End Try

                Try
                    doc = TryCast(
                        iSwApp.GetOpenDocumentByName(filePath),
                        ModelDoc2
                    )
                Catch ex As InvalidComObjectException
                    Exit For
                Catch ex As COMException
                    doc = Nothing
                Catch
                    doc = Nothing
                End Try

                If doc Is Nothing Then
                    removeFromQueue = True
                Else
                    'A SOLIDWORKS assembly can report GetSaveFlag=True for rebuild/display-state
                    'reasons even when no user-authored assembly change exists. The previous
                    'robustness patch treated that flag as a reason to skip SetReadOnlyState(False),
                    'which left legitimately locked assemblies read-only until the retry timed out.
                    '
                    'Changing only the read-only state does not discard or reload document data, so
                    'it is safe to request writable access even when SOLIDWORKS currently reports a
                    'save flag. We still reacquire the live ModelDoc2 by path, run on the UI thread,
                    'serialize through the native-mutation gate, and never call ReloadOrReplace.
                    Dim stateReadable As Boolean = True
                    Dim isReadOnly As Boolean = False

                    Try
                        isReadOnly = doc.IsOpenedReadOnly()
                    Catch
                        stateReadable = False
                    End Try

                    If stateReadable Then
                        If Not isReadOnly Then
                            removeFromQueue = True
                        ElseIf svnModule.shouldSkipBackgroundWritableTransitionPublic(filePath) AndAlso
                               Not svnModule.isPendingInContextAutoEditTargetPublic(filePath) Then
                            'This file's native writable transition previously blocked the
                            'SOLIDWORKS UI thread for a pathological duration. Never repeat it
                            'from this background timer; the explicit edit/save precheck will
                            'perform it if the user actually works on the file. The lock and
                            'the on-disk attribute are already correct, so drop it silently.
                            Try
                                svnModule.logOperationPublic(
                                    "Known-slow writable transition skipped by deferred timer: " & filePath
                                )
                            Catch
                            End Try

                            removeFromQueue = True
                        Else
                            Try
                                If File.Exists(filePath) Then
                                    File.SetAttributes(
                                        filePath,
                                        File.GetAttributes(filePath) And Not FileAttributes.ReadOnly
                                    )
                                End If
                            Catch
                            End Try

                            Dim transitionWatch As Stopwatch = Stopwatch.StartNew()

                            Try
                                doc.SetReadOnlyState(False)
                            Catch ex As COMException
                            Catch
                            End Try

                            Try
                                svnModule.noteWritableTransitionDurationPublic(
                                    filePath,
                                    transitionWatch.ElapsedMilliseconds
                                )
                            Catch
                            End Try

                            Try
                                removeFromQueue = Not doc.IsOpenedReadOnly()
                            Catch
                                removeFromQueue = False
                            End Try
                        End If
                    End If
                End If

                If removeFromQueue Then
                    pendingWriteAccessPaths.Remove(filePath)

                    Try
                        svnModule.logOperationPublic(
                            "Writable-state reconciliation completed: " & filePath
                        )
                    Catch
                    End Try

                    Try
                        svnModule.noteDeferredWriteAccessResultPublic(filePath, True)
                    Catch
                    End Try
                End If
            Next

            Dim refreshSnapshot As String() =
                pendingFeatureTreeRefreshPaths.ToArray()

            For Each filePath As String In refreshSnapshot
                Dim removeFromQueue As Boolean = False
                Dim doc As ModelDoc2 = Nothing

                Try
                    doc = TryCast(
                        iSwApp.GetOpenDocumentByName(filePath),
                        ModelDoc2
                    )
                Catch ex As InvalidComObjectException
                    Exit For
                Catch ex As COMException
                    doc = Nothing
                Catch
                    doc = Nothing
                End Try

                If doc Is Nothing Then
                    removeFromQueue = True
                Else
                    Try
                        Dim featureManager As FeatureManager = doc.FeatureManager

                        If featureManager IsNot Nothing Then
                            featureManager.UpdateFeatureTree()
                        End If

                        doc.GraphicsRedraw2()
                        removeFromQueue = True
                    Catch ex As COMException
                        removeFromQueue = False
                    Catch
                        removeFromQueue = False
                    End Try
                End If

                If removeFromQueue Then
                    pendingFeatureTreeRefreshPaths.Remove(filePath)

                    Try
                        svnModule.logOperationPublic(
                            "Deferred FeatureManager refresh completed: " & filePath
                        )
                    Catch
                    End Try
                End If
            Next

            If pendingSvnTreeStructureRefresh Then
                Try
                    refreshCurrentTreeViewOnly()
                    pendingSvnTreeStructureRefresh = False
                    lastGraphicalSelectionPath = ""
                    lastGraphicalSelectionComponentName = ""

                    'The selected newly-added component can now be revealed immediately.
                    syncSvnTreeToCurrentSolidWorksSelectionPublic()
                Catch ex As COMException
                    'Retry on the next deferred tick while SOLIDWORKS finishes the import.
                Catch
                    pendingSvnTreeStructureRefresh = False
                End Try
            End If

        Finally
            Try
                svnModule.endSolidWorksNativeMutationPublic(
                    "Deferred write access / FeatureManager refresh"
                )
            Catch
            End Try
        End Try

        If pendingWriteAccessPaths.Count = 0 AndAlso
           pendingFeatureTreeRefreshPaths.Count = 0 AndAlso
           Not pendingSvnTreeStructureRefresh Then

            deferredSolidWorksUiAttemptCount = 0

            Try
                deferredSolidWorksUiTimer.Stop()
            Catch
            End Try

            Exit Sub
        End If

        If deferredSolidWorksUiAttemptCount >=
           MAX_DEFERRED_SOLIDWORKS_UI_ATTEMPTS Then

            Try
                deferredSolidWorksUiTimer.Stop()
            Catch
            End Try

            If pendingWriteAccessPaths.Count > 0 Then
                Dim unresolvedPaths As String =
                    String.Join(
                        System.Environment.NewLine,
                        pendingWriteAccessPaths.Select(
                            Function(p As String) Path.GetFileName(p)
                        )
                    )

                Try
                    svnModule.logOperationPublic(
                        "Writable-state reconciliation timed out: " &
                        String.Join(" | ", pendingWriteAccessPaths.ToArray())
                    )
                Catch
                End Try

                Try
                    iSwApp.SendMsgToUser2(
                        "The SVN lock was obtained, but SOLIDWORKS did not safely switch " &
                        "the following open file(s) to writable:" &
                        vbCrLf & vbCrLf &
                        unresolvedPaths &
                        vbCrLf & vbCrLf &
                        "Your locks are still valid. Close and reopen only those documents " &
                        "before editing them." & vbCrLf & vbCrLf &
                        "Click Sync to refresh SVN status. If a file is out of date, use Get Latest first.",
                        swMessageBoxIcon_e.swMbWarning,
                        swMessageBoxBtn_e.swMbOk
                    )
                Catch
                End Try

                For Each unresolvedPath As String In pendingWriteAccessPaths.ToArray()
                    Try
                        svnModule.noteDeferredWriteAccessResultPublic(unresolvedPath, False)
                    Catch
                    End Try
                Next
            End If

            pendingWriteAccessPaths.Clear()
            pendingFeatureTreeRefreshPaths.Clear()
            pendingSvnTreeStructureRefresh = False
            deferredSolidWorksUiAttemptCount = 0
        End If
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

        Dim nodeFilePath As String = ""

        Try
            'Always resolve through the tree-node helper. For normal physical nodes this is
            'the same file path; for a virtual component opened in position it is the owning
            'physical assembly path, preventing the false [Not in SVN] state.
            nodeFilePath = getCadPathFromTreeNode(rootNode)

            If String.IsNullOrWhiteSpace(nodeFilePath) AndAlso modDoc IsNot Nothing Then
                nodeFilePath = modDoc.GetPathName()
            End If
        Catch
            nodeFilePath = ""
        End Try

        If Not String.IsNullOrWhiteSpace(nodeFilePath) Then
            status1 = findStatusForFile(nodeFilePath)
        Else
            status1 = findStatusForFile(baseNodeText)
        End If

        Dim pathIsOutsideSvn As Boolean = False

        If Not String.IsNullOrWhiteSpace(nodeFilePath) Then
            Try
                pathIsOutsideSvn = Not svnModule.isPathInsideLocalRepoPublic(nodeFilePath)
            Catch
                pathIsOutsideSvn = False
            End Try
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

        If pathIsOutsideSvn Then
            rootNode.BackColor = myCol.notOnVault
            rootNode.ToolTipText = "Not in SVN. PlumVault will skip this file during Sync."
            rootNode.Text &= " [Not in SVN]"

        ElseIf status1 Is Nothing Then
            rootNode.BackColor = myCol.unknown
            rootNode.ToolTipText = "Unknown"

        ElseIf status1.fp(0).addDelChg1 = "?" Then
            rootNode.BackColor = myCol.notOnVault
            rootNode.ToolTipText = "Not in SVN. PlumVault will skip this file during Sync."
            rootNode.Text &= " [Not in SVN]"
            If bModelDocAttached Then
                docMenu.Items.Add(myContextMenu.addToRepo)
            End If

        ElseIf status1.fp(0).upToDate9 = "*" Then
            rootNode.BackColor = myCol.outOfDate
            rootNode.ToolTipText = "Your Copy is Out Of Date"
            'If bModelDocAttached Then docMenu.Items.AddRange({myContextMenu.getLocksStealLabel})

        ElseIf status1.fp(0).addDelChg1 = "M" OrElse
            status1.fp(0).addDelChg1 = "A" Then

            rootNode.BackColor = myCol.localChangesNotCommitted
            rootNode.ToolTipText = "Local changes not committed"
            rootNode.Text &= " [Not committed]"

            If bModelDocAttached Then
                docMenu.Items.Add(myContextMenu.commitLabel)
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
                        myContextMenu.unlockLabel})
                Else
                    docMenu.Items.AddRange(
                        {myContextMenu.commitLabel,
                        myContextMenu.unlockLabel})
                End If
            End If


        ElseIf status1.fp(0).lock6 = "O" OrElse
            status1.fp(0).lock6 = "T" OrElse
            status1.fp(0).lock6 = "B" OrElse
            (Not String.IsNullOrWhiteSpace(status1.fp(0).lockOwner) AndAlso status1.fp(0).lock6 <> "K") Then
            rootNode.BackColor = myCol.lockedBySomeoneElse

            If status1.fp(0).lock6 = "T" Then
                rootNode.ToolTipText = "Your local SVN lock token was stolen"
                rootNode.Text &= " [Lock stolen]"
            ElseIf status1.fp(0).lock6 = "B" Then
                rootNode.ToolTipText = "Your local SVN lock token is broken"
                rootNode.Text &= " [Lock broken]"
            ElseIf Not String.IsNullOrWhiteSpace(status1.fp(0).lockOwner) Then
                rootNode.ToolTipText = "Locked by: " & status1.fp(0).lockOwner
                rootNode.Text &= " [Locked: " & status1.fp(0).lockOwner & "]"
            Else
                rootNode.ToolTipText = "Locked by someone else"
                rootNode.Text &= " [Locked]"
            End If
            If bModelDocAttached Then
                docMenu.Items.AddRange({myContextMenu.getLocksStealLabel})
            End If
            'If bCM Then rootNode.ContextMenuStrip.Items.Add(myContextMenu.getLocksStealLabel)
        ElseIf status1.fp(0).released = "||RELEASED||" Then
            rootNode.BackColor = myCol.released
            rootNode.ToolTipText = "Released"
            If bModelDocAttached Then
                docMenu.Items.AddRange({myContextMenu.upRevEdit})
            End If
        ElseIf status1.fp(0).lock6 = " " Then
            rootNode.BackColor = myCol.available
            rootNode.ToolTipText = "Available"
            If bModelDocAttached Then
                docMenu.Items.Add(myContextMenu.getLockActiveDoc)
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

        'When a virtual part/subassembly is open in its own edit tab, it still has no
        'independent SVN file. Route file actions to the physical owning assembly.
        Dim virtualOwnerDocument As ModelDoc2 = getOwningPhysicalAssemblyDocumentForVirtualModel(activeModDoc)
        If virtualOwnerDocument IsNot Nothing Then Return New ModelDoc2() {virtualOwnerDocument}

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

            If swComp IsNot Nothing AndAlso isComponentVirtualSafe(swComp) Then
                Dim ownerPath As String = getPhysicalOwnerAssemblyPathForComponent(swComp, activeModDoc)
                Dim ownerDocument As ModelDoc2 = getOpenDocumentByPathSafe(ownerPath)

                If ownerDocument Is Nothing Then Continue For
                modDocArr(UBound(modDocArr)) = ownerDocument

            ElseIf ensureResolvedComponent(swComp) Then
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

        Return distinctModelDocsByPhysicalPath(modDocArr)

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
    Private Function getExactSelectedCadPathsForUtilityAction() As String()
        Dim selectedPaths() As String = getBatchSelectedTreeCadPathsForAction(includeSingleSelectedNode:=True)
        If selectedPaths IsNot Nothing AndAlso selectedPaths.Length > 0 Then Return selectedPaths

        Dim selectedDocs() As ModelDoc2 = GetSelectedModDocList(iSwApp)
        If selectedDocs Is Nothing OrElse selectedDocs.Length = 0 Then
            Dim activeDoc As ModelDoc2 = TryCast(iSwApp.ActiveDoc, ModelDoc2)
            If activeDoc IsNot Nothing Then selectedDocs = {activeDoc}
        End If

        If selectedDocs Is Nothing OrElse selectedDocs.Length = 0 Then Return Nothing

        Dim output As New List(Of String)()
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        For Each selectedDoc As ModelDoc2 In selectedDocs
            If selectedDoc Is Nothing Then Continue For

            Dim selectedPath As String = getSafeModelPath(selectedDoc)
            If String.IsNullOrWhiteSpace(selectedPath) Then Continue For

            selectedPath = normalizeTreeActionPath(selectedPath)
            If Not isCadPathForSync(selectedPath) OrElse seen.Contains(selectedPath) Then Continue For

            seen.Add(selectedPath)
            output.Add(selectedPath)
        Next

        If output.Count = 0 Then Return Nothing
        Return output.ToArray()
    End Function

    Public Sub copyFileToClipboard(bWithDependents As Boolean, bTitleOnly As Boolean)
        'bWithDependents remains in the signature for designer/binary compatibility only.
        'Every exposed and indirect route now copies the exact selected files.
        Dim selectedPaths() As String = getExactSelectedCadPathsForUtilityAction()

        If selectedPaths Is Nothing OrElse selectedPaths.Length = 0 Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If

        Dim sOutput() As String

        If bTitleOnly Then
            sOutput = selectedPaths.Select(Function(selectedPath) Path.GetFileName(selectedPath)).ToArray()
        Else
            sOutput = selectedPaths
        End If

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
        Dim selectedPaths() As String = getExactSelectedCadPathsForUtilityAction()
        If selectedPaths Is Nothing OrElse selectedPaths.Length = 0 Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If

        Dim urls As String() = getUrlfromPaths(selectedPaths)

        CopyToClipboard(String.Join(vbCrLf, urls))
        hideButton(ToolStripSplitButFolder)
    End Sub
    Private Sub CopySvnUrlWithDependentsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopySvnUrlWithDependentsToolStripMenuItem.Click
        CopySvnUrlToolStripMenuItem_Click(sender, e)
    End Sub
    Private Sub CopyActiveFilesParentFolderToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyActiveFilesParentFolderToolStripMenuItem.Click
        Dim selectedPaths() As String = getExactSelectedCadPathsForUtilityAction()
        If selectedPaths Is Nothing OrElse selectedPaths.Length = 0 Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If

        Dim currentDir As DirectoryInfo = New FileInfo(selectedPaths(0)).Directory

        CopyToClipboard(currentDir.ToString)
        hideButton(ToolStripSplitButFolder)

    End Sub

    Private Sub ShareWithColleagueToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ShareWithColleagueToolStripMenuItem.Click
        Dim selectedPaths() As String = getExactSelectedCadPathsForUtilityAction()
        If selectedPaths Is Nothing OrElse selectedPaths.Length = 0 Then
            iSwApp.SendMsgToUser("Couldn't find an active document! Exiting.")
            Exit Sub
        End If

        Dim stringArr() As String = getUrlfromPaths({selectedPaths(0)})

        If IsNothing(stringArr) Then
            iSwApp.SendMsgToUser("Couldn't find get URL(s)! Exiting.")
            Exit Sub
        End If

        Dim stringToClip As String = "CAD is available on svn" & vbCrLf & "My Local Path (yours may be different):" & vbCrLf

        stringToClip &= selectedPaths(0) & vbCrLf & vbCrLf & "or remote path: " & vbCrLf
        stringToClip &= stringArr(0)

        CopyToClipboard(stringToClip)
        hideButton(ToolStripSplitButFolder)
    End Sub

    Private Sub CreateSvnFilelistToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CreateSvnFilelistToolStripMenuItem.Click
        If Not verifyLocalRepoPath() Then Exit Sub

        Dim sDest As String = localRepoPath.Text & "\" & "fileList.txt"
        Dim selectedPaths() As String = getExactSelectedCadPathsForUtilityAction()

        If selectedPaths Is Nothing OrElse selectedPaths.Length = 0 Then
            iSwApp.SendMsgToUser("Couldn't find document! Exiting.")
            Exit Sub
        End If

        Dim sFileNames As String = formatFilePathArrForProc(selectedPaths, sDelimiter:=vbCrLf)

        Try
            File.WriteAllText(sDest, sFileNames)
            iSwApp.SendMsgToUser("Wrote Filelist to " & vbCrLf & sDest)
        Catch ex As Exception
            iSwApp.SendMsgToUser("ERROR writing Filelist to " & vbCrLf & sDest)
        End Try
        hideButton(ToolStripSplitButFolder)
    End Sub

    Private Sub CreateSvnFilelistWithDependentsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CreateSvnFilelistWithDependentsToolStripMenuItem.Click
        CreateSvnFilelistToolStripMenuItem_Click(sender, e)
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
