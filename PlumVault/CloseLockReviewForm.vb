Imports System.Collections.Generic
Imports System.Drawing
Imports System.IO
Imports System.Linq
Imports System.Windows.Forms

Public Enum CloseLockReviewDecision
    ReturnToSolidWorks = 0
    ContinueClose = 1
End Enum

Public Class CloseLockReviewItem
    Public Property FilePath As String = ""
    Public Property IsSafeToUnlock As Boolean = True
    Public Property StateText As String = "Clean; nothing to commit"
    Public Property IsStillLocked As Boolean = True
    Public Property ResultText As String = "Lock retained"
    Public Property CanCommit As Boolean = False
    Public Property CanRevert As Boolean = False
End Class

Public Class CloseLockReviewForm
    Inherits Form

    Private ReadOnly reviewItems As List(Of CloseLockReviewItem)
    Private ReadOnly closingSolidWorks As Boolean

    Private ReadOnly headerLabel As New Label()
    Private ReadOnly grid As New DataGridView()
    Private ReadOnly footerLabel As New Label()
    Private ReadOnly returnButton As New Button()
    Private ReadOnly continueButton As New Button()

    Private unlockInProgress As Boolean = False
    Private pendingCommitPath As String = ""

    'Explicit fonts prevent SOLIDWORKS/system font inheritance from producing
    'thin or oddly scaled text on high-DPI displays.
    Private ReadOnly formUiFont As Font = CreateUiFont(10.0F, False)
    Private ReadOnly instructionUiFont As Font = CreateUiFont(10.25F, False)
    Private ReadOnly gridUiFont As Font = CreateUiFont(9.5F, False)
    Private ReadOnly gridBoldUiFont As Font = CreateUiFont(9.5F, True)
    Private ReadOnly buttonUiFont As Font = CreateUiFont(10.0F, False)

    Private Shared Function CreateUiFont(ByVal pointSize As Single,
                                         ByVal bold As Boolean) As Font
        Dim style As FontStyle = If(bold, FontStyle.Bold, FontStyle.Regular)

        Try
            Return New Font("Segoe UI", pointSize, style, GraphicsUnit.Point)
        Catch
            Return New Font(SystemFonts.MessageBoxFont.FontFamily,
                            pointSize,
                            style,
                            GraphicsUnit.Point)
        End Try
    End Function

    'Colour key for the lock-review table.
    'Yellow = the file is still locked to the current user.
    'Green = the lock was successfully released in this window.
    Private Shared ReadOnly LockedRowBackColor As Color = Color.FromArgb(255, 248, 204)
    Private Shared ReadOnly LockedRowForeColor As Color = Color.FromArgb(92, 69, 0)
    Private Shared ReadOnly UnlockedRowBackColor As Color = Color.FromArgb(220, 245, 224)
    Private Shared ReadOnly UnlockedRowForeColor As Color = Color.FromArgb(24, 92, 39)

    Public ReadOnly Property Decision As CloseLockReviewDecision
        Get
            Return _decision
        End Get
    End Property

    Private _decision As CloseLockReviewDecision = CloseLockReviewDecision.ReturnToSolidWorks

    Public Sub New(ByVal items As IEnumerable(Of CloseLockReviewItem),
                   ByVal isClosingSolidWorks As Boolean)
        reviewItems = If(items, Enumerable.Empty(Of CloseLockReviewItem)()).ToList()
        closingSolidWorks = isClosingSolidWorks

        AddHandler svnModule.CloseReviewCommitCompleted, AddressOf CloseReviewCommitCompleted
        InitializeWindow()
        PopulateRows()
        UpdateFooter()
    End Sub

    Private Sub InitializeWindow()
        Text = If(closingSolidWorks,
                  "Review SVN Locks Before Closing SOLIDWORKS",
                  "Review SVN Locks Before Closing File")

        StartPosition = FormStartPosition.CenterScreen

        'Keep the dialog usable on smaller/high-DPI displays. The grid scrolls horizontally
        'when the working area cannot show every decision column at once.
        MinimumSize = New Size(900, 430)
        Size = New Size(1180, 560)
        ShowIcon = False
        ShowInTaskbar = False
        MaximizeBox = True
        MinimizeBox = False
        AutoScaleMode = AutoScaleMode.Dpi
        Font = formUiFont

        Dim rootLayout As New TableLayoutPanel()
        rootLayout.Dock = DockStyle.Fill
        rootLayout.Padding = New Padding(14)
        rootLayout.ColumnCount = 1
        rootLayout.RowCount = 4
        rootLayout.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        rootLayout.RowStyles.Add(New RowStyle(SizeType.Percent, 100.0F))
        rootLayout.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        rootLayout.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        Controls.Add(rootLayout)

        headerLabel.AutoSize = True
        headerLabel.Font = instructionUiFont
        headerLabel.Dock = DockStyle.Top
        headerLabel.MaximumSize = New Size(1060, 0)
        headerLabel.Padding = New Padding(0, 0, 0, 10)
        headerLabel.Text =
            "You still have SVN locks on the files below. While a lock is held, edit access is reserved to you." &
            Environment.NewLine &
            "For each changed file, Commit to keep your work or Revert to discard it. Unlock files whose work is complete. " &
            "You may retain any lock you still need. Continuing close is the final decision: saved local SVN changes remain on disk, " &
            "but unsaved changes still only in SOLIDWORKS will be discarded without another Save/Don't Save prompt." &
            Environment.NewLine &
            "Yellow rows are still locked. Green rows have been successfully unlocked."
        rootLayout.Controls.Add(headerLabel, 0, 0)

        ConfigureGrid()
        rootLayout.Controls.Add(grid, 0, 1)

        footerLabel.AutoSize = True
        footerLabel.Font = formUiFont
        footerLabel.Dock = DockStyle.Fill
        footerLabel.Padding = New Padding(0, 9, 0, 7)
        rootLayout.Controls.Add(footerLabel, 0, 2)

        Dim buttonPanel As New TableLayoutPanel()
        buttonPanel.Dock = DockStyle.Fill
        buttonPanel.AutoSize = True
        buttonPanel.ColumnCount = 3
        buttonPanel.RowCount = 1
        buttonPanel.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))
        buttonPanel.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 100.0F))
        buttonPanel.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))
        rootLayout.Controls.Add(buttonPanel, 0, 3)

        returnButton.AutoSize = True
        returnButton.Font = buttonUiFont
        returnButton.MinimumSize = New Size(190, 38)
        returnButton.Text = "Return to SOLIDWORKS"
        AddHandler returnButton.Click, AddressOf ReturnButton_Click
        buttonPanel.Controls.Add(returnButton, 0, 0)

        continueButton.AutoSize = True
        continueButton.Font = buttonUiFont
        continueButton.MinimumSize = New Size(285, 38)
        continueButton.Text = If(closingSolidWorks,
                                 "Close SOLIDWORKS — no further saves",
                                 "Close file — no further saves")
        AddHandler continueButton.Click, AddressOf ContinueButton_Click
        buttonPanel.Controls.Add(continueButton, 2, 0)

        AcceptButton = continueButton
        CancelButton = returnButton

        AddHandler FormClosing, AddressOf CloseLockReviewForm_FormClosing
        AddHandler FormClosed, AddressOf CloseLockReviewForm_FormClosed
    End Sub

    Private Sub ConfigureGrid()
        grid.Dock = DockStyle.Fill
        grid.AllowUserToAddRows = False
        grid.AllowUserToDeleteRows = False
        grid.AllowUserToOrderColumns = False
        grid.AllowUserToResizeRows = False
        grid.MultiSelect = False
        grid.ReadOnly = True
        grid.RowHeadersVisible = False
        grid.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        grid.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
        grid.BackgroundColor = SystemColors.Window
        grid.BorderStyle = BorderStyle.Fixed3D
        grid.Font = gridUiFont
        grid.DefaultCellStyle.Font = gridUiFont
        grid.DefaultCellStyle.Padding = New Padding(5, 2, 5, 2)
        grid.ColumnHeadersDefaultCellStyle.Font = gridBoldUiFont
        grid.ColumnHeadersDefaultCellStyle.Padding = New Padding(5, 3, 5, 3)
        grid.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.DisableResizing
        grid.ColumnHeadersHeight = 38
        grid.RowTemplate.Height = 34

        Dim fileNameColumn As New DataGridViewTextBoxColumn()
        fileNameColumn.Name = "FileName"
        fileNameColumn.HeaderText = "File"
        fileNameColumn.Width = 190
        fileNameColumn.MinimumWidth = 150
        grid.Columns.Add(fileNameColumn)

        Dim locationColumn As New DataGridViewTextBoxColumn()
        locationColumn.Name = "Location"
        locationColumn.HeaderText = "Location"
        locationColumn.AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
        locationColumn.MinimumWidth = 220
        grid.Columns.Add(locationColumn)

        Dim stateColumn As New DataGridViewTextBoxColumn()
        stateColumn.Name = "State"
        stateColumn.HeaderText = "SVN state"
        stateColumn.Width = 180
        stateColumn.MinimumWidth = 130
        grid.Columns.Add(stateColumn)

        Dim commitColumn As New DataGridViewButtonColumn()
        commitColumn.Name = "Commit"
        commitColumn.HeaderText = "Commit"
        commitColumn.UseColumnTextForButtonValue = False
        commitColumn.Width = 90
        commitColumn.MinimumWidth = 90
        grid.Columns.Add(commitColumn)

        Dim revertColumn As New DataGridViewButtonColumn()
        revertColumn.Name = "Revert"
        revertColumn.HeaderText = "Discard"
        revertColumn.UseColumnTextForButtonValue = False
        revertColumn.Width = 90
        revertColumn.MinimumWidth = 90
        grid.Columns.Add(revertColumn)

        Dim unlockColumn As New DataGridViewButtonColumn()
        unlockColumn.Name = "Unlock"
        unlockColumn.HeaderText = "Release"
        unlockColumn.Text = "Unlock"
        unlockColumn.UseColumnTextForButtonValue = False
        unlockColumn.Width = 95
        unlockColumn.MinimumWidth = 95
        grid.Columns.Add(unlockColumn)

        Dim resultColumn As New DataGridViewTextBoxColumn()
        resultColumn.Name = "Result"
        resultColumn.HeaderText = "Result"
        resultColumn.Width = 240
        resultColumn.MinimumWidth = 160
        'Result messages can be a full sentence (e.g. "This file has no committable local
        'change"). Wrap instead of silently clipping it - AutoSizeRowsMode already grows
        'the row height to fit wrapped text.
        resultColumn.DefaultCellStyle.WrapMode = DataGridViewTriState.True
        grid.Columns.Add(resultColumn)

        AddHandler grid.CellContentClick, AddressOf Grid_CellContentClick
        AddHandler grid.CellToolTipTextNeeded, AddressOf Grid_CellToolTipTextNeeded
    End Sub

    Private Sub PopulateRows()
        grid.Rows.Clear()

        For Each item As CloseLockReviewItem In reviewItems
            Dim fileName As String = item.FilePath
            Dim folderPath As String = ""

            Try
                fileName = Path.GetFileName(item.FilePath)
                folderPath = Path.GetDirectoryName(item.FilePath)
            Catch
            End Try

            Dim buttonText As String = If(item.IsSafeToUnlock, "Unlock", "Return first")
            Dim commitText As String = If(item.CanCommit, "Commit", "—")
            Dim revertText As String = If(item.CanRevert, "Revert", "—")

            Dim rowIndex As Integer = grid.Rows.Add(
                fileName,
                folderPath,
                item.StateText,
                commitText,
                revertText,
                buttonText,
                item.ResultText
            )

            Dim row As DataGridViewRow = grid.Rows(rowIndex)
            row.Tag = item

            ApplyRowVisualState(row, item)
        Next
    End Sub

    Private Sub ApplyRowVisualState(ByVal row As DataGridViewRow,
                                    ByVal item As CloseLockReviewItem)
        If row Is Nothing OrElse item Is Nothing Then Exit Sub

        Dim rowBackColor As Color
        Dim rowForeColor As Color

        If item.IsStillLocked Then
            rowBackColor = LockedRowBackColor
            rowForeColor = LockedRowForeColor
        Else
            rowBackColor = UnlockedRowBackColor
            rowForeColor = UnlockedRowForeColor
        End If

        row.DefaultCellStyle.BackColor = rowBackColor
        row.DefaultCellStyle.ForeColor = rowForeColor

        'Keep the meaning visible even when a user clicks/selects the row.
        row.DefaultCellStyle.SelectionBackColor = rowBackColor
        row.DefaultCellStyle.SelectionForeColor = rowForeColor

        row.Cells("State").Style.Font = gridBoldUiFont
        row.Cells("Result").Style.Font = gridBoldUiFont

        If item.IsStillLocked AndAlso Not item.IsSafeToUnlock Then
            'The row remains yellow because the lock is still held, but the red
            'state text makes it clear that the user must return and commit first.
            row.Cells("State").Style.ForeColor = Color.DarkRed
            row.Cells("Result").Style.ForeColor = Color.DarkRed
        Else
            row.Cells("State").Style.ForeColor = rowForeColor
            row.Cells("Result").Style.ForeColor = rowForeColor
        End If

        Try
            Dim unlockCell As DataGridViewButtonCell = TryCast(row.Cells("Unlock"), DataGridViewButtonCell)
            If unlockCell IsNot Nothing Then
                unlockCell.FlatStyle = FlatStyle.Flat
                unlockCell.Style.BackColor = rowBackColor
                unlockCell.Style.ForeColor = rowForeColor
                unlockCell.Style.SelectionBackColor = rowBackColor
                unlockCell.Style.SelectionForeColor = rowForeColor
            End If
        Catch
        End Try
    End Sub

    Private Sub Grid_CellContentClick(sender As Object, e As DataGridViewCellEventArgs)
        If unlockInProgress Then Exit Sub
        If e.RowIndex < 0 Then Exit Sub
        Dim row As DataGridViewRow = grid.Rows(e.RowIndex)
        Dim item As CloseLockReviewItem = TryCast(row.Tag, CloseLockReviewItem)
        If item Is Nothing Then Exit Sub

        If e.ColumnIndex = grid.Columns("Commit").Index Then
            StartCommitForItem(row, item)
            Exit Sub
        End If

        If e.ColumnIndex = grid.Columns("Revert").Index Then
            RevertItem(row, item)
            Exit Sub
        End If

        If e.ColumnIndex <> grid.Columns("Unlock").Index Then Exit Sub

        If Not item.IsStillLocked Then
            row.Cells("Result").Value = "Already unlocked"
            Exit Sub
        End If

        If Not item.IsSafeToUnlock Then
            row.Cells("Result").Value = "Return to SOLIDWORKS and commit first"
            Return
        End If

        Dim errorMessage As String = ""

        Try
            unlockInProgress = True
            UseWaitCursor = True
            returnButton.Enabled = False
            continueButton.Enabled = False
            row.Cells("Result").Value = "Unlocking..."
            grid.Refresh()
            Application.DoEvents()

            Dim unlocked As Boolean = svnModule.unlockPathFromCloseReviewPublic(item.FilePath, errorMessage)

            If unlocked Then
                item.IsStillLocked = False
                item.ResultText = "Unlocked"
                row.Cells("Unlock").Value = "Unlocked"
                row.Cells("Result").Value = "Unlocked successfully"
                row.Cells("State").Value = "Unlocked"
                ApplyRowVisualState(row, item)
            Else
                If String.IsNullOrWhiteSpace(errorMessage) Then
                    errorMessage = "SVN did not release the lock."
                End If

                item.ResultText = errorMessage
                row.Cells("Result").Value = errorMessage
                row.Cells("Result").Style.ForeColor = Color.DarkRed
            End If
        Catch ex As Exception
            item.ResultText = ex.Message
            row.Cells("Result").Value = ex.Message
            row.Cells("Result").Style.ForeColor = Color.DarkRed
        Finally
            unlockInProgress = False
            UseWaitCursor = False
            returnButton.Enabled = True
            continueButton.Enabled = True
            UpdateFooter()
        End Try
    End Sub

    Private Sub StartCommitForItem(ByVal row As DataGridViewRow,
                                   ByVal item As CloseLockReviewItem)
        If Not item.CanCommit Then
            row.Cells("Result").Value = "This file has no committable local change"
            Exit Sub
        End If

        Dim errorMessage As String = ""

        unlockInProgress = True
        pendingCommitPath = item.FilePath
        returnButton.Enabled = False
        continueButton.Enabled = False
        grid.Enabled = False
        item.ResultText = "Preparing commit..."
        row.Cells("Result").Value = item.ResultText
        row.Cells("Result").Style.ForeColor = Color.DarkGoldenrod
        grid.Refresh()

        If Not svnModule.commitPathFromCloseReviewPublic(item.FilePath, errorMessage) Then
            row.Cells("Result").Value = If(String.IsNullOrWhiteSpace(errorMessage),
                                            "Commit could not be started",
                                            errorMessage)
            row.Cells("Result").Style.ForeColor = Color.DarkRed
            pendingCommitPath = ""
            unlockInProgress = False
            returnButton.Enabled = True
            continueButton.Enabled = True
            grid.Enabled = True
            Exit Sub
        End If

        item.ResultText = "Commit started"
        row.Cells("Result").Value = "Finish the TortoiseSVN commit; this table will refresh"
    End Sub

    Private Sub CloseReviewCommitCompleted(ByVal committedPaths() As String,
                                           ByVal success As Boolean,
                                           ByVal errorMessage As String)
        If String.IsNullOrWhiteSpace(pendingCommitPath) Then Exit Sub
        If committedPaths Is Nothing OrElse
           Not committedPaths.Any(Function(pathValue) PathsAreSame(pathValue, pendingCommitPath)) Then Exit Sub

        Dim completedPath As String = pendingCommitPath
        Dim row As DataGridViewRow = grid.Rows.Cast(Of DataGridViewRow)().FirstOrDefault(
            Function(candidate)
                Dim candidateItem As CloseLockReviewItem = TryCast(candidate.Tag, CloseLockReviewItem)
                Return candidateItem IsNot Nothing AndAlso PathsAreSame(candidateItem.FilePath, completedPath)
            End Function)

        Dim item As CloseLockReviewItem = If(row Is Nothing, Nothing, TryCast(row.Tag, CloseLockReviewItem))

        Try
            If item Is Nothing Then Exit Try

            If Not success Then
                item.ResultText = If(String.IsNullOrWhiteSpace(errorMessage),
                                     "Commit did not complete",
                                     errorMessage)
                row.Cells("Result").Value = item.ResultText
                row.Cells("Result").Style.ForeColor = Color.DarkRed
                Exit Try
            End If

            Dim querySucceeded As Boolean = False
            Dim refreshError As String = ""
            Dim refreshed As CloseLockReviewItem = svnModule.refreshPathFromCloseReviewPublic(
                completedPath,
                querySucceeded,
                refreshError
            )

            If Not querySucceeded Then
                'The commit itself was verified clean. Be conservative about the lock until
                'a local status refresh proves whether TortoiseSVN released or retained it.
                item.IsSafeToUnlock = True
                item.CanCommit = False
                item.CanRevert = False
                item.StateText = "Committed; lock status needs refresh"
                item.ResultText = refreshError
            ElseIf refreshed Is Nothing Then
                item.IsSafeToUnlock = True
                item.IsStillLocked = False
                item.CanCommit = False
                item.CanRevert = False
                item.StateText = "Committed; lock released"
                item.ResultText = "Committed successfully"
            Else
                item.IsSafeToUnlock = refreshed.IsSafeToUnlock
                item.IsStillLocked = refreshed.IsStillLocked
                item.CanCommit = refreshed.CanCommit
                item.CanRevert = refreshed.CanRevert
                item.StateText = refreshed.StateText
                item.ResultText = If(refreshed.IsSafeToUnlock,
                                     "Committed; lock retained",
                                     refreshed.ResultText)
            End If

            row.Cells("State").Value = item.StateText
            row.Cells("Commit").Value = If(item.CanCommit, "Commit", "—")
            row.Cells("Revert").Value = If(item.CanRevert, "Revert", "—")
            row.Cells("Unlock").Value = If(item.IsStillLocked AndAlso item.IsSafeToUnlock,
                                            "Unlock",
                                            If(item.IsStillLocked, "Return first", "Unlocked"))
            row.Cells("Result").Value = item.ResultText
            ApplyRowVisualState(row, item)
        Finally
            pendingCommitPath = ""
            unlockInProgress = False
            returnButton.Enabled = True
            continueButton.Enabled = True
            grid.Enabled = True
            UpdateFooter()
        End Try
    End Sub

    Private Shared Function PathsAreSame(ByVal firstPath As String,
                                         ByVal secondPath As String) As Boolean
        If String.IsNullOrWhiteSpace(firstPath) OrElse String.IsNullOrWhiteSpace(secondPath) Then Return False

        Try
            Return String.Equals(Path.GetFullPath(firstPath),
                                 Path.GetFullPath(secondPath),
                                 StringComparison.OrdinalIgnoreCase)
        Catch
            Return String.Equals(firstPath, secondPath, StringComparison.OrdinalIgnoreCase)
        End Try
    End Function

    Private Sub RevertItem(ByVal row As DataGridViewRow,
                           ByVal item As CloseLockReviewItem)
        If Not item.CanRevert Then
            row.Cells("Result").Value = "This file has no local change to revert"
            Exit Sub
        End If

        Dim errorMessage As String = ""

        Try
            unlockInProgress = True
            UseWaitCursor = True
            returnButton.Enabled = False
            continueButton.Enabled = False
            row.Cells("Result").Value = "Waiting for revert confirmation..."
            grid.Refresh()

            If svnModule.revertPathFromCloseReviewPublic(item.FilePath, errorMessage) Then
                item.IsSafeToUnlock = True
                item.CanCommit = False
                item.CanRevert = False
                item.StateText = "Reverted; no local changes"
                item.ResultText = "Reverted; lock retained"
                row.Cells("State").Value = item.StateText
                row.Cells("Commit").Value = "—"
                row.Cells("Revert").Value = "—"
                row.Cells("Unlock").Value = "Unlock"
                row.Cells("Result").Value = item.ResultText
                ApplyRowVisualState(row, item)
            ElseIf Not String.IsNullOrWhiteSpace(errorMessage) Then
                row.Cells("Result").Value = errorMessage
                row.Cells("Result").Style.ForeColor = Color.DarkRed
            End If
        Finally
            unlockInProgress = False
            UseWaitCursor = False
            returnButton.Enabled = True
            continueButton.Enabled = True
            UpdateFooter()
        End Try
    End Sub

    Private Sub Grid_CellToolTipTextNeeded(sender As Object, e As DataGridViewCellToolTipTextNeededEventArgs)
        If e.RowIndex < 0 Then Exit Sub

        Dim item As CloseLockReviewItem = TryCast(grid.Rows(e.RowIndex).Tag, CloseLockReviewItem)
        If item Is Nothing Then Exit Sub

        e.ToolTipText = item.FilePath
    End Sub

    Private Sub UpdateFooter()
        Dim stillLocked As Integer = reviewItems.FindAll(Function(item) item.IsStillLocked).Count
        Dim unlocked As Integer = reviewItems.Count - stillLocked
        Dim unsafeCount As Integer = reviewItems.FindAll(Function(item) Not item.IsSafeToUnlock).Count

        footerLabel.Text =
            "Locks remaining: " & stillLocked.ToString() &
            "    Unlocked now: " & unlocked.ToString()

        If unsafeCount > 0 Then
            footerLabel.Text &=
                "    Attention required: " & unsafeCount.ToString() &
                " file(s) still have local SVN changes."
            footerLabel.ForeColor = Color.DarkRed
        Else
            footerLabel.ForeColor = SystemColors.ControlText
        End If
    End Sub

    Private Sub ReturnButton_Click(sender As Object, e As EventArgs)
        If unlockInProgress Then Exit Sub

        _decision = CloseLockReviewDecision.ReturnToSolidWorks
        DialogResult = DialogResult.Cancel
        Close()
    End Sub

    Private Sub ContinueButton_Click(sender As Object, e As EventArgs)
        If unlockInProgress Then Exit Sub

        _decision = CloseLockReviewDecision.ContinueClose
        DialogResult = DialogResult.OK
        Close()
    End Sub

    Private Sub CloseLockReviewForm_FormClosing(sender As Object, e As FormClosingEventArgs)
        If unlockInProgress Then
            e.Cancel = True
            Return
        End If

        If DialogResult <> DialogResult.OK Then
            _decision = CloseLockReviewDecision.ReturnToSolidWorks
        End If
    End Sub

    Private Sub CloseLockReviewForm_FormClosed(sender As Object, e As FormClosedEventArgs)
        RemoveHandler svnModule.CloseReviewCommitCompleted, AddressOf CloseReviewCommitCompleted
    End Sub
End Class
