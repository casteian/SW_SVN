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
    Public Property RequiresLockBeforeCommit As Boolean = False
    Public Property CanGetLock As Boolean = False
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
    Private pendingLockPath As String = ""
    Private pendingCommitPath As String = ""
    Private pendingRevertPath As String = ""

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
    Private Shared ReadOnly LockRequiredRowBackColor As Color = Color.FromArgb(255, 226, 226)
    Private Shared ReadOnly LockRequiredRowForeColor As Color = Color.FromArgb(132, 24, 24)

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
        AddHandler svnModule.CloseReviewRevertCompleted, AddressOf CloseReviewRevertCompleted
        AddHandler svnModule.CloseReviewLockCompleted, AddressOf CloseReviewLockCompleted
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
        'Width must actually fit the sum of the grid's own column MinimumWidths plus the root
        'layout's side padding and the grid's own
        'border/scrollbar margin - otherwise the rightmost columns (Result, Unlock), which
        'carry the messages needed to safely resolve a lock, can be clipped or scrolled away
        'at the form's minimum size.
        MinimumSize = New Size(1090, 430)
        Size = New Size(1240, 560)
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
            "These files need a close decision. Red rows contain edits made without your SVN lock; select Get Lock, then Commit. " &
            "Yellow rows are locked by you and green rows have been successfully unlocked." &
            Environment.NewLine &
            "For each changed file, Commit to keep your work or Revert to discard it and return its lock. Unlock clean files whose work is complete. " &
            "You may retain any lock you still need. Continuing close is the final decision: saved local SVN changes remain on disk, " &
            "but unsaved changes still only in SOLIDWORKS will be discarded without another Save/Don't Save prompt." &
            Environment.NewLine &
            "Commit remains unavailable on a red row unless SVN confirms that the lock was obtained by you."
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

        Dim lockColumn As New DataGridViewButtonColumn()
        lockColumn.Name = "GetLock"
        lockColumn.HeaderText = "Lock"
        lockColumn.UseColumnTextForButtonValue = False
        lockColumn.Width = 95
        lockColumn.MinimumWidth = 95
        grid.Columns.Add(lockColumn)

        Dim commitColumn As New DataGridViewButtonColumn()
        commitColumn.Name = "Commit"
        commitColumn.HeaderText = "Commit"
        commitColumn.UseColumnTextForButtonValue = False
        commitColumn.Width = 90
        commitColumn.MinimumWidth = 90
        grid.Columns.Add(commitColumn)

        Dim revertColumn As New DataGridViewButtonColumn()
        revertColumn.Name = "Revert"
        revertColumn.HeaderText = "Revert"
        revertColumn.UseColumnTextForButtonValue = False
        revertColumn.Width = 90
        revertColumn.MinimumWidth = 90
        grid.Columns.Add(revertColumn)

        Dim unlockColumn As New DataGridViewButtonColumn()
        unlockColumn.Name = "Unlock"
        unlockColumn.HeaderText = "Release"
        unlockColumn.Text = "Unlock"
        unlockColumn.UseColumnTextForButtonValue = False
        unlockColumn.Width = 120
        unlockColumn.MinimumWidth = 120
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
            Dim lockText As String = If(item.CanGetLock, "Get Lock", If(item.RequiresLockBeforeCommit, "Required", "—"))
            Dim commitText As String = If(item.CanCommit, "Commit", "—")
            Dim revertText As String = If(item.CanRevert, "Revert", "—")

            Dim rowIndex As Integer = grid.Rows.Add(
                fileName,
                folderPath,
                item.StateText,
                lockText,
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

        If item.RequiresLockBeforeCommit Then
            rowBackColor = LockRequiredRowBackColor
            rowForeColor = LockRequiredRowForeColor
        ElseIf item.IsStillLocked Then
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

        Try
            Dim lockCell As DataGridViewButtonCell = TryCast(row.Cells("GetLock"), DataGridViewButtonCell)
            If lockCell IsNot Nothing Then
                lockCell.FlatStyle = FlatStyle.Flat
                lockCell.Style.BackColor = rowBackColor
                lockCell.Style.ForeColor = rowForeColor
                lockCell.Style.SelectionBackColor = rowBackColor
                lockCell.Style.SelectionForeColor = rowForeColor
            End If
        Catch
        End Try

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

        If e.ColumnIndex = grid.Columns("GetLock").Index Then
            StartGetLockForItem(row, item)
            Exit Sub
        End If

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

    Private Sub StartGetLockForItem(ByVal row As DataGridViewRow,
                                    ByVal item As CloseLockReviewItem)
        If Not item.RequiresLockBeforeCommit OrElse Not item.CanGetLock Then
            row.Cells("Result").Value = If(item.IsStillLocked,
                                            "You already own this lock",
                                            "This row does not require Get Lock")
            Exit Sub
        End If

        Dim errorMessage As String = ""

        unlockInProgress = True
        pendingLockPath = item.FilePath
        returnButton.Enabled = False
        continueButton.Enabled = False
        grid.Enabled = False
        item.ResultText = "Getting SVN lock..."
        row.Cells("Result").Value = item.ResultText
        row.Cells("Result").Style.ForeColor = Color.DarkGoldenrod
        grid.Refresh()

        If Not svnModule.lockPathFromCloseReviewPublic(item.FilePath, errorMessage) Then
            item.ResultText = If(String.IsNullOrWhiteSpace(errorMessage),
                                 "Get Lock could not be started",
                                 errorMessage)
            row.Cells("Result").Value = item.ResultText
            row.Cells("Result").Style.ForeColor = Color.DarkRed
            pendingLockPath = ""
            unlockInProgress = False
            returnButton.Enabled = True
            continueButton.Enabled = True
            grid.Enabled = True
            UpdateFooter()
        End If
    End Sub

    Private Sub CloseReviewLockCompleted(ByVal lockedPath As String,
                                         ByVal success As Boolean,
                                         ByVal errorMessage As String)
        If String.IsNullOrWhiteSpace(pendingLockPath) Then Exit Sub
        If Not PathsAreSame(lockedPath, pendingLockPath) Then Exit Sub

        Dim completedPath As String = pendingLockPath
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
                                     "SVN did not obtain the lock. Another user may already hold it.",
                                     errorMessage)
                row.Cells("Result").Value = item.ResultText
                row.Cells("Result").Style.ForeColor = Color.DarkRed
                Exit Try
            End If

            item.RequiresLockBeforeCommit = False
            item.CanGetLock = False
            item.IsStillLocked = True
            item.IsSafeToUnlock = False
            item.CanCommit = True
            item.CanRevert = False
            item.StateText = "Locked; edits need commit"
            item.ResultText = "Lock obtained. Select Commit."

            row.Cells("State").Value = item.StateText
            row.Cells("GetLock").Value = "Locked"
            row.Cells("Commit").Value = "Commit"
            row.Cells("Revert").Value = "—"
            row.Cells("Unlock").Value = "Return first"
            row.Cells("Result").Value = item.ResultText
            ApplyRowVisualState(row, item)
        Finally
            pendingLockPath = ""
            unlockInProgress = False
            returnButton.Enabled = True
            continueButton.Enabled = True
            grid.Enabled = True
            UpdateFooter()
        End Try
    End Sub

    Private Sub StartCommitForItem(ByVal row As DataGridViewRow,
                                   ByVal item As CloseLockReviewItem)
        If Not item.CanCommit Then
            row.Cells("Result").Value = If(item.RequiresLockBeforeCommit,
                                            "Select Get Lock first; Commit is disabled until SVN confirms the lock",
                                            "This file has no committable local change")
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
            row.Cells("GetLock").Value = If(item.CanGetLock, "Get Lock", "—")
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

        unlockInProgress = True
        pendingRevertPath = item.FilePath
        returnButton.Enabled = False
        continueButton.Enabled = False
        grid.Enabled = False
        row.Cells("Result").Value = "Waiting for revert confirmation..."
        grid.Refresh()

        If svnModule.revertPathFromCloseReviewPublic(item.FilePath, errorMessage) Then
            UseWaitCursor = True
            row.Cells("Result").Value = "Reverting changes, reloading the SOLIDWORKS file, then returning its lock..."
            Exit Sub
        End If

        Try
            If Not String.IsNullOrWhiteSpace(errorMessage) Then
                item.ResultText = errorMessage
                row.Cells("Result").Value = errorMessage
                row.Cells("Result").Style.ForeColor = If(errorMessage.StartsWith("Revert cancelled", StringComparison.OrdinalIgnoreCase),
                                                           Color.DarkGoldenrod,
                                                           Color.DarkRed)
            End If
        Finally
            pendingRevertPath = ""
            unlockInProgress = False
            UseWaitCursor = False
            returnButton.Enabled = True
            continueButton.Enabled = True
            grid.Enabled = True
            UpdateFooter()
        End Try
    End Sub

    Private Sub CloseReviewRevertCompleted(ByVal revertedPath As String,
                                           ByVal success As Boolean,
                                           ByVal errorMessage As String)
        If String.IsNullOrWhiteSpace(pendingRevertPath) Then Exit Sub
        If Not PathsAreSame(revertedPath, pendingRevertPath) Then Exit Sub

        Dim completedPath As String = pendingRevertPath
        Dim row As DataGridViewRow = grid.Rows.Cast(Of DataGridViewRow)().FirstOrDefault(
            Function(candidate)
                Dim candidateItem As CloseLockReviewItem = TryCast(candidate.Tag, CloseLockReviewItem)
                Return candidateItem IsNot Nothing AndAlso PathsAreSame(candidateItem.FilePath, completedPath)
            End Function)
        Dim item As CloseLockReviewItem = If(row Is Nothing, Nothing, TryCast(row.Tag, CloseLockReviewItem))

        Try
            If item Is Nothing Then Exit Try

            If Not success Then
                'A discard can fail after changing one layer (for example SVN reverted the
                'disk file but SOLIDWORKS could not reload it). Never turn that partial result
                'into permission to release the lock. Keep the original actions available for
                'a deliberate retry, but require the user to return to the document first.
                item.IsSafeToUnlock = False
                item.ResultText = If(String.IsNullOrWhiteSpace(errorMessage),
                                     "Revert did not complete; no lock return was attempted",
                                     errorMessage)
                row.Cells("Unlock").Value = If(item.IsStillLocked, "Return first", "Unlocked")
                row.Cells("Result").Value = item.ResultText
                row.Cells("Result").Style.ForeColor = Color.DarkRed
                ApplyRowVisualState(row, item)
                Exit Try
            End If

            Dim lockWasHeld As Boolean = item.IsStillLocked
            Dim lockReturned As Boolean = Not lockWasHeld
            Dim returnLockError As String = ""

            If lockWasHeld Then
                lockReturned = svnModule.unlockPathFromCloseReviewPublic(completedPath, returnLockError)
            End If

            If lockReturned Then
                item.RequiresLockBeforeCommit = False
                item.CanGetLock = False
                item.IsSafeToUnlock = True
                item.IsStillLocked = False
                item.CanCommit = False
                item.CanRevert = False
                item.StateText = If(lockWasHeld,
                                    "Reverted; lock returned",
                                    "Reverted; no lock held")
                item.ResultText = If(lockWasHeld,
                                     "Reverted and lock returned",
                                     "Reverted successfully; no lock was held")
            Else
                'The destructive half has already completed and was verified clean. If the
                'network/SVN unlock fails, refresh only this path so the table never claims a
                'lock was returned when it is still held (or vice versa).
                Dim querySucceeded As Boolean = False
                Dim refreshError As String = ""
                Dim refreshed As CloseLockReviewItem = svnModule.refreshPathFromCloseReviewPublic(
                    completedPath,
                    querySucceeded,
                    refreshError
                )

                If querySucceeded AndAlso refreshed Is Nothing Then
                    item.RequiresLockBeforeCommit = False
                    item.CanGetLock = False
                    item.IsSafeToUnlock = True
                    item.IsStillLocked = False
                    item.CanCommit = False
                    item.CanRevert = False
                    item.StateText = "Reverted; lock returned"
                    item.ResultText = "Reverted and lock returned"
                ElseIf querySucceeded Then
                    item.RequiresLockBeforeCommit = False
                    item.CanGetLock = False
                    item.IsSafeToUnlock = refreshed.IsSafeToUnlock
                    item.IsStillLocked = refreshed.IsStillLocked
                    item.CanCommit = False
                    item.CanRevert = False
                    item.StateText = "Reverted; lock retained"
                    item.ResultText = "Reverted, but the lock could not be returned"
                    If Not String.IsNullOrWhiteSpace(returnLockError) Then
                        item.ResultText &= ": " & returnLockError
                    End If
                Else
                    'Revert succeeded, but neither the unlock nor the follow-up status check
                    'proved the lock state. Keep the original lock state and fail closed.
                    item.IsSafeToUnlock = False
                    item.CanCommit = False
                    item.CanRevert = False
                    item.StateText = "Reverted; lock return needs verification"
                    item.ResultText = "Reverted, but PlumVault could not verify that the lock was returned"

                    If Not String.IsNullOrWhiteSpace(returnLockError) Then
                        item.ResultText &= ": " & returnLockError
                    ElseIf Not String.IsNullOrWhiteSpace(refreshError) Then
                        item.ResultText &= ": " & refreshError
                    End If
                End If
            End If

            row.Cells("State").Value = item.StateText
            row.Cells("GetLock").Value = If(item.CanGetLock, "Get Lock", "—")
            row.Cells("Commit").Value = If(item.CanCommit, "Commit", "—")
            row.Cells("Revert").Value = If(item.CanRevert, "Revert", "—")
            row.Cells("Unlock").Value = If(item.IsStillLocked AndAlso item.IsSafeToUnlock,
                                            "Unlock",
                                            If(item.IsStillLocked, "Return first", "Unlocked"))
            row.Cells("Result").Value = item.ResultText
            ApplyRowVisualState(row, item)
        Finally
            pendingRevertPath = ""
            unlockInProgress = False
            UseWaitCursor = False
            returnButton.Enabled = True
            continueButton.Enabled = True
            grid.Enabled = True
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
        Dim lockRequired As Integer = reviewItems.FindAll(Function(item) item.RequiresLockBeforeCommit).Count
        Dim unlocked As Integer = reviewItems.Count - stillLocked - lockRequired
        Dim unsafeCount As Integer = reviewItems.FindAll(Function(item) Not item.IsSafeToUnlock).Count

        footerLabel.Text =
            "Locks remaining: " & stillLocked.ToString() &
            "    Lock required: " & lockRequired.ToString() &
            "    Unlocked now: " & unlocked.ToString()

        If unsafeCount > 0 Then
            footerLabel.Text &=
                "    Attention required: " & unsafeCount.ToString() &
                " file(s) still have unsaved or local changes."
            footerLabel.ForeColor = Color.DarkRed

            continueButton.Text = If(closingSolidWorks,
                                     "Discard remaining changes and close SOLIDWORKS",
                                     "Discard remaining changes and close file")
        Else
            footerLabel.ForeColor = SystemColors.ControlText
            continueButton.Text = If(closingSolidWorks,
                                     "Close SOLIDWORKS — no further saves",
                                     "Close file — no further saves")
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

        Dim unsafeCount As Integer = reviewItems.FindAll(Function(item) Not item.IsSafeToUnlock).Count

        If unsafeCount > 0 Then
            Dim targetName As String = If(closingSolidWorks, "SOLIDWORKS", "this file")
            Dim response As DialogResult = MessageBox.Show(
                Me,
                unsafeCount.ToString() & " file(s) still have unsaved or local changes." & Environment.NewLine & Environment.NewLine &
                "Close " & targetName & " and discard any changes that exist only in SOLIDWORKS? " &
                "Saved local SVN changes will remain on disk for the next session.",
                "Discard remaining changes?",
                MessageBoxButtons.YesNo,
                MessageBoxIcon.Warning,
                MessageBoxDefaultButton.Button2
            )

            If response <> DialogResult.Yes Then Return
        End If

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
        RemoveHandler svnModule.CloseReviewRevertCompleted, AddressOf CloseReviewRevertCompleted
        RemoveHandler svnModule.CloseReviewLockCompleted, AddressOf CloseReviewLockCompleted
    End Sub
End Class
