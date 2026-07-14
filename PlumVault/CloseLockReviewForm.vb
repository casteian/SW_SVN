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
    Public Property StateText As String = "Saved and committed"
    Public Property IsStillLocked As Boolean = True
    Public Property ResultText As String = "Lock retained"
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

        InitializeWindow()
        PopulateRows()
        UpdateFooter()
    End Sub

    Private Sub InitializeWindow()
        Text = If(closingSolidWorks,
                  "Review SVN Locks Before Closing SOLIDWORKS",
                  "Review SVN Locks Before Closing File")

        StartPosition = FormStartPosition.CenterScreen
        MinimumSize = New Size(920, 430)
        Size = New Size(1120, 560)
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
            "Verify whether you still need each lock. It is recommended to unlock a file once your edits are complete. " &
            "You may retain any lock you still need and continue closing." &
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
        continueButton.MinimumSize = New Size(220, 38)
        continueButton.Text = If(closingSolidWorks,
                                 "Continue closing SOLIDWORKS",
                                 "Continue closing file")
        AddHandler continueButton.Click, AddressOf ContinueButton_Click
        buttonPanel.Controls.Add(continueButton, 2, 0)

        AcceptButton = continueButton
        CancelButton = returnButton

        AddHandler FormClosing, AddressOf CloseLockReviewForm_FormClosing
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
        fileNameColumn.Width = 240
        fileNameColumn.MinimumWidth = 150
        grid.Columns.Add(fileNameColumn)

        Dim locationColumn As New DataGridViewTextBoxColumn()
        locationColumn.Name = "Location"
        locationColumn.HeaderText = "Location"
        locationColumn.AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
        locationColumn.MinimumWidth = 280
        grid.Columns.Add(locationColumn)

        Dim stateColumn As New DataGridViewTextBoxColumn()
        stateColumn.Name = "State"
        stateColumn.HeaderText = "SVN state"
        stateColumn.Width = 170
        grid.Columns.Add(stateColumn)

        Dim unlockColumn As New DataGridViewButtonColumn()
        unlockColumn.Name = "Unlock"
        unlockColumn.HeaderText = "Action"
        unlockColumn.Text = "Unlock"
        unlockColumn.UseColumnTextForButtonValue = False
        unlockColumn.Width = 100
        grid.Columns.Add(unlockColumn)

        Dim resultColumn As New DataGridViewTextBoxColumn()
        resultColumn.Name = "Result"
        resultColumn.HeaderText = "Result"
        resultColumn.Width = 220
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

            Dim rowIndex As Integer = grid.Rows.Add(
                fileName,
                folderPath,
                item.StateText,
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
        If e.ColumnIndex <> grid.Columns("Unlock").Index Then Exit Sub

        Dim row As DataGridViewRow = grid.Rows(e.RowIndex)
        Dim item As CloseLockReviewItem = TryCast(row.Tag, CloseLockReviewItem)
        If item Is Nothing Then Exit Sub

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
End Class
