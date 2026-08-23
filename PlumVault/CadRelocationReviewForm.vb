Imports System.Collections.Generic
Imports System.Drawing
Imports System.IO
Imports System.Linq
Imports System.Windows.Forms

Public Enum CadRelocationMode
    ReId = 0
    Move = 1
    NewSave = 2
End Enum

Public Class CadRelocationReviewRow
    Public Property FilePath As String = ""
    Public Property RoleText As String = ""
    Public Property StateText As String = ""
    Public Property CheckText As String = ""
    Public Property Explanation As String = ""
    Public Property IsReady As Boolean = False
End Class

Public Class CadRelocationCheckResult
    Public Property CanProceed As Boolean = False
    Public Property Summary As String = ""
    Public Property Rows As New List(Of CadRelocationReviewRow)()
    Public Property DirectReferrerPaths As String() = Nothing
    Public Property OpenPathsToReopen As String() = Nothing
    Public Property ActivePathBeforeOperation As String = ""
End Class

Public Class CadRelocationReviewForm
    Inherits Form

    Private ReadOnly sourcePath As String
    Private ReadOnly repoRoot As String
    Private ReadOnly mode As CadRelocationMode
    Private ReadOnly checker As Func(Of String, CadRelocationCheckResult)

    Private ReadOnly nameTextBox As New TextBox()
    Private ReadOnly folderTextBox As New TextBox()
    Private ReadOnly browseButton As New Button()
    Private ReadOnly checkButton As New Button()
    Private ReadOnly grid As New DataGridView()
    Private ReadOnly summaryLabel As New Label()
    Private ReadOnly cancelButtonControl As New Button()
    Private ReadOnly continueButton As New Button()

    Public Property ApprovedDestinationPath As String = ""
    Public Property Approved As Boolean = False

    Public Sub New(ByVal sourceFilePath As String,
                   ByVal workingCopyRoot As String,
                   ByVal operationMode As CadRelocationMode,
                   ByVal checkCallback As Func(Of String, CadRelocationCheckResult))
        sourcePath = sourceFilePath
        repoRoot = workingCopyRoot
        mode = operationMode
        checker = checkCallback

        buildUi()
    End Sub

    Private Sub buildUi()
        Text = formTitleForMode()
        StartPosition = FormStartPosition.CenterParent
        MinimumSize = New Size(1050, 610)
        Size = New Size(1180, 720)
        Font = New Font("Segoe UI", 9.0F)

        Dim main As New TableLayoutPanel() With {
            .Dock = DockStyle.Fill,
            .ColumnCount = 1,
            .RowCount = 5,
            .Padding = New Padding(14)
        }
        main.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        main.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        main.RowStyles.Add(New RowStyle(SizeType.Percent, 100.0F))
        main.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        main.RowStyles.Add(New RowStyle(SizeType.AutoSize))

        Dim header As New Label() With {
            .AutoSize = True,
            .MaximumSize = New Size(1120, 0),
            .Font = New Font("Segoe UI", 11.0F, FontStyle.Bold),
            .Text = headerTextForMode()
        }
        main.Controls.Add(header, 0, 0)

        Dim fields As New TableLayoutPanel() With {
            .AutoSize = True,
            .Dock = DockStyle.Top,
            .ColumnCount = 4,
            .Padding = New Padding(0, 12, 0, 10)
        }
        fields.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))
        fields.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 38.0F))
        fields.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 62.0F))
        fields.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))

        fields.Controls.Add(New Label() With {.Text = "File name", .AutoSize = True, .Anchor = AnchorStyles.Left}, 0, 0)
        nameTextBox.Dock = DockStyle.Fill
        nameTextBox.Text = Path.GetFileNameWithoutExtension(sourcePath)
        nameTextBox.ReadOnly = (mode = CadRelocationMode.Move)
        fields.Controls.Add(nameTextBox, 1, 0)
        fields.SetColumnSpan(nameTextBox, 2)

        fields.Controls.Add(New Label() With {.Text = "Destination folder", .AutoSize = True, .Anchor = AnchorStyles.Left}, 0, 1)
        folderTextBox.Dock = DockStyle.Fill
        folderTextBox.Text = Path.GetDirectoryName(sourcePath)
        fields.SetColumnSpan(folderTextBox, 2)
        fields.Controls.Add(folderTextBox, 1, 1)

        browseButton.Text = "Browse..."
        browseButton.AutoSize = True
        fields.Controls.Add(browseButton, 3, 1)

        checkButton.Text = "Check"
        checkButton.AutoSize = True
        checkButton.Font = New Font("Segoe UI", 9.0F, FontStyle.Bold)
        fields.Controls.Add(checkButton, 3, 0)
        main.Controls.Add(fields, 0, 1)

        configureGrid()
        main.Controls.Add(grid, 0, 2)

        summaryLabel.AutoSize = True
        summaryLabel.MaximumSize = New Size(1120, 0)
        summaryLabel.Padding = New Padding(0, 8, 0, 8)
        summaryLabel.Text = "No files have been changed. Click Check to review the operation."
        main.Controls.Add(summaryLabel, 0, 3)

        Dim buttons As New FlowLayoutPanel() With {
            .AutoSize = True,
            .Dock = DockStyle.Fill,
            .FlowDirection = FlowDirection.RightToLeft,
            .WrapContents = False
        }
        cancelButtonControl.Text = "Cancel"
        cancelButtonControl.AutoSize = True
        cancelButtonControl.DialogResult = DialogResult.Cancel
        continueButton.Text = continueButtonTextForMode()
        continueButton.AutoSize = True
        continueButton.Enabled = False
        buttons.Controls.Add(cancelButtonControl)
        buttons.Controls.Add(continueButton)
        main.Controls.Add(buttons, 0, 4)

        Controls.Add(main)
        CancelButton = cancelButtonControl

        AddHandler browseButton.Click, AddressOf browseButton_Click
        AddHandler checkButton.Click, AddressOf checkButton_Click
        AddHandler continueButton.Click, AddressOf continueButton_Click
        AddHandler nameTextBox.TextChanged, AddressOf inputChanged
        AddHandler folderTextBox.TextChanged, AddressOf inputChanged
    End Sub

    Private Function formTitleForMode() As String
        If mode = CadRelocationMode.ReId Then Return "Re-ID CAD File"
        If mode = CadRelocationMode.Move Then Return "Move CAD File"
        Return "Save New CAD File"
    End Function

    Private Function headerTextForMode() As String
        Dim instruction As String

        If mode = CadRelocationMode.ReId Then
            instruction = "Choose the new ID and optional location, then click Check."
        ElseIf mode = CadRelocationMode.Move Then
            instruction = "Choose the new location, then click Check."
        Else
            instruction = "Choose a GRC27/CFD27-compliant name and destination, then click Check."
        End If

        Dim detail As String = If(
            mode = CadRelocationMode.NewSave,
            "PlumVault checks the naming convention, destination, and required lock before saving.",
            "PlumVault checks SVN locks and every assembly/drawing reference before changing files."
        )

        Return instruction & Environment.NewLine & detail
    End Function

    Private Function continueButtonTextForMode() As String
        If mode = CadRelocationMode.ReId Then Return "Re-ID and commit"
        If mode = CadRelocationMode.Move Then Return "Move and commit"
        Return "Save"
    End Function

    Private Sub configureGrid()
        grid.Dock = DockStyle.Fill
        grid.ReadOnly = True
        grid.AllowUserToAddRows = False
        grid.AllowUserToDeleteRows = False
        grid.AllowUserToResizeRows = True
        grid.AutoGenerateColumns = False
        grid.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
        grid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
        grid.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        grid.RowHeadersVisible = False
        grid.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        grid.MultiSelect = False
        grid.BackgroundColor = SystemColors.Window
        grid.DefaultCellStyle.WrapMode = DataGridViewTriState.True

        grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .HeaderText = "File",
            .Name = "File",
            .Width = 260,
            .MinimumWidth = 180,
            .FillWeight = 25.0F
        })
        grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .HeaderText = "Role",
            .Name = "Role",
            .Width = 130,
            .MinimumWidth = 100,
            .FillWeight = 12.0F
        })
        grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .HeaderText = "State",
            .Name = "State",
            .Width = 155,
            .MinimumWidth = 120,
            .FillWeight = 14.0F
        })
        grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .HeaderText = "Check",
            .Name = "Check",
            .Width = 90,
            .MinimumWidth = 75,
            .FillWeight = 9.0F
        })
        grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .HeaderText = "Explanation",
            .Name = "Explanation",
            .AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill,
            .MinimumWidth = 280,
            .FillWeight = 40.0F
        })
    End Sub

    Private Sub inputChanged(ByVal sender As Object, ByVal e As EventArgs)
        continueButton.Enabled = False
        ApprovedDestinationPath = ""
        summaryLabel.Text = "Inputs changed. Click Check again before continuing."
    End Sub

    Private Function proposedDestinationPath() As String
        Dim extension As String = Path.GetExtension(sourcePath)
        Dim proposedName As String = nameTextBox.Text.Trim()

        If proposedName.EndsWith(extension, StringComparison.OrdinalIgnoreCase) Then
            proposedName = Path.GetFileNameWithoutExtension(proposedName)
        End If

        If String.IsNullOrWhiteSpace(proposedName) OrElse String.IsNullOrWhiteSpace(folderTextBox.Text) Then Return ""

        Try
            Return Path.GetFullPath(Path.Combine(folderTextBox.Text.Trim(), proposedName & extension))
        Catch
            Return ""
        End Try
    End Function

    Private Sub browseButton_Click(ByVal sender As Object, ByVal e As EventArgs)
        Using picker As New FolderBrowserDialog()
            picker.Description = "Choose an existing folder inside the SVN working copy"
            picker.ShowNewFolderButton = False
            picker.SelectedPath = If(Directory.Exists(folderTextBox.Text), folderTextBox.Text, repoRoot)

            If picker.ShowDialog(Me) = DialogResult.OK Then folderTextBox.Text = picker.SelectedPath
        End Using
    End Sub

    Private Sub checkButton_Click(ByVal sender As Object, ByVal e As EventArgs)
        continueButton.Enabled = False
        grid.Rows.Clear()

        Dim destinationPath As String = proposedDestinationPath()
        If String.IsNullOrWhiteSpace(destinationPath) Then
            summaryLabel.Text = "Enter a valid file name and destination folder."
            Return
        End If

        If checker Is Nothing Then Return

        Cursor = Cursors.WaitCursor
        checkButton.Enabled = False

        Try
            Dim result As CadRelocationCheckResult = checker(destinationPath)
            If result Is Nothing Then
                summaryLabel.Text = "The check did not return a result. No files were changed."
                Return
            End If

            For Each item As CadRelocationReviewRow In result.Rows
                Dim index As Integer = grid.Rows.Add(
                    Path.GetFileName(item.FilePath),
                    item.RoleText,
                    item.StateText,
                    item.CheckText,
                    item.Explanation
                )
                grid.Rows(index).Tag = item.FilePath

                'Selection color must match the row's state color, not the WinForms default
                'blue - otherwise selecting/clicking a row (which DataGridView does on any
                'cell click) hides the ready/not-ready state the color is there to show.
                'ForeColor is explicit (Color.Black), not read back from DefaultCellStyle -
                'that property is never set elsewhere on this grid, so reading it back returns
                'Color.Empty, which rendered as near-invisible text once used as SelectionForeColor.
                If Not item.IsReady Then
                    grid.Rows(index).DefaultCellStyle.BackColor = Color.MistyRose
                    grid.Rows(index).DefaultCellStyle.SelectionBackColor = Color.MistyRose
                    grid.Rows(index).DefaultCellStyle.ForeColor = Color.Black
                    grid.Rows(index).DefaultCellStyle.SelectionForeColor = Color.Black
                Else
                    grid.Rows(index).DefaultCellStyle.BackColor = Color.Honeydew
                    grid.Rows(index).DefaultCellStyle.SelectionBackColor = Color.Honeydew
                    grid.Rows(index).DefaultCellStyle.ForeColor = Color.Black
                    grid.Rows(index).DefaultCellStyle.SelectionForeColor = Color.Black
                End If
            Next

            summaryLabel.Text = result.Summary
            ApprovedDestinationPath = If(result.CanProceed, destinationPath, "")
            continueButton.Enabled = result.CanProceed
        Catch ex As Exception
            summaryLabel.Text = "Check failed: " & ex.Message & " No files were changed."
        Finally
            checkButton.Enabled = True
            Cursor = Cursors.Default
        End Try
    End Sub

    Private Sub continueButton_Click(ByVal sender As Object, ByVal e As EventArgs)
        If String.IsNullOrWhiteSpace(ApprovedDestinationPath) Then Return
        Approved = True
        DialogResult = DialogResult.OK
        Close()
    End Sub
End Class
