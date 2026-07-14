Imports System
Imports System.Collections.Generic
Imports System.Drawing
Imports System.IO
Imports System.Linq
Imports System.Windows.Forms

Public Enum LegacyImportSourceType
    Assembly
    Part
    Drawing
End Enum

Public Enum LegacyImportTargetType
    Assembly
    Part
    VendorPart
    Drawing
End Enum

Public Class LegacyImportItem
    Public Property SourcePath As String
    Public Property SourceType As LegacyImportSourceType
    Public Property TargetType As LegacyImportTargetType
    Public Property ProposedId As String
    Public Property FinalFileName As String
    Public Property DestinationPath As String
    Public Property IsChecked As Boolean
    Public Property IsValid As Boolean
    Public Property ValidationMessage As String
    Public Property IsVirtualComponent As Boolean

    Public ReadOnly Property OriginalFileName As String
        Get
            If String.IsNullOrWhiteSpace(SourcePath) Then Return ""
            Try
                Return Path.GetFileName(SourcePath)
            Catch
                Return SourcePath
            End Try
        End Get
    End Property

    Public ReadOnly Property Extension As String
        Get
            If String.IsNullOrWhiteSpace(SourcePath) Then Return ""
            Try
                Return Path.GetExtension(SourcePath).ToUpperInvariant()
            Catch
                Return ""
            End Try
        End Get
    End Property
End Class

Public Class LegacyImportPlan
    Public Property SourceTopAssemblyPath As String
    Public Property PackAndGoSourceNames As String()
    Public Property Items As List(Of LegacyImportItem)
    Public Property GrcDestinationFolder As String
    Public Property VendorDestinationFolder As String
    Public Property LocalRepoRootFolder As String
    Public Property GrcRootFolder As String
    Public Property VendorRootFolder As String
    Public Property ExistingRepoFileNames As HashSet(Of String)
    Public Property ExistingRepoModelIds As HashSet(Of String)

    Public Sub New()
        Items = New List(Of LegacyImportItem)()
        ExistingRepoFileNames = New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        ExistingRepoModelIds = New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
    End Sub
End Class

Public Class LegacyImportValidationResult
    Public Property IsValid As Boolean
    Public Property FinalFileName As String
    Public Property DestinationPath As String
    Public Property Message As String
End Class

Public Class LegacyImportForm
    Inherits Form

    Private ReadOnly _plan As LegacyImportPlan

    Private ReadOnly _grid As New DataGridView()
    Private ReadOnly _txtGrcFolder As New TextBox()
    Private ReadOnly _txtVendorFolder As New TextBox()
    Private ReadOnly _lblSummary As New Label()
    Private ReadOnly _lblInstruction As New Label()
    Private ReadOnly _lblWorkingCopy As New Label()
    Private ReadOnly _btnContinue As New Button()
    Private ReadOnly _btnCancel As New Button()
    Private ReadOnly _btnCheckAll As New Button()
    Private ReadOnly _btnNextProblem As New Button()
    Private ReadOnly _btnBrowseGrc As New Button()
    Private ReadOnly _btnBrowseVendor As New Button()

    Private _suppressGridEvents As Boolean = False

    Private Const COL_OLD_NAME As String = "OldFileName"
    Private Const COL_ORIGINAL As String = "OriginalPath"
    Private Const COL_SOURCE_TYPE As String = "SourceType"
    Private Const COL_TARGET_TYPE As String = "TargetType"
    Private Const COL_NEW_ID As String = "NewId"
    Private Const COL_FINAL_NAME As String = "FinalName"
    Private Const COL_CHECK As String = "CheckRow"
    Private Const COL_STATUS As String = "Status"
    Private Const COL_EXPLANATION As String = "Explanation"

    Public Sub New(ByVal plan As LegacyImportPlan)
        If plan Is Nothing Then Throw New ArgumentNullException(NameOf(plan))

        _plan = plan

        Me.Text = "Copy Legacy Data to SVN"
        Me.StartPosition = FormStartPosition.CenterParent
        Me.MinimumSize = New Size(1050, 620)
        Me.Size = New Size(1320, 760)
        Me.AutoScaleMode = AutoScaleMode.Dpi
        Me.Font = SystemFonts.MessageBoxFont
        Me.ShowIcon = False
        Me.ShowInTaskbar = False

        buildLayout()
        populateRows()
        updateSummaryAndContinueState()
    End Sub

    Private Sub buildLayout()
        Dim root As New TableLayoutPanel()
        root.Dock = DockStyle.Fill
        root.Padding = New Padding(10)
        root.ColumnCount = 1
        root.RowCount = 5
        root.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        root.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        root.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        root.RowStyles.Add(New RowStyle(SizeType.Percent, 100.0F))
        root.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        Me.Controls.Add(root)

        _lblInstruction.AutoSize = True
        _lblInstruction.MaximumSize = New Size(1220, 0)
        _lblInstruction.Text =
            "The SVN destination folders were selected, created, and committed before this table opened. " &
            "Assign a valid GRC27/CFD27 ID to every assembly, part, and drawing. " &
            "Only source PART rows may be changed to VENDOR PART. " &
            "Press Check on each row, or use Check All. Continue remains disabled until every row passes."
        _lblInstruction.Padding = New Padding(0, 0, 0, 4)
        root.Controls.Add(_lblInstruction, 0, 0)

        _lblWorkingCopy.AutoSize = True
        _lblWorkingCopy.Padding = New Padding(0, 0, 0, 8)
        _lblWorkingCopy.Font = New Font(SystemFonts.MessageBoxFont, FontStyle.Bold)
        _lblWorkingCopy.Text = "SVN working copy: " & If(_plan.LocalRepoRootFolder, "")
        root.Controls.Add(_lblWorkingCopy, 0, 1)

        Dim destinations As New TableLayoutPanel()
        destinations.AutoSize = True
        destinations.Dock = DockStyle.Top
        destinations.ColumnCount = 3
        destinations.RowCount = 2
        destinations.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))
        destinations.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 100.0F))
        destinations.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))

        Dim lblGrc As New Label() With {
            .Text = "Selected import destination:",
            .AutoSize = True,
            .Anchor = AnchorStyles.Left,
            .Margin = New Padding(0, 5, 8, 5)
        }
        Dim lblVendor As New Label() With {
            .Text = "Automatic vendor destination:",
            .AutoSize = True,
            .Anchor = AnchorStyles.Left,
            .Margin = New Padding(0, 5, 8, 5)
        }

        _txtGrcFolder.Dock = DockStyle.Fill
        _txtGrcFolder.Text = If(_plan.GrcDestinationFolder, "")
        _txtGrcFolder.ReadOnly = True
        _txtGrcFolder.BackColor = SystemColors.Window

        _txtVendorFolder.Dock = DockStyle.Fill
        _txtVendorFolder.Text = If(_plan.VendorDestinationFolder, "")
        _txtVendorFolder.ReadOnly = True
        _txtVendorFolder.BackColor = SystemColors.Window

        'The normal destination is deliberately chosen before the assembly scan.
        'The vendor destination is automatically the working-copy Vendor Parts folder.
        'Keep the old controls hidden so this remains compatible with the existing layout code.
        _btnBrowseGrc.Text = "Browse..."
        _btnBrowseGrc.AutoSize = True
        _btnBrowseGrc.Visible = False
        _btnBrowseGrc.Enabled = False
        _btnBrowseGrc.TabStop = False

        _btnBrowseVendor.Text = "Browse..."
        _btnBrowseVendor.AutoSize = True
        _btnBrowseVendor.Visible = False
        _btnBrowseVendor.Enabled = False
        _btnBrowseVendor.TabStop = False

        destinations.Controls.Add(lblGrc, 0, 0)
        destinations.Controls.Add(_txtGrcFolder, 1, 0)
        destinations.Controls.Add(_btnBrowseGrc, 2, 0)
        destinations.Controls.Add(lblVendor, 0, 1)
        destinations.Controls.Add(_txtVendorFolder, 1, 1)
        destinations.Controls.Add(_btnBrowseVendor, 2, 1)

        root.Controls.Add(destinations, 0, 2)

        configureGrid()
        root.Controls.Add(_grid, 0, 3)

        Dim bottom As New TableLayoutPanel()
        bottom.AutoSize = True
        bottom.Dock = DockStyle.Fill
        bottom.ColumnCount = 2
        bottom.RowCount = 1
        bottom.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 100.0F))
        bottom.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))

        _lblSummary.AutoSize = True
        _lblSummary.Anchor = AnchorStyles.Left
        _lblSummary.Padding = New Padding(0, 8, 0, 0)
        bottom.Controls.Add(_lblSummary, 0, 0)

        Dim actions As New FlowLayoutPanel()
        actions.AutoSize = True
        actions.FlowDirection = FlowDirection.LeftToRight
        actions.WrapContents = False
        actions.Anchor = AnchorStyles.Right
        actions.Padding = New Padding(0, 8, 0, 0)

        _btnCheckAll.Text = "Check All"
        _btnCheckAll.AutoSize = True
        _btnNextProblem.Text = "Next Problem"
        _btnNextProblem.AutoSize = True
        _btnCancel.Text = "Cancel"
        _btnCancel.AutoSize = True
        _btnContinue.Text = "Continue"
        _btnContinue.AutoSize = True
        _btnContinue.Enabled = False

        actions.Controls.Add(_btnCheckAll)
        actions.Controls.Add(_btnNextProblem)
        actions.Controls.Add(_btnCancel)
        actions.Controls.Add(_btnContinue)
        bottom.Controls.Add(actions, 1, 0)

        root.Controls.Add(bottom, 0, 4)

        AddHandler _btnCheckAll.Click, AddressOf checkAllRows
        AddHandler _btnNextProblem.Click, AddressOf selectNextProblem
        AddHandler _btnCancel.Click, AddressOf cancelClicked
        AddHandler _btnContinue.Click, AddressOf continueClicked
    End Sub

    Private Sub configureGrid()
        _grid.Dock = DockStyle.Fill
        _grid.AllowUserToAddRows = False
        _grid.AllowUserToDeleteRows = False
        _grid.AllowUserToOrderColumns = False
        _grid.AllowUserToResizeRows = False
        _grid.AutoGenerateColumns = False
        _grid.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
        _grid.BackgroundColor = SystemColors.Window
        _grid.BorderStyle = BorderStyle.Fixed3D
        _grid.EditMode = DataGridViewEditMode.EditOnEnter
        _grid.MultiSelect = False
        _grid.RowHeadersVisible = False
        _grid.SelectionMode = DataGridViewSelectionMode.CellSelect
        _grid.ShowCellErrors = False
        _grid.ShowRowErrors = False
        _grid.StandardTab = True

        Dim oldNameColumn As New DataGridViewTextBoxColumn() With {
            .Name = COL_OLD_NAME,
            .HeaderText = "Old file name",
            .ReadOnly = True,
            .Width = 230,
            .MinimumWidth = 150
        }

        Dim originalColumn As New DataGridViewTextBoxColumn() With {
            .Name = COL_ORIGINAL,
            .HeaderText = "Original path",
            .ReadOnly = True,
            .Width = 285,
            .MinimumWidth = 180
        }

        Dim sourceTypeColumn As New DataGridViewTextBoxColumn() With {
            .Name = COL_SOURCE_TYPE,
            .HeaderText = "Source type",
            .ReadOnly = True,
            .Width = 90
        }

        Dim targetTypeColumn As New DataGridViewComboBoxColumn() With {
            .Name = COL_TARGET_TYPE,
            .HeaderText = "Import type",
            .Width = 110,
            .DisplayStyle = DataGridViewComboBoxDisplayStyle.DropDownButton,
            .FlatStyle = FlatStyle.Standard
        }
        targetTypeColumn.Items.AddRange(New Object() {"Assembly", "Part", "Vendor Part", "Drawing"})

        Dim newIdColumn As New DataGridViewTextBoxColumn() With {
            .Name = COL_NEW_ID,
            .HeaderText = "New ID / filename",
            .Width = 225,
            .MinimumWidth = 150
        }

        Dim finalNameColumn As New DataGridViewTextBoxColumn() With {
            .Name = COL_FINAL_NAME,
            .HeaderText = "Final filename",
            .ReadOnly = True,
            .Width = 235,
            .MinimumWidth = 160
        }

        Dim checkColumn As New DataGridViewButtonColumn() With {
            .Name = COL_CHECK,
            .HeaderText = "Check",
            .Text = "Check",
            .UseColumnTextForButtonValue = True,
            .Width = 68
        }

        Dim statusColumn As New DataGridViewTextBoxColumn() With {
            .Name = COL_STATUS,
            .HeaderText = "Status",
            .ReadOnly = True,
            .Width = 62,
            .DefaultCellStyle = New DataGridViewCellStyle() With {
                .Alignment = DataGridViewContentAlignment.MiddleCenter,
                .Font = New Font(SystemFonts.MessageBoxFont.FontFamily, 12.0F, FontStyle.Bold)
            }
        }

        Dim explanationColumn As New DataGridViewTextBoxColumn() With {
            .Name = COL_EXPLANATION,
            .HeaderText = "Explanation",
            .ReadOnly = True,
            .AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill,
            .MinimumWidth = 220,
            .DefaultCellStyle = New DataGridViewCellStyle() With {
                .WrapMode = DataGridViewTriState.True
            }
        }

        _grid.Columns.AddRange(New DataGridViewColumn() {
            oldNameColumn,
            originalColumn,
            sourceTypeColumn,
            targetTypeColumn,
            newIdColumn,
            finalNameColumn,
            checkColumn,
            statusColumn,
            explanationColumn
        })

        AddHandler _grid.CellBeginEdit, AddressOf gridCellBeginEdit
        AddHandler _grid.CellContentClick, AddressOf gridCellContentClick
        AddHandler _grid.CellValueChanged, AddressOf gridCellValueChanged
        AddHandler _grid.CurrentCellDirtyStateChanged, AddressOf gridCurrentCellDirtyStateChanged
        AddHandler _grid.DataError, AddressOf gridDataError
    End Sub

    Private Sub populateRows()
        _suppressGridEvents = True

        Try
            _grid.Rows.Clear()

            For Each item As LegacyImportItem In _plan.Items
                Dim rowIndex As Integer = _grid.Rows.Add()
                Dim row As DataGridViewRow = _grid.Rows(rowIndex)
                row.Tag = item

                row.Cells(COL_OLD_NAME).Value = item.OriginalFileName
                row.Cells(COL_ORIGINAL).Value = item.SourcePath
                row.Cells(COL_SOURCE_TYPE).Value = sourceTypeText(item.SourceType)
                Dim targetCell As DataGridViewComboBoxCell = TryCast(row.Cells(COL_TARGET_TYPE), DataGridViewComboBoxCell)
                If targetCell IsNot Nothing Then
                    targetCell.Items.Clear()
                    If item.SourceType = LegacyImportSourceType.Part Then
                        targetCell.Items.Add("Part")
                        targetCell.Items.Add("Vendor Part")
                    Else
                        targetCell.Items.Add(targetTypeText(item.TargetType))
                    End If
                End If

                row.Cells(COL_TARGET_TYPE).Value = targetTypeText(item.TargetType)
                row.Cells(COL_NEW_ID).Value = If(item.ProposedId, "")
                row.Cells(COL_FINAL_NAME).Value = ""
                row.Cells(COL_STATUS).Value = "○"
                row.Cells(COL_EXPLANATION).Value = If(item.IsVirtualComponent,
                    "Virtual component: SOLIDWORKS Pack and Go cannot rename it. Press Check for details.",
                    "Not checked")

                If item.IsVirtualComponent Then
                    row.Cells(COL_OLD_NAME).Style.BackColor = Color.LemonChiffon
                    row.Cells(COL_ORIGINAL).Style.BackColor = Color.LemonChiffon
                End If

                If item.SourceType <> LegacyImportSourceType.Part Then
                    row.Cells(COL_TARGET_TYPE).ReadOnly = True
                    row.Cells(COL_TARGET_TYPE).Style.BackColor = SystemColors.Control
                End If
            Next
        Finally
            _suppressGridEvents = False
        End Try
    End Sub

    Private Function sourceTypeText(ByVal value As LegacyImportSourceType) As String
        Select Case value
            Case LegacyImportSourceType.Assembly
                Return "Assembly"
            Case LegacyImportSourceType.Drawing
                Return "Drawing"
            Case Else
                Return "Part"
        End Select
    End Function

    Private Function targetTypeText(ByVal value As LegacyImportTargetType) As String
        Select Case value
            Case LegacyImportTargetType.Assembly
                Return "Assembly"
            Case LegacyImportTargetType.VendorPart
                Return "Vendor Part"
            Case LegacyImportTargetType.Drawing
                Return "Drawing"
            Case Else
                Return "Part"
        End Select
    End Function

    Private Function targetTypeFromText(ByVal value As Object) As LegacyImportTargetType
        Dim text As String = Convert.ToString(value).Trim()

        Select Case text.ToUpperInvariant()
            Case "ASSEMBLY"
                Return LegacyImportTargetType.Assembly
            Case "VENDOR PART"
                Return LegacyImportTargetType.VendorPart
            Case "DRAWING"
                Return LegacyImportTargetType.Drawing
            Case Else
                Return LegacyImportTargetType.Part
        End Select
    End Function

    Private Sub gridCellBeginEdit(ByVal sender As Object, ByVal e As DataGridViewCellCancelEventArgs)
        If e.RowIndex < 0 OrElse e.ColumnIndex < 0 Then Return

        Dim columnName As String = _grid.Columns(e.ColumnIndex).Name
        If columnName <> COL_TARGET_TYPE Then Return

        Dim item As LegacyImportItem = TryCast(_grid.Rows(e.RowIndex).Tag, LegacyImportItem)
        If item Is Nothing OrElse item.SourceType <> LegacyImportSourceType.Part Then
            e.Cancel = True
        End If
    End Sub

    Private Sub gridCellContentClick(ByVal sender As Object, ByVal e As DataGridViewCellEventArgs)
        If e.RowIndex < 0 OrElse e.ColumnIndex < 0 Then Return
        If _grid.Columns(e.ColumnIndex).Name <> COL_CHECK Then Return

        _grid.EndEdit()
        validateGridRow(e.RowIndex)
    End Sub

    Private Sub gridCurrentCellDirtyStateChanged(ByVal sender As Object, ByVal e As EventArgs)
        If _grid.IsCurrentCellDirty Then
            _grid.CommitEdit(DataGridViewDataErrorContexts.Commit)
        End If
    End Sub

    Private Sub gridCellValueChanged(ByVal sender As Object, ByVal e As DataGridViewCellEventArgs)
        If _suppressGridEvents Then Return
        If e.RowIndex < 0 OrElse e.ColumnIndex < 0 Then Return

        Dim columnName As String = _grid.Columns(e.ColumnIndex).Name
        If columnName <> COL_TARGET_TYPE AndAlso columnName <> COL_NEW_ID Then Return

        Dim row As DataGridViewRow = _grid.Rows(e.RowIndex)
        Dim item As LegacyImportItem = TryCast(row.Tag, LegacyImportItem)
        If item Is Nothing Then Return

        If columnName = COL_TARGET_TYPE Then
            Dim requestedType As LegacyImportTargetType = targetTypeFromText(row.Cells(COL_TARGET_TYPE).Value)

            'Hard rule: only a source PART can change type, and it can only switch
            'between normal PART and VENDOR PART.
            If item.SourceType = LegacyImportSourceType.Part Then
                If requestedType <> LegacyImportTargetType.Part AndAlso
                   requestedType <> LegacyImportTargetType.VendorPart Then
                    requestedType = LegacyImportTargetType.Part
                End If
            Else
                Select Case item.SourceType
                    Case LegacyImportSourceType.Assembly
                        requestedType = LegacyImportTargetType.Assembly
                    Case LegacyImportSourceType.Drawing
                        requestedType = LegacyImportTargetType.Drawing
                End Select
            End If

            item.TargetType = requestedType

            _suppressGridEvents = True
            Try
                row.Cells(COL_TARGET_TYPE).Value = targetTypeText(item.TargetType)

                If item.TargetType = LegacyImportTargetType.VendorPart Then
                    Dim currentText As String = Convert.ToString(row.Cells(COL_NEW_ID).Value).Trim()
                    If String.IsNullOrWhiteSpace(currentText) OrElse currentText.StartsWith("GRC27_", StringComparison.OrdinalIgnoreCase) OrElse currentText.StartsWith("CFD27_", StringComparison.OrdinalIgnoreCase) Then
                        row.Cells(COL_NEW_ID).Value = Path.GetFileNameWithoutExtension(item.SourcePath)
                    End If
                End If
            Finally
                _suppressGridEvents = False
            End Try
        End If

        resetRowValidation(row)
        updateSummaryAndContinueState()
    End Sub

    Private Sub gridDataError(ByVal sender As Object, ByVal e As DataGridViewDataErrorEventArgs)
        e.ThrowException = False
    End Sub

    Private Sub resetRowValidation(ByVal row As DataGridViewRow)
        Dim item As LegacyImportItem = TryCast(row.Tag, LegacyImportItem)
        If item Is Nothing Then Return

        item.IsChecked = False
        item.IsValid = False
        item.ValidationMessage = If(item.IsVirtualComponent,
            "Virtual component: SOLIDWORKS Pack and Go cannot rename it. Press Check for details.",
            "Not checked")
        item.FinalFileName = ""
        item.DestinationPath = ""

        row.Cells(COL_FINAL_NAME).Value = ""
        row.Cells(COL_STATUS).Value = "○"
        row.Cells(COL_STATUS).Style.BackColor = Color.Empty
        row.Cells(COL_STATUS).Style.ForeColor = Color.Empty
        row.Cells(COL_EXPLANATION).Value = item.ValidationMessage
    End Sub

    Private Sub validateGridRow(ByVal rowIndex As Integer)
        If rowIndex < 0 OrElse rowIndex >= _grid.Rows.Count Then Return

        Dim row As DataGridViewRow = _grid.Rows(rowIndex)
        Dim item As LegacyImportItem = TryCast(row.Tag, LegacyImportItem)
        If item Is Nothing Then Return

        item.TargetType = targetTypeFromText(row.Cells(COL_TARGET_TYPE).Value)
        item.ProposedId = Convert.ToString(row.Cells(COL_NEW_ID).Value).Trim()

        _plan.GrcDestinationFolder = _txtGrcFolder.Text.Trim()
        _plan.VendorDestinationFolder = _txtVendorFolder.Text.Trim()

        Dim result As LegacyImportValidationResult = svnModule.validateLegacyImportItemPublic(item, _plan)

        item.IsChecked = True
        item.IsValid = result IsNot Nothing AndAlso result.IsValid
        item.ValidationMessage = If(result Is Nothing, "Validation did not return a result.", result.Message)
        item.FinalFileName = If(result Is Nothing, "", result.FinalFileName)
        item.DestinationPath = If(result Is Nothing, "", result.DestinationPath)

        row.Cells(COL_FINAL_NAME).Value = item.FinalFileName
        row.Cells(COL_EXPLANATION).Value = item.ValidationMessage

        If item.IsValid Then
            row.Cells(COL_STATUS).Value = "✓"
            row.Cells(COL_STATUS).Style.BackColor = Color.Honeydew
            row.Cells(COL_STATUS).Style.ForeColor = Color.DarkGreen
        Else
            row.Cells(COL_STATUS).Value = "✕"
            row.Cells(COL_STATUS).Style.BackColor = Color.MistyRose
            row.Cells(COL_STATUS).Style.ForeColor = Color.DarkRed
        End If

        updateSummaryAndContinueState()
    End Sub

    Private Sub checkAllRows(ByVal sender As Object, ByVal e As EventArgs)
        _grid.EndEdit()

        For i As Integer = 0 To _grid.Rows.Count - 1
            validateGridRow(i)
        Next

        'Run a second pass so that duplicate-name messages are consistent on both rows.
        For i As Integer = 0 To _grid.Rows.Count - 1
            validateGridRow(i)
        Next
    End Sub

    Private Sub selectNextProblem(ByVal sender As Object, ByVal e As EventArgs)
        If _grid.Rows.Count = 0 Then Return

        Dim startIndex As Integer = 0
        If _grid.CurrentCell IsNot Nothing Then startIndex = _grid.CurrentCell.RowIndex + 1

        For offset As Integer = 0 To _grid.Rows.Count - 1
            Dim rowIndex As Integer = (startIndex + offset) Mod _grid.Rows.Count
            Dim item As LegacyImportItem = TryCast(_grid.Rows(rowIndex).Tag, LegacyImportItem)

            If item Is Nothing OrElse Not item.IsChecked OrElse Not item.IsValid Then
                _grid.CurrentCell = _grid.Rows(rowIndex).Cells(COL_NEW_ID)
                _grid.FirstDisplayedScrollingRowIndex = Math.Max(0, rowIndex)
                _grid.Focus()
                Return
            End If
        Next
    End Sub

    Private Sub destinationFolderChanged(ByVal sender As Object, ByVal e As EventArgs)
        If _suppressGridEvents Then Return

        For Each row As DataGridViewRow In _grid.Rows
            resetRowValidation(row)
        Next

        updateSummaryAndContinueState()
    End Sub

    Private Sub browseGrcFolder(ByVal sender As Object, ByVal e As EventArgs)
        Dim selected As String = svnModule.pickLegacyGrcDestinationFolderPublic(_txtGrcFolder.Text)
        If Not String.IsNullOrWhiteSpace(selected) Then _txtGrcFolder.Text = selected
    End Sub

    Private Sub browseVendorFolder(ByVal sender As Object, ByVal e As EventArgs)
        Dim selected As String = svnModule.pickLegacyVendorDestinationFolderPublic(_txtVendorFolder.Text)
        If Not String.IsNullOrWhiteSpace(selected) Then _txtVendorFolder.Text = selected
    End Sub

    Private Sub cancelClicked(ByVal sender As Object, ByVal e As EventArgs)
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub continueClicked(ByVal sender As Object, ByVal e As EventArgs)
        _grid.EndEdit()

        'Always perform a fresh, authoritative validation immediately before Pack and Go.
        checkAllRows(sender, e)
        If Not _btnContinue.Enabled Then
            MessageBox.Show(Me,
                            "Every row must have a green check before the import can continue.",
                            "Legacy Import",
                            MessageBoxButtons.OK,
                            MessageBoxIcon.Warning)
            Return
        End If

        _plan.GrcDestinationFolder = _txtGrcFolder.Text.Trim()
        _plan.VendorDestinationFolder = _txtVendorFolder.Text.Trim()

        Dim errorMessage As String = ""
        Me.UseWaitCursor = True
        _btnContinue.Enabled = False
        _btnCancel.Enabled = False
        _btnCheckAll.Enabled = False
        _btnNextProblem.Enabled = False

        Try
            Dim started As Boolean = svnModule.executeLegacyImportPublic(_plan, errorMessage)

            If Not started Then
                If String.IsNullOrWhiteSpace(errorMessage) Then errorMessage = "The legacy import could not be started."

                MessageBox.Show(Me,
                                errorMessage,
                                "Legacy Import",
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error)
                Return
            End If

            Me.DialogResult = DialogResult.OK
            Me.Close()
        Finally
            Me.UseWaitCursor = False
            _btnCancel.Enabled = True
            _btnCheckAll.Enabled = True
            _btnNextProblem.Enabled = True
            updateSummaryAndContinueState()
        End Try
    End Sub

    Private Sub updateSummaryAndContinueState()
        Dim total As Integer = _plan.Items.Count
        Dim valid As Integer = _plan.Items.FindAll(
            Function(item As LegacyImportItem) item.IsChecked AndAlso item.IsValid
        ).Count

        Dim invalid As Integer = _plan.Items.FindAll(
            Function(item As LegacyImportItem) item.IsChecked AndAlso Not item.IsValid
        ).Count
        Dim unchecked As Integer = total - valid - invalid

        _lblSummary.Text = String.Format("Files: {0}    Valid: {1}    Invalid: {2}    Not checked: {3}",
                                         total,
                                         valid,
                                         invalid,
                                         unchecked)

        _btnContinue.Enabled = total > 0 AndAlso valid = total AndAlso invalid = 0 AndAlso unchecked = 0
    End Sub
End Class
