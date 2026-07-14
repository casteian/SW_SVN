Imports System
Imports System.Collections.Generic
Imports System.Drawing
Imports System.IO
Imports System.Linq
Imports System.Text.RegularExpressions
Imports System.Windows.Forms
Imports SolidWorks.Interop.sldworks

Public Enum VirtualComponentHandlingType
    SaveExternally
    KeepEmbedded
End Enum

Public Enum VirtualComponentTargetType
    GrcCad
    VendorPart
End Enum

Public Class VirtualComponentExternalizeItem
    Public Property Component As Component2 = Nothing
    Public Property DisplayName As String = ""
    Public Property OwnerAssemblyPath As String = ""
    Public Property DocumentExtension As String = ""
    Public Property ComponentDepth As Integer = 0
    Public Property Handling As VirtualComponentHandlingType = VirtualComponentHandlingType.SaveExternally
    Public Property TargetType As VirtualComponentTargetType = VirtualComponentTargetType.GrcCad
    Public Property ProposedId As String = ""
    Public Property DestinationFolder As String = ""
    Public Property FinalFileName As String = ""
    Public Property DestinationPath As String = ""
    Public Property IsChecked As Boolean = False
    Public Property IsValid As Boolean = False
    Public Property ValidationMessage As String = ""

    Public ReadOnly Property SourceTypeText As String
        Get
            Select Case DocumentExtension.ToUpperInvariant()
                Case ".SLDPRT"
                    Return "Part"
                Case ".SLDASM"
                    Return "Assembly"
                Case Else
                    Return "CAD"
            End Select
        End Get
    End Property

    Public ReadOnly Property CanBeVendorPart As Boolean
        Get
            Return String.Equals(DocumentExtension, ".SLDPRT", StringComparison.OrdinalIgnoreCase)
        End Get
    End Property
End Class

Public Class VirtualComponentExternalizePlan
    Public Property LocalRepoRootFolder As String = ""
    Public Property VendorRootFolder As String = ""
    Public Property Items As New List(Of VirtualComponentExternalizeItem)()
End Class

Public Class VirtualComponentExternalizeForm
    Inherits Form

    Private ReadOnly _plan As VirtualComponentExternalizePlan
    Private ReadOnly _grid As New DataGridView()
    Private ReadOnly _lblInstruction As New Label()
    Private ReadOnly _lblWorkingCopy As New Label()
    Private ReadOnly _lblSummary As New Label()
    Private ReadOnly _btnCheckAll As New Button()
    Private ReadOnly _btnNextProblem As New Button()
    Private ReadOnly _btnReturn As New Button()
    Private ReadOnly _btnContinue As New Button()
    Private _suppressEvents As Boolean = False

    Private Const COL_COMPONENT As String = "Component"
    Private Const COL_OWNER As String = "Owner"
    Private Const COL_SOURCE_TYPE As String = "SourceType"
    Private Const COL_HANDLING As String = "Handling"
    Private Const COL_TARGET_TYPE As String = "TargetType"
    Private Const COL_NEW_ID As String = "NewId"
    Private Const COL_DESTINATION As String = "Destination"
    Private Const COL_BROWSE As String = "Browse"
    Private Const COL_FINAL_NAME As String = "FinalName"
    Private Const COL_CHECK As String = "Check"
    Private Const COL_STATUS As String = "Status"
    Private Const COL_EXPLANATION As String = "Explanation"

    Public Sub New(ByVal plan As VirtualComponentExternalizePlan)
        If plan Is Nothing Then Throw New ArgumentNullException(NameOf(plan))

        _plan = plan

        Me.Text = "Save Virtual Components as External SVN Files"
        Me.StartPosition = FormStartPosition.CenterParent
        Me.MinimumSize = New Size(1180, 620)
        Me.Size = New Size(1540, 790)
        Me.AutoScaleMode = AutoScaleMode.Dpi
        Me.Font = New Font("Segoe UI", 9.5F, FontStyle.Regular)
        Me.ShowIcon = False
        Me.ShowInTaskbar = False

        buildLayout()
        populateRows()
        updateSummaryAndContinueState()
    End Sub

    Private Sub buildLayout()
        Dim root As New TableLayoutPanel()
        root.Dock = DockStyle.Fill
        root.Padding = New Padding(12)
        root.ColumnCount = 1
        root.RowCount = 4
        root.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        root.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        root.RowStyles.Add(New RowStyle(SizeType.Percent, 100.0F))
        root.RowStyles.Add(New RowStyle(SizeType.AutoSize))
        Me.Controls.Add(root)

        _lblInstruction.AutoSize = True
        _lblInstruction.MaximumSize = New Size(1460, 0)
        _lblInstruction.Font = New Font("Segoe UI", 10.0F, FontStyle.Regular)
        _lblInstruction.Text =
            "This assembly contains virtual components stored inside an assembly file. PlumVault recommends saving them as external SVN-managed files. " &
            "Save externally is selected by default and starts in the physical owner assembly's folder. A virtual Part may be changed to Vendor Part; " &
            "virtual assemblies remain GRC CAD. Choose Keep embedded only when the component is intentionally meant to remain virtual. " &
            "Missing destination folders are created, added, and committed before the component is saved externally."
        _lblInstruction.Padding = New Padding(0, 0, 0, 6)
        root.Controls.Add(_lblInstruction, 0, 0)

        _lblWorkingCopy.AutoSize = True
        _lblWorkingCopy.Font = New Font("Segoe UI", 9.5F, FontStyle.Bold)
        _lblWorkingCopy.Text = "SVN working copy: " & If(_plan.LocalRepoRootFolder, "")
        _lblWorkingCopy.Padding = New Padding(0, 0, 0, 8)
        root.Controls.Add(_lblWorkingCopy, 0, 1)

        configureGrid()
        root.Controls.Add(_grid, 0, 2)

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

        configureActionButton(_btnCheckAll, "Check All")
        configureActionButton(_btnNextProblem, "Next Problem")
        configureActionButton(_btnReturn, "Return to SOLIDWORKS")
        configureActionButton(_btnContinue, "Continue Commit")
        _btnContinue.Enabled = False

        actions.Controls.Add(_btnCheckAll)
        actions.Controls.Add(_btnNextProblem)
        actions.Controls.Add(_btnReturn)
        actions.Controls.Add(_btnContinue)
        bottom.Controls.Add(actions, 1, 0)
        root.Controls.Add(bottom, 0, 3)

        AddHandler _btnCheckAll.Click, AddressOf checkAllClicked
        AddHandler _btnNextProblem.Click, AddressOf nextProblemClicked
        AddHandler _btnReturn.Click, AddressOf returnClicked
        AddHandler _btnContinue.Click, AddressOf continueClicked
    End Sub

    Private Sub configureActionButton(ByVal button As Button, ByVal text As String)
        button.Text = text
        button.AutoSize = True
        button.Font = New Font("Segoe UI", 9.5F, FontStyle.Regular)
        button.Margin = New Padding(5, 0, 0, 0)
    End Sub

    Private Sub configureGrid()
        _grid.Dock = DockStyle.Fill
        _grid.AllowUserToAddRows = False
        _grid.AllowUserToDeleteRows = False
        _grid.AllowUserToResizeRows = False
        _grid.MultiSelect = False
        _grid.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        _grid.AutoGenerateColumns = False
        _grid.RowHeadersVisible = False
        _grid.BackgroundColor = SystemColors.Window
        _grid.BorderStyle = BorderStyle.Fixed3D
        _grid.EnableHeadersVisualStyles = False
        _grid.ColumnHeadersDefaultCellStyle.Font = New Font("Segoe UI", 9.5F, FontStyle.Bold)
        _grid.ColumnHeadersDefaultCellStyle.BackColor = SystemColors.Control
        _grid.DefaultCellStyle.Font = New Font("Segoe UI", 9.25F, FontStyle.Regular)
        _grid.DefaultCellStyle.Padding = New Padding(3)
        _grid.RowTemplate.Height = 29
        _grid.ColumnHeadersHeight = 32

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_COMPONENT,
            .HeaderText = "Virtual component",
            .ReadOnly = True,
            .Width = 165
        })

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_OWNER,
            .HeaderText = "Physical owner assembly",
            .ReadOnly = True,
            .Width = 180
        })

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_SOURCE_TYPE,
            .HeaderText = "Type",
            .ReadOnly = True,
            .Width = 72
        })

        Dim handlingColumn As New DataGridViewComboBoxColumn()
        handlingColumn.Name = COL_HANDLING
        handlingColumn.HeaderText = "Handling"
        handlingColumn.Width = 120
        handlingColumn.FlatStyle = FlatStyle.Flat
        handlingColumn.Items.Add("Save externally")
        handlingColumn.Items.Add("Keep embedded")
        _grid.Columns.Add(handlingColumn)

        Dim targetColumn As New DataGridViewComboBoxColumn()
        targetColumn.Name = COL_TARGET_TYPE
        targetColumn.HeaderText = "File type"
        targetColumn.Width = 100
        targetColumn.FlatStyle = FlatStyle.Flat
        targetColumn.Items.Add("GRC CAD")
        targetColumn.Items.Add("Vendor Part")
        _grid.Columns.Add(targetColumn)

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_NEW_ID,
            .HeaderText = "New ID / filename",
            .Width = 170
        })

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_DESTINATION,
            .HeaderText = "Destination folder",
            .Width = 220
        })

        _grid.Columns.Add(New DataGridViewButtonColumn() With {
            .Name = COL_BROWSE,
            .HeaderText = "",
            .Text = "Browse...",
            .UseColumnTextForButtonValue = True,
            .Width = 76
        })

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_FINAL_NAME,
            .HeaderText = "Final filename",
            .ReadOnly = True,
            .Width = 175
        })

        _grid.Columns.Add(New DataGridViewButtonColumn() With {
            .Name = COL_CHECK,
            .HeaderText = "Check",
            .Text = "Check",
            .UseColumnTextForButtonValue = True,
            .Width = 65
        })

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_STATUS,
            .HeaderText = "Status",
            .ReadOnly = True,
            .Width = 62
        })

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_EXPLANATION,
            .HeaderText = "Explanation",
            .ReadOnly = True,
            .AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill,
            .MinimumWidth = 230
        })

        AddHandler _grid.CellContentClick, AddressOf gridCellContentClick
        AddHandler _grid.CellValueChanged, AddressOf gridCellValueChanged
        AddHandler _grid.CurrentCellDirtyStateChanged, AddressOf gridCurrentCellDirtyStateChanged
        AddHandler _grid.DataError, AddressOf gridDataError
    End Sub

    Private Sub populateRows()
        _suppressEvents = True

        Try
            For Each item As VirtualComponentExternalizeItem In _plan.Items
                If item Is Nothing Then Continue For

                If String.IsNullOrWhiteSpace(item.DestinationFolder) Then
                    item.DestinationFolder = safeOwnerFolder(item.OwnerAssemblyPath)
                End If

                If String.IsNullOrWhiteSpace(item.ProposedId) Then
                    item.ProposedId = cleanVirtualDisplayName(item.DisplayName)
                End If

                Dim rowIndex As Integer = _grid.Rows.Add()
                Dim row As DataGridViewRow = _grid.Rows(rowIndex)
                row.Tag = item

                row.Cells(COL_COMPONENT).Value = item.DisplayName
                row.Cells(COL_OWNER).Value = safeFileName(item.OwnerAssemblyPath)
                row.Cells(COL_SOURCE_TYPE).Value = item.SourceTypeText
                row.Cells(COL_HANDLING).Value = handlingText(item.Handling)
                row.Cells(COL_TARGET_TYPE).Value = targetTypeText(item.TargetType)
                row.Cells(COL_NEW_ID).Value = item.ProposedId
                row.Cells(COL_DESTINATION).Value = item.DestinationFolder
                row.Cells(COL_FINAL_NAME).Value = ""
                row.Cells(COL_STATUS).Value = "—"
                row.Cells(COL_EXPLANATION).Value = "Review this row, then press Check."

                If Not item.CanBeVendorPart Then
                    row.Cells(COL_TARGET_TYPE).ReadOnly = True
                    row.Cells(COL_TARGET_TYPE).Style.BackColor = SystemColors.Control
                End If

                updateRowEditability(row, item)
                applyPendingRowStyle(row)
            Next
        Finally
            _suppressEvents = False
        End Try
    End Sub

    Private Function safeOwnerFolder(ByVal ownerPath As String) As String
        Try
            Return Path.GetDirectoryName(ownerPath)
        Catch
            Return ""
        End Try
    End Function

    Private Function safeFileName(ByVal filePath As String) As String
        Try
            Return Path.GetFileName(filePath)
        Catch
            Return filePath
        End Try
    End Function

    Private Function cleanVirtualDisplayName(ByVal value As String) As String
        If String.IsNullOrWhiteSpace(value) Then Return "VirtualComponent"

        Dim result As String = value.Trim()
        result = Regex.Replace(result, "\s*\[Virtual\]\s*$", "", RegexOptions.IgnoreCase)
        result = Regex.Replace(result, "<\d+>$", "")
        result = Regex.Replace(result, "-\d+$", "")

        If result.Contains("^") Then result = result.Substring(0, result.IndexOf("^"c))
        If String.IsNullOrWhiteSpace(result) Then result = "VirtualComponent"
        Return result
    End Function

    Private Function handlingText(ByVal handling As VirtualComponentHandlingType) As String
        If handling = VirtualComponentHandlingType.KeepEmbedded Then Return "Keep embedded"
        Return "Save externally"
    End Function

    Private Function handlingFromText(ByVal value As Object) As VirtualComponentHandlingType
        If value IsNot Nothing AndAlso String.Equals(CStr(value), "Keep embedded", StringComparison.OrdinalIgnoreCase) Then
            Return VirtualComponentHandlingType.KeepEmbedded
        End If
        Return VirtualComponentHandlingType.SaveExternally
    End Function

    Private Function targetTypeText(ByVal targetType As VirtualComponentTargetType) As String
        If targetType = VirtualComponentTargetType.VendorPart Then Return "Vendor Part"
        Return "GRC CAD"
    End Function

    Private Function targetTypeFromText(ByVal value As Object) As VirtualComponentTargetType
        If value IsNot Nothing AndAlso String.Equals(CStr(value), "Vendor Part", StringComparison.OrdinalIgnoreCase) Then
            Return VirtualComponentTargetType.VendorPart
        End If
        Return VirtualComponentTargetType.GrcCad
    End Function

    Private Sub gridCurrentCellDirtyStateChanged(sender As Object, e As EventArgs)
        If _grid.IsCurrentCellDirty Then
            Try
                _grid.CommitEdit(DataGridViewDataErrorContexts.Commit)
            Catch
            End Try
        End If
    End Sub

    Private Sub gridDataError(sender As Object, e As DataGridViewDataErrorEventArgs)
        e.ThrowException = False
    End Sub

    Private Sub gridCellContentClick(sender As Object, e As DataGridViewCellEventArgs)
        If e.RowIndex < 0 OrElse e.ColumnIndex < 0 Then Exit Sub

        Dim columnName As String = _grid.Columns(e.ColumnIndex).Name

        If columnName = COL_BROWSE Then
            browseForRow(e.RowIndex)
        ElseIf columnName = COL_CHECK Then
            validateRow(e.RowIndex)
        End If
    End Sub

    Private Sub gridCellValueChanged(sender As Object, e As DataGridViewCellEventArgs)
        If _suppressEvents Then Exit Sub
        If e.RowIndex < 0 OrElse e.ColumnIndex < 0 Then Exit Sub

        Dim columnName As String = _grid.Columns(e.ColumnIndex).Name
        If columnName <> COL_HANDLING AndAlso columnName <> COL_TARGET_TYPE AndAlso
           columnName <> COL_NEW_ID AndAlso columnName <> COL_DESTINATION Then Exit Sub

        Dim row As DataGridViewRow = _grid.Rows(e.RowIndex)
        Dim item As VirtualComponentExternalizeItem = TryCast(row.Tag, VirtualComponentExternalizeItem)
        If item Is Nothing Then Exit Sub

        item.Handling = handlingFromText(row.Cells(COL_HANDLING).Value)
        item.TargetType = targetTypeFromText(row.Cells(COL_TARGET_TYPE).Value)

        If item.TargetType = VirtualComponentTargetType.VendorPart AndAlso Not item.CanBeVendorPart Then
            item.TargetType = VirtualComponentTargetType.GrcCad
            _suppressEvents = True
            row.Cells(COL_TARGET_TYPE).Value = "GRC CAD"
            _suppressEvents = False
        End If

        If item.Handling = VirtualComponentHandlingType.SaveExternally Then
            If item.TargetType = VirtualComponentTargetType.VendorPart Then
                _suppressEvents = True
                row.Cells(COL_DESTINATION).Value = _plan.VendorRootFolder
                _suppressEvents = False
            ElseIf String.IsNullOrWhiteSpace(CStr(row.Cells(COL_DESTINATION).Value)) OrElse
                   pathContainsNamedFolderSegment(CStr(row.Cells(COL_DESTINATION).Value), _plan.LocalRepoRootFolder, "Vendor Parts") Then
                _suppressEvents = True
                row.Cells(COL_DESTINATION).Value = safeOwnerFolder(item.OwnerAssemblyPath)
                _suppressEvents = False
            End If
        End If

        item.IsChecked = False
        item.IsValid = False
        item.ValidationMessage = ""
        item.FinalFileName = ""
        item.DestinationPath = ""
        row.Cells(COL_FINAL_NAME).Value = ""
        row.Cells(COL_STATUS).Value = "—"
        row.Cells(COL_EXPLANATION).Value = "Changed. Press Check again."
        updateRowEditability(row, item)
        applyPendingRowStyle(row)
        updateSummaryAndContinueState()
    End Sub

    Private Sub updateRowEditability(ByVal row As DataGridViewRow, ByVal item As VirtualComponentExternalizeItem)
        Dim keepEmbedded As Boolean = item.Handling = VirtualComponentHandlingType.KeepEmbedded

        row.Cells(COL_TARGET_TYPE).ReadOnly = keepEmbedded OrElse Not item.CanBeVendorPart
        row.Cells(COL_NEW_ID).ReadOnly = keepEmbedded
        row.Cells(COL_DESTINATION).ReadOnly = keepEmbedded
        row.Cells(COL_BROWSE).ReadOnly = keepEmbedded

        Dim disabledColor As Color = SystemColors.Control
        Dim enabledColor As Color = SystemColors.Window

        row.Cells(COL_TARGET_TYPE).Style.BackColor = If(keepEmbedded OrElse Not item.CanBeVendorPart, disabledColor, enabledColor)
        row.Cells(COL_NEW_ID).Style.BackColor = If(keepEmbedded, disabledColor, enabledColor)
        row.Cells(COL_DESTINATION).Style.BackColor = If(keepEmbedded, disabledColor, enabledColor)
    End Sub

    Private Sub browseForRow(ByVal rowIndex As Integer)
        If rowIndex < 0 OrElse rowIndex >= _grid.Rows.Count Then Exit Sub

        Dim row As DataGridViewRow = _grid.Rows(rowIndex)
        Dim item As VirtualComponentExternalizeItem = TryCast(row.Tag, VirtualComponentExternalizeItem)
        If item Is Nothing OrElse item.Handling = VirtualComponentHandlingType.KeepEmbedded Then Exit Sub

        Dim currentFolder As String = CStr(If(row.Cells(COL_DESTINATION).Value, ""))
        If String.IsNullOrWhiteSpace(currentFolder) OrElse Not Directory.Exists(currentFolder) Then
            currentFolder = If(item.TargetType = VirtualComponentTargetType.VendorPart,
                               _plan.VendorRootFolder,
                               safeOwnerFolder(item.OwnerAssemblyPath))
        End If

        Using dialog As New FolderBrowserDialog()
            dialog.Description = If(item.TargetType = VirtualComponentTargetType.VendorPart,
                                    "Choose a folder inside a Vendor Parts folder.",
                                    "Choose a folder inside the SVN working copy.")
            dialog.ShowNewFolderButton = True
            dialog.SelectedPath = currentFolder

            If dialog.ShowDialog(Me) = DialogResult.OK Then
                row.Cells(COL_DESTINATION).Value = dialog.SelectedPath
            End If
        End Using
    End Sub

    Private Sub checkAllClicked(sender As Object, e As EventArgs)
        For i As Integer = 0 To _grid.Rows.Count - 1
            validateRow(i)
        Next
    End Sub

    Private Sub nextProblemClicked(sender As Object, e As EventArgs)
        If _grid.Rows.Count = 0 Then Exit Sub

        Dim startIndex As Integer = If(_grid.CurrentCell Is Nothing, -1, _grid.CurrentCell.RowIndex)

        For offset As Integer = 1 To _grid.Rows.Count
            Dim index As Integer = (startIndex + offset) Mod _grid.Rows.Count
            Dim item As VirtualComponentExternalizeItem = TryCast(_grid.Rows(index).Tag, VirtualComponentExternalizeItem)

            If item IsNot Nothing AndAlso (Not item.IsChecked OrElse Not item.IsValid) Then
                _grid.CurrentCell = _grid.Rows(index).Cells(COL_COMPONENT)
                _grid.FirstDisplayedScrollingRowIndex = Math.Max(0, index)
                Exit Sub
            End If
        Next
    End Sub

    Private Sub returnClicked(sender As Object, e As EventArgs)
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub continueClicked(sender As Object, e As EventArgs)
        For i As Integer = 0 To _grid.Rows.Count - 1
            validateRow(i)
        Next

        If Not allRowsValid() Then
            updateSummaryAndContinueState()
            Exit Sub
        End If

        Me.DialogResult = DialogResult.OK
        Me.Close()
    End Sub

    Private Sub validateRow(ByVal rowIndex As Integer)
        If rowIndex < 0 OrElse rowIndex >= _grid.Rows.Count Then Exit Sub

        Dim row As DataGridViewRow = _grid.Rows(rowIndex)
        Dim item As VirtualComponentExternalizeItem = TryCast(row.Tag, VirtualComponentExternalizeItem)
        If item Is Nothing Then Exit Sub

        item.Handling = handlingFromText(row.Cells(COL_HANDLING).Value)
        item.TargetType = targetTypeFromText(row.Cells(COL_TARGET_TYPE).Value)
        item.ProposedId = CStr(If(row.Cells(COL_NEW_ID).Value, "")).Trim()
        item.DestinationFolder = CStr(If(row.Cells(COL_DESTINATION).Value, "")).Trim()
        item.FinalFileName = ""
        item.DestinationPath = ""
        item.IsChecked = True
        item.IsValid = False

        If item.Handling = VirtualComponentHandlingType.KeepEmbedded Then
            item.IsValid = True
            item.ValidationMessage = "Kept inside the physical owner assembly. The assembly lock and revision continue to control this virtual component."
            row.Cells(COL_FINAL_NAME).Value = "Embedded"
            row.Cells(COL_STATUS).Value = "✓"
            row.Cells(COL_EXPLANATION).Value = item.ValidationMessage
            applyValidRowStyle(row)
            updateSummaryAndContinueState()
            Exit Sub
        End If

        If item.Component Is Nothing Then
            setInvalid(item, row, "The SOLIDWORKS virtual-component object is no longer available. Refresh the tree and retry.")
            Return
        End If

        If item.TargetType = VirtualComponentTargetType.VendorPart AndAlso Not item.CanBeVendorPart Then
            setInvalid(item, row, "Only a virtual Part can be changed to Vendor Part. Virtual assemblies must remain GRC CAD.")
            Return
        End If

        If String.IsNullOrWhiteSpace(item.DestinationFolder) Then
            setInvalid(item, row, "Choose a destination folder.")
            Return
        End If

        Dim fullDestinationFolder As String = ""
        Try
            fullDestinationFolder = Path.GetFullPath(item.DestinationFolder).TrimEnd("\"c)
        Catch
            setInvalid(item, row, "The destination folder path is invalid.")
            Return
        End Try

        If Not isSameOrChildPath(fullDestinationFolder, _plan.LocalRepoRootFolder) Then
            setInvalid(item, row, "The destination must be inside the SVN working copy: " & _plan.LocalRepoRootFolder)
            Return
        End If

        If item.TargetType = VirtualComponentTargetType.VendorPart Then
            If Not pathContainsNamedFolderSegment(fullDestinationFolder, _plan.LocalRepoRootFolder, "Vendor Parts") Then
                setInvalid(item, row, "Vendor Part destinations must contain a folder segment named Vendor Parts.")
                Return
            End If
        ElseIf pathContainsNamedFolderSegment(fullDestinationFolder, _plan.LocalRepoRootFolder, "Vendor Parts") Then
            setInvalid(item, row, "GRC CAD cannot be saved inside Vendor Parts. Change the type or choose another folder.")
            Return
        End If

        Dim finalName As String = item.ProposedId.Trim()
        If String.IsNullOrWhiteSpace(finalName) Then
            setInvalid(item, row, "Enter the new GRC/CFD ID or vendor filename.")
            Return
        End If

        Dim ext As String = item.DocumentExtension.ToUpperInvariant()
        If Not finalName.EndsWith(ext, StringComparison.OrdinalIgnoreCase) Then finalName &= ext

        If item.TargetType = VirtualComponentTargetType.GrcCad Then
            If Not isValidGrc27FileName(finalName) Then
                setInvalid(item, row,
                           "Invalid GRC27/CFD27 filename. Use PREFIX_CODE_00000_R# or the approved letter-number variants.")
                Return
            End If
        Else
            Dim invalidChars() As Char = Path.GetInvalidFileNameChars()
            If finalName.IndexOfAny(invalidChars) >= 0 Then
                setInvalid(item, row, "The vendor filename contains characters Windows does not allow.")
                Return
            End If
        End If

        Dim destinationPath As String = Path.Combine(fullDestinationFolder, finalName)

        For Each otherRow As DataGridViewRow In _grid.Rows
            If otherRow.Index = rowIndex Then Continue For
            Dim otherItem As VirtualComponentExternalizeItem = TryCast(otherRow.Tag, VirtualComponentExternalizeItem)
            If otherItem Is Nothing OrElse Not otherItem.IsChecked OrElse Not otherItem.IsValid Then Continue For
            If otherItem.Handling = VirtualComponentHandlingType.KeepEmbedded Then Continue For
            If String.IsNullOrWhiteSpace(otherItem.DestinationPath) Then Continue For

            If String.Equals(normalizePath(otherItem.DestinationPath), normalizePath(destinationPath), StringComparison.OrdinalIgnoreCase) Then
                setInvalid(item, row, "Another virtual component is already assigned to this destination path.")
                Return
            End If
        Next

        If File.Exists(destinationPath) Then
            setInvalid(item, row, "A file already exists at this destination. Choose another ID or folder; PlumVault will not overwrite it.")
            Return
        End If

        item.DestinationFolder = fullDestinationFolder
        item.FinalFileName = finalName
        item.DestinationPath = destinationPath
        item.IsValid = True
        item.ValidationMessage = If(Directory.Exists(fullDestinationFolder),
                                    "Valid. The component will be saved externally and included in the assembly commit.",
                                    "Valid. The destination folder will be created and committed before the component is saved externally.")

        row.Cells(COL_DESTINATION).Value = item.DestinationFolder
        row.Cells(COL_FINAL_NAME).Value = item.FinalFileName
        row.Cells(COL_STATUS).Value = "✓"
        row.Cells(COL_EXPLANATION).Value = item.ValidationMessage
        applyValidRowStyle(row)
        updateSummaryAndContinueState()
    End Sub

    Private Sub setInvalid(ByVal item As VirtualComponentExternalizeItem,
                           ByVal row As DataGridViewRow,
                           ByVal message As String)
        item.IsValid = False
        item.ValidationMessage = message
        item.FinalFileName = ""
        item.DestinationPath = ""
        row.Cells(COL_FINAL_NAME).Value = ""
        row.Cells(COL_STATUS).Value = "✗"
        row.Cells(COL_EXPLANATION).Value = message
        applyInvalidRowStyle(row)
        updateSummaryAndContinueState()
    End Sub

    Private Function isValidGrc27FileName(ByVal fileName As String) As Boolean
        If String.IsNullOrWhiteSpace(fileName) Then Return False

        Return Regex.IsMatch(
            Path.GetFileName(fileName),
            "^(GRC|CFD)27_(BR|DT|AE|FR|EL|ST|SU|WT|MI)_[A-Z]{0,3}\d+_R\d+\.(SLDPRT|SLDASM)$",
            RegexOptions.IgnoreCase)
    End Function

    Private Function isSameOrChildPath(ByVal pathValue As String, ByVal rootValue As String) As Boolean
        If String.IsNullOrWhiteSpace(pathValue) OrElse String.IsNullOrWhiteSpace(rootValue) Then Return False
        Try
            Dim fullPath As String = Path.GetFullPath(pathValue).TrimEnd("\"c)
            Dim root As String = Path.GetFullPath(rootValue).TrimEnd("\"c)
            Return String.Equals(fullPath, root, StringComparison.OrdinalIgnoreCase) OrElse
                   fullPath.StartsWith(root & Path.DirectorySeparatorChar, StringComparison.OrdinalIgnoreCase)
        Catch
            Return False
        End Try
    End Function

    Private Function pathContainsNamedFolderSegment(ByVal pathValue As String,
                                                    ByVal rootValue As String,
                                                    ByVal folderName As String) As Boolean
        If Not isSameOrChildPath(pathValue, rootValue) Then Return False

        Try
            Dim fullPath As String = Path.GetFullPath(pathValue).TrimEnd("\"c, "/"c)
            Dim root As String = Path.GetFullPath(rootValue).TrimEnd("\"c, "/"c)
            Dim relative As String = fullPath.Substring(root.Length).TrimStart("\"c, "/"c)
            If String.IsNullOrWhiteSpace(relative) Then Return False

            For Each segment As String In relative.Split(New Char() {"\"c, "/"c}, StringSplitOptions.RemoveEmptyEntries)
                If String.Equals(segment, folderName, StringComparison.OrdinalIgnoreCase) Then Return True
            Next
        Catch
            Return False
        End Try

        Return False
    End Function

    Private Function normalizePath(ByVal value As String) As String
        Try
            Return Path.GetFullPath(value).TrimEnd("\"c)
        Catch
            Return value
        End Try
    End Function

    Private Function allRowsValid() As Boolean
        If _plan.Items Is Nothing OrElse _plan.Items.Count = 0 Then Return False
        Return _plan.Items.All(Function(item As VirtualComponentExternalizeItem) item IsNot Nothing AndAlso item.IsChecked AndAlso item.IsValid)
    End Function

    Private Sub updateSummaryAndContinueState()
        Dim total As Integer = If(_plan.Items Is Nothing, 0, _plan.Items.Count)
        Dim valid As Integer = 0
        Dim invalid As Integer = 0
        Dim pending As Integer = 0
        Dim externalCount As Integer = 0
        Dim embeddedCount As Integer = 0

        If _plan.Items IsNot Nothing Then
            For Each item As VirtualComponentExternalizeItem In _plan.Items
                If item Is Nothing Then Continue For

                If Not item.IsChecked Then
                    pending += 1
                ElseIf item.IsValid Then
                    valid += 1
                Else
                    invalid += 1
                End If

                If item.Handling = VirtualComponentHandlingType.KeepEmbedded Then
                    embeddedCount += 1
                Else
                    externalCount += 1
                End If
            Next
        End If

        _lblSummary.Text = String.Format(
            "Files: {0}    Valid: {1}    Invalid: {2}    Pending: {3}    Save externally: {4}    Keep embedded: {5}",
            total, valid, invalid, pending, externalCount, embeddedCount)

        _btnContinue.Enabled = allRowsValid()
    End Sub

    Private Sub applyPendingRowStyle(ByVal row As DataGridViewRow)
        row.DefaultCellStyle.BackColor = Color.LightYellow
        row.DefaultCellStyle.ForeColor = SystemColors.ControlText
    End Sub

    Private Sub applyValidRowStyle(ByVal row As DataGridViewRow)
        row.DefaultCellStyle.BackColor = Color.Honeydew
        row.DefaultCellStyle.ForeColor = SystemColors.ControlText
    End Sub

    Private Sub applyInvalidRowStyle(ByVal row As DataGridViewRow)
        row.DefaultCellStyle.BackColor = Color.MistyRose
        row.DefaultCellStyle.ForeColor = Color.DarkRed
    End Sub
End Class
