Imports System
Imports System.Collections.Generic
Imports System.Drawing
Imports System.IO
Imports System.Linq
Imports System.Text.RegularExpressions
Imports System.Windows.Forms

Public Enum ExternalReferenceImportTargetType
    GrcCad
    VendorPart
End Enum

Public Class ExternalReferenceImportItem
    Public Property SourcePath As String = ""
    Public Property TargetType As ExternalReferenceImportTargetType = ExternalReferenceImportTargetType.GrcCad
    Public Property ProposedId As String = ""
    Public Property DestinationFolder As String = ""
    Public Property FinalFileName As String = ""
    Public Property DestinationPath As String = ""
    Public Property ReuseExistingPath As String = ""
    Public Property IsChecked As Boolean = False
    Public Property IsValid As Boolean = False
    Public Property ValidationMessage As String = ""

    Public ReadOnly Property OriginalFileName As String
        Get
            Try
                Return Path.GetFileName(SourcePath)
            Catch
                Return SourcePath
            End Try
        End Get
    End Property

    Public ReadOnly Property Extension As String
        Get
            Try
                Return Path.GetExtension(SourcePath).ToUpperInvariant()
            Catch
                Return ""
            End Try
        End Get
    End Property

    Public ReadOnly Property SourceTypeText As String
        Get
            Select Case Extension
                Case ".SLDPRT"
                    Return "Part"
                Case ".SLDASM"
                    Return "Assembly"
                Case ".SLDDRW"
                    Return "Drawing"
                Case Else
                    Return "CAD"
            End Select
        End Get
    End Property

    Public ReadOnly Property CanBeVendorPart As Boolean
        Get
            Return String.Equals(Extension, ".SLDPRT", StringComparison.OrdinalIgnoreCase)
        End Get
    End Property
End Class

Public Class ExternalReferenceImportPlan
    Public Property LocalRepoRootFolder As String = ""
    Public Property DefaultGrcDestinationFolder As String = ""
    Public Property VendorRootFolder As String = ""
    Public Property Items As New List(Of ExternalReferenceImportItem)()
End Class

Public Class ExternalReferenceImportForm
    Inherits Form

    Private ReadOnly _plan As ExternalReferenceImportPlan
    Private ReadOnly _grid As New DataGridView()
    Private ReadOnly _lblInstruction As New Label()
    Private ReadOnly _lblWorkingCopy As New Label()
    Private ReadOnly _lblSummary As New Label()
    Private ReadOnly _btnCheckAll As New Button()
    Private ReadOnly _btnNextProblem As New Button()
    Private ReadOnly _btnReturn As New Button()
    Private ReadOnly _btnContinue As New Button()
    Private _suppressEvents As Boolean = False

    Private Const COL_OLD_NAME As String = "OldName"
    Private Const COL_ORIGINAL_PATH As String = "OriginalPath"
    Private Const COL_SOURCE_TYPE As String = "SourceType"
    Private Const COL_TARGET_TYPE As String = "TargetType"
    Private Const COL_NEW_ID As String = "NewId"
    Private Const COL_DESTINATION As String = "Destination"
    Private Const COL_BROWSE As String = "Browse"
    Private Const COL_FINAL_NAME As String = "FinalName"
    Private Const COL_CHECK As String = "Check"
    Private Const COL_STATUS As String = "Status"
    Private Const COL_EXPLANATION As String = "Explanation"

    Public Sub New(ByVal plan As ExternalReferenceImportPlan)
        If plan Is Nothing Then Throw New ArgumentNullException(NameOf(plan))

        _plan = plan

        Me.Text = "Copy Referenced CAD into SVN"
        Me.StartPosition = FormStartPosition.CenterParent
        Me.MinimumSize = New Size(1120, 620)
        Me.Size = New Size(1500, 780)
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
        _lblInstruction.MaximumSize = New Size(1420, 0)
        _lblInstruction.Font = New Font("Segoe UI", 10.0F, FontStyle.Regular)
        _lblInstruction.Text =
            "This assembly references CAD outside the SVN working copy. Review every file before PlumVault copies or relinks anything. " &
            "A source Part may remain GRC CAD or be changed to Vendor Part; assemblies and drawings remain GRC CAD. " &
            "Enter a new GRC27/CFD27 ID or vendor filename, choose the destination folder, then press Check. " &
            "Missing destination folders are created, added, and committed before the CAD file is copied."
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
        configureActionButton(_btnContinue, "Continue")
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
            .Name = COL_OLD_NAME,
            .HeaderText = "Old file name",
            .ReadOnly = True,
            .Width = 165
        })

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_ORIGINAL_PATH,
            .HeaderText = "Original path",
            .ReadOnly = True,
            .Width = 235
        })

        _grid.Columns.Add(New DataGridViewTextBoxColumn() With {
            .Name = COL_SOURCE_TYPE,
            .HeaderText = "Source type",
            .ReadOnly = True,
            .Width = 78
        })

        Dim targetColumn As New DataGridViewComboBoxColumn()
        targetColumn.Name = COL_TARGET_TYPE
        targetColumn.HeaderText = "Import type"
        targetColumn.Width = 105
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
            .Width = 235
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
            .MinimumWidth = 210
        })

        AddHandler _grid.CellContentClick, AddressOf gridCellContentClick
        AddHandler _grid.CellValueChanged, AddressOf gridCellValueChanged
        AddHandler _grid.CurrentCellDirtyStateChanged, AddressOf gridCurrentCellDirtyStateChanged
        AddHandler _grid.DataError, AddressOf gridDataError
    End Sub

    Private Sub populateRows()
        _suppressEvents = True

        Try
            For Each item As ExternalReferenceImportItem In _plan.Items
                If item Is Nothing Then Continue For

                If String.IsNullOrWhiteSpace(item.DestinationFolder) Then
                    item.DestinationFolder = If(item.TargetType = ExternalReferenceImportTargetType.VendorPart,
                                                _plan.VendorRootFolder,
                                                _plan.DefaultGrcDestinationFolder)
                End If

                If String.IsNullOrWhiteSpace(item.ProposedId) Then
                    item.ProposedId = Path.GetFileNameWithoutExtension(item.OriginalFileName)
                End If

                Dim rowIndex As Integer = _grid.Rows.Add()
                Dim row As DataGridViewRow = _grid.Rows(rowIndex)
                row.Tag = item

                row.Cells(COL_OLD_NAME).Value = item.OriginalFileName
                row.Cells(COL_ORIGINAL_PATH).Value = item.SourcePath
                row.Cells(COL_SOURCE_TYPE).Value = item.SourceTypeText
                row.Cells(COL_TARGET_TYPE).Value = targetTypeText(item.TargetType)
                row.Cells(COL_NEW_ID).Value = item.ProposedId
                row.Cells(COL_DESTINATION).Value = item.DestinationFolder
                row.Cells(COL_FINAL_NAME).Value = ""
                row.Cells(COL_STATUS).Value = "—"
                row.Cells(COL_EXPLANATION).Value = "Press Check after reviewing this row."

                If Not item.CanBeVendorPart Then
                    row.Cells(COL_TARGET_TYPE).ReadOnly = True
                    row.Cells(COL_TARGET_TYPE).Style.BackColor = SystemColors.Control
                End If

                applyPendingRowStyle(row)
            Next
        Finally
            _suppressEvents = False
        End Try
    End Sub

    Private Function targetTypeText(ByVal targetType As ExternalReferenceImportTargetType) As String
        If targetType = ExternalReferenceImportTargetType.VendorPart Then Return "Vendor Part"
        Return "GRC CAD"
    End Function

    Private Function targetTypeFromText(ByVal value As Object) As ExternalReferenceImportTargetType
        If value IsNot Nothing AndAlso String.Equals(CStr(value), "Vendor Part", StringComparison.OrdinalIgnoreCase) Then
            Return ExternalReferenceImportTargetType.VendorPart
        End If

        Return ExternalReferenceImportTargetType.GrcCad
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

        If columnName <> COL_TARGET_TYPE AndAlso
           columnName <> COL_NEW_ID AndAlso
           columnName <> COL_DESTINATION Then
            Exit Sub
        End If

        Dim row As DataGridViewRow = _grid.Rows(e.RowIndex)
        Dim item As ExternalReferenceImportItem = TryCast(row.Tag, ExternalReferenceImportItem)
        If item Is Nothing Then Exit Sub

        If columnName = COL_TARGET_TYPE Then
            Dim requestedType As ExternalReferenceImportTargetType = targetTypeFromText(row.Cells(COL_TARGET_TYPE).Value)

            If requestedType = ExternalReferenceImportTargetType.VendorPart AndAlso Not item.CanBeVendorPart Then
                requestedType = ExternalReferenceImportTargetType.GrcCad
                _suppressEvents = True
                row.Cells(COL_TARGET_TYPE).Value = "GRC CAD"
                _suppressEvents = False
            End If

            item.TargetType = requestedType

            If requestedType = ExternalReferenceImportTargetType.VendorPart Then
                row.Cells(COL_DESTINATION).Value = _plan.VendorRootFolder
                If String.IsNullOrWhiteSpace(CStr(row.Cells(COL_NEW_ID).Value)) Then
                    row.Cells(COL_NEW_ID).Value = Path.GetFileNameWithoutExtension(item.OriginalFileName)
                End If
            ElseIf String.IsNullOrWhiteSpace(CStr(row.Cells(COL_DESTINATION).Value)) OrElse
                   pathContainsNamedFolderSegment(CStr(row.Cells(COL_DESTINATION).Value), _plan.LocalRepoRootFolder, "Vendor Parts") Then
                row.Cells(COL_DESTINATION).Value = _plan.DefaultGrcDestinationFolder
            End If
        End If

        invalidateRow(row, "Changed — press Check again.")
        updateSummaryAndContinueState()
    End Sub

    Private Sub browseForRow(ByVal rowIndex As Integer)
        If rowIndex < 0 OrElse rowIndex >= _grid.Rows.Count Then Exit Sub

        Dim row As DataGridViewRow = _grid.Rows(rowIndex)
        Dim item As ExternalReferenceImportItem = TryCast(row.Tag, ExternalReferenceImportItem)
        If item Is Nothing Then Exit Sub

        Dim targetType As ExternalReferenceImportTargetType = targetTypeFromText(row.Cells(COL_TARGET_TYPE).Value)
        Dim currentFolder As String = CStr(If(row.Cells(COL_DESTINATION).Value, ""))

        If String.IsNullOrWhiteSpace(currentFolder) OrElse Not Directory.Exists(currentFolder) Then
            currentFolder = If(targetType = ExternalReferenceImportTargetType.VendorPart,
                               _plan.VendorRootFolder,
                               _plan.DefaultGrcDestinationFolder)
        End If

        Using dialog As New FolderBrowserDialog()
            dialog.Description = If(targetType = ExternalReferenceImportTargetType.VendorPart,
                                    "Choose a destination containing a folder named Vendor Parts.",
                                    "Choose a destination inside the SVN working copy for this GRC/CFD file.")
            dialog.SelectedPath = currentFolder
            dialog.ShowNewFolderButton = True

            If dialog.ShowDialog(Me) <> DialogResult.OK Then Exit Sub

            row.Cells(COL_DESTINATION).Value = dialog.SelectedPath
            invalidateRow(row, "Destination changed — press Check again.")
            updateSummaryAndContinueState()
        End Using
    End Sub

    Private Sub validateRow(ByVal rowIndex As Integer)
        If rowIndex < 0 OrElse rowIndex >= _grid.Rows.Count Then Exit Sub

        Try
            _grid.EndEdit()
        Catch
        End Try

        Dim row As DataGridViewRow = _grid.Rows(rowIndex)
        Dim item As ExternalReferenceImportItem = TryCast(row.Tag, ExternalReferenceImportItem)
        If item Is Nothing Then Exit Sub

        item.TargetType = targetTypeFromText(row.Cells(COL_TARGET_TYPE).Value)
        item.ProposedId = CStr(If(row.Cells(COL_NEW_ID).Value, "")).Trim()
        item.DestinationFolder = CStr(If(row.Cells(COL_DESTINATION).Value, "")).Trim()

        Dim message As String = ""
        Dim isValid As Boolean = validateItem(item, message)

        item.IsChecked = True
        item.IsValid = isValid
        item.ValidationMessage = message

        row.Cells(COL_FINAL_NAME).Value = item.FinalFileName
        row.Cells(COL_STATUS).Value = If(isValid, "✓", "✗")
        row.Cells(COL_EXPLANATION).Value = message

        If isValid Then
            applyValidRowStyle(row)
        Else
            applyInvalidRowStyle(row)
        End If

        updateSummaryAndContinueState()
    End Sub

    Private Function validateItem(ByVal item As ExternalReferenceImportItem,
                                  ByRef message As String) As Boolean
        message = ""
        item.FinalFileName = ""
        item.DestinationPath = ""
        item.ReuseExistingPath = ""

        If item Is Nothing Then
            message = "The row is missing its file information."
            Return False
        End If

        If String.IsNullOrWhiteSpace(item.SourcePath) OrElse Not File.Exists(item.SourcePath) Then
            message = "The original referenced file is missing or cannot be accessed."
            Return False
        End If

        If item.TargetType = ExternalReferenceImportTargetType.VendorPart AndAlso Not item.CanBeVendorPart Then
            message = "Only a source Part may be changed to Vendor Part."
            Return False
        End If

        Dim normalizedName As String = normalizeProposedName(item.ProposedId, item.Extension)

        If String.IsNullOrWhiteSpace(normalizedName) Then
            message = If(item.TargetType = ExternalReferenceImportTargetType.VendorPart,
                         "Enter a vendor filename.",
                         "Enter a GRC27 or CFD27 ID.")
            Return False
        End If

        item.FinalFileName = normalizedName & item.Extension

        If item.FinalFileName.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 Then
            message = "The proposed filename contains a Windows-invalid character."
            Return False
        End If

        If String.IsNullOrWhiteSpace(item.DestinationFolder) Then
            message = "Choose a destination folder."
            Return False
        End If

        If Not isSameOrChildPath(item.DestinationFolder, _plan.LocalRepoRootFolder) Then
            message = "The destination must be inside the SVN working copy."
            Return False
        End If

        If item.TargetType = ExternalReferenceImportTargetType.VendorPart Then
            If Not pathContainsNamedFolderSegment(item.DestinationFolder, _plan.LocalRepoRootFolder, "Vendor Parts") Then
                message = "Vendor parts may be saved anywhere inside the working copy, but the path must contain a folder named Vendor Parts."
                Return False
            End If

            Dim existingVendor As String = findExistingVendorPath(item.FinalFileName)

            If Not String.IsNullOrWhiteSpace(existingVendor) Then
                item.ReuseExistingPath = existingVendor
                item.DestinationPath = existingVendor
                item.DestinationFolder = Path.GetDirectoryName(existingVendor)
                message = "An existing SVN Vendor Parts file with this name will be reused and the assembly will be relinked to it."
                'Several assembly references may legitimately reuse the same canonical vendor file.
                Return True
            End If
        Else
            If Not isValidGrcFileName(item.FinalFileName) Then
                message = "Invalid ID. Use GRC27/CFD27, an allowed system code, an ID, and revision; for example GRC27_DT_00001_R1."
                Return False
            End If

            If pathContainsNamedFolderSegment(item.DestinationFolder, _plan.LocalRepoRootFolder, "Vendor Parts") Then
                message = "Normal GRC27/CFD27 CAD cannot be saved inside a Vendor Parts folder."
                Return False
            End If

            Dim existingGrc As String = findExistingRepoFile(item.FinalFileName, excludeVendor:=True)

            If Not String.IsNullOrWhiteSpace(existingGrc) Then
                message = "This GRC/CFD filename already exists in SVN: " & existingGrc
                Return False
            End If
        End If

        item.DestinationPath = Path.Combine(item.DestinationFolder, item.FinalFileName)

        If item.DestinationPath.Length >= 245 Then
            message = "The final Windows path is too long. Choose a shorter folder or ID."
            Return False
        End If

        If File.Exists(item.DestinationPath) OrElse Directory.Exists(item.DestinationPath) Then
            message = "A file or folder already exists at the proposed destination."
            Return False
        End If

        If destinationDuplicatesAnotherRow(item) Then
            message = "Another row is using the same final destination filename."
            Return False
        End If

        If Directory.Exists(item.DestinationFolder) Then
            message = "Valid. The CAD file will be copied and the assembly reference will be updated."
        Else
            message = "Valid. The destination folder will be created, added, and committed before the CAD file is copied."
        End If

        Return True
    End Function

    Private Function normalizeProposedName(ByVal proposed As String, ByVal extension As String) As String
        If String.IsNullOrWhiteSpace(proposed) Then Return ""

        Dim value As String = proposed.Trim()

        If Not String.IsNullOrWhiteSpace(extension) AndAlso value.EndsWith(extension, StringComparison.OrdinalIgnoreCase) Then
            value = value.Substring(0, value.Length - extension.Length)
        End If

        Return value.Trim()
    End Function

    Private Function isValidGrcFileName(ByVal fileName As String) As Boolean
        If String.IsNullOrWhiteSpace(fileName) Then Return False

        Return Regex.IsMatch(
            Path.GetFileName(fileName),
            "^(GRC|CFD)27_(BR|DT|AE|FR|EL|ST|SU|WT|MI)_[A-Z]{0,3}\d+_R\d+\.(SLDPRT|SLDASM|SLDDRW)$",
            RegexOptions.IgnoreCase
        )
    End Function

    Private Function destinationDuplicatesAnotherRow(ByVal currentItem As ExternalReferenceImportItem) As Boolean
        If currentItem Is Nothing OrElse String.IsNullOrWhiteSpace(currentItem.DestinationPath) Then Return False

        For Each otherItem As ExternalReferenceImportItem In _plan.Items
            If otherItem Is Nothing OrElse Object.ReferenceEquals(otherItem, currentItem) Then Continue For
            If String.IsNullOrWhiteSpace(otherItem.DestinationPath) Then Continue For

            If pathsEqual(otherItem.DestinationPath, currentItem.DestinationPath) Then Return True
        Next

        Return False
    End Function

    Private Function findExistingVendorPath(ByVal fileName As String) As String
        If String.IsNullOrWhiteSpace(fileName) Then Return ""
        If String.IsNullOrWhiteSpace(_plan.LocalRepoRootFolder) Then Return ""
        If Not Directory.Exists(_plan.LocalRepoRootFolder) Then Return ""

        Try
            For Each matchPath As String In Directory.GetFiles(_plan.LocalRepoRootFolder, fileName, SearchOption.AllDirectories)
                If pathContainsNamedFolderSegment(matchPath, _plan.LocalRepoRootFolder, "Vendor Parts") Then
                    Return matchPath
                End If
            Next
        Catch
        End Try

        Return ""
    End Function

    Private Function findExistingRepoFile(ByVal fileName As String,
                                          ByVal excludeVendor As Boolean) As String
        If String.IsNullOrWhiteSpace(fileName) Then Return ""
        If String.IsNullOrWhiteSpace(_plan.LocalRepoRootFolder) Then Return ""
        If Not Directory.Exists(_plan.LocalRepoRootFolder) Then Return ""

        Try
            For Each matchPath As String In Directory.GetFiles(_plan.LocalRepoRootFolder, fileName, SearchOption.AllDirectories)
                If excludeVendor AndAlso pathContainsNamedFolderSegment(matchPath, _plan.LocalRepoRootFolder, "Vendor Parts") Then Continue For
                Return matchPath
            Next
        Catch
        End Try

        Return ""
    End Function

    Private Function pathContainsNamedFolderSegment(ByVal candidatePath As String,
                                                    ByVal rootPath As String,
                                                    ByVal requiredFolderName As String) As Boolean
        If String.IsNullOrWhiteSpace(candidatePath) OrElse
           String.IsNullOrWhiteSpace(rootPath) OrElse
           String.IsNullOrWhiteSpace(requiredFolderName) Then
            Return False
        End If

        Try
            Dim root As String = Path.GetFullPath(rootPath).TrimEnd("\"c, "/"c)
            Dim fullPath As String = Path.GetFullPath(candidatePath).TrimEnd("\"c, "/"c)

            If Not isSameOrChildPath(fullPath, root) Then Return False

            Dim relativePath As String = fullPath.Substring(root.Length).TrimStart("\"c, "/"c)
            Dim segments() As String = relativePath.Split(New Char() {"\"c, "/"c}, StringSplitOptions.RemoveEmptyEntries)

            For Each segment As String In segments
                If String.Equals(segment, requiredFolderName, StringComparison.OrdinalIgnoreCase) Then Return True
            Next
        Catch
        End Try

        Return False
    End Function

    Private Function isSameOrChildPath(ByVal candidatePath As String, ByVal rootPath As String) As Boolean
        If String.IsNullOrWhiteSpace(candidatePath) OrElse String.IsNullOrWhiteSpace(rootPath) Then Return False

        Try
            Dim root As String = Path.GetFullPath(rootPath).TrimEnd("\"c, "/"c)
            Dim candidate As String = Path.GetFullPath(candidatePath).TrimEnd("\"c, "/"c)

            If String.Equals(candidate, root, StringComparison.OrdinalIgnoreCase) Then Return True
            Return candidate.StartsWith(root & Path.DirectorySeparatorChar, StringComparison.OrdinalIgnoreCase)
        Catch
            Return False
        End Try
    End Function

    Private Function pathsEqual(ByVal firstPath As String, ByVal secondPath As String) As Boolean
        If String.IsNullOrWhiteSpace(firstPath) OrElse String.IsNullOrWhiteSpace(secondPath) Then Return False

        Try
            Return String.Equals(Path.GetFullPath(firstPath), Path.GetFullPath(secondPath), StringComparison.OrdinalIgnoreCase)
        Catch
            Return String.Equals(firstPath, secondPath, StringComparison.OrdinalIgnoreCase)
        End Try
    End Function

    Private Sub invalidateRow(ByVal row As DataGridViewRow, ByVal explanation As String)
        Dim item As ExternalReferenceImportItem = TryCast(row.Tag, ExternalReferenceImportItem)

        If item IsNot Nothing Then
            item.IsChecked = False
            item.IsValid = False
            item.ValidationMessage = explanation
            item.FinalFileName = ""
            item.DestinationPath = ""
            item.ReuseExistingPath = ""
        End If

        row.Cells(COL_FINAL_NAME).Value = ""
        row.Cells(COL_STATUS).Value = "—"
        row.Cells(COL_EXPLANATION).Value = explanation
        applyPendingRowStyle(row)
    End Sub

    Private Sub applyPendingRowStyle(ByVal row As DataGridViewRow)
        row.DefaultCellStyle.BackColor = Color.LemonChiffon
        row.DefaultCellStyle.SelectionBackColor = Color.Khaki
        row.DefaultCellStyle.SelectionForeColor = Color.Black
        row.Cells(COL_STATUS).Style.ForeColor = Color.DarkGoldenrod
        row.Cells(COL_EXPLANATION).Style.ForeColor = SystemColors.ControlText
    End Sub

    Private Sub applyValidRowStyle(ByVal row As DataGridViewRow)
        row.DefaultCellStyle.BackColor = Color.Honeydew
        row.DefaultCellStyle.SelectionBackColor = Color.PaleGreen
        row.DefaultCellStyle.SelectionForeColor = Color.Black
        row.Cells(COL_STATUS).Style.ForeColor = Color.DarkGreen
        row.Cells(COL_STATUS).Style.Font = New Font("Segoe UI", 10.0F, FontStyle.Bold)
        row.Cells(COL_EXPLANATION).Style.ForeColor = Color.DarkGreen
    End Sub

    Private Sub applyInvalidRowStyle(ByVal row As DataGridViewRow)
        row.DefaultCellStyle.BackColor = Color.MistyRose
        row.DefaultCellStyle.SelectionBackColor = Color.LightSalmon
        row.DefaultCellStyle.SelectionForeColor = Color.Black
        row.Cells(COL_STATUS).Style.ForeColor = Color.DarkRed
        row.Cells(COL_STATUS).Style.Font = New Font("Segoe UI", 10.0F, FontStyle.Bold)
        row.Cells(COL_EXPLANATION).Style.ForeColor = Color.DarkRed
    End Sub

    Private Sub checkAllClicked(sender As Object, e As EventArgs)
        Try
            _grid.EndEdit()
        Catch
        End Try

        For index As Integer = 0 To _grid.Rows.Count - 1
            validateRow(index)
        Next
    End Sub

    Private Sub nextProblemClicked(sender As Object, e As EventArgs)
        If _grid.Rows.Count = 0 Then Exit Sub

        Dim startIndex As Integer = 0
        If _grid.CurrentRow IsNot Nothing Then startIndex = _grid.CurrentRow.Index + 1

        For offset As Integer = 0 To _grid.Rows.Count - 1
            Dim index As Integer = (startIndex + offset) Mod _grid.Rows.Count
            Dim item As ExternalReferenceImportItem = TryCast(_grid.Rows(index).Tag, ExternalReferenceImportItem)

            If item Is Nothing OrElse Not item.IsChecked OrElse Not item.IsValid Then
                _grid.ClearSelection()
                _grid.Rows(index).Selected = True
                _grid.CurrentCell = _grid.Rows(index).Cells(COL_NEW_ID)
                _grid.FirstDisplayedScrollingRowIndex = Math.Max(0, index)
                Exit Sub
            End If
        Next
    End Sub

    Private Sub updateSummaryAndContinueState()
        Dim total As Integer = _plan.Items.Count
        Dim valid As Integer = _plan.Items.FindAll(Function(item As ExternalReferenceImportItem) item IsNot Nothing AndAlso item.IsChecked AndAlso item.IsValid).Count
        Dim invalid As Integer = _plan.Items.FindAll(Function(item As ExternalReferenceImportItem) item Is Nothing OrElse (item.IsChecked AndAlso Not item.IsValid)).Count
        Dim unchecked As Integer = Math.Max(0, total - valid - invalid)

        _lblSummary.Text = "Files: " & total.ToString() &
                           "    Valid: " & valid.ToString() &
                           "    Invalid: " & invalid.ToString() &
                           "    Not checked: " & unchecked.ToString()

        _btnContinue.Enabled = total > 0 AndAlso valid = total
    End Sub

    Private Sub returnClicked(sender As Object, e As EventArgs)
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub continueClicked(sender As Object, e As EventArgs)
        checkAllClicked(sender, e)

        If Not _btnContinue.Enabled Then
            MessageBox.Show(Me,
                            "Every referenced file must have a green check before the copy can continue.",
                            "Referenced CAD is not ready",
                            MessageBoxButtons.OK,
                            MessageBoxIcon.Warning)
            Exit Sub
        End If

        Me.DialogResult = DialogResult.OK
        Me.Close()
    End Sub
End Class
