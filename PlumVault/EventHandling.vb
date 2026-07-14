Imports System.Collections.Generic
Imports System.Runtime.InteropServices
Imports System.Windows.Forms
Imports SolidWorks.Interop.sldworks
Imports SolidWorks.Interop.swconst


'Global keyboard hook used to catch Ctrl+W before SOLIDWORKS processes it.
'FileCloseNotify/DestroyNotify are too late for Ctrl+W because SOLIDWORKS has already started the close.
Public Class SolidWorksCtrlWCloseGuardKeyboardHook
    Private Delegate Function KeyboardHookProc(ByVal nCode As Integer, ByVal wParam As IntPtr, ByVal lParam As IntPtr) As IntPtr

    Private Shared hookHandle As IntPtr = IntPtr.Zero
    Private Shared hookCallback As KeyboardHookProc = AddressOf KeyboardProc

    Private Const WH_KEYBOARD As Integer = 2
    Private Const HC_ACTION As Integer = 0
    Private Const VK_W As Integer = &H57
    Private Const VK_CONTROL As Integer = &H11
    Private Const VK_LCONTROL As Integer = &HA2
    Private Const VK_RCONTROL As Integer = &HA3

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function SetWindowsHookEx(ByVal idHook As Integer,
                                                 ByVal lpfn As KeyboardHookProc,
                                                 ByVal hMod As IntPtr,
                                                 ByVal dwThreadId As UInteger) As IntPtr
    End Function

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function CallNextHookEx(ByVal hhk As IntPtr,
                                               ByVal nCode As Integer,
                                               ByVal wParam As IntPtr,
                                               ByVal lParam As IntPtr) As IntPtr
    End Function

    <DllImport("user32.dll")>
    Private Shared Function GetKeyState(ByVal nVirtKey As Integer) As Short
    End Function

    <DllImport("kernel32.dll")>
    Private Shared Function GetCurrentThreadId() As UInteger
    End Function

    Public Shared Sub Install()
        If hookHandle <> IntPtr.Zero Then Exit Sub

        Try
            hookHandle = SetWindowsHookEx(
                    WH_KEYBOARD,
                    hookCallback,
                    IntPtr.Zero,
                    GetCurrentThreadId()
                )
        Catch
            hookHandle = IntPtr.Zero
        End Try
    End Sub

    Private Shared Function IsKeyCurrentlyDown(ByVal virtualKey As Integer) As Boolean
        Try
            Return (GetKeyState(virtualKey) And &H8000) <> 0
        Catch
            Return False
        End Try
    End Function

    Private Shared Function KeyboardProc(ByVal nCode As Integer, ByVal wParam As IntPtr, ByVal lParam As IntPtr) As IntPtr
        Try
            If nCode = HC_ACTION Then
                Dim virtualKey As Integer = wParam.ToInt32()

                If virtualKey = VK_W Then
                    Dim lParamValue As Long = lParam.ToInt64()
                    Dim isKeyRelease As Boolean = ((lParamValue And &H80000000L) <> 0)

                    If Not isKeyRelease Then
                        Dim ctrlDown As Boolean =
                                IsKeyCurrentlyDown(VK_CONTROL) OrElse
                                IsKeyCurrentlyDown(VK_LCONTROL) OrElse
                                IsKeyCurrentlyDown(VK_RCONTROL)

                        If ctrlDown Then
                            If svnModule.blockCloseIfActiveDocUnsafe() Then
                                Return New IntPtr(1) 'Eat Ctrl+W, preventing SOLIDWORKS from closing the document.
                            End If
                        End If
                    End If
                End If
            End If

        Catch
        End Try

        Return CallNextHookEx(hookHandle, nCode, wParam, lParam)
    End Function
End Class

'Base class for model event handlers
Public Class DocumentEventHandler
    Protected openModelViews As New Hashtable()
    Protected userAddin As SwAddin
    Protected iDocument As ModelDoc2
    Protected iSwApp As SldWorks

    Protected Function IsThisDocumentFile(ByVal fileName As String) As Boolean
        If iDocument Is Nothing Then Return False

        Dim docPath As String = ""
        Dim docTitle As String = ""

        Try
            docPath = iDocument.GetPathName()
        Catch
            docPath = ""
        End Try

        Try
            docTitle = iDocument.GetTitle()
        Catch
            docTitle = ""
        End Try

        If String.IsNullOrWhiteSpace(fileName) Then Return True

        Try
            If Not String.IsNullOrWhiteSpace(docPath) Then
                If String.Equals(System.IO.Path.GetFullPath(docPath),
                                     System.IO.Path.GetFullPath(fileName),
                                     StringComparison.OrdinalIgnoreCase) Then
                    Return True
                End If

                If String.Equals(System.IO.Path.GetFileName(docPath),
                                     System.IO.Path.GetFileName(fileName),
                                     StringComparison.OrdinalIgnoreCase) Then
                    Return True
                End If
            End If
        Catch
        End Try

        If Not String.IsNullOrWhiteSpace(docTitle) Then
            If String.Equals(docTitle, fileName, StringComparison.OrdinalIgnoreCase) Then
                Return True
            End If
        End If

        Return False
    End Function

    Overridable Function Init(ByVal sw As SldWorks, ByVal addin As SwAddin, ByVal model As ModelDoc2) As Boolean
    End Function

    Overridable Function AttachEventHandlers() As Boolean
    End Function

    Overridable Function DetachEventHandlers() As Boolean
    End Function

    Function ConnectModelViews() As Boolean
        Dim iModelView As ModelView
        iModelView = iDocument.GetFirstModelView()

        While (Not iModelView Is Nothing)
            If Not openModelViews.Contains(iModelView) Then
                Dim mView As New DocView()
                mView.Init(userAddin, iModelView, Me)
                mView.AttachEventHandlers()
                openModelViews.Add(iModelView, mView)
            End If

            iModelView = iModelView.GetNext
        End While
    End Function

    Function DisconnectModelViews() As Boolean
        'Close events on all currently open docs
        Dim mView As DocView
        Dim key As ModelView
        Dim numKeys As Integer
        numKeys = openModelViews.Count
        Dim keys() As Object = New Object(numKeys - 1) {}

        'Remove all ModelView event handlers
        openModelViews.Keys.CopyTo(keys, 0)

        For Each key In keys
            mView = openModelViews.Item(key)
            mView.DetachEventHandlers()
            openModelViews.Remove(key)
            mView = Nothing
            key = Nothing
        Next
    End Function

    Sub DetachModelViewEventHandler(ByVal mView As ModelView)
        Dim docView As DocView

        If openModelViews.Contains(mView) Then
            docView = openModelViews.Item(mView)
            openModelViews.Remove(mView)
            mView = Nothing
            docView = Nothing
        End If
    End Sub
End Class

'Class to listen for Part Events
Public Class PartEventHandler
    Inherits DocumentEventHandler

    Dim WithEvents iPart As PartDoc
    Dim swAddin As SwAddin

    Overrides Function Init(ByVal sw As SldWorks, ByVal addin As SwAddin, ByVal model As ModelDoc2) As Boolean
        userAddin = addin
        iPart = model
        iDocument = iPart
        iSwApp = sw
        swAddin = addin
    End Function

    Overrides Function AttachEventHandlers() As Boolean
        AddHandler iPart.DestroyNotify, AddressOf Me.PartDoc_DestroyNotify
        AddHandler iPart.FileSaveNotify, AddressOf Me.PartDoc_FileSaveNotify
        AddHandler iPart.FileSaveAsNotify2, AddressOf Me.PartDoc_FileSaveAsNotify2
        AddHandler iPart.FileSavePostNotify, AddressOf Me.PartDoc_FileSavePostNotify
        AddHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify

        'AddHandler iPart.NewSelectionNotify, AddressOf Me.PartDoc_NewSelectionNotify
        'AddHandler iSwApp.ActiveModelDocChangeNotify, AddressOf Me.PartDoc_ActiveModelDocChangeNotify
        'AddHandler iSwApp.FileOpenPostNotify, AddressOf Me.PartDoc_FileOpenPostNotify

        SolidWorksCtrlWCloseGuardKeyboardHook.Install()
        ConnectModelViews()
    End Function

    Overrides Function DetachEventHandlers() As Boolean
        RemoveHandler iPart.DestroyNotify, AddressOf Me.PartDoc_DestroyNotify
        RemoveHandler iPart.FileSaveNotify, AddressOf Me.PartDoc_FileSaveNotify
        RemoveHandler iPart.FileSaveAsNotify2, AddressOf Me.PartDoc_FileSaveAsNotify2
        RemoveHandler iPart.FileSavePostNotify, AddressOf Me.PartDoc_FileSavePostNotify
        RemoveHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify

        'RemoveHandler iPart.NewSelectionNotify, AddressOf Me.PartDoc_NewSelectionNotify
        'RemoveHandler iSwApp.ActiveModelDocChangeNotify, AddressOf Me.PartDoc_ActiveModelDocChangeNotify
        'RemoveHandler iSwApp.FileOpenPostNotify, AddressOf Me.PartDoc_FileOpenPostNotify

        DisconnectModelViews()

        userAddin.DetachModelEventHandler(iDocument)
    End Function

    Function PartDoc_FileOpenPostNotify() As Integer

    End Function

    Function PartDoc_ActiveModelDocChangeNotify() As Integer

        'THIS CODE WILL BE RUN 1X THE NUMBER OF OPEN PARTS IN YOUR ASSEMBLY EACH TIME THE WINDOW CHANGES

        'Dim UC1 As UserControl1 = swAddin.myTaskPaneHost
        'Dim modDoc As ModelDoc2 = iSwApp.ActiveDoc
        'Dim status As UserControl1.SVNStatus

        'status = UC1.getFileSVNStatus(bCheckServer:=False, UC1.getComponentsOfAssemblyOptionalUpdateTree(modDoc))
        'UC1.getComponentsOfAssemblyOptionalUpdateTree(modDoc, status)

        'swAddin.myTaskPaneHost.switchTreeViewToCurrentModel()
    End Function

    Private Function PartDoc_FileSaveNotify(ByVal FileName As String) As Integer
        Return svnModule.handleSolidWorksFileSavePrePublic(iDocument, FileName, isSaveAs:=False)
    End Function

    Private Function PartDoc_FileSaveAsNotify2(ByVal FileName As String) As Integer
        Return svnModule.handleSolidWorksFileSavePrePublic(iDocument, FileName, isSaveAs:=True)
    End Function

    Private Function PartDoc_FileSavePostNotify(ByVal saveType As Integer, ByVal FileName As String) As Integer
        Return svnModule.handleSolidWorksFileSavePostPublic(iDocument, saveType, FileName)
    End Function

    Private Function SwApp_FileCloseNotify(ByVal FileName As String, ByVal Reason As Integer) As Integer

        If Not IsThisDocumentFile(FileName) Then
            Return 0
        End If

        If svnModule.blockCloseIfSingleDocUnsafe(iDocument) Then
            Return 1 'Cancel close
        End If

        Return 0 'Allow close
    End Function

    Function PartDoc_DestroyNotify() As Integer

        If svnModule.blockCloseIfSingleDocUnsafe(iDocument) Then
            Return 1 'Cancel close
        End If

        DetachEventHandlers()
        Return 0 'Allow close
    End Function

    Function PartDoc_NewSelectionNotify() As Integer

    End Function
End Class

'Class to listen for Assembly Events
Public Class AssemblyEventHandler
    Inherits DocumentEventHandler

    Dim WithEvents iAssembly As AssemblyDoc
    Dim swAddin As SwAddin

    Overrides Function Init(ByVal sw As SldWorks, ByVal addin As SwAddin, ByVal model As ModelDoc2) As Boolean
        userAddin = addin
        iAssembly = model
        iDocument = iAssembly
        iSwApp = sw
        swAddin = addin
    End Function

    Overrides Function AttachEventHandlers() As Boolean
        AddHandler iAssembly.DestroyNotify, AddressOf Me.AssemblyDoc_DestroyNotify
        AddHandler iAssembly.FileSaveNotify, AddressOf Me.AssemblyDoc_FileSaveNotify
        AddHandler iAssembly.FileSaveAsNotify2, AddressOf Me.AssemblyDoc_FileSaveAsNotify2
        AddHandler iAssembly.FileSavePostNotify, AddressOf Me.AssemblyDoc_FileSavePostNotify
        AddHandler iAssembly.NewSelectionNotify, AddressOf Me.AssemblyDoc_NewSelectionNotify
        AddHandler iAssembly.ComponentStateChangeNotify, AddressOf Me.AssemblyDoc_ComponentStateChangeNotify
        AddHandler iAssembly.ComponentStateChangeNotify2, AddressOf Me.AssemblyDoc_ComponentStateChangeNotify2
        AddHandler iAssembly.ComponentVisualPropertiesChangeNotify, AddressOf Me.AssemblyDoc_ComponentVisiblePropertiesChangeNotify
        AddHandler iAssembly.ComponentDisplayStateChangeNotify, AddressOf Me.AssemblyDoc_ComponentDisplayStateChangeNotify
        AddHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify

        'AddHandler iSwApp.ActiveModelDocChangeNotify, AddressOf Me.AssemblyDoc_ActiveModelDocChangeNotify
        'AddHandler iSwApp.FileCloseNotify, AddressOf Me.DSldWorksEvents_FileCloseNotifyEventHandler

        SolidWorksCtrlWCloseGuardKeyboardHook.Install()
        ConnectModelViews()
    End Function

    Overrides Function DetachEventHandlers() As Boolean
        RemoveHandler iAssembly.DestroyNotify, AddressOf Me.AssemblyDoc_DestroyNotify
        RemoveHandler iAssembly.FileSaveNotify, AddressOf Me.AssemblyDoc_FileSaveNotify
        RemoveHandler iAssembly.FileSaveAsNotify2, AddressOf Me.AssemblyDoc_FileSaveAsNotify2
        RemoveHandler iAssembly.FileSavePostNotify, AddressOf Me.AssemblyDoc_FileSavePostNotify
        RemoveHandler iAssembly.NewSelectionNotify, AddressOf Me.AssemblyDoc_NewSelectionNotify
        RemoveHandler iAssembly.ComponentStateChangeNotify, AddressOf Me.AssemblyDoc_ComponentStateChangeNotify
        RemoveHandler iAssembly.ComponentStateChangeNotify2, AddressOf Me.AssemblyDoc_ComponentStateChangeNotify2
        RemoveHandler iAssembly.ComponentVisualPropertiesChangeNotify, AddressOf Me.AssemblyDoc_ComponentVisiblePropertiesChangeNotify
        RemoveHandler iAssembly.ComponentDisplayStateChangeNotify, AddressOf Me.AssemblyDoc_ComponentDisplayStateChangeNotify
        RemoveHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify

        'RemoveHandler iSwApp.ActiveModelDocChangeNotify, AddressOf Me.AssemblyDoc_ActiveModelDocChangeNotify
        'RemoveHandler iSwApp.FileCloseNotify, AddressOf Me.DSldWorksEvents_FileCloseNotifyEventHandler

        DisconnectModelViews()

        userAddin.DetachModelEventHandler(iDocument)
    End Function

    Function AssemblyDoc_ActiveModelDocChangeNotify() As Integer
        'This code will be run 1X the number of assemblies open = RUN SO MANY TIMES
    End Function

    Function DSldWorksEvents_FileOpenPostNotifyEventHandler() As Integer

    End Function

    Private Function AssemblyDoc_FileSaveNotify(ByVal FileName As String) As Integer
        Return svnModule.handleSolidWorksFileSavePrePublic(iDocument, FileName, isSaveAs:=False)
    End Function

    Private Function AssemblyDoc_FileSaveAsNotify2(ByVal FileName As String) As Integer
        Return svnModule.handleSolidWorksFileSavePrePublic(iDocument, FileName, isSaveAs:=True)
    End Function

    Private Function AssemblyDoc_FileSavePostNotify(ByVal saveType As Integer, ByVal FileName As String) As Integer
        Return svnModule.handleSolidWorksFileSavePostPublic(iDocument, saveType, FileName)
    End Function

    Function AssemblyDoc_DestroyNotify() As Integer

        If svnModule.blockCloseIfSingleDocUnsafe(iDocument) Then
            Return 1 'Cancel close
        End If

        DetachEventHandlers()
        Return 0 'Allow close
    End Function

    Function AssemblyDoc_NewSelectionNotify() As Integer

    End Function

    Private Function SwApp_FileCloseNotify(ByVal FileName As String, ByVal Reason As Integer) As Integer

        If Not IsThisDocumentFile(FileName) Then
            Return 0
        End If

        If svnModule.blockCloseIfSingleDocUnsafe(iDocument) Then
            Return 1 'Cancel close
        End If

        Return 0 'Allow close
    End Function

    Protected Function ComponentStateChange(ByVal componentModel As Object, Optional ByVal newCompState As Short = swComponentSuppressionState_e.swComponentResolved) As Integer

        Dim modDoc As ModelDoc2 = componentModel
        Dim newState As swComponentSuppressionState_e = newCompState

        Select Case newState

            Case swComponentSuppressionState_e.swComponentFullyResolved, swComponentSuppressionState_e.swComponentResolved

                If ((Not modDoc Is Nothing) AndAlso Not Me.swAddin.OpenDocumentsTable.Contains(modDoc)) Then
                    Me.swAddin.AttachModelDocEventHandler(modDoc)
                End If

                Exit Select

        End Select

    End Function

    'attach events to a component if it becomes resolved
    Public Function AssemblyDoc_ComponentStateChangeNotify(ByVal componentModel As Object, ByVal oldCompState As Short, ByVal newCompState As Short) As Integer

        Return ComponentStateChange(componentModel, newCompState)

    End Function

    'attach events to a component if it becomes resolved
    Public Function AssemblyDoc_ComponentStateChangeNotify2(ByVal componentModel As Object, ByVal CompName As String, ByVal oldCompState As Short, ByVal newCompState As Short) As Integer

        Return ComponentStateChange(componentModel, newCompState)

    End Function

    Public Function AssemblyDoc_ComponentVisiblePropertiesChangeNotify(ByVal swObject As Object) As Integer

        Dim component As Component2
        Dim modDoc As ModelDoc2

        component = swObject

        modDoc = component.GetModelDoc

        Return ComponentStateChange(modDoc)

    End Function

    'Public Function DSldWorksEvents_FileCloseNotifyEventHandler(ByVal FileName As System.String, ByVal reason As System.Int32) As Integer

    '    Dim UC1 As UserControl1 = swAddin.myTaskPaneHost
    '    iSwApp.SendMsgToUser2("File closed", swMessageBoxIcon_e.swMbInformation, swMessageBoxBtn_e.swMbOk)

    '    If iSwApp.ActiveDoc = Nothing Then
    '        UC1.allTreeViews = Nothing
    '        UC1.TreeView1.Nodes.Clear()
    '    Else
    '        'Try
    '        '    svnAddInUtils.DeleteTreeViewAt(UC1.findStoredTreeView(FileName), UC1.allTreeViews)
    '        'Catch
    '        'End Try
    '    End If

    '    'allTreeViews(findStoredTreeView(modDoc.GetPathName, bRetryWithRefresh)).Nodes(0)
    'End Function

    Public Function AssemblyDoc_ComponentDisplayStateChangeNotify(ByVal swObject As Object) As Integer

        Dim component As Component2
        Dim modDoc As ModelDoc2

        component = swObject

        modDoc = component.GetModelDoc

        Return ComponentStateChange(modDoc)

    End Function
End Class

'Class to listen for Drawing Events
Public Class DrawingEventHandler
    Inherits DocumentEventHandler

    Dim WithEvents iDrawing As DrawingDoc

    Overrides Function Init(ByVal sw As SldWorks, ByVal addin As SwAddin, ByVal model As ModelDoc2) As Boolean
        userAddin = addin
        iDrawing = model
        iDocument = iDrawing
        iSwApp = sw
    End Function

    Overrides Function AttachEventHandlers() As Boolean
        AddHandler iDrawing.DestroyNotify, AddressOf Me.DrawingDoc_DestroyNotify
        AddHandler iDrawing.FileSaveNotify, AddressOf Me.DrawingDoc_FileSaveNotify
        AddHandler iDrawing.FileSaveAsNotify2, AddressOf Me.DrawingDoc_FileSaveAsNotify2
        AddHandler iDrawing.FileSavePostNotify, AddressOf Me.DrawingDoc_FileSavePostNotify
        AddHandler iDrawing.NewSelectionNotify, AddressOf Me.DrawingDoc_NewSelectionNotify
        AddHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify

        SolidWorksCtrlWCloseGuardKeyboardHook.Install()
        ConnectModelViews()
    End Function

    Overrides Function DetachEventHandlers() As Boolean
        RemoveHandler iDrawing.DestroyNotify, AddressOf Me.DrawingDoc_DestroyNotify
        RemoveHandler iDrawing.FileSaveNotify, AddressOf Me.DrawingDoc_FileSaveNotify
        RemoveHandler iDrawing.FileSaveAsNotify2, AddressOf Me.DrawingDoc_FileSaveAsNotify2
        RemoveHandler iDrawing.FileSavePostNotify, AddressOf Me.DrawingDoc_FileSavePostNotify
        RemoveHandler iDrawing.NewSelectionNotify, AddressOf Me.DrawingDoc_NewSelectionNotify
        RemoveHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify

        DisconnectModelViews()

        userAddin.DetachModelEventHandler(iDocument)
    End Function

    Private Function DrawingDoc_FileSaveNotify(ByVal FileName As String) As Integer
        Return svnModule.handleSolidWorksFileSavePrePublic(iDocument, FileName, isSaveAs:=False)
    End Function

    Private Function DrawingDoc_FileSaveAsNotify2(ByVal FileName As String) As Integer
        Return svnModule.handleSolidWorksFileSavePrePublic(iDocument, FileName, isSaveAs:=True)
    End Function

    Private Function DrawingDoc_FileSavePostNotify(ByVal saveType As Integer, ByVal FileName As String) As Integer
        Return svnModule.handleSolidWorksFileSavePostPublic(iDocument, saveType, FileName)
    End Function

    Function DrawingDoc_DestroyNotify() As Integer

        If svnModule.blockCloseIfSingleDocUnsafe(iDocument) Then
            Return 1 'Cancel close
        End If

        DetachEventHandlers()
        Return 0 'Allow close
    End Function

    Function DrawingDoc_NewSelectionNotify() As Integer

    End Function

    Private Function SwApp_FileCloseNotify(ByVal FileName As String, ByVal Reason As Integer) As Integer

        If Not IsThisDocumentFile(FileName) Then
            Return 0
        End If

        If svnModule.blockCloseIfSingleDocUnsafe(iDocument) Then
            Return 1 'Cancel close
        End If

        Return 0 'Allow close
    End Function
End Class

'Class for handling ModelView events
Public Class DocView

    Dim WithEvents iModelView As ModelView
    Dim userAddin As SwAddin
    Dim parentDoc As DocumentEventHandler
    Dim docWindowCloseGuards As New List(Of SolidWorksDocumentCloseGuardWindowHook)

    Function Init(ByVal addin As SwAddin, ByVal mView As ModelView, ByVal parent As DocumentEventHandler) As Boolean
        userAddin = addin
        iModelView = mView
        parentDoc = parent
    End Function

    Function AttachEventHandlers() As Boolean
        AddHandler iModelView.DestroyNotify2, AddressOf Me.ModelView_DestroyNotify2
        AddHandler iModelView.RepaintNotify, AddressOf Me.ModelView_RepaintNotify

        Try
            Dim hwnd As IntPtr = IntPtr.Zero

            Try
                hwnd = New IntPtr(Convert.ToInt64(iModelView.GetViewHWnd()))
            Catch
                hwnd = IntPtr.Zero
            End Try

            If hwnd <> IntPtr.Zero Then
                HookDocumentWindowAndParents(hwnd)
            End If

        Catch
            docWindowCloseGuards.Clear()
        End Try
    End Function

    Private Sub HookDocumentWindowAndParents(startHwnd As IntPtr)
        Dim currentHwnd As IntPtr = startHwnd
        Dim hookedHandles As New HashSet(Of IntPtr)()

        'Walk upward through the model view parent windows.
        'The little document X is usually on one of these parent MDI/document windows.
        For i As Integer = 0 To 8
            If currentHwnd = IntPtr.Zero Then Exit For

            If Not hookedHandles.Contains(currentHwnd) Then
                Dim hook As New SolidWorksDocumentCloseGuardWindowHook()
                hook.AssignSolidWorksDocumentHandle(currentHwnd)
                docWindowCloseGuards.Add(hook)
                hookedHandles.Add(currentHwnd)
            End If

            currentHwnd = SolidWorksDocumentCloseGuardWindowHook.GetParentWindow(currentHwnd)
        Next
    End Sub

    Function DetachEventHandlers() As Boolean
        RemoveHandler iModelView.DestroyNotify2, AddressOf Me.ModelView_DestroyNotify2
        RemoveHandler iModelView.RepaintNotify, AddressOf Me.ModelView_RepaintNotify

        Try
            For Each hook As SolidWorksDocumentCloseGuardWindowHook In docWindowCloseGuards
                If hook Is Nothing Then Continue For
                hook.ReleaseSolidWorksDocumentHandle()
            Next

            docWindowCloseGuards.Clear()
        Catch
            docWindowCloseGuards.Clear()
        End Try

        parentDoc.DetachModelViewEventHandler(iModelView)
    End Function

    Public Class SolidWorksDocumentCloseGuardWindowHook
        Inherits NativeWindow

        Private Const WM_CLOSE As Integer = &H10
        Private Const WM_SYSCOMMAND As Integer = &H112
        Private Const WM_KEYDOWN As Integer = &H100
        Private Const WM_SYSKEYDOWN As Integer = &H104
        Private Const SC_CLOSE As Integer = &HF060

        <DllImport("user32.dll", SetLastError:=True)>
        Private Shared Function GetParent(ByVal hWnd As IntPtr) As IntPtr
        End Function

        Public Shared Function GetParentWindow(hwnd As IntPtr) As IntPtr
            Try
                Return GetParent(hwnd)
            Catch
                Return IntPtr.Zero
            End Try
        End Function

        Public Sub AssignSolidWorksDocumentHandle(hwnd As IntPtr)
            If hwnd = IntPtr.Zero Then Exit Sub
            Me.AssignHandle(hwnd)
        End Sub

        Public Sub ReleaseSolidWorksDocumentHandle()
            Try
                Me.ReleaseHandle()
            Catch
            End Try
        End Sub

        Protected Overrides Sub WndProc(ByRef m As Message)

            'Catch Ctrl+W before SolidWorks processes it as a document close shortcut.
            If m.Msg = WM_KEYDOWN OrElse m.Msg = WM_SYSKEYDOWN Then
                Try
                    Dim keyCode As Integer = m.WParam.ToInt32()

                    If keyCode = CInt(Keys.W) AndAlso
                           ((Control.ModifierKeys And Keys.Control) = Keys.Control) Then

                        If svnModule.blockCloseIfActiveDocUnsafe() Then
                            Return
                        End If
                    End If
                Catch
                End Try
            End If

            If m.Msg = WM_CLOSE Then
                If svnModule.blockCloseIfActiveDocUnsafe() Then
                    Return
                End If
            End If

            If m.Msg = WM_SYSCOMMAND Then
                Dim command As Integer = m.WParam.ToInt32() And &HFFF0

                If command = SC_CLOSE Then
                    If svnModule.blockCloseIfActiveDocUnsafe() Then
                        Return
                    End If
                End If
            End If

            MyBase.WndProc(m)
        End Sub
    End Class

    Function ModelView_DestroyNotify2(ByVal destroyTYpe As Integer) As Integer
        DetachEventHandlers()
    End Function

    Function ModelView_RepaintNotify(ByVal repaintTYpe As Integer) As Integer

    End Function
End Class