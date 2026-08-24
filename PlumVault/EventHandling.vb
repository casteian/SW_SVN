Imports System.Collections.Generic
Imports System.Runtime.InteropServices
Imports System.Threading
Imports System.Windows.Forms
Imports SolidWorks.Interop.sldworks
Imports SolidWorks.Interop.swconst


'Coordinates all pre-close entry points so one click/shortcut is evaluated once.
'SOLIDWORKS can emit SC_CLOSE, WM_CLOSE, and nested window messages for the same action.
Friend NotInheritable Class SolidWorksCloseGuardCoordinator
    Private Shared ReadOnly closeCheckSync As New Object()
    Private Shared closeCheckInProgress As Boolean = False
    Private Shared lastDecisionUtc As DateTime = DateTime.MinValue
    Private Shared lastDecisionBlocked As Boolean = False
    Private Const CLOSE_DECISION_REUSE_MILLISECONDS As Double = 150.0

    Private Sub New()
    End Sub

    Public Shared Function ShouldBlockActiveDocumentClose() As Boolean
        SyncLock closeCheckSync
            'A modal close-safety prompt can pump another close message. While the first
            'check is still active, always block the duplicate message.
            If closeCheckInProgress Then Return True

            'SC_CLOSE and WM_CLOSE commonly arrive back-to-back for the same click.
            'Reuse the first decision briefly instead of re-entering SVN/COM checks.
            If (DateTime.UtcNow - lastDecisionUtc).TotalMilliseconds <= CLOSE_DECISION_REUSE_MILLISECONDS Then
                Return lastDecisionBlocked
            End If

            closeCheckInProgress = True
        End SyncLock

        Dim blocked As Boolean = False

        Try
            blocked = svnModule.blockCloseIfActiveDocUnsafe()
        Catch ex As Exception
            'A failed pre-close check is not evidence that the file is safe. Fail closed and
            'give one actionable message instead of allowing the native close/save dialogs to
            'take over after PlumVault lost track of the document state.
            blocked = True

            Try
                MessageBox.Show(
                    "The file close was cancelled because PlumVault could not verify its save and lock state." &
                    vbCrLf & vbCrLf &
                    "Click Sync and try again. If this repeats, disable the PlumVault add-in before closing SOLIDWORKS." &
                    vbCrLf & vbCrLf & ex.Message,
                    "PlumVault could not verify close",
                    MessageBoxButtons.OK,
                    MessageBoxIcon.Warning
                )
            Catch
            End Try
        Finally
            SyncLock closeCheckSync
                lastDecisionBlocked = blocked
                lastDecisionUtc = DateTime.UtcNow
                closeCheckInProgress = False
            End SyncLock
        End Try

        Return blocked
    End Function
End Class


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

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function UnhookWindowsHookEx(ByVal hhk As IntPtr) As Boolean
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

    Public Shared Sub Uninstall()
        Dim handleToRelease As IntPtr = hookHandle
        hookHandle = IntPtr.Zero

        If handleToRelease = IntPtr.Zero Then Exit Sub

        Try
            UnhookWindowsHookEx(handleToRelease)
        Catch
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
                            'Always eat Ctrl+W here and defer the actual safety check off this
                            'raw WH_KEYBOARD callback. Calling ShouldBlockActiveDocumentClose()
                            '(which can synchronously show the review dialog and query SVN)
                            'directly inside a low-level keyboard hook is a known-unsafe pattern -
                            'the hook is expected to return quickly, and a modal dialog shown from
                            'here fights the same message pump that is currently suspended inside
                            'the hook procedure. This was the likely cause of the review table
                            'failing to appear specifically for Ctrl+W, unlike the small-X/
                            'Window-menu-close paths, which run through a normal WndProc override
                            'in SolidWorksDocumentCloseGuardWindowHook and are not affected.
                            svnModule.queueDeferredCtrlWCloseCheckPublic()
                            Return New IntPtr(1) 'Eat Ctrl+W; the deferred check decides what happens next.
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

    Private detachQueued As Boolean = False
    Private detachStarted As Boolean = False

    'SOLIDWORKS close notifications can fire while its native close message is still
    'being processed. Releasing NativeWindow/COM event hooks synchronously in that
    'call stack can destabilize the document window, so cleanup is deferred one UI turn.
    Protected Sub ScheduleDetachEventHandlers()
        If detachQueued OrElse detachStarted Then Exit Sub
        detachQueued = True

        Dim cleanupAction As New MethodInvoker(
            Sub()
                detachQueued = False

                Try
                    Me.DetachEventHandlers()
                Catch
                    'The document may already be released by SOLIDWORKS. Cleanup is best-effort.
                End Try
            End Sub
        )

        Try
            Dim host As Control = Nothing

            If userAddin IsNot Nothing Then
                host = TryCast(userAddin.myTaskPaneHost, Control)
            End If

            If host IsNot Nothing AndAlso Not host.IsDisposed AndAlso host.IsHandleCreated Then
                host.BeginInvoke(cleanupAction)
                Exit Sub
            End If
        Catch
        End Try

        Try
            Dim context As SynchronizationContext = SynchronizationContext.Current

            If context IsNot Nothing Then
                context.Post(
                    New SendOrPostCallback(Sub(state As Object) cleanupAction.Invoke()),
                    Nothing
                )
                Exit Sub
            End If
        Catch
        End Try

        'Last-resort fallback. Normally the task pane or WinForms synchronization
        'context is available, so cleanup remains outside the native close callback.
        Try
            cleanupAction.Invoke()
        Catch
        End Try
    End Sub

    Protected Function TryBeginDetachEventHandlers() As Boolean
        If detachStarted Then Return False
        detachStarted = True
        Return True
    End Function

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
        Dim iModelView As ModelView = Nothing

        Try
            iModelView = iDocument.GetFirstModelView()
        Catch
            Return False
        End Try

        While (Not iModelView Is Nothing)
            ConnectModelView(iModelView)

            Try
                iModelView = iModelView.GetNext
            Catch
                iModelView = Nothing
            End Try
        End While

        Return True
    End Function

    'A document can gain another ModelView after its document event handler has already
    'been installed (notably Edit Part -> Open Part in New Window). ViewNewNotify2 supplies
    'that new view directly, so attach the close guard immediately instead of waiting for
    'the document to be reopened.
    Protected Function ConnectModelView(ByVal viewObject As Object) As Boolean
        Dim modelView As ModelView = TryCast(viewObject, ModelView)
        If modelView Is Nothing Then Return False

        Try
            If openModelViews.Contains(modelView) Then Return True

            Dim mView As New DocView()
            mView.Init(userAddin, modelView, Me)
            mView.AttachEventHandlers()
            openModelViews.Add(modelView, mView)
            Return True
        Catch
            Return False
        End Try
    End Function

    Function DisconnectModelViews() As Boolean
        Try
            If openModelViews Is Nothing OrElse openModelViews.Count = 0 Then Return True

            'Close events on all currently open docs.
            Dim keys() As Object = New Object(openModelViews.Count - 1) {}
            openModelViews.Keys.CopyTo(keys, 0)

            For Each keyObject As Object In keys
                Try
                    Dim key As ModelView = TryCast(keyObject, ModelView)
                    Dim mView As DocView = TryCast(openModelViews.Item(keyObject), DocView)

                    If mView IsNot Nothing Then mView.DetachEventHandlers()
                    openModelViews.Remove(keyObject)
                    key = Nothing
                    mView = Nothing
                Catch
                    Try
                        openModelViews.Remove(keyObject)
                    Catch
                    End Try
                End Try
            Next
        Catch
        End Try

        Return True
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
        AddHandler iPart.ViewNewNotify2, AddressOf Me.PartDoc_ViewNewNotify2
        AddHandler iPart.FileSaveNotify, AddressOf Me.PartDoc_FileSaveNotify
        AddHandler iPart.FileSaveAsNotify2, AddressOf Me.PartDoc_FileSaveAsNotify2
        AddHandler iPart.FileSavePostNotify, AddressOf Me.PartDoc_FileSavePostNotify
        AddHandler iPart.ModifyNotify, AddressOf Me.PartDoc_ModifyNotify
        AddHandler iPart.AddItemNotify, AddressOf Me.PartDoc_AddItemNotify
        AddHandler iPart.DeleteItemPreNotify, AddressOf Me.PartDoc_DeleteItemPreNotify
        AddHandler iPart.FeatureEditPreNotify, AddressOf Me.PartDoc_FeatureEditPreNotify
        AddHandler iPart.FeatureSketchEditPreNotify, AddressOf Me.PartDoc_FeatureSketchEditPreNotify
        AddHandler iPart.RegenNotify, AddressOf Me.PartDoc_RegenNotify
        AddHandler iPart.RegenPostNotify, AddressOf Me.PartDoc_RegenPostNotify
        AddHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify

        'AddHandler iPart.NewSelectionNotify, AddressOf Me.PartDoc_NewSelectionNotify
        'AddHandler iSwApp.ActiveModelDocChangeNotify, AddressOf Me.PartDoc_ActiveModelDocChangeNotify
        'AddHandler iSwApp.FileOpenPostNotify, AddressOf Me.PartDoc_FileOpenPostNotify

        SolidWorksCtrlWCloseGuardKeyboardHook.Install()
        ConnectModelViews()
    End Function

    Overrides Function DetachEventHandlers() As Boolean
        If Not TryBeginDetachEventHandlers() Then Return True

        Try
            RemoveHandler iPart.DestroyNotify, AddressOf Me.PartDoc_DestroyNotify
            RemoveHandler iPart.ViewNewNotify2, AddressOf Me.PartDoc_ViewNewNotify2
            RemoveHandler iPart.FileSaveNotify, AddressOf Me.PartDoc_FileSaveNotify
            RemoveHandler iPart.FileSaveAsNotify2, AddressOf Me.PartDoc_FileSaveAsNotify2
            RemoveHandler iPart.FileSavePostNotify, AddressOf Me.PartDoc_FileSavePostNotify
            RemoveHandler iPart.ModifyNotify, AddressOf Me.PartDoc_ModifyNotify
            RemoveHandler iPart.AddItemNotify, AddressOf Me.PartDoc_AddItemNotify
            RemoveHandler iPart.DeleteItemPreNotify, AddressOf Me.PartDoc_DeleteItemPreNotify
            RemoveHandler iPart.FeatureEditPreNotify, AddressOf Me.PartDoc_FeatureEditPreNotify
            RemoveHandler iPart.FeatureSketchEditPreNotify, AddressOf Me.PartDoc_FeatureSketchEditPreNotify
            RemoveHandler iPart.RegenNotify, AddressOf Me.PartDoc_RegenNotify
            RemoveHandler iPart.RegenPostNotify, AddressOf Me.PartDoc_RegenPostNotify
            RemoveHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify
        Catch
            'The SOLIDWORKS document may already have released its COM connection.
        End Try

        Try
            DisconnectModelViews()
        Catch
        End Try

        Try
            If userAddin IsNot Nothing Then userAddin.DetachModelEventHandler(iDocument)
        Catch
        End Try

        Return True
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

    Private Function PartDoc_ModifyNotify() As Integer
        svnModule.handlePartOwnedEditPostPublic(iDocument, "editing this part")
        Return 0
    End Function

    Private Function PartDoc_AddItemNotify(ByVal EntityType As Integer,
                                            ByVal itemName As String) As Integer
        svnModule.handlePartOwnedEditPostPublic(iDocument, "adding " & itemName)
        Return 0
    End Function

    Private Function PartDoc_DeleteItemPreNotify(ByVal EntityType As Integer,
                                                  ByVal itemName As String) As Integer
        Return svnModule.blockSelectedCadDestructiveEditPrePublic(
            iDocument,
            "delete " & itemName
        )
    End Function

    Private Function PartDoc_FeatureEditPreNotify(ByVal editFeature As Object) As Integer
        Return svnModule.handleCadFeatureEditPrePublic(iDocument, "editing this part feature", editFeature)
    End Function

    Private Function PartDoc_FeatureSketchEditPreNotify(ByVal editFeature As Object,
                                                        ByVal featureSketch As Object) As Integer
        Return svnModule.handleCadFeatureEditPrePublic(iDocument, "editing this part sketch", editFeature)
    End Function

    Private Function PartDoc_RegenNotify() As Integer
        svnModule.beginAssemblyRebuildPublic(iDocument)
        Return 0
    End Function

    Private Function PartDoc_RegenPostNotify() As Integer
        svnModule.endAssemblyRebuildPublic(iDocument)
        Return 0
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
        'FileCloseNotify is a post-close notification. It cannot safely cancel a close.
        If IsThisDocumentFile(FileName) Then ScheduleDetachEventHandlers()
        Return 0
    End Function

    Function PartDoc_DestroyNotify() As Integer
        'Pre-close blocking is handled by the Ctrl+W/document-window/main-window guards.
        'This obsolete destroy event is cleanup-only and must never return a cancel code.
        ScheduleDetachEventHandlers()
        Return 0
    End Function

    Private Function PartDoc_ViewNewNotify2(ByVal newView As Object) As Integer
        ConnectModelView(newView)
        Return 0
    End Function

    Function PartDoc_NewSelectionNotify() As Integer

    End Function
End Class

'Class to listen for Assembly Events
Public Class AssemblyEventHandler
    Inherits DocumentEventHandler

    Dim WithEvents iAssembly As AssemblyDoc
    Dim swAddin As SwAddin
    Private lastKnownComponentCount As Integer = -1

    Overrides Function Init(ByVal sw As SldWorks, ByVal addin As SwAddin, ByVal model As ModelDoc2) As Boolean
        userAddin = addin
        iAssembly = model
        iDocument = iAssembly
        iSwApp = sw
        swAddin = addin
        lastKnownComponentCount = getAssemblyComponentCountSafe()
    End Function

    Overrides Function AttachEventHandlers() As Boolean
        AddHandler iAssembly.DestroyNotify, AddressOf Me.AssemblyDoc_DestroyNotify
        AddHandler iAssembly.ViewNewNotify2, AddressOf Me.AssemblyDoc_ViewNewNotify2
        AddHandler iAssembly.FileSaveNotify, AddressOf Me.AssemblyDoc_FileSaveNotify
        AddHandler iAssembly.FileSaveAsNotify2, AddressOf Me.AssemblyDoc_FileSaveAsNotify2
        AddHandler iAssembly.FileSavePostNotify, AddressOf Me.AssemblyDoc_FileSavePostNotify
        AddHandler iAssembly.NewSelectionNotify, AddressOf Me.AssemblyDoc_NewSelectionNotify

        'Assembly-owned edit protection. Pre-notify events block directly; post-notify
        'events warn without mutating SOLIDWORKS' undo stack.
        AddHandler iAssembly.ModifyNotify, AddressOf Me.AssemblyDoc_ModifyNotify
        AddHandler iAssembly.ComponentMoveNotify2, AddressOf Me.AssemblyDoc_ComponentMoveNotify2
        AddHandler iAssembly.ComponentReorganizeNotify, AddressOf Me.AssemblyDoc_ComponentReorganizeNotify
        AddHandler iAssembly.AddItemNotify, AddressOf Me.AssemblyDoc_AddItemNotify
        AddHandler iAssembly.DeleteItemPreNotify, AddressOf Me.AssemblyDoc_DeleteItemPreNotify
        AddHandler iAssembly.PreRenameItemNotify, AddressOf Me.AssemblyDoc_PreRenameItemNotify
        AddHandler iAssembly.DimensionChangeNotify, AddressOf Me.AssemblyDoc_DimensionChangeNotify
        AddHandler iAssembly.FeatureEditPreNotify, AddressOf Me.AssemblyDoc_FeatureEditPreNotify
        AddHandler iAssembly.FeatureSketchEditPreNotify, AddressOf Me.AssemblyDoc_FeatureSketchEditPreNotify

        AddHandler iAssembly.ComponentStateChangeNotify, AddressOf Me.AssemblyDoc_ComponentStateChangeNotify
        AddHandler iAssembly.ComponentStateChangeNotify2, AddressOf Me.AssemblyDoc_ComponentStateChangeNotify2
        AddHandler iAssembly.ComponentVisibleChangeNotify, AddressOf Me.AssemblyDoc_ComponentVisibleChangeNotify
        AddHandler iAssembly.ComponentVisualPropertiesChangeNotify, AddressOf Me.AssemblyDoc_ComponentVisiblePropertiesChangeNotify
        AddHandler iAssembly.ComponentDisplayStateChangeNotify, AddressOf Me.AssemblyDoc_ComponentDisplayStateChangeNotify
        AddHandler iAssembly.BodyVisibleChangeNotify, AddressOf Me.AssemblyDoc_BodyVisibleChangeNotify
        AddHandler iAssembly.RegenNotify, AddressOf Me.AssemblyDoc_RegenNotify
        AddHandler iAssembly.RegenPostNotify, AddressOf Me.AssemblyDoc_RegenPostNotify
        AddHandler iAssembly.BeginInContextEditNotify, AddressOf Me.AssemblyDoc_BeginInContextEditNotify
        AddHandler iAssembly.EndInContextEditNotify, AddressOf Me.AssemblyDoc_EndInContextEditNotify
        AddHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify

        'AddHandler iSwApp.ActiveModelDocChangeNotify, AddressOf Me.AssemblyDoc_ActiveModelDocChangeNotify
        'AddHandler iSwApp.FileCloseNotify, AddressOf Me.DSldWorksEvents_FileCloseNotifyEventHandler

        SolidWorksCtrlWCloseGuardKeyboardHook.Install()
        ConnectModelViews()
    End Function

    Overrides Function DetachEventHandlers() As Boolean
        If Not TryBeginDetachEventHandlers() Then Return True

        Try
            RemoveHandler iAssembly.DestroyNotify, AddressOf Me.AssemblyDoc_DestroyNotify
            RemoveHandler iAssembly.ViewNewNotify2, AddressOf Me.AssemblyDoc_ViewNewNotify2
            RemoveHandler iAssembly.FileSaveNotify, AddressOf Me.AssemblyDoc_FileSaveNotify
            RemoveHandler iAssembly.FileSaveAsNotify2, AddressOf Me.AssemblyDoc_FileSaveAsNotify2
            RemoveHandler iAssembly.FileSavePostNotify, AddressOf Me.AssemblyDoc_FileSavePostNotify
            RemoveHandler iAssembly.NewSelectionNotify, AddressOf Me.AssemblyDoc_NewSelectionNotify
            RemoveHandler iAssembly.ModifyNotify, AddressOf Me.AssemblyDoc_ModifyNotify
            RemoveHandler iAssembly.ComponentMoveNotify2, AddressOf Me.AssemblyDoc_ComponentMoveNotify2
            RemoveHandler iAssembly.ComponentReorganizeNotify, AddressOf Me.AssemblyDoc_ComponentReorganizeNotify
            RemoveHandler iAssembly.AddItemNotify, AddressOf Me.AssemblyDoc_AddItemNotify
            RemoveHandler iAssembly.DeleteItemPreNotify, AddressOf Me.AssemblyDoc_DeleteItemPreNotify
            RemoveHandler iAssembly.PreRenameItemNotify, AddressOf Me.AssemblyDoc_PreRenameItemNotify
            RemoveHandler iAssembly.DimensionChangeNotify, AddressOf Me.AssemblyDoc_DimensionChangeNotify
            RemoveHandler iAssembly.FeatureEditPreNotify, AddressOf Me.AssemblyDoc_FeatureEditPreNotify
            RemoveHandler iAssembly.FeatureSketchEditPreNotify, AddressOf Me.AssemblyDoc_FeatureSketchEditPreNotify
            RemoveHandler iAssembly.ComponentStateChangeNotify, AddressOf Me.AssemblyDoc_ComponentStateChangeNotify
            RemoveHandler iAssembly.ComponentStateChangeNotify2, AddressOf Me.AssemblyDoc_ComponentStateChangeNotify2
            RemoveHandler iAssembly.ComponentVisibleChangeNotify, AddressOf Me.AssemblyDoc_ComponentVisibleChangeNotify
            RemoveHandler iAssembly.ComponentVisualPropertiesChangeNotify, AddressOf Me.AssemblyDoc_ComponentVisiblePropertiesChangeNotify
            RemoveHandler iAssembly.ComponentDisplayStateChangeNotify, AddressOf Me.AssemblyDoc_ComponentDisplayStateChangeNotify
            RemoveHandler iAssembly.BodyVisibleChangeNotify, AddressOf Me.AssemblyDoc_BodyVisibleChangeNotify
            RemoveHandler iAssembly.RegenNotify, AddressOf Me.AssemblyDoc_RegenNotify
            RemoveHandler iAssembly.RegenPostNotify, AddressOf Me.AssemblyDoc_RegenPostNotify
            RemoveHandler iAssembly.BeginInContextEditNotify, AddressOf Me.AssemblyDoc_BeginInContextEditNotify
            RemoveHandler iAssembly.EndInContextEditNotify, AddressOf Me.AssemblyDoc_EndInContextEditNotify
            RemoveHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify
        Catch
            'The SOLIDWORKS document may already have released its COM connection.
        End Try

        Try
            DisconnectModelViews()
        Catch
        End Try

        Try
            If userAddin IsNot Nothing Then userAddin.DetachModelEventHandler(iDocument)
        Catch
        End Try

        Return True
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
        svnModule.clearInContextEditSessionPublic(iDocument)
        ScheduleDetachEventHandlers()
        Return 0
    End Function

    Private Function AssemblyDoc_ViewNewNotify2(ByVal newView As Object) As Integer
        ConnectModelView(newView)
        Return 0
    End Function

    Function AssemblyDoc_NewSelectionNotify() As Integer
        svnModule.noteAssemblySelectionContextPublic(iDocument)
        svnModule.noteSelectedAssemblyFeatureOwnerPublic(iDocument)

        Try
            If swAddin IsNot Nothing AndAlso swAddin.myTaskPaneHost IsNot Nothing Then
                swAddin.myTaskPaneHost.syncSvnTreeToCurrentSolidWorksSelectionPublic()
            End If
        Catch
        End Try

        Return 0
    End Function

    Private Function SwApp_FileCloseNotify(ByVal FileName As String, ByVal Reason As Integer) As Integer
        If IsThisDocumentFile(FileName) Then ScheduleDetachEventHandlers()
        Return 0
    End Function

    Private Function AssemblyDoc_ModifyNotify() As Integer
        'ModifyNotify is also raised while a child-owned dimension dialog is active.
        'The module permits that narrow case only when the selected external child has its lock.
        svnModule.handleAssemblyOwnedEditPostPublic(
            svnModule.getSelectedAssemblyEditOwnerPublic(iDocument, selectedDocumentOwnsEdit:=True),
            "assembly-level modification",
            allowLockedChildDimensionFallback:=True,
            allowRecentlyEndedInContextEdit:=True,
            allowDisplayOnlyFallback:=True,
            allowRebuildModifyFallback:=True,
            allowActiveChildEditContext:=True,
            allowPendingSuppressionCommandFallback:=True,
            pendingSuppressionEventAssembly:=iDocument
        )
        Return 0
    End Function

    'RegenNotify/RegenPostNotify bracket a Rebuild. A Rebuild that only recomputes this
    'assembly from an already-correctly-updated child raises ModifyNotify with no structural
    'edit of this assembly, so the edit guard must not treat it as an unlocked modification.
    Function AssemblyDoc_RegenNotify() As Integer
        svnModule.beginAssemblyRebuildPublic(iDocument)
        Return 0
    End Function

    Function AssemblyDoc_RegenPostNotify() As Integer
        svnModule.endAssemblyRebuildPublic(iDocument)
        Return 0
    End Function

    'BeginInContextEditNotify/EndInContextEditNotify fire specifically for "Edit Part"
    'in-context editing from the assembly window. GetEditTarget can already be Nothing again
    'by the time ModifyNotify for the edit itself arrives (e.g. right after exiting edit
    'mode), which previously made a fully authorized edit to a locked child look like an
    'unlocked assembly edit and get falsely blocked/flagged.
    Function AssemblyDoc_BeginInContextEditNotify(ByVal docBeingEdited As Object, ByVal docType As Integer) As Integer
        svnModule.noteInContextEditBeganPublic(iDocument, TryCast(docBeingEdited, ModelDoc2))
        Return 0
    End Function

    Function AssemblyDoc_EndInContextEditNotify(ByVal docBeingEdited As Object, ByVal docType As Integer) As Integer
        svnModule.noteInContextEditEndedPublic(iDocument, TryCast(docBeingEdited, ModelDoc2))
        Return 0
    End Function

    Private Function AssemblyDoc_FeatureEditPreNotify(ByVal editFeature As Object) As Integer
        Return svnModule.handleCadFeatureEditPrePublic(iDocument, "editing the selected feature", editFeature)
    End Function

    Private Function AssemblyDoc_FeatureSketchEditPreNotify(ByVal editFeature As Object,
                                                            ByVal featureSketch As Object) As Integer
        Return svnModule.handleCadFeatureEditPrePublic(iDocument, "editing the selected sketch", editFeature)
    End Function

    Private Function AssemblyDoc_ComponentMoveNotify2(ByRef Components As Object) As Integer
        'The event payload identifies the component(s) that actually moved. Resolve their
        'immediate owning assembly instead of borrowing a possibly stale Edit Part context or
        'the top-level event document. A component move is persisted by its parent assembly;
        'it is never a child-part geometry edit.
        Dim owners() As ModelDoc2 =
            svnModule.getAssemblyEditOwnersForMovedComponentsPublic(iDocument, Components)

        If owners Is Nothing OrElse owners.Length = 0 Then
            svnModule.handleAssemblyOwnedEditPostPublic(
                iDocument,
                "moving or rotating an assembly component"
            )
        Else
            For Each owner As ModelDoc2 In owners
                svnModule.handleAssemblyOwnedEditPostPublic(
                    owner,
                    "moving or rotating an assembly component"
                )
            Next
        End If
        Return 0
    End Function

    Private Function AssemblyDoc_ComponentReorganizeNotify(ByVal sourceName As String,
                                                            ByVal targetName As String) As Integer
        'Dragging a component into/out of a FeatureManager folder is persisted in the assembly
        'file even though geometry does not move. ComponentMoveNotify2 does not cover this tree
        'reorganization, so guard its purpose-built event explicitly.
        Dim owner As ModelDoc2 =
            svnModule.getAssemblyEditOwnerForComponentStatePublic(iDocument, sourceName)
        If owner Is Nothing Then owner = iDocument

        svnModule.handleAssemblyOwnedEditPostPublic(
            owner,
            "reorganizing " & sourceName & " in the FeatureManager tree",
            allowActiveChildEditContext:=False
        )
        Return 0
    End Function

    Private Function AssemblyDoc_AddItemNotify(ByVal EntityType As Integer, ByVal itemName As String) As Integer
        svnModule.handleAssemblyOwnedEditPostPublic(
            iDocument,
            "adding " & itemName,
            addedEntityType:=EntityType,
            addedItemName:=itemName,
            allowActiveChildEditContext:=True
        )

        Dim currentComponentCount As Integer = getAssemblyComponentCountSafe()

        If currentComponentCount > lastKnownComponentCount Then
            Try
                If swAddin IsNot Nothing AndAlso swAddin.myTaskPaneHost IsNot Nothing Then
                    swAddin.myTaskPaneHost.queueSvnTreeStructureRefreshPublic()
                End If
            Catch
            End Try
        End If

        If currentComponentCount >= 0 Then lastKnownComponentCount = currentComponentCount
        Return 0
    End Function

    Private Function getAssemblyComponentCountSafe() As Integer
        Try
            'Count-only API avoids allocating/walking the full component array on every
            'AddItemNotify in a large assembly.
            Return iAssembly.GetComponentCount(False)
        Catch
            Return -1
        End Try
    End Function

    Private Function AssemblyDoc_DeleteItemPreNotify(ByVal EntityType As Integer, ByVal itemName As String) As Integer
        Return svnModule.blockSelectedCadDestructiveEditPrePublic(
            iDocument,
            "delete " & itemName
        )
    End Function

    Private Function AssemblyDoc_PreRenameItemNotify(ByVal EntityType As Integer,
                                                      ByVal oldName As String,
                                                      ByVal newName As String) As Integer
        Return svnModule.blockAssemblyOwnedEditPrePublic(iDocument, "renaming " & oldName)
    End Function

    Private Function AssemblyDoc_DimensionChangeNotify(ByVal displayDim As Object) As Integer
        Dim editOwner As ModelDoc2 = svnModule.getSelectedAssemblyEditOwnerPublic(
            iDocument,
            selectedDocumentOwnsEdit:=True
        )
        svnModule.handleAssemblyDimensionChangePostPublic(editOwner, displayDim)
        Return 0
    End Function

    Protected Function ComponentStateChange(ByVal componentModel As Object,
                                            Optional ByVal newCompState As Short = swComponentSuppressionState_e.swComponentResolved,
                                            Optional ByVal componentName As String = "") As Integer

        Dim modDoc As ModelDoc2 = componentModel
        Dim newState As swComponentSuppressionState_e = newCompState
        Dim editOwner As ModelDoc2 = Nothing

        If Not String.IsNullOrWhiteSpace(componentName) Then
            editOwner = svnModule.getAssemblyEditOwnerForComponentStatePublic(iDocument, componentName)
        End If
        If editOwner Is Nothing Then
            editOwner = svnModule.getSelectedAssemblyEditOwnerPublic(iDocument)
        End If

        Select Case newState

            Case swComponentSuppressionState_e.swComponentFullyResolved, swComponentSuppressionState_e.swComponentResolved

                If ((Not modDoc Is Nothing) AndAlso Not Me.swAddin.OpenDocumentsTable.Contains(modDoc)) Then
                    Me.swAddin.AttachModelDocEventHandler(modDoc)
                End If

                'Unsuppressing brings a component back into active computation and is saved to
                'the assembly file exactly like suppressing it - unlike hide/show/transparency
                '(pure local viewing state, still exempt), suppression state is real, persisted
                'assembly data and requires the assembly lock like any other structural edit.
                svnModule.handleAssemblyOwnedEditPostPublic(
                    editOwner,
                    "unsuppressing a component",
                    allowActiveChildEditContext:=True,
                    allowPendingSuppressionCommandFallback:=True,
                    pendingSuppressionEventAssembly:=iDocument
                )

                Exit Select

            Case swComponentSuppressionState_e.swComponentSuppressed

                svnModule.handleAssemblyOwnedEditPostPublic(
                    editOwner,
                    "suppressing a component",
                    allowActiveChildEditContext:=True,
                    allowPendingSuppressionCommandFallback:=True,
                    pendingSuppressionEventAssembly:=iDocument
                )

                Exit Select

        End Select

    End Function

    'attach events to a component if it becomes resolved
    Public Function AssemblyDoc_ComponentStateChangeNotify(ByVal componentModel As Object, ByVal oldCompState As Short, ByVal newCompState As Short) As Integer

        Return ComponentStateChange(componentModel, newCompState)

    End Function

    'attach events to a component if it becomes resolved
    Public Function AssemblyDoc_ComponentStateChangeNotify2(ByVal componentModel As Object, ByVal CompName As String, ByVal oldCompState As Short, ByVal newCompState As Short) As Integer

        Return ComponentStateChange(componentModel, newCompState, CompName)

    End Function

    Public Function AssemblyDoc_ComponentVisiblePropertiesChangeNotify(ByVal swObject As Object) As Integer
        'Appearance/transparency is a local display preference, not a geometry or structural
        'edit - a user commonly changes it just to see or measure something more clearly
        'while locked out of the assembly (e.g. editing one specific part in context).
        'Allowed without the assembly lock. Do not route this through ComponentStateChange:
        'its default state is Resolved and would misclassify appearance changes as Unsuppress.
        svnModule.noteAssemblyDisplayOnlyChangePublic(iDocument)
        Return 0

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
        'Hide/show and display-state changes are local viewing preferences, not geometry or
        'structural edits - allowed without the assembly lock for the same reason as
        'visual-properties changes above (e.g. hiding surrounding
        'components just to take a measurement while locked out of the assembly).
        svnModule.noteAssemblyDisplayOnlyChangePublic(iDocument)
        Return 0

    End Function

    Public Function AssemblyDoc_ComponentVisibleChangeNotify() As Integer
        'Hide/show is explicitly non-structural and may be used for inspection or measurement.
        svnModule.noteAssemblyDisplayOnlyChangePublic(iDocument)
        Return 0
    End Function

    Public Function AssemblyDoc_BodyVisibleChangeNotify() As Integer
        'Likewise allow temporary body visibility changes without an assembly lock.
        svnModule.noteAssemblyDisplayOnlyChangePublic(iDocument)
        Return 0
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
        AddHandler iDrawing.ViewNewNotify2, AddressOf Me.DrawingDoc_ViewNewNotify2
        AddHandler iDrawing.FileSaveNotify, AddressOf Me.DrawingDoc_FileSaveNotify
        AddHandler iDrawing.FileSaveAsNotify2, AddressOf Me.DrawingDoc_FileSaveAsNotify2
        AddHandler iDrawing.FileSavePostNotify, AddressOf Me.DrawingDoc_FileSavePostNotify
        AddHandler iDrawing.NewSelectionNotify, AddressOf Me.DrawingDoc_NewSelectionNotify
        AddHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify

        SolidWorksCtrlWCloseGuardKeyboardHook.Install()
        ConnectModelViews()

        svnModule.queueDrawingReferenceFreshnessCheckPublic(iDocument)
    End Function

    Overrides Function DetachEventHandlers() As Boolean
        If Not TryBeginDetachEventHandlers() Then Return True

        Try
            RemoveHandler iDrawing.DestroyNotify, AddressOf Me.DrawingDoc_DestroyNotify
            RemoveHandler iDrawing.ViewNewNotify2, AddressOf Me.DrawingDoc_ViewNewNotify2
            RemoveHandler iDrawing.FileSaveNotify, AddressOf Me.DrawingDoc_FileSaveNotify
            RemoveHandler iDrawing.FileSaveAsNotify2, AddressOf Me.DrawingDoc_FileSaveAsNotify2
            RemoveHandler iDrawing.FileSavePostNotify, AddressOf Me.DrawingDoc_FileSavePostNotify
            RemoveHandler iDrawing.NewSelectionNotify, AddressOf Me.DrawingDoc_NewSelectionNotify
            RemoveHandler iSwApp.FileCloseNotify, AddressOf Me.SwApp_FileCloseNotify
        Catch
            'The SOLIDWORKS document may already have released its COM connection.
        End Try

        Try
            DisconnectModelViews()
        Catch
        End Try

        Try
            If userAddin IsNot Nothing Then userAddin.DetachModelEventHandler(iDocument)
        Catch
        End Try

        Return True
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
        ScheduleDetachEventHandlers()
        Return 0
    End Function

    Private Function DrawingDoc_ViewNewNotify2(ByVal newView As Object) As Integer
        ConnectModelView(newView)
        Return 0
    End Function

    Function DrawingDoc_NewSelectionNotify() As Integer

    End Function

    Private Function SwApp_FileCloseNotify(ByVal FileName As String, ByVal Reason As Integer) As Integer
        If IsThisDocumentFile(FileName) Then ScheduleDetachEventHandlers()
        Return 0
    End Function
End Class

'Class for handling ModelView events
Public Class DocView

    Dim WithEvents iModelView As ModelView
    Dim userAddin As SwAddin
    Dim parentDoc As DocumentEventHandler
    Dim docWindowCloseGuards As New List(Of SolidWorksDocumentCloseGuardWindowHook)
    Private detachQueued As Boolean = False
    Private detachStarted As Boolean = False

    Private Sub ScheduleDetachEventHandlers()
        If detachQueued OrElse detachStarted Then Exit Sub
        detachQueued = True

        Dim cleanupAction As New MethodInvoker(
            Sub()
                detachQueued = False

                Try
                    DetachEventHandlers()
                Catch
                End Try
            End Sub
        )

        Try
            Dim host As Control = Nothing

            If userAddin IsNot Nothing Then
                host = TryCast(userAddin.myTaskPaneHost, Control)
            End If

            If host IsNot Nothing AndAlso Not host.IsDisposed AndAlso host.IsHandleCreated Then
                host.BeginInvoke(cleanupAction)
                Exit Sub
            End If
        Catch
        End Try

        Try
            Dim context As SynchronizationContext = SynchronizationContext.Current

            If context IsNot Nothing Then
                context.Post(
                    New SendOrPostCallback(Sub(state As Object) cleanupAction.Invoke()),
                    Nothing
                )
                Exit Sub
            End If
        Catch
        End Try

        Try
            cleanupAction.Invoke()
        Catch
        End Try
    End Sub

    Function Init(ByVal addin As SwAddin, ByVal mView As ModelView, ByVal parent As DocumentEventHandler) As Boolean
        userAddin = addin
        iModelView = mView
        parentDoc = parent
    End Function

    Function AttachEventHandlers() As Boolean
        AddHandler iModelView.DestroyNotify2, AddressOf Me.ModelView_DestroyNotify2
        AddHandler iModelView.RepaintNotify, AddressOf Me.ModelView_RepaintNotify

        EnsureDocumentWindowCloseGuards()
        Return True
    End Function

    Private Sub EnsureDocumentWindowCloseGuards()
        If detachStarted OrElse docWindowCloseGuards.Count > 0 Then Exit Sub

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
    End Sub

    Private Sub HookDocumentWindowAndParents(startHwnd As IntPtr)
        Dim currentHwnd As IntPtr = startHwnd
        Dim hookedHandles As New HashSet(Of IntPtr)()

        'Walk upward through the model-view/document windows. Do not subclass the
        'top-level SOLIDWORKS frame; SwAddin already owns the major-X close guard.
        For i As Integer = 0 To 8
            If currentHwnd = IntPtr.Zero Then Exit For

            Dim parentHwnd As IntPtr = SolidWorksDocumentCloseGuardWindowHook.GetParentWindow(currentHwnd)
            If parentHwnd = IntPtr.Zero Then Exit For

            If Not hookedHandles.Contains(currentHwnd) Then
                Dim hook As New SolidWorksDocumentCloseGuardWindowHook()
                hook.AssignSolidWorksDocumentHandle(currentHwnd)
                docWindowCloseGuards.Add(hook)
                hookedHandles.Add(currentHwnd)
            End If

            currentHwnd = parentHwnd
        Next
    End Sub

    Function DetachEventHandlers() As Boolean
        If detachStarted Then Return True
        detachStarted = True

        Try
            RemoveHandler iModelView.DestroyNotify2, AddressOf Me.ModelView_DestroyNotify2
            RemoveHandler iModelView.RepaintNotify, AddressOf Me.ModelView_RepaintNotify
        Catch
            'The model view may already have been destroyed by SOLIDWORKS.
        End Try

        Try
            For Each hook As SolidWorksDocumentCloseGuardWindowHook In docWindowCloseGuards
                If hook Is Nothing Then Continue For
                hook.ReleaseSolidWorksDocumentHandle()
            Next

            docWindowCloseGuards.Clear()
        Catch
            docWindowCloseGuards.Clear()
        End Try

        Try
            If parentDoc IsNot Nothing Then parentDoc.DetachModelViewEventHandler(iModelView)
        Catch
        End Try

        Return True
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
                        'Use the same deferred Ctrl+W path as the global keyboard hook.  Running a
                        'second synchronous close decision here could race the first path's modal
                        'table and its controlled-close flags.
                        svnModule.queueDeferredCtrlWCloseCheckPublic()
                        Return
                    End If
                Catch
                End Try
            End If

            If m.Msg = WM_CLOSE Then
                If SolidWorksCloseGuardCoordinator.ShouldBlockActiveDocumentClose() Then
                    Return
                End If
            End If

            If m.Msg = WM_SYSCOMMAND Then
                Dim command As Integer = m.WParam.ToInt32() And &HFFF0

                If command = SC_CLOSE Then
                    If SolidWorksCloseGuardCoordinator.ShouldBlockActiveDocumentClose() Then
                        Return
                    End If
                End If
            End If

            MyBase.WndProc(m)
        End Sub
    End Class

    Function ModelView_DestroyNotify2(ByVal destroyTYpe As Integer) As Integer
        ScheduleDetachEventHandlers()
        Return 0
    End Function

    Function ModelView_RepaintNotify(ByVal repaintTYpe As Integer) As Integer
        'Some SOLIDWORKS releases raise ViewNewNotify2 before the native view HWND exists.
        'The first repaint is a cheap, deterministic retry and avoids a polling timer.
        EnsureDocumentWindowCloseGuards()
        Return 0
    End Function
End Class
