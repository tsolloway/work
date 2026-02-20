VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Sheet1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = True
Option Explicit

Private Sub Worksheet_Change(ByVal Target As Range)
    ' Dropdown cells: push to _controls, sync Best Combos, rebuild
    If Not Intersect(Target, Me.Range("C5,F5,I5,L5")) Is Nothing Then
        Application.EnableEvents = False
        Application.Run "PushToControls", "Dashboard"
        Application.Run "SyncControls", "Dashboard"
        Application.Run "BuildTURF"
        Application.EnableEvents = True
        Exit Sub
    End If

    ' Items: All/None toggle (AB5 dropdown)
    If Not Intersect(Target, Me.Range("AB5")) Is Nothing Then
        Dim toggleVal As String
        toggleVal = Trim(CStr(Target.Value))
        If toggleVal = "All" Or toggleVal = "None" Then
            Application.EnableEvents = False

            Dim nItems As Long
            nItems = CLng(Sheets("_config").Range("B7").Value)
            Dim cbRange As Range
            Set cbRange = Me.Range("AB9:AB" & (8 + nItems))

            If toggleVal = "All" Then
                cbRange.Value = True
            Else
                cbRange.Value = False
            End If

            ' Reset dropdown back to dash
            Target.Value = "-"
            Application.EnableEvents = True
            Application.Run "BuildTURF"
        End If
        Exit Sub
    End If

    ' Checkbox cells: Include column (AB), rows 9 to 8 + n_items
    Dim nItems2 As Long
    nItems2 = CLng(Sheets("_config").Range("B7").Value)
    If Not Intersect(Target, Me.Range("AB9:AB" & (8 + nItems2))) Is Nothing Then
        Application.Run "BuildTURF"
        Exit Sub
    End If
End Sub
