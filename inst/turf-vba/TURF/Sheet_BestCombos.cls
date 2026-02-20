VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Sheet2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = True
Option Explicit

Private Sub Worksheet_Activate()
    ' Rebuild combos if flagged dirty (e.g. item toggles on Dashboard)
    Application.Run "RefreshCombosIfDirty"
End Sub

Private Sub Worksheet_Change(ByVal Target As Range)
    ' Dropdown cells (stacked in col C, rows 5-12): push to _controls, sync Dashboard, rebuild
    If Not Intersect(Target, Me.Range("C5,C6,C7,C8,C9,C10,C11,C12")) Is Nothing Then
        Application.EnableEvents = False
        Application.Run "PushToControls", "Best Combos"
        Application.Run "SyncControls", "Best Combos"
        Application.Run "BuildTURF"
        Application.EnableEvents = True
        Exit Sub
    End If
End Sub
