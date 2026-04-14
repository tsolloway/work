Attribute VB_Name = "Module1"
Option Explicit

' VBA code version — bump whenever Module1.bas changes.
' R writes this same value to _config!B10 so BuildTURF can detect mismatches.
Private Const VBA_VERSION As Long = 2

' =============================================================================
' TURF Dashboard — Main VBA Module (Multi-Sheet Architecture)
'
' Sheets:
'   Dashboard    — controls + chart + items panel (visible)
'   Best Combos  — combo results table + linked controls (visible)
'   Greedy       — greedy results table + chart staging (hidden)
'   _controls    — single source of truth for dropdown states (hidden)
'   _config      — metadata (hidden)
'   _raw         — respondent binary data (hidden)
'   d_*          — pre-computed combo results (hidden)
'
' All sheet references use Sheets() for Mac compatibility.
' =============================================================================

' Sheet names
Private Const DASH_SHEET As String = "Dashboard"
Private Const COMBOS_SHEET As String = "Best Combos"
Private Const GREEDY_SHEET As String = "Greedy"
Private Const CTRL_SHEET As String = "_controls"
Private Const RAW_SHEET As String = "_raw"
Private Const CFG_SHEET As String = "_config"
Private Const BC_CHART_SHEET As String = "_best_combo_charts"

' _config cell addresses (titles in A, values in B)
Private Const CFG_TOP As String = "B6"
Private Const CFG_N_ITEMS As String = "B7"
Private Const CFG_HAS_WEIGHTS As String = "B3"
Private Const CFG_SIG_THRESH As String = "B8"
Private Const CFG_MARG_THRESH As String = "B9"
Private Const CFG_ITEMS_START_ROW As Long = 21  ' row 20 = header, 21+ = data
Private Const CFG_ITEMS_VAR_COL As Long = 1     ' A
Private Const CFG_ITEMS_LABEL_COL As Long = 2   ' B

' _controls cell addresses (titles in A, values in B)
Private Const CC_SUBGROUP As String = "B1"
Private Const CC_COMBO As String = "B2"
Private Const CC_OPTIMIZE As String = "B3"
Private Const CC_WEIGHTED As String = "B4"
Private Const CC_CHART_LABEL As String = "B5"
Private Const CC_INVALIDATE_BC As String = "B9"

' Dashboard dropdown cell addresses (row 5)
Private Const CTRL_SUBGROUP As String = "C5"
Private Const CTRL_WEIGHTED As String = "L5"
Private Const CTRL_BASE As String = "O5"

' Best Combos control cell addresses (stacked in col C, rows 5-11)
Private Const BC_SUBGROUP As String = "C5"
Private Const BC_COMBO As String = "C6"
Private Const BC_DISPLAY As String = "C7"
Private Const BC_OPTIMIZE As String = "C8"
Private Const BC_WEIGHTED As String = "C9"
Private Const BC_AUTOFIT As String = "C10"
Private Const BC_CHART As String = "C11"
Private Const BC_CHART_LOC As String = "C12"
Private Const BC_BASE As String = "C13"

' Dashboard greedy table (cols Q-X, row 8 headers, row 9+ data)
Private Const DG_HEADER_ROW As Long = 8
Private Const DG_DATA_ROW As Long = 9
Private Const DG_STEP_COL As Long = 17        ' Q
Private Const DG_LABEL_COL As Long = 18       ' R — variable name
Private Const DG_ITEM_LABEL_COL As Long = 19  ' S — label text
Private Const DG_CUMUL_COL As Long = 20       ' T
Private Const DG_INCR_COL As Long = 21        ' U
Private Const DG_FREQ_COL As Long = 22        ' V
Private Const DG_ABS_COL As Long = 23         ' W
Private Const DG_PVAL_COL As Long = 24        ' X — p-value

' Items panel (Dashboard only, cols Z-AB, row 9+)
Private Const ITEMS_VAR_COL As Long = 26    ' Z — variable name
Private Const ITEMS_LABEL_COL As Long = 27  ' AA — label text
Private Const ITEMS_CHECK_COL As Long = 28  ' AB — checkbox
Private Const ITEMS_START_ROW As Long = 9

' Greedy sheet layout
Private Const GR_DATA_ROW As Long = 3      ' first data row (row 2 = headers)
Private Const GR_STAGE_STEP As Long = 8    ' H
Private Const GR_STAGE_LABEL As Long = 9   ' I
Private Const GR_STAGE_CUMUL As Long = 10  ' J
Private Const GR_STAGE_INCR As Long = 11   ' K

' Best Combos layout (row 15 headers, row 16+ data)
Private Const BC_HEADER_ROW As Long = 16
Private Const BC_DATA_ROW As Long = 17

' Best Combos chart name
Private Const BC_CHART_NAME As String = "BCChart"

' Guard against double initialization
Private mInitialized As Boolean

' Deferred BC chart parameters (set by WriteComboResults, used after CleanUp)
Private mBCLastRank As Long
Private mBCLastCombo As Long


' =============================================================================
' HasControlsSheet — returns True if _controls sheet exists
' All public entry points exit early if this returns False (workbook not ready)
' =============================================================================
Private Function HasControlsSheet() As Boolean
    Dim ws As Worksheet
    On Error Resume Next
    Set ws = Sheets(CTRL_SHEET)
    On Error GoTo 0
    HasControlsSheet = Not ws Is Nothing
End Function


' =============================================================================
' Auto_Open / OnWorkbookOpen — initialization entry points
' =============================================================================
Public Sub Auto_Open()
    If mInitialized Then Exit Sub
    If Not HasControlsSheet() Then Exit Sub
    mInitialized = True
    Call InitializeCheckboxes
    Call InitializeControls
    Call BuildTURF
End Sub

Public Sub OnWorkbookOpen()
    If mInitialized Then Exit Sub
    If Not HasControlsSheet() Then Exit Sub
    mInitialized = True
    Call InitializeCheckboxes
    Call InitializeControls
    Call BuildTURF
End Sub


' =============================================================================
' InitializeControls — push Dashboard defaults to _controls + sync Best Combos
' =============================================================================
Private Sub InitializeControls()
    Application.EnableEvents = False
    Call PushToControls(DASH_SHEET)
    Call SyncControls(DASH_SHEET)
    Sheets(CTRL_SHEET).Range(CC_INVALIDATE_BC).Value = 1
    Application.EnableEvents = True
End Sub


' =============================================================================
' PushToControls — read control cells from source sheet, write to _controls
' Sheet-aware: F5/I5 mean different things on Dashboard vs Best Combos
' =============================================================================
Public Sub PushToControls(ByVal sourceSheet As String)
    If Not HasControlsSheet() Then Exit Sub
    Dim ctrlWs As Worksheet
    Set ctrlWs = Sheets(CTRL_SHEET)
    Dim srcWs As Worksheet
    Set srcWs = Sheets(sourceSheet)

    Dim hasWeights As Boolean
    hasWeights = (UCase(CStr(Sheets(CFG_SHEET).Range(CFG_HAS_WEIGHTS).Value)) = "TRUE")

    If sourceSheet = DASH_SHEET Then
        ' Dashboard: C5 = Subgroup, F5 = Optimize, I5 = Chart Label, L5 = Weighted
        ctrlWs.Range(CC_SUBGROUP).Value = CStr(srcWs.Range(CTRL_SUBGROUP).Value)
        ctrlWs.Range(CC_OPTIMIZE).Value = CStr(srcWs.Range("F5").Value)
        ctrlWs.Range(CC_CHART_LABEL).Value = CStr(srcWs.Range("I5").Value)
        If hasWeights Then ctrlWs.Range(CC_WEIGHTED).Value = CStr(srcWs.Range(CTRL_WEIGHTED).Value)
    Else
        ' Best Combos: C5 = Subgroup, C6 = Combo, C8 = Optimize, C9 = Weighted
        ctrlWs.Range(CC_SUBGROUP).Value = CStr(srcWs.Range(BC_SUBGROUP).Value)
        ctrlWs.Range(CC_COMBO).Value = CStr(srcWs.Range(BC_COMBO).Value)
        ctrlWs.Range(CC_OPTIMIZE).Value = CStr(srcWs.Range(BC_OPTIMIZE).Value)
        If hasWeights Then ctrlWs.Range(CC_WEIGHTED).Value = CStr(srcWs.Range(BC_WEIGHTED).Value)
    End If
End Sub


' =============================================================================
' SyncControls — read _controls, push values to the OTHER sheet's dropdown cells
' Sheet-aware: F5/I5 map to different controls on each sheet
' =============================================================================
Public Sub SyncControls(ByVal sourceSheet As String)
    If Not HasControlsSheet() Then Exit Sub
    Dim ctrlWs As Worksheet
    Set ctrlWs = Sheets(CTRL_SHEET)

    Dim hasWeights As Boolean
    hasWeights = (UCase(CStr(Sheets(CFG_SHEET).Range(CFG_HAS_WEIGHTS).Value)) = "TRUE")

    Dim sg As String:  sg = CStr(ctrlWs.Range(CC_SUBGROUP).Value)
    Dim cb As String:  cb = CStr(ctrlWs.Range(CC_COMBO).Value)
    Dim opt As String: opt = CStr(ctrlWs.Range(CC_OPTIMIZE).Value)
    Dim wt As String:  wt = ""
    If hasWeights Then wt = CStr(ctrlWs.Range(CC_WEIGHTED).Value)
    Dim cl As String:  cl = CStr(ctrlWs.Range(CC_CHART_LABEL).Value)

    ' Sync TO Dashboard: C5 = Subgroup, F5 = Optimize, I5 = Chart Label, L5 = Weighted
    If sourceSheet <> DASH_SHEET Then
        Dim dashWs As Worksheet
        Set dashWs = Sheets(DASH_SHEET)
        dashWs.Range(CTRL_SUBGROUP).Value = sg
        dashWs.Range("F5").Value = opt
        dashWs.Range("I5").Value = cl
        If hasWeights Then dashWs.Range(CTRL_WEIGHTED).Value = wt
    End If

    ' Sync TO Best Combos: C5 = Subgroup, C6 = Combo, C8 = Optimize, C9 = Weighted
    If sourceSheet <> COMBOS_SHEET Then
        Dim bcWs As Worksheet
        Set bcWs = Sheets(COMBOS_SHEET)
        bcWs.Range(BC_SUBGROUP).Value = sg
        bcWs.Range(BC_COMBO).Value = cb
        bcWs.Range(BC_OPTIMIZE).Value = opt
        If hasWeights Then bcWs.Range(BC_WEIGHTED).Value = wt
    End If
End Sub


' =============================================================================
' BuildTURF — Main entry point
' =============================================================================
Public Sub BuildTURF()
    If Not HasControlsSheet() Then Exit Sub

    Dim dashWs As Worksheet
    Set dashWs = Sheets(DASH_SHEET)

    Dim grWs As Worksheet
    Set grWs = Sheets(GREEDY_SHEET)

    Dim bcWs As Worksheet
    Set bcWs = Sheets(COMBOS_SHEET)

    Dim ctrlWs As Worksheet
    Set ctrlWs = Sheets(CTRL_SHEET)

    Application.ScreenUpdating = False
    Application.Calculation = xlCalculationManual
    Application.EnableEvents = False

    On Error GoTo CleanUp

    ' ---- Read config ----
    Dim cfgWs As Worksheet
    Set cfgWs = Sheets(CFG_SHEET)

    ' ---- Version check: detect stale template ----
    Dim dataVersion As Long
    On Error Resume Next
    dataVersion = CLng(cfgWs.Range("B10").Value)
    On Error GoTo CleanUp
    If dataVersion <> VBA_VERSION Then
        Application.EnableEvents = True
        Application.Calculation = xlCalculationAutomatic
        Application.ScreenUpdating = True
        MsgBox "VBA template is out of date (template v" & VBA_VERSION & _
               ", data v" & dataVersion & ")." & vbNewLine & vbNewLine & _
               "Please rebuild the template by importing the updated" & vbNewLine & _
               "Module1.bas from inst/turf-vba/TURF/ into the VBA editor.", _
               vbExclamation, "TURF Template Mismatch"
        Exit Sub
    End If

    Dim nItems As Long
    nItems = CLng(cfgWs.Range(CFG_N_ITEMS).Value)

    Dim hasWeights As Boolean
    hasWeights = (UCase(CStr(cfgWs.Range(CFG_HAS_WEIGHTS).Value)) = "TRUE")

    ' ---- Clear ALL output areas upfront ----
    ' This ensures no stale/partial data remains if a write sub errors partway.
    Call ClearGreedyArea(grWs, nItems)
    Call ClearDashboardGreedy(dashWs, nItems)
    Call ClearChart(dashWs)

    ' ---- Read control states from _controls ----
    Dim selSubgroup As String
    selSubgroup = CStr(ctrlWs.Range(CC_SUBGROUP).Value)

    Dim selCombo As Long
    selCombo = CLng(ctrlWs.Range(CC_COMBO).Value)

    Dim selOptimize As String
    selOptimize = LCase(CStr(ctrlWs.Range(CC_OPTIMIZE).Value))

    Dim useWeighted As Boolean
    If hasWeights Then
        useWeighted = (UCase(CStr(ctrlWs.Range(CC_WEIGHTED).Value)) = "YES")
    Else
        useWeighted = False
    End If

    ' ---- Read checkbox states from Dashboard (items panel) ----
    Dim allVars() As String
    ReDim allVars(1 To nItems)

    Dim includedFlags() As Boolean
    ReDim includedFlags(1 To nItems)

    Dim includedVars() As String
    Dim nIncluded As Long
    nIncluded = 0

    Dim i As Long
    For i = 1 To nItems
        allVars(i) = CStr(cfgWs.Cells(CFG_ITEMS_START_ROW + i - 1, CFG_ITEMS_VAR_COL).Value)
        includedFlags(i) = CBool(dashWs.Cells(ITEMS_START_ROW + i - 1, ITEMS_CHECK_COL).Value)
        If includedFlags(i) Then
            nIncluded = nIncluded + 1
        End If
    Next i

    ' Build included vars array
    If nIncluded = 0 Then
        ' Greedy + Dashboard already cleared upfront; just clear combos too
        Call ClearComboArea(bcWs)
        GoTo CleanUp
    End If

    ReDim includedVars(1 To nIncluded)
    Dim idx As Long
    idx = 0
    For i = 1 To nItems
        If includedFlags(i) Then
            idx = idx + 1
            includedVars(idx) = allVars(i)
        End If
    Next i

    ' ---- Load raw data filtered by subgroup ----
    Dim rawWs As Worksheet
    Set rawWs = Sheets(RAW_SHEET)

    ' selSubgroup is a display label (spaces). Map back to raw name (underscores)
    ' via _config col I (display) → col F (raw).
    Dim rawSubgroup As String
    rawSubgroup = GetRawSubgroup(cfgWs, selSubgroup)

    Dim sgColName As String
    sgColName = "sg_" & rawSubgroup

    Dim sgColIdx As Long
    sgColIdx = FindColumnIndex(rawWs, sgColName)

    If sgColIdx = 0 Then
        MsgBox "Subgroup column '" & sgColName & "' not found in _raw sheet.", vbExclamation
        GoTo CleanUp
    End If

    Dim wColIdx As Long
    wColIdx = FindColumnIndex(rawWs, "weight")

    Dim itemColIndices() As Long
    ReDim itemColIndices(1 To nIncluded)
    For i = 1 To nIncluded
        itemColIndices(i) = FindColumnIndex(rawWs, includedVars(i))
        If itemColIndices(i) = 0 Then
            MsgBox "Item column '" & includedVars(i) & "' not found in _raw sheet.", vbExclamation
            GoTo CleanUp
        End If
    Next i

    ' Load data into arrays for speed
    Dim lastRow As Long
    lastRow = rawWs.Cells(rawWs.Rows.Count, 1).End(xlUp).Row
    Dim nResp As Long
    nResp = lastRow - 1

    Dim rawData As Variant
    rawData = rawWs.Range(rawWs.Cells(2, 1), rawWs.Cells(lastRow, rawWs.Cells(1, rawWs.Columns.Count).End(xlToLeft).Column)).Value

    ' Filter to subgroup
    Dim filtIdx() As Long
    Dim nFilt As Long
    nFilt = 0
    ReDim filtIdx(1 To nResp)

    For i = 1 To nResp
        If CLng(rawData(i, sgColIdx)) = 1 Then
            nFilt = nFilt + 1
            filtIdx(nFilt) = i
        End If
    Next i

    If nFilt = 0 Then
        Call ClearGreedyArea(grWs, nItems)
        Call ClearDashboardGreedy(dashWs, nItems)
        Call ClearComboArea(bcWs)
        Call ClearChart(dashWs)
        GoTo CleanUp
    End If

    ReDim Preserve filtIdx(1 To nFilt)

    ' Build respondent x item matrix + weight vector
    Dim respMatrix() As Long
    Dim respWeights() As Double
    ReDim respMatrix(1 To nFilt, 1 To nIncluded)
    ReDim respWeights(1 To nFilt)

    Dim j As Long
    For i = 1 To nFilt
        If useWeighted Then
            respWeights(i) = CDbl(rawData(filtIdx(i), wColIdx))
        Else
            respWeights(i) = 1#
        End If
        For j = 1 To nIncluded
            respMatrix(i, j) = CLng(rawData(filtIdx(i), itemColIndices(j)))
        Next j
    Next i

    ' ---- Run greedy build ----
    Dim greedyOrder() As Long
    Dim greedyCumul() As Double
    Dim greedyIncr() As Double
    Dim greedyAbs() As Double
    Dim greedyAvgFrq() As Double
    Dim greedyPval() As Double

    Call RunGreedy(respMatrix, respWeights, nFilt, nIncluded, _
                   selOptimize, _
                   greedyOrder, greedyCumul, greedyIncr, greedyAbs, greedyAvgFrq, _
                   greedyPval)

    ' ---- Write greedy results to Greedy sheet ----
    Call WriteGreedyResults(grWs, nIncluded, includedVars, cfgWs, nItems, _
                           greedyOrder, greedyCumul, greedyIncr, greedyAbs, greedyAvgFrq)

    ' ---- Write greedy table to Dashboard (cols R-Y) ----
    Call WriteDashboardGreedy(dashWs, nIncluded, includedVars, cfgWs, nItems, _
                              greedyOrder, greedyCumul, greedyIncr, greedyAbs, greedyAvgFrq, _
                              greedyPval)

    ' ---- Write chart staging to Greedy sheet + update chart on Dashboard ----
    Call WriteChartStaging(grWs, nIncluded, includedVars, cfgWs, nItems, _
                          greedyOrder, greedyCumul, greedyIncr, selOptimize)

    Call UpdateChart(dashWs, grWs, nIncluded)

    ' ---- Write combo results to Best Combos sheet ----
    ' Defer if triggered from Dashboard; rebuild when user switches to Best Combos
    mBCLastRank = 0
    mBCLastCombo = 0
    If ActiveSheet.Name = COMBOS_SHEET Then
        Call WriteComboResults(bcWs, selSubgroup, selCombo, selOptimize, useWeighted, _
                              includedVars, nIncluded, includedFlags, cfgWs, nItems)
        Sheets(CTRL_SHEET).Range(CC_INVALIDATE_BC).Value = 0
    Else
        Sheets(CTRL_SHEET).Range(CC_INVALIDATE_BC).Value = 1
    End If

    ' ---- Force recalc base on both visible sheets ----
    dashWs.Range(CTRL_BASE).Calculate
    bcWs.Range(BC_BASE).Calculate

CleanUp:
    Application.EnableEvents = True
    Application.Calculation = xlCalculationAutomatic
    Application.ScreenUpdating = True

    ' ---- Draw Best Combos chart AFTER ScreenUpdating restored ----
    ' Must happen after CleanUp so chart renders properly on Mac
    If mBCLastRank > 0 Then
        Application.EnableEvents = False
        Call UpdateBCChart(bcWs, mBCLastRank, mBCLastCombo)

        ' Apply AutoFilter — select a cell in the header row, then toggle
        If bcWs.AutoFilterMode Then bcWs.AutoFilterMode = False
        bcWs.Cells(BC_HEADER_ROW, 2).Select
        Selection.AutoFilter

        Application.EnableEvents = True
    End If

End Sub


' =============================================================================
' RunGreedy — Stepwise greedy TURF algorithm
' =============================================================================
Private Sub RunGreedy( _
    ByRef respMatrix() As Long, _
    ByRef respWeights() As Double, _
    ByVal nResp As Long, _
    ByVal nItems As Long, _
    ByVal optimizeBy As String, _
    ByRef outOrder() As Long, _
    ByRef outCumul() As Double, _
    ByRef outIncr() As Double, _
    ByRef outAbs() As Double, _
    ByRef outAvgFrq() As Double, _
    ByRef outPval() As Double _
)
    ReDim outOrder(1 To nItems)
    ReDim outCumul(1 To nItems)
    ReDim outIncr(1 To nItems)
    ReDim outAbs(1 To nItems)
    ReDim outAvgFrq(1 To nItems)
    ReDim outPval(1 To nItems)

    Dim totalWeight As Double
    totalWeight = 0#
    Dim i As Long, j As Long, k As Long
    For i = 1 To nResp
        totalWeight = totalWeight + respWeights(i)
    Next i

    If totalWeight = 0 Then Exit Sub

    Dim selected() As Boolean
    ReDim selected(1 To nItems)
    For j = 1 To nItems
        selected(j) = False
    Next j

    Dim reached() As Boolean
    ReDim reached(1 To nResp)
    For i = 1 To nResp
        reached(i) = False
    Next i

    Dim respCount() As Long
    ReDim respCount(1 To nResp)

    Dim curReachWeight As Double
    curReachWeight = 0#

    ' Pre-compute absolute (standalone) reach for each item
    ' absCountArr = unweighted respondent counts (for p-value binomial test)
    Dim absCountArr() As Long
    ReDim absCountArr(1 To nItems)
    For j = 1 To nItems
        Dim absW As Double
        absW = 0#
        Dim absCnt As Long
        absCnt = 0
        For i = 1 To nResp
            If respMatrix(i, j) = 1 Then
                absW = absW + respWeights(i)
                absCnt = absCnt + 1
            End If
        Next i
        outAbs(j) = absW / totalWeight * 100#
        absCountArr(j) = absCnt
    Next j

    ' Greedy steps
    For k = 1 To nItems
        Dim bestItem As Long
        bestItem = 0
        Dim bestScore As Double
        bestScore = -1#

        For j = 1 To nItems
            If Not selected(j) Then
                Dim candidateReachW As Double
                Dim candidateFreqSum As Double
                candidateReachW = curReachWeight
                candidateFreqSum = 0#

                For i = 1 To nResp
                    If respMatrix(i, j) = 1 Then
                        If Not reached(i) Then
                            candidateReachW = candidateReachW + respWeights(i)
                        End If
                    End If
                    Dim respTotal As Long
                    respTotal = respCount(i) + respMatrix(i, j)
                    If respTotal > 0 Then
                        candidateFreqSum = candidateFreqSum + CDbl(respTotal) * respWeights(i)
                    End If
                Next i

                Dim score As Double
                If optimizeBy = "freq" Then
                    score = candidateFreqSum / totalWeight
                Else
                    score = candidateReachW
                End If

                If score > bestScore Then
                    bestScore = score
                    bestItem = j
                End If
            End If
        Next j

        If bestItem = 0 Then Exit For

        selected(bestItem) = True
        outOrder(k) = bestItem

        ' --- P-value: count unreached and newly reached BEFORE updating reached() ---
        Dim nUnreached As Long
        Dim nNew As Long
        nUnreached = 0
        nNew = 0
        For i = 1 To nResp
            If Not reached(i) Then
                nUnreached = nUnreached + 1
                If respMatrix(i, bestItem) = 1 Then
                    nNew = nNew + 1
                End If
            End If
        Next i

        ' Binomial exact test: P(X >= nNew | nUnreached, p0)
        ' p0 = mean standalone reach rate of remaining UNSELECTED items
        ' This tests: "does this item reach more unreached respondents
        '              than a typical remaining item would?"
        If nUnreached > 0 And nNew > 0 Then
            ' Compute mean standalone rate of unselected items (excluding bestItem)
            Dim p0 As Double
            Dim p0Sum As Double
            Dim p0Count As Long
            p0Sum = 0#
            p0Count = 0
            For j = 1 To nItems
                If Not selected(j) And j <> bestItem Then
                    p0Sum = p0Sum + CDbl(absCountArr(j))
                    p0Count = p0Count + 1
                End If
            Next j

            If p0Count > 0 Then
                p0 = (p0Sum / CDbl(p0Count)) / CDbl(nResp)
            Else
                ' Last item standing — use overall mean
                p0 = CDbl(absCountArr(bestItem)) / CDbl(nResp)
            End If

            If p0 > 0# And p0 < 1# And nNew >= 1 And nUnreached >= 1 Then
                ' P(X >= nNew) = 1 - P(X <= nNew-1)
                outPval(k) = SafeBinomPval(CLng(nNew), CLng(nUnreached), p0)
            Else
                outPval(k) = 0#
            End If
        Else
            outPval(k) = 1#  ' no new reach = not significant
        End If

        ' --- Update reached() and curReachWeight ---
        For i = 1 To nResp
            respCount(i) = respCount(i) + respMatrix(i, bestItem)
            If respMatrix(i, bestItem) = 1 And Not reached(i) Then
                reached(i) = True
                curReachWeight = curReachWeight + respWeights(i)
            End If
        Next i

        outCumul(k) = curReachWeight / totalWeight * 100#

        If k = 1 Then
            outIncr(k) = outCumul(k)
        Else
            outIncr(k) = outCumul(k) - outCumul(k - 1)
        End If

        ' Average frequency among reached respondents only
        Dim freqSum As Double
        Dim reachedWeight As Double
        freqSum = 0#
        reachedWeight = 0#
        For i = 1 To nResp
            If respCount(i) > 0 Then
                freqSum = freqSum + CDbl(respCount(i)) * respWeights(i)
                reachedWeight = reachedWeight + respWeights(i)
            End If
        Next i
        If reachedWeight > 0# Then
            outAvgFrq(k) = freqSum / reachedWeight
        Else
            outAvgFrq(k) = 0#
        End If

    Next k

End Sub


' =============================================================================
' SafeBinomPval — Isolated wrapper for Binom_Dist with own error handler
' Returns 1 - P(X <= nNew-1) or 0 on failure
' =============================================================================
Private Function SafeBinomPval( _
    ByVal nNew As Long, _
    ByVal nTrials As Long, _
    ByVal p0 As Double _
) As Double
    On Error GoTo BinomFail
    SafeBinomPval = 1# - Application.WorksheetFunction.Binom_Dist( _
        nNew - 1, nTrials, p0, True)
    Exit Function
BinomFail:
    SafeBinomPval = 0#
End Function


' =============================================================================
' WriteGreedyResults — Writes greedy table to Greedy sheet row 3+
' =============================================================================
Private Sub WriteGreedyResults( _
    ByVal grWs As Worksheet, _
    ByVal nIncluded As Long, _
    ByRef includedVars() As String, _
    ByVal cfgWs As Worksheet, _
    ByVal nItems As Long, _
    ByRef greedyOrder() As Long, _
    ByRef greedyCumul() As Double, _
    ByRef greedyIncr() As Double, _
    ByRef greedyAbs() As Double, _
    ByRef greedyAvgFrq() As Double _
)
    ' NOTE: ClearGreedyArea already called upfront in BuildTURF

    Dim i As Long
    For i = 1 To nIncluded
        Dim r As Long
        r = GR_DATA_ROW + i - 1

        Dim itemVar As String
        itemVar = includedVars(greedyOrder(i))

        Dim itemLabel As String
        itemLabel = GetItemLabel(cfgWs, nItems, itemVar)

        grWs.Cells(r, 1).Value = i                                           ' A: step #
        grWs.Cells(r, 2).Value = itemLabel                                    ' B: item label
        grWs.Cells(r, 3).Value = Round(greedyCumul(i) / 100#, 3)             ' C: cumul
        grWs.Cells(r, 3).NumberFormat = "0.0%"
        grWs.Cells(r, 4).Value = Round(greedyIncr(i) / 100#, 3)              ' D: incr
        grWs.Cells(r, 4).NumberFormat = "0.0%"
        grWs.Cells(r, 5).Value = Round(greedyAvgFrq(i), 2)                    ' E: avg freq
        grWs.Cells(r, 5).NumberFormat = "0.00"
        grWs.Cells(r, 6).Value = Round(greedyAbs(greedyOrder(i)) / 100#, 3)  ' F: abs
        grWs.Cells(r, 6).NumberFormat = "0.0%"
    Next i

End Sub


' =============================================================================
' WriteDashboardGreedy — Mirrors greedy table to Dashboard cols R-Y
' =============================================================================
Private Sub WriteDashboardGreedy( _
    ByVal dashWs As Worksheet, _
    ByVal nIncluded As Long, _
    ByRef includedVars() As String, _
    ByVal cfgWs As Worksheet, _
    ByVal nItems As Long, _
    ByRef greedyOrder() As Long, _
    ByRef greedyCumul() As Double, _
    ByRef greedyIncr() As Double, _
    ByRef greedyAbs() As Double, _
    ByRef greedyAvgFrq() As Double, _
    ByRef greedyPval() As Double _
)
    ' NOTE: ClearDashboardGreedy already called upfront in BuildTURF

    Dim i As Long
    For i = 1 To nIncluded
        Dim r As Long
        r = DG_DATA_ROW + i - 1

        Dim itemVar As String
        itemVar = includedVars(greedyOrder(i))

        Dim itemLabel As String
        itemLabel = GetItemLabel(cfgWs, nItems, itemVar)

        dashWs.Cells(r, DG_STEP_COL).Value = i                                           ' R: step #
        dashWs.Cells(r, DG_LABEL_COL).Value = itemVar                                     ' S: variable name
        dashWs.Cells(r, DG_ITEM_LABEL_COL).Value = itemLabel                              ' T: label text
        dashWs.Cells(r, DG_CUMUL_COL).Value = Round(greedyCumul(i) / 100#, 3)             ' U: cumul
        dashWs.Cells(r, DG_CUMUL_COL).NumberFormat = "0.0%"
        dashWs.Cells(r, DG_INCR_COL).Value = Round(greedyIncr(i) / 100#, 3)              ' V: incr
        dashWs.Cells(r, DG_INCR_COL).NumberFormat = "0.0%"
        dashWs.Cells(r, DG_FREQ_COL).Value = Round(greedyAvgFrq(i), 2)                    ' W: avg freq
        dashWs.Cells(r, DG_FREQ_COL).NumberFormat = "0.00"
        dashWs.Cells(r, DG_ABS_COL).Value = Round(greedyAbs(greedyOrder(i)) / 100#, 3)   ' X: abs
        dashWs.Cells(r, DG_ABS_COL).NumberFormat = "0.0%"
        dashWs.Cells(r, DG_PVAL_COL).Value = greedyPval(i)                                ' Y: p-value
        dashWs.Cells(r, DG_PVAL_COL).NumberFormat = "0.00%"
    Next i

    ' Center-align numeric columns (R, U-Y)
    Dim lastR As Long
    lastR = DG_DATA_ROW + nIncluded - 1
    dashWs.Range(dashWs.Cells(DG_DATA_ROW, DG_STEP_COL), dashWs.Cells(lastR, DG_STEP_COL)).HorizontalAlignment = xlCenter
    dashWs.Range(dashWs.Cells(DG_DATA_ROW, DG_CUMUL_COL), dashWs.Cells(lastR, DG_PVAL_COL)).HorizontalAlignment = xlCenter

    ' Set font on entire results table
    With dashWs.Range(dashWs.Cells(DG_DATA_ROW, DG_STEP_COL), dashWs.Cells(lastR, DG_PVAL_COL)).Font
        .Name = "Calibri"
        .Size = 11
    End With

    ' Apply conditional formatting color scales to Cumul, Incr, Freq, Abs columns
    Call ApplyGreedyConditionalFormatting(dashWs, nIncluded)

    ' Apply p-value significance coloring
    Dim sigThresh As Double
    Dim margThresh As Double
    sigThresh = 0.1
    margThresh = 0.2
    If Not IsEmpty(cfgWs.Range(CFG_SIG_THRESH).Value) Then
        If IsNumeric(cfgWs.Range(CFG_SIG_THRESH).Value) Then sigThresh = CDbl(cfgWs.Range(CFG_SIG_THRESH).Value)
    End If
    If Not IsEmpty(cfgWs.Range(CFG_MARG_THRESH).Value) Then
        If IsNumeric(cfgWs.Range(CFG_MARG_THRESH).Value) Then margThresh = CDbl(cfgWs.Range(CFG_MARG_THRESH).Value)
    End If

    Dim pRow As Long
    For pRow = DG_DATA_ROW To lastR
        Dim pVal As Double
        pVal = CDbl(dashWs.Cells(pRow, DG_PVAL_COL).Value)

        If pVal < sigThresh Then
            ' Green: significant
            dashWs.Cells(pRow, DG_PVAL_COL).Font.Name = "Calibri"
            dashWs.Cells(pRow, DG_PVAL_COL).Font.Color = RGB(46, 125, 50)
            dashWs.Cells(pRow, DG_PVAL_COL).Font.Bold = True
        ElseIf pVal < margThresh Then
            ' Orange: marginal
            dashWs.Cells(pRow, DG_PVAL_COL).Font.Name = "Calibri"
            dashWs.Cells(pRow, DG_PVAL_COL).Font.Color = RGB(230, 81, 0)
            dashWs.Cells(pRow, DG_PVAL_COL).Font.Bold = False
        Else
            ' Red: not significant
            dashWs.Cells(pRow, DG_PVAL_COL).Font.Name = "Calibri"
            dashWs.Cells(pRow, DG_PVAL_COL).Font.Color = RGB(183, 28, 28)
            dashWs.Cells(pRow, DG_PVAL_COL).Font.Bold = False
        End If
    Next pRow

    ' Redraw outer border on TURF Results data area (R9:Y + lastR)
    Dim borderRng As Range
    Set borderRng = dashWs.Range(dashWs.Cells(DG_DATA_ROW, DG_STEP_COL), dashWs.Cells(lastR, DG_PVAL_COL))
    borderRng.Borders(7).LineStyle = xlContinuous   ' xlEdgeLeft
    borderRng.Borders(7).Weight = 3                  ' xlMedium
    borderRng.Borders(8).LineStyle = xlContinuous   ' xlEdgeTop
    borderRng.Borders(8).Weight = 3
    borderRng.Borders(9).LineStyle = xlContinuous   ' xlEdgeBottom
    borderRng.Borders(9).Weight = 3
    borderRng.Borders(10).LineStyle = xlContinuous  ' xlEdgeRight
    borderRng.Borders(10).Weight = 3

    ' Write metric definitions footer
    Call WriteGreedyFooter(dashWs, nIncluded, sigThresh, margThresh)

End Sub


' =============================================================================
' ClearDashboardGreedy — Clears greedy table area on Dashboard (R-Y) + footer
' =============================================================================
Private Sub ClearDashboardGreedy(ByVal dashWs As Worksheet, ByVal nItems As Long)
    Dim clearEnd As Long
    clearEnd = DG_DATA_ROW + nItems + 15  ' extra rows to cover footer

    Dim clearRng As Range
    Set clearRng = dashWs.Range(dashWs.Cells(DG_DATA_ROW, DG_STEP_COL), dashWs.Cells(clearEnd, DG_PVAL_COL))

    ' Unmerge the ENTIRE range first — footer merges live at
    ' DG_DATA_ROW + nIncluded (variable), not DG_DATA_ROW + nItems (fixed).
    On Error Resume Next
    clearRng.UnMerge
    On Error GoTo 0

    clearRng.Clear  ' ClearContents + formatting (font, fill, borders, number format)
End Sub


' =============================================================================
' WriteGreedyFooter — Metric definitions below greedy table
' =============================================================================
Private Sub WriteGreedyFooter( _
    ByVal dashWs As Worksheet, _
    ByVal nIncluded As Long, _
    ByVal sigThresh As Double, _
    ByVal margThresh As Double _
)
    Dim footerStart As Long
    footerStart = DG_DATA_ROW + nIncluded   ' one row below last data row

    Dim defs(1 To 7) As String
    defs(1) = Chr(35) & " " & ChrW(8212) & " Greedy step number (order in which items were selected)"
    defs(2) = "Cumul " & ChrW(8212) & " Cumulative unduplicated reach (% of respondents reached through this step)"
    defs(3) = "Incr " & ChrW(8212) & " Incremental reach (% points added by this item beyond previous step)"
    defs(4) = "Avg Freq " & ChrW(8212) & " Average frequency among reached respondents (mean items selected per reached respondent)"
    defs(5) = "Abs " & ChrW(8212) & " Absolute/standalone reach (% who selected this item regardless of others)"
    defs(6) = "p-value " & ChrW(8212) & " Binomial exact test for incremental reach significance"
    defs(7) = "Method: P(X >= n_new | n_unreached, p0 = mean rate of remaining items). " & _
              "Green < " & Format(sigThresh * 100, "0") & "%" & _
              ", Orange < " & Format(margThresh * 100, "0") & "%" & _
              ", Red >= " & Format(margThresh * 100, "0") & "%"

    Dim r As Long
    r = footerStart + 1   ' one blank row, then definitions start

    Dim d As Long
    For d = 1 To 7
        dashWs.Cells(r, DG_STEP_COL).Value = defs(d)

        ' Merge across R:Y for this row
        dashWs.Range(dashWs.Cells(r, DG_STEP_COL), dashWs.Cells(r, DG_PVAL_COL)).Merge

        With dashWs.Cells(r, DG_STEP_COL).Font
            .Name = "Calibri"
            .Size = 9
            .Italic = True
            .Color = RGB(89, 89, 89)
            .Bold = False
        End With
        dashWs.Cells(r, DG_STEP_COL).HorizontalAlignment = xlLeft

        r = r + 1
    Next d
End Sub


' =============================================================================
' ApplyGreedyConditionalFormatting — 3-color scale on Cumul, Incr, Abs cols
' Uses FormatConditions.AddColorScale for Mac-compatible color scales
' Green (high) → Yellow (mid) → Red (low)
' =============================================================================
Private Sub ApplyGreedyConditionalFormatting(ByVal dashWs As Worksheet, ByVal nItems As Long)
    If nItems = 0 Then Exit Sub

    Dim lastRow As Long
    lastRow = DG_DATA_ROW + nItems - 1

    ' Column indices for conditional formatting
    Dim colIndices As Variant
    colIndices = Array(DG_CUMUL_COL, DG_INCR_COL, DG_FREQ_COL, DG_ABS_COL)  ' U, V, W, X

    Dim c As Long
    Dim rng As Range
    Dim cs As Object  ' ColorScale

    Dim ci As Long
    For ci = LBound(colIndices) To UBound(colIndices)
        c = colIndices(ci)
        Set rng = dashWs.Range(dashWs.Cells(DG_DATA_ROW, c), dashWs.Cells(lastRow, c))

        ' Clear any existing conditional formatting on this range
        rng.FormatConditions.Delete

        ' Add 3-color scale: Red (low) → Yellow (mid) → Green (high)
        Set cs = rng.FormatConditions.AddColorScale(ColorScaleType:=3)

        ' Criteria 1: Minimum → Red
        cs.ColorScaleCriteria(1).Type = 1  ' xlConditionValueLowestValue
        cs.ColorScaleCriteria(1).FormatColor.Color = RGB(248, 105, 107)  ' red

        ' Criteria 2: Midpoint → Yellow
        cs.ColorScaleCriteria(2).Type = 4  ' xlConditionValuePercentile
        cs.ColorScaleCriteria(2).Value = 50
        cs.ColorScaleCriteria(2).FormatColor.Color = RGB(255, 235, 132)  ' yellow

        ' Criteria 3: Maximum → Green
        cs.ColorScaleCriteria(3).Type = 2  ' xlConditionValueHighestValue
        cs.ColorScaleCriteria(3).FormatColor.Color = RGB(99, 190, 123)  ' green
    Next ci
End Sub


' =============================================================================
' WriteChartStaging — Writes staging data to Greedy sheet cols H-K
' =============================================================================
Private Sub WriteChartStaging( _
    ByVal grWs As Worksheet, _
    ByVal nIncluded As Long, _
    ByRef includedVars() As String, _
    ByVal cfgWs As Worksheet, _
    ByVal nItems As Long, _
    ByRef greedyOrder() As Long, _
    ByRef greedyCumul() As Double, _
    ByRef greedyIncr() As Double, _
    ByVal optimizeBy As String _
)
    ' Clear staging area (cols H-K)
    grWs.Range(grWs.Cells(1, GR_STAGE_STEP), grWs.Cells(nItems + 5, GR_STAGE_INCR)).ClearContents

    ' Headers
    grWs.Cells(1, GR_STAGE_STEP).Value = "Step"
    grWs.Cells(1, GR_STAGE_LABEL).Value = "Item"
    grWs.Cells(1, GR_STAGE_CUMUL).Value = "PrevCumul"
    grWs.Cells(1, GR_STAGE_INCR).Value = "Incr"

    ' Read chart label mode from _controls
    Dim chartLabelMode As String
    chartLabelMode = CStr(Sheets(CTRL_SHEET).Range(CC_CHART_LABEL).Value)

    Dim i As Long
    For i = 1 To nIncluded
        Dim r As Long
        r = 1 + i  ' row 2, 3, 4, ...

        Dim itemVar As String
        itemVar = includedVars(greedyOrder(i))

        Dim itemLabel As String
        itemLabel = GetItemLabel(cfgWs, nItems, itemVar)

        ' Format chart label based on mode
        Dim chartLabel As String
        Select Case chartLabelMode
            Case "Variable"
                chartLabel = itemVar
            Case "Variable - Label"
                chartLabel = itemVar & " - " & itemLabel
            Case Else  ' "Label"
                chartLabel = itemLabel
        End Select

        If Len(chartLabel) > 50 Then
            chartLabel = Left(chartLabel, 47) & "..."
        End If

        grWs.Cells(r, GR_STAGE_STEP).Value = i
        grWs.Cells(r, GR_STAGE_LABEL).Value = chartLabel

        ' For stacked chart: PrevCumul = cumul - incr (the base portion)
        Dim prevCumul As Double
        If i = 1 Then
            prevCumul = 0#
        Else
            prevCumul = Round(greedyCumul(i) - greedyIncr(i), 1)
        End If
        grWs.Cells(r, GR_STAGE_CUMUL).Value = prevCumul
        grWs.Cells(r, GR_STAGE_INCR).Value = Round(greedyIncr(i), 1)
    Next i

End Sub


' =============================================================================
' UpdateChart — Creates/updates stacked horizontal bar chart on Dashboard
' Two series: PrevCumul (light grey) + Incr (dark grey)
' Data lives on Greedy sheet (cross-sheet reference)
' Mac-compatible: avoids .Format property chains
' =============================================================================
Public Sub UpdateChart(ByVal chartWs As Worksheet, ByVal dataWs As Worksheet, ByVal nItems As Long)
    If Not HasControlsSheet() Then Exit Sub

    Dim chartName As String
    chartName = "TURFGreedyChart"

    ' Delete existing chart if present
    Dim co As ChartObject
    For Each co In chartWs.ChartObjects
        If co.Name = chartName Then
            co.Delete
            Exit For
        End If
    Next co

    If nItems = 0 Then Exit Sub

    ' Charts require ScreenUpdating = True on Mac to render properly
    Dim prevSU As Boolean
    prevSU = Application.ScreenUpdating
    Application.ScreenUpdating = True

    ' Data ranges on Greedy sheet
    Dim labelRange As Range
    Set labelRange = dataWs.Range(dataWs.Cells(2, GR_STAGE_LABEL), dataWs.Cells(1 + nItems, GR_STAGE_LABEL))

    Dim prevCumulRange As Range
    Set prevCumulRange = dataWs.Range(dataWs.Cells(2, GR_STAGE_CUMUL), dataWs.Cells(1 + nItems, GR_STAGE_CUMUL))

    Dim incrRange As Range
    Set incrRange = dataWs.Range(dataWs.Cells(2, GR_STAGE_INCR), dataWs.Cells(1 + nItems, GR_STAGE_INCR))

    ' Create chart on Dashboard: B9 to end of O, height = table length
    Dim lastDataRow As Long
    lastDataRow = DG_DATA_ROW + nItems  ' one row past last data row

    Dim chartHeight As Double
    chartHeight = chartWs.Cells(lastDataRow, 2).Top - chartWs.Cells(9, 2).Top
    If chartHeight < 144 Then chartHeight = 144  ' min ~2 inches

    Dim cht As ChartObject
    Set cht = chartWs.ChartObjects.Add( _
        Left:=chartWs.Cells(9, 2).Left, _
        Top:=chartWs.Cells(9, 2).Top, _
        Width:=chartWs.Cells(9, 16).Left - chartWs.Cells(9, 2).Left, _
        Height:=chartHeight _
    )
    cht.Name = chartName

    With cht.Chart
        .ChartType = xlBarStacked

        ' Series 1: Previous cumulative (light grey — the base)
        Dim serBase As Series
        Set serBase = .SeriesCollection.NewSeries
        serBase.Values = prevCumulRange
        serBase.XValues = labelRange
        serBase.Name = "Previous"

        On Error Resume Next
        serBase.Interior.Color = RGB(217, 217, 217)  ' light grey
        On Error GoTo 0

        ' Hide data labels on base series
        serBase.HasDataLabels = False

        ' Series 2: Incremental gain (dark grey — the new portion)
        Dim serIncr As Series
        Set serIncr = .SeriesCollection.NewSeries
        serIncr.Values = incrRange
        serIncr.XValues = labelRange
        serIncr.Name = "Incremental"

        On Error Resume Next
        serIncr.Interior.Color = RGB(89, 89, 89)  ' dark grey
        On Error GoTo 0

        ' Data labels on incr series only — show cumulative total
        serIncr.HasDataLabels = True
        serIncr.DataLabels.ShowValue = False
        serIncr.DataLabels.ShowCategoryName = False
        serIncr.DataLabels.ShowSeriesName = False

        ' Category axis — reverse so step 1 is at top
        .Axes(xlCategory).ReversePlotOrder = True
        .Axes(xlCategory).TickLabels.Font.Size = 8
        .Axes(xlCategory).TickLabels.Font.Name = "Calibri"

        ' Value axis
        .Axes(xlValue).HasTitle = False
        .Axes(xlValue).MaximumScale = 100
        .Axes(xlValue).MinimumScale = 0

        ' No chart title (section header in row 7 serves as title)
        .HasTitle = False

        .HasLegend = False
    End With

    Application.ScreenUpdating = prevSU

End Sub


' =============================================================================
' WriteComboResults — Writes filtered combos to Best Combos sheet row 6+
' =============================================================================
Private Sub WriteComboResults( _
    ByVal bcWs As Worksheet, _
    ByVal selSubgroup As String, _
    ByVal selCombo As Long, _
    ByVal selOptimize As String, _
    ByVal useWeighted As Boolean, _
    ByRef includedVars() As String, _
    ByVal nIncluded As Long, _
    ByRef includedFlags() As Boolean, _
    ByVal cfgWs As Worksheet, _
    ByVal nItems As Long _
)
    Call ClearComboArea(bcWs)

    ' ---- Rebuild section header (row 15) + column headers (row 16) ----
    Dim lastItemCol As Long
    lastItemCol = 4 + selCombo  ' E=5, F=6, etc.

    ' Row 15: "Combo Results" merged across B:lastItemCol, grey fill
    bcWs.Range(bcWs.Cells(15, 2), bcWs.Cells(15, lastItemCol)).Merge
    bcWs.Cells(15, 2).Value = "Combo Results"
    With bcWs.Cells(15, 2).Font
            .Name = "Calibri"
        .Bold = True
        .Size = 12
    End With
    bcWs.Cells(15, 2).HorizontalAlignment = xlCenter
    bcWs.Range(bcWs.Cells(15, 2), bcWs.Cells(15, lastItemCol)).Interior.Color = RGB(217, 217, 217)

    ' Row 16: Column headers (Rank, Reach, Freq, Item 1..N)
    Dim hdrNames As Variant
    ReDim hdrNames(1 To 3 + selCombo)
    hdrNames(1) = "Rank"
    hdrNames(2) = "Reach"
    hdrNames(3) = "Freq"
    Dim hi As Long
    For hi = 1 To selCombo
        hdrNames(3 + hi) = "Item " & CStr(hi)
    Next hi

    For hi = 1 To UBound(hdrNames)
        bcWs.Cells(BC_HEADER_ROW, 1 + hi).Value = hdrNames(hi)  ' B=2, C=3, ...
        With bcWs.Cells(BC_HEADER_ROW, 1 + hi).Font
            .Name = "Calibri"
            .Bold = True
        End With
        bcWs.Cells(BC_HEADER_ROW, 1 + hi).HorizontalAlignment = xlCenter
        bcWs.Cells(BC_HEADER_ROW, 1 + hi).Interior.Color = RGB(217, 217, 217)
    Next hi

    ' Borders on rows 15-16 combined (no inner line)
    Dim hdrBorderRng As Range
    Set hdrBorderRng = bcWs.Range(bcWs.Cells(15, 2), bcWs.Cells(BC_HEADER_ROW, lastItemCol))
    hdrBorderRng.Borders(7).LineStyle = xlContinuous   ' xlEdgeLeft
    hdrBorderRng.Borders(7).Weight = 3                  ' xlMedium
    hdrBorderRng.Borders(8).LineStyle = xlContinuous   ' xlEdgeTop
    hdrBorderRng.Borders(8).Weight = 3
    hdrBorderRng.Borders(9).LineStyle = xlContinuous   ' xlEdgeBottom
    hdrBorderRng.Borders(9).Weight = 3
    hdrBorderRng.Borders(10).LineStyle = xlContinuous  ' xlEdgeRight
    hdrBorderRng.Borders(10).Weight = 3

    ' Look up short subgroup key from _config col H (keyed by display label in col I)
    Dim sgKey As String
    Dim sgMatch As Variant
    sgMatch = Application.Match(selSubgroup, cfgWs.Range("I2:I100"), 0)
    If IsError(sgMatch) Then
        sgKey = selSubgroup  ' fallback to full name
    Else
        sgKey = CStr(cfgWs.Cells(1 + CLng(sgMatch), 8).Value)  ' col H
    End If

    ' Single sheet per subgroup x combo size (no sort suffix)
    Dim dataSheetName As String
    dataSheetName = "d_" & sgKey & "_" & CStr(selCombo)

    Dim dataWs As Worksheet
    On Error Resume Next
    Set dataWs = Sheets(dataSheetName)
    On Error GoTo 0

    If dataWs Is Nothing Then Exit Sub

    Dim lastDataRow As Long
    lastDataRow = dataWs.Cells(dataWs.Rows.Count, 1).End(xlUp).Row
    If lastDataRow < 2 Then Exit Sub

    Dim lastDataCol As Long
    lastDataCol = dataWs.Cells(1, dataWs.Columns.Count).End(xlToLeft).Column

    Dim comboData As Variant
    comboData = dataWs.Range(dataWs.Cells(1, 1), dataWs.Cells(lastDataRow, lastDataCol)).Value

    Dim reachCol As Long, freqCol As Long
    Dim firstItemCol As Long

    Dim c As Long
    reachCol = 0: freqCol = 0: firstItemCol = 0
    For c = 1 To lastDataCol
        Dim hdr As String
        hdr = CStr(comboData(1, c))
        If useWeighted And hdr = "w_reach_pct" Then reachCol = c
        If useWeighted And hdr = "w_freq_avg" Then freqCol = c
        If Not useWeighted And hdr = "reach_pct" Then reachCol = c
        If Not useWeighted And hdr = "freq_avg" Then freqCol = c
        If hdr = "item_1" Then firstItemCol = c
    Next c

    ' Fallback if weighted columns not found
    If reachCol = 0 Then
        For c = 1 To lastDataCol
            If CStr(comboData(1, c)) = "reach_pct" Then reachCol = c
        Next c
    End If
    If freqCol = 0 Then
        For c = 1 To lastDataCol
            If CStr(comboData(1, c)) = "freq_avg" Then freqCol = c
        Next c
    End If

    If reachCol = 0 Or freqCol = 0 Or firstItemCol = 0 Then Exit Sub

    ' Limit rows to the smaller of _config top and Display control
    Dim maxComboRows As Long
    maxComboRows = CLng(Sheets(CFG_SHEET).Range(CFG_TOP).Value)

    Dim displayLimit As Long
    If IsNumeric(bcWs.Range(BC_DISPLAY).Value) And Not IsEmpty(bcWs.Range(BC_DISPLAY).Value) Then
        displayLimit = CLng(bcWs.Range(BC_DISPLAY).Value)
        If displayLimit < maxComboRows Then maxComboRows = displayLimit
    End If

    ' Number of output columns: Rank + Reach + Freq + selCombo item labels
    Dim nOutCols As Long
    nOutCols = 3 + selCombo

    ' Size output array to max possible rows (filter all, sort + trim later)
    Dim maxRows As Long
    maxRows = lastDataRow - 1

    Dim outArr() As Variant
    ReDim outArr(1 To maxRows, 1 To nOutCols)

    Dim rank As Long
    rank = 0

    ' Filter all rows (no display limit during filtering).
    ' If freq sort needed, we sort + trim after writing.
    Dim dr As Long
    For dr = 2 To lastDataRow
        Dim allIncluded As Boolean
        allIncluded = True

        ' Item columns now contain 1-based integer indices into _config items
        Dim ic As Long
        For ic = firstItemCol To firstItemCol + selCombo - 1
            If ic > lastDataCol Then
                allIncluded = False
                Exit For
            End If
            Dim itemIdx As Long
            itemIdx = CLng(comboData(dr, ic))
            If itemIdx < 1 Or itemIdx > nItems Then
                allIncluded = False
                Exit For
            End If
            If Not includedFlags(itemIdx) Then
                allIncluded = False
                Exit For
            End If
        Next ic

        If allIncluded Then
            rank = rank + 1

            outArr(rank, 1) = rank                                              ' Rank
            outArr(rank, 2) = CDbl(comboData(dr, reachCol)) / 100#              ' Reach
            outArr(rank, 3) = Round(CDbl(comboData(dr, freqCol)), 2)            ' Freq

            ' Look up item labels from _config using integer index
            For ic = 0 To selCombo - 1
                Dim iIdx As Long
                iIdx = CLng(comboData(dr, firstItemCol + ic))
                outArr(rank, 4 + ic) = GetItemLabelByIndex(cfgWs, iIdx)
            Next ic
        End If
    Next dr

    ' Write array to sheet in one shot
    If rank > 0 Then
        ' Trim to display limit
        Dim writeRows As Long
        writeRows = rank
        If writeRows > maxComboRows Then writeRows = maxComboRows

        ' Write all rows needed (writeRows for reach, rank for freq pre-sort)
        Dim rowsToWrite As Long
        If selOptimize = "freq" Then
            rowsToWrite = rank  ' write all, then sort + trim
        Else
            rowsToWrite = writeRows  ' already sorted by reach
        End If

        Dim destRng As Range
        Set destRng = bcWs.Range(bcWs.Cells(BC_DATA_ROW, 2), _
                                  bcWs.Cells(BC_DATA_ROW + rowsToWrite - 1, 1 + nOutCols))
        destRng.Value = outArr

        ' Sort by freq if needed (data is stored sorted by reach from R)
        If selOptimize = "freq" And rowsToWrite > 1 Then
            destRng.Sort Key1:=bcWs.Cells(BC_DATA_ROW, 4), Order1:=xlDescending, Header:=xlNo

            ' Clear excess rows beyond display limit
            If rowsToWrite > writeRows Then
                bcWs.Range(bcWs.Cells(BC_DATA_ROW + writeRows, 2), _
                           bcWs.Cells(BC_DATA_ROW + rowsToWrite - 1, 1 + nOutCols)).ClearContents
            End If
        End If

        ' Re-number ranks (always, since sort may have reordered)
        Dim ri As Long
        For ri = 1 To writeRows
            bcWs.Cells(BC_DATA_ROW + ri - 1, 2).Value = ri
        Next ri

        rank = writeRows

        ' Number formats (bulk)
        bcWs.Range(bcWs.Cells(BC_DATA_ROW, 3), bcWs.Cells(BC_DATA_ROW + rank - 1, 3)).NumberFormat = "0.0%"
        bcWs.Range(bcWs.Cells(BC_DATA_ROW, 4), bcWs.Cells(BC_DATA_ROW + rank - 1, 4)).NumberFormat = "0.00"

        ' Center-align Rank, Reach, Freq columns (B, C, D)
        bcWs.Range(bcWs.Cells(BC_DATA_ROW, 2), bcWs.Cells(BC_DATA_ROW + rank - 1, 4)).HorizontalAlignment = xlCenter

        ' Outer border around combo results data area
        Dim borderRng As Range
        Set borderRng = bcWs.Range(bcWs.Cells(BC_DATA_ROW, 2), bcWs.Cells(BC_DATA_ROW + rank - 1, lastItemCol))
        borderRng.Borders(7).LineStyle = xlContinuous   ' xlEdgeLeft
        borderRng.Borders(7).Weight = 3                  ' xlMedium
        borderRng.Borders(8).LineStyle = xlContinuous   ' xlEdgeTop
        borderRng.Borders(8).Weight = 3
        borderRng.Borders(9).LineStyle = xlContinuous   ' xlEdgeBottom
        borderRng.Borders(9).Weight = 3
        borderRng.Borders(10).LineStyle = xlContinuous  ' xlEdgeRight
        borderRng.Borders(10).Weight = 3

        ' Autofit or reset item column widths
        Dim af As Long
        If UCase(CStr(bcWs.Range(BC_AUTOFIT).Value)) = "YES" Then
            For af = 5 To lastItemCol
                bcWs.Columns(af).AutoFit
            Next af
        Else
            For af = 5 To lastItemCol
                bcWs.Columns(af).ColumnWidth = 45
            Next af
        End If

        ' AutoFilter is applied after ScreenUpdating is restored (deferred in BuildTURF)
    End If

    ' ---- Draw chart is deferred: called AFTER BuildTURF CleanUp ----
    ' Store rank for caller to pick up via module-level variable
    mBCLastRank = rank
    mBCLastCombo = selCombo

End Sub


' =============================================================================
' Best Combos Charts — dispatched by Chart control (C11)
' Chart Loc (C12): "Top" = E2 to row 14, "Right" = beside combo results table
' =============================================================================

Private Sub ClearBCChart(ByVal bcWs As Worksheet)
    Dim co As ChartObject
    For Each co In bcWs.ChartObjects
        If co.Name = BC_CHART_NAME Then
            co.Delete
            Exit For
        End If
    Next co
End Sub


Private Sub UpdateBCChart(ByVal bcWs As Worksheet, ByVal nRows As Long, ByVal selCombo As Long)
    Call ClearBCChart(bcWs)

    Dim chartMode As String
    chartMode = CStr(bcWs.Range(BC_CHART).Value)

    If chartMode = "None" Or nRows = 0 Then Exit Sub

    Dim chartLoc As String
    chartLoc = CStr(bcWs.Range(BC_CHART_LOC).Value)
    If chartLoc <> "Right" Then chartLoc = "Top"

    ' Set padding column width when Right
    If chartLoc = "Right" Then
        Dim padCol As Long
        padCol = 4 + selCombo + 1  ' 1 col gap between last item col and chart
        bcWs.Columns(padCol).ColumnWidth = 2
    End If

    Select Case chartMode
        Case "Top Reach"
            Call DrawBCTopReach(bcWs, nRows, chartLoc, selCombo)
        Case "Reach vs Freq"
            Call DrawBCReachVsFreq(bcWs, nRows, chartLoc, selCombo)
        Case "Item Frequency"
            Call DrawBCItemFrequency(bcWs, nRows, selCombo, chartLoc)
    End Select
End Sub


' -----------------------------------------------------------------------------
' Option 1: Top N Reach — horizontal bar chart of top 20 combos by reach
' Staging data written to _best_combo_charts sheet (A=label, B=value)
' -----------------------------------------------------------------------------
Private Sub DrawBCTopReach(ByVal bcWs As Worksheet, ByVal nRows As Long, ByVal chartLoc As String, ByVal selCombo As Long)
    Dim showN As Long
    showN = nRows
    If showN > 20 Then showN = 20

    Dim stgWs As Worksheet
    Set stgWs = Sheets(BC_CHART_SHEET)

    ' Clear and write staging data
    stgWs.Cells.ClearContents
    Dim i As Long
    For i = 1 To showN
        stgWs.Cells(i, 1).Value = "#" & CStr(bcWs.Cells(BC_DATA_ROW + i - 1, 2).Value)
        stgWs.Cells(i, 2).Value = CDbl(bcWs.Cells(BC_DATA_ROW + i - 1, 3).Value) * 100#
    Next i

    ' Find min/max reach values for axis bounds
    Dim minReach As Double, maxReach As Double
    minReach = CDbl(stgWs.Cells(1, 2).Value)
    maxReach = minReach
    For i = 2 To showN
        Dim v As Double
        v = CDbl(stgWs.Cells(i, 2).Value)
        If v < minReach Then minReach = v
        If v > maxReach Then maxReach = v
    Next i
    Dim axisMin As Double
    axisMin = Int(minReach / 10#) * 10#  ' floor to nearest 10
    Dim axisMax As Double
    axisMax = Int(maxReach / 10#) * 10# + 10#  ' ceil to nearest 10

    ' Chart position: anchor at cell, absolute dimensions in points (1 in = 72 pt)
    Dim chtLeft As Double, chtTop As Double, chtWidth As Double, chtHeight As Double
    If chartLoc = "Right" Then
        ' Right: anchor at data row, 1 col padding past last item col; 8" x 8"
        Dim chartCol As Long
        chartCol = 4 + selCombo + 2
        chtLeft = bcWs.Cells(BC_DATA_ROW, chartCol).Left
        chtTop = bcWs.Cells(BC_DATA_ROW, chartCol).Top
        chtWidth = 576   ' 8 inches
        chtHeight = 576  ' 8 inches
    Else
        ' Top: anchor at E2, 12" wide, height = rows 2-14
        chtLeft = bcWs.Cells(2, 5).Left
        chtTop = bcWs.Cells(2, 5).Top
        chtWidth = 864   ' 12 inches
        chtHeight = bcWs.Cells(14, 5).Top - bcWs.Cells(2, 5).Top
    End If

    Dim cht As ChartObject
    Set cht = bcWs.ChartObjects.Add( _
        Left:=chtLeft, Top:=chtTop, Width:=chtWidth, Height:=chtHeight)
    cht.Name = BC_CHART_NAME

    Dim labelRng As Range
    Set labelRng = stgWs.Range(stgWs.Cells(1, 1), stgWs.Cells(showN, 1))
    Dim valRng As Range
    Set valRng = stgWs.Range(stgWs.Cells(1, 2), stgWs.Cells(showN, 2))

    With cht.Chart
        .ChartType = xlBarClustered

        Dim ser As Series
        Set ser = .SeriesCollection.NewSeries
        ser.Values = valRng
        ser.XValues = labelRng
        ser.Name = "Reach %"

        On Error Resume Next
        ser.Interior.Color = RGB(89, 89, 89)  ' dark grey
        On Error GoTo 0

        ser.HasDataLabels = True
        ser.DataLabels.ShowValue = True
        ser.DataLabels.NumberFormat = "0.0"
        ser.DataLabels.Font.Size = 8
        ser.DataLabels.Font.Name = "Calibri"

        .Axes(xlCategory).ReversePlotOrder = True
        .Axes(xlCategory).TickLabels.Font.Size = 8
        .Axes(xlCategory).TickLabels.Font.Name = "Calibri"
        .Axes(xlValue).HasTitle = False
        .Axes(xlValue).MaximumScale = axisMax
        .Axes(xlValue).MinimumScale = axisMin

        .HasTitle = True
        .ChartTitle.Text = "Top " & CStr(showN) & " Combos by Reach"
        .ChartTitle.Font.Size = 11
        .ChartTitle.Font.Name = "Calibri"
        .ChartTitle.Font.Bold = True

        .HasLegend = False
    End With
End Sub


' -----------------------------------------------------------------------------
' Option 2: Reach vs Freq — scatter plot of top 50 combos
' Staging: A=reach%, B=freq
' -----------------------------------------------------------------------------
Private Sub DrawBCReachVsFreq(ByVal bcWs As Worksheet, ByVal nRows As Long, ByVal chartLoc As String, ByVal selCombo As Long)
    Dim showN As Long
    showN = nRows
    If showN > 50 Then showN = 50

    Dim stgWs As Worksheet
    Set stgWs = Sheets(BC_CHART_SHEET)

    ' Clear and write staging data
    stgWs.Cells.ClearContents
    Dim i As Long
    For i = 1 To showN
        stgWs.Cells(i, 1).Value = CDbl(bcWs.Cells(BC_DATA_ROW + i - 1, 3).Value) * 100#
        stgWs.Cells(i, 2).Value = CDbl(bcWs.Cells(BC_DATA_ROW + i - 1, 4).Value)
    Next i

    ' Compute axis bounds from staging data
    Dim minReach As Double, maxReach As Double
    Dim minFreq As Double, maxFreq As Double
    minReach = CDbl(stgWs.Cells(1, 1).Value): maxReach = minReach
    minFreq = CDbl(stgWs.Cells(1, 2).Value): maxFreq = minFreq
    For i = 2 To showN
        Dim vR As Double, vF As Double
        vR = CDbl(stgWs.Cells(i, 1).Value)
        vF = CDbl(stgWs.Cells(i, 2).Value)
        If vR < minReach Then minReach = vR
        If vR > maxReach Then maxReach = vR
        If vF < minFreq Then minFreq = vF
        If vF > maxFreq Then maxFreq = vF
    Next i

    ' Reach axis: floor/ceil to nearest 5
    Dim reachAxisMin As Double, reachAxisMax As Double
    reachAxisMin = Int(minReach / 5#) * 5#
    reachAxisMax = Int(maxReach / 5#) * 5# + 5#

    ' Freq axis: floor/ceil to nearest 0.1
    Dim freqAxisMin As Double, freqAxisMax As Double
    freqAxisMin = Int(minFreq / 0.1) * 0.1
    freqAxisMax = Int(maxFreq / 0.1) * 0.1 + 0.1

    ' Chart position: anchor at cell, absolute dimensions in points (1 in = 72 pt)
    Dim chtLeft As Double, chtTop As Double, chtWidth As Double, chtHeight As Double
    If chartLoc = "Right" Then
        Dim chartCol As Long
        chartCol = 4 + selCombo + 2
        chtLeft = bcWs.Cells(BC_DATA_ROW, chartCol).Left
        chtTop = bcWs.Cells(BC_DATA_ROW, chartCol).Top
        chtWidth = 576   ' 8 inches
        chtHeight = 576  ' 8 inches
    Else
        chtLeft = bcWs.Cells(2, 5).Left
        chtTop = bcWs.Cells(2, 5).Top
        chtWidth = 864   ' 12 inches
        chtHeight = bcWs.Cells(14, 5).Top - bcWs.Cells(2, 5).Top
    End If

    Dim cht As ChartObject
    Set cht = bcWs.ChartObjects.Add( _
        Left:=chtLeft, Top:=chtTop, Width:=chtWidth, Height:=chtHeight)
    cht.Name = BC_CHART_NAME

    ' Stage data split: cols A/B = others (rows 2+), cols C/D = #1 combo (row 1)
    ' Write #1 combo to C1/D1
    stgWs.Cells(1, 3).Value = CDbl(stgWs.Cells(1, 1).Value)
    stgWs.Cells(1, 4).Value = CDbl(stgWs.Cells(1, 2).Value)

    ' Shift others to rows 1..showN-1 in cols A/B (overwrite row 1)
    If showN > 1 Then
        Dim othersReachRng As Range
        Set othersReachRng = stgWs.Range(stgWs.Cells(2, 1), stgWs.Cells(showN, 1))
        Dim othersFreqRng As Range
        Set othersFreqRng = stgWs.Range(stgWs.Cells(2, 2), stgWs.Cells(showN, 2))
    End If

    Dim topReachRng As Range
    Set topReachRng = stgWs.Range(stgWs.Cells(1, 3), stgWs.Cells(1, 3))
    Dim topFreqRng As Range
    Set topFreqRng = stgWs.Range(stgWs.Cells(1, 4), stgWs.Cells(1, 4))

    With cht.Chart
        .ChartType = xlXYScatter

        ' Series 1: Others (blue, rows 2+)
        If showN > 1 Then
            Dim serOthers As Series
            Set serOthers = .SeriesCollection.NewSeries
            serOthers.XValues = othersReachRng
            serOthers.Values = othersFreqRng
            serOthers.Name = "Others"

            On Error Resume Next
            serOthers.MarkerStyle = 8  ' xlMarkerStyleCircle
            serOthers.MarkerSize = 6
            serOthers.MarkerBackgroundColor = RGB(217, 217, 217)
            serOthers.MarkerForegroundColor = RGB(217, 217, 217)
            On Error GoTo 0
        End If

        ' Series 2: #1 Combo (orange, single point)
        Dim serTop As Series
        Set serTop = .SeriesCollection.NewSeries
        serTop.XValues = topReachRng
        serTop.Values = topFreqRng
        serTop.Name = "#1 Combo"

        On Error Resume Next
        serTop.MarkerStyle = 8  ' xlMarkerStyleCircle
        serTop.MarkerSize = 9
        serTop.MarkerBackgroundColor = RGB(89, 89, 89)
        serTop.MarkerForegroundColor = RGB(89, 89, 89)
        On Error GoTo 0

        ' Reach axis (X / Category)
        .Axes(xlCategory).HasTitle = True
        .Axes(xlCategory).AxisTitle.Text = "Reach %"
        .Axes(xlCategory).AxisTitle.Font.Size = 9
        .Axes(xlCategory).AxisTitle.Font.Name = "Calibri"
        .Axes(xlCategory).MinimumScale = reachAxisMin
        .Axes(xlCategory).MaximumScale = reachAxisMax

        ' Freq axis (Y / Value)
        .Axes(xlValue).HasTitle = True
        .Axes(xlValue).AxisTitle.Text = "Avg Freq"
        .Axes(xlValue).AxisTitle.Font.Size = 9
        .Axes(xlValue).AxisTitle.Font.Name = "Calibri"
        .Axes(xlValue).MinimumScale = freqAxisMin
        .Axes(xlValue).MaximumScale = freqAxisMax

        .HasTitle = True
        .ChartTitle.Text = "Reach vs Avg Freq (Top " & CStr(showN) & ")"
        .ChartTitle.Font.Size = 11
        .ChartTitle.Font.Name = "Calibri"
        .ChartTitle.Font.Bold = True

        .HasLegend = True
        .Legend.Position = xlLegendPositionBottom
        .Legend.Font.Size = 9
        .Legend.Font.Name = "Calibri"
    End With
End Sub


' -----------------------------------------------------------------------------
' Option 3: Item Frequency — how often each item appears in top N combos
' Staging: A=label, B=count
' -----------------------------------------------------------------------------
Private Sub DrawBCItemFrequency(ByVal bcWs As Worksheet, ByVal nRows As Long, ByVal selCombo As Long, ByVal chartLoc As String)
    Dim showN As Long
    showN = nRows
    If showN > 100 Then showN = 100

    ' Count item appearances across top N combos
    ' Item labels are in columns 5 to 4+selCombo (E, F, G...)
    Dim maxUnique As Long
    maxUnique = showN * selCombo
    Dim sortLabels() As String
    Dim sortCounts() As Long
    ReDim sortLabels(1 To maxUnique)
    ReDim sortCounts(1 To maxUnique)
    Dim nUnique As Long
    nUnique = 0

    Dim i As Long, j As Long, k As Long
    For i = 1 To showN
        For j = 5 To 4 + selCombo
            Dim itemLbl As String
            itemLbl = CStr(bcWs.Cells(BC_DATA_ROW + i - 1, j).Value)
            If Len(itemLbl) > 0 Then
                Dim found As Boolean
                found = False
                For k = 1 To nUnique
                    If sortLabels(k) = itemLbl Then
                        sortCounts(k) = sortCounts(k) + 1
                        found = True
                        Exit For
                    End If
                Next k
                If Not found Then
                    nUnique = nUnique + 1
                    sortLabels(nUnique) = itemLbl
                    sortCounts(nUnique) = 1
                End If
            End If
        Next j
    Next i

    If nUnique = 0 Then Exit Sub

    ' Bubble sort descending by count
    Dim swapped As Boolean
    Dim tmpS As String
    Dim tmpL As Long
    Do
        swapped = False
        For i = 1 To nUnique - 1
            If sortCounts(i) < sortCounts(i + 1) Then
                tmpL = sortCounts(i): sortCounts(i) = sortCounts(i + 1): sortCounts(i + 1) = tmpL
                tmpS = sortLabels(i): sortLabels(i) = sortLabels(i + 1): sortLabels(i + 1) = tmpS
                swapped = True
            End If
        Next i
    Loop While swapped

    ' Limit to top 20 items for chart readability
    Dim chartN As Long
    chartN = nUnique
    If chartN > 20 Then chartN = 20

    ' Write staging data to _best_combo_charts sheet
    Dim stgWs As Worksheet
    Set stgWs = Sheets(BC_CHART_SHEET)
    stgWs.Cells.ClearContents

    For i = 1 To chartN
        Dim lbl As String
        lbl = sortLabels(i)
        If Len(lbl) > 40 Then lbl = Left(lbl, 37) & "..."
        stgWs.Cells(i, 1).Value = lbl
        stgWs.Cells(i, 2).Value = CDbl(sortCounts(i))
    Next i

    ' Chart position: anchor at cell, absolute dimensions in points (1 in = 72 pt)
    Dim chtLeft As Double, chtTop As Double, chtWidth As Double, chtHeight As Double
    If chartLoc = "Right" Then
        Dim chartCol As Long
        chartCol = 4 + selCombo + 2
        chtLeft = bcWs.Cells(BC_DATA_ROW, chartCol).Left
        chtTop = bcWs.Cells(BC_DATA_ROW, chartCol).Top
        chtWidth = 576   ' 8 inches
        chtHeight = 576  ' 8 inches
    Else
        chtLeft = bcWs.Cells(2, 5).Left
        chtTop = bcWs.Cells(2, 5).Top
        chtWidth = 864   ' 12 inches
        chtHeight = bcWs.Cells(14, 5).Top - bcWs.Cells(2, 5).Top
    End If

    Dim cht As ChartObject
    Set cht = bcWs.ChartObjects.Add( _
        Left:=chtLeft, Top:=chtTop, Width:=chtWidth, Height:=chtHeight)
    cht.Name = BC_CHART_NAME

    Dim labelRng As Range
    Set labelRng = stgWs.Range(stgWs.Cells(1, 1), stgWs.Cells(chartN, 1))
    Dim valRng As Range
    Set valRng = stgWs.Range(stgWs.Cells(1, 2), stgWs.Cells(chartN, 2))

    With cht.Chart
        .ChartType = xlBarClustered

        Dim ser As Series
        Set ser = .SeriesCollection.NewSeries
        ser.Values = valRng
        ser.XValues = labelRng
        ser.Name = "Appearances"

        On Error Resume Next
        ser.Interior.Color = RGB(89, 89, 89)  ' dark grey
        On Error GoTo 0

        ser.HasDataLabels = True
        ser.DataLabels.ShowValue = True
        ser.DataLabels.NumberFormat = "0"
        ser.DataLabels.Font.Size = 8
        ser.DataLabels.Font.Name = "Calibri"

        .Axes(xlCategory).ReversePlotOrder = True
        .Axes(xlCategory).TickLabels.Font.Size = 7
        .Axes(xlCategory).TickLabels.Font.Name = "Calibri"

        .Axes(xlValue).HasTitle = False

        .HasTitle = True
        .ChartTitle.Text = "Item Appearances in Top " & CStr(showN) & " Combos"
        .ChartTitle.Font.Size = 11
        .ChartTitle.Font.Name = "Calibri"
        .ChartTitle.Font.Bold = True

        .HasLegend = False
    End With
End Sub


' =============================================================================
' Helpers
' =============================================================================

Private Function GetItemLabel(ByVal cfgWs As Worksheet, ByVal nItems As Long, ByVal varName As String) As String
    Dim i As Long
    For i = 1 To nItems
        If CStr(cfgWs.Cells(CFG_ITEMS_START_ROW + i - 1, CFG_ITEMS_VAR_COL).Value) = varName Then
            GetItemLabel = CStr(cfgWs.Cells(CFG_ITEMS_START_ROW + i - 1, CFG_ITEMS_LABEL_COL).Value)
            Exit Function
        End If
    Next i
    GetItemLabel = varName
End Function


' Direct O(1) lookup by 1-based item index (used for combo data sheets)
Private Function GetItemLabelByIndex(ByVal cfgWs As Worksheet, ByVal itemIdx As Long) As String
    GetItemLabelByIndex = CStr(cfgWs.Cells(CFG_ITEMS_START_ROW + itemIdx - 1, CFG_ITEMS_LABEL_COL).Value)
End Function


Private Function GetItemVarByIndex(ByVal cfgWs As Worksheet, ByVal itemIdx As Long) As String
    GetItemVarByIndex = CStr(cfgWs.Cells(CFG_ITEMS_START_ROW + itemIdx - 1, CFG_ITEMS_VAR_COL).Value)
End Function


' Map display label (spaces) back to raw subgroup name (underscores)
' _config col I = display labels, col F = raw names
Private Function GetRawSubgroup(ByVal cfgWs As Worksheet, ByVal displayLabel As String) As String
    Dim sgMatch As Variant
    sgMatch = Application.Match(displayLabel, cfgWs.Range("I2:I100"), 0)
    If IsError(sgMatch) Then
        GetRawSubgroup = displayLabel  ' fallback
    Else
        GetRawSubgroup = CStr(cfgWs.Cells(1 + CLng(sgMatch), 6).Value)  ' col F
    End If
End Function


Private Function FindColumnIndex(ByVal ws As Worksheet, ByVal colName As String) As Long
    Dim lastCol As Long
    lastCol = ws.Cells(1, ws.Columns.Count).End(xlToLeft).Column

    Dim c As Long
    For c = 1 To lastCol
        If CStr(ws.Cells(1, c).Value) = colName Then
            FindColumnIndex = c
            Exit Function
        End If
    Next c

    FindColumnIndex = 0
End Function


Private Function IsInArray(ByVal needle As String, ByRef arr() As String, ByVal arrLen As Long) As Boolean
    Dim i As Long
    For i = 1 To arrLen
        If arr(i) = needle Then
            IsInArray = True
            Exit Function
        End If
    Next i
    IsInArray = False
End Function


Private Sub ClearGreedyArea(ByVal grWs As Worksheet, ByVal nItems As Long)
    Dim clearEnd As Long
    clearEnd = GR_DATA_ROW + nItems + 5
    ' Clear results (A-F)
    grWs.Range(grWs.Cells(GR_DATA_ROW, 1), grWs.Cells(clearEnd, 6)).ClearContents
    ' Clear staging (H-K)
    grWs.Range(grWs.Cells(2, GR_STAGE_STEP), grWs.Cells(nItems + 5, GR_STAGE_INCR)).ClearContents
End Sub


Private Sub ClearComboArea(ByVal bcWs As Worksheet)
    ' Clear chart
    Call ClearBCChart(bcWs)

    ' Clear chart staging sheet
    On Error Resume Next
    Sheets(BC_CHART_SHEET).Cells.ClearContents
    On Error GoTo 0

    ' Unmerge section header (row 15) first
    On Error Resume Next
    bcWs.Range(bcWs.Cells(15, 2), bcWs.Cells(15, 20)).UnMerge
    On Error GoTo 0

    ' Find actual last used row to avoid clearing massive empty ranges
    Dim lastUsed As Long
    lastUsed = bcWs.Cells(bcWs.Rows.Count, 2).End(xlUp).Row
    If lastUsed < BC_DATA_ROW Then lastUsed = BC_DATA_ROW

    ' Clear from row 15 to last used row + buffer
    Dim clearEnd As Long
    clearEnd = lastUsed + 5

    Dim fullRng As Range
    Set fullRng = bcWs.Range(bcWs.Cells(15, 2), bcWs.Cells(clearEnd, 20))
    fullRng.Clear  ' Clear everything: contents, formats, borders, comments
End Sub


Private Sub ClearChart(ByVal chartWs As Worksheet)
    Dim co As ChartObject
    For Each co In chartWs.ChartObjects
        If co.Name = "TURFGreedyChart" Then
            co.Delete
            Exit For
        End If
    Next co
End Sub


' =============================================================================
' InitializeCheckboxes — TRUE/FALSE data validation on Dashboard items panel
' =============================================================================
Public Sub InitializeCheckboxes()
    If Not HasControlsSheet() Then Exit Sub
    Dim ws As Worksheet
    Set ws = Sheets(DASH_SHEET)

    Dim cfgWs As Worksheet
    Set cfgWs = Sheets(CFG_SHEET)

    Dim nItems As Long
    nItems = CLng(cfgWs.Range(CFG_N_ITEMS).Value)

    Dim i As Long
    For i = 1 To nItems
        Dim r As Long
        r = ITEMS_START_ROW + i - 1

        With ws.Cells(r, ITEMS_CHECK_COL).Validation
            .Delete
            .Add Type:=xlValidateList, AlertStyle:=xlValidAlertStop, _
                 Formula1:="TRUE,FALSE"
            .ShowInput = False
            .ShowError = True
        End With
    Next i

    ' Conditional formatting: green for TRUE, red for FALSE
    Dim checkRng As Range
    Set checkRng = ws.Range(ws.Cells(ITEMS_START_ROW, ITEMS_CHECK_COL), _
                            ws.Cells(ITEMS_START_ROW + nItems - 1, ITEMS_CHECK_COL))
    checkRng.FormatConditions.Delete

    ' TRUE → green fill
    Dim fcTrue As FormatCondition
    Set fcTrue = checkRng.FormatConditions.Add(Type:=xlCellValue, Operator:=xlEqual, Formula1:="TRUE")
    fcTrue.Interior.Color = RGB(99, 190, 123)   ' green
    fcTrue.Font.Color = RGB(0, 97, 0)           ' dark green text

    ' FALSE → red fill
    Dim fcFalse As FormatCondition
    Set fcFalse = checkRng.FormatConditions.Add(Type:=xlCellValue, Operator:=xlEqual, Formula1:="FALSE")
    fcFalse.Interior.Color = RGB(248, 105, 107)  ' red
    fcFalse.Font.Color = RGB(156, 0, 6)          ' dark red text
End Sub


' =============================================================================
' RefreshCombosIfDirty — Called by Sheet_BestCombos.Worksheet_Activate
' =============================================================================
Public Sub RefreshCombosIfDirty()
    If Not HasControlsSheet() Then Exit Sub
    If CLng(Sheets(CTRL_SHEET).Range(CC_INVALIDATE_BC).Value) = 1 Then
        Call BuildTURF
    End If
End Sub


' =============================================================================
' RefreshTURF — Manual refresh (call from Macros dialog)
' =============================================================================
Public Sub RefreshTURF()
    If Not HasControlsSheet() Then Exit Sub
    Call BuildTURF
End Sub
