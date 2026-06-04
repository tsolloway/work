#' seg_compare_segments
#'
#' @description Builds an interactive workbook for comparing the shell mean
#'   scores of any two segments in a seg object. The two segments are chosen in
#'   Excel from dropdowns (a solution and a segment number for each side); the
#'   means, their difference, and the significance test recompute live.
#'
#'   \subsection{Sheets}{
#'   \itemize{
#'     \item \strong{Seg Compare} — rendered through the same machinery as
#'       [seg_write_shell()] (`.seg_append_sheet`), so the block tables, borders,
#'       control panel and conditional formatting are byte-for-byte the shell's.
#'       The two segment columns, Range, P Value, Total and the frequency row are
#'       overlaid with formulas keyed to the dropdown selection, so the shell
#'       layout becomes a live A-vs-B comparison. Holds the four dropdowns
#'       (solution in row 10, segment number in row 11, in the two segment
#'       columns) and the master threshold control panel.
#'     \item \strong{Seg by Diff} — the seg-specific view (segment A as
#'       "target", segment B as "others"), rendered through
#'       `.seg_append_sheet(seg_n = …)` and overlaid the same way. A hidden
#'       `_detail_ordering` sheet re-ranks the variables by live Diff within each
#'       block, so the rows re-sort as the selection changes.
#'     \item \strong{Seg Sort} — the same comparison as one flat, auto-filtered
#'       table (sort by any column, e.g. largest Diff), carrying the same shell
#'       control panel and conditional formatting.
#'     \item \strong{Solution Summary} — a dynamic cross-tabulation of the two
#'       selected solutions' segments, with count, row-\% and column-\% panels
#'       (unused segment rows/cols greyed out), plus the Adj Rand Index,
#'       Cramér's V and pairwise \% Agreement computed live from the crosstab,
#'       with explanatory footnotes.
#'   }
#'   The selection (and threshold) cells live on \strong{Seg Compare}; all other
#'   sheets read them via workbook-defined names.}
#'
#'   \subsection{Significance test}{
#'   The Yates-corrected 2x2 chi-square between the two selected segments — the
#'   same test `seg_write_shell` uses on its seg-specific sheets. The hidden
#'   backbone stores the precomputed weighted mean, raw N, weight total, and
#'   segment size per (solution, segment, variable).
#'   }
#'
#' @param seg A seg object with a shell ([seg_do_spec()]) and at least one
#'   written solution.
#' @param solutions Character vector or `NULL`. Restrict the selectable solutions
#'   to these `lda_name`s. `NULL` (default) embeds every solution.
#' @param where Character or `NULL`. Output directory. Defaults to the solution
#'   folder, then the working directory.
#' @param weighted Logical. Master weighting switch (default `TRUE`).
#' @param use_weight Logical. With `weighted = TRUE`, still required to auto-pick
#'   `seg$meta$weight_variable` (default `TRUE`).
#' @param var_weight Character or `NULL`. Explicit weight variable.
#' @param setting_diff,setting_pvalue Numeric (defaults `0.1`). Seed the master
#'   Diff / p-value thresholds on the \strong{Seg Compare} control panel
#'   (`H5`/`H6`); all sheets' significance colouring reads from there.
#' @param very_hide_all Logical. If `TRUE` (default), the hidden helper sheets
#'   (`_compare_data`, `_detail_ordering`, `_membership`) are set to
#'   `"veryHidden"` (only unhidden via VBA); if `FALSE`, they are regular hidden.
#'   Mirrors `bn_write`.
#' @param verbose Logical. Message the output path on completion.
#'
#' @return Invisibly, the workbook path.
#'
#' @export
seg_compare_segments <- function(
    seg,
    solutions = NULL,
    where = NULL,
    weighted = TRUE,
    use_weight = TRUE,
    var_weight = NULL,
    setting_diff = 0.1,
    setting_pvalue = 0.1,
    very_hide_all = TRUE,
    verbose = FALSE
){

  resp_id_name <- get_resp_id_name(seg)
  df_shell <- seg[["data"]][["with_shell"]]
  if (is.null(df_shell) || (length(df_shell) == 1 && all(is.na(df_shell)))) {
    stop("seg$data$with_shell is empty — run seg_do_spec() first.", call. = FALSE)
  }

  if (!weighted) {
    var_weight <- NULL
  } else if (is.null(var_weight) && use_weight && !is.null(seg[["meta"]][["weight_variable"]])) {
    var_weight <- seg[["meta"]][["weight_variable"]]
  }

  # ---- shell variable inventory (spec order) ----
  polar_prefixes <- seg[["spec"]][["polars"]][["prefix"]]
  shell_meta <- dplyr::bind_rows(
    seg[["shell"]][["polars"]]   %>% tidyr::unnest(cols = vars),
    seg[["shell"]][["profiles"]] %>% tidyr::unnest(cols = vars)
  ) %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(
      block_header = as.character(glue::glue("{prefix} - {block_label}")),
      kind = ifelse(prefix %in% polar_prefixes, "polar", "profile")
    ) %>%
    dplyr::distinct(block_header, kind, var, label)
  var_names <- shell_meta[["var"]]; V <- length(var_names)

  present <- var_names %in% names(df_shell)
  if (!all(present)) {
    stop(glue::glue("Shell variables missing from with_shell: {paste(utils::head(var_names[!present],10), collapse=', ')}"), call. = FALSE)
  }

  # ---- enumerate (solution, segment) + backbone (mean, n, wtot, size) ----
  sol_tbl <- purrr::keep(seg[["solutions"]][["analysis"]], is.list) %>%
    purrr::map(purrr::pluck, "solution_table") %>% purrr::compact() %>%
    dplyr::bind_rows() %>% dplyr::distinct(lda_name, .keep_all = TRUE)
  if (!is.null(solutions)) {
    miss <- setdiff(solutions, sol_tbl[["lda_name"]])
    if (length(miss)) {
      # fall back to solution columns living directly in seg$data$with_solutions
      # (assignment columns named by the solution, keyed on resp_id_name)
      ws <- seg[["data"]][["with_solutions"]]
      ws_cols <- if (is.data.frame(ws)) intersect(miss, names(ws)) else character(0)
      still_miss <- setdiff(miss, ws_cols)
      if (length(still_miss)) stop(glue::glue("Solutions not found: {paste(still_miss, collapse=', ')}."), call. = FALSE)
      extra <- tibble::tibble(
        lda_name    = ws_cols,
        df_solution = purrr::map(ws_cols, function(nm) {
          out <- tibble::tibble(id = ws[[resp_id_name]], v = ws[[nm]])
          names(out)[2] <- nm
          out[!is.na(out[[nm]]), , drop = FALSE]
        })
      )
      sol_tbl <- dplyr::bind_rows(sol_tbl, extra)
    }
    sol_tbl <- sol_tbl %>% dplyr::filter(lda_name %in% solutions)
  }
  if (nrow(sol_tbl) == 0) stop("No solutions found.", call. = FALSE)

  base <- df_shell %>% dplyr::select(dplyr::all_of(unique(c(resp_id_name, var_names, var_weight))))
  labels <- character(0); sol_names <- character(0)
  MEAN <- list(); NN <- list(); WTOT <- list(); SIZE <- numeric(0)
  WSIZE <- numeric(0); WSOL <- numeric(0); RAWSOL <- numeric(0); sol_seg <- list()

  for (i in seq_len(nrow(sol_tbl))) {
    nm <- sol_tbl[["lda_name"]][i]; d <- sol_tbl[["df_solution"]][[i]]
    if (is.null(d) || !nm %in% names(d)) next
    d <- d %>% dplyr::transmute(id, .seg = .data[[nm]]) %>% dplyr::filter(!is.na(.seg))
    if (nrow(d) == 0) next
    j <- dplyr::inner_join(base, d, by = rlang::set_names("id", resp_id_name))
    if (nrow(j) == 0) next
    W <- if (!is.null(var_weight)) j[[var_weight]] else rep(1, nrow(j))
    X <- as.matrix(j[var_names]); sid <- j[[".seg"]]; notNA <- !is.na(X)
    Xw0 <- X * W; Xw0[is.na(Xw0)] <- 0
    wsum_m <- rowsum(Xw0, sid); wtot_m <- rowsum(notNA * W, sid); n_m <- rowsum(notNA * 1, sid)
    mean_m <- wsum_m / wtot_m; mean_m[!is.finite(mean_m)] <- NA; mean_m <- round(mean_m, 5)
    size_t <- table(sid)
    wsol_val <- sum(W); rawsol_val <- nrow(j)   # weighted / raw TOTAL of this solution
    sol_seg[[nm]] <- as.integer(rownames(mean_m))
    for (k in rownames(mean_m)) {
      lab <- as.character(glue::glue("{nm} · Seg {k}"))
      labels <- c(labels, lab); sol_names <- c(sol_names, nm)
      MEAN[[lab]] <- mean_m[k, ]; NN[[lab]] <- n_m[k, ]; WTOT[[lab]] <- round(wtot_m[k, ], 5)
      SIZE[lab]   <- as.integer(size_t[[k]])
      WSIZE[lab]  <- sum(W[sid == as.integer(k)])  # weighted size of this segment
      WSOL[lab]   <- wsol_val                       # weighted total of its solution
      RAWSOL[lab] <- rawsol_val                     # raw total of its solution
    }
  }
  L <- length(labels)
  if (L < 2) stop("Need at least two (solution, segment) groups; found ", L, ".", call. = FALSE)

  to_df <- function(lst) { m <- do.call(cbind, lst); data.frame(var = rownames(m), m, check.names = FALSE, row.names = NULL) }
  mean_df <- to_df(MEAN); n_df <- to_df(NN); wtot_df <- to_df(WTOT)
  solution_list <- unique(sol_names); S <- length(solution_list)

  # per-respondent segment membership across all solutions (for the dynamic crosstab):
  # one row per respondent (union of ids), one column per solution, value = segment.
  memb_list <- purrr::map2(sol_tbl[["lda_name"]], sol_tbl[["df_solution"]], function(nm, d) {
    if (is.null(d) || !nm %in% names(d)) return(NULL)
    d %>% dplyr::transmute(id, !!nm := .data[[nm]])
  }) %>% purrr::compact()
  memb_wide <- purrr::reduce(memb_list, function(x, y) dplyr::full_join(x, y, by = "id"))
  memb_mat  <- memb_wide %>% dplyr::select(dplyr::all_of(solution_list))
  maxK <- max(unlist(sol_seg), na.rm = TRUE)

  if (is.null(where) || all(is.na(where))) where <- seg[["paths"]][["folders"]][["solution"]]
  if (is.null(where) || all(is.na(where))) where <- getwd()

  # ---- default pair (initial render) = first solution, first vs last seg ----
  segs1   <- sol_seg[[solution_list[1]]]
  defA    <- list(sol = solution_list[1], seg = segs1[1])
  defB    <- list(sol = solution_list[1], seg = utils::tail(segs1, 1))
  lab_def_a <- glue::glue("{defA$sol} · Seg {defA$seg}")
  lab_def_b <- glue::glue("{defB$sol} · Seg {defB$seg}")
  ids_a <- dplyr::filter(sol_tbl[sol_tbl$lda_name == defA$sol, ][["df_solution"]][[1]], .data[[defA$sol]] == defA$seg)[["id"]]
  ids_b <- dplyr::filter(sol_tbl[sol_tbl$lda_name == defB$sol, ][["df_solution"]][[1]], .data[[defB$sol]] == defB$seg)[["id"]]
  solv <- "Segment Comparison"
  stacked <- dplyr::bind_rows(
    base %>% dplyr::filter(.data[[resp_id_name]] %in% ids_a) %>% dplyr::mutate(!!solv := 1L),
    base %>% dplyr::filter(.data[[resp_id_name]] %in% ids_b) %>% dplyr::mutate(!!solv := 2L)
  )
  shell_tables <- .seg_do_shell_tables(seg = seg, solution_var = solv, df = stacked, key = NULL, var_weight = var_weight)

  # ====================================================================
  # render summary via the real shell machinery (sheet 1)
  # ====================================================================
  batch <- .style_batch_new()
  wb <- oxl_create_workbook()
  .seg_append_sheet(wb = wb, shell_tables = shell_tables, solution_var = solv,
                    setting_diff = setting_diff, setting_pvalue = setting_pvalue, batch = batch)
  suppressWarnings(.seg_append_sheet(wb = wb, shell_tables = shell_tables, solution_var = solv, seg_n = 1L,
                    setting_diff = setting_diff, setting_pvalue = setting_pvalue, batch = batch))
  batch$flush(wb)

  # NB: the seg-specific sheet keeps its machinery name "Seg 1" through all writes.
  # Renaming it now would desync the workbook state and later corrupt sheetVisibility;
  # all renames happen at the very end, after visibility is set.
  sm <- "Summary"; dt <- "Seg 1"; so <- "sortable"; ag <- "agreement"
  hidden <- "_compare_data"; ord <- "_detail_ordering"; memb_sheet <- "_membership"
  openxlsx::addWorksheet(wb, so)
  openxlsx::addWorksheet(wb, ag)
  openxlsx::addWorksheet(wb, hidden, visible = FALSE)
  openxlsx::addWorksheet(wb, ord, visible = FALSE)
  openxlsx::addWorksheet(wb, memb_sheet, visible = FALSE)
  openxlsx::writeData(wb, memb_sheet, t(solution_list), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, memb_sheet, memb_mat,         startRow = 2, startCol = 1, colNames = FALSE)

  # ---- backbone on hidden sheet ----
  mean_start <- 1L; n_start <- mean_start + V + 2L; wtot_start <- n_start + V + 2L
  openxlsx::writeData(wb, hidden, mean_df, startRow = mean_start, startCol = 1, colNames = TRUE)
  openxlsx::writeData(wb, hidden, n_df,    startRow = n_start,    startCol = 1, colNames = TRUE)
  openxlsx::writeData(wb, hidden, wtot_df, startRow = wtot_start, startCol = 1, colNames = TRUE)
  size_row   <- wtot_start + V + 2L
  wsize_row  <- size_row + 1L
  wsol_row   <- size_row + 2L
  rawsol_row <- size_row + 3L
  openxlsx::writeData(wb, hidden, t(as.numeric(SIZE[labels])),   startRow = size_row,   startCol = 2, colNames = FALSE)
  openxlsx::writeData(wb, hidden, t(as.numeric(WSIZE[labels])),  startRow = wsize_row,  startCol = 2, colNames = FALSE)
  openxlsx::writeData(wb, hidden, t(as.numeric(WSOL[labels])),   startRow = wsol_row,   startCol = 2, colNames = FALSE)
  openxlsx::writeData(wb, hidden, t(as.numeric(RAWSOL[labels])), startRow = rawsol_row, startCol = 2, colNames = FALSE)

  colB <- num2let(2); colLast <- num2let(L + 1)
  mk_blk <- function(start) list(
    hdr  = glue::glue("'{hidden}'!${colB}${start}:${colLast}${start}"),
    varc = glue::glue("'{hidden}'!$A${start + 1}:$A${start + V}"),
    body = glue::glue("'{hidden}'!${colB}${start + 1}:${colLast}${start + V}")
  )
  B_mean <- mk_blk(mean_start); B_n <- mk_blk(n_start); B_wtot <- mk_blk(wtot_start)
  size_hdr <- B_mean$hdr
  size_row_rng   <- glue::glue("'{hidden}'!${colB}${size_row}:${colLast}${size_row}")
  wsize_row_rng  <- glue::glue("'{hidden}'!${colB}${wsize_row}:${colLast}${wsize_row}")
  wsol_row_rng   <- glue::glue("'{hidden}'!${colB}${wsol_row}:${colLast}${wsol_row}")
  rawsol_row_rng <- glue::glue("'{hidden}'!${colB}${rawsol_row}:${colLast}${rawsol_row}")

  # dropdown sources
  soln_col_n <- L + 3L
  openxlsx::writeData(wb, hidden, data.frame(x = solution_list), startRow = 1, startCol = soln_col_n, colNames = FALSE)
  openxlsx::createNamedRegion(wb, hidden, cols = soln_col_n, rows = seq_len(S), name = "soln_list")
  seg_base_col <- L + 5L
  for (s in seq_len(S)) {
    segs <- sol_seg[[solution_list[s]]]
    openxlsx::writeData(wb, hidden, data.frame(x = segs), startRow = 1, startCol = seg_base_col + s - 1L, colNames = FALSE)
    openxlsx::createNamedRegion(wb, hidden, cols = seg_base_col + s - 1L, rows = seq_along(segs), name = glue::glue("segs_{s}"))
  }

  # ====================================================================
  # SORTABLE sheet — flat auto-filtered table (reads the summary selection)
  # ====================================================================
  keyA <- 'ctl_solA&" · Seg "&ctl_segA'
  keyB <- 'ctl_solB&" · Seg "&ctl_segB'
  mean_f <- function(rows, key, vcell) glue::glue('IFERROR(INDEX({B_mean$body},MATCH({vcell}{rows},{B_mean$varc},0),MATCH({key},{B_mean$hdr},0)),"")')
  n_f    <- function(rows, key, vcell) glue::glue('IFERROR(INDEX({B_n$body},MATCH({vcell}{rows},{B_n$varc},0),MATCH({key},{B_n$hdr},0)),0)')
  wtot_f <- function(rows, key, vcell) glue::glue('IFERROR(INDEX({B_wtot$body},MATCH({vcell}{rows},{B_wtot$varc},0),MATCH({key},{B_wtot$hdr},0)),0)')
  chi_f  <- function(rows, mA, mB, nA, nB) glue::glue(
    'IFERROR(CHIDIST(({nA}{rows}+{nB}{rows})*MAX(ABS(({mA}{rows}*{nA}{rows})*((1-{mB}{rows})*{nB}{rows})-((1-{mA}{rows})*{nA}{rows})*({mB}{rows}*{nB}{rows}))-({nA}{rows}+{nB}{rows})/2,0)^2/({nA}{rows}*{nB}{rows}*({mA}{rows}*{nA}{rows}+{mB}{rows}*{nB}{rows})*((1-{mA}{rows})*{nA}{rows}+(1-{mB}{rows})*{nB}{rows})),1),1)')

  pos_style    <- openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  neg_style    <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  pos_style_bw <- openxlsx::createStyle(fontColour = "white", bgFill = "black")
  neg_style_bw <- openxlsx::createStyle(fontColour = "black", bgFill = "#e0e0e0")
  white_font   <- openxlsx::createStyle(fontColour = "white")

  # title + shell-style control panel at E2:F8 (same as the detail tab), with the
  # table pushed down so it doesn't collide with the panel.
  #   layout: A gap | B var(hidden) | C Block | D Label | E MeanA | F MeanB
  #           G Diff | H P Value | I Type | J nA(hidden) | K nB(hidden)
  openxlsx::writeData(wb, so, "Solution - Segment Comparison Sortable", startRow = 9, startCol = 3, colNames = FALSE)  # row 9: below the collapsed panel, leaving row 1 empty
  # control panel: labels in E, values reference the summary's master panel (H2:H8)
  openxlsx::writeData(wb, so, data.frame(x = c("Polar", "Profile", "Tolerance", "P Value", "Diff", "Type", "Color")),
                      startRow = 2, startCol = 5, colNames = FALSE)
  openxlsx::writeFormula(wb, so, x = glue::glue("Summary!$H${2:8}"), startRow = 2, startCol = 6)

  hr <- 10L  # sortable header row (panel occupies rows 2-8)
  openxlsx::writeData(wb, so, t(c("var", "Block", "Label")), startRow = hr, startCol = 2, colNames = FALSE)
  openxlsx::writeFormula(wb, so, x = glue::glue('"A: "&{keyA}'), startRow = hr, startCol = 5)
  openxlsx::writeFormula(wb, so, x = glue::glue('"B: "&{keyB}'), startRow = hr, startCol = 6)
  openxlsx::writeData(wb, so, t(c("Diff", "P Value", "Type", "nA", "nB")), startRow = hr, startCol = 7, colNames = FALSE)
  s1 <- hr + 1L; s2 <- s1 + V - 1L; sr <- seq(s1, s2)
  openxlsx::writeData(wb, so, shell_meta %>% dplyr::transmute(var, block_header, label), startRow = s1, startCol = 2, colNames = FALSE)
  openxlsx::writeData(wb, so, data.frame(k = shell_meta[["kind"]]), startRow = s1, startCol = 9, colNames = FALSE)
  openxlsx::writeFormula(wb, so, x = mean_f(sr, keyA, "$B"), startRow = s1, startCol = 5)  # E MeanA
  openxlsx::writeFormula(wb, so, x = mean_f(sr, keyB, "$B"), startRow = s1, startCol = 6)  # F MeanB
  openxlsx::writeFormula(wb, so, x = glue::glue('IF(OR($E{sr}="",$F{sr}=""),"",$E{sr}-$F{sr})'), startRow = s1, startCol = 7)  # G Diff
  openxlsx::writeFormula(wb, so, x = chi_f(sr, "E", "F", "J", "K"), startRow = s1, startCol = 8)  # H P
  openxlsx::writeFormula(wb, so, x = n_f(sr, keyA, "$B"), startRow = s1, startCol = 10)  # J nA
  openxlsx::writeFormula(wb, so, x = n_f(sr, keyB, "$B"), startRow = s1, startCol = 11)  # K nB

  openxlsx::addStyle(wb, so, openxlsx::createStyle(textDecoration = "Bold", fontSize = 18), rows = 9, cols = 3, stack = TRUE)
  # panel: medium OUTSIDE border only (no inner lines, no bold)
  openxlsx::addStyle(wb, so, openxlsx::createStyle(border = "top",    borderStyle = "medium"), rows = 2, cols = 5:6, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, so, openxlsx::createStyle(border = "bottom", borderStyle = "medium"), rows = 8, cols = 5:6, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, so, openxlsx::createStyle(border = "left",   borderStyle = "medium"), rows = 2:8, cols = 5, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, so, openxlsx::createStyle(border = "right",  borderStyle = "medium"), rows = 2:8, cols = 6, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, so, openxlsx::createStyle(textDecoration = "Bold", halign = "center", fgFill = "#e0e0e0", border = "bottom"), rows = hr, cols = 3:9, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, so, openxlsx::createStyle(numFmt = "0%", halign = "center"), rows = sr, cols = 5:7, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, so, openxlsx::createStyle(numFmt = "0%", halign = "center"), rows = sr, cols = 8, gridExpand = TRUE, stack = TRUE)  # P Value as %
  openxlsx::addStyle(wb, so, openxlsx::createStyle(halign = "center"), rows = sr, cols = 9, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, so, openxlsx::createStyle(numFmt = "0", halign = "center"), rows = sr, cols = 10:11, gridExpand = TRUE, stack = TRUE)  # nA / nB

  # ---- full shell conditional formatting (max/min + significance, bw/color toggle) ----
  # panel cells: F2 polar, F3 profile, F4 tolerance, F5 p-value, F6 diff, F7 type(1/2), F8 color(1/0)
  sig <- glue::glue('OR(AND($F$7=1,$I{s1}="polar",ABS($G{s1})>=$F$2),AND($F$7=1,$I{s1}="profile",ABS($G{s1})>=$F$3),AND($F$7=2,$H{s1}<=$F$5))')
  openxlsx::conditionalFormatting(wb, so, cols = 5:6, rows = sr, rule = glue::glue('AND($F$8=1,E{s1}>=MAX($E{s1}:$F{s1})-$F$4,{sig})'), style = pos_style)
  openxlsx::conditionalFormatting(wb, so, cols = 5:6, rows = sr, rule = glue::glue('AND($F$8=0,E{s1}>=MAX($E{s1}:$F{s1})-$F$4,{sig})'), style = pos_style_bw)
  openxlsx::conditionalFormatting(wb, so, cols = 5:6, rows = sr, rule = glue::glue('AND($F$8=1,E{s1}<=MIN($E{s1}:$F{s1})+$F$4,{sig})'), style = neg_style)
  openxlsx::conditionalFormatting(wb, so, cols = 5:6, rows = sr, rule = glue::glue('AND($F$8=0,E{s1}<=MIN($E{s1}:$F{s1})+$F$4,{sig})'), style = neg_style_bw)
  # Diff column coloring (vs the Diff threshold), with 0 hidden in white
  openxlsx::conditionalFormatting(wb, so, cols = 7, rows = sr, rule = glue::glue('AND($F$8=1,$G{s1}>=$F$6)'),  style = pos_style)
  openxlsx::conditionalFormatting(wb, so, cols = 7, rows = sr, rule = glue::glue('AND($F$8=0,$G{s1}>=$F$6)'),  style = pos_style_bw)
  openxlsx::conditionalFormatting(wb, so, cols = 7, rows = sr, rule = glue::glue('AND($F$8=1,$G{s1}<=-$F$6)'), style = neg_style)
  openxlsx::conditionalFormatting(wb, so, cols = 7, rows = sr, rule = glue::glue('AND($F$8=0,$G{s1}<=-$F$6)'), style = neg_style_bw)
  openxlsx::conditionalFormatting(wb, so, cols = 7, rows = sr, rule = glue::glue('$G{s1}=0'), style = white_font)

  openxlsx::addFilter(wb, so, rows = hr, cols = 3:11)  # include nA / nB in the filter
  openxlsx::setColWidths(wb, so, cols = 1, widths = 2)        # A gap
  openxlsx::setColWidths(wb, so, cols = 2, hidden = TRUE)     # var
  openxlsx::setColWidths(wb, so, cols = 3, widths = 22)       # Block
  openxlsx::setColWidths(wb, so, cols = 4, widths = 60)       # Label
  openxlsx::setColWidths(wb, so, cols = 5:6, widths = 15)     # Mean A / B (match Seg Compare / Seg by Diff)
  openxlsx::setColWidths(wb, so, cols = 7:8, widths = 11)     # Diff / P
  openxlsx::setColWidths(wb, so, cols = 9, hidden = TRUE)     # Type (hidden; still drives CF)
  openxlsx::setColWidths(wb, so, cols = 10:11, hidden = TRUE) # nA / nB (hidden; still feed the p-value)
  openxlsx::setRowHeights(wb, so, rows = hr, heights = 50)    # room for wrapped segment headers (E10/F10)
  openxlsx::addStyle(wb, so, openxlsx::createStyle(wrapText = TRUE, valign = "center"), rows = hr, cols = 5:6, gridExpand = TRUE, stack = TRUE)
  openxlsx::freezePane(wb, so, firstActiveRow = s1, firstActiveCol = 5); openxlsx::showGridLines(wb, so, showGridLines = FALSE)
  openxlsx::groupRows(wb, so, rows = 2:8, hidden = TRUE)  # collapse the control panel (rows 2-8 all have data; avoids a stray out-of-order row 9)

  # ====================================================================
  # OVERLAY dynamic formulas onto the shell-rendered summary
  # layout (col_start=2, row_data_start=15, seg_count=2):
  #   B=var(key)  D=N  E=Total  G=segA  H=segB  J=Range  K=P  (header row 11)
  #   freq: row 12 = n, row 13 = proportion
  #   helper (hidden): AD=nA AE=nB AF=wtotA AG=wtotB
  # ====================================================================
  hAD <- "AD"; hAE <- "AE"; hAF <- "AF"; hAG <- "AG"
  data_rows <- integer(0)
  rds <- 15L; gap <- 2L
  st <- shell_tables[["summary_table"]]
  for (i in seq_len(nrow(st))) {
    temp <- st %>% dplyr::slice(i) %>% dplyr::select(by) %>% tidyr::unnest(by)
    ni <- nrow(temp); rb <- seq(rds, rds + ni - 1L)
    # hidden helper lookups
    openxlsx::writeFormula(wb, sm, x = n_f(rb, keyA, "$B"),    startRow = rds, startCol = 30)
    openxlsx::writeFormula(wb, sm, x = n_f(rb, keyB, "$B"),    startRow = rds, startCol = 31)
    openxlsx::writeFormula(wb, sm, x = wtot_f(rb, keyA, "$B"), startRow = rds, startCol = 32)
    openxlsx::writeFormula(wb, sm, x = wtot_f(rb, keyB, "$B"), startRow = rds, startCol = 33)
    # visible cells
    openxlsx::writeFormula(wb, sm, x = mean_f(rb, keyA, "$B"), startRow = rds, startCol = 7)   # G segA
    openxlsx::writeFormula(wb, sm, x = mean_f(rb, keyB, "$B"), startRow = rds, startCol = 8)   # H segB
    openxlsx::writeFormula(wb, sm, x = glue::glue('IFERROR(ABS($G{rb}-$H{rb}),"")'), startRow = rds, startCol = 10) # J range
    openxlsx::writeFormula(wb, sm, x = chi_f(rb, "G", "H", hAD, hAE), startRow = rds, startCol = 11)               # K pvalue
    openxlsx::writeFormula(wb, sm, x = glue::glue('{hAD}{rb}+{hAE}{rb}'), startRow = rds, startCol = 4)            # D N
    openxlsx::writeFormula(wb, sm, x = glue::glue('IFERROR(($G{rb}*{hAF}{rb}+$H{rb}*{hAG}{rb})/({hAF}{rb}+{hAG}{rb}),"")'), startRow = rds, startCol = 5) # E total
    openxlsx::writeFormula(wb, sm, x = glue::glue('IFERROR($G{rb}-$H{rb},"")'), startRow = rds, startCol = 13) # M Diff = A - B (no x/o logic)
    data_rows <- c(data_rows, rb)
    rds <- rds + ni + gap + 1L
  }
  # ---- selection dropdowns live in the two segment columns ----
  #   G10/H10 = solution dropdowns, G11/H11 = segment-number dropdowns
  #   (so each segment column is self-labeled by its own dropdowns).
  openxlsx::writeData(wb, sm, defA$sol, startRow = 10, startCol = 7); openxlsx::writeData(wb, sm, defB$sol, startRow = 10, startCol = 8)
  openxlsx::writeData(wb, sm, defA$seg, startRow = 11, startCol = 7); openxlsx::writeData(wb, sm, defB$seg, startRow = 11, startCol = 8)
  openxlsx::createNamedRegion(wb, sm, cols = 7, rows = 10, name = "ctl_solA")
  openxlsx::createNamedRegion(wb, sm, cols = 8, rows = 10, name = "ctl_solB")
  openxlsx::createNamedRegion(wb, sm, cols = 7, rows = 11, name = "ctl_segA")
  openxlsx::createNamedRegion(wb, sm, cols = 8, rows = 11, name = "ctl_segB")
  openxlsx::createNamedRegion(wb, sm, cols = 8, rows = 6, name = "ctl_diff")  # shell control panel "Diff"  (H6)
  openxlsx::createNamedRegion(wb, sm, cols = 8, rows = 5, name = "ctl_pval")  # shell control panel "P Value" (H5)
  # Polar (H2) and Profile (H3) thresholds follow the Diff threshold (H6)
  openxlsx::writeFormula(wb, sm, x = "$H$6", startRow = 2, startCol = 8)  # Polar  = Diff
  openxlsx::writeFormula(wb, sm, x = "$H$6", startRow = 3, startCol = 8)  # Profile = Diff
  openxlsx::writeFormula(wb, sm, x = '"segs_"&MATCH(ctl_solA,soln_list,0)', startRow = 1, startCol = 35)  # AI1
  openxlsx::writeFormula(wb, sm, x = '"segs_"&MATCH(ctl_solB,soln_list,0)', startRow = 2, startCol = 35)  # AI2
  openxlsx::dataValidation(wb, sm, cols = 7, rows = 10, type = "list", value = "soln_list", allowBlank = FALSE)
  openxlsx::dataValidation(wb, sm, cols = 8, rows = 10, type = "list", value = "soln_list", allowBlank = FALSE)
  openxlsx::dataValidation(wb, sm, cols = 7, rows = 11, type = "list", value = "INDIRECT($AI$1)", allowBlank = FALSE)
  openxlsx::dataValidation(wb, sm, cols = 8, rows = 11, type = "list", value = "INDIRECT($AI$2)", allowBlank = FALSE)
  openxlsx::setColWidths(wb, sm, cols = 35, hidden = TRUE)
  openxlsx::setColWidths(wb, sm, cols = 7:8, widths = 15)  # segment columns
  openxlsx::setRowHeights(wb, sm, rows = 10, heights = 40) # room for wrapped solution names
  # signal that G10/H10/G11/H11 are interactive controls: grey fill, centered, bold,
  # with an outer medium box (grey-scale, consistent with the shell theme)
  openxlsx::addStyle(wb, sm, openxlsx::createStyle(fgFill = "#e0e0e0", halign = "center", textDecoration = "Bold"),
                     rows = 10:11, cols = 7:8, gridExpand = TRUE, stack = TRUE)
  # solution-name dropdowns (row 10): wrap + vertically centre
  openxlsx::addStyle(wb, sm, openxlsx::createStyle(wrapText = TRUE, valign = "center"),
                     rows = 10, cols = 7:8, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sm, openxlsx::createStyle(border = "top",    borderStyle = "medium"), rows = 10, cols = 7:8, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sm, openxlsx::createStyle(border = "bottom", borderStyle = "medium"), rows = 11, cols = 7:8, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sm, openxlsx::createStyle(border = "left",   borderStyle = "medium"), rows = 10:11, cols = 7, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sm, openxlsx::createStyle(border = "right",  borderStyle = "medium"), rows = 10:11, cols = 8, gridExpand = TRUE, stack = TRUE)
  # frequency row, keyed to the selection:
  #   row 12 (N): G/H = raw segment counts; E (Total) = raw total of A's solution
  #   row 13 (%): G/H = each segment's WEIGHTED share of its OWN solution; E = 100%
  openxlsx::writeFormula(wb, sm, x = glue::glue('IFERROR(INDEX({size_row_rng},MATCH({keyA},{size_hdr},0)),0)'), startRow = 12, startCol = 7)
  openxlsx::writeFormula(wb, sm, x = glue::glue('IFERROR(INDEX({size_row_rng},MATCH({keyB},{size_hdr},0)),0)'), startRow = 12, startCol = 8)
  openxlsx::writeFormula(wb, sm, x = glue::glue('IFERROR(INDEX({rawsol_row_rng},MATCH({keyA},{size_hdr},0)),0)'), startRow = 12, startCol = 5)
  openxlsx::writeFormula(wb, sm, x = glue::glue('IFERROR(INDEX({wsize_row_rng},MATCH({keyA},{size_hdr},0))/INDEX({wsol_row_rng},MATCH({keyA},{size_hdr},0)),0)'), startRow = 13, startCol = 7)
  openxlsx::writeFormula(wb, sm, x = glue::glue('IFERROR(INDEX({wsize_row_rng},MATCH({keyB},{size_hdr},0))/INDEX({wsol_row_rng},MATCH({keyB},{size_hdr},0)),0)'), startRow = 13, startCol = 8)
  openxlsx::writeData(wb, sm, 1, startRow = 13, startCol = 5)
  openxlsx::writeData(wb, sm, "", startRow = 10, startCol = 5, colNames = FALSE)  # clear vestigial "X/O" label (Diff no longer uses x/o tags)
  openxlsx::writeData(wb, sm, "Pick the two segments with the dropdowns above each column (rows 10–11)", startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sm, openxlsx::createStyle(textDecoration = "italic", fontColour = "#808080"), rows = 1, cols = 1, stack = TRUE)
  openxlsx::setColWidths(wb, sm, cols = 30:33, hidden = TRUE)

  # ====================================================================
  # OVERLAY the seg-specific ("detail") sheet — A as target, B as others
  #   layout (segment_specific): E=target  F=others  H=Diff  I=P  (header row 11)
  #   helper (hidden): AD=nA  AE=nB
  # ====================================================================
  rds <- 15L
  st_d <- shell_tables[["segment_tables"]][[1]]
  for (i in seq_len(nrow(st_d))) {
    temp <- st_d %>% dplyr::slice(i) %>% dplyr::select(by) %>% tidyr::unnest(by)
    ni <- nrow(temp); rb <- seq(rds, rds + ni - 1L); rb1 <- rds; rb2 <- rds + ni - 1L

    # --- _detail_ordering: var, label, live diff (A-B), unique rank within block ---
    openxlsx::writeData(wb, ord, data.frame(v = temp$var),   startRow = rds, startCol = 1, colNames = FALSE)  # A var
    openxlsx::writeData(wb, ord, data.frame(l = temp$label), startRow = rds, startCol = 2, colNames = FALSE)  # B label
    openxlsx::writeFormula(wb, ord,
      x = glue::glue('IFERROR(INDEX({B_mean$body},MATCH($A{rb},{B_mean$varc},0),MATCH({keyA},{B_mean$hdr},0))-INDEX({B_mean$body},MATCH($A{rb},{B_mean$varc},0),MATCH({keyB},{B_mean$hdr},0)),-1E9)'),
      startRow = rds, startCol = 4)  # D diff
    openxlsx::writeFormula(wb, ord,
      x = glue::glue('SUMPRODUCT(($D${rb1}:$D${rb2}>$D{rb})*1)+SUMPRODUCT(($D${rb1}:$D${rb2}=$D{rb})*(ROW($D${rb1}:$D${rb2})<ROW($D{rb})))+1'),
      startRow = rds, startCol = 5)  # E rank (1 = largest diff; ties broken by row)

    # --- detail var (B, hidden) + label (C) pulled in diff-rank order ---
    pos <- seq_len(ni)
    ord_a <- glue::glue("'{ord}'!$A${rb1}:$A${rb2}")
    ord_b <- glue::glue("'{ord}'!$B${rb1}:$B${rb2}")
    ord_e <- glue::glue("'{ord}'!$E${rb1}:$E${rb2}")
    openxlsx::writeFormula(wb, dt, x = glue::glue('INDEX({ord_a},MATCH({pos},{ord_e},0))'), startRow = rds, startCol = 2)  # B var
    openxlsx::writeFormula(wb, dt, x = glue::glue('INDEX({ord_b},MATCH({pos},{ord_e},0))'), startRow = rds, startCol = 3)  # C label

    # --- detail data, keyed off the now-dynamic var in $B ---
    openxlsx::writeFormula(wb, dt, x = n_f(rb, keyA, "$B"), startRow = rds, startCol = 30)
    openxlsx::writeFormula(wb, dt, x = n_f(rb, keyB, "$B"), startRow = rds, startCol = 31)
    openxlsx::writeFormula(wb, dt, x = mean_f(rb, keyA, "$B"), startRow = rds, startCol = 5)  # E target = A
    openxlsx::writeFormula(wb, dt, x = mean_f(rb, keyB, "$B"), startRow = rds, startCol = 6)  # F others = B
    openxlsx::writeFormula(wb, dt, x = glue::glue('IFERROR($E{rb}-$F{rb},"")'), startRow = rds, startCol = 8)  # H Diff = A - B
    openxlsx::writeFormula(wb, dt, x = chi_f(rb, "E", "F", "AD", "AE"), startRow = rds, startCol = 9)           # I p-value
    rds <- rds + ni + gap + 1L
  }
  # title + segment headers + frequency row (keyed to the selection)
  #   E/F 11 = solution name, 12 = segment number, 13 = weighted % of own solution
  openxlsx::writeData(wb, dt, "Solution - Segment Comparison by Diff", startRow = 11, startCol = 3, colNames = FALSE)  # C11 title
  openxlsx::writeFormula(wb, dt, x = "ctl_solA", startRow = 10, startCol = 5)  # E10 solution A
  openxlsx::writeFormula(wb, dt, x = "ctl_solB", startRow = 10, startCol = 6)  # F10 solution B
  openxlsx::writeFormula(wb, dt, x = "ctl_segA", startRow = 11, startCol = 5)  # E11 seg # A
  openxlsx::writeFormula(wb, dt, x = "ctl_segB", startRow = 11, startCol = 6)  # F11 seg # B
  # center the solution/seg identification and box it (E10:F11)
  openxlsx::addStyle(wb, dt, openxlsx::createStyle(halign = "center"), rows = 10:11, cols = 5:6, gridExpand = TRUE, stack = TRUE)
  # solution-name row (10): wrap + vertically centre
  openxlsx::addStyle(wb, dt, openxlsx::createStyle(wrapText = TRUE, valign = "center"), rows = 10, cols = 5:6, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dt, openxlsx::createStyle(border = "top",    borderStyle = "medium"), rows = 10, cols = 5:6, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dt, openxlsx::createStyle(border = "bottom", borderStyle = "medium"), rows = 11, cols = 5:6, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dt, openxlsx::createStyle(border = "left",   borderStyle = "medium"), rows = 10:11, cols = 5, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dt, openxlsx::createStyle(border = "right",  borderStyle = "medium"), rows = 10:11, cols = 6, gridExpand = TRUE, stack = TRUE)
  openxlsx::writeFormula(wb, dt, x = glue::glue('IFERROR(INDEX({size_row_rng},MATCH({keyA},{size_hdr},0)),0)'), startRow = 12, startCol = 5)  # E12 count A
  openxlsx::writeFormula(wb, dt, x = glue::glue('IFERROR(INDEX({size_row_rng},MATCH({keyB},{size_hdr},0)),0)'), startRow = 12, startCol = 6)  # F12 count B
  openxlsx::writeFormula(wb, dt, x = glue::glue('IFERROR(INDEX({wsize_row_rng},MATCH({keyA},{size_hdr},0))/INDEX({wsol_row_rng},MATCH({keyA},{size_hdr},0)),0)'), startRow = 13, startCol = 5)  # E13 weighted %
  openxlsx::writeFormula(wb, dt, x = glue::glue('IFERROR(INDEX({wsize_row_rng},MATCH({keyB},{size_hdr},0))/INDEX({wsol_row_rng},MATCH({keyB},{size_hdr},0)),0)'), startRow = 13, startCol = 6)  # F13 weighted %
  openxlsx::setColWidths(wb, dt, cols = 5:6, widths = 15)    # segment columns (match Seg Compare)
  openxlsx::setRowHeights(wb, dt, rows = 10, heights = 40)   # room for wrapped solution names
  openxlsx::setColWidths(wb, dt, cols = 11, hidden = TRUE)   # hide the High/Low column (K)
  openxlsx::setColWidths(wb, dt, cols = 30:31, hidden = TRUE)

  # ====================================================================
  # AGREEMENT sheet — dynamic crosstab of the two selected solutions, with
  # count / row% / column% panels, plus agreement metrics. Unused segment
  # rows/cols (count margin 0) are whited out with grey headers via CF.
  # ====================================================================
  mh_last   <- num2let(S)
  Nresp     <- nrow(memb_mat)
  memb_hdr  <- glue::glue("'{memb_sheet}'!$A$1:${mh_last}$1")
  memb_data <- glue::glue("'{memb_sheet}'!$A$2:${mh_last}${Nresp + 1}")
  # column layout (content starts at B; col A is the narrow gap, like the other sheets)
  cb        <- 2L                  # corner / row-header / label column (B)
  cb_let    <- num2let(cb)         # "B"
  gc_first  <- num2let(cb + 1L)    # first grid column letter (C)
  gc_last   <- num2let(cb + maxK)  # last grid column letter
  totcol_n  <- cb + 1L + maxK      # Total column number
  totcol    <- num2let(totcol_n)
  vc        <- cb + 2L             # echo / metric value column (D)

  white_font <- openxlsx::createStyle(fontColour = "white")
  # Unused-header muting: same fill as the active headers (#e0e0e0) with the font
  # set to that same colour, so only the number disappears (no darker band).
  # CF fills must use bgFill (-> dxf bgColor) or Excel won't render the fill.
  grey_fill  <- openxlsx::createStyle(bgFill = "#e0e0e0", fontColour = "#e0e0e0")

  # selected-solution column indices into the membership matrix (hidden, col AD)
  openxlsx::writeFormula(wb, ag, x = glue::glue('MATCH(ctl_solA,{memb_hdr},0)'), startRow = 1, startCol = 30)
  openxlsx::writeFormula(wb, ag, x = glue::glue('MATCH(ctl_solB,{memb_hdr},0)'), startRow = 2, startCol = 30)

  # titles
  openxlsx::writeData(wb, ag, "Solution Agreement — cross-tabulation", startRow = 2, startCol = cb, colNames = FALSE)
  openxlsx::writeData(wb, ag, "Rows = Solution A:", startRow = 3, startCol = cb, colNames = FALSE)
  openxlsx::writeFormula(wb, ag, x = "ctl_solA", startRow = 3, startCol = vc)
  openxlsx::writeData(wb, ag, "Cols = Solution B:", startRow = 4, startCol = cb, colNames = FALSE)
  openxlsx::writeFormula(wb, ag, x = "ctl_solB", startRow = 4, startCol = vc)

  # ---- one crosstab panel (mode = "count" | "rowpct" | "colpct") ----
  xt_panel <- function(top, label, mode, cc = NULL) {
    hrow <- top + 1L; d1 <- top + 2L; dn <- d1 + maxK - 1L; trow <- dn + 1L
    drows <- seq(d1, dn)
    openxlsx::writeData(wb, ag, label,            startRow = top,  startCol = cb, colNames = FALSE)
    openxlsx::writeData(wb, ag, "A \\ B",          startRow = hrow, startCol = cb, colNames = FALSE)
    openxlsx::writeData(wb, ag, t(seq_len(maxK)), startRow = hrow, startCol = cb + 1L, colNames = FALSE)
    openxlsx::writeData(wb, ag, "Total",          startRow = hrow, startCol = totcol_n, colNames = FALSE)
    openxlsx::writeData(wb, ag, data.frame(x = seq_len(maxK)), startRow = d1, startCol = cb, colNames = FALSE)
    openxlsx::writeData(wb, ag, "Total",          startRow = trow, startCol = cb, colNames = FALSE)
    for (j in seq_len(maxK)) {
      cl <- num2let(cb + j)
      f <- if (mode == "count") {
        glue::glue('COUNTIFS(INDEX({memb_data},0,$AD$1),${cb_let}{drows},INDEX({memb_data},0,$AD$2),{cl}${hrow})')
      } else if (mode == "rowpct") {
        ccr <- seq(cc$d1, cc$dn); cct <- num2let(cc$totcol_n)
        glue::glue('IFERROR({cl}{ccr}/${cct}{ccr},"")')
      } else {
        ccr <- seq(cc$d1, cc$dn)
        glue::glue('IFERROR({cl}{ccr}/{cl}${cc$trow},"")')
      }
      openxlsx::writeFormula(wb, ag, x = f, startRow = d1, startCol = cb + j)
    }
    # ---- marginal totals (computed as in seg_shell_add_crosstab) ----
    if (mode == "count") {
      # right Total col = per-row sum; bottom Total row = per-col sum; grand = full sum
      openxlsx::writeFormula(wb, ag, x = glue::glue('SUM({gc_first}{drows}:{gc_last}{drows})'), startRow = d1, startCol = totcol_n)
      for (j in seq_len(maxK)) {
        cl <- num2let(cb + j)
        openxlsx::writeFormula(wb, ag, x = glue::glue('SUM({cl}{d1}:{cl}{dn})'), startRow = trow, startCol = cb + j)
      }
      openxlsx::writeFormula(wb, ag, x = glue::glue('SUM({gc_first}{d1}:{gc_last}{dn})'), startRow = trow, startCol = totcol_n)

    } else if (mode == "rowpct") {
      # right Total col: each row's %s sum to 1
      openxlsx::writeFormula(wb, ag, x = glue::glue('SUM({gc_first}{drows}:{gc_last}{drows})'), startRow = d1, startCol = totcol_n)
      # bottom Total row: each column's marginal share of N, from the COUNTS panel
      for (j in seq_len(maxK)) {
        cl <- num2let(cb + j)
        openxlsx::writeFormula(wb, ag, x = glue::glue('IFERROR({cl}${cc$trow}/${totcol}${cc$trow},"")'), startRow = trow, startCol = cb + j)
      }
      # grand: sum of the bottom marginal-share row (= 1)
      openxlsx::writeFormula(wb, ag, x = glue::glue('SUM({gc_first}${trow}:{gc_last}${trow})'), startRow = trow, startCol = totcol_n)

    } else {  # colpct
      ccr <- seq(cc$d1, cc$dn)
      # right Total col: each row's marginal share of N, from the COUNTS panel
      openxlsx::writeFormula(wb, ag, x = glue::glue('IFERROR(${totcol}{ccr}/${totcol}${cc$trow},"")'), startRow = d1, startCol = totcol_n)
      # bottom Total row: each column's %s sum to 1
      for (j in seq_len(maxK)) {
        cl <- num2let(cb + j)
        openxlsx::writeFormula(wb, ag, x = glue::glue('SUM({cl}{d1}:{cl}{dn})'), startRow = trow, startCol = cb + j)
      }
      # grand: sum of the right marginal-share col (= 1)
      openxlsx::writeFormula(wb, ag, x = glue::glue('SUM(${totcol}{d1}:${totcol}{dn})'), startRow = trow, startCol = totcol_n)
    }

    numfmt <- if (mode == "count") "0" else "0%"
    openxlsx::addStyle(wb, ag, openxlsx::createStyle(textDecoration = "Bold"), rows = top, cols = cb, stack = TRUE)
    openxlsx::addStyle(wb, ag, openxlsx::createStyle(textDecoration = "Bold", halign = "center", fgFill = "#e0e0e0"), rows = hrow, cols = cb:totcol_n, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, ag, openxlsx::createStyle(textDecoration = "Bold", halign = "center", fgFill = "#e0e0e0"), rows = d1:trow, cols = cb, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, ag, openxlsx::createStyle(border = c("top","bottom","left","right"), borderColour = "#BFBFBF", halign = "center", numFmt = numfmt), rows = d1:trow, cols = (cb + 1L):totcol_n, gridExpand = TRUE, stack = TRUE)
    # white out unused rows/cols (own margin 0); grey their headers.
    # Header CF is written per-cell with an explicit total reference so it does
    # not depend on Excel's relative-reference adjustment across a header range.
    openxlsx::conditionalFormatting(wb, ag, cols = (cb + 1L):totcol_n, rows = d1:trow, rule = glue::glue('OR(${totcol}{d1}=0,{gc_first}${trow}=0)'), style = white_font)
    for (ii in seq_len(maxK)) {
      openxlsx::conditionalFormatting(wb, ag, cols = cb, rows = d1 + ii - 1L, rule = glue::glue('${totcol}${d1 + ii - 1L}=0'), style = grey_fill)        # row header ii
    }
    for (jj in seq_len(maxK)) {
      cl <- num2let(cb + jj)
      openxlsx::conditionalFormatting(wb, ag, cols = cb + jj, rows = hrow, rule = glue::glue('{cl}${trow}=0'), style = grey_fill)                          # col header jj
    }
    list(top = top, hrow = hrow, d1 = d1, dn = dn, trow = trow, totcol_n = totcol_n)
  }

  cc <- xt_panel(6L, "Counts", "count")
  rp <- xt_panel(cc$trow + 2L, "Row %", "rowpct", cc = cc)
  cp <- xt_panel(rp$trow + 2L, "Column %", "colpct", cc = cc)

  # ---- metric helpers (hidden col AD), computed from the COUNTS panel ----
  grid_rng   <- glue::glue("{gc_first}{cc$d1}:{gc_last}{cc$dn}")
  rowtot_rng <- glue::glue("{totcol}{cc$d1}:{totcol}{cc$dn}")
  coltot_rng <- glue::glue("{gc_first}{cc$trow}:{gc_last}{cc$trow}")
  N_cell     <- glue::glue("{totcol}{cc$trow}")
  openxlsx::writeFormula(wb, ag, x = glue::glue('SUMPRODUCT({grid_rng}*({grid_rng}-1))/2'),     startRow = 4, startCol = 30)
  openxlsx::writeFormula(wb, ag, x = glue::glue('SUMPRODUCT({rowtot_rng}*({rowtot_rng}-1))/2'), startRow = 5, startCol = 30)
  openxlsx::writeFormula(wb, ag, x = glue::glue('SUMPRODUCT({coltot_rng}*({coltot_rng}-1))/2'), startRow = 6, startCol = 30)
  openxlsx::writeFormula(wb, ag, x = glue::glue('{N_cell}*({N_cell}-1)/2'),                     startRow = 7, startCol = 30)
  for (j in seq_len(maxK)) {
    cl <- num2let(cb + j); ccr <- seq(cc$d1, cc$dn)
    openxlsx::writeFormula(wb, ag, x = glue::glue('IF(OR(${totcol}{ccr}=0,{cl}${cc$trow}=0),0,{cl}{ccr}^2/(${totcol}{ccr}*{cl}${cc$trow}))'), startRow = cc$d1, startCol = 31 + j)
  }
  contrib_rng <- glue::glue("{num2let(32)}{cc$d1}:{num2let(31 + maxK)}{cc$dn}")
  openxlsx::writeFormula(wb, ag, x = glue::glue('{N_cell}*(SUM({contrib_rng})-1)'), startRow = 8,  startCol = 30)
  openxlsx::writeFormula(wb, ag, x = glue::glue('SUMPRODUCT(({rowtot_rng}>0)*1)'),  startRow = 9,  startCol = 30)
  openxlsx::writeFormula(wb, ag, x = glue::glue('SUMPRODUCT(({coltot_rng}>0)*1)'),  startRow = 10, startCol = 30)

  # ---- visible metrics, below the panels ----
  mr <- cp$trow + 2L
  openxlsx::writeData(wb, ag, data.frame(x = c("Adj Rand Index", "Cramér's V", "% Agree (pairwise)")), startRow = mr, startCol = cb, colNames = FALSE)
  openxlsx::writeFormula(wb, ag, x = 'IFERROR(($AD$4-$AD$5*$AD$6/$AD$7)/(0.5*($AD$5+$AD$6)-$AD$5*$AD$6/$AD$7),0)', startRow = mr,     startCol = vc)
  openxlsx::writeFormula(wb, ag, x = glue::glue('IFERROR(SQRT($AD$8/({N_cell}*(MIN($AD$9,$AD$10)-1))),0)'),         startRow = mr + 1, startCol = vc)
  openxlsx::writeFormula(wb, ag, x = 'IFERROR(($AD$7-$AD$5-$AD$6+2*$AD$4)/$AD$7,0)',                                startRow = mr + 2, startCol = vc)

  openxlsx::addStyle(wb, ag, openxlsx::createStyle(textDecoration = "Bold", fontSize = 18), rows = 2, cols = cb, stack = TRUE)
  openxlsx::addStyle(wb, ag, openxlsx::createStyle(textDecoration = "Bold"), rows = 3:4, cols = cb, stack = TRUE)
  openxlsx::addStyle(wb, ag, openxlsx::createStyle(textDecoration = "Bold"), rows = mr:(mr + 2), cols = cb, stack = TRUE)
  openxlsx::addStyle(wb, ag, openxlsx::createStyle(numFmt = "0.000", halign = "left"), rows = mr:(mr + 1), cols = vc, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, ag, openxlsx::createStyle(numFmt = "0%", halign = "left"), rows = mr + 2, cols = vc, stack = TRUE)

  # footnotes explaining each metric — turf_write style: italic, #595959, size 9,
  # merged across the panel width, wrapped.
  fr <- mr + 4L
  foot_txt <- c(
    "Adj Rand Index — how similarly the two solutions group respondents, corrected for chance (1 = identical grouping, 0 ≈ random, < 0 worse than chance). Label-agnostic.",
    "Cramér's V — strength of association between the two solutions' segment assignments (0 = none, 1 = perfect).",
    "% Agree (pairwise) — share of all respondent pairs the two solutions treat the same way: both in the same segment, or both in different segments."
  )
  openxlsx::writeData(wb, ag, data.frame(x = foot_txt), startRow = fr, startCol = cb, colNames = FALSE)
  foot_style  <- openxlsx::createStyle(textDecoration = "italic", fontColour = "#595959", fontSize = 9, wrapText = TRUE, valign = "top")
  panel_chars <- 7 * (maxK + 2)   # approx chars per line across the merged panel
  for (k in seq_along(foot_txt)) {
    openxlsx::mergeCells(wb, ag, cols = cb:totcol_n, rows = fr + k - 1L)
    openxlsx::addStyle(wb, ag, foot_style, rows = fr + k - 1L, cols = cb:totcol_n, gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::setRowHeights(wb, ag, rows = fr:(fr + 2),
                          heights = pmax(15, ceiling(nchar(foot_txt) / panel_chars) * 14))

  openxlsx::setColWidths(wb, ag, cols = 1, widths = 2)              # narrow gap (col A)
  openxlsx::setColWidths(wb, ag, cols = cb:totcol_n, widths = 7)    # corner + grid + total
  openxlsx::setColWidths(wb, ag, cols = 30:(31 + maxK), hidden = TRUE)
  openxlsx::showGridLines(wb, ag, showGridLines = FALSE)

  # helper-sheet visibility — set BEFORE renaming. renameWorksheet desyncs the
  # workbook's internal state vectors, after which sheetVisibility<- recycles a
  # mismatched-length vector and corrupts the sheet list. Build a clean
  # full-length all-character vector ("visible" everywhere, helpers veryHidden).
  sn  <- wb$sheet_names
  vis <- rep("visible", length(sn))
  vis[sn %in% c(hidden, ord, memb_sheet)] <- if (very_hide_all) "veryHidden" else "hidden"
  openxlsx::sheetVisibility(wb) <- vis

  # name the visible tabs
  openxlsx::renameWorksheet(wb, "Summary",   "Seg Compare")
  openxlsx::renameWorksheet(wb, "Seg 1",     "Seg by Diff")
  openxlsx::renameWorksheet(wb, "sortable",  "Seg Sort")
  openxlsx::renameWorksheet(wb, "agreement", "Solution Summary")

  # renameWorksheet updates defined names but NOT formula text. The shell-rendered
  # "Seg by Diff" sheet (control panel, type lookup, high/low) and the "Seg Sort"
  # panel hardcode "summary!" cross-references; rewrite them in the workbook's
  # stored formulas to the renamed sheet ("Seg Compare", single-quoted for the
  # space). Done in-memory on wb$worksheets[[i]]$sheet_data$f — no zip round-trip.
  for (i in seq_along(wb[["worksheets"]])) {
    f <- wb[["worksheets"]][[i]][["sheet_data"]][["f"]]
    if (length(f)) wb[["worksheets"]][[i]][["sheet_data"]][["f"]] <- gsub("summary!", "'Seg Compare'!", f, ignore.case = TRUE)
  }

  file_name <- glue::glue("{where}/Segment Comparison.xlsx")
  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
  if (verbose) message(glue::glue("Written: {file_name}"))
  invisible(file_name)
}
