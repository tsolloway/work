#' append_bn_impact_dynamic
#'
#' @description Internal helper that adds a dynamic dashboard sheet, results
#'   sheet, and lookup sheet to an existing workbook. Used by
#'   \code{bn_impact_write()} for both attribute and community dynamic
#'   dashboards.
#'
#' @param wb An openxlsx workbook object.
#' @param table The analysis table (from \code{bn_impact()$table}).
#' @param subgroups Character vector of subgroup names.
#' @param dash_sheet Character. Dashboard sheet name.
#' @param results_sheet Character. Results sheet name.
#' @param lookup_sheet Character. Lookup sheet name.
#' @param title Character. Title text.
#' @param sub_title Character or NULL. Subtitle text.
#' @param engine_footer Character. Engine description for footer.
#' @param variable_width Numeric. Column width for first ID column.
#' @param community_width Numeric. Column width for Community column.
#' @param label_width Column width for Label column.
#'
#' @return The modified workbook object (invisibly).
#'
#' @keywords internal
append_bn_impact_dynamic <- function(
    wb,
    table,
    subgroups,
    dash_sheet,
    results_sheet,
    lookup_sheet,
    title,
    sub_title = NULL,
    engine_footer,
    variable_width = 20,
    community_width = 20,
    label_width = "auto",
    has_weights = FALSE,
    weighted_results_sheet = NULL
) {

  n_results_rows <- nrow(table)
  n_results_cols <- ncol(table)

  # ---------------------------------------------------------------------------
  # Determine dropdown options from column names
  # ---------------------------------------------------------------------------
  sg1 <- if (!is.null(subgroups)) subgroups[1] else NULL
  names(table) <- as.character(names(table))
  all_cols <- names(table)
  sgs <- if (!is.null(subgroups)) subgroups else "Total"

  if (!is.null(sg1)) {
    sg_cols <- all_cols[startsWith(all_cols, paste0(sg1, "_"))]
    metric_suffixes <- gsub(paste0("^", sg1, "_"), "", sg_cols)
  } else {
    metric_suffixes <- setdiff(all_cols, c("Variable", "Label", "Community"))
  }

  # Focus options: Market + any brand names found in lift columns
  all_lift_suffixes <- grep("^lift", metric_suffixes, value = TRUE)
  market_lift_suffixes <- grep("^lift$|^lift_\\d+$", all_lift_suffixes, value = TRUE)
  brand_lift_suffixes <- setdiff(all_lift_suffixes, market_lift_suffixes)

  if (length(brand_lift_suffixes) > 0) {
    brand_names <- unique(gsub("^lift_\\d+_|^lift_", "", brand_lift_suffixes))
    focus_options <- c("Market", brand_names)
  } else {
    focus_options <- "Market"
  }

  # Metric options
  metric_labels <- character(0)
  metric_keys <- character(0)

  for (ml in market_lift_suffixes) {
    if (ml == "lift" || ml == "lift_0") {
      metric_labels <- c(metric_labels, "Average Lift")
      metric_keys <- c(metric_keys, ml)
    } else {
      pct <- gsub("lift_", "", ml)
      metric_labels <- c(metric_labels, paste0(pct, "% Lift"))
      metric_keys <- c(metric_keys, ml)
    }
  }
  if ("maxVmin" %in% metric_suffixes) {
    metric_labels <- c(metric_labels, "Max vs Min")
    metric_keys <- c(metric_keys, "maxVmin")
  }
  if ("mi" %in% metric_suffixes) {
    metric_labels <- c(metric_labels, "Mutual Information")
    metric_keys <- c(metric_keys, "mi")
  }

  # ---------------------------------------------------------------------------
  # Sheet 1: Dashboard (created first so it appears first)
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, dash_sheet)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(fgFill = "#FFFFFF"),
    rows = 1:200, cols = 1:50, gridExpand = TRUE, stack = TRUE)

  # ---------------------------------------------------------------------------
  # Sheet 2: Results (raw data)
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, results_sheet)
  openxlsx::writeData(wb, results_sheet, table, startRow = 1, startCol = 1)

  # ---------------------------------------------------------------------------
  # Sheet 3: _lookup (hidden)
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, lookup_sheet)

  # Focus options in A column
  openxlsx::writeData(wb, lookup_sheet, "Focus", startRow = 1, startCol = 1)
  for (fi in seq_along(focus_options)) {
    openxlsx::writeData(wb, lookup_sheet, focus_options[fi], startRow = fi + 1, startCol = 1)
  }

  # Metric display labels in B column, keys in C column
  openxlsx::writeData(wb, lookup_sheet, "Metric", startRow = 1, startCol = 2)
  openxlsx::writeData(wb, lookup_sheet, "Key", startRow = 1, startCol = 3)
  for (mi in seq_along(metric_labels)) {
    openxlsx::writeData(wb, lookup_sheet, metric_labels[mi], startRow = mi + 1, startCol = 2)
    openxlsx::writeData(wb, lookup_sheet, metric_keys[mi], startRow = mi + 1, startCol = 3)
  }

  # Subgroup names in D column
  openxlsx::writeData(wb, lookup_sheet, "Subgroup", startRow = 1, startCol = 4)
  for (si in seq_along(sgs)) {
    openxlsx::writeData(wb, lookup_sheet, sgs[si], startRow = si + 1, startCol = 4)
  }

  # Index description in E column
  index_descriptions <- purrr::map_chr(metric_keys, function(mk) {
    if (mk == "lift" || mk == "lift_0") {
      "Indexed by average market lift. Average lift measures the overall influence of each attribute on the outcome by shifting each attribute level up by 5% and averaging the resulting changes."
    } else if (grepl("^lift_", mk)) {
      pct <- gsub("lift_", "", mk)
      paste0("Indexed by ", pct, "% market lift. ", pct, "% lift measures how much the outcome changes when ", pct, "% of respondents for each attribute shift up by one level.")
    } else if (mk == "maxVmin") {
      "Indexed by max vs min impact. Max vs min measures the difference in the outcome between the best-case and worst-case scenario for each attribute."
    } else if (mk == "mi") {
      "Indexed by mutual information. Mutual information measures the strength of the relationship between each attribute and the outcome."
    } else {
      paste("Indexed by", mk)
    }
  })
  openxlsx::writeData(wb, lookup_sheet, "Index Description", startRow = 1, startCol = 5)
  for (di in seq_along(index_descriptions)) {
    openxlsx::writeData(wb, lookup_sheet, index_descriptions[di], startRow = di + 1, startCol = 5)
  }

  # Sequential column counter for remaining lookup columns (after E = Index descriptions)
  lk_col <- 6L

  # Weight options (if weighted results available)
  weight_opt_col <- NULL
  if (has_weights) {
    weight_opt_col <- lk_col
    weight_options <- c("Unweighted", "Weighted")
    openxlsx::writeData(wb, lookup_sheet, "Weight", startRow = 1, startCol = lk_col)
    for (wi in seq_along(weight_options)) {
      openxlsx::writeData(wb, lookup_sheet, weight_options[wi], startRow = wi + 1, startCol = lk_col)
    }
    lk_col <- lk_col + 1L
  }

  # ---------------------------------------------------------------------------
  # Styles
  # ---------------------------------------------------------------------------
  styles <- list(
    title     = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header    = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE,
                                      border = "TopBottom", borderStyle = "medium",
                                      fgFill = "#D9D9D9"),
    center    = openxlsx::createStyle(numFmt = "0", halign = "center"),
    left      = openxlsx::createStyle(halign = "left"),
    total_impact = openxlsx::createStyle(numFmt = "0.0%", halign = "center"),
    separator = openxlsx::createStyle(fgFill = "black"),
    neg_sign  = openxlsx::createStyle(textDecoration = c("bold", "italic")),
    insig     = openxlsx::createStyle(bgFill = "black"),
    dropdown_label = openxlsx::createStyle(textDecoration = "bold", halign = "right"),
    dropdown_cell  = openxlsx::createStyle(
      border = "Bottom", borderStyle = "thin", halign = "center"
    )
  )

  # ---------------------------------------------------------------------------
  # Layout
  # ---------------------------------------------------------------------------
  row_title <- 2L
  row_subtitle <- if (!is.null(sub_title)) 3L else NULL
  row_dropdowns <- if (!is.null(sub_title)) 5L else 4L
  col_data_start <- 2L

  # Determine leading columns
  id_col <- if ("Variable" %in% names(table)) "Variable" else "Community"
  has_label <- "Label" %in% names(table)
  has_community <- "Community" %in% names(table) && id_col != "Community"
  leading_cols <- id_col
  if (has_community) leading_cols <- c(leading_cols, "Community")
  if (has_label) leading_cols <- c(leading_cols, "Label")
  n_leading <- length(leading_cols)

  # Column layout per subgroup: Metric (hidden) | P Value (hidden) | Index (visible)
  n_total_cols <- n_leading + length(sgs) * 3

  raw_metric_cols <- integer(length(sgs))
  p_val_cols <- integer(length(sgs))
  index_cols_pos <- integer(length(sgs))
  hidden_cols <- integer(0)

  for (sg_i in seq_along(sgs)) {
    sg_offset <- col_data_start + n_leading + (sg_i - 1) * 3
    raw_metric_cols[sg_i] <- sg_offset
    p_val_cols[sg_i] <- sg_offset + 1
    index_cols_pos[sg_i] <- sg_offset + 2
    hidden_cols <- c(hidden_cols, sg_offset, sg_offset + 1)
  }

  # ---------------------------------------------------------------------------
  # Title and subtitle
  # ---------------------------------------------------------------------------
  openxlsx::writeData(wb, dash_sheet, title, startRow = row_title, startCol = col_data_start)
  openxlsx::addStyle(wb, dash_sheet, style = styles$title,
    rows = row_title, cols = col_data_start, stack = TRUE)

  if (!is.null(row_subtitle)) {
    openxlsx::writeData(wb, dash_sheet, sub_title, startRow = row_subtitle, startCol = col_data_start)
    openxlsx::addStyle(wb, dash_sheet, style = styles$sub_title,
      rows = row_subtitle, cols = col_data_start, stack = TRUE)
  }

  # ---------------------------------------------------------------------------
  # Dropdown controls — stacked vertically
  # When n_leading >= 2, controls go in leading columns (B, C)
  # When n_leading < 2, controls go over first index column area
  # ---------------------------------------------------------------------------
  label_col <- col_data_start
  if (n_leading >= 2L) {
    cell_col <- col_data_start + 1L
  } else {
    cell_col <- index_cols_pos[1]
  }
  current_row <- row_dropdowns

  dropdown_cell_style <- openxlsx::createStyle(
    border = "Bottom", borderStyle = "thin", halign = "left"
  )

  # Metric dropdown
  metric_cell_col <- cell_col
  metric_cell_row <- current_row
  openxlsx::writeData(wb, dash_sheet, "Metric: ", startRow = current_row, startCol = label_col)
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
    rows = current_row, cols = label_col, stack = TRUE)
  openxlsx::writeData(wb, dash_sheet, metric_labels[1], startRow = current_row, startCol = cell_col)
  openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
    rows = current_row, cols = cell_col, stack = TRUE)
  current_row <- current_row + 1L

  # Focus dropdown
  focus_cell_col <- cell_col
  focus_cell_row <- current_row
  openxlsx::writeData(wb, dash_sheet, "Focus: ", startRow = current_row, startCol = label_col)
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
    rows = current_row, cols = label_col, stack = TRUE)
  openxlsx::writeData(wb, dash_sheet, focus_options[1],
    startRow = current_row, startCol = cell_col)
  openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
    rows = current_row, cols = cell_col, stack = TRUE)

  current_row <- current_row + 1L

  # Weight dropdown (only when weighted results available)
  weight_cell_col <- NULL
  weight_cell_row <- NULL
  if (has_weights) {
    weight_cell_col <- cell_col
    weight_cell_row <- current_row
    openxlsx::writeData(wb, dash_sheet, "Weight: ", startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)
    openxlsx::writeData(wb, dash_sheet, "Unweighted", startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
      rows = current_row, cols = cell_col, stack = TRUE)

    weight_range <- paste0(lookup_sheet, "!$", num2let(weight_opt_col), "$2:$",
      num2let(weight_opt_col), "$3")
    openxlsx::dataValidation(wb, dash_sheet,
      col = cell_col, rows = current_row,
      type = "list", value = weight_range)
    current_row <- current_row + 1L
  }

  # Build metric key lookup formula
  dash_sheet_escaped <- gsub("'", "''", dash_sheet)
  metric_cell_abs <- paste0("'", dash_sheet_escaped, "'!$", num2let(metric_cell_col), "$", metric_cell_row)
  mk_lookup <- paste0(
    "INDEX(", lookup_sheet, "!$C$2:$C$", length(metric_labels) + 1,
    ",MATCH(", metric_cell_abs, ",", lookup_sheet, "!$B$2:$B$", length(metric_labels) + 1, ",0))"
  )

  # Warning column = next column after the dropdown cell
  warning_col <- cell_col + 1L

  # Focus warning — to the right of the Focus control (same row)
  focus_cell_let <- num2let(focus_cell_col)
  red_warning <- openxlsx::createStyle(fontColour = "#FF0000", textDecoration = "bold")
  red_rule <- paste0(
    "AND(", focus_cell_let, focus_cell_row, "<>\"Market\",",
    "OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\"))"
  )

  focus_warning_formula <- paste0(
    "IF(AND(", focus_cell_let, focus_cell_row, "<>\"Market\",",
    "OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\")),",
    num2let(metric_cell_col), metric_cell_row, "&\" must have a Market focus\",\"\")"
  )
  openxlsx::writeFormula(wb, dash_sheet, x = focus_warning_formula,
    startRow = focus_cell_row, startCol = warning_col)
  openxlsx::conditionalFormatting(wb, dash_sheet,
    cols = warning_col, rows = focus_cell_row,
    style = red_warning, type = "expression", rule = red_rule)

  # Red background on Focus dropdown cell itself
  red_cell <- openxlsx::createStyle(bgFill = "#FF0000", fontColour = "#FFFFFF")
  openxlsx::conditionalFormatting(wb, dash_sheet,
    cols = focus_cell_col, rows = focus_cell_row,
    style = red_cell, type = "expression", rule = red_rule)

  # Weight warning — to the right of the Weight control (same row)
  if (has_weights) {
    weight_warning_formula <- paste0(
      "IF(OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\"),",
      "\"Weights don't affect this metric\",\"\")"
    )
    openxlsx::writeFormula(wb, dash_sheet, x = weight_warning_formula,
      startRow = weight_cell_row, startCol = warning_col)
    weight_warn_rule <- paste0("OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\")")
    openxlsx::conditionalFormatting(wb, dash_sheet,
      cols = warning_col, rows = weight_cell_row,
      style = openxlsx::createStyle(fontColour = "#888888", textDecoration = "italic"),
      type = "expression", rule = weight_warn_rule)
  }

  # Dynamic Focus column in _lookup
  dyn_focus_col <- lk_col
  dyn_focus_let <- num2let(dyn_focus_col)
  lk_col <- lk_col + 1L
  openxlsx::writeData(wb, lookup_sheet, "Dynamic Focus", startRow = 1, startCol = dyn_focus_col)
  openxlsx::writeData(wb, lookup_sheet, "Market", startRow = 2, startCol = dyn_focus_col)
  if (length(focus_options) > 1) {
    for (fi in 2:length(focus_options)) {
      f_formula <- paste0(
        "IF(OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\"),\"\",A", fi + 1, ")"
      )
      openxlsx::writeFormula(wb, lookup_sheet, x = f_formula,
        startRow = fi + 1, startCol = dyn_focus_col)
    }
  }

  # Focus Count
  focus_count_col <- lk_col
  focus_count_let <- num2let(focus_count_col)
  lk_col <- lk_col + 1L
  openxlsx::writeData(wb, lookup_sheet, "Focus Count", startRow = 1, startCol = focus_count_col)
  count_formula <- paste0("COUNTIF($", dyn_focus_let, "$2:$", dyn_focus_let, "$", length(focus_options) + 1, ",\"<>\"&\"\")")
  openxlsx::writeFormula(wb, lookup_sheet, x = count_formula, startRow = 2, startCol = focus_count_col)

  # Data validation for dropdowns
  focus_range <- paste0("OFFSET(", lookup_sheet, "!$", dyn_focus_let, "$2,0,0,", lookup_sheet, "!$", focus_count_let, "$2,1)")
  metric_range <- paste0(lookup_sheet, "!$B$2:$B$", length(metric_labels) + 1)
  openxlsx::dataValidation(wb, dash_sheet,
    col = focus_cell_col, rows = focus_cell_row,
    type = "list", value = focus_range)
  openxlsx::dataValidation(wb, dash_sheet,
    col = metric_cell_col, rows = metric_cell_row,
    type = "list", value = metric_range)

  # ---------------------------------------------------------------------------
  # Data table
  # ---------------------------------------------------------------------------
  row_data_start <- current_row + 1L

  # Write leading headers
  for (ci in seq_along(leading_cols)) {
    openxlsx::writeData(wb, dash_sheet, gsub("_", " ", leading_cols[ci]),
      startRow = row_data_start, startCol = col_data_start + ci - 1)
  }

  # Write subgroup headers (Metric, P Value, Index per subgroup)
  for (sg_i in seq_along(sgs)) {
    sg_label <- gsub("_", " ", sgs[sg_i])
    openxlsx::writeData(wb, dash_sheet, paste(sg_label, "Metric"),
      startRow = row_data_start, startCol = raw_metric_cols[sg_i])
    openxlsx::writeData(wb, dash_sheet, paste(sg_label, "P Value"),
      startRow = row_data_start, startCol = p_val_cols[sg_i])
    openxlsx::writeData(wb, dash_sheet, sg_label,
      startRow = row_data_start, startCol = index_cols_pos[sg_i])
  }

  all_header_cols <- seq(col_data_start, col_data_start + n_total_cols - 1)
  openxlsx::addStyle(wb, dash_sheet, style = styles$header,
    rows = row_data_start, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "TopBottomLeft", borderStyle = "medium"),
    rows = row_data_start, cols = min(all_header_cols), stack = TRUE)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "TopBottomRight", borderStyle = "medium"),
    rows = row_data_start, cols = max(all_header_cols), stack = TRUE)

  # Write leading columns — static values
  for (ri in seq_len(n_results_rows)) {
    for (ci in seq_along(leading_cols)) {
      val <- as.character(table[[leading_cols[ci]]][ri])
      openxlsx::writeData(wb, dash_sheet, val,
        startRow = row_data_start + ri, startCol = col_data_start + ci - 1)
    }
  }

  # Excel references
  focus_ref <- paste0("$", num2let(focus_cell_col), "$", focus_cell_row)
  metric_ref <- paste0("$", num2let(metric_cell_col), "$", metric_cell_row)
  metric_key_range <- paste0(lookup_sheet, "!$B$2:$B$", length(metric_labels) + 1)
  metric_key_col <- paste0(lookup_sheet, "!$C$2:$C$", length(metric_labels) + 1)

  if (has_weights) {
    # Dynamic sheet reference via INDIRECT based on Weight dropdown
    weight_ref <- paste0("$", num2let(weight_cell_col), "$", weight_cell_row)
    # Build sheet name: IF(weight="Weighted", weighted_sheet, results_sheet)
    active_sheet <- paste0(
      "IF(", weight_ref, "=\"Weighted\",\"", weighted_results_sheet, "\",\"", results_sheet, "\")"
    )
    results_header_range <- paste0("INDIRECT(", active_sheet, "&\"!$1:$1\")")
    results_all_rows <- paste0("INDIRECT(", active_sheet, "&\"!$2:$", n_results_rows + 1, "\")")
    results_count_ref <- paste0("INDIRECT(", active_sheet, "&\"!$A$2:$A$", n_results_rows + 1, "\")")
  } else {
    results_header_range <- paste0(results_sheet, "!$1:$1")
    results_all_rows <- paste0(results_sheet, "!$2:$", n_results_rows + 1)
    results_count_ref <- paste0(results_sheet, "!$A$2:$A$", n_results_rows + 1)
  }

  # Write INDEX/MATCH formulas
  data_rows <- seq(row_data_start + 1, row_data_start + n_results_rows)

  .col_name_formula <- function(sg, mk) {
    paste0(
      "IF(", focus_ref, "=\"Market\",",
        "\"", sg, "_\"&", mk, ",",
        "IF(OR(", mk, "=\"maxVmin\",", mk, "=\"mi\"),",
          "\"", sg, "_\"&", mk, ",",
          "\"", sg, "_\"&", mk, "&\"_\"&", focus_ref,
        ")",
      ")"
    )
  }

  mk <- paste0(
    "INDEX(", metric_key_col, ",MATCH(", metric_ref, ",", metric_key_range, ",0))"
  )

  for (sg_i in seq_along(sgs)) {
    sg <- sgs[sg_i]
    col_name_f <- .col_name_formula(sg, mk)
    match_col <- paste0("MATCH(", col_name_f, ",", results_header_range, ",0)")

    pval_col_name <- paste0("\"", sg, "_p_val\"")
    pval_match <- paste0("MATCH(", pval_col_name, ",", results_header_range, ",0)")

    for (ri in seq_along(data_rows)) {
      row <- data_rows[ri]
      if (has_weights) {
        results_row_ref <- paste0("INDIRECT(", active_sheet, "&\"!", ri + 1, ":", ri + 1, "\")")
      } else {
        results_row_ref <- paste0(results_sheet, "!", ri + 1, ":", ri + 1)
      }

      # Raw metric (hidden)
      raw_formula <- paste0("INDEX(", results_row_ref, ",", match_col, ")")
      openxlsx::writeFormula(wb, dash_sheet, x = raw_formula,
        startRow = row, startCol = raw_metric_cols[sg_i])

      # P-value (hidden)
      pval_formula <- paste0("INDEX(", results_row_ref, ",", pval_match, ")")
      openxlsx::writeFormula(wb, dash_sheet, x = pval_formula,
        startRow = row, startCol = p_val_cols[sg_i])

      # Index = ABS(raw) / mean(ABS(all)) * 100
      cell_formula <- paste0(
        "ABS(INDEX(", results_row_ref, ",", match_col, "))",
        "/(SUMPRODUCT(ABS(INDEX(", results_all_rows, ",0,", match_col, ")))",
        "/ROWS(", results_count_ref, "))*100"
      )
      openxlsx::writeFormula(wb, dash_sheet, x = cell_formula,
        startRow = row, startCol = index_cols_pos[sg_i])
    }
  }

  # Hide raw metric + p-value columns
  openxlsx::setColWidths(wb, dash_sheet, cols = hidden_cols,
    widths = 8.43, hidden = rep(TRUE, length(hidden_cols)))

  # Style index columns
  openxlsx::addStyle(wb, dash_sheet, style = styles$center,
    rows = data_rows, cols = index_cols_pos, gridExpand = TRUE, stack = TRUE)

  # Left-align leading columns
  for (lci in seq_len(n_leading)) {
    openxlsx::addStyle(wb, dash_sheet, style = styles$left,
      rows = data_rows, cols = col_data_start + lci - 1, gridExpand = TRUE, stack = TRUE)
  }

  # Column widths
  openxlsx::setColWidths(wb, dash_sheet, cols = col_data_start, widths = variable_width)
  if (has_community) {
    openxlsx::setColWidths(wb, dash_sheet, cols = col_data_start + 1, widths = community_width)
    if (has_label) {
      openxlsx::setColWidths(wb, dash_sheet, cols = col_data_start + 2, widths = label_width)
    }
  } else if (has_label) {
    openxlsx::setColWidths(wb, dash_sheet, cols = col_data_start + 1, widths = label_width)
  }

  # ---------------------------------------------------------------------------
  # Separator and Total Impact
  # ---------------------------------------------------------------------------
  separator_row <- max(data_rows) + 1
  total_impact_row <- separator_row + 1

  openxlsx::addStyle(wb, dash_sheet, style = styles$separator,
    rows = separator_row, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)
  openxlsx::setRowHeights(wb, dash_sheet, rows = separator_row, heights = 0)

  openxlsx::writeData(wb, dash_sheet, "Total Impact",
    startRow = total_impact_row, startCol = col_data_start)

  for (sg_i in seq_along(sgs)) {
    sg <- sgs[sg_i]
    col_name_f <- .col_name_formula(sg, mk)
    match_col <- paste0("MATCH(", col_name_f, ",", results_header_range, ",0)")

    ti_formula <- paste0(
      "SUMPRODUCT(ABS(INDEX(", results_all_rows, ",0,", match_col, ")))",
      "/ROWS(", results_count_ref, ")"
    )
    openxlsx::writeFormula(wb, dash_sheet, x = ti_formula,
      startRow = total_impact_row, startCol = index_cols_pos[sg_i])
  }

  openxlsx::addStyle(wb, dash_sheet, style = styles$total_impact,
    rows = total_impact_row, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)

  # Outer box around table (header through total impact)
  table_rows <- seq(row_data_start, total_impact_row)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "Left", borderStyle = "medium"),
    rows = table_rows, cols = min(all_header_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "Right", borderStyle = "medium"),
    rows = table_rows, cols = max(all_header_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "Bottom", borderStyle = "medium"),
    rows = total_impact_row, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)

  # Dynamic footer
  index_desc_range <- paste0(lookup_sheet, "!$E$2:$E$", length(metric_labels) + 1)
  footer_formula <- paste0(
    "\"", engine_footer, ". \"&INDEX(", index_desc_range,
    ",MATCH(", metric_ref, ",", metric_key_range, ",0))"
  )
  openxlsx::writeFormula(wb, dash_sheet, x = footer_formula,
    startRow = total_impact_row + 1, startCol = col_data_start)
  openxlsx::writeData(wb, dash_sheet, "Bold italicized index means a negative relationship",
    startRow = total_impact_row + 2, startCol = col_data_start)
  openxlsx::writeData(wb, dash_sheet, "Black cells mean an insignificant relationship",
    startRow = total_impact_row + 3, startCol = col_data_start)

  # ---------------------------------------------------------------------------
  # Conditional formatting
  # ---------------------------------------------------------------------------
  for (sg_i in seq_along(index_cols_pos)) {
    i <- index_cols_pos[sg_i]
    raw_col <- raw_metric_cols[sg_i]
    pval_col <- p_val_cols[sg_i]

    # Colour scale (lowest priority)
    openxlsx::conditionalFormatting(wb, dash_sheet, cols = i, rows = data_rows,
      style = c("#f66a6e", "#feea8a", "#66bd7d"), type = "colourScale")

    # Bold italic for negative raw metric
    raw_col_let <- num2let(raw_col)
    neg_formula <- paste0(raw_col_let, data_rows[1], "<0")
    openxlsx::conditionalFormatting(wb, dash_sheet, cols = i, rows = data_rows,
      style = styles$neg_sign, type = "expression", rule = neg_formula)

    # Blackout for insignificant p-value (highest priority)
    pval_col_let <- num2let(pval_col)
    p_formula <- paste0(pval_col_let, data_rows[1], ">0.1")
    openxlsx::conditionalFormatting(wb, dash_sheet, cols = i, rows = data_rows,
      style = styles$insig, type = "expression", rule = p_formula)
  }

  # ---------------------------------------------------------------------------
  # Borders, freeze, filter
  # ---------------------------------------------------------------------------
  oxl_outer_box(wb, dash_sheet,
    row_start = row_data_start, row_end = max(data_rows),
    col_start = min(all_header_cols), col_end = max(all_header_cols),
    borderStyle = "medium")
  oxl_outer_box(wb, dash_sheet,
    row_start = total_impact_row, row_end = total_impact_row,
    col_start = min(all_header_cols), col_end = max(all_header_cols),
    borderStyle = "medium")

  openxlsx::freezePane(wb, dash_sheet,
    firstActiveRow = row_data_start + 1,
    firstActiveCol = col_data_start + n_leading)

  openxlsx::addFilter(wb, dash_sheet, rows = row_data_start, cols = all_header_cols)

  invisible(wb)
}
