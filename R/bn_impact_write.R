#' bn_impact_write
#'
#' @description Takes the output of \code{bn_impact()} and writes a formatted
#'   Excel workbook. Reads metadata (type, index_by, subgroups) from the result
#'   object.
#'
#' @param bn_impact_result The output of \code{bn_impact()}.
#' @param file_name Character. Prefix for output file name. File is saved as
#'   \code{{file_name} - Network Drivers of {dv}.xlsx}. If NULL, inherits from
#'   \code{sub_title}. If both are NULL, file is saved as
#'   \code{Network Drivers of {dv}.xlsx}.
#' @param sub_title Character. Subtitle text (e.g. project name). If NULL,
#'   inherits from \code{file_name}. Default NULL.
#' @param title Character. Title displayed in the sheet header. If NULL,
#'   defaults to \code{"Network Drivers of {dv}"} using the DV display name.
#' @param variable_width Column width for the Variable column (default 20).
#' @param label_width Column width for the Label column (default \code{"auto"}).
#' @param wb_type Character. Workbook type: \code{"standard"} (default) or
#'   \code{"dynamic"}.
#' @param add_simple_simulator Logical. If TRUE, adds a Simulator sheet with
#'   interactive dropdowns for exploring conditional probability distributions.
#'   Requires \code{bn_obj} to be provided. Default FALSE.
#' @param bn_obj The BN subgroups object (named list with \code{fit} and
#'   \code{meta} elements), as produced by \code{bn_finalize_network()}.
#'   Required when \code{add_simple_simulator = TRUE}.
#' @param dictionary Optional. A data frame or named object for variable labels.
#'   Passed to the simulator for labeling target variables.
#' @param path Directory to write workbook to (default \code{"."}).
#'
#' @return Workbook object (invisibly).
#'
#' @export
bn_impact_write <- function(
    bn_impact_result,
    file_name = NULL,
    sub_title = NULL,
    title = NULL,
    variable_width = 20,
    community_width = 20,
    label_width = "auto",
    wb_type = c("standard", "dynamic"),
    add_simple_simulator = FALSE,
    bn_obj = NULL,
    dictionary = NULL,
    path = "."
){

  wb_type <- match.arg(wb_type)

  if (is.null(file_name)) file_name <- sub_title
  if (is.null(sub_title)) sub_title <- file_name

  table <- bn_impact_result[["table"]]
  meta  <- bn_impact_result[["meta"]]
  subgroups <- meta[["subgroups"]]
  index_by  <- meta[["index_by"]]
  type      <- meta[["type"]]
  dv        <- meta[["dv"]]

  # Use name of dv if named, otherwise the value itself
dv_display <- if (!is.null(names(dv))) names(dv) else dv

  # Title
  if (is.null(title)) {
    title <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
  }

  # Footer based on estimation type
  engine_footer <- switch(type,
    "gr" = "Impact estimated with exact conditional probability distributions",
    "cp" = "Impact estimated with Monte Carlo conditional probability",
    "mi" = "Impact estimated with mutual information",
    "Impact estimated with Bayesian network inference"
  )

  lift <- meta[["lift"]]

  index_footer <- if (index_by %in% c("lift_first", "lift_second")) {
    lift_idx <- if (index_by == "lift_first") 1L else min(2L, length(lift))
    lift_val <- lift[lift_idx]
    if (lift_val == 0) {
      "Indexed by average market lift"
    } else {
      paste0("Indexed by ", round(lift_val * 100), "% market lift")
    }
  } else {
    switch(index_by,
      "maxVmin" = "Indexed by max vs min impact",
      "mi"      = "Indexed by mutual information",
      "none"    = NULL
    )
  }

  footer <- paste(c(engine_footer, index_footer), collapse = ". ")

  # Sheet name
  sheet_name <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
  if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)

  if (wb_type == "standard") {

    wb <- oxl_create_workbook()

    wb <- append_bn_impact(
      analysis_table = table,
      subgroups = subgroups,
      wb = wb,
      sheet_name = sheet_name,
      title = title,
      sub_title = sub_title,
      footer = footer,
      variable_width = variable_width,
      label_width = label_width,
      index_by = index_by
    )

    # File name: "{file_name} - Network Drivers of {dv}.xlsx" or "Network Drivers of {dv}.xlsx"
    dv_suffix <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
    fname <- if (!is.null(file_name)) {
      paste0(file_name, " - ", dv_suffix, ".xlsx")
    } else {
      paste0(dv_suffix, ".xlsx")
    }
    file_path <- file.path(path, fname)

    if (add_simple_simulator) {
      if (is.null(bn_obj)) stop("bn_obj is required when add_simple_simulator = TRUE")
      # Extract community lookup from table if Community column exists
      comm_lookup <- NULL
      if ("Community" %in% names(table)) {
        comm_lookup <- rlang::set_names(table$Community, table$Variable)
      }
      wb <- append_bn_simulator(
        wb = wb, obj = bn_obj, df = NULL, dv = dv,
        subgroups = subgroups, dictionary = dictionary,
        community_lookup = comm_lookup
      )
      # _sim_data and _sim_lookup sheets added — hide them
      n_sheets <- length(names(wb))
      vis <- rep(TRUE, n_sheets)
      sheet_names <- names(wb)
      for (si in seq_along(sheet_names)) {
        sn <- sheet_names[si]
        if (sn == "_sim_data") vis[si] <- FALSE
        if (sn == "_sim_lookup") vis[si] <- "veryHidden"
      }
      openxlsx::sheetVisibility(wb) <- vis
    }

    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

    invisible(wb)

  } else if (wb_type == "dynamic") {

    wb <- oxl_create_workbook()

    n_results_rows <- nrow(table)
    n_results_cols <- ncol(table)

    # -------------------------------------------------------------------------
    # Determine dropdown options from column names
    # -------------------------------------------------------------------------
    # Subgroup prefix (first subgroup used to detect available metrics)
    sg1 <- if (!is.null(subgroups)) subgroups[1] else NULL
    all_cols <- names(table)

    # Detect available metrics from first subgroup columns
    if (!is.null(sg1)) {
      sg_cols <- all_cols[startsWith(all_cols, paste0(sg1, "_"))]
      # Strip subgroup prefix to get metric suffixes
      metric_suffixes <- gsub(paste0("^", sg1, "_"), "", sg_cols)
    } else {
      metric_suffixes <- setdiff(all_cols, c("Variable", "Label", "Community"))
    }

    # Focus options: Market + any brand names found in lift columns
    all_lift_suffixes <- grep("^lift", metric_suffixes, value = TRUE)
    market_lift_suffixes <- grep("^lift$|^lift_\\d+$", all_lift_suffixes, value = TRUE)
    brand_lift_suffixes <- setdiff(all_lift_suffixes, market_lift_suffixes)

    # Extract unique brand names
    if (length(brand_lift_suffixes) > 0) {
      # Brand suffixes: lift_Brand_A or lift_0_Brand_A → strip lift prefix to get brand
      brand_names <- unique(gsub("^lift_\\d+_|^lift_", "", brand_lift_suffixes))
      focus_options <- c("Market", brand_names)
    } else {
      focus_options <- "Market"
    }

    # Metric options: human-readable labels → column suffix mapping
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

    # -------------------------------------------------------------------------
    # Sheet 1: Dashboard (created first so it appears first)
    # -------------------------------------------------------------------------
    dash_sheet <- sheet_name
    openxlsx::addWorksheet(wb, dash_sheet)

    # -------------------------------------------------------------------------
    # Sheet 2: Results (raw data)
    # -------------------------------------------------------------------------
    results_sheet <- "Results"
    openxlsx::addWorksheet(wb, results_sheet)
    openxlsx::writeData(wb, results_sheet, table, startRow = 1, startCol = 1)

    # -------------------------------------------------------------------------
    # Sheet 3: _lookup (hidden) — maps dropdown selections to column names
    # -------------------------------------------------------------------------
    lookup_sheet <- "_lookup"
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
    sgs <- if (!is.null(subgroups)) subgroups else "Total"
    openxlsx::writeData(wb, lookup_sheet, "Subgroup", startRow = 1, startCol = 4)
    for (si in seq_along(sgs)) {
      openxlsx::writeData(wb, lookup_sheet, sgs[si], startRow = si + 1, startCol = 4)
    }

    # Index description in E column (maps metric label → footer text)
    index_descriptions <- purrr::map_chr(metric_keys, function(mk) {
      if (mk == "lift" || mk == "lift_0") {
        "Indexed by average market lift"
      } else if (grepl("^lift_", mk)) {
        pct <- gsub("lift_", "", mk)
        paste0("Indexed by ", pct, "% market lift")
      } else if (mk == "maxVmin") {
        "Indexed by max vs min impact"
      } else if (mk == "mi") {
        "Indexed by mutual information"
      } else {
        paste("Indexed by", mk)
      }
    })
    openxlsx::writeData(wb, lookup_sheet, "Index Description", startRow = 1, startCol = 5)
    for (di in seq_along(index_descriptions)) {
      openxlsx::writeData(wb, lookup_sheet, index_descriptions[di], startRow = di + 1, startCol = 5)
    }

    # Styles
    styles <- list(
      title     = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
      sub_title = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
      header    = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE),
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

    # Layout
    row_title <- 2L
    row_subtitle <- if (!is.null(sub_title)) 3L else NULL
    row_dropdowns <- if (!is.null(sub_title)) 5L else 4L
    col_data_start <- 2L

    # Determine leading columns (Variable, Community, Label)
    id_col <- if ("Variable" %in% names(table)) "Variable" else "Community"
    has_label <- "Label" %in% names(table)
    has_community <- "Community" %in% names(table) && id_col != "Community"
    leading_cols <- id_col
    if (has_community) leading_cols <- c(leading_cols, "Community")
    if (has_label) leading_cols <- c(leading_cols, "Label")
    n_leading <- length(leading_cols)

    # Total columns on dashboard = leading + subgroups
    dash_col_names <- c(leading_cols, sgs)
    n_dash_cols <- length(dash_col_names)

    # -----------------------------------------------------------------------
    # Column layout per subgroup: Metric (hidden) | P Value (hidden) | Index (visible)
    # Compute positions FIRST so dropdowns can use visible columns
    # -----------------------------------------------------------------------
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

    # Visible columns = leading cols + index cols
    visible_cols <- c(seq(col_data_start, col_data_start + n_leading - 1), index_cols_pos)

    # Write title
    openxlsx::writeData(wb, dash_sheet, title, startRow = row_title, startCol = col_data_start)
    openxlsx::addStyle(wb, dash_sheet, style = styles$title,
      rows = row_title, cols = col_data_start, stack = TRUE)

    if (!is.null(row_subtitle)) {
      openxlsx::writeData(wb, dash_sheet, sub_title, startRow = row_subtitle, startCol = col_data_start)
      openxlsx::addStyle(wb, dash_sheet, style = styles$sub_title,
        rows = row_subtitle, cols = col_data_start, stack = TRUE)
    }

    # Dropdown placement — use only visible columns
    # Dropdown controls — stacked vertically in columns B (label) and C (value)
    label_col <- col_data_start
    cell_col <- col_data_start + 1L
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

    # Build metric key lookup formula (used by Focus warning and _lookup column F)
    dash_sheet_escaped <- gsub("'", "''", dash_sheet)
    metric_cell_abs <- paste0("'", dash_sheet_escaped, "'!$", num2let(metric_cell_col), "$", metric_cell_row)
    mk_lookup <- paste0(
      "INDEX(_lookup!$C$2:$C$", length(metric_labels) + 1,
      ",MATCH(", metric_cell_abs, ",_lookup!$B$2:$B$", length(metric_labels) + 1, ",0))"
    )

    # Warning text below Focus cell — formula shows message when non-Market + maxVmin/mi
    current_row <- current_row + 1L
    focus_cell_let <- num2let(focus_cell_col)
    warning_row <- current_row
    warning_formula <- paste0(
      "IF(AND(", focus_cell_let, focus_cell_row, "<>\"Market\",",
      "OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\")),",
      num2let(metric_cell_col), metric_cell_row, "&\" must have a Market focus\",\"\")"
    )
    openxlsx::writeFormula(wb, dash_sheet, x = warning_formula,
      startRow = warning_row, startCol = focus_cell_col)

    # Red styling on warning cell when message is shown
    red_warning <- openxlsx::createStyle(fontColour = "#FF0000", textDecoration = "bold")
    red_rule <- paste0(
      "AND(", focus_cell_let, focus_cell_row, "<>\"Market\",",
      "OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\"))"
    )
    openxlsx::conditionalFormatting(wb, dash_sheet,
      cols = focus_cell_col, rows = warning_row,
      style = red_warning, type = "expression", rule = red_rule)

    # Extend red bold styling across remaining columns on warning row
    openxlsx::conditionalFormatting(wb, dash_sheet,
      cols = seq(focus_cell_col + 1, col_data_start + n_total_cols - 1), rows = warning_row,
      style = red_warning, type = "expression", rule = red_rule)

    # Red background on the Focus dropdown cell itself
    red_cell <- openxlsx::createStyle(bgFill = "#FF0000", fontColour = "#FFFFFF")
    openxlsx::conditionalFormatting(wb, dash_sheet,
      cols = focus_cell_col, rows = focus_cell_row,
      style = red_cell, type = "expression", rule = red_rule)

    # Dynamic Focus column in _lookup (col F) — filters brands when metric is maxVmin/mi
    openxlsx::writeData(wb, lookup_sheet, "Dynamic Focus", startRow = 1, startCol = 6)
    openxlsx::writeData(wb, lookup_sheet, "Market", startRow = 2, startCol = 6)
    if (length(focus_options) > 1) {
      for (fi in 2:length(focus_options)) {
        f_formula <- paste0(
          "IF(OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\"),\"\",A", fi + 1, ")"
        )
        openxlsx::writeFormula(wb, lookup_sheet, x = f_formula,
          startRow = fi + 1, startCol = 6)
      }
    }

    # G2 = count of non-blank dynamic focus entries
    openxlsx::writeData(wb, lookup_sheet, "Focus Count", startRow = 1, startCol = 7)
    count_formula <- paste0("COUNTIF($F$2:$F$", length(focus_options) + 1, ",\"<>\"&\"\")")
    openxlsx::writeFormula(wb, lookup_sheet, x = count_formula, startRow = 2, startCol = 7)

    # Data validation for dropdowns
    focus_range <- paste0("OFFSET(_lookup!$F$2,0,0,_lookup!$G$2,1)")
    metric_range <- paste0("_lookup!$B$2:$B$", length(metric_labels) + 1)
    openxlsx::dataValidation(wb, dash_sheet,
      col = focus_cell_col, rows = focus_cell_row,
      type = "list", value = focus_range)
    openxlsx::dataValidation(wb, dash_sheet,
      col = metric_cell_col, rows = metric_cell_row,
      type = "list", value = metric_range)

    # Update row_data_start to account for stacked controls
    row_data_start <- current_row + 2L

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

    # Write leading columns (Variable, Label, etc.) — static values
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
    metric_key_range <- paste0("_lookup!$B$2:$B$", length(metric_labels) + 1)
    metric_key_col <- paste0("_lookup!$C$2:$C$", length(metric_labels) + 1)
    results_header_range <- "Results!$1:$1"
    results_all_rows <- paste0("Results!$2:$", n_results_rows + 1)
    results_count_ref <- paste0("Results!$A$2:$A$", n_results_rows + 1)

    # Write INDEX/MATCH formulas for each subgroup
    data_rows <- seq(row_data_start + 1, row_data_start + n_results_rows)

    # Helper: build col_name formula for a given subgroup
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

      # P-value column name (always {sg}_p_val)
      pval_col_name <- paste0("\"", sg, "_p_val\"")
      pval_match <- paste0("MATCH(", pval_col_name, ",", results_header_range, ",0)")

      for (ri in seq_along(data_rows)) {
        row <- data_rows[ri]
        results_row_ref <- paste0("Results!", ri + 1, ":", ri + 1)

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

    # Separator and Total Impact row
    separator_row <- max(data_rows) + 1
    total_impact_row <- separator_row + 1

    openxlsx::addStyle(wb, dash_sheet, style = styles$separator,
      rows = separator_row, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb, dash_sheet, rows = separator_row, heights = 0)

    # Total Impact = mean of absolute values in the index column
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

    # Dynamic footer: engine_footer & ". " & INDEX(lookup index descriptions)
    index_desc_range <- paste0("_lookup!$E$2:$E$", length(metric_labels) + 1)
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

    # Conditional formatting per index column
    # Order: colour scale first (lowest priority), then neg sign, then blackout (highest)
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

    # Borders
    oxl_outer_box(wb, dash_sheet,
      row_start = row_data_start, row_end = max(data_rows),
      col_start = min(all_header_cols), col_end = max(all_header_cols),
      borderStyle = "medium")
    oxl_outer_box(wb, dash_sheet,
      row_start = total_impact_row, row_end = total_impact_row,
      col_start = min(all_header_cols), col_end = max(all_header_cols),
      borderStyle = "medium")

    # Freeze panes
    openxlsx::freezePane(wb, dash_sheet,
      firstActiveRow = row_data_start + 1,
      firstActiveCol = col_data_start + n_leading)

    # Filter
    openxlsx::addFilter(wb, dash_sheet, rows = row_data_start, cols = all_header_cols)

    # File name
    dv_suffix <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
    fname <- if (!is.null(file_name)) {
      paste0(file_name, " - ", dv_suffix, ".xlsx")
    } else {
      paste0(dv_suffix, ".xlsx")
    }
    file_path <- file.path(path, fname)

    if (add_simple_simulator) {
      if (is.null(bn_obj)) stop("bn_obj is required when add_simple_simulator = TRUE")
      # Extract community lookup from table if Community column exists
      comm_lookup <- NULL
      if ("Community" %in% names(table)) {
        comm_lookup <- rlang::set_names(table$Community, table$Variable)
      }
      wb <- append_bn_simulator(
        wb = wb, obj = bn_obj, df = NULL, dv = dv,
        subgroups = subgroups, dictionary = dictionary,
        community_lookup = comm_lookup
      )
    }

    # Hide helper sheets — set visibility for all sheets
    n_sheets <- length(names(wb))
    vis <- rep(TRUE, n_sheets)
    sheet_names <- names(wb)
    for (si in seq_along(sheet_names)) {
      sn <- sheet_names[si]
      if (sn %in% c("Results", "_sim_data")) vis[si] <- FALSE
      if (sn %in% c("_lookup", "_sim_lookup")) vis[si] <- "veryHidden"
    }
    openxlsx::sheetVisibility(wb) <- vis

    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

    invisible(wb)
  }
}
