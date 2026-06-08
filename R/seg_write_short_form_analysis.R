#' seg_write_short_form_analysis
#'
#' @description Writes the output of [seg_short_form_analysis()] to a formatted
#'   Excel workbook with native Excel line charts. Creates a single sheet with:
#'   - Title and project name header
#'   - Metric definitions (precision, recall, overall accuracy)
#'   - Optional segment legend with abbreviations and full names
#'   - Data table with merged headers, bordered, percentage-formatted
#'   - Precision and Recall line charts (via openpyxl/reticulate)
#'
#' @param short_form_analysis Output list from [seg_short_form_analysis()]. Must contain
#'   `$analysis` and `$results`. Project metadata (`project_name`, `project_number`)
#'   is pulled from this object when `project_name` is `NULL`.
#' @param where Character. Directory to save the Excel file. Defaults to
#'   the current working directory.
#' @param file_name Character. File name (without extension). If `NULL`,
#'   defaults to `"{project_name} - Short Form Analysis"`.
#' @param project_name Character. Project name displayed in the subtitle row.
#'   If `NULL`, derived from `short_form_analysis$project_name` and/or
#'   `short_form_analysis$project_number`. Falls back to `"Project (123456789)"`.
#' @param segment_labels Character vector. Short labels for each segment in
#'   order (e.g., `c("IRS", "BCC", "LPLB", "AE", "W&S", "TPP")`). If `NULL`,
#'   defaults to `"Seg 1"`, `"Seg 2"`, etc.
#' @param segment_descriptions Named character vector. Full descriptions keyed
#'   by segment label (e.g., `c("IRS" = "Involved Rx Skeptics", ...)`). If
#'   `NULL`, the legend section is omitted.
#'
#' @return Invisibly returns the file path.
#'
#' @export
seg_write_short_form_analysis <- function(
    short_form_analysis,
    where = getwd(),
    file_name = NULL,
    project_name = NULL,
    segment_labels = NULL,
    segment_descriptions = NULL
) {

  # short_form_analysis = short_form_analysis
  # where = getwd()
  # file_name = NULL
  # project_name = NULL
  # segment_labels = NULL
  # segment_descriptions = NULL

  # -- ensure reticulate (+ openpyxl) for the native Excel chart step --
  if(!"reticulate" %in% rownames(utils::installed.packages())){
    pak::pkg_install("reticulate")
    reticulate::py_install("openpyxl")
  }

  # -- resolve project_name from short_form_analysis if not provided --
  if (is.null(project_name)) {
    pn <- short_form_analysis[["project_name"]]
    pnum <- short_form_analysis[["project_number"]]
    if (!is.null(pn) && !is.null(pnum)) {
      project_name <- glue::glue("{pn} ({pnum})")
    } else {
      project_name <- pn %||% pnum
    }
  }
  project_name <- project_name %||% "Project (123456789)"

  # -- resolve file path --
  if (is.null(file_name)) {
    file_name <- glue::glue("{project_name} - Short Form Analysis")
  }
  file_path <- file.path(where, paste0(file_name, ".xlsx"))

  analysis <- short_form_analysis[["analysis"]]
  n_segs <- (ncol(analysis) - 2) / 2
  n_rows <- nrow(analysis)

  if (is.null(segment_labels)) {
    segment_labels <- glue::glue("Seg {seq(n_segs)}")
  }

  if (length(segment_labels) != n_segs) {
    stop(glue::glue("segment_labels length ({length(segment_labels)}) must match number of segments ({n_segs})."))
  }


  # -- build data matrix --
  data_mat <- analysis %>%
    dplyr::select(-dplyr::starts_with("Precision_"), -dplyr::starts_with("Recall_")) %>%
    dplyr::bind_cols(
      analysis %>% dplyr::select(dplyr::starts_with("Precision_")),
      analysis %>% dplyr::select(dplyr::starts_with("Recall_"))
    )

  precision_cols <- paste0("Precision_", segment_labels)
  recall_cols <- paste0("Recall_", segment_labels)
  names(data_mat) <- c("Items", "Overall Accuracy", precision_cols, recall_cols)


  # -- layout constants --
  sheet <- "short_form_analysis"
  col_start <- 2
  row_title <- 2
  row_subtitle <- 3
  row_defs_start <- 5
  row_header_1 <- row_defs_start + 4
  row_header_2 <- row_header_1 + 1
  row_data_start <- row_header_2 + 1
  row_data_end <- row_data_start + n_rows - 1

  col_items <- col_start
  col_overall <- col_start + 1
  col_prec_start <- col_start + 2
  col_prec_end <- col_prec_start + n_segs - 1
  col_rec_start <- col_prec_end + 1
  col_rec_end <- col_rec_start + n_segs - 1


  # -- create workbook --
  wb <- oxl_create_workbook()
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)


  # -- styles (consistent with seg_write_shell / seg_generate_spec) --
  s_title <- openxlsx::createStyle(textDecoration = "Bold", fontSize = 18)
  s_subtitle <- openxlsx::createStyle(
    textDecoration = c("Bold", "italic"), fontSize = 14
  )
  s_header <- openxlsx::createStyle(
    textDecoration = "Bold", fontSize = 12,
    halign = "center", valign = "center"
  )
  s_header_wrap <- openxlsx::createStyle(
    textDecoration = "Bold", fontSize = 12,
    halign = "center", valign = "center", wrapText = TRUE
  )
  s_percent <- oxl_style_percent(0)
  s_center <- oxl_style_center()
  s_legend_fill <- openxlsx::createStyle(fgFill = oxl_colorscale_neutral(1))


  # -- title --
  openxlsx::writeData(wb, sheet, "Short Form Analysis",
    startRow = row_title, startCol = col_start, colNames = FALSE
  )
  openxlsx::addStyle(wb, sheet, s_title,
    rows = row_title, cols = col_start, stack = TRUE
  )

  # -- subtitle --
  if (!is.null(project_name)) {
    openxlsx::writeData(wb, sheet, project_name,
      startRow = row_subtitle, startCol = col_start, colNames = FALSE
    )
    openxlsx::addStyle(wb, sheet, s_subtitle,
      rows = row_subtitle, cols = col_start, stack = TRUE
    )
  }


  # -- metric definitions --
  defs <- c(
    "Precision is the percentage of the shortened solution segment that are actually from the presented segment (i.e., purity)",
    "Recall is the percentage of the presented segment in the shortened solution (i.e., coverage)",
    "Overall accuracy is the percentage that the entire shortened solution matches the presented solution"
  )
  for (i in seq_along(defs)) {
    openxlsx::writeData(wb, sheet, defs[i],
      startRow = row_defs_start + i - 1, startCol = col_start, colNames = FALSE
    )
  }


  # -- segment legend --
  if (!is.null(segment_descriptions)) {
    legend_col_1 <- col_rec_start
    legend_col_2 <- legend_col_1 + 3
    legend_labels <- names(segment_descriptions)
    n_legend <- length(legend_labels)
    legend_rows <- ceiling(n_legend / 2)

    for (i in seq_along(legend_labels)) {
      lbl <- legend_labels[i]
      desc <- segment_descriptions[lbl]
      text <- glue::glue("{lbl} = {desc}")
      r <- row_defs_start + ((i - 1) %% legend_rows)
      col <- if (i <= legend_rows) legend_col_1 else legend_col_2

      openxlsx::writeData(wb, sheet, text,
        startRow = r, startCol = col, colNames = FALSE
      )
    }

    legend_row_range <- seq(row_defs_start, row_defs_start + legend_rows - 1)
    legend_col_range <- seq(legend_col_1, legend_col_2 + 2)
    openxlsx::addStyle(wb, sheet, s_legend_fill,
      rows = legend_row_range, cols = legend_col_range,
      gridExpand = TRUE, stack = TRUE
    )
  }


  # -- table headers --
  openxlsx::writeData(wb, sheet, "Items",
    startRow = row_header_1, startCol = col_items, colNames = FALSE
  )
  openxlsx::writeData(wb, sheet, "Overall Accuracy",
    startRow = row_header_1, startCol = col_overall, colNames = FALSE
  )
  openxlsx::writeData(wb, sheet, "Precision",
    startRow = row_header_1, startCol = col_prec_start, colNames = FALSE
  )
  openxlsx::writeData(wb, sheet, "Recall",
    startRow = row_header_1, startCol = col_rec_start, colNames = FALSE
  )

  # merge cells
  openxlsx::mergeCells(wb, sheet,
    cols = col_items, rows = row_header_1:row_header_2
  )
  openxlsx::mergeCells(wb, sheet,
    cols = col_overall, rows = row_header_1:row_header_2
  )
  openxlsx::mergeCells(wb, sheet,
    cols = col_prec_start:col_prec_end, rows = row_header_1
  )
  openxlsx::mergeCells(wb, sheet,
    cols = col_rec_start:col_rec_end, rows = row_header_1
  )

  # header styles
  openxlsx::addStyle(wb, sheet, s_header_wrap,
    rows = row_header_1, cols = c(col_items, col_overall),
    gridExpand = FALSE, stack = TRUE
  )
  openxlsx::addStyle(wb, sheet, s_header,
    rows = row_header_1, cols = c(col_prec_start, col_rec_start),
    gridExpand = FALSE, stack = TRUE
  )

  # segment sub-headers
  for (i in seq_along(segment_labels)) {
    openxlsx::writeData(wb, sheet, segment_labels[i],
      startRow = row_header_2, startCol = col_prec_start + i - 1, colNames = FALSE
    )
    openxlsx::writeData(wb, sheet, segment_labels[i],
      startRow = row_header_2, startCol = col_rec_start + i - 1, colNames = FALSE
    )
  }

  openxlsx::addStyle(wb, sheet, s_header,
    rows = row_header_2,
    cols = seq(col_prec_start, col_rec_end),
    gridExpand = FALSE, stack = TRUE
  )


  # -- write data --
  openxlsx::writeData(wb, sheet, data_mat[["Items"]],
    startRow = row_data_start, startCol = col_items, colNames = FALSE
  )
  openxlsx::addStyle(wb, sheet, s_center,
    rows = seq(row_data_start, row_data_end), cols = col_items,
    gridExpand = FALSE, stack = TRUE
  )

  openxlsx::writeData(wb, sheet, data_mat[["Overall Accuracy"]],
    startRow = row_data_start, startCol = col_overall, colNames = FALSE
  )

  for (i in seq_along(precision_cols)) {
    openxlsx::writeData(wb, sheet, data_mat[[precision_cols[i]]],
      startRow = row_data_start, startCol = col_prec_start + i - 1, colNames = FALSE
    )
  }

  for (i in seq_along(recall_cols)) {
    openxlsx::writeData(wb, sheet, data_mat[[recall_cols[i]]],
      startRow = row_data_start, startCol = col_rec_start + i - 1, colNames = FALSE
    )
  }

  # percentage format on all data columns except Items
  openxlsx::addStyle(wb, sheet, s_percent,
    rows = seq(row_data_start, row_data_end),
    cols = seq(col_overall, col_rec_end),
    gridExpand = TRUE, stack = TRUE
  )


  # -- borders --
  oxl_outer_box(wb, sheet,
    row_start = row_header_1, row_end = row_data_end,
    col_start = col_items, col_end = col_rec_end,
    borderStyle = "medium"
  )

  # header bottom border
  openxlsx::addStyle(wb, sheet,
    openxlsx::createStyle(border = "bottom", borderStyle = "medium"),
    rows = row_header_2,
    cols = seq(col_items, col_rec_end),
    gridExpand = FALSE, stack = TRUE
  )

  # vertical dividers
  divider_cols <- c(col_overall, col_prec_start, col_rec_start)
  s_divider <- openxlsx::createStyle(border = "left", borderStyle = "medium")
  for (dc in divider_cols) {
    openxlsx::addStyle(wb, sheet, s_divider,
      rows = seq(row_header_1, row_data_end),
      cols = dc,
      gridExpand = FALSE, stack = TRUE
    )
  }


  # -- column widths --
  openxlsx::setColWidths(wb, sheet, cols = col_items, widths = 8)
  openxlsx::setColWidths(wb, sheet, cols = col_overall, widths = 14)
  openxlsx::setColWidths(wb, sheet,
    cols = seq(col_prec_start, col_rec_end), widths = 10
  )


  # ====================================================================
  # Sheet 2: Variable Reduction Steps
  # ====================================================================
  sheet2 <- "reduction_steps"
  openxlsx::addWorksheet(wb, sheet2, gridLines = FALSE)

  results <- short_form_analysis[["results"]]
  input_lists <- results[["inputs"]]
  item_counts <- results[["n"]]
  max_vars <- max(item_counts)

  # derive which variable was removed at each step
  removed <- character(length(item_counts))
  removed[1] <- "\u2014"
  for (i in seq_along(item_counts)[-1]) {
    dropped <- setdiff(input_lists[[i - 1]], input_lists[[i]])
    removed[i] <- if (length(dropped) > 0) paste(dropped, collapse = ", ") else "\u2014"
  }

  # -- sheet 2 layout --
  s2_col_start <- 2
  s2_row_title <- 2
  s2_row_subtitle <- 3
  s2_row_header <- 5
  s2_row_data_start <- 6
  s2_row_data_end <- s2_row_data_start + length(item_counts) - 1

  s2_col_items <- s2_col_start
  s2_col_removed <- s2_col_start + 1
  s2_col_vars_start <- s2_col_start + 2
  s2_col_vars_end <- s2_col_vars_start + max_vars - 1

  # -- title --
  openxlsx::writeData(wb, sheet2, "Variable Reduction Steps",
    startRow = s2_row_title, startCol = s2_col_start, colNames = FALSE
  )
  openxlsx::addStyle(wb, sheet2, s_title,
    rows = s2_row_title, cols = s2_col_start, stack = TRUE
  )

  # -- subtitle --
  if (!is.null(project_name)) {
    openxlsx::writeData(wb, sheet2, project_name,
      startRow = s2_row_subtitle, startCol = s2_col_start, colNames = FALSE
    )
    openxlsx::addStyle(wb, sheet2, s_subtitle,
      rows = s2_row_subtitle, cols = s2_col_start, stack = TRUE
    )
  }

  # -- headers --
  openxlsx::writeData(wb, sheet2, "Items",
    startRow = s2_row_header, startCol = s2_col_items, colNames = FALSE
  )
  openxlsx::writeData(wb, sheet2, "Removed",
    startRow = s2_row_header, startCol = s2_col_removed, colNames = FALSE
  )
  openxlsx::writeData(wb, sheet2, "Remaining Variables",
    startRow = s2_row_header, startCol = s2_col_vars_start, colNames = FALSE
  )

  if (max_vars > 1) {
    openxlsx::mergeCells(wb, sheet2,
      cols = s2_col_vars_start:s2_col_vars_end, rows = s2_row_header
    )
  }

  openxlsx::addStyle(wb, sheet2, s_header,
    rows = s2_row_header,
    cols = seq(s2_col_items, s2_col_vars_end),
    gridExpand = FALSE, stack = TRUE
  )

  # -- data rows --
  for (i in seq_along(item_counts)) {
    r <- s2_row_data_start + i - 1

    openxlsx::writeData(wb, sheet2, item_counts[i],
      startRow = r, startCol = s2_col_items, colNames = FALSE
    )
    openxlsx::writeData(wb, sheet2, removed[i],
      startRow = r, startCol = s2_col_removed, colNames = FALSE
    )

    vars_at_step <- input_lists[[i]]
    for (j in seq_along(vars_at_step)) {
      openxlsx::writeData(wb, sheet2, vars_at_step[j],
        startRow = r, startCol = s2_col_vars_start + j - 1, colNames = FALSE
      )
    }
  }

  # center Items column
  openxlsx::addStyle(wb, sheet2, s_center,
    rows = seq(s2_row_data_start, s2_row_data_end), cols = s2_col_items,
    gridExpand = FALSE, stack = TRUE
  )

  # -- borders --
  oxl_outer_box(wb, sheet2,
    row_start = s2_row_header, row_end = s2_row_data_end,
    col_start = s2_col_items, col_end = s2_col_vars_end,
    borderStyle = "medium"
  )

  openxlsx::addStyle(wb, sheet2,
    openxlsx::createStyle(border = "bottom", borderStyle = "medium"),
    rows = s2_row_header,
    cols = seq(s2_col_items, s2_col_vars_end),
    gridExpand = FALSE, stack = TRUE
  )

  # vertical dividers
  s2_divider_cols <- c(s2_col_removed, s2_col_vars_start)
  for (dc in s2_divider_cols) {
    openxlsx::addStyle(wb, sheet2, s_divider,
      rows = seq(s2_row_header, s2_row_data_end),
      cols = dc,
      gridExpand = FALSE, stack = TRUE
    )
  }

  # -- column widths --
  openxlsx::setColWidths(wb, sheet2, cols = s2_col_items, widths = 8)
  openxlsx::setColWidths(wb, sheet2, cols = s2_col_removed, widths = 18)
  openxlsx::setColWidths(wb, sheet2,
    cols = seq(s2_col_vars_start, s2_col_vars_end), widths = 18
  )


  # ====================================================================
  # Sheet 3: All Combinations (best_combo only)
  # ====================================================================
  direction <- short_form_analysis[["direction"]] %||% "backward"
  all_combos <- short_form_analysis[["all_combos"]]

  if (direction == "best_combo" && !is.null(all_combos)) {
    sheet3 <- "all_combinations"
    openxlsx::addWorksheet(wb, sheet3, gridLines = FALSE)

    # pivot accuracy_seg into wide columns (same as sheet 1)
    combo_wide <- all_combos %>%
      dplyr::arrange(dplyr::desc(accuracy_overall), n) %>%
      dplyr::mutate(
        inputs_str = purrr::map_chr(inputs, ~paste(.x, collapse = ", "))
      )

    seg_wide <- combo_wide %>%
      dplyr::select(accuracy_seg) %>%
      purrr::flatten() %>%
      purrr::map(~ tidyr::pivot_wider(
        .x,
        names_from = Segment,
        values_from = c(Precision, Recall)
      )) %>%
      purrr::list_rbind()

    combo_tbl <- dplyr::bind_cols(
      combo_wide %>% dplyr::select(n, accuracy_overall, inputs_str),
      seg_wide
    )

    s3_prec_cols <- paste0("Precision_", segment_labels)
    s3_rec_cols <- paste0("Recall_", segment_labels)
    names(combo_tbl) <- c("Items", "Overall Accuracy", "Variables",
                          s3_prec_cols, s3_rec_cols)

    # layout
    s3_col_start <- 2
    s3_row_title <- 2
    s3_row_subtitle <- 3
    s3_row_header_1 <- 5
    s3_row_header_2 <- 6
    s3_row_data_start <- 7
    s3_row_data_end <- s3_row_data_start + nrow(combo_tbl) - 1

    s3_col_items <- s3_col_start
    s3_col_acc <- s3_col_start + 1
    s3_col_vars <- s3_col_start + 2
    s3_col_prec_start <- s3_col_start + 3
    s3_col_prec_end <- s3_col_prec_start + n_segs - 1
    s3_col_rec_start <- s3_col_prec_end + 1
    s3_col_rec_end <- s3_col_rec_start + n_segs - 1

    # title
    openxlsx::writeData(wb, sheet3, "All Combinations",
      startRow = s3_row_title, startCol = s3_col_start, colNames = FALSE
    )
    openxlsx::addStyle(wb, sheet3, s_title,
      rows = s3_row_title, cols = s3_col_start, stack = TRUE
    )

    # subtitle
    if (!is.null(project_name)) {
      openxlsx::writeData(wb, sheet3, project_name,
        startRow = s3_row_subtitle, startCol = s3_col_start, colNames = FALSE
      )
      openxlsx::addStyle(wb, sheet3, s_subtitle,
        rows = s3_row_subtitle, cols = s3_col_start, stack = TRUE
      )
    }

    # row 1 headers
    openxlsx::writeData(wb, sheet3, "Items",
      startRow = s3_row_header_1, startCol = s3_col_items, colNames = FALSE
    )
    openxlsx::writeData(wb, sheet3, "Overall Accuracy",
      startRow = s3_row_header_1, startCol = s3_col_acc, colNames = FALSE
    )
    openxlsx::writeData(wb, sheet3, "Variables",
      startRow = s3_row_header_1, startCol = s3_col_vars, colNames = FALSE
    )
    openxlsx::writeData(wb, sheet3, "Precision",
      startRow = s3_row_header_1, startCol = s3_col_prec_start, colNames = FALSE
    )
    openxlsx::writeData(wb, sheet3, "Recall",
      startRow = s3_row_header_1, startCol = s3_col_rec_start, colNames = FALSE
    )

    # merge: Items, Overall Accuracy, Variables span both header rows
    openxlsx::mergeCells(wb, sheet3,
      cols = s3_col_items, rows = s3_row_header_1:s3_row_header_2
    )
    openxlsx::mergeCells(wb, sheet3,
      cols = s3_col_acc, rows = s3_row_header_1:s3_row_header_2
    )
    openxlsx::mergeCells(wb, sheet3,
      cols = s3_col_vars, rows = s3_row_header_1:s3_row_header_2
    )
    if (n_segs > 1) {
      openxlsx::mergeCells(wb, sheet3,
        cols = s3_col_prec_start:s3_col_prec_end, rows = s3_row_header_1
      )
      openxlsx::mergeCells(wb, sheet3,
        cols = s3_col_rec_start:s3_col_rec_end, rows = s3_row_header_1
      )
    }

    # segment sub-headers
    for (i in seq_along(segment_labels)) {
      openxlsx::writeData(wb, sheet3, segment_labels[i],
        startRow = s3_row_header_2, startCol = s3_col_prec_start + i - 1, colNames = FALSE
      )
      openxlsx::writeData(wb, sheet3, segment_labels[i],
        startRow = s3_row_header_2, startCol = s3_col_rec_start + i - 1, colNames = FALSE
      )
    }

    # header styles
    openxlsx::addStyle(wb, sheet3, s_header_wrap,
      rows = s3_row_header_1,
      cols = c(s3_col_items, s3_col_acc, s3_col_vars),
      gridExpand = FALSE, stack = TRUE
    )
    openxlsx::addStyle(wb, sheet3, s_header,
      rows = s3_row_header_1,
      cols = c(s3_col_prec_start, s3_col_rec_start),
      gridExpand = FALSE, stack = TRUE
    )
    openxlsx::addStyle(wb, sheet3, s_header,
      rows = s3_row_header_2,
      cols = seq(s3_col_prec_start, s3_col_rec_end),
      gridExpand = FALSE, stack = TRUE
    )

    # data: Items
    openxlsx::writeData(wb, sheet3, combo_tbl[["Items"]],
      startRow = s3_row_data_start, startCol = s3_col_items, colNames = FALSE
    )
    openxlsx::addStyle(wb, sheet3, s_center,
      rows = seq(s3_row_data_start, s3_row_data_end), cols = s3_col_items,
      gridExpand = FALSE, stack = TRUE
    )

    # data: Overall Accuracy
    openxlsx::writeData(wb, sheet3, combo_tbl[["Overall Accuracy"]],
      startRow = s3_row_data_start, startCol = s3_col_acc, colNames = FALSE
    )

    # data: Variables
    openxlsx::writeData(wb, sheet3, combo_tbl[["Variables"]],
      startRow = s3_row_data_start, startCol = s3_col_vars, colNames = FALSE
    )

    # data: Precision + Recall per segment
    for (i in seq_along(s3_prec_cols)) {
      openxlsx::writeData(wb, sheet3, combo_tbl[[s3_prec_cols[i]]],
        startRow = s3_row_data_start, startCol = s3_col_prec_start + i - 1, colNames = FALSE
      )
    }
    for (i in seq_along(s3_rec_cols)) {
      openxlsx::writeData(wb, sheet3, combo_tbl[[s3_rec_cols[i]]],
        startRow = s3_row_data_start, startCol = s3_col_rec_start + i - 1, colNames = FALSE
      )
    }

    # percentage format on accuracy + precision + recall columns
    openxlsx::addStyle(wb, sheet3, s_percent,
      rows = seq(s3_row_data_start, s3_row_data_end),
      cols = seq(s3_col_acc, s3_col_rec_end),
      gridExpand = TRUE, stack = TRUE
    )

    # borders
    oxl_outer_box(wb, sheet3,
      row_start = s3_row_header_1, row_end = s3_row_data_end,
      col_start = s3_col_items, col_end = s3_col_rec_end,
      borderStyle = "medium"
    )
    openxlsx::addStyle(wb, sheet3,
      openxlsx::createStyle(border = "bottom", borderStyle = "medium"),
      rows = s3_row_header_2,
      cols = seq(s3_col_items, s3_col_rec_end),
      gridExpand = FALSE, stack = TRUE
    )

    # vertical dividers
    for (dc in c(s3_col_acc, s3_col_vars, s3_col_prec_start, s3_col_rec_start)) {
      openxlsx::addStyle(wb, sheet3, s_divider,
        rows = seq(s3_row_header_1, s3_row_data_end),
        cols = dc,
        gridExpand = FALSE, stack = TRUE
      )
    }

    # column widths
    openxlsx::setColWidths(wb, sheet3, cols = s3_col_items, widths = 8)
    openxlsx::setColWidths(wb, sheet3, cols = s3_col_acc, widths = 18)
    openxlsx::setColWidths(wb, sheet3, cols = s3_col_vars, widths = 60)
    openxlsx::setColWidths(wb, sheet3,
      cols = seq(s3_col_prec_start, s3_col_rec_end), widths = 10
    )

    # autoFilter for sorting
    openxlsx::addFilter(wb, sheet3,
      rows = s3_row_header_2,
      cols = seq(s3_col_items, s3_col_rec_end)
    )

    # freeze pane below headers
    openxlsx::freezePane(wb, sheet3,
      firstActiveRow = s3_row_data_start,
      firstActiveCol = s3_col_start
    )
  }


  # -- save to temp file, add charts, copy to final location --
  temp_path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, temp_path, overwrite = TRUE)

  .add_charts(temp_path, sheet,
    col_items = col_items, col_prec_start = col_prec_start,
    col_prec_end = col_prec_end, col_rec_start = col_rec_start,
    col_rec_end = col_rec_end, row_header_2 = row_header_2,
    row_data_start = row_data_start, row_data_end = row_data_end,
    data_mat = data_mat, precision_cols = precision_cols,
    recall_cols = recall_cols
  )

  file.copy(temp_path, file_path, overwrite = TRUE)
  unlink(temp_path)

  cli::cli_alert_success("Wrote short form analysis to {.file {file_path}}")
  invisible(file_path)
}


#' Add native Excel charts via openpyxl
#'
#' @description Internal helper — strips broken drawing references from
#'   openxlsx output, copies to a fresh openpyxl workbook, adds Precision
#'   and Recall line charts, and saves back.
#'
#' @keywords internal
#' @noRd
.add_charts <- function(
    file_path, sheet,
    col_items, col_prec_start, col_prec_end,
    col_rec_start, col_rec_end,
    row_header_2, row_data_start, row_data_end,
    data_mat, precision_cols, recall_cols
) {

  row_chart_start <- row_data_end + 2

  # -- y-axis limits --
  all_values <- c(unlist(data_mat[precision_cols]), unlist(data_mat[recall_cols]))
  y_min <- floor(min(all_values, na.rm = TRUE) * 10) / 10

  # -- chart anchors --
  .col_letter <- function(n) {
    result <- ""
    while (n > 0) {
      n <- n - 1
      result <- paste0(LETTERS[n %% 26 + 1], result)
      n <- n %/% 26
    }
    result
  }
  prec_anchor <- paste0(.col_letter(col_items), row_chart_start)
  rec_anchor <- paste0(.col_letter(col_items + 9), row_chart_start)

  file_path <- normalizePath(file_path, mustWork = TRUE)

  py_script <- glue::glue("
import zipfile
import os
import re
from openpyxl import load_workbook, Workbook
from openpyxl.chart import LineChart, Reference
from copy import copy

# strip broken drawing references from openxlsx output before loading
fixed_path = r'<<file_path>>' + '.tmp'
with zipfile.ZipFile(r'<<file_path>>', 'r') as zin:
    with zipfile.ZipFile(fixed_path, 'w') as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            if item.filename.startswith('xl/drawings/'):
                continue
            if item.filename.endswith('.rels'):
                content = data.decode('utf-8')
                content = re.sub(
                    r'<Relationship[^>]*Type=\"[^\"]*drawing[^\"]*\"[^>]*/>', '', content
                )
                data = content.encode('utf-8')
            zout.writestr(item, data)
os.replace(fixed_path, r'<<file_path>>')

# load cleaned file
src = load_workbook(r'<<file_path>>')

# copy ALL sheets to fresh workbook (avoids archive KeyError when adding charts)
dst = Workbook()
# remove default empty sheet
dst.remove(dst.active)

for sheet_name in src.sheetnames:
    src_ws = src[sheet_name]
    dst_ws = dst.create_sheet(title=sheet_name)
    dst_ws.sheet_view.showGridLines = False

    for row in src_ws.iter_rows():
        for cell in row:
            new_cell = dst_ws.cell(row=cell.row, column=cell.column, value=cell.value)
            if cell.has_style:
                new_cell.font = copy(cell.font)
                new_cell.fill = copy(cell.fill)
                new_cell.border = copy(cell.border)
                new_cell.alignment = copy(cell.alignment)
                new_cell.number_format = cell.number_format
                new_cell.protection = copy(cell.protection)

    for merge in src_ws.merged_cells.ranges:
        dst_ws.merge_cells(str(merge))

    for col_letter, dim in src_ws.column_dimensions.items():
        dst_ws.column_dimensions[col_letter].width = dim.width

    for row_num, dim in src_ws.row_dimensions.items():
        if dim.height:
            dst_ws.row_dimensions[row_num].height = dim.height

    # preserve freeze panes
    if src_ws.freeze_panes:
        dst_ws.freeze_panes = src_ws.freeze_panes

    # preserve auto filters
    if src_ws.auto_filter.ref:
        dst_ws.auto_filter.ref = src_ws.auto_filter.ref

dst_ws = dst['<<sheet>>']

# shared chart builder
from openpyxl.chart.layout import Layout, ManualLayout
from openpyxl.chart.shapes import GraphicalProperties

def build_chart(title, data_ref, cats_ref):
    lc = LineChart()
    lc.title = title
    lc.style = 2
    lc.roundedCorners = False
    lc.legend.position = 'b'
    lc.y_axis.numFmt = '0%'
    lc.y_axis.scaling.min = <<y_min>>
    lc.y_axis.scaling.max = 1
    lc.y_axis.delete = False
    lc.y_axis.tickLblPos = 'nextTo'
    from openpyxl.chart.axis import ChartLines
    from openpyxl.drawing.line import LineProperties
    lc.y_axis.majorGridlines = ChartLines(
        spPr=GraphicalProperties(ln=LineProperties(solidFill='D9D9D9'))
    )
    lc.x_axis.numFmt = '0'
    lc.x_axis.delete = False
    lc.x_axis.tickLblPos = 'low'
    lc.x_axis.majorGridlines = None
    lc.width = 18
    lc.height = 12
    lc.layout = Layout(
        manualLayout=ManualLayout(
            xMode='edge', yMode='edge',
            x=0.05, y=0.05, w=0.9, h=0.82
        )
    )
    lc.add_data(data_ref, titles_from_data=True)
    lc.set_categories(cats_ref)
    for s in lc.series:
        s.smooth = False
    return lc

cats = Reference(dst_ws, min_col=<<col_items>>, min_row=<<row_data_start>>,
                 max_row=<<row_data_end>>)

# precision chart
data1 = Reference(dst_ws, min_col=<<col_prec_start>>, min_row=<<row_header_2>>,
                   max_col=<<col_prec_end>>, max_row=<<row_data_end>>)
dst_ws.add_chart(build_chart('Precision', data1, cats), '<<prec_anchor>>')

# recall chart
data2 = Reference(dst_ws, min_col=<<col_rec_start>>, min_row=<<row_header_2>>,
                   max_col=<<col_rec_end>>, max_row=<<row_data_end>>)
dst_ws.add_chart(build_chart('Recall', data2, cats), '<<rec_anchor>>')

dst.save(r'<<file_path>>')
", .open = "<<", .close = ">>"
  )

  reticulate::py_run_string(py_script)
}
