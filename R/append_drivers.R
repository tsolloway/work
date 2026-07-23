#' append_drivers
#'
#' @description Appends a formatted driver analysis sheet to a workbook.
#'   Handles the "Total Impact" summary row, column hiding, conditional
#'   formatting, and styling.
#'
#' @param analysis_table Tibble from \code{driver_impact()$table}.
#' @param subgroups Character vector or NULL. Subgroup names used to identify
#'   display columns.
#' @param wb Workbook object. If NULL, creates a new one.
#' @param sheet_name Character. Sheet name (default \code{"drivers"}).
#' @param title Character. Title text above the data.
#' @param sub_title Character. Subtitle text below the title.
#' @param footer Character. Footer text below the data.
#' @param variable_width Column width for Variable column (default 20).
#' @param label_width Column width for Label column (default \code{"auto"}).
#' @param engine Character. \code{"linear"} or \code{"logistic"}. Controls
#'   which metric is used for the Total Impact row and conditional formatting.
#'
#' @return Modified workbook object.
#'
#' @export
append_drivers <- function(
    analysis_table,
    subgroups = NULL,
    wb = NULL,
    sheet_name = NULL,
    title = NULL,
    sub_title = NULL,
    footer = NULL,
    variable_width = 20,
    label_width = "auto",
    engine = c("linear", "logistic")
){

  engine <- match.arg(engine)
  if (is.null(wb)) wb <- oxl_create_workbook()
  if (is.null(sheet_name)) sheet_name <- "drivers"
  if (is.null(subgroups)) subgroups <- "Total"


  # ---------------------------------------------------------------------------
  # Styles
  # ---------------------------------------------------------------------------
  styles <- list(
    title         = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title     = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header        = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE),
    center        = openxlsx::createStyle(numFmt = "0", halign = "center"),
    left          = openxlsx::createStyle(halign = "left"),
    total_impact  = openxlsx::createStyle(numFmt = "0.0%", halign = "center"),
    separator     = openxlsx::createStyle(fgFill = "black"),
    # fontColour matches the HTML dashboards' .rdx-neg danger red so the
    # "Bold italicized red index" footer note holds across deliverables.
    neg_sign      = openxlsx::createStyle(textDecoration = c("bold", "italic"),
                                          fontColour = resondex_brand()$semantic$danger),
    insig         = openxlsx::createStyle(bgFill = "black", fontColour = "white")
  )


  # ---------------------------------------------------------------------------
  # Compute Total Impact row
  # ---------------------------------------------------------------------------
  metric_suffix <- if (engine == "linear") "_r2" else "_prob_shift"

  total_row <- analysis_table[1, ]
  total_row[1, ] <- NA
  total_row[["Variable"]] <- "Total Impact"

  for (sg in subgroups) {
    metric_col <- paste0(sg, metric_suffix)
    if (sg %in% names(analysis_table) && metric_col %in% names(analysis_table)) {
      total_row[[sg]] <- mean(abs(analysis_table[[metric_col]]), na.rm = TRUE)
    }
  }

  first_index_col <- subgroups[[1]]
  if (first_index_col %in% names(analysis_table)) {
    analysis_table <- analysis_table %>%
      dplyr::arrange(dplyr::desc(.data[[first_index_col]]))
  }

  analysis_table <- dplyr::bind_rows(analysis_table, total_row)


  # ---------------------------------------------------------------------------
  # Layout constants
  # ---------------------------------------------------------------------------
  row_title <- 2
  row_subtitle <- if (!is.null(sub_title)) 3L else NULL
  row_data_start <- if (!is.null(sub_title)) 5L else 4L
  col_data_start <- 2L

  display_names <- c("Variable", "Label", subgroups)
  cols_all <- seq(ncol(analysis_table)) + (col_data_start - 1)
  col_first <- utils::head(cols_all, 1)
  col_last <- utils::tail(cols_all, 1)
  cols_to_hide <- (which(!names(analysis_table) %in% display_names) + (col_data_start - 1))
  cols_to_format <- (which(names(analysis_table) %in% display_names) + (col_data_start - 1))

  driver_cols <- cols_to_format[-(1:2)]
  driver_rows <- (seq(nrow(analysis_table)) + row_data_start)[-nrow(analysis_table)]
  separator_row <- max(driver_rows) + 1
  total_impact_row <- separator_row + 1


  # ---------------------------------------------------------------------------
  # Write data
  # ---------------------------------------------------------------------------
  write_table <- analysis_table
  names(write_table) <- gsub("_", " ", names(write_table))

  write_data <- write_table[-nrow(write_table), ]
  write_total <- write_table[nrow(write_table), ]

  openxlsx::addWorksheet(wb, sheet_name)

  openxlsx::writeData(wb, sheet_name, title, startRow = row_title, startCol = col_data_start)
  openxlsx::addStyle(wb, sheet_name, style = styles$title,
    rows = row_title, cols = col_data_start, stack = TRUE
  )

  if (!is.null(row_subtitle)) {
    openxlsx::writeData(wb, sheet_name, sub_title, startRow = row_subtitle, startCol = col_data_start)
    openxlsx::addStyle(wb, sheet_name, style = styles$sub_title,
      rows = row_subtitle, cols = col_data_start, stack = TRUE
    )
  }
  openxlsx::writeData(wb, sheet_name, write_data, startRow = row_data_start, startCol = col_data_start)
  # Pin the header row height. wrapText = TRUE on the header style would
  # otherwise auto-fit row height based on every cell — including hidden
  # columns whose long names wrap into many lines and bloat the header.
  openxlsx::setRowHeights(wb, sheet_name, rows = row_data_start, heights = 36)
  openxlsx::addFilter(wb, sheet_name, rows = row_data_start, cols = seq(col_data_start, col_data_start + ncol(write_table) - 1))

  # Hidden black separator row to break the filter range before Total Impact
  openxlsx::addStyle(wb, sheet_name, style = styles$separator,
    rows = separator_row, cols = cols_all, gridExpand = TRUE, stack = TRUE
  )
  openxlsx::setRowHeights(wb, sheet_name, rows = separator_row, heights = 0)

  openxlsx::writeData(wb, sheet_name, write_total, startRow = total_impact_row, startCol = col_data_start, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, footer, startRow = total_impact_row + 1, startCol = col_data_start)
  openxlsx::writeData(wb, sheet_name, "Bold italicized red index means a negative relationship", startRow = total_impact_row + 2, startCol = col_data_start)
  openxlsx::writeData(wb, sheet_name, "Black cells mean an insignificant relationship", startRow = total_impact_row + 3, startCol = col_data_start)
  openxlsx::setColWidths(wb, sheet_name, cols = cols_to_hide, widths = 8.43, hidden = rep(TRUE, length(cols_to_hide)))


  # ---------------------------------------------------------------------------
  # Base styles
  # ---------------------------------------------------------------------------
  openxlsx::addStyle(wb, sheet_name, style = styles$center,
    rows = driver_rows, cols = driver_cols, gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(wb, sheet_name, style = styles$left,
    rows = driver_rows, cols = col_data_start, gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(wb, sheet_name, style = styles$left,
    rows = driver_rows, cols = col_data_start + 1, gridExpand = TRUE, stack = TRUE
  )
  openxlsx::setColWidths(wb, sheet_name, cols = col_data_start, widths = variable_width)
  openxlsx::setColWidths(wb, sheet_name, cols = col_data_start + 1, widths = label_width)

  # Header row
  openxlsx::addStyle(wb, sheet_name, style = styles$header,
    rows = row_data_start, cols = cols_all, gridExpand = TRUE, stack = TRUE
  )

  # Total impact row
  openxlsx::addStyle(wb, sheet_name, style = styles$total_impact,
    rows = total_impact_row, cols = cols_all, gridExpand = TRUE, stack = TRUE
  )


  # ---------------------------------------------------------------------------
  # Conditional formatting per driver column
  # ---------------------------------------------------------------------------
  sign_suffix <- if (engine == "linear") "_coefficient" else "_prob_shift"
  p_suffix <- "_p"

  for (i in driver_cols) {

    col_idx_in_table <- i - (col_data_start - 1)
    sg <- names(analysis_table)[col_idx_in_table]

    if (length(sg) == 1) {
      sign_col_name <- paste0(sg, sign_suffix)
      p_col_name <- paste0(sg, p_suffix)

      sign_col_pos <- which(names(analysis_table) == sign_col_name)
      p_col_pos <- which(names(analysis_table) == p_col_name)

      # openxlsx writes rules in reverse order — last call = highest priority in Excel
      openxlsx::conditionalFormatting(wb, sheet_name, cols = i, rows = driver_rows,
        style = c("#f66a6e", "#feea8a", "#66bd7d"), type = "colourScale")

      if (length(p_col_pos) == 1) {
        p_excel_col <- p_col_pos + (col_data_start - 1)
        p_formula <- paste0(num2let(p_excel_col), driver_rows[1], " > .1")
        openxlsx::conditionalFormatting(wb, sheet_name, cols = i, rows = driver_rows,
          style = styles$insig, type = "expression", rule = p_formula)
      }

      # neg_sign added LAST so it sits ABOVE insig in Excel's rule order:
      # on a negative + insignificant cell the red bold italic font wins
      # the conflict while insig still supplies the black fill (mirrors
      # the HTML dashboards' compound .rdx-neg.rdx-insig rule).
      if (length(sign_col_pos) == 1) {
        sign_excel_col <- sign_col_pos + (col_data_start - 1)
        neg_formula <- paste0(num2let(sign_excel_col), driver_rows[1], " < 0")
        openxlsx::conditionalFormatting(wb, sheet_name, cols = i, rows = driver_rows,
          style = styles$neg_sign, type = "expression", rule = neg_formula)
      }
    }
  }


  # ---------------------------------------------------------------------------
  # Borders
  # ---------------------------------------------------------------------------
  # Header box
  oxl_outer_box(wb, sheet_name,
    row_start = row_data_start, row_end = max(driver_rows),
    col_start = col_first, col_end = col_last,
    borderStyle = "medium"
  )

  # Total impact box
  oxl_outer_box(wb, sheet_name,
    row_start = total_impact_row, row_end = total_impact_row,
    col_start = col_first, col_end = col_last,
    borderStyle = "medium"
  )

  # Freeze panes
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = row_data_start + 1, firstActiveCol = col_data_start + 2)

  return(wb)
}
