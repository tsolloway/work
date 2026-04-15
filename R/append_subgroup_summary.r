#' append_subgroup_summary
#'
#' @description Appends a formatted subgroup count sheet to a workbook.
#'   Styling matches `append_bn_impact` / `append_means_check`.
#'
#' @param df_subgroup Tibble with Subgroup and Count columns.
#' @param wb Workbook object. If `NULL`, creates a new one.
#' @param sheet_name Character. Sheet name (default `"subgroup_count"`).
#' @param title Character. Title text above the data.
#' @param sub_title Character. Subtitle text below the title.
#' @param subgroup_width Numeric. Column width for the Subgroup column.
#'   Default 25.
#' @param write_file If `TRUE` (default), save the workbook to disk.
#'
#' @return Modified workbook object.
#'
#' @export
append_subgroup_summary <- function(
    df_subgroup,
    wb = NULL,
    sheet_name = NULL,
    title = "Subgroup Count",
    sub_title = NULL,
    subgroup_width = 25,
    write_file = TRUE
){

  if(is.null(wb)) wb <- oxl_create_workbook()
  if(is.null(sheet_name)) sheet_name <- "subgroup_count"


  # ---------------------------------------------------------------------------
  # Styles (matches bn_impact / append_means_check)
  # ---------------------------------------------------------------------------
  styles <- list(
    title     = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header    = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE,
                                      border = "TopBottom", borderStyle = "medium",
                                      fgFill = "#D9D9D9"),
    left      = openxlsx::createStyle(halign = "left"),
    count_fmt = openxlsx::createStyle(numFmt = "0", halign = "center")
  )


  # ---------------------------------------------------------------------------
  # Layout
  # ---------------------------------------------------------------------------
  row_title <- 2
  row_subtitle <- if(!is.null(sub_title)) 3L else NULL
  row_header <- if(!is.null(sub_title)) 5L else 4L
  col_data_start <- 2

  col_all <- seq(ncol(df_subgroup)) + col_data_start - 1
  col_first <- min(col_all)
  col_last <- max(col_all)

  col_subgroup <- col_data_start
  col_count <- col_subgroup + 1

  row_data_all <- seq(nrow(df_subgroup)) + row_header
  row_data_start <- min(row_data_all)
  row_data_end <- max(row_data_all)


  # ---------------------------------------------------------------------------
  # Write data
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)

  openxlsx::writeData(wb, sheet_name, title, startRow = row_title, startCol = col_data_start)
  openxlsx::addStyle(wb, sheet_name, style = styles$title,
    rows = row_title, cols = col_data_start, stack = TRUE)

  if(!is.null(row_subtitle)){
    openxlsx::writeData(wb, sheet_name, sub_title, startRow = row_subtitle, startCol = col_data_start)
    openxlsx::addStyle(wb, sheet_name, style = styles$sub_title,
      rows = row_subtitle, cols = col_data_start, stack = TRUE)
  }

  openxlsx::writeData(wb, sheet_name, df_subgroup, startRow = row_header, startCol = col_data_start)


  # ---------------------------------------------------------------------------
  # Cell formatting
  # ---------------------------------------------------------------------------
  openxlsx::addStyle(wb, sheet_name, style = styles$left,
    rows = row_data_all, cols = col_subgroup, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet_name, style = styles$count_fmt,
    rows = row_data_all, cols = col_count, gridExpand = TRUE, stack = TRUE)

  openxlsx::setColWidths(wb, sheet_name, cols = col_subgroup, widths = subgroup_width)


  # ---------------------------------------------------------------------------
  # Header
  # ---------------------------------------------------------------------------
  openxlsx::addStyle(wb, sheet_name, style = styles$header,
    rows = row_header, cols = col_all, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet_name,
    style = openxlsx::createStyle(border = "TopBottomLeft", borderStyle = "medium"),
    rows = row_header, cols = col_first, stack = TRUE)
  openxlsx::addStyle(wb, sheet_name,
    style = openxlsx::createStyle(border = "TopBottomRight", borderStyle = "medium"),
    rows = row_header, cols = col_last, stack = TRUE)


  # ---------------------------------------------------------------------------
  # Conditional formatting
  # ---------------------------------------------------------------------------
  openxlsx::conditionalFormatting(wb, sheet_name, cols = col_count, rows = row_data_all,
    style = c("#f66a6e", "#feea8a", "#66bd7d"), type = "colourScale")


  # ---------------------------------------------------------------------------
  # Outer box border
  # ---------------------------------------------------------------------------
  oxl_outer_box(wb, sheet_name,
    row_start = row_header, row_end = row_data_end,
    col_start = col_first, col_end = col_last,
    borderStyle = "medium"
  )


  # ---------------------------------------------------------------------------
  # Filter
  # ---------------------------------------------------------------------------
  openxlsx::addFilter(wb, sheet_name, rows = row_header, cols = col_all)


  if(write_file){
    openxlsx::saveWorkbook(wb, glue::glue("{title} - Subgroup Count.xlsx"), overwrite = TRUE)
  }

  return(wb)
}
