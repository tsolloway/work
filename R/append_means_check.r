#' append_means_check
#' @description append_means_check
#' @export
append_means_check <- function(
    df_means,
    wb = NULL,
    sheet_name = NULL,
    title = "ProjectName (Number)",
    sub_title = "Means Check",
    variable_width = "auto",
    label_width = "auto",
    write_file = TRUE
){

  if(is.null(wb)) wb <- oxl_create_workbook()
  if(is.null(sheet_name)) sheet_name <- "means_check"


  # ---------------------------------------------------------------------------
  # Styles (matches bn_impact)
  # ---------------------------------------------------------------------------
  styles <- list(
    title     = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header    = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE,
                                      border = "TopBottom", borderStyle = "medium",
                                      fgFill = "#D9D9D9"),
    center    = openxlsx::createStyle(halign = "center"),
    left      = openxlsx::createStyle(halign = "left"),
    mean_fmt  = openxlsx::createStyle(numFmt = "0.00", halign = "center"),
    count_fmt = openxlsx::createStyle(numFmt = "0", halign = "center")
  )


  # ---------------------------------------------------------------------------
  # Layout
  # ---------------------------------------------------------------------------
  row_title <- 2
  row_subtitle <- 3
  row_header <- 5
  col_data_start <- 2

  col_all <- seq(ncol(df_means)) + col_data_start - 1
  col_first <- min(col_all)
  col_last <- max(col_all)

  col_var <- col_data_start
  col_label <- col_var + 1
  col_count <- grep("- N", names(df_means)) + col_data_start - 1
  col_mean <- setdiff(col_all, c(col_var, col_label, col_count))

  row_data_all <- seq(nrow(df_means)) + row_header
  row_data_start <- min(row_data_all)
  row_data_end <- max(row_data_all)


  # ---------------------------------------------------------------------------
  # Write data
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)

  openxlsx::writeData(wb, sheet_name, title, startRow = row_title, startCol = col_data_start)
  openxlsx::addStyle(wb, sheet_name, style = styles$title,
    rows = row_title, cols = col_data_start, stack = TRUE)

  openxlsx::writeData(wb, sheet_name, sub_title, startRow = row_subtitle, startCol = col_data_start)
  openxlsx::addStyle(wb, sheet_name, style = styles$sub_title,
    rows = row_subtitle, cols = col_data_start, stack = TRUE)

  openxlsx::writeData(wb, sheet_name, df_means, startRow = row_header, startCol = col_data_start)


  # ---------------------------------------------------------------------------
  # Cell formatting
  # ---------------------------------------------------------------------------
  openxlsx::addStyle(wb, sheet_name, style = styles$mean_fmt,
    rows = row_data_all, cols = col_mean, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet_name, style = styles$count_fmt,
    rows = row_data_all, cols = col_count, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet_name, style = styles$center,
    rows = row_data_all, cols = col_var, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet_name, style = styles$left,
    rows = row_data_all, cols = col_label, gridExpand = TRUE, stack = TRUE)

  openxlsx::setColWidths(wb, sheet_name, cols = col_var, widths = variable_width)
  openxlsx::setColWidths(wb, sheet_name, cols = col_label, widths = label_width)


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
  for(i in col_mean){
    openxlsx::conditionalFormatting(wb, sheet_name, cols = i, rows = row_data_all,
      style = c("#f66a6e", "#feea8a", "#66bd7d"), type = "colourScale")
  }


  # ---------------------------------------------------------------------------
  # Outer box border (matches bn_impact)
  # ---------------------------------------------------------------------------
  oxl_outer_box(wb, sheet_name,
    row_start = row_header, row_end = row_data_end,
    col_start = col_first, col_end = col_last,
    borderStyle = "medium"
  )


  # ---------------------------------------------------------------------------
  # Freeze & filter
  # ---------------------------------------------------------------------------
  openxlsx::freezePane(wb, sheet_name,
    firstActiveRow = row_data_start,
    firstActiveCol = col_label + 1)

  openxlsx::addFilter(wb, sheet_name, rows = row_header, cols = col_all)


  if(write_file){
    openxlsx::saveWorkbook(wb, glue::glue("{title} - Means Check.xlsx"), overwrite = TRUE)
  }

  return(wb)
}
