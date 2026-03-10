#' append_drivers
#' @description append_drivers
#' @export
append_drivers <- function(
    analysis_table, subgroups = NULL, wb = NULL,
    sheet_name = NULL , title = NULL, footer = NULL, label_width = "auto",
    engine = c("linear", "logistic")
){

  if( is.null(wb) ) wb <- oxl_create_workbook()
  if( is.null(sheet_name) ) sheet_name <- "drivers"

  row_data_start <- 4
  col_data_start <- 2


  cols_all <- seq(ncol(analysis_table)) + (col_data_start - 1)
  cols_to_hide <- (which(!names(analysis_table) %in% c("Variable", "Label", gsub("_", " ", subgroups))) + (col_data_start - 1))
  cols_to_format <- (which(names(analysis_table) %in% c("Variable", "Label", gsub("_", " ", subgroups))) + (col_data_start - 1))
  driver_cols <- cols_to_format[-(1:2)]
  driver_rows <- (seq(nrow(analysis_table)) + row_data_start)[-nrow(analysis_table)]
  total_impact_row <- nrow(analysis_table) + row_data_start
  header_rows <- row_data_start


  openxlsx::addWorksheet(wb, sheet_name)

  openxlsx::writeData(wb, sheet_name, title, startRow = row_data_start - 1, startCol = col_data_start)

  openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(fontSize = 20, textDecoration = "bold"), rows = row_data_start - 1, cols = col_data_start)

  openxlsx::writeData(wb, sheet_name, footer, startRow = total_impact_row + 1, startCol = col_data_start)

  openxlsx::writeData(wb, sheet_name, analysis_table, startRow = row_data_start, startCol = col_data_start)

  openxlsx::setColWidths(wb, sheet_name, cols = cols_to_hide, hidden = rep(T, length(cols_to_hide)))

  openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(numFmt = "0.0", halign = "center"), rows = driver_rows, cols = cols_to_format, gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(halign = "left"), rows = driver_rows, cols = col_data_start + 1, gridExpand = TRUE)

  openxlsx::setColWidths(wb, sheet_name, cols = col_data_start + 1, widths = label_width)



  for(i in driver_cols){

    if(engine == "logistic"){
      neg_formula <- paste0(num2let(i-2), driver_rows[1], " < 0")
      p_formula <- paste0(num2let(i-13), driver_rows[1], " > .1")
    }else if(engine == "linear"){
      neg_formula <- paste0(num2let(i-5), driver_rows[1], " < 0")
      p_formula <- paste0(num2let(i-2), driver_rows[1], " > .1")
    }

    openxlsx::conditionalFormatting(wb, sheet_name, cols = i, rows = driver_rows, style = c("#f66a6e","#feea8a","#66bd7d"), type = "colourScale")
    openxlsx::conditionalFormatting(wb, sheet_name, cols = i, rows = driver_rows, style = openxlsx::createStyle(textDecoration = c("bold","italic")), rule = neg_formula)
    openxlsx::conditionalFormatting(wb, sheet_name, cols = i, rows = driver_rows, style = openxlsx::createStyle(bgFill = "black"), rule = p_formula)

  }



  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(
      border = "left" , borderStyle = "thick", borderColour = "black"
    ),
    rows = seq(row_data_start, total_impact_row), cols = cols_all %>% head(1), gridExpand = TRUE
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(
      numFmt = "0.0", halign = "center",
      border = "right", borderStyle = "thick", borderColour = "black"
    ),
    rows = seq(row_data_start, total_impact_row), cols = cols_all %>% tail(1), gridExpand = TRUE
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(
      textDecoration = "bold", halign = "center", wrapText = TRUE,
      border = "TopBottom", borderStyle = "thick", borderColour = "black"
    ),
    rows = header_rows, cols = cols_all, gridExpand = TRUE
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(
      numFmt = "0.0%", halign = "center",
      border = "TopBottom", borderStyle = "thick", borderColour = "black"
    ),
    rows = total_impact_row, cols = cols_all, gridExpand = TRUE
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(
      textDecoration = "bold", halign = "center", wrapText = TRUE,
      border = c("left", "top", "bottom") , borderStyle = "thick", borderColour = "black"
    ),
    rows = row_data_start, cols = cols_all %>% head(1)
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(
      border = c("left", "top", "bottom") , borderStyle = "thick", borderColour = "black"
    ),
    rows = total_impact_row, cols = cols_all %>% head(1)
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(
      textDecoration = "bold", halign = "center", wrapText = TRUE, border = c("right", "top", "bottom") , borderStyle = "thick", borderColour = "black"
    ),
    rows = row_data_start, cols = cols_all %>% tail(1)
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(
      numFmt = "0.0%", halign = "center", border = c("right", "top", "bottom") , borderStyle = "thick", borderColour = "black"
    ),
    rows = total_impact_row, cols = cols_all %>% tail(1)
  )


  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = row_data_start + 1,
    firstActiveCol = col_data_start + 2
  )


  return(wb)
}
