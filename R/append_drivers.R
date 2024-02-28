#' append_drivers
#' @description append_drivers
#' @export
append_drivers <- function(analysis_table, wb = NULL, sheet_name = NULL , title = NULL, footer = NULL, label_width = "auto"){

  require(openxlsx)

  if( is.null(wb) ) wb <- createWorkbook()
  if( is.null(sheet_name) ) sheet_name <- "drivers"

  row_data_start <- 4
  col_data_start <- 2


  cols_all <- seq(ncol(analysis_table)) + (col_data_start - 1)
  cols_to_hide <- (which(!names(analysis_table) %in% c("Variables", "Labels", gsub("_", " ", subgroups))) + (col_data_start - 1))
  cols_to_format <- (which(names(analysis_table) %in% c("Variables", "Labels", gsub("_", " ", subgroups))) + (col_data_start - 1))
  driver_cols <- cols_to_format[-(1:2)]
  driver_rows <- (seq(nrow(analysis_table)) + row_data_start)[-nrow(analysis_table)]
  total_impact_row <- nrow(analysis_table) + row_data_start
  header_rows <- row_data_start


  addWorksheet(wb, sheet_name)

  writeData(wb, sheet_name, title, startRow = row_data_start - 1, startCol = col_data_start)
  addStyle(wb, sheet_name, style = createStyle(fontSize = 20, textDecoration = "bold"), rows = row_data_start - 1, cols = col_data_start)

  writeData(wb, sheet_name, footer, startRow = total_impact_row + 1, startCol = col_data_start)


  writeData(
    wb, sheet_name, analysis_table, startRow = row_data_start, startCol = col_data_start
  )

  setColWidths(wb, sheet_name, cols = cols_to_hide, hidden = rep(T, length(cols_to_hide)))

  addStyle(wb, sheet_name, style = createStyle(numFmt = "0.0", halign = "center"), rows = driver_rows, cols = cols_to_format, gridExpand = TRUE)

  addStyle(wb, sheet_name, style = createStyle(halign = "left"), rows = driver_rows, cols = col_data_start + 1, gridExpand = TRUE)
  setColWidths(wb, sheet_name, cols = col_data_start + 1, widths = label_width)

  for(i in driver_cols){
    neg_formula <- paste0(work::num2let(i-2), driver_rows[1], " < 0")
    p_formula <- paste0(work::num2let(i-13), driver_rows[1], " > .1")
    conditionalFormatting(wb, sheet_name, cols = i, rows = driver_rows, style = c("#f66a6e","#feea8a","#66bd7d"), type = "colourScale")
    conditionalFormatting(wb, sheet_name, cols = i, rows = driver_rows, style = createStyle(textDecoration = c("bold","italic")), rule = neg_formula)
    conditionalFormatting(wb, sheet_name, cols = i, rows = driver_rows, style = createStyle(bgFill = "black"), rule = p_formula)
  };rm(i, p_formula)


  addStyle(wb, sheet_name,
           style = createStyle(
             border = "left" , borderStyle = "thick", borderColour = "black"
           ),
           rows = seq(row_data_start, total_impact_row), cols = cols_all %>% head(1), gridExpand = TRUE)

  addStyle(wb, sheet_name,
           style = createStyle(
             numFmt = "0.0", halign = "center",
             border = "right", borderStyle = "thick", borderColour = "black"
           ),
           rows = seq(row_data_start, total_impact_row), cols = cols_all %>% tail(1), gridExpand = TRUE)


  addStyle(wb, sheet_name,
           style = createStyle(
             textDecoration = "bold", halign = "center", wrapText = TRUE,
             border = "TopBottom", borderStyle = "thick", borderColour = "black"
           ),
           rows = header_rows, cols = cols_all, gridExpand = TRUE)

  addStyle(wb, sheet_name,
           style = createStyle(
             numFmt = "0.0%", halign = "center",
             border = "TopBottom", borderStyle = "thick", borderColour = "black"
           ),
           rows = total_impact_row, cols = cols_all, gridExpand = TRUE)


  addStyle(wb, sheet_name, style = createStyle(
    textDecoration = "bold", halign = "center", wrapText = TRUE,
    border = c("left", "top", "bottom") , borderStyle = "thick", borderColour = "black"
  ), rows = row_data_start, cols = cols_all %>% head(1))

  addStyle(wb, sheet_name, style = createStyle(
    border = c("left", "top", "bottom") , borderStyle = "thick", borderColour = "black"
  ), rows = total_impact_row, cols = cols_all %>% head(1))

  addStyle(wb, sheet_name, style = createStyle(
    textDecoration = "bold", halign = "center", wrapText = TRUE,
    border = c("right", "top", "bottom") , borderStyle = "thick", borderColour = "black"
  ), rows = row_data_start, cols = cols_all %>% tail(1))

  addStyle(wb, sheet_name, style = createStyle(
    numFmt = "0.0%", halign = "center",
    border = c("right", "top", "bottom") , borderStyle = "thick", borderColour = "black"
  ),rows = total_impact_row, cols = cols_all %>% tail(1))

  freezePane(
    wb,
    sheet_name,
    firstActiveRow = row_data_start + 1,
    firstActiveCol = col_data_start + 2
  )

  return(wb)
}


