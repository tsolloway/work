#' append_dmd_check
#' @description append_dmd_check
#' @export
append_dmd_check <- function(df, sheet_name = "dmd_check", wb = NULL){


  require(openxlsx)

  if( is.null(wb) ) wb <- createWorkbook()


  class(df$pre_lit_settlement) <- "accounting"
  class(df$med_pay) <- "accounting"
  class(df$total_settlement) <- "accounting"
  class(df$settlement_any) <- "accounting"
  class(df$resolution_amount) <- "accounting"

  class(df$resolution_link) <- "hyperlink"

  options("openxlsx.dateFormat" = "m/d/yy")


  addWorksheet(wb, sheet_name)

  writeData(wb, sheet_name, df, startRow = 1, startCol = 1)

  # logical conditional formatting
  conditionalFormatting(wb, sheet_name, cols = 15:23, rows = seq(nrow(df)) + 1, type = "contains", rule = "FALSE")

  #header
  addStyle(wb, sheet_name, style = createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE), rows = 1, cols = 1:48)

  #center
  addStyle(
    wb, sheet_name, style = createStyle(halign = "center"),
    rows = seq(nrow(df)) + 1, cols = c(1, 3, 5, 8, 9, 13, 25, 27, 28, 29, 30, 43, 45),
    gridExpand = TRUE
  )


  setColWidths(wb, sheet_name, cols = 1:48, widths = "auto")

  setColWidths(wb, sheet_name, cols = 15:23, widths = 6)
  setColWidths(wb, sheet_name, cols = c(25, 43), widths = 5)
  setColWidths(wb, sheet_name, cols = c(1,3, 8, 9, 28, 29, 30, 45), widths = 8)
  setColWidths(wb, sheet_name, cols = c(5, 27, 33, 34), widths = 10)
  setColWidths(wb, sheet_name, cols = c(4, 6, 7, 14, 44), widths = 12)
  setColWidths(wb, sheet_name, cols = c(10, 11, 12, 31, 32, 35, 36, 37:43, 46, 47), widths = 15)
  setColWidths(wb, sheet_name, cols = c(2, 24), widths = 23)

  freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)


  # box boarder the logic checks
  addStyle(
    wb, sheet_name, rows = seq(nrow(df)) + 1, cols = 15, stack = TRUE,
    style = createStyle(border = "Left" , borderStyle = "thick", borderColour = "black")
  )

  addStyle(
    wb, sheet_name, rows = seq(nrow(df)) + 1, cols = 23, stack = TRUE,
    style = createStyle(border = "Right" , borderStyle = "thick", borderColour = "black")
  )

  addStyle(
    wb, sheet_name, rows = 2, cols = 15:23, stack = TRUE,
    style = createStyle(border = "Top" , borderStyle = "thick", borderColour = "black")
  )

  addStyle(
    wb, sheet_name, rows = nrow(df) + 1, cols = 15:23, stack = TRUE,
    style = createStyle(border = "Bottom" , borderStyle = "thick", borderColour = "black")
  )


  attr(wb, "write_type") <- "openxlsx_formatted"
  attr(wb, "analysis") <- "dmd_check"

  # saveWorkbook(wb, "temp.xlsx", overwrite = TRUE)

  return(wb)
}
