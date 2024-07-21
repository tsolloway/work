#' oxl_outer_box
#' @description oxl_outer_box
#' @export
oxl_outer_box <- function(
    wb, sheet_name, row_start, row_end, col_start, col_end, borderStyle = "thick"
){
  require(openxlsx)

  bt <- createStyle(border = "top", borderStyle = borderStyle)
  bb <- createStyle(border = "bottom", borderStyle = borderStyle)
  bl <- createStyle(border = "left", borderStyle = borderStyle)
  br <- createStyle(border = "right", borderStyle = borderStyle)


  addStyle(wb, sheet_name, style = bt,
           rows = row_start,
           cols = seq(col_start, col_end),
           gridExpand = F, stack = TRUE)

  addStyle(wb, sheet_name, style = bb,
           rows = row_end,
           cols = seq(col_start, col_end),
           gridExpand = F, stack = TRUE)

  addStyle(wb, sheet_name, style = bl,
           rows = seq(row_start, row_end),
           cols = col_start,
           gridExpand = F, stack = TRUE)

  addStyle(wb, sheet_name, style = br,
           rows = seq(row_start, row_end),
           cols = col_end,
           gridExpand = F, stack = TRUE)
}
