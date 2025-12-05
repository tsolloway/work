#' oxl_outer_box
#'
#' @description Draws an outer border around a rectangular range in an Excel sheet.
#' Applies top, bottom, left, and right borders individually using `openxlsx::addStyle`.
#'
#' @param wb An `openxlsx` workbook object.
#' @param sheet_name Name of the worksheet where the box will be applied.
#' @param row_start Integer. First row of the box.
#' @param row_end Integer. Last row of the box.
#' @param col_start Integer. First column of the box.
#' @param col_end Integer. Last column of the box.
#' @param borderStyle Character. Style of the border (default = "thick"). See `openxlsx::createStyle` for options.
#'
#' @return Invisibly returns `NULL`. The function modifies the workbook in place.
#'
#' @examples
#' wb <- openxlsx::createWorkbook()
#' openxlsx::addWorksheet(wb, "Sheet1")
#' openxlsx::writeData(wb, "Sheet1", head(iris))
#' oxl_outer_box(wb, "Sheet1", row_start = 1, row_end = 6, col_start = 1, col_end = 5)
#' openxlsx::saveWorkbook(wb, "example.xlsx", overwrite = TRUE)
#'
#' @export
oxl_outer_box <- function(
    wb, sheet_name, row_start, row_end, col_start, col_end, borderStyle = "thick"
) {
  # Ensure openxlsx is available
  stopifnot(requireNamespace("openxlsx", quietly = TRUE))

  # Create individual border styles
  bt <- openxlsx::createStyle(border = "top", borderStyle = borderStyle)
  bb <- openxlsx::createStyle(border = "bottom", borderStyle = borderStyle)
  bl <- openxlsx::createStyle(border = "left", borderStyle = borderStyle)
  br <- openxlsx::createStyle(border = "right", borderStyle = borderStyle)

  # Apply top border
  openxlsx::addStyle(
    wb, sheet = sheet_name, style = bt,
    rows = row_start, cols = seq(col_start, col_end),
    gridExpand = FALSE, stack = TRUE
  )

  # Apply bottom border
  openxlsx::addStyle(
    wb, sheet = sheet_name, style = bb,
    rows = row_end, cols = seq(col_start, col_end),
    gridExpand = FALSE, stack = TRUE
  )

  # Apply left border
  openxlsx::addStyle(
    wb, sheet = sheet_name, style = bl,
    rows = seq(row_start, row_end), cols = col_start,
    gridExpand = FALSE, stack = TRUE
  )

  # Apply right border
  openxlsx::addStyle(
    wb, sheet = sheet_name, style = br,
    rows = seq(row_start, row_end), cols = col_end,
    gridExpand = FALSE, stack = TRUE
  )

  invisible(NULL)
}
