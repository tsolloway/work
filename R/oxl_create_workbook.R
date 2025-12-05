#' oxl_create_workbook
#'
#' @description Creates a new Excel workbook with customized window dimensions.
#' Uses `openxlsx::createWorkbook()` and sets workbook view size via XML.
#'
#' @param window_width Numeric. Width of the Excel window (default = 33125).
#' @param window_height Numeric. Height of the Excel window (default = 16105).
#'
#' @return An `openxlsx` workbook object.
#'
#' @examples
#' wb <- oxl_create_workbook()
#' openxlsx::addWorksheet(wb, "Sheet1")
#' openxlsx::writeData(wb, "Sheet1", head(iris))
#' @export
oxl_create_workbook <- function(
    window_width = 33125,
    window_height = 16105
) {

  # Load required libraries
  start(lib_oxl = TRUE)


  # Create workbook
  wb <- openxlsx::createWorkbook()


  # Set workbook view dimensions via XML
  wb[["workbook"]][["bookViews"]] <- glue::glue(
    '<bookViews>
       <workbookView xWindow="0" yWindow="0" windowWidth="{window_width}" windowHeight="{window_height}"/>
     </bookViews>'
  )


  wb
}
