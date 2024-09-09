#' oxl_create_workbook
#' @description oxl_create_workbook
#' @export
oxl_create_workbook <- function(
    window_width = 33125,
    window_height = 16105
){

  start(lib_oxl = TRUE)

  wb <- openxlsx::createWorkbook()
  wb[["workbook"]][["bookViews"]] <- glue('<bookViews><workbookView xWindow=\"0\" yWindow=\"0\" windowWidth=\"{window_width}\" windowHeight=\"{window_height}\"/></bookViews>')

  return(wb)
}


