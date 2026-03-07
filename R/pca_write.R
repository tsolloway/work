#' pca_write
#'
#' @description Writes a PCA analysis object to a formatted Excel workbook with
#'   one sheet per factor solution, including loading tables, variance explained,
#'   and color-coded formatting.
#'
#' @param pca_analysis_object A list returned by [pca_analysis()].
#' @param clean_max Numeric. Loading threshold for highlighting clean variables
#'   (default: `0.25`).
#' @param file_name Character. Base file name (default: `"Factor Analysis"`).
#' @param where Character. Output directory (default: current working directory).
#' @param return_location Logical. If `TRUE` (default), returns the file path
#'   instead of the workbook object.
#'
#' @return The saved file path (if `return_location = TRUE`) or the workbook
#'   object.
#'
#' @export
pca_write <- function(
    pca_analysis_object, clean_max = .25, file_name = "Factor Analysis",  where = NULL, return_location = TRUE
){

  if(is.null(where)){
    where <- getwd()
  }


  styles <- list(
    title        = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    header       = openxlsx::createStyle(textDecoration = "bold", halign = "center"),
    header_grey  = openxlsx::createStyle(textDecoration = "bold", halign = "center", fgFill = "#e0e0e0"),
    center       = openxlsx::createStyle(halign = "center"),
    center_dec   = openxlsx::createStyle(halign = "center", numFmt = "0.00"),
    center_pct   = openxlsx::createStyle(halign = "center", numFmt = "0%"),
    bold         = openxlsx::createStyle(textDecoration = "bold"),
    grey_fill    = openxlsx::createStyle(bgFill = "#e0e0e0")
  )


  wb <- oxl_create_workbook()


  rotation <- pca_analysis_object[["parameters"]][["rotation"]]


  variance_explained <- pca_analysis_object[["variance_explained"]]


  .pca_append_variance_explained(wb, variance_explained = variance_explained, styles = styles)


  purrr::iwalk(
    pca_analysis_object[["pca_tables"]],
    ~ .pca_append_analysis(
      wb, pca_table = .x,
      sheet_name = .y,
      styles = styles,
      footer = glue::glue("Rotation: {rotation}"),
      clean_max = clean_max
    )
  )


  file_location <- glue::glue("{where}/{file_name}.xlsx")


  openxlsx::saveWorkbook(wb, file_location, overwrite = TRUE)


  if(return_location){
    return(file_location)
  }

}


#' @keywords internal
.pca_append_analysis <- function(wb, pca_table, sheet_name, styles, footer = NULL, clean_max = .25, row_start = 4, col_start = 2){

  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)

  col_fa <- pca_table %>% dplyr::select(tidyselect::starts_with("F", ignore.case = FALSE)) %>% names()

  pca_table[, col_fa][abs(pca_table[, col_fa]) <= clean_max] <- NA

  col_fa <- which(names(pca_table) %in% col_fa) + col_start -1
  col_max <- col_fa %>% head(1) - 1

  names(pca_table) <- pca_table %>% names() %>% gsub("_", " ", .) %>% stringr::str_to_title()

  rows_all <- seq(row_start, row_start + nrow(pca_table))


  openxlsx::writeData(
    wb, sheet_name,
    x = glue::glue("FA {sheet_name}"),
    startRow = row_start - 2,
    startCol = col_start,
    colNames = FALSE,
    borders = "none"
  )


  if(!is.null(footer)){
    openxlsx::writeData(
      wb, sheet_name,
      x = footer,
      startRow = rows_all %>% tail(1) + 1,
      startCol = col_start,
      colNames = FALSE,
      borders = "none"
    )
  }


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$title,
    rows = row_start - 2,
    cols = col_start,
    stack = T
  )


  openxlsx::writeData(
    wb, sheet_name,
    x = pca_table,
    startRow = row_start,
    startCol = col_start,
    colNames = TRUE,
    borders = "all",
    headerStyle = styles$header
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = row_start,
    col_start = col_start, col_end = col_start + ncol(pca_table) - 1,
    borderStyle = "medium"
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = row_start + nrow(pca_table),
    col_start = col_start, col_end = col_start + ncol(pca_table) - 1,
    borderStyle = "medium"
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$header_grey,
    cols = seq(col_start, col_fa %>% tail(1)),
    rows = row_start,
    gridExpand = T,
    stack = T
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$center,
    rows = rows_all,
    cols = c(col_start, col_start + 1, col_start + 3),
    gridExpand = T,
    stack = T
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$center_dec,
    rows = rows_all,
    cols = c(col_max, col_fa),
    gridExpand = T,
    stack = T
  )


  openxlsx::conditionalFormatting(
    wb, sheet_name,
    cols = seq(col_start, col_fa %>% tail(1)),
    rows = rows_all,
    rule = glue::glue('=ISEVEN(${num2let(col_start + 3)}{row_start})'),
    style = styles$grey_fill
  )


  openxlsx::conditionalFormatting(
    wb, sheet_name,
    cols = col_fa,
    rows = rows_all,
    rule = glue::glue('${num2let(col_start + 4)}{row_start} == {num2let(col_fa %>% head(1))}{row_start}'),
    style = styles$bold
  )


  openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 2)
  openxlsx::setColWidths(wb, sheet_name, cols = col_start, widths = 10)
  openxlsx::setColWidths(wb, sheet_name, cols = col_start + 1, widths = 15)
  openxlsx::setColWidths(wb, sheet_name, cols = col_start + 2, widths = 70)
  openxlsx::setColWidths(wb, sheet_name, cols = c(col_start + 3, col_max, col_fa), widths = 5)

  openxlsx::freezePane(wb, sheet_name, firstActiveRow = row_start + 1, firstActiveCol = "G")
}


#' @keywords internal
.pca_append_variance_explained <- function(wb, variance_explained, styles, sheet_name = "variance_explained", row_start = 4, col_start = 2){
  rows_all <- seq(row_start, row_start + nrow(variance_explained))

  names(variance_explained) <- names(variance_explained) %>% gsub("_", " ", .) %>% stringr::str_to_title()

  sheet_name <- "variance_explained"

  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)


  openxlsx::writeData(
    wb, sheet_name,
    x = "Variance Explained",
    startRow = row_start - 2,
    startCol = col_start,
    colNames = FALSE,
    borders = "none"
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$title,
    rows = row_start - 2,
    cols = col_start,
    stack = T
  )


  openxlsx::writeData(
    wb, sheet_name,
    x = variance_explained,
    startRow = row_start,
    startCol = col_start,
    colNames = TRUE,
    borders = "all",
    headerStyle = styles$header
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = row_start,
    col_start = col_start, col_end = col_start + ncol(variance_explained) - 1,
    borderStyle = "medium"
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = row_start + nrow(variance_explained),
    col_start = col_start, col_end = col_start + ncol(variance_explained) - 1,
    borderStyle = "medium"
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$header_grey,
    cols = seq(col_start, col_start + 2),
    rows = row_start,
    gridExpand = T,
    stack = T
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$center,
    rows = rows_all,
    cols = col_start,
    gridExpand = T,
    stack = T
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$center_pct,
    rows = rows_all,
    cols = c(col_start + 1, col_start + 2),
    gridExpand = T,
    stack = T
  )


  openxlsx::setColWidths(wb, sheet_name, cols = seq(col_start, col_start + 2), widths = 15)

  openxlsx::freezePane(wb, sheet_name, firstActiveRow = row_start + 1, firstActiveCol = "C")
}
