#' read_xl
#' @description reads in excel files
#' @param path Path to the xls/xlsx file.
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Ignored if the sheet is specified via range. If neither argument specifies the sheet, defaults to the first sheet.
#' @param range A cell range to read from, as described in cell-specification. Includes typical Excel ranges like "B3:D87", possibly including the sheet name like "Budget!B2:G14", and more. Interpreted strictly, even if the range forces the inclusion of leading or trailing empty rows or columns. Takes precedence over skip, n_max and sheet.
#' @param col_names TRUE to use the first row as column names, FALSE to get default names, or a character vector giving a name for each column. If user provides col_types as a vector, col_names can have one entry per column, i.e. have the same length as col_types, or one entry per unskipped column.
#' @param col_types Either NULL to guess all from the spreadsheet or a character vector containing one entry per column from these options: "skip", "guess", "logical", "numeric", "date", "text" or "list". If exactly one col_type is specified, it will be recycled. The content of a cell in a skipped column is never read and that column will not appear in the data frame output. A list cell loads a column as a list of length 1 vectors, which are typed using the type guessing logic from col_types = NULL, but on a cell-by-cell basis.
#' @param clean_col_names logical on whether to clean the column names.
#' @export
read_xl <- function(
    path, sheet = NULL,
    clean_col_names = TRUE,
    range = NULL,
    col_names = TRUE,
    col_types = NULL){

  df <- readxl::read_excel(
    path = path,
    sheet = sheet,
    range = range,
    col_names = col_names,
    col_types = col_types
  ) %>%
    tibble::as_tibble() %>%
    suppressWarnings()

  if(clean_col_names){
    df <- df %>% names_clean()
  }

  df
}
