#' read_xl
#'
#' @description Reads an Excel file (`.xls` or `.xlsx`) into a tibble.
#' Optionally cleans column names after reading.
#'
#' @param path Path to the Excel file.
#' @param sheet Sheet to read. Either a string (sheet name) or integer (sheet index). Ignored if `range` is specified. Defaults to the first sheet if not provided.
#' @param range Cell range to read, e.g. `"B3:D87"` or `"Budget!B2:G14"`. Takes precedence over `sheet`.
#' @param col_names Logical or character vector. Use first row as column names (`TRUE`), default names (`FALSE`), or custom names.
#' @param col_types NULL (guess types) or a character vector specifying column types (`"skip"`, `"guess"`, `"logical"`, `"numeric"`, `"date"`, `"text"`, `"list"`). Single values are recycled.
#' @param clean_col_names Logical. If `TRUE`, column names are cleaned using `names_clean()`.
#'
#' @return A tibble containing the Excel data.
#'
#' @examples
#' \dontrun{
#' # Read first sheet and clean column names
#' df <- read_xl("data.xlsx")
#'
#' # Read specific sheet by name
#' df <- read_xl("data.xlsx", sheet = "Budget")
#'
#' # Read a specific range
#' df <- read_xl("data.xlsx", range = "B3:D20")
#'
#' # Do not clean column names
#' df <- read_xl("data.xlsx", clean_col_names = FALSE)
#' }
#'
#' @export
read_xl <- function(
    path, sheet = NULL,
    clean_col_names = TRUE,
    range = NULL,
    col_names = TRUE,
    col_types = NULL
) {

  # Read the Excel file
  df <- readxl::read_excel(
    path = path,
    sheet = sheet,
    range = range,
    col_names = col_names,
    col_types = col_types
  ) %>%
    tibble::as_tibble() %>%
    suppressWarnings()


  # Optionally clean column names
  if (clean_col_names) {
    df <- df %>% names_clean()
  }


  df
}
