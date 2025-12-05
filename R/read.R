#' read
#'
#' @description Reads a file into R. Supports `.csv`, `.xls`, `.xlsx`, and `.sav` files.
#' For `.sav` files, returns both the data frame and a dictionary of value labels.
#'
#' @param path Path to the file.
#' @param sheet Sheet to read (for Excel). Either a string (sheet name) or integer (sheet index). Ignored if `range` is provided.
#' @param range Cell range to read from Excel, e.g., `"B3:D87"` or `"Budget!B2:G14"`. Takes precedence over `sheet`.
#' @param col_names Logical or character vector. Use first row as column names (`TRUE`), default names (`FALSE`), or custom names.
#' @param col_types NULL or character vector specifying column types for Excel (`"skip"`, `"guess"`, `"logical"`, `"numeric"`, `"date"`, `"text"`, `"list"`).
#' @param clean_col_names Logical. If `TRUE`, column names are cleaned using `names_clean()`.
#' @param hard_stop Logical. If `TRUE`, stops immediately on unsupported file types.
#' @param always_list Logical. If `TRUE`, output is always a list with elements `df` and `dictionary` (Excel/CSV will have `dictionary = NULL`).
#' @param warning Logical. If `TRUE`, displays a warning for unsupported file types.
#'
#' @return For CSV/Excel: tibble or list (if `always_list = TRUE`).
#' For SAV: list with elements `df` (data frame) and `dictionary` (value labels).
#'
#' @examples
#' \dontrun{
#' # Read Excel
#' df <- read("data.xlsx", sheet = 1)
#'
#' # Read CSV
#' df <- read("data.csv")
#'
#' # Read SPSS file and get dictionary
#' output <- read("survey.sav")
#' df <- output$df
#' dictionary <- output$dictionary
#' }
#'
#' @export
read <- function(
    path,
    sheet = NULL,
    clean_col_names = TRUE,
    range = NULL,
    col_names = TRUE,
    col_types = NULL,
    hard_stop = FALSE,
    always_list = FALSE,
    warning = TRUE
) {

  # Initialize work environment
  work::start()


  # Determine file extension
  ext <- path %>%
    tools::file_ext() %>%
    tolower()


  # Check supported file types
  if (!ext %in% c("csv", "xls", "xlsx", "sav")) {
    msg <- glue::glue("File type '{ext}' not supported yet.")
    if (hard_stop) stop(msg)
    if (warning) warning(msg)
    return(NULL)
  }


  # Read file based on extension
  df <- switch(
    ext,
    csv = read.csv(file = path, stringsAsFactors = FALSE),
    xls = read_xl(
      path = path, sheet = sheet, range = range,
      col_names = col_names, col_types = col_types,
      clean_col_names = clean_col_names
    ),
    xlsx = read_xl(
      path = path, sheet = sheet, range = range,
      col_names = col_names, col_types = col_types,
      clean_col_names = clean_col_names
    ),
    sav = haven::read_sav(file = path),
    NULL
  ) %>%
    dplyr::as_tibble() %>%
    suppressWarnings()


  # Process SPSS (.sav) files
  if (ext == "sav") {
    dictionary <- df %>%
      labelled::look_for() %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        value_labels = purrr::map(value_labels, ~ paste(.x, names(.x), sep = " = ", collapse = ", ")) %>%
          unlist()
      ) %>%
      dplyr::select(-levels)

    df <- df %>% haven::zap_labels()
  }


  # Optionally clean column names
  if (clean_col_names) {
    df <- df %>% names_clean()
    if (ext == "sav") {
      dictionary <- dictionary %>%
        dplyr::mutate(variable = variable %>% names_clean())
    }
  }


  # Determine output
  if (ext == "sav") {
    output <- list(df = df, dictionary = dictionary)
  } else {
    if (always_list) {
      output <- list(df = df, dictionary = NULL)
    } else {
      output <- df
    }
  }

  return(output)
}
