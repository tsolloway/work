#' other_col_names
#'
#' @description Returns the names of all columns in a data frame except those specified.
#' Works nicely with the tidyverse pipe (`%>%`) and quasiquotation.
#'
#' @param .data A data frame or tibble.
#' @param ... Column names to exclude (unquoted).
#'
#' @return A character vector of column names not specified in `...`.
#'
#' @examples
#' # Exclude 'Species' and 'Sepal.Length' from iris
#' iris %>% other_col_names(Species, Sepal.Length)
#'
#' # Can also use with a single column
#' iris %>% other_col_names(Petal.Width)
#'
#' @export
other_col_names <- function(.data, ...) {
  # Capture columns to exclude
  excluded <- rlang::enquos(...) %>% purrr::map(rlang::quo_get_expr) %>% unlist()

  # Get all column names
  cols <- colnames(.data)

  # Return those not in excluded
  cols[!cols %in% excluded]
}
