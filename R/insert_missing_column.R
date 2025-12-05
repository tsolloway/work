#' insert_missing_column
#'
#' @description Add columns to a data frame if they do not already exist.
#' Missing columns are filled with NA.
#'
#' @param .data A data frame or tibble.
#' @param ... Unquoted column names to ensure exist in `.data`.
#' @return Data frame with missing columns added as NA
#' @examples
#' iris %>% insert_missing_column(Sepal.Length, mom, boo) %>% head()
#' @export
insert_missing_column <- function(.data, ...){
  x <- rlang::enquos(...)

  old_columns <- purrr::map_chr(x, rlang::as_name)
  missing_columns <- setdiff(old_columns, names(.data))

  if (length(missing_columns) > 0) {
    for (col in missing_columns) .data[[col]] <- NA
  }

  .data
}
