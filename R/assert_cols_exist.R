#' Assert that columns exist in a data frame
#'
#' @description
#' Checks whether all specified columns are present in a given data frame.
#' Throws an error with an informative message if any are missing.
#'
#' @param df A data frame to check.
#' @param cols Character vector of column names expected to be in `df`.
#' @param context Optional character string providing context in the error message.
#'
#' @return Invisibly returns `TRUE` if all columns are present.
#'
#' @examples
#' df <- data.frame(a = 1, b = 2)
#' assert_cols_exist(df, c("a", "b"))
#'
#' \dontrun{
#' assert_cols_exist(df, c("a", "c"))  # Throws error
#' }
#'
#' @export
assert_cols_exist <- function(df, cols, context = "data frame") {
  if (!is.data.frame(df)) {
    stop(glue::glue("Expected a data frame for {context}, got {class(df)[1]} instead."))
  }

  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(glue::glue(
      "The following column(s) are missing from {context}: {paste(missing, collapse = ', ')}"
    ))
  }

  invisible(TRUE)
}
