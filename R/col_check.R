#' col_check
#'
#' @description Checks that all specified column names exist in a data frame.
#' @param df Data frame to check.
#' @param check_names Character vector of column names to check.
#' @param hard_stop Logical; if TRUE, stops with an error when columns are missing.
#' @return TRUE if all columns exist; FALSE (or stops) otherwise.
#' @export
col_check <- function(df, check_names, hard_stop = TRUE) {
  missing_names <- setdiff(check_names, colnames(df))

  if (length(missing_names) > 0) {
    msg <- paste("Could not find columns:", paste(missing_names, collapse = ", "))
    if (hard_stop) stop(msg)
    return(FALSE)
  }

  TRUE
}
