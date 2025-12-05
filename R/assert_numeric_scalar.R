#' Assert that an argument is a single numeric scalar (or NULL)
#'
#' @description
#' Validates that a supplied value is either `NULL` or a single numeric scalar
#' (length 1, not `NA`). Stops with an informative message if the condition is not met.
#'
#' @param x Object to validate. Can be `NULL` or a single numeric scalar.
#' @param arg_name Optional character; name of the argument being checked for
#'   clearer error messaging.
#'
#' @return Invisibly returns `TRUE` if valid.
#'
#' @examples
#' assert_numeric_scalar(5)
#' assert_numeric_scalar(NULL)  # Allowed
#'
#' \dontrun{
#' assert_numeric_scalar(c(1, 2))  # Error
#' assert_numeric_scalar("a")      # Error
#' }
#'
#' @export
assert_numeric_scalar <- function(x, arg_name = deparse(substitute(x))) {
  # Allow NULL explicitly
  if (is.null(x)) {
    return(invisible(TRUE))
  }

  # Strict numeric validation otherwise
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop(glue::glue("Argument '{arg_name}' must be NULL or a single non-NA numeric value."))
  }

  invisible(TRUE)
}
