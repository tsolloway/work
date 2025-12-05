#' Assert that an argument is a positive integer
#'
#' @description
#' Checks that a given value is a positive integer scalar (length 1, whole number > 0).
#' Throws a descriptive error if not.
#'
#' @param x Object to validate.
#' @param arg_name Optional character; name of the argument for the error message.
#'
#' @return Invisibly returns `TRUE` if valid.
#'
#' @examples
#' assert_positive_integer(10)
#'
#' \dontrun{
#' assert_positive_integer(-1)   # Error
#' assert_positive_integer(2.5)  # Error
#' }
#'
#' @export
assert_positive_integer <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0 || x %% 1 != 0) {
    stop(glue::glue("Argument '{arg_name}' must be a positive integer scalar."))
  }
  invisible(TRUE)
}
