#' if_na_return
#' @description Replaces NA values in a vector with a specified value.
#' @param x Vector
#' @param return_what Value to return in place of NA (default = FALSE)
#' @return Vector with NAs replaced
#' @export
if_na_return <- function(x, return_what = FALSE) {
  ifelse(is.na(x), return_what, x)
}
