#' remove_na
#'
#' @description Removes all `NA` values from a vector.
#'
#' @param x A vector.
#'
#' @return A vector with `NA` values removed.
#'
#' @examples
#' remove_na(c(1, NA, 3, NA, 5))
#' remove_na(c("a", NA, "b"))
#'
#' @export
remove_na <- function(x) {
  x[!is.na(x)]
}

#' remove_empty
#'
#' @description Removes all `NA`, `NULL`, or empty string (`""`) values from a vector.
#'
#' @param x A vector.
#'
#' @return A vector with empty values removed.
#'
#' @examples
#' remove_empty(c(1, NA, 2, NULL, 3, ""))
#' remove_empty(c("a", "", "b", NA, "c"))
#'
#' @export
remove_empty <- function(x) {
  if (is.null(x)) return(x)
  x[!is.na(x) & !(x == "")]
}
