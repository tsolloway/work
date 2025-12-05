#' print_all
#'
#' @description Prints all rows of a data frame or tibble.
#' Useful for tibbles that truncate output by default.
#'
#' @param x A data frame or tibble.
#' @param n Number of rows to print (default = `Inf`, prints all rows).
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' df <- tibble::tibble(a = 1:5, b = letters[1:5])
#' print_all(df)
#'
#' @export
print_all <- function(x, n = Inf) {
  print(x, n = n)
  invisible(x)
}
