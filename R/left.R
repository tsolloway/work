#' left
#'
#' @description
#' Returns the leftmost `n` characters of a string (or each element of a character vector).
#'
#' @param x Character vector. Each element will be truncated to its leftmost `n` characters.
#' @param n Integer scalar specifying how many characters to return from the left.
#'
#' @return
#' A character vector of the same length as `x`, where each element is truncated to `n` leftmost characters.
#'
#' @examples
#' left("abcdef", 3)
#' left(c("apple", "banana", "cherry"), 2)
#'
#' @export
left <- function(x, n = 1) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.")
  }
  if (!is.numeric(n) || length(n) != 1 || n < 0) {
    stop("`n` must be a non-negative integer scalar.")
  }

  substr(x, 1, n)
}
