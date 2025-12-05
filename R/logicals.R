#' is_all_unique
#' @description Returns TRUE if all non-empty values in a vector are unique
#' @param x Vector to check
#' @return Logical
#' @export
is_all_unique <- function(x) {
  x_clean <- x[!is.na(x) & x != ""]
  length(unique(x_clean)) == length(x_clean)
}



#' is_truthy
#' @description Mirror of shiny::isTruthy
#' @inheritParams shiny::isTruthy
#' @return Logical indicating if `x` is truthy
#' @export
is_truthy <- function(x)shiny::isTruthy(x)




#' Check if an object is "nothing"
#'
#' @description Returns TRUE if the input is NULL, has length zero, or consists entirely of NA values.
#'
#' @param x Object to check.
#' @return Logical; TRUE if x is NULL, length zero, or all NA.
#' @examples
#' is_nothing(NULL)          # TRUE
#' is_nothing(NA)            # TRUE
#' is_nothing(1:3)           # FALSE
#' is_nothing(character(0))  # TRUE
#' @export
is_nothing <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x))
}
