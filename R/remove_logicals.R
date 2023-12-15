#' remove_na
#' @description Returns vector with no values equaling NA
#' @param x vector
#' @export
remove_na <- function(x){
  x[!is.na(x)]
}


#' remove_empty
#' @description Returns vector with no values that are empty
#' @param x vector
#' @export
remove_empty <- function(x){
  x[!is.null(x) & !is.na(x) & !x == ""]
}
