#' if_na_return
#' @description Returns something if NA. Default to FALSe
#' @param x vector
#' @param return_what what to return
#' @export
if_na_return <- function(x, return_what = FALSE){
  x %>% ifelse(is.na(.), return_what, .)
}