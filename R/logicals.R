#' is_all_unique
#' @description Returns logical if all values in vector are unique
#' @param x vector
#' @export
is_all_unique <- function(x){
  (x %>% work::remove_empty() %>% unique() %>% length()) == length(x)
}


#' is_truthy
#' @description Mirror of shiny::isTruthy
#' @inheritParams shiny::isTruthy
#' @export
is_truthy <- function(x)shiny::isTruthy(x)
