#' top2
#' @description top2
#' @export
top2 <- function(x){
  x %>% .[!is.na(.)] %>% unique() %>% sort() %>% tail(2)
}


#' bottom2
#' @description bottom2
#' @export
bottom2 <- function(x){
  x %>% .[!is.na(.)] %>% unique() %>% sort() %>% head(2)
}
