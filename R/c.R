#' cq
#' @description cq
#' @examples
#' c(!!(1:4))
#' c(!!(1:4), hi ="5")
#' c(!!(1:4), hi ="5", foo = boo, "hello" = "world")
cq <- function(...){

  x <- rlang::enquos(...)

  x <- purrr::map(x, rlang::quo_get_expr) %>% unlist()

  if( purrr::map(x, ~!is.na(as.numeric(as.character(.x)))) %>% suppressWarnings() %>% unlist() %>% all() ){
    x %>% as.numeric() %>% setNames(names(x))
  }else{
    x %>% as.character() %>% setNames(names(x))
  }
}

#' cbase
#' @description base::c()
#' @export
cbase <- function(...){
  .Primitive("c")
}

