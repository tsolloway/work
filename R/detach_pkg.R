#' detach_igraph
#' @description detach_igraph
#' @export
detach_pkg <- function(x){

  x <- glue("package:{x}") %>% as.character()

  if (any(grepl(x, search()))) {
    detach(x, unload = TRUE, character.only = TRUE)
  }

}
