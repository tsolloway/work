#' make_arcs
#' @description make_arcs
#' @export
make_arcs <- function(
  x,
  y = NULL,
  bidirectional = TRUE
){

  if(is.null(y)){
    y <- x
  }


  arcs <- expand.grid(x, y, stringsAsFactors = FALSE)


  if(bidirectional){

    arcs <- arcs %>%
      bind_rows(
        expand.grid(y, x,  stringsAsFactors = FALSE)
      )
  }


  arcs <- arcs %>%
    setNames(c("from", "to")) %>%
    distinct()


  return(arcs)
}
