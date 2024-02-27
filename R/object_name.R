#' object_name
#' @description object_name
#' @export
object_name <- function(x){

  x_expression <- function(x) {
    getAST <- function(ee) purrr::map_if(as.list(ee), is.call, getAST)

    sc <- sys.calls()
    ASTs <- purrr::map( as.list(sc), getAST ) %>%
      purrr::keep( ~identical(.[[1]], quote(`%>%`)) )  # Match first element to %>%

    if( length(ASTs) == 0 ) return( enexpr(x) )        # Not in a pipe
    dplyr::last( ASTs )[[2]]    # Second element is the left-hand side
  }

  y <- x_expression(x)

  if( y == "x" ){
    y <- deparse(substitute(x))
  }

  y %>% as.character()
}



object_name2 <-  function(x){
  y <- work::object_name(x)

  if(y == "x")y  <- deparse(substitute(x))
  # if(z=="y")z = work::object_name(y)
  y
}



