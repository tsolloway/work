#' left
#' @description Returns left n characters of single string
#' @param x Character / string of single length
#' @param n Integer of how many n characters to return
#' @export
left <- function(x, n = 1){
  
  if( !is.character(x) && length(x) == 1 ){
    stop("left input needs to be a character input with a length of 1")
  }
  
  substr(x, 1, n)
}