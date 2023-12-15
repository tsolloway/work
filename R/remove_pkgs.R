#' remove_pkgs
#' @description Remvoes packages
#' @param x Character vector. If x is NULL, remove all non-essential packages.
#' @export
remove_pkgs <- function(x = NULL){

  if( is.null(x) ) x <- installed.packages( priority = "NA" )[,1]

  remove.packages(x)
}
