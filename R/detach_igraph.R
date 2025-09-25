#' detach_igraph
#' @description detach_igraph
#' @export
detach_igraph <- function(){
  detach_pkg("gRain")
  detach_pkg("gRbase")
  detach_pkg("highcharter")
  detach_pkg("igraph")
}

