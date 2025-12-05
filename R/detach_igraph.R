#' Safely detach igraph-related packages
#'
#' @description
#' Detaches a fixed set of packages commonly used with igraph and Bayesian networks:
#' `"gRain"`, `"gRbase"`, `"highcharter"`, and `"igraph"`.
#' Only packages that are currently loaded will be detached. Safe to call in
#' functions or scripts without throwing errors if a package is not attached.
#'
#' @export
detach_igraph <- function() {
  work::detach_pkg(c("gRain", "gRbase", "highcharter"))
  work::detach_pkg("igraph")
}
