#' bn_get_unreachable
#' @description Helper function to get unreachable nodes
#' @export
bn_get_unreachable <- function(bn, dv) {

  reachable_nodes <- bn_get_reachable(bn, dv)

  dplyr::setdiff(bnlearn::nodes(bn), reachable_nodes)

}


#' bn_get_reachable
#' @description Helper function to get reachable nodes
#' @export
bn_get_reachable <- function(bn, dv) {

  g <- bnlearn::as.igraph(bn)

  igraph::V(g)$name[igraph::subcomponent(g, v = dv, mode = "in")]

}
