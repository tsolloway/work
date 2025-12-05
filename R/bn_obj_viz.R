#' Quickly visualize a bnlearn Bayesian network with visNetwork
#'
#' @description
#' Creates an interactive visualization of a `bnlearn` Bayesian network using `visNetwork`.
#' Supports bn objects or lists containing a bn object.
#'
#' @param bn Object to visualize. Can be a `bnlearn::bn` object or a list containing one.
#' @param recursive Logical; if TRUE, searches nested lists for a bn object. Default TRUE.
#' @param community Optional vector of node groups (same length/order as `bnlearn::nodes(bn)`) to color nodes by community.
#'
#' @return A `visNetwork` object.
#' @export
bn_obj_viz <- function(bn, recursive = TRUE, community = NULL) {

  # Validate/extract bn object
  bn <- check_or_extract_bn(bn, recursive = recursive)

  # Nodes
  node_ids <- bnlearn::nodes(bn)
  nodes <- data.frame(
    id = node_ids,
    label = node_ids,
    stringsAsFactors = FALSE
  )

  if (!is.null(community)) {
    if (length(community) != length(node_ids)) {
      stop("Length of community vector must match the number of nodes in the bn object.")
    }
    nodes$group <- community
  }

  # Edges
  edges <- data.frame(as.data.frame(bnlearn::arcs(bn)), stringsAsFactors = FALSE)
  colnames(edges) <- c("from", "to")

  # visNetwork plot
  vis_net <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visEdges(arrows = "to") %>%
    visNetwork::visOptions(highlightNearest = TRUE)


  vis_net
}
