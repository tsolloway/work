#' Prepare Community-Level Bayesian Network for visNetwork
#'
#' Aggregates a Bayesian network's nodes and edges into communities,
#' producing node and edge data suitable for `visNetwork` visualization.
#'
#' @param attribute_viz_prep A list containing `nodes` and `edges` from
#'   `bn_to_netviz_prep()`.
#' @param community_edge_by One of `"sum"` or `"mean"`; how to aggregate
#'   mutual information (`mi`) for edges between communities.
#'
#' @return A list with:
#' \describe{
#'   \item{nodes}{Community-level nodes with `id`, `label`, and `value`.}
#'   \item{edges}{Community-level edges with aggregated `mi`, `value`, and `index`.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' library(bnlearn)
#' library(visNetwork)
#' data(learning.test)
#' bn <- hc(learning.test)
#'
#' attribute_viz <- bn_to_netviz_prep(obj = bn, df = learning.test)
#' community_viz <- bn_to_netviz_prep_for_communities(
#'   attribute_viz_prep = attribute_viz$attribute_viz_prep,
#'   community_edge_by = "sum"
#' )
#' visNetwork(community_viz$nodes, community_viz$edges)
#' }
bn_to_netviz_prep_for_communities <- function(
    attribute_viz_prep,
    community_edge_by = c("sum", "mean")
){

  community_edge_by <- match.arg(community_edge_by)


  # --- create community nodes ---
  community_nodes <- attribute_viz_prep[["nodes"]] %>%
    dplyr::mutate(
      id = community_name,
      label = community_name,
      value = 1
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(id) %>%
    dplyr::select(id, label, group, community_name, color, value)


  # --- create community edges ---
  community_edges <- attribute_viz_prep[["edges"]] %>%
    dplyr::left_join(
      attribute_viz_prep[["nodes"]] %>% dplyr::select(id, community_name) %>% dplyr::rename(from_community = community_name),
      by = dplyr::join_by(from == id)
    ) %>%
    dplyr::left_join(
      attribute_viz_prep[["nodes"]] %>% dplyr::select(id, community_name) %>% dplyr::rename(to_community = community_name),
      by = dplyr::join_by(to == id)
    ) %>%
    dplyr::filter(from_community != to_community) %>%
    dplyr::mutate(
      from = from_community,
      to = to_community
    ) %>%
    dplyr::select(from, to, mi) %>%
    dplyr::arrange(from, to)



  # --- aggregate edges by community ---
  community_edges <- community_edges %>%
    dplyr::group_by(from, to) %>%
    dplyr::summarise(
      mi = if (community_edge_by == "sum") sum(mi) else mean(mi),
      .groups = "drop"
    )


  # --- normalize MI for visualization ---
  community_edges <- community_edges %>%
    dplyr::mutate(
      value = mi / mean(abs(mi)),
      index = value * 100
    ) %>%
    as.data.frame()


  # --- return result ---
  list(
    "nodes" = community_nodes,
    "edges" = community_edges
  )

}
