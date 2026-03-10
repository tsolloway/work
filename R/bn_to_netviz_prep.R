#' Prepare Bayesian Network for visNetwork Visualization
#'
#' Converts a fitted `bnlearn` Bayesian network into node and edge data
#' compatible with `visNetwork`, optionally grouping nodes into detected communities.
#'
#' @param obj A fitted Bayesian network (`bn` object) or a list containing
#'   both `bn` and `arcs`.
#' @param df Optional data frame used for scoring or arc strength computation.
#' @param dictionary Optional lookup table with columns `var` and `label` for node relabeling.
#' @param remove_nodes Character vector of node names to exclude from the network.
#' @param node_label_type One of `"both"`, `"variable"`, or `"label"`; controls how node labels appear.
#' @param manual_groups Optional data frame or list specifying manual node group assignments
#'   (`id`, `group`, `community_name`, `color`).
#' @param n_groups Optional integer specifying the number of communities to cut into.
#' @param node_size Numeric value controlling node size in the visualization.
#' @param return_community Logical; if `TRUE`, also returns a community-level summary graph.
#' @param community_edge_by One of `"sum"` or `"mean"`; how to aggregate inter-community edges.
#' @param tool_tip_edge_prefix Optional character string to prepend to edge tooltips (e.g., "MI =").
#' @param on_exit_detach_igraph Logical; if `TRUE`, detaches igraph-related packages on exit
#'
#' @return A list with:
#' \describe{
#'   \item{attribute_viz_prep}{Node and edge data for visNetwork visualization.}
#'   \item{community_viz_prep}{Community-level summary graph (if `return_community = TRUE`).}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bn <- bnlearn::hc(learning.test)
#' prep <- bn_to_netviz_prep(obj = bn, df = learning.test)
#' visNetwork::visNetwork(
#'   prep$attribute_viz_prep$nodes,
#'   prep$attribute_viz_prep$edges
#' )
#' }
bn_to_netviz_prep <- function(
    obj,
    df = NULL,
    dictionary = NULL,
    remove_nodes = NULL,
    node_label_type = c("both", "variable", "label"),
    manual_groups = NULL,
    n_groups = NULL,
    node_size = 1,
    return_community = TRUE,
    community_edge_by = c("sum", "mean"),
    tool_tip_edge_prefix = NULL,
    on_exit_detach_igraph = FALSE
){

  # df = NULL
  # dictionary = NULL
  # remove_nodes = NULL
  # node_label_type = "both"
  # manual_groups = NULL
  # n_groups = NULL
  # node_size = 1
  # return_community = TRUE
  # community_edge_by = "sum"
  # tool_tip_edge_prefix = NULL
  # on_exit_detach_igraph = TRUE


  community_edge_by <- match.arg(community_edge_by)
  node_label_type <- match.arg(node_label_type)

  dictionary <- work::dictionary_from_named_object(dictionary)

  x_edges <- NULL



  if("meta" %in% names(obj)){

    # bn_tan() stores $arcs$ivs; bn_engine / bn_engine_unsupervised do not
    if (!is.null(obj[["arcs"]])) {
      x_edges <- obj[["arcs"]][["ivs"]]
    }

    obj <- obj[["bn"]]
  }



  if(!"bn" %in% class(obj)){
    stop("Can't find the bn object")
  }



  # --- compute or use existing arcs ---
  if (is.null(x_edges)) {

    if (is.null(df)) {
      stop(
        "bn_to_netviz_prep() requires 'df' when the input object doesn't contain pre-computed arcs.\n",
        "Either pass df explicitly, or use the pre-computed $viz_prep from the engine result.",
        call. = FALSE
      )
    }

    x_edges <- obj %>% work::bn_arc_chisq(df) %>% as.data.frame()

    if (!is.null(remove_nodes)) {
      x_edges <- x_edges %>%
        dplyr::filter(!from %in% remove_nodes & !to %in% remove_nodes) %>%
        as.data.frame()
    }
  }


  # --- normalize mutual information for edge weights ---
  x_edges <- x_edges %>%
    dplyr::mutate(
      value = mi / mean(abs(mi)),
      index = value * 100
    ) %>%
    as.data.frame()


  # --- create igraph with edge weights ---
  x_graph <- x_edges %>%
    dplyr::select(from, to) %>%
    as.matrix() %>%
    igraph::graph_from_edgelist(directed = FALSE)


  igraph::E(x_graph)$weight <- x_edges$value


  # --- community detection ---
  x_clusters <- x_graph %>%
    igraph::cluster_fast_greedy(weights = igraph::E(x_graph)$weight)

  if (!is.null(n_groups)) {
    group_assignment <- x_clusters %>% igraph::cut_at(n_groups)
  } else {
    group_assignment <- x_clusters %>% igraph::membership()
  }

  # fully connected networks return unnamed membership — use graph node names
  if (is.null(names(group_assignment)) || length(names(group_assignment)) == 0) {
    names(group_assignment) <- igraph::V(x_graph)$name
  }

  group_assignment <- data.frame(
    id = group_assignment %>% names(),
    group = group_assignment %>% as.numeric()
  )

  # --- convert to visNetwork format ---
  viz_prep <- visNetwork::toVisNetworkData(x_graph)

  # --- apply community colors ---
  group_colors <- group_assignment[["group"]] %>% max() %>% work::bn_community_color()


  viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
    dplyr::left_join(group_assignment, by = dplyr::join_by(id)) %>%
    dplyr::mutate(
      group_id = group,
      value = node_size
    ) %>%
    dplyr::left_join(group_colors, by = dplyr::join_by(group_id == group)) %>%
    dplyr::select(-group_id) %>%
    dplyr::arrange(id)


  if(!is.null(tool_tip_edge_prefix)){
    x_edges <- x_edges %>%
      dplyr::mutate(
        title = glue("{tool_tip_edge_prefix} {round(mi, 2)}") %>% stringr::str_squish()
      )
  }else{
    x_edges <- x_edges %>%
      dplyr::mutate(
        title = mi %>% round(2) %>% stringr::str_squish()
      )
  }


  viz_prep[["edges"]] <- x_edges %>%
    dplyr::arrange(from, to) %>%
    as.data.frame()


  # --- apply dictionary labels ---
  if (!is.null(dictionary) && node_label_type != "variable") {

    if (all(c("var", "label") %in% names(dictionary))) {

      viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
        dplyr::select(-label) %>%
        dplyr::left_join(
          dictionary %>% dplyr::select(var, label),
          by = dplyr::join_by(id == var)
        ) %>%
        dplyr::mutate(
          label = dplyr::case_when(is.na(label) ~ "", .default = label) %>%
            stringr::str_squish()
        )

      if (node_label_type == "both") {
        viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
          dplyr::mutate(
            label = glue::glue("{id} - {label}") %>%
              gsub(" - NA", "", .) %>%
              stringr::str_squish()
          )
      }
    } else {
      warning("Dictionary must include columns 'var' and 'label'. Skipping relabeling.")
    }
  }



  # --- handle manual groups ---
  if (!is.null(manual_groups)) {


    # If passed as a list with a "nodes" element, extract the data frame
    if (is.list(manual_groups) && "nodes" %in% names(manual_groups)) {
      manual_groups <- manual_groups[["nodes"]]
    }


    # Ensure it is a data frame
    if (!is.data.frame(manual_groups)) {
      stop("manual_groups must be a data frame.")
    }


    # Compute fallback group number safely
    next_group <- suppressWarnings(max(manual_groups[["group"]], na.rm = TRUE))
    if (!is.finite(next_group)) next_group <- 0
    next_group <- next_group + 1


    # Ensure community_name column exists before join
    if (!"community_name" %in% names(viz_prep[["nodes"]])) {
      viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
        dplyr::mutate(community_name = NA_character_)
    }


    # Prepare manual columns with clear suffixes
    manual_groups <- manual_groups %>%
      dplyr::rename(
        group_manual = dplyr::any_of("group"),
        color_manual = dplyr::any_of("color"),
        community_name_manual = dplyr::any_of("community_name")
      )


    # Join and safely coalesce
    viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
      dplyr::left_join(
        manual_groups %>%
          dplyr::select(dplyr::any_of(c("id", "group_manual", "color_manual", "community_name_manual"))),
        by = dplyr::join_by(id)
      ) %>%
      dplyr::mutate(
        group = dplyr::coalesce(group_manual, next_group),
        color = dplyr::coalesce(color_manual, "#FF0000"),
        community_name = dplyr::coalesce(community_name_manual, glue::glue("Group {next_group}"))
      ) %>%
      dplyr::select(id, value, label, group, color, community_name)

  }



  # --- ensure community_name exists even if manual_groups is NULL ---
  if (!"community_name" %in% names(viz_prep[["nodes"]])) {
    viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
      dplyr::mutate(
        community_name = glue::glue("Group {group}")
      )
  }


  # --- ensure columns always in same order ---
  viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
    dplyr::select(id, label, community_name, group, color, value)



  # --- optionally compute community-level summary ---
  if (return_community) {

    community_viz_prep <- work::bn_to_netviz_prep_for_communities(
      attribute_viz_prep = viz_prep,
      community_edge_by = community_edge_by
    )

    result <- list(
      attribute_viz_prep = viz_prep,
      community_viz_prep = community_viz_prep
    )

  } else {
    result <- viz_prep
  }



  # --- cleanup ---
  if(on_exit_detach_igraph) work::detach_igraph()


  return(result)
}
