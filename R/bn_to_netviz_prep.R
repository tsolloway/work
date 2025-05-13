#' bn_to_netviz_prep
#' @description bn_to_netviz_prep
#' @export
bn_to_netviz_prep <- function(
    bn,
    dictionary = NULL,
    node_label_type = c("both", "variable", "label"),
    n_groups = NULL,
    node_size = 1
  ){


  require(igraph)

  node_label_type <- match.arg(node_label_type)

  edges_ivs <- bn %>%
    pluck("arcs") %>%
    pluck("ivs") %>%
    rowwise() %>%
    mutate(
      edge_id = paste(sort(c(from, to)),collapse = "")
    ) %>%
    ungroup()


  ig_graph <- edges_ivs %>%
    select(c(from, to)) %>%
    as.matrix() %>%
    graph_from_edgelist(directed = FALSE)


  ig_graph_cluster <- ig_graph %>%
    cluster_fast_greedy(
      weights = edges_ivs %>% select(mi) %>% unlist()
    )


  if (!is.null(n_groups)) {
    ivs_group_assignment <- ig_graph_cluster %>% cut_at(n_groups)
  } else {
    ivs_group_assignment <- ig_graph_cluster %>% membership()
  }


  ivs_group_assignment <- ivs_group_assignment %>%
    data.frame("group" = .) %>%
    tibble::rownames_to_column("id")


  viz_prep <- visNetwork::toVisNetworkData(ig_graph)


  pal <- ivs_group_assignment[["group"]] %>% max() %>% bn_community_color()


  viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
    left_join(
      ivs_group_assignment,
      by = join_by(id)
    ) %>%
    mutate(
      group_id = group %>% as.numeric(),
      value = node_size
    ) %>%
    left_join(
      pal,
      by = join_by(group_id == group)
    ) %>%
    select(-group_id)


  viz_prep[["edges"]] <- viz_prep[["edges"]] %>%
    rowwise() %>%
    mutate(
      edge_id = paste(sort(c(from, to)),collapse = "")
    ) %>%
    ungroup() %>%
    left_join(
      edges_ivs %>% select(edge_id, mi),
      by = join_by(edge_id)
    ) %>%
    rename(value = mi)



  if(!is.null(dictionary) && node_label_type != "variable"){

    viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
      select(-label) %>%
      left_join(
        dictionary %>% select(var, label),
        by = join_by(id == var)
      )

    if(node_label_type == "both"){

      viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
        mutate(
          label = glue("{id} - {label}")
        )
    }

  }


  return(viz_prep)
}
