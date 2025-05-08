#' bn_to_netD3_prep
#' @description bn_to_netD3_prep
#' @export
bn_to_netD3_prep <- function(
    bn,
    dictionary = NULL,
    node_label_type = c("both", "variable", "label"),
    n_groups = NULL,
    edge_multiplier = 10,
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


  viz_prep <- ig_graph %>%
    networkD3::igraph_to_networkD3(group = ivs_group_assignment)


  vid_node_id <- ig_graph %>%
    igraph::V() %>%
    as.matrix %>%
    data.frame %>%
    tibble::rownames_to_column("node") %>%
    mutate(
      id = . - 1
    ) %>%
    select(node, id)


  viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
    left_join(
      vid_node_id,
      by = join_by(name == node)
    ) %>%
    mutate(
      size = node_size
    )


  viz_prep[["links"]] <- viz_prep[["links"]] %>%
    left_join(
      viz_prep[["nodes"]] %>% select(id, name),
      by = join_by(source == id)
    ) %>%
    rename(from = name) %>%
    left_join(
      viz_prep[["nodes"]] %>% select(id, name),
      by = join_by(target == id)
    ) %>%
    rename(to = name) %>%
    rowwise() %>%
    mutate(
      edge_id = paste(sort(c(from, to)),collapse = "")
    ) %>%
    ungroup() %>%
    left_join(
      edges_ivs %>% select(edge_id, mi),
      by = join_by(edge_id)
    ) %>%
    select(source, target, mi) %>%
    mutate(
      mi = mi * edge_multiplier
    ) %>%
    as.data.frame()



  if(!is.null(dictionary)){

    viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
      left_join(
        dictionary %>% select(var, label),
        by = join_by(name == var)
      )

    if(node_label_type == "both"){

      viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
        mutate(
          name = glue("{name} - {label}")
        )

    }else if(node_label_type == "label"){

      viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
        mutate(
          name = label
        )
    }

    viz_prep[["nodes"]] <- viz_prep[["nodes"]] %>%
      select(-label) %>%
      as.data.frame()

  }


  return(viz_prep)
}

