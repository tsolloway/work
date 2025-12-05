
bn_finalize_network <- function(
    obj = NULL,
    df,
    dv = NULL,
    subgroups = NULL,
    dictionary = NULL,
    viz_prep = NULL,
    bn = NULL,
    previous_dv = NULL,
    # traditional_driver_engine = c("linear", "logistic"),
    # community_score_by = c("sum", "mean"),
    node_label_type = c("both", "variable", "label"),
    manual_groups = NULL,
    # node_size = NULL,
    # standardize_traditional_drivers = FALSE,
    # attribute_viz_label_var_only = TRUE,

    connections_max = 3,
    connections_multiple_boot_threshold = "auto",
    connections_multiple_boot_ratio = 1/4,
    complexity_boot_n = 100,
    complexity_boot_strength_min = 0.01,

    impact_use_parallel = TRUE,
    impact_n_boot = 1000,

    viz_include_dv = FALSE,
    vs_layout = "layout_with_fr",
    vs_height = "100vh",
    vs_width = "100%",
    viz_interactive_map_deliverable = TRUE,
    viz_community_edge_by = c("sum", "mean"),
    viz_tool_tip_edge_prefix = NULL,
    save_visuals = TRUE,
    seed = 1
){

  # traditional_driver_engine = c("linear", "logistic"),
  # community_score_by = c("sum", "mean"),
  viz_include_dv = FALSE
  viz_prep = NULL
  bn = NULL
  dv = NULL
  viz_tool_tip_edge_prefix = NULL
  previous_dv = NULL
  node_label_type = "both"
  manual_groups = NULL
  connections_max = 3
  connections_multiple_boot_threshold = "auto"
  connections_multiple_boot_ratio = 1/4
  impact_n_boot = 10
  impact_use_parallel = T
  viz_community_edge_by = "sum"


  node_label_type <- match.arg(node_label_type)
  viz_community_edge_by <- match.arg(viz_community_edge_by)

  # traditional_driver_engine <- match.arg(traditional_driver_engine)
  # community_score_by <- match.arg(community_score_by)


  results <- list()
  dictionary <- work::dictionary_from_named_object(dictionary)
  df <- df %>% as.data.frame()



  ###############################
  # Object setup
  ###############################

  if("meta" %in% names(obj)){

    #logic for bn_engine return objects
    if(obj[["meta"]][["analysis"]] == "bn_model_single"){

      if(is.null(bn)) bn <- obj[["bn"]]
      if(is.null(viz_prep)) viz_prep <- obj[["viz_prep"]]

      if(is.null(dv)) dv <- obj[["meta"]][["dv"]]
      if(is.null(previous_dv)) previous_dv <- obj[["meta"]][["dv"]]
      x_ivs <- obj[["meta"]][["ivs"]] %>% unlist() %>% setNames(NULL)

    }else{
      stop("Unknown situation being passed to the obj parameter.")
    }

  }else if(inherits(obj, "bn")){

    bn <- obj

  }else if(all(c("nodes", "edges") %in% names(obj))){

    viz_prep <- obj

  }else if("attribute_viz_prep" %in% names(obj)){

    viz_prep <- obj[["attribute_viz_prep"]]

  }else if(is.null(obj) && !is.null(bn) && !is.null(viz_prep)){

    #do nothing

  }else{
    stop("Unknown object being passed to the obj parameter.")
  }


  if(work::find_recursive(viz_prep, x_name = "attribute_viz_prep", return_logical = TRUE)){
    viz_prep <- work::find_recursive(viz_prep, x_name = "attribute_viz_prep")
  }


  if(is.null(bn)){
    stop("Cannot find a bn object.")
  }


  ###############################
  # prep work
  ###############################

  if(!is.null(viz_prep) && is.null(manual_groups)){
    manual_groups <- viz_prep[["nodes"]] %>% as.data.frame()
  }


  x_nodes <- bn %>% bnlearn::nodes()
  x_edges <- bn %>% bnlearn::arcs() %>% as.data.frame()


  if(is.null(previous_dv) && !is.null(viz_prep)){
    if(viz_prep[["nodes"]][["id"]][[1]] != dv){
      previous_dv <- viz_prep[["nodes"]][["id"]][[1]]
    }
  }


  if(!is.null(previous_dv)){
    if(dv != previous_dv){
      if((previous_dv %in% x_nodes) && !dv %in% x_nodes){
        x_nodes <- x_nodes %>% gsub(previous_dv, dv, .)
        x_edges[["from"]] <- x_edges[["from"]] %>% gsub(previous_dv, dv, .)
        x_edges[["to"]] <- x_edges[["to"]] %>% gsub(previous_dv, dv, .)
      }else{
        stop("It's not programatically clear how to insert the new DV.  Please do it outside of this wrapper")
      }
    }
  }


  if(!exists("x_ivs")) x_ivs <- x_nodes %>% setdiff(dv)


  ###############################
  # final model
  ###############################

  results[["bn"]] <- work::bn_engine(
    df = df,
    dv = dv,
    ivs = x_ivs,
    dictionary = dictionary,
    white_list = x_edges,
    connections_max = connections_max,
    connections_multiple_boot_threshold = connections_multiple_boot_threshold,
    connections_multiple_boot_ratio = connections_multiple_boot_ratio,
    complexity_boot_n = complexity_boot_n,
    complexity_boot_strength_min = complexity_boot_strength_min,
    node_label_type = node_label_type,
    manual_groups = manual_groups,
    all_ivs_connect_to_dv = viz_include_dv,
    only_white_list = TRUE,
    tool_tip_edge_prefix = viz_tool_tip_edge_prefix,
    suppress_bn_warning = TRUE,
    on_exit_detach_igraph = FALSE
  )



  results[["bn_subgroups"]] <- work::map_progress(
    subgroups,
    function(x) {
      work::bn_engine(
        df = df %>%
          dplyr::filter(.data[[x]] == 1) %>%
          droplevels() %>%
          as.data.frame(),
        dv = dv,
        ivs = x_ivs,
        dictionary = dictionary,
        white_list = x_edges,
        connections_max = 1,
        manual_groups = manual_groups,
        only_white_list = TRUE,
        suppress_bn_warning = TRUE,
        on_exit_detach_igraph = FALSE
      )[c("bn", "fit", "summary", "meta")]
    },
    .label = "Running subgroup"
  ) %>%
    setNames(subgroups)


  results[["bn_subgroups_summary"]] <- results[["bn_subgroups"]] %>%
    purrr::map(~.x[["summary"]][["model"]]) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(-accuracy)


  message("Completed model estimation")

  ###############################
  # Impacts attributes
  ###############################



  results[["bn_subgroups"]]
  obj = results$bn

  foo <- work::bn_impact_grain(
    bn_final = results,
    df = df,
    community_assignment = results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]],
    n_boot = impact_n_boot,
    use_parallel = impact_use_parallel,
    do_community = FALSE)

  bn_impact_grain_final
  bn_impact_grain

  bn_final$bn_subgroups$Potential_User %>% names

  work::bn_impact_grain(
    bn_final = results,
    df = df,
    community_assignment = results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]],
    n_boot = 1,
    # return_dv_estimate = T,
    use_parallel = FALSE,
    do_community = F
  )

  bn_impact_grain_engine
  bn_impact_grain_engine(
    obj = results$bn_subgroups$Total,
    df = df,
    iv = "q14a_10",
    ivs = NULL,
    n_boot = 1,
    return_dv_estimate = T,
    make_tibble = T,
    use_parallel = F,
    seed = 1
  )

  bn_arc_chisq

}







