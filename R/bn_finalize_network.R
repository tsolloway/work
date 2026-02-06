
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
    node_label_type = c("both", "variable", "label"),
    manual_groups = NULL,
    impact_type = c("cp", "gr", "mi"),
    impact_n_boot = 1000,
    impact_n_querry = 1e5,

    viz_size_node_by_impact = TRUE,
    viz_include_dv = FALSE,
    viz_store = FALSE,
    model_parallel = FALSE,
    impact_parallel = TRUE,
    seed = 1
){

  # traditional_driver_engine = c("linear", "logistic"),

  viz_size_node_by_impact = TRUE
  viz_include_dv = FALSE
  viz_prep = NULL
  bn = NULL
  dv = NULL
  previous_dv = NULL
  node_label_type = "both"
  manual_groups = NULL
  impact_n_boot = 1
  impact_n_querry = 1e5
  impact_type = "cp"
  seed = 1
  model_parallel = FALSE
  impact_parallel = TRUE


  node_label_type <- match.arg(node_label_type)
  impact_type <- match.arg(impact_type)

  # traditional_driver_engine <- match.arg(traditional_driver_engine)


  results <- list()
  dictionary <- work::dictionary_from_named_object(dictionary)
  df <- df %>% as.data.frame()


  if(impact_parallel || model_parallel){
    original_future_plan <- future::plan()
    future::plan(future::multisession)
  }


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

  message("Beginning model estimation")

  results[["bn"]] <- work::bn_engine(
    df = df,
    dv = dv,
    ivs = x_ivs,
    dictionary = dictionary,
    white_list = x_edges,
    node_label_type = node_label_type,
    manual_groups = manual_groups,
    only_white_list = TRUE,
    tool_tip_edge_prefix = viz_tool_tip_edge_prefix,
    remove_dv_from_viz_prep = if(viz_include_dv) NULL else dv,
    suppress_bn_warning = TRUE,
    on_exit_detach_igraph = FALSE,
    seed = seed
  )


  results[["bn_subgroups"]] <- map_progress(
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
        on_exit_detach_igraph = FALSE,
        seed = seed
      )[c("bn", "fit", "summary", "meta")]
    },
    .label = "Running subgroup",
    .parallel = model_parallel
  ) %>%
    setNames(subgroups)


  results[["bn_subgroups_summary"]] <- results[["bn_subgroups"]] %>%
    purrr::imap(
      ~.x[["summary"]][["model"]] %>%
        dplyr::mutate(subgroup = .y) %>%
        dplyr::relocate(subgroup, .before = 1)
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(-accuracy)


  message("Completed model estimation")



  ###############################
  # Impacts attributes
  ###############################

  message("Beginning impact estimation")
  message("attribute estimation...")

  results[["impact"]] <- list()

  results[["impact"]][["attribute"]] <- work::bn_impact(
    obj = results[["bn_subgroups"]],
    df = df,
    dv = dv,
    ivs = x_ivs,
    do_community = FALSE,
    community_assignment = results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]],
    type = impact_type,
    process_subgroups = TRUE,
    n_boot = impact_n_boot,
    n_querry = impact_n_querry,
    use_parallel = impact_parallel,
    seed = seed
  )

  message("community estimation...")

  results[["impact"]][["community"]] <- work::bn_impact(
    obj = results[["bn_subgroups"]],
    df = df,
    dv = dv,
    ivs = x_ivs,
    do_community = TRUE,
    community_assignment = results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]],
    type = impact_type,
    process_subgroups = TRUE,
    n_boot = impact_n_boot,
    n_querry = impact_n_querry,
    use_parallel = impact_parallel,
    seed = seed
  )

  message("Completed impact estimation")



  ###############################
  # Edge List & Viz Prep Storage
  ###############################

  results[["edge_list"]] <- list()

  results[["edge_list"]][["all"]] <- results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["edges"]]

  results[["edge_list"]][["iv"]] <- results[["edge_list"]][["all"]] %>% dplyr::filter(from != dv & to != dv)

  results[["edge_list"]][["dv"]] <- results[["edge_list"]][["all"]] %>% dplyr::filter(from == dv | to == dv)



  if(viz_size_node_by_impact){

    attribute_nodes <- results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]]
    community_nodes <- results[["bn"]][["viz_prep"]][["community_viz_prep"]][["nodes"]]

    attribute_node_impacts <- results[["impact"]][["attribute"]] %>%
      dplyr::select(Variable, Total)
      # dplyr::mutate(Total = Total / 100)

    community_node_impacts <- results[["impact"]][["community"]] %>%
      dplyr::select(Community, Total)
      # dplyr::mutate(Total = Total / 100)

    attribute_nodes <- attribute_nodes %>%
      dplyr::left_join(attribute_node_impacts, by = dplyr::join_by(id == Variable)) %>%
      dplyr::mutate(value = Total) %>%
      dplyr::select(-Total)

    community_nodes <- community_nodes %>%
      dplyr::left_join(community_node_impacts, by = dplyr::join_by(id == Community)) %>%
      dplyr::mutate(value = Total) %>%
      dplyr::select(-Total)


    results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]] <- attribute_nodes
    results[["bn"]][["viz_prep"]][["community_viz_prep"]][["nodes"]] <- community_nodes


  }



  results[["viz_prep"]] <- list()

  results[["viz_prep"]][["attribute_viz_prep"]] <- results[["bn"]][["viz_prep"]][["attribute_viz_prep"]]

  results[["viz_prep"]][["community_viz_prep"]] <- results[["bn"]][["viz_prep"]][["community_viz_prep"]]



  ###############################
  # Visuals
  ###############################

  if(viz_store){

    results[["visuals"]] <- list()

    results[["visuals"]][["attribute"]][["none"]] <- results %>% work::bn_visual(type = "none")
    results[["visuals"]][["attribute"]][["gravity"]] <- results %>% work::bn_visual(type = "gravity")
    results[["visuals"]][["attribute"]][["charge"]] <- results %>% work::bn_visual(type = "charge")
    results[["visuals"]][["attribute"]][["hierarchy"]] <- results %>% work::bn_visual(type = "hierarchy")

    results[["visuals"]][["community"]][["none"]] <- results %>% work::bn_visual(type = "none", do_community = TRUE)
    results[["visuals"]][["community"]][["gravity"]] <- results %>% work::bn_visual(type = "gravity", do_community = TRUE)
    results[["visuals"]][["community"]][["charge"]] <- results %>% work::bn_visual(type = "charge", do_community = TRUE)
    results[["visuals"]][["community"]][["hierarchy"]] <- results %>% work::bn_visual(type = "hierarchy", do_community = TRUE)

  }



  if(impact_parallel || model_parallel){
    future::plan(original_future_plan)
  }



  return(results)
}







