#' bn_finalize_network
#' @description Finalize a Bayesian network by re-estimating with a fixed structure,
#'   computing subgroup models, and running impact analysis.
#' @export
bn_finalize_network <- function(
    obj = NULL,
    df,
    dv = NULL,
    subgroups = NULL,
    dictionary = NULL,
    viz_prep = NULL,
    bn = NULL,
    previous_dv = NULL,
    node_label_type = c("both", "variable", "label"),
    manual_groups = NULL,
    impact_type = c("gr", "cp", "mi"),
    impact_n_boot = 1,
    impact_n_querry = 1e4,
    impact_lift = 0,
    impact_lift_type = c("proportional", "absolute"),
    impact_brand = NULL,
    tool_tip_edge_prefix = NULL,
    viz_size_node_by_impact = TRUE,
    viz_include_dv = FALSE,
    model_parallel = FALSE,
    impact_parallel = TRUE,
    seed = 1
){

  node_label_type <- match.arg(node_label_type)
  impact_type <- match.arg(impact_type)
  impact_lift_type <- match.arg(impact_lift_type)

  results <- list()
  dictionary <- work::dictionary_from_named_object(dictionary)
  df <- df %>% as.data.frame()
  source_unsupervised <- FALSE
  x_ivs <- NULL


  if(impact_parallel || model_parallel){
    original_future_plan <- future::plan()
    on.exit(future::plan(original_future_plan), add = TRUE)
    future::plan(future::multisession)
  }


  ###############################
  # Object setup
  ###############################

  if("meta" %in% names(obj)){

    analysis_type <- obj[["meta"]][["analysis"]]

    if(analysis_type == "bn_model_single"){

      if(is.null(bn)) bn <- obj[["bn"]]
      if(is.null(viz_prep)) viz_prep <- obj[["viz_prep"]]

      if(is.null(dv)) dv <- obj[["meta"]][["dv"]]
      if(is.null(previous_dv)) previous_dv <- obj[["meta"]][["dv"]]
      x_ivs <- obj[["meta"]][["ivs"]] %>% unlist() %>% setNames(NULL)

    }else if(analysis_type == "bn_model_unsupervised"){

      if(is.null(dv)) stop("dv is required when finalizing an unsupervised network.")
      source_unsupervised <- TRUE

      if(is.null(bn)) bn <- obj[["bn"]]
      if(is.null(viz_prep)) viz_prep <- obj[["viz_prep"]]
      x_ivs <- obj[["meta"]][["ivs"]] %>% unlist() %>% setNames(NULL)

    }else{
      stop("Unknown analysis type: ", analysis_type)
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


  if(is.null(bn)) stop("Cannot find a bn object.")
  if(is.null(dv)) stop("dv is required. Supply it directly or pass an obj with meta$dv.")


  ###############################
  # prep work
  ###############################

  if(!is.null(viz_prep) && is.null(manual_groups)){
    manual_groups <- viz_prep[["nodes"]] %>% as.data.frame()
  }


  x_nodes <- bn %>% bnlearn::nodes()
  x_edges <- bn %>% bnlearn::arcs() %>% as.data.frame()


  if(!source_unsupervised){
    if(is.null(previous_dv) && !is.null(viz_prep)){
      if(viz_prep[["nodes"]][["id"]][[1]] != dv){
        previous_dv <- viz_prep[["nodes"]][["id"]][[1]]
      }
    }

    if(!is.null(previous_dv)){
      if(dv != previous_dv){
        if((previous_dv %in% x_nodes) && !dv %in% x_nodes){
          x_nodes <- x_nodes %>% gsub(previous_dv, dv, ., fixed = TRUE)
          x_edges[["from"]] <- x_edges[["from"]] %>% gsub(previous_dv, dv, ., fixed = TRUE)
          x_edges[["to"]] <- x_edges[["to"]] %>% gsub(previous_dv, dv, ., fixed = TRUE)
        }else{
          stop("It's not programatically clear how to insert the new DV.  Please do it outside of this wrapper")
        }
      }
    }
  }


  if(is.null(x_ivs)) x_ivs <- x_nodes %>% setdiff(dv)


  # --- subgroups default ---
  if(is.null(subgroups)){
    subgroups <- "Total"
    if(!"Total" %in% names(df)) df[["Total"]] <- 1L
  }


  ###############################
  # final model
  ###############################

  cli::cli_alert_info("Beginning model estimation")

  results[["bn"]] <- work::bn_engine(
    df = df,
    dv = dv,
    ivs = x_ivs,
    dictionary = dictionary,
    white_list = x_edges,
    node_label_type = node_label_type,
    manual_groups = manual_groups,
    only_white_list = !source_unsupervised,
    tool_tip_edge_prefix = tool_tip_edge_prefix,
    remove_dv_from_viz_prep = if(viz_include_dv) NULL else dv,
    suppress_bn_warning = TRUE,
    on_exit_detach_igraph = FALSE,
    seed = seed
  )

  # Update edges from the main model so subgroups get the full structure
  # (critical for unsupervised sources where x_edges has no DV connections)
  x_edges <- results[["bn"]][["bn"]] %>% bnlearn::arcs() %>% as.data.frame()

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
    dplyr::arrange(dplyr::desc(accuracy))

  cli::cli_alert_info("Completed model estimation")



  ###############################
  # Impacts attributes
  ###############################

  attribute_nodes <- results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]]
  community_nodes <- results[["bn"]][["viz_prep"]][["community_viz_prep"]][["nodes"]]

  cli::cli_alert_info("Beginning impact estimation — attributes")

  results[["impact"]] <- list()

  results[["impact"]][["attribute"]] <- work::bn_impact(
    obj = results[["bn_subgroups"]],
    df = df,
    dv = dv,
    ivs = x_ivs,
    do_community = FALSE,
    community_assignment = attribute_nodes,
    type = impact_type,
    process_subgroups = TRUE,
    n_boot = impact_n_boot,
    n_querry = impact_n_querry,
    lift = impact_lift,
    lift_type = impact_lift_type,
    brand = impact_brand,
    use_parallel = impact_parallel,
    seed = seed
  )

  cli::cli_alert_info("Beginning impact estimation — communities")

  results[["impact"]][["community"]] <- work::bn_impact(
    obj = results[["bn_subgroups"]],
    df = df,
    dv = dv,
    ivs = x_ivs,
    do_community = TRUE,
    community_assignment = attribute_nodes,
    type = impact_type,
    process_subgroups = TRUE,
    n_boot = impact_n_boot,
    n_querry = impact_n_querry,
    lift = impact_lift,
    lift_type = impact_lift_type,
    brand = impact_brand,
    use_parallel = impact_parallel,
    seed = seed
  )

  cli::cli_alert_info("Completed impact estimation")



  ###############################
  # Edge List & Viz Prep Storage
  ###############################

  results[["edge_list"]] <- list()

  results[["edge_list"]][["all"]] <- results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["edges"]]

  results[["edge_list"]][["iv"]] <- results[["edge_list"]][["all"]] %>% dplyr::filter(from != dv & to != dv)

  results[["edge_list"]][["dv"]] <- results[["edge_list"]][["all"]] %>% dplyr::filter(from == dv | to == dv)



  if(viz_size_node_by_impact){

    # bn_impact() prefixes index as {subgroup}_index, then gsub strips _index
    # so the index column is just the subgroup name (e.g., "Total")
    index_col <- subgroups[1]

    join_impact <- function(nodes, impacts, id_col) {
      impact_vals <- impacts %>% dplyr::select(dplyr::all_of(c(id_col, index_col)))
      nodes %>%
        dplyr::left_join(impact_vals, by = stats::setNames(id_col, "id")) %>%
        dplyr::mutate(value = .data[[index_col]]) %>%
        dplyr::select(-dplyr::all_of(index_col))
    }

    results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]] <-
      join_impact(attribute_nodes, results[["impact"]][["attribute"]], "Variable")

    results[["bn"]][["viz_prep"]][["community_viz_prep"]][["nodes"]] <-
      join_impact(community_nodes, results[["impact"]][["community"]], "Community")

  }


  return(results)
}







