#' bn_finalize_network
#'
#' @description Finalize a Bayesian network by re-estimating with a fixed
#'   structure, computing subgroup models, running impact analysis, and
#'   optionally running full impacts (attribute + community, weighted/unweighted)
#'   and prioritization analysis.
#'
#' @param obj Network object. Accepts output of \code{bn_model_single()},
#'   \code{bn_model_unsupervised()}, a bare \code{bnlearn::bn} object, or a
#'   viz_prep list.
#' @param df Data frame containing the DV, IV, and optionally brand/weight
#'   columns.
#' @param bn Optional. A \code{bnlearn::bn} object. If NULL, extracted from
#'   \code{obj}.
#' @param viz_prep Optional. Visualization prep object. If NULL, extracted from
#'   \code{obj}.
#' @param dv Character. Dependent variable name. If NULL, auto-detected from
#'   \code{obj$meta$dv}.
#' @param previous_dv Character or NULL. Previous DV name for node swapping.
#' @param manual_groups Data frame or NULL. Manual community groupings.
#' @param node_label_type Character. Label format for network nodes:
#'   \code{"both"}, \code{"variable"}, or \code{"label"}. Default
#'   \code{"both"}.
#' @param subgroups Character vector or NULL. Column names in \code{df} that
#'   define subgroups (each should be 0/1). Default NULL (single "Total"
#'   subgroup).
#' @param dictionary Optional. Dictionary for variable labels.
#' @param brand Character or NULL. Brand column name. Default NULL.
#' @param brand_names Character vector or NULL. Brand levels to include.
#'   Default NULL (all brands).
#' @param weight Character or NULL. Weight column name. Default NULL.
#' @param dv_metric Character. \code{"mean"} or \code{"top_box"}.
#'   Default \code{"mean"}.
#' @param min_base_for_calc Integer. Minimum sample size for brand lift
#'   calculations in \code{bn_impacts()} and for bootstrap p-values in
#'   \code{bn_prioritizations()}. Default 100.
#' @param n_boot_final Integer. Bootstrap replicates used for community MI
#'   in \code{bn_impacts()} and for p-values in \code{bn_prioritizations()}.
#'   Default 100.
#' @param do_impacts Logical. If TRUE, run \code{bn_impacts()} to produce full
#'   attribute/community, weighted/unweighted impact tables. Default TRUE.
#' @param impact_type Character. Impact estimation type: \code{"gr"},
#'   \code{"cp"}, or \code{"mi"}. Default \code{"gr"}.
#' @param impact_index_by Character. Indexing method for impact. Default
#'   \code{"lift_first"}.
#' @param impact_n_boot Integer. Bootstrap replicates for impact. Default 1.
#' @param impact_n_querry Integer. Query sample size. Default 1e4.
#' @param impact_lift Numeric vector. Lift fractions for impact. Default
#'   \code{c(0, 0.1)}.
#' @param impact_metric_type Character. \code{"proportional"} or
#'   \code{"absolute"}. Default \code{"proportional"}.
#' @param impact_include_base Logical. Include base sizes in impact tables.
#'   Default TRUE.
#' @param do_prioritizations Logical. If TRUE, run \code{bn_prioritizations()}
#'   to produce prioritization analysis. Default TRUE.
#' @param prioritize_lift Numeric. Lift fraction for prioritization. Default
#'   0.10.
#' @param prioritize_ivs_excluded Character vector or NULL. Variables to exclude
#'   from prioritization. Default NULL.
#' @param prioritize_threshold Numeric or NULL. Early stopping threshold for
#'   prioritization. Default 0.01.
#' @param prioritize_max_rounds Integer or NULL. Maximum priority rounds.
#'   Default NULL.
#' @param prioritize_noise_tail Numeric. Fraction of tail steps for noise floor.
#'   Default 1/3.
#' @param prioritize_sig_threshold Numeric. P-value threshold for the
#'   "significant" colour band stored in the prioritizations meta. Used by
#'   downstream writers (\code{bn_prioritize_write}, \code{bn_report}) for
#'   consistent colour coding. Default 0.05.
#' @param prioritize_marginal_threshold Numeric. P-value threshold for the
#'   "marginal" colour band stored in the prioritizations meta. Default 0.10.
#' @param tool_tip_edge_prefix Character or NULL. Prefix for edge tooltips.
#' @param viz_size_node_by_impact Logical. Size network nodes by impact index.
#'   Default TRUE.
#' @param viz_include_dv Logical. Include DV node in visualization. Default
#'   FALSE.
#' @param model_parallel Logical. Parallelize subgroup model estimation.
#'   Default FALSE.
#' @param impact_parallel Logical. Parallelize impact and prioritization
#'   estimation. Default TRUE.
#' @param seed Integer. Random seed. Default 1.
#'
#' @return A list with:
#' \describe{
#'   \item{bn}{The finalized Bayesian network object with viz_prep.}
#'   \item{bn_subgroups}{Named list of per-subgroup BN objects.}
#'   \item{bn_subgroups_summary}{Summary table of subgroup model accuracy.}
#'   \item{impacts}{Output of \code{bn_impacts()} (when \code{do_impacts = TRUE}).}
#'   \item{prioritizations}{Output of \code{bn_prioritizations()} (when
#'     \code{do_prioritizations = TRUE}).}
#'   \item{edge_list}{List with \code{all}, \code{iv}, and \code{dv} edge tables.}
#' }
#'
#' @seealso [bn_impact()], [bn_impacts()], [bn_prioritize()],
#'   [bn_prioritizations()]
#'
#' @export
bn_finalize_network <- function(
    obj = NULL,
    df,
    bn = NULL,
    viz_prep = NULL,
    dv = NULL,
    previous_dv = NULL,
    manual_groups = NULL,
    node_label_type = c("both", "variable", "label"),
    subgroups = NULL,
    dictionary = NULL,
    brand = NULL,
    brand_names = NULL,
    weight = NULL,
    # --- Model ---
    dv_metric = c("mean", "top_box"),
    min_base_for_calc = 100,
    n_boot_final = 100,
    # --- Impact ---
    do_impacts = TRUE,
    impact_type = c("gr", "cp", "mi"),
    impact_index_by = c("lift_first", "lift_second", "maxVmin", "mi", "none"),
    impact_n_boot = 1,
    impact_n_querry = 1e4,
    impact_lift = c(0, 0.1),
    impact_metric_type = c("proportional", "absolute"),
    impact_include_base = TRUE,
    # --- Prioritization ---
    do_prioritizations = TRUE,
    prioritize_lift = 0.10,
    prioritize_ivs_excluded = NULL,
    prioritize_threshold = 0.01,
    prioritize_max_rounds = NULL,
    prioritize_noise_tail = 1/3,
    prioritize_sig_threshold = 0.05,
    prioritize_marginal_threshold = 0.10,
    # --- Visualization ---
    tool_tip_edge_prefix = NULL,
    viz_size_node_by_impact = TRUE,
    viz_include_dv = FALSE,
    # --- Parallel ---
    model_parallel = FALSE,
    impact_parallel = TRUE,
    seed = 1
){

  node_label_type <- match.arg(node_label_type)
  impact_type <- match.arg(impact_type)
  impact_metric_type <- match.arg(impact_metric_type)
  dv_metric <- match.arg(dv_metric)

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

  # Preserve named dv for display, strip for bnlearn
  dv_original <- dv
  dv <- unname(dv)


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

  # Report levels that will be dropped per subgroup for DV and IVs — gives
  # visibility into data gaps without the raw bnlearn "levels not observed"
  # warnings that droplevels() suppresses.
  .report_dropped_levels <- function(sg_name) {
    sg_df_raw <- df %>% dplyr::filter(.data[[sg_name]] == 1)
    relevant_vars <- intersect(c(dv, x_ivs), names(sg_df_raw))
    for (var in relevant_vars) {
      col <- sg_df_raw[[var]]
      if (!is.factor(col)) next
      observed <- unique(as.character(col))
      missing <- setdiff(levels(col), observed)
      if (length(missing) > 0) {
        cli::cli_alert_info(
          "Subgroup {.val {sg_name}}: variable {.val {var}} lacks level{?s} {.val {missing}}"
        )
      }
    }
  }
  for (sg in subgroups) .report_dropped_levels(sg)

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
  # Impacts (bn_impacts)
  ###############################

  attribute_nodes <- results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]]
  community_nodes <- results[["bn"]][["viz_prep"]][["community_viz_prep"]][["nodes"]]

  if (do_impacts) {
    results[["impacts"]] <- work::bn_impacts(
      obj = results[["bn_subgroups"]],
      df = df,
      dv = dv_original,
      ivs = x_ivs,
      do_community = TRUE,
      community_assignment = attribute_nodes,
      type = impact_type,
      index_by = impact_index_by,
      process_subgroups = TRUE,
      dictionary = dictionary,
      n_boot = impact_n_boot,
      n_querry = impact_n_querry,
      lift = impact_lift,
      impact_metric_type = impact_metric_type,
      brand = brand,
      brand_names = brand_names,
      min_base_for_lift = min_base_for_calc,
      include_base = impact_include_base,
      dv_metric = dv_metric,
      weight = weight,
      mi_boot = n_boot_final,
      use_parallel = impact_parallel,
      seed = seed
    )

  }


  ###############################
  # Prioritizations (bn_prioritizations)
  ###############################

  if (do_prioritizations) {
    results[["prioritizations"]] <- work::bn_prioritizations(
      obj = results,
      df = df,
      dv = dv_original,
      ivs = x_ivs,
      ivs_excluded = prioritize_ivs_excluded,
      process_subgroups = length(subgroups) > 1 || subgroups[1] != "Total",
      brand = brand,
      brand_names = brand_names,
      weight = weight,
      impact_result = if (!is.null(results[["impacts"]])) results[["impacts"]] else NULL,
      dv_metric = dv_metric,
      lift = prioritize_lift,
      impact_metric_type = impact_metric_type,
      threshold = prioritize_threshold,
      max_rounds = prioritize_max_rounds,
      n_boot_final = n_boot_final,
      noise_tail = prioritize_noise_tail,
      sig_threshold = prioritize_sig_threshold,
      marginal_threshold = prioritize_marginal_threshold,
      min_base_for_boot = min_base_for_calc,
      dictionary = dictionary,
      community_assignment = attribute_nodes,
      use_parallel = impact_parallel,
      seed = seed
    )

  }


  ###############################
  # Edge List & Viz Prep Storage
  ###############################

  results[["edge_list"]] <- list()

  results[["edge_list"]][["all"]] <- results[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["edges"]]

  results[["edge_list"]][["iv"]] <- results[["edge_list"]][["all"]] %>% dplyr::filter(from != dv & to != dv)

  results[["edge_list"]][["dv"]] <- results[["edge_list"]][["all"]] %>% dplyr::filter(from == dv | to == dv)



  if(viz_size_node_by_impact && !is.null(results[["impacts"]])){

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
      join_impact(attribute_nodes, results[["impacts"]][["table_attribute"]], "Variable")

    results[["bn"]][["viz_prep"]][["community_viz_prep"]][["nodes"]] <-
      join_impact(community_nodes, results[["impacts"]][["table_community"]], "Community")

  }


  return(results)
}







