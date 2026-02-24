#' Engine to Build and Summarize an Unsupervised Bayesian Network
#'
#' @description
#' Builds a Bayesian network without a dependent variable using score-based
#' structure learning (tabu or hill climbing). Optionally enforces cross-battery
#' priority, ensures full connectivity, and layers additional parent connections
#' via bootstrapped arc strengths. Returns the network, fitted model,
#' visualization prep, and model summary.
#'
#' @param df Data frame containing the variables to model.
#' @param ivs Character vector or list of character vectors. Variable names to include.
#'   When a list of batteries, `cross_battery_priority` controls within-battery arcs.
#' @param white_list Optional data frame with columns `from`, `to` of arcs to force include.
#' @param black_list Optional data frame with columns `from`, `to` of arcs to blacklist.
#' @param algorithm Character. Structure learning algorithm: `"tabu"` or `"hc"`.
#' @param score Character. Scoring function: `"bic"` or `"aic"`.
#' @param connections_max Integer (>= 1). Maximum number of parents per node.
#'   1 = base structure only; higher values add layers via `bn_increase_complexity()`.
#' @param connections_multiple_boot_threshold Numeric in \[0,1\] or `"auto"`.
#'   Threshold for arc strength when adding layers.
#' @param connections_multiple_boot_ratio Numeric in (0,1]. When `threshold = "auto"`,
#'   proportion of top arcs to retain.
#' @param cross_battery_priority Logical. If `TRUE` and `ivs` is a list of batteries,
#'   blacklists within-battery arcs so the base structure favors cross-battery connections.
#' @param only_white_list Logical. If `TRUE`, skips structure learning and uses only `white_list`.
#' @param dictionary Optional mapping or data frame for pretty labels.
#' @param manual_groups Optional data frame of node-level group/color/labels for viz.
#' @param node_label_type One of `c("both","variable","label")`. Label style in viz.
#' @param n_groups Optional integer. Target number of communities for viz.
#' @param node_size Numeric. Node size scaling for viz.
#' @param ensure_connectivity Logical. If `TRUE` (default), checks that the learned
#'   network is fully connected and bridges disconnected components using bootstrapped
#'   arc strengths via `bn_ensure_connectivity()`.
#' @param connectivity_boot_n Integer. Number of bootstrap replicates for connectivity bridging.
#' @param complexity_boot_n Integer (default = 100). Number of bootstrap replicates
#'   for complexity expansion via `bn_increase_complexity()`.
#' @param complexity_boot_strength_min Numeric (default = 0.01). Minimum arc strength
#'   retained during complexity expansion.
#' @param suppress_bn_warning Logical. Suppress BIC direction warning.
#' @param on_exit_detach_igraph Logical. Detach igraph on exit.
#' @param tool_tip_edge_prefix Optional character string to prepend to edge tooltips.
#' @param seed Optional integer. Set seed for reproducible structure learning.
#'
#' @details
#' **Workflow**
#' 1) Learn base structure via `bnlearn::tabu()` or `bnlearn::hc()` with `maxp = 1`.
#'    When `cross_battery_priority = TRUE` and `ivs` is a list, within-battery arcs
#'    are blacklisted so the base structure only learns cross-battery connections.
#' 2) Optionally ensure full connectivity via `bn_ensure_connectivity()`.
#' 3) Fit with `bnlearn::bn.fit(method = "bayes")`.
#' 4) Optionally add layers of additional parents up to `connections_max` using
#'    `bn_increase_complexity()`.
#' 5) Prepare network visualization with `bn_to_netviz_prep()`.
#'
#' This is the unsupervised counterpart to [bn_engine()], which requires a DV
#' and uses a TAN backbone. The return structure is identical except `meta$analysis`
#' is `"bn_model_unsupervised"` and there is no `dv` in metadata.
#'
#' @return A list with elements:
#' \itemize{
#'   \item `bn`: the learned `bnlearn` network.
#'   \item `fit`: the fitted parameters (`bn.fit`).
#'   \item `viz_prep`: visualization-prep object from `bn_to_netviz_prep()`.
#'   \item `summary`: model summary list from `bn_summary_statistics()` (no accuracy metrics).
#'   \item `results_layers` (optional): list with per-layer results when `connections_max > 1`.
#'   \item `meta`: list with metadata (`analysis = "bn_model_unsupervised"`).
#' }
#'
#' @examples
#' \dontrun{
#' library(bnlearn)
#' library(dplyr)
#' df <- iris %>% mutate(across(everything(), as.factor))
#' ivs <- names(df)
#'
#' out <- bn_engine_unsupervised(
#'   df = df,
#'   ivs = ivs,
#'   connections_max = 2,
#'   seed = 1
#' )
#'
#' out[["bn"]]
#' out[["summary"]][["model"]]
#' }
#'
#' @export
bn_engine_unsupervised <- function(
    df,
    ivs,
    dictionary = NULL,
    manual_groups = NULL,
    white_list = NULL,
    black_list = NULL,
    algorithm = c("tabu", "hc"),
    score = c("bic", "aic"),
    connections_max = 1,
    connections_multiple_boot_threshold = "auto",
    connections_multiple_boot_ratio = 1/4,
    cross_battery_priority = TRUE,
    only_white_list = FALSE,
    node_label_type = c("both", "variable", "label"),
    n_groups = NULL,
    node_size = 1,
    ensure_connectivity = TRUE,
    connectivity_boot_n = 50,
    complexity_boot_n = 100,
    complexity_boot_strength_min = 0.01,
    suppress_bn_warning = FALSE,
    on_exit_detach_igraph = TRUE,
    tool_tip_edge_prefix = NULL,
    seed = 1
){

  # dictionary = NULL
  # manual_groups = NULL
  # white_list = NULL
  # black_list = NULL
  # algorithm = "tabu"
  # score = "bic"
  # connections_max = 1
  # connections_multiple_boot_threshold = "auto"
  # connections_multiple_boot_ratio = 1/4
  # cross_battery_priority = TRUE
  # only_white_list = FALSE
  # node_label_type = "both"
  # n_groups = NULL
  # node_size = 1
  # ensure_connectivity = TRUE
  # connectivity_boot_n = 50
  # complexity_boot_n = 100
  # complexity_boot_strength_min = 0.01
  # suppress_bn_warning = FALSE
  # on_exit_detach_igraph = TRUE
  # tool_tip_edge_prefix = NULL
  # seed = 1


  if (!is.null(seed)) set.seed(seed)

  algorithm <- match.arg(algorithm)
  score <- match.arg(score)
  node_label_type <- match.arg(node_label_type)

  dictionary <- dictionary %>% work::dictionary_from_named_object()

  results <- list()


  # ---------------------------
  # Validate required arguments
  # ---------------------------

  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }

  if (is.null(ivs) || length(ivs) == 0) {
    stop("'ivs' must be specified as a character vector or list of character vectors.")
  }

  work::assert_cols_exist(df, unlist(ivs), "data frame for bn_engine_unsupervised()")
  work::assert_positive_integer(connections_max, "connections_max")

  if (!is.null(seed)) {
    work::assert_numeric_scalar(seed, "seed")
  }

  if (!is.null(connections_multiple_boot_ratio)) {
    work::assert_numeric_scalar(connections_multiple_boot_ratio, "connections_multiple_boot_ratio")
  }

  if (only_white_list && is.null(white_list)) {
    stop("You set 'only_white_list = TRUE' but did not provide a valid 'white_list'.")
  }



  ##############################
  # set up
  ##############################

  vars <- ivs %>% unlist() %>% setNames(NULL)

  if (!is.null(white_list)) {
    white_list <- white_list %>% as.data.frame()
  }

  if (!is.null(black_list)) {
    black_list <- black_list %>% as.data.frame()
  }

  dfx <- df %>% dplyr::select(dplyr::all_of(vars)) %>% as.data.frame()



  ##############################
  # learn base structure
  ##############################

  if (!only_white_list) {

    # --- build blacklist ---
    if (cross_battery_priority && is.list(ivs)) {

      temp_black_list <- ivs %>%
        purrr::map_dfr(make_arcs) %>%
        dplyr::bind_rows(black_list) %>%
        dplyr::distinct() %>%
        as.data.frame()

    } else {

      temp_black_list <- if (is.null(black_list)) NULL else as.data.frame(black_list)

    }


    # --- structure learning ---
    if (!is.null(seed)) set.seed(seed)

    learn_fn <- switch(
      algorithm,
      "tabu" = bnlearn::tabu,
      "hc"   = bnlearn::hc
    )

    base_learned <- learn_fn(
      x = dfx,
      maxp = 1,
      score = score,
      blacklist = temp_black_list,
      whitelist = white_list
    )

    white_list_base <- base_learned %>%
      bnlearn::arcs() %>%
      as.data.frame()

  } else {

    white_list_base <- white_list %>% as.data.frame()

  }


  # --- check we have arcs ---
  if (nrow(white_list_base) == 0) {
    stop("Structure learning produced no arcs. Consider relaxing blacklist constraints or providing a white_list.")
  }



  ##############################
  # build and fit base model
  ##############################

  base_bn <- bnlearn::empty.graph(vars)
  bnlearn::arcs(base_bn) <- white_list_base


  # --- ensure connectivity ---
  if (ensure_connectivity) {

    base_bn <- work::bn_ensure_connectivity(
      bn = base_bn,
      df = dfx,
      algorithm = algorithm,
      score = score,
      black_list = black_list,
      white_list = white_list,
      bootstrap_reps = connectivity_boot_n,
      seed = seed
    )

    # update white_list_base after bridging
    white_list_base <- bnlearn::arcs(base_bn) %>% as.data.frame()

  }


  base_fit <- bnlearn::bn.fit(x = base_bn, data = dfx, method = "bayes")



  ##############################
  # viz prep
  ##############################

  base_netviz <- work::bn_to_netviz_prep(
    obj = base_bn,
    df = dfx,
    dictionary = dictionary,
    manual_groups = manual_groups,
    remove_nodes = NULL,
    node_label_type = node_label_type,
    n_groups = n_groups,
    node_size = node_size,
    tool_tip_edge_prefix = tool_tip_edge_prefix,
    on_exit_detach_igraph = FALSE
  )



  ##############################
  # layer on more parents
  ##############################

  results_layers <- NULL

  if (connections_max > 1 && !only_white_list) {

    white_list_layer <- work::bn_increase_complexity(
      df = dfx,
      dv = NULL,
      white_list_base = white_list_base,
      black_list = black_list,
      dv_arcs = NULL,
      connections_max = connections_max,
      bootstrap_reps = complexity_boot_n,
      algorithm = algorithm,
      score = score,
      threshold = connections_multiple_boot_threshold,
      auto_threshold_ratio = connections_multiple_boot_ratio,
      strength_min = complexity_boot_strength_min,
      align_direction = TRUE,
      return_strength = FALSE,
      seed = seed
    )


    results_layers <- white_list_layer %>%
      purrr::map(
        ~{
          temp_results <- list()

          temp_bn <- bnlearn::empty.graph(vars)
          bnlearn::arcs(temp_bn) <- .x

          temp_results[["bn"]] <- temp_bn
          temp_results[["fit"]] <- bnlearn::bn.fit(x = temp_bn, data = dfx, method = "bayes")

          temp_results[["viz_prep"]] <- work::bn_to_netviz_prep(
            obj = temp_bn,
            df = dfx,
            dictionary = dictionary,
            manual_groups = base_netviz[["attribute_viz_prep"]][["nodes"]],
            remove_nodes = NULL,
            node_label_type = node_label_type,
            n_groups = n_groups,
            node_size = node_size,
            tool_tip_edge_prefix = tool_tip_edge_prefix,
            on_exit_detach_igraph = FALSE
          )


          temp_results[["summary"]] <- work::bn_summary_statistics(
            bn = temp_bn,
            df = dfx,
            dv = NULL,
            fit = temp_results[["fit"]],
            compare_to_naive = FALSE,
            suppress_bn_warning = TRUE
          )

          temp_results
        }
      )


    results_layers[["summary"]] <- results_layers %>%
      purrr::map(~ .x[["summary"]][["model"]]) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        layer = dplyr::row_number(),
        .before = 1
      )

  }



  ##############################
  # results
  ##############################

  results[["bn"]] <- base_bn
  results[["fit"]] <- base_fit
  results[["viz_prep"]] <- base_netviz

  results[["summary"]] <- work::bn_summary_statistics(
    bn = base_bn,
    df = dfx,
    dv = NULL,
    fit = base_fit,
    compare_to_naive = FALSE,
    suppress_bn_warning = TRUE
  )


  if (!is.null(results_layers)) {
    results[["results_layers"]] <- results_layers
    results[["summary"]] <- results_layers[["summary"]]
  }


  results[["meta"]] <- list(
    analysis = "bn_model_unsupervised",
    ivs = ivs
  )


  # --- cleanup ---
  if (on_exit_detach_igraph) work::detach_igraph()

  if (!suppress_bn_warning) work::warning_bnlearn_bic()


  return(results)
}
