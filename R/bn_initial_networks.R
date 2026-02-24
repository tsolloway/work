#' Build Initial Bayesian Network Configurations
#'
#' @description
#' Constructs one or more Bayesian network configurations based on combinations
#' of cross-battery and path structure types. When a DV is provided, generates
#' supervised networks:
#' - Cross-battery direct
#' - Cross-battery hierarchical
#' - Natural-fallout direct
#' - Natural-fallout hierarchical
#'
#' When `dv = NULL`, generates unsupervised networks using
#' [work::bn_engine_unsupervised()]. In this mode, `path_type` does not apply
#' (there is no DV to create direct/hierarchical paths to), and only cross-battery
#' vs natural-fallout configurations are generated.
#'
#' Each configuration calls [work::bn_engine()] or [work::bn_engine_unsupervised()]
#' internally with parameterized control over connection strength, direction, and
#' bootstrapping complexity. The resulting networks are stored in a named list,
#' with a combined summary table appended under `$summary`.
#'
#' @param df A data frame containing the variables used for the network(s).
#' @param dv Character or NULL; dependent variable(s). When `NULL`, builds
#'   unsupervised networks via [work::bn_engine_unsupervised()].
#' @param ivs Character vector or named list of independent variables. If not
#'   a list, the function automatically switches to `"natural_fallout"` mode.
#' @param dictionary Optional variable dictionary or named vector, passed to
#'   [work::dictionary_from_named_object()].
#' @param manual_groups Optional manual node grouping data frame.
#' @param white_list,black_list Optional arc constraints passed to
#'   [work::bn_engine()] or [work::bn_engine_unsupervised()].
#' @param cb_type Character; type of cross-battery configuration.
#'   One of `"both"`, `"cross_battery"`, or `"natural_fallout"`.
#' @param path_type Character; type of path structure (supervised only).
#'   One of `"both"`, `"direct"`, or `"hierarchical"`. Ignored when `dv = NULL`.
#' @param algorithm Character; structure learning algorithm for unsupervised mode.
#'   One of `"tabu"` or `"hc"`. Ignored when `dv` is provided.
#' @param score Character; scoring function for unsupervised mode.
#'   One of `"bic"` or `"aic"`. Ignored when `dv` is provided.
#' @param connections_max Integer; maximum parent connections per node.
#' @param connections_multiple_boot_threshold,connections_multiple_boot_ratio
#'   Control multiple-connection bootstrap behavior (see [work::bn_engine()]).
#' @param dv_connection_strength Optional numeric \[0, \1]; override for DV arc strength.
#'   Ignored when `dv = NULL`.
#' @param force_dv_connection Optional logical; force DV arcs regardless of strength.
#'   Ignored when `dv = NULL`.
#' @param only_white_list Logical; if `TRUE`, only whitelist arcs are considered.
#' @param compare_to_naive Logical; if `TRUE`, adds naive-Bayes comparison.
#'   Ignored when `dv = NULL`.
#' @param node_label_type Character; node label display mode (`"both"`, `"variable"`, `"label"`).
#' @param n_groups Optional integer; manual override for detected groups.
#' @param node_size Numeric; relative node size in visualization.
#' @param reachability_max_iter Integer; max iterations for reachability check.
#'   Ignored when `dv = NULL`.
#' @param ensure_connectivity Logical; if `TRUE` (default), ensures unsupervised
#'   networks are fully connected. Ignored when `dv` is provided.
#' @param connectivity_boot_n Integer; bootstrap replicates for connectivity bridging.
#'   Ignored when `dv` is provided.
#' @param complexity_boot_n Integer; number of bootstrap replicates for complexity.
#' @param complexity_boot_strength_min Numeric; minimum strength for complexity retention.
#' @param suppress_bn_warning Logical; suppress bnlearn warnings.
#' @param on_exit_detach_igraph Logical; whether to detach igraph on exit.
#' @param tool_tip_edge_prefix Optional character string to prepend to edge tooltips.
#' @param seed Optional integer; random seed for reproducibility.
#'
#' @return
#' A named list of Bayesian network objects, potentially containing:
#' - Supervised: `$cb_direct`, `$cb_hierarchy`, `$ncb_direct`, `$ncb_hierarchy`
#' - Unsupervised: `$cb`, `$ncb`
#' - `$summary`: a combined data frame summarizing all models.
#'
#' @examples
#' \dontrun{
#' # --- Supervised (with DV) ---
#' bn_initial_networks(
#'   df = iris,
#'   dv = "Species",
#'   ivs = list(
#'     measures = c("Sepal.Length", "Sepal.Width"),
#'     attributes = c("Petal.Length", "Petal.Width")
#'   ),
#'   cb_type = "both",
#'   path_type = "both"
#' )
#'
#' # --- Unsupervised (no DV) ---
#' bn_initial_networks(
#'   df = iris,
#'   dv = NULL,
#'   ivs = list(
#'     measures = c("Sepal.Length", "Sepal.Width"),
#'     attributes = c("Petal.Length", "Petal.Width")
#'   ),
#'   cb_type = "both"
#' )
#' }
#'
#' @export
bn_initial_networks <- function(
    df,
    dv = NULL,
    ivs,
    dictionary = NULL,
    manual_groups = NULL,
    white_list = NULL,
    black_list = NULL,
    cb_type = c("both", "cross_battery", "natural_fallout"),
    path_type = c("both", "direct", "hierarchical"),
    algorithm = c("tabu", "hc"),
    score = c("bic", "aic"),
    connections_max = 1,
    connections_multiple_boot_threshold = "auto",
    connections_multiple_boot_ratio = 1/4,
    dv_connection_strength = NULL,
    force_dv_connection = NULL,
    only_white_list = FALSE,
    compare_to_naive = TRUE,
    node_label_type = c("both", "variable", "label"),
    n_groups = NULL,
    node_size = 1,
    reachability_max_iter = 10,
    ensure_connectivity = TRUE,
    connectivity_boot_n = 50,
    complexity_boot_n = 10,
    complexity_boot_strength_min = 0.01,
    suppress_bn_warning = FALSE,
    on_exit_detach_igraph = TRUE,
    tool_tip_edge_prefix = NULL,
    seed = 1
){

  # cb_type = "both"
  # path_type = "both"


  # --- Setup ---
  cb_type <- match.arg(cb_type)
  path_type <- match.arg(path_type)
  algorithm <- match.arg(algorithm)
  score <- match.arg(score)
  node_label_type <- match.arg(node_label_type)

  dictionary <- work::dictionary_from_named_object(dictionary)
  df <- df %>% as.data.frame()
  ivs <- ivs %>% purrr::map(as.character)

  unsupervised <- is.null(dv)

  if (!unsupervised) {
    dv <- dv %>% unlist() %>% as.character() %>% setNames(NULL)
    work::assert_cols_exist(df, dv)
  }

  # --- Validation ---
  work::assert_cols_exist(df, unlist(ivs) %>% as.character() %>% setNames(NULL))
  work::assert_positive_integer(connections_max)

  # --- Adjust IV structure ---
  if (!is.list(ivs)) {
    ivs <- ivs %>% unlist() %>% as.character() %>% setNames(NULL)
    if (cb_type != "natural_fallout") {
      cb_type <- "natural_fallout"
      warning("ivs is not a multiple-slot list. Switching to natural_fallout mode.")
    }
  }


  # --- Warn if path_type set in unsupervised mode ---
  if (unsupervised && path_type != "both") {
    message("path_type is ignored in unsupervised mode (no DV). Running cross-battery configurations only.")
  }



  ##############################
  # Unsupervised path
  ##############################

  if (unsupervised) {

    build_unsupervised <- function(cross_battery) {
      work::bn_engine_unsupervised(
        df = df,
        ivs = ivs,
        dictionary = dictionary,
        manual_groups = manual_groups,
        white_list = white_list,
        black_list = black_list,
        algorithm = algorithm,
        score = score,
        connections_max = connections_max,
        connections_multiple_boot_threshold = connections_multiple_boot_threshold,
        connections_multiple_boot_ratio = connections_multiple_boot_ratio,
        cross_battery_priority = cross_battery,
        only_white_list = only_white_list,
        node_label_type = node_label_type,
        n_groups = n_groups,
        node_size = node_size,
        ensure_connectivity = ensure_connectivity,
        connectivity_boot_n = connectivity_boot_n,
        complexity_boot_n = complexity_boot_n,
        complexity_boot_strength_min = complexity_boot_strength_min,
        suppress_bn_warning = suppress_bn_warning,
        on_exit_detach_igraph = FALSE,
        tool_tip_edge_prefix = tool_tip_edge_prefix,
        seed = seed
      )
    }

    results <- list()

    if (cb_type != "natural_fallout")
      results[["cb"]] <- build_unsupervised(TRUE)

    if (cb_type != "cross_battery")
      results[["ncb"]] <- build_unsupervised(FALSE)


    # --- Summarize ---
    results[["summary"]] <- results %>%
      purrr::imap(
        ~.x[["summary"]][["model"]] %>%
          dplyr::mutate(model_type = .y, .before = 1)
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(dplyr::desc(bic))


    # --- cleanup ---
    if (on_exit_detach_igraph) work::detach_igraph()
    if (!suppress_bn_warning) work::warning_bnlearn_bic()

    return(results)
  }



  ##############################
  # Supervised path
  ##############################

  # --- Internal helper for network construction ---
  build_networks <- function(cross_battery, direct) {
    purrr::map(
      dv,
      ~work::bn_engine(
        df = df,
        dv = .x,
        ivs = ivs,
        dictionary = dictionary,
        cross_battery_priority = cross_battery,
        all_ivs_connect_to_dv = direct,
        suppress_bn_warning = suppress_bn_warning,
        on_exit_detach_igraph = FALSE,
        manual_groups = manual_groups,
        white_list = white_list,
        black_list = black_list,
        connections_max = connections_max,
        connections_multiple_boot_threshold = connections_multiple_boot_threshold,
        connections_multiple_boot_ratio = connections_multiple_boot_ratio,
        dv_connection_strength = dv_connection_strength,
        force_dv_connection = force_dv_connection,
        only_white_list = only_white_list,
        compare_to_naive = compare_to_naive,
        node_label_type = node_label_type,
        n_groups = n_groups,
        node_size = node_size,
        reachability_max_iter = reachability_max_iter,
        complexity_boot_n = complexity_boot_n,
        complexity_boot_strength_min = complexity_boot_strength_min,
        tool_tip_edge_prefix = tool_tip_edge_prefix,
        seed = seed
      )
    ) %>% setNames(dv)
  }


  # --- Network generation ---
  results <- list()

  if (cb_type != "natural_fallout" && path_type != "hierarchical")
    results[["cb_direct"]] <- build_networks(TRUE, TRUE)

  if (cb_type != "natural_fallout" && path_type != "direct")
    results[["cb_hierarchy"]] <- build_networks(TRUE, FALSE)

  if (cb_type != "cross_battery" && path_type != "hierarchical")
    results[["ncb_direct"]] <- build_networks(FALSE, TRUE)

  if (cb_type != "cross_battery" && path_type != "direct")
    results[["ncb_hierarchy"]] <- build_networks(FALSE, FALSE)



  # --- Summarize ---
  results <- results %>%
    purrr::compact() %>%
    purrr::map(~{

      .x[["summary"]] <- purrr::map(.x, ~.x[["summary"]][["model"]]) %>%
        dplyr::bind_rows()

      .x
    })


  results[["summary"]] <- results %>%
    purrr::imap(
      ~.x[["summary"]] %>%
        dplyr::mutate(model_type = .y, .before = 1)
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(-accuracy)




  # --- cleanup ---
  if(on_exit_detach_igraph) work::detach_igraph()
  if(!suppress_bn_warning) work::warning_bnlearn_bic()


  return(results)
}

