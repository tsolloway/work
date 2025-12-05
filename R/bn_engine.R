#' Engine to Build and Summarize a Bayesian Network with Optional Layered Complexity
#'
#' @description
#' Builds a Bayesian network starting from a Tree-Augmented Naive (TAN)-like
#' backbone (or a provided whitelist), optionally adds directed connections
#' to the dependent variable (DV), and can layer in additional parent
#' connections up to `connections_max`. Returns the network, fitted model,
#' visualization prep, and model summary (optionally with per-layer summaries).
#'
#' @param df Data frame containing `dv` and `ivs`.
#' @param dv Character scalar. Dependent variable name (column in `df`).
#' @param ivs Character vector or list of character vectors. Independent variable names.
#' @param white_list Optional data frame with columns `from`, `to` of arcs to force include.
#' @param black_list Optional data frame with columns `from`, `to` of arcs to blacklist.
#' @param connections_max Integer (>= 1). Maximum number of parents per non-DV node to allow
#'   after the base structure (1 = base only).
#' @param connections_multiple_boot_threshold Numeric in \[0,1\] or `"auto"`.
#'   Threshold for arc strength when adding layers (passed to `bn_increase_complexity()`).
#' @param connections_multiple_boot_ratio Numeric in (0,1]. When `threshold = "auto"`,
#'   proportion of top arcs to retain.
#' @param cross_battery_priority Logical. If `TRUE`, constructs a blacklist to avoid
#'   cross-battery links using `ivs %>% purrr::map_dfr(make_arcs)`.
#' @param all_ivs_connect_to_dv Logical. If `TRUE`, forces all IVs (or nodes in base)
#'   to connect to `dv` (direction handled downstream).
#' @param dv_connection_strength Optional numeric in \[0,1\]. If provided, uses bootstrap
#'   arc strength cutoff for selecting DV connections; otherwise uses a data-driven cutoff.
#' @param force_dv_connection Optional character vector of nodes that must connect to `dv`
#'   when `all_ivs_connect_to_dv = FALSE`.
#' @param only_white_list Logical. If `TRUE`, skips TAN backbone and uses only `white_list`.
#' @param compare_to_naive Logical. Include naive Bayes comparison in summaries.
#' @param dictionary Optional mapping or data frame for pretty labels. Passed through
#'   `dictionary_from_named_object()`.
#' @param manual_groups Optional data frame of node-level group/color/labels for viz.
#' @param node_label_type One of `c("both","variable","label")`. Label style in viz.
#' @param n_groups Optional integer. Target number of communities for viz (if applicable).
#' @param node_size Numeric. Node size scaling for viz.
#' @param reachability_max_iter Integer. Max passes to enforce path reachability to `dv`.
#' @param complexity_boot_n Integer (default = 100). Number of bootstrap replicates
#'   used when expanding network complexity via `bn_increase_complexity()`.
#'   This controls the number of resamples passed to `bnlearn::boot.strength()`
#'   for estimating arc strengths before filtering and layer selection.
#' @param complexity_boot_strength_min Numeric (default = 0.01). Minimum arc
#'   strength retained when evaluating candidate connections during complexity
#'   expansion. Arcs with estimated strength below this threshold are discarded
#'   prior to direction alignment and layering.
#' @param suppress_bn_warning Logical. Suppress learning/fitting warnings in summaries.
#' @param on_exit_detach_igraph Logical. Detach igraph on exit via `detach_igraph()`.
#' @param tool_tip_edge_prefix Optional character string to prepend to edge tooltips (e.g., "MI =").
#' @param seed Optional integer. Set seed for reproducible structure learning.
#'
#' @details
#' **Workflow**
#' 1) Create base arcs via `bnlearn::tree.bayes()` unless `only_white_list = TRUE`.
#' 2) Add DV connections either to all IVs or via selection/bootstrapping.
#' 3) Ensure IVs have reachability to DV via `bn_ensure_reachability()`.
#' 4) Fit with `bnlearn::bn.fit(method = "bayes")`.
#' 5) Optionally add layers of additional parents up to `connections_max` using
#'    `bn_increase_complexity()` + bootstrapped arc strengths.
#' 6) Prepare network visualization with `bn_to_netviz_prep()`.
#'
#' All graphs are initialized with **all variables** in `c(dv, ivs)` to prevent
#' mismatches between the network nodes and the data frame columns.
#'
#' This function depends on helper functions in your package:
#' `make_arcs()`, `bn_ensure_reachability()`, `bn_to_netviz_prep()`,
#' `bn_community_color()`, `bn_increase_complexity()`, `bn_summary_statistics()`,
#' and `detach_igraph()`.
#'
#' @return A list with elements:
#' \itemize{
#'   \item `bn`: the learned `bnlearn` network.
#'   \item `fit`: the fitted parameters (`bn.fit`).
#'   \item `viz_prep`: visualization-prep object from `bn_to_netviz_prep()`.
#'   \item `summary`: model summary list from `bn_summary_statistics()`.
#'   \item `results_layers` (optional): list with two components when layering was used:
#'       \itemize{
#'         \item `layers`: list of per-layer results (bn, fit, viz_prep, summary).
#'         \item `summary`: combined data frame of per-layer model summaries.
#'       }
#'   \item `meta`: list with metadata (e.g., `analysis = "bn_model_single"`).
#' }
#'
#' @examples
#' \dontrun{
#' # Example with iris: DV = Species, IVs = numeric columns
#' library(bnlearn)
#' library(dplyr)
#' df <- iris
#' dv <- "Species"
#' ivs <- names(df)[names(df) != dv]
#'
#' out <- bn_engine(
#'   df = df,
#'   dv = dv,
#'   ivs = ivs,
#'   connections_max = 1,
#'   compare_to_naive = TRUE,
#'   seed = 1
#' )
#'
#' out[["bn"]]
#' out[["summary"]][["model"]]
#' }
#'
#' @export
bn_engine <- function(
    df,
    dv,
    ivs,
    dictionary = NULL,
    manual_groups = NULL,
    white_list = NULL,
    black_list = NULL,
    connections_max = 1,
    connections_multiple_boot_threshold = "auto",
    connections_multiple_boot_ratio = 1/4,
    cross_battery_priority = TRUE,
    all_ivs_connect_to_dv = TRUE,
    dv_connection_strength = NULL,
    force_dv_connection = NULL,
    only_white_list = FALSE,
    compare_to_naive = TRUE,
    node_label_type = c("both", "variable", "label"),
    n_groups = NULL,
    node_size = 1,
    reachability_max_iter = 10,
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
  # connections_max = 1
  # connections_multiple_boot_threshold = "auto"
  # connections_multiple_boot_ratio = 1/4
  # cross_battery_priority = TRUE
  # all_ivs_connect_to_dv = TRUE
  # dv_connection_strength = NULL
  # force_dv_connection = NULL
  # only_white_list = FALSE
  # compare_to_naive = TRUE
  # node_label_type = "both"
  # n_groups = NULL
  # node_size = 1
  # reachability_max_iter = 10
  # complexity_boot_n = 100
  # complexity_boot_strength_min = 0.01
  # suppress_bn_warning = FALSE
  # on_exit_detach_igraph = TRUE
  # seed = 1

  if (!is.null(seed)) set.seed(seed)

  node_label_type <- match.arg(node_label_type)

  dictionary <- dictionary %>% work::dictionary_from_named_object()

  results <- list()


  # ---------------------------
  # Validate required arguments
  # ---------------------------

  # Ensure df is a data frame
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame containing the dependent and independent variables.")
  }

  # Ensure DV and IVs are provided
  if (is.null(dv)) {
    stop("'dv' (dependent variable) must be specified.")
  }

  if (is.null(ivs) || length(ivs) == 0) {
    stop("'ivs' (independent variables) must be specified as a character vector.")
  }

  # Ensure dependent and independent variables exist in df
  work::assert_cols_exist(df, dv, "data frame for bn_engine()")
  work::assert_cols_exist(df, unlist(ivs), "data frame for bn_engine()")

  # Check optional parameters
  assert_positive_integer(connections_max, "connections_max")

  if (!is.null(seed)) {
    work::assert_numeric_scalar(seed, "seed")
  }

  if (!is.null(connections_multiple_boot_ratio)) {
    work::assert_numeric_scalar(connections_multiple_boot_ratio, "connections_multiple_boot_ratio")
  }

  # Check for conflicting or incomplete configuration
  if (only_white_list && is.null(white_list)) {
    stop("You set 'only_white_list = TRUE' but did not provide a valid 'white_list'.")
  }




  ##############################
  # set up
  ##############################

  vars <- c(dv, ivs) %>% unlist() %>% setNames(NULL)


  if(!is.null(white_list)){
    white_list <- white_list %>% as.data.frame()
  }


  if(!is.null(black_list)){
    black_list <- black_list %>% as.data.frame()
  }


  dfx <- df %>% dplyr::select(dplyr::all_of(vars)) %>% as.data.frame()


  ##############################
  # start with tan
  ##############################

  if(!only_white_list){

    if(cross_battery_priority){

      temp_black_list <- ivs %>%
        purrr::map_dfr(make_arcs) %>%
        dplyr::bind_rows(black_list) %>%
        dplyr::distinct() %>%
        as.data.frame()

    }else if(!cross_battery_priority){

      temp_black_list <- if (is.null(black_list)) NULL else as.data.frame(black_list)

    }

    if (!is.null(seed)) set.seed(seed)
    white_list_base <- bnlearn::tree.bayes(
      x = dfx,
      training = dv,
      blacklist = temp_black_list,
      whitelist = white_list
    ) %>%
      bnlearn::arcs() %>%
      as.data.frame() %>%
      dplyr::filter(
        from != dv & to != dv
      ) %>%
      as.data.frame()

  }else if(only_white_list){

    if(is.null(white_list)){
      stop("You set only_white_list = TRUE but did not provide a valid white_list.")
    }

    white_list_base <- white_list %>% as.data.frame()
  }


  x_nodes <- white_list_base %>% unlist() %>% unique()




  ##############################
  # create connection to dv
  ##############################

  if(all_ivs_connect_to_dv && !only_white_list){

    dv_arcs <- data.frame(
      from = dv,
      to = x_nodes
    )

    white_list_base <- dplyr::bind_rows(dv_arcs, white_list_base) %>% as.data.frame()

  }else if(!all_ivs_connect_to_dv && !only_white_list && !is.null(force_dv_connection)){


    dv_arcs <- data.frame(
      from = dv,
      to = force_dv_connection
    )

    white_list_base <- dplyr::bind_rows(dv_arcs, white_list_base) %>% as.data.frame()


  }else if(!all_ivs_connect_to_dv && !only_white_list && is.null(force_dv_connection)){


    if (!is.null(seed)) set.seed(seed)
    dv_arcs_select <- bnlearn::tabu(
      x = dfx,
      score = "bic",
      blacklist =  x_nodes %>% make_arcs() %>%
        dplyr::bind_rows(
          data.frame(
            from = dv,
            to = x_nodes
          )
        ) %>%
        as.data.frame(),
      whitelist = white_list_base
    ) %>%
      bnlearn::arcs() %>%
      as.data.frame() %>%
      dplyr::filter(from == dv | to == dv) %>%
      as.data.frame()



    if (!is.null(seed)) set.seed(seed)
    dv_arcs_boot <- bnlearn::boot.strength(
      data = dfx,
      R = complexity_boot_n,
      algorithm = "tabu",
      algorithm.args = list(
        score = "bic",
        blacklist =  x_nodes %>% make_arcs() %>%
          dplyr::bind_rows(
            data.frame(
              from = dv,
              to = x_nodes
            )
          ) %>%
          dplyr::bind_rows(black_list) %>%
          as.data.frame()
        ,
        whitelist = white_list_base
      )
    ) %>%
      dplyr::filter(to == dv & direction == 1 & strength > .1) %>%
      dplyr::arrange(-strength) %>%
      as.data.frame()


    if(is.null(dv_connection_strength)){

      lowest_dv_cut_off <- which(dv_arcs_boot[["from"]] %in% dv_arcs_select[["from"]]) %>% max(na.rm = TRUE)

      dv_arcs <- data.frame(
        from = dv_arcs_boot[["from"]][seq(lowest_dv_cut_off)],
        to = dv
      )

    }else if(!is.null(dv_connection_strength)){

      dv_arcs <- dv_arcs_boot %>%
        dplyr::filter(strength >= dv_connection_strength) %>%
        dplyr::select(from, to) %>%
        as.data.frame()

    }



    intermediary_nodes <- dv_arcs %>% unlist() %>% dplyr::setdiff(dv)


    # Re-orient arcs so flow favors intermediary nodes toward DV
    white_list_base <- white_list_base %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        from_original = from,
        to_original = to,

        from = dplyr::case_when(
          all(c(from_original, to_original) %in% intermediary_nodes) ~ from_original,
          all((c(from_original, to_original) %in% intermediary_nodes) == c(FALSE, TRUE)) ~ from_original,
          all((c(from_original, to_original) %in% intermediary_nodes) == c(TRUE, FALSE)) ~ to_original,
          .default = from_original
        ),

        to = dplyr::case_when(
          all(c(from_original, to_original) %in% intermediary_nodes) ~ to_original,
          all((c(from_original, to_original) %in% intermediary_nodes) == c(FALSE, TRUE)) ~ to_original,
          all((c(from_original, to_original) %in% intermediary_nodes) == c(TRUE, FALSE)) ~ from_original,
          .default = to_original
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(from, to) %>%
      as.data.frame()


    # Ensure iv reachability to DV
    temp_arcs <- dplyr::bind_rows(dv_arcs, white_list_base) %>% dplyr::distinct()
    temp_bn <- temp_arcs %>% unlist() %>% unique() %>% bnlearn::empty.graph()
    bnlearn::arcs(temp_bn) <- temp_arcs

    temp_bn <- work::bn_ensure_reachability(bn = temp_bn, dv = dv, max_iter = reachability_max_iter)

    white_list_base <- temp_bn %>% bnlearn::arcs() %>% as.data.frame()
  }




  ##############################
  # model base model
  ##############################

  # --- fit base model (ensure all vars are in the graph) ---
  base_bn <- white_list_base %>% unlist() %>% unique() %>% bnlearn::empty.graph()
  if (nrow(white_list_base) == 0) stop("white_list_base has no rows")
  bnlearn::arcs(base_bn) <- white_list_base
  base_fit <- bnlearn::bn.fit(x = base_bn, data = dfx, method = "bayes")


  if(all_ivs_connect_to_dv){

    base_netviz <- work::bn_to_netviz_prep(
      obj = base_bn,
      df = dfx,
      dictionary = dictionary,
      manual_groups = manual_groups,
      remove_nodes = dv,
      node_label_type = node_label_type,
      n_groups = n_groups,
      node_size = node_size,
      tool_tip_edge_prefix = tool_tip_edge_prefix,
      on_exit_detach_igraph = FALSE
    )

  }else if(!all_ivs_connect_to_dv){

    if(!is.null(manual_groups)){

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

    }else if(is.null(manual_groups)){

      base_netviz <- work::bn_to_netviz_prep(
        obj = base_bn,
        df = dfx,
        dictionary = dictionary,
        manual_groups = NULL,
        remove_nodes = NULL,
        node_label_type = node_label_type,
        n_groups = n_groups,
        node_size = node_size,
        tool_tip_edge_prefix = tool_tip_edge_prefix,
        on_exit_detach_igraph = FALSE
      )


      max_group <- base_netviz[["attribute_viz_prep"]][["nodes"]][["group"]] %>% max()
      intermediary_nodes_number <- seq(length(intermediary_nodes))


      temp_manual_groups <- base_netviz[["attribute_viz_prep"]][["nodes"]] %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          group = dplyr::case_when(
            id %in% intermediary_nodes ~ (max_group + match(id, intermediary_nodes)),
            id == dv ~ (max_group + max(intermediary_nodes_number) + 1),
            .default = group
          ),

          community_name = dplyr::case_when(
            id %in% intermediary_nodes ~ id,
            id == dv ~ dv,
            .default = community_name
          ),

          label = dplyr::case_when(
            id == dv ~ label %>% gsub("- NA", "", .) %>% stringr::str_squish(),
            .default = label
          )
        ) %>%
        dplyr::ungroup() %>%
        as.data.frame()


      temp_group_colors <- temp_manual_groups[["group"]] %>%
        max() %>%
        work::bn_community_color() %>%
        dplyr::rename(new_color = color)


      temp_manual_groups <- temp_manual_groups %>%
        dplyr::left_join(temp_group_colors, by = dplyr::join_by(group)) %>%
        dplyr::mutate(color = new_color) %>%
        dplyr::select(-new_color)


      base_netviz <- work::bn_to_netviz_prep(
        obj = base_bn,
        df = dfx,
        dictionary = dictionary,
        manual_groups = temp_manual_groups,
        remove_nodes = NULL,
        node_label_type = node_label_type,
        n_groups = n_groups,
        node_size = node_size,
        tool_tip_edge_prefix = tool_tip_edge_prefix,
        on_exit_detach_igraph = FALSE
      )

    }

  }



  ##############################
  # allow to layer on more parents
  ##############################

  results_layers <- NULL

  if(connections_max > 1 && !only_white_list){

    white_list_layer <- list()
    white_list_layer[[1]] <- white_list_base %>% dplyr::setdiff(dv_arcs)


    white_list_layer <- work::bn_increase_complexity(
      df = dfx,
      dv = dv,
      white_list_base = white_list_base,
      black_list = black_list,
      dv_arcs = dv_arcs,
      connections_max = connections_max,
      bootstrap_reps = complexity_boot_n,
      algorithm = "tabu",
      score = "bic",
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

          temp_arcs <- dplyr::bind_rows(dv_arcs, .x)
          temp_bn <- temp_arcs %>% unlist() %>% unique() %>% bnlearn::empty.graph()
          bnlearn::arcs(temp_bn) <- temp_arcs

          temp_results[["bn"]] <- temp_bn
          temp_results[["fit"]] <- bnlearn::bn.fit(x = temp_bn, data = dfx, method = "bayes")

          temp_results[["viz_prep"]] <- work::bn_to_netviz_prep(
            obj = temp_bn,
            df = dfx,
            dictionary = dictionary,
            manual_groups = base_netviz[["attribute_viz_prep"]][["nodes"]],
            remove_nodes = dv,
            node_label_type = node_label_type,
            n_groups = n_groups,
            node_size = node_size,
            tool_tip_edge_prefix = tool_tip_edge_prefix,
            on_exit_detach_igraph = FALSE
          )


          temp_results[["summary"]] <- work::bn_summary_statistics(
            bn = temp_bn,
            df = dfx,
            dv = dv,
            fit = temp_results[["fit"]],
            compare_to_naive = compare_to_naive,
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
    dv = dv,
    fit = base_fit,
    compare_to_naive = compare_to_naive,
    suppress_bn_warning = TRUE
  )


  if(!is.null(results_layers)){
    results[["results_layers"]] <- results_layers
    results[["summary"]] <- results_layers[["summary"]]
  }


  results[["meta"]] <- list(
    analysis = "bn_model_single",
    dv = dv,
    ivs = ivs
  )


  # --- cleanup ---
  if(on_exit_detach_igraph) work::detach_igraph()

  if(!suppress_bn_warning) work::warning_bnlearn_bic()


  return(results)
}
