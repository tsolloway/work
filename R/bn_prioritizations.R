#' bn_prioritizations
#'
#' @description Convenience wrapper around \code{bn_prioritize()} that runs
#'   multiple strategy/weight/brand variants and returns them in a single list.
#'   Runs greedy max, greedy lift (unweighted), greedy lift (weighted if
#'   \code{weight} is provided), and per-brand lift (if \code{brand} is
#'   provided). When \code{process_subgroups = TRUE}, each variant is computed
#'   for every subgroup.
#'
#' @param obj A Bayesian network object. Accepts the full output of
#'   \code{bn_finalize_network()} (with \code{bn_subgroups}), a single
#'   subgroup element, or a bare \code{bnlearn::bn.fit} object.
#' @param df Data frame containing the DV, IV, and optionally brand/weight
#'   columns.
#' @param dictionary Optional. Dictionary for variable labels.
#' @param dv Character. Dependent variable name. If NULL, auto-detected from
#'   \code{obj$meta$dv}.
#' @param ivs Character vector or NULL. Independent variable names. If NULL,
#'   auto-detected from \code{obj$meta$ivs}.
#' @param ivs_excluded Character vector or NULL. Variables to exclude from
#'   the analysis. Default NULL.
#' @param process_subgroups Logical. If TRUE and \code{obj} contains
#'   \code{bn_subgroups}, iterate over each subgroup (filtering \code{df}
#'   to rows where \code{df[[subgroup_name]] == 1}). If FALSE, compute once
#'   on the full data. Default TRUE.
#' @param community_assignment Optional. Community assignment node table (e.g.,
#'   \code{bn$viz_prep$attribute_viz_prep$nodes}) with \code{id} and
#'   \code{community_name} columns. When provided, a \code{community} column
#'   is added to each result tibble after \code{variable}.
#' @param lift Numeric. Distribution shift for lift strategy. Default 0.10.
#' @param min_base_for_boot Integer. Minimum sample size to run bootstrap
#'   p-values and to include a brand-subgroup slice as a task. Default 75.
#' @param dv_metric Character. \code{"mean"} (default) or \code{"top_box"}.
#' @param impact_metric_type Character. \code{"proportional"} (default) or
#'   \code{"absolute"}.
#' @param impact_result Optional. Output of \code{bn_impact()} passed through
#'   to seed greedy round 1 ordering.
#' @param threshold Numeric or NULL. Early stopping threshold. Default 0.01.
#' @param max_rounds Integer or NULL. Maximum priority rounds. Default NULL.
#' @param noise_tail Numeric. Fraction of tail steps for noise floor.
#'   Default 1/3.
#' @param n_boot_final Integer. Bootstrap replicates for p-values.
#'   Default 100.
#' @param brand Character or NULL. Column name in \code{df} containing brand
#'   labels. When provided, lift is computed separately for each brand using
#'   brand-filtered data. Default NULL.
#' @param brand_names Character vector or NULL. When provided, only compute
#'   brand-specific lift for these brand levels. Default NULL (all brands).
#' @param weight Character or NULL. Column name in \code{df} containing
#'   observation weights. When provided, adds weighted lift variants.
#'   Default NULL.
#' @param use_parallel Logical. Parallelize candidate evaluation. Default TRUE.
#' @param seed Integer. Random seed. Default 1.
#'
#' @return A list with:
#' \describe{
#'   \item{greedy_max}{Greedy max results. A tibble (single subgroup) or
#'     named list of tibbles (per subgroup).}
#'   \item{greedy_lift}{Greedy lift results (unweighted).}
#'   \item{greedy_lift_weighted}{Greedy lift results (weighted). NULL if
#'     \code{weight} is NULL.}
#'   \item{greedy_lift_brand}{Named list of per-brand lift results. NULL if
#'     \code{brand} is NULL. Each element is a tibble (single subgroup) or
#'     named list of tibbles (per subgroup).}
#'   \item{greedy_lift_brand_weighted}{Named list of per-brand weighted lift
#'     results. NULL if \code{brand} or \code{weight} is NULL.}
#'   \item{meta}{Shared metadata: dv, subgroups, brand, brand_names, weight,
#'     lift.}
#' }
#'
#' @seealso [bn_prioritize()], [bn_impact()], [bn_impacts()]
#'
#' @export
bn_prioritizations <- function(
    obj,
    df,
    dictionary = NULL,
    dv = NULL,
    ivs = NULL,
    ivs_excluded = NULL,
    process_subgroups = TRUE,
    community_assignment = NULL,
    lift = 0.10,
    min_base_for_boot = 75,
    dv_metric = c("mean", "top_box"),
    impact_metric_type = c("proportional", "absolute"),
    impact_result = NULL,
    threshold = 0.01,
    max_rounds = NULL,
    noise_tail = 1/3,
    n_boot_final = 100,
    brand = NULL,
    brand_names = NULL,
    weight = NULL,
    use_parallel = TRUE,
    seed = 1
) {

  dv_metric <- match.arg(dv_metric)
  impact_metric_type <- match.arg(impact_metric_type)

  # ---------------------------------------------------------------------------
  # Resolve subgroups
  # ---------------------------------------------------------------------------
  if (process_subgroups && "bn_subgroups" %in% names(obj)) {
    sg_list <- obj[["bn_subgroups"]]
    sg_names <- names(sg_list)
  } else {
    sg_list <- list(Total = obj)
    sg_names <- "Total"
  }

  # ---------------------------------------------------------------------------
  # Resolve brands
  # ---------------------------------------------------------------------------
  if (!is.null(brand) && brand %in% names(df)) {
    all_brands <- sort(unique(as.character(df[[brand]])))
    brands_to_run <- if (!is.null(brand_names)) intersect(all_brands, brand_names) else all_brands
  } else {
    brands_to_run <- NULL
  }

  # ---------------------------------------------------------------------------
  # Build flat task list: each element is one bn_prioritize() call
  # ---------------------------------------------------------------------------
  tasks <- list()

  for (sg_name in sg_names) {
    sg_obj <- sg_list[[sg_name]]

    if (process_subgroups && sg_name %in% names(df)) {
      sg_df <- df %>%
        dplyr::filter(.data[[sg_name]] == 1) %>%
        as.data.frame()
    } else {
      sg_df <- as.data.frame(df)
    }

    # Greedy max
    tasks[[paste0("max__", sg_name)]] <- list(
      type = "max", sg_name = sg_name, brand_name = NULL,
      sg_obj = sg_obj, sg_df = sg_df, strategy = "max", wt = NULL
    )

    # Greedy lift (unweighted)
    tasks[[paste0("lift__", sg_name)]] <- list(
      type = "lift", sg_name = sg_name, brand_name = NULL,
      sg_obj = sg_obj, sg_df = sg_df, strategy = "lift", wt = NULL
    )

    # Greedy lift (weighted)
    if (!is.null(weight)) {
      tasks[[paste0("lift_weighted__", sg_name)]] <- list(
        type = "lift_weighted", sg_name = sg_name, brand_name = NULL,
        sg_obj = sg_obj, sg_df = sg_df, strategy = "lift", wt = weight
      )
    }

    # Per-brand lift
    if (!is.null(brands_to_run)) {
      for (b in brands_to_run) {
        brand_df <- sg_df[sg_df[[brand]] == b, , drop = FALSE]
        if (nrow(brand_df) < min_base_for_boot) next

        tasks[[paste0("brand__", b, "__", sg_name)]] <- list(
          type = "brand", sg_name = sg_name, brand_name = b,
          sg_obj = sg_obj, sg_df = brand_df, strategy = "lift", wt = NULL
        )

        # Per-brand lift (weighted)
        if (!is.null(weight)) {
          tasks[[paste0("brand_weighted__", b, "__", sg_name)]] <- list(
            type = "brand_weighted", sg_name = sg_name, brand_name = b,
            sg_obj = sg_obj, sg_df = brand_df, strategy = "lift", wt = weight
          )
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Run all tasks in parallel (inner parallelism off)
  # ---------------------------------------------------------------------------
  cli::cli_alert_info("Running {length(tasks)} prioritization tasks in parallel")

  task_results <- imap_progress(
    tasks,
    function(task, task_name) {
      bn_prioritize(
        obj = task$sg_obj, df = task$sg_df, dv = dv, ivs = ivs,
        ivs_excluded = ivs_excluded,
        strategy = task$strategy, search = "greedy",
        impact_result = impact_result,
        dv_metric = dv_metric, lift = lift,
        impact_metric_type = impact_metric_type,
        threshold = threshold, max_rounds = max_rounds,
        n_boot_final = n_boot_final, noise_tail = noise_tail,
        min_base_for_boot = min_base_for_boot,
        weight = task$wt, dictionary = dictionary,
        use_parallel = FALSE, verbose = FALSE, seed = seed
      )
    },
    .parallel = use_parallel,
    .label = "Prioritizations"
  )

  # ---------------------------------------------------------------------------
  # Reassemble results into structured output
  # ---------------------------------------------------------------------------
  results_max <- list()
  results_lift <- list()
  results_lift_weighted <- if (!is.null(weight)) list() else NULL
  results_lift_brand <- if (!is.null(brands_to_run)) list() else NULL
  results_lift_brand_weighted <- if (!is.null(brands_to_run) && !is.null(weight)) list() else NULL
  base_sizes <- list()

  for (task_name in names(task_results)) {
    task <- tasks[[task_name]]
    res <- task_results[[task_name]]
    base_sizes[[task_name]] <- attr(res, "n_obs")

    if (task$type == "max") {
      results_max[[task$sg_name]] <- res
    } else if (task$type == "lift") {
      results_lift[[task$sg_name]] <- res
    } else if (task$type == "lift_weighted") {
      results_lift_weighted[[task$sg_name]] <- res
    } else if (task$type == "brand") {
      if (is.null(results_lift_brand[[task$brand_name]])) results_lift_brand[[task$brand_name]] <- list()
      results_lift_brand[[task$brand_name]][[task$sg_name]] <- res
    } else if (task$type == "brand_weighted") {
      if (is.null(results_lift_brand_weighted[[task$brand_name]])) results_lift_brand_weighted[[task$brand_name]] <- list()
      results_lift_brand_weighted[[task$brand_name]][[task$sg_name]] <- res
    }
  }

  # ---------------------------------------------------------------------------
  # Simplify single-subgroup case: unwrap inner lists to bare tibbles
  # ---------------------------------------------------------------------------
  if (length(sg_names) == 1) {
    results_max <- results_max[[1]]
    results_lift <- results_lift[[1]]
    if (!is.null(weight)) results_lift_weighted <- results_lift_weighted[[1]]
    if (!is.null(brands_to_run)) {
      for (b in names(results_lift_brand)) {
        results_lift_brand[[b]] <- results_lift_brand[[b]][[1]]
      }
    }
    if (!is.null(brands_to_run) && !is.null(weight)) {
      for (b in names(results_lift_brand_weighted)) {
        results_lift_brand_weighted[[b]] <- results_lift_brand_weighted[[b]][[1]]
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Add community assignment (variable → community name)
  # ---------------------------------------------------------------------------
  if (!is.null(community_assignment)) {
    comm_lookup <- community_assignment %>%
      dplyr::mutate(community_name = as.character(community_name)) %>%
      dplyr::select(id, community_name)

    .add_community <- function(tbl) {
      if (is.data.frame(tbl)) {
        tbl %>%
          dplyr::left_join(comm_lookup, by = dplyr::join_by(variable == id)) %>%
          dplyr::relocate(community_name, .after = variable) %>%
          dplyr::rename(community = community_name)
      } else if (is.list(tbl)) {
        purrr::map(tbl, .add_community)
      } else {
        tbl
      }
    }

    results_max <- .add_community(results_max)
    results_lift <- .add_community(results_lift)
    if (!is.null(results_lift_weighted)) results_lift_weighted <- .add_community(results_lift_weighted)
    if (!is.null(results_lift_brand)) {
      results_lift_brand <- purrr::map(results_lift_brand, .add_community)
    }
    if (!is.null(results_lift_brand_weighted)) {
      results_lift_brand_weighted <- purrr::map(results_lift_brand_weighted, .add_community)
    }
  }

  # ---------------------------------------------------------------------------
  # Assemble output
  # ---------------------------------------------------------------------------
  output <- list(
    greedy_max = results_max,
    greedy_lift = results_lift
  )

  if (!is.null(weight)) output$greedy_lift_weighted <- results_lift_weighted
  if (!is.null(brands_to_run)) output$greedy_lift_brand <- results_lift_brand
  if (!is.null(brands_to_run) && !is.null(weight)) output$greedy_lift_brand_weighted <- results_lift_brand_weighted

  # Only report brands that actually produced results (slices meeting
  # the min_base_for_boot threshold)
  brands_with_results <- if (!is.null(results_lift_brand)) names(results_lift_brand) else NULL

  output$meta <- list(
    dv = dv,
    subgroups = if (length(sg_names) > 1) sg_names else NULL,
    brand = brand,
    brand_names = brands_with_results,
    weight = weight,
    lift = lift,
    base_sizes = base_sizes
  )

  cli::cli_alert_success("All {length(tasks)} prioritizations complete")

  output
}
