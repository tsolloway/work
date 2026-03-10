#' cluster_iterative
#'
#' @description Runs iterative greedy swap optimization for a single solution
#'   family across a range of cluster counts. Seeds each k with k-means on
#'   strategy variables, then optimizes threshold hits via [iterative_optimize()].
#'   Pipes results through [cluster_add_lda()] for discriminant analysis.
#'
#'   Matches the [cluster_kmeans()] interface and return shape so it integrates
#'   directly with [seg_cluster_input_sheet()] and [cluster_solution_family()].
#'
#' @param df A data frame containing shell data.
#' @param vars Character vector of source variable names to cluster on.
#' @param vars_profiles Character vector of polar profile variable names
#'   (binary indicators derived from source vars).
#' @param solution_name Character. Solution identifier (e.g. `"A"`).
#' @param solution_name_prefix Character. Prefix for cluster names
#'   (default: `"iter"`).
#' @param resp_id_name Character. Respondent ID column (default: `"seg_uuid"`).
#' @param filter_name Character. Column name for logical filter (e.g.
#'   `"okay_filter"`). `NULL` uses all rows.
#' @param n_min Integer. Minimum clusters (default: `4`).
#' @param n_max Integer. Maximum clusters (default: `7`).
#' @param reduced_inputs_max Integer. Max reduced input variables for
#'   discriminant analysis. `NULL` uses all.
#' @param priors Character. Prior probability method for LDA: `"equal"` or
#'   `"size"` (default: `"equal"`).
#' @param iter_max Integer. Max k-means iterations for seeding
#'   (default: `1000`).
#' @param nstart Integer. Number of random starts for k-means seeding
#'   (default: `10`).
#' @param lda_vars Character vector. Override LDA input variables. `NULL`
#'   uses `vars`.
#' @param lda_vars_profiles Character vector. Override LDA profile variables.
#'   `NULL` uses `vars_profiles`.
#' @param seed Integer. Random seed (default: `1`).
#' @param vars_shell Character vector. Shell profile variable names used in
#'   the evaluation matrix (e.g. from `seg_get_vars(seg, type = "profiles")`).
#' @param group_source_vars Named list. Each element is a character vector of
#'   source variable names for one polar group
#'   (e.g. `list(kg = kg_source, pr_tt = c(pr_source, tt_source))`).
#' @param group_polar_pvs Named list. Each element is a character vector of
#'   polar profile variable names for one group
#'   (e.g. `list(kg = kg_pv, pr_tt = c(pr_pv, tt_pv))`).
#' @param target Character. Which entry in `target_defs` to use as the
#'   optimization objective (default: `"total"`). This controls which polar
#'   batteries the optimizer tries to differentiate. Profile variables always
#'   count regardless.
#'
#'   For example, with three polar groups (KG, PR, TT) and:
#'   ```
#'   target_defs = list(
#'     total      = c("kg", "pr_tt"),   # count hits from all polars
#'     kg_only    = "kg",               # count only KG polar hits
#'     pr_tt_only = "pr_tt"             # count only PR/TT polar hits
#'   )
#'   ```
#'   Setting `target = "total"` tells the optimizer to maximize
#'   differentiation across all batteries. Setting `target = "kg_only"`
#'   makes the optimizer ignore PR/TT polar separation entirely — it will
#'   only move respondents when doing so improves KG differentiation (plus
#'   profile differentiation, which always counts). This is useful when one
#'   battery is the primary segmentation driver and others are secondary.
#'
#'   Note: `target` controls what the optimizer *counts*. `strategy`
#'   (separately) controls which source vars *seed the initial k-means*.
#'   You can seed with KG vars but optimize on all polars, or vice versa.
#' @param target_defs Named list. Each element is a character vector of
#'   group names (keys from `group_source_vars`) whose polar hits count
#'   toward that target. The `target` parameter selects which entry to use.
#'   `NULL` defaults to `list(total = names(group_source_vars))` — a single
#'   target that counts all groups.
#' @param strategy Character. Which strategy definition to use for k-means
#'   seeding. Must be a key in `strategy_defs`. `NULL` uses the first strategy.
#' @param strategy_defs Named list. Each element is a character vector of group
#'   names whose source vars seed k-means. `NULL` defaults to
#'   `list(all = names(group_source_vars))`.
#' @param polar_threshold Numeric. Polar hit threshold (default: `0.20`).
#' @param profile_threshold Numeric. Profile hit threshold (default: `0.15`).
#' @param min_seg_pct Numeric (0-1). Minimum segment size as fraction
#'   (default: `0.05`).
#' @param swap_max_iter Integer. Maximum optimizer iterations (default: `1000`).
#' @param weight_var Character. Weight variable column name. `NULL` for
#'   unweighted.
#'
#' @return A list with `all_inputs` and `reduced_inputs` tibbles (same shape
#'   as [cluster_kmeans()]).
#'
#' @export
cluster_iterative <- function(
    df,
    vars,
    vars_profiles,
    solution_name,
    solution_name_prefix = "iter",
    resp_id_name         = "seg_uuid",
    filter_name          = NULL,
    n_min                = 4,
    n_max                = 7,
    reduced_inputs_max   = NULL,
    priors               = c("equal", "size"),
    iter_max             = 1000,
    nstart               = 10,
    lda_vars             = NULL,
    lda_vars_profiles    = NULL,
    seed                 = 1,
    # ---- iterative-specific ----
    vars_shell,
    group_source_vars,
    group_polar_pvs,
    target               = "total",
    target_defs          = NULL,
    strategy             = NULL,
    strategy_defs        = NULL,
    polar_threshold      = 0.20,
    profile_threshold    = 0.15,
    min_seg_pct          = 0.05,
    swap_max_iter        = 1000,
    weight_var           = NULL
) {

  # df = seg[["data"]][["with_shell"]]
  # vars = all_source
  # vars_profiles = all_polar_pv
  # solution_name = "A"
  # solution_name_prefix = "iter"
  # resp_id_name = "seg_uuid"
  # filter_name = "okay_filter"
  # n_min = 4
  # n_max = 7
  # reduced_inputs_max = 14
  # priors = "equal"
  # iter_max = 1000
  # nstart = 10
  # seed = 1
  # vars_shell = seg_get_vars(seg, type = "profiles")
  # group_source_vars = list(kg = kg_source, pr_tt = c(pr_source, tt_source))
  # group_polar_pvs = list(kg = kg_pv, pr_tt = c(pr_pv, tt_pv))
  # target = "total"
  # target_defs = list(total = c("kg", "pr_tt"))
  # strategy = "all"
  # strategy_defs = list(all = c("kg", "pr_tt"))


  if (!is.null(seed)) set.seed(seed)
  priors <- match.arg(priors)


  # ---- defaults ----

  if (is.null(target_defs)) {
    target_defs <- list(total = names(group_source_vars))
  }
  if (is.null(strategy_defs)) {
    strategy_defs <- list(all = names(group_source_vars))
  }
  if (is.null(strategy)) {
    strategy <- names(strategy_defs)[1]
  }

  # ---- auto-scope to family's variables ----
  # For input-sheet families, this scopes the eval matrix to only the polar
  # groups present in the family's vars/vars_profiles. For optimized strategies
  # (which pass all vars), this is a no-op.

  group_polar_pvs <- lapply(group_polar_pvs, function(pvs) {
    pvs[pvs %in% vars_profiles]
  })
  group_polar_pvs <- Filter(length, group_polar_pvs)

  group_source_vars <- lapply(group_source_vars, function(svs) {
    svs[svs %in% vars]
  })
  group_source_vars <- Filter(length, group_source_vars)

  remaining_polar_groups  <- names(group_polar_pvs)
  remaining_source_groups <- names(group_source_vars)

  target_defs <- lapply(target_defs, function(groups) {
    intersect(groups, remaining_polar_groups)
  })

  strategy_defs <- lapply(strategy_defs, function(groups) {
    intersect(groups, remaining_source_groups)
  })

  strategy_source_vars <- lapply(strategy_defs, function(groups) {
    unlist(group_source_vars[groups], use.names = FALSE)
  })

  cluster_vars <- strategy_source_vars[[strategy]]


  # ---- filter + na.exclude (same as cluster_kmeans) ----

  if (!is.null(filter_name)) {
    df_filtered <- df %>% dplyr::filter(.data[[filter_name]])
  } else {
    df_filtered <- df
  }

  df_temp <- df_filtered %>%
    dplyr::select(dplyr::all_of(c(resp_id_name, vars))) %>%
    na.exclude()

  id      <- df[[resp_id_name]]
  id_temp <- df_temp[[resp_id_name]]

  df_temp <- df_temp %>% dplyr::select(-dplyr::all_of(resp_id_name))


  # ---- lda_vars handling (same as cluster_kmeans) ----

  if (!is.null(lda_vars)) {
    reduced_vars          <- vars[vars %in% lda_vars]
    reduced_vars_profiles <- vars_profiles[vars_profiles %in% lda_vars_profiles]
  } else {
    reduced_vars          <- vars
    reduced_vars_profiles <- vars_profiles
  }


  # ---- evaluation matrix for optimizer ----
  # Same respondents as df_temp (complete source vars), but with all eval columns.

  df_eval <- df_filtered %>%
    dplyr::filter(.data[[resp_id_name]] %in% id_temp)

  all_polar_pvs_scoped <- unlist(group_polar_pvs, use.names = FALSE)
  all_eval_vars <- c(all_polar_pvs_scoped, vars_shell)
  all_eval_vars <- all_eval_vars[all_eval_vars %in% names(df_eval)]

  data_mat_raw <- as.matrix(df_eval[, all_eval_vars, drop = FALSE])
  non_na_mat   <- !is.na(data_mat_raw)
  data_mat     <- data_mat_raw
  data_mat[is.na(data_mat)] <- 0

  if (!is.null(weight_var) && weight_var %in% names(df_eval) && nchar(weight_var) > 0) {
    w <- df_eval[[weight_var]]
    w[is.na(w)] <- 1
  } else {
    w <- rep(1, nrow(df_eval))
  }

  polar_idx     <- which(all_eval_vars %in% all_polar_pvs_scoped)
  prof_idx      <- which(all_eval_vars %in% vars_shell)
  group_col_idx <- lapply(group_polar_pvs, function(pvs) {
    which(all_eval_vars %in% pvs)
  })

  cat(sprintf("Eval matrix: %d respondents x %d variables\n", nrow(df_eval), length(all_eval_vars)))
  cat(sprintf("  Polar cols: %d   Profile cols: %d\n", length(polar_idx), length(prof_idx)))
  cat(sprintf("  Target: %s   Strategy: %s\n\n", target, strategy))


  # ---- build result tibble (matches cluster_kmeans structure) ----

  result <- tibble::tibble("n" = n_min:n_max) %>%
    dplyr::mutate(
      "solution_name" = solution_name,
      "cluster_name"  = glue::glue("{solution_name_prefix}_{solution_name}{n}"),
      "inputs"        = list(vars),
      "profiles"      = list(vars_profiles),

      "cluster_fit" = purrr::map(n, purrr::possibly(~{

        if (!is.null(seed)) set.seed(seed)

        # kmeans seed (on strategy vars)
        km_data <- as.matrix(df_eval[, cluster_vars, drop = FALSE])
        km <- stats::kmeans(km_data, .x, iter.max = iter_max, nstart = nstart)

        cat(sprintf("--- k=%d ---\n", .x))
        cat(sprintf("  kmeans seed: sizes=%s\n", paste(table(km$cluster), collapse = "/")))

        # iterative optimization
        opt <- iterative_optimize(
          data_mat          = data_mat,
          non_na_mat        = non_na_mat,
          w                 = w,
          init_assign       = km$cluster,
          polar_idx         = polar_idx,
          prof_idx          = prof_idx,
          group_col_idx     = group_col_idx,
          target_groups     = target_defs[[target]],
          polar_threshold   = polar_threshold,
          profile_threshold = profile_threshold,
          min_seg_pct       = min_seg_pct,
          max_iter          = swap_max_iter,
          verbose           = TRUE
        )

        list(km = km, opt = opt)

      }, otherwise = NA)),

      "cluster_seed" = purrr::map2(cluster_fit, cluster_name, purrr::possibly(~{
        purrr::pluck(.x, "opt", "assignments") %>%
          dplyr::bind_cols(id_temp, .) %>%
          setNames(c("id", .y)) %>%
          suppressMessages()
      }, otherwise = NA)),

      "cluster_glance" = purrr::map(cluster_fit, purrr::possibly(
        ~broom::glance(.x$km), otherwise = NA
      )),

      "priors_equal" = purrr::map(n, ~rep(1 / .x, .x)),
      "priors_size"  = purrr::map2(cluster_seed, cluster_name,
        ~.x[[.y]] %>% table_percent()
      ),

      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~{
        if (!is.null(seed)) set.seed(seed)
        cluster_reduce_vars(
          df_temp, reduced_vars, .x[[.y]],
          type = "greedy_step", return_only_var = TRUE, seed = seed
        )
      }, otherwise = NA)),

      "reduced_profiles" = purrr::map(
        reduced_inputs,
        ~reduced_vars_profiles[match(.x, reduced_vars)]
      )
    )


  # ---- cap reduced inputs ----

  if (!is.null(reduced_inputs_max)) {
    result <- result %>%
      dplyr::mutate(
        "reduced_inputs"  = purrr::map(reduced_inputs, head, reduced_inputs_max),
        "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
      )
  }


  # ---- LDA (all inputs) ----

  if (!is.null(seed)) set.seed(seed)
  result_all <- result %>%
    cluster_add_lda(
      df                = df,
      resp_id_name      = resp_id_name,
      filter_name       = filter_name,
      priors            = priors,
      use_reduced       = FALSE,
      lda_vars          = reduced_vars,
      lda_vars_profiles = reduced_vars_profiles
    )


  # ---- LDA (reduced inputs) ----

  if (!is.null(seed)) set.seed(seed)
  result_reduced <- result %>%
    cluster_add_lda(
      df           = df,
      resp_id_name = resp_id_name,
      filter_name  = filter_name,
      priors       = priors,
      use_reduced  = TRUE
    )


  output <- list(
    all_inputs     = result_all,
    reduced_inputs = result_reduced
  )

  return(output)
}
