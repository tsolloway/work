#' seg_cluster_input_sheet
#'
#' @description Runs clustering across all solution families (or a single
#'   family) defined in the input sheet. For each family, calls
#'   [cluster_solution_family()] with the selected variables, applying one or
#'   more clustering methods across a range of cluster counts.
#'
#'   ## Clustering methods
#'
#'   Seven methods are available, each toggled by a `do_*` flag:
#'
#'   **K-means** (`do_kmeans`, default `TRUE`). Partitions respondents by
#'   minimizing within-cluster variance. Fast, scalable, the standard
#'   workhorse. Uses `stats::kmeans()` with `nstart` random initializations.
#'
#'   **Medoid / PAM** (`do_medoid`, default `TRUE`). Partitioning Around
#'   Medoids. Like k-means but uses actual data points as cluster centers.
#'   More robust to outliers. Uses `cluster::pam()`.
#'
#'   **Gaussian mixture** (`do_gaus_mix`, default `TRUE`). Fits a mixture of
#'   k multivariate Gaussians via EM. Soft assignment (probabilities), then
#'   hard-assigned to most probable cluster. Can capture elliptical clusters.
#'   Uses `mclust::Mclust()`.
#'
#'   **Hierarchical** (`do_hierarchical`, default `TRUE`). Agglomerative
#'   bottom-up clustering using Ward's method. Produces a dendrogram; cutting
#'   at different heights gives different k values without re-running.
#'   Deterministic. Uses `stats::hclust()` + `stats::cutree()`.
#'
#'   **Spectral** (`do_spectral`, default `FALSE`). Embeds respondents into
#'   eigenvector space of an RBF similarity matrix, then clusters in that
#'   space. Can find non-convex cluster shapes that distance-based methods
#'   miss. Uses `kernlab::specc()`.
#'
#'   **Iterative** (`do_iterative`, default `FALSE`). Greedy swap optimizer
#'   that directly maximizes threshold hits — the number of variables where a
#'   segment's mean differs from everyone else's mean by more than a threshold.
#'   Seeds from k-means, then iteratively moves respondents to maximize
#'   differentiation. Unlike the other methods which optimize generic
#'   statistical criteria, this one optimizes the metric that determines
#'   whether a solution is practically useful. Only uses source vars (no RS
#'   variant). Configured via `iterative_config`. See [iterative_optimize()]
#'   for the engine details.
#'
#'   **Consensus** (`do_consensus`, default `FALSE`). Combines assignments from
#'   all other enabled methods via a co-association matrix. For each pair of
#'   respondents, counts the fraction of methods that placed them together,
#'   then runs hierarchical clustering on the dissimilarity matrix. Stabilizes
#'   results across methods. Requires ≥ 2 methods. No external dependency.
#'   See [cluster_consensus()].
#'
#'   ## How methods are crossed
#'
#'   K-means, medoid, Gaussian mixture, and hierarchical are each crossed
#'   with `polar_type` (RS vs source vs both) and `priors` (equal vs size vs
#'   both). Iterative only varies by priors (it always uses source vars).
#'
#'   ## Post-processing
#'
#'   All methods share the same post-processing: variable reduction
#'   (`klaR::greedy.wilks` + `klaR::stepclass`), then LDA via
#'   [cluster_add_lda()] which trains a linear discriminant typing tool and
#'   computes confusion matrix accuracy.
#'
#' @param seg A seg object with data, spec, and solution inputs populated.
#' @param solution_family Character. A single solution letter (e.g. `"A"`) to
#'   re-run. If `NULL` (default), runs all solution families in parallel.
#' @param n_min Integer. Minimum number of clusters to evaluate (default: `4`).
#' @param n_max Integer. Maximum number of clusters to evaluate (default: `7`).
#' @param reduced_inputs_max Integer. Maximum number of reduced input variables
#'   for discriminant analysis (default: `14`).
#' @param filter_logical_vector Logical vector or column name to subset
#'   respondents before clustering. `NULL` uses all rows.
#' @param vary_percent Numeric (0–1). Fraction of respondents to flag at each
#'   tail of the variability distribution (default: `0.1`).
#' @param side_bias_percent Numeric (0–1). Fraction of respondents to flag at
#'   each tail of the side-bias distribution (default: `0.1`).
#' @param priors Character. Prior probability method for LDA: `"both"` (default,
#'   runs both equal and size-proportional priors), `"equal"` (each segment
#'   equally likely), or `"size"` (priors proportional to segment sizes).
#' @param polar_type Character. Which polar variables to cluster on:
#'   `"both"` (default, runs RS and source separately), `"rs"` (rescaled
#'   only), or `"source"` (source only). Does not affect iterative method.
#' @param resp_id_name Character. Respondent ID column name. Auto-detected if
#'   `NULL`.
#' @param iter_max Integer. Maximum iterations for k-means (default: `1000`).
#' @param nstart Integer. Number of random starts for k-means (default: `10`).
#' @param ok_filter Logical. If `TRUE` (default), applies variability and
#'   side-bias filtering before clustering.
#' @param do_kmeans Logical. Run k-means clustering (default: `TRUE`).
#' @param do_medoid Logical. Run medoid / PAM clustering (default: `TRUE`).
#' @param do_gaus_mix Logical. Run Gaussian mixture clustering
#'   (default: `TRUE`).
#' @param do_hierarchical Logical. Run hierarchical clustering
#'   (default: `TRUE`).
#' @param do_spectral Logical. Run spectral clustering (default: `FALSE`).
#'   Uses `kernlab::specc()` with automatic kernel bandwidth estimation.
#' @param do_iterative Logical. Run iterative greedy swap optimization
#'   (default: `FALSE`). Uses source vars only. Configure via
#'   `iterative_config`.
#' @param do_consensus Logical. Run consensus clustering (default: `FALSE`).
#'   Combines assignments from all other enabled methods via a co-association
#'   matrix. Requires ≥ 2 methods to have run. Runs after all other methods.
#'   See [cluster_consensus()].
#' @param do_optimized Logical. Run three `clust_optimized_*` strategies
#'   (default: `FALSE`). Requires `do_iterative = TRUE`. These run the
#'   iterative optimizer independently of input-sheet families, using all
#'   spec variables with different eval matrix compositions:
#'   - `clust_optimized_polar` — optimizer evaluates all polar pvs only
#'   - `clust_optimized_profile` — optimizer evaluates shell profiles only
#'   - `clust_optimized_all` — optimizer evaluates both polar pvs + profiles
#'   All three seed k-means with all source vars and end with LDA respecting
#'   `reduced_inputs_max`.
#' @param iterative_config List. Optional overrides for iterative optimization.
#'   All have sensible defaults derived from `seg`. Available keys:
#'   \describe{
#'     \item{polar_groups}{Named list of prefix vectors. Each element maps a
#'       group name to one or more battery prefixes
#'       (e.g. `list(kg = "KG", pr_tt = c("PR", "TT"))`). Default: one group
#'       per polars block in the spec.}
#'     \item{target}{Character. Which `target_defs` entry to use as the
#'       optimization objective (default: `"total"`). Controls which polar
#'       batteries the optimizer tries to differentiate. The optimizer only
#'       counts hits from groups listed in `target_defs[[target]]`. Profile
#'       hits always count regardless. Independent of `strategy`, which
#'       controls the k-means seed. You can seed with KG vars but optimize
#'       on all polars, or vice versa.}
#'     \item{target_defs}{Named list. Each entry is a character vector of
#'       group names whose polar hits count toward that target. The `target`
#'       key selects which entry to use. Default:
#'       `list(total = names(polar_groups))`.}
#'     \item{strategy}{Character. Which `strategy_defs` entry to use for
#'       k-means seeding. Default: first strategy.}
#'     \item{strategy_defs}{Named list. Each entry specifies which groups'
#'       source vars seed the initial k-means. Default:
#'       `list(all = names(polar_groups))`.}
#'     \item{polar_threshold}{Numeric. Hit threshold for polar vars
#'       (default: `0.20`).}
#'     \item{profile_threshold}{Numeric. Hit threshold for profile vars
#'       (default: `0.15`).}
#'     \item{min_seg_pct}{Numeric (0-1). Minimum segment size as fraction
#'       of n (default: `0.05`).}
#'     \item{swap_max_iter}{Integer. Maximum optimizer iterations
#'       (default: `500`).}
#'   }
#' @param strategy Character. Parallelization strategy passed to
#'   [future_plan()] (default: `"multisession"`).
#' @param workers Integer. Number of parallel workers. `NULL` auto-detects.
#' @param seed Integer. Random seed for reproducibility (default: `1`).
#'
#' @return The seg object with clustering results populated:
#'   `seg[["solutions"]][["analysis"]]` (raw results per family),
#'   `seg[["solutions"]][["summary_table"]]` (one row per solution with
#'   accuracy, LDA info), `seg[["solutions"]][["df_segment_append"]]`
#'   (segment assignments), and `seg[["data"]][["with_solutions"]]` (full
#'   data frame with segment columns appended).
#'
#' @export
seg_cluster_input_sheet <- function(
    seg,
    solution_family = NULL,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = 14,
    filter_logical_vector = NULL,
    vary_percent = .1,
    side_bias_percent = .1,
    priors = c("both", "equal", "size"),
    polar_type = c("both", "rs", "source"),
    resp_id_name = NULL,
    iter_max = 1000,
    nstart = 10,
    ok_filter = TRUE,
    do_kmeans = TRUE,
    do_medoid = TRUE,
    do_gaus_mix = FALSE,
    do_hierarchical = FALSE,
    do_spectral = FALSE,
    do_iterative = FALSE,
    do_consensus = FALSE,
    do_optimized = FALSE,
    iterative_config = list(),
    strategy = c("multisession", "multicore", "sequential", 'cluster'),
    workers = NULL,
    seed = 1
){


  # solution_family = "A"
  # n_min = 4
  # n_max = 5
  # reduced_inputs_max = 14
  # filter_logical_vector = NULL
  # vary_percent = .1
  # side_bias_percent = .1
  # priors = "equal"
  # resp_id_name = NULL
  # iter_max = 100000
  # nstart = 10
  # ok_filter = TRUE
  # do_kmeans = TRUE
  # do_medoid = TRUE
  # do_gaus_mix = TRUE
  # do_hierarchical = TRUE
  # strategy = "multisession"
  # workers = NULL


  priors <- match.arg(priors)
  polar_type <- match.arg(polar_type)

  # ---- cli: config summary ----
  methods_enabled <- c(
    if (do_kmeans) "kmeans", if (do_medoid) "medoid",
    if (do_gaus_mix) "gaus_mix", if (do_hierarchical) "hierarchical",
    if (do_spectral) "spectral", if (do_iterative) "iterative",
    if (do_consensus) "consensus"
  )
  n_families <- if (is.null(solution_family)) length(seg[["solutions"]][["inputs"]]) else length(solution_family)
  cli::cli_h2("seg_cluster_input_sheet")
  cli::cli_alert_info("Methods: {.val {methods_enabled}}")
  cli::cli_alert_info("Families: {.val {n_families}} | K range: {n_min}\u2013{n_max} | Priors: {.val {priors}} | Polar type: {.val {polar_type}}")
  if (do_optimized) cli::cli_alert_info("Optimized strategies: polar, profile, all")

  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }


  df <- seg[["data"]][["with_shell"]]


  if(ok_filter){
    filter_name <- "okay_filter"

    if(!filter_name %in% names(df)){
      cli::cli_alert("Computing variability & side-bias filters\u2026")

      df <- df %>%
        seg_cluster_variability(
          vars = seg_get_vars_polars(seg, .return="rs"),
          vary_percent = vary_percent,
          side_bias_percent = side_bias_percent,
          filter_logical_vector = filter_logical_vector,
          var_id = resp_id_name
        )

      df <- df[["df"]]

      seg[["data"]][["with_shell"]] <- df
    }
  }


  source_vars <- seg_get_vars_polars(seg, .return = "sources")
  solution_inputs <- seg[["solutions"]][["inputs"]]


  # ---- iterative auto-derivation ----

  iter_vars    <- NULL
  polars_table <- NULL

  if (do_iterative) {

    polars_table <- seg[["spec"]][["polars_table"]]
    all_blocks   <- seg_get_vars_polars(seg, .return = "blocks")

    # polar_groups: each block = one group (user can override)
    polar_groups <- iterative_config[["polar_groups"]]
    if (is.null(polar_groups)) {
      polar_groups <- stats::setNames(
        as.list(all_blocks),
        tolower(all_blocks)
      )
    }

    # group_source_vars + group_polar_pvs: derived from polar_groups
    group_source_vars <- lapply(polar_groups, function(prefixes) {
      polars_table %>%
        dplyr::filter(grepl(
          paste0("^(", paste(prefixes, collapse = "|"), ")"),
          polars_table[["profile_var"]]
        )) %>%
        dplyr::pull(source_var)
    })

    group_polar_pvs <- lapply(polar_groups, function(prefixes) {
      polars_table %>%
        dplyr::filter(grepl(
          paste0("^(", paste(prefixes, collapse = "|"), ")"),
          polars_table[["profile_var"]]
        )) %>%
        dplyr::pull(profile_var)
    })

    vars_shell  <- seg_get_vars(seg, type = "profiles")
    weight_var  <- seg[["meta"]][["weight_variable"]]

    # extract config overrides with defaults
    iterative_target    <- iterative_config[["target"]]    %||% "total"
    target_defs         <- iterative_config[["target_defs"]]
    if (is.null(target_defs)) {
      target_defs <- list(total = names(polar_groups))
    }
    iterative_strategy  <- iterative_config[["strategy"]]
    strategy_defs_iter  <- iterative_config[["strategy_defs"]]
    if (is.null(strategy_defs_iter)) {
      strategy_defs_iter <- list(all = names(polar_groups))
    }
    polar_threshold     <- iterative_config[["polar_threshold"]]   %||% 0.20
    profile_threshold   <- iterative_config[["profile_threshold"]] %||% 0.15
    min_seg_pct         <- iterative_config[["min_seg_pct"]]       %||% 0.05
    swap_max_iter       <- iterative_config[["swap_max_iter"]]     %||% 500

    iter_vars <- list(
      vars_shell         = vars_shell,
      group_source_vars  = group_source_vars,
      group_polar_pvs    = group_polar_pvs,
      weight_var         = weight_var,
      iterative_target   = iterative_target,
      target_defs        = target_defs,
      iterative_strategy = iterative_strategy,
      strategy_defs      = strategy_defs_iter,
      polar_threshold    = polar_threshold,
      profile_threshold  = profile_threshold,
      min_seg_pct        = min_seg_pct,
      swap_max_iter      = swap_max_iter
    )
  }


  # trim df to only columns needed for clustering — the full with_shell
  # data frame carries all original survey columns which bloat the parallel
  # closure (~1 GB serialized to each worker)
  all_cluster_vars <- solution_inputs %>%
    purrr::map(~c(.x[["RS"]], .x[["Source"]], .x[["Profile"]])) %>%
    unlist() %>%
    unique()

  keep_cols <- unique(c(
    resp_id_name, "seg_uuid",
    all_cluster_vars,
    source_vars
  ))

  if(!is.null(filter_logical_vector) && is.character(filter_logical_vector)){
    keep_cols <- c(keep_cols, filter_logical_vector)
  }

  keep_cols <- c(keep_cols, intersect(
    c("okay_filter", "ok_variability", "ok_side_bias", "variability", "side_bias_sum"),
    names(df)
  ))

  # iterative needs shell profile vars + polar pvs in the worker df
  if (do_iterative) {
    keep_cols <- unique(c(
      keep_cols,
      iter_vars$vars_shell,
      unlist(iter_vars$group_source_vars, use.names = FALSE),
      unlist(iter_vars$group_polar_pvs, use.names = FALSE)
    ))
    if (!is.null(iter_vars$weight_var) && nchar(iter_vars$weight_var) > 0) {
      keep_cols <- c(keep_cols, iter_vars$weight_var)
    }
  }

  # optimized strategies use ALL source vars + polar pvs from polars_table
  if (do_optimized) {
    keep_cols <- unique(c(
      keep_cols,
      polars_table[["source_var"]],
      polars_table[["profile_var"]]
    ))
  }

  df_worker <- df %>% dplyr::select(dplyr::any_of(keep_cols))


  solutions_existing <- seg[["solutions"]][["analysis"]]

  if(is.null(solution_family)){

    solutions_new <- .cluster_parallel(
      solution_inputs = solution_inputs,
      df = df_worker, resp_id_name = resp_id_name,
      filter_logical_vector = filter_logical_vector,
      ok_filter = ok_filter,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, polar_type = polar_type,
      iter_max = iter_max, nstart = nstart,
      do_kmeans = do_kmeans, do_medoid = do_medoid,
      do_gaus_mix = do_gaus_mix, do_hierarchical = do_hierarchical,
      do_spectral = do_spectral,
      do_iterative = do_iterative, do_consensus = do_consensus,
      do_optimized = do_optimized,
      iter_vars = iter_vars, polars_table = polars_table,
      seed = seed, strategy = strategy, workers = workers
    )

  }else if (length(solution_family) > 1) {

    solutions_new <- .cluster_parallel(
      solution_inputs = solution_inputs[solution_family],
      df = df_worker, resp_id_name = resp_id_name,
      filter_logical_vector = filter_logical_vector,
      ok_filter = ok_filter,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, polar_type = polar_type,
      iter_max = iter_max, nstart = nstart,
      do_kmeans = do_kmeans, do_medoid = do_medoid,
      do_gaus_mix = do_gaus_mix, do_hierarchical = do_hierarchical,
      do_spectral = do_spectral,
      do_iterative = do_iterative, do_consensus = do_consensus,
      do_optimized = do_optimized,
      iter_vars = iter_vars, polars_table = polars_table,
      seed = seed, strategy = strategy, workers = workers
    )

  }else{

    # single family — run sequentially
    cli::cli_alert("Running family {.val {solution_family}} sequentially\u2026")
    solutions_new <- list()

    sf <- solution_family

    if(!is.null(seed)) set.seed(seed)

    csf_args <- list(
      df = df,
      source_vars = source_vars,
      inputs = solution_inputs[[sf]],
      solution_name = sf,
      resp_id_name = resp_id_name,
      filter_logical_vector = filter_logical_vector,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      vary_percent = vary_percent,
      side_bias_percent = side_bias_percent,
      priors = priors, iter_max = iter_max, nstart = nstart,
      do_kmeans = do_kmeans, do_medoid = do_medoid,
      do_gaus_mix = do_gaus_mix, do_hierarchical = do_hierarchical,
      do_spectral = do_spectral, do_iterative = do_iterative,
      do_consensus = do_consensus, polar_type = polar_type, seed = seed
    )

    if (do_iterative) {
      csf_args <- c(csf_args, iter_vars)
    }

    solutions_new[[sf]] <- do.call(cluster_solution_family, csf_args)


    # ---- optimized strategies (single-family path) ----
    if (do_iterative && do_optimized) {

      priors_list <- if (priors == "both") c("equal", "size") else priors

      all_source_vars <- polars_table[["source_var"]]
      all_polar_pvs   <- polars_table[["profile_var"]]
      all_shell_vars  <- iter_vars$vars_shell
      filter_name_opt <- if (ok_filter) "okay_filter" else NULL

      optimized_defs <- list(
        clust_optimized_polar = list(
          group_polar_pvs = iter_vars$group_polar_pvs,
          vars_shell      = character(0)
        ),
        clust_optimized_profile = list(
          group_polar_pvs = list(),
          vars_shell      = all_shell_vars
        ),
        clust_optimized_all = list(
          group_polar_pvs = iter_vars$group_polar_pvs,
          vars_shell      = all_shell_vars
        )
      )

      for (opt_name in names(optimized_defs)) {
        cli::cli_alert("Running optimized strategy: {.val {opt_name}}")
        opt_def <- optimized_defs[[opt_name]]
        result  <- list()

        for (pr in priors_list) {
          rk_suffix <- if (pr == "size") "size" else "eq"
          pr_suffix <- if (pr == "equal") "eq" else "sz"

          if (!is.null(seed)) set.seed(seed)
          result[[paste0("iterative_", rk_suffix)]] <- cluster_iterative(
            df = df, vars = all_source_vars, vars_profiles = all_polar_pvs,
            solution_name = opt_name,
            solution_name_prefix = paste0("iter_", pr_suffix),
            resp_id_name = resp_id_name, filter_name = filter_name_opt,
            n_min = n_min, n_max = n_max,
            reduced_inputs_max = reduced_inputs_max,
            priors = pr, iter_max = iter_max, nstart = nstart, seed = seed,
            vars_shell        = opt_def$vars_shell,
            group_source_vars = iter_vars$group_source_vars,
            group_polar_pvs   = opt_def$group_polar_pvs,
            target            = iter_vars$iterative_target,
            target_defs       = iter_vars$target_defs,
            strategy          = iter_vars$iterative_strategy,
            strategy_defs     = iter_vars$strategy_defs,
            polar_threshold   = iter_vars$polar_threshold,
            profile_threshold = iter_vars$profile_threshold,
            min_seg_pct       = iter_vars$min_seg_pct,
            swap_max_iter     = iter_vars$swap_max_iter,
            weight_var        = iter_vars$weight_var
          ) %>% suppressWarnings()
        }

        # build solution_table + df_segment_append (same as .cluster_parallel)
        sol_tbl <- result %>%
          purrr::flatten() %>%
          purrr::map(
            ~dplyr::select(
              .x, solution_name, n, cluster_name,
              lda_name, lda_inputs, lda_profiles,
              lda_coefficient_function, lda_predict,
              confusion, accuracy, kappa, cv, collinear, split_half, df_append
            )
          ) %>%
          dplyr::bind_rows() %>%
          dplyr::filter(!is.na(df_append))

        df_seg <- sol_tbl %>%
          dplyr::select(df_append) %>%
          unlist(recursive = FALSE) %>%
          purrr::reduce(function(x, y) {
            x %>%
              dplyr::select(!dplyr::any_of(setdiff(names(y), "id"))) %>%
              dplyr::left_join(y, by = "id")
          })

        sol_tbl <- sol_tbl %>% dplyr::select(-df_append)

        solutions_new[[opt_name]] <- list(
          result = result,
          solution_table = sol_tbl,
          df_segment_append = df_seg
        )
      }
    }
  }

  # ---- merge new results into existing solutions ----
  cli::cli_alert("Merging results & building summary table\u2026")
  if (!is.null(solutions_existing)) {
    solutions <- solutions_existing
    for (nm in names(solutions_new)) {
      if (!is.null(solutions[[nm]])) {
        # merge within family: combine result entries, solution_table rows,
        # and df_segment_append columns — replacing any duplicates
        old <- solutions[[nm]]
        new <- solutions_new[[nm]]

        # result: overwrite matching keys, keep the rest
        merged_result <- old[["result"]]
        for (rn in names(new[["result"]])) merged_result[[rn]] <- new[["result"]][[rn]]

        # solution_table: drop old rows whose lda_name appears in new, then bind
        new_lda_names <- new[["solution_table"]][["lda_name"]]
        merged_st <- old[["solution_table"]] %>%
          dplyr::filter(!lda_name %in% new_lda_names) %>%
          dplyr::bind_rows(new[["solution_table"]])

        # df_segment_append: drop old columns that appear in new, then bind
        new_seg_cols <- setdiff(names(new[["df_segment_append"]]), "id")
        merged_df <- old[["df_segment_append"]] %>%
          dplyr::select(!dplyr::any_of(new_seg_cols)) %>%
          dplyr::left_join(new[["df_segment_append"]], by = "id")

        solutions[[nm]] <- list(
          result = merged_result,
          solution_table = merged_st,
          df_segment_append = merged_df
        )
      } else {
        solutions[[nm]] <- solutions_new[[nm]]
      }
    }
  } else {
    solutions <- solutions_new
  }


  solution_table <- solutions %>%
    purrr::map("solution_table") %>%
    dplyr::bind_rows()


  df_segment_append <- solutions %>%
    purrr::map("df_segment_append") %>%
    purrr::reduce(dplyr::full_join, by = "id")


  df_temp <- seg[["data"]][["with_solutions"]]
  if(is.null(df_temp) || all(is.na(df_temp))){
    df_temp <- seg[["data"]][["with_shell"]]
  }


  df_return <- dplyr::left_join(
    df_temp %>%
      dplyr::select(
        !dplyr::any_of(
          names(df_segment_append) %>%
            tail(-1)
        )
      ),
    df_segment_append,
    by = dplyr::join_by(seg_uuid == id)
  )


  if("okay_filter" %in% names(df) && !"okay_filter" %in% names(df_return)){
    df_return <- df_return %>%
      dplyr::left_join(
        df %>% dplyr::select(c(!!resp_id_name, "okay_filter")),
        by = dplyr::join_by(!!resp_id_name)
      )
  }


  seg[["solutions"]][["analysis"]] <- solutions
  seg[["solutions"]][["summary_table"]] <- solution_table
  seg[["solutions"]][["df_segment_append"]] <- df_segment_append
  seg[["data"]][["with_solutions"]] <- df_return

  n_solutions <- nrow(solution_table)
  cli::cli_alert_success("Done \u2014 {n_solutions} solution{?s} across {length(solutions)} famil{?y/ies}")

  return(seg)
}


#' Run clustering in parallel across flattened task list
#'
#' @description Builds a flat list of all individual clustering tasks
#'   (family x method x polar_type x priors) and parallelizes across them.
#'   This keeps all cores busy — the previous approach parallelized across
#'   solution families only, leaving cores idle when families outnumbered
#'   workers. Results are reconstructed into the nested per-family structure
#'   expected by downstream code.
#'
#' @keywords internal
.cluster_parallel <- function(
    solution_inputs, df, resp_id_name,
    filter_logical_vector, ok_filter,
    n_min, n_max, reduced_inputs_max,
    priors, polar_type, iter_max, nstart,
    do_kmeans, do_medoid, do_gaus_mix, do_hierarchical,
    do_spectral = FALSE,
    do_iterative = FALSE, do_consensus = FALSE,
    do_optimized = FALSE,
    iter_vars = NULL, polars_table = NULL,
    seed, strategy, workers
) {

  # apply filter_logical_vector once (instead of per-family)
  if (!is.null(filter_logical_vector)) {
    if (is.character(filter_logical_vector)) {
      df <- df %>% dplyr::filter(.data[[filter_logical_vector]])
    } else if (is.logical(filter_logical_vector)) {
      df <- df %>% dplyr::filter(filter_logical_vector)
    }
  }

  filter_name <- if (ok_filter) "okay_filter" else NULL

  # determine which priors / polar types to run
  priors_list <- if (priors == "both") c("equal", "size") else priors
  polar_types <- if (polar_type == "both") c("rs", "source") else polar_type

  # build flat task list: family x method x polar_type x priors
  tasks <- list()

  for (family_name in names(solution_inputs)) {
    fam <- solution_inputs[[family_name]]

    for (pr in priors_list) {
      pr_suffix <- if (pr == "equal") "eq" else "sz"
      rk_suffix <- if (pr == "size") "size" else "eq"

      for (pt in polar_types) {
        vars_key <- if (pt == "rs") "RS" else "Source"

        if (do_kmeans) {
          tid <- paste0("kmeans_", pt, "_", pr_suffix, "_", family_name)
          tasks[[tid]] <- list(
            solution_name = family_name, method = "kmeans",
            result_key = paste0("kmeans_", pt, "_", rk_suffix),
            vars = fam[[vars_key]], vars_profiles = fam[["Profile"]],
            prefix = paste0("kmeans_", pt, "_", pr_suffix), priors = pr
          )
        }

        if (do_medoid) {
          tid <- paste0("medoid_", pt, "_", pr_suffix, "_", family_name)
          tasks[[tid]] <- list(
            solution_name = family_name, method = "medoid",
            result_key = paste0("medoid_", pt, "_", rk_suffix),
            vars = fam[[vars_key]], vars_profiles = fam[["Profile"]],
            prefix = paste0("medoid_", pt, "_", pr_suffix), priors = pr
          )
        }

        if (do_hierarchical) {
          tid <- paste0("hierarchical_", pt, "_", pr_suffix, "_", family_name)
          tasks[[tid]] <- list(
            solution_name = family_name, method = "hierarchical",
            result_key = paste0("hierarchical_", pt, "_", rk_suffix),
            vars = fam[[vars_key]], vars_profiles = fam[["Profile"]],
            prefix = paste0("hierarchical_", pt, "_", pr_suffix), priors = pr
          )
        }

        if (do_gaus_mix) {
          tid <- paste0("gaus_mix_", pt, "_", pr_suffix, "_", family_name)
          tasks[[tid]] <- list(
            solution_name = family_name, method = "gaus_mix",
            result_key = paste0("gaus_mix_", pt, "_", rk_suffix),
            vars = fam[[vars_key]], vars_profiles = fam[["Profile"]],
            prefix = paste0("gaus_mix_", pt, "_", pr_suffix), priors = pr
          )
        }

        if (do_spectral) {
          tid <- paste0("spectral_", pt, "_", pr_suffix, "_", family_name)
          tasks[[tid]] <- list(
            solution_name = family_name, method = "spectral",
            result_key = paste0("spectral_", pt, "_", rk_suffix),
            vars = fam[[vars_key]], vars_profiles = fam[["Profile"]],
            prefix = paste0("spectral_", pt, "_", pr_suffix), priors = pr
          )
        }
      }

      # iterative: source vars only, no polar_type branching
      if (do_iterative) {
        tid <- paste0("iterative_", pr_suffix, "_", family_name)
        tasks[[tid]] <- list(
          solution_name = family_name, method = "iterative",
          result_key = paste0("iterative_", rk_suffix),
          vars = fam[["Source"]], vars_profiles = fam[["Profile"]],
          prefix = paste0("iter_", pr_suffix), priors = pr,
          iter_vars_shell          = iter_vars$vars_shell,
          iter_group_source_vars   = iter_vars$group_source_vars,
          iter_group_polar_pvs     = iter_vars$group_polar_pvs,
          iter_weight_var          = iter_vars$weight_var,
          iter_target              = iter_vars$iterative_target,
          iter_target_defs         = iter_vars$target_defs,
          iter_strategy            = iter_vars$iterative_strategy,
          iter_strategy_defs       = iter_vars$strategy_defs,
          iter_polar_threshold     = iter_vars$polar_threshold,
          iter_profile_threshold   = iter_vars$profile_threshold,
          iter_min_seg_pct         = iter_vars$min_seg_pct,
          iter_swap_max_iter       = iter_vars$swap_max_iter
        )
      }
    }
  }

  # ---- optimized strategy tasks (independent of input-sheet families) ----
  if (do_iterative && do_optimized && !is.null(polars_table)) {

    all_source_vars <- polars_table[["source_var"]]
    all_polar_pvs   <- polars_table[["profile_var"]]
    all_shell_vars  <- iter_vars$vars_shell

    optimized_defs <- list(
      clust_optimized_polar = list(
        group_polar_pvs = iter_vars$group_polar_pvs,
        vars_shell      = character(0)
      ),
      clust_optimized_profile = list(
        group_polar_pvs = list(),
        vars_shell      = all_shell_vars
      ),
      clust_optimized_all = list(
        group_polar_pvs = iter_vars$group_polar_pvs,
        vars_shell      = all_shell_vars
      )
    )

    for (opt_name in names(optimized_defs)) {
      opt_def <- optimized_defs[[opt_name]]
      for (pr in priors_list) {
        pr_suffix <- if (pr == "equal") "eq" else "sz"
        rk_suffix <- if (pr == "size") "size" else "eq"
        tid <- paste0("iterative_", pr_suffix, "_", opt_name)
        tasks[[tid]] <- list(
          solution_name            = opt_name,
          method                   = "iterative",
          result_key               = paste0("iterative_", rk_suffix),
          vars                     = all_source_vars,
          vars_profiles            = all_polar_pvs,
          prefix                   = paste0("iter_", pr_suffix),
          priors                   = pr,
          iter_vars_shell          = opt_def$vars_shell,
          iter_group_source_vars   = iter_vars$group_source_vars,
          iter_group_polar_pvs     = opt_def$group_polar_pvs,
          iter_weight_var          = iter_vars$weight_var,
          iter_target              = iter_vars$iterative_target,
          iter_target_defs         = iter_vars$target_defs,
          iter_strategy            = iter_vars$iterative_strategy,
          iter_strategy_defs       = iter_vars$strategy_defs,
          iter_polar_threshold     = iter_vars$polar_threshold,
          iter_profile_threshold   = iter_vars$profile_threshold,
          iter_min_seg_pct         = iter_vars$min_seg_pct,
          iter_swap_max_iter       = iter_vars$swap_max_iter
        )
      }
    }
  }


  ntasks <- length(tasks)
  cli::cli_alert_info("{ntasks} task{?s} queued for parallel execution")
  future_plan(strategy = strategy, workers = workers, ntasks = ntasks)


  # worker dispatches to the right clustering method
  worker_fn <- function(.x, .y) {
    task <- .x
    if (!is.null(seed)) set.seed(seed)

    if (task$method == "iterative") {
      cluster_iterative(
        df = df, vars = task$vars, vars_profiles = task$vars_profiles,
        solution_name = task$solution_name,
        solution_name_prefix = task$prefix,
        resp_id_name = resp_id_name, filter_name = filter_name,
        n_min = n_min, n_max = n_max,
        reduced_inputs_max = reduced_inputs_max,
        priors = task$priors, iter_max = iter_max, nstart = nstart,
        seed = seed,
        vars_shell        = task$iter_vars_shell,
        group_source_vars = task$iter_group_source_vars,
        group_polar_pvs   = task$iter_group_polar_pvs,
        target            = task$iter_target,
        target_defs       = task$iter_target_defs,
        strategy          = task$iter_strategy,
        strategy_defs     = task$iter_strategy_defs,
        polar_threshold   = task$iter_polar_threshold,
        profile_threshold = task$iter_profile_threshold,
        min_seg_pct       = task$iter_min_seg_pct,
        swap_max_iter     = task$iter_swap_max_iter,
        weight_var        = task$iter_weight_var
      )
    } else {
      fn <- switch(task$method,
        "kmeans" = cluster_kmeans,
        "medoid" = cluster_medoid,
        "gaus_mix" = cluster_gaus_mix,
        "hierarchical" = cluster_hierarchical,
        "spectral" = cluster_spectral
      )
      fn(
        df = df, vars = task$vars, vars_profiles = task$vars_profiles,
        solution_name = task$solution_name,
        solution_name_prefix = task$prefix,
        resp_id_name = resp_id_name, filter_name = filter_name,
        n_min = n_min, n_max = n_max,
        reduced_inputs_max = reduced_inputs_max,
        priors = task$priors, iter_max = iter_max, nstart = nstart,
        seed = seed
      )
    }
  }

  # clean environment — only data + shared scalar params
  env_list <- list(
    df = df, resp_id_name = resp_id_name, filter_name = filter_name,
    n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
    iter_max = iter_max, nstart = nstart, seed = seed
  )

  # iterative params are now stored per-task (not in worker env) so that
  # optimized strategies can carry different params than regular families
  environment(worker_fn) <- list2env(env_list, parent = globalenv())

  .furrr_pkgs <- c(
    "work", "dplyr", "tibble", "tidyr", "purrr", "glue",
    "stats", "klaR", "MASS", "caret", "cluster"
  )
  if (do_gaus_mix)  .furrr_pkgs <- c(.furrr_pkgs, "mclust")
  if (do_spectral)  .furrr_pkgs <- c(.furrr_pkgs, "kernlab")

  raw_results <- imap_progress(
    tasks, worker_fn,
    .parallel = TRUE,
    .furrr_packages = .furrr_pkgs,
    .label = "Clustering"
  )

  future::plan(future::sequential)


  # reconstruct nested structure grouped by solution family
  cli::cli_alert("Reconstructing solution families\u2026")
  family_names <- unique(purrr::map_chr(tasks, "solution_name"))

  solutions <- purrr::map(purrr::set_names(family_names), function(sn) {
    family_ids <- names(tasks)[purrr::map_chr(tasks, "solution_name") == sn]

    result <- list()
    for (tid in family_ids) {
      task <- tasks[[tid]]
      raw <- raw_results[[tid]]

      if (task$method == "hierarchical") {
        result[[task$result_key]] <- list(
          all_inputs = raw[["all_inputs"]],
          reduced_inputs = raw[["reduced_inputs"]]
        )
        result[[paste0(task$result_key, "_fit")]] <- raw[["hierarchical_fit"]]
      } else {
        result[[task$result_key]] <- raw
      }
    }

    # ---- consensus second-pass (runs sequentially after parallel tasks) ----
    if (do_consensus) {
      cli::cli_alert("Running consensus for family {.val {sn}}\u2026")
      fam <- solution_inputs[[sn]]
      filter_name_con <- if (ok_filter) "okay_filter" else NULL

      .run_consensus_parallel <- function(pt_pattern, vars_to_use, pt_label) {
        all_keys <- grep(pt_pattern, names(result), value = TRUE)
        all_keys <- all_keys[!grepl("_fit$", all_keys)]
        if (pt_label == "src") {
          all_keys <- c(all_keys, grep("^iterative_", names(result), value = TRUE))
        }
        method_prefixes <- sub("_(src|rs)_.*$|_(eq|size)$", "", all_keys)
        unique_keys <- all_keys[!duplicated(method_prefixes)]
        method_results_filtered <- result[unique_keys]
        if (length(method_results_filtered) < 2) return(NULL)

        con_results <- list()
        if (priors != "equal") {
          if (!is.null(seed)) set.seed(seed)
          con_results[[paste0("consensus_", pt_label, "_size")]] <- cluster_consensus(
            df = df, vars = vars_to_use, vars_profiles = fam[["Profile"]],
            solution_name = sn,
            solution_name_prefix = paste0("consensus_", pt_label, "_sz"),
            resp_id_name = resp_id_name, filter_name = filter_name_con,
            n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
            priors = "size", seed = seed,
            method_results = method_results_filtered
          ) %>% suppressWarnings()
        }
        if (priors != "size") {
          if (!is.null(seed)) set.seed(seed)
          con_results[[paste0("consensus_", pt_label, "_eq")]] <- cluster_consensus(
            df = df, vars = vars_to_use, vars_profiles = fam[["Profile"]],
            solution_name = sn,
            solution_name_prefix = paste0("consensus_", pt_label, "_eq"),
            resp_id_name = resp_id_name, filter_name = filter_name_con,
            n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
            priors = "equal", seed = seed,
            method_results = method_results_filtered
          ) %>% suppressWarnings()
        }
        con_results
      }

      if (polar_type != "source") {
        rs_con <- .run_consensus_parallel("_rs_", fam[["RS"]], "rs")
        if (!is.null(rs_con)) result <- c(result, rs_con)
      }
      if (polar_type != "rs") {
        src_con <- .run_consensus_parallel("_src_", fam[["Source"]], "src")
        if (!is.null(src_con)) result <- c(result, src_con)
      }
    }

    # build solution_table (same logic as cluster_solution_family)
    solution_table <- result[!grepl("_fit$", names(result))] %>%
      purrr::flatten() %>%
      purrr::map(
        ~dplyr::select(
          .x, solution_name, n, cluster_name,
          lda_name, lda_inputs, lda_profiles,
          lda_coefficient_function, lda_predict,
          confusion, accuracy, kappa, cv, split_half, df_append
        )
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!is.na(df_append))

    # build df_segment_append
    df_segment_append <- solution_table %>%
      dplyr::select(df_append) %>%
      unlist(recursive = FALSE) %>%
      purrr::reduce(function(x, y) {
        x %>%
          dplyr::select(!dplyr::any_of(setdiff(names(y), "id"))) %>%
          dplyr::left_join(y, by = "id")
      })

    solution_table <- solution_table %>% dplyr::select(-df_append)

    list(
      result = result,
      solution_table = solution_table,
      df_segment_append = df_segment_append
    )
  })

  solutions
}
