#' cluster_solution_family
#'
#' @description Runs all enabled clustering methods for a single solution
#'   family across a range of cluster counts (`n_min` to `n_max`), polar types,
#'   and prior settings. Returns solution tables and segment assignments.
#'
#'   Each method is crossed with `polar_type` (RS vs source variables) and
#'   `priors` (equal vs size-proportional) to produce multiple solutions per
#'   method. The iterative method is an exception — it only uses source
#'   variables (no RS variant) but still varies by priors.
#'
#'   ## Clustering methods
#'
#'   **K-means** (`do_kmeans`). Partitions respondents into k clusters by
#'   minimizing total within-cluster variance. Each respondent is assigned to
#'   the cluster whose centroid is nearest. Fast and scalable; the default
#'   workhorse. Sensitive to initialization — `nstart` random starts mitigate
#'   this. Assumes spherical, equal-sized clusters.
#'   Calls [cluster_kmeans()] → `stats::kmeans()`.
#'
#'   **Medoid / PAM** (`do_medoid`). Partitioning Around Medoids. Like k-means
#'   but uses actual data points (medoids) as cluster centers instead of
#'   centroids. More robust to outliers. Minimizes sum of dissimilarities
#'   rather than squared distances. Slower than k-means for large n.
#'   Calls [cluster_medoid()] → `cluster::pam()`.
#'
#'   **Gaussian mixture** (`do_gaus_mix`). Fits a mixture of k multivariate
#'   Gaussian distributions via expectation-maximization (EM). Each respondent
#'   gets a probability of belonging to each cluster (soft assignment), then is
#'   assigned to the most probable one. Can capture elliptical and unequal-size
#'   clusters that k-means misses. More computationally expensive; can fail
#'   with high-dimensional or collinear data.
#'   Calls [cluster_gaus_mix()] → `mclust::Mclust()`.
#'
#'   **Hierarchical** (`do_hierarchical`). Agglomerative (bottom-up)
#'   clustering. Starts with each respondent as its own cluster and
#'   iteratively merges the two closest clusters until k remain. Produces a
#'   dendrogram; cutting at different heights gives different k values without
#'   re-running. Uses Ward's method by default (minimizes increase in total
#'   within-cluster variance at each merge). Deterministic — no random
#'   initialization. Also returns the hierarchical fit object for dendrogram
#'   inspection.
#'   Calls [cluster_hierarchical()] → `stats::hclust()` + `stats::cutree()`.
#'
#'   **Spectral** (`do_spectral`). Embeds respondents into a low-dimensional
#'   space derived from eigenvectors of an RBF similarity matrix, then clusters
#'   in that space. Can find non-convex cluster shapes (spirals, rings,
#'   crescents) that distance-based methods miss. O(n^2) memory for the
#'   similarity matrix and O(n^3) for eigendecomposition — significantly slower
#'   than k-means on large datasets. Kernel bandwidth estimated automatically.
#'   Calls [cluster_spectral()] → `kernlab::specc()`.
#'
#'   **Iterative** (`do_iterative`). Greedy swap optimizer that directly
#'   maximizes threshold hits — the count of variables where
#'   `abs(seg_mean - others_mean)` exceeds a threshold. Starts from a k-means
#'   seed and iteratively moves individual respondents between segments,
#'   picking the swap that most increases the hit count. Unlike the other
#'   methods which optimize a generic statistical criterion (variance,
#'   dissimilarity, likelihood), this one optimizes the metric that actually
#'   determines whether a segmentation solution is useful in practice. Only
#'   uses source variables (no RS variant). Uses source vars only (no
#'   RS/polar_type branching).
#'   Calls [cluster_iterative()] → [iterative_optimize()].
#'
#'   **Consensus** (`do_consensus`). Combines assignments from all other enabled
#'   methods via a co-association matrix. For each pair of respondents, counts
#'   the fraction of methods that placed them in the same cluster, then runs
#'   hierarchical clustering on the resulting dissimilarity matrix. Stabilizes
#'   results — if most methods agree, the consensus follows the majority. Runs
#'   after all other methods and requires ≥ 2 to have succeeded. No external
#'   dependency.
#'   Calls [cluster_consensus()].
#'
#'   ## Shared post-processing
#'
#'   All methods follow the same post-processing pipeline:
#'   1. **Variable reduction** via `klaR::greedy.wilks()` + `klaR::stepclass()`
#'      — identifies the most discriminating subset of input variables.
#'   2. **LDA** via [cluster_add_lda()] — trains a linear discriminant
#'      function on the cluster assignments, producing a typing tool that can
#'      classify new respondents. Run twice: once with all inputs, once with
#'      the reduced variable set.
#'   3. **Accuracy** — confusion matrix + overall accuracy from
#'      `caret::confusionMatrix()`, comparing LDA-predicted segments to
#'      original cluster assignments.
#'
#' @param df A data frame containing the shell data and any pre-computed filter
#'   columns (e.g. `okay_filter`).
#' @param source_vars Character vector of source polar variable names, used by
#'   [seg_cluster_variability()] when `ok_filter = TRUE` and no prior filter
#'   exists on `df`.
#' @param inputs Named list with `Source`, `Profile`, and `RS` variable vectors
#'   for this solution family. `Source` = raw polar source vars, `RS` =
#'   rescaled polar vars, `Profile` = polar profile (binary indicator) vars.
#' @param solution_name Character. Solution identifier (e.g. `"A"`).
#' @param resp_id_name Character. Respondent ID column.
#' @param filter_logical_vector Logical vector or column name to subset rows.
#' @param n_min Integer. Minimum clusters (default: `4`).
#' @param n_max Integer. Maximum clusters (default: `7`).
#' @param reduced_inputs_max Integer. Max reduced inputs for discriminant
#'   analysis. `NULL` uses all.
#' @param vary_percent Numeric (0–1). Variability filter tail fraction
#'   (default: `0.1`).
#' @param side_bias_percent Numeric (0–1). Side-bias filter tail fraction
#'   (default: `0.1`).
#' @param priors Character. Prior method for LDA: `"both"` (runs both equal
#'   and size), `"equal"` (equal priors — each segment equally likely), or
#'   `"size"` (size-proportional priors — larger segments get higher prior
#'   probability).
#' @param iter_max Integer. Max k-means iterations (default: `1000`).
#' @param nstart Integer. K-means random starts (default: `10`).
#' @param ok_filter Logical. Apply variability/side-bias filter (default: `TRUE`).
#' @param do_kmeans Logical. Run k-means clustering (default: `TRUE`).
#' @param do_medoid Logical. Run PAM clustering (default: `TRUE`).
#' @param do_hierarchical Logical. Run hierarchical clustering (default: `TRUE`).
#' @param do_gaus_mix Logical. Run Gaussian mixture clustering
#'   (default: `FALSE`).
#' @param do_spectral Logical. Run spectral clustering (default: `FALSE`).
#'   Uses `kernlab::specc()` with automatic kernel bandwidth estimation.
#' @param do_iterative Logical. Run iterative greedy swap optimization
#'   (default: `FALSE`). Uses source vars only (no RS/polar_type branching).
#' @param do_consensus Logical. Run consensus clustering (default: `FALSE`).
#'   Combines assignments from all other enabled methods via a co-association
#'   matrix. Requires ≥ 2 methods to have run successfully. Runs AFTER all
#'   other methods. Crossed with `polar_type` and `priors` like the standard
#'   methods. See [cluster_consensus()] for algorithm details.
#' @param polar_type Character. Which polar variables to use for clustering:
#'   `"both"` (runs RS and source separately), `"rs"` (rescaled only), or
#'   `"source"` (source only). Does not apply to iterative method.
#' @param seed Integer. Random seed (default: `1`).
#' @param vars_shell Character vector. Shell profile variable names for the
#'   iterative evaluation matrix. Required when `do_iterative = TRUE`.
#' @param group_source_vars Named list. Source vars per polar group for
#'   iterative. Required when `do_iterative = TRUE`.
#' @param group_polar_pvs Named list. Polar profile vars per group for
#'   iterative. Required when `do_iterative = TRUE`.
#' @param iterative_target Character. Which entry in `target_defs` the
#'   iterative optimizer uses as its objective (default: `"total"`). Controls
#'   which polar groups' hits count when the optimizer evaluates swaps.
#'   Profile hits always count regardless. For example, `"total"` counts all
#'   polar groups, while `"kg_only"` would count only KG polar hits — making
#'   the optimizer focus on KG differentiation at the expense of other
#'   batteries. See [iterative_optimize()] for details.
#' @param target_defs Named list. Maps target names to character vectors of
#'   group names. The `iterative_target` parameter selects which entry to
#'   use. For example, `list(total = c("kg", "pr_tt"), kg_only = "kg")`.
#'   `NULL` auto-derives as `list(total = names(group_source_vars))`.
#' @param iterative_strategy Character. Strategy definition key for iterative
#'   k-means seeding. `NULL` uses first strategy.
#' @param strategy_defs Named list. Strategy definitions for iterative. `NULL`
#'   auto-derives.
#' @param polar_threshold Numeric. Polar hit threshold for iterative
#'   (default: `0.20`).
#' @param profile_threshold Numeric. Profile hit threshold for iterative
#'   (default: `0.15`).
#' @param min_seg_pct Numeric (0–1). Minimum segment size fraction for
#'   iterative (default: `0.05`).
#' @param swap_max_iter Integer. Max optimizer iterations for iterative
#'   (default: `1000`).
#' @param weight_var Character. Weight variable for iterative. `NULL` for
#'   unweighted.
#'
#' @return A list with `result` (raw clustering output keyed by method/polar/
#'   priors combination), `solution_table` (summary tibble of all solutions
#'   with accuracy, LDA info), and `df_segment_append` (segment assignments
#'   for all solutions joined by respondent ID).
#'
#' @export
cluster_solution_family <- function(
    df,
    source_vars,
    inputs,
    solution_name,
    resp_id_name,
    filter_logical_vector = NULL,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = NULL,
    vary_percent = .1,
    side_bias_percent = .1,
    priors = c("both", "equal", "size"),
    iter_max = 1000,
    nstart = 10,
    ok_filter = TRUE,
    do_kmeans = TRUE,
    do_medoid = TRUE,
    do_hierarchical = TRUE,
    do_gaus_mix = FALSE,
    do_spectral = FALSE,
    do_iterative = FALSE,
    do_consensus = FALSE,
    polar_type = c("both", "rs", "source"),
    seed = 1,
    # ---- iterative-specific (ignored unless do_iterative = TRUE) ----
    vars_shell          = NULL,
    group_source_vars   = NULL,
    group_polar_pvs     = NULL,
    iterative_target    = "total",
    target_defs         = NULL,
    iterative_strategy  = NULL,
    strategy_defs       = NULL,
    polar_threshold     = 0.20,
    profile_threshold   = 0.15,
    min_seg_pct         = 0.05,
    swap_max_iter       = 1000,
    weight_var          = NULL
){

  result <- list()
  priors <- match.arg(priors)
  polar_type <- match.arg(polar_type)


  # filter data if available
  if(!is.null(filter_logical_vector)){

    if(is.character(filter_logical_vector)){

      df <- df %>% dplyr::filter(.data[[filter_logical_vector]])

    }else if(is.logical(filter_logical_vector)){

      df <- df %>% dplyr::filter(filter_logical_vector)
    }
  }



  if(ok_filter){
    filter_name <- "okay_filter"

    if(!filter_name %in% names(df)){

      df <- df %>%
        seg_cluster_variability(
          vars = source_vars,
          vary_percent = vary_percent,
          side_bias_percent = side_bias_percent
        )

      df <- df[["df"]]
    }

  }else{
    filter_name <- NULL
  }



  if(do_kmeans){

    if(priors != "equal"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["kmeans_rs_size"]] <- cluster_kmeans(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "kmeans_rs_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["kmeans_src_size"]] <- cluster_kmeans(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "kmeans_src_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

    if(priors != "size"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["kmeans_rs_eq"]] <- cluster_kmeans(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "kmeans_rs_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["kmeans_src_eq"]] <- cluster_kmeans(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "kmeans_src_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

  }



  if(do_medoid){

    if(priors != "equal"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["medoid_rs_size"]] <- cluster_medoid(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "medoid_rs_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["medoid_src_size"]] <- cluster_medoid(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "medoid_src_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

    if(priors != "size"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["medoid_rs_eq"]] <- cluster_medoid(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "medoid_rs_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["medoid_src_eq"]] <- cluster_medoid(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "medoid_src_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

  }



  if(do_gaus_mix){

    if(priors != "equal"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["gaus_mix_rs_size"]] <- cluster_gaus_mix(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "gaus_mix_rs_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["gaus_mix_src_size"]] <- cluster_gaus_mix(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "gaus_mix_src_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

    if(priors != "size"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["gaus_mix_rs_eq"]] <- cluster_gaus_mix(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "gaus_mix_rs_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["gaus_mix_src_eq"]] <- cluster_gaus_mix(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "gaus_mix_src_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

  }



  if(do_spectral){

    if(priors != "equal"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["spectral_rs_size"]] <- cluster_spectral(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "spectral_rs_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["spectral_src_size"]] <- cluster_spectral(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "spectral_src_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

    if(priors != "size"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["spectral_rs_eq"]] <- cluster_spectral(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "spectral_rs_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["spectral_src_eq"]] <- cluster_spectral(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "spectral_src_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

  }



  if(do_hierarchical){

    if(priors != "equal"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        temp_rssz <- cluster_hierarchical(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "hierarchical_rs_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
        result[["hierarchical_rs_size"]] <- list(all_inputs = temp_rssz[["all_inputs"]], reduced_inputs = temp_rssz[["reduced_inputs"]])
        result[["hierarchical_rs_size_fit"]] <- temp_rssz[["hierarchical_fit"]]
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        temp_srcsz <- cluster_hierarchical(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "hierarchical_src_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
        result[["hierarchical_src_size"]] <- list(all_inputs = temp_srcsz[["all_inputs"]], reduced_inputs = temp_srcsz[["reduced_inputs"]])
        result[["hierarchical_src_size_fit"]] <- temp_srcsz[["hierarchical_fit"]]
      }
    }

    if(priors != "size"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        temp_rseq <- cluster_hierarchical(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "hierarchical_rs_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
        result[["hierarchical_rs_eq"]] <- list(all_inputs = temp_rseq[["all_inputs"]], reduced_inputs = temp_rseq[["reduced_inputs"]])
        result[["hierarchical_rs_eq_fit"]] <- temp_rseq[["hierarchical_fit"]]
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        temp_srceq <- cluster_hierarchical(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "hierarchical_src_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
        result[["hierarchical_src_eq"]] <- list(all_inputs = temp_srceq[["all_inputs"]], reduced_inputs = temp_srceq[["reduced_inputs"]])
        result[["hierarchical_src_eq_fit"]] <- temp_srceq[["hierarchical_fit"]]
      }
    }

  }



  if(do_iterative){

    if(priors != "equal"){
      if(!is.null(seed)) set.seed(seed)
      result[["iterative_size"]] <- cluster_iterative(
        df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
        solution_name = solution_name, solution_name_prefix = "iter_sz",
        resp_id_name = resp_id_name, filter_name = filter_name,
        n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
        priors = "size", iter_max = iter_max, nstart = nstart, seed = seed,
        vars_shell = vars_shell, group_source_vars = group_source_vars,
        group_polar_pvs = group_polar_pvs, target = iterative_target,
        target_defs = target_defs, strategy = iterative_strategy,
        strategy_defs = strategy_defs, polar_threshold = polar_threshold,
        profile_threshold = profile_threshold, min_seg_pct = min_seg_pct,
        swap_max_iter = swap_max_iter, weight_var = weight_var
      ) %>% suppressWarnings()
    }

    if(priors != "size"){
      if(!is.null(seed)) set.seed(seed)
      result[["iterative_eq"]] <- cluster_iterative(
        df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
        solution_name = solution_name, solution_name_prefix = "iter_eq",
        resp_id_name = resp_id_name, filter_name = filter_name,
        n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
        priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed,
        vars_shell = vars_shell, group_source_vars = group_source_vars,
        group_polar_pvs = group_polar_pvs, target = iterative_target,
        target_defs = target_defs, strategy = iterative_strategy,
        strategy_defs = strategy_defs, polar_threshold = polar_threshold,
        profile_threshold = profile_threshold, min_seg_pct = min_seg_pct,
        swap_max_iter = swap_max_iter, weight_var = weight_var
      ) %>% suppressWarnings()
    }

  }



  if (do_consensus) {

    # collect method results per polar_type, deduplicate eq/size variants
    # (eq and size have identical cluster assignments — only LDA differs)
    .run_consensus <- function(pt_pattern, vars_to_use, pt_label) {
      # find all result keys matching this polar_type
      all_keys <- grep(pt_pattern, names(result), value = TRUE)
      all_keys <- all_keys[!grepl("_fit$", all_keys)]

      # for src, also include iterative (source vars only, no pt suffix)
      if (pt_label == "src") {
        all_keys <- c(all_keys, grep("^iterative_", names(result), value = TRUE))
      }

      # deduplicate: one key per method (eq and size variants are identical assignments)
      method_prefixes <- sub("_(src|rs)_.*$|_(eq|size)$", "", all_keys)
      unique_keys <- all_keys[!duplicated(method_prefixes)]
      method_results_filtered <- result[unique_keys]

      if (length(method_results_filtered) < 2) return(NULL)

      consensus_results <- list()

      if (priors != "equal") {
        if (!is.null(seed)) set.seed(seed)
        consensus_results[[paste0("consensus_", pt_label, "_size")]] <- cluster_consensus(
          df = df, vars = vars_to_use, vars_profiles = inputs[["Profile"]],
          solution_name = solution_name,
          solution_name_prefix = paste0("consensus_", pt_label, "_sz"),
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", seed = seed,
          method_results = method_results_filtered
        ) %>% suppressWarnings()
      }

      if (priors != "size") {
        if (!is.null(seed)) set.seed(seed)
        consensus_results[[paste0("consensus_", pt_label, "_eq")]] <- cluster_consensus(
          df = df, vars = vars_to_use, vars_profiles = inputs[["Profile"]],
          solution_name = solution_name,
          solution_name_prefix = paste0("consensus_", pt_label, "_eq"),
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", seed = seed,
          method_results = method_results_filtered
        ) %>% suppressWarnings()
      }

      consensus_results
    }

    if (polar_type != "source") {
      rs_results <- .run_consensus("_rs_", inputs[["RS"]], "rs")
      if (!is.null(rs_results)) result <- c(result, rs_results)
    }

    if (polar_type != "rs") {
      src_results <- .run_consensus("_src_", inputs[["Source"]], "src")
      if (!is.null(src_results)) result <- c(result, src_results)
    }

  }



  solution_table <- result[!grepl("_fit$", names(result))] %>%
    purrr::flatten() %>%
    purrr::map(
      ~dplyr::select(
        .x, solution_name, n, cluster_name,
        lda_name, lda_inputs, lda_profiles,
        lda_coefficient_function, lda_predict,
        confusion, accuracy, df_append
      )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(df_append))



  df_segment_append <- solution_table %>%
    dplyr::select(df_append) %>%
    unlist(recursive = FALSE) %>%
    purrr::reduce(function(x, y) {
      x %>%
        dplyr::select(!dplyr::any_of(setdiff(names(y), "id"))) %>%
        dplyr::left_join(y, by = "id")
    })



  solution_table <- solution_table %>% dplyr::select(-df_append)



  output <- list(
    result = result,
    solution_table = solution_table,
    df_segment_append = df_segment_append
  )


  return(output)
}
