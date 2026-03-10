#' cluster_hierarchical
#'
#' @description Agglomerative hierarchical clustering wrapper for the
#'   segmentation pipeline. Builds a single dendrogram via `stats::hclust()`,
#'   then cuts it at each k in the requested range using `stats::cutree()`.
#'   Reduces inputs via discriminant analysis and pipes results through
#'   [cluster_add_lda()] to produce a standardized output.
#'
#'   **What hierarchical clustering does.** Agglomerative (bottom-up)
#'   hierarchical clustering starts with every respondent as its own cluster,
#'   then iteratively merges the two closest clusters until only one remains.
#'   The result is a dendrogram (tree) that encodes the full merge history.
#'   To get k clusters, the tree is "cut" at the height that produces k
#'   branches. The default linkage is "complete" (furthest-neighbor), which
#'   tends to produce compact, roughly equal-sized clusters.
#'
#'   **Key difference from other methods.** The dendrogram is built once and
#'   then sliced at different heights for each k. This means all k-solutions
#'   are nested — a respondent's k=4 assignment is always a merge of two
#'   k=5 groups. K-means, PAM, and iterative re-partition from scratch for
#'   each k, so their solutions across k values are independent.
#'
#'   **Strengths.** Deterministic (no random initialization). The dendrogram
#'   gives a visual summary of cluster structure at all levels. Nested
#'   solutions provide a natural hierarchy — useful when you need to explain
#'   "these two segments merge into one at a higher level." No need to
#'   specify k upfront; the tree contains all possible k-solutions.
#'
#'   **Limitations.** O(n^2) memory for the distance matrix and O(n^2 log n)
#'   time for the merge algorithm — does not scale to very large datasets.
#'   Merge decisions are irrevocable (once two clusters merge, they can never
#'   split), so early bad merges propagate. Sensitive to the choice of
#'   linkage and distance metric.
#'
#'   **Pipeline integration.** This is a method wrapper called by
#'   [cluster_solution_family()] (and indirectly by [seg_cluster_input_sheet()]).
#'   Unlike other wrappers, the return value includes a third element
#'   (`hierarchical_fit`) containing the raw hclust object for downstream
#'   dendrogram plotting.
#'
#' @param df A data frame containing shell data (the full dataset including
#'   all respondents, not just filtered ones).
#' @param vars Character vector of input variable names to cluster on.
#' @param vars_profiles Character vector of profile variable names
#'   corresponding to `vars` (binary polar indicators).
#' @param solution_name Character. Solution family identifier (e.g. `"A"`).
#' @param solution_name_prefix Character. Prefix for cluster names
#'   (default: `"hierarchical"`). Combined with `solution_name` and k to form
#'   names like `"hierarchical_A4"`, `"hierarchical_A5"`.
#' @param resp_id_name Character or `NULL`. Respondent ID column name.
#' @param filter_name Character or `NULL`. Column name containing a logical
#'   filter vector. `NULL` uses all rows.
#' @param n_min Integer. Minimum number of clusters (default: `4`).
#' @param n_max Integer. Maximum number of clusters (default: `7`).
#' @param reduced_inputs_max Integer or `NULL`. Cap on the number of reduced
#'   input variables. `NULL` uses all selected variables.
#' @param priors Character. Prior probability method for LDA: `"equal"` or
#'   `"size"` (default: `"equal"`).
#' @param iter_max Integer. Not used by hclust but kept for signature
#'   compatibility with other method wrappers.
#' @param nstart Integer. Not used by hclust but kept for signature
#'   compatibility with other method wrappers.
#' @param lda_vars Character vector or `NULL`. Override which input variables
#'   are used for LDA. `NULL` uses all `vars`.
#' @param lda_vars_profiles Character vector or `NULL`. Override which profile
#'   variables are used for LDA. `NULL` uses all `vars_profiles`.
#' @param seed Integer or `NULL`. Random seed for reproducibility
#'   (default: `1`).
#'
#' @return A list with three elements:
#' \describe{
#'   \item{all_inputs}{Tibble with one row per k (`n_min:n_max`). Contains
#'     clustering results and LDA fit using all input variables. Note: no
#'     `cluster_fit` column — assignments come directly from `cutree()`.}
#'   \item{reduced_inputs}{Same structure but LDA uses only the reduced
#'     variable set selected by [cluster_reduce_vars()].}
#'   \item{hierarchical_fit}{The raw `hclust` object (the dendrogram). Can
#'     be passed to `plot()` or `stats::as.dendrogram()` for visualization.}
#' }
#'
#' @export
cluster_hierarchical <- function(
    df, vars, vars_profiles, solution_name,
    solution_name_prefix = "hierarchical",
    resp_id_name = NULL,
    filter_name = NULL,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = NULL,
    priors = c("equal", "size"),
    iter_max = 100000,
    nstart = 10,
    lda_vars = NULL,
    lda_vars_profiles = NULL,
    seed = 1
){


  if(!is.null(seed)) set.seed(seed)

  priors <- match.arg(priors)


  if(!is.null(filter_name)){
    df_temp <- df %>%
      dplyr::filter(.data[[filter_name]])
  }else{
    df_temp <- df
  }


  df_temp <- df_temp %>%
    dplyr::select(all_of(c(resp_id_name, vars))) %>%
    na.exclude()


  id <- df[[resp_id_name]]
  id_temp <- df_temp[[resp_id_name]]


  df_temp <- df_temp %>% dplyr::select(-all_of(resp_id_name))


  if(!is.null(lda_vars)){
    reduced_vars <- vars[vars %in% lda_vars]
    reduced_vars_profiles <- vars_profiles[vars_profiles %in% lda_vars_profiles]
  }else{
    reduced_vars <- vars
    reduced_vars_profiles <- vars_profiles
  }


  if(!is.null(seed)) set.seed(seed)
  hierarchical_fit <- stats::hclust(stats::dist(df_temp))


  result <- tibble::tibble("n" = n_min : n_max) %>%
    mutate(
      "solution_name" = solution_name,
      "cluster_name" = glue("{solution_name_prefix}_{solution_name}{n}"),
      "inputs" = list(vars),
      "profiles" = list(vars_profiles),
      "cluster_seed" = purrr::map2(
        n, cluster_name,
        function(x,y)possibly(
          ~{stats::cutree(hierarchical_fit, k = x) %>%
            bind_cols(id_temp, .) %>%
              set_names(c("id", y)) %>%
              suppressMessages()}
          , otherwise = NA)()),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~.x[[.y]] %>% table_percent(), otherwise = NA)),
      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, possibly(~{
        if(!is.null(seed)) set.seed(seed)
        cluster_reduce_vars(df_temp, reduced_vars, .x[[.y]], type = "greedy_step", return_only_var = TRUE, seed = seed)
        }, otherwise = NA)),
      "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, reduced_vars)])
    )


  if(!is.null(reduced_inputs_max)){
    result <- result %>%
      mutate(
        "reduced_inputs" = purrr::map(reduced_inputs, ~.x %>% head(reduced_inputs_max)),
        "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
      )
  }


  if(!is.null(seed)) set.seed(seed)
  result_all <- result %>%
    cluster_add_lda(
      df = df,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      priors = priors,
      use_reduced = FALSE,
      lda_vars = reduced_vars,
      lda_vars_profiles = reduced_vars_profiles
    )

  if(!is.null(seed)) set.seed(seed)
  result_reduced <- result %>%
    cluster_add_lda(
      df = df,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      priors = priors,
      use_reduced = TRUE
    )


  output <- list(
    all_inputs = result_all,
    reduced_inputs = result_reduced,
    hierarchical_fit = hierarchical_fit
  )


  return(output)
}
