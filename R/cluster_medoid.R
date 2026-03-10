#' cluster_medoid
#'
#' @description PAM (Partitioning Around Medoids) clustering wrapper for the
#'   segmentation pipeline. Runs `cluster::pam()` across a range of cluster
#'   counts (k), reduces inputs via discriminant analysis, and pipes results
#'   through [cluster_add_lda()] to produce a standardized output.
#'
#'   **What PAM does.** Like k-means, PAM partitions n respondents into k
#'   groups — but instead of using centroids (computed means), it selects k
#'   actual respondents as "medoids" (representative exemplars). Each
#'   remaining respondent is assigned to the nearest medoid. The algorithm
#'   iteratively swaps medoids with non-medoids to minimize the total
#'   dissimilarity (sum of distances from each point to its medoid).
#'
#'   **Strengths.** More robust to outliers than k-means because medoids are
#'   real data points, not means that can be pulled by extreme values. The
#'   medoids themselves are interpretable — they are the most "typical"
#'   respondent in each segment. Works with any distance metric (not just
#'   Euclidean). The `variant = "faster"` option uses the O(n) Reynolds
#'   algorithm for improved performance on large datasets.
#'
#'   **Limitations.** Slower than k-means (O(n^2) for distance computation
#'   even with the faster variant). Constrained to choose actual respondents
#'   as centers, which can be suboptimal when the true center lies between
#'   data points. Like k-means, optimizes distance rather than segment
#'   differentiation directly.
#'
#'   **Pipeline integration.** This is a method wrapper called by
#'   [cluster_solution_family()] (and indirectly by [seg_cluster_input_sheet()]).
#'   All method wrappers share the same signature and return shape so they
#'   can be dispatched interchangeably.
#'
#' @param df A data frame containing shell data (the full dataset including
#'   all respondents, not just filtered ones).
#' @param vars Character vector of input variable names to cluster on.
#' @param vars_profiles Character vector of profile variable names
#'   corresponding to `vars` (binary polar indicators).
#' @param solution_name Character. Solution family identifier (e.g. `"A"`).
#' @param solution_name_prefix Character. Prefix for cluster names
#'   (default: `"medoid"`). Combined with `solution_name` and k to form
#'   names like `"medoid_A4"`, `"medoid_A5"`.
#' @param resp_id_name Character or `NULL`. Respondent ID column name.
#' @param filter_name Character or `NULL`. Column name containing a logical
#'   filter vector. `NULL` uses all rows.
#' @param n_min Integer. Minimum number of clusters (default: `4`).
#' @param n_max Integer. Maximum number of clusters (default: `7`).
#' @param reduced_inputs_max Integer or `NULL`. Cap on the number of reduced
#'   input variables. `NULL` uses all selected variables.
#' @param priors Character. Prior probability method for LDA: `"equal"` or
#'   `"size"` (default: `"equal"`).
#' @param iter_max Integer. Not used by PAM but kept for signature
#'   compatibility with other method wrappers.
#' @param nstart Integer. Not used by PAM but kept for signature
#'   compatibility with other method wrappers.
#' @param lda_vars Character vector or `NULL`. Override which input variables
#'   are used for LDA. `NULL` uses all `vars`.
#' @param lda_vars_profiles Character vector or `NULL`. Override which profile
#'   variables are used for LDA. `NULL` uses all `vars_profiles`.
#' @param seed Integer or `NULL`. Random seed for reproducibility
#'   (default: `1`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{all_inputs}{Tibble with one row per k (`n_min:n_max`). Contains
#'     clustering results and LDA fit using all input variables. Columns
#'     include `cluster_name`, `cluster_fit` (pam object), `cluster_seed`
#'     (assignment tibble), `cluster_glance`, `lda_fit`, `accuracy`,
#'     `df_append`, etc.}
#'   \item{reduced_inputs}{Same structure but LDA uses only the reduced
#'     variable set selected by [cluster_reduce_vars()].}
#' }
#'
#' @export
cluster_medoid <- function(
    df,
    vars,
    vars_profiles,
    solution_name,
    solution_name_prefix = "medoid",
    resp_id_name = NULL,
    filter_name = NULL,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = NULL,
    priors = c("equal", "size"),
    iter_max = 1000,
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


  result <- tibble::tibble("n" = n_min : n_max) %>%
    mutate(
      "solution_name" = solution_name,
      "cluster_name" = glue("{solution_name_prefix}_{solution_name}{n}"),
      "inputs" = list(vars),
      "profiles" = list(vars_profiles),
      "cluster_fit" = purrr::map(n, possibly(~{
        if(!is.null(seed)) set.seed(seed)
        cluster::pam(df_temp, .x, variant = "faster")
      }, otherwise = NA)),
      "cluster_seed" = purrr::map2(cluster_fit, cluster_name, possibly(~{
        pluck(.x, "clustering") %>%
          bind_cols(id_temp, .) %>%
          set_names(c("id", .y)) %>%
          suppressMessages()
      }, otherwise = NA)),
      "cluster_glance" = purrr::map(cluster_fit, possibly(broom::glance, otherwise = NA)),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~.x[[.y]] %>% table_percent(), otherwise = NA)),
      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, possibly(~{
        if(!is.null(seed)) set.seed(seed)
        cluster_reduce_vars(df_temp, reduced_vars, .x[[.y]], type = "greedy_step", return_only_var = TRUE, seed = seed)
      }, otherwise = NA)),
      "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
    )


  if(!is.null(reduced_inputs_max)){
    result <- result %>%
      mutate(
        "reduced_inputs" = purrr::map(reduced_inputs, head, reduced_inputs_max),
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
    reduced_inputs = result_reduced
  )


  return(output)
}



