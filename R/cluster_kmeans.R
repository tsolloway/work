#' cluster_kmeans
#'
#' @description K-means clustering wrapper for the segmentation pipeline.
#'   Runs `stats::kmeans()` across a range of cluster counts (k), reduces
#'   inputs via discriminant analysis, and pipes results through
#'   [cluster_add_lda()] to produce a standardized output.
#'
#'   **What k-means does.** K-means partitions n respondents into k groups by
#'   minimizing total within-cluster variance (sum of squared Euclidean
#'   distances from each point to its cluster centroid). The algorithm
#'   alternates between (1) assigning each respondent to the nearest centroid
#'   and (2) recomputing centroids as the mean of their assigned respondents,
#'   until assignments stabilize. Because the result depends on the initial
#'   random centroids, `nstart` independent runs are performed and the best
#'   (lowest total within-SS) is kept.
#'
#'   **Strengths.** Fast, scalable, well-understood. Works well when clusters
#'   are roughly spherical and similarly sized. The `nstart` parameter helps
#'   avoid poor local optima.
#'
#'   **Limitations.** Optimizes variance, not segment differentiation — a
#'   low-variance solution may still show poor separation on the variables
#'   that matter for the segmentation. Sensitive to outliers (centroids get
#'   pulled toward extreme values). Assumes equal-variance spherical clusters.
#'
#'   **Pipeline integration.** This is a method wrapper called by
#'   [cluster_solution_family()] (and indirectly by [seg_cluster_input_sheet()]).
#'   All method wrappers share the same signature and return shape so they
#'   can be dispatched interchangeably. The return value is a list with two
#'   tibbles (`all_inputs` and `reduced_inputs`), each containing one row per
#'   k with clustering results, LDA fit, predictions, and accuracy.
#'
#' @param df A data frame containing shell data (the full dataset including
#'   all respondents, not just filtered ones).
#' @param vars Character vector of input variable names to cluster on.
#'   Typically source variables (rescaled survey items) or RS variables
#'   (row-standardized polar scores).
#' @param vars_profiles Character vector of profile variable names
#'   corresponding to `vars` (binary polar indicators). Used for LDA
#'   evaluation — these map 1:1 with `vars`.
#' @param solution_name Character. Solution family identifier (e.g. `"A"`,
#'   `"B"`). Used in cluster naming.
#' @param solution_name_prefix Character. Prefix for cluster names
#'   (default: `"kmeans"`). Combined with `solution_name` and k to form
#'   names like `"kmeans_A4"`, `"kmeans_A5"`.
#' @param resp_id_name Character. Respondent ID column name
#'   (default: `"seg_uuid"`).
#' @param filter_name Character or `NULL`. Column name containing a logical
#'   filter vector. When non-NULL, only rows where this column is `TRUE` are
#'   used for clustering. `NULL` uses all rows.
#' @param n_min Integer. Minimum number of clusters (default: `4`).
#' @param n_max Integer. Maximum number of clusters (default: `7`).
#' @param reduced_inputs_max Integer or `NULL`. Cap on the number of reduced
#'   input variables. When non-NULL, the greedy variable selection is
#'   truncated to this many variables. `NULL` uses all selected variables.
#' @param priors Character. Prior probability method for LDA: `"equal"`
#'   (uniform priors) or `"size"` (proportional to segment sizes)
#'   (default: `"equal"`).
#' @param iter_max Integer. Maximum iterations per k-means run
#'   (default: `1000`). Passed to `stats::kmeans(iter.max)`.
#' @param nstart Integer. Number of random starts — k-means is run this many
#'   times with different random centroids and the best result is kept
#'   (default: `10`). Passed to `stats::kmeans(nstart)`.
#' @param lda_vars Character vector or `NULL`. Override which input variables
#'   are used for LDA. When non-NULL, only variables in both `vars` and
#'   `lda_vars` are used. `NULL` uses all `vars`.
#' @param lda_vars_profiles Character vector or `NULL`. Override which profile
#'   variables are used for LDA. Paired with `lda_vars`. `NULL` uses all
#'   `vars_profiles`.
#' @param seed Integer or `NULL`. Random seed for reproducibility
#'   (default: `1`). `NULL` skips seeding.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{all_inputs}{Tibble with one row per k (`n_min:n_max`). Contains
#'     clustering results and LDA fit using all (or `lda_vars`) input
#'     variables. Columns include `cluster_name`, `cluster_fit` (kmeans
#'     object), `cluster_seed` (assignment tibble), `cluster_glance`
#'     (broom summary), `lda_fit`, `accuracy`, `df_append`, etc.}
#'   \item{reduced_inputs}{Same structure as `all_inputs` but LDA uses only
#'     the reduced variable set selected by [cluster_reduce_vars()].}
#' }
#'
#' @export
cluster_kmeans <- function(
    df,
    vars,
    vars_profiles,
    solution_name,
    solution_name_prefix = "kmeans",
    resp_id_name = "seg_uuid",
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

    # df = seg[["data"]][["with_solutions"]]
    # vars = c(
    #   seg_get_vars(seg, type = "polars", .return = "rs")#,
    #   # seg_get_vars(seg, block = "CVP", type = "profiles")
    # )
    # vars_profiles = c(
    #   seg_get_vars(seg, type = "polars", .return = "profiles")#,
    #   # seg_get_vars(seg, block = "CVP", type = "profiles")
    # )
    # solution_name = "foo"
    # resp_id_name = "seg_uuid"
    # filter_name = NULL
    # n_min = 4
    # n_max = 7
    # reduced_inputs_max = NULL
    # priors = "equal"
    # iter_max = 100000
    # nstart = 10
    # lda_vars = NULL


  if(!is.null(seed)) set.seed(seed)

  priors <- match.arg(priors)


  if(!is.null(filter_name)){
    df_temp <- df %>%
      dplyr::filter(.data[[filter_name]])
  }else{
    df_temp <- df
  }


  df_temp <- df_temp %>%
    dplyr::select(dplyr::all_of(c(resp_id_name, vars))) %>%
    na.exclude()


  id <- df[[resp_id_name]]
  id_temp <- df_temp[[resp_id_name]]


  df_temp <- df_temp %>% dplyr::select(-dplyr::all_of(resp_id_name))


  if(!is.null(lda_vars)){
    reduced_vars <- vars[vars %in% lda_vars]
    reduced_vars_profiles <- vars_profiles[vars_profiles %in% lda_vars_profiles]
  }else{
    reduced_vars <- vars
    reduced_vars_profiles <- vars_profiles
  }


  result <- tibble::tibble("n" = n_min : n_max) %>%
    dplyr::mutate(
      "solution_name" = solution_name,
      "cluster_name" = glue::glue("{solution_name_prefix}_{solution_name}{n}"),
      "inputs" = list(vars),
      "profiles" = list(vars_profiles),
      "cluster_fit" = purrr::map(n, purrr::possibly(~{
        if(!is.null(seed)) set.seed(seed)
        stats::kmeans(df_temp, .x, iter.max = iter_max, nstart = nstart)
      }, otherwise = NA)),
      "cluster_seed" = purrr::map2(cluster_fit, cluster_name, purrr::possibly(~{
        purrr::pluck(.x, "cluster") %>% dplyr::bind_cols(id_temp, .) %>% rlang::set_names(c("id", .y)) %>% suppressMessages()
      }, otherwise = NA)),
      "cluster_glance" = purrr::map(cluster_fit, purrr::possibly(broom::glance, otherwise = NA)),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~.x[[.y]] %>% table_percent(), otherwise = NA)),
      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~{
        if(!is.null(seed)) set.seed(seed)
        cluster_reduce_vars(df_temp, reduced_vars, .x[[.y]], type = "greedy_step", return_only_var = TRUE, seed = seed)
      }, otherwise = NA)),
      "reduced_profiles" = purrr::map(reduced_inputs, ~reduced_vars_profiles[match(.x, reduced_vars)])
    )



  if(!is.null(reduced_inputs_max)){
    result <- result %>%
      dplyr::mutate(
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

