#' cluster_spectral
#'
#' @description Spectral clustering wrapper for the segmentation pipeline.
#'   Runs `kernlab::specc()` across a range of cluster counts (k), reduces
#'   inputs via discriminant analysis, and pipes results through
#'   [cluster_add_lda()] to produce a standardized output.
#'
#'   **What spectral clustering does.** Spectral clustering embeds respondents
#'   into a low-dimensional space derived from the eigenvectors of a similarity
#'   (affinity) matrix, then clusters in that space. Specifically: (1) compute
#'   a pairwise similarity matrix using an RBF (Gaussian) kernel, (2) form the
#'   normalized graph Laplacian, (3) take the top k eigenvectors, (4) run
#'   k-means on the eigenvector embedding. Because the embedding captures the
#'   connectivity structure of the data rather than raw Euclidean distances,
#'   spectral clustering can find clusters with non-convex shapes that k-means
#'   and PAM miss entirely.
#'
#'   **Strengths.** Can discover clusters of arbitrary shape — spirals, rings,
#'   interleaving crescents — that distance-based methods (k-means, PAM) cannot
#'   separate. The RBF kernel bandwidth (`sigma`) is estimated automatically by
#'   `kernlab::specc()`. No assumption of spherical or equal-sized clusters.
#'   Works well when the cluster structure is defined by density connectivity
#'   rather than proximity to a center.
#'
#'   **Limitations.** Requires computing and eigen-decomposing an n x n
#'   similarity matrix — O(n^2) memory and O(n^3) time for the eigendecomposition.
#'   This makes it significantly slower than k-means for large datasets. The
#'   quality of results is sensitive to the kernel bandwidth parameter. The
#'   final step is still k-means on the embedding, so results can vary across
#'   runs (mitigated by setting `seed`). No built-in `broom::glance()` support,
#'   so the return value has no `cluster_glance` column.
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
#'   (default: `"spectral"`). Combined with `solution_name` and k to form
#'   names like `"spectral_A4"`, `"spectral_A5"`.
#' @param resp_id_name Character or `NULL`. Respondent ID column name.
#' @param filter_name Character or `NULL`. Column name containing a logical
#'   filter vector. `NULL` uses all rows.
#' @param n_min Integer. Minimum number of clusters (default: `4`).
#' @param n_max Integer. Maximum number of clusters (default: `7`).
#' @param reduced_inputs_max Integer or `NULL`. Cap on the number of reduced
#'   input variables. `NULL` uses all selected variables.
#' @param priors Character. Prior probability method for LDA: `"equal"` or
#'   `"size"` (default: `"equal"`).
#' @param iter_max Integer. Not used by specc but kept for signature
#'   compatibility with other method wrappers.
#' @param nstart Integer. Not used by specc but kept for signature
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
#'     include `cluster_name`, `cluster_fit` (specc object), `cluster_seed`
#'     (assignment tibble), `lda_fit`, `accuracy`, `df_append`, etc.
#'     Note: no `cluster_glance` column (broom does not support specc).}
#'   \item{reduced_inputs}{Same structure but LDA uses only the reduced
#'     variable set selected by [cluster_reduce_vars()].}
#' }
#'
#' @export
cluster_spectral <- function(
    df, vars, vars_profiles, solution_name,
    solution_name_prefix = "spectral",
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
        kernlab::specc(as.matrix(df_temp), centers = .x)
      }, otherwise = NA)),
      "cluster_seed" = purrr::map2(cluster_fit, cluster_name, purrr::possibly(~{
        as.integer(.x) %>%
          dplyr::bind_cols(id_temp, .) %>%
          setNames(c("id", .y)) %>%
          suppressMessages()
      }, otherwise = NA)),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~.x[[.y]] %>% table_percent(), otherwise = NA)),
      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~{
        if(!is.null(seed)) set.seed(seed)
        cluster_reduce_vars(df_temp, reduced_vars, .x[[.y]], type = "greedy_step", return_only_var = TRUE, seed = seed)
      }, otherwise = NA)),
      "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, reduced_vars)])
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
