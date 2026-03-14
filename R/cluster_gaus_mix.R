#' cluster_gaus_mix
#'
#' @description Gaussian mixture model clustering wrapper for the segmentation
#'   pipeline. Runs `mclust::Mclust()` across a range of cluster counts (k),
#'   reduces inputs via discriminant analysis, and pipes results through
#'   [cluster_add_lda()] to produce a standardized output.
#'
#'   **What Gaussian mixture models do.** A GMM models the data as a mixture
#'   of k multivariate Gaussian (normal) distributions, each with its own
#'   mean vector and covariance matrix. The EM (Expectation-Maximization)
#'   algorithm iterates between (1) computing the posterior probability that
#'   each respondent belongs to each component (E-step) and (2) updating the
#'   Gaussian parameters to maximize the data likelihood (M-step). Final
#'   assignments use hard classification — each respondent is assigned to
#'   their most probable component.
#'
#'   **Strengths.** The most flexible geometric model of the four distance-
#'   based methods. Clusters can be ellipsoidal (not just spherical), with
#'   different sizes, shapes, and orientations. mclust automatically selects
#'   the best covariance structure (e.g. VVV = variable volume, shape, and
#'   orientation) via BIC. Produces soft probabilities — useful for
#'   identifying respondents who sit between segments.
#'
#'   **Limitations.** Slowest of the four methods, especially with many
#'   variables (covariance estimation is O(p^2) per component). Can fail
#'   to converge or produce degenerate solutions when components collapse
#'   (singular covariance). Like all distance-based methods, optimizes
#'   likelihood rather than segment differentiation. mclust masks `map()`
#'   from purrr, so the function loads and unloads the package explicitly.
#'
#'   **Pipeline integration.** This is a method wrapper called by
#'   [cluster_solution_family()] (and indirectly by [seg_cluster_input_sheet()]).
#'   All method wrappers share the same signature and return shape so they
#'   can be dispatched interchangeably. Note: mclust is loaded via
#'   `require()` and unloaded after fitting to avoid namespace conflicts
#'   with purrr.
#'
#' @param df A data frame containing shell data (the full dataset including
#'   all respondents, not just filtered ones).
#' @param vars Character vector of input variable names to cluster on.
#' @param vars_profiles Character vector of profile variable names
#'   corresponding to `vars` (binary polar indicators).
#' @param solution_name Character. Solution family identifier (e.g. `"A"`).
#' @param solution_name_prefix Character. Prefix for cluster names
#'   (default: `"gaus_mix"`). Combined with `solution_name` and k to form
#'   names like `"gaus_mix_A4"`, `"gaus_mix_A5"`.
#' @param resp_id_name Character or `NULL`. Respondent ID column name.
#' @param filter_name Character or `NULL`. Column name containing a logical
#'   filter vector. `NULL` uses all rows.
#' @param n_min Integer. Minimum number of clusters (default: `4`).
#' @param n_max Integer. Maximum number of clusters (default: `7`).
#' @param reduced_inputs_max Integer or `NULL`. Cap on the number of reduced
#'   input variables. `NULL` uses all selected variables.
#' @param priors Character. Prior probability method for LDA: `"equal"` or
#'   `"size"` (default: `"equal"`).
#' @param iter_max Integer. Not used by Mclust but kept for signature
#'   compatibility with other method wrappers.
#' @param nstart Integer. Not used by Mclust but kept for signature
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
#'     include `cluster_name`, `cluster_fit` (Mclust object), `cluster_seed`
#'     (assignment tibble), `lda_fit`, `accuracy`, `df_append`, etc.
#'     Note: no `cluster_glance` column (broom does not support Mclust).}
#'   \item{reduced_inputs}{Same structure but LDA uses only the reduced
#'     variable set selected by [cluster_reduce_vars()].}
#' }
#'
#' @export
cluster_gaus_mix <- function(
    df, vars, vars_profiles, solution_name,
    solution_name_prefix = "gaus_mix",
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

  require(mclust) %>% suppressMessages()

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
        mclust::Mclust(df_temp, .x, verbose = FALSE)
      }, otherwise = NA)),
      "cluster_seed" = purrr::map2(cluster_fit, cluster_name, purrr::possibly(~{
        purrr::pluck(.x, "classification") %>%
          dplyr::bind_cols(id_temp, .) %>%
          rlang::set_names(c("id", .y)) %>%
          suppressMessages()
      }, otherwise = NA)),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~.x[[.y]] %>% table_percent(), otherwise = NA)),
      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~{
        if(!is.null(seed)) set.seed(seed)
        cluster_reduce_vars(df_temp, reduced_vars, .x[[.y]], type = "greedy_step", return_only_var = TRUE, seed = seed) %>%
          suppressWarnings()
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


  purrr::possibly(~detach("package:mclust", unload=TRUE))()


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
