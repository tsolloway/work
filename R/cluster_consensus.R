#' cluster_consensus
#'
#' @description Consensus clustering wrapper for the segmentation pipeline.
#'   Combines cluster assignments from multiple methods into a single consensus
#'   solution using a co-association matrix approach. No external dependencies —
#'   pure base R.
#'
#'   **What consensus clustering does.** Given assignment vectors from m
#'   clustering methods (e.g. k-means, medoid, hierarchical), builds an n×n
#'   co-association matrix C where `C[i,j]` = fraction of methods that placed
#'   respondent i and respondent j in the same cluster. Converts to a
#'   dissimilarity matrix (`D = 1 - C`), applies hierarchical clustering
#'   (average linkage), and cuts at k to produce the consensus assignments.
#'   Respondents that consistently cluster together across methods end up
#'   together; one weird method result gets outvoted.
#'
#'   **Strengths.** Stabilizes results across methods — if three out of four
#'   methods agree, the consensus follows the majority. No tuning parameters
#'   beyond the methods fed in. Naturally handles methods with different
#'   assumptions (variance-based k-means, medoid-based PAM, likelihood-based
#'   Gaussian mixtures). Needs ≥ 2 contributing methods; if fewer are available
#'   for a given k, that k returns NA.
#'
#'   **Limitations.** Inherits the n×n memory cost of the co-association matrix
#'   (same as hierarchical clustering). Cannot produce better results than all
#'   input methods — it's a voting mechanism, not a discovery mechanism. If all
#'   methods agree on a bad partition, consensus will too.
#'
#'   **Pipeline integration.** This is a method wrapper called by
#'   [cluster_solution_family()] (and indirectly by [seg_cluster_input_sheet()]).
#'   Unlike standalone methods, consensus runs AFTER all other methods because
#'   it depends on their assignments. It follows the same standard return shape:
#'   a list with `all_inputs` and `reduced_inputs` tibbles.
#'
#' @param df A data frame containing shell data (the full dataset including
#'   all respondents, not just filtered ones).
#' @param vars Character vector of input variable names to cluster on.
#'   Used for variable reduction and LDA (the consensus assignments come from
#'   `method_results`, not from clustering `vars` directly).
#' @param vars_profiles Character vector of profile variable names
#'   corresponding to `vars` (binary polar indicators).
#' @param solution_name Character. Solution family identifier (e.g. `"A"`).
#' @param solution_name_prefix Character. Prefix for cluster names
#'   (default: `"consensus"`). Combined with `solution_name` and k to form
#'   names like `"consensus_A4"`, `"consensus_A5"`.
#' @param resp_id_name Character or `NULL`. Respondent ID column name.
#' @param filter_name Character or `NULL`. Column name containing a logical
#'   filter vector. `NULL` uses all rows.
#' @param n_min Integer. Minimum number of clusters (default: `4`).
#' @param n_max Integer. Maximum number of clusters (default: `7`).
#' @param reduced_inputs_max Integer or `NULL`. Cap on the number of reduced
#'   input variables. `NULL` uses all selected variables.
#' @param priors Character. Prior probability method for LDA: `"equal"` or
#'   `"size"` (default: `"equal"`).
#' @param iter_max Integer. Not used by consensus but kept for signature
#'   compatibility with other method wrappers.
#' @param nstart Integer. Not used by consensus but kept for signature
#'   compatibility with other method wrappers.
#' @param lda_vars Character vector or `NULL`. Override which input variables
#'   are used for LDA. `NULL` uses all `vars`.
#' @param lda_vars_profiles Character vector or `NULL`. Override which profile
#'   variables are used for LDA. `NULL` uses all `vars_profiles`.
#' @param seed Integer or `NULL`. Random seed for reproducibility
#'   (default: `1`).
#' @param method_results Named list. Results from other clustering methods.
#'   Each element is a list with `all_inputs` (tibble) and `reduced_inputs`
#'   (tibble), following the standard method wrapper return shape. Consensus
#'   extracts `cluster_seed` and `cluster_name` from `all_inputs` to build
#'   the co-association matrix. Needs ≥ 2 methods with valid assignments.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{all_inputs}{Tibble with one row per k (`n_min:n_max`). Contains
#'     consensus clustering results and LDA fit using all input variables.}
#'   \item{reduced_inputs}{Same structure but LDA uses only the reduced
#'     variable set selected by [cluster_reduce_vars()].}
#' }
#'
#' @export
cluster_consensus <- function(
    df, vars, vars_profiles, solution_name,
    solution_name_prefix = "consensus",
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
    seed = 1,
    method_results = list()
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


  # for each k, extract assignments from all methods and build consensus
  n_vals <- n_min:n_max

  result <- tibble::tibble("n" = n_vals) %>%
    mutate(
      "solution_name" = solution_name,
      "cluster_name" = glue("{solution_name_prefix}_{solution_name}{n}"),
      "inputs" = list(vars),
      "profiles" = list(vars_profiles),
      "cluster_seed" = purrr::map2(
        seq_along(n), cluster_name,
        purrr::possibly(function(k_idx, cname) {

          # collect assignment vectors from each method at this k index
          method_assigns <- list()

          for (mname in names(method_results)) {
            mr <- method_results[[mname]]
            ai <- mr[["all_inputs"]]

            # safety: check this method has results at this k index
            if (is.null(ai) || nrow(ai) < k_idx) next
            seed_tbl <- ai$cluster_seed[[k_idx]]
            if (is.null(seed_tbl) || !is.data.frame(seed_tbl)) next

            mclust_name <- ai$cluster_name[[k_idx]]
            ids_m <- seed_tbl[["id"]]
            assigns_m <- seed_tbl[[mclust_name]]

            if (is.null(assigns_m) || any(is.na(assigns_m))) next

            method_assigns[[mname]] <- list(ids = ids_m, assigns = assigns_m)
          }

          # need >= 2 methods
          if (length(method_assigns) < 2) return(NA)

          # align to common respondent set intersected with id_temp
          common_ids <- id_temp
          for (ma in method_assigns) {
            common_ids <- intersect(common_ids, ma$ids)
          }

          if (length(common_ids) < 2) return(NA)

          # build assignment matrix (n_common x m_methods)
          assignments_mat <- matrix(NA_integer_,
                                    nrow = length(common_ids),
                                    ncol = length(method_assigns))

          for (j in seq_along(method_assigns)) {
            ma <- method_assigns[[j]]
            idx <- match(common_ids, ma$ids)
            assignments_mat[, j] <- ma$assigns[idx]
          }

          # consensus via co-association matrix
          consensus_assign <- .consensus_assign(
            assignments_mat,
            k = n_vals[k_idx],
            seed = seed
          )

          # build output tibble matching standard cluster_seed format
          dplyr::bind_cols(
            tibble::tibble(id = common_ids),
            tibble::tibble(!!cname := consensus_assign)
          )

        }, otherwise = NA)
      ),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(
        ~.x[[.y]] %>% table_percent(), otherwise = NA
      )),
      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, purrr::possibly(~{
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
    reduced_inputs = result_reduced
  )


  return(output)
}


#' Build consensus assignments via co-association matrix
#'
#' @description Internal helper. Given an n × m assignment matrix (respondents ×
#'   methods), builds an n × n co-association matrix where `C[i,j]` = fraction
#'   of methods placing i and j in the same cluster. Converts to dissimilarity,
#'   runs hierarchical clustering (average linkage), and cuts at k.
#'
#' @param assignments_mat Integer matrix, n rows (respondents) × m columns
#'   (methods). Each entry is an integer cluster ID.
#' @param k Integer. Number of clusters to produce.
#' @param seed Integer or `NULL`. Random seed.
#'
#' @return Integer vector of length n with consensus cluster assignments.
#'
#' @keywords internal
.consensus_assign <- function(assignments_mat, k, seed = 1) {
  n <- nrow(assignments_mat)
  m <- ncol(assignments_mat)

  # co-association matrix: C[i,j] = fraction of methods agreeing
  coassoc <- matrix(0, n, n)
  for (j in seq_len(m)) {
    a <- assignments_mat[, j]
    coassoc <- coassoc + outer(a, a, "==")
  }
  coassoc <- coassoc / m

  # dissimilarity -> hierarchical clustering -> cut
  dissim <- 1 - coassoc
  diag(dissim) <- 0
  if (!is.null(seed)) set.seed(seed)
  hc <- stats::hclust(stats::as.dist(dissim), method = "average")
  as.integer(stats::cutree(hc, k = k))
}
