#' seg_bind_summary_tables
#'
#' @description Binds all per-family solution tables into a single global
#'   summary table. Drops the `df_solution` list-column from the result since
#'   the combined table is for display/lookup, not for rebuilding segment
#'   assignments (use per-family `solution_table` for that).
#'
#' @param seg A seg object with `seg$solutions$analysis` populated.
#'
#' @return A tibble with one row per solution across all families. Columns:
#'   `solution_name`, `n`, `cluster_name`, `lda_name`, `lda_inputs`,
#'   `lda_profiles`, `lda_coefficient_function`, `n_segments`, `accuracy`,
#'   `kappa`, `cv`, `collinear`, `split_half`.
#'
#' @export
seg_bind_summary_tables <- function(seg) {
  seg[["solutions"]][["analysis"]] %>%
    purrr::map(purrr::pluck, "solution_table") %>%
    dplyr::bind_rows() %>%
    dplyr::select(-dplyr::any_of("df_solution"))
}
