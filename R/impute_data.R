#' impute_data
#'
#' @description Impute missing values using a Bayesian Network learned from
#'   complete cases. Rows below a completeness threshold are dropped, a tabu
#'   structure is learned on the remaining complete cases, and the fitted model
#'   is used to impute any remaining missing values via `bnlearn::impute()`.
#'
#' @param df Data frame to impute.
#' @param vars Character vector (or named list of character vectors) specifying
#'   the columns to impute. Lists are flattened and names are stripped.
#' @param id Name of the row-id column used to re-join `weight` after
#'   imputation. Default `NULL`, which is treated as `"uuid_stack"`. Only used
#'   when `weight` is supplied.
#' @param weight Name of a weight column. Default `NULL`. If supplied, the
#'   weight is set aside before imputation (so it is excluded from the
#'   completeness filter and the BN), then joined back by `id` and placed
#'   after `brand_name` (or after `id` if `brand_name` is absent).
#' @param threshold_impute Numeric 0-1. Minimum proportion of non-missing
#'   values a row must have to be kept. Rows below this threshold are dropped
#'   before imputation. Default `0.8`.
#' @param make_factor Logical. If `TRUE` (default), columns in `vars` are
#'   converted to factors before structure learning and imputation (required
#'   by `bnlearn` for discrete networks).
#' @param un_make_factor Logical. If `TRUE`, converts imputed columns back to
#'   numeric after imputation. Default `FALSE`.
#'
#' @return A tibble with:
#'   - Rows that met the `threshold_impute` completeness requirement
#'   - All `vars` columns with missing values filled via BN imputation
#'   - Non-`vars` columns unchanged
#'
#' @details
#' The imputation workflow:
#' 1. Drop rows where the proportion of non-missing values is below
#'    `threshold_impute` (warns with count of dropped rows).
#' 2. Optionally convert `vars` to factors (`make_factor`).
#' 3. Subset to `vars` columns, isolate fully complete rows.
#' 4. Learn a tabu BN structure on complete cases, fit with MLE.
#' 5. Impute missing values in the full (filtered) subset.
#' 6. Re-attach non-`vars` columns.
#' 7. Optionally convert back to numeric (`un_make_factor`).
#' 8. If `weight` was supplied, join it back by `id` and relocate it after
#'    `brand_name`.
#'
#' @export
impute_data <- function(
    df,
    vars,
    id = NULL,
    weight = NULL,
    threshold_impute = .8,
    make_factor = TRUE,
    un_make_factor = FALSE
){

  if (is.null(id)) id <- "uuid_stack"

  vars <- vars %>%
    unlist() %>%
    setNames(NULL)

  # set weight aside so it doesn't count toward the completeness filter
  if (!is.null(weight)) {
    weight <- weight %>% unlist() %>% as.character()

    df_weight <- df %>%
      dplyr::select(dplyr::all_of(c(id, weight)))

    df <- df %>%
      dplyr::select(!dplyr::all_of(weight))
  }

  # filter to missing threshold
  df_filtered <- df[rowMeans(!is.na(df)) >= threshold_impute, ]

  if (nrow(df) != nrow(df_filtered)) {
    warning(
      glue::glue("Dropped {nrow(df) - nrow(df_filtered)} rows due to below missing threshold")
    )
  }

  if (make_factor) {
    df_filtered <- df_filtered %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(vars), as.factor))
  }

  dfx <- df_filtered %>%
    dplyr::select(dplyr::all_of(vars))

  dfx_no_missing <- dfx[rowMeans(!is.na(dfx)) == 1, ]

  df_imputed <- dfx_no_missing %>%
    as.data.frame() %>%
    bnlearn::tabu() %>%
    bnlearn::bn.fit(
      dfx_no_missing %>% as.data.frame(),
      method = "mle"
    ) %>%
    bnlearn::impute(dfx %>% as.data.frame()) %>%
    tibble::as_tibble()

  df_imputed <- df_filtered %>%
    dplyr::select(!dplyr::all_of(vars)) %>%
    dplyr::bind_cols(df_imputed)

  if (identical(df_filtered, df_imputed)) {
    warning("Data did not need to be imputed")
  }

  if (un_make_factor) {
    df_imputed <- df_imputed %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(vars), ~as.character(.) %>% as.numeric())
      )
  }

  if (!is.null(weight)) {
    after <- ifelse("brand_name" %in% names(df_imputed), "brand_name", id)

    df_imputed <- df_imputed %>%
      dplyr::left_join(df_weight, by = id) %>%
      dplyr::relocate(dplyr::all_of(weight), .after = dplyr::all_of(after))
  }

  df_imputed
}
