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
#'
#' @export
impute_data <- function(
    df,
    vars,
    threshold_impute = .8,
    make_factor = TRUE,
    un_make_factor = FALSE
){

  vars <- vars %>%
    unlist() %>%
    setNames(NULL)

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

  df_imputed
}
