#' seg_cluster_variability
#'
#' @description Flags respondents with extreme variability or side bias across
#'   polar variables, producing an `okay_filter` column that can be used to
#'   exclude outliers before clustering.
#'
#' Variability is measured as the row-wise standard deviation across polar
#' variables. Side bias is measured as the count of polar variables falling
#' below the midpoint of the scale. Respondents in the tails of either
#' distribution are flagged.
#'
#' When `filter_logical_vector` is supplied, cutoffs are computed on the
#' filtered subset only, and the resulting columns are joined back onto the
#' full data frame via `var_id`.
#'
#' @param df A data frame containing the polar variables.
#' @param vars Character vector of polar variable names to evaluate.
#' @param vary_percent Numeric (0–1). Fraction of respondents to flag at each
#'   tail of the row-wise SD distribution. `NULL` skips the variability filter.
#' @param side_bias_percent Numeric (0–1). Fraction of respondents to flag at
#'   each tail of the side-bias sum distribution. `NULL` skips the side-bias
#'   filter.
#' @param filter_logical_vector Character name of a logical column in `df` used
#'   to subset rows before computing cutoffs. The resulting filter columns are
#'   joined back onto the full data frame. `NULL` (default) uses all rows.
#' @param var_id Character name of the ID column used to join filtered results
#'   back to the full data frame. Required when `filter_logical_vector` is set.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{df}{The input data frame with added filter columns
#'       (`variability`, `ok_variability`, `side_bias_sum`, `ok_side_bias`,
#'       `okay_filter`).}
#'     \item{ok_filter_exists}{Logical indicating whether any filter was applied.}
#'   }
#'
#' @export
seg_cluster_variability <- function(df, vars, vary_percent = .1, side_bias_percent = .1, filter_logical_vector = NULL, var_id = NULL){


  if(!is.null(filter_logical_vector)){
    df_original <- df
    df <- df %>% dplyr::filter(.data[[filter_logical_vector]])
  }


  if(!is.null(vary_percent)){

    df[, "variability"] <- df %>%
      dplyr::select(dplyr::all_of(vars)) %>%
      as.matrix() %>%
      matrixStats::rowSds(na.rm = TRUE)


    variability_cut_offs <- df[, "variability"] %>%
      unlist() %>%
      quantile(c(vary_percent/2, 1-(vary_percent/2)), na.rm = TRUE)


    df <- df %>% dplyr::mutate(
      ok_variability = dplyr::case_when(
        variability >= tail(variability_cut_offs, 1) ~ FALSE,
        variability <= head(variability_cut_offs, 1) ~ FALSE,
        .default = TRUE)
    )
  }


  if(!is.null(side_bias_percent)){
    polar_values <- df[, vars] %>% unlist() %>% unique() %>% sort()

    side_bias_threshold <- mean(polar_values, na.rm = TRUE)

    df[, "side_bias_sum"] <- df %>%
      dplyr::select(dplyr::all_of(vars)) %>%
      dplyr::mutate(dplyr::across(everything(), ~as.integer(.x < side_bias_threshold))) %>%
      rowSums(na.rm = TRUE)


    side_bias_cut_offs <- df[, "side_bias_sum"] %>%
      unlist() %>%
      quantile(c(side_bias_percent/2, 1-(side_bias_percent/2)), na.rm = TRUE)


    df <- df %>% dplyr::mutate(
      ok_side_bias = dplyr::case_when(
        side_bias_sum >= tail(side_bias_cut_offs, 1) ~ FALSE,
        side_bias_sum <= head(side_bias_cut_offs, 1) ~ FALSE,
        .default = TRUE)
    )
  }


  if(!is.null(side_bias_percent) || !is.null(vary_percent)){
    ok_filter_exists <- TRUE

    df[, "okay_filter"] <- df %>%
      dplyr::select(dplyr::any_of(c("ok_variability", "ok_side_bias"))) %>%
      rowSums() %>% equals(., max(.))

  }else{
    ok_filter_exists <- FALSE
  }


  if(!is.null(filter_logical_vector)){
    df <- df %>% dplyr::select(dplyr::all_of(c(var_id, setdiff(names(df), names(df_original)))))

    df <- dplyr::left_join(
      df_original,
      df,
      by = var_id
    )
  }


  result <- list(
    df = df,
    ok_filter_exists = ok_filter_exists
  )

  return(result)
}
