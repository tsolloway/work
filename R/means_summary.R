#' means_summary
#'
#' @description Compute brand-level and total means and counts for stacked
#'   survey data. Returns a summary tibble with Variable, Label, Total,
#'   Total - N, then interleaved brand mean and N columns.
#'
#' @param df_stack Stacked data frame (one row per respondent-brand).
#' @param stack_labels Named character vector of variable labels for the
#'   variables to summarise (names = variable names in `df_stack`).
#' @param weight Name of a weight column in `df_stack`. If provided, means are
#'   computed as weighted means via `stats::weighted.mean()`. Default `NULL`
#'   (unweighted).
#'
#' @return A tibble with one row per variable, columns for total and per-brand
#'   means and counts.
#'
#' @export
means_summary <- function(
    df_stack,
    stack_labels,
    weight = NULL
){

  vars <- names(stack_labels) %>% unname()

  summarise_vars <- function(df){
    w_col <- weight

    df %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(vars),
          list(
            Mean = if(!is.null(w_col)){
              function(x, w = dplyr::pick(dplyr::all_of(w_col))[[1]]){
                stats::weighted.mean(x, w = w, na.rm = TRUE)
              }
            } else {
              function(x) mean(x, na.rm = TRUE)
            },
            n = ~sum(!is.na(.))
          ),
          .names = "{.col}__{.fn}"
        )
      )
  }

  df_by_brand <- df_stack %>%
    dplyr::group_by(brand_number, brand_name) %>%
    summarise_vars() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Brand = glue::glue("{brand_number} - {brand_name}")
    ) %>%
    dplyr::select(-brand_number, -brand_name) %>%
    tidyr::pivot_longer(
      -Brand,
      names_to = c("Variable", ".value"),
      names_sep = "__"
    )

  df_total <- df_stack %>%
    summarise_vars() %>%
    tidyr::pivot_longer(
      everything(),
      names_to = c("Variable", ".value"),
      names_sep = "__"
    ) %>%
    dplyr::rename(Total = Mean, `Total - N` = n)

  df_summary <- df_by_brand %>%
    tidyr::pivot_wider(
      names_from = Brand,
      values_from = c(Mean, n),
      names_glue = "{Brand}__{.value}"
    ) %>%
    dplyr::left_join(df_total, by = dplyr::join_by(Variable)) %>%
    dplyr::relocate(Variable, Total, `Total - N`) %>%
    dplyr::left_join(
      tibble::tibble(
        Variable = names(stack_labels),
        Label = stack_labels
      ),
      by = dplyr::join_by(Variable)
    ) %>%
    dplyr::relocate(Label, .after = Variable) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 4)))

  brand_names <- df_summary %>%
    dplyr::select(-Variable, -Label, -Total, -`Total - N`) %>%
    names()

  mean_cols <- brand_names[grepl("__Mean$", brand_names)]
  n_cols <- brand_names[grepl("__n$", brand_names)]

  col_order <- c("Variable", "Label", "Total", "Total - N")
  for(i in seq_along(mean_cols)){
    col_order <- c(col_order, mean_cols[i], n_cols[i])
  }

  df_summary <- df_summary %>%
    dplyr::select(dplyr::all_of(col_order))

  clean_names <- names(df_summary) %>%
    gsub("__Mean$", "", .) %>%
    gsub("__n$", " - N", .)
  names(df_summary) <- clean_names

  return(df_summary)

}
