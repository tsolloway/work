#' means_summaries
#'
#' @description Generate four means summary tables from a `stack_data()` result:
#'   unweighted, weighted, filtered to assigned, and filtered to assigned +
#'   weighted. Skips weighted variants if no weight variable exists, and skips
#'   assigned variants if no assigner variable exists.
#'
#' @param stack_result List returned by `stack_data()`. Must contain `df_stack`,
#'   `dictionary_stack`, and `var_types`.
#'
#' @return A named list with up to five tibbles:
#'   \item{means_stacked}{Unweighted means across all rows.}
#'   \item{means_stacked_weighted}{Weighted means across all rows.}
#'   \item{means_stacked_assigned}{Unweighted means filtered to assigned rows.}
#'   \item{means_stacked_assigned_weighted}{Weighted means filtered to assigned rows.}
#'   \item{subgroup_count}{Subgroup counts from the flat data (last, if
#'     subgroups and `df_flat` exist).}
#'
#' @export
means_summaries <- function(stack_result){

  df_stack   <- stack_result[["df_stack"]]
  dictionary <- stack_result[["dictionary_stack"]]
  var_types  <- stack_result[["var_types"]]

  summary_vars <- dictionary %>%
    dplyr::filter(dvs | ivs)

  stack_labels <- rlang::set_names(summary_vars[["label"]], summary_vars[["var"]])

  has_weight   <- length(var_types[["weight"]]) > 0
  has_assigner <- length(var_types[["assigner"]]) > 0

  weight_var   <- if(has_weight) var_types[["weight"]] else NULL
  assigner_var <- if(has_assigner) var_types[["assigner"]] else NULL

  has_subgroups <- length(var_types[["subgroups"]]) > 0
  df_flat <- stack_result[["df_flat"]]

  output <- list()

  output[["means_stacked"]] <- means_summary(
    df_stack = df_stack,
    stack_labels = stack_labels
  )

  if(has_weight){
    output[["means_stacked_weighted"]] <- means_summary(
      df_stack = df_stack,
      stack_labels = stack_labels,
      weight = weight_var
    )
  }

  if(has_assigner){
    df_assigned <- df_stack %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(assigner_var), ~. == 1))

    output[["means_stacked_assigned"]] <- means_summary(
      df_stack = df_assigned,
      stack_labels = stack_labels
    )

    if(has_weight){
      output[["means_stacked_assigned_weighted"]] <- means_summary(
        df_stack = df_assigned,
        stack_labels = stack_labels,
        weight = weight_var
      )
    }
  }

  if(has_subgroups && !is.null(df_flat)){
    subgroup_vars <- var_types[["subgroups"]]
    output[["subgroup_count"]] <- df_flat %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(subgroup_vars), ~sum(., na.rm = TRUE))
      ) %>%
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "Subgroup",
        values_to = "Count"
      ) %>%
      dplyr::mutate(Subgroup = gsub("_", " ", Subgroup))
  }

  return(output)

}
