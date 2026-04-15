#' stack_data
#'
#' @description Reshape flat survey data into a brand-stacked (long) format.
#' Each respondent-brand combination becomes a row. Builds a dictionary
#' cataloging variable roles (id, dv, iv, subgroup, assigner, weight).
#'
#' @param df Data frame of flat (wide) survey data, one row per respondent.
#' @param instructions Named list mapping output variable names to vectors of
#'   source columns (one per brand). Each entry is pivoted into a single column.
#' @param brand_table Data frame with `id` and `name` columns defining brands.
#'   Row order must match the column order within each `instructions` entry.
#' @param labels Named character vector of variable labels (names = var names).
#' @param uuid_flat Name of the respondent ID column in `df`.
#' @param dv Character vector of variable names (from `instructions`) to tag as
#'   dependent variables in the dictionary.
#' @param ivs Named list of IV batteries. Each element is a character vector of
#'   variable names (from `instructions`) belonging to that battery.
#' @param dv_mutates Named list of expressions (e.g. `rlang::exprs(...)`) to
#'   create or overwrite DV columns on the stacked data. New names are appended
#'   to the dictionary as DVs; existing names are overwritten in place.
#' @param ivs_mutates Named list of expressions (e.g. `rlang::exprs(...)`) to
#'   create or overwrite IV columns on the stacked data. New names are appended
#'   to the dictionary as IVs with `iv_battery = "_mutated"`.
#' @param subgroups Character vector of flat-level variables to join onto the
#'   stacked data (e.g. demographics, segments).
#' @param assigner Name of a stacked variable (already in `instructions`) that
#'   indicates brand assignment (1 = assigned).
#' @param weight Name of a flat-level weight variable to join onto the stacked
#'   data.
#' @param uuid_stack Name for the generated row-level UUID column. Default
#'   `"uuid_stack"`.
#' @param remove_no_data If `TRUE` (default), drop rows where all instruction
#'   variables are `NA`.
#' @param include_mean_summaries If `TRUE` (default), run `means_summaries()`
#'   on the unfiltered stacked data and include the result as `mean_summaries`
#'   in the output list.
#' @param filter_to_assigned If `TRUE` (default), filter to rows where
#'   `assigner == 1`. Only applies when `assigner` is provided.
#' @param store_flat If `TRUE`, include the original flat data frame
#'   in the returned list as `df_flat`. Default `FALSE`.
#' @param return_only_data If `TRUE`, return just the stacked data frame instead
#'   of the full result list. Default `FALSE`.
#'
#' @return A list with components:
#'   \item{df_stack}{The stacked data frame.}
#'   \item{dictionary_stack}{Tibble cataloging each variable's role, label,
#'     IV battery, and stacking instructions.}
#'   \item{var_types}{Named list of variable name vectors by type.}
#'   \item{df_flat}{Original flat data (only if `store_flat = TRUE`).}
#'   \item{mean_summaries}{Output of `means_summaries()` (only if
#'     `include_mean_summaries = TRUE`).}
#'   If `return_only_data = TRUE`, returns `df_stack` directly.
#'
#' @export
stack_data <- function(
    df,
    instructions,
    brand_table,
    labels,
    uuid_flat,
    dv = NULL,
    ivs = NULL,
    dv_mutates = NULL,
    ivs_mutates = NULL,
    subgroups = NULL,
    assigner = NULL,
    weight = NULL,
    uuid_stack = "uuid_stack",
    remove_no_data = TRUE,
    include_mean_summaries = TRUE,
    filter_to_assigned = TRUE,
    store_flat = FALSE,
    return_only_data = FALSE
){


  # dv = NULL
  # ivs = NULL
  # dv_mutates = NULL
  # ivs_mutates = NULL
  # subgroups = NULL
  # assigner = NULL
  # weight = NULL
  # uuid_stack = "uuid_stack"
  # remove_no_data = TRUE
  # include_mean_summaries = TRUE
  # filter_to_assigned = TRUE
  # store_flat = FALSE
  # return_only_data = FALSE


  tagged <- c(
    assigner, weight,
    subgroups %>% unlist(),
    dv %>% unlist(),
    ivs %>% unlist()
  ) %>% as.character()

  other <- setdiff(names(instructions), tagged)

  var_types <- list(
    ids = c(uuid_stack, uuid_flat, "brand_number", "brand_name"),
    assigner = assigner %>% unlist() %>% as.character(),
    weight = weight %>% unlist() %>% as.character(),
    subgroups = subgroups %>% unlist() %>% as.character(),
    dvs = dv %>% unlist() %>% as.character(),
    ivs = ivs %>% purrr::map(~unlist(.) %>% as.character()),
    other = other
  )


  dictionary <- tibble::tibble(
    var = var_types %>% unlist(),
    id = var %in% var_types[["ids"]],
    subgroup = var %in% var_types[["subgroups"]],
    assigner = var %in% var_types[["assigner"]],
    weight = var %in% var_types[["weight"]],
    dvs = var %in% var_types[["dvs"]],
    ivs = var %in% unlist(var_types[["ivs"]]),
    other = var %in% var_types[["other"]]
  ) %>% dplyr::left_join(
    tibble::tibble(
      var = names(labels),
      label = labels
    ),
    by = dplyr::join_by(var)
  ) %>%
    dplyr::left_join(
      var_types[["ivs"]] %>%
        purrr::imap(
          ~tibble::tibble(
            iv_battery = .y,
            var = .x
          )
        ) %>%
        dplyr::bind_rows(),
      by = dplyr::join_by(var)
    ) %>%
    dplyr::left_join(
      instructions %>%
        purrr::imap(
          ~tibble::tibble(
            var = .y,
            stack_instructions = list(.x %>% unlist() %>% as.character())
          )
        ) %>%
        dplyr::bind_rows(),
      by = dplyr::join_by(var)
    ) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        var == uuid_flat ~ uuid_flat,
        var == uuid_stack ~ uuid_stack,
        is.na(label)  ~ var %>% gsub("_", " ", .) %>% stringr::str_to_title(),
        .default = label
      )
    ) %>%
    dplyr::relocate(
      label, .after = var
    )



  df_stack <- instructions %>%
    purrr::imap(~{
      df %>% dplyr::select(dplyr::all_of(c(uuid_flat, !!.x))) %>%
        tidyr::pivot_longer(
          -dplyr::all_of(uuid_flat),
          cols_vary = "fastest",
          values_to = .y
        ) %>%
        dplyr::select(-name) %>%
        dplyr::mutate(
          brand_number = rep(
            brand_table %>%
              dplyr::select("id") %>%
              unlist(),
            nrow(df)
          ),
          brand_name = rep(
            brand_table %>%
              dplyr::select("name") %>%
              unlist() %>%
              unname(),
            nrow(df)
          )
        ) %>%
        dplyr::relocate(
          c("brand_number", "brand_name"),
          .after = dplyr::all_of(uuid_flat)
        )
    }) %>%
    purrr::reduce(dplyr::left_join, by = c(uuid_flat, "brand_number", "brand_name"))



  if(remove_no_data){
    df_stack <- df_stack %>%
      dplyr::filter(
        rowSums(dplyr::across(names(instructions), ~ !is.na(.))) > 0
      )
  }



  flat_vars <- c(subgroups, weight)

  if(length(flat_vars) > 0){
    df_stack <- df_stack %>%
      dplyr::right_join(
        df %>%
          dplyr::select(dplyr::all_of(c(uuid_flat, flat_vars))),
        .,
        by = dplyr::join_by(!!uuid_flat)
      )
  }


  df_stack <- df_stack %>%
    dplyr::mutate(
      {{uuid_stack}} := uuid::UUIDgenerate(n = dplyr::n())
    )

  if(!is.null(dv_mutates)){
    df_stack <- df_stack %>%
      dplyr::mutate(!!!dv_mutates)

    new_dvs <- setdiff(names(dv_mutates), dictionary[["var"]])
    var_types[["dvs"]] <- c(var_types[["dvs"]], new_dvs)

    new_dict_rows <- tibble::tibble(
      var = new_dvs,
      label = new_dvs %>% gsub("_", " ", .) %>% stringr::str_to_title(),
      id = FALSE,
      subgroup = FALSE,
      assigner = FALSE,
      weight = FALSE,
      dvs = TRUE,
      ivs = FALSE,
      other = FALSE,
      iv_battery = NA_character_,
      stack_instructions = list(NULL)
    )

    dictionary <- dplyr::bind_rows(dictionary, new_dict_rows)
  }

  if(!is.null(ivs_mutates)){
    df_stack <- df_stack %>%
      dplyr::mutate(!!!ivs_mutates)

    new_ivs <- setdiff(names(ivs_mutates), dictionary[["var"]])
    var_types[["ivs"]][["_mutated"]] <- new_ivs

    new_dict_rows <- tibble::tibble(
      var = new_ivs,
      label = new_ivs %>% gsub("_", " ", .) %>% stringr::str_to_title(),
      id = FALSE,
      subgroup = FALSE,
      assigner = FALSE,
      weight = FALSE,
      dvs = FALSE,
      ivs = TRUE,
      other = FALSE,
      iv_battery = "_mutated",
      stack_instructions = list(NULL)
    )

    dictionary <- dplyr::bind_rows(dictionary, new_dict_rows)
  }

  dv_vars <- var_types[["dvs"]]
  iv_vars <- unlist(var_types[["ivs"]])
  non_dv_iv <- setdiff(dictionary[["var"]], c(dv_vars, iv_vars))
  dictionary <- dictionary %>%
    dplyr::arrange(match(var, c(non_dv_iv, dv_vars, iv_vars)))

  df_stack <- df_stack %>%
    dplyr::select(
      dictionary[["var"]] %>% unname()
    )


  result <- list(
    df_stack = df_stack,
    dictionary_stack = dictionary,
    var_types = var_types,
    df_flat = df
  )

  if(include_mean_summaries){
    result[["mean_summaries"]] <- means_summaries(result)
  }

  if(filter_to_assigned && length(var_types[["assigner"]]) > 0){
    df_stack <- df_stack %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(var_types[["assigner"]]), ~. == 1))
    result[["df_stack"]] <- df_stack
  }

  if(!store_flat){
    result[["df_flat"]] <- NULL
  }

  if(return_only_data){
    result <- df_stack
  }

  return(result)
}


