#' cluster_prototype_seed
#' @description cluster_prototype_seed
#' @export
cluster_prototype_seed <- function(
    seg, solution_family_name, seed_name, seed, vars = NULL, vars_profiles = NULL,
    reduced_inputs_max = 14, priors = c("size", "equal"), id_name = "seg_uuid"
){

  priors <- match.arg(priors)

  if(is.null(vars)){
    vars <- seg[["input_sheet"]][["input_table"]][["rs_var"]] %>% as.character()
    vars_profiles <- seg[["input_sheet"]][["input_table"]][["profile_var"]] %>% as.character()
  }

  df <- seg[["data"]][["with_shell"]]


  results <- tibble::tibble(
    n = max(seed[[seed_name]], na.rm = TRUE),
    solution_name = solution_family_name,
    cluster_name = seed_name,
    inputs = list(vars),
    profiles = list(vars_profiles),
    cluster_fit = NA,
    cluster_seed = list(seed %>% setNames(c("id", seed_name))),
    cluster_glance = NA,
    priors_equal = purrr::map(n, ~rep(1/.x, .x)),
    priors_size = purrr::map2(cluster_seed, cluster_name, ~.x[[.y]] %>% table_percent()),
    reduced_inputs = purrr::map2(cluster_seed, cluster_name, ~cluster_reduce_vars(df, vars, .x[[.y]], type = "greedy_step", return_only_var = TRUE)),
    reduced_profiles = purrr::map(reduced_inputs, ~vars_profiles[match(vars, .x) %>% remove_na()])
  )


  if(!is.null(reduced_inputs_max)){
    results <- results %>%
      mutate(
        "reduced_inputs" = purrr::map(reduced_inputs, head, reduced_inputs_max),
        "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(vars, .x) %>% remove_na()])
      )
  }


  result_all <- results %>%
    cluster_add_lda(
      df = df, id_name = id_name,
      filter_name = NULL, priors = priors,
      use_reduced = FALSE
    )


  result_reduced <- results %>%
    cluster_add_lda(
      df = df, id_name = id_name,
      filter_name = NULL, priors = priors,
      use_reduced = TRUE
    )


  if(is_truthy(seg[["solutions"]][["solutions"]][[solution_family_name]])){
    solution_family_results <- seg[["solutions"]][["solutions"]][[solution_family_name]]
  }else{
    solution_family_results <- list()
  }


  solution_family_results[["result"]][["prototype"]] <- list(
    all_inputs = result_all,
    reduced_inputs = result_reduced
  )


  solution_family_results[["solution_table"]] <- solution_family_results[["result"]] %>%
    discard_at("hierarchical_fit") %>%
    flatten() %>%
    purrr::map(~dplyr::select(.x, solution_name, n, cluster_name, lda_name, lda_inputs, lda_profiles, confusion, accuracy, df_append)) %>%
    bind_rows()


  solution_family_results[["df_segment_append"]] <- solution_family_results[["solution_table"]] %>%
    dplyr::select(df_append) %>%
    unlist(recursive = FALSE) %>%
    reduce(full_join, by = "id") %>%
    dplyr::select(-ends_with(".y")) %>%
    setNames(., names(.) %>% gsub(".x", "", .))


  seg[["solutions"]][["solutions"]][[solution_family_name]] <- solution_family_results


  solution_table <- seg[["solutions"]][["solutions"]] %>%
    map(pluck, "solution_table") %>%
    bind_rows()


  df_segment_append <- seg[["solutions"]][["solutions"]] %>%
    map(pluck, "df_segment_append") %>%
    reduce(full_join, by = "id")


  df <- left_join(
    seg[["data"]][["with_shell"]],
    df_segment_append,
    by = join_by(seg_uuid == id)
  )


  seg[["solutions"]][["summary_table"]] <- solution_table
  seg[["solutions"]][["df_segment_append"]] <- df_segment_append
  seg[["data"]][["with_solutions"]] <- df


  return(seg)

}

