#' seg_cluster_prototype_seed
#'
#' @description Fits a segmentation solution from a pre-defined prototype seed.
#'   Instead of discovering clusters from scratch (like [seg_cluster_input_sheet()]
#'   or [seg_cluster_with_profiles()]), this function takes an existing set of
#'   segment assignments — the "seed" — and builds the full LDA typing tool
#'   around it.
#'
#'   Typical use case: you have a hand-crafted or modified prototype solution
#'   (e.g., from [seg_prototype_split_segments()]) where segments were manually
#'   merged, split, or reassigned. This function takes those assignments as
#'   ground truth and produces the discriminant analysis, confusion matrix,
#'   accuracy metrics, and segment append columns — everything needed to
#'   evaluate and export the solution.
#'
#'   **Workflow:**
#'   1. Extracts the seed assignments (from `seg$data$with_solutions` or a
#'      provided tibble).
#'   2. Optionally selects polar inputs via greedy Wilks' Lambda
#'      ([get_greedy_vars()]).
#'   3. Reduces inputs via [cluster_reduce_vars()] (greedy_step, falling back
#'      to stepwise if greedy_step fails).
#'   4. Fits LDA twice via [cluster_add_lda()] — once with all inputs, once
#'      with the reduced set.
#'   5. Merges results into the solution family in `seg$solutions$analysis`,
#'      rebuilds the global summary table and segment append columns.
#'
#' @param seg A seg object with data and solutions loaded.
#' @param solution_family_name Character. The solution family key (e.g.
#'   `"D7to5"`). Results are stored under
#'   `seg$solutions$analysis[[solution_family_name]]`.
#' @param seed_name Character. Column name in the seed tibble (and in
#'   `seg$data$with_solutions`) containing the segment assignments
#'   (e.g. `"proto_D7to5"`).
#' @param seed Data frame or `NULL`. Two-column tibble with respondent IDs and
#'   segment assignments. If `NULL`, extracted from `seg$data$with_solutions`
#'   using `resp_id_name` and `seed_name`.
#' @param vars Character vector or `NULL`. Polar input variables for LDA. If
#'   `NULL` and `use_greedy = TRUE`, selected automatically via
#'   [get_greedy_vars()]. If `NULL` and `use_greedy = FALSE`, uses all RS
#'   polars from the spec.
#' @param vars_profiles Character vector or `NULL`. Profile variables
#'   corresponding to `vars`. If `NULL`, derived from the spec's polar table.
#' @param filter_name Character or `NULL`. Column name containing a logical
#'   filter (e.g. `"okay_filter"`). `NULL` uses all rows.
#' @param use_greedy Logical. If `TRUE` (default), select polar inputs via
#'   greedy Wilks' Lambda when `vars = NULL`.
#' @param use_top_n_polars Integer. Number of top polars to consider during
#'   greedy variable selection (default: `20`).
#' @param reduced_inputs_max Integer or character vector or `NULL`. Cap on the
#'   number of reduced input variables. If numeric, truncates the greedy
#'   selection to this many. If character, uses exactly those variables. `NULL`
#'   uses all selected variables. Default: `14`.
#' @param priors Character. LDA prior method: `"size"` (default) or `"equal"`.
#' @param resp_id_name Character or `NULL`. Respondent ID column. If `NULL`,
#'   auto-detected via [get_resp_id_name()].
#'
#' @return The seg object with updated `seg$solutions$analysis`,
#'   `seg$solutions$summary_table`, `seg$solutions$df_segment_append`, and
#'   `seg$data$with_solutions`.
#'
#' @export
seg_cluster_prototype_seed <- function(
    seg,
    solution_family_name,
    seed_name,
    seed = NULL,
    vars = NULL,
    vars_profiles = NULL,
    filter_name = NULL,
    use_greedy = TRUE,
    use_top_n_polars = 20,
    reduced_inputs_max = 14,
    priors = c("size", "equal"),
    resp_id_name = NULL
){


  # solution_family_name = "D7to5"
  # seed_name = "proto_D7to5"
  # seed = NULL
  # vars = NULL
  # vars_profiles = NULL
  # filter_name = NULL
  # reduced_inputs_max = 14
  # priors = "size"
  # resp_id_name = NULL


  priors <- match.arg(priors)


  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }


  df <- seg[["data"]][["with_solutions"]]


  if(is.null(seed)){
    seed <- df %>% dplyr::select(dplyr::all_of(c(resp_id_name, seed_name)))
  }
  seed <- seed %>% dplyr::filter(!is.na(.data[[seed_name]]))



  if(!is.null(filter_name)){
    df_temp <- df %>% dplyr::filter(.data[[filter_name]])
  }else if(is.null(filter_name)){
    df_temp <- df
  }


  if( !all(df_temp[[resp_id_name]] %in% seed[[resp_id_name]]) ){
    df_temp <- df_temp %>%
      dplyr::filter(
        df_temp[[resp_id_name]] %in% seed[[resp_id_name]]
      )
  }


  if(use_greedy && is.null(vars)){
    vars <- seg %>% get_greedy_vars(df = df_temp, top = use_top_n_polars, grp = unlist(seed[[seed_name]]))
  }


  if(is.null(vars)){
    vars <- seg %>% seg_get_vars_polars(.return = "rs")
    vars_profiles <- seg %>% seg_get_vars_polars(.return = "profiles")
  }


  if(is.null(vars_profiles)){
    vars_profiles <- seg_get_vars_polars(seg, .return = "all") %>%
      dplyr::filter(rs_var %in% vars) %>%
      dplyr::select(profile_var) %>%
      unlist() %>%
      setNames(NULL)
  }



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
    priors_size = ifelse(
      all(priors == "size"),
      purrr::map2(cluster_seed, cluster_name, ~.x[[.y]] %>% table_percent()),
      purrr::map(n, ~priors)
    ),
    reduced_inputs = purrr::map2(
      cluster_seed, cluster_name,
      function(x,y)purrr::possibly(
        function(x,y)cluster_reduce_vars(df_temp, vars, x[[y]], type = "greedy_step", return_only_var = TRUE),
        otherwise = cluster_reduce_vars(df_temp, vars, x[[y]], type = "step", return_only_var = TRUE)
      )()) %>% suppressWarnings(),
    reduced_profiles = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
  )


  if(!is.null(reduced_inputs_max)){

    if(is.numeric(reduced_inputs_max)){
      results <- results %>%
        dplyr::mutate(
          "reduced_inputs" = purrr::map(reduced_inputs, head, reduced_inputs_max),
          "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
        )

    }else if(is.character(reduced_inputs_max)){
      results <- results %>%
        dplyr::mutate(
          "reduced_inputs" = list(reduced_inputs_max),
          "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
        )
    }

  }


  result_all <- results %>%
    cluster_add_lda(
      df = df,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      priors = priors,
      use_reduced = FALSE
    )


  result_reduced <- results %>%
    cluster_add_lda(
      df = df,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      priors = priors,
      use_reduced = TRUE
    )


  if(is_truthy(seg[["solutions"]][["analysis"]][[solution_family_name]])){
    solution_family_results <- seg[["solutions"]][["analysis"]][[solution_family_name]]
  }else{
    solution_family_results <- list()
  }


  solution_family_results[["result"]][["prototype"]] <- list(
    all_inputs = result_all,
    reduced_inputs = result_reduced
  )


  solution_family_results[["solution_table"]] <- solution_family_results[["result"]] %>%
    purrr::discard_at("hierarchical_fit") %>%
    purrr::flatten() %>%
    purrr::map(~dplyr::select(.x, solution_name, n, cluster_name, lda_name, lda_inputs, lda_profiles, lda_coefficient_function, lda_predict, confusion, accuracy, df_append)) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(df_append))



  solution_family_results[["df_segment_append"]] <- solution_family_results[["solution_table"]] %>%
    dplyr::select(df_append) %>%
    unlist(recursive = FALSE) %>%
    purrr::reduce(dplyr::full_join, by = "id") %>%
    dplyr::select(-dplyr::ends_with(".y")) %>%
    setNames(., names(.) %>% gsub(".x", "", .))



  solution_family_results[["solution_table"]] <- solution_family_results[["solution_table"]] %>% dplyr::select(-df_append)



  seg[["solutions"]][["analysis"]][[solution_family_name]] <- solution_family_results



  solution_table <- seg[["solutions"]][["analysis"]] %>%
    purrr::map(purrr::pluck, "solution_table") %>%
    dplyr::bind_rows()



  df_segment_append <- seg[["solutions"]][["analysis"]] %>%
    purrr::map(purrr::pluck, "df_segment_append") %>%
    purrr::reduce(dplyr::full_join, by = "id")



  df_temp <- seg[["data"]][["with_solutions"]]
  if(is.null(df_temp) || all(is.na(df_temp))){
    df_temp <- seg[["data"]][["with_shell"]]
  }


  df_return <- dplyr::left_join(
    df_temp %>%
      dplyr::select(
        !dplyr::any_of(
          names(df_segment_append) %>%
            tail(-1)
        )
      ),
    df_segment_append,
    by = dplyr::join_by(seg_uuid == id)
  )



  if("okay_filter" %in% names(df) && !"okay_filter" %in% names(df_return)){
    df_return <- df_return %>%
      dplyr::left_join(
        df %>% dplyr::select(c(!!resp_id_name, "okay_filter")),
        by = dplyr::join_by(!!resp_id_name)
      )
  }


  seg[["solutions"]][["summary_table"]] <- solution_table
  seg[["solutions"]][["df_segment_append"]] <- df_segment_append
  seg[["data"]][["with_solutions"]] <- df_return


  return(seg)

}
