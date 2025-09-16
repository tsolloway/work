#' seg_cluster_with_profiles
#' @description seg_cluster_with_profiles
#' @export
seg_cluster_with_profiles <- function(
    seg,
    solution_previous = NULL,
    solution_name = NULL,
    inputs_polars = NULL,
    inputs_profiles = NULL,
    include_polars_lda = TRUE,
    include_profiles_lda = TRUE,
    resp_id_name = NULL,
    filter_logical_vector = NULL,
    ok_filter = TRUE,
    use_greedy = TRUE,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = 16,
    vary_percent = .1,
    side_bias_percent = .1,
    priors = c("size", "equal"),
    iter_max = 100000,
    nstart = 10
){

  # solution_previous = NULL
  # solution_name = "H"
  # inputs_polars = NULL
  # # inputs_profiles = c("BT14", "BT15", "BT16", seg_get_vars_profiles(seg, c("TER", "USE")))
  # resp_id_name = NULL
  # filter_logical_vector = NULL
  # ok_filter = TRUE
  # use_greedy = TRUE
  # n_min = 4
  # n_max = 7
  # reduced_inputs_max = 16
  # vary_percent = .1
  # side_bias_percent = .1
  # priors = "size"
  # iter_max = 100000
  # nstart = 10
  # include_polars_lda = T
  # include_profiles_lda = T


  priors <- match.arg(priors)

  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }


  if(!is.null(solution_previous)){
    if(!solution_previous %in% names(seg[["solutions"]][["inputs"]])){
      stop("solution_previous not in seg object")
    }
    inputs_polars <- seg[["solutions"]][["inputs"]][[solution_previous]][["RS"]]
  }


  df <- seg[["data"]][["with_shell"]]


  # filter data if available
  if(!is.null(filter_logical_vector)){

    if(is.vector(filter_logical_vector)){

      df <- df %>% filter(filter_logical_vector)

    }else  if(is.character(filter_logical_vector)){

      df <- df %>% filter(.data[[filter_logical_vector]])
    }
  }


  if(ok_filter){
    filter_name <- "okay_filter"

    if(!filter_name %in% names(df)){

      df <- df %>%
        seg_cluster_variability(
          vars = seg_get_vars_polars(seg, .return="rs"),
          vary_percent = vary_percent,
          side_bias_percent = side_bias_percent
        )

      df <- df[["df"]]
    }

  }else{
    filter_name <- NULL
  }


  if(use_greedy && is.null(solution_previous) && is.null(inputs_polars)){
    inputs_polars <- seg %>% get_greedy_vars(df=df, filter_name = filter_name)
  }


  if(is.null(inputs_polars) || isFALSE(inputs_polars) || isTRUE(inputs_polars)){
    all_inputs_polars <- seg %>% seg_get_vars_polars(.return = "rs")
  }else{
    all_inputs_polars <- inputs_polars
  }




  all_inputs_polars_profiles <- seg_get_vars_polars(seg, .return = "all") %>%
    dplyr::filter(
      rs_var %in% all_inputs_polars | source_var %in% all_inputs_polars
    ) %>%
    dplyr::select(profile_var) %>%
    unlist() %>%
    setNames(NULL)




  if(isFALSE(inputs_polars)){
    cluster_vars <- inputs_profiles
    cluster_profiles <- inputs_profiles
  }else{
    cluster_vars <- c(all_inputs_polars, inputs_profiles)
    cluster_profiles <- c(all_inputs_polars_profiles, inputs_profiles)
  }



  if(include_polars_lda && include_profiles_lda){

    lda_vars <- c(all_inputs_polars, inputs_profiles)
    lda_profiles <- c(all_inputs_polars_profiles, inputs_profiles)

  }else if(!include_polars_lda && include_profiles_lda){

    lda_vars <- inputs_profiles
    lda_profiles <- inputs_profiles

  }else if(include_polars_lda && !include_profiles_lda){

    lda_vars <- all_inputs_polars
    lda_profiles <- all_inputs_polars_profiles

  }else if(!include_polars_lda && !include_profiles_lda){

    stop("Both include_polars_lda and include_profiles_lda cannot be false")

  }


  results <- cluster_kmeans(
    df = df,
    vars = cluster_vars,
    vars_profiles = cluster_profiles,
    solution_name = solution_name,
    lda_vars = lda_vars,
    lda_vars_profiles = lda_profiles,
    reduced_inputs_max = reduced_inputs_max,
    resp_id_name = resp_id_name,
    filter_name = filter_name,
    n_min = n_min,
    n_max = n_max,
    priors = priors,
    iter_max = iter_max,
    nstart = nstart
  ) %>%
    suppressWarnings()


  if(is_truthy(seg[["solutions"]][["analysis"]][[solution_name]])){
    solution_family_results <- seg[["solutions"]][["analysis"]][[solution_name]]
  }else{
    solution_family_results <- list()
  }

  solution_family_results[["result"]][["kmeans"]] <- results


  solution_family_results[["solution_table"]] <- solution_family_results[["result"]] %>%
    discard_at("hierarchical_fit") %>%
    flatten() %>%
    purrr::map(~dplyr::select(.x, solution_name, n, cluster_name, lda_name, lda_inputs, lda_profiles, confusion, accuracy, df_append)) %>%
    bind_rows() %>%
    filter(!is.na(accuracy))


  solution_family_results[["df_segment_append"]] <- solution_family_results[["solution_table"]] %>%
    dplyr::select(df_append) %>%
    unlist(recursive = FALSE) %>%
    reduce(full_join, by = "id") %>%
    dplyr::select(-ends_with(".y")) %>%
    setNames(., names(.) %>% gsub(".x", "", .))


  seg[["solutions"]][["analysis"]][[solution_name]] <- solution_family_results


  solution_table <- seg[["solutions"]][["analysis"]] %>%
    map(pluck, "solution_table") %>%
    bind_rows()


  df_segment_append <- seg[["solutions"]][["analysis"]] %>%
    map(pluck, "df_segment_append") %>%
    reduce(full_join, by = "id")


  df_return <- left_join(
    seg[["data"]][["with_shell"]],
    df_segment_append,
    by = join_by(!!resp_id_name == id)
  )


  if("okay_filter" %in% names(df) && !"okay_filter" %in% names(df_return)){
    df_return <- df_return %>%
      left_join(
        df %>% select(c(!!resp_id_name, "okay_filter")),
        by = join_by(!!resp_id_name)
      )
  }


  seg[["solutions"]][["summary_table"]] <- solution_table
  seg[["solutions"]][["df_segment_append"]] <- df_segment_append
  seg[["data"]][["with_solutions"]] <- df_return


  return(seg)

}
