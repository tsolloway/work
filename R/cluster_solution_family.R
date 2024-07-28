#' cluster_solution_family
#' @description cluster_solution_family
#' @export
cluster_solution_family <- function(
    seg, inputs, solution_name, id_name = "seg_uuid", filter_logical_vector = NULL,
    n_min = 4, n_max = 7, reduced_inputs_max = NULL,
    vary_percent = .1, side_bias_percent = .1,
    priors = c("equal", "size"), iter_max = 100000, nstart = 10,
    do_kmeans = TRUE, do_medoid = TRUE, do_gaus_mix = TRUE, do_hierarchical = TRUE
){

  result <- list()

  df <- seg[["data"]][["with_shell"]]

  all_polars_rs <- seg[["input_sheet"]][["input_table"]][["rs_var"]] %>% as.character()
  all_inputs <- seg[["input_sheet"]][["input_table"]][["profile_var"]] %>% as.character()


  # filter data if available
  if(!is.null(filter_logical_vector)){
    if(is.vector(filter_logical_vector)){
      df <- df %>% filter(filter_logical_vector)
    }else  if(is.character(filter_logical_vector)){
      df <- df %>% filter(.data[[filter_logical_vector]])
    }
  }

  df <- df %>% seg_cluster_variability(vars = all_polars_rs, vary_percent = vary_percent, side_bias_percent = side_bias_percent)

  ok_filter_exists <- df[["ok_filter_exists"]]
  df <- df[["df"]]


  if(ok_filter_exists){
    filter_name <- "okay_filter"
  }else if(!ok_filter_exists){
    filter_name <- NULL
  }



  if(do_kmeans){
    result[["kmeans"]] <- cluster_kmeans(
      df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]], solution_name = solution_name, id_name = id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    )
  }



  if(do_medoid){
    result[["medoid"]] <- cluster_medoid(
      df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]], solution_name = solution_name, id_name = id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    )
  }



  if(do_gaus_mix){
    result[["gaus_mix"]] <- cluster_gaus_mix(
      df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]], solution_name = solution_name, id_name = id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    )
    possibly(~detach("package:mclust", unload=TRUE))()
  }



  if(do_hierarchical){
    temp <- cluster_hierarchical(
      df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]], solution_name = solution_name, id_name = id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    )

    result[["hierarchical"]] <- list(
      all_inputs = temp[["all_inputs"]],
      reduced_inputs = temp[["reduced_inputs"]]
    )

    result[["hierarchical_fit"]] <- temp[["hierarchical_fit"]]
  }



  solution_table <- result %>%
    discard_at("hierarchical_fit") %>%
    flatten() %>%
    purrr::map(~dplyr::select(.x, solution_name, n, cluster_name, lda_name, lda_inputs, lda_profiles, confusion, accuracy, df_append)) %>%
    bind_rows()



  df_segment_append <- solution_table %>%
    dplyr::select(df_append) %>%
    unlist(recursive = FALSE) %>%
    reduce(full_join, by = "id") %>%
    dplyr::select(-ends_with(".y")) %>%
    setNames(., names(.) %>% gsub(".x", "", .))



  solution_table <- solution_table %>% dplyr::select(-df_append)


  output <- list(
    result = result,
    solution_table = solution_table,
    df_segment_append = df_segment_append
  )


  return(output)
}
