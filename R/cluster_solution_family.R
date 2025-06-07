#' cluster_solution_family
#' @description cluster_solution_family
#' @export
cluster_solution_family <- function(
    seg,
    inputs,
    solution_name,
    resp_id_name = NULL,
    filter_logical_vector = NULL,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = NULL,
    vary_percent = .1,
    side_bias_percent = .1,
    priors = c("equal", "size"),
    iter_max = 100000,
    nstart = 10,
    ok_filter = TRUE,
    do_kmeans = TRUE,
    do_medoid = TRUE,
    do_hierarchical = TRUE,
    do_gaus_mix = FALSE
){

  result <- list()

  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }

  df <- seg[["data"]][["with_shell"]]

  all_polars_rs <- seg %>% seg_get_vars_polars(.return = "rs")
  # all_inputs <- seg[["input_sheet"]][["input_table"]][["profile_var"]] %>% as.character()  # I don't think we need.  Commenting out incase we do.


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
          vary_percent = .1,
          side_bias_percent = .1
        )

      df <- df[["df"]]
    }

  }else{
    filter_name <- NULL
  }


  # if(ok_filter_exists){
  #   filter_name <- "okay_filter"
  # }else if(!ok_filter_exists){
  #   filter_name <- NULL
  # }



  if(do_kmeans){
    set.seed(1)
    result[["kmeans"]] <- cluster_kmeans(
      df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
      solution_name = solution_name, resp_id_name = resp_id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    ) %>%
      suppressWarnings()
  }



  if(do_medoid){
    set.seed(1)
    result[["medoid"]] <- cluster_medoid(
      df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
      solution_name = solution_name, resp_id_name = resp_id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    ) %>%
      suppressWarnings()
  }



  if(do_gaus_mix){
    set.seed(1)
    result[["gaus_mix"]] <- cluster_gaus_mix(
      df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
      solution_name = solution_name, resp_id_name = resp_id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    ) %>%
      suppressWarnings()
    possibly(~detach("package:mclust", unload=TRUE))()
  }



  if(do_hierarchical){
    set.seed(1)
    temp <- cluster_hierarchical(
      df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
      solution_name = solution_name, resp_id_name = resp_id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    ) %>%
      suppressWarnings()

    result[["hierarchical"]] <- list(
      all_inputs = temp[["all_inputs"]],
      reduced_inputs = temp[["reduced_inputs"]]
    )

    result[["hierarchical_fit"]] <- temp[["hierarchical_fit"]]
  }




  solution_table <- result %>%
    discard_at("hierarchical_fit") %>%
    flatten() %>%
    purrr::map(
      ~dplyr::select(
        .x, solution_name, n, cluster_name,
        lda_name, lda_inputs, lda_profiles,
        lda_coefficient_function, lda_predict,
        confusion, accuracy, df_append
      )
    ) %>%
    bind_rows() %>%
    filter(!is.na(df_append))



  df_segment_append <- solution_table %>%
    dplyr::select(df_append) %>%
    unlist(recursive = FALSE) %>%
    reduce(full_join, by = "id") %>%
    dplyr::select(-ends_with(".y")) %>%
    setNames(
      .,
      names(.) %>% gsub(".x", "", .)
    )



  solution_table <- solution_table %>% dplyr::select(-df_append)



  output <- list(
    result = result,
    solution_table = solution_table,
    df_segment_append = df_segment_append
  )


  return(output)
}
