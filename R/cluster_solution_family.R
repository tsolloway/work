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
    priors = c("both", "equal", "size"),
    iter_max = 100000,
    nstart = 10,
    ok_filter = TRUE,
    do_kmeans = TRUE,
    do_medoid = TRUE,
    do_hierarchical = TRUE,
    do_gaus_mix = FALSE,
    polar_type = c("both", "rs", "source"),
    seed = 1
){

  result <- list()
  polar_type <- match.arg(polar_type)


  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
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
          vars = seg_get_vars_polars(seg, .return="sources"),
          vary_percent = .1,
          side_bias_percent = .1
        )

      df <- df[["df"]]
    }

  }else{
    filter_name <- NULL
  }



  if(do_kmeans){

    if(priors != "equal"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["kmeans_rs_size"]] <- cluster_kmeans(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "kmeans_rs_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["kmeans_src_size"]] <- cluster_kmeans(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "kmeans_src_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

    if(priors != "size"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["kmeans_rs_eq"]] <- cluster_kmeans(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "kmeans_rs_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["kmeans_src_eq"]] <- cluster_kmeans(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "kmeans_src_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

  }



  if(do_medoid){

    if(priors != "equal"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["medoid_rs_size"]] <- cluster_medoid(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "medoid_rs_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["medoid_src_size"]] <- cluster_medoid(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "medoid_src_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

    if(priors != "size"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["medoid_rs_eq"]] <- cluster_medoid(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "medoid_rs_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["medoid_src_eq"]] <- cluster_medoid(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "medoid_src_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

  }



  if(do_gaus_mix){

    if(priors != "equal"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["gaus_mix_rs_size"]] <- cluster_gaus_mix(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "gaus_mix_rs_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["gaus_mix_src_size"]] <- cluster_gaus_mix(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "gaus_mix_src_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

    if(priors != "size"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        result[["gaus_mix_rs_eq"]] <- cluster_gaus_mix(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "gaus_mix_rs_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        result[["gaus_mix_src_eq"]] <- cluster_gaus_mix(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "gaus_mix_src_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
      }
    }

    possibly(~detach("package:mclust", unload=TRUE))()

  }



  if(do_hierarchical){

    if(priors != "equal"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        temp_rssz <- cluster_hierarchical(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "hierarchical_rs_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
        result[["hierarchical_rs_size"]] <- list(all_inputs = temp_rssz[["all_inputs"]], reduced_inputs = temp_rssz[["reduced_inputs"]])
        result[["hierarchical_rs_size_fit"]] <- temp_rssz[["hierarchical_fit"]]
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        temp_srcsz <- cluster_hierarchical(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "hierarchical_src_sz",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "size", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
        result[["hierarchical_src_size"]] <- list(all_inputs = temp_srcsz[["all_inputs"]], reduced_inputs = temp_srcsz[["reduced_inputs"]])
        result[["hierarchical_src_size_fit"]] <- temp_srcsz[["hierarchical_fit"]]
      }
    }

    if(priors != "size"){
      if(polar_type != "source"){
        if(!is.null(seed)) set.seed(seed)
        temp_rseq <- cluster_hierarchical(
          df = df, vars = inputs[["RS"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "hierarchical_rs_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
        result[["hierarchical_rs_eq"]] <- list(all_inputs = temp_rseq[["all_inputs"]], reduced_inputs = temp_rseq[["reduced_inputs"]])
        result[["hierarchical_rs_eq_fit"]] <- temp_rseq[["hierarchical_fit"]]
      }
      if(polar_type != "rs"){
        if(!is.null(seed)) set.seed(seed)
        temp_srceq <- cluster_hierarchical(
          df = df, vars = inputs[["Source"]], vars_profiles = inputs[["Profile"]],
          solution_name = solution_name, solution_name_prefix = "hierarchical_src_eq",
          resp_id_name = resp_id_name, filter_name = filter_name,
          n_min = n_min, n_max = n_max, reduced_inputs_max = reduced_inputs_max,
          priors = "equal", iter_max = iter_max, nstart = nstart, seed = seed
        ) %>% suppressWarnings()
        result[["hierarchical_src_eq"]] <- list(all_inputs = temp_srceq[["all_inputs"]], reduced_inputs = temp_srceq[["reduced_inputs"]])
        result[["hierarchical_src_eq_fit"]] <- temp_srceq[["hierarchical_fit"]]
      }
    }

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
