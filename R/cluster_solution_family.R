#' cluster_solution_family
#' @description cluster_solution_family
#' @export
cluster_solution_family <- function(
    seg, inputs, solution_name, id_name = "seg_uuid", filter_logical_vector = NULL,
    n_min = 4, n_max = 7,
    vary_percent = .1, side_bias_percent = .1,
    priors = c("equal", "size"), iter_max = 100000, nstart = 10,
    do_kmeans = TRUE, do_medoid = TRUE, do_gaus_mix = TRUE, do_hierarchical = TRUE
){

  result <- list()

  df <- seg[["data"]][["with_shell"]]

  polars_rs <- seg[["input_sheet"]][["input_table"]][["rs_var"]] %>% as.character()


  # filter data if availabe
  if(!is.null(filter_logical_vector)){
    if(is.vector(filter_logical_vector)){
      df <- df %>% filter(filter_logical_vector)
    }else  if(is.character(filter_logical_vector)){
      df <- df %>% filter(.data[[filter_logical_vector]])
    }
  }

  df <- df %>% seg_cluster_variability(vars = polars_rs, vary_percent = vary_percent, side_bias_percent = side_bias_percent)

  ok_filter_exists <- df[["ok_filter_exists"]]
  df <- df[["df"]]


  if(ok_filter_exists){
    filter_name <- "okay_filter"
  }else if(!ok_filter_exists){
    filter_name <- NULL
  }



  if(do_kmeans){
    result[["kmeans"]] <- cluster_kmeans(
      df = df, vars = inputs[["RS"]], solution_name = solution_name, id_name = id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    )
  }



  if(do_medoid){
    result[["medoid"]] <- cluster_medoid(
      df = df, vars = inputs[["RS"]], solution_name = solution_name, id_name = id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    )
  }



  if(do_gaus_mix){
    result[["gaus_mix"]] <- cluster_gaus_mix(
      df = df, vars = inputs[["RS"]], solution_name = solution_name, id_name = id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    )
  }



  if(do_hierarchical){
    temp <- cluster_hierarchical(
      df = df, vars = inputs[["RS"]], solution_name = solution_name, id_name = id_name, filter_name = filter_name,
      n_min = n_min, n_max = n_max,
      priors = priors, iter_max = iter_max, nstart = nstart
    )

    result[["hierarchical"]] <- temp[["result"]]
    result[["hierarchical_fit"]] <- temp[["hierarchical_fit"]]
  }


  return(result)
}
