#' cluster_kmeans
#' @description cluster_kmeans
#' @export
cluster_kmeans <- function(
    df,
    vars,
    vars_profiles,
    solution_name,
    resp_id_name = NULL,
    filter_name = NULL,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = NULL,
    priors = c("equal", "size"),
    iter_max = 100000,
    nstart = 10
){

  # df = seg[["data"]][["with_solutions"]]
  # vars = c(
  #   seg_get_vars(seg, type = "polars", .return = "rs"),
  #   seg_get_vars(seg, block = "CVP", type = "profiles")
  # )
  # vars_profiles = c(
  #   seg_get_vars(seg, type = "polars", .return = "profiles"),
  #   seg_get_vars(seg, block = "CVP", type = "profiles")
  # )
  # solution_name = "foo"
  # resp_id_name = NULL
  # filter_name = NULL
  # n_min = 4
  # n_max = 7
  # reduced_inputs_max = NULL
  # priors = "equal"
  # iter_max = 100000
  # nstart = 10


  set.seed(1)

  priors <- match.arg(priors)

  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }

  id <- df %>%
    dplyr::select(!!resp_id_name) %>%
    unlist() %>%
    setNames(NULL)


  if(!is.null(filter_name)){
    df_temp <- df %>% filter(.data[[filter_name]]) %>% dplyr::select(all_of(vars))
    id_temp <- id[df[[filter_name]]]
  }else if(is.null(filter_name)){
    df_temp <- df %>% dplyr::select(all_of(vars))
    id_temp <- id
  }


  reduced_vars <- vars[!vars %in% seg_get_vars_profiles(seg)]
  reduced_vars_profiles <- vars_profiles[!vars_profiles %in% seg_get_vars_profiles(seg)]


  result <- tibble::tibble("n" = n_min : n_max) %>%
    mutate(
      "solution_name" = solution_name,
      "cluster_name" = glue("kmeans_cluster_{solution_name}{n}"),
      "inputs" = list(vars),
      "profiles" = list(vars_profiles),
      "cluster_fit" = purrr::map(n, possibly(~{
        set.seed(1)
        stats::kmeans(df_temp, .x, iter.max = iter_max, nstart = nstart)
      }, otherwise = NA)),
      "cluster_seed" = purrr::map2(cluster_fit, cluster_name, possibly(~{
        pluck(.x, "cluster") %>% bind_cols(id_temp, .) %>% set_names(c("id", .y)) %>% suppressMessages()
      }, otherwise = NA)),
      "cluster_glance" = purrr::map(cluster_fit, possibly(broom::glance, otherwise = NA)),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = purrr::map2(cluster_seed, cluster_name, ~.x[[.y]] %>% table_percent()),
      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, possibly(~{
        set.seed(1)
        cluster_reduce_vars(df_temp, reduced_vars, .x[[.y]], type = "greedy_step", return_only_var = TRUE)
      }, otherwise = NA)),
      "reduced_profiles" = purrr::map(reduced_inputs, ~reduced_vars_profiles[match(.x, reduced_vars)])
    )



  if(!is.null(reduced_inputs_max)){
    result <- result %>%
      mutate(
        "reduced_inputs" = purrr::map(reduced_inputs, head, reduced_inputs_max),
        "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
      )
  }



  set.seed(1)
  result_all <- result %>%
    cluster_add_lda(
      df = df,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      priors = priors,
      use_reduced = FALSE
    )

  set.seed(1)
  result_reduced <- result %>%
    cluster_add_lda(
      df = df,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      priors = priors,
      use_reduced = TRUE
    )


  output <- list(
    all_inputs = result_all,
    reduced_inputs = result_reduced
  )



  return(output)
}

