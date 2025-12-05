#' cluster_medoid
#' @description cluster_medoid
#' @export
cluster_medoid <- function(
    df,
    vars,
    vars_profiles,
    solution_name,
    solution_name_prefix = "medoid",
    resp_id_name = NULL,
    filter_name = NULL,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = NULL,
    priors = c("equal", "size"),
    iter_max = 100000,
    nstart = 10,
    lda_vars = NULL,
    lda_vars_profiles = NULL,
    seed = 1
){

  if(!is.null(seed)) set.seed(seed)

  priors <- match.arg(priors)


  if(!is.null(filter_name)){
    df_temp <- df %>%
      dplyr::filter(.data[[filter_name]])
  }else{
    df_temp <- df
  }


  df_temp <- df_temp %>%
    dplyr::select(all_of(c(resp_id_name, vars))) %>%
    na.exclude()


  id <- df[[resp_id_name]]
  id_temp <- df_temp[[resp_id_name]]


  df_temp <- df_temp %>% dplyr::select(-all_of(resp_id_name))


  if(!is.null(lda_vars)){
    reduced_vars <- vars[vars %in% lda_vars]
    reduced_vars_profiles <- vars_profiles[vars_profiles %in% lda_vars_profiles]
  }else{
    reduced_vars <- vars
    reduced_vars_profiles <- vars_profiles
  }


  result <- tibble::tibble("n" = n_min : n_max) %>%
    mutate(
      "solution_name" = solution_name,
      "cluster_name" = glue("{solution_name_prefix}_{solution_name}{n}"),
      "inputs" = list(vars),
      "profiles" = list(vars_profiles),
      "cluster_fit" = purrr::map(n, possibly(~{
        if(!is.null(seed)) set.seed(seed)
        cluster::pam(df_temp, .x)
      }, otherwise = NA)),
      "cluster_seed" = purrr::map2(cluster_fit, cluster_name, possibly(~{
        pluck(.x, "clustering") %>%
          bind_cols(id_temp, .) %>%
          set_names(c("id", .y)) %>%
          suppressMessages()
      }, otherwise = NA)),
      "cluster_glance" = purrr::map(cluster_fit, possibly(broom::glance, otherwise = NA)),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = purrr::map2(cluster_seed, cluster_name, ~.x[[.y]] %>% table_percent()),
      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, possibly(~{
        if(!is.null(seed)) set.seed(seed)
        cluster_reduce_vars(df_temp, reduced_vars, .x[[.y]], type = "greedy_step", return_only_var = TRUE, seed = seed)
      }, otherwise = NA)),
      "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
    )


  if(!is.null(reduced_inputs_max)){
    result <- result %>%
      mutate(
        "reduced_inputs" = purrr::map(reduced_inputs, head, reduced_inputs_max),
        "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
      )
  }


  if(!is.null(seed)) set.seed(seed)
  result_all <- result %>%
    cluster_add_lda(
      df = df,
      resp_id_name = resp_id_name,
      filter_name = filter_name,
      priors = priors,
      use_reduced = FALSE,
      lda_vars = reduced_vars,
      lda_vars_profiles = reduced_vars_profiles
    )

  if(!is.null(seed)) set.seed(seed)
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



