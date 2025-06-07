#' cluster_kmeans
#' @description cluster_kmeans
#' @export
cluster_kmeans <- function(
    df,
    vars,
    vars_profiles,
    solution_name,
    resp_id_name = "seg_uuid",
    filter_name = NULL,
    n_min = 4,
    n_max = 7,
    reduced_inputs_max = NULL,
    priors = c("equal", "size"),
    iter_max = 100000,
    nstart = 10,
    lda_vars = NULL,
    lda_vars_profiles = NULL
){

    # df = seg[["data"]][["with_solutions"]]
    # vars = c(
    #   seg_get_vars(seg, type = "polars", .return = "rs")#,
    #   # seg_get_vars(seg, block = "CVP", type = "profiles")
    # )
    # vars_profiles = c(
    #   seg_get_vars(seg, type = "polars", .return = "profiles")#,
    #   # seg_get_vars(seg, block = "CVP", type = "profiles")
    # )
    # solution_name = "foo"
    # resp_id_name = "seg_uuid"
    # filter_name = NULL
    # n_min = 4
    # n_max = 7
    # reduced_inputs_max = NULL
    # priors = "equal"
    # iter_max = 100000
    # nstart = 10
    # lda_vars = NULL


  set.seed(1)

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
      use_reduced = FALSE,
      lda_vars = reduced_vars,
      lda_vars_profiles = reduced_vars_profiles
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

