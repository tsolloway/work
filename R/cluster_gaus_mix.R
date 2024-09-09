#' cluster_gaus_mix
#' @description cluster_gaus_mix
#' @export
cluster_gaus_mix <- function(
    df, vars, vars_profiles, solution_name,
    id_name = "seg_uuid", filter_name = NULL,
    n_min = 4, n_max = 7, reduced_inputs_max = NULL,
    priors = c("equal", "size"), iter_max = 100000, nstart = 10
){

  require(mclust) %>% suppressMessages()

  set.seed(1)

  priors <- match.arg(priors)

  id <- df[[id_name]]


  if(!is.null(filter_name)){
    df_temp <- df %>% filter(.data[[filter_name]]) %>% dplyr::select(all_of(vars))
    id_temp <- id[df[[filter_name]]]
  }else if(is.null(filter_name)){
    df_temp <- df %>% dplyr::select(all_of(vars))
    id_temp <- id
  }


  result <- tibble::tibble("n" = n_min : n_max) %>%
    mutate(
      "solution_name" = solution_name,
      "cluster_name" = glue("gaus_mix_cluster_{solution_name}{n}"),
      "inputs" = list(vars),
      "profiles" = list(vars_profiles),
      "cluster_fit" = purrr::map(n, possibly(~{
        set.seed(1)
        mclust::Mclust(df_temp, .x, verbose = FALSE)
      }, otherwise = NA)),
      "cluster_seed" = purrr::map2(cluster_fit, cluster_name, possibly(~{
        pluck(.x, "classification") %>%
          bind_cols(id_temp, .) %>%
          set_names(c("id", .y)) %>%
          suppressMessages()
      }, otherwise = NA)),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = purrr::map2(cluster_seed, cluster_name, ~.x[[.y]] %>% table_percent()),
      "reduced_inputs" = purrr::map2(cluster_seed, cluster_name, possibly(~{
        set.seed(1)
        cluster_reduce_vars(df_temp, vars, .x[[.y]], type = "greedy_step", return_only_var = TRUE) %>%
          suppressWarnings()
      }, otherwise = NA)),
      "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
    )



  if(!is.null(reduced_inputs_max)){
    result <- result %>%
      mutate(
        "reduced_inputs" = purrr::map(reduced_inputs, ~.x %>% head(reduced_inputs_max)),
        "reduced_profiles" = purrr::map(reduced_inputs, ~vars_profiles[match(.x, vars)])
      )
  }


  possibly(~detach("package:mclust", unload=TRUE))()


  set.seed(1)
  result_all <- result %>%
    cluster_add_lda(
      df = df, id_name = id_name,
      filter_name = filter_name, priors = priors,
      use_reduced = FALSE
    )

  set.seed(1)
  result_reduced <- result %>%
    cluster_add_lda(
      df = df, id_name = id_name,
      filter_name = filter_name, priors = priors,
      use_reduced = TRUE
    )


  output <- list(
    all_inputs = result_all,
    reduced_inputs = result_reduced
  )


  return(output)
}
