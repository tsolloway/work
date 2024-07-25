#' cluster_gaus_mix
#' @description cluster_gaus_mix
#' @export
cluster_gaus_mix <- function(
    df, vars, solution_name, id_name, filter_name = NULL,
    n_min = 4, n_max = 7,
    priors = c("equal", "size"), iter_max = 100000, nstart = 10
){

  require(mclust)

  set.seed(1)

  priors <- match.arg(priors)

  id <- df[[id_name]]


  if(!is.null(filter_name)){
    df_temp <- df %>% filter(.data[[filter_name]]) %>% select(all_of(vars))
    id_temp <- id[df[[filter_name]]]
  }else if(is.null(filter_name)){
    df_temp <- df %>% select(all_of(vars))
    id_temp <- id
  }


  result <- tibble::tibble("n" = n_min : n_max) %>%
    dplyr::mutate(
      "cluster_name" = glue("gaus_mix_cluster_{solution_name}{n}"),
      "cluster_fit" = purrr::map(n, possibly(~mclust::Mclust(df_temp, .x, verbose = FALSE), otherwise = NA)),
      "cluster_seed" = map2(cluster_fit, cluster_name, possibly(~pluck(.x, "classification") %>% bind_cols(id_temp, .) %>% set_names(c("id", .y)) %>% suppressMessages(), otherwise = NA)),
      "priors_equal" = purrr::map(n, ~rep(1/.x, .x)),
      "priors_size" = map2(cluster_seed, cluster_name, ~.x %>% pluck(.y) %>% table_percent())
    )

  detach("package:mclust", unload=TRUE)

  result <- result %>% cluster_add_lda(df = df, vars = vars, id_name = id_name, filter_name = filter_name)


  return(result)
}
