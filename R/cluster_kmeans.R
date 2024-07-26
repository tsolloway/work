#' cluster_kmeans
#' @description cluster_kmeans
#' @export
cluster_kmeans <- function(
    df, vars, solution_name, id_name, filter_name = NULL,
    n_min = 4, n_max = 7,
    priors = c("equal", "size"), iter_max = 100000, nstart = 10
){

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



  tibble::tibble("n" = n_min : n_max) %>%
    dplyr::mutate(
      "solution_name" = solution_name,
      "cluster_name" = glue("kmeans_cluster_{solution_name}{n}"),
      "cluster_fit" = map(n, possibly(~stats::kmeans(df_temp, .x, iter.max = iter_max, nstart = nstart), otherwise = NA)),
      "cluster_seed" = map2(cluster_fit, cluster_name, possibly(~pluck(.x, "cluster") %>% bind_cols(id_temp, .) %>% set_names(c("id", .y)) %>% suppressMessages(), otherwise = NA)),
      "cluster_tidy" = map(cluster_fit, possibly(broom::tidy, otherwise = NA)),
      "cluster_glance" = map(cluster_fit, possibly(broom::glance, otherwise = NA)),
      "priors_equal" = map(n, ~rep(1/.x, .x)),
      "priors_size" = map2(cluster_seed, cluster_name, ~.x %>% pluck(.y) %>% table_percent())
    ) %>%
    cluster_add_lda(df = df, vars = vars, id_name = id_name, filter_name = filter_name)

}

