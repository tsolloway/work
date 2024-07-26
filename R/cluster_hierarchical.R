#' cluster_hierarchical
#' @description cluster_hierarchical
#' @export
cluster_hierarchical <- function(
    df, vars, vars_profiles, solution_name, id_name, filter_name = NULL,
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


  hierarchical_fit <- stats::hclust(stats::dist(df_temp))


  result <- tibble::tibble("n" = n_min : n_max) %>%
    dplyr::mutate(
      "solution_name" = solution_name,
      "cluster_name" = glue("hierarchical_cluster_{solution_name}{n}"),
      "inputs" = list(vars),
      "profiles" = list(vars_profiles),
      "cluster_seed" = map2(
        n, cluster_name,
        function(x,y)possibly(
          ~stats::cutree(hierarchical_fit, k = x) %>%
            bind_cols(id_temp, .) %>% set_names(c("id", y)) %>% suppressMessages()
          , otherwise = NA)()),
      "priors_equal" = map(n, ~rep(1/.x, .x)),
      "priors_size" = map2(cluster_seed, cluster_name, ~.x %>% pluck(.y) %>% table_percent())
    ) %>%
    cluster_add_lda(df = df, vars = vars, id_name = id_name, filter_name = filter_name)


  result <- list(
    result = result,
    hierarchical_fit = hierarchical_fit
  )


  return(result)
}
