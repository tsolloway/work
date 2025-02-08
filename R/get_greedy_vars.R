#' get_greedy_vars
#' @description get_greedy_vars
#' @export
get_greedy_vars <- function(
    seg,
    top = 30,
    n = seq(4,8),
    df = NULL,
    vars = NULL,
    filter_name = NULL,
    iter_max = 10000,
    nstart = 10
){

  set.seed(1)

  if(is.null(df)){
    df <- seg[["data"]][["with_shell"]]
  }

  if(!is.null(filter_name)){
    df <- df %>% dplyr::filter(.data[[filter_name]])
  }

  if(is.null(vars)){
    vars <- seg_get_vars_polars(seg, .return="rs")
  }

  df <- df %>%
    dplyr::select(
      dplyr::all_of(
        vars
      )
    )


  result <- tibble::tibble(
    n = n,

    grp = map(
      n,
      ~stats::kmeans(df_temp, .x, iter.max = iter_max, nstart = nstart) %>%
        pluck("cluster")
    ),

    greedy = map(
      grp,
      ~cluster_reduce_vars(df_temp, reduced_vars, .x, type = "greedy", return_only_var = FALSE)
    )
  )

  result %>%
    dplyr::select(greedy) %>%
    tidyr::unnest(greedy) %>%
    dplyr::group_by(vars) %>%
    dplyr::summarise(
      mean = mean(Wilks.lambda)
    ) %>%
    dplyr::arrange(-mean) %>%
    head(top) %>%
    dplyr::select(vars) %>%
    unlist() %>%
    setNames(NULL)

}

