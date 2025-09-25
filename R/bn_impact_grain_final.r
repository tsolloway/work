#' bn_impact_grain_final
#' @description bn_impact_grain_final
#' @export
bn_impact_grain_final <- function(
    bn_final,
    df,
    n_boot = 1,
    use_parallel = FALSE,
    do_community = FALSE
){

  library(work)
  work::start()

  bns <- bn_final[["bn_subgroups"]]


  if(!use_parallel){

    result <- imap(
      bns,
      function(x, y){
        print(y)
        bn_impact_grain(
          bn = x,
          df = df %>% filter(.data[[y]] == 1),
          n_boot = n_boot, use_parallel = FALSE, do_community = do_community
        ) %>%
          select(-dv) %>%
          setNames(., glue("{y}_{names(.)}"))
      }
    )

  }else if(use_parallel){

    library(future)
    library(furrr)

    plan(multisession)
    result <- future_imap(
      bns,
      function(x, y){
        bn_impact_grain(
          bn = x,
          df = df %>% filter(.data[[y]] == 1),
          n_boot = n_boot, use_parallel = FALSE, do_community = do_community
        ) %>%
          select(-dv) %>%
          setNames(., glue("{y}_{names(.)}"))
      }
    )
  }



  result <- result %>%
    map(
      ~{
        names(.)[1] <- "join_col"
        .
      }
    ) %>%
    reduce(
      full_join,
      by = join_by("join_col")
    ) %>%
    rename(Variable = join_col) %>%
    setNames(., names(.) %>% gsub("_index", "", .)) %>%
    select(-ends_with("_iv"))



  if(do_community){
    result <- result %>%
      rename(Community = Variable)
  }



  return(result)

}



