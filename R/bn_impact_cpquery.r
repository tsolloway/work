#' bn_impact_cpquery
#' @description bn_impact_cpquery
#' @export
bn_impact_cpquery <- function(
    bn, df,
    n_boot = 2000,
    n_querry = 10000,
    only_monte_carlo = FALSE,
    use_parallel = FALSE
){


  if("meta" %in% names(bn)){
    if(bn[["meta"]] == "bn_model_single"){
      ivs <- bn[["arcs"]][["ivs"]] %>% select(from, to) %>% unlist() %>% unique() %>% sort()
    }
  }


  if(only_monte_carlo || !use_parallel){
    results <-map(
      ivs,
      ~bn_impact_cpquery_engine(
        bn = bn,
        df = df,
        iv = .x,
        n_boot = n_boot,
        n_querry =  n_querry,
        only_monte_carlo = only_monte_carlo
      )
    )
  }else if(!only_monte_carlo && use_parallel){

    library(work)
    library(future)
    library(furrr)
    library(withr)
    work::start()

    plan(multisession)

    results <- future_imap(
      ivs,
      ~with_seed(
        1,
        {
          bn_impact_cpquery_engine(
            bn = bn,
            df = df,
            iv = .x,
            n_boot = n_boot,
            n_querry =  n_querry,
            only_monte_carlo = only_monte_carlo
          )
        }
      ),
      options = furrr_options(
        packages = c("dplyr", "work", "bnlearn", "parallel", "glue", "withr")
      )
    )
  }


  results <- results %>%
    bind_rows() %>%
    mutate(
      estimate_abs = estimate %>% abs(),
      index = (estimate_abs / mean(estimate_abs)) * 100
    )


  return(results)
}
