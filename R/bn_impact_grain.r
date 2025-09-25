#' bn_impact_grain
#' @description bn_impact_grain
#' @export
bn_impact_grain <- function(
    bn,
    df,
    ivs = NULL,
    n_boot = 1,
    return_dv_estimate = FALSE,
    use_parallel = FALSE,
    do_community = FALSE
){


  if("meta" %in% names(bn) && is.null(ivs)){
    if(bn[["meta"]] == "bn_model_single"){

      ivs <- bn[["arcs"]][["ivs"]] %>%
        select(from, to) %>%
        unlist() %>%
        unique() %>%
        sort()

      ivs <- ivs[order(ivs)]

      if(do_community){
        ivs <- bn[["viz_prep"]][["nodes"]] %>%
          group_by(community_name) %>%
          summarise(ids = list(id), .groups = "drop") %>%
          {setNames(.[["ids"]], .[["community_name"]])}

        ivs <- ivs[order(names(ivs))]
      }


    }
  }





  if(!use_parallel){
    results <- map(
      ivs,
      ~bn_impact_grain_engine(
        bn = bn,
        df = df,
        iv = .x,
        n_boot = n_boot,
        return_dv_estimate = return_dv_estimate
      )
    )
  }else if(use_parallel){

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
          bn_impact_grain_engine(
            bn = bn,
            df = df,
            iv = .x,
            n_boot = n_boot,
            return_dv_estimate = return_dv_estimate
          )
        }
      ),
      options = furrr_options(
        packages = c("dplyr", "work", "bnlearn", "parallel", "glue", "withr", "gRbase", "gRain")
      )
    )
  }



  results <- results %>%
    bind_rows() %>%
    mutate(
      estimate_abs = estimate %>% abs(),
      index = (estimate_abs / mean(estimate_abs)) * 100
    )



  if(do_community){

    ivs <- ivs %>%
      map(paste0, collapse = "+") %>%
      {tibble(
        iv = unlist(.),
        label = names(.)
      )}


    results <- results %>%
      left_join(
        ivs,
        by = join_by(iv)
      ) %>%
      mutate(
        iv = label
      ) %>%
      select(-label)

  }


  return(results)
}
