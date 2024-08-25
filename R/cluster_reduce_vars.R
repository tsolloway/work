#' cluster_reduce_vars
#' @description cluster_reduce_vars
#' @export
cluster_reduce_vars <- function(df, vars, grp, type = c("greedy_step", "greedy", "step"), return_only_var = FALSE){

  type <- match.arg(type)

  set.seed(1)


  if(is.character(grp)){
    grp <- df %>% dplyr::select(all_of(grp))
  }


  df <- df %>% dplyr::select(all_of(vars))


  if(type == "greedy_step" || type == "greedy"){

    set.seed(1)

    greedy <- klaR::greedy.wilks(
      df[!is.na(grp), ], grp[!is.na(grp)]
    ) %>%
      pluck("results") %>% mutate(
        across(where(is.numeric), ~round(.x, 20))
      )


    if(type == "greedy"){

      result <- greedy

    }else if(type == "greedy_step"){

      df <- df %>% dplyr::select(all_of(greedy[["vars"]]))

    }
  }


  if(type == "greedy_step"  || type == "step"){

    set.seed(1)

    step <- klaR::stepclass(
      df[!is.na(grp), ],  grp[!is.na(grp)],
      "lda", improvement = 0.001, direction = "forward", output = FALSE
    ) %>%
      suppressMessages()


    step <- list(
      vars = step[["model"]][["name"]],
      accuracy = step[["result.pm"]][["crossval.rate"]]
    )


    result <- step
  }



  if(return_only_var){

    return(result[["vars"]])

  }else if(!return_only_var){

    return(result)

  }

}



