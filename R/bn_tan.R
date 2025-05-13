#' bn_tan
#' @description bn_tan
#' @export
bn_tan <- function(
    df,
    dv,
    ivs,
    white_list = NULL,
    black_list = NULL,
    cross_battery_first = TRUE,
    compare_to_niave = TRUE,
    suppress_bn_warning = FALSE
){

  set.seed(1)

  require(bnlearn)
  results <- list()

  ##############################
  # set up
  ##############################

  vars <- c(dv, ivs) %>%
    unlist() %>%
    setNames(NULL)

  if(!is.null(white_list)){
    white_list <- white_list %>%
      as.data.frame()
  }

  if(!is.null(black_list)){
    black_list <- black_list %>%
      as.data.frame()
  }


  dfx <- df %>%
    select(all_of(vars)) %>%
    as.data.frame()



  ##############################
  # find cross battery whitelist
  ##############################

  if(cross_battery_first){

    cb_black_list <- ivs %>%
      map_dfr(make_arcs) %>%
      bind_rows(black_list) %>%
      distinct()


    cb_white_list <- tree.bayes(
      dfx,
      dv,
      blacklist = cb_black_list,
      whitelist = white_list
    ) %>%
      arcs() %>%
      as.data.frame() %>%
      filter(
        from != dv | from != dv
      )



    if(is.null(white_list)){

      white_list <- cb_white_list

    }else if(!is.null(white_list)){

      white_list <- white_list %>%
        bind_rows(cb_white_list) %>%
        distinct()
    }
  }


  ##############################
  # model
  ##############################

  bn <- tree.bayes(
    dfx,
    training = dv,
    blacklist = black_list,
    whitelist = white_list
  )


  ##############################
  # results
  ##############################

  results[["bn"]] <- bn


  results[["fit"]] <- bn %>%
    bn.fit(dfx, method = "bayes")


  results[["arcs"]] <- bn %>%
    bn_arc_chisq(dfx, dv = dv)


  results[["summary"]] <- bn %>% bn_summary_statistics(
    df = dfx,
    dv = dv,
    fit = results[["fit"]],
    compare_to_niave = compare_to_niave,
    suppress_bn_warning = suppress_bn_warning
  )

  return(results)
}
