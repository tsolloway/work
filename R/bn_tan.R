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
    dictionary = NULL,
    manual_groups = NULL,
    node_label_type = c("both", "variable", "label"),
    n_groups = NULL,
    node_size = 1,
    suppress_bn_warning = FALSE,
    on_exit_detach_igraph = TRUE
){

  set.seed(1)

  require(bnlearn, quietly = TRUE) %>% suppressMessages()

  node_label_type <- match.arg(node_label_type)

  dictionary <- dictionary %>% dictionary_from_named_object()

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
      dplyr::bind_rows(black_list) %>%
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


  results[["viz_prep"]] <- bn_to_netviz_prep(
      bn = results,
      dictionary = dictionary,
      node_label_type = node_label_type,
      manual_groups = manual_groups,
      n_groups = n_groups,
      node_size = node_size,
      on_exit_detach_igraph = on_exit_detach_igraph
    )


  results[["meta"]] <- list(analysis = "bn_model_single")


  return(results)
}
