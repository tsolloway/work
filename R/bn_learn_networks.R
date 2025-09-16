#' bn_learn_networks
#' @description bn_learn_networks
#' @export
bn_learn_networks <- function(
    df,
    dv,
    ivs,
    white_list = NULL,
    black_list = NULL,
    type = c("both", "cross_battery", "no_cross_battery")
){

  set.seed(1)
  type <- match.arg(type)
  cb_exist <- FALSE
  ncb_exist <- FALSE
  results <- list()


  df <- df %>% as.data.frame()


  dv <- dv %>% unlist() %>% as.character() %>% setNames(NULL)
  ivs <- ivs %>% map(as.character)


  if(length(ivs) == 1 || !is.list(ivs)){
    ivs <- ivs %>% unlist() %>% as.character() %>% setNames(NULL)
  }



  if(
    (type != "no_cross_battery") && (is.list(ivs) && length(ivs) > 1)
  ){

    cb_exist <- TRUE

    results[["cross_battery"]] <- dv %>%
      map(
        ~bn_tan(
          df = df,
          dv = .,
          ivs = ivs,
          white_list = white_list,
          black_list = black_list,
          cross_battery_first = TRUE,
          suppress_bn_warning = TRUE
        )
      ) %>%
      setNames(dv)

    summary_cb <- results[["cross_battery"]] %>%
      map(pluck("summary")) %>%
      map(pluck("model")) %>%
      bind_rows()
  }


  if(type != "cross_battery"){

    ncb_exist <- TRUE

    results[["no_cross_battery"]] <- dv %>%
      map(
        ~bn_tan(
          df = df,
          dv = .,
          ivs = ivs,
          white_list = white_list,
          black_list = black_list,
          cross_battery_first = FALSE,
          suppress_bn_warning = TRUE
        )
      ) %>%
      setNames(glue("{dv} no cb"))

    summary_ncb <- results[["no_cross_battery"]] %>%
      map(pluck("summary")) %>%
      map(pluck("model")) %>%
      bind_rows() %>%
      mutate(
        dv = glue("{dv} no cb")
      )
  }



  if(cb_exist && ncb_exist){

    results[["summary"]] <- bind_rows(
      summary_cb,
      summary_ncb
    ) %>%
      arrange(dv)

  }else if(cb_exist){

    results[["summary"]] <- summary_cb %>% arrange(dv)

  }else if(ncb_exist){

    results[["summary"]] <- summary_ncb %>% arrange(dv)

  }


  results[["meta"]] <- list(analysis = "bn_model_multiple")


  return(results)
}


