#' bn_learn_networks
#' @description bn_learn_networks
#' @export
bn_learn_networks <- function(
    df,
    dv,
    ivs,
    white_list = NULL,
    black_list = NULL
){

  set.seed(1)
  results <- list()

  df <- df %>% as.data.frame()

  dv <- dv %>% unlist() %>% as.character()
  ivs <- ivs %>% unlist() %>% as.character()


  results[["cross_battery"]] <- dv %>%
    map(
      ~bn_tan(
        df = df,
        dv = .,
        ivs = ivs,
        white_list = white_list,
        black_list = black_list,
        cross_battery_first = TRUE,
        suppress_bn_warning = TRUE)
    ) %>%
    setNames(dv)


  results[["no_cross_battery"]] <- dv %>%
    map(
      ~bn_tan(
        df = df_impute,
        dv = .,
        ivs = ivs,
        white_list = white_list,
        black_list = black_list,
        cross_battery_first = FALSE,
        suppress_bn_warning = TRUE)
    ) %>%
    setNames(glue("{dv} no cb"))


  results[["summary"]] <- results %>%
    pluck("cross_battery") %>%
    map(pluck("summary")) %>%
    map(pluck("model")) %>%
    bind_rows() %>%
    bind_rows(
      results %>%
        pluck("no_cross_battery") %>%
        map(pluck("summary")) %>%
        map(pluck("model")) %>%
        bind_rows() %>%
        mutate(
          dv = glue("{dv} no cb")
        )
    ) %>%
    arrange(dv)


  return(results)
}


