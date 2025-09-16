#' impute_data
#' @description impute_data
#' @export
impute_data <- function(
    df,
    vars,
    threshold_impute = .8,
    make_factor = TRUE,
    un_make_factor = FALSE
){

  require(bnlearn)
  work::start()

  vars <- vars %>%
    unlist() %>%
    setNames(NULL)


  # filter to missing threshold
  df_filtered <- df %>%
    filter(
      (across(everything(), function(x)!is.na(x)) %>% rowMeans()) >= threshold_impute
    )


  if(nrow(df) != nrow(df_filtered)){
    warning(
      glue("Dropped {nrow(df) - nrow(df_filtered)} rows due to bellow missing theshold")
    )
  }


  if(make_factor){
    df_filtered <- df_filtered %>% mutate(
      across(all_of(vars), as.factor)
    )
  }


  dfx <- df_filtered %>%
    select(all_of(vars))


  dfx_no_missing <- dfx %>%
    filter(
      (across(everything(), function(x)!is.na(x)) %>% rowMeans()) == 1
    )

  df_imputed <- dfx_no_missing %>%
    as.data.frame() %>%
    bnlearn::tabu() %>%
    bnlearn::bn.fit(
      dfx_no_missing %>%
        as.data.frame(),
      method = "mle"
    ) %>%
    bnlearn::impute(dfx %>% as.data.frame()) %>%
    as_tibble()


  df_imputed <- df_filtered %>%
    select(
      ., !all_of(vars)
    ) %>%
    bind_cols(df_imputed)


  if(identical(df_filtered, df_imputed)){
    warning("Data did not need to be imputed")
  }


  if(un_make_factor){

    df_imputed <- df_imputed %>%
      mutate(
        across(all_of(vars), ~as.character(.) %>% as.numeric())
      )

  }

  return(df_imputed)
}
