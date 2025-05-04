#' bn_summary_statistics
#' @description bn_summary_statistics
#' @export
bn_summary_statistics <- function(
    bn,
    df,
    dv = NULL,
    fit = NULL,
    compare_to_niave = TRUE,
    suppress_bn_warning = FALSE
){

  require(bnlearn)

  results <- list()


  results[["nodes"]] <- list(
    vars = bn %>% score(df, type = "bic", by.node = T) %>% names(),
    bic = bn %>% score(df, type = "bic", by.node = T),
    ebic = bn %>% score(df, type = "ebic", by.node = T),
    aic = bn %>% score(df, type = "aic", by.node = T),
    loglik = bn %>% score(df, type = "loglik", by.node = T)
  ) %>%
    as_tibble()



  results[["model"]] <- list(
    bic = bn %>% score(df, type = "bic"),
    ebic = bn %>% score(df, type = "ebic"),
    aic = bn %>% score(df, type = "aic"),
    loglik = bn %>% score(df, type = "loglik")
  ) %>%
    as_tibble()



  if(!is.null(dv)){


    if(is.null(fit)){
      fit <- bn %>%
        bn.fit(df, method = "bayes")
    }


    df_predict <- bnlearn:::predict.bn.fit(
      object = fit,
      node = dv,
      data = df,
      method = 'bayes-lw'
    ) %>%
      bind_cols(df[[dv]]) %>%
      setNames(c("predicted", "actual")) %>%
      suppressMessages()


    results[["confusion_matrix"]] <- caret::confusionMatrix(
      df_predict[["predicted"]], df_predict[["actual"]]
    )


    results[["model"]] <- results[["model"]] %>%
      mutate(
        dv =  dv,
        accuracy = results[["confusion_matrix"]] %>%
          pluck("overall") %>%
          pluck("Accuracy")
      ) %>%
      relocate(
        accuracy, .before = 1
      ) %>%
      relocate(
        dv, .before = accuracy
      )



    if(compare_to_niave){

      fit_naive <- naive.bayes(df, training = dv)

      df_predict_naive <- predict(
        object = fit_naive,
        data = df
      ) %>%
        bind_cols(df[[dv]]) %>%
        setNames(c("predicted", "actual")) %>%
        suppressMessages()


      results[["model"]] <- results[["model"]] %>%
        mutate(
          accuracy_naive = caret::confusionMatrix(
            df_predict_naive[["predicted"]], df_predict_naive[["actual"]]
          ) %>%
            pluck("overall") %>%
            pluck("Accuracy"),

          bic_naive = fit_naive %>%
            score(df, type = "bic"),

          accuracy_improve_perc = (accuracy - accuracy_naive) / accuracy_naive,

          bic_improve_perc = (bic_naive - bic) / bic_naive,
        )
    }
  }


  results <- results[c("confusion_matrix", "nodes", "model")]


  if(!suppress_bn_warning){
    warning("Higher BIC values are better in bnlearn, as it's rescaled by -2")
  }


  return(results)
}
