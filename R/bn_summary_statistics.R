#' Summarize Bayesian Network Model Fit and Node-Level Statistics
#'
#' @description
#' Computes key summary statistics for a fitted or unfitted Bayesian network, including
#' node-level scores (BIC, EBIC, AIC, log-likelihood), model-level scores, and—if a
#' dependent variable (`dv`) is provided—prediction accuracy and comparison against a
#' naive Bayes baseline.
#'
#' @param bn A fitted or unfitted Bayesian network object (class `"bn"`).
#' @param df A data frame used for scoring or fitting the Bayesian network.
#' @param dv Optional. Character string specifying the dependent variable node to predict.
#' @param fit Optional. A pre-fitted `bn.fit` object. If `NULL`, the network is fitted
#'   using `bn.fit(bn, df, method = "bayes")`.
#' @param compare_to_naive Logical; if `TRUE` (default), computes a naive Bayes model
#'   for comparison to the full network.
#' @param suppress_bn_warning Logical; if `TRUE`, suppresses the BIC direction warning.
#' @param seed Integer random seed for reproducibility. Default = 1.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{`confusion_matrix`}{A confusion matrix (if `dv` specified).}
#'   \item{`nodes`}{A tibble of node-level scores (BIC, EBIC, AIC, log-likelihood).}
#'   \item{`model`}{A tibble of overall model scores and, if applicable, accuracy metrics.}
#' }
#'
#' @details
#' BIC values from `bnlearn::score()` are *rescaled* (multiplied by -2) in `bnlearn`,
#' so higher values indicate better models.
#'
#' @importFrom bnlearn score bn.fit naive.bayes
#' @importFrom caret confusionMatrix
#' @importFrom dplyr mutate relocate as_tibble bind_cols
#' @importFrom purrr pluck
#'
#' @export
bn_summary_statistics <- function(
    bn,
    df,
    dv = NULL,
    fit = NULL,
    compare_to_naive = TRUE,
    suppress_bn_warning = FALSE,
    seed = 1
){

  results <- list()


  # --- Node-level statistics ---
  results[["nodes"]] <- list(
    vars   = bnlearn::score(bn, df, type = "bic", by.node = TRUE) %>% names(),
    bic    = bnlearn::score(bn, df, type = "bic", by.node = TRUE),
    ebic   = bnlearn::score(bn, df, type = "ebic", by.node = TRUE),
    aic    = bnlearn::score(bn, df, type = "aic", by.node = TRUE),
    loglik = bnlearn::score(bn, df, type = "loglik", by.node = TRUE)
  ) %>%
    as_tibble()


  # --- Model-level statistics ---
  results[["model"]] <- list(
    bic    = bnlearn::score(bn, df, type = "bic"),
    ebic   = bnlearn::score(bn, df, type = "ebic"),
    aic    = bnlearn::score(bn, df, type = "aic"),
    loglik = bnlearn::score(bn, df, type = "loglik")
  ) %>%
    as_tibble()



  # --- Dependent variable prediction ---
  if (!is.null(dv)) {


    if (is.null(fit)) {
      fit <- bnlearn::bn.fit(bn, data = df, method = "bayes")
    }


    df_predict <- bnlearn:::predict.bn.fit(
      object = fit,
      node = dv,
      data = df,
      method = "bayes-lw"
    ) %>%
      dplyr::bind_cols(df[[dv]]) %>%
      setNames(c("predicted", "actual")) %>%
      suppressMessages()


    results[["confusion_matrix"]] <- caret::confusionMatrix(
      df_predict[["predicted"]],
      df_predict[["actual"]]
    )


    results[["model"]] <- results[["model"]] %>%
      dplyr::mutate(
        dv = dv,
        accuracy = results[["confusion_matrix"]][["overall"]][["Accuracy"]]
      ) %>%
      dplyr::relocate(dv, accuracy)



    # --- Compare to naive Bayes ---
    if (compare_to_naive) {

      if(!is.null(seed)) set.seed(seed)
      fit_naive <- bnlearn::naive.bayes(df, training = dv)

      df_predict_naive <- predict(fit_naive, data = df) %>%
        dplyr::bind_cols(df[[dv]]) %>%
        setNames(c("predicted", "actual")) %>%
        suppressMessages()

      results[["model"]] <- results[["model"]] %>%
        dplyr::mutate(
          accuracy_naive = caret::confusionMatrix(
            df_predict_naive[["predicted"]],
            df_predict_naive[["actual"]]
          ) %>%
            purrr::pluck("overall") %>%
            purrr::pluck("Accuracy"),

          bic_naive = bnlearn::score(fit_naive, df, type = "bic"),
          accuracy_improve_perc = (accuracy - accuracy_naive) / accuracy_naive,
          bic_improve_perc = (bic_naive - bic) / bic_naive
        )
    }
  }



  # --- Output cleanup ---
  results <- results[c("confusion_matrix", "nodes", "model")]


  if(!suppress_bn_warning) work::warning_bnlearn_bic()


  return(results)
}
