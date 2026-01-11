#' Batch segment classification with reproducible parallel execution
#'
#' Runs \code{\link{seg_classification_models}} over many segment solution columns
#' (each element of \code{xs_vec}) using a reproducible parallel RNG stream.
#' You can choose whether each \code{xs} gets the same seed or a deterministic
#' different seed, and whether all model types share the same CV folds or use
#' deterministic different folds.
#'
#' Reproducibility:
#' \itemize{
#'   \item Parallel reproducibility is enforced via \code{RNGkind("L'Ecuyer-CMRG")}
#'   and \code{mc.set.seed = TRUE}.
#'   \item Seeds used for each \code{xs} and each model type are stored in the returned object.
#' }
#'
#' @param xs_vec Character vector of segment solution column names (e.g., \code{"seg_solution_1"}).
#' @param temp_seg_data Data frame containing \code{hVendorID} and each \code{xs} column.
#' @param temp_db_data Data frame of predictors keyed by \code{hVendorID}.
#' @param n_folds Positive integer. Number of CV folds.
#' @param rf_tune_length Positive integer. Number of RF \code{mtry} values to try when \code{rf_mtry} is \code{NULL}.
#' @param glmnet_tune_length Positive integer. Number of tuning values for glmnet.
#' @param rda_tune_length Positive integer. Number of tuning values for RDA.
#' @param rf_ntree Positive integer. Number of trees for random forest (\code{randomForest::randomForest(ntree=...)}).
#' @param rf_mtry Optional integer scalar. If provided, RF uses a fixed \code{mtry} (no tuning).
#' @param rf_nodesize Optional integer scalar. If provided, RF uses a fixed \code{nodesize}.
#' @param seed Integer scalar. Global seed controlling all deterministic randomness.
#' @param mc.cores Positive integer. Number of forked processes for \code{parallel::mclapply()}.
#' @param xs_seed_mode Character, one of \code{"same"} or \code{"different"}.
#'   If \code{"different"}, each \code{xs} gets a deterministic unique base seed.
#' @param folds_mode Character, one of \code{"same"} or \code{"different"}.
#'   If \code{"same"}, all model types within an \code{xs} share the same folds.
#'   If \code{"different"}, each model type within an \code{xs} has deterministic different folds.
#'
#' @return A list with:
#' \describe{
#'   \item{summaries}{A combined data frame with one row per \code{xs} x model type, including CV/training accuracy and RF hyperparameters used.}
#'   \item{models}{Named list (by \code{xs}) of full per-\code{xs} results from \code{seg_classification_models}.}
#'   \item{errors}{A data frame of failures with \code{xs}, \code{xs_seed}, and \code{error} message.}
#'   \item{settings}{Batch settings (seed, modes, mc.cores).}
#' }
#'
#' @examples
#' \dontrun{
#' tmp_seg <- dplyr::tibble(
#'   hVendorID = seq_len(nrow(iris)),
#'   Species  = iris[["Species"]]
#' )
#' tmp_db <- dplyr::tibble(
#'   hVendorID = seq_len(nrow(iris)),
#'   Sepal.Length = iris[["Sepal.Length"]],
#'   Sepal.Width  = iris[["Sepal.Width"]],
#'   Petal.Length = iris[["Petal.Length"]],
#'   Petal.Width  = iris[["Petal.Width"]]
#' )
#'
#' out <- seg_classification_models_batch(
#'   xs_vec = "Species",
#'   temp_seg_data = tmp_seg,
#'   temp_db_data = tmp_db,
#'   n_folds = 5,
#'   rf_ntree = 100,
#'   rf_mtry = 2,
#'   rf_nodesize = 1,
#'   seed = 42,
#'   xs_seed_mode = "different",
#'   folds_mode = "same",
#'   mc.cores = 2
#' )
#' }
#'
#' @export
seg_classification_models_batch <- function(
    xs_vec,
    temp_seg_data,
    temp_db_data,
    n_folds = 5,
    rf_tune_length = 3,
    glmnet_tune_length = 10,
    rda_tune_length = 10,
    rf_ntree = 5,
    rf_mtry = NULL,
    rf_nodesize = NULL,
    seed = 1,
    mc.cores = parallel::detectCores(logical = FALSE),
    xs_seed_mode = c("same", "different"),
    folds_mode = c("same", "different")
) {
  xs_seed_mode <- match.arg(xs_seed_mode)
  folds_mode   <- match.arg(folds_mode)

  xs_vec <- unique(as.character(xs_vec))

  # Reproducible parallel RNG streams
  RNGkind(kind = "L'Ecuyer-CMRG")
  set.seed(seed)

  xs_seeds <- if (identical(xs_seed_mode, "different")) {
    seed + seq_along(xs_vec) - 1L
  } else {
    rep.int(seed, length(xs_vec))
  }

  results_list <- parallel::mclapply(
    seq_along(xs_vec),
    FUN = function(i) {
      one_xs   <- xs_vec[[i]]
      one_seed <- xs_seeds[[i]]

      res_try <- try(
        seg_classification_models(
          temp_seg_data = temp_seg_data,
          temp_db_data = temp_db_data,
          xs = one_xs,
          n_folds = n_folds,
          rf_tune_length = rf_tune_length,
          glmnet_tune_length = glmnet_tune_length,
          rda_tune_length = rda_tune_length,
          rf_ntree = rf_ntree,
          rf_mtry = rf_mtry,
          rf_nodesize = rf_nodesize,
          seed = one_seed,
          folds_mode = folds_mode
        ),
        silent = TRUE
      )

      if (inherits(res_try, "try-error")) {
        list(
          xs = one_xs,
          xs_seed = one_seed,
          ok = FALSE,
          error = as.character(res_try)
        )
      } else {
        res_try[["xs"]]      <- one_xs
        res_try[["xs_seed"]] <- one_seed

        list(
          xs = one_xs,
          xs_seed = one_seed,
          ok = TRUE,
          result = res_try
        )
      }
    },
    mc.cores = mc.cores,
    mc.set.seed = TRUE
  )

  ok_idx  <- vapply(results_list, function(z) isTRUE(z[["ok"]]), logical(1))
  success <- results_list[ok_idx]
  failure <- results_list[!ok_idx]

  models <- lapply(success, function(z) z[["result"]])
  if (length(models) > 0L) {
    names(models) <- vapply(success, function(z) z[["xs"]], character(1))
  }

  summary_tbl <- if (length(models) > 0L) {
    dplyr::bind_rows(
      lapply(success, function(z) {
        z[["result"]][["summary"]] %>%
          dplyr::mutate(
            xs = z[["xs"]],
            xs_seed = z[["xs_seed"]],
            .before = 1L
          )
      })
    )
  } else {
    NULL
  }

  error_tbl <- if (length(failure) > 0L) {
    data.frame(
      xs = vapply(failure, function(z) z[["xs"]], character(1)),
      xs_seed = vapply(failure, function(z) z[["xs_seed"]], numeric(1)),
      error = vapply(failure, function(z) z[["error"]], character(1)),
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  list(
    summaries = summary_tbl,
    models = models,
    errors = error_tbl,
    settings = list(
      seed = seed,
      xs_seed_mode = xs_seed_mode,
      folds_mode = folds_mode,
      mc.cores = mc.cores
    )
  )
}


#' Fit and compare classification models for a single segmentation solution
#'
#' Joins a segment label column (\code{xs}) from \code{temp_seg_data} onto predictors
#' in \code{temp_db_data} by \code{hVendorID}, drops rows with missing data, and fits:
#' \itemize{
#'   \item Random Forest (\code{method = "rf"})
#'   \item Multinomial glmnet (\code{method = "glmnet"})
#'   \item LDA (\code{method = "lda"}) (captured as \code{NULL} if it errors)
#'   \item RDA (\code{method = "rda"}) (captured as \code{NULL} if it errors)
#' }
#'
#' Reproducibility:
#' \itemize{
#'   \item CV folds are deterministic based on \code{seed} and \code{folds_mode}.
#'   \item Per-model seeds are deterministic and stored in \code{$seeds} and in each \code{$models[[...]]$seed}.
#' }
#'
#' Random Forest hyperparameters:
#' \itemize{
#'   \item \code{rf_ntree} (tree count) is passed to the RF fit.
#'   \item \code{rf_mtry} if provided fixes \code{mtry}; otherwise caret tunes \code{mtry}.
#'   \item \code{rf_nodesize} if provided fixes \code{nodesize}.
#'   \item The final RF \code{mtry} used is recorded from \code{fit_rf$bestTune}.
#' }
#'
#' @param temp_seg_data Data frame containing \code{hVendorID} and the segment column \code{xs}.
#' @param temp_db_data Data frame of predictors keyed by \code{hVendorID}.
#' @param xs Character scalar. Name of the segment column in \code{temp_seg_data}.
#' @param n_folds Positive integer. Number of CV folds.
#' @param rf_tune_length Positive integer. RF tuning length (ignored if \code{rf_mtry} is provided).
#' @param glmnet_tune_length Positive integer. Number of tuning values for glmnet.
#' @param rda_tune_length Positive integer. Number of tuning values for RDA.
#' @param rf_ntree Positive integer. Number of trees for RF.
#' @param rf_mtry Optional integer scalar. If provided, fixed RF \code{mtry} (no tuning).
#' @param rf_nodesize Optional integer scalar. If provided, fixed RF \code{nodesize}.
#' @param seed Integer scalar. Base seed controlling deterministic behavior.
#' @param folds_mode Character, one of \code{"same"} or \code{"different"}.
#'   If \code{"same"}, all model types use the same folds. If \code{"different"}, each model type uses different deterministic folds.
#'
#' @return A list with:
#' \describe{
#'   \item{summary}{Data frame with one row per model type. Includes CV and training accuracy plus RF hyperparameters where applicable.}
#'   \item{best_model_name}{Character scalar naming the best model by CV Accuracy (NA-safe).}
#'   \item{best_model}{The best model object (caret train object or \code{NULL} if all fail).}
#'   \item{best_confusion}{Confusion matrix for best model (or \code{NULL}).}
#'   \item{models}{List with entries \code{rf}, \code{glmnet}, \code{lda}, \code{rda}. Each contains \code{model}, \code{cm}, \code{seed}, and RF \code{params} when applicable.}
#'   \item{seeds}{List containing the base seed, folds seeds, and per-model seeds used.}
#'   \item{diagnostics}{Near-zero variance metrics and linear-combo diagnostics for troubleshooting LDA/RDA singularities.}
#' }
#'
#' @examples
#' \dontrun{
#' tmp_seg <- dplyr::tibble(
#'   hVendorID = seq_len(nrow(iris)),
#'   Species  = iris[["Species"]]
#' )
#' tmp_db <- dplyr::tibble(
#'   hVendorID = seq_len(nrow(iris)),
#'   Sepal.Length = iris[["Sepal.Length"]],
#'   Sepal.Width  = iris[["Sepal.Width"]],
#'   Petal.Length = iris[["Petal.Length"]],
#'   Petal.Width  = iris[["Petal.Width"]]
#' )
#'
#' fit <- seg_classification_models(
#'   temp_seg_data = tmp_seg,
#'   temp_db_data = tmp_db,
#'   xs = "Species",
#'   n_folds = 5,
#'   rf_ntree = 200,
#'   rf_mtry = 2,
#'   rf_nodesize = 1,
#'   seed = 123,
#'   folds_mode = "same"
#' )
#'
#' fit[["summary"]]
#' fit[["models"]][["rf"]][["params"]]
#' fit[["seeds"]]
#' }
#'
#' @export
seg_classification_models <- function(
    temp_seg_data,
    temp_db_data,
    xs,
    n_folds = 5,
    rf_tune_length = 3,
    glmnet_tune_length = 10,
    rda_tune_length = 10,
    rf_ntree = 10,
    rf_mtry = NULL,
    rf_nodesize = NULL,
    seed = 1,
    folds_mode = c("same", "different")
) {
  folds_mode <- match.arg(folds_mode)

  temp_data <- dplyr::right_join(
    temp_seg_data %>% dplyr::select(hVendorID, dplyr::all_of(xs)),
    temp_db_data,
    by = dplyr::join_by(hVendorID)
  ) %>%
    tidyr::drop_na()

  temp_data_input <- temp_data %>%
    dplyr::select(-hVendorID, -dplyr::all_of(xs))

  temp_data_class <- temp_data[[xs]] %>%
    paste0("seg_", .) %>%
    as.factor()

  rm(temp_data, temp_seg_data, temp_db_data)

  stopifnot(is.data.frame(temp_data_input))
  stopifnot(length(temp_data_class) == nrow(temp_data_input))
  stopifnot(is.factor(temp_data_class))

  # ----------------------------------------------------------
  # Diagnostics: NZV + linear combos (useful for LDA/RDA issues)
  # ----------------------------------------------------------
  nzv_metrics <- caret::nearZeroVar(
    temp_data_input,
    saveMetrics = TRUE
  )
  nzv_vars <- rownames(nzv_metrics)[nzv_metrics[["nzv"]]]

  lin_combo_try <- try(
    caret::findLinearCombos(as.matrix(temp_data_input)),
    silent = TRUE
  )

  if (!inherits(lin_combo_try, "try-error") && !is.null(lin_combo_try[["linearCombos"]])) {
    lin_combo_idx <- lin_combo_try[["linearCombos"]]
    lin_combo_names <- lapply(
      lin_combo_idx,
      function(idx) colnames(temp_data_input)[idx]
    )
  } else {
    lin_combo_idx <- NULL
    lin_combo_names <- NULL
  }

  # ----------------------------------------------------------
  # Deterministic folds (and deterministic per-model seeds)
  # ----------------------------------------------------------
  make_folds <- function(folds_seed) {
    set.seed(folds_seed)
    caret::createFolds(y = temp_data_class, k = n_folds, returnTrain = TRUE)
  }

  seeds_used <- list(
    global_seed = seed,
    folds_mode = folds_mode,
    model_seeds = list(
      rf = seed + 1L,
      glmnet = seed + 2L,
      lda = seed + 3L,
      rda = seed + 4L
    )
  )

  if (identical(folds_mode, "same")) {
    folds_seed <- seed + 10000L
    folds_index <- make_folds(folds_seed)

    seeds_used[["folds_seed"]] <- folds_seed

    ctrl_shared <- caret::trainControl(
      method = "cv",
      number = n_folds,
      index = folds_index,
      classProbs = TRUE,
      summaryFunction = caret::multiClassSummary,
      allowParallel = FALSE
    )

    ctrl_rf <- ctrl_shared
    ctrl_glmnet <- ctrl_shared
    ctrl_lda <- ctrl_shared
    ctrl_rda <- ctrl_shared
  } else {
    folds_seed_rf     <- seed + 10001L
    folds_seed_glmnet <- seed + 10002L
    folds_seed_lda    <- seed + 10003L
    folds_seed_rda    <- seed + 10004L

    seeds_used[["folds_seed"]] <- list(
      rf = folds_seed_rf,
      glmnet = folds_seed_glmnet,
      lda = folds_seed_lda,
      rda = folds_seed_rda
    )

    ctrl_rf <- caret::trainControl(
      method = "cv",
      number = n_folds,
      index = make_folds(folds_seed_rf),
      classProbs = TRUE,
      summaryFunction = caret::multiClassSummary,
      allowParallel = FALSE
    )

    ctrl_glmnet <- caret::trainControl(
      method = "cv",
      number = n_folds,
      index = make_folds(folds_seed_glmnet),
      classProbs = TRUE,
      summaryFunction = caret::multiClassSummary,
      allowParallel = FALSE
    )

    ctrl_lda <- caret::trainControl(
      method = "cv",
      number = n_folds,
      index = make_folds(folds_seed_lda),
      classProbs = TRUE,
      summaryFunction = caret::multiClassSummary,
      allowParallel = FALSE
    )

    ctrl_rda <- caret::trainControl(
      method = "cv",
      number = n_folds,
      index = make_folds(folds_seed_rda),
      classProbs = TRUE,
      summaryFunction = caret::multiClassSummary,
      allowParallel = FALSE
    )
  }

  # ----------------------------------------------------------
  # Random Forest
  # ----------------------------------------------------------
  rf_tune_grid <- if (!is.null(rf_mtry)) {
    data.frame(mtry = as.integer(rf_mtry))
  } else {
    NULL
  }

  set.seed(seeds_used[["model_seeds"]][["rf"]])
  fit_rf <- caret::train(
    x = temp_data_input,
    y = temp_data_class,
    method = "rf",
    trControl = ctrl_rf,
    metric = "Accuracy",
    tuneGrid = rf_tune_grid,
    tuneLength = if (is.null(rf_tune_grid)) rf_tune_length else NULL,
    ntree = as.integer(rf_ntree),
    nodesize = if (is.null(rf_nodesize)) NULL else as.integer(rf_nodesize)
  )

  pred_rf <- predict(fit_rf, temp_data_input)
  cm_rf <- caret::confusionMatrix(pred_rf, temp_data_class)
  acc_rf <- max(fit_rf[["results"]][["Accuracy"]], na.rm = TRUE)

  rf_mtry_used <- fit_rf[["bestTune"]][["mtry"]]
  rf_params_used <- list(
    ntree = as.integer(rf_ntree),
    mtry = as.integer(rf_mtry_used),
    nodesize = if (is.null(rf_nodesize)) NA_integer_ else as.integer(rf_nodesize)
  )

  # ----------------------------------------------------------
  # Multinomial glmnet
  # ----------------------------------------------------------
  set.seed(seeds_used[["model_seeds"]][["glmnet"]])
  fit_glmnet <- caret::train(
    x = temp_data_input,
    y = temp_data_class,
    method = "glmnet",
    trControl = ctrl_glmnet,
    tuneLength = glmnet_tune_length,
    metric = "Accuracy"
  )

  pred_glmnet <- predict(fit_glmnet, temp_data_input)
  cm_glmnet <- caret::confusionMatrix(pred_glmnet, temp_data_class)
  acc_glmnet <- max(fit_glmnet[["results"]][["Accuracy"]], na.rm = TRUE)

  # ----------------------------------------------------------
  # LDA
  # ----------------------------------------------------------
  fit_lda_try <- try(
    {
      set.seed(seeds_used[["model_seeds"]][["lda"]])
      caret::train(
        x = temp_data_input,
        y = temp_data_class,
        method = "lda",
        trControl = ctrl_lda,
        metric = "Accuracy"
      )
    },
    silent = TRUE
  )

  if (inherits(fit_lda_try, "try-error")) {
    fit_lda <- NULL
    cm_lda <- NULL
    acc_lda <- NA_real_
  } else {
    fit_lda <- fit_lda_try
    pred_lda <- predict(fit_lda, temp_data_input)
    cm_lda <- caret::confusionMatrix(pred_lda, temp_data_class)
    acc_lda <- max(fit_lda[["results"]][["Accuracy"]], na.rm = TRUE)
  }

  # ----------------------------------------------------------
  # RDA
  # ----------------------------------------------------------
  fit_rda_try <- tryCatch(
    {
      set.seed(seeds_used[["model_seeds"]][["rda"]])
      caret::train(
        x = temp_data_input,
        y = temp_data_class,
        method = "rda",
        trControl = ctrl_rda,
        tuneLength = rda_tune_length,
        metric = "Accuracy"
      )
    },
    error = function(e) e
  )

  if (inherits(fit_rda_try, "error")) {
    warning(
      paste0(
        "RDA failed with error: ", fit_rda_try[["message"]],
        "\nCheck $diagnostics for NZV and linear combos that may be causing singularity."
      )
    )
    fit_rda <- NULL
    cm_rda <- NULL
    acc_rda <- NA_real_
  } else {
    fit_rda <- fit_rda_try
    pred_rda <- predict(fit_rda, temp_data_input)
    cm_rda <- caret::confusionMatrix(pred_rda, temp_data_class)
    acc_rda <- max(fit_rda[["results"]][["Accuracy"]], na.rm = TRUE)
  }

  # ----------------------------------------------------------
  # Summary + best model (NA-safe)
  # ----------------------------------------------------------
  acc_vec <- c(
    rf = acc_rf,
    glmnet = acc_glmnet,
    lda = acc_lda,
    rda = acc_rda
  )

  train_acc_vec <- c(
    rf = cm_rf[["overall"]][["Accuracy"]],
    glmnet = cm_glmnet[["overall"]][["Accuracy"]],
    lda = if (!is.null(cm_lda)) cm_lda[["overall"]][["Accuracy"]] else NA_real_,
    rda = if (!is.null(cm_rda)) cm_rda[["overall"]][["Accuracy"]] else NA_real_
  )

  # RF params columns (NA for non-RF rows)
  rf_ntree_col <- c(rf = as.integer(rf_ntree), glmnet = NA_integer_, lda = NA_integer_, rda = NA_integer_)
  rf_mtry_col  <- c(rf = as.integer(rf_mtry_used), glmnet = NA_integer_, lda = NA_integer_, rda = NA_integer_)
  rf_nodesize_col <- c(
    rf = if (is.null(rf_nodesize)) NA_integer_ else as.integer(rf_nodesize),
    glmnet = NA_integer_,
    lda = NA_integer_,
    rda = NA_integer_
  )

  seed_model_col <- c(
    rf = seeds_used[["model_seeds"]][["rf"]],
    glmnet = seeds_used[["model_seeds"]][["glmnet"]],
    lda = seeds_used[["model_seeds"]][["lda"]],
    rda = seeds_used[["model_seeds"]][["rda"]]
  )

  summary_tbl <- data.frame(
    model = names(acc_vec),
    CV_Accuracy = as.numeric(acc_vec),
    Training_Accuracy = as.numeric(train_acc_vec),
    seed_model = as.numeric(seed_model_col),
    rf_ntree = as.integer(rf_ntree_col),
    rf_mtry = as.integer(rf_mtry_col),
    rf_nodesize = as.integer(rf_nodesize_col),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  best_name <- names(acc_vec)[which.max(replace(acc_vec, is.na(acc_vec), -Inf))]

  best_model <- switch(
    best_name,
    rf = fit_rf,
    glmnet = fit_glmnet,
    lda = fit_lda,
    rda = fit_rda
  )

  best_cm <- switch(
    best_name,
    rf = cm_rf,
    glmnet = cm_glmnet,
    lda = cm_lda,
    rda = cm_rda
  )

  list(
    summary = summary_tbl,
    best_model_name = best_name,
    best_model = best_model,
    best_confusion = best_cm,
    models = list(
      rf = list(
        model = fit_rf,
        cm = cm_rf,
        seed = seeds_used[["model_seeds"]][["rf"]],
        params = rf_params_used
      ),
      glmnet = list(
        model = fit_glmnet,
        cm = cm_glmnet,
        seed = seeds_used[["model_seeds"]][["glmnet"]]
      ),
      lda = list(
        model = fit_lda,
        cm = cm_lda,
        seed = seeds_used[["model_seeds"]][["lda"]]
      ),
      rda = list(
        model = fit_rda,
        cm = cm_rda,
        seed = seeds_used[["model_seeds"]][["rda"]]
      )
    ),
    seeds = seeds_used,
    diagnostics = list(
      nzv_metrics = nzv_metrics,
      nzv_vars = nzv_vars,
      linear_combo_idx = lin_combo_idx,
      linear_combo_vars = lin_combo_names
    )
  )
}
