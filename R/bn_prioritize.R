#' bn_prioritize
#'
#' @description Forward selection to find the optimal ordering of IV
#'   attributes by their joint impact on the DV using exact Bayesian network
#'   inference. Supports greedy (one-at-a-time) or exhaustive (best combo at
#'   each size) search, with either max-level or lift-shifted evidence.
#'
#' @param obj A Bayesian network object. Accepts:
#'   \itemize{
#'     \item Output of \code{bn_finalize_network()} (extracts first subgroup fit)
#'     \item A single subgroup element with \code{fit} and \code{meta}
#'     \item A bare \code{bnlearn::bn.fit} object
#'   }
#' @param df Data frame containing the DV and IV columns.
#' @param dictionary Optional. Dictionary for variable labels.
#' @param dv Character or NULL. Dependent variable name. If NULL, auto-detected
#'   from \code{obj$meta$dv}.
#' @param ivs Character vector or NULL. Independent variable names. If NULL,
#'   auto-detected from \code{obj$meta$ivs}.
#' @param ivs_excluded Character vector or NULL. Variables to exclude from the
#'   analysis. Removed from \code{ivs} before any computation. Default NULL.
#' @param strategy Character. Evidence strategy:
#'   \code{"lift"} (default) shifts selected IVs' distributions via soft evidence.
#'   \code{"max"} sets selected IVs to their max level as hard evidence.
#' @param search Character. Search method:
#'   \code{"greedy"} (default) adds one IV at a time, keeping the best
#'   extension at each step.
#'   \code{"exhaustive"} evaluates all C(n, k) combinations at each size k
#'   to find the globally best combo.
#' @param lift Numeric. Distribution shift for \code{strategy = "lift"}.
#'   Interpretation depends on \code{impact_metric_type}. Default 0.10.
#' @param min_base_for_boot Integer. Minimum sample size required to run
#'   bootstrap p-values. When \code{n_obs < min_base_for_boot}, p-values are
#'   skipped. Default 75.
#' @param dv_metric Character. \code{"mean"} (default) computes the expected DV
#'   value; \code{"top_box"} uses the probability of the highest DV level.
#' @param impact_metric_type Character. How \code{lift} is interpreted:
#'   \code{"proportional"} (default) shifts by a fraction of the current mean;
#'   \code{"absolute"} shifts by a fixed number of scale points.
#' @param impact_result Optional. Output of \code{bn_impact()} or a tibble with
#'   \code{Variable} and an index column. Used to seed round 1 ordering in
#'   greedy search.
#' @param threshold Numeric or NULL. Stop when marginal gain drops below this
#'   fraction of the current DV estimate. Default 0.01 (1 percent). Set NULL
#'   to disable.
#' @param max_rounds Integer or NULL. Maximum combo size (exhaustive) or number
#'   of priority rounds (greedy). NULL means no limit.
#' @param noise_tail Numeric. Fraction of tail steps used to estimate the
#'   noise floor for bootstrap p-values. Default 1/3.
#' @param n_boot_final Integer or NULL. If set, bootstraps the final result
#'   to produce p-values via a noise-floor test. Default 100.
#' @param weight Character or NULL. Column name in \code{df} containing
#'   observation weights for the lift strategy frequency distribution. Default
#'   NULL (unweighted).
#' @param use_parallel Logical. Parallelize candidate evaluation within rounds.
#' @param verbose Logical. Print round/step/bootstrap progress messages.
#'   Default TRUE.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return A tibble with columns: \code{priority}, \code{variable},
#'   \code{label}, \code{combo}, \code{dv_estimate}, \code{marginal_gain},
#'   \code{marginal_gain_pct}. If \code{n_boot_final} is set, also includes
#'   \code{p_value} (proportion of bootstrap replicates where marginal gain
#'   is at or below the noise floor).
#'
#' @export
bn_prioritize <- function(
    obj,
    df,
    dictionary = NULL,
    dv = NULL,
    ivs = NULL,
    ivs_excluded = NULL,
    strategy = c("lift", "max"),
    search = c("greedy", "exhaustive"),
    lift = 0.10,
    min_base_for_boot = 75,
    dv_metric = c("mean", "top_box"),
    impact_metric_type = c("proportional", "absolute"),
    impact_result = NULL,
    threshold = 0.01,
    max_rounds = NULL,
    noise_tail = 1/3,
    n_boot_final = 100,
    weight = NULL,
    use_parallel = TRUE,
    verbose = TRUE,
    seed = 1
) {

  strategy <- match.arg(strategy)
  search <- match.arg(search)
  dv_metric <- match.arg(dv_metric)
  impact_metric_type <- match.arg(impact_metric_type)

  # Preserve named dv for display, strip for bnlearn
  dv <- unname(dv)

  set.seed(seed)

  # gRain uses cat() not message() — sink stdout to suppress
  sink(nullfile())
  on.exit(sink(), add = TRUE)

  # Flatten ivs if passed as a list (e.g., community groups)
  if (is.list(ivs)) ivs <- unlist(ivs) %>% setNames(NULL)

  # Weighted frequency helper
  .get_freq <- function(x, w = NULL) {
    if (is.null(w)) return(table(x))
    out <- tapply(w, x, sum)
    out[is.na(out)] <- 0
    out
  }

  # ---------------------------------------------------------------------------
  # 1. Extract fit object
  # ---------------------------------------------------------------------------
  if ("bn_subgroups" %in% names(obj)) {
    sg_names <- names(obj[["bn_subgroups"]])
    first_sg <- obj[["bn_subgroups"]][[sg_names[1]]]
    fit <- first_sg[["fit"]]
    bn <- first_sg[["bn"]] %||% bnlearn::bn.net(fit)
    if (is.null(ivs)) ivs <- first_sg[["meta"]][["ivs"]] %>% unlist() %>% setNames(NULL)
    if (is.null(dv) || missing(dv)) dv <- first_sg[["meta"]][["dv"]]
  } else if ("fit" %in% names(obj) && "meta" %in% names(obj)) {
    fit <- obj[["fit"]]
    bn <- obj[["bn"]] %||% bnlearn::bn.net(fit)
    if (is.null(ivs)) ivs <- obj[["meta"]][["ivs"]] %>% unlist() %>% setNames(NULL)
    if (is.null(dv) || missing(dv)) dv <- obj[["meta"]][["dv"]]
  } else if (inherits(obj, "bn.fit")) {
    fit <- obj
    bn <- bnlearn::bn.net(obj)
  } else {
    stop("Cannot extract fitted BN from obj.")
  }

  # Remove excluded IVs
  if (!is.null(ivs_excluded)) ivs <- setdiff(ivs, ivs_excluded)

  # ---------------------------------------------------------------------------
  # 2. Compile grain object once
  # ---------------------------------------------------------------------------
  grain_bn <- suppressMessages(bnlearn::as.grain(fit))

  # ---------------------------------------------------------------------------
  # 3. DV query helper (hard evidence)
  # ---------------------------------------------------------------------------
  .query_dv <- function(grain, evidence) {
    dist <- suppressMessages(gRain::querygrain(grain, nodes = dv, evidence = evidence, simplify = TRUE))
    if (dv_metric == "top_box") {
      dist %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
    } else {
      sum(as.numeric(names(dist)) * as.numeric(dist))
    }
  }

  # Baseline DV estimate (no evidence)
  baseline <- .query_dv(grain_bn, evidence = list())
  if (verbose) cli::cli_alert_info("Baseline DV estimate: {round(baseline, 4)}")

  baseline_row <- tibble::tibble(
    priority = 0L,
    variable = "Baseline",
    combo = NA_character_,
    dv_estimate = baseline,
    marginal_gain = 0,
    marginal_gain_pct = 0
  )

  # ---------------------------------------------------------------------------
  # 4. Strategy-specific setup + unified combo evaluator
  # ---------------------------------------------------------------------------
  if (strategy == "max") {
    ivs_max <- df %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(ivs),
        ~as.character(.x) %>% as.numeric() %>% max(na.rm = TRUE) %>% as.character()
      )) %>%
      as.list()

    .eval_combo <- function(combo) {
      ev <- ivs_max[combo]
      .query_dv(grain_bn, evidence = ev)
    }
  } else {
    w <- if (!is.null(weight)) df[[weight]] else NULL
    ivs_likelihood <- purrr::map(rlang::set_names(ivs), function(iv) {
      prior <- suppressMessages(gRain::querygrain(grain_bn, nodes = iv, simplify = TRUE))
      freq <- .get_freq(df[[iv]], w)
      shifted <- bn_freq_prob_shift(
        freq = freq, type = "exponential",
        lift = lift, impact_metric_type = impact_metric_type
      )
      as.numeric(shifted) / as.numeric(prior)
    })

    .eval_combo <- function(combo) {
      ev <- purrr::map(combo, ~ivs_likelihood[[.x]]) %>% stats::setNames(combo)
      updated <- suppressMessages(gRain::setEvidence(grain_bn, evidence = ev))
      dist <- suppressMessages(gRain::querygrain(updated, nodes = dv, simplify = TRUE))
      if (dv_metric == "top_box") {
        dist %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
      } else {
        sum(as.numeric(names(dist)) * as.numeric(dist))
      }
    }
  }

  # ===========================================================================
  # 5. Search
  # ===========================================================================
  if (search == "greedy") {
    # =========================================================================
    # Greedy forward selection
    # =========================================================================
    max_r <- if (!is.null(max_rounds)) min(max_rounds, length(ivs)) else length(ivs)

    # Round 1: seed from impact_result or compute individually
    if (!is.null(impact_result)) {
      impact_table <- if (is.data.frame(impact_result)) impact_result else impact_result[["table"]]
      idx_cols <- intersect(names(impact_table), c("index", "maxVmin", "dv_max_value"))
      if (length(idx_cols) == 0) {
        idx_cols <- grep("_index$|^Total$|^index$", names(impact_table), value = TRUE)
      }
      if (length(idx_cols) == 0) {
        if (verbose) cli::cli_alert_warning("Cannot find ranking column in impact_result, computing from scratch")
        impact_result <- NULL
      } else {
        round1_order <- impact_table %>%
          dplyr::arrange(dplyr::desc(.data[[idx_cols[1]]])) %>%
          dplyr::pull(Variable)
        round1_order <- intersect(round1_order, ivs)
      }
    }

    if (is.null(impact_result)) {
      if (verbose) cli::cli_alert_info("Round 1: Evaluating {length(ivs)} individual IVs")
      individual_estimates <- purrr::map_dbl(rlang::set_names(ivs), function(iv) {
        .eval_combo(iv)
      })
      round1_order <- names(sort(individual_estimates, decreasing = TRUE))
    }

    best_iv <- round1_order[1]
    best_estimate <- .eval_combo(best_iv)

    selected <- best_iv
    remaining <- setdiff(round1_order, best_iv)
    prev_estimate <- best_estimate

    results_list <- list(
      tibble::tibble(
        priority = 1L, variable = best_iv, combo = best_iv,
        dv_estimate = best_estimate,
        marginal_gain = best_estimate - baseline,
        marginal_gain_pct = (best_estimate - baseline) / baseline
      )
    )

    round <- 2L
    while (length(remaining) > 0 && round <= max_r) {
      if (verbose) cli::cli_alert_info("Round {round}: Testing {length(remaining)} candidates with {length(selected)} selected")

      combo_base <- selected

      if (use_parallel && length(remaining) > 1) {
        combo_estimates <- furrr::future_map_dbl(
          rlang::set_names(remaining),
          function(cand) .eval_combo(c(combo_base, cand)),
          .options = furrr::furrr_options(seed = TRUE)
        )
      } else {
        combo_estimates <- purrr::map_dbl(
          rlang::set_names(remaining),
          function(cand) .eval_combo(c(combo_base, cand))
        )
      }

      best_idx <- which.max(combo_estimates)
      best_iv <- names(combo_estimates)[best_idx]
      best_estimate <- combo_estimates[best_idx]

      marginal_gain <- best_estimate - prev_estimate
      marginal_gain_pct <- if (prev_estimate != 0) marginal_gain / prev_estimate else 0

      if (!is.null(threshold) && marginal_gain_pct < threshold) {
        if (verbose) cli::cli_alert_info("Stopping at round {round}: marginal gain {round(marginal_gain_pct * 100, 4)}% < threshold {threshold * 100}%")
        break
      }

      selected <- c(selected, best_iv)
      remaining <- setdiff(remaining, best_iv)
      prev_estimate <- best_estimate

      results_list[[round]] <- tibble::tibble(
        priority = as.integer(round),
        variable = best_iv,
        combo = paste(selected, collapse = ", "),
        dv_estimate = best_estimate,
        marginal_gain = marginal_gain,
        marginal_gain_pct = marginal_gain_pct
      )

      round <- round + 1L
    }

    result <- dplyr::bind_rows(results_list)

  } else {
    # =========================================================================
    # Exhaustive: best combo at each size k
    # =========================================================================
    max_k <- if (!is.null(max_rounds)) min(max_rounds, length(ivs)) else length(ivs)

    prev_combo <- character(0)
    prev_estimate <- baseline
    results_list <- list()

    for (k in seq_len(max_k)) {
      combos <- utils::combn(ivs, k, simplify = FALSE)
      if (verbose) cli::cli_alert_info("Size {k}: Evaluating {length(combos)} combinations")

      if (use_parallel && length(combos) > 1) {
        estimates <- furrr::future_map_dbl(combos, .eval_combo,
          .options = furrr::furrr_options(seed = TRUE))
      } else {
        estimates <- purrr::map_dbl(combos, .eval_combo)
      }

      best_idx <- which.max(estimates)
      best_combo <- combos[[best_idx]]
      best_estimate <- estimates[[best_idx]]

      new_vars <- setdiff(best_combo, prev_combo)

      marginal_gain <- best_estimate - prev_estimate
      marginal_gain_pct <- if (prev_estimate != 0) marginal_gain / prev_estimate else 0

      if (!is.null(threshold) && k > 1 && marginal_gain_pct < threshold) {
        if (verbose) cli::cli_alert_info("Stopping at size {k}: marginal gain {round(marginal_gain_pct * 100, 4)}% < threshold {threshold * 100}%")
        break
      }

      results_list[[k]] <- tibble::tibble(
        priority = as.integer(k),
        variable = paste(new_vars, collapse = ", "),
        combo = paste(sort(best_combo), collapse = ", "),
        dv_estimate = best_estimate,
        marginal_gain = marginal_gain,
        marginal_gain_pct = marginal_gain_pct
      )

      prev_combo <- best_combo
      prev_estimate <- best_estimate
    }

    result <- dplyr::bind_rows(results_list)
  }

  # ---------------------------------------------------------------------------
  # 6. Optional bootstrap p-values: noise-floor test
  # Replays the combos from the result on bootstrapped data. Compares each
  # step's marginal gain to the noise floor (average gain of the tail steps).
  # ---------------------------------------------------------------------------
  n_obs <- nrow(df)
  if (!is.null(n_boot_final) && n_boot_final > 1 && n_obs >= min_base_for_boot) {
    if (verbose) cli::cli_alert_info("Bootstrapping {n_boot_final} replicates for p-values (n = {n_obs})")

    n_steps <- nrow(result)
    bn_nodes <- bnlearn::nodes(bn)
    fit_df <- as.data.frame(df[, intersect(bn_nodes, names(df)), drop = FALSE])

    # Parse combos from result for replaying
    final_combos <- purrr::map(seq_len(n_steps), function(i) {
      strsplit(result$combo[i], ", ")[[1]]
    })

    weight_vec <- if (!is.null(weight)) df[[weight]] else NULL
    boot_gains <- matrix(NA_real_, nrow = n_boot_final, ncol = n_steps)

    if (verbose) cli::cli_progress_bar("Bootstrap", total = n_boot_final)

    for (b in seq_len(n_boot_final)) {
      if (verbose) cli::cli_progress_update()

      boot_idx <- sample(nrow(fit_df), replace = TRUE)
      boot_df <- fit_df[boot_idx, , drop = FALSE]
      boot_w <- if (!is.null(weight_vec)) weight_vec[boot_idx] else NULL

      # Suppress bnlearn "variable X has levels that are not observed in the
      # data" warnings from bn.fit(). They fire when a bootstrap resample
      # (with replacement) from a small slice happens to miss a rare factor
      # level — a sampling artifact, not a real issue. The Bayesian fit still
      # produces a valid grain object (unobserved levels get prior-driven
      # non-zero probabilities), and the bootstrap aggregate is unaffected.
      # suppressMessages is kept separately for gRain "chatty" init messages.
      boot_grain <- suppressWarnings(suppressMessages(
        bnlearn::bn.fit(bn, boot_df, method = "bayes") %>%
          bnlearn::as.grain()
      ))

      boot_baseline <- .query_dv(boot_grain, evidence = list())

      if (strategy == "max") {
        combo_estimates <- purrr::map_dbl(final_combos, function(combo) {
          ev <- ivs_max[combo]
          .query_dv(boot_grain, evidence = ev)
        })
      } else {
        all_ivs_in_combos <- unique(unlist(final_combos))
        boot_likelihoods <- purrr::map(rlang::set_names(all_ivs_in_combos), function(iv) {
          prior <- suppressMessages(gRain::querygrain(boot_grain, nodes = iv, simplify = TRUE))
          freq <- .get_freq(boot_df[[iv]], boot_w)
          shifted <- bn_freq_prob_shift(
            freq = freq, type = "exponential",
            lift = lift, impact_metric_type = impact_metric_type
          )
          as.numeric(shifted) / as.numeric(prior)
        })

        combo_estimates <- purrr::map_dbl(final_combos, function(combo) {
          ev <- purrr::map(combo, ~boot_likelihoods[[.x]]) %>% stats::setNames(combo)
          updated <- suppressMessages(gRain::setEvidence(boot_grain, evidence = ev))
          dist <- suppressMessages(gRain::querygrain(updated, nodes = dv, simplify = TRUE))
          if (dv_metric == "top_box") {
            dist %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
          } else {
            sum(as.numeric(names(dist)) * as.numeric(dist))
          }
        })
      }

      # Marginal gains: baseline → step 1 → step 2 → ...
      cumulative <- c(boot_baseline, combo_estimates)
      boot_gains[b, ] <- diff(cumulative)
    }

    if (verbose) cli::cli_progress_done()

    # Noise floor: average gain of the tail Q steps per bootstrap
    if (n_steps <= 4) {
      n_tail <- 1
    } else {
      n_tail <- max(3, floor(n_steps * noise_tail))
    }
    tail_cols <- seq(n_steps - n_tail + 1, n_steps)

    result$p_value <- purrr::map_dbl(seq_len(n_steps), function(k) {
      gains_k <- boot_gains[, k]
      noise_k <- rowMeans(boot_gains[, tail_cols, drop = FALSE])
      valid <- which(!is.na(gains_k) & !is.na(noise_k))
      if (length(valid) < 2) return(NA_real_)
      # Proportion of bootstraps where step k's gain <= its noise floor
      round(mean(gains_k[valid] <= noise_k[valid]), 5)
    })
  } else if (!is.null(n_boot_final) && n_boot_final > 1 && n_obs < min_base_for_boot) {
    if (verbose) cli::cli_alert_warning("Skipping bootstrap: base too small (n = {n_obs}, minimum = {min_base_for_boot})")
  }

  # ---------------------------------------------------------------------------
  # 7. Prepend baseline and add labels
  # ---------------------------------------------------------------------------
  result <- dplyr::bind_rows(baseline_row, result)

  if (!is.null(dictionary)) {
    dict_df <- work::dictionary_from_named_object(dictionary)
    var_col <- intersect(names(dict_df), c("var", "variable"))[1]
    label_lookup <- rlang::set_names(dict_df$label, dict_df[[var_col]])
    result$label <- purrr::map_chr(result$variable, function(v) {
      if (v == "Baseline") return("Estimate with no shifts")
      if (v %in% names(label_lookup)) label_lookup[[v]] else v
    })
  } else {
    result$label <- purrr::map_chr(result$variable, function(v) {
      if (v == "Baseline") return("Estimate with no shifts")
      v
    })
  }

  # Reorder columns
  col_order <- c("priority", "variable", "label", "combo", "dv_estimate",
                 "marginal_gain", "marginal_gain_pct")
  if ("p_value" %in% names(result)) {
    col_order <- c(col_order, "p_value")
  }
  result <- result[, col_order]

  if (verbose) cli::cli_alert_success("Prioritization complete: {nrow(result) - 1} steps (n = {n_obs})")

  attr(result, "n_obs") <- n_obs
  result
}
