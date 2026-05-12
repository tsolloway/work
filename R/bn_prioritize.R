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
#'   Interpretation depends on \code{impact_shift_type}. Default 0.10.
#' @param min_base_for_boot Integer. Minimum sample size required to run
#'   bootstrap p-values. When \code{n_obs < min_base_for_boot}, p-values are
#'   skipped. Default 75.
#' @param dv_metric Character. \code{"mean"} (default) computes the expected DV
#'   value; \code{"top_box"} uses the probability of the highest DV level.
#' @param impact_shift_type Character. How \code{lift} is interpreted when
#'   shifting IV distributions: \code{"headroom"} (default) shifts by
#'   \code{lift} times the available room in the requested direction
#'   (\code{(max - mean)} for positive lift, \code{(mean - min)} for
#'   negative) — every IV closes the same fraction of its own gap to
#'   the boundary, so cross-scale rankings are comparable;
#'   \code{"proportional"} shifts by a fraction of the current mean;
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
#' @param subtract_baseline Logical. When \code{TRUE} (default) cumulative
#'   and incremental gains are computed as \code{dv_estimate - baseline}
#'   (where \code{baseline} is the no-evidence DV estimate). When
#'   \code{FALSE} the baseline is treated as 0 — cumulative_gain becomes
#'   the raw \code{dv_estimate} and the percent columns become \code{NA}.
#'   Used by \code{bn_prioritizations(include_maximum_lift_deprecated = TRUE)}
#'   for backward compatibility with the older output format.
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
    impact_shift_type = c("headroom", "proportional", "absolute", "range"),
    impact_result = NULL,
    threshold = 0.01,
    max_rounds = NULL,
    noise_tail = 1/3,
    n_boot_final = 100,
    weight = NULL,
    use_parallel = TRUE,
    verbose = TRUE,
    subtract_baseline = TRUE,
    scale_ranges = NULL,
    seed = 1
) {

  strategy <- match.arg(strategy)
  search <- match.arg(search)
  dv_metric <- match.arg(dv_metric)
  impact_shift_type <- match.arg(impact_shift_type)

  # Preserve named dv for display, strip for bnlearn
  dv <- unname(dv)

  set.seed(seed)

  # gRain uses cat() not message() — sink stdout to suppress
  sink(nullfile())
  on.exit(sink(), add = TRUE)

  # Flatten ivs if passed as a list (e.g., community groups)
  if (is.list(ivs)) ivs <- unlist(ivs) %>% setNames(NULL)

  # Weighted frequency helper — pads absent levels with 0 so the result aligns
  # with the BN's factor levels (otherwise length(freq) != length(prior) and
  # the downstream array()/setEvidence calls fail).
  .get_freq <- function(x, w = NULL) {
    lv <- if (is.factor(x)) levels(x) else sort(unique(stats::na.omit(x)))
    xf <- factor(x, levels = lv)
    if (is.null(w)) return(table(xf))
    out <- tapply(w, xf, sum)
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

  # Baseline DV estimate (no evidence). When subtract_baseline = FALSE we
  # treat baseline as 0 in all gain calculations — the deprecated behaviour
  # where cumulative_gain == dv_estimate (no comparison against the
  # no-evidence DV). The TRUE baseline is still computed for the baseline
  # row's dv_estimate field.
  baseline_true <- .query_dv(grain_bn, evidence = list())
  baseline <- if (isTRUE(subtract_baseline)) baseline_true else 0
  if (verbose) cli::cli_alert_info("Baseline DV estimate: {round(baseline_true, 4)}{if (!isTRUE(subtract_baseline)) ' (no-baseline mode: gains computed against 0)' else ''}")

  baseline_row <- tibble::tibble(
    priority = 0L,
    variable = "Baseline",
    combo = NA_character_,
    dv_estimate = baseline_true,
    cumulative_gain = 0,
    cumulative_gain_pct = 0,
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
        lift = lift, impact_shift_type = impact_shift_type,
        scale_range = scale_ranges[[iv]]
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
  # Round 1 individual ordering — set inside the greedy branch and read
  # later by the bootstrap noise-tail logic. NULL when the path didn't
  # populate it (e.g., exhaustive search), in which case the bootstrap
  # falls back to the legacy "tail of survivors" noise definition.
  round1_order <- NULL
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

    # In subtract_baseline = FALSE mode, baseline is forced to 0 above —
    # cumulative_gain becomes the raw dv_estimate, and the percent columns
    # are NA because a baseline-relative percentage is undefined when the
    # baseline is zero.
    cum_pct_first <- if (isTRUE(subtract_baseline)) (best_estimate - baseline) / baseline else NA_real_
    marg_pct_first <- cum_pct_first

    results_list <- list(
      tibble::tibble(
        priority = 1L, variable = best_iv, combo = best_iv,
        dv_estimate = best_estimate,
        cumulative_gain = best_estimate - baseline,
        cumulative_gain_pct = cum_pct_first,
        marginal_gain = best_estimate - baseline,
        marginal_gain_pct = marg_pct_first
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

      cum_pct <- if (isTRUE(subtract_baseline) && baseline != 0) {
        (best_estimate - baseline) / baseline
      } else {
        NA_real_
      }
      results_list[[round]] <- tibble::tibble(
        priority = as.integer(round),
        variable = best_iv,
        combo = paste(selected, collapse = ", "),
        dv_estimate = best_estimate,
        cumulative_gain = best_estimate - baseline,
        cumulative_gain_pct = cum_pct,
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

      cum_pct_k <- if (isTRUE(subtract_baseline) && baseline != 0) {
        (best_estimate - baseline) / baseline
      } else {
        NA_real_
      }
      results_list[[k]] <- tibble::tibble(
        priority = as.integer(k),
        variable = paste(new_vars, collapse = ", "),
        combo = paste(sort(best_combo), collapse = ", "),
        dv_estimate = best_estimate,
        cumulative_gain = best_estimate - baseline,
        cumulative_gain_pct = cum_pct_k,
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

    # ---- Noise tail composition ---------------------------------------------
    # Tail is built from sub-threshold (rejected) IVs first, ordered by their
    # Round 1 individual effect (strongest first — IVs that almost made the
    # cut). Quota is `max(3, floor(n_total * noise_tail))`, computed against
    # the FULL IV count so the noise estimate is anchored to the dataset
    # rather than the surviving step count. If rejected < quota, the
    # remainder is topped up with the bottom-by-step-number survivors
    # (preserves the legacy "weakest survivors" fallback). For exhaustive
    # search there's no Round 1 ordering — fall back to the old tail-based
    # noise definition.
    survivor_ivs <- unique(unlist(final_combos))
    n_total <- length(ivs)
    # Quota anchored to total IV count (Tyler\'s spec), not step count.
    # With small datasets (n_total <= 4) drop the floor of 3 so we don\'t
    # try to use more noise samples than IVs exist.
    noise_quota <- if (n_total <= 4) {
      max(1, floor(n_total * noise_tail))
    } else {
      max(3, floor(n_total * noise_tail))
    }
    noise_quota <- min(noise_quota, n_total)
    if (!is.null(round1_order)) {
      rejected_ranked  <- setdiff(round1_order, survivor_ivs)
      take_rej         <- min(noise_quota, length(rejected_ranked))
      noise_rejected   <- if (take_rej > 0) rejected_ranked[seq_len(take_rej)] else character(0)
      topup_n          <- max(0L, noise_quota - length(noise_rejected))
      topup_n          <- min(topup_n, n_steps)
      noise_topup_steps <- if (topup_n > 0) seq.int(n_steps - topup_n + 1, n_steps) else integer(0)
    } else {
      noise_rejected    <- character(0)
      topup_n           <- min(noise_quota, n_steps)
      noise_topup_steps <- if (topup_n > 0) seq.int(n_steps - topup_n + 1, n_steps) else integer(0)
    }
    n_noise_rej <- length(noise_rejected)
    noise_extra <- if (n_noise_rej > 0) {
      matrix(NA_real_, nrow = n_boot_final, ncol = n_noise_rej,
             dimnames = list(NULL, noise_rejected))
    } else {
      NULL
    }

    if (verbose) cli::cli_progress_bar("Bootstrap", total = n_boot_final)

    # ---- Lift-strategy precomputation (fixed-grain bootstrap) ---------------
    # The lift-strategy bootstrap holds the ORIGINAL full-sample grain fixed
    # across iterations. The only quantity that varies per bootstrap is the
    # IV frequency distribution (from resampling df's rows). This matches
    # what the non-bootstrap original run actually does: it applies brand
    # (or focus) IV frequencies as virtual evidence against a fixed BN.
    #
    # Priors for likelihood ratios come from the ORIGINAL grain, so they are
    # constant across bootstraps (precompute once, out of the loop).
    #
    # The previous implementation refit the BN on each bootstrap-of-df —
    # which for brand-focus prioritization (where df is brand-filtered) was
    # fitting a DIFFERENT BN than the original run used, producing chaotic
    # non-monotone p-values. See
    # https://github.com/... (bn_prioritize p-value fix) for history.
    if (strategy != "max") {
      # Include rejected noise IVs alongside the survivor IVs so their
      # priors are precomputed too (each will be evaluated as
      # `survivors_combo + iv_j` in the bootstrap loop).
      all_ivs_in_combos <- unique(c(unlist(final_combos), noise_rejected))
      # Original-grain prior for each IV — fixed across bootstraps.
      orig_priors <- purrr::map(rlang::set_names(all_ivs_in_combos), function(iv) {
        as.numeric(
          suppressMessages(
            gRain::querygrain(grain_bn, nodes = iv, simplify = TRUE)
          )
        )
      })
      # Original baseline — fixed across bootstraps.
      orig_baseline <- baseline
    }

    for (b in seq_len(n_boot_final)) {
      if (verbose) cli::cli_progress_update()

      boot_idx <- sample(nrow(fit_df), replace = TRUE)
      boot_df <- fit_df[boot_idx, , drop = FALSE]
      boot_w <- if (!is.null(weight_vec)) weight_vec[boot_idx] else NULL

      # For computing rejected-IV noise gains (added on top of full
      # survivor combo) — closures created here so both strategy branches
      # can fill `noise_extra` consistently below.
      eval_noise_after_survivors <- NULL

      if (strategy == "max") {
        # Max-strategy bootstrap still refits on the resampled df. At the
        # subgroup level this is the full subgroup data (same population the
        # grain was fit on), so a refit samples the legitimate fit
        # uncertainty. Unlike the lift case, hard-evidence combos against a
        # fixed grain would produce identical results across bootstraps and
        # degenerate p-values — so refit is the correct design for max.
        # Suppress bnlearn's "variable X has levels not observed" warnings;
        # Bayesian smoothing keeps probabilities finite when rare levels
        # drop out of a resample, so the aggregate is unaffected.
        boot_grain <- suppressWarnings(suppressMessages(
          bnlearn::bn.fit(bn, boot_df, method = "bayes") %>%
            bnlearn::as.grain()
        ))
        boot_baseline <- if (isTRUE(subtract_baseline)) {
          .query_dv(boot_grain, evidence = list())
        } else {
          0
        }
        combo_estimates <- purrr::map_dbl(final_combos, function(combo) {
          ev <- ivs_max[combo]
          .query_dv(boot_grain, evidence = ev)
        })
        # Noise IVs evaluated against the full survivor combo with hard
        # evidence at each rejected IV's max level.
        survivor_full <- final_combos[[length(final_combos)]]
        eval_noise_after_survivors <- function(iv_j) {
          ev <- ivs_max[c(survivor_full, iv_j)]
          .query_dv(boot_grain, evidence = ev)
        }
      } else {
        # Lift strategy: bootstrap only the IV frequency distribution. Use
        # the ORIGINAL grain (and its priors) — don't refit. This isolates
        # the only empirical thing the focus actually contributes (the
        # brand's / subgroup's row-level IV distribution) and produces p-
        # values that reflect uncertainty in that distribution, not BN-
        # refit chaos.
        boot_likelihoods <- purrr::map(rlang::set_names(all_ivs_in_combos), function(iv) {
          freq <- .get_freq(boot_df[[iv]], boot_w)
          shifted <- bn_freq_prob_shift(
            freq = freq, type = "exponential",
            lift = lift, impact_shift_type = impact_shift_type,
            scale_range = scale_ranges[[iv]]
          )
          as.numeric(shifted) / orig_priors[[iv]]
        })

        boot_baseline <- orig_baseline
        combo_estimates <- purrr::map_dbl(final_combos, function(combo) {
          ev <- purrr::map(combo, ~boot_likelihoods[[.x]]) %>% stats::setNames(combo)
          updated <- suppressMessages(gRain::setEvidence(grain_bn, evidence = ev))
          dist <- suppressMessages(gRain::querygrain(updated, nodes = dv, simplify = TRUE))
          if (dv_metric == "top_box") {
            dist %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
          } else {
            sum(as.numeric(names(dist)) * as.numeric(dist))
          }
        })
        survivor_full <- final_combos[[length(final_combos)]]
        eval_noise_after_survivors <- function(iv_j) {
          combo_full <- c(survivor_full, iv_j)
          ev <- purrr::map(combo_full, ~boot_likelihoods[[.x]]) %>%
            stats::setNames(combo_full)
          updated <- suppressMessages(gRain::setEvidence(grain_bn, evidence = ev))
          dist <- suppressMessages(gRain::querygrain(updated, nodes = dv, simplify = TRUE))
          if (dv_metric == "top_box") {
            dist %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
          } else {
            sum(as.numeric(names(dist)) * as.numeric(dist))
          }
        }
      }

      # Marginal gains: baseline → step 1 → step 2 → ...
      cumulative <- c(boot_baseline, combo_estimates)
      boot_gains[b, ] <- diff(cumulative)

      # Per-iteration noise gains for each rejected IV in the noise tail:
      # (survivors_combo + iv_j) − survivors_combo. Reuses the survivor
      # combo's already-computed estimate as the baseline.
      if (n_noise_rej > 0 && !is.null(eval_noise_after_survivors)) {
        survivor_full_estimate <- combo_estimates[length(combo_estimates)]
        for (j in seq_len(n_noise_rej)) {
          iv_j <- noise_rejected[j]
          full_est <- eval_noise_after_survivors(iv_j)
          noise_extra[b, j] <- full_est - survivor_full_estimate
        }
      }
    }

    if (verbose) cli::cli_progress_done()

    # Noise floor — built from rejected (sub-threshold) IVs first, then
    # topped up with the bottom-by-step survivors when fewer rejected
    # IVs exist than the quota. Per-iteration mean is computed by
    # combining the rejected-IV gains (`noise_extra`) with the topped-up
    # survivor step gains (`boot_gains[, noise_topup_steps]`).
    has_rejected_noise <- !is.null(noise_extra) && ncol(noise_extra) > 0
    has_topup_noise    <- length(noise_topup_steps) > 0

    noise_per_iter <- if (has_rejected_noise && has_topup_noise) {
      combined <- cbind(noise_extra, boot_gains[, noise_topup_steps, drop = FALSE])
      rowMeans(combined, na.rm = TRUE)
    } else if (has_rejected_noise) {
      rowMeans(noise_extra, na.rm = TRUE)
    } else if (has_topup_noise) {
      rowMeans(boot_gains[, noise_topup_steps, drop = FALSE], na.rm = TRUE)
    } else {
      # Defensive fallback — shouldn\'t normally hit this path
      rep(NA_real_, n_boot_final)
    }

    result$p_value <- purrr::map_dbl(seq_len(n_steps), function(k) {
      gains_k <- boot_gains[, k]
      valid <- which(!is.na(gains_k) & !is.na(noise_per_iter))
      if (length(valid) < 2) return(NA_real_)
      # Proportion of bootstraps where step k's gain <= its noise floor
      round(mean(gains_k[valid] <= noise_per_iter[valid]), 5)
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

  # Reorder columns. Absolute values (dv_estimate, cumulative_gain,
  # marginal_gain) come first, then percent versions (cumulative_gain_pct,
  # marginal_gain_pct). p_value, if present, goes last.
  col_order <- c("priority", "variable", "label", "combo", "dv_estimate",
                 "cumulative_gain", "marginal_gain",
                 "cumulative_gain_pct", "marginal_gain_pct")
  if ("p_value" %in% names(result)) {
    col_order <- c(col_order, "p_value")
  }
  result <- result[, col_order]

  if (verbose) cli::cli_alert_success("Prioritization complete: {nrow(result) - 1} steps (n = {n_obs})")

  attr(result, "n_obs") <- n_obs
  result
}
