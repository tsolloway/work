#' append_bn_simulator
#'
#' @description Appends a simple Bayesian network simulator sheet to an existing
#'   workbook. Pre-computes conditional probability distributions for every
#'   variable x level combination using exact gRain inference, then builds an
#'   interactive Excel dashboard with dropdowns for variable and level selection.
#'
#' @param wb An openxlsx workbook object.
#' @param obj A named list of BN subgroup objects (each with \code{fit} and
#'   \code{meta} elements), as produced by \code{bn_finalize_network()}.
#' @param df Data frame used to fit the network (needed for variable levels).
#' @param dv Character. Dependent variable name.
#' @param subgroups Character vector of subgroup names. If NULL, treats obj as
#'   a single model.
#' @param dictionary Optional. A data frame or named object for variable labels.
#' @param min_base_for_sim Integer. Minimum number of rows required in a
#'   \code{brand x subgroup} slice for the simulator to compute frequency-shift
#'   outputs for that focus. Slices below this threshold are written to the
#'   dashboard with NA values and surfaced on a \code{Simulator Base Warnings}
#'   sheet listing each offending slice's actual n. Default 75. The Market
#'   focus is never skipped.
#' @param sim_dv_only Logical. When TRUE, the simulator only provides results
#'   for the dependent variable as the target. Dramatically shrinks stored
#'   data by removing all non-DV target columns and rows. Default FALSE.
#' @param weight Character or NULL. Name of a survey-weight column in
#'   \code{df}. When provided, the per-IV level frequencies that feed
#'   \code{bn_freq_prob_shift()} are computed as sums of \code{weight}
#'   (via \code{stats::xtabs()}) rather than raw counts — so the
#'   "Mean" display and the freq-shifted EVs reflect the weighted
#'   distribution. NULL (default) keeps the original unweighted behaviour.
#'
#' @return The modified workbook object (invisibly).
#'
#' @keywords internal
append_bn_simulator <- function(
    wb,
    obj,
    df,
    dv,
    subgroups = NULL,
    dictionary = NULL,
    community_nodes = NULL,
    brand = NULL,
    brand_names = NULL,
    add_freq_shifts = FALSE,
    shift_range = c(-0.25, 0.25),
    shift_step = 0.05,
    min_base_for_sim = 75,
    sim_dv_only = FALSE,
    weight = NULL
) {

  # Track ALL (subgroup, focus) slices and their observed base size. The
  # Focus-adjacent cell in the dashboard uses this table to either (a)
  # display "Base: N" when n >= min_base_for_sim, or (b) show the red
  # "Results not calculated because base is below N" warning when n < threshold.
  # Structure: list of tibbles with subgroup, focus, n_obs, min_required.
  base_slices <- list()

  # ---------------------------------------------------------------------------
  # 1. Pre-compute posteriors for every (subgroup, evidence_var, evidence_level)
  # ---------------------------------------------------------------------------

  if (!is.null(subgroups)) {
    sg_list <- rlang::set_names(subgroups)
  } else {
    sg_list <- rlang::set_names("Total")
    obj <- list(Total = obj)
  }

  all_posteriors <- list()

  # Helper: extract probabilities from querygrain result (simplify = FALSE)
  .extract_probs <- function(result) {
    purrr::map(result, function(x) {
      p <- as.numeric(x)
      names(p) <- dimnames(x)[[1]]
      p
    })
  }

  for (sg in names(sg_list)) {

    fit <- obj[[sg]][["fit"]]
    meta <- obj[[sg]][["meta"]]
    ivs <- meta[["ivs"]]

    # Compile grain object
    grain_bn <- bnlearn::as.grain(fit) %>% gRain:::compile.grain()

    # Get all nodes and their levels from the fitted object
    all_nodes <- names(fit)
    node_levels <- purrr::map(rlang::set_names(all_nodes), function(nd) {
      dimnames(fit[[nd]][["prob"]])[[1]]
    })

    # Max number of levels across all nodes (for consistent column count)
    max_levels <- max(purrr::map_int(node_levels, length))

    # Targets queried from the grain. When sim_dv_only, restrict to the DV
    # only; this dramatically shrinks sim_data and pct_data. Evidence still
    # uses all_nodes (IVs are unchanged).
    target_nodes <- if (isTRUE(sim_dv_only)) dv else all_nodes
    n_targets <- length(target_nodes)

    # Global P_ column set for this subgroup = union of every target node's
    # level names. We preallocate a matrix this wide and fill per-row by
    # name-matching into column indices — avoids the old per-row tibble
    # construction that created thousands of tiny tibbles per subgroup.
    all_level_names <- unique(unlist(node_levels[target_nodes], use.names = FALSE))
    all_p_cols <- paste0("P_", all_level_names)

    total_combos <- 1L + sum(purrr::map_int(ivs, ~ length(node_levels[[.x]])))
    total_rows <- total_combos * n_targets

    ev_var_vec     <- character(total_rows)
    ev_lev_vec     <- character(total_rows)
    target_var_vec <- character(total_rows)
    prob_mat <- matrix(NA_real_, nrow = total_rows, ncol = length(all_p_cols),
                       dimnames = list(NULL, all_p_cols))

    # Fill a block of n_targets rows starting at `start_idx` from a posterior
    # list (named list: target_name -> named-numeric-vector of probabilities).
    # Modifies the enclosing-frame vectors/matrix by name — scoped to the
    # subgroup loop, so the side-effect is bounded.
    fill_block <- function(start_idx, posterior, ev_var, ev_lev) {
      for (ni in seq_len(n_targets)) {
        nm <- target_nodes[ni]
        probs <- posterior[[nm]]
        if (is.null(probs)) next
        idx <- start_idx + ni - 1L
        ev_var_vec[idx]     <<- ev_var
        ev_lev_vec[idx]     <<- ev_lev
        target_var_vec[idx] <<- nm
        col_idx <- match(paste0("P_", names(probs)), all_p_cols)
        prob_mat[idx, col_idx] <<- probs
      }
    }

    # Prior (no evidence)
    marginal <- gRain::querygrain(grain_bn, nodes = target_nodes, simplify = FALSE) %>%
      .extract_probs()
    fill_block(1L, marginal, "(None)", "(Prior)")

    # Each IV x each level
    cursor <- n_targets + 1L
    for (iv in ivs) {
      iv_levels <- node_levels[[iv]]
      for (lv in iv_levels) {
        evidence <- rlang::set_names(list(lv), iv)
        posterior <- tryCatch(
          gRain::querygrain(grain_bn, nodes = target_nodes,
            evidence = evidence, simplify = FALSE) %>% .extract_probs(),
          error = function(e) NULL
        )
        if (!is.null(posterior)) {
          # Force evidence variable to point mass at selected level. Only
          # add it to the output when it's among the target nodes — with
          # sim_dv_only, targets are just the DV so the IV stays out.
          if (iv %in% target_nodes) {
            ev_probs <- rep(0, length(node_levels[[iv]]))
            names(ev_probs) <- node_levels[[iv]]
            ev_probs[lv] <- 1
            posterior[[iv]] <- ev_probs
          }
          fill_block(cursor, posterior, iv, lv)
        }
        cursor <- cursor + n_targets
      }
    }

    # Collapse to rows actually filled (failed tryCatch calls leave empty
    # target_var_vec entries).
    filled <- nzchar(target_var_vec)
    sg_tbl <- tibble::tibble(
      Subgroup          = rep(sg, sum(filled)),
      Evidence_Variable = ev_var_vec[filled],
      Evidence_Level    = ev_lev_vec[filled],
      Target_Variable   = target_var_vec[filled]
    ) %>% dplyr::bind_cols(
      tibble::as_tibble(prob_mat[filled, , drop = FALSE])
    )

    all_posteriors[[sg]] <- sg_tbl
  }

  sim_data <- dplyr::bind_rows(all_posteriors)

  # Identify all P_ columns and pad missing ones with NA
  p_cols <- grep("^P_", names(sim_data), value = TRUE)

  # Add Expected_Value: SUMPRODUCT(level_values, probabilities)
  nd_levels_numeric <- as.numeric(gsub("^P_", "", p_cols))
  sim_data$Expected_Value <- as.numeric(
    as.matrix(sim_data[, p_cols]) %*% nd_levels_numeric
  )

  # ---------------------------------------------------------------------------
  # 1a. Factored storage: pivot long → wide + prior
  # ---------------------------------------------------------------------------
  # Instead of a long "{sg}|{ev_var}|{ev_level}|{target}" table with per-row
  # Expected_Value, we now emit:
  #   * sim_data_wide:  Key = "{sg}|{iv}|{target}", cols EV_L1..EV_L{max}
  #   * sim_data_prior: Key = "{sg}|{target}",      col Prior_EV
  # This halves the row count of the evidence table (one row per iv x target
  # instead of per level x iv x target) and lets Mean mode use SUMPRODUCT
  # against a shared shifted-probs sheet (see Phase 2) — factoring out the
  # focus/pct/weight combos that previously exploded per target.

  # Per-subgroup node level lookup (for level-position resolution)
  nl_by_sg <- purrr::map(rlang::set_names(names(sg_list)), function(sg) {
    fit_sg <- obj[[sg]][["fit"]]
    purrr::map(rlang::set_names(names(fit_sg)), function(nd) {
      dimnames(fit_sg[[nd]][["prob"]])[[1]]
    })
  })

  # Global max IV levels (column count for both sim_data_wide EV_L* and
  # Phase 2 shifted_probs P_L* — must align so Mean-mode SUMPRODUCT works).
  max_iv_levels_all <- max(purrr::map_int(names(sg_list), function(sg) {
    ivs_sg <- obj[[sg]][["meta"]][["ivs"]]
    max(purrr::map_int(ivs_sg, function(iv) length(nl_by_sg[[sg]][[iv]])))
  }))
  ev_col_names <- paste0("EV_L", seq_len(max_iv_levels_all))

  # Split prior vs evidence rows
  prior_rows <- sim_data$Evidence_Variable == "(None)"

  sim_data_prior <- tibble::tibble(
    Key      = paste(sim_data$Subgroup[prior_rows],
                     sim_data$Target_Variable[prior_rows], sep = "|"),
    Prior_EV = round(sim_data$Expected_Value[prior_rows], 4)
  )

  # Resolve level position for every evidence row: which index in the IV's
  # level vector does Evidence_Level correspond to?
  ev_sg  <- sim_data$Subgroup[!prior_rows]
  ev_iv  <- sim_data$Evidence_Variable[!prior_rows]
  ev_lv  <- sim_data$Evidence_Level[!prior_rows]
  ev_tg  <- sim_data$Target_Variable[!prior_rows]
  ev_val <- round(sim_data$Expected_Value[!prior_rows], 4)

  level_pos <- integer(length(ev_sg))
  for (i in seq_along(ev_sg)) {
    level_pos[i] <- match(ev_lv[i], nl_by_sg[[ ev_sg[i] ]][[ ev_iv[i] ]])
  }

  # Pivot to wide: one row per (sg, iv, target), columns = EV_L{position}
  # Build composite key and a keyed matrix fill. Faster than tidyr for this
  # small pivot (and avoids adding tidyr to package deps).
  wide_keys <- paste(ev_sg, ev_iv, ev_tg, sep = "|")
  unique_wide_keys <- unique(wide_keys)
  n_wide <- length(unique_wide_keys)

  ev_mat <- matrix(0, nrow = n_wide, ncol = max_iv_levels_all,
                   dimnames = list(NULL, ev_col_names))
  row_idx <- match(wide_keys, unique_wide_keys)
  cell_idx <- cbind(row_idx, level_pos)
  ev_mat[cell_idx] <- ev_val

  sim_data_wide <- tibble::tibble(Key = unique_wide_keys) %>%
    dplyr::bind_cols(tibble::as_tibble(ev_mat))

  # ---------------------------------------------------------------------------
  # 1b. Pre-compute percent change (freq shift) expected values
  # ---------------------------------------------------------------------------
  pct_steps <- seq(shift_range[1], shift_range[2], by = shift_step)
  pct_labels <- ifelse(pct_steps > 0,
    paste0("+", round(pct_steps * 100, 1), " percent"),
    paste0(round(pct_steps * 100, 1), " percent")
  )
  has_freq_shifts <- add_freq_shifts && !is.null(df)

  # Whether to expose a Weight (Weighted/Unweighted) dropdown on the dashboard.
  # We only precompute the weighted variant of pct_data when this is TRUE.
  has_weight <- !is.null(weight) && !is.null(df) && (weight %in% names(df))

  if (has_freq_shifts) {

    # Focus options: "Market" + brand names
    focus_options <- "Market"
    if (!is.null(brand) && !is.null(brand_names)) {
      focus_options <- c(focus_options, brand_names)
    }

    # Process each subgroup × focus
    pct_data_list <- purrr::map(names(sg_list), function(sg) {

      fit <- obj[[sg]][["fit"]]
      meta_sg <- obj[[sg]][["meta"]]
      ivs <- meta_sg[["ivs"]]

      # Filter df to subgroup
      sg_df <- if (length(sg_list) > 1) df[df[[sg]] == 1, , drop = FALSE] else df

      node_levels_sg <- nl_by_sg[[sg]]

      # Factored storage (Option 1): emit SHIFTED PROBABILITIES directly.
      # The dashboard computes EV at render time via SUMPRODUCT of the
      # shifted_probs row and the matching row of _sim_data_wide (per-level
      # posterior EVs from Phase 1). This cuts the stored sheet by ~5-10x
      # vs pre-multiplying every (target, pct, focus, weight) combination.
      #
      # Output shape: one row per (iv, pct_label), columns P_L1..P_L{max}
      # aligned globally to max_iv_levels_all. Shorter IVs pad trailing
      # P_L* columns with 0 — SUMPRODUCT contributions are 0 there.
      p_col_names_sg <- paste0("P_L", seq_len(max_iv_levels_all))

      .compute_focus <- function(focus_df, focus_name, use_weight) {
        per_iv <- purrr::map(ivs, function(iv) {
          iv_vals <- focus_df[[iv]]
          if (isTRUE(use_weight) && !is.null(weight) &&
              weight %in% names(focus_df)) {
            w <- focus_df[[weight]]
            ok <- !is.na(iv_vals) & !is.na(w)
            iv_freq <- tapply(w[ok], iv_vals[ok], sum)
          } else {
            iv_freq <- table(iv_vals)
          }
          iv_levels <- node_levels_sg[[iv]]
          n_iv_levels <- length(iv_levels)

          shifted_mat <- matrix(NA_real_, nrow = length(pct_steps),
            ncol = n_iv_levels)
          for (pi in seq_along(pct_steps)) {
            sp <- tryCatch(
              as.numeric(bn_freq_prob_shift(freq = iv_freq, lift = pct_steps[pi],
                impact_shift_type = "proportional")),
              error = function(e) NULL
            )
            if (!is.null(sp) && !anyNA(sp)) shifted_mat[pi, ] <- sp
          }

          good <- !is.na(shifted_mat[, 1])
          if (!any(good)) return(NULL)
          shifted_mat <- shifted_mat[good, , drop = FALSE]
          good_labels <- pct_labels[good]

          # Pad to global max_iv_levels_all columns (0 for levels beyond
          # the IV's own range) so every row is rectangular and aligned
          # across subgroups AND with sim_data_wide's EV_L* columns.
          padded <- matrix(0, nrow = nrow(shifted_mat), ncol = max_iv_levels_all)
          padded[, seq_len(n_iv_levels)] <- shifted_mat
          colnames(padded) <- p_col_names_sg

          # Precompute the weighted mean of the IV's shifted distribution.
          # (Used by the dashboard's "mean from X to Y" display — avoids
          # SUMPRODUCT+TRANSPOSE contortions in Excel.)
          iv_level_num <- suppressWarnings(as.numeric(iv_levels))
          mean_vec <- as.numeric(shifted_mat %*% iv_level_num)

          tibble::tibble(
            Subgroup    = sg,
            IV          = iv,
            pct_label   = good_labels,
            Focus       = focus_name,
            n_iv_levels = n_iv_levels,
            Mean        = mean_vec
          ) %>%
            dplyr::bind_cols(tibble::as_tibble(padded))
        })

        dplyr::bind_rows(purrr::compact(per_iv))
      }

      # When a weight column was provided we precompute BOTH unweighted and
      # weighted shifted-probs tables. Stack instead of join (focus is now
      # a row dimension, not a column dimension).
      weight_modes <- if (has_weight) c("Unweighted", "Weighted") else "Unweighted"

      per_wmode <- purrr::map(weight_modes, function(wm) {
        use_weight_now <- (wm == "Weighted")

        # Market always runs (no base-size restriction).
        sg_rows <- .compute_focus(sg_df, "Market", use_weight = use_weight_now)
        if (wm == "Unweighted") {
          base_slices[[length(base_slices) + 1L]] <<- tibble::tibble(
            subgroup = sg, focus = "Market",
            n_obs = nrow(sg_df), min_required = min_base_for_sim
          )
        }

        # Each brand focus. Below-min_base_for_sim focuses still get rows
        # (NA-filled) so the dropdown resolves; the Focus warning fires.
        for (focus in setdiff(focus_options, "Market")) {
          focus_df <- sg_df[sg_df[[brand]] == focus, , drop = FALSE]
          if (wm == "Unweighted") {
            base_slices[[length(base_slices) + 1L]] <<- tibble::tibble(
              subgroup = sg, focus = focus,
              n_obs = nrow(focus_df), min_required = min_base_for_sim
            )
          }

          if (nrow(focus_df) < min_base_for_sim) {
            # NA-fill one row per IV per pct to keep the dropdown shape.
            na_rows <- purrr::map(ivs, function(iv) {
              iv_levels <- node_levels_sg[[iv]]
              tibble::tibble(
                Subgroup    = sg,
                IV          = iv,
                pct_label   = pct_labels,
                Focus       = focus,
                n_iv_levels = length(iv_levels),
                Mean        = NA_real_
              ) %>%
                dplyr::bind_cols(
                  matrix(NA_real_, nrow = length(pct_labels),
                    ncol = max_iv_levels_all,
                    dimnames = list(NULL, p_col_names_sg)) %>%
                  tibble::as_tibble()
                )
            }) %>% dplyr::bind_rows()
            sg_rows <- dplyr::bind_rows(sg_rows, na_rows)
          } else {
            sg_rows <- dplyr::bind_rows(
              sg_rows,
              .compute_focus(focus_df, focus, use_weight = use_weight_now)
            )
          }
        }

        sg_rows$WeightMode <- wm
        sg_rows
      })

      dplyr::bind_rows(per_wmode)
    }) %>% dplyr::bind_rows()

    # Build the shifted_probs sheet: Key = {sg}|{iv}|{pct_label}|{focus}|{wm},
    # columns P_L1..P_Lmax. Max-levels across all subgroups determines column
    # count; pad shorter-IV rows with 0 via dplyr::bind_rows (fills missing
    # cols with NA, which we replace below).
    shifted_probs_data <- pct_data_list %>%
      dplyr::mutate(
        Key = paste(Subgroup, IV, pct_label, Focus, WeightMode, sep = "|"),
        .before = 1
      )

    # Align P_L columns to the global max (in case different subgroups have
    # different IV level counts).
    all_p_cols_sp <- grep("^P_L\\d+$", names(shifted_probs_data), value = TRUE)
    all_p_cols_sp <- all_p_cols_sp[
      order(as.integer(sub("^P_L", "", all_p_cols_sp)))
    ]
    # NA → 0 for padding columns (IV had fewer levels than the global max).
    for (col in all_p_cols_sp) {
      v <- shifted_probs_data[[col]]
      # Only replace NA where it's a padding-position NA (column index >
      # n_iv_levels for that row). Keep NAs from failed shifts intact.
      pad_na <- is.na(v) & (as.integer(sub("^P_L", "", col)) >
        shifted_probs_data[["n_iv_levels"]])
      v[pad_na] <- 0
      shifted_probs_data[[col]] <- v
    }

    # Keep Key, Mean (for "mean from X to Y" display), and P_L* columns.
    shifted_probs_data <- shifted_probs_data[, c("Key", "Mean", all_p_cols_sp)]

  }

  # ---------------------------------------------------------------------------
  # 2. Get variable labels if dictionary provided
  # ---------------------------------------------------------------------------

  if (!is.null(dictionary)) {
    dict_df <- work::dictionary_from_named_object(dictionary)
    label_lookup <- rlang::set_names(dict_df$label, dict_df$var)
  } else {
    label_lookup <- NULL
  }

  .get_label <- function(var_name) {
    if (!is.null(label_lookup) && var_name %in% names(label_lookup)) {
      label_lookup[[var_name]]
    } else {
      var_name
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Write hidden data sheets (factored storage — see Phase 1a + Phase 2)
  # ---------------------------------------------------------------------------
  #   _sim_data_wide   rows: Key="{sg}|{iv}|{target}", cols EV_L1..EV_L{max}
  #   _sim_data_prior  rows: Key="{sg}|{target}",      col  Prior_EV
  #   _sim_shifted_probs (Mean mode only): Key="{sg}|{iv}|{pct}|{focus}|{wm}",
  #                    cols P_L1..P_L{max} — shifted probability vectors
  # Mean-mode EV is computed in-dashboard as SUMPRODUCT(P_L row, EV_L row).

  wide_sheet  <- "_sim_data_wide"
  prior_sheet <- "_sim_data_prior"

  openxlsx::addWorksheet(wb, wide_sheet)
  openxlsx::writeData(wb, wide_sheet, sim_data_wide, startRow = 1, startCol = 1)

  openxlsx::addWorksheet(wb, prior_sheet)
  openxlsx::writeData(wb, prior_sheet, sim_data_prior, startRow = 1, startCol = 1)

  n_wide_rows  <- nrow(sim_data_wide)
  n_prior_rows <- nrow(sim_data_prior)

  probs_sheet <- "_sim_shifted_probs"
  if (has_freq_shifts) {
    # Round P_L* columns to 6 decimals — probabilities, so more precision
    # needed than EVs. Still much smaller than default ~15-digit XML.
    prob_cols <- setdiff(names(shifted_probs_data), "Key")
    shifted_probs_data[prob_cols] <- lapply(shifted_probs_data[prob_cols],
      round, 6)

    openxlsx::addWorksheet(wb, probs_sheet)
    openxlsx::writeData(wb, probs_sheet, shifted_probs_data,
      startRow = 1, startCol = 1)
    n_probs_rows <- nrow(shifted_probs_data)
  }

  # ---------------------------------------------------------------------------
  # 3b. Hidden helper sheet: base sizes for every (subgroup, focus) slice.
  # The dashboard uses this to (a) show "Base: N" next to the Focus cell,
  # and (b) switch to the red warning style when n < min_base_for_sim.
  # ---------------------------------------------------------------------------
  base_sheet <- "_sim_base"
  if (length(base_slices) > 0) {
    base_df <- dplyr::bind_rows(base_slices) %>%
      dplyr::mutate(key = paste(subgroup, focus, sep = "|")) %>%
      dplyr::select(key, n_obs)
    openxlsx::addWorksheet(wb, base_sheet)
    openxlsx::writeData(wb, base_sheet, base_df, startRow = 1, startCol = 1)
  } else {
    base_sheet <- NULL
  }

  # ---------------------------------------------------------------------------
  # 4. Write lookup sheet for simulator dropdowns
  # ---------------------------------------------------------------------------

  sim_lookup <- "_sim_lookup"
  openxlsx::addWorksheet(wb, sim_lookup)

  # Get all nodes from first subgroup for variable list. When sim_dv_only,
  # the dashboard only shows the DV as a target row (everything else is
  # collapsed from the lookup tables).
  first_sg <- names(sg_list)[1]
  first_fit <- obj[[first_sg]][["fit"]]
  all_nodes <- if (isTRUE(sim_dv_only)) dv else names(first_fit)
  node_levels <- purrr::map(rlang::set_names(all_nodes), function(nd) {
    dimnames(first_fit[[nd]][["prob"]])[[1]]
  })
  max_levels <- max(purrr::map_int(node_levels, length))

  # Sequential column counter for _sim_lookup
  lk_col <- 1L

  # Small helper: write a header at (row 1, startCol) and a values vector
  # starting at row 2 — single-call, avoids per-cell writeData overhead.
  .write_lookup_col <- function(startCol, header, values) {
    openxlsx::writeData(wb, sim_lookup, header,
      startRow = 1, startCol = startCol)
    if (length(values) > 0) {
      openxlsx::writeData(wb, sim_lookup, values,
        startRow = 2, startCol = startCol, colNames = FALSE)
    }
  }

  # Subgroup options
  sg_col <- lk_col
  .write_lookup_col(lk_col, "Subgroup", names(sg_list))
  lk_col <- lk_col + 1L

  # Variable options (all IVs)
  first_ivs <- obj[[first_sg]][["meta"]][["ivs"]]
  var_col <- lk_col
  .write_lookup_col(lk_col, "Variable", first_ivs)
  lk_col <- lk_col + 1L

  # Variable labels
  label_lk_col <- lk_col
  .write_lookup_col(lk_col, "Label", vapply(first_ivs, .get_label, character(1)))
  lk_col <- lk_col + 1L

  # Levels per variable (one column per variable, rows = levels)
  levels_start_col <- lk_col
  for (vi in seq_along(first_ivs)) {
    iv <- first_ivs[vi]
    .write_lookup_col(lk_col, iv, as.numeric(node_levels[[iv]]))
    lk_col <- lk_col + 1L
  }
  levels_end_col <- lk_col - 1L
  n_lookup_cols <- levels_end_col

  # Mode options
  mode_options <- if (has_freq_shifts) c("Level", "Mean") else "Level"
  mode_col <- lk_col
  .write_lookup_col(lk_col, "Mode", mode_options)
  lk_col <- lk_col + 1L

  # Percent change steps — base labels (used for key matching)
  pct_col <- NULL
  if (has_freq_shifts) {
    pct_col <- lk_col
    .write_lookup_col(lk_col, "Mean", pct_labels)
    lk_col <- lk_col + 1L
  }

  # Focus options (for Mean mode)
  focus_opt_col <- NULL
  if (has_freq_shifts && !is.null(brand) && !is.null(brand_names)) {
    focus_opt_col <- lk_col
    sim_focus_options <- c("Market", brand_names)
    .write_lookup_col(lk_col, "Focus", sim_focus_options)
    lk_col <- lk_col + 1L
  }

  # Metric options
  metric_options <- c("Absolute", "Percent Change", "Absolute Change")
  metric_opt_col <- lk_col
  .write_lookup_col(lk_col, "Metric", metric_options)
  lk_col <- lk_col + 1L

  # Weight Mode options (only when a weight column was provided so the
  # dashboard has a real Weighted/Unweighted choice to switch between).
  weight_opt_col <- NULL
  weight_options <- character(0)
  if (has_weight) {
    weight_options <- c("Unweighted", "Weighted")
    weight_opt_col <- lk_col
    .write_lookup_col(lk_col, "Weight", weight_options)
    lk_col <- lk_col + 1L
  }

  # ---------------------------------------------------------------------------
  # 5. Build Simulator dashboard sheet
  # ---------------------------------------------------------------------------

  dash_sheet <- "Simulator"
  openxlsx::addWorksheet(wb, dash_sheet)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(fgFill = "#FFFFFF"),
    rows = 1:200, cols = 1:50, gridExpand = TRUE, stack = TRUE)

  # Styles
  styles <- list(
    title     = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header    = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE,
                                      border = "TopBottom", borderStyle = "medium",
                                      fgFill = "#D9D9D9"),
    pct       = openxlsx::createStyle(numFmt = "0.0%", halign = "center"),
    left      = openxlsx::createStyle(halign = "left"),
    dropdown_label = openxlsx::createStyle(textDecoration = "bold", halign = "right"),
    dropdown_cell  = openxlsx::createStyle(
      border = "Bottom", borderStyle = "thin", halign = "center"
    )
  )

  # Layout
  col_data_start <- 2L
  row_title <- 2L
  row_subtitle <- 3L
  row_dropdowns <- 5L

  n_targets <- length(all_nodes)
  n_p_cols <- length(p_cols)

  # Title
  openxlsx::writeData(wb, dash_sheet, "Bayesian Network Simulator", startRow = row_title, startCol = col_data_start)
  openxlsx::addStyle(wb, dash_sheet, style = styles$title,
    rows = row_title, cols = col_data_start, stack = TRUE)
  openxlsx::writeData(wb, dash_sheet, "Set evidence on one variable to see updated probability distributions",
    startRow = row_subtitle, startCol = col_data_start)
  openxlsx::addStyle(wb, dash_sheet, style = styles$sub_title,
    rows = row_subtitle, cols = col_data_start, stack = TRUE)

  # Dropdown controls — stacked vertically in columns B (label) and C (value)
  has_subgroups <- length(sg_list) > 1
  label_col <- col_data_start
  cell_col <- col_data_start + 1L

  dropdown_cell_style <- openxlsx::createStyle(
    border = "Bottom", borderStyle = "thin", halign = "left"
  )

  current_row <- row_dropdowns

  # Mode dropdown (only if freq shifts available)
  if (has_freq_shifts) {
    mode_cell_col <- cell_col
    mode_cell_row <- current_row
    openxlsx::writeData(wb, dash_sheet, "Mode: ", startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)
    openxlsx::writeData(wb, dash_sheet, if (has_freq_shifts) "Mean" else "Level", startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
      rows = current_row, cols = cell_col, stack = TRUE)

    mode_range <- paste0(sim_lookup, "!$", num2let(mode_col), "$2:$", num2let(mode_col), "$", length(mode_options) + 1)
    openxlsx::dataValidation(wb, dash_sheet,
      col = cell_col, rows = current_row,
      type = "list", value = mode_range)
    current_row <- current_row + 1L
  }

  # Focus dropdown (only when brands available, only applies to Mean mode)
  focus_sim_cell_col <- NULL
  focus_sim_cell_row <- NULL
  if (!is.null(focus_opt_col)) {
    focus_sim_cell_col <- cell_col
    focus_sim_cell_row <- current_row
    openxlsx::writeData(wb, dash_sheet, "Focus: ", startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)
    openxlsx::writeData(wb, dash_sheet, "Market", startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
      rows = current_row, cols = cell_col, stack = TRUE)

    focus_sim_range <- paste0(sim_lookup, "!$", num2let(focus_opt_col), "$2:$",
      num2let(focus_opt_col), "$", length(sim_focus_options) + 1)
    openxlsx::dataValidation(wb, dash_sheet,
      col = cell_col, rows = current_row,
      type = "list", value = focus_sim_range)
    current_row <- current_row + 1L
  }

  # Metric dropdown
  metric_sim_cell_col <- cell_col
  metric_sim_cell_row <- current_row
  openxlsx::writeData(wb, dash_sheet, "Metric: ", startRow = current_row, startCol = label_col)
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
    rows = current_row, cols = label_col, stack = TRUE)
  openxlsx::writeData(wb, dash_sheet, "Percent Change", startRow = current_row, startCol = cell_col)
  openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
    rows = current_row, cols = cell_col, stack = TRUE)

  metric_sim_range <- paste0(sim_lookup, "!$", num2let(metric_opt_col), "$2:$", num2let(metric_opt_col), "$", length(metric_options) + 1)
  openxlsx::dataValidation(wb, dash_sheet,
    col = cell_col, rows = current_row,
    type = "list", value = metric_sim_range)
  current_row <- current_row + 1L

  # Weight dropdown (only appears when a weight column was provided).
  # Affects Mean mode only; Level mode reads from _sim_data_wide (gRain
  # posteriors), which are weight-independent and aren't reshaped by this control.
  weight_sim_cell_col <- NULL
  weight_sim_cell_row <- NULL
  if (has_weight) {
    weight_sim_cell_col <- cell_col
    weight_sim_cell_row <- current_row
    openxlsx::writeData(wb, dash_sheet, "Weight: ",
      startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)
    openxlsx::writeData(wb, dash_sheet, "Weighted",
      startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
      rows = current_row, cols = cell_col, stack = TRUE)

    weight_sim_range <- paste0(sim_lookup, "!$", num2let(weight_opt_col),
      "$2:$", num2let(weight_opt_col), "$", length(weight_options) + 1)
    openxlsx::dataValidation(wb, dash_sheet,
      col = cell_col, rows = current_row,
      type = "list", value = weight_sim_range)
    current_row <- current_row + 1L
  }

  if (has_subgroups) {
    openxlsx::writeData(wb, dash_sheet, "Subgroup: ", startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)
    openxlsx::writeData(wb, dash_sheet, names(sg_list)[1], startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
      rows = current_row, cols = cell_col, stack = TRUE)

    sg_range <- paste0(sim_lookup, "!$A$2:$A$", length(sg_list) + 1)
    openxlsx::dataValidation(wb, dash_sheet,
      col = cell_col, rows = current_row,
      type = "list", value = sg_range)

    sg_cell_col <- cell_col
    sg_cell_row <- current_row
    current_row <- current_row + 1L
  }

  # ---------------------------------------------------------------------------
  # Base-size display + warning for Focus dropdown (matches dynamic dashboard)
  #   n >= min_base_for_sim → "Base: N" (grey)
  #   n <  min_base_for_sim → "Results not calculated because base is below N" (red)
  # ---------------------------------------------------------------------------
  if (!is.null(base_sheet) && !is.null(focus_sim_cell_col)) {
    focus_let <- num2let(focus_sim_cell_col)
    focus_ref <- paste0("$", focus_let, "$", focus_sim_cell_row)

    if (has_subgroups) {
      sg_let <- num2let(sg_cell_col)
      sg_ref <- paste0("$", sg_let, "$", sg_cell_row)
      base_key <- paste0(sg_ref, "&\"|\"&", focus_ref)
    } else {
      base_key <- paste0("\"", names(sg_list)[1], "|\"&", focus_ref)
    }

    warn_col <- focus_sim_cell_col + 1L
    match_expr <- paste0("MATCH(", base_key, ",",
      base_sheet, "!$A:$A,0)")
    n_lookup <- paste0("INDEX(", base_sheet, "!$B:$B,", match_expr, ")")

    focus_warn_formula <- paste0(
      "IFERROR(",
        "IF(", n_lookup, "<", min_base_for_sim, ",",
          "\"Results not calculated because base is below ", min_base_for_sim, "\",",
          "\"Base: \"&", n_lookup,
        "),",
      "\"\")"
    )
    openxlsx::writeFormula(wb, dash_sheet, x = focus_warn_formula,
      startRow = focus_sim_cell_row, startCol = warn_col)

    # Conditional formatting — red only when base is below the threshold.
    red_rule <- paste0("IFERROR(", n_lookup, "<", min_base_for_sim, ",FALSE)")
    red_warning <- openxlsx::createStyle(fontColour = "#FF0000",
      textDecoration = "bold")
    red_cell <- openxlsx::createStyle(bgFill = "#FF0000",
      fontColour = "#FFFFFF")

    openxlsx::conditionalFormatting(wb, dash_sheet,
      cols = warn_col, rows = focus_sim_cell_row,
      style = red_warning, type = "expression", rule = red_rule)
    openxlsx::conditionalFormatting(wb, dash_sheet,
      cols = focus_sim_cell_col, rows = focus_sim_cell_row,
      style = red_cell, type = "expression", rule = red_rule)
  }

  # Variable dropdown
  var_cell_col <- cell_col
  var_cell_row <- current_row
  openxlsx::writeData(wb, dash_sheet, "Variable: ", startRow = current_row, startCol = label_col)
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
    rows = current_row, cols = label_col, stack = TRUE)
  openxlsx::writeData(wb, dash_sheet, first_ivs[1], startRow = current_row, startCol = cell_col)
  openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
    rows = current_row, cols = cell_col, stack = TRUE)

  var_range <- paste0(sim_lookup, "!$B$2:$B$", length(first_ivs) + 1)
  openxlsx::dataValidation(wb, dash_sheet,
    col = cell_col, rows = current_row,
    type = "list", value = var_range)
  current_row <- current_row + 1L

  # Level / Pct Change dropdown (label changes based on mode)
  level_cell_col <- cell_col
  level_cell_row <- current_row
  if (has_freq_shifts) {
    mode_ref_label <- paste0("$", num2let(mode_cell_col), "$", mode_cell_row)
    label_formula <- paste0("IF(", mode_ref_label, "=\"Mean\",\"Pct Change: \",\"Level: \")")
    openxlsx::writeFormula(wb, dash_sheet, x = label_formula,
      startRow = current_row, startCol = label_col)
  } else {
    openxlsx::writeData(wb, dash_sheet, "Level: ", startRow = current_row, startCol = label_col)
  }
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
    rows = current_row, cols = label_col, stack = TRUE)
  default_level_val <- if (has_freq_shifts) {
    pct_labels[which(pct_steps == 0)]
  } else {
    as.numeric(node_levels[[first_ivs[1]]][1])
  }
  openxlsx::writeData(wb, dash_sheet, default_level_val,
    startRow = current_row, startCol = cell_col)
  openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
    rows = current_row, cols = cell_col, stack = TRUE)

  # Dynamic level dropdown via active levels helper column
  var_ref <- paste0("$", num2let(var_cell_col), "$", var_cell_row)
  lookup_header_range <- paste0(sim_lookup, "!$", num2let(levels_start_col), "$1:$", num2let(levels_end_col), "$1")

  active_level_col <- lk_col
  lk_col <- lk_col + 1L
  dash_sheet_escaped <- gsub("'", "''", dash_sheet)
  var_ref_from_lookup <- paste0("'", dash_sheet_escaped, "'!$", num2let(var_cell_col), "$", var_cell_row)

  levels_start_let <- num2let(levels_start_col)
  levels_end_let <- num2let(levels_end_col)

  openxlsx::writeData(wb, sim_lookup, "Active Levels", startRow = 1, startCol = active_level_col)
  # Build all max_levels formulas as a single character vector, then write
  # once — openxlsx::writeFormula accepts vectors and is dramatically faster
  # than per-cell calls.
  level_formulas <- vapply(seq_len(max_levels), function(li) {
    paste0(
      "IFERROR(INDEX(", sim_lookup, "!$", levels_start_let, "$", li + 1,
      ":$", levels_end_let, "$", li + 1,
      ",1,MATCH(", var_ref_from_lookup, ",", sim_lookup, "!$", levels_start_let,
      "$1:$", levels_end_let, "$1,0)),\"\")"
    )
  }, character(1))
  openxlsx::writeFormula(wb, sim_lookup, x = level_formulas,
    startRow = 2, startCol = active_level_col)

  active_level_let <- num2let(active_level_col)

  level_count_col <- lk_col
  lk_col <- lk_col + 1L
  openxlsx::writeData(wb, sim_lookup, "Level Count", startRow = 1, startCol = level_count_col)
  count_formula <- paste0(
    "COUNTIF($", active_level_let, "$2:$", active_level_let, "$", max_levels + 1, ",\"<>\"&\"\")"
  )
  openxlsx::writeFormula(wb, sim_lookup, x = count_formula, startRow = 2, startCol = level_count_col)

  if (has_freq_shifts) {
    mode_ref_from_lookup <- paste0("'", dash_sheet_escaped, "'!$", num2let(mode_cell_col), "$", mode_cell_row)

    # Active Options column: switches between levels and pct labels based on Mode
    active_options_col <- lk_col
    lk_col <- lk_col + 1L
    openxlsx::writeData(wb, sim_lookup, "Active Options", startRow = 1, startCol = active_options_col)

    pct_let <- num2let(pct_col)

    # Subgroup and focus refs from lookup sheet
    if (has_subgroups) {
      sg_ref_from_lookup <- paste0("'", dash_sheet_escaped, "'!$", num2let(sg_cell_col), "$", sg_cell_row)
    } else {
      sg_ref_from_lookup <- paste0("\"", names(sg_list)[1], "\"")
    }
    if (!is.null(focus_sim_cell_col)) {
      focus_ref_from_lookup <- paste0("'", dash_sheet_escaped, "'!$", num2let(focus_sim_cell_col), "$", focus_sim_cell_row)
    } else {
      focus_ref_from_lookup <- "\"Market\""
    }

    n_option_rows <- max(length(pct_labels), max_levels)
    # Build all option formulas as a vector, write once.
    opt_formulas <- vapply(seq_len(n_option_rows), function(oi) {
      level_cell <- paste0("$", active_level_let, "$", oi + 1)
      if (oi <= length(pct_labels)) {
        # Base pct label from the Percent Change column (no mean suffix)
        base_pct <- paste0("$", pct_let, "$", oi + 1)
        paste0(
          "IF(", mode_ref_from_lookup, "=\"Mean\",", base_pct, ",",
          "IF(", level_cell, "=\"\",\"\",", level_cell, "))"
        )
      } else {
        paste0(
          "IF(", mode_ref_from_lookup, "=\"Mean\",\"\",",
          "IF(", level_cell, "=\"\",\"\",", level_cell, "))"
        )
      }
    }, character(1))
    openxlsx::writeFormula(wb, sim_lookup, x = opt_formulas,
      startRow = 2, startCol = active_options_col)

    # Count non-blank active options
    options_count_col <- lk_col
    lk_col <- lk_col + 1L
    openxlsx::writeData(wb, sim_lookup, "Options Count", startRow = 1, startCol = options_count_col)
    count_formula <- paste0(
      "COUNTIF($", num2let(active_options_col), "$2:$", num2let(active_options_col), "$",
      n_option_rows + 1, ",\"<>\"&\"\")"
    )
    openxlsx::writeFormula(wb, sim_lookup, x = count_formula, startRow = 2, startCol = options_count_col)

    validation_formula <- paste0(
      "OFFSET(", sim_lookup, "!$", num2let(active_options_col), "$2,0,0,",
      sim_lookup, "!$", num2let(options_count_col), "$2,1)"
    )
    openxlsx::dataValidation(wb, dash_sheet,
      col = level_cell_col, rows = level_cell_row,
      type = "list", value = validation_formula)

  } else {
    level_validation <- paste0(
      "OFFSET(", sim_lookup, "!$", active_level_let, "$2,0,0,",
      sim_lookup, "!$", num2let(level_count_col), "$2,1)"
    )
    openxlsx::dataValidation(wb, dash_sheet,
      col = level_cell_col, rows = level_cell_row,
      type = "list", value = level_validation)
  }

  # "mean from x to y" display next to Level/Pct dropdown (Mean mode only)
  if (has_freq_shifts) {
    mean_display_col <- level_cell_col + 1L
    level_ref_display <- paste0("$", num2let(level_cell_col), "$", level_cell_row)
    mode_ref_display <- paste0("$", num2let(mode_cell_col), "$", mode_cell_row)

    # Refs for subgroup, focus, variable
    if (has_subgroups) {
      sg_ref_d <- paste0("$", num2let(sg_cell_col), "$", sg_cell_row)
    } else {
      sg_ref_d <- paste0("\"", names(sg_list)[1], "\"")
    }
    if (!is.null(focus_sim_cell_col)) {
      focus_ref_d <- paste0("$", num2let(focus_sim_cell_col), "$", focus_sim_cell_row)
    } else {
      focus_ref_d <- "\"Market\""
    }
    var_ref_d <- paste0("$", num2let(var_cell_col), "$", var_cell_row)
    # Weight mode ref: live cell when the dropdown exists, otherwise a
    # constant "Unweighted" literal (matching how pct_data was built).
    if (has_weight) {
      weight_ref_d <- paste0("$", num2let(weight_sim_cell_col), "$", weight_sim_cell_row)
      weight_suffix_d <- paste0("&\"|\"&", weight_ref_d)
    } else {
      weight_suffix_d <- "&\"|Unweighted\""
    }

    # Keys: subgroup|variable|pct_label|focus|weight_mode  → _sim_shifted_probs
    zero_label <- pct_labels[which(pct_steps == 0)]
    base_probs_key    <- paste0(sg_ref_d, "&\"|\"&", var_ref_d,
      "&\"|", zero_label, "\"&\"|\"&", focus_ref_d, weight_suffix_d)
    shifted_probs_key <- paste0(sg_ref_d, "&\"|\"&", var_ref_d,
      "&\"|\"&", level_ref_display, "&\"|\"&", focus_ref_d, weight_suffix_d)

    # _sim_shifted_probs: Mean column (B) is precomputed per row, so the
    # display is a plain INDEX/MATCH — no array-formula contortions.
    probs_key_range_d  <- paste0(probs_sheet, "!$A$2:$A$", n_probs_rows + 1)
    probs_mean_range_d <- paste0(probs_sheet, "!$B$2:$B$", n_probs_rows + 1)

    base_mean_val <- paste0(
      "ROUND(INDEX(", probs_mean_range_d, ",MATCH(",
      base_probs_key, ",", probs_key_range_d, ",0)),2)"
    )
    shifted_mean_val <- paste0(
      "ROUND(INDEX(", probs_mean_range_d, ",MATCH(",
      shifted_probs_key, ",", probs_key_range_d, ",0)),2)"
    )

    mean_display_formula <- paste0(
      "IF(", mode_ref_display, "=\"Mean\",IFERROR(\"mean from \"&",
      base_mean_val, "&\" to \"&", shifted_mean_val, ",\"\"),\"\")"
    )

    openxlsx::writeFormula(wb, dash_sheet, x = mean_display_formula,
      startRow = level_cell_row, startCol = mean_display_col)
    openxlsx::addStyle(wb, dash_sheet,
      style = openxlsx::createStyle(textDecoration = "italic", halign = "left"),
      rows = level_cell_row, cols = mean_display_col, stack = TRUE)
  }

  current_row <- current_row + 1L

  # Update row_data_start to be 1 row after last control
  row_data_start <- current_row + 1L

  # ---------------------------------------------------------------------------
  # Headers: Variable | Community | Label | Metric
  # ---------------------------------------------------------------------------

  # Community column is pointless when there's only one row (DV-only mode).
  has_community <- !is.null(community_nodes) && !isTRUE(sim_dv_only)

  # Build community lookup from nodes data
  if (has_community) {
    community_lookup <- rlang::set_names(
      as.character(community_nodes$community_name), community_nodes$id
    )
    community_group <- rlang::set_names(community_nodes$group, community_nodes$id)
    community_color <- rlang::set_names(community_nodes$color, community_nodes$id)
  }

  # Sort nodes: DV first, then by community group number, then variable name
  dv_name <- if (is.null(names(dv))) dv else unname(dv)
  dv_nodes <- intersect(dv_name, all_nodes)
  iv_nodes <- setdiff(all_nodes, dv_nodes)

  if (has_community) {
    iv_order <- order(
      purrr::map_int(iv_nodes, ~ as.integer(community_group[.x] %||% 999L)),
      iv_nodes
    )
    iv_nodes <- iv_nodes[iv_order]
  }

  all_nodes <- c(dv_nodes, iv_nodes)
  n_targets <- length(all_nodes)

  leading_cols <- c("Variable")
  if (has_community) leading_cols <- c(leading_cols, "Community")
  leading_cols <- c(leading_cols, "Label")
  n_leading <- length(leading_cols)

  header_names <- c(leading_cols, "Metric")

  # Write the whole header row in one call (1-row data frame -> horizontal
  # strip starting at startCol). Faster than looping one writeData per header.
  header_row_df <- as.data.frame(matrix(header_names, nrow = 1),
    stringsAsFactors = FALSE)
  openxlsx::writeData(wb, dash_sheet, header_row_df,
    startRow = row_data_start, startCol = col_data_start, colNames = FALSE)
  all_header_cols <- seq(col_data_start, col_data_start + length(header_names) - 1)
  openxlsx::addStyle(wb, dash_sheet, style = styles$header,
    rows = row_data_start, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "TopBottomLeft", borderStyle = "medium"),
    rows = row_data_start, cols = min(all_header_cols), stack = TRUE)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "TopBottomRight", borderStyle = "medium"),
    rows = row_data_start, cols = max(all_header_cols), stack = TRUE)

  # ---------------------------------------------------------------------------
  # Write target variable names, community, labels, and apply community colors
  # ---------------------------------------------------------------------------

  data_rows <- seq(row_data_start + 1, row_data_start + n_targets)

  # Build the full leading-column block (Variable [, Community], Label) as
  # a single data frame and write it once. Styling passes (community colors)
  # still loop per-row because the colours are per-target.
  leading_block <- data.frame(Variable = all_nodes, stringsAsFactors = FALSE)
  if (has_community) {
    leading_block$Community <- vapply(all_nodes, function(nd) {
      if (nd %in% names(community_lookup)) as.character(community_lookup[[nd]]) else ""
    }, character(1))
  }
  leading_block$Label <- vapply(all_nodes, .get_label, character(1))
  openxlsx::writeData(wb, dash_sheet, leading_block,
    startRow = row_data_start + 1L, startCol = col_data_start,
    colNames = FALSE)

  # Apply per-target community colors (styling must stay per-row).
  if (has_community) {
    for (ri in seq_along(all_nodes)) {
      nd <- all_nodes[ri]
      if (nd %in% names(community_color)) {
        fill_color <- as.character(community_color[[nd]])
        if (grepl("^#[0-9A-Fa-f]{6}$", fill_color)) {
          comm_style <- openxlsx::createStyle(fgFill = fill_color)
          openxlsx::addStyle(wb, dash_sheet, style = comm_style,
            rows = data_rows[ri], cols = all_header_cols,
            gridExpand = TRUE, stack = TRUE)
        }
      }
    }
  }

  # Left-align leading columns
  for (lci in seq_len(n_leading)) {
    openxlsx::addStyle(wb, dash_sheet, style = styles$left,
      rows = data_rows, cols = col_data_start + lci - 1, gridExpand = TRUE, stack = TRUE)
  }

  # ---------------------------------------------------------------------------
  # Metric formulas: INDEX/MATCH against factored sheets
  # ---------------------------------------------------------------------------
  # Level mode raw EV : INDEX(_sim_data_wide EV cols, row="{sg}|{iv}|{target}",
  #                     col=position of selected level in active levels)
  # Level mode prior   : INDEX(_sim_data_prior, row="{sg}|{target}")
  # Mean  mode raw EV  : SUMPRODUCT(
  #                       INDEX(_sim_shifted_probs P cols,
  #                             row="{sg}|{iv}|{pct}|{focus}|{wm}", 0),
  #                       INDEX(_sim_data_wide EV cols,
  #                             row="{sg}|{iv}|{target}", 0))
  # Mean  mode baseline: Same with pct set to 0-label.

  var_ref <- paste0("$", num2let(var_cell_col), "$", var_cell_row)
  level_ref <- paste0("$", num2let(level_cell_col), "$", level_cell_row)
  if (has_subgroups) {
    sg_ref <- paste0("$", num2let(sg_cell_col), "$", sg_cell_row)
  }

  ev_col <- col_data_start + n_leading
  metric_sim_ref <- paste0("$", num2let(metric_sim_cell_col), "$", metric_sim_cell_row)

  # _sim_data_wide ranges (Key + EV_L1..EV_Lmax)
  wide_last_col_let <- num2let(1 + max_iv_levels_all)
  wide_key_range <- paste0(wide_sheet, "!$A$2:$A$", n_wide_rows + 1)
  wide_ev_range  <- paste0(wide_sheet, "!$B$2:$", wide_last_col_let,
    "$", n_wide_rows + 1)

  # _sim_data_prior ranges (Key + Prior_EV)
  prior_key_range <- paste0(prior_sheet, "!$A$2:$A$", n_prior_rows + 1)
  prior_ev_range  <- paste0(prior_sheet, "!$B$2:$B$", n_prior_rows + 1)

  # Position lookup range (active levels, string form) — for Level mode
  # col-index resolution. Uses the original Active Levels column so the
  # match is on the user-visible string form, not the numeric-padded form.
  active_levels_range <- paste0(sim_lookup, "!$", active_level_let,
    "$2:$", active_level_let, "$", max_levels + 1)

  sg_prefix <- if (has_subgroups) {
    paste0(sg_ref, "&\"|\"&")
  } else {
    paste0("\"", names(sg_list)[1], "|\"&")
  }
  bl_sg_prefix_str <- if (has_subgroups) {
    paste0(sg_ref, "&\"")
  } else {
    paste0("\"", names(sg_list)[1])
  }

  if (has_freq_shifts) {
    mode_ref <- paste0("$", num2let(mode_cell_col), "$", mode_cell_row)
    is_mean_mode <- paste0(mode_ref, "=\"Mean\"")

    if (!is.null(focus_sim_cell_col)) {
      focus_ref <- paste0("$", num2let(focus_sim_cell_col), "$", focus_sim_cell_row)
    } else {
      focus_ref <- "\"Market\""
    }

    # _sim_shifted_probs ranges. Layout: Key (A), Mean (B), P_L1..P_Lmax
    # (C onward). P_L width matches max_iv_levels_all so Mean-mode
    # SUMPRODUCT against _sim_data_wide EV_L* is elementwise.
    probs_last_col_let <- num2let(2 + max_iv_levels_all) # A + Mean + P_L1..P_Lmax
    probs_key_range  <- paste0(probs_sheet, "!$A$2:$A$", n_probs_rows + 1)
    probs_mean_range <- paste0(probs_sheet, "!$B$2:$B$", n_probs_rows + 1)
    probs_p_range    <- paste0(probs_sheet, "!$C$2:$", probs_last_col_let,
      "$", n_probs_rows + 1)

    if (has_weight) {
      weight_ref_fmla <- paste0("$", num2let(weight_sim_cell_col), "$", weight_sim_cell_row)
      weight_suffix_fmla <- paste0("&\"|\"&", weight_ref_fmla)
    } else {
      weight_suffix_fmla <- "&\"|Unweighted\""
    }
  }

  .metric_formula <- function(raw_ev, baseline_ev) {
    paste0(
      "IF(", metric_sim_ref, "=\"Absolute\",", raw_ev, ",",
      "IF(", metric_sim_ref, "=\"Absolute Change\",", raw_ev, "-", baseline_ev, ",",
      "TEXT(ROUND((", raw_ev, "-", baseline_ev, ")/", baseline_ev, "*100,1),\"0.0\")&\"%\"))"
    )
  }

  ev_formulas <- vapply(all_nodes, function(nd) {
    # --- Level mode ---
    # Row: MATCH("{sg}|{var}|{target}", wide_key_range)
    # Col: MATCH(level_ref, active_levels_range, 0)  — position of user's
    #      level choice within the active variable's level list.
    level_wide_key <- paste0(sg_prefix, var_ref, "&\"|", nd, "\"")
    level_row_match <- paste0("MATCH(", level_wide_key, ",", wide_key_range, ",0)")
    level_col_match <- paste0("MATCH(", level_ref, ",", active_levels_range, ",0)")
    raw_level_ev <- paste0("IFERROR(INDEX(", wide_ev_range, ",",
      level_row_match, ",", level_col_match, "),0)")

    # Level baseline: prior EV for (sg, target)
    bl_prior_key <- paste0(bl_sg_prefix_str, "|", nd, "\"")
    bl_level_ev <- paste0("IFERROR(INDEX(", prior_ev_range, ",MATCH(",
      bl_prior_key, ",", prior_key_range, ",0)),0)")

    level_ev <- .metric_formula(raw_level_ev, bl_level_ev)

    if (has_freq_shifts) {
      # Shared wide row lookup (per target) — sim_data_wide key is the same
      # for Mean mode as for Level mode.
      mean_wide_row <- paste0("MATCH(", level_wide_key, ",",
        wide_key_range, ",0)")

      # Shift row: "{sg}|{iv}|{pct}|{focus}|{wm}"
      shift_key <- paste0(sg_prefix, var_ref, "&\"|\"&", level_ref,
        "&\"|\"&", focus_ref, weight_suffix_fmla)
      shift_row_match <- paste0("MATCH(", shift_key, ",", probs_key_range, ",0)")

      raw_pct_ev <- paste0(
        "IFERROR(SUMPRODUCT(",
          "INDEX(", probs_p_range, ",", shift_row_match, ",0),",
          "INDEX(", wide_ev_range, ",", mean_wide_row, ",0)",
        "),0)"
      )

      # Mean-mode baseline: same SUMPRODUCT but with the 0-pct key.
      zero_label <- pct_labels[which(pct_steps == 0)]
      bl_shift_key <- paste0(sg_prefix, var_ref, "&\"|", zero_label,
        "\"&\"|\"&", focus_ref, weight_suffix_fmla)
      bl_shift_row <- paste0("MATCH(", bl_shift_key, ",", probs_key_range, ",0)")
      bl_pct_ev <- paste0(
        "IFERROR(SUMPRODUCT(",
          "INDEX(", probs_p_range, ",", bl_shift_row, ",0),",
          "INDEX(", wide_ev_range, ",", mean_wide_row, ",0)",
        "),0)"
      )

      mean_ev <- .metric_formula(raw_pct_ev, bl_pct_ev)

      paste0("IFERROR(IF(", is_mean_mode, ",", mean_ev, ",", level_ev, "),\"\")")
    } else {
      paste0("IFERROR(", level_ev, ",\"\")")
    }
  }, character(1))

  openxlsx::writeFormula(wb, dash_sheet, x = ev_formulas,
    startRow = data_rows[1], startCol = ev_col)

  openxlsx::addStyle(wb, dash_sheet, style = openxlsx::createStyle(numFmt = "0.0", halign = "center"),
    rows = data_rows, cols = ev_col, gridExpand = TRUE, stack = TRUE)

  # Column widths
  openxlsx::setColWidths(wb, dash_sheet, cols = col_data_start, widths = 20)  # Variable
  if (has_community) {
    openxlsx::setColWidths(wb, dash_sheet, cols = col_data_start + 1, widths = "auto")  # Community
    openxlsx::setColWidths(wb, dash_sheet, cols = col_data_start + 2, widths = "auto")  # Label
  } else {
    openxlsx::setColWidths(wb, dash_sheet, cols = col_data_start + 1, widths = "auto")  # Label
  }

  # Borders
  oxl_outer_box(wb, dash_sheet,
    row_start = row_data_start, row_end = max(data_rows),
    col_start = min(all_header_cols), col_end = max(all_header_cols),
    borderStyle = "medium")

  # Filter — skip in DV-only mode (only one data row, filter is pointless)
  if (!isTRUE(sim_dv_only)) {
    openxlsx::addFilter(wb, dash_sheet, rows = row_data_start, cols = all_header_cols)
  }

  # Hide helper sheets — visibility set by bn_impact_write after all sheets added

  invisible(wb)
}
