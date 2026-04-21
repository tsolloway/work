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

  # Add concatenated key for simple MATCH lookup
  sim_data <- sim_data %>%
    dplyr::mutate(
      Key = paste(Subgroup, Evidence_Variable, Evidence_Level, Target_Variable, sep = "|"),
      .before = 1
    )

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

    nd_levels_numeric <- as.numeric(gsub("^P_", "", p_cols))

    # Build key index for fast lookup
    sim_key_idx <- rlang::set_names(seq_len(nrow(sim_data)), sim_data$Key)

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

      all_nodes_sg <- names(fit)
      node_levels_sg <- purrr::map(rlang::set_names(all_nodes_sg), function(nd) {
        dimnames(fit[[nd]][["prob"]])[[1]]
      })

      # Targets queried as pct_data EV columns. With sim_dv_only the pivot
      # collapses to a single target (the DV).
      target_nodes_sg <- if (isTRUE(sim_dv_only)) dv else all_nodes_sg

      # Build EV matrix once per subgroup (shared across focus options).
      # Columns correspond to target_nodes_sg (one column per target).
      ev_mats <- purrr::map(rlang::set_names(ivs), function(iv) {
        iv_levels <- node_levels_sg[[iv]]
        n_levels <- length(iv_levels)
        n_nodes <- length(target_nodes_sg)
        ev_mat <- matrix(NA_real_, nrow = n_levels, ncol = n_nodes)
        for (li in seq_along(iv_levels)) {
          lv <- iv_levels[li]
          for (ni in seq_along(target_nodes_sg)) {
            key <- paste(sg, iv, lv, target_nodes_sg[ni], sep = "|")
            idx <- sim_key_idx[key]
            if (!is.na(idx)) {
              probs <- as.numeric(sim_data[idx, p_cols])
              ev_mat[li, ni] <- sum(nd_levels_numeric * probs, na.rm = TRUE)
            }
          }
        }
        ev_mat
      })

      # Build one row per (sg, iv, pct_label) with:
      #   Mean_{focus}        — the shifted IV's own mean (per-row scalar)
      #   EV_{focus}_{target} — one column per target node (pivot target from
      #                         rows to columns; eliminates 28x duplication
      #                         and dramatically shrinks the stored sheet).
      .compute_focus <- function(focus_df, focus_name, use_weight) {
        focus_clean <- gsub(" ", "_", focus_name)
        mean_col_name <- paste0("Mean_", focus_clean)
        ev_col_names  <- paste0("EV_", focus_clean, "_", target_nodes_sg)

        # For each IV, build one matrix of shifted probabilities (rows =
        # pct_steps, cols = iv levels), then do ONE matrix multiply against
        # ev_mat to get weighted EVs for every step at once. Replaces the
        # old "build a tibble per pct_step" pattern (thousands of tibbles)
        # with a single per-IV matrix assembly.
        per_iv <- purrr::map(ivs, function(iv) {
          iv_vals <- focus_df[[iv]]
          # Use the survey-weight column ONLY when this run is the weighted
          # version AND the column actually exists on focus_df. Otherwise
          # fall back to raw counts. Returns a named numeric vector whose
          # names are level strings, which is what bn_freq_prob_shift expects.
          if (isTRUE(use_weight) && !is.null(weight) &&
              weight %in% names(focus_df)) {
            w <- focus_df[[weight]]
            ok <- !is.na(iv_vals) & !is.na(w)
            iv_freq <- tapply(w[ok], iv_vals[ok], sum)
          } else {
            iv_freq <- table(iv_vals)
          }
          iv_levels <- node_levels_sg[[iv]]
          iv_values <- as.numeric(iv_levels)
          ev_mat <- ev_mats[[iv]]

          shifted_mat <- matrix(NA_real_, nrow = length(pct_steps),
            ncol = length(iv_levels))
          for (pi in seq_along(pct_steps)) {
            sp <- tryCatch(
              as.numeric(bn_freq_prob_shift(freq = iv_freq, lift = pct_steps[pi],
                impact_shift_type = "proportional")),
              error = function(e) NULL
            )
            if (!is.null(sp) && !anyNA(sp)) shifted_mat[pi, ] <- sp
          }

          # Drop rows where the shift failed (any NA in that pct_step's row).
          good <- !is.na(shifted_mat[, 1])
          if (!any(good)) return(NULL)
          shifted_mat <- shifted_mat[good, , drop = FALSE]
          good_labels <- pct_labels[good]

          # Single matrix multiply: (n_good x n_levels) %*% (n_levels x n_targets)
          # -> (n_good x n_targets) of weighted EVs.
          weighted_evs <- shifted_mat %*% ev_mat
          colnames(weighted_evs) <- ev_col_names
          mean_vals <- as.numeric(shifted_mat %*% iv_values)

          keys <- paste(sg, iv, good_labels, sep = "|")
          tibble::tibble(Key = keys, !!mean_col_name := mean_vals) %>%
            dplyr::bind_cols(tibble::as_tibble(weighted_evs))
        })

        dplyr::bind_rows(purrr::compact(per_iv))
      }

      # When a weight column was provided we precompute BOTH the unweighted
      # and weighted variants and tag each row's Key with the mode suffix —
      # the dashboard's Weight dropdown picks which set of rows to read at
      # runtime. Without a weight we only compute Unweighted.
      weight_modes <- if (has_weight) c("Unweighted", "Weighted") else "Unweighted"

      per_wmode <- purrr::map(weight_modes, function(wm) {
        use_weight_now <- (wm == "Weighted")

        # Market always runs (no base-size restriction)
        sg_result <- .compute_focus(sg_df, "Market", use_weight = use_weight_now)
        if (wm == "Unweighted") {
          # Base sample sizes don't depend on weighting — record once, on the
          # unweighted pass, to avoid duplicating rows in the _sim_base sheet.
          base_slices[[length(base_slices) + 1L]] <<- tibble::tibble(
            subgroup = sg,
            focus = "Market",
            n_obs = nrow(sg_df),
            min_required = min_base_for_sim
          )
        }

        # Join each brand, skipping brand x subgroup slices below
        # min_base_for_sim. Skipped focuses still appear in the workbook
        # dropdown; their Mean_/EV_ columns are NA and a red warning shows.
        for (focus in setdiff(focus_options, "Market")) {
          focus_df <- sg_df[sg_df[[brand]] == focus, , drop = FALSE]

          if (wm == "Unweighted") {
            base_slices[[length(base_slices) + 1L]] <<- tibble::tibble(
              subgroup = sg,
              focus = focus,
              n_obs = nrow(focus_df),
              min_required = min_base_for_sim
            )
          }

          if (nrow(focus_df) < min_base_for_sim) {
            focus_clean <- gsub(" ", "_", focus)
            mean_col_name <- paste0("Mean_", focus_clean)
            ev_col_names <- paste0("EV_", focus_clean, "_", target_nodes_sg)
            na_ev_cols <- rlang::set_names(
              as.list(rep(NA_real_, length(ev_col_names))),
              ev_col_names
            )
            focus_result <- sg_result %>%
              dplyr::select(Key) %>%
              dplyr::mutate(
                !!mean_col_name := NA_real_,
                !!!na_ev_cols
              )
          } else {
            focus_result <- .compute_focus(focus_df, focus,
              use_weight = use_weight_now)
          }

          sg_result <- sg_result %>%
            dplyr::left_join(focus_result, by = "Key")
        }

        # Tag every Key with the weight-mode suffix so the dashboard formulas
        # can disambiguate. Format: subgroup|variable|pct_label|<mode>
        sg_result$Key <- paste0(sg_result$Key, "|", wm)
        sg_result
      })

      dplyr::bind_rows(per_wmode)
    }) %>% dplyr::bind_rows()

    pct_data <- pct_data_list

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
  # 3. Write hidden data sheet
  # ---------------------------------------------------------------------------

  # Trim to Key + Expected_Value
  sim_data <- sim_data[, c("Key", "Expected_Value")]

  # Round numeric cells to 4 decimals before writing. Dashboards format to 1-2
  # decimals anyway, so the extra precision is noise that bloats the XML text
  # representation (e.g., "0.123456789012345" vs "0.1235"). This typically
  # shrinks the stored data sheets by ~60-70% with no visible behavior change.
  sim_data$Expected_Value <- round(sim_data$Expected_Value, 4)

  sim_sheet <- "_sim_data"
  openxlsx::addWorksheet(wb, sim_sheet)
  openxlsx::writeData(wb, sim_sheet, sim_data, startRow = 1, startCol = 1)

  pct_sheet <- "_sim_pct_data"
  if (has_freq_shifts) {
    # Keep Key + all EV_ and Mean_ columns
    keep_cols <- c("Key", grep("^EV_|^Mean_", names(pct_data), value = TRUE))
    pct_data <- pct_data[, keep_cols]

    # Round all numeric columns (EV_ / Mean_) to 4 decimals — same rationale
    # as sim_data above. This is the largest sheet in the workbook so rounding
    # here is where most of the file-size reduction comes from.
    num_cols <- setdiff(keep_cols, "Key")
    pct_data[num_cols] <- lapply(pct_data[num_cols], round, 4)

    n_pct_rows <- nrow(pct_data)
    openxlsx::addWorksheet(wb, pct_sheet)
    openxlsx::writeData(wb, pct_sheet, pct_data, startRow = 1, startCol = 1)
    n_pct_rows <- nrow(pct_data)
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
  # Affects Mean mode only; Level mode reads from _sim_data (gRain posteriors)
  # which aren't reshaped by this control.
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

    # References for dynamic mean lookup from _sim_pct_data
    pct_sim_key_range <- paste0(pct_sheet, "!$A$2:$A$", n_pct_rows + 1)
    pct_sim_data_range <- paste0(pct_sheet, "!$A$2:$", num2let(ncol(pct_data)), "$", n_pct_rows + 1)
    pct_sim_header_range <- paste0(pct_sheet, "!$1:$1")
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

    # Keys: subgroup|variable|pct_label|weight_mode
    zero_label <- pct_labels[which(pct_steps == 0)]
    base_mean_key    <- paste0(sg_ref_d, "&\"|\"&", var_ref_d,
      "&\"|", zero_label, "\"", weight_suffix_d)
    shifted_mean_key <- paste0(sg_ref_d, "&\"|\"&", var_ref_d,
      "&\"|\"&", level_ref_display, weight_suffix_d)

    # Dynamic Mean column
    mean_col_d <- paste0("\"Mean_\"&SUBSTITUTE(", focus_ref_d, ",\" \",\"_\")")
    mean_col_match_d <- paste0("MATCH(", mean_col_d, ",", pct_sheet, "!$1:$1,0)")
    pct_key_range_d <- paste0(pct_sheet, "!$A$2:$A$", n_pct_rows + 1)
    pct_data_range_d <- paste0(pct_sheet, "!$A$2:$", num2let(ncol(pct_data)), "$", n_pct_rows + 1)

    base_mean_val <- paste0("ROUND(INDEX(", pct_data_range_d, ",MATCH(", base_mean_key, ",", pct_key_range_d, ",0),", mean_col_match_d, "),2)")
    shifted_mean_val <- paste0("ROUND(INDEX(", pct_data_range_d, ",MATCH(", shifted_mean_key, ",", pct_key_range_d, ",0),", mean_col_match_d, "),2)")

    mean_display_formula <- paste0(
      "IF(", mode_ref_display, "=\"Mean\",IFERROR(\"mean from \"&", base_mean_val, "&\" to \"&", shifted_mean_val, ",\"\"),\"\")"
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
  # Metric formulas: INDEX/MATCH against _sim_data or _sim_pct_data
  # ---------------------------------------------------------------------------

  var_ref <- paste0("$", num2let(var_cell_col), "$", var_cell_row)
  level_ref <- paste0("$", num2let(level_cell_col), "$", level_cell_row)
  if (has_subgroups) {
    sg_ref <- paste0("$", num2let(sg_cell_col), "$", sg_cell_row)
  }

  n_sim_rows <- nrow(sim_data)
  ev_col <- col_data_start + n_leading
  metric_sim_ref <- paste0("$", num2let(metric_sim_cell_col), "$", metric_sim_cell_row)

  # sim_data: Key, Expected_Value
  sim_key_range <- paste0(sim_sheet, "!$A$2:$A$", n_sim_rows + 1)
  sim_ev_col_pos <- which(names(sim_data) == "Expected_Value")
  sim_ev_range <- paste0(sim_sheet, "!$", num2let(sim_ev_col_pos), "$2:$",
    num2let(sim_ev_col_pos), "$", n_sim_rows + 1)

  if (has_freq_shifts) {
    mode_ref <- paste0("$", num2let(mode_cell_col), "$", mode_cell_row)
    is_mean_mode <- paste0(mode_ref, "=\"Mean\"")

    # level_ref now contains the base pct label directly (no mean suffix to strip)
    pct_base_label <- paste0("\"\"&", level_ref)

    # Focus reference for dynamic column selection
    if (!is.null(focus_sim_cell_col)) {
      focus_ref <- paste0("$", num2let(focus_sim_cell_col), "$", focus_sim_cell_row)
    } else {
      focus_ref <- "\"Market\""
    }

    pct_key_range <- paste0(pct_sheet, "!$A$2:$A$", n_pct_rows + 1)
    pct_header_range <- paste0(pct_sheet, "!$1:$1")
    pct_data_range <- paste0(pct_sheet, "!$A$2:$", num2let(ncol(pct_data)), "$", n_pct_rows + 1)

    # EV column name depends on focus AND the target node, so it is built
    # per-target inside the row loop below. The Mean display uses a
    # separate mean_col_match_d defined earlier with its own focus ref.
  }

  # Helper: wrap raw EV + baseline EV into metric formula
  .metric_formula <- function(raw_ev, baseline_ev) {
    paste0(
      "IF(", metric_sim_ref, "=\"Absolute\",", raw_ev, ",",
      "IF(", metric_sim_ref, "=\"Absolute Change\",", raw_ev, "-", baseline_ev, ",",
      "TEXT(ROUND((", raw_ev, "-", baseline_ev, ")/", baseline_ev, "*100,1),\"0.0\")&\"%\"))"
    )
  }

  # Build every target node's metric formula as one character vector, then
  # write all formulas in a single openxlsx::writeFormula call starting at
  # data_rows[1]. Replaces a per-target writeFormula loop — same output,
  # one call to openxlsx instead of n_targets calls.
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

  # Mean-mode keys include a weight-mode suffix: subgroup|variable|pct_label|<mode>.
  # When the dashboard has a Weight dropdown we reference that cell; otherwise
  # the suffix is a literal "Unweighted" (the only variant we precomputed).
  if (has_weight) {
    weight_ref_fmla <- paste0("$", num2let(weight_sim_cell_col), "$", weight_sim_cell_row)
    weight_suffix_fmla <- paste0("&\"|\"&", weight_ref_fmla)
  } else {
    weight_suffix_fmla <- "&\"|Unweighted\""
  }

  ev_formulas <- vapply(all_nodes, function(nd) {
    # --- Level mode ---
    key_formula  <- paste0(sg_prefix, var_ref, "&\"|\"&\"\"&", level_ref, "&\"|", nd, "\"")
    match_formula <- paste0("MATCH(", key_formula, ",", sim_key_range, ",0)")
    raw_level_ev <- paste0("IFERROR(INDEX(", sim_ev_range, ",", match_formula, "),0)")

    # Baseline for Level mode: prior key = subgroup|(None)|(Prior)|target
    bl_level_key <- paste0(bl_sg_prefix_str, "|(None)|(Prior)|", nd, "\"")
    bl_level_ev <- paste0("IFERROR(INDEX(", sim_ev_range, ",MATCH(",
      bl_level_key, ",", sim_key_range, ",0)),0)")

    level_ev <- .metric_formula(raw_level_ev, bl_level_ev)

    if (has_freq_shifts) {
      # Key for Mean mode: subgroup|variable|pct_label|weight_mode
      pct_key <- paste0(sg_prefix, var_ref, "&\"|\"&", pct_base_label,
        weight_suffix_fmla)
      pct_row_match <- paste0("MATCH(", pct_key, ",", pct_key_range, ",0)")

      # EV column header = "EV_{focus}_{target}" — target fixed, focus dynamic.
      ev_col_formula_nd <- paste0("\"EV_\"&SUBSTITUTE(", focus_ref,
        ",\" \",\"_\")&\"_", nd, "\"")
      pct_ev_match_nd <- paste0("MATCH(", ev_col_formula_nd, ",",
        pct_header_range, ",0)")

      raw_pct_ev <- paste0("IFERROR(INDEX(", pct_data_range, ",",
        pct_row_match, ",", pct_ev_match_nd, "),0)")

      # Baseline Mean key: subgroup|variable|0 percent|weight_mode
      zero_label <- pct_labels[which(pct_steps == 0)]
      bl_pct_key <- paste0(sg_prefix, var_ref, "&\"|", zero_label, "\"",
        weight_suffix_fmla)
      bl_pct_row <- paste0("MATCH(", bl_pct_key, ",", pct_key_range, ",0)")
      bl_pct_ev <- paste0("IFERROR(INDEX(", pct_data_range, ",",
        bl_pct_row, ",", pct_ev_match_nd, "),0)")

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
