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
    shift_range = c(-0.50, 0.50),
    shift_step = 0.025
) {

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

    # Helper: extract probabilities from querygrain result (simplify = FALSE)
    .extract_probs <- function(result) {
      purrr::map(result, function(x) {
        p <- as.numeric(x)
        names(p) <- dimnames(x)[[1]]
        p
      })
    }

    # Query with no evidence (marginal/prior)
    marginal <- gRain::querygrain(grain_bn, nodes = all_nodes, simplify = FALSE)
    marginal <- .extract_probs(marginal)

    prior_rows <- purrr::imap(marginal, function(probs, nm) {
      row <- tibble::tibble(
        Subgroup = sg,
        Evidence_Variable = "(None)",
        Evidence_Level = "(Prior)",
        Target_Variable = nm
      )
      for (li in seq_along(probs)) {
        row[[paste0("P_", names(probs)[li])]] <- probs[li]
      }
      row
    }) %>% dplyr::bind_rows()

    all_posteriors[[paste0(sg, "_prior")]] <- prior_rows

    # Query for each IV x each level
    for (iv in ivs) {
      iv_levels <- node_levels[[iv]]

      for (lv in iv_levels) {
        evidence <- rlang::set_names(list(lv), iv)

        posterior <- tryCatch(
          gRain::querygrain(grain_bn, nodes = all_nodes, evidence = evidence, simplify = FALSE),
          error = function(e) NULL
        )

        if (is.null(posterior)) next

        posterior <- .extract_probs(posterior)

        # Force evidence variable to point mass at selected level
        ev_probs <- rep(0, length(node_levels[[iv]]))
        names(ev_probs) <- node_levels[[iv]]
        ev_probs[lv] <- 1
        posterior[[iv]] <- ev_probs

        rows <- purrr::imap(posterior, function(probs, nm) {
          row <- tibble::tibble(
            Subgroup = sg,
            Evidence_Variable = iv,
            Evidence_Level = lv,
            Target_Variable = nm
          )
          for (li in seq_along(probs)) {
            row[[paste0("P_", names(probs)[li])]] <- probs[li]
          }
          row
        }) %>% dplyr::bind_rows()

        all_posteriors[[paste0(sg, "_", iv, "_", lv)]] <- rows
      }
    }
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

      # Build EV matrix once per subgroup (shared across focus options)
      ev_mats <- purrr::map(rlang::set_names(ivs), function(iv) {
        iv_levels <- node_levels_sg[[iv]]
        n_levels <- length(iv_levels)
        n_nodes <- length(all_nodes_sg)
        ev_mat <- matrix(NA_real_, nrow = n_levels, ncol = n_nodes)
        for (li in seq_along(iv_levels)) {
          lv <- iv_levels[li]
          for (ni in seq_along(all_nodes_sg)) {
            key <- paste(sg, iv, lv, all_nodes_sg[ni], sep = "|")
            idx <- sim_key_idx[key]
            if (!is.na(idx)) {
              probs <- as.numeric(sim_data[idx, p_cols])
              ev_mat[li, ni] <- sum(nd_levels_numeric * probs, na.rm = TRUE)
            }
          }
        }
        ev_mat
      })

      # Process Market first to get base table with Key
      .compute_focus <- function(focus_df, focus_name) {
        focus_clean <- gsub(" ", "_", focus_name)
        ev_col_name <- paste0("EV_", focus_clean)
        mean_col_name <- paste0("Mean_", focus_clean)

        purrr::map(ivs, function(iv) {
          iv_freq <- table(focus_df[[iv]])
          iv_levels <- node_levels_sg[[iv]]
          iv_values <- as.numeric(iv_levels)
          ev_mat <- ev_mats[[iv]]

          shifted_list <- purrr::map(pct_steps, function(pct) {
            tryCatch(
              as.numeric(bn_freq_prob_shift(freq = iv_freq, lift = pct,
                impact_metric_type = "proportional")),
              error = function(e) NULL
            )
          })

          purrr::imap(shifted_list, function(shifted_probs, pi) {
            if (is.null(shifted_probs) || anyNA(shifted_probs)) return(NULL)

            weighted_evs <- as.numeric(shifted_probs %*% ev_mat)
            shifted_input_mean <- sum(iv_values * shifted_probs)
            pct_label <- pct_labels[pi]

            tibble::tibble(
              Key = paste(sg, iv, pct_label, all_nodes_sg, sep = "|"),
              !!mean_col_name := shifted_input_mean,
              !!ev_col_name := weighted_evs
            )
          }) %>% dplyr::bind_rows()
        }) %>% dplyr::bind_rows()
      }

      # Start with Market
      sg_result <- .compute_focus(sg_df, "Market")

      # Join each brand
      for (focus in setdiff(focus_options, "Market")) {
        focus_df <- sg_df[sg_df[[brand]] == focus, , drop = FALSE]
        focus_result <- .compute_focus(focus_df, focus)
        sg_result <- sg_result %>%
          dplyr::left_join(focus_result, by = "Key")
      }

      sg_result
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

  sim_sheet <- "_sim_data"
  openxlsx::addWorksheet(wb, sim_sheet)
  openxlsx::writeData(wb, sim_sheet, sim_data, startRow = 1, startCol = 1)

  pct_sheet <- "_sim_pct_data"
  if (has_freq_shifts) {
    # Keep Key + all EV_ and Mean_ columns
    keep_cols <- c("Key", grep("^EV_|^Mean_", names(pct_data), value = TRUE))
    pct_data <- pct_data[, keep_cols]
    n_pct_rows <- nrow(pct_data)
    openxlsx::addWorksheet(wb, pct_sheet)
    openxlsx::writeData(wb, pct_sheet, pct_data, startRow = 1, startCol = 1)
    n_pct_rows <- nrow(pct_data)
  }

  # ---------------------------------------------------------------------------
  # 4. Write lookup sheet for simulator dropdowns
  # ---------------------------------------------------------------------------

  sim_lookup <- "_sim_lookup"
  openxlsx::addWorksheet(wb, sim_lookup)

  # Get all nodes from first subgroup for variable list
  first_sg <- names(sg_list)[1]
  first_fit <- obj[[first_sg]][["fit"]]
  all_nodes <- names(first_fit)
  node_levels <- purrr::map(rlang::set_names(all_nodes), function(nd) {
    dimnames(first_fit[[nd]][["prob"]])[[1]]
  })
  max_levels <- max(purrr::map_int(node_levels, length))

  # Sequential column counter for _sim_lookup
  lk_col <- 1L

  # Subgroup options
  sg_col <- lk_col
  openxlsx::writeData(wb, sim_lookup, "Subgroup", startRow = 1, startCol = lk_col)
  for (si in seq_along(names(sg_list))) {
    openxlsx::writeData(wb, sim_lookup, names(sg_list)[si], startRow = si + 1, startCol = lk_col)
  }
  lk_col <- lk_col + 1L

  # Variable options (all IVs)
  first_ivs <- obj[[first_sg]][["meta"]][["ivs"]]
  var_col <- lk_col
  openxlsx::writeData(wb, sim_lookup, "Variable", startRow = 1, startCol = lk_col)
  for (vi in seq_along(first_ivs)) {
    openxlsx::writeData(wb, sim_lookup, first_ivs[vi], startRow = vi + 1, startCol = lk_col)
  }
  lk_col <- lk_col + 1L

  # Variable labels
  label_lk_col <- lk_col
  openxlsx::writeData(wb, sim_lookup, "Label", startRow = 1, startCol = lk_col)
  for (vi in seq_along(first_ivs)) {
    openxlsx::writeData(wb, sim_lookup, .get_label(first_ivs[vi]), startRow = vi + 1, startCol = lk_col)
  }
  lk_col <- lk_col + 1L

  # Levels per variable (one column per variable, rows = levels)
  levels_start_col <- lk_col
  for (vi in seq_along(first_ivs)) {
    iv <- first_ivs[vi]
    lvls <- node_levels[[iv]]
    openxlsx::writeData(wb, sim_lookup, iv, startRow = 1, startCol = lk_col)
    for (li in seq_along(lvls)) {
      openxlsx::writeData(wb, sim_lookup, as.numeric(lvls[li]), startRow = li + 1, startCol = lk_col)
    }
    lk_col <- lk_col + 1L
  }
  levels_end_col <- lk_col - 1L
  n_lookup_cols <- levels_end_col

  # Mode options
  mode_options <- if (has_freq_shifts) c("Level", "Mean") else "Level"
  mode_col <- lk_col
  openxlsx::writeData(wb, sim_lookup, "Mode", startRow = 1, startCol = lk_col)
  for (mi in seq_along(mode_options)) {
    openxlsx::writeData(wb, sim_lookup, mode_options[mi], startRow = mi + 1, startCol = lk_col)
  }
  lk_col <- lk_col + 1L

  # Percent change steps — base labels (used for key matching)
  pct_col <- NULL
  if (has_freq_shifts) {
    pct_col <- lk_col
    openxlsx::writeData(wb, sim_lookup, "Mean", startRow = 1, startCol = lk_col)
    for (pi in seq_along(pct_labels)) {
      openxlsx::writeData(wb, sim_lookup, pct_labels[pi], startRow = pi + 1, startCol = lk_col)
    }
    lk_col <- lk_col + 1L
  }

  # Focus options (for Mean mode)
  focus_opt_col <- NULL
  if (has_freq_shifts && !is.null(brand) && !is.null(brand_names)) {
    focus_opt_col <- lk_col
    sim_focus_options <- c("Market", brand_names)
    openxlsx::writeData(wb, sim_lookup, "Focus", startRow = 1, startCol = lk_col)
    for (fi in seq_along(sim_focus_options)) {
      openxlsx::writeData(wb, sim_lookup, sim_focus_options[fi], startRow = fi + 1, startCol = lk_col)
    }
    lk_col <- lk_col + 1L
  }

  # Metric options
  metric_options <- c("Absolute", "Percent Change", "Absolute Change")
  metric_opt_col <- lk_col
  openxlsx::writeData(wb, sim_lookup, "Metric", startRow = 1, startCol = lk_col)
  for (moi in seq_along(metric_options)) {
    openxlsx::writeData(wb, sim_lookup, metric_options[moi], startRow = moi + 1, startCol = lk_col)
  }
  lk_col <- lk_col + 1L

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
  for (li in seq_len(max_levels)) {
    level_formula <- paste0(
      "IFERROR(INDEX(", sim_lookup, "!$", levels_start_let, "$", li + 1,
      ":$", levels_end_let, "$", li + 1,
      ",1,MATCH(", var_ref_from_lookup, ",", sim_lookup, "!$", levels_start_let,
      "$1:$", levels_end_let, "$1,0)),\"\")"
    )
    openxlsx::writeFormula(wb, sim_lookup, x = level_formula,
      startRow = li + 1, startCol = active_level_col)
  }

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
    for (oi in seq_len(n_option_rows)) {
      level_cell <- paste0("$", active_level_let, "$", oi + 1)
      if (oi <= length(pct_labels)) {
        # Base pct label from the Percent Change column (no mean suffix)
        base_pct <- paste0("$", pct_let, "$", oi + 1)

        opt_formula <- paste0(
          "IF(", mode_ref_from_lookup, "=\"Mean\",", base_pct, ",",
          "IF(", level_cell, "=\"\",\"\",", level_cell, "))"
        )
      } else {
        opt_formula <- paste0(
          "IF(", mode_ref_from_lookup, "=\"Mean\",\"\",",
          "IF(", level_cell, "=\"\",\"\",", level_cell, "))"
        )
      }
      openxlsx::writeFormula(wb, sim_lookup, x = opt_formula,
        startRow = oi + 1, startCol = active_options_col)
    }

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

    # Base key (0 percent): subgroup|variable|0_label|variable
    zero_label <- pct_labels[which(pct_steps == 0)]
    base_mean_key <- paste0(sg_ref_d, "&\"|\"&", var_ref_d, "&\"|", zero_label, "|\"&", var_ref_d)
    # Shifted key: subgroup|variable|selected_pct|variable
    shifted_mean_key <- paste0(sg_ref_d, "&\"|\"&", var_ref_d, "&\"|\"&", level_ref_display, "&\"|\"&", var_ref_d)

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

  has_community <- !is.null(community_nodes)

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

  for (hi in seq_along(header_names)) {
    openxlsx::writeData(wb, dash_sheet, header_names[hi],
      startRow = row_data_start, startCol = col_data_start + hi - 1)
  }
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

  for (ri in seq_along(all_nodes)) {
    nd <- all_nodes[ri]
    row <- data_rows[ri]
    ci <- 0
    openxlsx::writeData(wb, dash_sheet, nd, startRow = row, startCol = col_data_start + ci)
    ci <- ci + 1
    if (has_community) {
      comm_name <- if (nd %in% names(community_lookup)) as.character(community_lookup[[nd]]) else ""
      openxlsx::writeData(wb, dash_sheet, comm_name, startRow = row, startCol = col_data_start + ci)
      ci <- ci + 1

      # Apply community color as background fill across the row
      if (nd %in% names(community_color)) {
        fill_color <- as.character(community_color[[nd]])
        if (grepl("^#[0-9A-Fa-f]{6}$", fill_color)) {
          comm_style <- openxlsx::createStyle(fgFill = fill_color)
          openxlsx::addStyle(wb, dash_sheet, style = comm_style,
            rows = row, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)
        }
      }
    }
    openxlsx::writeData(wb, dash_sheet, .get_label(nd), startRow = row, startCol = col_data_start + ci)
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

    # Build EV column name dynamically: "EV_" & SUBSTITUTE(focus, " ", "_")
    ev_col_formula <- paste0("\"EV_\"&SUBSTITUTE(", focus_ref, ",\" \",\"_\")")
    mean_col_formula <- paste0("\"Mean_\"&SUBSTITUTE(", focus_ref, ",\" \",\"_\")")

    # Match the column name against pct_data headers
    pct_header_range <- paste0(pct_sheet, "!$1:$1")
    pct_ev_match <- paste0("MATCH(", ev_col_formula, ",", pct_header_range, ",0)")
    pct_mean_match <- paste0("MATCH(", mean_col_formula, ",", pct_header_range, ",0)")

    # Dynamic column INDEX
    pct_data_range <- paste0(pct_sheet, "!$A$2:$", num2let(ncol(pct_data)), "$", n_pct_rows + 1)
  }

  # Helper: wrap raw EV + baseline EV into metric formula
  .metric_formula <- function(raw_ev, baseline_ev) {
    paste0(
      "IF(", metric_sim_ref, "=\"Absolute\",", raw_ev, ",",
      "IF(", metric_sim_ref, "=\"Absolute Change\",", raw_ev, "-", baseline_ev, ",",
      "TEXT(ROUND((", raw_ev, "-", baseline_ev, ")/", baseline_ev, "*100,1),\"0.0\")&\"%\"))"
    )
  }

  for (ri in seq_along(all_nodes)) {
    nd <- all_nodes[ri]
    row <- data_rows[ri]

    # Key for Level mode
    if (has_subgroups) {
      key_formula <- paste0(sg_ref, "&\"|\"&", var_ref, "&\"|\"&\"\"&", level_ref, "&\"|", nd, "\"")
    } else {
      key_formula <- paste0("\"", names(sg_list)[1], "|\"&", var_ref, "&\"|\"&\"\"&", level_ref, "&\"|", nd, "\"")
    }
    match_formula <- paste0("MATCH(", key_formula, ",", sim_key_range, ",0)")
    raw_level_ev <- paste0("IFERROR(INDEX(", sim_ev_range, ",", match_formula, "),0)")

    # Baseline for Level mode: prior key = subgroup|(None)|(Prior)|target
    if (has_subgroups) {
      bl_level_key <- paste0(sg_ref, "&\"|(None)|(Prior)|", nd, "\"")
    } else {
      bl_level_key <- paste0("\"", names(sg_list)[1], "|(None)|(Prior)|", nd, "\"")
    }
    bl_level_ev <- paste0("IFERROR(INDEX(", sim_ev_range, ",MATCH(", bl_level_key, ",", sim_key_range, ",0)),0)")

    level_ev <- .metric_formula(raw_level_ev, bl_level_ev)

    if (has_freq_shifts) {
      # Key for Mean mode: subgroup|variable|pct_label|target (no focus — focus selects column)
      if (has_subgroups) {
        pct_key <- paste0(sg_ref, "&\"|\"&", var_ref, "&\"|\"&", pct_base_label, "&\"|", nd, "\"")
      } else {
        pct_key <- paste0("\"", names(sg_list)[1], "|\"&", var_ref, "&\"|\"&", pct_base_label, "&\"|", nd, "\"")
      }
      pct_row_match <- paste0("MATCH(", pct_key, ",", pct_key_range, ",0)")
      # INDEX into the dynamic EV column for the selected focus
      raw_pct_ev <- paste0("IFERROR(INDEX(", pct_data_range, ",", pct_row_match, ",", pct_ev_match, "),0)")

      # Baseline for Mean mode: subgroup|variable|0 percent|target
      zero_label <- pct_labels[which(pct_steps == 0)]
      if (has_subgroups) {
        bl_pct_key <- paste0(sg_ref, "&\"|\"&", var_ref, "&\"|", zero_label, "|", nd, "\"")
      } else {
        bl_pct_key <- paste0("\"", names(sg_list)[1], "|\"&", var_ref, "&\"|", zero_label, "|", nd, "\"")
      }
      bl_pct_row <- paste0("MATCH(", bl_pct_key, ",", pct_key_range, ",0)")
      bl_pct_ev <- paste0("IFERROR(INDEX(", pct_data_range, ",", bl_pct_row, ",", pct_ev_match, "),0)")

      mean_ev <- .metric_formula(raw_pct_ev, bl_pct_ev)

      ev_formula <- paste0("IFERROR(IF(", is_mean_mode, ",", mean_ev, ",", level_ev, "),\"\")")
    } else {
      ev_formula <- paste0("IFERROR(", level_ev, ",\"\")")
    }

    openxlsx::writeFormula(wb, dash_sheet, x = ev_formula,
      startRow = row, startCol = ev_col)
  }

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

  # Freeze panes
  openxlsx::freezePane(wb, dash_sheet,
    firstActiveRow = row_data_start + 1,
    firstActiveCol = col_data_start + n_leading)

  # Filter
  openxlsx::addFilter(wb, dash_sheet, rows = row_data_start, cols = all_header_cols)

  # Hide helper sheets — visibility set by bn_impact_write after all sheets added

  invisible(wb)
}
