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
    community_lookup = NULL
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

  # Add concatenated key for simple MATCH lookup
  sim_data <- sim_data %>%
    dplyr::mutate(
      Key = paste(Subgroup, Evidence_Variable, Evidence_Level, Target_Variable, sep = "|"),
      .before = 1
    )

  # Identify all P_ columns and pad missing ones with NA
  p_cols <- grep("^P_", names(sim_data), value = TRUE)

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

  sim_sheet <- "_sim_data"
  openxlsx::addWorksheet(wb, sim_sheet)
  openxlsx::writeData(wb, sim_sheet, sim_data, startRow = 1, startCol = 1)

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

  # Column A: Subgroup options
  openxlsx::writeData(wb, sim_lookup, "Subgroup", startRow = 1, startCol = 1)
  for (si in seq_along(names(sg_list))) {
    openxlsx::writeData(wb, sim_lookup, names(sg_list)[si], startRow = si + 1, startCol = 1)
  }

  # Column B: Variable options (all IVs)
  first_ivs <- obj[[first_sg]][["meta"]][["ivs"]]
  openxlsx::writeData(wb, sim_lookup, "Variable", startRow = 1, startCol = 2)
  for (vi in seq_along(first_ivs)) {
    openxlsx::writeData(wb, sim_lookup, first_ivs[vi], startRow = vi + 1, startCol = 2)
  }

  # Column C: Variable labels
  openxlsx::writeData(wb, sim_lookup, "Label", startRow = 1, startCol = 3)
  for (vi in seq_along(first_ivs)) {
    openxlsx::writeData(wb, sim_lookup, .get_label(first_ivs[vi]), startRow = vi + 1, startCol = 3)
  }

  # Columns D+: levels per variable (one column per variable, rows = levels)
  # Header row = variable name, data rows = level values
  openxlsx::writeData(wb, sim_lookup, "Levels Start", startRow = 1, startCol = 4)
  for (vi in seq_along(first_ivs)) {
    iv <- first_ivs[vi]
    lvls <- node_levels[[iv]]
    col <- 3 + vi
    openxlsx::writeData(wb, sim_lookup, iv, startRow = 1, startCol = col)
    for (li in seq_along(lvls)) {
      openxlsx::writeData(wb, sim_lookup, lvls[li], startRow = li + 1, startCol = col)
    }
  }

  # ---------------------------------------------------------------------------
  # 5. Build Simulator dashboard sheet
  # ---------------------------------------------------------------------------

  dash_sheet <- "Simulator"
  openxlsx::addWorksheet(wb, dash_sheet)

  # Styles
  styles <- list(
    title     = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header    = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE),
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

  current_row <- row_dropdowns

  if (has_subgroups) {
    openxlsx::writeData(wb, dash_sheet, "Subgroup: ", startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)
    openxlsx::writeData(wb, dash_sheet, names(sg_list)[1], startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_cell,
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
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_cell,
    rows = current_row, cols = cell_col, stack = TRUE)

  var_range <- paste0(sim_lookup, "!$B$2:$B$", length(first_ivs) + 1)
  openxlsx::dataValidation(wb, dash_sheet,
    col = cell_col, rows = current_row,
    type = "list", value = var_range)
  current_row <- current_row + 1L

  # Level dropdown
  level_cell_col <- cell_col
  level_cell_row <- current_row
  openxlsx::writeData(wb, dash_sheet, "Level: ", startRow = current_row, startCol = label_col)
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
    rows = current_row, cols = label_col, stack = TRUE)
  openxlsx::writeData(wb, dash_sheet, node_levels[[first_ivs[1]]][1],
    startRow = current_row, startCol = cell_col)
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_cell,
    rows = current_row, cols = cell_col, stack = TRUE)

  # Dynamic level dropdown via OFFSET
  var_ref <- paste0("$", num2let(var_cell_col), "$", var_cell_row)
  n_lookup_cols <- 3 + length(first_ivs)
  lookup_header_range <- paste0(sim_lookup, "!$D$1:$", num2let(n_lookup_cols), "$1")

  level_validation_formula <- paste0(
    "OFFSET(", sim_lookup, "!$D$1,1,MATCH(", var_ref, ",",
    lookup_header_range, ",0)-1,COUNTA(OFFSET(", sim_lookup, "!$D$2,0,MATCH(",
    var_ref, ",", lookup_header_range, ",0)-1,", max_levels, ",1)),1)"
  )

  openxlsx::dataValidation(wb, dash_sheet,
    col = level_cell_col, rows = level_cell_row,
    type = "list", value = level_validation_formula)

  # Update row_data_start to be 2 rows after last control
  row_data_start <- current_row + 2L

  # ---------------------------------------------------------------------------
  # Headers: Variable | Community | Label | P(1) | ... | P(n) | Expected Value
  # ---------------------------------------------------------------------------

  has_community <- !is.null(community_lookup)

  # Build leading columns
  leading_cols <- c("Variable")
  if (has_community) leading_cols <- c(leading_cols, "Community")
  leading_cols <- c(leading_cols, "Label")
  n_leading <- length(leading_cols)

  header_names <- leading_cols
  for (pc in p_cols) {
    header_names <- c(header_names, gsub("^P_", "P(", paste0(pc, ")")))
  }
  header_names <- c(header_names, "Expected Value")

  for (hi in seq_along(header_names)) {
    openxlsx::writeData(wb, dash_sheet, header_names[hi],
      startRow = row_data_start, startCol = col_data_start + hi - 1)
  }
  all_header_cols <- seq(col_data_start, col_data_start + length(header_names) - 1)
  openxlsx::addStyle(wb, dash_sheet, style = styles$header,
    rows = row_data_start, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)

  # ---------------------------------------------------------------------------
  # Write target variable names, community, and labels (static)
  # ---------------------------------------------------------------------------

  data_rows <- seq(row_data_start + 1, row_data_start + n_targets)

  for (ri in seq_along(all_nodes)) {
    nd <- all_nodes[ri]
    row <- data_rows[ri]
    ci <- 0
    openxlsx::writeData(wb, dash_sheet, nd, startRow = row, startCol = col_data_start + ci)
    ci <- ci + 1
    if (has_community) {
      comm_name <- if (nd %in% names(community_lookup)) community_lookup[[nd]] else ""
      openxlsx::writeData(wb, dash_sheet, comm_name, startRow = row, startCol = col_data_start + ci)
      ci <- ci + 1
    }
    openxlsx::writeData(wb, dash_sheet, .get_label(nd), startRow = row, startCol = col_data_start + ci)
  }

  # Left-align leading columns
  for (lci in seq_len(n_leading)) {
    openxlsx::addStyle(wb, dash_sheet, style = styles$left,
      rows = data_rows, cols = col_data_start + lci - 1, gridExpand = TRUE, stack = TRUE)
  }

  # ---------------------------------------------------------------------------
  # Probability formulas: INDEX/MATCH against _sim_data using Key column
  # ---------------------------------------------------------------------------

  # References
  var_ref <- paste0("$", num2let(var_cell_col), "$", var_cell_row)
  level_ref <- paste0("$", num2let(level_cell_col), "$", level_cell_row)
  if (has_subgroups) {
    sg_ref <- paste0("$", num2let(sg_cell_col), "$", sg_cell_row)
  }

  n_sim_rows <- nrow(sim_data)

  # Key column is column A in _sim_data (added by dplyr::mutate .before = 1)
  sim_key_range <- paste0(sim_sheet, "!$A$2:$A$", n_sim_rows + 1)

  # P_ column positions in sim_data (Key is col 1, so original cols shifted by 1)
  p_col_positions <- which(names(sim_data) %in% p_cols)

  for (ri in seq_along(all_nodes)) {
    nd <- all_nodes[ri]
    row <- data_rows[ri]

    # Build the key formula: subgroup|variable|level|target_variable
    # Use ""& to coerce level to text (Excel may treat numeric dropdown values as numbers)
    if (has_subgroups) {
      key_formula <- paste0(sg_ref, "&\"|\"&", var_ref, "&\"|\"&\"\"&", level_ref, "&\"|", nd, "\"")
    } else {
      key_formula <- paste0("\"", names(sg_list)[1], "|\"&", var_ref, "&\"|\"&\"\"&", level_ref, "&\"|", nd, "\"")
    }

    match_formula <- paste0("MATCH(", key_formula, ",", sim_key_range, ",0)")

    for (pi in seq_along(p_cols)) {
      col <- col_data_start + n_leading + pi - 1
      sim_p_col <- paste0(sim_sheet, "!$", num2let(p_col_positions[pi]), "$2:$",
        num2let(p_col_positions[pi]), "$", n_sim_rows + 1)

      cell_formula <- paste0("IFERROR(INDEX(", sim_p_col, ",", match_formula, "),\"\")")

      openxlsx::writeFormula(wb, dash_sheet, x = cell_formula,
        startRow = row, startCol = col)
    }
  }

  # Expected Value column: SUMPRODUCT(level_values, probabilities)
  ev_col <- col_data_start + n_leading + length(p_cols)

  # Extract numeric level values from P_ column headers
  level_names <- gsub("^P_", "", p_cols)
  level_values_str <- paste(level_names, collapse = ",")
  level_array <- paste0("{", level_values_str, "}")

  for (ri in seq_along(all_nodes)) {
    row <- data_rows[ri]
    first_p <- paste0(num2let(col_data_start + n_leading), row)
    last_p <- paste0(num2let(col_data_start + n_leading + length(p_cols) - 1), row)
    p_range <- paste0(first_p, ":", last_p)

    ev_formula <- paste0("IFERROR(SUMPRODUCT(", level_array, ",", p_range, "),\"\")")
    openxlsx::writeFormula(wb, dash_sheet, x = ev_formula,
      startRow = row, startCol = ev_col)
  }

  openxlsx::addStyle(wb, dash_sheet, style = openxlsx::createStyle(numFmt = "0.00", halign = "center"),
    rows = data_rows, cols = ev_col, gridExpand = TRUE, stack = TRUE)

  # Format probability columns
  p_data_cols <- seq(col_data_start + n_leading, col_data_start + n_leading + length(p_cols) - 1)
  openxlsx::addStyle(wb, dash_sheet, style = styles$pct,
    rows = data_rows, cols = p_data_cols, gridExpand = TRUE, stack = TRUE)

  # all_header_cols includes everything through Expected Value
  all_header_cols <- seq(col_data_start, ev_col)

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

  # Hide helper sheets
  openxlsx::sheetVisibility(wb)[which(names(wb) == sim_sheet)] <- FALSE
  openxlsx::sheetVisibility(wb)[which(names(wb) == sim_lookup)] <- "veryHidden"

  invisible(wb)
}
