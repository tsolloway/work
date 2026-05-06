#' bn_prioritize_write
#'
#' @description Takes the output of \code{bn_prioritize()} or
#'   \code{bn_prioritizations()} and writes a formatted Excel workbook with
#'   a dynamic dashboard. Dropdown controls for strategy, search, subgroup,
#'   focus (brand), and weight switch between hidden data sheets. Controls
#'   are only shown when multiple options exist for that dimension.
#'
#' @param result Output of \code{bn_prioritize()} (a single tibble) or
#'   \code{bn_prioritizations()} (a list of results with meta).
#' @param title Character or NULL. Title displayed in the sheet header. If NULL,
#'   defaults to \code{"Prioritization Analysis of {dv}"} using the DV display
#'   name from \code{result$meta$dv}. Default NULL.
#' @param sub_title Character or NULL. Subtitle text displayed in the sheet
#'   header (e.g. project name). If NULL, inherits from \code{file_name}.
#'   Default NULL.
#' @param file_name Character or NULL. Prefix for output file name. If NULL,
#'   inherits from \code{sub_title}. Default NULL.
#' @param variable_width Numeric. Column width for the Variable column.
#'   Default 20.
#' @param community_width Numeric. Column width for the Community column.
#'   Default 20.
#' @param label_width Numeric. Column width for the Label column. Default 20.
#' @param combo_width Numeric. Column width for the Combo column.
#'   Default 40.
#' @param sig_threshold Numeric or NULL. P-value threshold for green
#'   (significant). If NULL, inherits from \code{result$meta$sig_threshold}
#'   (set at \code{bn_prioritizations()} / \code{bn_finalize_network()}
#'   time); falls back to 0.05.
#' @param marginal_threshold Numeric or NULL. P-value threshold for orange
#'   (marginal). If NULL, inherits from
#'   \code{result$meta$marginal_threshold}; falls back to 0.10.
#' @param lift Numeric. The lift fraction used in the prioritization analysis,
#'   displayed in the footer. Default 0.10 (10 percent).
#' @param min_base_for_boot Integer or NULL. Threshold used for the base /
#'   warning feedback shown next to the Focus dropdown. When the current
#'   selection's base is at or above this value, the cell reads
#'   \code{"Base: N"}; below it the cell reads
#'   \code{"Results not calculated because base is below N"} in red, and the Focus
#'   dropdown cell itself turns red. If NULL, inherits from
#'   \code{result$meta$min_base_for_boot} (falling back to 100).
#' @param very_hide_all Logical. If TRUE (default), all sheets except
#'   Prioritization are set to veryHidden. If FALSE, they are simply hidden.
#' @param path Character. Directory to write workbook to. Default \code{"."}.
#' @param wb openxlsx workbook object or NULL. When NULL (default), a new
#'   workbook is created. When provided, prioritization sheets are appended
#'   to the existing workbook — used by \code{bn_write()}.
#' @param save Logical. When TRUE (default), the workbook is saved to disk.
#'   When FALSE, the workbook is returned without saving.
#' @param add_guide Logical. When TRUE (default), a Guide tab is appended.
#'   When FALSE, no Guide is added — used by \code{bn_write()} which builds
#'   a single unified Guide for the combined workbook.
#'
#' @return Workbook object (invisibly).
#'
#' @seealso [bn_prioritize()], [bn_prioritizations()]
#'
#' @export
bn_prioritize_write <- function(
    result,
    title = NULL,
    sub_title = NULL,
    file_name = NULL,
    variable_width = 20,
    community_width = 20,
    label_width = 20,
    combo_width = 40,
    sig_threshold = NULL,
    marginal_threshold = NULL,
    lift = 0.10,
    min_base_for_boot = NULL,
    very_hide_all = TRUE,
    path = ".",
    wb = NULL,
    save = TRUE,
    add_guide = TRUE
) {

  # Resolve min_base threshold for the base/warning display next to Focus.
  # Pull from meta when not supplied; fall back to 100 (matches
  # min_base_for_calc default downstream).
  if (is.null(min_base_for_boot)) {
    min_base_for_boot <- result[["meta"]][["min_base_for_boot"]] %||% 100L
  }
  if (is.null(sig_threshold)) {
    sig_threshold <- result[["meta"]][["sig_threshold"]] %||% 0.05
  }
  if (is.null(marginal_threshold)) {
    marginal_threshold <- result[["meta"]][["marginal_threshold"]] %||% 0.10
  }

  if (is.null(file_name)) file_name <- sub_title
  if (is.null(sub_title)) sub_title <- file_name

  if (is.null(title)) {
    dv <- result[["meta"]][["dv"]]
    dv_display <- if (!is.null(names(dv))) names(dv) else dv
    title <- if (!is.null(dv_display)) {
      paste("Prioritization Analysis of", dv_display)
    } else {
      "Prioritization Analysis"
    }
  }

  if (is.null(wb)) wb <- oxl_create_workbook()
  pre_sheets <- names(wb)

  # Strategy display labels — must match what .prioritize_build_registry()
  # writes into the registry (and therefore into _priorit_lookup) so that
  # the "Max-strategy → force focus/weight" Excel formulas compare apples
  # to apples. Pull lift% from meta so the label reflects the actual shift.
  meta_lift <- result[["meta"]][["lift"]]
  lift_pct_label <- if (!is.null(meta_lift)) round(meta_lift * 100) else round(lift * 100)
  lift_label            <- paste0("Moderate Lift (", lift_pct_label, "%)")
  max_label             <- "Maximum Lift"
  max_deprecated_label  <- "Maximum Lift (Deprecated)"

  # ---------------------------------------------------------------------------
  # Build registry: flat list of tagged entries
  # ---------------------------------------------------------------------------
  registry <- .prioritize_build_registry(result)

  # Detect whether the outcome is binary (top_box / 0-1 probability) by
  # checking whether every dv_estimate falls in [0, 1]. When true, the
  # absolute metric columns (Outcome Estimate, Cumulative Gain, Incremental
  # Lift) render as XX.X% instead of XX.XX. Pct columns are already in
  # XX.X% form.
  is_binary_outcome <- {
    dv_vals <- unlist(purrr::map(registry, function(e) {
      if (is.null(e$tbl) || !is.data.frame(e$tbl)) return(numeric(0))
      as.numeric(e$tbl$dv_estimate)
    }))
    dv_vals <- dv_vals[!is.na(dv_vals)]
    length(dv_vals) > 0 && min(dv_vals) >= -1e-6 && max(dv_vals) <= 1 + 1e-6
  }

  # Phantom entries (skipped slices) have tbl = NULL — they only exist to
  # populate _lookup for the Focus warning/base display. Guard all NULL-tbl
  # cases in the registry scans below.
  has_p <- any(purrr::map_lgl(registry, function(e) {
    !is.null(e$tbl) && "p_value" %in% names(e$tbl)
  }))
  has_community <- any(purrr::map_lgl(registry, function(e) {
    !is.null(e$tbl) && "community" %in% names(e$tbl)
  }))
  max_rows <- max(purrr::map_int(registry, function(e) {
    if (is.null(e$tbl)) 0L else as.integer(nrow(e$tbl))
  }))
  n_entries <- length(registry)

  # Determine which dimensions have multiple values
  dims <- list(
    strategy = unique(purrr::map_chr(registry, "strategy")),
    search   = unique(purrr::map_chr(registry, "search")),
    subgroup = unique(purrr::map_chr(registry, "subgroup")),
    focus    = unique(purrr::map_chr(registry, "focus")),
    weight   = unique(purrr::map_chr(registry, "weight"))
  )
  active_dims <- names(dims)[purrr::map_int(dims, length) > 1]

  # ---------------------------------------------------------------------------
  # 1. Prioritization sheet (created first)
  # ---------------------------------------------------------------------------
  # No tabColour — let Excel render the default tab strip color, matching
  # the Network Drivers / Simulator dashboards. (Was previously "#FFFFFF",
  # which painted the tab invisible against Excel's default tab background.)
  openxlsx::addWorksheet(wb, "Prioritization", gridLines = FALSE)

  # ---------------------------------------------------------------------------
  # 2. Write hidden data sheets. Phantom entries (skipped slices) have no
  # tibble and no sheet — they only populate _lookup so the Focus dropdown
  # can resolve base/warning lookups.
  # ---------------------------------------------------------------------------
  for (entry in registry) {
    sn <- entry$sheet_name
    if (is.null(entry$tbl) || is.na(sn)) next
    openxlsx::addWorksheet(wb, sn, gridLines = FALSE)
    openxlsx::writeData(wb, sn, entry$tbl, startRow = 1, startCol = 1)
  }

  # ---------------------------------------------------------------------------
  # 3. Write _lookup sheet
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "_priorit_lookup", gridLines = FALSE)

  # Columns: Strategy | Search | Subgroup | Focus | Weight | Sheet | Base | Key
  lookup_headers <- c("Strategy", "Search", "Subgroup", "Focus", "Weight",
                       "Sheet", "Base", "Key")
  for (ci in seq_along(lookup_headers)) {
    openxlsx::writeData(wb, "_priorit_lookup", lookup_headers[ci], startRow = 1, startCol = ci)
  }

  for (i in seq_along(registry)) {
    r <- i + 1
    e <- registry[[i]]
    openxlsx::writeData(wb, "_priorit_lookup", e$strategy, startRow = r, startCol = 1)
    openxlsx::writeData(wb, "_priorit_lookup", e$search, startRow = r, startCol = 2)
    openxlsx::writeData(wb, "_priorit_lookup", e$subgroup, startRow = r, startCol = 3)
    openxlsx::writeData(wb, "_priorit_lookup", e$focus, startRow = r, startCol = 4)
    openxlsx::writeData(wb, "_priorit_lookup", e$weight, startRow = r, startCol = 5)
    openxlsx::writeData(wb, "_priorit_lookup", e$sheet_name, startRow = r, startCol = 6)
    openxlsx::writeData(wb, "_priorit_lookup", if (!is.null(e$n_obs)) e$n_obs else NA_integer_,
      startRow = r, startCol = 7)
  }

  # Key column: concatenation of all 5 dims
  for (i in seq_along(registry)) {
    r <- i + 1
    key_formula <- paste0("A", r, "&\"|\"&B", r, "&\"|\"&C", r, "&\"|\"&D", r, "&\"|\"&E", r)
    openxlsx::writeFormula(wb, "_priorit_lookup", x = key_formula, startRow = r, startCol = 8)
  }

  # Unique option lists for dropdowns (columns I onward)
  opt_col <- 9L
  dim_names <- c("strategy", "search", "subgroup", "focus", "weight")
  dim_labels <- c("Strategy", "Search", "Subgroup", "Focus", "Weight")
  dim_opt_cols <- list()  # track which lookup column has options for each dim

  for (di in seq_along(dim_names)) {
    dn <- dim_names[di]
    if (dn %in% active_dims) {
      vals <- dims[[dn]]
      openxlsx::writeData(wb, "_priorit_lookup", dim_labels[di], startRow = 1, startCol = opt_col)
      for (vi in seq_along(vals)) {
        openxlsx::writeData(wb, "_priorit_lookup", vals[vi], startRow = vi + 1, startCol = opt_col)
      }
      dim_opt_cols[[dn]] <- list(col = opt_col, n = length(vals))
      opt_col <- opt_col + 1L
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Build Prioritization
  # ---------------------------------------------------------------------------
  styles <- list(
    title      = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title  = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header     = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE, fgFill = "#D9D9D9"),
    center_int = openxlsx::createStyle(numFmt = "0", halign = "center"),
    # When the outcome is binary (probability in [0, 1]), the absolute
    # columns render as XX.X% (multiplied by 100); otherwise XX.XX.
    center_2dp = openxlsx::createStyle(
      numFmt = if (is_binary_outcome) "0.0%" else "0.00",
      halign = "center"),
    center_pct = openxlsx::createStyle(numFmt = "0.0%", halign = "center"),
    left       = openxlsx::createStyle(halign = "left"),
    baseline   = openxlsx::createStyle(textDecoration = "italic", fgFill = "#F2F2F2"),
    p_green    = openxlsx::createStyle(fontColour = "#2E7D32", textDecoration = "bold"),
    p_orange   = openxlsx::createStyle(fontColour = "#E65100"),
    p_red      = openxlsx::createStyle(fontColour = "#B71C1C"),
    dropdown_label = openxlsx::createStyle(textDecoration = "bold", halign = "right"),
    dropdown_cell  = openxlsx::createStyle(border = "Bottom", borderStyle = "thin", halign = "left")
  )

  dash <- "Prioritization"
  col_data_start <- 2L

  # Title
  openxlsx::writeData(wb, dash, title, startRow = 2L, startCol = col_data_start)
  openxlsx::addStyle(wb, dash, style = styles$title,
    rows = 2L, cols = col_data_start, stack = TRUE)

  # Subtitle
  if (!is.null(sub_title)) {
    openxlsx::writeData(wb, dash, sub_title, startRow = 3L, startCol = col_data_start)
    openxlsx::addStyle(wb, dash, style = styles$sub_title,
      rows = 3L, cols = col_data_start, stack = TRUE)
  }

  # --- Dropdown controls (stacked vertically in C/D) ---
  row_controls_start <- if (!is.null(sub_title)) 5L else 4L
  label_col <- 3L  # Column C
  cell_col <- 4L   # Column D

  dropdown_refs <- list()
  current_row <- row_controls_start
  focus_cell_row <- NULL   # captured during the loop for the base/focus warning
  weight_cell_row <- NULL  # captured for the "Weights don't affect this strategy" warning

  dim_display_labels <- c(
    strategy = "Analysis:",
    search   = "Search:",
    subgroup = "Subgroup:",
    focus    = "Focus:",
    weight   = "Weight:"
  )

  for (dn in active_dims) {
    info <- dim_opt_cols[[dn]]
    default_val <- dims[[dn]][1]

    # Label in C
    openxlsx::writeData(wb, dash, dim_display_labels[[dn]],
      startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)

    # Cell in D
    openxlsx::writeData(wb, dash, default_val,
      startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash, style = styles$dropdown_cell,
      rows = current_row, cols = cell_col, stack = TRUE)

    # Data validation
    opt_range <- paste0("_priorit_lookup!$", num2let(info$col), "$2:$",
      num2let(info$col), "$", info$n + 1)
    openxlsx::dataValidation(wb, dash,
      col = cell_col, rows = current_row,
      type = "list", value = opt_range)

    dropdown_refs[[dn]] <- paste0("$", num2let(cell_col), "$", current_row)

    if (dn == "focus")  focus_cell_row  <- current_row
    if (dn == "weight") weight_cell_row <- current_row

    current_row <- current_row + 1L
  }

  # --- Display-mode dropdown (always shown, UI-only — doesn't touch the
  # registry / key lookup, only controls which numbers feed the chart). ---
  # Default = "Percent Change" (cumulative gain %). "Point Change"
  # reproduces the original DV-Estimate-based stacked chart.
  chart_options <- c("Percent Change", "Point Change")
  # Place chart options on _priorit_lookup right after the existing dim opts
  chart_opt_col <- opt_col
  openxlsx::writeData(wb, "_priorit_lookup", "Display",
    startRow = 1, startCol = chart_opt_col)
  for (vi in seq_along(chart_options)) {
    openxlsx::writeData(wb, "_priorit_lookup", chart_options[vi],
      startRow = vi + 1, startCol = chart_opt_col)
  }
  opt_col <- opt_col + 1L

  chart_cell_row <- current_row
  openxlsx::writeData(wb, dash, "Display:",
    startRow = current_row, startCol = label_col)
  openxlsx::addStyle(wb, dash, style = styles$dropdown_label,
    rows = current_row, cols = label_col, stack = TRUE)
  openxlsx::writeData(wb, dash, chart_options[1],
    startRow = current_row, startCol = cell_col)
  openxlsx::addStyle(wb, dash, style = styles$dropdown_cell,
    rows = current_row, cols = cell_col, stack = TRUE)
  chart_opt_range <- paste0("_priorit_lookup!$", num2let(chart_opt_col),
    "$2:$", num2let(chart_opt_col), "$", length(chart_options) + 1)
  openxlsx::dataValidation(wb, dash,
    col = cell_col, rows = current_row,
    type = "list", value = chart_opt_range)
  chart_cell_ref <- paste0("Prioritization!$", num2let(cell_col), "$", current_row)
  current_row <- current_row + 1L

  openxlsx::setColWidths(wb, dash, cols = label_col, widths = 10)
  openxlsx::setColWidths(wb, dash, cols = cell_col, widths = 14)

  last_control_row <- current_row - 1L

  # --- Build key formula from dropdown refs ---
  # For dimensions with only one value, hardcode that value in the key.
  # When the strategy dropdown is active and the user picks "Max", force
  # focus="Market" and weight="Unweighted" in the key — Max is registered
  # only at that combination (it's brand-invariant and weight-invariant
  # by definition), so without this override switching Strategy → Max
  # while Focus or Weight is on something else produces a key that
  # doesn't exist in _priorit_lookup → empty table.
  strategy_active <- "strategy" %in% active_dims
  strategy_ref <- if (strategy_active) {
    paste0("Prioritization!", dropdown_refs[["strategy"]])
  } else {
    paste0("\"", dims[["strategy"]], "\"")
  }
  # Both "Maximum Lift" and "Maximum Lift (Deprecated)" are brand- and
  # weight-invariant — registered only at focus = Market / weight = Unweighted.
  is_max_expr <- if (strategy_active) {
    paste0("OR(", strategy_ref, "=\"", max_label, "\",",
                  strategy_ref, "=\"", max_deprecated_label, "\")")
  } else {
    if (identical(dims[["strategy"]], max_label) ||
        identical(dims[["strategy"]], max_deprecated_label)) "TRUE" else "FALSE"
  }
  # "Maximum Lift (Deprecated)" detection — used by the Display warning
  # (next to the chart dropdown) and by the dynamic-column formulas to
  # force Point Change rendering even when the Display dropdown sits on
  # Percent Change.
  is_deprecated_expr <- if (strategy_active) {
    paste0(strategy_ref, "=\"", max_deprecated_label, "\"")
  } else {
    if (identical(dims[["strategy"]], max_deprecated_label)) "TRUE" else "FALSE"
  }
  key_parts <- purrr::map_chr(dim_names, function(dn) {
    base <- if (dn %in% active_dims) {
      paste0("Prioritization!", dropdown_refs[[dn]])
    } else {
      paste0("\"", dims[[dn]], "\"")
    }
    if (dn == "focus") {
      paste0("IF(", is_max_expr, ",\"Market\",", base, ")")
    } else if (dn == "weight") {
      paste0("IF(", is_max_expr, ",\"Unweighted\",", base, ")")
    } else {
      base
    }
  })
  key_formula <- paste(key_parts, collapse = "&\"|\"&")

  # Sheet lookup: INDEX(Sheet col, MATCH(key, Key col, 0))
  sheet_match <- paste0(
    "MATCH(", key_formula, ",_priorit_lookup!$H$2:$H$", n_entries + 1, ",0)"
  )
  sheet_lookup <- paste0(
    "INDEX(_priorit_lookup!$F$2:$F$", n_entries + 1, ",", sheet_match, ")"
  )

  # -------------------------------------------------------------------------
  # Base / warning feedback next to the Focus dropdown — mirrors the simulator
  #   n >= min_base_for_boot → "Base: N"    (default grey style)
  #   n <  min_base_for_boot → "Results not calculated because base is below N" (red)
  # Uses the same key_formula the sheet switcher uses, but pulls the Base
  # column (G) from _lookup.
  # -------------------------------------------------------------------------
  if (!is.null(focus_cell_row)) {
    base_lookup <- paste0(
      "INDEX(_priorit_lookup!$G$2:$G$", n_entries + 1, ",", sheet_match, ")"
    )
    warn_col <- cell_col + 1L

    focus_warn_formula <- paste0(
      "IFERROR(",
        "IF(", base_lookup, "<", min_base_for_boot, ",",
          "\"Results not calculated because base is below ", min_base_for_boot, "\",",
          "\"Base: \"&", base_lookup,
        "),",
      "\"\")"
    )
    openxlsx::writeFormula(wb, dash, x = focus_warn_formula,
      startRow = focus_cell_row, startCol = warn_col)

    # Red formatting only when the base is actually below threshold.
    red_rule <- paste0("IFERROR(", base_lookup, "<", min_base_for_boot, ",FALSE)")
    red_warning <- openxlsx::createStyle(fontColour = "#FF0000",
      textDecoration = "bold")
    red_cell <- openxlsx::createStyle(bgFill = "#FF0000",
      fontColour = "#FFFFFF")

    openxlsx::conditionalFormatting(wb, dash,
      cols = warn_col, rows = focus_cell_row,
      style = red_warning, type = "expression", rule = red_rule)
    openxlsx::conditionalFormatting(wb, dash,
      cols = cell_col, rows = focus_cell_row,
      style = red_cell, type = "expression", rule = red_rule)
  }

  # -------------------------------------------------------------------------
  # Weight warning — mirrors the impact dashboard's "Weights don't affect
  # this metric" note. Fires when Strategy = "Max", since the Max strategy
  # uses exact gRain inference (no frequency-shift distribution) so toggling
  # the Weight dropdown has no effect on the result. Shown in grey italic
  # to the right of the Weight control cell.
  # -------------------------------------------------------------------------
  if (!is.null(weight_cell_row) && "strategy" %in% names(dropdown_refs)) {
    strategy_ref <- paste0("Prioritization!", dropdown_refs[["strategy"]])
    weight_warn_col <- cell_col + 1L
    weight_warn_formula <- paste0(
      "IF(OR(", strategy_ref, "=\"", max_label, "\",",
              strategy_ref, "=\"", max_deprecated_label, "\"),",
      "\"Weights don't affect this strategy\",\"\")"
    )
    openxlsx::writeFormula(wb, dash, x = weight_warn_formula,
      startRow = weight_cell_row, startCol = weight_warn_col)

    weight_warn_rule <- paste0(
      "OR(", strategy_ref, "=\"", max_label, "\",",
             strategy_ref, "=\"", max_deprecated_label, "\")"
    )
    openxlsx::conditionalFormatting(wb, dash,
      cols = weight_warn_col, rows = weight_cell_row,
      style = openxlsx::createStyle(fontColour = "#888888",
        textDecoration = "italic"),
      type = "expression", rule = weight_warn_rule)
  }

  # -------------------------------------------------------------------------
  # Display warning — fires when Strategy = "Maximum Lift (Deprecated)" AND
  # Display = "Percent Change". The deprecated strategy stores the raw
  # dv_estimate as cumulative gain (no baseline comparison), so the percent
  # columns are NA and Percent Change rendering would be blank. The
  # dynamic-column formulas already coerce to Point Change in this case;
  # this warning explains why the user's Percent Change selection has no
  # visible effect. Mirrors the Focus base warning's grey-italic style.
  # -------------------------------------------------------------------------
  if (max_deprecated_label %in% dims[["strategy"]]) {
    chart_warn_col <- cell_col + 1L
    chart_warn_rule <- paste0(
      "AND(", is_deprecated_expr, ",",
              chart_cell_ref, "=\"Percent Change\")"
    )
    chart_warn_formula <- paste0(
      "IF(", chart_warn_rule, ",",
        "\"Point change display is the only option for this analysis.\",",
        "\"\")"
    )
    openxlsx::writeFormula(wb, dash, x = chart_warn_formula,
      startRow = chart_cell_row, startCol = chart_warn_col)
    openxlsx::conditionalFormatting(wb, dash,
      cols = chart_warn_col, rows = chart_cell_row,
      style = openxlsx::createStyle(fontColour = "#888888",
        textDecoration = "italic"),
      type = "expression", rule = chart_warn_rule)
  }

  # --- Data table ---
  # Prioritization columns and their source column letters in the data sheets.
  # The bn_prioritize tibble emits columns in this order (combo is not shown
  # on the dashboard but is still in the data sheet):
  # Without community: A=priority, B=variable, C=label, D=combo, E=dv_estimate,
  #   F=cumulative_gain, G=marginal_gain, H=cumulative_gain_pct,
  #   I=marginal_gain_pct, J=p_value
  # With community: A=priority, B=variable, C=community, D=label, E=combo,
  #   F=dv_estimate, G=cumulative_gain, H=marginal_gain,
  #   I=cumulative_gain_pct, J=marginal_gain_pct, K=p_value
  # Two physical metric columns (Cumulative + Incremental) carry whichever
  # value matches the Display dropdown — Cumulative Gain / Incremental Lift
  # in Point Change, Cumulative Gain % / Incremental Lift % in Percent
  # Change. Outcome Estimate stays in the layout but its column is hidden
  # (kept available for chart references). source_letters[i] is the Point
  # Change source letter; percent_source_letters[i] is the Percent Change
  # source letter for dynamic columns (NA for non-dynamic columns).
  if (has_community) {
    display_headers <- c("Step", "Variable", "Community", "Label",
                         "Outcome Estimate",
                         "Cumulative Gain", "Incremental Lift")
    source_letters         <- c("A", "B", "C", "D", "F", "G", "H")
    percent_source_letters <- c(NA,  NA,  NA,  NA,  NA,  "I", "J")
    if (has_p) {
      display_headers        <- c(display_headers, "p-value")
      source_letters         <- c(source_letters, "K")
      percent_source_letters <- c(percent_source_letters, NA)
    }
  } else {
    display_headers <- c("Step", "Variable", "Label",
                         "Outcome Estimate",
                         "Cumulative Gain", "Incremental Lift")
    source_letters         <- c("A", "B", "C", "E", "F", "G")
    percent_source_letters <- c(NA,  NA,  NA,  NA,  "H", "I")
    if (has_p) {
      display_headers        <- c(display_headers, "p-value")
      source_letters         <- c(source_letters, "J")
      percent_source_letters <- c(percent_source_letters, NA)
    }
  }

  # Per-column kind. "dynamic" columns swap their header text + value source
  # based on the Display dropdown; "static" columns are written once.
  column_kind <- ifelse(is.na(percent_source_letters), "static", "dynamic")

  n_display_cols <- length(display_headers)
  row_data_start <- last_control_row + 2L
  chart_height <- 19L

  # When strategy = "Maximum Lift (Deprecated)" the percent columns are
  # NA, so the dynamic columns are forced to Point Change regardless of
  # the Display dropdown — selecting Percent Change while on Deprecated
  # would otherwise show a blank table.
  is_point_change_expr <- paste0(
    "OR(", chart_cell_ref, "=\"Point Change\",", is_deprecated_expr, ")"
  )

  # Table at col B (left), chart to the right after a spacer
  cols_all <- seq(col_data_start, col_data_start + n_display_cols - 1)
  col_chart_start <- max(cols_all) + 2L  # one spacer column after table

  # Write headers — dynamic columns (Cumulative / Incremental) become
  # formulas that switch label between point and percent versions.
  for (ci in seq_along(display_headers)) {
    if (column_kind[ci] == "static") {
      openxlsx::writeData(wb, dash, display_headers[ci],
        startRow = row_data_start, startCol = col_data_start + ci - 1)
    } else {
      # Dynamic — swap "Cumulative Gain" / "Cumulative Gain %", etc.
      point_label   <- display_headers[ci]
      percent_label <- paste0(point_label, " %")
      header_formula <- paste0(
        'IF(', is_point_change_expr, ',"',
        point_label, '","', percent_label, '")'
      )
      openxlsx::writeFormula(wb, dash, x = header_formula,
        startRow = row_data_start, startCol = col_data_start + ci - 1)
    }
  }
  openxlsx::addStyle(wb, dash, style = styles$header,
    rows = row_data_start, cols = cols_all, gridExpand = TRUE, stack = TRUE)

  # Write INDIRECT formulas for each data row
  data_rows <- seq(row_data_start + 1, row_data_start + max_rows)

  for (ri in seq_along(data_rows)) {
    row <- data_rows[ri]
    source_row <- ri + 1  # row 2+ in data sheet

    for (ci in seq_len(n_display_cols)) {
      point_let <- source_letters[ci]
      point_ref <- paste0(sheet_lookup, "&\"!$", point_let, "$", source_row, "\"")
      point_branch <- paste0(
        "IF(ISBLANK(INDIRECT(", point_ref, ")),\"\",IFERROR(INDIRECT(", point_ref, "),\"\"))"
      )

      cell_formula <- if (column_kind[ci] == "static") {
        point_branch
      } else {
        # Dynamic — pick point_ref or percent_ref based on Display dropdown.
        pct_let <- percent_source_letters[ci]
        percent_ref <- paste0(sheet_lookup, "&\"!$", pct_let, "$", source_row, "\"")
        percent_branch <- paste0(
          "IF(ISBLANK(INDIRECT(", percent_ref, ")),\"\",IFERROR(INDIRECT(", percent_ref, "),\"\"))"
        )
        paste0(
          'IF(', is_point_change_expr, ',',
          point_branch, ',', percent_branch, ')'
        )
      }
      openxlsx::writeFormula(wb, dash, x = cell_formula,
        startRow = row, startCol = col_data_start + ci - 1)
    }
  }

  # Hide the baseline (priority == 0) dashboard row. The data is preserved
  # in the source data sheet but the dashboard collapses the row out of
  # view. ri == 1 corresponds to source_row == 2 — the first data row of
  # the source sheet, which is always the baseline emitted by
  # bn_prioritize().
  baseline_dash_row <- data_rows[1]
  openxlsx::setRowHeights(wb, dash, rows = baseline_dash_row, heights = 0)

  # --- Column positions (derived from display_headers order) ---
  col_step <- col_data_start + match("Step", display_headers) - 1
  col_variable <- col_data_start + match("Variable", display_headers) - 1
  col_community <- if (has_community) col_data_start + match("Community", display_headers) - 1 else NULL
  col_label <- col_data_start + match("Label", display_headers) - 1
  col_estimate <- col_data_start + match("Outcome Estimate", display_headers) - 1
  col_cum  <- col_data_start + match("Cumulative Gain", display_headers) - 1
  col_gain <- col_data_start + match("Incremental Lift", display_headers) - 1
  col_pvalue <- if (has_p) col_data_start + match("p-value", display_headers) - 1 else NULL

  # --- Column widths ---
  # Step is a tight integer column (fixed 5). Variable auto-sizes to its
  # short attribute names. Community is fixed 20 and Label fixed 30 — the
  # INDIRECT formula cells defeat openxlsx's "auto" (bestFit only honors
  # direct values), so we set them explicitly. Metric columns get a uniform
  # width of 10. Outcome Estimate is hidden but kept structurally so chart
  # references and sort keys still work.
  openxlsx::setColWidths(wb, dash, cols = col_step, widths = 5)
  openxlsx::setColWidths(wb, dash, cols = col_variable, widths = "auto")
  if (!is.null(col_community)) {
    openxlsx::setColWidths(wb, dash, cols = col_community, widths = 20)
  }
  openxlsx::setColWidths(wb, dash, cols = col_label, widths = 30)
  openxlsx::setColWidths(wb, dash, cols = col_estimate, widths = 10, hidden = TRUE)
  openxlsx::setColWidths(wb, dash, cols = col_cum, widths = 10)
  openxlsx::setColWidths(wb, dash, cols = col_gain, widths = 10)
  if (!is.null(col_pvalue)) {
    openxlsx::setColWidths(wb, dash, cols = col_pvalue, widths = 10)
  }
  # Spacer column between table and chart
  openxlsx::setColWidths(wb, dash, cols = max(cols_all) + 1, widths = 3)
  # Chart area columns — even widths
  openxlsx::setColWidths(wb, dash, cols = col_chart_start:(col_chart_start + 9), widths = 8.43)

  # --- Data styles ---
  # Step = integer
  openxlsx::addStyle(wb, dash, style = styles$center_int,
    rows = data_rows, cols = col_step, gridExpand = TRUE, stack = TRUE)

  # Outcome Estimate, Cumulative, Incremental — 2 decimals (or XX.X% when
  # the outcome is binary). The dynamic Cumulative/Incremental columns
  # alternate between point and percent values; for binary outcomes the
  # "0.0%" format works for both modes, for non-binary the "0.00" format
  # is correct in Point Change and the user accepts fractional display in
  # Percent Change (single static format can't represent both perfectly
  # without conditional formatting).
  openxlsx::addStyle(wb, dash, style = styles$center_2dp,
    rows = data_rows, cols = c(col_estimate, col_cum, col_gain), gridExpand = TRUE, stack = TRUE)

  # Left-align text columns
  text_cols <- c(col_variable, col_community, col_label)
  openxlsx::addStyle(wb, dash, style = styles$left,
    rows = data_rows, cols = text_cols, gridExpand = TRUE, stack = TRUE)

  # White-to-green colour scale for the visible numeric columns
  for (cc in c(col_cum, col_gain)) {
    openxlsx::conditionalFormatting(wb, dash,
      cols = cc, rows = data_rows,
      style = c("#FFFFFF", "#66bd7d"), type = "colourScale")
  }

  # --- Conditional borders (only on rows with data) ---
  # $column locks the reference so it doesn't shift across columns
  step_let <- num2let(col_step)
  border_rule <- paste0("$", step_let, data_rows[1], "<>\"\"")

  # Left border on first column
  openxlsx::conditionalFormatting(wb, dash,
    cols = min(cols_all), rows = data_rows,
    style = openxlsx::createStyle(border = "Left", borderStyle = "medium"),
    type = "expression", rule = border_rule)

  # Right border on last column
  openxlsx::conditionalFormatting(wb, dash,
    cols = max(cols_all), rows = data_rows,
    style = openxlsx::createStyle(border = "Right", borderStyle = "medium"),
    type = "expression", rule = border_rule)

  # Bottom border only on last data row (this row has data, next row is blank)
  bottom_rule <- paste0("AND($", step_let, data_rows[1], "<>\"\",$",
                         step_let, data_rows[1] + 1, "=\"\")")
  openxlsx::conditionalFormatting(wb, dash,
    cols = cols_all, rows = data_rows,
    style = openxlsx::createStyle(border = "Bottom", borderStyle = "medium"),
    type = "expression", rule = bottom_rule)

  # --- P-value conditional formatting (after borders so font colors win) ---
  if (has_p && !is.null(col_pvalue)) {
    step_rows <- data_rows[-1]

    openxlsx::addStyle(wb, dash, style = styles$center_2dp,
      rows = data_rows, cols = col_pvalue, gridExpand = TRUE, stack = TRUE)

    if (length(step_rows) > 0) {
      p_let <- num2let(col_pvalue)
      p_not_blank <- paste0(p_let, step_rows[1], "<>\"\"")

      openxlsx::conditionalFormatting(wb, dash,
        cols = col_pvalue, rows = step_rows,
        style = styles$p_red, type = "expression",
        rule = paste0("AND(", p_not_blank, ",", p_let, step_rows[1], ">=", marginal_threshold, ")"))

      openxlsx::conditionalFormatting(wb, dash,
        cols = col_pvalue, rows = step_rows,
        style = styles$p_orange, type = "expression",
        rule = paste0("AND(", p_not_blank, ",", p_let, step_rows[1], ">=", sig_threshold,
                       ",", p_let, step_rows[1], "<", marginal_threshold, ")"))

      openxlsx::conditionalFormatting(wb, dash,
        cols = col_pvalue, rows = step_rows,
        style = styles$p_green, type = "expression",
        rule = paste0("AND(", p_not_blank, ",", p_let, step_rows[1], "<", sig_threshold, ")"))

      # Reset font when step is blank or 0 (last = highest priority in Excel)
      openxlsx::conditionalFormatting(wb, dash,
        cols = col_pvalue, rows = step_rows,
        style = openxlsx::createStyle(fontColour = "#000000"), type = "expression",
        rule = paste0("OR(", step_let, step_rows[1], "=\"\",",
                       step_let, step_rows[1], "=0)"))
    }
  }

  # Header row — outer box only
  openxlsx::addStyle(wb, dash,
    style = openxlsx::createStyle(border = "TopBottom", borderStyle = "medium"),
    rows = row_data_start, cols = cols_all, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dash,
    style = openxlsx::createStyle(border = "TopBottomLeft", borderStyle = "medium"),
    rows = row_data_start, cols = min(cols_all), stack = TRUE)
  openxlsx::addStyle(wb, dash,
    style = openxlsx::createStyle(border = "TopBottomRight", borderStyle = "medium"),
    rows = row_data_start, cols = max(cols_all), stack = TRUE)

  # --- Footer ---
  footer_row <- max(data_rows) + 2

  # Dynamic base size — always resolves: for skipped brand x subgroup slices
  # the _lookup sheet still carries an n_obs value (via phantom entries).
  base_formula <- paste0(
    "IFERROR(\"Base: \"&INDEX(_priorit_lookup!$G$2:$G$", n_entries + 1,
    ",", sheet_match, "),\"\")"
  )
  openxlsx::writeFormula(wb, dash, x = base_formula,
    startRow = footer_row, startCol = col_data_start)
  openxlsx::addStyle(wb, dash, style = openxlsx::createStyle(textDecoration = "bold"),
    rows = footer_row, cols = col_data_start, stack = TRUE)
  footer_row <- footer_row + 1

  footer_lines <- c(
    "Step \u2014 Priority step number (order in which attributes were selected)",
    "Outcome Estimate \u2014 Expected DV value with all selected attributes shifted",
    "Cumulative Gain \u2014 Absolute increase in outcome estimate from baseline (all attributes through this step)",
    "Incremental Lift \u2014 Absolute increase in outcome estimate from adding this attribute",
    "Cumulative Gain % \u2014 Percentage increase from baseline through this step",
    "Incremental Lift % \u2014 Percentage increase relative to the previous step"
  )

  if (has_p) {
    footer_lines <- c(footer_lines,
      "p-value \u2014 Noise-floor test: proportion of bootstraps where this step's gain \u2264 the noise floor",
      paste0("Green < ", sig_threshold * 100, "%, ",
             "Orange < ", marginal_threshold * 100, "%, ",
             "Red \u2265 ", marginal_threshold * 100, "%")
    )
  }

  footer_lines <- c(footer_lines,
    "",
    paste0(lift_label, " \u2014 Shifts each IV's distribution upward by ", lift_pct_label,
           "%, reflecting a realistic improvement scenario"),
    paste0(max_label, " \u2014 Sets each IV to its highest level as hard evidence, representing the theoretical ceiling")
  )
  # Deprecated strategy only appears when bn_prioritizations() was called
  # with include_maximum_lift_deprecated = TRUE \u2014 check dims to know.
  if (max_deprecated_label %in% dims[["strategy"]]) {
    footer_lines <- c(footer_lines,
      paste0(max_deprecated_label,
             " \u2014 Same as Maximum Lift but cumulative gain is the raw outcome estimate (no comparison to baseline). Provided for backward compatibility.")
    )
  }

  for (fi in seq_along(footer_lines)) {
    openxlsx::writeData(wb, dash, footer_lines[fi],
      startRow = footer_row + fi - 1, startCol = col_data_start)
  }

  # ---------------------------------------------------------------------------
  # 4b. Chart data sheet (formulas referencing dashboard for dynamic chart)
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "_priorit_chart_data", gridLines = FALSE)

  chart_headers <- c("Label", "Previous", "Incremental", "Cumulative DV")
  for (ci in seq_along(chart_headers)) {
    openxlsx::writeData(wb, "_priorit_chart_data", chart_headers[ci],
      startRow = 1, startCol = ci)
  }

  var_let  <- num2let(col_variable)
  cum_let  <- num2let(col_cum)
  gain_let <- num2let(col_gain)

  # The dashboard's Cumulative / Incremental columns auto-switch between
  # point and percent values based on the Display dropdown, so the chart
  # can simply reference those cells without re-doing the mode check —
  # whatever the dashboard is showing is what the chart should plot.
  # Skip data_rows[1] — that's the baseline row in the dashboard, hidden
  # from the table and excluded from the chart.
  chart_data_rows <- if (length(data_rows) >= 2) data_rows[-1] else data_rows
  for (ri in seq_along(chart_data_rows)) {
    dr <- chart_data_rows[ri]
    chart_r <- ri + 1
    blank_check <- paste0('Prioritization!', var_let, dr, '=""')

    # Col A: Label ("" so category axis doesn't show #N/A text)
    openxlsx::writeFormula(wb, "_priorit_chart_data",
      x = paste0('IF(', blank_check, ',"",Prioritization!', var_let, dr, ')'),
      startRow = chart_r, startCol = 1)

    # Col B: Previous (stacked baseline of the bar) = cumulative − incremental
    openxlsx::writeFormula(wb, "_priorit_chart_data",
      x = paste0(
        'IF(', blank_check, ',NA(),',
          'Prioritization!', cum_let, dr, '-Prioritization!', gain_let, dr,
        ')'),
      startRow = chart_r, startCol = 2)

    # Col C: Incremental (stacked top of the bar)
    openxlsx::writeFormula(wb, "_priorit_chart_data",
      x = paste0(
        'IF(', blank_check, ',NA(),Prioritization!', gain_let, dr, ')'),
      startRow = chart_r, startCol = 3)

    # Col D: Cumulative DV (line series)
    openxlsx::writeFormula(wb, "_priorit_chart_data",
      x = paste0(
        'IF(', blank_check, ',NA(),Prioritization!', cum_let, dr, ')'),
      startRow = chart_r, startCol = 4)
  }

  # Tag the chart_data numeric cells (B/C/D) with the binary-aware number
  # format so Excel has a consistent format on both the source cells and
  # the chart axis. This matches the table's center_2dp style.
  if (length(chart_data_rows) > 0) {
    chart_data_style <- openxlsx::createStyle(
      numFmt = if (is_binary_outcome) "0.0%" else "0.00")
    openxlsx::addStyle(wb, "_priorit_chart_data", style = chart_data_style,
      rows = seq_len(length(chart_data_rows)) + 1L,
      cols = 2:4, gridExpand = TRUE, stack = TRUE)
  }

  # ---------------------------------------------------------------------------
  # 5. Hide data + lookup sheets — only touch visibility on sheets WE added.
  # ---------------------------------------------------------------------------
  sheet_names <- names(wb)
  cur_vis <- openxlsx::sheetVisibility(wb)
  for (sn in setdiff(sheet_names, pre_sheets)) {
    idx <- match(sn, sheet_names)
    cur_vis[idx] <- if (sn == "Prioritization") {
      TRUE
    } else if (very_hide_all) {
      "veryHidden"
    } else {
      FALSE
    }
  }
  openxlsx::sheetVisibility(wb) <- cur_vis

  # ---------------------------------------------------------------------------
  # 6. Guide tab — added last so it appears as the final tab
  # ---------------------------------------------------------------------------
  meta <- result[["meta"]]
  dv_guide <- meta[["dv"]]
  dv_display_guide <- if (!is.null(dv_guide) && !is.null(names(dv_guide))) {
    names(dv_guide)
  } else if (!is.null(dv_guide)) dv_guide else NULL

  # Bootstrap was applied when at least one registry tibble has a p_value
  # column with non-NA values. (has_p only checks column presence.)
  boot_applied_guide <- any(purrr::map_lgl(registry, function(e) {
    !is.null(e$tbl) && "p_value" %in% names(e$tbl) &&
      any(!is.na(e$tbl$p_value))
  }))

  if (isTRUE(add_guide)) {
    wb <- append_bn_prioritize_guide(
      wb = wb,
      dv_display = dv_display_guide,
      has_brands = "focus" %in% active_dims,
      has_weights = "weight" %in% active_dims,
      has_subgroups = "subgroup" %in% active_dims,
      has_strategy = "strategy" %in% active_dims,
      has_community = has_community,
      lift = lift,
      sig_threshold = sig_threshold,
      marginal_threshold = marginal_threshold,
      min_base_for_boot = min_base_for_boot,
      boot_applied = boot_applied_guide,
      n_boot_final = meta[["n_boot_final"]],
      noise_tail = meta[["noise_tail"]],
      threshold = meta[["threshold"]]
    )
  }

  # ---------------------------------------------------------------------------
  # 7. Save
  # ---------------------------------------------------------------------------
  fname <- if (!is.null(file_name)) {
    paste0(file_name, " - Prioritization.xlsx")
  } else {
    "Prioritization.xlsx"
  }
  file_path <- file.path(path, fname)
  if (isTRUE(save)) openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  # ---------------------------------------------------------------------------
  # 7. Add chart via openxlsx2 (reload → insert chart XML → re-save)
  # ---------------------------------------------------------------------------
  # Compute axis minimum from default data set baseline (priority == 0 row)
  default_tbl <- registry[[1]]$tbl
  baseline <- default_tbl$dv_estimate[1]
  n_steps <- nrow(default_tbl)
  if (n_steps > 2) {
    axis_min <- floor((baseline - 0.5) / 0.25) * 0.25
  } else {
    axis_min <- floor((baseline - 0.2) / 0.1) * 0.1
  }
  axis_min <- max(0, axis_min)

  # max_rows includes the baseline row in the dashboard, but the chart
  # data sheet skips baseline (chart_data_rows = data_rows[-1]), so the
  # chart's data range is one row shorter.
  chart_xml <- .prioritize_chart_xml(max_rows - 1L, axis_min,
    is_binary = is_binary_outcome)
  chart_dims <- paste0(
    num2let(col_chart_start), row_data_start, ":",
    num2let(col_chart_start + 9), row_data_start + chart_height - 1
  )

  if (isTRUE(save)) {
    # save + reload + inject chart + save (openxlsx doesn't support chart
    # XML directly — openxlsx2 does).
    wb2 <- openxlsx2::wb_load(file_path)
    wb2$add_chart_xml(sheet = "Prioritization", dims = chart_dims, xml = chart_xml)
    wb2$save(file_path, overwrite = TRUE)
    cli::cli_alert_success("Prioritization workbook saved: {file_path}")
  } else {
    # Defer chart injection — the caller (e.g., bn_write) must do it post-save.
    # Attach the chart metadata to the workbook so the caller can retrieve it.
    attr(wb, "priorit_chart_xml") <- chart_xml
    attr(wb, "priorit_chart_dims") <- chart_dims
  }

  invisible(wb)
}


# =============================================================================
# Internal: build flat registry with dimension tags
# =============================================================================
.prioritize_build_registry <- function(result) {

  registry <- list()
  sheet_counter <- 1L

  .add <- function(tbl, strategy, search, subgroup, focus, weight) {
    sn <- paste0("_pd", sheet_counter)
    n_obs <- attr(tbl, "n_obs")
    registry[[length(registry) + 1]] <<- list(
      strategy = strategy,
      search = search,
      subgroup = subgroup,
      focus = focus,
      weight = weight,
      sheet_name = sn,
      tbl = tbl,
      n_obs = n_obs
    )
    sheet_counter <<- sheet_counter + 1L
  }

  # Single bn_prioritize() tibble
  if (is.data.frame(result)) {
    .add(result, strategy = "Greedy", search = "Greedy",
         subgroup = "Total", focus = "Market", weight = "Unweighted")
    return(registry)
  }

  meta <- result[["meta"]]
  has_subgroups <- !is.null(meta[["subgroups"]])

  # Display labels for the strategy dropdown — substituted into the registry
  # so they appear in the dropdown, the _lookup keys, and the JS payload
  # consistently. The raw "Lift" / "Max" tags only live inside
  # bn_prioritizations() output; the writer / report never sees them.
  lift_pct <- if (!is.null(meta[["lift"]])) round(meta[["lift"]] * 100) else 10
  lift_label            <- paste0("Moderate Lift (", lift_pct, "%)")
  max_label             <- "Maximum Lift"
  max_deprecated_label  <- "Maximum Lift (Deprecated)"

  # Helper to iterate subgroups or treat as single
  .iter <- function(variant_data, strategy, search, focus, weight) {
    if (has_subgroups && is.list(variant_data) && !is.data.frame(variant_data)) {
      for (sg_name in names(variant_data)) {
        .add(variant_data[[sg_name]], strategy = strategy, search = search,
             subgroup = sg_name, focus = focus, weight = weight)
      }
    } else {
      sg <- if (has_subgroups) meta[["subgroups"]][1] else "Total"
      .add(variant_data, strategy = strategy, search = search,
           subgroup = sg, focus = focus, weight = weight)
    }
  }

  # greedy_lift
  if (!is.null(result[["greedy_lift"]])) {
    .iter(result[["greedy_lift"]], strategy = lift_label, search = "Greedy",
          focus = "Market", weight = "Unweighted")
  }

  # greedy_max
  if (!is.null(result[["greedy_max"]])) {
    .iter(result[["greedy_max"]], strategy = max_label, search = "Greedy",
          focus = "Market", weight = "Unweighted")
  }

  # greedy_max_deprecated — same shape as greedy_max but the cumulative
  # gain is the raw dv_estimate (no baseline subtraction). Only present
  # when bn_prioritizations() was called with
  # include_maximum_lift_deprecated = TRUE.
  if (!is.null(result[["greedy_max_deprecated"]])) {
    .iter(result[["greedy_max_deprecated"]], strategy = max_deprecated_label,
          search = "Greedy", focus = "Market", weight = "Unweighted")
  }

  # greedy_lift_weighted
  if (!is.null(result[["greedy_lift_weighted"]])) {
    .iter(result[["greedy_lift_weighted"]], strategy = lift_label, search = "Greedy",
          focus = "Market", weight = "Weighted")
  }

  # greedy_lift_brand
  if (!is.null(result[["greedy_lift_brand"]])) {
    for (brand_name in names(result[["greedy_lift_brand"]])) {
      brand_result <- result[["greedy_lift_brand"]][[brand_name]]
      .iter(brand_result, strategy = lift_label, search = "Greedy",
            focus = brand_name, weight = "Unweighted")
    }
  }

  # greedy_lift_brand_weighted
  if (!is.null(result[["greedy_lift_brand_weighted"]])) {
    for (brand_name in names(result[["greedy_lift_brand_weighted"]])) {
      brand_result <- result[["greedy_lift_brand_weighted"]][[brand_name]]
      .iter(brand_result, strategy = lift_label, search = "Greedy",
            focus = brand_name, weight = "Weighted")
    }
  }

  # Phantom entries for brand x subgroup slices that were skipped during
  # prioritization (base below min_base_for_boot). They have no tibble / no
  # data sheet, but they populate _lookup so the user can still (a) pick the
  # focus in the dropdown, and (b) see the base and warning next to it.
  skipped <- meta[["skipped_slices"]]
  if (length(skipped) > 0) {
    for (s in skipped) {
      wt_label <- if (isTRUE(s$weighted)) "Weighted" else "Unweighted"
      registry[[length(registry) + 1]] <- list(
        strategy = lift_label,
        search = "Greedy",
        subgroup = s$sg_name,
        focus = s$brand_name,
        weight = wt_label,
        sheet_name = NA_character_,
        tbl = NULL,
        n_obs = s$n_obs
      )
    }
  }

  registry
}


# =============================================================================
# Internal: generate OOXML chart XML for combo stacked bar + line chart
# =============================================================================
.prioritize_chart_xml <- function(max_rows, axis_min = 0, is_binary = FALSE) {
  n <- max_rows + 1  # last data row in _priorit_chart_data (row 1 = header)

  # Match the table's binary-aware rule: probability-scale outcomes render
  # as XX.X% on the value axis; everything else as XX.XX. Excel needs the
  # ampersand escaped (`&amp;`) in chart XML, but neither character appears
  # in our format strings.
  axis_num_fmt <- if (isTRUE(is_binary)) "0.0%" else "0.00"

  # Helper to build a cell range reference for _priorit_chart_data
  ref <- function(col, r1 = 2, r2 = n) {
    paste0("'_priorit_chart_data'!$", col, "$", r1, ":$", col, "$", r2)
  }
  hdr <- function(col) {
    paste0("'_priorit_chart_data'!$", col, "$1")
  }

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<c:roundedCorners val="0"/>',
    '<c:chart>',
    '<c:autoTitleDeleted val="1"/>',
    '<c:plotArea>',
    '<c:layout/>',

    # ---- Stacked bar chart (Previous + Incremental) ----
    '<c:barChart>',
    '<c:barDir val="col"/>',
    '<c:grouping val="stacked"/>',
    '<c:varyColors val="0"/>',

    # Series 1: Previous (light grey)
    '<c:ser>',
    '<c:idx val="0"/><c:order val="0"/>',
    '<c:tx><c:strRef><c:f>', hdr("B"), '</c:f></c:strRef></c:tx>',
    '<c:spPr>',
    '<a:solidFill><a:srgbClr val="D9D9D9"/></a:solidFill>',
    '<a:ln><a:noFill/></a:ln>',
    '</c:spPr>',
    '<c:cat><c:strRef><c:f>', ref("A"), '</c:f></c:strRef></c:cat>',
    '<c:val><c:numRef><c:f>', ref("B"), '</c:f></c:numRef></c:val>',
    '</c:ser>',

    # Series 2: Incremental (dark grey)
    '<c:ser>',
    '<c:idx val="1"/><c:order val="1"/>',
    '<c:tx><c:strRef><c:f>', hdr("C"), '</c:f></c:strRef></c:tx>',
    '<c:spPr>',
    '<a:solidFill><a:srgbClr val="595959"/></a:solidFill>',
    '<a:ln><a:noFill/></a:ln>',
    '</c:spPr>',
    '<c:cat><c:strRef><c:f>', ref("A"), '</c:f></c:strRef></c:cat>',
    '<c:val><c:numRef><c:f>', ref("C"), '</c:f></c:numRef></c:val>',
    '</c:ser>',

    '<c:overlap val="100"/>',
    '<c:axId val="111111111"/>',
    '<c:axId val="222222222"/>',
    '</c:barChart>',

    # ---- Line chart (Cumulative DV Estimate) ----
    '<c:lineChart>',
    '<c:grouping val="standard"/>',
    '<c:varyColors val="0"/>',
    '<c:ser>',
    '<c:idx val="2"/><c:order val="2"/>',
    '<c:tx><c:strRef><c:f>', hdr("D"), '</c:f></c:strRef></c:tx>',
    '<c:spPr>',
    '<a:ln w="22225"><a:solidFill><a:srgbClr val="595959"/></a:solidFill></a:ln>',
    '</c:spPr>',
    '<c:marker>',
    '<c:symbol val="circle"/><c:size val="5"/>',
    '<c:spPr>',
    '<a:solidFill><a:srgbClr val="595959"/></a:solidFill>',
    '<a:ln><a:solidFill><a:srgbClr val="595959"/></a:solidFill></a:ln>',
    '</c:spPr>',
    '</c:marker>',
    '<c:cat><c:strRef><c:f>', ref("A"), '</c:f></c:strRef></c:cat>',
    '<c:val><c:numRef><c:f>', ref("D"), '</c:f></c:numRef></c:val>',
    '<c:smooth val="0"/>',
    '</c:ser>',
    '<c:marker val="1"/>',
    '<c:axId val="111111111"/>',
    '<c:axId val="222222222"/>',
    '</c:lineChart>',

    # ---- Category axis (bottom) ----
    '<c:catAx>',
    '<c:axId val="111111111"/>',
    '<c:scaling><c:orientation val="minMax"/></c:scaling>',
    '<c:delete val="0"/>',
    '<c:axPos val="b"/>',
    '<c:txPr>',
    '<a:bodyPr rot="-2700000"/>',
    '<a:lstStyle/>',
    '<a:p><a:pPr><a:defRPr sz="800">',
    '<a:latin typeface="Calibri"/><a:cs typeface="Calibri"/>',
    '</a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p>',
    '</c:txPr>',
    '<c:crossAx val="222222222"/>',
    '</c:catAx>',

    # ---- Value axis (left) ----
    '<c:valAx>',
    '<c:axId val="222222222"/>',
    '<c:scaling><c:orientation val="minMax"/>',
    '<c:min val="', axis_min, '"/>',
    '</c:scaling>',
    '<c:delete val="0"/>',
    '<c:axPos val="l"/>',
    '<c:numFmt formatCode="', axis_num_fmt, '" sourceLinked="0"/>',
    '<c:txPr>',
    '<a:bodyPr/>',
    '<a:lstStyle/>',
    '<a:p><a:pPr><a:defRPr sz="800">',
    '<a:latin typeface="Calibri"/><a:cs typeface="Calibri"/>',
    '</a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p>',
    '</c:txPr>',
    '<c:crossAx val="111111111"/>',
    '</c:valAx>',

    '</c:plotArea>',

    # ---- Legend (bottom) ----
    '<c:legend>',
    '<c:legendPos val="b"/>',
    '<c:overlay val="0"/>',
    '<c:txPr>',
    '<a:bodyPr/><a:lstStyle/>',
    '<a:p><a:pPr><a:defRPr sz="900">',
    '<a:latin typeface="Calibri"/><a:cs typeface="Calibri"/>',
    '</a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p>',
    '</c:txPr>',
    '</c:legend>',

    '<c:plotVisOnly val="1"/>',
    '<c:dispBlanksAs val="gap"/>',
    '</c:chart>',
    '<c:spPr><a:noFill/><a:ln><a:noFill/></a:ln></c:spPr>',
    '</c:chartSpace>'
  )
}
