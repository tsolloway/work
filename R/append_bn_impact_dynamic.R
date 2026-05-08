#' append_bn_impact_dynamic
#'
#' @description Internal helper that adds a dynamic dashboard sheet, results
#'   sheet, and lookup sheet to an existing workbook. Used by
#'   \code{bn_impact_write()} for both attribute and community dynamic
#'   dashboards.
#'
#' @param wb An openxlsx workbook object.
#' @param table The analysis table (from \code{bn_impact()$table}).
#' @param subgroups Character vector of subgroup names.
#' @param dash_sheet Character. Dashboard sheet name.
#' @param results_sheet Character. Results sheet name.
#' @param lookup_sheet Character. Lookup sheet name.
#' @param title Character. Title text.
#' @param sub_title Character or NULL. Subtitle text.
#' @param engine_footer Character. Engine description for footer.
#' @param variable_width Numeric. Column width for first ID column.
#' @param community_width Numeric. Column width for Community column.
#' @param label_width Column width for Label column.
#'
#' @return The modified workbook object (invisibly).
#'
#' @keywords internal
append_bn_impact_dynamic <- function(
    wb,
    table,
    subgroups,
    dash_sheet,
    results_sheet,
    lookup_sheet,
    title,
    sub_title = NULL,
    engine_footer,
    variable_width = 20,
    community_width = 20,
    label_width = "auto",
    has_weights = FALSE,
    weighted_results_sheet = NULL,
    min_base_for_lift = NULL,
    # Survey-battery grouping. Named list mapping battery name -> IV vector.
    # When non-NULL, the dashboard's leading cols gain a Battery column.
    batteries = NULL,
    # Optional battery groups — named list of vectors of battery names to
    # combine into within-group views. When `battery_filter` matches a
    # name in here (rather than in `batteries`), the dashboard scopes to
    # the union of the group's component batteries' IVs.
    battery_groups = NULL,
    # Optional name pointing at either a single battery (in `batteries`) or
    # a group (in `battery_groups`). When set, the dashboard is restricted
    # to that battery's / group's IVs and the index / Total Impact
    # denominators scope to the same set on the Results sheet.
    battery_filter = NULL,
    # When FALSE, skip writing the Results / Results_Weighted / lookup
    # helper sheets (assumes a prior call already wrote them). The
    # dashboard sheet still gets written with formulas that reference the
    # already-existing helper sheets.
    write_helper_sheets = TRUE,
    # Initial values for the Outcome Display + Shift Type dropdowns.
    # Defaults: "absolute" for both — keeps the dynamic dashboard aligned
    # with the static-write defaults. Caller may override; "headroom"
    # and "range" are valid shift_type initial values when bn_impact()
    # emitted those variants.
    outcome_display = c("absolute", "proportional"),
    shift_type      = c("absolute", "proportional", "headroom", "range")
) {

  outcome_display <- match.arg(outcome_display)
  shift_type      <- match.arg(shift_type)
  display_default_label <- if (outcome_display == "absolute") "Point Change" else "% Change"
  shift_default_label   <- switch(shift_type,
    "absolute"     = "Fixed Step",
    "proportional" = "% of Current Mean",
    "headroom"     = "% Toward Top",
    "range"        = "% of Range"
  )

  has_batteries <- !is.null(batteries) && length(batteries) > 0L
  if (has_batteries && "Variable" %in% names(table)) {
    iv2b <- character(0)
    for (b_name in names(batteries)) {
      ivs_in_b <- batteries[[b_name]]
      iv2b <- c(iv2b, rlang::set_names(rep(b_name, length(ivs_in_b)), ivs_in_b))
    }
    table$Battery <- ifelse(
      table$Variable %in% names(iv2b),
      iv2b[table$Variable],
      ""
    )
    if ("Community" %in% names(table)) {
      table <- table %>% dplyr::relocate(Battery, .after = "Community")
    } else {
      table <- table %>% dplyr::relocate(Battery, .after = "Variable")
    }
    # Per-group helper columns — one column per battery group, with 1 if
    # the row's IV is in any of the group's component batteries, else 0.
    # The dashboard's per-group tab references these columns in its
    # SUMPRODUCT / COUNTIF masks (we can't filter by Battery=<name> when
    # the group spans multiple batteries).
    if (!is.null(battery_groups) && length(battery_groups) > 0L) {
      for (g_name in names(battery_groups)) {
        comp <- battery_groups[[g_name]]
        ivs_in_g <- unique(unlist(batteries[comp], use.names = FALSE))
        helper_col <- paste0("BatteryGroup_", g_name)
        table[[helper_col]] <- as.integer(table$Variable %in% ivs_in_g)
      }
    }
  }
  has_battery_col <- has_batteries && "Battery" %in% names(table)

  # Bootstrap detection: bn_impact emits per-metric `<col>_p_value` columns
  # only when n_boot > 1. When present, the dashboard's blackout rule (p
  # > 0.10) switches from the static MI chi-squared p_val to the
  # bootstrap p_value of WHATEVER metric the user has selected.
  boot_applied <- any(grepl("_p_value$", names(table)))

  # Snapshot the full (cross-battery) table for the Results sheet write.
  # `table` itself becomes the display set — filtered to the selected
  # battery / group when `battery_filter` is set, so the dashboard renders
  # only those IV rows. The Results sheet still carries every IV so that
  # downstream lookups / sibling tabs can share one Results sheet.
  full_table <- table
  # Filter kind: "none" (no filter), "battery" (single battery), or "group"
  # (union of multiple batteries). Drives both the table filter and the
  # SUMPRODUCT mask used in the index / Total Impact formulas below.
  filter_kind <- "none"
  if (!is.null(battery_filter) && has_battery_col) {
    if (battery_filter %in% names(batteries)) {
      keep_ivs <- batteries[[battery_filter]]
      filter_kind <- "battery"
    } else if (!is.null(battery_groups) &&
               battery_filter %in% names(battery_groups)) {
      comp <- battery_groups[[battery_filter]]
      keep_ivs <- unique(unlist(batteries[comp], use.names = FALSE))
      filter_kind <- "group"
    } else {
      stop("battery_filter '", battery_filter,
           "' is not a name in `batteries` or `battery_groups`.")
    }
    table <- table[table$Variable %in% keep_ivs, , drop = FALSE]
    if (nrow(table) == 0L) {
      stop("battery_filter '", battery_filter,
           "' matched no IVs in the impact table.")
    }
  }

  # n_results_rows / n_results_cols describe the *Results sheet* (full
  # cross-battery dataset). They feed every formula range that hits Results
  # (the SUMPRODUCT/INDEX denominators, the MATCH lookup column, etc.) so
  # those references always cover every IV regardless of which dashboard
  # tab we're on. n_dash_rows describes only this dashboard's display
  # height — it shrinks for per-battery tabs to avoid trailing blank rows.
  n_results_rows <- nrow(full_table)
  n_results_cols <- ncol(full_table)
  n_dash_rows    <- nrow(table)

  # ---------------------------------------------------------------------------
  # Determine dropdown options from column names
  # ---------------------------------------------------------------------------
  sg1 <- if (!is.null(subgroups)) subgroups[1] else NULL
  names(table) <- as.character(names(table))
  all_cols <- names(table)
  sgs <- if (!is.null(subgroups)) subgroups else "Total"

  if (!is.null(sg1)) {
    sg_cols <- all_cols[startsWith(all_cols, paste0(sg1, "_"))]
    # Strip bootstrap-stat sibling columns from the metric inventory.
    # `<metric>_p_value` / `_ci_low` / `_ci_high` are statistical siblings
    # of a metric, not distinct metrics — including them here would
    # bloat the Metric / Outcome Display / Shift Type dropdown options
    # with bogus entries like "lift_0_propdisplay_ci_low".
    sg_cols <- sg_cols[!grepl("_(sd|se|t|ci_low|ci_high|p_value)$", sg_cols)]
    metric_suffixes <- gsub(paste0("^", sg1, "_"), "", sg_cols)
  } else {
    metric_suffixes <- setdiff(all_cols, c("Variable", "Label", "Community"))
  }

  # The impact engine now emits both outcome-display variants per lift / maxVmin
  # (columns end in "_propdisplay" or "_absdisplay"). Pass B also adds
  # "_propshift_" / "_absshift_" tags on lift columns. For dropdown /
  # metric-key construction, collapse ALL variant tags first so each metric
  # appears once in the Metric dropdown.
  # Display tag can appear mid-string on brand lift columns
  # ("lift_0_propdisplay_Bing") or at the end on market lift columns
  # ("lift_0_propdisplay"). Strip in both positions so brand name
  # extraction later sees a clean "lift_N_<brand>" form.
  .strip_display <- function(x) gsub("_(propdisplay|absdisplay)(_|$)", "\\2", x)
  .strip_shift   <- function(x) gsub("_(propshift|absshift|headshift|rangeshift)(_|$)", "\\2", x)
  base_suffixes <- unique(.strip_display(.strip_shift(metric_suffixes)))

  # Focus options: Market + any brand names found in lift columns. Operate on
  # the stripped base so brand detection doesn't see the display suffix.
  all_lift_base_suffixes <- grep("^lift", base_suffixes, value = TRUE)
  market_lift_suffixes <- grep("^lift$|^lift_\\d+$", all_lift_base_suffixes, value = TRUE)
  brand_lift_suffixes <- setdiff(all_lift_base_suffixes, market_lift_suffixes)

  if (length(brand_lift_suffixes) > 0) {
    brand_names <- unique(gsub("^lift_\\d+_|^lift_", "", brand_lift_suffixes))
    focus_options <- c("Market", brand_names)
  } else {
    focus_options <- "Market"
  }

  # Metric options (base keys — display suffix appended at formula time).
  # Labels use the bare lift parameter value ("Effect at 0.10") so they
  # stay accurate across all Shift Type modes — the actual IV perturbation
  # is determined by Shift Type, the dropdown just identifies the
  # parameter value.
  metric_labels <- character(0)
  metric_keys <- character(0)

  for (ml in market_lift_suffixes) {
    if (ml == "lift" || ml == "lift_0") {
      metric_labels <- c(metric_labels, "Average Effect")
      metric_keys <- c(metric_keys, ml)
    } else {
      pct <- gsub("lift_", "", ml)
      pct_num <- suppressWarnings(as.numeric(pct))
      lift_val <- if (!is.na(pct_num)) format(pct_num / 100, nsmall = 2) else pct
      metric_labels <- c(metric_labels, paste0("Effect at ", lift_val))
      metric_keys <- c(metric_keys, ml)
    }
  }
  if ("maxVmin" %in% base_suffixes) {
    metric_labels <- c(metric_labels, "Best-vs-Worst Effect")
    metric_keys <- c(metric_keys, "maxVmin")
  }
  if ("mi" %in% base_suffixes) {
    metric_labels <- c(metric_labels, "Explanatory Value")
    metric_keys <- c(metric_keys, "mi")
  }

  # Outcome-display options. Always offered when any display-variant column
  # exists (which is always, now that the engine always emits both).
  has_outcome_display <- any(grepl("_(propdisplay|absdisplay)$", metric_suffixes))
  outcome_display_labels <- c("% Change", "Point Change")
  outcome_display_keys   <- c("propdisplay", "absdisplay")

  # Shift-type options. Offered when the impact table has columns with
  # _propshift_ / _absshift_ / _headshift_ tags (bn_impact Pass B). If not
  # present (older builds or engine-only output), the dashboard falls back
  # to not injecting the shift tag and column-name formulas won't include
  # it. Headroom is the cross-scale-comparable option (every IV moves the
  # same fraction of its own gap to the boundary).
  has_shift_type <- any(grepl("_(propshift|absshift|headshift|rangeshift)_", metric_suffixes))
  shift_type_labels <- c("% of Current Mean", "Fixed Step", "% Toward Top", "% of Range")
  shift_type_keys   <- c("propshift", "absshift", "headshift", "rangeshift")
  # Drop any shift-type entries that aren't actually present in the
  # column suffixes — keeps the dropdown honest if e.g. an older
  # bn_impact run skipped the headroom variant.
  present_shift_keys <- vapply(shift_type_keys, function(k) {
    any(grepl(paste0("_", k, "_"), metric_suffixes))
  }, logical(1))
  if (any(present_shift_keys)) {
    shift_type_labels <- shift_type_labels[present_shift_keys]
    shift_type_keys   <- shift_type_keys[present_shift_keys]
  }

  # Collapse base metric suffixes so the Metric dropdown shows one entry per
  # base (e.g. "lift_0", "maxVmin", "mi") even though the underlying columns
  # are multiplied by shift × display variants.
  .strip_variant_suffix <- function(x) {
    # Order matters: strip display first (it's the innermost suffix), then shift.
    sub("_(propshift|absshift|headshift|rangeshift)", "",
      sub("_(propdisplay|absdisplay)$", "", x))
  }

  # ---------------------------------------------------------------------------
  # Sheet 1: Dashboard (created first so it appears first)
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, dash_sheet, gridLines = FALSE)

  # ---------------------------------------------------------------------------
  # Sheet 2: Results (raw data) — written only on the first call. Subsequent
  # per-battery calls share the existing Results sheet (write_helper_sheets = FALSE).
  # ---------------------------------------------------------------------------
  if (write_helper_sheets) {
    openxlsx::addWorksheet(wb, results_sheet, gridLines = FALSE)
    openxlsx::writeData(wb, results_sheet, full_table, startRow = 1, startCol = 1)
  }

  # ---------------------------------------------------------------------------
  # Sheet 3: _lookup (hidden) — same gating as Results.
  # ---------------------------------------------------------------------------
  if (write_helper_sheets) {
    openxlsx::addWorksheet(wb, lookup_sheet, gridLines = FALSE)
  }

  # Focus options in A column
  openxlsx::writeData(wb, lookup_sheet, "Focus", startRow = 1, startCol = 1)
  for (fi in seq_along(focus_options)) {
    openxlsx::writeData(wb, lookup_sheet, focus_options[fi], startRow = fi + 1, startCol = 1)
  }

  # Metric display labels in B column, keys in C column
  openxlsx::writeData(wb, lookup_sheet, "Metric", startRow = 1, startCol = 2)
  openxlsx::writeData(wb, lookup_sheet, "Key", startRow = 1, startCol = 3)
  for (mi in seq_along(metric_labels)) {
    openxlsx::writeData(wb, lookup_sheet, metric_labels[mi], startRow = mi + 1, startCol = 2)
    openxlsx::writeData(wb, lookup_sheet, metric_keys[mi], startRow = mi + 1, startCol = 3)
  }

  # Subgroup names in D column
  openxlsx::writeData(wb, lookup_sheet, "Subgroup", startRow = 1, startCol = 4)
  for (si in seq_along(sgs)) {
    openxlsx::writeData(wb, lookup_sheet, sgs[si], startRow = si + 1, startCol = 4)
  }

  # Index description in E column
  index_descriptions <- purrr::map_chr(metric_keys, function(mk) {
    if (mk == "lift" || mk == "lift_0") {
      "Indexed by average effect. Measures the outcome's sensitivity to a small symmetric perturbation (±5%) around each attribute's current state. The interpretation of 5% depends on the selected Shift Type."
    } else if (grepl("^lift_", mk)) {
      pct <- gsub("lift_", "", mk)
      paste0(
        "Indexed by ", pct, "% lift. Measures how much the outcome changes when each attribute's distribution shifts by ", pct,
        "% — the meaning of ", pct, "% follows the selected Shift Type (% of current mean, fixed step, % toward top, or % of range)."
      )
    } else if (mk == "maxVmin") {
      "Indexed by best-vs-worst effect. Measures the outcome difference between setting all respondents to the top of each attribute versus all at the bottom."
    } else if (mk == "mi") {
      "Indexed by explanatory value. Measures the statistical strength of the relationship between each attribute and the outcome (mutual information), independent of intervention direction or shift type."
    } else {
      paste("Indexed by", mk)
    }
  })
  openxlsx::writeData(wb, lookup_sheet, "Index Description", startRow = 1, startCol = 5)
  for (di in seq_along(index_descriptions)) {
    openxlsx::writeData(wb, lookup_sheet, index_descriptions[di], startRow = di + 1, startCol = 5)
  }

  # Sequential column counter for remaining lookup columns (after E = Index descriptions)
  lk_col <- 6L

  # Weight options (if weighted results available)
  weight_opt_col <- NULL
  if (has_weights) {
    weight_opt_col <- lk_col
    weight_options <- c("Unweighted", "Weighted")
    openxlsx::writeData(wb, lookup_sheet, "Weight", startRow = 1, startCol = lk_col)
    for (wi in seq_along(weight_options)) {
      openxlsx::writeData(wb, lookup_sheet, weight_options[wi], startRow = wi + 1, startCol = lk_col)
    }
    lk_col <- lk_col + 1L
  }

  # Outcome-display options (always offered when any display-variant column
  # was detected). Labels in one column, storage keys in the next.
  outcome_display_opt_col <- NULL
  outcome_display_key_col <- NULL
  if (has_outcome_display) {
    outcome_display_opt_col <- lk_col
    openxlsx::writeData(wb, lookup_sheet, "Outcome Display", startRow = 1, startCol = lk_col)
    for (oi in seq_along(outcome_display_labels)) {
      openxlsx::writeData(wb, lookup_sheet, outcome_display_labels[oi],
        startRow = oi + 1, startCol = lk_col)
    }
    lk_col <- lk_col + 1L

    outcome_display_key_col <- lk_col
    openxlsx::writeData(wb, lookup_sheet, "Outcome Display Key",
      startRow = 1, startCol = lk_col)
    for (oi in seq_along(outcome_display_keys)) {
      openxlsx::writeData(wb, lookup_sheet, outcome_display_keys[oi],
        startRow = oi + 1, startCol = lk_col)
    }
    lk_col <- lk_col + 1L
  }

  # Shift-type options (Pass B). Labels column + keys column.
  shift_type_opt_col <- NULL
  shift_type_key_col <- NULL
  if (has_shift_type) {
    shift_type_opt_col <- lk_col
    openxlsx::writeData(wb, lookup_sheet, "Shift Type", startRow = 1, startCol = lk_col)
    for (oi in seq_along(shift_type_labels)) {
      openxlsx::writeData(wb, lookup_sheet, shift_type_labels[oi],
        startRow = oi + 1, startCol = lk_col)
    }
    lk_col <- lk_col + 1L

    shift_type_key_col <- lk_col
    openxlsx::writeData(wb, lookup_sheet, "Shift Type Key",
      startRow = 1, startCol = lk_col)
    for (oi in seq_along(shift_type_keys)) {
      openxlsx::writeData(wb, lookup_sheet, shift_type_keys[oi],
        startRow = oi + 1, startCol = lk_col)
    }
    lk_col <- lk_col + 1L
  }

  # (No Index By options column. In Excel-dynamic mode the per-battery
  # views are emitted as separate dashboard tabs by bn_impact_write rather
  # than driven by a dropdown — see `battery_filter` argument.)

  # ---------------------------------------------------------------------------
  # Styles
  # ---------------------------------------------------------------------------
  styles <- list(
    title     = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header    = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE,
                                      border = "TopBottom", borderStyle = "medium",
                                      fgFill = "#D9D9D9"),
    center    = openxlsx::createStyle(numFmt = "0", halign = "center"),
    left      = openxlsx::createStyle(halign = "left"),
    total_impact = openxlsx::createStyle(numFmt = "0.0%", halign = "center"),
    separator = openxlsx::createStyle(fgFill = "black"),
    neg_sign  = openxlsx::createStyle(textDecoration = c("bold", "italic")),
    insig     = openxlsx::createStyle(bgFill = "black", fontColour = "white"),
    dropdown_label = openxlsx::createStyle(textDecoration = "bold", halign = "right"),
    dropdown_cell  = openxlsx::createStyle(
      border = "Bottom", borderStyle = "thin", halign = "center"
    )
  )

  # ---------------------------------------------------------------------------
  # Layout
  # ---------------------------------------------------------------------------
  row_title <- 2L
  row_subtitle <- if (!is.null(sub_title)) 3L else NULL
  row_dropdowns <- if (!is.null(sub_title)) 5L else 4L
  col_data_start <- 2L

  # Determine leading columns. Battery info is on the Results sheet (so the
  # SUMPRODUCT/COUNTIF for per-battery scoping can filter on it), but it
  # isn't shown as a dashboard column — per-battery views live on their own
  # tabs, and the main tab doesn't need a redundant grouping label.
  id_col <- if ("Variable" %in% names(table)) "Variable" else "Community"
  has_label <- "Label" %in% names(table)
  has_community <- "Community" %in% names(table) && id_col != "Community"
  leading_cols <- id_col
  if (has_community) leading_cols <- c(leading_cols, "Community")
  if (has_label) leading_cols <- c(leading_cols, "Label")
  n_leading <- length(leading_cols)
  # Column index of Battery within the Results sheet (1-indexed). Used by
  # the within-battery SUMPRODUCT/COUNTIF formulas. The static-write
  # injection above places Battery after Community/Variable so its index
  # in `table` matches its column position on the Results sheet.
  battery_results_col <- if (has_battery_col) {
    which(names(table) == "Battery")
  } else NA_integer_
  # Column index of the BatteryGroup_<filter> helper column on Results.
  # Set only when filter_kind == "group". Used by the SUMPRODUCT mask
  # below (filter mask = (BatteryGroup_<name> = 1)) instead of the
  # battery-column equality.
  battery_group_results_col <- if (filter_kind == "group") {
    which(names(table) == paste0("BatteryGroup_", battery_filter))
  } else NA_integer_

  # Column layout per subgroup: Metric (hidden) | P Value (hidden) | Index (visible)
  n_total_cols <- n_leading + length(sgs) * 3

  raw_metric_cols <- integer(length(sgs))
  p_val_cols <- integer(length(sgs))
  index_cols_pos <- integer(length(sgs))
  hidden_cols <- integer(0)

  for (sg_i in seq_along(sgs)) {
    sg_offset <- col_data_start + n_leading + (sg_i - 1) * 3
    raw_metric_cols[sg_i] <- sg_offset
    p_val_cols[sg_i] <- sg_offset + 1
    index_cols_pos[sg_i] <- sg_offset + 2
    hidden_cols <- c(hidden_cols, sg_offset, sg_offset + 1)
  }

  # ---------------------------------------------------------------------------
  # Title and subtitle
  # ---------------------------------------------------------------------------
  openxlsx::writeData(wb, dash_sheet, title, startRow = row_title, startCol = col_data_start)
  openxlsx::addStyle(wb, dash_sheet, style = styles$title,
    rows = row_title, cols = col_data_start, stack = TRUE)

  if (!is.null(row_subtitle)) {
    openxlsx::writeData(wb, dash_sheet, sub_title, startRow = row_subtitle, startCol = col_data_start)
    openxlsx::addStyle(wb, dash_sheet, style = styles$sub_title,
      rows = row_subtitle, cols = col_data_start, stack = TRUE)
  }

  # ---------------------------------------------------------------------------
  # Dropdown controls — stacked vertically
  # When n_leading >= 2, controls go in leading columns (B, C)
  # When n_leading < 2, controls go over first index column area
  # ---------------------------------------------------------------------------
  label_col <- col_data_start
  if (n_leading >= 2L) {
    cell_col <- col_data_start + 1L
  } else {
    cell_col <- index_cols_pos[1]
  }
  current_row <- row_dropdowns

  dropdown_cell_style <- openxlsx::createStyle(
    border = "Bottom", borderStyle = "thin", halign = "left"
  )

  # Metric dropdown
  metric_cell_col <- cell_col
  metric_cell_row <- current_row
  openxlsx::writeData(wb, dash_sheet, "Analysis: ", startRow = current_row, startCol = label_col)
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
    rows = current_row, cols = label_col, stack = TRUE)
  openxlsx::writeData(wb, dash_sheet, metric_labels[1], startRow = current_row, startCol = cell_col)
  openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
    rows = current_row, cols = cell_col, stack = TRUE)
  current_row <- current_row + 1L

  # Focus dropdown
  focus_cell_col <- cell_col
  focus_cell_row <- current_row
  openxlsx::writeData(wb, dash_sheet, "Focus: ", startRow = current_row, startCol = label_col)
  openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
    rows = current_row, cols = label_col, stack = TRUE)
  openxlsx::writeData(wb, dash_sheet, focus_options[1],
    startRow = current_row, startCol = cell_col)
  openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
    rows = current_row, cols = cell_col, stack = TRUE)

  current_row <- current_row + 1L

  # Weight dropdown (only when weighted results available)
  weight_cell_col <- NULL
  weight_cell_row <- NULL
  if (has_weights) {
    weight_cell_col <- cell_col
    weight_cell_row <- current_row
    openxlsx::writeData(wb, dash_sheet, "Weight: ", startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)
    openxlsx::writeData(wb, dash_sheet, "Unweighted", startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
      rows = current_row, cols = cell_col, stack = TRUE)

    weight_range <- paste0(lookup_sheet, "!$", num2let(weight_opt_col), "$2:$",
      num2let(weight_opt_col), "$3")
    openxlsx::dataValidation(wb, dash_sheet,
      col = cell_col, rows = current_row,
      type = "list", value = weight_range)
    current_row <- current_row + 1L
  }

  # Outcome Display dropdown (always offered when the engine emits both
  # outcome-display variants, which it always does now). Controls whether the
  # lift / maxVmin columns pull their proportional or absolute variant.
  outcome_display_cell_col <- NULL
  outcome_display_cell_row <- NULL
  if (has_outcome_display) {
    outcome_display_cell_col <- cell_col
    outcome_display_cell_row <- current_row
    openxlsx::writeData(wb, dash_sheet, "Outcome Display: ",
      startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)
    openxlsx::writeData(wb, dash_sheet, display_default_label,
      startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
      rows = current_row, cols = cell_col, stack = TRUE)

    outcome_display_range <- paste0(lookup_sheet, "!$",
      num2let(outcome_display_opt_col), "$2:$",
      num2let(outcome_display_opt_col), "$",
      length(outcome_display_labels) + 1)
    openxlsx::dataValidation(wb, dash_sheet,
      col = cell_col, rows = current_row,
      type = "list", value = outcome_display_range)
    current_row <- current_row + 1L
  }

  # Shift Type dropdown (Pass B). Controls whether lift columns pull their
  # proportional-shift or absolute-shift variant. Mathematically equivalent
  # to Outcome Display for maxVmin (affects nothing there — maxVmin is
  # shift-independent) and for MI (no variants at all); so grey-out logic
  # below suppresses the warning only when it's truly active for a given
  # metric + focus + weight combination.
  shift_type_cell_col <- NULL
  shift_type_cell_row <- NULL
  if (has_shift_type) {
    shift_type_cell_col <- cell_col
    shift_type_cell_row <- current_row
    openxlsx::writeData(wb, dash_sheet, "Shift Type: ",
      startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash_sheet, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)
    openxlsx::writeData(wb, dash_sheet, shift_default_label,
      startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash_sheet, style = dropdown_cell_style,
      rows = current_row, cols = cell_col, stack = TRUE)

    shift_type_range <- paste0(lookup_sheet, "!$",
      num2let(shift_type_opt_col), "$2:$",
      num2let(shift_type_opt_col), "$",
      length(shift_type_labels) + 1)
    openxlsx::dataValidation(wb, dash_sheet,
      col = cell_col, rows = current_row,
      type = "list", value = shift_type_range)
    current_row <- current_row + 1L
  }

  # (No Index By dropdown — per-battery views are separate tabs.)

  # Build metric key lookup formula
  dash_sheet_escaped <- gsub("'", "''", dash_sheet)
  metric_cell_abs <- paste0("'", dash_sheet_escaped, "'!$", num2let(metric_cell_col), "$", metric_cell_row)
  mk_lookup <- paste0(
    "INDEX(", lookup_sheet, "!$C$2:$C$", length(metric_labels) + 1,
    ",MATCH(", metric_cell_abs, ",", lookup_sheet, "!$B$2:$B$", length(metric_labels) + 1, ",0))"
  )

  # Warning column = next column after the dropdown cell
  warning_col <- cell_col + 1L

  # Pass-B grey-out rule: when Shift Type = Absolute AND metric is a lift,
  # focus AND weight have no mathematical effect on the lift value (the
  # shift magnitude is fixed to `lift` regardless of focus / weighting).
  # We surface this as a grey italic note on both dropdowns so users
  # don't expect changes when toggling.
  shift_is_abs_and_lift <- if (has_shift_type) {
    shift_cell_let <- num2let(shift_type_cell_col)
    paste0(
      "AND(", shift_cell_let, shift_type_cell_row, "=\"Fixed Step\",",
      "LEFT(", mk_lookup, ",4)=\"lift\")"
    )
  } else {
    "FALSE"
  }

  # Focus warning — to the right of the Focus control (same row).
  # Priority: red error for brand+maxVmin/mi (invalid lookup), then grey
  # italic note for absolute-shift + lift (valid but unaffected by focus).
  focus_cell_let <- num2let(focus_cell_col)
  red_warning <- openxlsx::createStyle(fontColour = "#FF0000", textDecoration = "bold")
  grey_italic <- openxlsx::createStyle(fontColour = "#888888", textDecoration = "italic")
  red_rule <- paste0(
    "AND(", focus_cell_let, focus_cell_row, "<>\"Market\",",
    "OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\"))"
  )
  focus_grey_rule <- paste0(
    "AND(NOT(", red_rule, "),", shift_is_abs_and_lift, ")"
  )

  focus_warning_formula <- paste0(
    "IF(", red_rule, ",",
      num2let(metric_cell_col), metric_cell_row, "&\" must have a Market focus\",",
    "IF(", shift_is_abs_and_lift, ",",
      "\"Focus does not affect this metric when shift is a fixed step\",",
      "\"\"))"
  )
  openxlsx::writeFormula(wb, dash_sheet, x = focus_warning_formula,
    startRow = focus_cell_row, startCol = warning_col)
  openxlsx::conditionalFormatting(wb, dash_sheet,
    cols = warning_col, rows = focus_cell_row,
    style = red_warning, type = "expression", rule = red_rule)
  openxlsx::conditionalFormatting(wb, dash_sheet,
    cols = warning_col, rows = focus_cell_row,
    style = grey_italic, type = "expression", rule = focus_grey_rule)

  # Red background on Focus dropdown cell itself (only when the red rule fires)
  red_cell <- openxlsx::createStyle(bgFill = "#FF0000", fontColour = "#FFFFFF")
  openxlsx::conditionalFormatting(wb, dash_sheet,
    cols = focus_cell_col, rows = focus_cell_row,
    style = red_cell, type = "expression", rule = red_rule)

  # Weight warning — to the right of the Weight control (same row). Same
  # priority logic: maxVmin/mi already-grey note, then absolute-shift-lift
  # grey note.
  if (has_weights) {
    weight_existing_rule <- paste0("OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\")")
    weight_grey_rule <- paste0(
      "OR(", weight_existing_rule, ",", shift_is_abs_and_lift, ")"
    )
    weight_warning_formula <- paste0(
      "IF(", weight_existing_rule, ",",
        "\"Weights don't affect this metric\",",
      "IF(", shift_is_abs_and_lift, ",",
        "\"Weights don't affect this metric when shift is a fixed step\",",
        "\"\"))"
    )
    openxlsx::writeFormula(wb, dash_sheet, x = weight_warning_formula,
      startRow = weight_cell_row, startCol = warning_col)
    openxlsx::conditionalFormatting(wb, dash_sheet,
      cols = warning_col, rows = weight_cell_row,
      style = grey_italic, type = "expression", rule = weight_grey_rule)
  }

  # Outcome Display warning — grey italic "doesn't affect this metric"
  # when metric = mi (mi has no display variants).
  if (has_outcome_display) {
    display_warn_rule <- paste0(mk_lookup, "=\"mi\"")
    display_warning_formula <- paste0(
      "IF(", display_warn_rule, ",",
        "\"Outcome display doesn't affect this metric\",\"\")"
    )
    openxlsx::writeFormula(wb, dash_sheet, x = display_warning_formula,
      startRow = outcome_display_cell_row, startCol = warning_col)
    openxlsx::conditionalFormatting(wb, dash_sheet,
      cols = warning_col, rows = outcome_display_cell_row,
      style = grey_italic, type = "expression", rule = display_warn_rule)
  }

  # Shift Type warning — grey italic "doesn't affect this metric" when
  # metric = maxVmin or mi (neither is computed via bn_freq_prob_shift).
  if (has_shift_type) {
    shift_warn_rule <- paste0(
      "OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\")"
    )
    shift_warning_formula <- paste0(
      "IF(", shift_warn_rule, ",",
        "\"Shift type doesn't affect this metric\",\"\")"
    )
    openxlsx::writeFormula(wb, dash_sheet, x = shift_warning_formula,
      startRow = shift_type_cell_row, startCol = warning_col)
    openxlsx::conditionalFormatting(wb, dash_sheet,
      cols = warning_col, rows = shift_type_cell_row,
      style = grey_italic, type = "expression", rule = shift_warn_rule)
  }

  # Dynamic Focus column in _lookup
  dyn_focus_col <- lk_col
  dyn_focus_let <- num2let(dyn_focus_col)
  lk_col <- lk_col + 1L
  openxlsx::writeData(wb, lookup_sheet, "Dynamic Focus", startRow = 1, startCol = dyn_focus_col)
  openxlsx::writeData(wb, lookup_sheet, "Market", startRow = 2, startCol = dyn_focus_col)
  if (length(focus_options) > 1) {
    for (fi in 2:length(focus_options)) {
      f_formula <- paste0(
        "IF(OR(", mk_lookup, "=\"maxVmin\",", mk_lookup, "=\"mi\"),\"\",A", fi + 1, ")"
      )
      openxlsx::writeFormula(wb, lookup_sheet, x = f_formula,
        startRow = fi + 1, startCol = dyn_focus_col)
    }
  }

  # Focus Count
  focus_count_col <- lk_col
  focus_count_let <- num2let(focus_count_col)
  lk_col <- lk_col + 1L
  openxlsx::writeData(wb, lookup_sheet, "Focus Count", startRow = 1, startCol = focus_count_col)
  count_formula <- paste0("COUNTIF($", dyn_focus_let, "$2:$", dyn_focus_let, "$", length(focus_options) + 1, ",\"<>\"&\"\")")
  openxlsx::writeFormula(wb, lookup_sheet, x = count_formula, startRow = 2, startCol = focus_count_col)

  # Data validation for dropdowns
  focus_range <- paste0("OFFSET(", lookup_sheet, "!$", dyn_focus_let, "$2,0,0,", lookup_sheet, "!$", focus_count_let, "$2,1)")
  metric_range <- paste0(lookup_sheet, "!$B$2:$B$", length(metric_labels) + 1)
  openxlsx::dataValidation(wb, dash_sheet,
    col = focus_cell_col, rows = focus_cell_row,
    type = "list", value = focus_range)
  openxlsx::dataValidation(wb, dash_sheet,
    col = metric_cell_col, rows = metric_cell_row,
    type = "list", value = metric_range)

  # ---------------------------------------------------------------------------
  # Data table
  # ---------------------------------------------------------------------------
  row_data_start <- current_row + 1L

  # Write leading headers
  for (ci in seq_along(leading_cols)) {
    openxlsx::writeData(wb, dash_sheet, gsub("_", " ", leading_cols[ci]),
      startRow = row_data_start, startCol = col_data_start + ci - 1)
  }

  # Write subgroup headers (Metric, P Value, Index per subgroup)
  for (sg_i in seq_along(sgs)) {
    sg_label <- gsub("_", " ", sgs[sg_i])
    openxlsx::writeData(wb, dash_sheet, paste(sg_label, "Metric"),
      startRow = row_data_start, startCol = raw_metric_cols[sg_i])
    openxlsx::writeData(wb, dash_sheet, paste(sg_label, "P Value"),
      startRow = row_data_start, startCol = p_val_cols[sg_i])
    openxlsx::writeData(wb, dash_sheet, sg_label,
      startRow = row_data_start, startCol = index_cols_pos[sg_i])
  }

  all_header_cols <- seq(col_data_start, col_data_start + n_total_cols - 1)
  openxlsx::addStyle(wb, dash_sheet, style = styles$header,
    rows = row_data_start, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)
  # Pin the header row height (same rationale as the static path) — keeps
  # multi-word subgroup labels readable on two lines without auto-fit
  # ballooning the row from any other cell content.
  openxlsx::setRowHeights(wb, dash_sheet, rows = row_data_start, heights = 36)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "TopBottomLeft", borderStyle = "medium"),
    rows = row_data_start, cols = min(all_header_cols), stack = TRUE)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "TopBottomRight", borderStyle = "medium"),
    rows = row_data_start, cols = max(all_header_cols), stack = TRUE)

  # Write leading columns — static values. Iterate over n_dash_rows (this
  # tab's display height) rather than n_results_rows so per-battery tabs
  # don't carry trailing blank rows below the filtered IV set.
  for (ri in seq_len(n_dash_rows)) {
    for (ci in seq_along(leading_cols)) {
      val <- as.character(table[[leading_cols[ci]]][ri])
      openxlsx::writeData(wb, dash_sheet, val,
        startRow = row_data_start + ri, startCol = col_data_start + ci - 1)
    }
  }

  # Excel references
  focus_ref <- paste0("$", num2let(focus_cell_col), "$", focus_cell_row)
  metric_ref <- paste0("$", num2let(metric_cell_col), "$", metric_cell_row)
  metric_key_range <- paste0(lookup_sheet, "!$B$2:$B$", length(metric_labels) + 1)
  metric_key_col <- paste0(lookup_sheet, "!$C$2:$C$", length(metric_labels) + 1)

  if (has_weights) {
    # Dynamic sheet reference via INDIRECT based on Weight dropdown
    weight_ref <- paste0("$", num2let(weight_cell_col), "$", weight_cell_row)
    # Build sheet name: IF(weight="Weighted", weighted_sheet, results_sheet)
    active_sheet <- paste0(
      "IF(", weight_ref, "=\"Weighted\",\"", weighted_results_sheet, "\",\"", results_sheet, "\")"
    )
    results_header_range <- paste0("INDIRECT(", active_sheet, "&\"!$1:$1\")")
    results_all_rows <- paste0("INDIRECT(", active_sheet, "&\"!$2:$", n_results_rows + 1, "\")")
    results_count_ref <- paste0("INDIRECT(", active_sheet, "&\"!$A$2:$A$", n_results_rows + 1, "\")")
  } else {
    results_header_range <- paste0(results_sheet, "!$1:$1")
    results_all_rows <- paste0(results_sheet, "!$2:$", n_results_rows + 1)
    results_count_ref <- paste0(results_sheet, "!$A$2:$A$", n_results_rows + 1)
  }

  # Write INDEX/MATCH formulas. data_rows spans this dashboard's display
  # height only — Results-sheet ranges further inside formulas still use
  # n_results_rows so they cover the full data.
  data_rows <- seq(row_data_start + 1, row_data_start + n_dash_rows)

  # Build the fully-qualified column name that corresponds to the current
  # Metric / Focus / Outcome-Display selection.
  #
  # Engine's column shape:
  #   Market:  {sg}_{mk}                 for mi
  #            {sg}_{mk}_{display}       for lift* and maxVmin
  #   Brand:   {sg}_{mk}_{focus}         for mi (mi is brand-invariant —
  #                                      falls back to Market)
  #            {sg}_{mk}_{focus}_{display} for lift* brand-specific
  #            {sg}_{mk}_{display}       for maxVmin (maxVmin is not brand-
  #                                      specific — falls back to Market)
  #
  # Because mi and maxVmin fall back to Market (no brand-specific columns),
  # we nest: focus=Market OR metric=mi OR metric=maxVmin use the Market
  # shape; brand lift uses the brand shape.
  if (has_outcome_display) {
    display_ref <- paste0("$", num2let(outcome_display_cell_col), "$", outcome_display_cell_row)
    # Map "Proportional"/"Absolute" (the label shown to the user) → "propdisplay"/"absdisplay".
    display_key <- paste0(
      "INDEX(", lookup_sheet, "!$", num2let(outcome_display_key_col), "$2:$",
      num2let(outcome_display_key_col), "$", length(outcome_display_keys) + 1,
      ",MATCH(", display_ref, ",",
      lookup_sheet, "!$", num2let(outcome_display_opt_col), "$2:$",
      num2let(outcome_display_opt_col), "$", length(outcome_display_labels) + 1, ",0))"
    )
    display_suffix <- paste0("&\"_\"&", display_key)
  } else {
    display_suffix <- "\"\""
  }

  # Shift tag resolves to "propshift" or "absshift" based on the Shift Type
  # dropdown — only used for lift columns (maxVmin and mi are shift-invariant).
  if (has_shift_type) {
    shift_ref <- paste0("$", num2let(shift_type_cell_col), "$", shift_type_cell_row)
    shift_key <- paste0(
      "INDEX(", lookup_sheet, "!$", num2let(shift_type_key_col), "$2:$",
      num2let(shift_type_key_col), "$", length(shift_type_keys) + 1,
      ",MATCH(", shift_ref, ",",
      lookup_sheet, "!$", num2let(shift_type_opt_col), "$2:$",
      num2let(shift_type_opt_col), "$", length(shift_type_labels) + 1, ",0))"
    )
    lift_shift_suffix <- paste0("&\"_\"&", shift_key)
  } else {
    lift_shift_suffix <- "\"\""
  }

  # Build the fully-qualified column name that corresponds to the current
  # Metric / Focus / Outcome-Display / Shift-Type selection.
  #
  # Column shapes (post Pass B):
  #   Market lift:      {sg}_{lift_N}_{shift}_{display}
  #   Brand lift:       {sg}_{lift_N}_{brand}_{shift}_{display}
  #   maxVmin (Market): {sg}_maxVmin_{display}            (shift-invariant)
  #   maxVmin (Brand):  falls back to Market (no brand-specific maxVmin)
  #   mi:               {sg}_mi                          (no variants)
  .col_name_formula <- function(sg, mk) {
    paste0(
      "IF(", focus_ref, "=\"Market\",",
        # --- Market focus ---
        "IF(", mk, "=\"mi\",",
          "\"", sg, "_\"&", mk, ",",
          "IF(", mk, "=\"maxVmin\",",
            # maxVmin: shift-independent, only display suffix
            "\"", sg, "_\"&", mk, display_suffix, ",",
            # lift: shift + display
            "\"", sg, "_\"&", mk, lift_shift_suffix, display_suffix,
          ")",
        "),",
        # --- Non-Market (brand) focus ---
        "IF(OR(", mk, "=\"maxVmin\",", mk, "=\"mi\"),",
          # maxVmin/mi are brand-invariant — fall back to Market shape
          "IF(", mk, "=\"mi\",",
            "\"", sg, "_\"&", mk, ",",
            "\"", sg, "_\"&", mk, display_suffix,
          "),",
          # Brand lift: {sg}_lift_N_{brand}_{shift}_{display}
          "\"", sg, "_\"&", mk, "&\"_\"&", focus_ref, lift_shift_suffix, display_suffix,
        ")",
      ")"
    )
  }

  mk <- paste0(
    "INDEX(", metric_key_col, ",MATCH(", metric_ref, ",", metric_key_range, ",0))"
  )

  for (sg_i in seq_along(sgs)) {
    sg <- sgs[sg_i]
    col_name_f <- .col_name_formula(sg, mk)
    match_col <- paste0("MATCH(", col_name_f, ",", results_header_range, ",0)")

    # Significance source:
    #   boot_applied = FALSE → static MI chi-squared p-value `<sg>_p_val`
    #   boot_applied = TRUE  → bootstrap p-value of the *currently selected*
    #     metric column: `<col_name_f>_p_value`. The dashboard's blackout
    #     rule (p > 0.10) follows whichever metric the user is viewing.
    if (boot_applied) {
      pval_col_name <- paste0("(", col_name_f, ")&\"_p_value\"")
    } else {
      pval_col_name <- paste0("\"", sg, "_p_val\"")
    }
    pval_match <- paste0("MATCH(", pval_col_name, ",", results_header_range, ",0)")

    # ID-column cell reference for THIS dashboard row. Column anchored
    # ($), row left relative — so when Excel sorts the table, the formula
    # in the moved <tr> auto-adjusts to point at the same row's ID cell
    # (e.g. row 5's "$B5" becomes "$B12" when that row sorts to position 12).
    # That's how the metric formula below stays bonded to its displayed
    # Variable / Community label rather than to a hardcoded Results row.
    id_col_let <- num2let(col_data_start)

    for (ri in seq_along(data_rows)) {
      row <- data_rows[ri]
      id_cell_ref <- paste0("$", id_col_let, row)
      # Find the row on Results whose ID column equals this dashboard row's
      # ID. Sort-stable: id_cell_ref shifts with the row; results_count_ref
      # is fully absolute and stays put.
      match_row <- paste0("MATCH(", id_cell_ref, ",", results_count_ref, ",0)")

      # Raw metric (hidden — blank if missing/zero)
      raw_let <- num2let(raw_metric_cols[sg_i])
      raw_formula <- paste0(
        "IFERROR(IF(INDEX(", results_all_rows, ",", match_row, ",", match_col, ")=0,\"\",",
        "INDEX(", results_all_rows, ",", match_row, ",", match_col, ")),\"\")"
      )
      openxlsx::writeFormula(wb, dash_sheet, x = raw_formula,
        startRow = row, startCol = raw_metric_cols[sg_i])

      # P-value (hidden — 0 if raw metric is blank, no blackout)
      pval_formula <- paste0(
        "IF(", raw_let, row, "=\"\",0,",
        "IFERROR(INDEX(", results_all_rows, ",", match_row, ",", pval_match, "),\"\"))"
      )
      openxlsx::writeFormula(wb, dash_sheet, x = pval_formula,
        startRow = row, startCol = p_val_cols[sg_i])

      # Index denominator:
      #   filter_kind="none"    → mean(ABS(raw)) across every row in Results
      #   filter_kind="battery" → mean(ABS(raw)) within rows whose Battery=<name>
      #   filter_kind="group"   → mean(ABS(raw)) within rows whose
      #                           BatteryGroup_<name>=1
      if (filter_kind != "none") {
        mask_col_idx <- if (filter_kind == "battery") battery_results_col
                        else battery_group_results_col
        mask_let <- num2let(mask_col_idx)
        mask_range <- if (has_weights) {
          paste0("INDIRECT(", active_sheet, "&\"!$", mask_let, "$2:$",
            mask_let, "$", n_results_rows + 1, "\")")
        } else {
          paste0(results_sheet, "!$", mask_let, "$2:$",
            mask_let, "$", n_results_rows + 1)
        }
        mask_lit <- if (filter_kind == "battery") {
          paste0("\"", gsub('"', '""', battery_filter), "\"")
        } else {
          "1"
        }
        denom <- paste0(
          "IFERROR((SUMPRODUCT((", mask_range, "=", mask_lit, ")",
            "*ABS(INDEX(", results_all_rows, ",0,", match_col, ")))",
          "/COUNTIF(", mask_range, ",", mask_lit, ")),0)"
        )
      } else {
        denom <- paste0(
          "(SUMPRODUCT(ABS(INDEX(", results_all_rows, ",0,", match_col, ")))",
          "/ROWS(", results_count_ref, "))"
        )
      }
      # Numerator uses the same id-cell-keyed lookup as the raw / pval cells
      # above so the row stays correctly linked under sort.
      cell_formula <- paste0(
        "IFERROR(ABS(INDEX(", results_all_rows, ",", match_row, ",", match_col, "))/",
        denom, "*100,\"\")"
      )
      openxlsx::writeFormula(wb, dash_sheet, x = cell_formula,
        startRow = row, startCol = index_cols_pos[sg_i])
    }
  }

  # Hide raw metric + p-value columns
  openxlsx::setColWidths(wb, dash_sheet, cols = hidden_cols,
    widths = 8.43, hidden = rep(TRUE, length(hidden_cols)))

  # Style index columns
  openxlsx::addStyle(wb, dash_sheet, style = styles$center,
    rows = data_rows, cols = index_cols_pos, gridExpand = TRUE, stack = TRUE)

  # Left-align leading columns
  for (lci in seq_len(n_leading)) {
    openxlsx::addStyle(wb, dash_sheet, style = styles$left,
      rows = data_rows, cols = col_data_start + lci - 1, gridExpand = TRUE, stack = TRUE)
  }

  # Column widths — apply per leading-col position. Battery isn't a
  # dashboard column (it's only on the Results sheet for SUMPRODUCT
  # filtering), so it doesn't appear here.
  for (lci in seq_along(leading_cols)) {
    nm <- leading_cols[lci]
    w <- switch(nm,
      Community = community_width,
      Label     = label_width,
      variable_width)
    openxlsx::setColWidths(wb, dash_sheet,
      cols = col_data_start + lci - 1, widths = w)
  }

  # ---------------------------------------------------------------------------
  # Separator, Total Impact, Base
  # ---------------------------------------------------------------------------
  separator_row <- max(data_rows) + 1
  total_impact_row <- separator_row + 1
  base_row <- total_impact_row + 1

  openxlsx::addStyle(wb, dash_sheet, style = styles$separator,
    rows = separator_row, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)
  openxlsx::setRowHeights(wb, dash_sheet, rows = separator_row, heights = 0)

  openxlsx::writeData(wb, dash_sheet, "Total Impact",
    startRow = total_impact_row, startCol = col_data_start)

  for (sg_i in seq_along(sgs)) {
    sg <- sgs[sg_i]
    col_name_f <- .col_name_formula(sg, mk)
    match_col <- paste0("MATCH(", col_name_f, ",", results_header_range, ",0)")

    sum_expr <- paste0("SUMPRODUCT(ABS(INDEX(", results_all_rows, ",0,", match_col, ")))")

    if (filter_kind != "none") {
      # Total Impact scoped to the dashboard's battery / group — same
      # SUMPRODUCT / COUNTIF filter used for the row-level index denominator.
      mask_col_idx <- if (filter_kind == "battery") battery_results_col
                      else battery_group_results_col
      mask_let <- num2let(mask_col_idx)
      mask_range <- if (has_weights) {
        paste0("INDIRECT(", active_sheet, "&\"!$", mask_let, "$2:$",
          mask_let, "$", n_results_rows + 1, "\")")
      } else {
        paste0(results_sheet, "!$", mask_let, "$2:$",
          mask_let, "$", n_results_rows + 1)
      }
      mask_lit <- if (filter_kind == "battery") {
        paste0("\"", gsub('"', '""', battery_filter), "\"")
      } else {
        "1"
      }
      ti_formula <- paste0(
        "IFERROR(SUMPRODUCT((", mask_range, "=", mask_lit, ")",
          "*ABS(INDEX(", results_all_rows, ",0,", match_col, ")))",
        "/COUNTIF(", mask_range, ",", mask_lit, "),\"\")"
      )
    } else {
      ti_formula <- paste0(
        "IFERROR(IF(", sum_expr, "=0,\"\",",
        sum_expr, "/ROWS(", results_count_ref, ")),\"\")"
      )
    }
    openxlsx::writeFormula(wb, dash_sheet, x = ti_formula,
      startRow = total_impact_row, startCol = index_cols_pos[sg_i])
  }

  openxlsx::addStyle(wb, dash_sheet, style = styles$total_impact,
    rows = total_impact_row, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)

  # Base row — sits just below Total Impact. Renders regardless of whether the
  # selected metric produced values (bases live in the data, not derived from
  # the lift calc). Column lookup format: "{sg}_base" for Market focus, or
  # "{sg}_base_{focus}" for brand focuses. Value pulled from the first row of
  # the results sheet, since base is (approximately) constant across IVs.
  openxlsx::writeData(wb, dash_sheet, "Base",
    startRow = base_row, startCol = col_data_start)

  for (sg_i in seq_along(sgs)) {
    sg <- sgs[sg_i]
    base_col_name <- paste0(
      "IF(", focus_ref, "=\"Market\",\"", sg, "_base\",",
        "\"", sg, "_base_\"&", focus_ref, ")"
    )
    base_match_col <- paste0("MATCH(", base_col_name, ",",
      results_header_range, ",0)")
    base_formula <- paste0(
      "IFERROR(ROUND(INDEX(", results_all_rows, ",1,", base_match_col, "),0),\"\")"
    )
    openxlsx::writeFormula(wb, dash_sheet, x = base_formula,
      startRow = base_row, startCol = index_cols_pos[sg_i])
  }

  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(numFmt = "0", halign = "center",
      fontColour = "#595959"),
    rows = base_row, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)

  # Outer box around table (header through Base row)
  table_rows <- seq(row_data_start, base_row)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "Left", borderStyle = "medium"),
    rows = table_rows, cols = min(all_header_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "Right", borderStyle = "medium"),
    rows = table_rows, cols = max(all_header_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dash_sheet,
    style = openxlsx::createStyle(border = "Bottom", borderStyle = "medium"),
    rows = base_row, cols = all_header_cols, gridExpand = TRUE, stack = TRUE)

  # Dynamic footer (one blank row after table)
  footer_start <- base_row + 2
  index_desc_range <- paste0(lookup_sheet, "!$E$2:$E$", length(metric_labels) + 1)
  footer_formula <- paste0(
    "\"", engine_footer, ". \"&INDEX(", index_desc_range,
    ",MATCH(", metric_ref, ",", metric_key_range, ",0))"
  )
  openxlsx::writeFormula(wb, dash_sheet, x = footer_formula,
    startRow = footer_start, startCol = col_data_start)
  openxlsx::writeData(wb, dash_sheet, "Bold italicized index means a negative relationship",
    startRow = footer_start + 1, startCol = col_data_start)
  openxlsx::writeData(wb, dash_sheet, "Black cells mean an insignificant relationship",
    startRow = footer_start + 2, startCol = col_data_start)

  if (!is.null(min_base_for_lift)) {
    openxlsx::writeData(wb, dash_sheet,
      paste0("Lift impacts are not calculated when the base is below ", min_base_for_lift),
      startRow = footer_start + 3, startCol = col_data_start)
  }

  # ---------------------------------------------------------------------------
  # Conditional formatting
  # ---------------------------------------------------------------------------
  for (sg_i in seq_along(index_cols_pos)) {
    i <- index_cols_pos[sg_i]
    raw_col <- raw_metric_cols[sg_i]
    pval_col <- p_val_cols[sg_i]

    # Colour scale (lowest priority)
    openxlsx::conditionalFormatting(wb, dash_sheet, cols = i, rows = data_rows,
      style = c("#f66a6e", "#feea8a", "#66bd7d"), type = "colourScale")

    # Bold italic for negative raw metric
    raw_col_let <- num2let(raw_col)
    neg_formula <- paste0(raw_col_let, data_rows[1], "<0")
    openxlsx::conditionalFormatting(wb, dash_sheet, cols = i, rows = data_rows,
      style = styles$neg_sign, type = "expression", rule = neg_formula)

    # Blackout for insignificant p-value (highest priority)
    pval_col_let <- num2let(pval_col)
    p_formula <- paste0(pval_col_let, data_rows[1], ">0.1")
    openxlsx::conditionalFormatting(wb, dash_sheet, cols = i, rows = data_rows,
      style = styles$insig, type = "expression", rule = p_formula)
  }

  # (Per-battery dashboards are pre-filtered tabs rather than a dropdown,
  # so no row-hiding rule is needed.)

  # ---------------------------------------------------------------------------
  # Borders, freeze, filter
  # ---------------------------------------------------------------------------
  oxl_outer_box(wb, dash_sheet,
    row_start = row_data_start, row_end = max(data_rows),
    col_start = min(all_header_cols), col_end = max(all_header_cols),
    borderStyle = "medium")
  oxl_outer_box(wb, dash_sheet,
    row_start = total_impact_row, row_end = total_impact_row,
    col_start = min(all_header_cols), col_end = max(all_header_cols),
    borderStyle = "medium")
  oxl_outer_box(wb, dash_sheet,
    row_start = base_row, row_end = base_row,
    col_start = min(all_header_cols), col_end = max(all_header_cols),
    borderStyle = "medium")

  openxlsx::freezePane(wb, dash_sheet,
    firstActiveRow = row_data_start + 1,
    firstActiveCol = col_data_start + n_leading)

  openxlsx::addFilter(wb, dash_sheet, rows = row_data_start, cols = all_header_cols)

  invisible(wb)
}
