# =============================================================================
# Native DT impact dashboard for app_deliverable_network_drivers
#
# Replaces the embedded bn_report HTML+JS dashboard with Shiny inputs in the
# sidebar + a reactive DT::datatable in the main area. Shares the dimension
# parsing logic with bn_report via .bn_report_impacts_metadata().
# =============================================================================

# ---- Pure data: pick the column name for a (subgroup, focus, metric) -------

#' @noRd
.network_drivers_impacts_metric_col <- function(sg, metric_key, focus,
                                                outcome_display, shift_type) {
  # MI has no outcome-display or shift variant — bare key.
  if (identical(metric_key, "mi")) {
    return(paste0(sg, "_", metric_key))
  }
  # maxVmin is shift-independent: base + display.
  if (identical(metric_key, "maxVmin")) {
    return(paste0(sg, "_", metric_key, "_", outcome_display))
  }
  # Lift metrics: market = base_shift_display, brand = base_focus_shift_display.
  if (!is.null(focus) && nzchar(focus) && focus != "Market") {
    return(paste0(sg, "_", metric_key, "_", focus, "_",
                  shift_type, "_", outcome_display))
  }
  paste0(sg, "_", metric_key, "_", shift_type, "_", outcome_display)
}


# ---- Pure data: which rows are visible given the Index By filter -----------

#' @noRd
.network_drivers_impacts_visible_idx <- function(metadata, tbl, index_by) {
  if (is.null(index_by) || identical(index_by, "All")) {
    return(seq_len(nrow(tbl)))
  }
  # Group filter (battery_groups are sets of batteries flattened to IVs).
  group_ivs <- if (!is.null(metadata$battery_group_ivs)) {
    metadata$battery_group_ivs[[index_by]]
  } else NULL
  if (!is.null(group_ivs)) {
    return(which(as.character(tbl[[metadata$id_col_name]]) %in% group_ivs))
  }
  # Battery filter: rows whose IV maps to this battery.
  if (metadata$has_battery && !is.null(metadata$iv_to_battery)) {
    iv_names <- as.character(tbl[[metadata$id_col_name]])
    batteries_for_rows <- unname(metadata$iv_to_battery[iv_names])
    batteries_for_rows[is.na(batteries_for_rows)] <- ""
    return(which(batteries_for_rows == index_by))
  }
  seq_len(nrow(tbl))
}


# ---- Pure data: build the display data frame for the current selections ----

#' @noRd
.network_drivers_impacts_data <- function(metadata,
                                          weight, focus, metric_key,
                                          outcome_display, shift_type,
                                          index_by, sig_threshold = 0.10) {
  m <- metadata
  tbl <- if (identical(weight, "Weighted") && !is.null(m$tbl_w)) m$tbl_w else m$tbl
  sgs <- m$sgs

  visible <- .network_drivers_impacts_visible_idx(m, tbl, index_by)
  if (length(visible) == 0) return(NULL)

  # Leading text columns
  out_list <- list()
  out_list[[m$id_col_label]] <- as.character(tbl[[m$id_col_name]])[visible]
  if (m$has_community) out_list[["Community"]] <- as.character(tbl$Community)[visible]
  if (m$has_label)     out_list[["Label"]]     <- as.character(tbl$Label)[visible]

  # P-values for blackout. Bootstrap mode (any `<col>_p_value` exists) →
  # use the per-metric p-value of the column we're displaying. Otherwise
  # fall back to the row-level `p_val` (static chi-squared MI p-value).
  boot_applied <- any(grepl("_p_value$", names(tbl)))
  static_pval_col <- intersect(c("p_val", "p_value"), names(tbl))[1]
  static_pvals_visible <- if (!is.na(static_pval_col)) {
    suppressWarnings(as.numeric(tbl[[static_pval_col]][visible]))
  } else rep(NA_real_, length(visible))

  # Per-subgroup: index = round(|raw| / mean(|raw|) * 100) over visible
  # rows; NA for blacked-out cells. Also compute Total Impact and Base for
  # the footer row.
  total_impact_per_sg <- character(length(sgs))
  base_per_sg        <- character(length(sgs))
  names(total_impact_per_sg) <- sgs
  names(base_per_sg)         <- sgs

  for (i in seq_along(sgs)) {
    sg <- sgs[[i]]
    col <- .network_drivers_impacts_metric_col(
      sg, metric_key, focus, outcome_display, shift_type
    )
    if (!col %in% names(tbl)) {
      out_list[[sg]] <- rep(NA_real_, length(visible))
      total_impact_per_sg[[sg]] <- ""
      base_per_sg[[sg]] <- ""
      next
    }
    raw_visible <- suppressWarnings(as.numeric(tbl[[col]][visible]))
    abs_visible <- abs(raw_visible)
    abs_visible[is.na(abs_visible)] <- 0
    mean_abs <- if (length(abs_visible) > 0) mean(abs_visible) else 0

    # Resolve p-values: per-metric column if bootstrap mode, else static.
    pcol <- paste0(col, "_p_value")
    pvals <- if (boot_applied && pcol %in% names(tbl)) {
      suppressWarnings(as.numeric(tbl[[pcol]][visible]))
    } else {
      static_pvals_visible
    }
    insig <- !is.na(pvals) & pvals > sig_threshold

    idx <- if (mean_abs == 0) {
      rep(NA_real_, length(visible))
    } else {
      round(abs_visible / mean_abs * 100)
    }
    idx[insig] <- NA_real_
    out_list[[sg]] <- idx

    # Total Impact = mean of |raw| across visible rows, formatted by
    # outcome_display (% Change vs Point Change). Suppress for non-lift
    # metrics (maxVmin, mi).
    if (metric_key %in% c("maxVmin", "mi") || mean_abs == 0) {
      total_impact_per_sg[[sg]] <- ""
    } else {
      ti <- mean_abs  # mean of |raw|
      total_impact_per_sg[[sg]] <- if (identical(outcome_display, "absdisplay")) {
        sprintf("%.2f", ti)
      } else {
        sprintf("%.1f%%", ti * 100)
      }
    }

    # Base = sample size for this subgroup. Look for `<sg>_base` or
    # `<sg>_base_<focus>` (brand-specific) on the first row. Round to int.
    base_key <- if (!is.null(focus) && nzchar(focus) && focus != "Market") {
      paste0(sg, "_base_", focus)
    } else paste0(sg, "_base")
    base_val <- if (base_key %in% names(tbl)) {
      tbl[[base_key]][1]
    } else if (paste0(sg, "_base") %in% names(tbl)) {
      tbl[[paste0(sg, "_base")]][1]
    } else NA_real_
    base_per_sg[[sg]] <- if (is.na(base_val)) "" else as.character(round(as.numeric(base_val)))
  }
  out <- as.data.frame(out_list, stringsAsFactors = FALSE,
                       check.names = FALSE)
  attr(out, "total_impact") <- total_impact_per_sg
  attr(out, "base")         <- base_per_sg

  # Attach raw sign matrix as attribute so DT renderer can mark negatives
  # bold/italic without affecting the displayed numeric.
  signs <- vapply(sgs, function(sg) {
    col <- .network_drivers_impacts_metric_col(
      sg, metric_key, focus, outcome_display, shift_type
    )
    if (!col %in% names(tbl)) return(rep(0L, length(visible)))
    rv <- suppressWarnings(as.numeric(tbl[[col]][visible]))
    out_signs <- ifelse(is.na(rv), 0L, ifelse(rv < 0, -1L, 1L))
    as.integer(out_signs)
  }, integer(length(visible)))
  if (length(visible) == 1) signs <- matrix(signs, nrow = 1)
  attr(out, "raw_signs") <- signs
  attr(out, "subgroup_cols") <- sgs
  out
}


# ---- UI: sidebar inputs for impact controls --------------------------------

#' @noRd
.network_drivers_impacts_sidebar <- function(ns, prefix, metadata,
                                             view_value, view_input_id,
                                             default_outcome = "propdisplay",
                                             default_shift = "propshift",
                                             include_indexby = TRUE) {
  m <- metadata
  if (is.null(m)) return(NULL)

  # `view_value` can be a single string OR a vector of strings (when the
  # same shared controls should appear on multiple tabs, e.g. both
  # impacts_attr and impacts_comm).
  view_js <- sprintf("input['%s']", view_input_id)
  cond <- paste(sprintf("%s === '%s'", view_js, view_value), collapse = " || ")

  # Build choice lists
  metric_choices <- stats::setNames(
    vapply(m$metric_info, function(x) x$key,   character(1)),
    vapply(m$metric_info, function(x) x$label, character(1))
  )
  if (length(metric_choices) == 0) {
    metric_choices <- c("Average Effect" = "lift_0")
  }
  default_metric <- if (!is.null(m$preset_map[["Current Impact"]])) {
    m$preset_map[["Current Impact"]]$metric
  } else {
    unname(metric_choices[1])
  }

  focus_choices <- m$focus_options
  weight_choices <- if (m$has_weights) c("Unweighted", "Weighted") else "Unweighted"

  index_choices <- c("All")
  if (m$has_battery) {
    index_choices <- c(index_choices, names(m$batteries))
    if (!is.null(m$battery_group_ivs)) {
      index_choices <- c(index_choices, names(m$battery_group_ivs))
    }
  }

  # Assess preset — high-level dropdown that drives Analysis (metric) +
  # Shift Type to one of the curated combos. Adds "Custom" so users can
  # tune the underlying controls directly.
  inputs <- list()
  metric_shift_cond <- NULL
  if (length(m$preset_map) > 0) {
    assess_choices <- c(names(m$preset_map), "Custom")
    inputs <- c(inputs, list(
      shiny::selectInput(ns(paste0(prefix, "_assess")), "Assess:",
                         choices = assess_choices,
                         selected = assess_choices[1])
    ))
    metric_shift_cond <- sprintf("input['%s'] === 'Custom'",
                                 ns(paste0(prefix, "_assess")))
  }
  metric_input <- shiny::selectInput(
    ns(paste0(prefix, "_metric")), "Analysis:",
    choices = metric_choices, selected = default_metric
  )
  if (!is.null(metric_shift_cond)) {
    inputs <- c(inputs, list(
      shiny::conditionalPanel(condition = metric_shift_cond, metric_input)
    ))
  } else {
    inputs <- c(inputs, list(metric_input))
  }
  if (length(focus_choices) > 1) {
    inputs <- c(inputs, list(
      shiny::selectInput(ns(paste0(prefix, "_focus")),   "Focus:",
                         choices = focus_choices, selected = focus_choices[1])
    ))
  }
  if (m$has_outcome_display) {
    inputs <- c(inputs, list(
      shiny::selectInput(ns(paste0(prefix, "_display")), "Outcome:",
                         choices = c("% Change" = "propdisplay",
                                     "Point Change" = "absdisplay"),
                         selected = default_outcome)
    ))
  }
  if (m$has_shift_type) {
    shift_input <- shiny::selectInput(
      ns(paste0(prefix, "_shift")), "Shift Type:",
      choices = c("% of Current Mean" = "propshift",
                  "Fixed Step"        = "absshift",
                  "% Toward Top"      = "headshift",
                  "% of Range"        = "rangeshift"),
      selected = default_shift
    )
    if (!is.null(metric_shift_cond)) {
      inputs <- c(inputs, list(
        shiny::conditionalPanel(condition = metric_shift_cond, shift_input)
      ))
    } else {
      inputs <- c(inputs, list(shift_input))
    }
  }
  if (m$has_weights) {
    inputs <- c(inputs, list(
      shiny::selectInput(ns(paste0(prefix, "_weight")),  "Weight:",
                         choices = weight_choices, selected = "Unweighted")
    ))
  }
  if (include_indexby && m$has_battery && length(index_choices) > 1) {
    inputs <- c(inputs, list(
      shiny::selectInput(ns(paste0(prefix, "_indexby")), "Index By:",
                         choices = index_choices, selected = "All")
    ))
  }

  shiny::conditionalPanel(
    condition = cond,
    do.call(shiny::tagList, inputs)
  )
}


# ---- UI: standalone Index By dropdown (attribute-only) ---------------------

#' @noRd
.network_drivers_impacts_indexby_input <- function(ns, prefix, metadata,
                                                   view_value, view_input_id) {
  m <- metadata
  if (is.null(m) || !isTRUE(m$has_battery)) return(NULL)
  index_choices <- c("All", names(m$batteries))
  if (!is.null(m$battery_group_ivs)) {
    index_choices <- c(index_choices, names(m$battery_group_ivs))
  }
  if (length(index_choices) <= 1) return(NULL)
  view_js <- sprintf("input['%s']", view_input_id)
  shiny::conditionalPanel(
    condition = sprintf("%s === '%s'", view_js, view_value),
    shiny::selectInput(ns(paste0(prefix, "_indexby")), "Index By:",
                       choices = index_choices, selected = "All")
  )
}


# ---- UI: DT renderer with color coding -------------------------------------

#' @noRd
.network_drivers_impacts_dt <- function(display, metadata,
                                        marginal_threshold = 0.20,
                                        sig_threshold = 0.05) {
  if (is.null(display) || nrow(display) == 0) {
    return(DT::datatable(
      data.frame(Message = "No impact results."),
      options = list(dom = "t", paging = FALSE),
      rownames = FALSE, selection = "none"
    ))
  }
  sg_cols <- attr(display, "subgroup_cols") %||% setdiff(
    names(display), c(metadata$id_col_label, "Community", "Label")
  )
  total_impact <- attr(display, "total_impact") %||%
    stats::setNames(rep("", length(sg_cols)), sg_cols)
  base_vals    <- attr(display, "base") %||%
    stats::setNames(rep("", length(sg_cols)), sg_cols)

  display_clean <- display
  attr(display_clean, "raw_signs") <- NULL
  attr(display_clean, "subgroup_cols") <- NULL
  attr(display_clean, "total_impact") <- NULL
  attr(display_clean, "base") <- NULL

  # Build a custom container with <thead> AND a <tfoot> populated with
  # Total Impact + Base rows. The first cell of each footer row spans the
  # leading text columns ("Variable" / optional "Community" / "Label").
  # Header text gets underscores replaced with spaces for prettier display
  # ("Gen_Z" → "Gen Z"); the underlying data frame column names stay
  # untouched so `formatStyle(col)` etc. still target them correctly.
  col_names    <- names(display_clean)
  display_cols <- gsub("_", " ", col_names, fixed = TRUE)
  n_leading    <- length(col_names) - length(sg_cols)
  if (n_leading < 1) n_leading <- 1
  th <- htmltools::tags$th
  tr <- htmltools::tags$tr
  # Footer cells tagged with class + data-sg so a custom-message handler
  # can update them in place after replaceData (matching bn_report's
  # behavior of recomputing Total Impact / Base on every input change).
  container <- htmltools::withTags(
    table(
      class = "display",
      thead(do.call(tr, lapply(display_cols, function(c) th(c)))),
      tfoot(
        do.call(tr, c(
          list(th("Total Impact",
                  colspan = n_leading,
                  style = "text-align:left; font-weight:600;")),
          lapply(sg_cols, function(sg) {
            th(total_impact[[sg]] %||% "",
               class = "ti-cell",
               `data-sg` = sg,
               style = "text-align:center; font-weight:600;")
          })
        )),
        do.call(tr, c(
          list(th("Base",
                  colspan = n_leading,
                  style = "text-align:left; font-weight:600;")),
          lapply(sg_cols, function(sg) {
            th(base_vals[[sg]] %||% "",
               class = "base-cell",
               `data-sg` = sg,
               style = "text-align:center; font-weight:600;")
          })
        ))
      )
    )
  )

  sg_targets <- which(names(display_clean) %in% sg_cols) - 1L
  first_sg_target <- if (length(sg_targets) > 0) sg_targets[1] else NULL

  # Per-column 3-color gradient applied via drawCallback so it re-runs
  # on every draw, including after replaceData (when control inputs
  # change). Each subgroup column computes its own min/mid/max from the
  # currently visible cells and paints backgrounds via interpolation
  # between #F8696B (red), #FFEB84 (yellow), #63BE7B (green) — Excel's
  # default 3-color-scale palette.
  sg_targets_js <- paste0("[", paste(sg_targets, collapse = ","), "]")
  draw_cb <- DT::JS(
    "function(settings) {",
    "  var api = this.api();",
    "  var sgCols = ", sg_targets_js, ";",
    "  function hex2rgb(h) {",
    "    return [parseInt(h.slice(1,3),16), parseInt(h.slice(3,5),16), parseInt(h.slice(5,7),16)];",
    "  }",
    "  function interp(a, b, t) {",
    "    return 'rgb(' + Math.round(a[0]+(b[0]-a[0])*t) + ',' +",
    "                   Math.round(a[1]+(b[1]-a[1])*t) + ',' +",
    "                   Math.round(a[2]+(b[2]-a[2])*t) + ')';",
    "  }",
    "  var red = hex2rgb('#F8696B'),",
    "      yel = hex2rgb('#FFEB84'),",
    "      grn = hex2rgb('#63BE7B');",
    "  sgCols.forEach(function(ci) {",
    "    var data = api.column(ci).data().toArray()",
    "      .map(function(v) { return parseFloat(v); })",
    "      .filter(function(v) { return !isNaN(v); });",
    "    if (data.length === 0) return;",
    "    var min = Math.min.apply(null, data),",
    "        max = Math.max.apply(null, data),",
    "        mid = (min + max) / 2;",
    "    api.cells(null, ci).every(function() {",
    "      var cell = this.node();",
    "      var v = parseFloat(this.data());",
    "      if (isNaN(v) || min === max) { cell.style.backgroundColor = ''; return; }",
    "      var color;",
    "      if (v <= mid) {",
    "        var t = (mid - min) === 0 ? 0 : (v - min) / (mid - min);",
    "        color = interp(red, yel, t);",
    "      } else {",
    "        var t = (max - mid) === 0 ? 1 : (v - mid) / (max - mid);",
    "        color = interp(yel, grn, t);",
    "      }",
    "      cell.style.backgroundColor = color;",
    "    });",
    "  });",
    "}"
  )
  options_list <- list(
    dom = "t",
    paging = FALSE,
    pageLength = nrow(display_clean),
    scrollX = TRUE,
    scrollY = "100%",
    scrollCollapse = FALSE,
    autoWidth = TRUE,
    drawCallback = draw_cb,
    columnDefs = list(
      list(className = "dt-center", targets = sg_targets),
      list(width = "90px",          targets = sg_targets)
    )
  )
  if (!is.null(first_sg_target)) {
    # Sort the first subgroup column descending on initial load.
    options_list$order <- list(list(first_sg_target, "desc"))
  }

  DT::datatable(
    display_clean,
    container = container,
    rownames = FALSE,
    selection = "none",
    class = "row-border hover",
    options = options_list
  )
}


