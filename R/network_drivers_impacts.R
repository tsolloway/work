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
  # rows. Insufficient-base rows render BLANK (NA in idx). Insignificant
  # rows keep their actual numeric value but are flagged in `insig_mat`
  # so the renderer can style them black-on-white-text. Also compute
  # Total Impact and Base for the footer row.
  total_impact_per_sg <- character(length(sgs))
  base_per_sg        <- character(length(sgs))
  names(total_impact_per_sg) <- sgs
  names(base_per_sg)         <- sgs
  insig_mat <- matrix(FALSE, nrow = length(visible), ncol = length(sgs))
  colnames(insig_mat) <- sgs

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
    # NAs (insufficient-base rows) participate as 0 in the mean
    # denominator, but are preserved as NA in `abs_visible` so the
    # per-row idx propagates NA → blank cell rather than "0".
    mean_abs_input <- abs_visible
    mean_abs_input[is.na(mean_abs_input)] <- 0
    mean_abs <- if (length(mean_abs_input) > 0) mean(mean_abs_input) else 0

    # Resolve p-values: per-metric column if bootstrap mode, else static.
    pcol <- paste0(col, "_p_value")
    pvals <- if (boot_applied && pcol %in% names(tbl)) {
      suppressWarnings(as.numeric(tbl[[pcol]][visible]))
    } else {
      static_pvals_visible
    }
    insig <- !is.na(pvals) & pvals > sig_threshold
    insig_mat[, i] <- insig

    idx <- if (mean_abs == 0) {
      rep(NA_real_, length(visible))
    } else {
      round(abs_visible / mean_abs * 100)
    }
    # Don't NA out insig cells — keep their value; renderer reads
    # insig_mat to apply the black-cell-with-white-text style instead.
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
  attr(out, "insig_mask") <- insig_mat
  out
}


# ---- UI: sidebar inputs for impact controls --------------------------------

#' @noRd
.network_drivers_impacts_sidebar <- function(ns, prefix, metadata,
                                             view_value, view_input_id,
                                             default_outcome = NULL,
                                             default_shift = "absshift",
                                             include_indexby = TRUE) {
  m <- metadata
  if (is.null(m)) return(NULL)

  # Auto-default Outcome Display by DV type when caller didn't pass an
  # explicit value: dichotomous DVs read more naturally as raw probability
  # points (Point Change / absdisplay), everything else as % change.
  # Mirrors bn_report (.bn_report_render_attribute_impacts_dashboard,
  # bn_helpers.R ~ line 624) and bn_impact_write's auto-detect so all
  # three surfaces (app / report / Excel) land on the same initial
  # outcome display for any given DV.
  if (is.null(default_outcome)) {
    default_outcome <- if (isTRUE(m$is_dichotomous_dv)) "absdisplay" else "propdisplay"
  }
  # default_shift = "absshift" (Fixed Step) mirrors bn_report
  # (shift_type = "absolute" → absshift) and bn_impact_write's default.
  # In practice this only surfaces when the user flips Assess from
  # "Current Impact" (which forces rangeshift) to "Custom" — at that
  # point all three surfaces now land on Fixed Step instead of the
  # previous "% of Current Mean" mismatch.

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

  # Tooltip-on-label helper. Returns a <span> wrapped in a bslib tooltip
  # for use as a selectInput's `label` arg — hovering the label text
  # triggers the tooltip, hovering the dropdown does not.
  tt <- function(label_text, tip_text) {
    bslib::tooltip(
      htmltools::span(
        label_text,
        style = "cursor: help; border-bottom: 1px dotted var(--ndr-muted);"
      ),
      tip_text,
      placement = "right"
    )
  }
  # Uniform-spacing wrapper. Class `netdrv-ctrl-wrap` is paired with a
  # CSS rule (in .network_drivers_module_css) that zeroes the inner
  # .form-group's bottom margin so this wrapper's margin alone controls
  # the gap between controls. Wrap goes INSIDE conditionalPanel for
  # toggleable controls — when the panel is hidden, the wrapper is too,
  # so no empty 12px gap is left behind.
  cw <- function(...) {
    shiny::div(class = "netdrv-ctrl-wrap", ...)
  }

  # Assess preset — high-level dropdown that drives Analysis (metric) +
  # Shift Type to one of the curated combos. Adds "Custom Impact" so
  # users can tune the underlying controls directly.
  inputs <- list()
  metric_shift_cond <- NULL
  if (length(m$preset_map) > 0) {
    assess_choices <- c(names(m$preset_map), "Custom Impact")
    inputs <- c(inputs, list(
      cw(shiny::selectInput(
        ns(paste0(prefix, "_assess")),
        label = tt("Assess:",
                   "Preset driver analyses that address specific questions."),
        choices = assess_choices,
        selected = assess_choices[1]
      ))
    ))
    metric_shift_cond <- sprintf("input['%s'] === 'Custom Impact'",
                                 ns(paste0(prefix, "_assess")))
  }
  metric_input <- shiny::selectInput(
    ns(paste0(prefix, "_metric")),
    label = tt("Analysis:",
               "The metric used to score impact."),
    choices = metric_choices, selected = default_metric
  )
  if (!is.null(metric_shift_cond)) {
    inputs <- c(inputs, list(
      shiny::conditionalPanel(condition = metric_shift_cond, cw(metric_input))
    ))
  } else {
    inputs <- c(inputs, list(cw(metric_input)))
  }
  if (length(focus_choices) > 1) {
    inputs <- c(inputs, list(
      cw(shiny::selectInput(
        ns(paste0(prefix, "_focus")),
        label = tt("Focus:",
                   "‘Market’ analyzes overall performance, while a brand uses only that brand’s."),
        choices = focus_choices, selected = focus_choices[1]
      ))
    ))
  }
  if (m$has_outcome_display) {
    inputs <- c(inputs, list(
      cw(shiny::selectInput(
        ns(paste0(prefix, "_display")),
        label = tt("Outcome:",
                   "How outcome change is displayed — relative vs absolute point change."),
        choices = c("% Change" = "propdisplay",
                    "Point Change" = "absdisplay"),
        selected = default_outcome
      ))
    ))
  }
  if (m$has_shift_type) {
    shift_input <- shiny::selectInput(
      ns(paste0(prefix, "_shift")),
      label = tt("Shift Type:",
                 "How each attribute’s movement is calculated when computing impact."),
      choices = c("% of Current Mean" = "propshift",
                  "Fixed Step"        = "absshift",
                  "% Toward Top"      = "headshift",
                  "% of Range"        = "rangeshift"),
      selected = default_shift
    )
    if (!is.null(metric_shift_cond)) {
      inputs <- c(inputs, list(
        shiny::conditionalPanel(condition = metric_shift_cond, cw(shift_input))
      ))
    } else {
      inputs <- c(inputs, list(cw(shift_input)))
    }
  }
  if (m$has_weights) {
    inputs <- c(inputs, list(
      cw(shiny::selectInput(
        ns(paste0(prefix, "_weight")),
        label = tt("Weight:",
                   "Whether weights are applied when calculating impacts."),
        choices = weight_choices, selected = "Unweighted"
      ))
    ))
  }
  if (include_indexby && m$has_battery && length(index_choices) > 1) {
    inputs <- c(inputs, list(
      cw(shiny::selectInput(
        ns(paste0(prefix, "_indexby")),
        label = tt("Index By:",
                   "Filter rows to a battery or group; the index is re-normalised within the visible rows."),
        choices = index_choices, selected = "All"
      ))
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
  indexby_label <- bslib::tooltip(
    htmltools::span(
      "Index By:",
      style = "cursor: help; border-bottom: 1px dotted var(--ndr-muted);"
    ),
    "Filter rows to a battery or group; the index is re-normalised within the visible rows.",
    placement = "right"
  )
  shiny::conditionalPanel(
    condition = sprintf("%s === '%s'", view_js, view_value),
    shiny::div(
      class = "netdrv-ctrl-wrap",
      shiny::selectInput(ns(paste0(prefix, "_indexby")),
                         label = indexby_label,
                         choices = index_choices, selected = "All")
    )
  )
}


# ---- Pure text: control-feedback (assess question + dim warnings) ----------

#' @noRd
.network_drivers_impacts_min_base_for_focus <- function(metadata, focus) {
  tbl <- metadata$tbl
  if (is.null(tbl) || nrow(tbl) == 0) return(NA_real_)
  base_key <- paste0("base_", focus)
  bases <- vapply(metadata$sgs, function(sg) {
    col <- paste0(sg, "_", base_key)
    if (col %in% names(tbl)) suppressWarnings(as.numeric(tbl[[col]][1])) else NA_real_
  }, numeric(1))
  bases <- bases[!is.na(bases)]
  if (length(bases) == 0) return(NA_real_)
  min(bases)
}

#' @noRd
.network_drivers_impacts_assess_question <- function(metadata, assess_value) {
  if (is.null(assess_value) || identical(assess_value, "Custom Impact")) return("")
  pm <- metadata$preset_map
  if (is.null(pm) || is.null(pm[[assess_value]])) return("")
  pm[[assess_value]]$question %||% ""
}

#' @noRd
.network_drivers_impacts_warning <- function(dim, metric_key, shift_type,
                                             focus, metadata,
                                             outcome_display = "propdisplay") {
  if (is.null(metric_key) || !nzchar(metric_key)) return("")
  is_lift <- !metric_key %in% c("maxVmin", "mi")
  # Focus/weight are inert ONLY when the shift is fixed-step (absshift =
  # Fixed Step, rangeshift = % of Range — both add a constant increment
  # independent of the distribution, so the POINT-CHANGE numerator is
  # focus/weight-invariant), the metric is a lift, AND Outcome = Point
  # Change. Under % Change the figure is lift_abs / observed_expected and
  # the baseline is focus/weight-specific, so focus/weight always matter.
  shift_abs_lift <- shift_type %in% c("absshift", "rangeshift") &&
    is_lift && identical(outcome_display, "absdisplay")

  if (identical(dim, "focus")) {
    # (a) base below minimum — only meaningful for non-Market focus + lift
    if (!is.null(focus) && nzchar(focus) && focus != "Market" && is_lift) {
      base_min <- .network_drivers_impacts_min_base_for_focus(metadata, focus)
      min_req <- as.integer(metadata$min_base_for_lift %||% 75L)
      if (!is.na(base_min) && base_min < min_req) {
        return(sprintf("Results not calculated because base is below %d", min_req))
      }
    }
    # (b) focus has no effect under a fixed-step shift (Fixed Step / % of Range)
    if (shift_abs_lift) {
      return("Focus does not affect this metric when shift is a fixed step or % of range")
    }
    return("")
  }
  if (identical(dim, "weight")) {
    if (metric_key %in% c("maxVmin", "mi")) {
      return("Weights don’t affect this metric")
    }
    if (shift_abs_lift) {
      return("Weights don’t affect this metric when shift is a fixed step or % of range")
    }
    return("")
  }
  if (identical(dim, "display")) {
    if (identical(metric_key, "mi")) {
      return("Outcome display doesn’t affect this metric")
    }
    return("")
  }
  if (identical(dim, "shift")) {
    if (metric_key %in% c("maxVmin", "mi")) {
      return("Shift type doesn’t affect this metric")
    }
    return("")
  }
  ""
}


# ---- Pure text: index-note describing the current metric/shift -------------

#' @noRd
.network_drivers_impacts_shift_meaning <- function(pct, shift_key) {
  k <- shift_key %||% "propshift"
  n <- suppressWarnings(as.numeric(pct))
  step <- if (is.finite(n)) sprintf("%.2f", n / 100) else "0.10"
  switch(k,
    propshift  = sprintf("Each attribute’s mean is shifted by %s%% of its current value.", pct),
    absshift   = sprintf("Each attribute’s mean is shifted by %s scale points (a fixed step).", step),
    headshift  = sprintf("Each attribute closes %s%% of its gap to the top of its scale.", pct),
    rangeshift = sprintf("Each attribute’s mean is shifted by %s%% of its scale’s range.", pct),
    ""
  )
}

#' @noRd
.network_drivers_impacts_metric_description <- function(metric_key, shift_key) {
  if (is.null(metric_key) || !nzchar(metric_key)) return("")
  if (metric_key %in% c("lift", "lift_0")) {
    return("Indexed by average effect. Measures the outcome’s sensitivity to a small symmetric perturbation around each attribute’s current state.")
  }
  if (startsWith(metric_key, "lift_")) {
    pct <- sub("^lift_", "", metric_key)
    head <- sprintf("Indexed by %s%% improvement. Measures how much the outcome changes when each attribute’s distribution shifts by %s%%. ",
                   pct, pct)
    return(paste0(head, .network_drivers_impacts_shift_meaning(pct, shift_key)))
  }
  if (identical(metric_key, "maxVmin")) {
    return("Indexed by best-vs-worst effect. Measures the outcome difference between the top of each attribute versus the bottom.")
  }
  if (identical(metric_key, "mi")) {
    return("Indexed by explanatory value. Measures the statistical strength of the relationship between each attribute and the outcome (mutual information), independent of intervention direction or shift type.")
  }
  paste0("Indexed by ", metric_key)
}

#' @noRd
.network_drivers_impacts_footer_notes <- function(metadata, metric_key,
                                                  shift_type,
                                                  sig_threshold = 0.10) {
  index_note <- .network_drivers_impacts_metric_description(metric_key, shift_type)
  min_base <- metadata$min_base_for_lift %||% 75L
  htmltools::tagList(
    htmltools::div(
      # Visual styling (padding, font-size, color) comes from
      # `.impact-footer, .priort-footer` rule in resondex_css()'s
      # `table_footer_notes` block — single source of truth shared
      # with the prio footer + bn_report's HTML.
      class = "impact-footer",
      htmltools::p(
        index_note,
        style = "margin: 0 0 4px 0; font-style: italic;"
      ),
      htmltools::p(
        style = "margin: 0; color: var(--ndr-muted);",
        sprintf(
          "Bold italicized index means a negative relationship. Black cells mean an insignificant relationship (p > %s). Blank cells are not calculated due to insufficient base (below %d).",
          format(sig_threshold, nsmall = 2),
          as.integer(min_base)
        )
      )
    )
  )
}


# ---- UI: reactable renderer with per-column color gradient -----------------

#' @noRd
.network_drivers_impacts_reactable <- function(display, metadata,
                                               marginal_threshold = 0.20,
                                               sig_threshold = 0.05) {
  if (is.null(display) || nrow(display) == 0) {
    return(reactable::reactable(
      data.frame(Message = "No impact results."),
      pagination = FALSE, sortable = FALSE, compact = TRUE
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

  # Build a per-column R style function that closes over the column's
  # min/max, per-row sign, and per-row insignificance flag. Reactable R
  # style functions receive (value, index) — index is the 1-based row
  # position in the underlying data, which we use to look up signs and
  # insig flags. Three rendering modes per cell:
  #   1. Insignificant (p > 0.10) → black bg, white text (overrides all)
  #   2. NA value (insufficient base) → no styling, blank cell
  #   3. Normal value → red→yellow→green gradient + bold/italic if negative
  raw_signs_mat <- attr(display, "raw_signs")
  insig_mask    <- attr(display, "insig_mask")
  make_color_style <- function(col_values, col_signs, col_insig) {
    # Exclude insig cells from the gradient min/max so they don't
    # distort the scale (their displayed value is hidden anyway).
    if (!is.null(col_insig) && length(col_insig) == length(col_values)) {
      col_values_g <- ifelse(col_insig, NA_real_, col_values)
    } else {
      col_values_g <- col_values
    }
    finite <- col_values_g[is.finite(col_values_g)]
    has_gradient <- length(finite) > 0 && min(finite) != max(finite)
    vmin <- if (has_gradient) min(finite) else NA_real_
    vmax <- if (has_gradient) max(finite) else NA_real_
    function(value, index) {
      style <- list(textAlign = "center")
      # 1. Insignificant cells: black bg + white text. Overrides everything.
      is_insig <- !is.null(col_insig) &&
                  index >= 1 && index <= length(col_insig) &&
                  isTRUE(col_insig[[index]])
      if (is_insig) {
        # Match shared .rdx-insig (resondex_css): deliberate blackout,
        # mode-independent — #111/#fff is the brand's "suppressed" signal.
        style$background <- "#111"
        style$color      <- "#fff"
        return(style)
      }
      # 2. Normal value: diverging brand scale (matches showcase Index).
      # color-mix(var(--ndr-success/danger) X%, transparent) — strength
      # scales with normalized distance from the midpoint, capped at 45%.
      # Resolves through brand tokens so it adapts to light/dark.
      if (has_gradient && !is.na(value)) {
        normalized <- (value - vmin) / (vmax - vmin)
        normalized <- max(0, min(1, normalized))
        d <- normalized - 0.5
        pct <- as.integer(round(min(0.45, abs(d) * 2 * 0.45) * 100))
        if (pct > 0) {
          tok <- if (d >= 0) "--ndr-success" else "--ndr-danger"
          style$background <- sprintf(
            "color-mix(in srgb, var(%s) %d%%, transparent)", tok, pct
          )
        }
      }
      # 3. Negative-sign overlay (italic/bold) — applied on top of gradient
      sign_val <- if (!is.null(col_signs) &&
                      index >= 1 && index <= length(col_signs)) {
        col_signs[[index]]
      } else 0L
      if (isTRUE(sign_val < 0)) {
        style$fontWeight <- "bold"
        style$fontStyle  <- "italic"
      }
      if (length(style) == 1) return(NULL)  # only textAlign, nothing to apply
      style
    }
  }

  # Two-row footer per cell: Total Impact value on top, Base on bottom.
  # Stacked via flex column. Reactable doesn't natively support multiple
  # footer rows so we put both into a single footer cell.
  make_two_row_footer <- function(top, bottom) {
    htmltools::div(
      style = "display: flex; flex-direction: column; line-height: 1.4;",
      htmltools::div(top,    style = "font-weight: 600;"),
      htmltools::div(bottom, style = "font-weight: 500; color: var(--ndr-muted);")
    )
  }

  # Build colDefs — leading text cols (auto-content width via minWidth, no
  # max so they grow to fit content), then subgroup cols at fixed 90px.
  col_defs <- list()
  id_col <- metadata$id_col_label
  if (id_col %in% names(display_clean)) {
    col_defs[[id_col]] <- reactable::colDef(
      name = id_col,
      minWidth = 80,
      footer = make_two_row_footer("Total Impact", "Base")
    )
  }
  if (metadata$has_community && "Community" %in% names(display_clean)) {
    col_defs[["Community"]] <- reactable::colDef(
      name = "Community",
      minWidth = 120,
      footer = make_two_row_footer("", "")
    )
  }
  if (metadata$has_label && "Label" %in% names(display_clean)) {
    col_defs[["Label"]] <- reactable::colDef(
      name = "Label",
      minWidth = 120,
      footer = make_two_row_footer("", "")
    )
  }
  for (i in seq_along(sg_cols)) {
    col <- sg_cols[[i]]
    pretty_label <- gsub("_", " ", col, fixed = TRUE)
    col_signs <- if (!is.null(raw_signs_mat) && i <= ncol(raw_signs_mat)) {
      raw_signs_mat[, i]
    } else NULL
    col_insig <- if (!is.null(insig_mask) && i <= ncol(insig_mask)) {
      insig_mask[, i]
    } else NULL
    col_defs[[col]] <- reactable::colDef(
      name      = pretty_label,
      align     = "center",
      minWidth  = 70,
      style     = make_color_style(display_clean[[col]], col_signs, col_insig),
      footer    = make_two_row_footer(
        total_impact[[col]] %||% "",
        base_vals[[col]]    %||% ""
      ),
      footerStyle = list(textAlign = "center")
    )
  }

  default_sorted <- if (length(sg_cols) > 0) {
    stats::setNames(list("desc"), sg_cols[1])
  } else NULL

  reactable::reactable(
    display_clean,
    columns         = col_defs,
    defaultSorted   = default_sorted,
    pagination      = FALSE,
    sortable        = TRUE,
    resizable       = TRUE,
    bordered        = FALSE,
    highlight       = TRUE,
    compact         = TRUE,
    # Reactable renders at content height. The card_body's wrapper
    # div uses `flex: 0 1 auto` + `overflow-y: auto` so short tables
    # let the footer hug the last row, and tall tables hit the
    # available space and scroll inside the wrapper. Same pattern
    # as the prio table.
    # Body theme is the canonical reference (bn_report HTML tables converge
    # on this). Header + footer mirror bn_report's .impact-table thead th /
    # .impact-footer so reactable renders identically to the report.
    # Brand reactable theme (resondex_brand.R). Single source of truth —
    # every reactable across the package uses this so chrome matches
    # bn_report's HTML tables.
    theme           = resondex_reactable_theme()
  )
}




