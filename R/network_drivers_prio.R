# =============================================================================
# Native DT prioritization dashboard for app_deliverable_network_drivers
#
# Mirrors the impacts pattern: Shiny inputs in the sidebar (only for the
# dimensions that vary in the registry) + a reactive DT::datatable in the
# main area. Shares dimension parsing with bn_report via .bn_report_prio_metadata().
# =============================================================================

# ---- Path A dark-mode pairing -----------------------------------------------
#
# Map a user-selected theme to its dark counterpart when the host app is in
# dark mode. Returns the input theme unchanged for themes that don't have a
# paired dark variant (Economist, Monokai, already-dark themes, etc.). The
# render reactive (in app_deliverable_network_drivers.R) calls this with the
# current dark_mode() reactive so the prio chart re-renders with the right
# surface when the user toggles light/dark — zero flicker, no client-side
# relayout race.

#' @noRd
.prio_dark_pair <- function(theme) {
  switch(
    theme,
    "Default"      = "Default Dark",
    "Flat"         = "Flat Dark",
    "plotly"       = "plotly_dark",
    "plotly_white" = "plotly_dark",
    theme
  )
}


# ---- Pure data: build the display data frame for the current selections ----

#' @noRd
.network_drivers_prio_data <- function(metadata,
                                       strategy = NULL, search = NULL,
                                       subgroup = NULL, focus = NULL,
                                       weight = NULL,
                                       outcome = NULL) {
  pm <- metadata
  if (is.null(pm)) return(NULL)

  # Default each dim to its first available value.
  pick <- function(value, dim_name) {
    if (!is.null(value) && nzchar(value)) return(value)
    opts <- pm$dims[[dim_name]]
    if (length(opts) > 0) opts[[1]] else ""
  }
  s_strat <- pick(strategy, "strategy")
  s_srch  <- pick(search,   "search")
  s_subg  <- pick(subgroup, "subgroup")
  s_focus <- pick(focus,    "focus")
  s_wt    <- pick(weight,   "weight")
  key <- paste(s_strat, s_srch, s_subg, s_focus, s_wt, sep = "|")

  entry <- pm$lookup[[key]]
  if (is.null(entry) || length(entry$rows) == 0) return(NULL)
  rows <- entry$rows

  # Display-mode switch: "% Change" (propdisplay) shows ratio columns
  # (cumulative_gain_pct / marginal_gain_pct) under "Cumulative Gain %"
  # / "Incremental Lift %" headers. "Point Change" (absdisplay) shows
  # absolute columns (cumulative_gain / marginal_gain) under bare
  # "Cumulative Gain" / "Incremental Lift" headers. Mirrors the
  # bn_write Excel + bn_report HTML behaviour.
  outcome_disp <- outcome %||% "propdisplay"
  is_pct_mode  <- identical(outcome_disp, "propdisplay")
  cum_label  <- if (is_pct_mode) "Cumulative Gain %"  else "Cumulative Gain"
  incr_label <- if (is_pct_mode) "Incremental Lift %" else "Incremental Lift"

  # Convert list-of-lists to a tidy data frame.
  out_list <- list(
    Priority    = vapply(rows, function(r) r$priority %||% NA_integer_, integer(1)),
    Variable    = vapply(rows, function(r) r$variable %||% NA_character_, character(1))
  )
  if (pm$has_community) {
    out_list[["Community"]] <- vapply(rows, function(r) r$community %||% NA_character_, character(1))
  }
  if (pm$has_label) {
    out_list[["Label"]] <- vapply(rows, function(r) r$label %||% NA_character_, character(1))
  }
  out_list[["Outcome Estimate"]] <- vapply(rows, function(r) r$dv_estimate %||% NA_real_, numeric(1))
  if (is_pct_mode) {
    out_list[[cum_label]]  <- vapply(rows, function(r) r$cumulative_gain_pct %||% NA_real_, numeric(1))
    out_list[[incr_label]] <- vapply(rows, function(r) r$marginal_gain_pct   %||% NA_real_, numeric(1))
  } else {
    out_list[[cum_label]]  <- vapply(rows, function(r) r$cumulative_gain %||% NA_real_, numeric(1))
    out_list[[incr_label]] <- vapply(rows, function(r) r$marginal_gain   %||% NA_real_, numeric(1))
  }
  if (pm$has_p) {
    out_list[["p-value"]] <- vapply(rows, function(r) r$p_value %||% NA_real_, numeric(1))
  }
  out <- as.data.frame(out_list, stringsAsFactors = FALSE,
                       check.names = FALSE)
  attr(out, "n_obs")           <- entry$n_obs
  attr(out, "is_binary")       <- pm$is_binary
  attr(out, "outcome_display") <- outcome_disp
  attr(out, "cum_label")       <- cum_label
  attr(out, "incr_label")      <- incr_label
  out
}


# ---- UI: sidebar inputs for active dimensions ------------------------------

#' @noRd
.network_drivers_prio_sidebar <- function(ns, prefix, metadata,
                                          view_value, view_input_id) {
  pm <- metadata
  if (is.null(pm)) return(NULL)

  view_js <- sprintf("input['%s']", view_input_id)
  cond <- sprintf("%s === '%s'", view_js, view_value)

  dim_labels <- c(
    strategy = "Analysis:",
    subgroup = "Subgroup:",
    focus    = "Focus:",
    weight   = "Weight:"
  )
  dim_tips <- c(
    strategy = "The prioritization strategy used to rank attributes.",
    subgroup = "Which subgroup the prioritization was computed against.",
    focus    = "‘Market’ analyzes overall performance, while a brand uses only that brand’s.",
    weight   = "Whether weights are applied when calculating prioritization."
  )
  # `search` dim is always "greedy" out of bn_finalize_network → never
  # active. Drop it so it can't be surfaced even if someone hand-built a
  # mixed-search registry.
  active_dims <- setdiff(pm$active_dims, "search")

  # Tooltip-on-label helper — same pattern as impacts.
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
  # Uniform-spacing wrapper — same pattern as impacts.
  cw <- function(...) shiny::div(class = "netdrv-ctrl-wrap", ...)

  inputs <- list()
  for (dn in active_dims) {
    opts <- vapply(pm$dims[[dn]], function(x) as.character(x), character(1))
    # Pretty-print: replace underscores with spaces in the visible label
    pretty_labels <- gsub("_", " ", opts, fixed = TRUE)
    choices <- stats::setNames(opts, pretty_labels)
    inputs <- c(inputs, list(
      cw(shiny::selectInput(
        ns(paste0(prefix, "_", dn)),
        label = tt(dim_labels[[dn]] %||% paste0(dn, ":"),
                   dim_tips[[dn]]   %||% paste0("The ", dn, " dimension.")),
        choices = choices,
        selected = opts[1]
      ))
    ))
  }
  # Outcome control — display-only toggle between % and absolute formatting.
  # Only meaningful when the dependent variable is a proportion / binary
  # outcome (values bounded in [0, 1]); for continuous outcomes the
  # decimal display is the only sensible option, so the control is
  # omitted entirely.
  if (isTRUE(pm$is_binary)) {
    inputs <- c(inputs, list(
      cw(shiny::selectInput(
        ns(paste0(prefix, "_outcome")),
        label = tt("Outcome:",
                   "How outcome change is displayed — relative vs absolute point change."),
        choices = c("% Change" = "propdisplay",
                    "Point Change" = "absdisplay"),
        selected = "propdisplay"
      ))
    ))
  }
  # Show / hide the Outcome Estimate column. Defaults OFF to match
  # bn_write (hidden) and bn_report (omitted entirely). Formatted like
  # the other controls: tooltip-wrapped "Outcome Estimate:" label on
  # top, action button ("Show" / "Hide") on the row below. Server
  # tracks state in a reactiveVal and flips the button label.
  est_btn_id <- ns(paste0(prefix, "_show_estimate"))
  inputs <- c(inputs, list(
    cw(shiny::div(
      class = "form-group shiny-input-container",
      htmltools::tags$label(
        `for`  = est_btn_id,
        class  = "control-label",
        tt("Outcome Estimate:",
           "Predicted outcome value at each step.")
      ),
      shiny::div(
        shiny::actionButton(
          est_btn_id,
          label = "Show",
          class = "btn-sm",
          style = paste(
            "background-color: var(--ndr-card-bg);",
            "border: 1px solid var(--ndr-border);",
            "color: var(--ndr-text);"
          ),
          width = "100%"
        )
      )
    ))
  ))

  # Chart theme — drives the prioritization chart's plotly_theme().
  inputs <- c(inputs, list(
    cw(shiny::selectInput(
      ns(paste0(prefix, "_chart_theme")),
      label = tt("Chart Theme:",
                 "Visual theme applied to the prioritization chart."),
      choices = plotly_theme_names(),
      selected = "Default"
    ))
  ))
  if (length(inputs) == 0) return(NULL)
  shiny::conditionalPanel(
    condition = cond,
    do.call(shiny::tagList, inputs)
  )
}


# ---- UI: reactable renderer with p-value coloring --------------------------

#' @noRd
.network_drivers_prio_reactable <- function(display, metadata,
                                            sig_threshold = 0.05,
                                            marginal_threshold = 0.10,
                                            show_estimate = FALSE) {
  if (is.null(display) || nrow(display) == 0) {
    return(reactable::reactable(
      data.frame(Message = "No prioritization results."),
      pagination = FALSE, sortable = FALSE
    ))
  }
  is_binary    <- isTRUE(attr(display, "is_binary"))
  outcome_disp <- attr(display, "outcome_display") %||% "propdisplay"
  cum_label    <- attr(display, "cum_label")  %||% "Cumulative Gain"
  incr_label   <- attr(display, "incr_label") %||% "Incremental Lift"
  n_obs        <- attr(display, "n_obs")
  is_pct_mode  <- identical(outcome_disp, "propdisplay")

  # Drop Outcome Estimate column when toggled off. Preserves attrs so
  # downstream metadata (n_obs, etc.) is intact.
  if (!isTRUE(show_estimate) && "Outcome Estimate" %in% names(display)) {
    keep_attrs <- attributes(display)
    display <- display[, setdiff(names(display), "Outcome Estimate"), drop = FALSE]
    keep_attrs$names <- names(display)
    keep_attrs$row.names <- attr(display, "row.names")
    attributes(display) <- keep_attrs
  }

  numeric_cols <- intersect(
    c("Outcome Estimate", cum_label, incr_label, "p-value"),
    names(display)
  )
  # Percent formatting for the GAIN columns only (Cumulative, Incremental):
  #   - % Change mode (any DV): render as XX.X%
  #   - Point Change + binary DV: raw values bounded 0–1 → render as XX.X%
  #   - Point Change + continuous DV: raw values in original units → 0.000
  pct_cols <- if (is_pct_mode || is_binary) {
    intersect(c(cum_label, incr_label), names(display))
  } else character(0)
  # Outcome Estimate: depends ONLY on DV type, not on Outcome toggle.
  #   - Binary  → XX.X% (probability reads better as %)
  #   - Continuous → 0.00 (2 decimals in original units)
  est_is_pct <- is_binary

  display_clean <- display
  attr(display_clean, "n_obs") <- NULL
  attr(display_clean, "is_binary") <- NULL
  attr(display_clean, "outcome_display") <- NULL
  attr(display_clean, "cum_label") <- NULL
  attr(display_clean, "incr_label") <- NULL

  # Per-column sequential brand scale (matches showcase Index₂):
  # color-mix(var(--ndr-success) X%, transparent), strength 0..55% scaling
  # with the normalized value. Light/dark adaptive via the brand token.
  # NOTE: bn_write_prio Excel still uses #FFFFFF → #66BD7D — kept matched
  # to bn_report's whiteToGreen JS (also tokenized).
  make_color_style <- function(col_values) {
    finite <- col_values[is.finite(col_values)]
    if (length(finite) == 0) return(NULL)
    vmin <- min(finite); vmax <- max(finite)
    if (vmin == vmax) return(NULL)
    function(value) {
      if (is.na(value)) return(list(textAlign = "center"))
      normalized <- (value - vmin) / (vmax - vmin)
      normalized <- max(0, min(1, normalized))
      pct <- as.integer(round(normalized * 55))
      bgc <- if (pct == 0) "transparent" else sprintf(
        "color-mix(in srgb, var(--ndr-success) %d%%, transparent)", pct
      )
      list(background = bgc, textAlign = "center")
    }
  }

  # Build colDefs
  col_defs <- list()
  if ("Priority" %in% names(display_clean)) {
    col_defs[["Priority"]] <- reactable::colDef(align = "center", width = 80)
  }
  if ("Variable" %in% names(display_clean)) {
    col_defs[["Variable"]] <- reactable::colDef(name = "Variable")
  }
  if ("Community" %in% names(display_clean)) {
    col_defs[["Community"]] <- reactable::colDef(name = "Community")
  }
  if ("Label" %in% names(display_clean)) {
    col_defs[["Label"]] <- reactable::colDef(name = "Label")
  }
  for (col in setdiff(numeric_cols, "p-value")) {
    col_style <- make_color_style(display_clean[[col]])
    # Outcome Estimate: special-cased — depends on DV type only.
    if (identical(col, "Outcome Estimate")) {
      col_defs[[col]] <- reactable::colDef(
        name = col, align = "center",
        format = if (est_is_pct) {
          reactable::colFormat(percent = TRUE, digits = 1)
        } else {
          reactable::colFormat(digits = 2)
        },
        style = col_style
      )
    } else if (col %in% pct_cols) {
      col_defs[[col]] <- reactable::colDef(
        name = col, align = "center",
        format = reactable::colFormat(percent = TRUE, digits = 1),
        style = col_style
      )
    } else {
      col_defs[[col]] <- reactable::colDef(
        name = col, align = "center",
        format = reactable::colFormat(digits = 3),
        style = col_style
      )
    }
  }
  if ("p-value" %in% names(display_clean)) {
    sig <- sig_threshold; marg <- marginal_threshold
    # Brand semantic colours so reactable p-values match the shared
    # .rdx-pval-* treatment used everywhere else (resondex_brand single source).
    .sem <- resondex_brand()$semantic
    p_style <- function(value) {
      if (is.na(value)) return(NULL)
      color <- if (value < sig) .sem$success
               else if (value < marg) .sem$warning
               else .sem$danger
      list(color = color,
           fontWeight = if (value < sig) "bold" else "normal")
    }
    col_defs[["p-value"]] <- reactable::colDef(
      name = "p-value", align = "center",
      format = reactable::colFormat(digits = 3),
      style  = p_style
    )
  }

  reactable::reactable(
    display_clean,
    columns         = col_defs,
    pagination      = FALSE,
    sortable        = TRUE,
    resizable       = TRUE,
    bordered        = FALSE,
    highlight       = TRUE,
    # Reactable fills its flex parent (the .flex-1 wrapper inside
    # card_body). The footer takes its natural height beneath; no
    # need to reserve a fixed amount via maxHeight — the flex column
    # in card_body handles the split dynamically.
    height          = "100%",
    style           = list(height = "100%"),
    # Body theme matches the impact reactables (canonical reference);
    # header + footer mirror bn_report's .priort-table thead th /
    # .priort-footer so reactable renders identically to the report.
    theme           = reactable::reactableTheme(
      color           = "var(--ndr-text)",
      backgroundColor = "var(--ndr-card-bg)",
      borderColor     = "var(--ndr-border)",
      stripedColor    = "transparent",
      highlightColor  = "var(--ndr-secondary-bg)",
      cellPadding     = "8px 10px",
      style           = list(fontFamily = "inherit", fontSize = "14px",
                             color = "var(--ndr-text)"),
      headerStyle    = list(fontWeight   = "600",
                            color        = "var(--ndr-text)",
                            background   = "var(--ndr-card-bg)",
                            border       = "none",
                            borderBottom = "1px solid var(--ndr-border)"),
      footerStyle    = list(fontSize  = "12px",
                            color     = "var(--ndr-muted)",
                            background = "transparent",
                            borderTop = "1px solid var(--ndr-border)",
                            padding   = "8px 10px")
    )
  )
}


# ---- Pure HTML: prio footer notes (Base line + p-value legend) -------------

#' @noRd
.network_drivers_prio_footer_notes <- function(display, metadata = NULL,
                                               show_estimate = FALSE,
                                               current_strategy = NULL,
                                               sig_threshold = 0.05,
                                               marginal_threshold = 0.10) {
  if (is.null(display)) return(NULL)
  n_obs <- attr(display, "n_obs")
  has_p <- "p-value" %in% names(display)
  outcome_disp <- attr(display, "outcome_display") %||% "propdisplay"
  is_pct_mode  <- identical(outcome_disp, "propdisplay")
  base_text <- if (!is.null(n_obs) && !is.na(n_obs)) {
    sprintf("Base: %d", as.integer(n_obs))
  } else ""
  legend_text <- if (has_p) {
    sprintf(
      "Green p-values are significant (< %s); orange are marginal (< %s); red are insignificant.",
      format(sig_threshold, nsmall = 2),
      format(marginal_threshold, nsmall = 2)
    )
  } else ""

  # Glossary block. Each item shows only when its column is visible in
  # the table OR (for strategy items) when the user has the matching
  # Analysis selected. Plain (non-bold) term + em-dash + definition.
  pm <- metadata
  lift_label  <- pm$lift_label  %||% NULL
  max_label   <- pm$max_label   %||% "Maximum Lift"
  max_dep_lbl <- pm$max_deprecated_label %||% "Maximum Lift (Deprecated)"
  lift_explainer <- pm$lift_shift_explainer %||% ""
  cur_strat <- current_strategy %||% ""

  glossary_p <- function(term, defn) {
    htmltools::p(
      style = "margin: 0 0 4px 0;",
      term, " — ", defn
    )
  }
  glossary <- htmltools::tagList(
    glossary_p("Step", "Priority step number (order in which attributes were selected)."),
    if (isTRUE(show_estimate)) glossary_p(
      "Outcome Estimate",
      "Expected outcome value with all selected attributes shifted."
    ),
    if (!is_pct_mode) glossary_p(
      "Cumulative Gain",
      "Absolute increase in outcome estimate from baseline (all attributes through this step)."
    ),
    if (!is_pct_mode) glossary_p(
      "Incremental Lift",
      "Absolute increase in outcome estimate from adding this attribute."
    ),
    if (is_pct_mode) glossary_p(
      "Cumulative Gain %",
      "Percentage increase from baseline through this step."
    ),
    if (is_pct_mode) glossary_p(
      "Incremental Lift %",
      "Percentage increase relative to the previous step."
    ),
    if (has_p) glossary_p(
      "p-value",
      "Noise-floor test: proportion of bootstraps where this step’s gain ≤ the noise floor."
    ),
    if (!is.null(lift_label) && nzchar(lift_label) &&
        identical(cur_strat, lift_label)) glossary_p(
      lift_label, paste0(lift_explainer, ".")
    ),
    if (identical(cur_strat, max_label)) glossary_p(
      max_label,
      "Sets each attribute to its highest level as hard evidence, representing the theoretical ceiling."
    ),
    if (identical(cur_strat, max_dep_lbl)) glossary_p(
      max_dep_lbl,
      "Same as Maximum Lift but cumulative gain is the raw outcome estimate (no comparison to baseline). Provided for backward compatibility."
    )
  )

  if (!nzchar(base_text) && !nzchar(legend_text) && length(glossary) == 0) {
    return(NULL)
  }
  htmltools::tagList(
    htmltools::div(
      class = "priort-footer",
      style = paste(
        "margin-top: 12px;",
        "padding: 4px 10px 10px 10px;",
        "font-size: 12px;",
        "color: var(--ndr-muted);"
      ),
      if (nzchar(base_text)) htmltools::p(
        base_text,
        style = "margin: 0 0 4px 0; font-weight: 600;"
      ),
      if (nzchar(legend_text)) htmltools::p(
        legend_text,
        style = "margin: 0 0 8px 0; color: var(--ndr-muted);"
      ),
      htmltools::div(
        class = "priort-glossary",
        style = "color: var(--ndr-muted);",
        glossary
      )
    )
  )
}


# ---- Plotly waterfall: vertical stacked bars + cumulative line -------------

#' @noRd
.network_drivers_prio_chart <- function(display, metadata,
                                        theme = "Default") {
  if (is.null(display) || nrow(display) == 0) {
    return(plotly::plotly_empty())
  }
  is_binary    <- isTRUE(attr(display, "is_binary"))
  outcome_disp <- attr(display, "outcome_display") %||% "propdisplay"
  cum_label    <- attr(display, "cum_label")  %||% "Cumulative Gain"
  incr_label   <- attr(display, "incr_label") %||% "Incremental Lift"
  is_pct_mode  <- identical(outcome_disp, "propdisplay")

  if (!cum_label %in% names(display) || !incr_label %in% names(display)) {
    return(plotly::plotly_empty())
  }

  cumul <- as.numeric(display[[cum_label]])
  incr  <- as.numeric(display[[incr_label]])
  prev  <- cumul - incr
  vars  <- as.character(display$Variable)
  labels_col <- if ("Label" %in% names(display)) {
    as.character(display$Label)
  } else vars
  steps <- if ("Priority" %in% names(display)) {
    as.integer(display$Priority)
  } else seq_along(vars)

  # Y-axis values + label format. % mode and binary point-mode multiply
  # the underlying ratio/probability by 100 so the axis reads as XX%;
  # continuous point-mode plots in the original units.
  use_pct <- is_pct_mode || is_binary
  if (use_pct) {
    y_prev   <- prev  * 100
    y_incr   <- incr  * 100
    y_cumul  <- cumul * 100
    y_suffix <- "%"
    y_fmt    <- ".0f"   # whole percents on the y-axis (e.g. "12%")
  } else {
    y_prev   <- prev
    y_incr   <- incr
    y_cumul  <- cumul
    y_suffix <- ""
    y_fmt    <- ".2f"
  }

  # Preserve order — plotly otherwise alphabetises bar categories.
  x_factor <- factor(labels_col, levels = labels_col)

  # Theme-driven colors. Use the theme's first colorway entry as the
  # "incremental" (salient) bar AND the cumulative line — they're the
  # same conceptual series. Mix that color with white for the muted
  # "previous" base (50/50 by default; falls back to grey palette when
  # the theme has no colorway, e.g., "Default").
  pal <- plotly_theme_colors(theme)
  # Brand accent as the fallback when the chosen plotly theme has no colorway.
  primary <- if (length(pal) > 0) pal[[1]] else resondex_brand()$colors$accent
  lighten_to_white <- function(hex, mix = 0.55) {
    rgb_v <- tryCatch(grDevices::col2rgb(hex)[, 1],
                      error = function(e) c(89, 89, 89))
    mixed <- round(rgb_v * (1 - mix) + c(255, 255, 255) * mix)
    sprintf("rgb(%d, %d, %d)",
            as.integer(mixed[1]),
            as.integer(mixed[2]),
            as.integer(mixed[3]))
  }
  bar_prev_color <- lighten_to_white(primary, mix = 0.65)
  bar_incr_color <- primary
  line_color     <- primary

  # Bar-top labels: short cumulative value, displayed above each bar.
  fmt_v <- function(v) {
    if (is.na(v)) return("")
    if (use_pct) sprintf("%.0f%%", v) else sprintf("%.2f", v)
  }
  bar_labels <- vapply(y_cumul, fmt_v, character(1))

  # Per-row tooltip body. Pre-formatted strings so we don't depend on
  # plotly's template-substitution semantics for matrix customdata.
  display_name <- ifelse(
    !is.na(labels_col) & nzchar(labels_col) & labels_col != vars,
    paste0(vars, " (", labels_col, ")"),
    vars
  )
  community_line <- if ("Community" %in% names(display)) {
    cm <- as.character(display$Community)
    ifelse(is.na(cm) | !nzchar(cm), "", paste0("<br>Community: ", cm))
  } else rep("", length(vars))
  hover_text <- paste0(
    "Step ", steps, "<br>",
    display_name,
    community_line, "<br>",
    cum_label,  ": ", bar_labels, "<br>",
    incr_label, ": ", vapply(y_incr, fmt_v, character(1))
  )

  fig <- plotly::plot_ly() |>
    plotly::add_bars(
      x = x_factor, y = y_prev,
      name = "Previous",
      marker = list(color = bar_prev_color),
      hoverinfo = "skip",
      showlegend = FALSE
    ) |>
    plotly::add_bars(
      x = x_factor, y = y_incr,
      name = "Incremental",
      marker = list(color = bar_incr_color),
      text = bar_labels,
      textposition = "outside",
      hovertext = hover_text,
      hoverinfo = "text",
      showlegend = FALSE
    ) |>
    plotly::add_trace(
      x = x_factor, y = y_cumul,
      type = "scatter", mode = "lines+markers",
      name = "Cumulative",
      line = list(color = line_color, width = 2),
      marker = list(color = line_color, size = 6),
      hoverinfo = "skip",
      showlegend = FALSE
    ) |>
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "", tickangle = -45,
                   categoryorder = "array",
                   categoryarray = labels_col),
      yaxis = list(title = "", ticksuffix = y_suffix,
                   tickformat = y_fmt),
      margin = list(b = 100, l = 60, r = 20, t = 20)
    )

  # Apply user-selected theme. "Default" is now the branded Resondex theme
  # (no longer a no-op), so always apply.
  fig <- plotly_theme(fig, theme)
  fig
}
