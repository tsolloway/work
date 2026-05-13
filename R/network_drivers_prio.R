# =============================================================================
# Native DT prioritization dashboard for app_deliverable_network_drivers
#
# Mirrors the impacts pattern: Shiny inputs in the sidebar (only for the
# dimensions that vary in the registry) + a reactive DT::datatable in the
# main area. Shares dimension parsing with bn_report via .bn_report_prio_metadata().
# =============================================================================

# ---- Pure data: build the display data frame for the current selections ----

#' @noRd
.network_drivers_prio_data <- function(metadata,
                                       strategy = NULL, search = NULL,
                                       subgroup = NULL, focus = NULL,
                                       weight = NULL) {
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
  out_list[["Cumulative Gain"]]  <- vapply(rows, function(r) r$cumulative_gain %||% NA_real_, numeric(1))
  out_list[["Marginal Gain"]]    <- vapply(rows, function(r) r$marginal_gain %||% NA_real_, numeric(1))
  if (pm$has_p) {
    out_list[["p-value"]] <- vapply(rows, function(r) r$p_value %||% NA_real_, numeric(1))
  }
  attr_obj <- list(
    n_obs = entry$n_obs,
    is_binary = pm$is_binary
  )
  out <- as.data.frame(out_list, stringsAsFactors = FALSE,
                       check.names = FALSE)
  attr(out, "n_obs") <- entry$n_obs
  attr(out, "is_binary") <- pm$is_binary
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
    search   = "Search:",
    subgroup = "Subgroup:",
    focus    = "Focus:",
    weight   = "Weight:"
  )

  inputs <- list()
  for (dn in pm$active_dims) {
    opts <- vapply(pm$dims[[dn]], function(x) as.character(x), character(1))
    # Pretty-print: replace underscores with spaces in the visible label
    pretty_labels <- gsub("_", " ", opts, fixed = TRUE)
    choices <- stats::setNames(opts, pretty_labels)
    inputs <- c(inputs, list(
      shiny::selectInput(
        ns(paste0(prefix, "_", dn)),
        label = dim_labels[[dn]] %||% paste0(dn, ":"),
        choices = choices,
        selected = opts[1]
      )
    ))
  }
  if (length(inputs) == 0) return(NULL)
  shiny::conditionalPanel(
    condition = cond,
    do.call(shiny::tagList, inputs)
  )
}


# ---- UI: DT renderer with p-value coloring ---------------------------------

#' @noRd
.network_drivers_prio_dt <- function(display, metadata,
                                     sig_threshold = 0.05,
                                     marginal_threshold = 0.10) {
  if (is.null(display) || nrow(display) == 0) {
    return(DT::datatable(
      data.frame(Message = "No prioritization results."),
      options = list(dom = "t", paging = FALSE),
      rownames = FALSE, selection = "none"
    ))
  }
  is_binary <- isTRUE(attr(display, "is_binary"))
  numeric_cols <- intersect(
    c("Outcome Estimate", "Cumulative Gain", "Marginal Gain", "p-value"),
    names(display)
  )
  pct_cols <- if (is_binary) {
    intersect(c("Outcome Estimate", "Cumulative Gain", "Marginal Gain"),
              names(display))
  } else character(0)

  # Strip attrs before passing to DT (avoids stray classes in widget)
  display_clean <- display
  attr(display_clean, "n_obs") <- NULL
  attr(display_clean, "is_binary") <- NULL

  dt <- DT::datatable(
    display_clean,
    rownames = FALSE,
    selection = "none",
    class = "row-border hover",
    options = list(
      dom = "t",
      paging = FALSE,
      pageLength = nrow(display_clean),
      scrollX = TRUE,
      scrollY = "100%",
      scrollCollapse = FALSE,
      autoWidth = TRUE,
      columnDefs = list(
        list(className = "dt-center",
             targets = which(names(display_clean) %in%
                             c("Priority", numeric_cols)) - 1L)
      )
    )
  )
  # Format numeric columns: percentages for binary outcome, fixed-decimal else.
  if (length(pct_cols) > 0) {
    dt <- DT::formatPercentage(dt, pct_cols, digits = 1)
  }
  non_pct_num <- setdiff(numeric_cols, pct_cols)
  non_pct_num <- setdiff(non_pct_num, "p-value")
  if (length(non_pct_num) > 0) {
    dt <- DT::formatRound(dt, non_pct_num, digits = 3)
  }
  if ("p-value" %in% names(display_clean)) {
    dt <- DT::formatRound(dt, "p-value", digits = 3)
    dt <- DT::formatStyle(dt, "p-value",
      color = DT::styleInterval(
        c(sig_threshold, marginal_threshold),
        c("#198754", "#E67E22", "#DC3545")
      ),
      fontWeight = DT::styleInterval(c(sig_threshold), c("bold", "normal"))
    )
  }
  dt
}
