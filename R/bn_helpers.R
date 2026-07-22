# =============================================================================
# bn_report shared helpers
#
# Single home for the data-prep / dependency-plumbing helpers that are
# byte-identical across the report variants. Consumed by bn_report() and
# the app_deliverable_network_drivers Shiny module
# (network_drivers_impacts / network_drivers_prio). Do NOT fork these —
# there is exactly one copy, under the canonical .bn_report_* name.
# =============================================================================

# --- internal: drop unused boot-stat columns from an impacts list ---
# bn_impact emits 7 stats per metric in boot mode
# (mean / sd / se / t / ci_low / ci_high / p_value). Only `mean` (display)
# and `p_value` (blackout rule) are consumed by the writers and dashboard.
# The other 5 are dead weight — they push Excel's per-sheet column count
# past the 16,384 limit and inflate the bn_report JSON payload. Strip them
# at the consumer boundary (bn_write / bn_report), not at the engine, so
# the in-memory impacts object stays full-fat for ad-hoc analysis.
.bn_impact_drop_unused_boot_stats <- function(impacts) {
  if (is.null(impacts)) return(impacts)
  drop_one <- function(tbl) {
    if (is.null(tbl)) return(tbl)
    drop_cols <- grep("_(sd|se|t|ci_low|ci_high)$", names(tbl), value = TRUE)
    if (length(drop_cols)) tbl[, setdiff(names(tbl), drop_cols), drop = FALSE]
    else tbl
  }
  for (k in c("table_attribute", "table_attribute_weighted",
              "table_community", "table_community_weighted")) {
    if (!is.null(impacts[[k]])) impacts[[k]] <- drop_one(impacts[[k]])
  }
  impacts
}

# --- internal: assert per-table column count fits Excel's per-sheet cap ---
# Excel's hard limit is 16,384 columns per sheet (column XFD). When the
# caller opts out of `trim_wb` and an impact table would overflow, fail
# loudly here rather than letting openxlsx emit a corrupt workbook. The
# threshold is also used as a uniform sanity gate by bn_report — at that
# scale the embedded JSON payload becomes pathological even though HTML
# itself has no column limit.
.bn_impact_assert_column_cap <- function(impacts, fn_label = "bn_write",
                                         cap = 16384L) {
  if (is.null(impacts)) return(invisible(NULL))
  for (k in c("table_attribute", "table_attribute_weighted",
              "table_community", "table_community_weighted")) {
    tbl <- impacts[[k]]
    if (!is.null(tbl) && ncol(tbl) > cap) {
      stop(sprintf(
        "%s: impacts$%s has %d columns, exceeding the %d-column cap. ",
        fn_label, k, ncol(tbl), cap
      ),
      "Set `trim_wb = TRUE` (the default) to strip the 5 unused boot ",
      "stats (`_sd`, `_se`, `_t`, `_ci_low`, `_ci_high`) — only `_mean` ",
      "and `_p_value` are consumed downstream.",
      call. = FALSE)
    }
  }
  invisible(NULL)
}

# --- internal: normalize results to named list of engine results ---
.bn_report_normalize_results <- function(results) {

  # single engine result (has $meta or $bn)
  if (!is.null(results[["meta"]]) || !is.null(results[["bn"]])) {
    return(list(Network = results))
  }

  # helper: does this object look like an engine result?
  # Mirrors the top-level wrap check above: an engine has either a top-level
  # $meta (bn_engine output) or a top-level $bn (bn_finalize_network output,
  # whose $meta lives inside $bn). Without the $bn check, a user-supplied
  # named list like list("My Label" = bn_final) gets misclassified as a
  # nested-engine container and explodes into separate bn / impacts /
  # prioritizations accordions.
  .is_engine <- function(x) {
    is.list(x) && (!is.null(x[["meta"]]) || !is.null(x[["bn"]]))
  }

  # bn_initial_networks output: mixed bare engines (unsupervised) and
  # nested engine lists (supervised, keyed by DV name)
  children_have_meta <- purrr::map_lgl(results, .is_engine)
  children_have_nested_engines <- purrr::map_lgl(results, function(x) {
    is.list(x) && !.is_engine(x) && any(purrr::map_lgl(x, .is_engine))
  })

  if (any(children_have_meta) || any(children_have_nested_engines)) {
    flat <- list()

    # bare engines (unsupervised): results$cb_unsupervised = <engine>
    for (nm in names(results)[children_have_meta]) {
      flat[[nm]] <- results[[nm]]
    }

    # nested engines (supervised): results$cb_direct$ltr = <engine>
    for (nm in names(results)[children_have_nested_engines]) {
      child <- results[[nm]]
      engines <- purrr::keep(child, .is_engine)
      for (dv_nm in names(engines)) {
        label <- if (length(engines) == 1) nm else paste(nm, dv_nm, sep = " - ")
        flat[[label]] <- engines[[dv_nm]]
      }
    }

    return(flat)
  }

  # already a named list of engine results
  if (is.null(names(results))) {
    names(results) <- paste("Network", seq_along(results))
  }

  results
}

# Internal: align a user-supplied results_excel argument with the (already
# normalized) results list. Returns a named list of length(results); each
# slot holds either a file path (character, length 1) or NULL.
#
# Accepted shapes for results_excel:
#   * NULL                     — every slot NULL.
#   * single character string  — used as the path for the one (and only)
#                                 result; if length(results) > 1, only the
#                                 first slot is filled.
#   * named list / vector      — matched to results by name.
#   * unnamed list / vector    — matched to results by position.
#
# Slot values can themselves be NULL or NA; both are treated as "no button".
.bn_report_normalize_results_excel <- function(results_excel, results) {

  empty <- setNames(rep(list(NULL), length(results)), names(results))

  if (is.null(results_excel)) return(empty)

  # If the user passed a single bn_write() result (S3 bn_write_result, which
  # is a list with $path), unwrap to the path.
  if (inherits(results_excel, "bn_write_result")) {
    results_excel <- as.character(results_excel)
  }

  # Coerce a bare character vector to a list so [[i]] returns a single string.
  if (is.character(results_excel)) results_excel <- as.list(results_excel)

  # Map any bn_write_result entries (or lists with a $path slot) down to their
  # path strings. Leaves plain strings / NULL / NA alone.
  results_excel <- purrr::map(results_excel, function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "bn_write_result")) return(as.character(x))
    if (is.list(x) && !is.null(x[["path"]])) return(x[["path"]])
    x
  })

  # Drop NA slots — treat them as "no button".
  results_excel <- purrr::map(results_excel, function(x) {
    if (length(x) == 0 || (length(x) == 1 && is.na(x))) NULL else x
  })

  out <- empty
  if (!is.null(names(results_excel)) && all(nzchar(names(results_excel)))) {
    # Named — match by name; warn on unmatched names.
    matched   <- intersect(names(results_excel), names(results))
    unmatched <- setdiff(names(results_excel), names(results))
    if (length(unmatched) > 0) {
      warning("results_excel: name(s) not present in results: ",
              paste(unmatched, collapse = ", "), call. = FALSE)
    }
    for (nm in matched) out[[nm]] <- results_excel[[nm]]
  } else {
    # Unnamed — positional, padded / truncated to length(results).
    n <- min(length(out), length(results_excel))
    if (n > 0) for (i in seq_len(n)) out[[i]] <- results_excel[[i]]
  }
  out
}

# --- internal: full Impacts dashboard (HTML + inline JS) -------------------
# Mirrors the bn_impact_write dynamic dashboard: Metric, Focus, and Weight
# dropdowns; one Index column per subgroup; conditional formatting (green/
# yellow/red color scale on index, bold-italic for negative raw metric,
# blackout for p > 0.1, red warning next to Focus when base is below the
# minimum). Total Impact + Base rows recompute as the dropdowns change.
# Works for both attribute-level and community-level impact tables —
# pass is_community = TRUE for the latter (no Variable/Label columns; the
# leading column is "Community" instead).
#' @noRd
# --- internal: parse impact-table metadata --------------------------------
# Pure data-prep extracted from .bn_report_render_attribute_impacts_dashboard
# so other consumers (the app_deliverable_network_drivers Shiny module) can
# reuse the dimension-parsing logic without re-rendering the bn_report HTML
# dashboard. Returns NULL when the requested table is empty/absent — caller
# decides how to handle that.
#' @noRd
.bn_report_impacts_metadata <- function(impacts, is_community = FALSE) {
  if (isTRUE(is_community)) {
    tbl   <- impacts[["table_community"]]
    tbl_w <- impacts[["table_community_weighted"]]
    id_col_name <- "Community"
    id_col_label <- "Community"
  } else {
    tbl   <- impacts[["table_attribute"]]
    tbl_w <- impacts[["table_attribute_weighted"]]
    id_col_name <- "Variable"
    id_col_label <- "Variable"
  }

  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
    return(NULL)
  }

  meta <- impacts[["meta"]] %||% list()
  has_weights <- !is.null(tbl_w)
  min_base_for_lift <- meta[["min_base_for_lift"]] %||% 75L

  all_cols <- names(tbl)
  sgs <- meta[["subgroups"]]
  if (is.null(sgs) || length(sgs) == 0) sgs <- "Total"
  sgs <- sgs[vapply(sgs, function(sg) {
    any(startsWith(all_cols, paste0(sg, "_")))
  }, logical(1))]
  if (length(sgs) == 0) sgs <- "Total"

  sg1 <- sgs[1]
  sg1_cols <- all_cols[startsWith(all_cols, paste0(sg1, "_"))]
  sg1_cols <- sg1_cols[!grepl("_(sd|se|t|ci_low|ci_high|p_value)$", sg1_cols)]
  metric_suffixes <- sub(paste0("^", sg1, "_"), "", sg1_cols)

  .strip_display <- function(x) gsub("_(propdisplay|absdisplay)(_|$)", "\\2", x)
  .strip_shift   <- function(x) gsub("_(propshift|absshift|headshift|rangeshift)(_|$)", "\\2", x)
  base_suffixes <- unique(.strip_display(.strip_shift(metric_suffixes)))

  all_lift_bases    <- grep("^lift", base_suffixes, value = TRUE)
  market_lift_bases <- grep("^lift$|^lift_\\d+$", all_lift_bases, value = TRUE)
  brand_lift_bases  <- setdiff(all_lift_bases, market_lift_bases)

  brand_names <- if (length(brand_lift_bases) > 0) {
    unique(sub("^lift_\\d+_|^lift_", "", brand_lift_bases))
  } else character(0)
  focus_options <- c("Market", brand_names)

  metric_info <- list()
  for (ml in market_lift_bases) {
    label <- if (ml %in% c("lift", "lift_0")) {
      "Average Effect"
    } else {
      pct <- sub("lift_", "", ml)
      pct_num <- suppressWarnings(as.numeric(pct))
      lift_val <- if (!is.na(pct_num)) format(pct_num / 100, nsmall = 2) else pct
      paste0("Effect at ", lift_val)
    }
    metric_info[[length(metric_info) + 1]] <- list(label = label, key = ml)
  }
  if ("maxVmin" %in% base_suffixes) {
    metric_info[[length(metric_info) + 1]] <- list(
      label = "Best-vs-Worst Effect", key = "maxVmin"
    )
  }
  if ("mi" %in% base_suffixes) {
    metric_info[[length(metric_info) + 1]] <- list(
      label = "Explanatory Value", key = "mi"
    )
  }

  .find_average_key <- function() {
    candidates <- c("lift_0", "lift")
    hit <- intersect(candidates, market_lift_bases)
    if (length(hit) > 0) hit[1] else NA_character_
  }
  .find_improvement_key <- function() {
    nz <- setdiff(market_lift_bases, c("lift", "lift_0"))
    if (length(nz) == 0) return(NA_character_)
    pcts <- suppressWarnings(as.numeric(sub("^lift_", "", nz)))
    valid <- !is.na(pcts)
    if (!any(valid)) return(NA_character_)
    nz <- nz[valid]; pcts <- pcts[valid]
    nz[which.min(abs(pcts - 10))]
  }
  preset_map <- list()
  avg_key <- .find_average_key()
  if (!is.na(avg_key)) {
    preset_map[["Current Impact"]] <- list(
      metric = avg_key, shift = "rangeshift",
      question = "What is happening?"
    )
  }
  imp_key <- .find_improvement_key()
  if (!is.na(imp_key)) {
    preset_map[["Intervention Impact"]] <- list(
      metric = imp_key, shift = "headshift",
      question = "What should we do?"
    )
  }
  if ("maxVmin" %in% base_suffixes) {
    preset_map[["Maximum Impact"]] <- list(
      metric = "maxVmin", shift = "rangeshift",
      question = "What is possible?"
    )
  }

  has_outcome_display <- any(grepl("_(propdisplay|absdisplay)$", metric_suffixes))
  has_shift_type      <- any(grepl("_(propshift|absshift|headshift|rangeshift)_", metric_suffixes))
  is_dichotomous_dv   <- isTRUE(meta[["is_dichotomous_dv"]])
  has_community       <- (!isTRUE(is_community)) && ("Community" %in% names(tbl))
  has_label           <- (!isTRUE(is_community)) && ("Label"     %in% names(tbl))

  batteries <- meta[["batteries"]]
  battery_groups <- meta[["battery_groups"]]
  has_battery <- (!isTRUE(is_community)) && !is.null(batteries) &&
    length(batteries) > 0L
  iv_to_battery <- if (has_battery) {
    iv2b <- character(0)
    for (b_name in names(batteries)) {
      ivs_in_b <- batteries[[b_name]]
      iv2b <- c(iv2b, rlang::set_names(rep(b_name, length(ivs_in_b)), ivs_in_b))
    }
    iv2b
  } else NULL
  battery_group_ivs <- if (has_battery && !is.null(battery_groups) &&
                           length(battery_groups) > 0L) {
    lapply(battery_groups, function(comp) {
      unname(unique(unlist(batteries[comp], use.names = FALSE)))
    })
  } else NULL

  list(
    tbl                 = tbl,
    tbl_w               = tbl_w,
    id_col_name         = id_col_name,
    id_col_label        = id_col_label,
    meta                = meta,
    has_weights         = has_weights,
    min_base_for_lift   = min_base_for_lift,
    is_dichotomous_dv   = is_dichotomous_dv,
    sgs                 = sgs,
    metric_info         = metric_info,
    focus_options       = focus_options,
    preset_map          = preset_map,
    has_outcome_display = has_outcome_display,
    has_shift_type      = has_shift_type,
    has_community       = has_community,
    has_label           = has_label,
    has_battery         = has_battery,
    batteries           = batteries,
    battery_groups      = battery_groups,
    iv_to_battery       = iv_to_battery,
    battery_group_ivs   = battery_group_ivs,
    market_lift_bases   = market_lift_bases,
    brand_lift_bases    = brand_lift_bases,
    base_suffixes       = base_suffixes,
    metric_suffixes     = metric_suffixes
  )
}

# --- internal: parse prioritization registry metadata ----------------------
# Pure data-prep extracted from .bn_report_render_prioritization_dashboard
# so the app_deliverable_network_drivers Shiny module can reuse the
# dimension-parsing logic. Returns NULL when there are no prioritization
# results in the registry.
#' @noRd
.bn_report_prio_metadata <- function(priort,
                                     add_prioritization_pvalue = FALSE) {
  registry <- tryCatch(.prioritize_build_registry(priort),
    error = function(e) list())
  if (length(registry) == 0) return(NULL)

  meta <- priort[["meta"]] %||% list()
  min_base_for_boot <- meta[["min_base_for_boot"]] %||% 100L
  lift_pct <- meta[["lift"]] %||% 0.10

  strategies <- unique(vapply(registry, function(e) e$strategy %||% "", character(1)))
  searches   <- unique(vapply(registry, function(e) e$search   %||% "", character(1)))
  subgroups  <- unique(vapply(registry, function(e) e$subgroup %||% "", character(1)))
  focuses    <- unique(vapply(registry, function(e) e$focus    %||% "", character(1)))
  weights    <- unique(vapply(registry, function(e) e$weight   %||% "", character(1)))
  dims <- list(
    strategy = as.list(strategies),
    search   = as.list(searches),
    subgroup = as.list(subgroups),
    focus    = as.list(focuses),
    weight   = as.list(weights)
  )
  active_dims <- names(dims)[vapply(dims, length, integer(1)) > 1]

  lookup <- list()
  for (e in registry) {
    key <- paste(e$strategy, e$search, e$subgroup, e$focus, e$weight, sep = "|")
    tbl <- e$tbl
    if (!is.null(tbl) && is.data.frame(tbl) && "priority" %in% names(tbl)) {
      tbl <- tbl[!(!is.na(tbl$priority) & tbl$priority == 0L), , drop = FALSE]
    }
    rows <- if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
      list()
    } else {
      lapply(seq_len(nrow(tbl)), function(i) {
        list(
          priority            = if ("priority"            %in% names(tbl)) as.integer(tbl$priority[i])            else i - 1L,
          variable            = if ("variable"            %in% names(tbl)) as.character(tbl$variable[i])          else NA_character_,
          community           = if ("community"           %in% names(tbl)) as.character(tbl$community[i])         else NULL,
          label               = if ("label"               %in% names(tbl)) as.character(tbl$label[i])             else NULL,
          dv_estimate         = if ("dv_estimate"         %in% names(tbl)) as.numeric(tbl$dv_estimate[i])         else NA_real_,
          cumulative_gain     = if ("cumulative_gain"     %in% names(tbl)) as.numeric(tbl$cumulative_gain[i])     else NA_real_,
          cumulative_gain_pct = if ("cumulative_gain_pct" %in% names(tbl)) as.numeric(tbl$cumulative_gain_pct[i]) else NA_real_,
          marginal_gain       = if ("marginal_gain"       %in% names(tbl)) as.numeric(tbl$marginal_gain[i])       else NA_real_,
          marginal_gain_pct   = if ("marginal_gain_pct"   %in% names(tbl)) as.numeric(tbl$marginal_gain_pct[i])   else NA_real_,
          p_value             = if ("p_value"             %in% names(tbl)) as.numeric(tbl$p_value[i])             else NA_real_
        )
      })
    }
    lookup[[key]] <- list(
      rows  = rows,
      n_obs = if (is.null(e$n_obs)) NA_integer_ else as.integer(e$n_obs)
    )
  }

  has_community <- any(vapply(lookup, function(x) {
    length(x$rows) > 0 && !is.null(x$rows[[1]]$community)
  }, logical(1)))
  has_label <- any(vapply(lookup, function(x) {
    length(x$rows) > 0 && !is.null(x$rows[[1]]$label)
  }, logical(1)))
  has_p_data <- any(vapply(lookup, function(x) {
    if (length(x$rows) == 0) return(FALSE)
    any(vapply(x$rows, function(r) {
      !is.null(r$p_value) && !is.na(r$p_value)
    }, logical(1)))
  }, logical(1)))
  has_p <- isTRUE(add_prioritization_pvalue) && has_p_data

  lift_pct_int <- round(lift_pct * 100)
  lift_label            <- paste0("Moderate Lift (", lift_pct_int, "%)")
  max_label             <- "Maximum Lift"
  max_deprecated_label  <- "Maximum Lift (Deprecated)"

  meta_shift_type <- meta[["impact_shift_type"]] %||% "headroom"
  lift_shift_explainer <- switch(meta_shift_type,
    "headroom"     = paste0("Closes ", lift_pct_int, "% of each IV’s gap to its top level — every IV moves the same fraction of its own headroom, so cross-scale rankings stay comparable"),
    "proportional" = paste0("Shifts each IV’s mean by ", lift_pct_int, "% of its current value"),
    "absolute"     = paste0("Adds ", lift_pct_int / 100, " scale points to each IV’s mean"),
    paste0("Shifts each IV’s distribution by ", lift_pct_int, "%")
  )

  is_binary_outcome <- {
    dv_vals <- unlist(lapply(lookup, function(x) {
      vapply(x$rows, function(r) r$dv_estimate %||% NA_real_, numeric(1))
    }))
    dv_vals <- dv_vals[!is.na(dv_vals)]
    length(dv_vals) > 0 && min(dv_vals) >= -1e-6 && max(dv_vals) <= 1 + 1e-6
  }

  list(
    meta                  = meta,
    dims                  = dims,
    active_dims           = active_dims,
    lookup                = lookup,
    has_community         = has_community,
    has_label             = has_label,
    has_p                 = has_p,
    has_p_data            = has_p_data,
    min_base_for_boot     = as.integer(min_base_for_boot),
    lift                  = lift_pct,
    lift_label            = lift_label,
    max_label             = max_label,
    max_deprecated_label  = max_deprecated_label,
    is_binary             = is_binary_outcome,
    lift_shift_explainer  = lift_shift_explainer,
    meta_shift_type       = meta_shift_type
  )
}

# --- internal: render a bn_impacts table as an HTML table ----------------
# Consistent with bn_write's dashboard: grey header fill, bold, centered
# numerics, color-coded p-values (green < 0.05, yellow < 0.10), "Index"
# column bolded. Returns an HTML string.
#' @noRd
.bn_report_render_impacts_table <- function(tbl, is_community = FALSE) {
  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
    return('<div class="extra-empty">No impact results to display.</div>')
  }

  cols <- names(tbl)

  # Order columns consistently: variable, label, community, then metric groups
  first_cols <- intersect(c("Variable", "Community", "Label"), cols)
  metric_cols <- setdiff(cols, first_cols)
  ordered_cols <- c(first_cols, metric_cols)
  tbl <- tbl[, ordered_cols, drop = FALSE]

  # Detect p-value columns (for coloring) — anything ending in _p_val
  pval_cols <- grep("_p_val$|^p_val$", names(tbl), value = TRUE)
  # Detect index columns — anything ending in _index or a bare "index"
  index_cols <- grep("_index$|^index$|^Index$", names(tbl), value = TRUE)
  # Numeric columns (for centering / formatting)
  num_cols <- names(tbl)[vapply(tbl, is.numeric, logical(1))]

  .fmt_cell <- function(col, val) {
    # Flatten list-column entries and normalize to a single scalar for
    # formatting. Some impact tables carry list columns (e.g., bootstrap
    # arrays) that would otherwise trip up is.na() / as.numeric().
    if (is.list(val)) val <- unlist(val, use.names = FALSE)
    if (length(val) == 0) return("")
    if (length(val) > 1) val <- paste(format(val), collapse = ", ")
    if (is.na(val)) return("")
    if (col %in% pval_cols) {
      pv <- suppressWarnings(as.numeric(val))
      if (!is.finite(pv)) return("")
      cls <- if (pv < 0.05) "rdx-pval-sig" else if (pv < 0.10) "rdx-pval-marg" else "rdx-pval-insig"
      sprintf('<span class="%s">%s</span>', cls, formatC(pv, format = "f", digits = 3))
    } else if (col %in% index_cols) {
      num <- suppressWarnings(as.numeric(val))
      if (!is.finite(num)) return("")
      sprintf('<strong>%s</strong>', formatC(num, format = "d"))
    } else if (col %in% num_cols) {
      num <- suppressWarnings(as.numeric(val))
      if (!is.finite(num)) return("")
      if (abs(num) < 1) formatC(num, format = "f", digits = 3)
      else formatC(num, format = "f", digits = 2)
    } else {
      htmltools::htmlEscape(as.character(val))
    }
  }

  .col_class <- function(col) {
    if (col %in% num_cols) "num-col" else "txt-col"
  }

  header_html <- paste0(
    "<tr>",
    paste(
      vapply(names(tbl), function(c) {
        sprintf('<th class="%s">%s</th>', .col_class(c), htmltools::htmlEscape(c))
      }, character(1)),
      collapse = ""
    ),
    "</tr>"
  )

  body_rows <- vapply(seq_len(nrow(tbl)), function(i) {
    cells <- vapply(names(tbl), function(c) {
      sprintf('<td class="%s">%s</td>', .col_class(c), .fmt_cell(c, tbl[[c]][[i]]))
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))

  paste0(
    '<div class="extra-wrap">',
    '<table class="extra-table">',
    '<thead>', header_html, '</thead>',
    '<tbody>', paste(body_rows, collapse = ""), '</tbody>',
    '</table>',
    '</div>'
  )
}

# --- internal: cache shared widget dependency files ---
#' @noRd
.bn_report_cache_deps <- function(widget_html, lib_dir) {
  # extract ordered <script src="widget_N_files/..."> tags
  script_pattern <- '<script[^>]+src="([^"]+_files/[^"]+)"[^>]*>\\s*</script>'
  script_tags <- regmatches(widget_html, gregexpr(script_pattern, widget_html, perl = TRUE))[[1]]
  script_srcs <- sub(script_pattern, "\\1", script_tags, perl = TRUE)

  # extract ordered <link href="widget_N_files/..."> tags
  link_pattern <- '<link[^>]+href="([^"]+_files/[^"]+)"[^>]*>'
  link_tags <- regmatches(widget_html, gregexpr(link_pattern, widget_html, perl = TRUE))[[1]]
  link_hrefs <- sub(link_pattern, "\\1", link_tags, perl = TRUE)

  inline_map <- list()

  for (i in seq_along(link_tags)) {
    file_path <- file.path(dirname(lib_dir), link_hrefs[i])
    if (file.exists(file_path)) {
      content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
      inline_map[[link_tags[i]]] <- paste0("<style>", content, "</style>")
    }
  }

  for (i in seq_along(script_tags)) {
    file_path <- file.path(dirname(lib_dir), script_srcs[i])
    if (file.exists(file_path)) {
      content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
      inline_map[[script_tags[i]]] <- paste0("<script>", content, "</script>")
    }
  }

  if (length(inline_map) == 0) {
    cli::cli_warn("No widget dependencies found to cache. Self-contained inlining may fail.")
  }

  inline_map
}

# --- internal: concatenate cached dep content into shared deps string ---
#' @noRd
.bn_report_shared_deps_string <- function(dep_cache) {
  paste(unlist(dep_cache, use.names = FALSE), collapse = "\n")
}

# --- internal: strip shared dep tags, leaving <!--SHARED_DEPS--> marker ---
#' @noRd
.bn_report_strip_deps <- function(widget_html, dep_cache, widget_lib_prefix, first_lib_prefix) {
  placeholder_inserted <- FALSE
  for (original_tag in names(dep_cache)) {
    this_tag <- gsub(first_lib_prefix, widget_lib_prefix, original_tag, fixed = TRUE)
    if (!placeholder_inserted) {
      widget_html <- sub(this_tag, "<!--SHARED_DEPS-->", widget_html, fixed = TRUE)
      placeholder_inserted <- TRUE
    } else {
      widget_html <- sub(this_tag, "", widget_html, fixed = TRUE)
    }
  }
  widget_html
}

# --- internal: build download filename prefix ---
#' @noRd
.bn_report_download_prefix <- function(title, subtitle, result_name, tab_label) {
  dl_parts <- c(title, subtitle, result_name, tab_label)
  paste(dl_parts[nchar(dl_parts) > 0], collapse = " - ")
}


# =============================================================================
# Membership view builder — extracted from bn_report's inner closures.
# Produces the dual-view (table + card) HTML for a single accordion's
# Membership tab. Pure function — closes over nothing.
# =============================================================================
.bn_report_render_membership <- function(result, result_name) {
  nodes_df <- tryCatch(
    work::find_recursive(result, x_name = "attribute_viz_prep")$nodes,
    error = function(e) NULL
  )
  if (is.null(nodes_df)) return("")

  # group nodes by community
  groups <- nodes_df %>%
    dplyr::arrange(group) %>%
    dplyr::group_by(community_name, color) %>%
    dplyr::summarise(
      nodes = list(tibble::tibble(id = id, label = label)),
      .groups = "drop"
    )

  # --- table view ---
  table_rows <- purrr::pmap_chr(groups, function(community_name, color, nodes) {
    pills <- purrr::map_chr(seq_len(nrow(nodes)), function(i) {
      glue::glue('<span class="node-pill" data-node-id="{nodes$id[i]}">{nodes$label[i]}</span>')
    })
    pills_str <- paste(pills, collapse = "")
    n_nodes <- nrow(nodes)
    glue::glue(
      '<tr>',
      '<td><span class="membership-dot" style="background: {color};"></span>',
      '<span class="community-label" data-color="{color}" data-orig-comm="{community_name}">{community_name}</span>',
      '<span class="card-count">{n_nodes}</span></td>',
      '<td><div class="card-nodes">{pills_str}</div></td>',
      '</tr>'
    )
  })
  table_html <- paste0(
    '<table class="membership-table">',
    '<thead><tr><th>Community</th><th>Attributes</th></tr></thead>',
    '<tbody>', paste(table_rows, collapse = ""), '</tbody></table>'
  )

  # --- card view ---
  cards <- purrr::pmap_chr(groups, function(community_name, color, nodes) {
    pills <- purrr::map_chr(seq_len(nrow(nodes)), function(i) {
      glue::glue('<span class="node-pill" data-node-id="{nodes$id[i]}">{nodes$label[i]}</span>')
    })
    pills_str <- paste(pills, collapse = "")
    n_nodes <- nrow(nodes)
    glue::glue(
      '<div class="membership-card" style="border-left: 4px solid {color};">',
      '<div class="mc-header"><span class="membership-dot" style="background: {color};"></span>',
      '<span class="community-label" data-color="{color}" data-orig-comm="{community_name}">{community_name}</span>',
      '<span class="card-count">{n_nodes}</span></div>',
      '<div class="card-nodes">{pills_str}</div>',
      '</div>'
    )
  })
  cards_html <- paste0('<div class="membership-cards">', paste(cards, collapse = ""), '</div>')

  # wrap both views with toggle
  paste0(
    '<div class="membership-wrap" data-result="', result_name, '">',
    '<div class="membership-toolbar">',
    '<button class="report-btn membership-toggle" onclick="toggleMembershipView(this)" title="Switch view">',
    '&#9776; Toggle View</button></div>',
    '<div class="membership-view membership-table-view" style="display:none;">', table_html, '</div>',
    '<div class="membership-view membership-card-view">', cards_html, '</div>',
    '</div>'
  )
}


# =============================================================================
# Widget render — extracted from bn_report's inner closure. Builds one
# visNetwork iframe slot (the actual network canvas), including the
# brand-tokenized iframe CSS injection, the saveWidget detour, and the
# self_contained-vs-lib branching.
#
# Two threaded-through containers:
#   - `cfg`: named list of IMMUTABLE per-bn_report settings
#     (add_key, interactive, physics, gravity_constant, central_gravity,
#     charge_layout, seed, title, subtitle, self_contained, tmp_dir).
#   - `state`: ENVIRONMENT carrying mutable per-bn_report counters and
#     dep cache (widget_counter, dep_cache, first_lib_prefix,
#     shared_deps_b64). Uses an environment instead of a list because
#     the helper updates it across calls within one bn_report
#     invocation, and environments give us reference semantics.
# =============================================================================
.bn_report_render_widget <- function(
    result, type, do_community_val, result_name = NULL,
    cfg, state
) {

  # no legend on community tabs
  use_key <- cfg$add_key && !do_community_val

  # build namespace key for report-level save/load
  view_name <- if (do_community_val) "community" else "attribute"
  ns <- if (!is.null(result_name)) {
    paste(result_name, type, view_name, sep = "|")
  } else NULL

  # build download prefix: {title} - {subtitle} - {accordion} - {tab}
  tab_label <- if (do_community_val) "Community" else "Attribute"
  dl_prefix <- .bn_report_download_prefix(cfg$title, cfg$subtitle, result_name, tab_label)

  viz <- tryCatch(
    bn_visual(
      obj = result,
      type = type,
      do_community = do_community_val,
      # vs_height intentionally omitted. The inline saveWidget height
      # is overridden inside the iframe by the injected
      # `.html-widget { height: 100vh !important }` rule below, so
      # passing vs_height here has no effect on the rendered canvas.
      # If you want canvas sizing to come from vs_height, drop the
      # `!important` from that CSS rule first.
      interactive = cfg$interactive,
      # always TRUE: vis.js needs physics ON to compute force-directed layout.
      # when user passes physics=FALSE, the __disablePhysicsAfterStabilize
      # flag (injected below) freezes nodes after stabilization.
      physics = TRUE,
      gravity_constant = cfg$gravity_constant,
      central_gravity = cfg$central_gravity,
      charge_layout = cfg$charge_layout,
      add_key = use_key,
      panel_ns = ns,
      download_prefix = dl_prefix,
      save_visuals = FALSE,
      seed = cfg$seed
    ),
    error = function(e) {
      warning("bn_report render_widget failed for [", result_name, " / ", type, " / ", view_name, "]: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(viz)) {
    return(glue::glue(
      '<div style="height: 100px; padding: 20px; color: #888;">',
      '<p>Could not render this view.</p>',
      '</div>'
    ))
  }

  state$widget_counter <- state$widget_counter + 1L
  widget_file <- file.path(cfg$tmp_dir, glue::glue("widget_{state$widget_counter}.html"))
  widget_lib_prefix <- paste0("widget_", state$widget_counter, "_files")

  # always save non-self-contained to avoid redundant pandoc calls.
  # when self_contained = TRUE, we cache shared lib files from the first
  # widget and manually inline them for all subsequent widgets.
  htmlwidgets::saveWidget(
    viz,
    file = widget_file,
    selfcontained = FALSE
  )

  # read and inject iframe-level CSS overrides
  widget_html <- readLines(widget_file, warn = FALSE) %>% paste(collapse = "\n")
  inject_head <- paste0(
    "<head><style>",
    # Brand layer first (Inter @import must lead): the network iframe is
    # an isolated sandboxed document, so resondex_css() carries the
    # --ndr-* tokens + Inter into it. Without this the visNetwork
    # toolbar's var(--ndr-*) styles fall back to unstyled.
    resondex_css(include_import = TRUE),
    "body,html{margin:0!important;padding:0!important;height:100%!important;overflow:hidden!important;",
    # Background tracks --ndr-card-bg so the iframe surface flips with
    # the parent's dark/light toggle (Stage 1 dark mode for network
    # iframes). Parent sends a postMessage({type:'setMode',mode:...})
    # which toggles data-bs-theme on this iframe's <html>, and the
    # brand layer's dark overrides take care of the rest.
    "background-color:var(--ndr-card-bg)!important;color:var(--ndr-text);}",
    # .html-widget and .vis-network default to white — that paints OVER
    # the body bg. Force them transparent so the body's --ndr-card-bg
    # shows through and flips with mode.
    " .html-widget{height:100vh!important;background-color:transparent!important;}",
    " .vis-network,.vis-network canvas{background-color:transparent!important;}",
    " #pngButton,#svgButton,#fontButton,#physicsButton{width:130px!important;height:30px!important;}",
    "</style>"
  )

  # when physics = FALSE, set a global flag the interactivity JS will read
  if (!cfg$physics) {
    inject_head <- paste0(inject_head, "<script>window.__disablePhysicsAfterStabilize=true;</script>")
  }

  widget_html <- sub("<head>", inject_head, widget_html)

  # Branches set the only two things that differ between the
  # self-contained and non-self-contained iframe markup:
  #   - wrap_attr:   `data-widget="{b64}"` on `.iframe-wrap` (TRUE only).
  #                  Picked up by client-side JS to populate the iframe
  #                  via a blob URL once shared deps are injected.
  #   - iframe_attr: `src="lib/widget_N.html"` on the <iframe> (FALSE
  #                  only). TRUE has no `src` because the widget HTML
  #                  lives in the wrap's data attribute, not on disk.
  # Everything else (spinner overlay, iframe style, sandbox) is
  # identical between the two paths, so we emit it once below.
  if (cfg$self_contained) {
    widget_lib_dir <- file.path(cfg$tmp_dir, widget_lib_prefix)

    # cache deps from the first widget; reuse for all subsequent
    if (is.null(state$dep_cache)) {
      state$dep_cache <- .bn_report_cache_deps(widget_html, widget_lib_dir)
      state$first_lib_prefix <- widget_lib_prefix
      # store shared deps as base64 once — JS injects into each iframe via blob URL
      deps_string <- .bn_report_shared_deps_string(state$dep_cache)
      state$shared_deps_b64 <- base64enc::base64encode(charToRaw(deps_string))
    }

    # strip shared dep tags, leaving <!--SHARED_DEPS--> marker for JS injection
    widget_html <- .bn_report_strip_deps(
      widget_html, state$dep_cache, widget_lib_prefix, state$first_lib_prefix
    )
    widget_b64  <- base64enc::base64encode(charToRaw(widget_html))
    wrap_attr   <- paste0(' data-widget="', widget_b64, '"')
    iframe_attr <- ""
  } else {
    widget_rel <- glue::glue("lib/widget_{state$widget_counter}.html")
    writeLines(widget_html, widget_file)

    wrap_attr   <- ""
    iframe_attr <- paste0('src="', widget_rel, '" ')
  }

  glue::glue(
    '<div class="iframe-wrap"{wrap_attr}>',
    '<div class="spinner-overlay"><div class="spinner"><div class="spinner-bar"></div><div class="spinner-bar"></div><div class="spinner-bar"></div></div></div>',
    '<iframe {iframe_attr}',
    'style="width: 100%; height: 70vh; border: none;" ',
    'sandbox="allow-scripts allow-downloads" allowfullscreen>',
    '</iframe></div>'
  )
}


# =============================================================================
# Per-type-panel HTML builder used by bn_report() — extracted from the
# 250-line `purrr::imap(results, ...)` closure inside bn_report so that
# function's body stays readable.
#
# For each layout type (none / gravity / charge / hierarchy) inside a
# result accordion, returns a <div class="type-panel"> wrapping either:
#   (a) a 3-tab block (Attribute / Community / Membership) plus the
#       optional extras (Attribute Impacts / Community Impacts /
#       Prioritization) when `has_tabs` is TRUE; or
#   (b) a single network view when `has_tabs` is FALSE.
#
# `render_widget` and `render_membership` are passed in as closures so
# their captured state (widget_counter, dep_cache, shared_deps_b64,
# tmp_dir, etc.) stays scoped to the bn_report call site.
# =============================================================================
.bn_report_build_type_panel <- function(
    type, label, panel_id, visible, has_tabs,
    result, name, rid, types, type_labels,
    do_community,
    render_widget, render_membership,
    add_additional_results,
    impacts_res, prioritizations_res,
    shared_attr_id, shared_comm_id,
    qc_mode, outcome_display, shift_type,
    add_prioritization_pvalue, prioritize_display
) {

  # Per-type layout dropdown — each type-panel carries its own copy with
  # its own type pre-selected, so when switchType swaps to this panel the
  # dropdown already reads the right value (no JS sync needed).
  type_options <- purrr::map2_chr(types, type_labels, function(t, l) {
    sel <- if (t == type) " selected" else ""
    glue::glue('<option value="{rid}_{t}"{sel}>{l}</option>')
  })
  type_options_str <- paste(type_options, collapse = "\n            ")
  layout_ctrl_html <- glue::glue(
    '<div class="layout-controls">',
    '<label for="{panel_id}_layout">Layout</label>',
    '<select id="{panel_id}_layout" class="layout-select" ',
    'onchange="switchType(\'{rid}\', this.value)">',
    '{type_options_str}',
    '</select>',
    '</div>'
  )

  # Shared toggle-button markup — used by both branches.
  ctrl_toggle_html <- paste0(
    '<button type="button" class="ctrl-toggle" onclick="toggleCtrls(this)" ',
    'aria-label="Toggle controls panel" title="Toggle controls">',
    '<span class="chev">&#9664;</span></button>'
  )

  if (has_tabs) {

    # Network views: .network-dashboard hosts the collapsible
    # .network-controls well panel (Layout dropdown) + the visNetwork
    # widget inside .network-main (grid-area: main). The grid layout
    # flips cleanly between expanded (248px sidebar + main) and
    # collapsed (44px rail + main).
    tab_attr <- paste0(
      '<div class="network-dashboard">',
      ctrl_toggle_html,
      '<div class="network-controls">', layout_ctrl_html, '</div>',
      '<div class="network-main">',
      render_widget(result, type, FALSE, result_name = name),
      '</div>',
      '</div>'
    )
    tab_comm <- paste0(
      '<div class="network-dashboard">',
      ctrl_toggle_html,
      '<div class="network-controls">', layout_ctrl_html, '</div>',
      '<div class="network-main">',
      render_widget(result, type, TRUE,  result_name = name),
      '</div>',
      '</div>'
    )
    tab_memb <- render_membership(result, name)

    attr_id <- glue::glue("{panel_id}_attr")
    comm_id <- glue::glue("{panel_id}_comm")
    memb_id <- glue::glue("{panel_id}_memb")

    # Optional extra tabs: Attribute Impacts / Community Impacts /
    # Prioritization. Each only emitted when the corresponding result
    # is present AND add_additional_results = TRUE.
    extras_buttons <- character(0)
    extras_panels  <- character(0)

    if (isTRUE(add_additional_results)) {

      if (!is.null(impacts_res) && !is.null(impacts_res[["table_attribute"]])) {
        impact_attr_id <- glue::glue("{panel_id}_impact_attr")
        impact_attr_res <- .bn_report_render_attribute_impacts_dashboard(
          impacts_res, result_name = name, dashboard_id = impact_attr_id,
          qc_mode = qc_mode,
          outcome_display = outcome_display, shift_type = shift_type,
          shared_data_id = shared_attr_id
        )
        impact_attr_html <- impact_attr_res$html
        extras_buttons <- c(extras_buttons, glue::glue(
          '    <button class="tab-btn" onclick="switchTab(this, \'{impact_attr_id}\')">Attribute Impacts</button>'
        ))
        extras_panels <- c(extras_panels, glue::glue(
          '  <div id="{impact_attr_id}" class="tab-panel impact-panel" data-result="{name}" data-layout="{type}" data-view="impact_attr">{impact_attr_html}</div>'
        ))
      }

      if (!is.null(impacts_res) && !is.null(impacts_res[["table_community"]])) {
        impact_comm_id <- glue::glue("{panel_id}_impact_comm")
        impact_comm_res <- .bn_report_render_attribute_impacts_dashboard(
          impacts_res, result_name = name, dashboard_id = impact_comm_id,
          is_community = TRUE, qc_mode = qc_mode,
          outcome_display = outcome_display, shift_type = shift_type,
          shared_data_id = shared_comm_id
        )
        impact_comm_html <- impact_comm_res$html
        extras_buttons <- c(extras_buttons, glue::glue(
          '    <button class="tab-btn" onclick="switchTab(this, \'{impact_comm_id}\')">Community Impacts</button>'
        ))
        extras_panels <- c(extras_panels, glue::glue(
          '  <div id="{impact_comm_id}" class="tab-panel impact-panel" data-result="{name}" data-layout="{type}" data-view="impact_comm">{impact_comm_html}</div>'
        ))
      }

      if (!is.null(prioritizations_res)) {
        priort_id <- glue::glue("{panel_id}_priort")
        # Pull thresholds from the prioritizations meta (set at
        # bn_finalize_network / bn_prioritizations time). Fall back to
        # standard defaults if absent.
        priort_meta <- prioritizations_res[["meta"]] %||% list()
        priort_html <- .bn_report_render_prioritization_dashboard(
          prioritizations_res, result_name = name, dashboard_id = priort_id,
          sig_threshold = priort_meta[["sig_threshold"]] %||% 0.05,
          marginal_threshold = priort_meta[["marginal_threshold"]] %||% 0.10,
          add_prioritization_pvalue = add_prioritization_pvalue,
          prioritize_display = prioritize_display
        )
        extras_buttons <- c(extras_buttons, glue::glue(
          '    <button class="tab-btn" onclick="switchTab(this, \'{priort_id}\')">Prioritization</button>'
        ))
        extras_panels <- c(extras_panels, glue::glue(
          '  <div id="{priort_id}" class="tab-panel priort-panel" data-result="{name}" data-layout="{type}" data-view="prioritization">{priort_html}</div>'
        ))
      }
    }

    extras_buttons_str <- paste(extras_buttons, collapse = "\n")
    extras_panels_str  <- paste(extras_panels,  collapse = "\n")

    glue::glue(
      '<div id="{panel_id}" class="type-panel" style="display: {visible};">',
      '  <div class="tab-bar">',
      '    <button class="tab-btn active" onclick="switchTab(this, \'{attr_id}\')">Attribute</button>',
      '    <button class="tab-btn" onclick="switchTab(this, \'{comm_id}\')">Community</button>',
      '    <button class="tab-btn" onclick="switchTab(this, \'{memb_id}\')">Membership</button>',
      '{extras_buttons_str}',
      '  </div>',
      '  <div id="{attr_id}" class="tab-panel active attr-panel" data-result="{name}" data-layout="{type}" data-view="attribute">{tab_attr}</div>',
      '  <div id="{comm_id}" class="tab-panel comm-panel" data-result="{name}" data-layout="{type}" data-view="community">{tab_comm}</div>',
      '  <div id="{memb_id}" class="tab-panel membership-panel" data-result="{name}" data-layout="{type}" data-view="membership">{tab_memb}</div>',
      '{extras_panels_str}',
      '</div>'
    )

  } else {

    # Single-view branch (do_community is a length-1 vector). Same
    # network-dashboard + collapsible well panel layout as the has_tabs
    # branch, just without the tab strip and extras.
    panel_content <- render_widget(result, type, do_community[1], result_name = name)

    glue::glue(
      '<div id="{panel_id}" class="type-panel" style="display: {visible};">',
      '  <div class="network-dashboard">',
      '    {ctrl_toggle_html}',
      '    <div class="network-controls">{layout_ctrl_html}</div>',
      '    <div class="network-main">{panel_content}</div>',
      '  </div>',
      '</div>'
    )
  }
}


# =============================================================================
# Restyled (app-look) render/CSS/JS helpers — canonical .bn_report_* names.
# Used by bn_report() AND embedded by the app_deliverable_network_drivers
# Shiny module (it calls .bn_report_css / .bn_report_js by name, which
# resolve here).
# =============================================================================

.bn_report_render_attribute_impacts_dashboard <- function(
    impacts, result_name, dashboard_id, is_community = FALSE,
    qc_mode = FALSE,
    outcome_display = NULL, shift_type = "absolute",
    # When non-NULL, emit the dashboard markup with a `data-impact-data-id`
    # pointer instead of an inline `<script class="impact-data">` payload —
    # the caller is then responsible for emitting that shared script once.
    # This dedupe lets a single JSON payload back N type-panel copies of
    # the same dashboard (one per layout), cutting bn_report file size
    # dramatically when add_additional_results = TRUE.
    shared_data_id = NULL
) {
  m <- .bn_report_impacts_metadata(impacts, is_community = is_community)
  if (is.null(m)) {
    return('<div class="extra-empty">No impact results.</div>')
  }
  # Unpack metadata into local names so the rest of the function (the
  # rendering layer) reads the same as before the refactor.
  tbl                 <- m$tbl
  tbl_w               <- m$tbl_w
  id_col_name         <- m$id_col_name
  id_col_label        <- m$id_col_label
  meta                <- m$meta
  has_weights         <- m$has_weights
  min_base_for_lift   <- m$min_base_for_lift
  is_dichotomous_dv   <- m$is_dichotomous_dv
  sgs                 <- m$sgs
  metric_info         <- m$metric_info
  focus_options       <- m$focus_options
  preset_map          <- m$preset_map
  has_outcome_display <- m$has_outcome_display
  has_shift_type      <- m$has_shift_type
  has_community       <- m$has_community
  has_label           <- m$has_label
  has_battery         <- m$has_battery
  batteries           <- m$batteries
  battery_groups      <- m$battery_groups
  iv_to_battery       <- m$iv_to_battery
  battery_group_ivs   <- m$battery_group_ivs
  metric_suffixes     <- m$metric_suffixes
  all_cols <- names(tbl)

  # Auto-default Outcome Display by DV type when caller didn't pass an
  # explicit value: dichotomous DVs read more naturally as raw probability
  # points (Point Change / absdisplay), everything else as % change.
  if (is.null(outcome_display)) {
    outcome_display <- if (is_dichotomous_dv) "absolute" else "proportional"
  }

  # Per-subgroup column allow-list: in bootstrap mode the table can carry
  # thousands of `<col>_<stat>` columns (mean, sd, se, t, ci_low, ci_high,
  # p_value × every metric × every brand × every shift × every display).
  # The HTML dashboard only ever reads VALUE columns (no suffix) and
  # `_p_value`. Filtering here drops dropbox payload size by ~80% on a
  # bootstrap table without losing any rendered information.
  sg_cols_keep <- if (any(grepl("_p_value$", all_cols))) {
    # boot mode → keep value cols + _p_value cols + base + p_val + dv_max/min
    all_cols[!grepl("_(sd|se|t|ci_low|ci_high)$", all_cols)]
  } else {
    all_cols
  }

  # --- Flatten one table (unweighted or weighted) into per-row JSON lists
  .flatten <- function(tt) {
    lapply(seq_len(nrow(tt)), function(i) {
      row <- list(
        id        = as.character(tt[[id_col_name]][i]),
        community = if (has_community) as.character(tt$Community[i]) else NULL,
        battery   = if (has_battery) {
          v <- as.character(tt[[id_col_name]][i])
          if (v %in% names(iv_to_battery)) unname(iv_to_battery[v]) else ""
        } else NULL,
        label     = if (has_label)     as.character(tt$Label[i])     else NULL,
        sg        = list()
      )
      for (sg in sgs) {
        sg_data <- list()
        sg_cols <- sg_cols_keep[startsWith(sg_cols_keep, paste0(sg, "_"))]
        for (col in sg_cols) {
          suf <- sub(paste0("^", sg, "_"), "", col)
          v <- tt[[col]][i]
          sg_data[[suf]] <- if (is.numeric(v) && is.finite(v)) as.numeric(v) else NA
        }
        row$sg[[sg]] <- sg_data
      }
      row
    })
  }

  data_obj <- list(
    subgroups         = as.list(sgs),
    focuses           = as.list(focus_options),
    metrics           = metric_info,
    has_weights       = has_weights,
    has_outcome_display = has_outcome_display,
    has_shift_type    = has_shift_type,
    has_community     = has_community,
    has_battery       = has_battery,
    has_label         = has_label,
    battery_groups    = battery_group_ivs,
    # Assess preset map — JS reads this on Assess dropdown change to
    # set the Analysis (data-dim="metric") and Shift Type (data-dim="shift")
    # dropdowns. Empty list => no Assess dropdown rendered.
    presets           = preset_map,
    # Bootstrap mode: bn_impact emits per-metric `<col>_p_value` columns
    # when impact_n_boot > 1. When TRUE, the JS blackout rule looks up the
    # bootstrap p-value of whichever metric is currently selected, instead
    # of the static MI chi-squared p_val.
    boot_applied      = any(grepl("_p_value$", all_cols)),
    min_base_for_lift = as.integer(min_base_for_lift),
    qc_mode           = isTRUE(qc_mode),
    rows_unweighted   = .flatten(tbl),
    rows_weighted     = if (has_weights) .flatten(tbl_w) else NULL
  )

  # digits = NA preserves full numeric precision (default is 4 decimals,
  # which truncates lift values around 0.01 into 0.01 — losing 3+
  # significant digits and producing off-by-one index discrepancies vs
  # the Excel dashboard which reads the full-precision stored values).
  data_json <- jsonlite::toJSON(data_obj, auto_unbox = TRUE,
    null = "null", na = "null", digits = NA)

  # --- HTML scaffold
  # Controls row — Focus always shown. Metric always shown. Weight only if
  # weighted data is available. Subgroup is rendered as columns (not a dropdown).
  focus_options_html <- paste0(
    vapply(focus_options, function(f) {
      sprintf('<option value="%s">%s</option>',
        htmltools::htmlEscape(f), htmltools::htmlEscape(f))
    }, character(1)),
    collapse = "\n"
  )
  metric_options_html <- paste0(
    vapply(metric_info, function(m) {
      sprintf('<option value="%s">%s</option>',
        htmltools::htmlEscape(m$key), htmltools::htmlEscape(m$label))
    }, character(1)),
    collapse = "\n"
  )
  weight_options_html <- '<option value="Unweighted">Unweighted</option><option value="Weighted">Weighted</option>'

  # Each control is wrapped in its own .impact-ctrl-cell div so a 3-column
  # CSS grid can align them in a tidy 3x2 layout (Metric, Focus, Weight on
  # row 1; Outcome Display, Shift Type, (empty) on row 2). Cells that don't
  # apply (no weights, no shift variants) render empty <div>s so grid
  # placement stays stable regardless of which controls are available.
  # Each control cell has two stacked children: .impact-ctrl-row (label +
  # select inline) and .impact-warning (drops to its own line when populated).
  # Keeping the warning outside the row means warning text wraps within the
  # cell's width instead of expanding the cell and pushing neighbouring
  # controls out of the grey container.
  weight_ctrl <- if (has_weights) {
    sprintf(paste0(
      '<div class="impact-ctrl-cell">',
        '<div class="impact-ctrl-row">',
          '<label><span class="ndr-tip" data-tip="Whether weights are applied when calculating impacts.">Weight:</span></label>',
          '<select class="impact-ctrl" data-dim="weight">%s</select>',
        '</div>',
        '<span class="impact-warning" data-for="weight"></span>',
      '</div>'
    ), weight_options_html)
  } else '<div class="impact-ctrl-cell"></div>'

  # Outcome Display dropdown — offered when both display variants exist.
  # Proportional = (p1-p0)/p0 for maxVmin and shifted/observed for lift.
  # Absolute = p1-p0 for maxVmin and raw DV probability shift for lift.
  # MI is unaffected (no display variant in the data).
  display_ctrl <- if (has_outcome_display) {
    abs_sel  <- if (outcome_display == "absolute")     " selected" else ""
    prop_sel <- if (outcome_display == "proportional") " selected" else ""
    paste0(
      '<div class="impact-ctrl-cell">',
        '<div class="impact-ctrl-row">',
          '<label><span class="ndr-tip" data-tip="How outcome change is displayed — relative vs absolute point change.">Outcome:</span></label>',
          '<select class="impact-ctrl" data-dim="display">',
            '<option value="propdisplay"', prop_sel, '>% Change</option>',
            '<option value="absdisplay"', abs_sel, '>Point Change</option>',
          '</select>',
        '</div>',
        '<span class="impact-warning" data-for="display"></span>',
      '</div>'
    )
  } else '<div class="impact-ctrl-cell"></div>'

  # Shift Type dropdown (Pass B). Controls how the lift metric interprets
  # the IV distribution shift. MaxVmin/mi are shift-invariant; JS suppresses
  # the effect when those metrics are selected. "Headroom" is the most
  # comparable cross-scale option (every IV moves the same fraction of its
  # own gap to the boundary).
  shift_ctrl <- if (has_shift_type) {
    abs_sel   <- if (shift_type == "absolute")     " selected" else ""
    prop_sel  <- if (shift_type == "proportional") " selected" else ""
    head_sel  <- if (shift_type == "headroom")     " selected" else ""
    range_sel <- if (shift_type == "range")        " selected" else ""
    paste0(
      '<div class="impact-ctrl-cell">',
        '<div class="impact-ctrl-row">',
          '<label><span class="ndr-tip" data-tip="How each attribute&#39;s movement is calculated when computing impact.">Shift Type:</span></label>',
          '<select class="impact-ctrl" data-dim="shift">',
            '<option value="propshift"',  prop_sel,  '>% of Current Mean</option>',
            '<option value="absshift"',   abs_sel,   '>Fixed Step</option>',
            '<option value="headshift"',  head_sel,  '>% Toward Top</option>',
            '<option value="rangeshift"', range_sel, '>% of Range</option>',
          '</select>',
        '</div>',
        '<span class="impact-warning" data-for="shift"></span>',
      '</div>'
    )
  } else '<div class="impact-ctrl-cell"></div>'

  # Index By dropdown — visible only when battery info is present.
  # Options: "All" (global index, all rows shown), each battery name (filter
  # to that battery), then each group name (filter to the union of the
  # group's component batteries). JS reads `data-dim="indexby"`.
  indexby_ctrl <- if (has_battery) {
    battery_options_html <- paste0(
      vapply(names(batteries), function(b) {
        sprintf('<option value="%s">%s</option>',
          htmltools::htmlEscape(b), htmltools::htmlEscape(b))
      }, character(1)),
      collapse = ""
    )
    group_options_html <- if (!is.null(battery_group_ivs) &&
                              length(battery_group_ivs) > 0L) {
      paste0(
        vapply(names(battery_group_ivs), function(g) {
          sprintf('<option value="%s">%s</option>',
            htmltools::htmlEscape(g), htmltools::htmlEscape(g))
        }, character(1)),
        collapse = ""
      )
    } else ""
    paste0(
      '<div class="impact-ctrl-cell">',
        '<div class="impact-ctrl-row">',
          '<label><span class="ndr-tip" data-tip="Filter rows to a battery or group; the index is re-normalised within the visible rows.">Index By:</span></label>',
          '<select class="impact-ctrl" data-dim="indexby">',
            '<option value="All">All</option>',
            battery_options_html,
            group_options_html,
          '</select>',
        '</div>',
      '</div>'
    )
  } else '<div class="impact-ctrl-cell"></div>'

  # Header row: leading cols (sortable, text) + one metric column per
  # subgroup (sortable, numeric). Subgroup label "_" -> " " for display.
  # Battery isn't shown as a column — its info is captured by the Index By
  # dropdown selection rather than a redundant grouping label.
  leading_headers <- c(
    sprintf('<th class="sortable" data-sort="text" data-col="id">%s</th>',
      htmltools::htmlEscape(id_col_label)),
    if (has_community) '<th class="sortable" data-sort="text" data-col="community">Community</th>' else NULL,
    if (has_label)     '<th class="sortable" data-sort="text" data-col="label">Label</th>'         else NULL
  )
  subgroup_headers <- vapply(sgs, function(sg) {
    sprintf(
      '<th class="sg-col sortable metric-col" data-sort="num" data-sg="%s">%s</th>',
      htmltools::htmlEscape(sg),
      htmltools::htmlEscape(gsub("_", " ", sg, fixed = TRUE))
    )
  }, character(1))
  header_row <- paste0("<tr>",
    paste(c(leading_headers, subgroup_headers), collapse = ""),
    "</tr>")

  # Body row template — one <tr> per row; index cells populated by JS.
  # data-col attributes mirror the header so CSS column-width / responsive
  # rules can target both th and td uniformly.
  body_rows <- vapply(seq_along(data_obj$rows_unweighted), function(i) {
    row <- data_obj$rows_unweighted[[i]]
    leading_cells <- c(
      # In the Community Impacts table the id column IS the community
      # name (no separate community/label columns), so tag it with
      # data-orig-comm too — that lets community renames propagate here
      # the same way they do via the dedicated community column in the
      # Attribute Impacts table.
      if (isTRUE(is_community)) sprintf(
        '<td class="txt-col" data-col="id" data-orig-comm="%s">%s</td>',
        htmltools::htmlEscape(row$id),
        htmltools::htmlEscape(row$id)
      ) else sprintf('<td class="txt-col" data-col="id">%s</td>',
        htmltools::htmlEscape(row$id)),
      if (has_community) sprintf(
        '<td class="txt-col" data-col="community" data-orig-comm="%s">%s</td>',
        htmltools::htmlEscape(row$community %||% ""),
        htmltools::htmlEscape(row$community %||% "")) else NULL,
      if (has_label)     sprintf(
        '<td class="txt-col" data-col="label" data-node-id="%s" data-orig-label="%s">%s</td>',
        htmltools::htmlEscape(row$id),
        htmltools::htmlEscape(row$label %||% ""),
        htmltools::htmlEscape(row$label %||% "")) else NULL
    )
    sg_cells <- vapply(sgs, function(sg) {
      sprintf('<td class="idx-cell num-col" data-sg="%s" data-row="%d"></td>',
        htmltools::htmlEscape(sg), i - 1L)
    }, character(1))
    paste0("<tr>", paste(c(leading_cells, sg_cells), collapse = ""), "</tr>")
  }, character(1))

  # Footer rows: Total Impact + Base
  ti_cells <- vapply(sgs, function(sg) {
    sprintf('<td class="ti-cell num-col" data-sg="%s"></td>',
      htmltools::htmlEscape(sg))
  }, character(1))
  base_cells <- vapply(sgs, function(sg) {
    sprintf('<td class="base-cell num-col" data-sg="%s"></td>',
      htmltools::htmlEscape(sg))
  }, character(1))
  n_leading <- length(leading_headers)

  total_row <- paste0(
    '<tr class="ti-row"><td class="txt-col" colspan="', n_leading, '">Total Impact</td>',
    paste(ti_cells, collapse = ""), '</tr>'
  )
  base_row <- paste0(
    '<tr class="base-row"><td class="txt-col" colspan="', n_leading, '">Base</td>',
    paste(base_cells, collapse = ""), '</tr>'
  )

  # Compose. When sharing a payload, the dashboard root carries a
  # `data-impact-data-id` attribute pointing at the shared <script>; the
  # init JS reads that attribute and looks up the JSON once.
  shared_attr <- if (!is.null(shared_data_id)) {
    paste0(' data-impact-data-id="', shared_data_id, '"')
  } else ""
  paste0(
    '<div class="impact-dashboard" data-dashboard-id="', dashboard_id, '"', shared_attr, '>',
    # Card-title row — JS populates with "{Assess value}: <em>{question}</em>"
    # (mirrors the Shiny app card header). Spans the full grid width above
    # the sidebar + table.
    '  <div class="ndr-card-title impact-card-title"></div>',
    # Controls-collapse toggle. Sits in the top-left corner of the
    # dashboard, OUTSIDE the sidebar so it stays visible whether the
    # sidebar is open or collapsed. Click toggles `.controls-collapsed`
    # on the dashboard root; CSS handles the rest. Hidden on narrow
    # viewports (< 1200 px) where the sidebar layout doesn\'t apply.
    '  <button type="button" class="ctrl-toggle" onclick="toggleCtrls(this)" ',
    'aria-label="Toggle controls panel" title="Toggle controls">',
    '<span class="chev">&#9664;</span></button>',
    '  <div class="impact-controls">',
    # Row 1 (framing / scope controls): Assess, Index By, Focus, Outcome Display, Weight.
    # Assess drives Row 2; Index By / Focus filter the table; Outcome
    # Display picks the units; Weight toggles unweighted/weighted view.
    # Assess is only emitted when at least one preset is supported by
    # the data. "Current Impact" is selected by default.
    if (length(preset_map) > 0) {
      assess_names <- names(preset_map)
      assess_options_html <- paste0(
        paste0('<option value="', assess_names, '"',
               ifelse(assess_names == "Current Impact", " selected", ""),
               '>', assess_names, '</option>',
               collapse = ""),
        '<option value="Custom">Custom</option>'
      )
      paste0(
        '    <div class="impact-ctrl-cell">',
          '<div class="impact-ctrl-row">',
            '<label><span class="ndr-tip" data-tip="Preset driver analyses that address specific questions.">Assess:</span></label>',
            '<select class="impact-ctrl" data-dim="assess">', assess_options_html, '</select>',
          '</div>',
          # Question feedback — JS populates with the matching preset\'s
          # question (e.g. "What is happening?"). Empty when on Custom.
          '<span class="impact-warning assess-feedback"></span>',
        '</div>'
      )
    } else "",
    '    ', indexby_ctrl,
    '    <div class="impact-ctrl-cell">',
    '      <div class="impact-ctrl-row">',
    '        <label><span class="ndr-tip" data-tip="&#39;Market&#39; analyzes overall performance, while a brand uses only that brand&#39;s.">Focus:</span></label>',
    '        <select class="impact-ctrl" data-dim="focus">', focus_options_html, '</select>',
    '      </div>',
    '      <span class="impact-warning" data-for="focus"></span>',
    '    </div>',
    '    ', display_ctrl,
    '    ', weight_ctrl,
    # Math controls Assess drives. Marked .assess-driven so they hide
    # when the Assess dropdown is on a preset (Current / Intervention /
    # Maximum Impact) and only reveal on Custom. Inline with the framing
    # controls — auto-fit grid wraps as needed.
    '    <div class="impact-ctrl-cell assess-driven">',
    '      <div class="impact-ctrl-row">',
    '        <label><span class="ndr-tip" data-tip="The metric used to score impact.">Analysis:</span></label>',
    '        <select class="impact-ctrl" data-dim="metric">', metric_options_html, '</select>',
    '      </div>',
    '    </div>',
    '    ', sub('class="impact-ctrl-cell', 'class="impact-ctrl-cell assess-driven', shift_ctrl, fixed = TRUE),
    '  </div>',
    '  <div class="impact-table-wrap">',
    paste0('    <table class="impact-table',
           if (isTRUE(is_community)) ' community-mode' else '',
           '">'),
    '      <thead>', header_row, '</thead>',
    '      <tbody>', paste(body_rows, collapse = ""), '</tbody>',
    '      <tfoot>', total_row, base_row, '</tfoot>',
    '    </table>',
    '  </div>',
    '  <div class="impact-footer">',
    '    <p class="index-note"></p>',
    '    <p class="muted">Bold italicized index means a negative relationship. ',
    'Black cells mean an insignificant relationship (p &gt; 0.10). ',
    'Lift impacts are not calculated when the base is below ', min_base_for_lift, '.</p>',
    '  </div>',
    if (is.null(shared_data_id)) {
      paste0('  <script type="application/json" class="impact-data">', data_json, '</script>')
    } else "",
    '  <script>(function(){ initImpactDashboard("', dashboard_id, '"); })();</script>',
    '</div>'
  ) -> html_out

  # Caller can use the returned `data_json` to emit a single shared
  # <script> at the result level when N type-panel dashboards reuse the
  # same payload. Backward compat: callers that coerce the result to
  # character get the html string (`as.character.list` returns the
  # vector — fall back via `$html` instead in those callsites).
  list(html = html_out, data_json = data_json)
}

# --- internal: full Prioritization dashboard (HTML + inline JS) ------------
# Mirrors the bn_prioritize_write dynamic dashboard: Analysis, Search,
# Subgroup, Focus, Weight dropdowns (only those with multiple values are
# shown); one row per priority step; conditional formatting on p-values
# (green < sig_threshold, orange < marginal_threshold, blackout otherwise)
# and bold-italic for negative marginal gain; Base display + warning next
# to the Focus dropdown.
#' @noRd
.bn_report_render_prioritization_dashboard <- function(
    priort, result_name, dashboard_id,
    sig_threshold = 0.05, marginal_threshold = 0.10,
    add_prioritization_pvalue = FALSE,
    prioritize_display = NULL
) {
  pm <- .bn_report_prio_metadata(priort, add_prioritization_pvalue)
  if (is.null(pm)) {
    return('<div class="extra-empty">No prioritization results.</div>')
  }
  # Unpack metadata into local names so the rest of the function (the
  # rendering layer) reads the same as before the refactor.
  meta                 <- pm$meta
  dims                 <- pm$dims
  active_dims          <- pm$active_dims
  lookup               <- pm$lookup
  has_community        <- pm$has_community
  has_label            <- pm$has_label
  has_p                <- pm$has_p
  min_base_for_boot    <- pm$min_base_for_boot
  lift_pct             <- pm$lift
  lift_label           <- pm$lift_label
  max_label            <- pm$max_label
  max_deprecated_label <- pm$max_deprecated_label
  is_binary_outcome    <- pm$is_binary
  lift_shift_explainer <- pm$lift_shift_explainer
  strategies           <- unlist(dims$strategy)
  searches             <- unlist(dims$search)
  subgroups            <- unlist(dims$subgroup)
  focuses              <- unlist(dims$focus)
  weights              <- unlist(dims$weight)

  data_obj <- list(
    dims              = dims,
    active_dims       = as.list(active_dims),
    has_community     = has_community,
    has_label         = has_label,
    has_p             = has_p,
    sig_threshold     = sig_threshold,
    marginal_threshold = marginal_threshold,
    min_base_for_boot = as.integer(min_base_for_boot),
    lift              = lift_pct,
    lift_label        = lift_label,
    max_label         = max_label,
    max_deprecated_label = max_deprecated_label,
    is_binary         = is_binary_outcome,
    lookup            = lookup
  )

  # digits = NA preserves full numeric precision (default is 4 decimals,
  # which truncates lift values around 0.01 into 0.01 — losing 3+
  # significant digits and producing off-by-one index discrepancies vs
  # the Excel dashboard which reads the full-precision stored values).
  data_json <- jsonlite::toJSON(data_obj, auto_unbox = TRUE,
    null = "null", na = "null", digits = NA)

  # --- HTML scaffold
  dim_labels <- c(
    strategy = "Analysis:",
    search   = "Search:",
    subgroup = "Subgroup:",
    focus    = "Focus:",
    weight   = "Weight:"
  )
  # Tooltip text per dimension — mirrors the Shiny app's control tips.
  dim_tips <- c(
    strategy = "The prioritization strategy used to rank attributes.",
    search   = "The search algorithm used to find the best ordering.",
    subgroup = "Which subgroup the prioritization was computed against.",
    focus    = "&#39;Market&#39; analyzes overall performance, while a brand uses only that brand&#39;s.",
    weight   = "Whether weights are applied when calculating prioritization."
  )

  controls <- character(0)
  for (dn in names(dims)) {
    if (!(dn %in% active_dims)) next
    opts <- dims[[dn]]
    opts_html <- paste(
      vapply(opts, function(o) {
        # Keep `value` as the raw token (so the lookup key matches the
        # registry); pretty-print the visible label by replacing
        # underscores with spaces — e.g., "Regular_Google_User" displays
        # as "Regular Google User".
        display <- if (dn == "subgroup") gsub("_", " ", o, fixed = TRUE) else o
        sprintf('<option value="%s">%s</option>',
          htmltools::htmlEscape(o), htmltools::htmlEscape(display))
      }, character(1)),
      collapse = ""
    )
    warning_html <- if (dn == "focus") {
      '<span class="priort-warning" data-for="focus"></span>'
    } else ""
    controls <- c(controls,
      sprintf(
        paste0(
          '<div class="priort-ctrl-cell">',
            '<div class="priort-ctrl-row">',
              '<label><span class="ndr-tip" data-tip="%s">%s</span></label>',
              '<select class="priort-ctrl" data-dim="%s">%s</select>',
            '</div>',
            '%s',
          '</div>'
        ),
        dim_tips[[dn]] %||% "",
        htmltools::htmlEscape(dim_labels[[dn]]), dn, opts_html, warning_html
      )
    )
  }
  # Chart-mode dropdown (UI-only — controls which numbers feed the chart,
  # doesn't touch the table lookup). Default flips to "Point Change" when
  # the DV is dichotomous (every dv_estimate in [0, 1]) — point changes
  # read more naturally in probability terms than as a % of a probability.
  # User can override via `prioritize_display` ("Point Change" / "% Change").
  resolved_display <- if (!is.null(prioritize_display)) {
    match.arg(prioritize_display, c("% Change", "Point Change"))
  } else if (isTRUE(is_binary_outcome)) {
    "Point Change"
  } else {
    "% Change"
  }
  pct_selected   <- if (resolved_display == "% Change")   " selected" else ""
  point_selected <- if (resolved_display == "Point Change") " selected" else ""
  controls <- c(controls,
    paste0(
      '<div class="priort-ctrl-cell">',
        '<div class="priort-ctrl-row">',
          '<label><span class="ndr-tip" data-tip="How outcome change is displayed &#8212; relative vs absolute point change.">Display:</span></label>',
          '<select class="priort-ctrl" data-dim="chart">',
            '<option value="% Change"', pct_selected, '>% Change</option>',
            '<option value="Point Change"', point_selected, '>Point Change</option>',
          '</select>',
        '</div>',
      '</div>',
      # Show / Hide Outcome Estimate — default hidden (matches the prior
      # bn_report behaviour of omitting the column). The prio init JS
      # wires the click to toggle .ndr-show-estimate on the dashboard
      # root + re-render (so the glossary entry follows).
      '<div class="priort-ctrl-cell">',
        '<div class="priort-ctrl-row">',
          '<label><span class="ndr-tip" data-tip="Predicted outcome value at each step.">Outcome Estimate:</span></label>',
        '</div>',
        '<button type="button" class="ndr-estimate-toggle" data-est-toggle="1">Show</button>',
      '</div>'
    )
  )
  controls_html <- paste(controls, collapse = "")

  # Header columns (same as Excel dashboard)
  # data-mode marks columns that belong to one Display chart mode only:
  # "point" → shown only when Display = Point Change
  # "percent" → shown only when Display = Percent Change
  # No data-mode → always shown.
  # Outcome Estimate is hidden by default (class est-col, CSS display:none)
  # and revealed when the user clicks the Show/Hide control — the prio
  # init JS toggles `.ndr-show-estimate` on the dashboard root, which the
  # stylesheet keys off to reveal the est-col header + cells. It carries
  # no data-mode so it shows in both Point and % Change modes when on.
  headers <- c(
    '<th class="sortable" data-sort="num" data-col="priority">Step</th>',
    '<th class="sortable" data-sort="text" data-col="variable">Variable</th>',
    if (has_community) '<th class="sortable" data-sort="text" data-col="community">Community</th>' else NULL,
    if (has_label)     '<th class="sortable" data-sort="text" data-col="label">Label</th>'         else NULL,
    '<th class="sortable est-col" data-sort="num" data-col="dv_estimate">Outcome Estimate</th>',
    '<th class="sortable" data-sort="num" data-col="cumulative_gain" data-mode="point">Cumulative Gain</th>',
    '<th class="sortable" data-sort="num" data-col="marginal_gain" data-mode="point">Incremental Lift</th>',
    '<th class="sortable" data-sort="num" data-col="cumulative_gain_pct" data-mode="percent">Cumulative Gain %</th>',
    '<th class="sortable" data-sort="num" data-col="marginal_gain_pct" data-mode="percent">Incremental Lift %</th>',
    if (has_p)         '<th class="sortable" data-sort="num" data-col="p_value">p-value</th>'     else NULL
  )
  header_row <- paste0("<tr>", paste(headers, collapse = ""), "</tr>")

  paste0(
    '<div class="priort-dashboard" data-dashboard-id="', dashboard_id, '">',
    # Card-title row — JS populates with "Prioritization: <em>{Analysis}</em>"
    # (mirrors the Shiny app card header).
    '  <div class="ndr-card-title priort-card-title"></div>',
    # Controls-collapse toggle — see impact dashboard above for rationale.
    '  <button type="button" class="ctrl-toggle" onclick="toggleCtrls(this)" ',
    'aria-label="Toggle controls panel" title="Toggle controls">',
    '<span class="chev">&#9664;</span></button>',
    '  <div class="priort-controls">', controls_html, '</div>',
    '  <div class="priort-split">',
    '    <div class="priort-table-wrap">',
    '      <table class="priort-table">',
    '        <thead>', header_row, '</thead>',
    '        <tbody></tbody>',
    '      </table>',
    '    </div>',
    '    <div class="priort-chart-wrap">',
    paste0(
      '      <button type="button" class="priort-download-btn" title="Download chart as PNG">',
      # Image icon — same SVG the network view\'s Download PNG button uses
      # (bn_visNetwork_deliverable_interactivity.R icons.image). Reused
      # here so all "download chart as PNG" affordances in the report look
      # identical.
      '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" viewBox="0 0 512 512" fill="currentColor"><path d="M0 96c0-35.3 28.7-64 64-64h384c35.3 0 64 28.7 64 64v320c0 35.3-28.7 64-64 64H64c-35.3 0-64-28.7-64-64V96zM323.8 202.5c-4.5-6.6-11.9-10.5-19.8-10.5s-15.4 3.9-19.8 10.5l-87 127.6L170.7 297c-4.6-5.7-11.5-9-18.7-9s-14.2 3.3-18.7 9l-64 80c-5.8 7.2-6.9 17.1-2.9 25.4s12.4 13.6 21.6 13.6h96 32H424c8.9 0 17.1-4.9 21.2-12.8s3.6-17.4-1.4-24.7l-120-176zM112 192a48 48 0 1 0 0-96 48 48 0 1 0 0 96z"/></svg>',
      '</button>'
    ),
    '      <svg class="priort-chart" xmlns="http://www.w3.org/2000/svg"></svg>',
    '      <div class="priort-tooltip" style="display:none;"></div>',
    '    </div>',
    '  </div>',
    '  <div class="priort-footer">',
    '    <p class="priort-footer-base"></p>',
    if (has_p) paste0(
      '    <p class="muted">',
      'Green p-values are significant (&lt; ', sig_threshold, '); ',
      'orange are marginal (&lt; ', marginal_threshold, '); ',
      'red are insignificant.</p>'
    ) else "",
    # Glossary rows are tagged with data-gl so updatePriortGlossary() (in
    # the prio init JS) can show only the entries relevant to the current
    # Display mode + selected Analysis:
    #   data-gl="always"   -> always shown
    #   data-gl="point"    -> shown in Point Change mode only
    #   data-gl="pct"      -> shown in % Change mode only
    #   data-gl="estimate" -> shown only when the Outcome Estimate column is on
    #   data-gl="strategy" data-gl-strat="<name>" -> shown only when that
    #                       strategy is the selected Analysis
    '    <div class="priort-glossary muted">',
    '      <p data-gl="always">Step &mdash; Priority step number (order in which attributes were selected).</p>',
    '      <p data-gl="estimate">Outcome Estimate &mdash; Expected outcome value with all selected attributes shifted.</p>',
    '      <p data-gl="point">Cumulative Gain &mdash; Absolute increase in outcome estimate from baseline (all attributes through this step).</p>',
    '      <p data-gl="point">Incremental Lift &mdash; Absolute increase in outcome estimate from adding this attribute.</p>',
    '      <p data-gl="pct">Cumulative Gain % &mdash; Percentage increase from baseline through this step.</p>',
    '      <p data-gl="pct">Incremental Lift % &mdash; Percentage increase relative to the previous step.</p>',
    if (has_p) paste0(
      '      <p data-gl="always">p-value &mdash; Noise-floor test: proportion of bootstraps where this step’s gain &le; the noise floor.</p>'
    ) else "",
    paste0('      <p data-gl="strategy" data-gl-strat="', htmltools::htmlEscape(lift_label), '">',
           htmltools::htmlEscape(lift_label), ' &mdash; ', htmltools::htmlEscape(lift_shift_explainer), '.</p>'),
    paste0('      <p data-gl="strategy" data-gl-strat="', htmltools::htmlEscape(max_label), '">',
           htmltools::htmlEscape(max_label), ' &mdash; Sets each attribute to its highest level as hard evidence, representing the theoretical ceiling.</p>'),
    # The deprecated strategy is only present when bn_prioritizations() was
    # called with include_maximum_lift_deprecated = TRUE; check the dims
    # payload to know whether to surface the explainer.
    if (max_deprecated_label %in% strategies) paste0(
      '      <p data-gl="strategy" data-gl-strat="', htmltools::htmlEscape(max_deprecated_label), '">',
      htmltools::htmlEscape(max_deprecated_label), ' &mdash; Same as Maximum Lift but cumulative gain is the raw outcome estimate (no comparison to baseline). Provided for backward compatibility.</p>'
    ) else "",
    '    </div>',
    '  </div>',
    '  <script type="application/json" class="priort-data">', data_json, '</script>',
    '  <script>(function(){ initPriortDashboard("', dashboard_id, '"); })();</script>',
    '</div>'
  )
}

# --- internal: report CSS ---
#' @noRd
.bn_report_css <- function() {
  paste(c(
    # Brand layer first: Inter @import (must lead the stylesheet) + both
    # colour-mode :root token blocks + shared tooltip / disabled / focus /
    # conditional-format classes. bn_report's own rules below all reference
    # var(--ndr-*), so they resolve from here.
    resondex_css(include_import = TRUE),
    'body {',
    '  font-family: var(--ndr-font);',
    '  margin: 20px 40px;',
    '  background: #fafafa;',
    '}',
    'h1 { margin: 0; }',
    '.subtitle {',
    '  margin: 2px 0 0 0;',
    '  font-size: 13px;',
    '  font-weight: 400;',
    '  color: #666;',
    '}',
    '.page-header {',
    '  border-bottom: 2px solid #333;',
    '  padding-bottom: 4px;',
    '  display: flex;',
    '  align-items: flex-end;',
    '  justify-content: space-between;',
    '}',
    '.header-actions {',
    '  display: flex;',
    '  gap: 8px;',
    '  flex-shrink: 0;',
    '}',
    '',
    '/* accordion */',
    '.result-accordion {',
    '  margin: 8px 0;',
    '  border: 1px solid #ddd;',
    '  border-radius: 8px;',
    '  background: #fff;',
    '  overflow: hidden;',
    '}',
    '.result-accordion summary {',
    '  padding: 14px 20px;',
    '  font-size: 18px;',
    '  font-weight: 700;',
    '  color: #333;',
    '  cursor: pointer;',
    '  user-select: none;',
    '  background: #f7f7f7;',
    '  border-bottom: 1px solid #eee;',
    '  display: flex; align-items: center; gap: 12px;',
    '}',
    '.result-accordion summary:hover { background: #f0f0f0; }',
    '/* Title takes the leftover space so the download button hugs the right. */',
    '.accordion-title { flex: 1 1 auto; min-width: 0; }',
    '/* Download button sits on the right, normal-weight, smaller. */',
    '.accordion-download-btn {',
    '  flex: 0 0 auto;',
    '  font-size: 13px; font-weight: 500;',
    '}',
    '.accordion-body { padding: 0; }',
    '',
    '/* Wrapper for the Attribute / Community network tabs — matches the 20px',
    '   outer padding used by .impact-dashboard / .priort-dashboard /',
    '   .membership-wrap so the controls box has identical spacing on every tab. */',
    # 8px top + 20px sides/bottom — tighter gap between the tab-bar
    # and the network canvas while keeping horizontal breathing room
    # consistent with .impact-dashboard / .priort-dashboard.
    '.network-dashboard { padding: 8px 20px 20px 20px; }',
    '/* Layout dropdown — label stacked on top, select left-aligned with the',
    '   in-iframe "Select by ID" below it. NOT a well/card. */',
    '.layout-controls {',
    '  display: flex; flex-direction: column; align-items: flex-start;',
    '  gap: 4px; margin: 0 0 8px 0;',
    # left:10px matches the in-iframe Select-by-ID + legend offset
    '  padding: 0 0 0 10px;',
    '  background: transparent; border: none; border-radius: 0;',
    '}',
    '.layout-controls label {',
    '  font-weight: 600; color: var(--ndr-text); font-size: 13px;',
    '}',
    '.layout-select {',
    '  padding: 4px 8px; font-size: 13px;',
    # 130px = visNetwork btnW: matches Select-by-ID + legend min-width
    '  width: 130px; box-sizing: border-box;',
    '  border: 1px solid #bbb; border-radius: 3px; background: #fff;',
    '  cursor: pointer;',
    '}',
    '.report-btn {',
    '  padding: 6px 12px;',
    '  font-size: 13px;',
    '  border: 1px solid #ccc;',
    '  border-radius: 4px;',
    '  background: #fff;',
    '  color: #333;',
    '  cursor: pointer;',
    '}',
    '.report-btn:hover { background: #f0f0f0; }',
    '',
    '/* type panels */',
    '.type-panel { padding: 0; }',
    '',
    '/* tabs */',
    '.tab-bar {',
    '  display: flex;',
    '  border-bottom: 2px solid #ddd;',
    '  background: #f9f9f9;',
    '}',
    '.tab-btn {',
    '  padding: 10px 24px;',
    '  border: none;',
    '  background: transparent;',
    '  font-size: 14px;',
    '  font-weight: 500;',
    '  color: #888;',
    '  cursor: pointer;',
    '  border-bottom: 2px solid transparent;',
    '  margin-bottom: -2px;',
    '  transition: color 0.15s, border-color 0.15s;',
    '}',
    '.tab-btn:hover { color: #444; }',
    '.tab-btn.active {',
    '  color: #222;',
    '  border-bottom-color: #333;',
    '}',
    '.tab-panel { display: none; padding: 0; }',
    '.tab-panel iframe { display: block; }',
    '.tab-panel.active { display: block; }',
    '',
    '/* membership tab */',
    '.membership-wrap { padding: 8px 20px 20px 20px; }',
    '.membership-toolbar {',
    '  display: flex;',
    '  justify-content: flex-end;',
    '  margin-bottom: 12px;',
    '}',
    # var(--ndr-fs-sm) = 12px — matches the app's brand button sizing.
    # Note: was 13px (matching table body) but report-side controls now
    # consistently use the smaller fs-sm token for parity with the app.
    '.membership-toggle { font-size: var(--ndr-fs-sm, 12px); padding: 4px 10px; }',
    '.membership-table {',
    '  width: 100%;',
    '  border-collapse: collapse;',
    '  font-size: 13px;',
    '}',
    '.membership-table th {',
    '  text-align: left;',
    '  padding: 10px 12px;',
    '  border-bottom: 2px solid #ddd;',
    '  font-weight: 600;',
    '  color: #555;',
    '}',
    '.membership-table th:first-child,',
    '.membership-table td:first-child {',
    '  min-width: 125px;',
    '  max-width: 150px;',
    '}',
    '.membership-table td {',
    '  padding: 10px 12px;',
    '  border-bottom: 1px solid #eee;',
    '  vertical-align: top;',
    '}',
    '.membership-table tr:hover { background: #f8f8f8; }',
    '.membership-dot {',
    '  display: inline-block;',
    '  width: 12px;',
    '  height: 12px;',
    '  border-radius: 50%;',
    '  margin-right: 8px;',
    '  vertical-align: middle;',
    '}',
    '.membership-cards {',
    '  display: grid;',
    '  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));',
    '  gap: 16px;',
    '}',
    # Membership card surface — brand-tokenized so light AND dark modes
    # both work in the report and the app. Previously hardcoded #fff /
    # #e0e0e0 which left the cards stark white against a dark page in
    # dark mode. var(--ndr-card-bg) matches the reactable table surface,
    # so cards and the table view feel like the same component.
    '.membership-card {',
    '  background: var(--ndr-card-bg);',
    '  border: 1px solid var(--ndr-border);',
    '  border-radius: var(--ndr-radius, 8px);',
    '  padding: 16px;',
    '  box-shadow: var(--ndr-shadow);',
    '}',
    # The membership card's inner header div uses class `.mc-header`
    # (NOT `.card-header`) to avoid colliding with bslib's generic
    # .card-header brand styling. Color reads from --ndr-text so dark
    # mode tracks automatically.
    '.membership-card .mc-header {',
    '  font-weight: 600;',
    '  font-size: 15px;',
    '  color: var(--ndr-text);',
    '  display: flex;',
    '  align-items: center;',
    '  margin-bottom: 12px;',
    '}',
    # `.card-count` is the small pill chip showing attribute count next
    # to a community name. Used in BOTH views: card view (inside a
    # `.membership-card`) AND table view (inline beside the community
    # name in the first `<td>`). The previous `.membership-card`
    # ancestor scoped this rule to the card view only, leaving the
    # table view's count rendering as bare unformatted text. Drop the
    # ancestor so the pill applies wherever `.card-count` appears.
    '.card-count {',
    '  margin-left: 8px;',
    '  font-size: 12px;',
    '  font-weight: 500;',
    '  color: var(--ndr-muted);',
    '  background: var(--ndr-secondary-bg);',
    '  padding: 2px 8px;',
    '  border-radius: 10px;',
    # `display: inline-block` so vertical padding renders correctly in
    # both contexts. Inline alone (the default) ignores top/bottom
    # padding in table cells, which was making the table-view chip
    # look squashed compared to the card-view chip.
    '  display: inline-block;',
    '  vertical-align: middle;',
    '  line-height: 1.4;',
    '}',
    '.membership-card .card-nodes { display: flex; flex-wrap: wrap; gap: 6px; }',
    '.node-pill {',
    '  display: inline-block;',
    '  padding: 4px 10px;',
    '  background: var(--ndr-secondary-bg);',
    '  border-radius: 12px;',
    '  font-size: 13px;',
    '  color: var(--ndr-text);',
    '}',
    '',
    '/* extra tabs (impacts / prioritization) — styling matches bn_write */',
    '.extra-wrap { padding: 20px; overflow-x: auto; }',
    '.extra-empty { padding: 40px; text-align: center; color: #999; }',
    '.extra-section { margin-bottom: 24px; }',
    '.extra-section-title {',
    '  margin: 0 0 8px 0;',
    '  font-size: 16px;',
    '  font-weight: 600;',
    '  color: #333;',
    '}',
    '.extra-table {',
    '  width: 100%;',
    '  border-collapse: collapse;',
    '  font-size: 13px;',
    '  background: #fff;',
    '}',
    '.extra-table thead th {',
    '  background: #D9D9D9;',
    '  color: #222;',
    '  font-weight: 700;',
    '  padding: 8px 10px;',
    '  text-align: center;',
    '  border: 1px solid #BFBFBF;',
    '}',
    '.extra-table tbody td {',
    '  padding: 6px 10px;',
    '  border: 1px solid #e0e0e0;',
    '  vertical-align: middle;',
    '}',
    '.extra-table tbody td.num-col { text-align: center; }',
    '.extra-table tbody td.txt-col { text-align: left; }',
    '.extra-table tbody tr:hover { background: #f8f8f8; }',
    '/* p-value colours: shared .rdx-pval-* from resondex_css() */',
    '',
    '/* Attribute Impacts dashboard (mirrors bn_impact_write dynamic) */',
    '.impact-dashboard { padding: 8px 20px 20px 20px; overflow-x: auto; }',
    # Layout: responsive grid. auto-fit + minmax(280px, 1fr) means the grid
    # fits as many 280px-minimum columns as will fit the container — so at
    # full desktop width you get 3 columns, narrower viewports collapse to
    # 2, and mobile collapses to 1. Gutters stay equal between whatever
    # columns remain. box-sizing keeps padding inside the declared width.
    '.impact-controls {',
    '  display: grid; grid-template-columns: repeat(auto-fit, minmax(330px, 1fr));',
    '  column-gap: 24px; row-gap: 10px; align-items: start;',
    '  box-sizing: border-box; width: 100%;',
    '  margin-bottom: 16px; padding: 12px;',
    '  background: #fafafa; border: 1px solid #e0e0e0; border-radius: 6px;',
    '}',
    # When Assess is on a preset (Current / Intervention / Maximum
    # Impact), the underlying controls it drives (Analysis + Shift Type)
    # hide. They reappear on Custom so the user can tune freely. JS
    # toggles `.assess-preset` on the dashboard root.
    '.impact-dashboard.assess-preset .impact-ctrl-cell.assess-driven { display: none; }',
    # Assess feedback question — black italic, sits under the Assess
    # dropdown. Overrides the parent .impact-warning red and the
    # .warn-grey grey so the question reads as informational, not a
    # warning. Italic is kept to differentiate from regular body text.
    '.impact-warning.assess-feedback {',
    '  display: block; padding-left: 108px;',  # match label gutter (100 + 8)
    '  margin-top: 2px; line-height: 1.3;',
    '  color: #000; font-weight: 400; font-style: italic;',
    '}',
    # Each cell stacks: (row 1) label + select inline, (row 2) warning.
    # min-width: 0 is required so the cell can shrink below its content's
    # natural width — otherwise long labels would force the column wider
    # than 1fr and push siblings out of the grey container.
    '.impact-ctrl-cell {',
    '  display: flex; flex-direction: column; gap: 4px;',
    '  min-width: 0;',
    '}',
    # Drop empty placeholder cells (rendered when a control is unavailable)
    # so responsive packing doesn't leave awkward gaps.
    '.impact-ctrl-cell:empty { display: none; }',
    '.impact-ctrl-row {',
    '  display: flex; align-items: center; gap: 8px; min-width: 0;',
    '}',
    '.impact-ctrl-row label {',
    '  font-weight: 600; color: #333; font-size: 13px; white-space: nowrap;',
    '  flex: 0 0 100px; text-align: right;',
    '}',
    '.impact-ctrl-row .impact-ctrl { flex: 1 1 auto; min-width: 0; max-width: 200px; }',
    # Warning sits below the ctrl-row so its text can wrap to multiple
    # lines without shoving the cell wider.
    '.impact-warning {',
    '  display: block; padding-left: 108px;',  # 100px label + 8px gap
    '  white-space: normal; overflow-wrap: anywhere; line-height: 1.3;',
    '}',
    '.impact-ctrl {',
    '  padding: 4px 8px; font-size: 13px; width: 180px;',
    '  border: 1px solid #bbb; border-radius: 3px; background: #fff;',
    '  transition: background 0.15s, color 0.15s, border-color 0.15s;',
    '}',
    '.impact-ctrl.warn {',
    '  background: #FF0000; color: #FFFFFF; border-color: #FF0000;',
    '}',
    '.impact-warning {',
    '  color: #FF0000; font-weight: 700; font-size: 13px;',
    '}',
    '/* Grey italic variant for advisory notes (not errors) — used for the',
    '   Weight/Focus "metric invariant to this control" notes. Overrides',
    '   the red base style above. */',
    '.impact-warning.warn-grey {',
    '  color: #888888; font-weight: 400; font-style: italic;',
    '}',
    '.impact-table-wrap { overflow-x: auto; }',
    '.impact-table {',
    '  width: 100%; border-collapse: collapse; font-size: 13px;',
    '  background: #fff; table-layout: auto;',
    '}',
    '.impact-table thead th {',
    '  background: #fafafa; color: #222; font-weight: 700;',  # match .impact-controls bg
    '  padding: 8px 10px; text-align: center; vertical-align: middle;',
    '  border: 1px solid #BFBFBF;',
    '  white-space: normal; word-wrap: break-word; overflow-wrap: break-word;',
    '  position: relative;',
    '}',
    '.col-resize-handle {',
    '  position: absolute; top: 0; right: 0;',
    '  width: 6px; height: 100%;',
    '  cursor: col-resize; user-select: none; z-index: 1;',
    '}',
    '.col-resize-handle:hover { background: rgba(0,0,0,0.15); }',
    '.impact-table.resizing { cursor: col-resize; user-select: none; }',
    '.impact-table.resizing * { cursor: col-resize !important; user-select: none !important; }',
    # Per-column min/max widths — replaces the previous fixed-100px metric
    # rule and adds sensible bands for the leading text columns. data-col
    # attributes are emitted on both th and td so the rules apply to all
    # cells. Long Label / Community text wraps within the max width
    # instead of dragging the column out wider.
    '.impact-table th[data-col="id"], .impact-table td[data-col="id"] {',
    '  min-width: 80px; max-width: 140px;',
    '}',
    # In community mode the id column carries community names ("Effortless
    # Interaction") rather than short IV IDs ("func_7"), so let it grow.
    '.impact-table.community-mode th[data-col="id"],',
    '.impact-table.community-mode td[data-col="id"] {',
    '  min-width: 160px; max-width: 280px;',
    '  white-space: normal; word-wrap: break-word;',
    '}',
    '.impact-table th[data-col="community"], .impact-table td[data-col="community"] {',
    '  min-width: 120px; max-width: 200px;',
    '  overflow: hidden; text-overflow: ellipsis;',
    '}',
    '.impact-table th[data-col="label"], .impact-table td[data-col="label"] {',
    '  min-width: 200px; max-width: 380px;',
    '  white-space: normal; word-wrap: break-word; overflow-wrap: break-word;',
    '}',
    '.impact-table thead th.metric-col, .impact-table tbody td.idx-cell {',
    '  min-width: 70px; max-width: 110px;',
    '}',
    # Tier-2 responsive breakpoints. Below 1100px (typical tablet
    # landscape) hide the Community column to free width. Below 768px
    # (tablet portrait / mobile) also hide the Label column — Variable
    # plus the metric columns are the minimum useful set. Above 1400px
    # let the Label expand and metric columns grow modestly.
    '@media (max-width: 1100px) {',
    '  .impact-table th[data-col="community"], .impact-table td[data-col="community"] { display: none; }',
    '}',
    '@media (max-width: 768px) {',
    '  .impact-table th[data-col="label"], .impact-table td[data-col="label"] { display: none; }',
    '}',
    '@media (min-width: 1400px) {',
    '  .impact-table th[data-col="label"], .impact-table td[data-col="label"] {',
    '    max-width: 480px;',
    '  }',
    '  .impact-table thead th.metric-col, .impact-table tbody td.idx-cell {',
    '    max-width: 130px;',
    '  }',
    '}',
    '.impact-table thead th.sortable {',
    '  cursor: pointer; user-select: none; position: relative;',
    '  padding-right: 18px;',  # room for the absolutely-positioned triangle
    '}',
    '.impact-table thead th.sortable:hover { background: #ececec; }',  # subtle hover, slightly darker than #fafafa
    '.impact-table thead th.sortable::after {',
    '  content: ""; position: absolute; right: 4px; top: 50%;',
    '  transform: translateY(-50%);',
    '  border: 4px solid transparent; opacity: 0.3;',
    '}',
    '.impact-table thead th.sortable.sorted-asc::after {',
    '  border-bottom-color: #333; border-top: 0; opacity: 1;',
    '  transform: translateY(-50%) translateY(-2px);',
    '}',
    '.impact-table thead th.sortable.sorted-desc::after {',
    '  border-top-color: #333; border-bottom: 0; opacity: 1;',
    '  transform: translateY(-50%) translateY(2px);',
    '}',
    '.impact-table tbody td {',
    # 4px 6px — matches reactable `compact = TRUE` spacing so the
    # standalone bn_report HTML reads with the same row density as
    # the in-app reactable. Header (thead) keeps 8px 10px so column
    # labels stay visually distinct from data rows.
    '  padding: 4px 6px; border: 1px solid #e0e0e0; vertical-align: middle;',
    '}',
    '.impact-table tbody td.num-col { text-align: center; font-variant-numeric: tabular-nums; }',
    '.impact-table tbody td.txt-col { text-align: left; }',
    '/* negative / insignificant: shared .rdx-neg / .rdx-insig (resondex_css) */',
    '.impact-table tfoot td {',
    '  padding: 8px 10px; border: 1px solid #BFBFBF;',
    '  background: #f5f5f5; font-weight: 600;',
    '}',
    '.impact-table tfoot td.num-col { text-align: center; }',
    '.impact-table tfoot tr.ti-row td { border-top: 2px solid #333; }',
    '.impact-table tfoot tr.base-row td {',
    '  color: #595959; font-weight: 400;',
    '}',
    # .impact-footer outer styling (padding / font-size / color) is in
    # resondex_css()'s table_footer_notes block — single source of truth
    # shared with the network-drivers app. Inner-paragraph styling stays
    # here because the inner classes are bn_report-specific markup.
    '.impact-footer .index-note { margin: 0 0 4px 0; font-style: italic; }',
    '.impact-footer .muted { margin: 0; color: var(--ndr-muted); }',
    '',
    '/* Prioritization dashboard (mirrors bn_prioritize_write) */',
    '.priort-dashboard { padding: 8px 20px 20px 20px; overflow-x: auto; }',
    # Layout: same responsive grid as .impact-controls — auto-fit + minmax
    # collapses 3 → 2 → 1 column based on viewport width, with consistent
    # gutters between whatever columns remain.
    '.priort-controls {',
    '  display: grid; grid-template-columns: repeat(auto-fit, minmax(330px, 1fr));',
    '  column-gap: 24px; row-gap: 10px; align-items: start;',
    '  box-sizing: border-box; width: 100%;',
    '  margin-bottom: 16px; padding: 12px;',
    '  background: #fafafa; border: 1px solid #e0e0e0; border-radius: 6px;',
    '}',
    # Each cell stacks: (row 1) label + select inline, (row 2) warning.
    '.priort-ctrl-cell {',
    '  display: flex; flex-direction: column; gap: 4px;',
    '  min-width: 0;',
    '}',
    '.priort-ctrl-cell:empty { display: none; }',
    '.priort-ctrl-row {',
    '  display: flex; align-items: center; gap: 8px; min-width: 0;',
    '}',
    '.priort-ctrl-row label {',
    '  font-weight: 600; color: #333; font-size: 13px; white-space: nowrap;',
    '  flex: 0 0 100px; text-align: right;',
    '}',
    '.priort-ctrl-row .priort-ctrl { flex: 1 1 auto; min-width: 0; max-width: 200px; }',
    '.priort-ctrl {',
    '  padding: 4px 8px; font-size: 13px; width: 180px;',
    '  border: 1px solid #bbb; border-radius: 3px; background: #fff;',
    '  transition: background 0.15s, color 0.15s, border-color 0.15s;',
    '}',
    '.priort-ctrl.warn {',
    '  background: #FF0000; color: #FFFFFF; border-color: #FF0000;',
    '}',
    # Warning sits below the ctrl-row so its text can wrap to multiple lines
    # without shoving the cell wider.
    '.priort-warning {',
    '  display: block; padding-left: 108px;',  # 100px label + 8px gap
    '  color: #FF0000; font-weight: 700; font-size: 13px;',
    '  white-space: normal; overflow-wrap: anywhere; line-height: 1.3;',
    '}',
    '.priort-split {',
    '  display: flex; flex-direction: row; gap: 16px;',
    '  align-items: stretch;',
    '}',
    '/* Column mode is driven entirely by JS (see checkOverflow in priort init).',
    '   It triggers when the viewport is narrow OR when the table\\u2019s natural',
    '   width would overflow its row-mode container — preventing any visual',
    '   overlap between the table and chart. */',
    '.priort-split.force-column { flex-direction: column; }',
    '.priort-split.force-column .priort-table-wrap,',
    '.priort-split.force-column .priort-chart-wrap {',
    '  flex: 0 0 auto; width: 100%;',
    '  box-sizing: border-box;',
    '}',
    '/* In row mode, align-items: stretch made the table inherit the chart',
    '   height; column mode loses that, so a min-height keeps the box balanced',
    '   when there are only a few data rows. */',
    '.priort-split.force-column .priort-table-wrap { min-height: 452px; }',
    '.priort-split.force-column .priort-chart { height: 420px; }',
    '.priort-card {',
    '  background: #fff; border: 1px solid #d8d8d8; border-radius: 8px;',
    '  padding: 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.04);',
    '}',
    '.priort-table-wrap {',
    '  flex: 1 1 0; min-width: 0; overflow-x: auto;',
    '  background: #fff; border: 1px solid #d8d8d8; border-radius: 8px;',
    '  padding: 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.04);',
    '}',
    '.priort-chart-wrap {',
    '  flex: 1 1 0; min-width: 0; position: relative;',
    '  background: #fff; border: 1px solid #d8d8d8; border-radius: 8px;',
    '  padding: 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.04);',
    '}',
    # Download-PNG affordance — visually matches the network view\'s vis-button
    # (bn_visNetwork_deliverable_interactivity.R btnStyle) but icon-only:
    # square, same border / radius / hover, anchored to the chart card
    # top-right.
    '.priort-download-btn {',
    '  position: absolute; top: 10px; right: 10px; z-index: 2;',
    '  width: 34px; height: 34px; padding: 0;',
    '  background: transparent; color: var(--ndr-text);',
    '  border: 1px solid var(--ndr-border); border-radius: 6px;',
    '  cursor: pointer; box-sizing: border-box;',
    '  display: flex; align-items: center; justify-content: center;',
    '  flex-shrink: 0;',
    '}',
    '.priort-download-btn:hover { background: var(--ndr-secondary-bg); border-color: var(--ndr-border); }',
    '.priort-download-btn:active { background: color-mix(in srgb, var(--ndr-secondary-bg) 80%, black); }',
    # Inline SVG icon — inherits currentColor and stays vertically aligned.
    '.priort-download-btn svg { display: block; }',
    '.priort-tooltip,',
    '.impact-tooltip {',
    '  position: fixed; pointer-events: none; z-index: 1000;',
    '  background: rgba(0, 0, 0, 0.55); color: #fff;',
    '  padding: 8px 12px; border-radius: 6px;',
    '  font-size: 12px; line-height: 1.45;',
    '  box-shadow: 0 4px 12px rgba(0,0,0,0.15);',
    '  max-width: 300px; white-space: pre-line;',
    '  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;',
    '  backdrop-filter: blur(2px);',
    '}',
    '.priort-chart { width: 100%; height: 480px; display: block; }',
    '.priort-chart .ax-line { stroke: #888; stroke-width: 1; }',
    '.priort-chart .grid-line { stroke: #e0e0e0; stroke-width: 1; }',
    '.priort-chart .ax-text { fill: #555; font-size: 11px; }',
    # matches bn_prioritize_write colours: light grey base (#D9D9D9), dark grey incremental (#595959)
    '.priort-chart .bar-prev { fill: #D9D9D9; }',
    '.priort-chart .bar-incr { fill: #595959; }',
    '.priort-chart .cum-line { stroke: #595959; stroke-width: 2; fill: none; }',
    '.priort-chart .cum-marker { fill: #595959; stroke: #595959; }',
    '.priort-chart .bar-label { fill: #333; font-size: 11px; text-anchor: middle; }',
    '.priort-chart .x-label { fill: #333; font-size: 11px; }',
    '.priort-table {',
    '  width: 100%; border-collapse: collapse; font-size: 13px;',
    '  background: #fff; table-layout: auto;',
    '}',
    '.priort-table thead th {',
    '  background: #fafafa; color: #222; font-weight: 700;',  # match .priort-controls bg
    '  padding: 8px 10px; text-align: center; vertical-align: middle;',
    '  border: 1px solid #BFBFBF;',
    '  white-space: normal; word-wrap: break-word; overflow-wrap: break-word;',
    '  position: relative;',
    '}',
    '.priort-table thead th.sortable {',
    '  cursor: pointer; user-select: none; position: relative;',
    '  padding-right: 18px;',  # room for the absolutely-positioned triangle
    '}',
    '.priort-table thead th.sortable:hover { background: #ececec; }',  # subtle hover, slightly darker than #fafafa
    '.priort-table thead th.sortable::after {',
    '  content: ""; position: absolute; right: 4px; top: 50%;',
    '  transform: translateY(-50%);',
    '  border: 4px solid transparent; opacity: 0.3;',
    '}',
    '.priort-table thead th.sortable.sorted-asc::after {',
    '  border-bottom-color: #333; border-top: 0; opacity: 1;',
    '  transform: translateY(-50%) translateY(-2px);',
    '}',
    '.priort-table thead th.sortable.sorted-desc::after {',
    '  border-top-color: #333; border-bottom: 0; opacity: 1;',
    '  transform: translateY(-50%) translateY(2px);',
    '}',
    '.priort-table tbody td {',
    # 4px 6px — parity with .impact-table tbody and the in-app
    # reactable's compact spacing. Header keeps 8px 10px.
    '  padding: 4px 6px; border: 1px solid #e0e0e0; vertical-align: middle;',
    '}',
    '.priort-table tbody td.num-col {',
    '  text-align: center; font-variant-numeric: tabular-nums;',
    '}',
    '.priort-table tbody td.txt-col { text-align: left; }',
    '/* p-value colours: shared .rdx-pval-* from resondex_css() */',
    '.priort-table tfoot td {',
    '  padding: 8px 10px; border: 1px solid #BFBFBF;',
    '  background: #f5f5f5; font-weight: 600; text-align: center;',
    '  color: #595959;',
    '}',
    # .priort-footer outer styling (padding / font-size / color) is in
    # resondex_css()'s table_footer_notes block — single source of
    # truth shared with the network-drivers app. Inner-paragraph
    # styling stays here because the inner classes are bn_report
    # markup.
    '.priort-footer .muted { margin: 0; }',
    '.priort-footer .priort-footer-base {',
    '  margin: 0 0 4px 0; font-weight: 700; color: var(--ndr-text);',
    '}',
    # Hide the off-mode metric columns. The Display dropdown adds
    # mode-percent / mode-point on the OUTER tab-panel wrapper (.priort-
    # panel, the element initPriortDashboard\'s `root` actually points at —
    # the .priort-dashboard div inside doesn\'t carry the id), so the
    # selectors anchor on .priort-panel.
    '.priort-panel.mode-percent .priort-table th[data-mode="point"],',
    '.priort-panel.mode-percent .priort-table td[data-mode="point"] { display: none; }',
    '.priort-panel.mode-point   .priort-table th[data-mode="percent"],',
    '.priort-panel.mode-point   .priort-table td[data-mode="percent"] { display: none; }',
    '.priort-glossary { margin: 8px 0 0 0; line-height: 1.5; }',
    '.priort-glossary p { margin: 0 0 2px 0; }',
    '.priort-glossary b { color: #222; }',
    '',
    '/* loading spinner */',
    '.iframe-wrap { position: relative; }',
    '.spinner-overlay {',
    '  position: absolute;',
    '  top: 0; left: 0; right: 0; bottom: 0;',
    '  display: flex;',
    '  align-items: center;',
    '  justify-content: center;',
    '  background: #fff;',
    '  z-index: 1;',
    '}',
    '.spinner {',
    '  display: flex;',
    '  gap: 16px;',
    '  align-items: flex-end;',
    '  height: 96px;',
    '}',
    '.spinner-bar {',
    '  width: 16px;',
    '  background: #333;',
    '  border-radius: 6px;',
    '  animation: bars 0.8s ease-in-out infinite;',
    '}',
    '.spinner-bar:nth-child(1) { height: 48px; animation-delay: 0s; }',
    '.spinner-bar:nth-child(2) { height: 72px; animation-delay: 0.15s; }',
    '.spinner-bar:nth-child(3) { height: 56px; animation-delay: 0.3s; }',
    '@keyframes bars {',
    '  0%, 100% { transform: scaleY(0.4); opacity: 0.3; }',
    '  50% { transform: scaleY(1); opacity: 1; }',
    '}',
    '',
    '/* =====================================================================',
    '   bn_report app-look override layer. Appended last so it wins on',
    '   equal specificity. Goal: make the static report read like the',
    '   bslib + reactable Shiny app (app_deliverable_network_drivers).',
    '   ===================================================================== */',
    '/* :root tokens now come from resondex_css() (single source, both',
    '   colour modes); the bn_report-specific overrides continue below. */',
    'body {',
    # Page background now comes from resondex_css() (shared layer) so every
    # app_deliverable surface gets it, not only ones that include bn_report.
    # The 18/28px margin is the report's own page frame and stays here.
    '  color: var(--ndr-text);',
    '  margin: 18px 28px !important;',
    '}',
    '.page-header {',
    '  border-bottom: 1px solid var(--ndr-border) !important;',
    '  padding-bottom: 6px !important;',
    '}',
    'h1 { font-size: 16px; font-weight: 600; letter-spacing: -0.01em; }',
    '.subtitle { color: var(--ndr-muted); }',
    '',
    '/* ---- Accordion sections read as bslib cards ---- */',
    '.result-accordion {',
    '  border: 1px solid var(--ndr-border) !important;',
    '  border-radius: var(--ndr-radius) !important;',
    '  box-shadow: var(--ndr-shadow) !important;',
    '  background: var(--ndr-card-bg) !important;',
    '}',
    '.result-accordion summary {',
    '  background: var(--ndr-header-bg) !important;',
    '  border-bottom: 1px solid var(--ndr-border) !important;',
    '  font-size: 15px !important;',
    '  font-weight: 600 !important;',
    '  color: var(--ndr-text) !important;',
    '  padding: 12px 16px !important;',
    '}',
    '.result-accordion summary:hover { background: var(--bs-tertiary-bg, #f1f3f5) !important; }',
    '',
    '/* ---- Tab bar → navset_underline look ---- */',
    '.tab-bar {',
    '  background: transparent !important;',
    '  border-bottom: 1px solid var(--ndr-border) !important;',
    '  padding: 0 12px !important;',
    '}',
    '.tab-btn {',
    '  font-size: 14px !important;',
    '  font-weight: 500 !important;',
    '  color: var(--ndr-muted) !important;',
    '  padding: 10px 16px !important;',
    '}',
    '.tab-btn:hover { color: var(--ndr-text) !important; }',
    '.tab-btn.active {',
    '  color: var(--ndr-text) !important;',
    '  border-bottom-color: var(--ndr-text) !important;',
    '}',
    '',
    '/* ---- Impact + Prio dashboards: controls become a left sidebar ---- */',
    '/* DOM order is controls / table(or split) / footer as siblings inside',
    '   .impact-dashboard | .priort-dashboard. CSS grid pins controls to a',
    '   fixed-width left column spanning both rows; the table and footer',
    '   stack in the right column. No HTML restructuring needed. */',
    '@media (min-width: 1200px) {',
    # .network-dashboard joins .impact-dashboard / .priort-dashboard in
    # the sidebar-layout grid so the network tab\'s Layout dropdown can
    # live inside a matching collapsible well panel.
    '  .impact-dashboard, .priort-dashboard, .network-dashboard {',
    '    display: grid !important;',
    # 186px ~= 248 * 0.75 — tightened well panel. Inner content area
    # (after the panel\'s 14px padding) is ~158px, enough for the
    # 12px-font controls without wrapping.
    '    grid-template-columns: 186px minmax(0, 1fr);',
    '    grid-template-rows: auto 1fr auto;',
    # The toggle sits in column 1 of the first row, the optional card
    # title spans column 2. With both items in the same row the toggle
    # naturally top-aligns with whatever sits at the top of column 2
    # (title text, or just the start of the well panel below).
    '    grid-template-areas: "toggle title" "side main" "side foot";',
    '    column-gap: 18px; row-gap: 0;',
    '    align-items: start;',
    '  }',
    '  .ndr-card-title { grid-area: title; }',
    # Network dashboard variant: no title, no footer — just toggle in
    # column 1 row 1, sidebar in column 1 row 2, and main (the iframe)
    # spanning column 2 across both rows so the visNetwork canvas
    # starts at the very top of the tab-panel instead of sitting below
    # an empty toggle row.
    '  .network-dashboard {',
    '    grid-template-rows: auto 1fr !important;',
    '    grid-template-areas: "toggle main" "side main" !important;',
    '  }',
    # Single well-panel styling for all three dashboard types.
    '  .impact-controls, .priort-controls, .network-controls {',
    '    grid-area: side; display: flex !important; flex-direction: column !important;',
    '    align-items: stretch !important; gap: 12px !important;',
    '    background: var(--ndr-sidebar-bg) !important;',
    '    border: 1px solid var(--ndr-border) !important;',
    '    border-radius: var(--ndr-radius) !important;',
    '    padding: 14px !important; margin: 0 !important;',
    '    align-self: start !important;',
    # Smooth transition into / out of the collapsed state.
    '    transition: opacity 0.18s ease, padding 0.18s ease;',
    '  }',
    '  .impact-table-wrap, .priort-split, .network-main { grid-area: main; }',
    # .network-main spans rows 1+2 in the network grid (areas
    # "toggle main" / "side main"). Anchor it explicitly at the start
    # of the spanned area so the iframe top sits flush with the
    # .ctrl-toggle button top, regardless of the toggle\'s margin-bottom
    # or the .network-controls (sidebar) content height. Zero margins
    # / padding so nothing else can push the iframe down inside its
    # grid cell.
    '  .network-main {',
    '    align-self: start !important;',
    '    margin: 0 !important;',
    '    padding: 0 !important;',
    '  }',
    '  .network-main > .iframe-wrap {',
    '    margin: 0 !important;',
    '    padding: 0 !important;',
    '  }',
    '  .impact-footer, .priort-footer { grid-area: foot; }',
    '  .impact-ctrl-cell, .priort-ctrl-cell { width: 100%; }',
    '  .impact-ctrl-row, .priort-ctrl-row {',
    '    flex-direction: column !important; align-items: stretch !important;',
    '    gap: 4px !important;',
    '  }',
    '  .impact-ctrl-row label, .priort-ctrl-row label {',
    '    text-align: left !important; flex: none !important;',
    '  }',
    '  .impact-ctrl-row .impact-ctrl, .priort-ctrl-row .priort-ctrl {',
    '    max-width: none !important; width: 100% !important;',
    '  }',
    # Layout dropdown lives inside .network-controls now — zero the
    # padding it carried for its old standalone position (was offsetting
    # the label to match the in-iframe Select-by-ID), since the well
    # panel\'s own 14px padding handles spacing.
    '  .network-controls .layout-controls {',
    '    padding: 0 !important; margin: 0 !important;',
    '  }',
    # Stretch the Layout <select> to fill the well-panel width, same
    # as `.impact-ctrl` / `.priort-ctrl`. Overrides the bare-control
    # `.layout-select` rule that hardcodes width: 130px (matched the
    # in-iframe Select-by-ID when the dropdown lived above the iframe;
    # irrelevant now that it\'s in a sidebar well panel).
    '  .network-controls .layout-select {',
    '    width: 100% !important;',
    '    max-width: none !important;',
    '    box-sizing: border-box !important;',
    '  }',
    # ---- Controls-collapsed state (1200 px+ only) ----
    # The sidebar collapses leftward, but column 1 keeps a narrow rail
    # (44px) so the toggle stays in a dedicated lane and the table /
    # main don\'t slide underneath the button. The sidebar itself loses
    # padding / border / opacity so it gracefully fades out behind the
    # rail. Main + foot start to the right of the rail.
    '  .impact-dashboard.controls-collapsed,',
    '  .priort-dashboard.controls-collapsed,',
    '  .network-dashboard.controls-collapsed {',
    '    grid-template-columns: 44px minmax(0, 1fr) !important;',
    '    column-gap: 0 !important;',
    '  }',
    '  .impact-dashboard.controls-collapsed .impact-controls,',
    '  .priort-dashboard.controls-collapsed .priort-controls,',
    '  .network-dashboard.controls-collapsed .network-controls {',
    '    padding: 0 !important;',
    '    border-width: 0 !important;',
    '    overflow: hidden;',
    '    opacity: 0;',
    '    pointer-events: none;',
    '  }',
    # ---- .ctrl-toggle — grid-placed in column 1 of the top row ----
    # Left-aligned with the well panel below (justify-self: start +
    # margin so it sits flush with the well panel\'s left edge). Background
    # matches the well panel so the toggle reads as part of the sidebar
    # surface. Chevron flips horizontally when collapsed to flag the
    # direction reversal.
    '  .ctrl-toggle {',
    '    display: inline-flex; align-items: center; justify-content: center;',
    '    grid-area: toggle;',
    '    justify-self: start; align-self: start;',
    # margin-top: 10px nudges the toggle down to align its TOP with
    # the in-iframe `Select by ID` dropdown, which is itself positioned
    # at `top: 10px` inside the iframe by
    # bn_visNetwork_deliverable_interactivity.R. Without this offset
    # the toggle sits at the iframe ELEMENT top (10px above where the
    # iframe\'s first visible chrome renders), making them look mis-
    # aligned by ~10 px.
    '    margin-top: 10px;',
    '    margin-bottom: 8px;',
    '    width: 24px; height: 24px;',
    '    background: var(--ndr-sidebar-bg);',
    '    border: 1px solid var(--ndr-border);',
    '    border-radius: 6px;',
    '    color: var(--ndr-text);',
    '    cursor: pointer; font-size: 11px; line-height: 1;',
    '  }',
    '  .ctrl-toggle:hover { background: var(--ndr-secondary-bg); }',
    '  .ctrl-toggle .chev { display: inline-block; transition: transform 0.2s ease; }',
    '  .controls-collapsed .ctrl-toggle .chev { transform: scaleX(-1); }',
    '}',
    # Hide the toggle entirely below the sidebar-layout breakpoint —
    # the controls stack inline above the table at that width, so a
    # collapse affordance would be meaningless.
    '@media (max-width: 1199px) {',
    '  .ctrl-toggle { display: none; }',
    '}',
    '',
    '/* ---- Bootstrap-style selects ---- */',
    '.impact-ctrl, .priort-ctrl, .layout-select {',
    '  border: 1px solid var(--ndr-border) !important;',
    '  border-radius: 6px !important;',
    '  padding: 6px 28px 6px 10px !important;',
    # var(--ndr-fs-sm) = 12px — matches the app's brand form controls
    # (.form-select / .form-control / .selectize-*) so the report's
    # impact + prio control selects render at the same size as the
    # equivalent app sidebar inputs.
    '  font-size: var(--ndr-fs-sm, 12px) !important;',
    '  background-color: var(--ndr-card-bg) !important;',
    '  color: var(--ndr-text) !important;',
    '  -webkit-appearance: none; appearance: none;',
    '  background-image: url("data:image/svg+xml,%3Csvg xmlns=\'http://www.w3.org/2000/svg\' viewBox=\'0 0 16 16\'%3E%3Cpath fill=\'none\' stroke=\'%23343a40\' stroke-linecap=\'round\' stroke-linejoin=\'round\' stroke-width=\'2\' d=\'M2 5l6 6 6-6\'/%3E%3C/svg%3E");',
    '  background-repeat: no-repeat; background-position: right 8px center;',
    '  background-size: 12px;',
    '}',
    '.impact-ctrl:focus, .priort-ctrl:focus, .layout-select:focus {',
    '  border-color: var(--ndr-accent) !important;',
    '  box-shadow: 0 0 0 0.2rem var(--ndr-focus) !important;',
    '  outline: 0;',
    '}',
    '.impact-ctrl-row label, .priort-ctrl-row label, .layout-controls label {',
    # var(--ndr-fs-sm) = 12px — parity with the app's .form-check-label
    # (brand-tokenized) so control labels match across surfaces.
    '  font-weight: 500 !important; font-size: var(--ndr-fs-sm, 12px) !important;',
    '  color: var(--ndr-text) !important;',
    '}',
    '/* Layout control is a bare control row (not a well/card) — overrides',
    '   any earlier card styling. padding-left:10px matches the in-iframe',
    '   Select-by-ID + legend offset (was being clobbered by padding:0). */',
    '.layout-controls {',
    '  background: transparent !important;',
    '  border: none !important;',
    '  border-radius: 0 !important;',
    '  padding: 0 0 0 10px !important;',
    '}',
    '',
    '/* ---- Tables read like reactable ---- */',
    '.impact-table-wrap, .priort-table-wrap {',
    '  border: 1px solid var(--ndr-border);',
    '  border-radius: var(--ndr-radius);',
    '  background: var(--ndr-card-bg);',
    '  box-shadow: var(--ndr-shadow);',
    '  overflow: auto;',
    '}',
    # 13px — parity with --ndr-fs-md / reactable theme so standalone
    # bn_report HTML matches the in-app reactables.
    '.impact-table, .priort-table { font-size: 13px !important; }',
    '.impact-table thead th, .priort-table thead th {',
    '  background: var(--ndr-card-bg) !important;',
    '  color: var(--ndr-text) !important;',
    '  font-weight: 600 !important;',
    '  border: none !important;',
    '  border-bottom: 1px solid var(--ndr-border) !important;',
    '  position: sticky; top: 0; z-index: 2;',
    '}',
    '.impact-table tbody td, .priort-table tbody td {',
    '  border: none !important;',
    '  border-bottom: 1px solid #f0f1f3 !important;',
    # 4px 6px — matches the in-app reactable's `compact = TRUE`
    # spacing so the standalone bn_report HTML reads with the same
    # row density. Header / footer keep 8px 10px so they stay
    # visually distinct from data rows.
    '  padding: 4px 6px !important;',
    '}',
    '.impact-table tbody tr:hover td, .priort-table tbody tr:hover td {',
    '  background: var(--bs-secondary-bg, #f0f0f0) !important;',
    '}',
    '.impact-table tfoot td, .priort-table tfoot td {',
    '  background: var(--bs-tertiary-bg, #f8f9fa) !important;',
    '  border-top: 1px solid var(--ndr-border) !important;',
    '  position: sticky; bottom: 0;',
    '}',
    '.impact-table tfoot tr.ti-row td { border-top: 1px solid var(--ndr-border) !important; }',
    '',
    '/* ---- Prio chart: greyscale to match the original bn_report ---- */',
    '.priort-chart .bar-prev { fill: #D9D9D9 !important; }',
    '.priort-chart .bar-incr { fill: #595959 !important; }',
    '.priort-chart .cum-line { stroke: #595959 !important; stroke-width: 2; }',
    '.priort-chart .cum-marker { fill: #595959 !important; stroke: #595959 !important; }',
    '.priort-chart-wrap {',
    '  border: 1px solid var(--ndr-border); border-radius: var(--ndr-radius);',
    '  background: var(--ndr-card-bg); box-shadow: var(--ndr-shadow);',
    '  padding: 8px;',
    '  /* reserve a band so the absolute download button (top:10 h:34)',
    '     sits above the chart instead of overlapping it */',
    '  padding-top: 52px;',
    '}',
    '',
    '/* ---- Footer notes ---- */',
    '/* Outer styling now in resondex_css() table_footer_notes block. */',
    '.impact-footer .index-note { font-style: italic; }',
    '',
    '/* ---- Label tooltip affordance — underline only. The tooltip itself',
    '   is the shared resondex floating tooltip (data-tip + resondex_tooltip_js,',
    '   injected by .bn_report_js). The old :hover::after pseudo-element was',
    '   removed: it reflowed the flex row and caused the prio hover flicker. */',
    '.ndr-tip { cursor: help; border-bottom: 1px dotted var(--ndr-muted); }',
    '',
    '/* ---- Card-title row (mirrors the app card header) ---- */',
    '.ndr-card-title {',
    '  font-size: 15px; font-weight: 600; color: var(--ndr-text);',
    '  padding: 6px 4px 12px 2px; margin: 0 0 6px 0;',
    '  border-bottom: 1px solid var(--ndr-border);',
    '}',
    '.ndr-card-title:empty { display: none; }',
    '.ndr-card-title em { font-weight: 500; color: var(--ndr-muted); font-style: italic; }',
    '/* The old inline assess-feedback span is superseded by the card title. */',
    '.impact-warning.assess-feedback { display: none !important; }',
    '',
    '/* ---- Outcome Estimate column: hidden until the Show/Hide control',
    '   adds .ndr-show-estimate on an ancestor (the JS `root` is the',
    '   .priort-panel wrapper, not the .priort-dashboard div — so use a',
    '   descendant selector that matches regardless of which ancestor',
    '   carries the class) ---- */',
    '.priort-table th.est-col, .priort-table td.est-col { display: none; }',
    '.ndr-show-estimate .priort-table th.est-col,',
    '.ndr-show-estimate .priort-table td.est-col {',
    '  display: table-cell;',
    '}',
    '/* Show/Hide control button — matches the app\\u2019s small outlined',
    '   white button. */',
    '.ndr-estimate-toggle {',
    # var(--ndr-fs-sm) = 12px — matches the app's .btn-rdx so the
    # toggle button reads the same size as the app's equivalent
    # outlined action buttons.
    '  width: 100%; padding: 6px 10px; font-size: var(--ndr-fs-sm, 12px);',
    '  background: var(--ndr-card-bg); color: var(--ndr-text);',
    '  border: 1px solid var(--ndr-border); border-radius: 6px;',
    '  cursor: pointer;',
    '}',
    '.ndr-estimate-toggle:hover { background: var(--ndr-secondary-bg); }'
  ), collapse = "\n")
}

# --- internal: report JS ---
#' @noRd
.bn_report_js <- function(save_name) {

  # membership sync snippet (applied to static HTML membership tab)
  membership_sync <- paste0(
    'var rEdits = legendEdits[rName] || {};\n',
    '        Object.keys(rEdits).forEach(function(color) {\n',
    '          panel.querySelectorAll(\'.community-label[data-color="\' + color + \'"]\').forEach(function(el) {\n',
    '            el.textContent = rEdits[color];\n',
    '          });\n',
    '        });\n',
    '        var nEdits = nodeLabelEdits;\n',
    '        Object.keys(nEdits).forEach(function(nodeId) {\n',
    '          panel.querySelectorAll(\'[data-node-id="\' + nodeId + \'"]\').forEach(function(el) {\n',
    '            el.textContent = nEdits[nodeId];\n',
    '          });\n',
    '        });'
  )

  # save download snippet
  save_download <- paste0(
    'var a = document.createElement("a");\n',
    '      a.href = URL.createObjectURL(blob);\n',
    '      a.download = "', save_name, '.resondex_bn";\n',
    '      a.click();'
  )

  paste(c(
    # Shared brand tooltip: one floating element bound to every [data-tip]
    # (the control labels formerly using the .ndr-tip :hover::after CSS).
    # Idempotent IIFE — safe at the top of the report script.
    resondex_tooltip_js(),
    'function switchType(resultId, panelId) {',
    '  var accordion = document.getElementById(panelId).closest(".result-accordion");',
    '  accordion.querySelectorAll(".type-panel").forEach(function(p) {',
    '    p.style.display = "none";',
    '  });',
    '  var panel = document.getElementById(panelId);',
    '',
    '  // Re-sync this panel\'s layout dropdown(s) to its own type. Each panel',
    '  // ships pre-selected to itself, but once the user navigates away via a',
    '  // panel\'s dropdown the browser keeps that mutated value, so returning',
    '  // to the panel would show the layout they left to instead of this one.',
    '  panel.querySelectorAll(".layout-select").forEach(function(s) { s.value = panelId; });',
    '',
    '  var activePanel = panel.querySelector(".tab-panel.active");',
    '  var hasEdits = false;',
    '  if (activePanel) {',
    '    var rName = activePanel.getAttribute("data-result") || "_default";',
    '    var rEdits = legendEdits[rName] || {};',
    '    hasEdits = Object.keys(rEdits).length > 0 || Object.keys(nodeLabelEdits).length > 0;',
    '  }',
    '',
    '  panel.style.display = "block";',
    '',
    '  if (hasEdits) {',
    '    panel.style.opacity = "0";',
    '    requestAnimationFrame(function() {',
    '      var rEdits = legendEdits[rName] || {};',
    '      sendSyncEdits(activePanel, rEdits);',
    '    });',
    '  } else {',
    '    panel.querySelectorAll("iframe").forEach(function(iframe) {',
    '      try { iframe.contentWindow.postMessage({ type: "fitNetwork" }, "*"); } catch(e) {}',
    '    });',
    '  }',
    '',
    '  if (Object.keys(window.pendingLoads).length > 0) {',
    '    panel.querySelectorAll(".tab-panel[data-result]").forEach(function(tp) {',
    '      sendSnapshotToPanel(tp);',
    '    });',
    '  }',
    '}',
    '',
    'function sendSyncEdits(tabPanel, legend) {',
    '  var iframe = tabPanel.querySelector("iframe");',
    '  if (!iframe) return;',
    '  try {',
    '    iframe.contentWindow.postMessage({',
    '      type: "syncEdits",',
    '      legend: legend,',
    '      nodeLabels: nodeLabelEdits',
    '    }, "*");',
    '  } catch(e) {}',
    '  try { iframe.contentWindow.postMessage({ type: "fitNetwork" }, "*"); } catch(e) {}',
    '}',
    '',
    'function switchTab(btn, panelId) {',
    '  var bar = btn.parentElement;',
    '  bar.querySelectorAll(".tab-btn").forEach(function(b) { b.classList.remove("active"); });',
    '  btn.classList.add("active");',
    '  var typePanel = bar.parentElement;',
    '  typePanel.querySelectorAll(".tab-panel").forEach(function(p) { p.classList.remove("active"); });',
    '  document.getElementById(panelId).classList.add("active");',
    '}',
    '',
    'function toggleMembershipView(btn) {',
    '  var wrap = btn.closest(".membership-wrap");',
    '  var tbl = wrap.querySelector(".membership-table-view");',
    '  var crd = wrap.querySelector(".membership-card-view");',
    '  if (tbl.style.display === "none") {',
    '    tbl.style.display = ""; crd.style.display = "none";',
    '  } else {',
    '    tbl.style.display = "none"; crd.style.display = "";',
    '  }',
    '}',
    '',
    '/* --- Collapse / expand the controls sidebar on impact + prio dashboards. */',
    '/* Toggle is per-dashboard (each .impact-dashboard / .priort-dashboard tracks */',
    '/* its own .controls-collapsed class), so collapsing one sidebar leaves the */',
    '/* others untouched. CSS in .bn_report_css() handles the actual layout flip. */',
    'function toggleCtrls(btn) {',
    '  var root = btn.closest(".impact-dashboard, .priort-dashboard, .network-dashboard");',
    '  if (!root) return;',
    '  root.classList.toggle("controls-collapsed");',
    '}',
    '',
    '/* --- Attribute Impacts dashboard --- */',
    'function initImpactDashboard(dashId) {',
    '  var root = document.getElementById(dashId);',
    '  if (!root) return;',
    '  // Prefer a shared payload referenced by data-impact-data-id (one',
    '  // <script> serves N dashboards across layout types). The attribute',
    '  // lives on the inner .impact-dashboard div — root may be its outer',
    '  // tab-panel wrapper, so we search WITHIN root instead of reading off',
    '  // root itself. Fall back to the dashboard\'s own embedded payload.',
    '  var dataScript = null;',
    '  var sharedHost = root.matches("[data-impact-data-id]") ? root : root.querySelector("[data-impact-data-id]");',
    '  var sharedId = sharedHost ? sharedHost.getAttribute("data-impact-data-id") : null;',
    '  if (sharedId) dataScript = document.getElementById(sharedId);',
    '  if (!dataScript) dataScript = root.querySelector("script.impact-data");',
    '  if (!dataScript) return;',
    '  var data;',
    '  try { data = JSON.parse(dataScript.textContent); } catch (e) { return; }',
    '',
    '  function currentValue(dim) {',
    '    var sel = root.querySelector(\'.impact-ctrl[data-dim="\' + dim + \'"]\');',
    '    return sel ? sel.value : null;',
    '  }',
    '',
    '  function getRows() {',
    '    var weight = currentValue("weight") || "Unweighted";',
    '    if (weight === "Weighted" && data.rows_weighted) return data.rows_weighted;',
    '    return data.rows_unweighted;',
    '  }',
    '',
    '  function metricKey(focus) {',
    '    var key = currentValue("metric");',
    '    if (!key) return null;',
    '    // MI has no outcome-display or shift variant — bare key.',
    '    if (key === "mi") return key;',
    '    var display = currentValue("display") || "propdisplay";',
    '    // maxVmin is shift-independent: base + display.',
    '    if (key === "maxVmin") return key + "_" + display;',
    '    // lift metrics: shift tag is inserted between the base (or base+focus)',
    '    // and the display tag. Market lift: base_shift_display.',
    '    // Brand lift:  base_focus_shift_display.',
    '    var shift = (data.has_shift_type ? currentValue("shift") : null) || "propshift";',
    '    if (focus && focus !== "Market") return key + "_" + focus + "_" + shift + "_" + display;',
    '    return key + "_" + shift + "_" + display;',
    '  }',
    '',
    '  function isInsignificant(sgData, focus) {',
    '    if (!sgData) return false;',
    '    // Bootstrap mode: each metric column has its own `<col>_p_value`',
    '    // sibling. Use the bootstrap p-value of the *currently selected*',
    '    // metric so the blackout follows whatever the user is viewing.',
    '    // Static mode: fall back to the chi-squared MI p-value (one per row).',
    '    var pv;',
    '    if (data.boot_applied && focus) {',
    '      var k = metricKey(focus);',
    '      pv = (k ? sgData[k + "_p_value"] : null);',
    '      if (pv == null) pv = sgData.p_val; // safety fallback',
    '    } else {',
    '      pv = sgData.p_val;',
    '    }',
    '    return (pv != null && pv > 0.10);',
    '  }',
    '',
    '  function getRaw(row, sg, focus) {',
    '    var sgData = row.sg[sg]; if (!sgData) return null;',
    '    var k = metricKey(focus); if (!k) return null;',
    '    var v = sgData[k];',
    '    return (v == null ? null : v);',
    '  }',
    '',
    '  function update() {',
    '    var allRows = getRows();',
    '    var focus = currentValue("focus") || "Market";',
    '    var mkey = currentValue("metric");',
    '    var weight = currentValue("weight") || "Unweighted";',
    '    var indexBy = currentValue("indexby") || "All";',
    '',
    '    // Index By dropdown semantics:',
    '    //   "All"          → all rows visible, indexed against the global',
    '    //                    per-subgroup mean.',
    '    //   "<batteryName>" → only rows in that battery are visible.',
    '    //   "<groupName>"  → only rows whose IV is in any of the group\'s',
    '    //                    component batteries are visible.',
    '    //   In both filtered cases the index normalizes against the visible',
    '    //   rows\' mean for each subgroup.',
    '    var groupIvs = (data.battery_groups && data.battery_groups[indexBy]) || null;',
    '    var memberOf = function(r) {',
    '      if (indexBy === "All") return true;',
    '      if (groupIvs) return groupIvs.indexOf(r.id) !== -1;',
    '      return (r.battery || "") === indexBy;',
    '    };',
    '    var batteryFilter = (data.has_battery && indexBy !== "All") ? indexBy : null;',
    '    var rows = batteryFilter == null ? allRows : allRows.filter(memberOf);',
    '',
    '    // Hide / show <tr>s based on filter so the table visually reflects',
    '    // the active selection.',
    '    allRows.forEach(function(r, i) {',
    '      var firstCell = root.querySelector(\'td.idx-cell[data-row="\' + i + \'"]\');',
    '      if (!firstCell) return;',
    '      var tr = firstCell.parentElement;',
    '      tr.style.display = memberOf(r) ? "" : "none";',
    '    });',
    '',
    '    // 1. Compute the per-subgroup mean of |raw| over the *visible* rows.',
    '    //    That denominator drives every index cell shown.',
    '    data.subgroups.forEach(function(sg) {',
    '      var absVals = rows.map(function(r) {',
    '        var v = getRaw(r, sg, focus);',
    '        return v == null ? 0 : Math.abs(v);',
    '      });',
    '      var sum = absVals.reduce(function(a, b) { return a + b; }, 0);',
    '      var mean = absVals.length > 0 ? (sum / absVals.length) : 0;',
    '',
    '      // 2. Fill index cells + collect for color scaling',
    '      var idxValues = [];',
    '      rows.forEach(function(r) {',
    '        // Find this row\'s index in allRows so we can target the right <td>.',
    '        var i = allRows.indexOf(r);',
    '        var cell = root.querySelector(\'td.idx-cell[data-sg="\' + sg + \'"][data-row="\' + i + \'"]\');',
    '        if (!cell) return;',
    '        var raw = getRaw(r, sg, focus);',
    '        var sgData = r.sg[sg];',
    '        cell.classList.remove("rdx-insig", "rdx-neg");',
    '        cell.style.background = "";',
    '        cell.removeAttribute("data-qc-tip");',
    '',
    '        if (raw == null || mean === 0) { cell.textContent = ""; idxValues.push(null); return; }',
    '        var idx = Math.abs(raw) / mean * 100;',
    '        cell.textContent = Math.round(idx);',
    '        idxValues.push(idx);',
    '',
    '        // QC-mode hover: surface the raw metric value (the hidden column',
    '        // in the Excel equivalent) plus the unrounded index. We use a',
    '        // custom tooltip (see initTooltip below) instead of the native',
    '        // `title=` attribute — native tooltips have unreliable rendering',
    '        // on some browser/OS combos and long appearance delays.',
    '        if (data.qc_mode) {',
    '          var rawFmt = Math.abs(raw) >= 0.01 ? raw.toFixed(4) : raw.toExponential(3);',
    '          cell.setAttribute("data-qc-tip",',
    '            "Raw metric: " + rawFmt + "\\nIndex: " + idx.toFixed(2));',
    '        }',
    '',
    '        if (raw < 0) cell.classList.add("rdx-neg");',
    '        if (isInsignificant(sgData, focus)) cell.classList.add("rdx-insig");',
    '      });',
    '',
    '      // 3. Apply 3-color scale across non-null, non-insig cells in this subgroup',
    '      var vals = idxValues.filter(function(v) { return v != null; });',
    '      if (vals.length > 0) {',
    '        var minV = Math.min.apply(null, vals);',
    '        var maxV = Math.max.apply(null, vals);',
    '        var midV = (minV + maxV) / 2;',
    '        rows.forEach(function(r, fi) {',
    '          var allI = allRows.indexOf(r);',
    '          var cell = root.querySelector(\'td.idx-cell[data-sg="\' + sg + \'"][data-row="\' + allI + \'"]\');',
    '          if (!cell || cell.classList.contains("rdx-insig")) return;',
    '          var v = idxValues[fi]; if (v == null) return;',
    '          cell.style.background = interpolate3(v, minV, midV, maxV);',
    '        });',
    '      }',
    '',
    '      // 4. Total Impact = sum(|raw|) / count (only for lift-type metrics)',
    '      // Outcome-aware format: Point Change ("absdisplay") -> decimal',
    '      // (e.g. "0.10"), % Change ("propdisplay") -> percent ("3.5%").',
    '      // Mirrors the Excel dashboard total_impact behavior.',
    '      var tiCell = root.querySelector(\'td.ti-cell[data-sg="\' + sg + \'"]\');',
    '      if (tiCell) {',
    '        if (mkey && (mkey === "maxVmin" || mkey === "mi")) {',
    '          tiCell.textContent = "";',
    '        } else if (sum === 0 || rows.length === 0) {',
    '          tiCell.textContent = "";',
    '        } else {',
    '          var ti = sum / rows.length;',
    '          var displayKey = currentValue("display") || "propdisplay";',
    '          tiCell.textContent = (displayKey === "absdisplay")',
    '            ? ti.toFixed(2)',
    '            : (ti * 100).toFixed(1) + "%";',
    '        }',
    '      }',
    '',
    '      // 5. Base cell: base or base_{focus} from the first row',
    '      var baseCell = root.querySelector(\'td.base-cell[data-sg="\' + sg + \'"]\');',
    '      if (baseCell && rows.length > 0) {',
    '        var baseKey = (focus === "Market") ? "base" : ("base_" + focus);',
    '        var b = rows[0].sg[sg] ? rows[0].sg[sg][baseKey] : null;',
    '        baseCell.textContent = (b == null) ? "" : Math.round(b);',
    '      }',
    '    });',
    '',
    '    // Pass-B shift-type grey-out: focus/weight have no effect on the',
    '    // lift value ONLY when ALL of: (1) shift is a FIXED-STEP shift',
    '    // (absshift = Fixed Step OR rangeshift = % of Range — both add a',
    '    // constant increment independent of the distribution, so the',
    '    // POINT-CHANGE numerator is focus/weight-invariant), (2) the',
    '    // metric is a lift, AND (3) Outcome = Point Change. Under',
    '    // % Change the figure is lift_abs / observed_expected, and the',
    '    // baseline (observed_expected) is focus/weight-specific, so',
    '    // focus/weight ALWAYS matter under % Change regardless of shift.',
    '    var shiftVal = data.has_shift_type ? currentValue("shift") : "propshift";',
    '    var displayVal = currentValue("display") || "propdisplay";',
    '    var isLiftMetric = mkey && mkey !== "maxVmin" && mkey !== "mi";',
    '    var isFixedStepShift = (shiftVal === "absshift" || shiftVal === "rangeshift");',
    '    var shiftAbsAndLift = isFixedStepShift && isLiftMetric && displayVal === "absdisplay";',
    '',
    '    // 6. Focus warning: grey italic in both cases —',
    '    //   (a) any subgroup base below minimum (results not calculated)',
    '    //   (b) fixed-step shift (Fixed Step / % of Range) + lift metric',
    '    var focusWarn = root.querySelector(\'.impact-warning[data-for="focus"]\');',
    '    if (focusWarn) {',
    '      var focusSel = root.querySelector(\'.impact-ctrl[data-dim="focus"]\');',
    '      focusWarn.textContent = "";',
    '      focusWarn.classList.remove("warn-grey");',
    '      focusSel.classList.remove("warn");',
    '      if (focus !== "Market" && mkey && mkey !== "maxVmin" && mkey !== "mi" && rows.length > 0) {',
    '        var baseKey = "base_" + focus;',
    '        var minBaseAll = null;',
    '        data.subgroups.forEach(function(sg) {',
    '          var b = rows[0].sg[sg] ? rows[0].sg[sg][baseKey] : null;',
    '          if (b != null && (minBaseAll == null || b < minBaseAll)) minBaseAll = b;',
    '        });',
    '        if (minBaseAll != null && minBaseAll < data.min_base_for_lift) {',
    '          focusWarn.textContent = "Results not calculated because base is below " + data.min_base_for_lift;',
    '          focusWarn.classList.add("warn-grey");',
    '        }',
    '      }',
    '      if (!focusWarn.textContent && shiftAbsAndLift) {',
    '        focusWarn.textContent = "Focus does not affect this metric when shift is a fixed step or % of range";',
    '        focusWarn.classList.add("warn-grey");',
    '      }',
    '    }',
    '',
    '    // 7. Weight warning: grey italic for maxVmin/mi OR shift=absolute+lift.',
    '    var weightWarn = root.querySelector(\'.impact-warning[data-for="weight"]\');',
    '    if (weightWarn) {',
    '      weightWarn.classList.remove("warn-grey");',
    '      if (mkey === "maxVmin" || mkey === "mi") {',
    '        weightWarn.textContent = "Weights don\\u2019t affect this metric";',
    '        weightWarn.classList.add("warn-grey");',
    '      } else if (shiftAbsAndLift) {',
    '        weightWarn.textContent = "Weights don\\u2019t affect this metric when shift is a fixed step or % of range";',
    '        weightWarn.classList.add("warn-grey");',
    '      } else {',
    '        weightWarn.textContent = "";',
    '      }',
    '    }',
    '',
    '    // 7b. Outcome Display warning: grey italic "doesn\\u2019t affect this metric"',
    '    // when metric = mi (mi has no display variants).',
    '    var displayWarn = root.querySelector(\'.impact-warning[data-for="display"]\');',
    '    if (displayWarn) {',
    '      displayWarn.classList.remove("warn-grey");',
    '      if (mkey === "mi") {',
    '        displayWarn.textContent = "Outcome display doesn\\u2019t affect this metric";',
    '        displayWarn.classList.add("warn-grey");',
    '      } else {',
    '        displayWarn.textContent = "";',
    '      }',
    '    }',
    '',
    '    // 7c. Shift Type warning: grey italic for maxVmin/mi (neither is',
    '    // computed via bn_freq_prob_shift, so shift type has no effect).',
    '    var shiftWarn = root.querySelector(\'.impact-warning[data-for="shift"]\');',
    '    if (shiftWarn) {',
    '      shiftWarn.classList.remove("warn-grey");',
    '      if (mkey === "maxVmin" || mkey === "mi") {',
    '        shiftWarn.textContent = "Shift type doesn\\u2019t affect this metric";',
    '        shiftWarn.classList.add("warn-grey");',
    '      } else {',
    '        shiftWarn.textContent = "";',
    '      }',
    '    }',
    '',
    '    // 8. Index note below the table',
    '    var note = root.querySelector(".index-note");',
    '    if (note) {',
    '      var desc = metricDescription(mkey, shiftVal);',
    '      note.textContent = desc;',
    '    }',
    '  }',
    '',
    '  // Sentence fragment describing what an N% lift means under the',
    '  // current Shift Type. Used as a tail clause on the index note.',
    '  function shiftMeaningSentence(pct, shiftKey) {',
    '    var k = shiftKey || "propshift";',
    '    var n = parseFloat(pct);',
    '    var step = isFinite(n) ? (n / 100).toFixed(2) : "0.10";',
    '    if (k === "propshift") {',
    '      return "Each attribute\\u2019s mean is shifted by " + pct + "% of its current value.";',
    '    } else if (k === "absshift") {',
    '      return "Each attribute\\u2019s mean is shifted by " + step + " scale points (a fixed step).";',
    '    } else if (k === "headshift") {',
    '      return "Each attribute closes " + pct + "% of its gap to the top of its scale.";',
    '    } else if (k === "rangeshift") {',
    '      return "Each attribute\\u2019s mean is shifted by " + pct + "% of its scale\\u2019s range.";',
    '    }',
    '    return "";',
    '  }',
    '',
    '  function metricDescription(mkey, shiftKey) {',
    '    if (!mkey) return "";',
    '    if (mkey === "lift" || mkey === "lift_0") {',
    '      return "Indexed by average effect. Measures the outcome\\u2019s sensitivity to a small symmetric perturbation around each attribute\\u2019s current state.";',
    '    }',
    '    if (mkey.indexOf("lift_") === 0) {',
    '      var pct = mkey.replace("lift_", "");',
    '      var head = "Indexed by " + pct + "% improvement. Measures how much the outcome changes when each attribute\\u2019s distribution shifts by " + pct + "%. ";',
    '      return head + shiftMeaningSentence(pct, shiftKey);',
    '    }',
    '    if (mkey === "maxVmin") {',
    '      return "Indexed by best-vs-worst effect. Measures the outcome difference between the top of each attribute versus the bottom.";',
    '    }',
    '    if (mkey === "mi") {',
    '      return "Indexed by explanatory value. Measures the statistical strength of the relationship between each attribute and the outcome (mutual information), independent of intervention direction or shift type.";',
    '    }',
    '    return "Indexed by " + mkey;',
    '  }',
    '',
    '  function interpolate3(v, lo, mid, hi) {',
    '    // Diverging brand scale (matches showcase Index + the reactable):',
    '    // var(--ndr-success) above the midpoint, var(--ndr-danger) below;',
    '    // tint strength scales with distance from mid, capped at 45%.',
    '    // Adapts to light/dark via the tokens; bn_impact_write Excel keeps',
    '    // its own red/yellow/green scale (no longer mirrored here).',
    '    if (hi === lo) return "transparent";',
    '    var t;',
    '    if (v >= mid) { t = (v - mid) / (hi - mid || 1); }',
    '    else          { t = (mid - v) / (mid - lo || 1); }',
    '    t = Math.max(0, Math.min(1, t));',
    '    var pct = Math.round(t * 45);',
    '    if (pct === 0) return "transparent";',
    '    var tok = (v >= mid) ? "--ndr-success" : "--ndr-danger";',
    '    return "color-mix(in srgb, var(" + tok + ") " + pct + "%, transparent)";',
    '  }',
    '',
    '  // Assess preset dropdown — drives Analysis (metric) and Shift Type',
    '  // to one of three curated combos. Selecting Custom unhides those',
    '  // dropdowns and leaves them user-tunable. Toggling metric or shift',
    '  // directly auto-flips Assess back to Custom so the visible state',
    '  // and the underlying controls stay in sync.',
    '  var assessSel = root.querySelector(\'.priort-ctrl[data-dim="assess"]\') ||',
    '                  root.querySelector(\'.impact-ctrl[data-dim="assess"]\');',
    '  var metricSel = root.querySelector(\'.impact-ctrl[data-dim="metric"]\');',
    '  var shiftSel  = root.querySelector(\'.impact-ctrl[data-dim="shift"]\');',
    '  var assessFeedback = root.querySelector(".assess-feedback");',
    '',
    '  function applyPreset(presetName) {',
    '    if (!data.presets || !data.presets[presetName]) return;',
    '    var p = data.presets[presetName];',
    '    var changed = false;',
    '    if (metricSel && p.metric && metricSel.value !== p.metric) {',
    '      metricSel.value = p.metric;',
    '      changed = true;',
    '    }',
    '    if (shiftSel && p.shift && shiftSel.value !== p.shift) {',
    '      shiftSel.value = p.shift;',
    '      changed = true;',
    '    }',
    '    if (changed) update();',
    '  }',
    '',
    '  function presetMatchingCurrent() {',
    '    if (!data.presets) return null;',
    '    var mv = metricSel ? metricSel.value : null;',
    '    var sv = shiftSel  ? shiftSel.value  : null;',
    '    var keys = Object.keys(data.presets);',
    '    for (var i = 0; i < keys.length; i++) {',
    '      var p = data.presets[keys[i]];',
    '      var metricOk = !p.metric || p.metric === mv;',
    '      var shiftOk  = !p.shift  || p.shift  === sv || !shiftSel;',
    '      if (metricOk && shiftOk) return keys[i];',
    '    }',
    '    return null;',
    '  }',
    '',
    '  // Reflect the Assess state visually: hide row 2 (Analysis + Shift)',
    '  // when on a preset, show on Custom. Populate the feedback span',
    '  // with the matching preset\'s question.',
    '  function applyAssessVisualState() {',
    '    if (!assessSel) return;',
    '    var dash = root.classList.contains("impact-dashboard")',
    '      ? root',
    '      : (root.querySelector(".impact-dashboard") || root);',
    '    var v = assessSel.value;',
    '    if (v === "Custom") {',
    '      dash.classList.remove("assess-preset");',
    '    } else {',
    '      dash.classList.add("assess-preset");',
    '    }',
    '    if (assessFeedback) {',
    '      var q = (data.presets && data.presets[v] && data.presets[v].question) || "";',
    '      assessFeedback.textContent = q;',
    '    }',
    '    // Card-title row: "{Assess value}: <em>{question}</em>" — mirrors',
    '    // the Shiny app card header. Falls back to bare value when there',
    '    // is no question (Custom).',
    '    var cardTitle = root.querySelector(".impact-card-title");',
    '    if (cardTitle) {',
    '      var cq = (data.presets && data.presets[v] && data.presets[v].question) || "";',
    '      function esc(s){var d=document.createElement("div");d.textContent=s;return d.innerHTML;}',
    '      cardTitle.innerHTML = (v && cq)',
    '        ? esc(v) + ": <em>" + esc(cq) + "</em>"',
    '        : esc(v || "");',
    '    }',
    '  }',
    '',
    '  function syncAssessFromControls() {',
    '    if (!assessSel) return;',
    '    var match = presetMatchingCurrent();',
    '    assessSel.value = match || "Custom";',
    '    applyAssessVisualState();',
    '  }',
    '',
    '  if (assessSel) {',
    '    assessSel.addEventListener("change", function() {',
    '      if (assessSel.value !== "Custom") {',
    '        applyPreset(assessSel.value);',
    '      }',
    '      applyAssessVisualState();',
    '    });',
    '    // On initial render, if the dropdown is set to a preset (default',
    '    // is "Now"), apply that preset to the underlying controls so the',
    '    // table loads against the right metric/shift.',
    '    if (assessSel.value && assessSel.value !== "Custom") {',
    '      applyPreset(assessSel.value);',
    '    }',
    '    applyAssessVisualState();',
    '  }',
    '',
    '  root.querySelectorAll(".impact-ctrl").forEach(function(sel) {',
    '    var dim = sel.getAttribute("data-dim");',
    '    sel.addEventListener("change", function() {',
    '      // Direct edits to metric / shift bump Assess to Custom (unless',
    '      // the new combo happens to match another preset).',
    '      if (assessSel && (dim === "metric" || dim === "shift")) {',
    '        syncAssessFromControls();',
    '      }',
    '      update();',
    '    });',
    '  });',
    '',
    '  // --- Sortable headers ---',
    '  // Click toggles asc -> desc -> original (no sort) on the clicked column.',
    '  // Sorting operates on the <tbody> rows; <tfoot> (Total Impact + Base) is untouched.',
    '  var tbody = root.querySelector(".impact-table tbody");',
    '  var originalOrder = Array.from(tbody.querySelectorAll("tr"));',
    '',
    '  // Apply a sort direction ("asc" | "desc" | "none") to a header. Extracted',
    '  // so both the click handler and the init code path below can reuse it.',
    '  function applySort(th, direction) {',
    '    root.querySelectorAll(".impact-table thead th.sortable").forEach(function(x) {',
    '      x.classList.remove("sorted-asc", "sorted-desc");',
    '      x.setAttribute("data-sort-state", "none");',
    '    });',
    '    if (direction === "none" || !th) {',
    '      originalOrder.forEach(function(tr) { tbody.appendChild(tr); });',
    '      return;',
    '    }',
    '    th.classList.add(direction === "asc" ? "sorted-asc" : "sorted-desc");',
    '    th.setAttribute("data-sort-state", direction);',
    '    var sortType = th.getAttribute("data-sort") || "text";',
    '    var rows = Array.from(tbody.querySelectorAll("tr"));',
    '    var headerCells = Array.from(th.parentElement.children);',
    '    var idx = headerCells.indexOf(th);',
    '    rows.sort(function(a, b) {',
    '      var av = a.children[idx] ? a.children[idx].textContent.trim() : "";',
    '      var bv = b.children[idx] ? b.children[idx].textContent.trim() : "";',
    '      if (sortType === "num") {',
    '        var an = parseFloat(av); var bn = parseFloat(bv);',
    '        // Push blanks to the bottom regardless of direction',
    '        if (isNaN(an) && isNaN(bn)) return 0;',
    '        if (isNaN(an)) return 1;',
    '        if (isNaN(bn)) return -1;',
    '        return direction === "asc" ? an - bn : bn - an;',
    '      }',
    '      var cmp = av.localeCompare(bv, undefined, { sensitivity: "base" });',
    '      return direction === "asc" ? cmp : -cmp;',
    '    });',
    '    rows.forEach(function(tr) { tbody.appendChild(tr); });',
    '  }',
    '',
    '  root.querySelectorAll(".impact-table thead th.sortable").forEach(function(th) {',
    '    th.addEventListener("click", function() {',
    '      var state = th.getAttribute("data-sort-state") || "none";',
    '      var next = state === "none" ? "asc" : (state === "asc" ? "desc" : "none");',
    '      applySort(th, next);',
    '    });',
    '  });',
    '',
    '  // --- Column resize ---',
    '  // Drag the right edge of any header to resize. Resizing a metric',
    '  // column resizes ALL metric columns in sync (so they stay uniform).',
    '  var impactTable = root.querySelector(".impact-table");',
    '  root.querySelectorAll(".impact-table thead th").forEach(function(th) {',
    '    var handle = document.createElement("div");',
    '    handle.className = "col-resize-handle";',
    '    th.appendChild(handle);',
    '    // Prevent the handle from firing sort clicks',
    '    handle.addEventListener("click", function(e) { e.stopPropagation(); });',
    '    handle.addEventListener("mousedown", function(e) {',
    '      e.preventDefault(); e.stopPropagation();',
    '      var startX = e.clientX;',
    '      var startW = th.offsetWidth;',
    '      var isMetric = th.classList.contains("metric-col");',
    '      impactTable.classList.add("resizing");',
    '      var suppressClick = function(ev) { ev.stopPropagation(); ev.preventDefault(); };',
    '      function applyWidth(w) {',
    '        var targets = isMetric',
    '          ? root.querySelectorAll(".impact-table thead th.metric-col")',
    '          : [th];',
    '        targets.forEach(function(t) {',
    '          t.style.width = w + "px";',
    '          t.style.minWidth = w + "px";',
    '          t.style.maxWidth = w + "px";',
    '        });',
    '      }',
    '      function onMove(ev) {',
    '        var newW = Math.max(40, startW + (ev.clientX - startX));',
    '        applyWidth(newW);',
    '      }',
    '      function onUp() {',
    '        document.removeEventListener("mousemove", onMove);',
    '        document.removeEventListener("mouseup", onUp);',
    '        impactTable.classList.remove("resizing");',
    '        // Swallow the trailing click so sort doesn\\u2019t fire',
    '        th.addEventListener("click", suppressClick, { once: true, capture: true });',
    '      }',
    '      document.addEventListener("mousemove", onMove);',
    '      document.addEventListener("mouseup", onUp);',
    '    });',
    '  });',
    '',
    '  update();',
    '',
    '  // QC-mode tooltip: create a single floating .impact-tooltip div and',
    '  // wire it via event delegation on the dashboard root. We install ONCE',
    '  // (idempotent) — re-invoking update() does not re-attach handlers.',
    '  if (data.qc_mode && !root.dataset.qcTooltipInstalled) {',
    '    root.dataset.qcTooltipInstalled = "1";',
    '    var qcTip = document.createElement("div");',
    '    qcTip.className = "impact-tooltip";',
    '    qcTip.style.display = "none";',
    '    document.body.appendChild(qcTip);',
    '    root.addEventListener("mousemove", function(e) {',
    '      var t = e.target.closest(".idx-cell[data-qc-tip]");',
    '      if (!t) { qcTip.style.display = "none"; return; }',
    '      qcTip.textContent = t.getAttribute("data-qc-tip");',
    '      qcTip.style.display = "block";',
    '      qcTip.style.left = (e.clientX + 12) + "px";',
    '      qcTip.style.top  = (e.clientY + 12) + "px";',
    '    });',
    '    root.addEventListener("mouseleave", function() {',
    '      qcTip.style.display = "none";',
    '    });',
    '  }',
    '',
    '  // Default sort: the first metric (subgroup) column, descending — not',
    '  // the Variable/ID column. update() only refills cell values (not row',
    '  // order), so this sort sticks across subsequent metric/focus/weight',
    '  // changes.',
    '  var defaultSortTh = root.querySelector(".impact-table thead th.metric-col");',
    '  if (defaultSortTh) applySort(defaultSortTh, "desc");',
    '',
    '  // Apply any existing network label/community renames to this',
    '  // freshly-initialised Impact dashboard.',
    '  var ndrImpPanel = root.closest(".tab-panel[data-result]");',
    '  ndrApplyImpactEdits(root,',
    '    ndrImpPanel ? (ndrImpPanel.getAttribute("data-result") || "_default") : "_default");',
    '}',
    '',
    '/* --- Prioritization dashboard --- */',
    'function initPriortDashboard(dashId) {',
    '  var root = document.getElementById(dashId);',
    '  if (!root) return;',
    '  var dataScript = root.querySelector("script.priort-data");',
    '  if (!dataScript) return;',
    '  var data;',
    '  try { data = JSON.parse(dataScript.textContent); } catch (e) { return; }',
    '',
    '  // Result name for resolving color-keyed community renames.',
    '  var ndrPrioPanel = root.closest(".tab-panel[data-result]");',
    '  var ndrPrioRName = ndrPrioPanel',
    '    ? (ndrPrioPanel.getAttribute("data-result") || "_default") : "_default";',
    '  // Register this dashboard\\u2019s render so edits elsewhere can force',
    '  // a live re-render (label/community renames flow into the table+chart).',
    '  window.__ndrPriortRenderers = window.__ndrPriortRenderers || [];',
    '  window.__ndrPriortRerender = function() {',
    '    (window.__ndrPriortRenderers || []).forEach(function(fn){ try { fn(); } catch(e){} });',
    '  };',
    '',
    '  function ctrl(dim) {',
    '    return root.querySelector(\'.priort-ctrl[data-dim="\' + dim + \'"]\');',
    '  }',
    '  function currentValue(dim) {',
    '    var sel = ctrl(dim);',
    '    if (sel) return sel.value;',
    '    // Inactive dim — use the single available value',
    '    return (data.dims[dim] && data.dims[dim][0]) || "";',
    '  }',
    '',
    '  function currentKey() {',
    '    var strat = currentValue("strategy");',
    '    // Max strategy is registered only at focus=Market, weight=Unweighted',
    '    // (it\'s brand- and weight-invariant). Force those in the key so',
    '    // switching to Max while other dropdowns sit elsewhere still resolves.',
    '    var isMax = (strat === data.max_label || strat === data.max_deprecated_label);',
    '    var focus = isMax ? "Market" : currentValue("focus");',
    '    var weight = isMax ? "Unweighted" : currentValue("weight");',
    '    return [strat, currentValue("search"),',
    '            currentValue("subgroup"), focus, weight].join("|");',
    '  }',
    '',
    '  function pvalClass(pv) {',
    '    if (pv == null || isNaN(pv)) return "";',
    '    if (pv < data.sig_threshold) return "rdx-pval-sig";',
    '    if (pv < data.marginal_threshold) return "rdx-pval-marg";',
    '    return "rdx-pval-insig";',
    '  }',
    '',
    '  var tbody = root.querySelector(".priort-table tbody");',
    '  var footerBase = root.querySelector(".priort-footer-base");',
    '  var focusWarn = root.querySelector(\'.priort-warning[data-for="focus"]\');',
    '  var focusSel = root.querySelector(\'.priort-ctrl[data-dim="focus"]\');',
    '',
    '  // Apply the Display dropdown to column visibility. Sets a class',
    '  // mode-percent / mode-point on the dashboard root; CSS hides the',
    '  // off-mode th/td via [data-mode="..."] selectors. Runs on every',
    '  // render() and on the dropdown change handler.',
    '  // When the user picks "Maximum Lift (Deprecated)" the dropdown is',
    '  // locked to Point Change — the percent columns are NA in that',
    '  // strategy (no comparison to baseline).',
    '  function syncDisplayMode() {',
    '    var stratSel = root.querySelector(\'.priort-ctrl[data-dim="strategy"]\');',
    '    var strat = stratSel ? stratSel.value : ((data.dims.strategy && data.dims.strategy[0]) || "");',
    '    var isDeprecated = (data.max_deprecated_label && strat === data.max_deprecated_label);',
    '',
    '    var disp = root.querySelector(\'.priort-ctrl[data-dim="chart"]\');',
    '    if (disp) {',
    '      var pctOpt = disp.querySelector(\'option[value="% Change"]\');',
    '      if (pctOpt) pctOpt.disabled = isDeprecated;',
    '      if (isDeprecated && disp.value !== "Point Change") {',
    '        disp.value = "Point Change";',
    '      }',
    '      disp.disabled = isDeprecated;',
    '    }',
    '',
    '    var mode = isDeprecated ? "Point Change" : (disp ? disp.value : "% Change");',
    '    if (mode === "% Change") {',
    '      root.classList.add("mode-percent");',
    '      root.classList.remove("mode-point");',
    '    } else {',
    '      root.classList.add("mode-point");',
    '      root.classList.remove("mode-percent");',
    '    }',
    '  }',
    '',
  '  function whiteToGreen(v, lo, hi) {',
    '    // Sequential brand scale (matches showcase Index₂ + the reactable):',
    '    // color-mix(var(--ndr-success) X%, transparent), 0..55% with t.',
    '    // Adapts to light/dark via the brand token; bn_write_prio Excel',
    '    // keeps its own #FFFFFF→#66BD7D scale.',
    '    if (v == null || isNaN(v)) return "";',
    '    if (hi === lo) return "transparent";',
    '    var t = Math.max(0, Math.min(1, (v - lo) / (hi - lo)));',
    '    var pct = Math.round(t * 55);',
    '    if (pct === 0) return "transparent";',
    '    return "color-mix(in srgb, var(--ndr-success) " + pct + "%, transparent)";',
    '  }',
    '',
    '  function render() {',
    '    var key = currentKey();',
    '    var entry = data.lookup[key] || { rows: [], n_obs: null };',
    '',
    '    // Card-title row: "Prioritization: <em>{Analysis}</em>" — mirrors',
    '    // the Shiny app card header (Analysis = the strategy dimension).',
    '    var pCardTitle = root.querySelector(".priort-card-title");',
    '    if (pCardTitle) {',
    '      var strat = currentValue("strategy") || "";',
    '      var pretty = strat.replace(/_/g, " ");',
    '      function pesc(s){var d=document.createElement("div");d.textContent=s;return d.innerHTML;}',
    '      pCardTitle.innerHTML = pretty',
    '        ? "Prioritization: <em>" + pesc(pretty) + "</em>"',
    '        : "Prioritization";',
    '    }',
    '',
    '    // Conditional glossary: show only the entries relevant to the',
    '    // current Display mode + selected Analysis (+ Outcome Estimate',
    '    // visibility). Mirrors the Shiny app footer behaviour.',
    '    (function(){',
    '      var chartSel = root.querySelector(\'.priort-ctrl[data-dim="chart"]\');',
    '      var isPct = !chartSel || chartSel.value === "% Change";',
    '      var curStrat = currentValue("strategy") || "";',
    '      var showEst = root.classList.contains("ndr-show-estimate");',
    '      root.querySelectorAll(".priort-glossary p[data-gl]").forEach(function(p){',
    '        var g = p.getAttribute("data-gl");',
    '        var vis = true;',
    '        if (g === "point") vis = !isPct;',
    '        else if (g === "pct") vis = isPct;',
    '        else if (g === "estimate") vis = showEst;',
    '        else if (g === "strategy") vis = (p.getAttribute("data-gl-strat") === curStrat);',
    '        p.style.display = vis ? "" : "none";',
    '      });',
    '    })();',
    '',
    '    // Base — bold line below the table',
    '    var nObs = entry.n_obs;',
    '    var baseText = (nObs != null && !isNaN(nObs)) ? ("Base: " + Math.round(nObs)) : "";',
    '    if (footerBase) footerBase.textContent = baseText;',
    '',
    '    // Focus warning',
    '    if (focusSel) focusSel.classList.remove("warn");',
    '    if (focusWarn) focusWarn.textContent = "";',
    '    if (nObs != null && !isNaN(nObs) && nObs < data.min_base_for_boot) {',
    '      if (focusWarn) focusWarn.textContent = "Results not calculated because base is below " + data.min_base_for_boot;',
    '      if (focusSel) focusSel.classList.add("warn");',
    '    }',
    '',
    '    // Precompute min/max for each gradient-scaled metric column',
    '    function rangeOf(key) {',
    '      var vals = entry.rows.map(function(r) { return r[key]; })',
    '        .filter(function(v) { return v != null && !isNaN(v); });',
    '      if (vals.length === 0) return null;',
    '      return { lo: Math.min.apply(null, vals), hi: Math.max.apply(null, vals) };',
    '    }',
    '    var rDV  = rangeOf("dv_estimate");',
    '    var rCG  = rangeOf("cumulative_gain");',
    '    var rCGP = rangeOf("cumulative_gain_pct");',
    '    var rMG  = rangeOf("marginal_gain");',
    '    var rMGP = rangeOf("marginal_gain_pct");',
    '',
    '    // Rebuild body',
    '    tbody.innerHTML = "";',
    '    entry.rows.forEach(function(r) {',
    '      var tr = document.createElement("tr");',
    '      var cells = [];',
    '      cells.push({cls: \'num-col\', text: r.priority});',
    '      cells.push({cls: \'txt-col\', text: r.variable});',
    '      if (data.has_community) cells.push({cls: \'txt-col\',',
    '        text: ndrResolveComm(r.community == null ? "" : r.community, ndrPrioRName)});',
    '      if (data.has_label)     cells.push({cls: \'txt-col\',',
    '        text: ndrResolveLabel(r.variable, r.label == null ? "" : r.label)});',
    '      // Outcome / Cumulative Gain / Incremental Lift formatter — when the',
    '      // outcome is binary (data.is_binary), all three render as XX.X%.',
    '      function fmtAbs(v) {',
    '        if (v == null) return "";',
    '        return data.is_binary ? (v * 100).toFixed(1) + "%" : v.toFixed(2);',
    '      }',
    '      // Outcome Estimate — hidden by default (est-col), shown via the',
    '      // Show/Hide control. binary -> XX.X%, continuous -> 0.00; same',
    '      // white-to-green gradient as the gain columns.',
    '      cells.push({',
    '        cls: \'num-col est-col\',',
    '        text: fmtAbs(r.dv_estimate),',
    '        bg: rDV ? whiteToGreen(r.dv_estimate, rDV.lo, rDV.hi) : ""',
    '      });',
    '      // Cumulative Gain — white-to-green gradient (point-mode)',
    '      cells.push({',
    '        cls: \'num-col\',',
    '        mode: "point",',
    '        text: fmtAbs(r.cumulative_gain),',
    '        bg: rCG ? whiteToGreen(r.cumulative_gain, rCG.lo, rCG.hi) : ""',
    '      });',
    '      // Incremental Lift — white-to-green gradient (point-mode)',
    '      cells.push({',
    '        cls: \'num-col\',',
    '        mode: "point",',
    '        text: fmtAbs(r.marginal_gain),',
    '        bg: rMG ? whiteToGreen(r.marginal_gain, rMG.lo, rMG.hi) : ""',
    '      });',
    '      // Cumulative Gain % — white-to-green gradient (percent-mode)',
    '      cells.push({',
    '        cls: \'num-col\',',
    '        mode: "percent",',
    '        text: r.cumulative_gain_pct == null ? "" : (r.cumulative_gain_pct * 100).toFixed(1) + "%",',
    '        bg: rCGP ? whiteToGreen(r.cumulative_gain_pct, rCGP.lo, rCGP.hi) : ""',
    '      });',
    '      // Incremental Lift % — white-to-green gradient (percent-mode)',
    '      cells.push({',
    '        cls: \'num-col\',',
    '        mode: "percent",',
    '        text: r.marginal_gain_pct == null ? "" : (r.marginal_gain_pct * 100).toFixed(1) + "%",',
    '        bg: rMGP ? whiteToGreen(r.marginal_gain_pct, rMGP.lo, rMGP.hi) : ""',
    '      });',
    '      // p-value — green / orange / red coloring (not blackout)',
    '      if (data.has_p) {',
    '        var pCls = pvalClass(r.p_value);',
    '        cells.push({',
    '          cls: \'num-col priort-pval \' + pCls,',
    '          text: (r.p_value == null || isNaN(r.p_value)) ? "" : r.p_value.toFixed(2)',
    '        });',
    '      }',
    '      cells.forEach(function(c) {',
    '        var td = document.createElement("td");',
    '        td.className = c.cls;',
    '        td.textContent = c.text;',
    '        if (c.bg) td.style.background = c.bg;',
    '        if (c.mode) td.setAttribute("data-mode", c.mode);',
    '        tr.appendChild(td);',
    '      });',
    '      tbody.appendChild(tr);',
    '    });',
    '',
    '    // Sync the body+header column visibility to match the current Display',
    '    // dropdown — point-mode columns hide in Percent Change, percent-mode',
    '    // columns hide in Point Change.',
    '    syncDisplayMode();',
    '',
    '    // --- Cumulative-effect chart (waterfall): each step\\u2019s marginal',
    '    // gain stacked on the previous step\\u2019s DV estimate.',
    '    drawChart(entry.rows);',
    '',
    '    // After the table rebuilds, re-evaluate whether it can fit beside the',
    '    // chart in row mode — if not, switch to top/bottom layout so the chart',
    '    // never appears to crowd or overlap the table.',
    '    checkOverflow();',
    '  }',
    '',
    '  // Force column (top/bottom) layout when the viewport is narrow OR when',
    '  // the table\\u2019s natural width would overflow its row-mode container.',
    '  // Implementation: temporarily strip the force-column class so we can',
    '  // measure row-mode dimensions, then re-apply if needed. The temporary',
    '  // toggle costs one synchronous reflow but no visible flicker.',
    '  function checkOverflow() {',
    '    var split = root.querySelector(".priort-split");',
    '    var tableWrap = root.querySelector(".priort-table-wrap");',
    '    var table = root.querySelector(".priort-table");',
    '    if (!split || !tableWrap || !table) return;',
    '',
    '    var wasForced = split.classList.contains("force-column");',
    '',
    '    // Narrow viewport always uses column mode (stacked top/bottom).',
    '    // 1400px matches the Shiny app\\u2019s xxl breakpoint so the table',
    '    // and chart stack earlier rather than cramping side-by-side.',
    '    if (window.innerWidth < 1400) {',
    '      split.classList.add("force-column");',
    '      if (!wasForced && lastChartRows) drawChart();',
    '      return;',
    '    }',
    '',
    '    // Measure in row mode',
    '    split.classList.remove("force-column");',
    '    // +1 px tolerance for sub-pixel rounding',
    '    var overflows = table.scrollWidth > tableWrap.clientWidth + 1;',
    '    if (overflows) split.classList.add("force-column");',
    '',
    '    var nowForced = split.classList.contains("force-column");',
    '    if (nowForced !== wasForced && lastChartRows) drawChart();',
    '  }',
    '',
    '  // React to viewport resizes (data didn\\u2019t change, but available width did)',
    '  window.addEventListener("resize", checkOverflow);',
    '',
  '  var lastChartRows = null;',
    '  function drawChart(rows) {',
    '    if (rows) lastChartRows = rows;',
    '    var svg = root.querySelector(".priort-chart");',
    '    if (!svg) return;',
    '    while (svg.firstChild) svg.removeChild(svg.firstChild);',
    '',
    '    var W = svg.clientWidth || svg.parentElement.clientWidth || 0;',
    '    var H = svg.clientHeight || 480;',
    '    if (W === 0) return; // tab hidden — re-rendered later by observer',
    '    svg.setAttribute("viewBox", "0 0 " + W + " " + H);',
    '    if (!lastChartRows || lastChartRows.length === 0) return;',
    '    rows = lastChartRows;',
    '',
    '    var pad = { l: 50, r: 16, t: 24, b: 70 };',
    '    var plotW = W - pad.l - pad.r;',
    '    var plotH = H - pad.t - pad.b;',
    '',
    '    // Display mode: "Point Change" plots cumulative_gain (with',
    '    // incremental lift as the bar segment) or "% Change" plots',
    '    // cumulative_gain_pct × 100.',
    '    var chartCtrl = root.querySelector(\'.priort-ctrl[data-dim="chart"]\');',
    '    var chartMode = chartCtrl ? chartCtrl.value : "% Change";',
    '    function valueOf(r) {',
    '      if (chartMode === "% Change") {',
    '        return (r.cumulative_gain_pct == null) ? null : r.cumulative_gain_pct * 100;',
    '      }',
    '      return r.cumulative_gain;',
    '    }',
    '    // Percent-label rule: integer when |x| >= 1% (bar height already',
    '    // conveys magnitude; trailing decimals add visual noise), 1 decimal',
    '    // when |x| < 1% (preserves precision so sub-percent values don\\u2019t',
    '    // collapse to "0%"). Point-change for continuous DVs stays at 2',
    '    // decimals (raw scale units).',
    '    function fmtPctLabel(pct) {',
    '      return (Math.abs(pct) >= 1 ? pct.toFixed(0) : pct.toFixed(1)) + "%";',
    '    }',
    '    function valueLabel(v) {',
    '      if (v == null || isNaN(v)) return "";',
    '      if (chartMode === "% Change") return fmtPctLabel(v);',
    '      return data.is_binary ? fmtPctLabel(v * 100) : v.toFixed(2);',
    '    }',
    '',
    '    // y range: floor at min(0, min value); ceiling at max',
    '    var dvVals = rows.map(function(r) { return valueOf(r); })',
    '      .filter(function(v) { return v != null && !isNaN(v); });',
    '    if (dvVals.length === 0) return;',
    '    var rawMin = Math.min(0, Math.min.apply(null, dvVals));',
    '    var rawMax = Math.max.apply(null, dvVals);',
    '    var rawRange = (rawMax - rawMin) || 1;',
    '',
    '    // Pick a "nice" tick step that produces ~4–7 gridlines, snapping',
    '    // yMin / yMax outward so labels land on round numbers (5, 10, 0.5,',
    '    // 1, etc.) instead of arbitrary fractions of the data range.',
    '    function niceStep(range) {',
    '      var rough = range / 5;',
    '      var mag = Math.pow(10, Math.floor(Math.log10(rough)));',
    '      var n = rough / mag;',
    '      var step;',
    '      if (n < 1.5) step = 1;',
    '      else if (n < 3.5) step = 2;',
    '      else if (n < 7.5) step = 5;',
    '      else step = 10;',
    '      return step * mag;',
    '    }',
    '    var step = niceStep(rawRange);',
    '    var yMin = Math.floor(rawMin / step) * step;',
    '    var yMax = Math.ceil(rawMax / step) * step;',
    '    if (yMax === yMin) yMax = yMin + step;',
    '',
    '    function yScale(v) {',
    '      return pad.t + plotH * (1 - (v - yMin) / (yMax - yMin));',
    '    }',
    '',
    '    var n = rows.length;',
    '    var bandW = plotW / n;',
    '    var barW = Math.max(8, bandW * 0.6);',
    '    var barOffset = (bandW - barW) / 2;',
    '',
    '    var ns = "http://www.w3.org/2000/svg";',
    '    function el(tag, attrs, parent) {',
    '      var e = document.createElementNS(ns, tag);',
    '      for (var k in attrs) e.setAttribute(k, attrs[k]);',
    '      (parent || svg).appendChild(e);',
    '      return e;',
    '    }',
    '',
    '    // Gridlines at every `step` between yMin and yMax. Labels land on',
    '    // round numbers (e.g. 0%, 5%, 10%, ... or 0, 0.5, 1.0, ...).',
    '    var EPS = step * 1e-6;',
    '    for (var v = yMin; v <= yMax + EPS; v += step) {',
    '      var y = yScale(v);',
    '      el("line", { x1: pad.l, x2: pad.l + plotW, y1: y, y2: y, "class": "grid-line" });',
    '      var labelV = Math.abs(v) < EPS ? 0 : v;',
    '      var lbl = (chartMode === "% Change")',
    '        ? labelV.toFixed(0) + "%"',
    '        : (data.is_binary ? (labelV * 100).toFixed(0) + "%" : labelV.toFixed(2));',
    '      el("text", { x: pad.l - 6, y: y + 3, "text-anchor": "end", "class": "ax-text" }).textContent = lbl;',
    '    }',
    '    // baseline at 0 (only if 0 is within range)',
    '    if (yMin <= 0 && yMax >= 0) {',
    '      var y0 = yScale(0);',
    '      el("line", { x1: pad.l, x2: pad.l + plotW, y1: y0, y2: y0, "class": "ax-line" });',
    '    }',
    '    // x-axis line',
    '    el("line", { x1: pad.l, x2: pad.l + plotW, y1: pad.t + plotH, y2: pad.t + plotH, "class": "ax-line" });',
    '',
    '    // Stacked bars (matches bn_prioritize_write):',
    '    //   light grey "Previous" base from 0 to min(prev, current)',
    '    //   dark grey "Incremental" segment from min(prev, current) to max(prev, current)',
    '    // Plus a cumulative line + circle markers tracing each step\\u2019s DV.',
    '    var prev = 0;',
    '    var linePoints = [];',
    '',
    '    // Tooltip shows step / variable / community plus the two metrics that',
    '    // correspond to what the chart is rendering: Outcome Estimate +',
    '    // Incremental Lift in Absolute mode, Cumulative Gain % + Incremental',
    '    // Lift % in Percent mode.',
    '    function tooltipText(r) {',
    '      var parts = [];',
    '      parts.push("Step " + (r.priority == null ? "?" : r.priority));',
    '      var name = (r.variable == null ? "" : String(r.variable));',
    '      var rl = ndrResolveLabel(r.variable, r.label);',
    '      if (rl != null && rl !== "" && rl !== name) {',
    '        parts.push(name + " (" + rl + ")");',
    '      } else {',
    '        parts.push(name);',
    '      }',
    '      var rc = ndrResolveComm(r.community, ndrPrioRName);',
    '      if (rc != null && rc !== "") parts.push("Community: " + rc);',
    '      function fmtTipAbs(v) {',
    '        if (v == null || isNaN(v)) return "";',
    '        return data.is_binary ? (v * 100).toFixed(1) + "%" : v.toFixed(2);',
    '      }',
    '      function fmtTipPct(v) {',
    '        if (v == null || isNaN(v)) return "";',
    '        return (v * 100).toFixed(1) + "%";',
    '      }',
    '      if (chartMode === "% Change") {',
    '        if (r.cumulative_gain_pct != null) parts.push("Cumulative Gain %: " + fmtTipPct(r.cumulative_gain_pct));',
    '        if (r.marginal_gain_pct != null) parts.push("Incremental Lift %: " + fmtTipPct(r.marginal_gain_pct));',
    '      } else {',
    '        if (r.cumulative_gain != null) parts.push("Cumulative Gain: " + fmtTipAbs(r.cumulative_gain));',
    '        if (r.marginal_gain != null) parts.push("Incremental Lift: " + fmtTipAbs(r.marginal_gain));',
    '      }',
    '      return parts.join("\\n");',
    '    }',
    '',
    '    var tooltip = root.querySelector(".priort-tooltip");',
    '    function bindTip(node, text) {',
    '      if (!tooltip) return;',
    '      node.style.cursor = "pointer";',
    '      node.addEventListener("mousemove", function(e) {',
    '        tooltip.textContent = text;',
    '        tooltip.style.display = "block";',
    '        // Position with a small offset; clamp to viewport so it stays on screen',
    '        var px = e.clientX + 12;',
    '        var py = e.clientY + 12;',
    '        var tw = tooltip.offsetWidth;',
    '        var th = tooltip.offsetHeight;',
    '        if (px + tw > window.innerWidth - 8) px = e.clientX - tw - 12;',
    '        if (py + th > window.innerHeight - 8) py = e.clientY - th - 12;',
    '        tooltip.style.left = px + "px";',
    '        tooltip.style.top  = py + "px";',
    '      });',
    '      node.addEventListener("mouseleave", function() {',
    '        tooltip.style.display = "none";',
    '      });',
    '    }',
    '',
    '    rows.forEach(function(r, idx) {',
    '      var dv = valueOf(r);',
    '      if (dv == null || isNaN(dv)) return;',
    '      var lo = Math.min(prev, dv);',
    '      var hi = Math.max(prev, dv);',
    '      var x  = pad.l + idx * bandW + barOffset;',
    '      var y0 = yScale(0);',
    '      var yLo = yScale(lo);',
    '      var yHi = yScale(hi);',
    '      var tt = tooltipText(r);',
    '',
    '      // Previous (light grey base): 0 -> lo',
    '      var prevH = Math.max(0, y0 - yLo);',
    '      if (prevH > 0) {',
    '        var rectPrev = el("rect", { x: x, y: yLo, width: barW, height: prevH, "class": "bar-prev" });',
    '        bindTip(rectPrev, tt);',
    '      }',
    '      // Incremental (dark grey): lo -> hi',
    '      var incrH = Math.max(0, yLo - yHi);',
    '      if (incrH > 0) {',
    '        var rectIncr = el("rect", { x: x, y: yHi, width: barW, height: incrH, "class": "bar-incr" });',
    '        bindTip(rectIncr, tt);',
    '      }',
    '',
    '      // Track cumulative-DV line point at the bar centre',
    '      linePoints.push({ x: x + barW / 2, y: yScale(dv), val: dv, tip: tt });',
    '',
    '      // x-axis label (variable name) — rotated -45; hover shows variable + label',
    '      var name = (r.variable == null) ? "" : String(r.variable);',
    '      var labelX = pad.l + idx * bandW + bandW / 2;',
    '      var labelY = pad.t + plotH + 12;',
    '      var t = el("text", {',
    '        x: labelX, y: labelY,',
    '        "text-anchor": "end",',
    '        "transform": "rotate(-45 " + labelX + " " + labelY + ")",',
    '        "class": "x-label"',
    '      });',
    '      t.textContent = name.length > 18 ? name.substring(0, 16) + "\\u2026" : name;',
    '      var hoverName = name;',
    '      var rlbl = ndrResolveLabel(r.variable, r.label);',
    '      if (rlbl != null && rlbl !== "" && rlbl !== name) {',
    '        hoverName = name + " (" + rlbl + ")";',
    '      }',
    '      bindTip(t, hoverName + "\\n\\n" + tt);',
    '      prev = dv;',
    '    });',
    '',
    '    // Draw the cumulative line + markers + value labels above each marker',
    '    if (linePoints.length > 0) {',
    '      var d = linePoints.map(function(p, i) {',
    '        return (i === 0 ? "M" : "L") + p.x + " " + p.y;',
    '      }).join(" ");',
    '      el("path", { d: d, "class": "cum-line" });',
    '      linePoints.forEach(function(p) {',
    '        var c = el("circle", { cx: p.x, cy: p.y, r: 3.5, "class": "cum-marker" });',
    '        bindTip(c, p.tip);',
    '        var lab = el("text", { x: p.x, y: p.y - 6, "class": "bar-label" });',
    '        lab.textContent = valueLabel(p.val);',
    '        bindTip(lab, p.tip);',
    '      });',
    '    }',
    '  }',
    '',
    '  // Wire dropdowns',
    '  root.querySelectorAll(".priort-ctrl").forEach(function(sel) {',
    '    sel.addEventListener("change", render);',
    '  });',
    '',
    '  // Show / Hide Outcome Estimate toggle. Toggles .ndr-show-estimate on',
    '  // the dashboard root (CSS reveals the est-col header + cells), flips',
    '  // the button label, and re-renders so the glossary entry follows.',
    '  var estBtn = root.querySelector("[data-est-toggle]");',
    '  if (estBtn) {',
    '    estBtn.addEventListener("click", function() {',
    '      var on = root.classList.toggle("ndr-show-estimate");',
    '      estBtn.textContent = on ? "Hide" : "Show";',
    '      render();',
    '    });',
    '  }',
    '',
    '  // Download-PNG button — serializes the SVG with inline styles +',
    '  // a white background, draws it onto a 2× canvas for retina-quality',
    '  // rasterization, and triggers a file download. Filename includes',
    '  // the active strategy / focus so multiple downloads stay distinct.',
    '  function downloadChartPng() {',
    '    var svg = root.querySelector(".priort-chart");',
    '    if (!svg) return;',
    '    var rect = svg.getBoundingClientRect();',
    '    var W = Math.round(rect.width || svg.clientWidth || 800);',
    '    var H = Math.round(rect.height || svg.clientHeight || 480);',
    '    if (W === 0 || H === 0) return;',
    '',
    '    // Clone the SVG and inline the styles. The chart elements use CSS',
    '    // classes that resolve via the page stylesheet — once the SVG is',
    '    // serialized into a standalone image, those classes have no rules',
    '    // unless we embed them inside the SVG.',
    '    var clone = svg.cloneNode(true);',
    '    clone.setAttribute("xmlns", "http://www.w3.org/2000/svg");',
    '    clone.setAttribute("width", W);',
    '    clone.setAttribute("height", H);',
    '    clone.setAttribute("viewBox", "0 0 " + W + " " + H);',
    '',
    '    var styleEl = document.createElementNS("http://www.w3.org/2000/svg", "style");',
    '    styleEl.textContent = ',
    '      ".ax-line { stroke: #888; stroke-width: 1; }" +',
    '      ".grid-line { stroke: #e0e0e0; stroke-width: 1; }" +',
    '      ".ax-text { fill: #555; font-size: 11px; font-family: -apple-system, BlinkMacSystemFont, \'Segoe UI\', Roboto, sans-serif; }" +',
    '      ".bar-prev { fill: #D9D9D9; }" +',
    '      ".bar-incr { fill: #595959; }" +',
    '      ".cum-line { stroke: #595959; stroke-width: 2; fill: none; }" +',
    '      ".cum-marker { fill: #595959; stroke: #595959; }" +',
    '      ".bar-label { fill: #333; font-size: 11px; text-anchor: middle; font-family: -apple-system, BlinkMacSystemFont, \'Segoe UI\', Roboto, sans-serif; }" +',
    '      ".x-label { fill: #333; font-size: 11px; font-family: -apple-system, BlinkMacSystemFont, \'Segoe UI\', Roboto, sans-serif; }";',
    '    clone.insertBefore(styleEl, clone.firstChild);',
    '',
    '    // White background — SVG is transparent by default and a',
    '    // checkerboard PNG would look broken in slides / docs.',
    '    var bgRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");',
    '    bgRect.setAttribute("width", "100%");',
    '    bgRect.setAttribute("height", "100%");',
    '    bgRect.setAttribute("fill", "#ffffff");',
    '    clone.insertBefore(bgRect, clone.firstChild);',
    '',
    '    var serializer = new XMLSerializer();',
    '    var svgStr = serializer.serializeToString(clone);',
    '    var svgBlob = new Blob([svgStr], { type: "image/svg+xml;charset=utf-8" });',
    '    var url = URL.createObjectURL(svgBlob);',
    '',
    '    var img = new Image();',
    '    img.onload = function() {',
    '      var scale = 2;  // 2× for retina-quality',
    '      var canvas = document.createElement("canvas");',
    '      canvas.width  = W * scale;',
    '      canvas.height = H * scale;',
    '      var ctx = canvas.getContext("2d");',
    '      ctx.scale(scale, scale);',
    '      ctx.drawImage(img, 0, 0, W, H);',
    '      URL.revokeObjectURL(url);',
    '',
    '      // Build a sensible filename from the active dropdowns.',
    '      function slug(s) {',
    '        return (s || "").replace(/[^A-Za-z0-9]+/g, "_").replace(/^_+|_+$/g, "") || "chart";',
    '      }',
    '      var parts = ["prioritization"];',
    '      var stratSel = root.querySelector(\'.priort-ctrl[data-dim="strategy"]\');',
    '      if (stratSel) parts.push(slug(stratSel.value));',
    '      var sgSel = root.querySelector(\'.priort-ctrl[data-dim="subgroup"]\');',
    '      if (sgSel && sgSel.value !== "Total") parts.push(slug(sgSel.value));',
    '      var focusSel = root.querySelector(\'.priort-ctrl[data-dim="focus"]\');',
    '      if (focusSel && focusSel.value !== "Market") parts.push(slug(focusSel.value));',
    '      var fname = parts.join("_") + ".png";',
    '',
    '      canvas.toBlob(function(blob) {',
    '        if (!blob) return;',
    '        var pngUrl = URL.createObjectURL(blob);',
    '        var a = document.createElement("a");',
    '        a.href = pngUrl;',
    '        a.download = fname;',
    '        document.body.appendChild(a);',
    '        a.click();',
    '        document.body.removeChild(a);',
    '        URL.revokeObjectURL(pngUrl);',
    '      }, "image/png");',
    '    };',
    '    img.onerror = function() { URL.revokeObjectURL(url); };',
    '    img.src = url;',
    '  }',
    '  var dlBtn = root.querySelector(".priort-download-btn");',
    '  if (dlBtn) dlBtn.addEventListener("click", downloadChartPng);',
    '',
    '  // Redraw chart when the panel becomes visible (e.g., user clicks the',
    '  // Prioritization tab for the first time after page load — the SVG was',
    '  // zero-width while the tab was hidden, so the initial render was a no-op).',
    '  if (window.ResizeObserver) {',
    '    var chartWrap = root.querySelector(".priort-chart-wrap");',
    '    if (chartWrap) {',
    '      // Debounced + settled-width guard. A header tooltip (:hover::after)',
    '      // momentarily reflows the flex row; without this guard the observer',
    '      // would redraw, the redraw would shift layout, the hover would be',
    '      // lost, the layout would revert, and the observer would fire again',
    '      // — a visible flicker loop. We only act on a width that is still',
    '      // changed after the debounce settles.',
    '      var lastROWidth = -1, roTimer = null;',
    '      var ro = new ResizeObserver(function(entries) {',
    '        var w = 0;',
    '        entries.forEach(function(e) { w = Math.round(e.contentRect.width); });',
    '        if (w <= 0) return;',
    '        if (Math.abs(w - lastROWidth) < 2) return;',
    '        if (roTimer) clearTimeout(roTimer);',
    '        roTimer = setTimeout(function() {',
    '          var cw = chartWrap ? Math.round(chartWrap.getBoundingClientRect().width) : w;',
    '          if (cw <= 0 || Math.abs(cw - lastROWidth) < 2) return;',
    '          lastROWidth = cw;',
    '          checkOverflow();',
    '          if (lastChartRows) drawChart();',
    '        }, 120);',
    '      });',
    '      ro.observe(chartWrap);',
    '    }',
    '  }',
    '',
    '  // Sortable headers (resets on each render because table rebuilds)',
    '  root.querySelectorAll(".priort-table thead th.sortable").forEach(function(th) {',
    '    th.addEventListener("click", function() {',
    '      var state = th.getAttribute("data-sort-state") || "none";',
    '      var next = state === "none" ? "asc" : (state === "asc" ? "desc" : "none");',
    '      root.querySelectorAll(".priort-table thead th.sortable").forEach(function(x) {',
    '        x.classList.remove("sorted-asc", "sorted-desc");',
    '        x.setAttribute("data-sort-state", "none");',
    '      });',
    '      if (next === "none") { render(); return; }',
    '      th.classList.add(next === "asc" ? "sorted-asc" : "sorted-desc");',
    '      th.setAttribute("data-sort-state", next);',
    '      var sortType = th.getAttribute("data-sort") || "text";',
    '      var tb = root.querySelector(".priort-table tbody");',
    '      var rows = Array.from(tb.querySelectorAll("tr"));',
    '      var headerCells = Array.from(th.parentElement.children);',
    '      var idx = headerCells.indexOf(th);',
    '      rows.sort(function(a, b) {',
    '        var av = a.children[idx] ? a.children[idx].textContent.trim() : "";',
    '        var bv = b.children[idx] ? b.children[idx].textContent.trim() : "";',
    '        if (sortType === "num") {',
    '          // Strip "%" for numeric compare',
    '          var an = parseFloat(av.replace("%", ""));',
    '          var bn = parseFloat(bv.replace("%", ""));',
    '          if (isNaN(an) && isNaN(bn)) return 0;',
    '          if (isNaN(an)) return 1;',
    '          if (isNaN(bn)) return -1;',
    '          return next === "asc" ? an - bn : bn - an;',
    '        }',
    '        var cmp = av.localeCompare(bv, undefined, { sensitivity: "base" });',
    '        return next === "asc" ? cmp : -cmp;',
    '      });',
    '      rows.forEach(function(tr) { tb.appendChild(tr); });',
    '    });',
    '  });',
    '',
    '  // Expose this dashboard\\u2019s render for cross-view edit propagation.',
    '  window.__ndrPriortRenderers.push(render);',
    '  render();',
    '}',
    '',
    'var legendEdits = {};',
    'var nodeLabelEdits = {};',
    '',
    '// ---- Network-edit propagation to the Impact / Prio dashboards ----',
    '// Network label + community renames also drive the Attribute Impacts,',
    '// Community Impacts and Prioritization tables/chart. Labels key by',
    '// the variable id (node id); community renames are color-keyed in',
    '// legendEdits, so we resolve them through an origName->color map',
    '// captured from the membership panel (.community-label carries both',
    '// data-color and data-orig-comm, preserved even after its text is',
    '// replaced). All resolution is defensive: with no matching edit the',
    '// original value is returned unchanged, so it can never corrupt data.',
    'function ndrCommColorMap() {',
    '  var m = {};',
    '  document.querySelectorAll(".community-label[data-orig-comm][data-color]").forEach(function(el){',
    '    var o = el.getAttribute("data-orig-comm");',
    '    var c = el.getAttribute("data-color");',
    '    if (o && c && !(o in m)) m[o] = c;',
    '  });',
    '  return m;',
    '}',
    '// Strip the variable-id prefix a propagated network label may carry.',
    '// Network node labels are often "{variable} - {label}" / "{variable}-',
    '// {label}" / "{variable}{label}"; the tables already show the variable',
    '// in its own column, so the duplicate prefix is removed before display.',
    'function ndrStripVarPrefix(varId, label) {',
    '  if (label == null || varId == null) return label;',
    '  var v = String(varId);',
    '  var e = v.replace(/[.*+?^${}()|[\\]\\\\]/g, "\\\\$&");',
    '  var s = String(label);',
    '  s = s.replace(new RegExp(e + "\\\\s*-\\\\s*", "g"), "");',
    '  s = s.replace(new RegExp(e + "-", "g"), "");',
    '  s = s.replace(new RegExp(e, "g"), "");',
    '  return s.replace(/^\\s*-\\s*/, "").trim();',
    '}',
    'function ndrResolveLabel(varId, orig) {',
    '  if (varId != null && Object.prototype.hasOwnProperty.call(nodeLabelEdits, varId)) {',
    '    return ndrStripVarPrefix(varId, nodeLabelEdits[varId]);',
    '  }',
    '  return orig;',
    '}',
    'function ndrResolveComm(origName, rName) {',
    '  if (!origName) return origName;',
    '  var edits = legendEdits[rName] || legendEdits["_default"] || {};',
    '  var color = ndrCommColorMap()[origName];',
    '  if (color != null && Object.prototype.hasOwnProperty.call(edits, color)) {',
    '    return edits[color];',
    '  }',
    '  return origName;',
    '}',
    '// Re-apply edits to a static Impact table inside `scope` (the panel).',
    'function ndrApplyImpactEdits(scope, rName) {',
    '  if (!scope) return;',
    '  scope.querySelectorAll(".impact-table td[data-col=\\"label\\"][data-node-id]").forEach(function(td){',
    '    var id = td.getAttribute("data-node-id");',
    '    var orig = td.getAttribute("data-orig-label") || "";',
    '    td.textContent = ndrResolveLabel(id, orig);',
    '  });',
    '  // Matches both the Attribute Impacts community column and the',
    '  // Community Impacts id column (community name lives there).',
    '  scope.querySelectorAll(".impact-table td[data-orig-comm]").forEach(function(td){',
    '    var orig = td.getAttribute("data-orig-comm") || "";',
    '    td.textContent = ndrResolveComm(orig, rName);',
    '  });',
    '}',
    '// Re-apply edits across every Impact table on the page (called when',
    '// edits change so already-rendered dashboards update live).',
    'function ndrApplyAllImpactEdits() {',
    '  document.querySelectorAll(".tab-panel[data-result]").forEach(function(tp){',
    '    ndrApplyImpactEdits(tp, tp.getAttribute("data-result") || "_default");',
    '  });',
    '  document.querySelectorAll(".impact-dashboard").forEach(function(d){',
    '    var tp = d.closest(".tab-panel[data-result]");',
    '    ndrApplyImpactEdits(d, tp ? (tp.getAttribute("data-result")||"_default") : "_default");',
    '  });',
    '  // Prio tables/chart are JS-rebuilt — re-render the visible ones.',
    '  if (typeof window.__ndrPriortRerender === "function") window.__ndrPriortRerender();',
    '}',
    '',
    'function findSourceResult(evtSource) {',
    '  var result = { accordion: null, name: null };',
    '  document.querySelectorAll("iframe").forEach(function(f) {',
    '    if (f.contentWindow === evtSource) {',
    '      result.accordion = f.closest(".result-accordion");',
    '      var panel = f.closest(".tab-panel[data-result]");',
    '      if (panel) result.name = panel.getAttribute("data-result");',
    '    }',
    '  });',
    '  return result;',
    '}',
    '',
    'window.addEventListener("message", function(evt) {',
    '  if (!evt.data) return;',
    '  var src = findSourceResult(evt.source);',
    '  var rName = src.name || "_default";',
    '',
    '  if (evt.data.type === "legendUpdate") {',
    '    if (!legendEdits[rName]) legendEdits[rName] = {};',
    '    var legendEditsMap = {};',
    '    evt.data.keyData.forEach(function(item) {',
    '      legendEdits[rName][item.color] = item.label;',
    '      legendEditsMap[item.color] = item.label;',
    '    });',
    '    var scope = src.accordion || document;',
    '    scope.querySelectorAll(".comm-panel iframe").forEach(function(iframe) {',
    '      try { iframe.contentWindow.postMessage({ type: "legendUpdate", edits: legendEditsMap }, "*"); } catch(e) {}',
    '    });',
    '    scope.querySelectorAll(".attr-panel iframe").forEach(function(iframe) {',
    '      if (iframe.contentWindow !== evt.source) {',
    '        try { iframe.contentWindow.postMessage({ type: "nodeUpdate", edits: legendEditsMap }, "*"); } catch(e) {}',
    '      }',
    '    });',
    '    ndrApplyAllImpactEdits();',
    '  }',
    '  if (evt.data.type === "nodeUpdate") {',
    '    var edits = evt.data.edits || {};',
    '    if (!legendEdits[rName]) legendEdits[rName] = {};',
    '    Object.keys(edits).forEach(function(color) {',
    '      legendEdits[rName][color] = edits[color];',
    '    });',
    '    var scope = src.accordion || document;',
    '    scope.querySelectorAll(".attr-panel iframe").forEach(function(iframe) {',
    '      try { iframe.contentWindow.postMessage({ type: "nodeUpdate", edits: edits }, "*"); } catch(e) {}',
    '    });',
    '    scope.querySelectorAll(".comm-panel iframe").forEach(function(iframe) {',
    '      if (iframe.contentWindow !== evt.source) {',
    '        try { iframe.contentWindow.postMessage({ type: "legendUpdate", edits: edits }, "*"); } catch(e) {}',
    '      }',
    '    });',
    '    ndrApplyAllImpactEdits();',
    '  }',
    '  if (evt.data.type === "nodeLabelUpdate") {',
    '    nodeLabelEdits[evt.data.nodeId] = evt.data.label;',
    '    document.querySelectorAll(".attr-panel iframe").forEach(function(iframe) {',
    '      if (iframe.contentWindow !== evt.source) {',
    '        try { iframe.contentWindow.postMessage({ type: "nodeLabelUpdate", nodeId: evt.data.nodeId, label: evt.data.label }, "*"); } catch(e) {}',
    '      }',
    '    });',
    '    ndrApplyAllImpactEdits();',
    '  }',
    '',
    '  if (evt.data.type === "editsSynced") {',
    '    document.querySelectorAll("iframe").forEach(function(f) {',
    '      if (f.contentWindow === evt.source) {',
    '        var typePanel = f.closest(".type-panel");',
    '        if (typePanel) typePanel.style.opacity = "1";',
    '      }',
    '    });',
    '  }',
    '});',
    '',
    'var origSwitchTab = switchTab;',
    'switchTab = function(btn, panelId, skipToggle) {',
    '  if (!skipToggle) origSwitchTab(btn, panelId);',
    '  var panel = document.getElementById(panelId);',
    '  if (!panel) return;',
    '  var rName = panel.getAttribute("data-result") || "_default";',
    '',
    '  if (panel.classList.contains("membership-panel")) {',
    paste0('    ', membership_sync),
    '    return;',
    '  }',
    '',
    '  var rEdits = legendEdits[rName] || {};',
    '  sendSyncEdits(panel, rEdits);',
    '',
    '  if (Object.keys(window.pendingLoads).length > 0) {',
    '    sendSnapshotToPanel(panel);',
    '  }',
    '};',
    '',
    'var snapshotStore = {};',
    'window.pendingLoads = {};',
    '',
    'function dismissSpinner(source) {',
    '  document.querySelectorAll("iframe").forEach(function(f) {',
    '    if (f.contentWindow === source) {',
    '      var overlay = f.parentElement && f.parentElement.querySelector(".spinner-overlay");',
    '      if (overlay) overlay.style.display = "none";',
    '    }',
    '  });',
    '}',
    '',
    'window.addEventListener("message", function(evt) {',
    '  if (!evt.data) return;',
    '  if (evt.data.type === "snapshotPush" && evt.data.nsKey) {',
    '    snapshotStore[evt.data.nsKey] = evt.data.data;',
    '    dismissSpinner(evt.source);',
    '    delete window.pendingLoads[evt.data.nsKey];',
    '  }',
    '  if (evt.data.type === "iframeReady" && evt.data.nsKey) {',
    '    var nsKey = evt.data.nsKey;',
    '    var rName = nsKey.split("|")[0] || "_default";',
    '',
    '    // send pending snapshot load if any',
    '    var data = window.pendingLoads[nsKey];',
    '    if (data) {',
    '      var merged = mergeEditsIntoSnapshot(data, rName);',
    '      try { evt.source.postMessage({ type: "applyReportLoad", snapshot: merged }, "*"); } catch(e) {}',
    '    }',
    '',
    '    // send current legend/node edits so late-loading iframes get them',
    '    var rEdits = legendEdits[rName] || {};',
    '    if (Object.keys(rEdits).length > 0 || Object.keys(nodeLabelEdits).length > 0) {',
    '      try {',
    '        evt.source.postMessage({ type: "syncEdits", legend: rEdits, nodeLabels: nodeLabelEdits }, "*");',
    '      } catch(e) {}',
    '    }',
    '  }',
    '});',
    '',
    'function panelKey(panel) {',
    '  var r = panel.getAttribute("data-result") || "";',
    '  var l = panel.getAttribute("data-layout") || "";',
    '  var v = panel.getAttribute("data-view") || "";',
    '  return r + "|" + l + "|" + v;',
    '}',
    '',
    '// Walk every Impact and Prioritization dashboard and snapshot the value of',
    '// each dropdown (keyed by data-dim) under the dashboard\\u2019s data-dashboard-id.',
    '// Dashboards live in the same DOM regardless of which tab is active, so this',
    '// captures all of them in one pass.',
    'function captureDashboardStates() {',
    '  var states = {};',
    '  document.querySelectorAll(".impact-dashboard, .priort-dashboard").forEach(function(d) {',
    '    var id = d.getAttribute("data-dashboard-id");',
    '    if (!id) return;',
    '    var s = {};',
    '    d.querySelectorAll(".impact-ctrl, .priort-ctrl").forEach(function(sel) {',
    '      var dim = sel.getAttribute("data-dim");',
    '      if (dim) s[dim] = sel.value;',
    '    });',
    '    states[id] = s;',
    '  });',
    '  return states;',
    '}',
    '',
    '// Inverse of captureDashboardStates: set each dropdown\\u2019s value and',
    '// dispatch a "change" event so the dashboard\\u2019s render() runs and the',
    '// table + chart update to match the restored selections.',
    'function restoreDashboardStates(saved) {',
    '  if (!saved) return;',
    '  Object.keys(saved).forEach(function(id) {',
    '    var d = document.querySelector(\'[data-dashboard-id="\' + id + \'"]\');',
    '    if (!d) return;',
    '    var s = saved[id] || {};',
    '    var changedAny = false;',
    '    Object.keys(s).forEach(function(dim) {',
    '      var sel = d.querySelector(\'[data-dim="\' + dim + \'"]\');',
    '      if (!sel) return;',
    '      // Only set if the option exists — otherwise leave the default.',
    '      var has = Array.from(sel.options).some(function(o) { return o.value === s[dim]; });',
    '      if (!has) return;',
    '      if (sel.value !== s[dim]) { sel.value = s[dim]; changedAny = true; }',
    '    });',
    '    // One change event triggers the dashboard\\u2019s render() once. Dispatch',
    '    // on any select that exists so the dashboard re-renders end-to-end.',
    '    if (changedAny) {',
    '      var anySel = d.querySelector(".impact-ctrl, .priort-ctrl");',
    '      if (anySel) anySel.dispatchEvent(new Event("change", { bubbles: true }));',
    '    }',
    '  });',
    '}',
    '',
    '// ---- Layout-dropdown state (per result) -----------------------------',
    '// Capture which layout-type panel (Dynamic / Gravity / Charge /',
    '// Hierarchy) is currently active for each result accordion. The visible',
    '// .type-panel inside an accordion is the one whose inline display',
    '// style is "flex" (or not "none"). Stored as { rid: panelId }, where',
    '// rid is the accordion key (e.g. "r1") and panelId is the type-panel\\u2019s',
    '// DOM id (e.g. "r1_gravity").',
    'function captureLayoutSelections() {',
    '  var sel = {};',
    '  document.querySelectorAll("details.result-accordion").forEach(function(acc) {',
    '    var firstTP = acc.querySelector(".type-panel");',
    '    if (!firstTP) return;',
    '    // rid is the prefix of the first type-panel id before the underscore.',
    '    var idPrefix = firstTP.id.split("_")[0];',
    '    var visible = Array.from(acc.querySelectorAll(".type-panel")).find(function(p) {',
    '      var disp = p.style.display;',
    '      return disp !== "none" && disp !== "";',
    '    }) || firstTP;',
    '    sel[idPrefix] = visible.id;',
    '  });',
    '  return sel;',
    '}',
    '',
    '// Restore the saved layout-type panel per accordion. Calls switchType()',
    '// which (a) hides every type-panel in the accordion, (b) shows the named',
    '// one, and (c) syncs the layout-dropdown\\u2019s pre-selected option (each',
    '// type-panel carries its own copy of the dropdown with its own type',
    '// already selected). Silently no-ops if the saved panel id doesn\\u2019t',
    '// exist in this report (e.g. a different set of `types` was passed).',
    'function restoreLayoutSelections(saved) {',
    '  if (!saved) return;',
    '  Object.keys(saved).forEach(function(rid) {',
    '    var panelId = saved[rid];',
    '    if (!document.getElementById(panelId)) return;',
    '    try { switchType(rid, panelId); } catch(e) {}',
    '  });',
    '}',
    '',
    '// Per-accordion Download Report button. Reads the prebaked .xlsx bytes',
    '// from a sibling <script type="application/octet-stream"> element inside',
    '// the accordion, decodes base64 to a Blob, and triggers a download.',
    'function downloadAccordionReport(btn) {',
    '  var id = btn.getAttribute("data-xlsx-id");',
    '  var filename = btn.getAttribute("data-filename") || "report.xlsx";',
    '  var script = document.getElementById(id);',
    '  if (!script) { alert("Embedded report not found."); return; }',
    '  var b64 = (script.textContent || "").trim();',
    '  if (!b64) { alert("Embedded report is empty."); return; }',
    '  try {',
    '    var binStr = atob(b64);',
    '    var len = binStr.length;',
    '    var bytes = new Uint8Array(len);',
    '    for (var i = 0; i < len; i++) bytes[i] = binStr.charCodeAt(i);',
    '    var blob = new Blob([bytes], {',
    '      type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"',
    '    });',
    '    var url = URL.createObjectURL(blob);',
    '    var a = document.createElement("a");',
    '    a.href = url; a.download = filename;',
    '    document.body.appendChild(a); a.click(); document.body.removeChild(a);',
    '    setTimeout(function() { URL.revokeObjectURL(url); }, 1000);',
    '  } catch(e) {',
    '    alert("Could not download report: " + e.message);',
    '  }',
    '}',
    '',
    'function saveAllLayouts() {',
    '  // Tell every network iframe to deselect any active node before we',
    '  // serialize. Selection-time color mutations from highlightNearest are',
    '  // already filtered out in the iframe (originalNodeColors cache), but',
    '  // this also makes the loaded-back visual state match what was saved.',
    '  document.querySelectorAll(".tab-panel iframe").forEach(function(iframe) {',
    '    try { iframe.contentWindow.postMessage({ type: "deselectAll" }, "*"); } catch(e) {}',
    '  });',
    '  var payload = {',
    '    panels: snapshotStore,',
    '    legendEdits: legendEdits,',
    '    nodeLabelEdits: nodeLabelEdits,',
    '    dashboards: captureDashboardStates(),',
    '    layouts: captureLayoutSelections()',
    '  };',
    '  var json = JSON.stringify(payload, null, 2);',
    '  var blob = new Blob([json], { type: "application/json" });',
    paste0('  ', save_download),
    '}',
    '',
    'function mergeEditsIntoSnapshot(data, rName) {',
    '  var rEdits = legendEdits[rName] || {};',
    '  if (!data || !data.keyLabels || Object.keys(rEdits).length === 0) return data;',
    '  var copy = JSON.parse(JSON.stringify(data));',
    '  copy.keyLabels.forEach(function(item) {',
    '    if (rEdits[item.color]) item.label = rEdits[item.color];',
    '  });',
    '  return copy;',
    '}',
    '',
    'function sendSnapshotToPanel(panel) {',
    '  var key = panelKey(panel);',
    '  var data = window.pendingLoads[key];',
    '  if (!data) return;',
    '  var iframe = panel.querySelector("iframe");',
    '  if (!iframe) return;',
    '  try {',
    '    var rName = panel.getAttribute("data-result") || "_default";',
    '    var merged = mergeEditsIntoSnapshot(data, rName);',
    '    iframe.contentWindow.postMessage({ type: "applyReportLoad", snapshot: merged }, "*");',
    '  } catch(ex) {}',
    '}',
    '',
    'function loadAllLayouts(input) {',
    '  var file = input.files[0];',
    '  if (!file) return;',
    '  var reader = new FileReader();',
    '  reader.onload = function(e) {',
    '    try {',
    '      var parsed = JSON.parse(e.target.result);',
    '      var savedPanels = parsed.panels || {};',
    '',
    '      if (parsed.legendEdits) {',
    '        Object.keys(parsed.legendEdits).forEach(function(key) {',
    '          legendEdits[key] = parsed.legendEdits[key];',
    '        });',
    '      }',
    '      if (parsed.nodeLabelEdits) {',
    '        Object.keys(parsed.nodeLabelEdits).forEach(function(key) {',
    '          nodeLabelEdits[key] = parsed.nodeLabelEdits[key];',
    '        });',
    '      }',
    '',
    '      Object.keys(savedPanels).forEach(function(nsKey) {',
    '        window.pendingLoads[nsKey] = savedPanels[nsKey];',
    '      });',
    '',
    '      document.querySelectorAll(".tab-panel[data-result]").forEach(function(panel) {',
    '        sendSnapshotToPanel(panel);',
    '      });',
    '',
    '      // Restore dashboard control state for Impact and Prioritization tabs.',
    '      // Tolerant of older save files: missing key just leaves dashboards alone.',
    '      restoreDashboardStates(parsed.dashboards);',
    '',
    '      // Restore the per-accordion Layout dropdown selection (which',
    '      // layout-type panel — Dynamic / Gravity / Charge / Hierarchy —',
    '      // is currently active). Tolerant of older save files (missing',
    '      // key = no-op) and of layout-type sets that differ from the',
    '      // saved file (unknown panel ids are skipped).',
    '      restoreLayoutSelections(parsed.layouts);',
    '',
    '      // Push the restored label/community renames into the Impact,',
    '      // Community Impacts and Prioritization tables + prio chart',
    '      // (the network iframes get them via the panel snapshots above;',
    '      // the dashboards need an explicit re-apply on load).',
    '      ndrApplyAllImpactEdits();',
    '',
    '    } catch(err) {',
    '      alert("Invalid layout file.");',
    '    }',
    '  };',
    '  reader.readAsText(file);',
    '  input.value = "";',
    '}',
    '',
    '/* --- lazy srcdoc iframe initialization (self_contained mode) --- */',
    '/* only init iframes in open accordions; defer closed ones until toggled */',
    'document.addEventListener("DOMContentLoaded", function() {',
    '  if (typeof __sharedDepsB64 === "undefined" || !__sharedDepsB64) return;',
    '  var sharedBytes = Uint8Array.from(atob(__sharedDepsB64), function(c) { return c.charCodeAt(0); });',
    '  var sharedDeps = new TextDecoder().decode(sharedBytes);',
    '',
    '  function initIframes(container) {',
    '    container.querySelectorAll(".iframe-wrap[data-widget]").forEach(function(wrap) {',
    '      var wb64 = wrap.getAttribute("data-widget");',
    '      var wBytes = Uint8Array.from(atob(wb64), function(c) { return c.charCodeAt(0); });',
    '      var widgetHtml = new TextDecoder().decode(wBytes);',
    '      var fullHtml = widgetHtml.replace("<!--SHARED_DEPS-->", sharedDeps);',
    '      wrap.querySelector("iframe").srcdoc = fullHtml;',
    '      wrap.removeAttribute("data-widget");',
    '    });',
    '  }',
    '',
    '  // init iframes in accordions that are already open',
    '  document.querySelectorAll("details[open]").forEach(function(d) {',
    '    initIframes(d);',
    '  });',
    '',
    '  // lazy-load: init iframes when a closed accordion is opened',
    '  document.querySelectorAll("details.result-accordion").forEach(function(d) {',
    '    d.addEventListener("toggle", function() {',
    '      if (d.open) initIframes(d);',
    '    });',
    '  });',
    '});'
  ), collapse = "\n")
}

