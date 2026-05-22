#' append_bn_prioritize_guide
#'
#' @description Appends a client-friendly "Guide" sheet to an existing
#'   \code{bn_prioritize_write()} workbook. Mirrors the format and tone of
#'   \code{append_bn_impact_guide()}: a short client-facing section followed
#'   by a technical appendix for statisticians. Added as the final tab.
#'
#' @param wb An openxlsx workbook object.
#' @param dv_display Character or NULL. Display name of the DV for the header.
#' @param has_brands Logical. Whether a Focus (brand) control exists.
#' @param has_weights Logical. Whether a Weight control exists.
#' @param has_subgroups Logical. Whether a Subgroup control exists.
#' @param has_strategy Logical. Whether multiple strategies (lift/max) exist.
#' @param has_community Logical. Whether community labels appear in the table.
#' @param sig_threshold Numeric. P-value threshold for the "significant"
#'   colour band.
#' @param marginal_threshold Numeric. P-value threshold for the "marginal"
#'   colour band.
#' @param min_base_for_boot Integer. Minimum sample size used for the base /
#'   warning feedback.
#' @param boot_applied Logical. Whether parameter-bootstrap replicates were
#'   actually run (i.e., \code{n_boot_final > 1}).
#' @param n_boot_final Integer or NULL. Number of bootstrap replicates.
#' @param noise_tail Numeric or NULL. Fraction of tail steps used to estimate
#'   the noise floor.
#' @param threshold Numeric or NULL. Early-stopping threshold (fraction).
#' @param prioritize_display Character or NULL. Initial value of the
#'   prioritization Display dropdown — \code{"Point Change"} or \code{"\% Change"}.
#'   NULL (default) auto-detects from DV type: dichotomous -> "Point Change",
#'   continuous -> "% Change".
#' @param add_prioritization_pvalue Logical. When TRUE AND the bootstrap
#'   actually ran (\code{boot_applied}), the guide includes the
#'   "Bootstrap p-values" and "Why compare to a noise floor" technical
#'   sections. When FALSE (default) or when the bootstrap didn't run, both
#'   sections are omitted (the reader never sees a p-value column either).
#'
#' @return The modified workbook object (invisibly).
#'
#' @keywords internal
append_bn_prioritize_guide <- function(
    wb,
    dv_display = NULL,
    has_brands = FALSE,
    has_weights = FALSE,
    has_subgroups = FALSE,
    has_strategy = FALSE,
    has_community = FALSE,
    sig_threshold = 0.05,
    marginal_threshold = 0.10,
    min_base_for_boot = 100,
    boot_applied = FALSE,
    n_boot_final = NULL,
    noise_tail = NULL,
    threshold = NULL,
    impact_shift_type = "headroom",
    meta = list(),
    add_prioritization_pvalue = FALSE
) {

  # Default lift now lives on result$meta$lift (set by bn_finalize_network).
  # Caller may pass it via `meta`; fall back to 0.10 when absent.
  lift <- meta[["lift"]] %||% 0.10

  # Bootstrap p-value gating — used in two places (Reading the table cols
  # and Technical Appendix bootstrap section). Compute up front so both
  # call sites share a single condition.
  show_pvalue_section <- isTRUE(add_prioritization_pvalue) && isTRUE(boot_applied)

  guide_sheet <- "Guide"
  openxlsx::addWorksheet(wb, guide_sheet, tabColour = "#2E75B6", gridLines = FALSE)

  # Styles — identical to append_bn_impact_guide for cross-workbook consistency
  s_title    <- openxlsx::createStyle(textDecoration = "bold", fontSize = 18)
  s_subtitle <- openxlsx::createStyle(textDecoration = c("bold", "italic"),
                                      fontSize = 14)
  s_h2       <- openxlsx::createStyle(textDecoration = "bold", halign = "center",
                                      wrapText = TRUE,
                                      fgFill = "#D9D9D9")
  s_body     <- openxlsx::createStyle(fontSize = 11, wrapText = TRUE,
                                      valign = "top")
  s_tbl_hdr  <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                      halign = "left", valign = "top",
                                      wrapText = TRUE)
  s_tbl_body <- openxlsx::createStyle(fontSize = 11, halign = "left",
                                      valign = "top", wrapText = TRUE)
  s_tech_body <- openxlsx::createStyle(fontSize = 11, wrapText = TRUE,
                                       valign = "top")
  s_tech_hdr  <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                       valign = "top", wrapText = TRUE)

  col_left  <- 2L
  col_right <- 3L
  col_end   <- 3L

  r <- 2L
  section_start <- NULL

  .close_section <- function() {
    if (!is.null(section_start) && r - 2 >= section_start) {
      oxl_outer_box(wb, guide_sheet,
        row_start = section_start,
        row_end = r - 2,
        col_start = col_left,
        col_end = col_end,
        borderStyle = "medium")
    }
    section_start <<- NULL
  }

  # -- Header (title + subtitle) — no borders --
  openxlsx::writeData(wb, guide_sheet,
    "How to Read This Network Drivers Workbook",
    startRow = r, startCol = col_left)
  openxlsx::addStyle(wb, guide_sheet, s_title, rows = r,
    cols = col_left:col_end, gridExpand = TRUE, stack = TRUE)
  openxlsx::mergeCells(wb, guide_sheet, rows = r, cols = col_left:col_end)
  openxlsx::setRowHeights(wb, guide_sheet, rows = r, heights = 30)
  r <- r + 1L

  openxlsx::writeData(wb, guide_sheet,
    "A quick guide to the dashboard, the controls, and how to interpret the numbers.",
    startRow = r, startCol = col_left)
  openxlsx::addStyle(wb, guide_sheet, s_subtitle, rows = r,
    cols = col_left:col_end, gridExpand = TRUE, stack = TRUE)
  openxlsx::mergeCells(wb, guide_sheet, rows = r, cols = col_left:col_end)
  r <- r + 2L

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------
  .write_h2 <- function(text) {
    .close_section()
    openxlsx::writeData(wb, guide_sheet, text,
      startRow = r, startCol = col_left)
    openxlsx::addStyle(wb, guide_sheet, s_h2,
      rows = r, cols = col_left:col_end,
      gridExpand = TRUE, stack = TRUE)
    openxlsx::mergeCells(wb, guide_sheet, rows = r, cols = col_left:col_end)
    openxlsx::setRowHeights(wb, guide_sheet, rows = r, heights = 22)
    section_start <<- r
    r <<- r + 1L
  }

  .write_labelled <- function(df) {
    openxlsx::writeData(wb, guide_sheet, names(df)[1],
      startRow = r, startCol = col_left)
    openxlsx::writeData(wb, guide_sheet, names(df)[2],
      startRow = r, startCol = col_right)
    openxlsx::addStyle(wb, guide_sheet, s_tbl_hdr,
      rows = r, cols = col_left:col_right,
      gridExpand = TRUE, stack = TRUE)
    r <<- r + 1L

    for (i in seq_len(nrow(df))) {
      openxlsx::writeData(wb, guide_sheet, df[i, 1],
        startRow = r, startCol = col_left)
      openxlsx::writeData(wb, guide_sheet, df[i, 2],
        startRow = r, startCol = col_right)
      openxlsx::addStyle(wb, guide_sheet, s_tbl_body,
        rows = r, cols = col_left:col_right,
        gridExpand = TRUE, stack = TRUE)
      r <<- r + 1L
    }

    r <<- r + 1L
  }

  .write_para <- function(text) {
    openxlsx::writeData(wb, guide_sheet, text,
      startRow = r, startCol = col_right)
    openxlsx::addStyle(wb, guide_sheet, s_body,
      rows = r, cols = col_right, stack = TRUE)
    r <<- r + 2L
  }

  .write_tech <- function(label, body) {
    openxlsx::writeData(wb, guide_sheet, label,
      startRow = r, startCol = col_left)
    openxlsx::writeData(wb, guide_sheet, body,
      startRow = r, startCol = col_right)
    openxlsx::addStyle(wb, guide_sheet, s_tech_hdr,
      rows = r, cols = col_left, stack = TRUE)
    openxlsx::addStyle(wb, guide_sheet, s_tech_body,
      rows = r, cols = col_right, stack = TRUE)
    r <<- r + 2L
  }

  # ---------------------------------------------------------------------------
  # Section 1: What's in this workbook
  # ---------------------------------------------------------------------------
  .write_h2("What's in this workbook")

  tabs_df <- data.frame(
    Tab = "Prioritization",
    Description = paste(
      "An ordered list of attributes ranked by how much each one adds",
      "to the outcome when layered on top of the ones already selected.",
      "The chart to the right shows the cumulative effect build-up."
    ),
    stringsAsFactors = FALSE
  )
  .write_labelled(tabs_df)

  # ---------------------------------------------------------------------------
  # Section 2: How to use the dashboard
  # ---------------------------------------------------------------------------
  .write_h2("How to use the dashboard")
  .write_para(
    "The dropdowns at the top of the Prioritization tab change what you see. Pick any combination; the table and chart below update immediately."
  )

  ctrl_df <- data.frame(
    Control = character(),
    Description = character(),
    stringsAsFactors = FALSE
  )
  if (has_strategy) {
    lift_pct <- round((meta[["lift"]] %||% 0.10) * 100, 1)
    lift_explainer <- switch(impact_shift_type %||% "headroom",
      "headroom"     = paste0("'Moderate Lift' closes ", lift_pct, "% of each attribute's gap to its top level — every attribute moves the same fraction of its own headroom, so cross-scale rankings stay comparable"),
      "proportional" = paste0("'Moderate Lift' shifts each attribute's mean by ", lift_pct, "% of its current value"),
      "absolute"     = paste0("'Moderate Lift' adds ", round(lift, 2), " scale points to each attribute's mean"),
      "rangeshift"   = paste0("'Moderate Lift' shifts each attribute's mean by ", lift_pct, "% of its scale's range (max minus min)"),
      "range"        = paste0("'Moderate Lift' shifts each attribute's mean by ", lift_pct, "% of its scale's range (max minus min)"),
      paste0("'Moderate Lift' shifts each attribute's distribution by ", lift_pct, "%")
    )
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Analysis",
      Description = paste0(
        lift_explainer,
        " and reads the change in the outcome. 'Maximum Lift' sets each attribute to its highest observed level — a best-case-scenario read."
      ),
      stringsAsFactors = FALSE
    ))
  }
  if (has_subgroups) {
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Subgroup",
      Description = "Limits the prioritization to a specific segment of respondents. 'Total' uses everyone.",
      stringsAsFactors = FALSE
    ))
  }
  if (has_brands) {
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Focus",
      Description = "'Market' uses the whole sample within the chosen subgroup. Selecting a brand re-runs the prioritization using only that brand's respondents, so the ordering reflects what matters for that brand's customers.",
      stringsAsFactors = FALSE
    ))
  }
  if (has_weights) {
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Weight",
      Description = "Toggles between weighted and unweighted results. Weighting influences the observed distribution of each attribute, which feeds the lift calculation.",
      stringsAsFactors = FALSE
    ))
  }
  if (nrow(ctrl_df) > 0) .write_labelled(ctrl_df)

  # ---------------------------------------------------------------------------
  # Section 3: Reading the results table
  # ---------------------------------------------------------------------------
  .write_h2("Reading the results table")

  cols_df <- data.frame(
    Column = character(),
    Description = character(),
    stringsAsFactors = FALSE
  )
  cols_df <- rbind(cols_df, data.frame(
    Column = "Step",
    Description = "The order in which attributes were added by the greedy search. Step 1 is the single attribute with the largest effect on its own; step 2 is the attribute that adds the most on top of step 1, and so on.",
    stringsAsFactors = FALSE
  ))
  cols_df <- rbind(cols_df, data.frame(
    Column = "Variable / Label",
    Description = "The attribute added at this step. The Label is the human-readable version from the dictionary.",
    stringsAsFactors = FALSE
  ))
  if (has_community) {
    cols_df <- rbind(cols_df, data.frame(
      Column = "Community",
      Description = "The thematic group the attribute belongs to. Useful for seeing whether the top steps concentrate in one theme or span several.",
      stringsAsFactors = FALSE
    ))
  }
  cols_df <- rbind(cols_df, data.frame(
    Column = "Marginal Gain",
    Description = "How much the DV Estimate moved compared to the previous step. This is the incremental value the current attribute adds on top of what's already been layered in.",
    stringsAsFactors = FALSE
  ))
  cols_df <- rbind(cols_df, data.frame(
    Column = "Marginal Gain %",
    Description = "The marginal gain expressed as a percentage of the previous step's DV Estimate. Used for the early-stopping rule: the search stops once a step's relative gain falls below the threshold.",
    stringsAsFactors = FALSE
  ))
  # p-value row only when the column is actually shown in the dashboard
  # AND the bootstrap was run. Mirrors the gating in the technical-appendix
  # bootstrap section.
  if (isTRUE(show_pvalue_section)) {
    cols_df <- rbind(cols_df, data.frame(
      Column = "p-value",
      Description = paste0(
        "How confident we are that this step's gain is real signal rather than noise. Smaller is stronger; cells are shaded green below ",
        sig_threshold, ", orange below ", marginal_threshold,
        ", and red otherwise (the step's gain isn't statistically significant). A blank p-value means the bootstrap wasn't applied to this slice (typically because the sample size was too small)."
      ),
      stringsAsFactors = FALSE
    ))
  }
  .write_labelled(cols_df)

  # ---------------------------------------------------------------------------
  # Section 4: Chart
  # ---------------------------------------------------------------------------
  .write_h2("The cumulative-effect chart")
  .write_para(
    "The chart on the right of the Prioritization tab visualises the same data as the table. Each bar shows one step's marginal gain, stacked on top of the previous step. The top of the final bar is the DV Estimate after all selected steps have been applied. It's the fastest way to see which steps contribute most to the total build-up."
  )
  .write_para(
    "The chart's Y-axis is anchored at 0 so that the cumulative gain (or cumulative gain %) bars always start from baseline. Earlier versions anchored the axis at baseline_DV - margin, which clipped bars for continuous DVs."
  )
  .write_para(
    "Bar values are formatted with one decimal when below 1% (e.g., '0.3%') and as integers when at or above 1% (e.g., '12%')."
  )
  .write_para(
    "The Display dropdown defaults to 'Point Change' for dichotomous DVs (probability points read naturally) and '% Change' for continuous DVs (% of baseline reads better). Override with the prioritize_display parameter."
  )
  .write_para(
    "By default the p-value column is hidden (add_prioritization_pvalue = FALSE). Pass add_prioritization_pvalue = TRUE to surface it."
  )

  # ---------------------------------------------------------------------------
  # Section 5: Notes
  # ---------------------------------------------------------------------------
  .write_h2("Notes on reading the numbers")
  .write_para(paste0(
    "Brand-by-subgroup slices with fewer than ", min_base_for_boot,
    " respondents are flagged next to the Focus dropdown with a red warning. The table is left blank for those slices because the base is too small to support a reliable prioritization."
  ))
  if (!is.null(threshold)) {
    .write_para(paste0(
      "The greedy search stops when a step's marginal gain drops below ",
      round(threshold * 100, 2), "% of the current DV estimate. Steps past that point add essentially nothing and are omitted."
    ))
  }

  # ---------------------------------------------------------------------------
  # Technical appendix
  # ---------------------------------------------------------------------------
  r <- r + 1L
  .write_h2("Technical Appendix")

  .write_tech(
    "Greedy forward selection",
    paste(
      "The prioritization is built by forward selection. In round 1, every",
      "attribute is evaluated on its own — for each attribute, the chosen",
      "strategy is applied (lift or max) and the expected outcome is",
      "computed by exact inference on the subgroup's fitted network. The",
      "attribute with the largest resulting outcome becomes step 1. In",
      "subsequent rounds, every remaining attribute is layered on top of",
      "the currently-selected set and re-evaluated; the one that produces",
      "the largest incremental outcome becomes the next step. The search",
      "stops when the fractional marginal gain falls below the",
      "early-stopping threshold (or when a maximum step count is reached)."
    )
  )

  .write_tech(
    "Strategy: Lift",
    paste0(
      "For each attribute X, the observed (optionally weighted) frequency",
      " distribution of X is shifted by an exponential tilt that raises its",
      " mean by ", round((meta[["lift"]] %||% 0.10) * 100, 1), "% (proportional) or by a fixed",
      " number of scale points (absolute). Under this shifted distribution",
      " of X — and the shifted distributions of every previously-selected",
      " attribute — the network produces a new expected outcome. This",
      " captures what would happen if the attribute's distribution moved",
      " upward realistically, rather than being set to an extreme."
    )
  )

  .write_tech(
    "Strategy: Max",
    paste(
      "Each selected attribute is hard-clamped to its maximum observed",
      "level (hard evidence) and the network is re-queried. This is a",
      "best-case-scenario read: the theoretical ceiling of the outcome if",
      "every selected attribute were at its top level."
    )
  )

  .write_tech(
    "Conditional outcome",
    paste(
      "In both strategies, the outcome is obtained by exact Bayesian",
      "inference on the subgroup's fitted network. Depending on the",
      "metric, this is either the probability of the top DV level or the",
      "expected value of the DV across all levels. The network structure",
      "is held fixed across the search; only the evidence changes."
    )
  )

  .write_tech(
    "Weighting",
    paste(
      "When a weight variable is provided, the observed frequency",
      "distribution used in the lift strategy is computed as the sum of",
      "weights within each level rather than the raw count. Weighting",
      "therefore propagates into the lift calculations; it does not affect",
      "the conditional outcome query, which comes from the network's",
      "conditional probability tables. Those tables are fitted as",
      "unweighted Bayesian posteriors under a Dirichlet prior."
    )
  )

  # Bootstrap section — only emit when the p-value column is exposed in
  # the dashboard AND the bootstrap actually ran. Uses the
  # `show_pvalue_section` flag computed at function top.
  if (show_pvalue_section) {
    .write_tech(
      "Bootstrap p-values",
      paste0(
        "After the forward selection completes, significance of each step",
        " is assessed by bootstrapping the rows. ", n_boot_final,
        " replicates were drawn with replacement from the input data. For",
        " each replicate the conditional probability tables were re-fitted",
        " on the fixed network structure, and the already-selected",
        " attributes were re-queried in the same order to produce a new",
        " marginal gain per step. For each step, the p-value reported is",
        " the fraction of replicates where that step's gain was at or below",
        " a noise-floor estimate — the average gain across the last ",
        if (!is.null(noise_tail)) paste0(round(noise_tail * 100, 0), "%") else "fraction",
        " of steps (where additional value is expected to be indistinguishable",
        " from noise). A small p-value means the step consistently beats noise",
        " across resamples."
      )
    )

    .write_tech(
      "Why compare to a noise floor (not zero)",
      paste(
        "A step's marginal gain can be positive simply because any new",
        "evidence adds some information. Comparing to zero would flag almost",
        "every step as 'significant'. The noise floor — the average gain of",
        "the tail steps, where the search has effectively run out of real",
        "signal — gives a more useful bar: a step is meaningful only when it",
        "outperforms what the search looks like once it's scraping the bottom."
      )
    )
  }

  .write_tech(
    "Base-size threshold",
    paste0(
      "Brand-by-subgroup slices with fewer than ", min_base_for_boot,
      " respondents are skipped: no prioritization is run, no bootstrap is",
      " applied, and no table rows are produced. The Focus dropdown still",
      " lists the brand so the user sees the base and the warning. The",
      " threshold guards against prioritizations driven by Bayesian prior",
      " probabilities rather than observed data."
    )
  )

  # Close the final section + column widths
  .close_section()

  openxlsx::setColWidths(wb, guide_sheet, cols = 1, widths = 3)
  openxlsx::setColWidths(wb, guide_sheet, cols = col_left, widths = 28)
  openxlsx::setColWidths(wb, guide_sheet, cols = col_right, widths = 95)

  invisible(wb)
}
