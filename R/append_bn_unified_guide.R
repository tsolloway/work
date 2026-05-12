#' append_bn_unified_guide
#'
#' @description Appends a single unified Guide sheet to a combined workbook
#'   produced by \code{bn_write()}. Combines sections from the impact and
#'   prioritization guides into one tab. Format and tone match
#'   \code{append_bn_impact_guide()} and \code{append_bn_prioritize_guide()}.
#'
#' @param wb An openxlsx workbook object.
#' @param impacts The impacts result list (from \code{bn_impacts()}).
#' @param prioritizations The prioritizations result list (from
#'   \code{bn_prioritizations()}).
#' @param dv_display Character or NULL. Display DV name for the header.
#' @param has_weights Logical. Whether weighted variants exist.
#' @param has_community Logical. Whether community results exist.
#' @param has_simulator Logical. Whether the simulator is present.
#' @param has_brands Logical. Whether a brand (Focus) dimension exists.
#' @param wb_type \code{"dynamic"} or \code{"standard"}.
#' @param sim_dv_only Logical. Simulator restricted to DV only.
#' @param sig_threshold,marginal_threshold P-value bands.
#' @param min_base_for_lift,min_base_for_sim,min_base_for_boot Base thresholds.
#' @param impact_outcome_display Character or NULL. Initial value of the
#'   Attribute Drivers Outcome dropdown — \code{"Point Change"} (raw DV-unit
#'   delta) or \code{"\% Change"} (scaled by baseline DV).
#' @param prioritize_display Character or NULL. Initial value of the
#'   prioritization Display dropdown — \code{"Point Change"} or \code{"\% Change"}.
#'   NULL (default) auto-detects from DV type: dichotomous -> "Point Change",
#'   continuous -> "% Change".
#' @param add_prioritization_pvalue Logical. When TRUE, the prioritization
#'   table includes its p-value column AND the guide includes the
#'   "Prioritization bootstrap p-values" technical section. When FALSE
#'   (default), both the column and the explanatory section are omitted.
#' @param add_impacts_by_battery Logical. When TRUE (default), per-battery
#'   "AD - <name>" dashboards are emitted alongside the main Attribute Drivers
#'   tab. When FALSE, only the main tab is written.
#'
#' @return The modified workbook object (invisibly).
#'
#' @keywords internal
append_bn_unified_guide <- function(
    wb,
    impacts,
    prioritizations,
    dv_display = NULL,
    has_weights = FALSE,
    has_community = FALSE,
    has_simulator = FALSE,
    has_brands = FALSE,
    wb_type = c("dynamic", "standard"),
    sim_dv_only = FALSE,
    sig_threshold = 0.05,
    marginal_threshold = 0.10,
    min_base_for_lift = 100,
    min_base_for_sim = 100,
    min_base_for_boot = 100,
    impact_shift_type = "headroom",
    add_prioritization_pvalue = FALSE
) {

  # Detect whether bootstrap p-values exist in the prioritization data —
  # any non-NA p_value across any greedy result tibble means the bootstrap
  # was actually run.
  boot_applied_in_data <- FALSE
  if (!is.null(prioritizations)) {
    for (nm in setdiff(names(prioritizations), "meta")) {
      tbl <- prioritizations[[nm]]
      if (is.data.frame(tbl) && "p_value" %in% names(tbl) &&
          any(!is.na(tbl[["p_value"]]))) {
        boot_applied_in_data <- TRUE
        break
      }
      # Subgroup-nested list form: walk one level deeper.
      if (is.list(tbl) && !is.data.frame(tbl)) {
        for (sub in tbl) {
          if (is.list(sub) && !is.null(sub$tbl) &&
              "p_value" %in% names(sub$tbl) &&
              any(!is.na(sub$tbl[["p_value"]]))) {
            boot_applied_in_data <- TRUE
            break
          }
        }
        if (boot_applied_in_data) break
      }
    }
  }
  # Show the p-value technical section only when bootstrap was actually
  # run AND the dashboard exposes the column (add_prioritization_pvalue =
  # TRUE). When the column is hidden, the guide reader will never see a
  # p-value, so no need to explain.
  show_priort_pvalue_section <- isTRUE(add_prioritization_pvalue) &&
    isTRUE(boot_applied_in_data)

  # Default lift now lives on result$meta$lift (set by bn_finalize_network).
  # Pull from impacts/prioritizations meta when available, fall back to 0.10.
  meta <- impacts$meta %||% prioritizations$meta %||% list()
  lift <- meta[["lift"]] %||% 0.10

  wb_type <- match.arg(wb_type)

  guide_sheet <- "Guide"
  openxlsx::addWorksheet(wb, guide_sheet, tabColour = "#2E75B6", gridLines = FALSE)

  # Styles — same as the individual guides
  s_title    <- openxlsx::createStyle(textDecoration = "bold", fontSize = 18)
  s_subtitle <- openxlsx::createStyle(textDecoration = c("bold", "italic"),
                                      fontSize = 14)
  s_h2       <- openxlsx::createStyle(textDecoration = "bold", halign = "center",
                                      wrapText = TRUE, fgFill = "#D9D9D9")
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
    "A quick guide to the tabs, the controls, and how to interpret the numbers.",
    startRow = r, startCol = col_left)
  openxlsx::addStyle(wb, guide_sheet, s_subtitle, rows = r,
    cols = col_left:col_end, gridExpand = TRUE, stack = TRUE)
  openxlsx::mergeCells(wb, guide_sheet, rows = r, cols = col_left:col_end)
  r <- r + 2L

  # Helpers (same as in the individual guides)
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
  # What's in this workbook
  # ---------------------------------------------------------------------------
  .write_h2("What's in this workbook")
  tabs_df <- data.frame(
    Tab = "Attribute Drivers",
    Description = "Ranks individual attributes by how strongly they influence the outcome. The main tab indexing across all attributes.",
    stringsAsFactors = FALSE
  )
  tabs_df <- rbind(tabs_df, data.frame(
    Tab = "AD - <battery> (optional)",
    Description = "Per-battery tabs (one per defined battery, and one per battery group), each indexed WITHIN that battery's IVs.",
    stringsAsFactors = FALSE
  ))
  if (has_community) {
    tabs_df <- rbind(tabs_df, data.frame(
      Tab = "Community Drivers",
      Description = "Ranks groups of related attributes (communities) by their joint influence on the outcome.",
      stringsAsFactors = FALSE
    ))
  }
  if (has_simulator) {
    tabs_df <- rbind(tabs_df, data.frame(
      Tab = "Simulator",
      Description = "Interactive 'what-if' tool. Choose a variable and see how changing it would flow through the network to other variables.",
      stringsAsFactors = FALSE
    ))
  }
  tabs_df <- rbind(tabs_df, data.frame(
    Tab = "Prioritization",
    Description = "An ordered list of attributes ranked by how much each one adds to the outcome when layered on top of the ones already selected. Includes a cumulative-effect chart.",
    stringsAsFactors = FALSE
  ))
  .write_labelled(tabs_df)

  # ---------------------------------------------------------------------------
  # How to use the dashboards
  # ---------------------------------------------------------------------------
  if (wb_type == "dynamic") {
    .write_h2("How to use the dashboards")
    .write_para(
      "Each dashboard tab has dropdown controls at the top. Pick any combination; the table below (and chart where present) updates immediately."
    )

    ctrl_df <- data.frame(
      Control = "Assess (Attribute / Community Drivers)",
      Description = paste(
        "High-level lens selector. Three presets curate the Analysis and Shift Type controls into common analytical questions:",
        "'Current Impact' — Average Effect + % of Range shift. Answers: 'What is happening?'",
        "'Intervention Impact' — Effect at the configured fraction + % Toward Top. Answers: 'What should we do?'",
        "'Maximum Impact' — Best-vs-Worst Effect + % of Range. Answers: 'What is possible?'",
        "'Custom' — Unlocks the Analysis and Shift Type dropdowns for manual control.",
        "When a preset is selected, the Analysis and Shift Type cells grey out and display 'Fixed to X when Assess is Y' feedback indicating which combination is currently driving the table.",
        sep = " "
      ),
      stringsAsFactors = FALSE
    )
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Analysis (Attribute / Community Drivers)",
      Description = "Switches what the numbers represent: an effect at the current state ('Average Effect'), an effect at a defined intervention magnitude ('Effect at X'), the best-case-to-worst-case range ('Best-vs-Worst Effect'), or relationship-strength ('Explanatory Value').",
      stringsAsFactors = FALSE
    ))
    if (has_brands) {
      ctrl_df <- rbind(ctrl_df, data.frame(
        Control = "Focus",
        Description = "'Market' analyzes the overall market performance, while selecting a brand uses only that brand's performance.",
        stringsAsFactors = FALSE
      ))
    }
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Subgroup",
      Description = "Limits results to a specific segment of respondents. 'Total' uses everyone.",
      stringsAsFactors = FALSE
    ))
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Shift Type (Attribute / Community Drivers)",
      Description = paste(
        "Controls how each attribute's distribution is reshaped when an effect is computed. Four options:",
        "'% of Current Mean' — each attribute's average shifts by a percentage of where it currently sits. Attributes already rated high move more in absolute terms; low-rated attributes move less.",
        "'Fixed Step' — every attribute's average shifts by the same fixed amount on the scale, regardless of where it currently sits.",
        "'% Toward Top' — each attribute closes a percentage of the gap between its current average and the top of its scale. Attributes near the ceiling barely move; attributes with room to grow move more. Most natural when asking 'what if everyone got a little closer to the ideal?'.",
        "'% of Range' — each attribute moves by a percentage of its scale's full width (max minus min). Same absolute movement for every attribute, normalised to the scale.",
        sep = " "
      ),
      stringsAsFactors = FALSE
    ))
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Outcome (Attribute / Community Drivers)",
      Description = "Controls how impact values are formatted. '% Change' scales each value by baseline DV (relative read); 'Point Change' shows the raw DV-unit delta (absolute read).",
      stringsAsFactors = FALSE
    ))
    if (has_weights) {
      ctrl_df <- rbind(ctrl_df, data.frame(
        Control = "Weight",
        Description = "Toggles between weighted and unweighted results. Weighting influences effect-based metrics (Average Effect, Effect at X), not relationship-strength metrics (Best-vs-Worst Effect, Explanatory Value).",
        stringsAsFactors = FALSE
      ))
    }
    moderate_explainer <- switch(impact_shift_type %||% "headroom",
      "headroom"     = "'Moderate Lift (XX%)' closes XX% of each attribute's gap to its top level — every attribute moves the same fraction of its own headroom, so cross-scale rankings stay comparable — and reads the cumulative change in the outcome at each priority step.",
      "proportional" = "'Moderate Lift (XX%)' shifts each attribute's mean by XX% of its current value and reads the cumulative change in the outcome at each priority step.",
      "absolute"     = "'Moderate Lift (XX%)' adds XX scale points to each attribute's mean and reads the cumulative change in the outcome at each priority step.",
      "rangeshift"   = "'Moderate Lift (XX%)' shifts each attribute's mean by XX% of its scale's range (max minus min) and reads the cumulative change in the outcome at each priority step.",
      "range"        = "'Moderate Lift (XX%)' shifts each attribute's mean by XX% of its scale's range (max minus min) and reads the cumulative change in the outcome at each priority step.",
      "'Moderate Lift (XX%)' shifts each attribute's distribution by XX% and reads the cumulative change in the outcome at each priority step."
    )
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Analysis (Prioritization)",
      Description = paste(
        moderate_explainer,
        "'Maximum Lift' sets each attribute to its highest observed level (full saturation) — showing the upper bound the network could produce if every priority were taken to its top — and reads the cumulative outcome at each step. Use it to benchmark the maximum achievable change.",
        sep = " "
      ),
      stringsAsFactors = FALSE
    ))
    .write_labelled(ctrl_df)
  }

  # ---------------------------------------------------------------------------
  # Reading the driver tables
  # ---------------------------------------------------------------------------
  .write_h2("Reading the Attribute/Community Drivers tables")

  cols_df <- data.frame(
    Column = "Variable / Label",
    Description = "The attribute being measured.",
    stringsAsFactors = FALSE
  )
  if (has_community) {
    cols_df <- rbind(cols_df, data.frame(
      Column = "Community",
      Description = "The thematic group this attribute belongs to.",
      stringsAsFactors = FALSE
    ))
  }
  cols_df <- rbind(cols_df, data.frame(
    Column = "Index",
    Description = "A relative ranking score where the top attribute in view is anchored at 100.",
    stringsAsFactors = FALSE
  ))
  .write_labelled(cols_df)

  .write_para(
    "The Total Impact row aggregates each attribute's absolute impact into one summary. Its format follows the Outcome dropdown: '3.5%' when Outcome = % Change, '0.10' when Outcome = Point Change."
  )

  # ---------------------------------------------------------------------------
  # Reading the Prioritization table
  # ---------------------------------------------------------------------------
  .write_h2("Reading the Prioritization table")

  prior_cols_df <- data.frame(
    Column = "Step",
    Description = "The order in which attributes were added by the greedy search. Step 1 is the single attribute with the largest effect on its own; step 2 is the attribute that adds the most on top of step 1, and so on.",
    stringsAsFactors = FALSE
  )
  prior_cols_df <- rbind(prior_cols_df, data.frame(
    Column = "Variable / Label",
    Description = "The attribute added at this step.",
    stringsAsFactors = FALSE
  ))
  if (has_community) {
    prior_cols_df <- rbind(prior_cols_df, data.frame(
      Column = "Community",
      Description = "The thematic group the attribute belongs to.",
      stringsAsFactors = FALSE
    ))
  }
  prior_cols_df <- rbind(prior_cols_df, data.frame(
    Column = "Marginal Gain",
    Description = "How much the DV Estimate moved compared to the previous step — the incremental value this attribute adds.",
    stringsAsFactors = FALSE
  ))
  prior_cols_df <- rbind(prior_cols_df, data.frame(
    Column = "Marginal Gain %",
    Description = "Marginal gain as a percentage of the previous step's DV Estimate. Used for the early-stopping rule.",
    stringsAsFactors = FALSE
  ))
  # p-value row only when the column is actually shown in the dashboard
  # AND the bootstrap was run. Mirrors the gating in the technical-appendix
  # bootstrap section.
  if (isTRUE(show_priort_pvalue_section)) {
    prior_cols_df <- rbind(prior_cols_df, data.frame(
      Column = "p-value",
      Description = paste0(
        "How confident we are this step's gain is real. Smaller is stronger; green below ",
        sig_threshold, ", yellow below ", marginal_threshold, "."
      ),
      stringsAsFactors = FALSE
    ))
  }
  .write_labelled(prior_cols_df)

  # ---------------------------------------------------------------------------
  # Cumulative-effect chart
  # ---------------------------------------------------------------------------
  .write_h2("The cumulative-effect chart (Prioritization tab)")
  .write_para(
    "Each bar shows one step's marginal gain stacked on top of the previous step's DV estimate. The top of the final bar is the expected outcome after all selected steps have been applied — the fastest way to see which steps contribute most."
  )
  .write_para(
    "The chart's Y-axis is anchored at 0 so that the cumulative gain (or cumulative gain %) bars always start from baseline. Earlier versions anchored the axis at baseline_DV - margin, which clipped bars for continuous DVs."
  )
  .write_para(
    "Bar values are formatted with one decimal when below 1% (e.g., '0.3%') and as integers when at or above 1% (e.g., '12%')."
  )
  .write_para(
    "The Display dropdown defaults to 'Point Change' for dichotomous DVs (probability points read naturally) and '% Change' for continuous DVs (% of baseline reads better). Override via the Display dropdown."
  )

  # ---------------------------------------------------------------------------
  # Simulator
  # ---------------------------------------------------------------------------
  if (has_simulator) {
    .write_h2("Using the Simulator")
    .write_para(
      "The Simulator lets you ask 'what if this attribute changed?' and see the expected effect on any other variable in the network (or just the DV when configured that way)."
    )

    sim_df <- data.frame(
      Step = "1. Subgroup",
      Description = "Pick the segment you want to simulate within.",
      stringsAsFactors = FALSE
    )
    sim_df <- rbind(sim_df, data.frame(
      Step = "2. Variable",
      Description = "The attribute you're changing (the driver).",
      stringsAsFactors = FALSE
    ))
    sim_df <- rbind(sim_df, data.frame(
      Step = "3. Mode",
      Description = "'Current Level' shows the expected outcome at each observed level of the variable. 'Percent Change' shifts the variable's distribution by a percentage and shows the downstream effect.",
      stringsAsFactors = FALSE
    ))
    if (has_brands) {
      sim_df <- rbind(sim_df, data.frame(
        Step = "4. Focus",
        Description = "Simulate using the whole market's performance or a specific brand's performance. Brands below the minimum sample size are flagged with a red warning and blank values.",
        stringsAsFactors = FALSE
      ))
    }
    .write_labelled(sim_df)
  }

  # ---------------------------------------------------------------------------
  # Notes
  # ---------------------------------------------------------------------------
  .write_h2("Notes on reading the numbers")
  .write_para(paste0(
    "Brand-by-subgroup slices with fewer than ", min_base_for_lift,
    " respondents are blanked in the Attribute / Community Drivers dashboards."
  ))
  if (has_simulator) {
    .write_para(paste0(
      "The Simulator applies a minimum base of ", min_base_for_sim,
      " respondents for brand slices — offending slices are flagged next to the Focus dropdown and return blank values."
    ))
  }
  .write_para(paste0(
    "In the Prioritization tab, brand-by-subgroup slices with fewer than ",
    min_base_for_boot,
    " respondents are flagged next to the Focus dropdown. No prioritization is run for those slices."
  ))

  # ---------------------------------------------------------------------------
  # Technical appendix — combined from both individual guides
  # ---------------------------------------------------------------------------
  r <- r + 1L
  .write_h2("Technical Appendix")

  .write_tech(
    "Model structure",
    paste(
      "A discrete Bayesian network is learned over the attributes and the",
      "dependent variable. Structure learning uses a score-based search",
      "(hill-climbing or tabu) with a penalized likelihood criterion (BIC or",
      "AIC). Given the learned structure, the conditional probability tables",
      "are estimated as Bayesian posterior means under a Dirichlet prior.",
      "Each subgroup model shares the same structure as the overall model",
      "but is re-fitted on the subgroup's rows. Factor levels that do not",
      "appear in the subgroup are removed so every level in the model is",
      "backed by observed data."
    )
  )

  .write_tech(
    "Best-vs-Worst",
    paste(
      "For each attribute X, Best-vs-Worst is the difference in the outcome",
      "between its best and worst observed level, under exact Bayesian",
      "inference on the fitted network. This captures the theoretical",
      "ceiling of the attribute's influence, independent of where",
      "respondents actually sit on X."
    )
  )

  .write_tech(
    "Probability Shift",
    paste(
      "The shift accounts for the observed distribution of respondents on",
      "the attribute. The observed frequency distribution p_obs is shifted",
      "by an exponential tilt that raises its mean by the requested",
      "percentage (proportional) or by a fixed number of scale points",
      "(absolute). The reported shift is the difference between the",
      "expected target under the shifted distribution and the expected",
      "target under the observed distribution. When a brand is specified,",
      "p_obs is computed from the brand's rows while the conditional",
      "target is shared across brands because the brand variable does not",
      "enter the network."
    )
  )

  .write_tech(
    "Weighting",
    paste(
      "When a weight variable is provided, the observed frequency",
      "distribution is computed as the sum of weights within each level",
      "rather than the raw count. Weighting therefore propagates into the",
      "shift-based metrics. It does not propagate into Best-vs-Worst or mutual",
      "information, which come from the network's conditional probability",
      "tables (fitted as unweighted Bayesian posteriors)."
    )
  )

  .write_tech(
    "Mutual information and p-values",
    paste(
      "For individual attributes, mutual information with the DV is",
      "computed from unconditional sample counts, and its significance is",
      "assessed with the standard chi-squared approximation to the",
      "likelihood-ratio test statistic. For communities, attributes are",
      "tested sequentially using a chained conditional mutual information",
      "decomposition; the likelihood-ratio test statistics and degrees of",
      "freedom are summed across steps and referred to a single chi-squared",
      "distribution. Optionally, the chi-squared p-value can be replaced by",
      "a bootstrap p-value."
    )
  )

  .write_tech(
    "Greedy forward selection (Prioritization)",
    paste(
      "The prioritization is built by forward selection. In round 1, every",
      "attribute is evaluated on its own using the chosen strategy (lift or",
      "max) and the expected outcome is computed by exact inference on the",
      "subgroup's network. The best attribute becomes step 1. In subsequent",
      "rounds every remaining attribute is layered on top and re-evaluated;",
      "the one producing the largest incremental outcome becomes the next",
      "step. The search stops when the fractional marginal gain falls below",
      "the early-stopping threshold."
    )
  )

  if (isTRUE(show_priort_pvalue_section)) {
    .write_tech(
      "Prioritization bootstrap p-values",
      paste(
        "After the greedy search completes, significance of each step is",
        "assessed by bootstrapping the rows. For each replicate the",
        "conditional probability tables are re-fitted on the fixed structure",
        "and the selected attributes are re-queried in the same order. Each",
        "step's p-value is the fraction of replicates where its marginal gain",
        "was at or below a noise-floor estimate — the average gain across the",
        "tail steps of the selected path, where additional value is expected",
        "to be indistinguishable from noise."
      )
    )
  }

  .write_tech(
    "Indexing",
    paste(
      "The Index column rescales a chosen metric so the top-ranked",
      "attribute in the current view is anchored at 100 and all others are",
      "proportional. Possible anchors include a lift value, Best-vs-Worst, or",
      "mutual information. Indexing is applied within each subgroup",
      "independently."
    )
  )

  if (has_simulator) {
    .write_tech(
      "Simulator precomputation",
      paste(
        "In 'Current Level' mode, the Simulator displays precomputed",
        "posterior distributions of every target given the selected",
        "attribute at each of its observed levels. In 'Percent Change' mode,",
        "it displays precomputed expected values under the attribute's",
        "shifted distribution, across a range of percentages and either the",
        "whole market's distribution or a specific brand's. Values are",
        "rounded to four decimal places to keep the workbook compact."
      )
    )
  }

  .write_tech(
    "Base-size thresholds",
    paste0(
      "Brand-by-subgroup slices below ", min_base_for_lift,
      " respondents are blanked in the driver dashboards; slices below ",
      min_base_for_boot,
      " are skipped in the Prioritization. The Focus dropdown in each",
      " dashboard displays the actual base or a red warning. The thresholds",
      " guard against estimates dominated by the Bayesian prior rather than",
      " observed data."
    )
  )

  .close_section()

  openxlsx::setColWidths(wb, guide_sheet, cols = 1, widths = 3)
  openxlsx::setColWidths(wb, guide_sheet, cols = col_left, widths = 28)
  openxlsx::setColWidths(wb, guide_sheet, cols = col_right, widths = 95)

  invisible(wb)
}
