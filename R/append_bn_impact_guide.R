#' append_bn_impact_guide
#'
#' @description Appends a client-friendly "Guide" sheet to an existing
#'   \code{bn_impact_write()} workbook. The guide explains what the workbook
#'   contains, how to use the dashboard, and how to interpret the metrics.
#'   Added as the first tab so it's the first thing a user sees.
#'
#' @param wb An openxlsx workbook object.
#' @param wb_type Character. \code{"dynamic"} or \code{"standard"}. Affects
#'   whether dropdown-control instructions are included.
#' @param dv_display Character or NULL. Display name of the DV for the header.
#' @param has_weights Logical. Whether a Weight control exists.
#' @param has_community Logical. Whether a Community Drivers tab exists.
#' @param has_simulator Logical. Whether a Simulator tab exists.
#' @param has_brands Logical. Whether a Focus (brand) control exists.
#' @param index_by Character. The indexing method used (for notes section).
#' @param type Character. Engine type: \code{"gr"}, \code{"cp"}, or \code{"mi"}.
#' @param min_base_for_lift Integer. Minimum sample size used for brand lift.
#' @param min_base_for_sim Integer or NULL. Minimum sample size used for
#'   simulator brand slices.
#' @param boot_applied Logical. Whether parameter-bootstrap replicates were
#'   actually run (i.e., \code{n_boot > 1}). Default FALSE.
#' @param n_boot Integer or NULL. Number of parameter-bootstrap replicates
#'   (if \code{boot_applied}). Used to describe what actually ran.
#' @param mi_boot_applied Logical. Whether community mutual-information
#'   bootstrap was applied. Default FALSE.
#' @param mi_boot Integer or NULL. Number of community-MI bootstrap replicates
#'   (if \code{mi_boot_applied}).
#'
#' @return The modified workbook object (invisibly).
#'
#' @keywords internal
append_bn_impact_guide <- function(
    wb,
    wb_type = c("dynamic", "standard"),
    dv_display = NULL,
    has_weights = FALSE,
    has_community = FALSE,
    has_simulator = FALSE,
    has_brands = FALSE,
    index_by = "lift_first",
    type = "gr",
    min_base_for_lift = 75,
    min_base_for_sim = NULL,
    boot_applied = FALSE,
    n_boot = NULL,
    mi_boot_applied = FALSE,
    mi_boot = NULL
) {

  wb_type <- match.arg(wb_type)

  guide_sheet <- "Guide"
  openxlsx::addWorksheet(wb, guide_sheet, tabColour = "#2E75B6", gridLines = FALSE)

  # Styles — title/subtitle plain (no borders); section headers get a grey
  # fill with an outer box around each section; table headers are just bold;
  # table bodies have no borders or fill.
  s_title    <- openxlsx::createStyle(textDecoration = "bold", fontSize = 18)
  s_subtitle <- openxlsx::createStyle(textDecoration = c("bold", "italic"),
                                      fontSize = 14)
  s_h2       <- openxlsx::createStyle(textDecoration = "bold", halign = "center",
                                      wrapText = TRUE,
                                      fgFill = "#D9D9D9")
  s_body     <- openxlsx::createStyle(fontSize = 11, wrapText = TRUE,
                                      valign = "top")
  s_tbl_hdr  <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                      halign = "left", valign = "top")
  s_tbl_body <- openxlsx::createStyle(fontSize = 11, halign = "left",
                                      valign = "top", wrapText = TRUE)
  s_tech_body <- openxlsx::createStyle(fontSize = 11, wrapText = TRUE,
                                       valign = "top")
  s_tech_hdr  <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                       valign = "top")

  # Layout: two columns only. Column B = label (narrower), column C =
  # description/content (wide). The only merges in the sheet are the section
  # header rows, which span B:C.
  col_left  <- 2L   # B = label column
  col_right <- 3L   # C = content column
  col_end   <- 3L   # C (alias for readability; same as col_right)

  r <- 2L

  # Section-box tracker — .write_h2 sets this to the header row so a medium
  # outer box can be drawn around the entire section at close time.
  section_start <- NULL

  .close_section <- function() {
    # Only close if a section is open AND we've written content past the header.
    # Section's last content row is (r - 2) because every writer leaves a
    # trailing blank row before advancing.
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

  # ---------------------------------------------------------------------------
  # Helper: write a section header (row `r`) and advance
  # ---------------------------------------------------------------------------
  .write_h2 <- function(text) {
    # Close out the previous section (if any) with a medium outer box before
    # starting this new one.
    .close_section()

    openxlsx::writeData(wb, guide_sheet, text,
      startRow = r, startCol = col_left)
    # Apply style to every cell in the merge range BEFORE merging so the
    # border and fill render continuously across the header.
    openxlsx::addStyle(wb, guide_sheet, s_h2,
      rows = r, cols = col_left:col_end,
      gridExpand = TRUE, stack = TRUE)
    openxlsx::mergeCells(wb, guide_sheet, rows = r, cols = col_left:col_end)
    openxlsx::setRowHeights(wb, guide_sheet, rows = r, heights = 22)
    section_start <<- r
    r <<- r + 1L
  }

  # Helper: write a 2-col labelled table (label | description) starting at `r`.
  # Advances `r` past the table. No merges, no fills, no borders — just bold
  # the header row.
  .write_labelled <- function(df) {
    # Header row — bold, no fill, no merge
    openxlsx::writeData(wb, guide_sheet, names(df)[1],
      startRow = r, startCol = col_left)
    openxlsx::writeData(wb, guide_sheet, names(df)[2],
      startRow = r, startCol = col_right)
    openxlsx::addStyle(wb, guide_sheet, s_tbl_hdr,
      rows = r, cols = col_left:col_right,
      gridExpand = TRUE, stack = TRUE)
    r <<- r + 1L

    # Body rows
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

    r <<- r + 1L  # trailing blank row
  }

  # Helper: write a paragraph in the wide content column (C), no merge.
  # Column B is left blank on the row.
  .write_para <- function(text) {
    openxlsx::writeData(wb, guide_sheet, text,
      startRow = r, startCol = col_right)
    openxlsx::addStyle(wb, guide_sheet, s_body,
      rows = r, cols = col_right, stack = TRUE)
    r <<- r + 2L
  }

  # ---------------------------------------------------------------------------
  # Section 1: What's in this workbook
  # ---------------------------------------------------------------------------
  .write_h2("What's in this workbook")

  tabs_df <- data.frame(
    Tab = character(),
    Description = character(),
    stringsAsFactors = FALSE
  )
  tabs_df <- rbind(tabs_df, data.frame(
    Tab = "Attribute Drivers",
    Description = "Ranks individual attributes by how strongly they influence the outcome. The main results tab.",
    stringsAsFactors = FALSE
  ))
  if (has_community) {
    tabs_df <- rbind(tabs_df, data.frame(
      Tab = "Community Drivers",
      Description = "Ranks groups of related attributes (communities) by their joint influence on the outcome. Useful when several attributes move together.",
      stringsAsFactors = FALSE
    ))
  }
  if (has_simulator) {
    tabs_df <- rbind(tabs_df, data.frame(
      Tab = "Simulator",
      Description = "Interactive 'what-if' tool. Choose a variable and see how changing it would be expected to flow through the network to other variables.",
      stringsAsFactors = FALSE
    ))
  }
  .write_labelled(tabs_df)

  # ---------------------------------------------------------------------------
  # Section 2: Controls (dynamic only)
  # ---------------------------------------------------------------------------
  if (wb_type == "dynamic") {
    .write_h2("How to use the dashboard")
    .write_para(
      "The dropdowns at the top of each dashboard tab change what you see. Pick any combination; the table below updates immediately."
    )

    ctrl_df <- data.frame(
      Control = character(),
      Description = character(),
      stringsAsFactors = FALSE
    )
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Subgroup",
      Description = "Limits results to a specific segment of respondents (e.g., a demographic or behavioural cut). 'Total' shows everyone.",
      stringsAsFactors = FALSE
    ))
    if (has_brands) {
      ctrl_df <- rbind(ctrl_df, data.frame(
        Control = "Focus",
        Description = "'Market' uses the whole sample within the chosen subgroup. Selecting a brand uses only that brand's respondents, so you see the drivers for that brand's customers.",
        stringsAsFactors = FALSE
      ))
    }
    ctrl_df <- rbind(ctrl_df, data.frame(
      Control = "Metric",
      Description = "Switches what the numbers represent: the no-shift baseline impact, the 10% lift scenario, the best-case-to-worst-case range (MaxVmin), or mutual information (statistical dependency strength).",
      stringsAsFactors = FALSE
    ))
    if (has_weights) {
      ctrl_df <- rbind(ctrl_df, data.frame(
        Control = "Weight",
        Description = "Toggles between weighted and unweighted results. Weighting influences mean shifts (Lift), not relationship-strength metrics (MaxVmin, MI). The control turns red when a non-affected metric is selected.",
        stringsAsFactors = FALSE
      ))
    }
    .write_labelled(ctrl_df)
  }

  # ---------------------------------------------------------------------------
  # Section 3: Reading the columns
  # ---------------------------------------------------------------------------
  .write_h2("Reading the results table")

  cols_df <- data.frame(
    Column = character(),
    Description = character(),
    stringsAsFactors = FALSE
  )
  cols_df <- rbind(cols_df, data.frame(
    Column = "Variable / Label",
    Description = "The attribute being measured. The Label is the human-readable version from the dictionary.",
    stringsAsFactors = FALSE
  ))
  if (has_community) {
    cols_df <- rbind(cols_df, data.frame(
      Column = "Community",
      Description = "The thematic group this attribute belongs to. Attributes in the same community tend to move together.",
      stringsAsFactors = FALSE
    ))
  }
  cols_df <- rbind(cols_df, data.frame(
    Column = "Index",
    Description = "A relative ranking score where the top attribute in view is anchored at 100. Use this to compare attributes against each other quickly.",
    stringsAsFactors = FALSE
  ))
  cols_df <- rbind(cols_df, data.frame(
    Column = "Lift",
    Description = "Expected change in the outcome (in probability points or mean score) from shifting the attribute's distribution by the selected percentage. A lift of 0.05 means a 5-point move in the outcome.",
    stringsAsFactors = FALSE
  ))
  cols_df <- rbind(cols_df, data.frame(
    Column = "MaxVmin",
    Description = "The theoretical ceiling: the outcome difference between this attribute's best and worst levels. Useful to bound potential impact, regardless of where respondents currently sit.",
    stringsAsFactors = FALSE
  ))
  cols_df <- rbind(cols_df, data.frame(
    Column = "MI / p-value",
    Description = "Mutual information is the statistical strength of the attribute-outcome relationship; the p-value says how confident we are that relationship is real (smaller is more confident).",
    stringsAsFactors = FALSE
  ))
  .write_labelled(cols_df)

  # ---------------------------------------------------------------------------
  # Section 4: Simulator
  # ---------------------------------------------------------------------------
  if (has_simulator) {
    .write_h2("Using the Simulator")
    .write_para(
      "The Simulator lets you ask 'what if this attribute changed?' and see the expected effect on any other variable in the network."
    )

    sim_df <- data.frame(
      Step = character(),
      Description = character(),
      stringsAsFactors = FALSE
    )
    sim_df <- rbind(sim_df, data.frame(
      Step = "1. Subgroup",
      Description = "Pick the segment you want to simulate within.",
      stringsAsFactors = FALSE
    ))
    sim_df <- rbind(sim_df, data.frame(
      Step = "2. Variable",
      Description = "The attribute you're changing (the driver).",
      stringsAsFactors = FALSE
    ))
    sim_df <- rbind(sim_df, data.frame(
      Step = "3. Mode",
      Description = "'Current Level' shows the expected outcome at each observed level of the variable. 'Percent Change' lets you shift the variable's distribution by a percentage and see the downstream effect.",
      stringsAsFactors = FALSE
    ))
    if (has_brands) {
      sim_df <- rbind(sim_df, data.frame(
        Step = "4. Focus",
        Description = "Whether to simulate using the whole market's distribution or a specific brand's. Brands with fewer than the minimum sample size are shown with a red warning and blank values.",
        stringsAsFactors = FALSE
      ))
    }
    .write_labelled(sim_df)
  }

  # ---------------------------------------------------------------------------
  # Section 5: Notes & caveats
  # ---------------------------------------------------------------------------
  .write_h2("Notes on reading the numbers")

  engine_note <- switch(type,
    "gr" = "Impacts are computed using exact Bayesian network inference, which produces deterministic point estimates.",
    "cp" = "Impacts are computed via Monte Carlo conditional probability queries. Results are stable but include a small amount of sampling variability.",
    "mi" = "Impacts are summarized using mutual information — a measure of statistical dependency between each attribute and the outcome.",
    "Impacts are computed from a Bayesian network fitted to your data."
  )
  .write_para(engine_note)

  index_note <- switch(index_by,
    "lift_first"  = "The Index column ranks attributes by their first lift value (the baseline-variation impact), with mutual information as a secondary tie-breaker.",
    "lift_second" = "The Index column ranks attributes by the second lift value (e.g., 10% shift), with mutual information as a secondary tie-breaker.",
    "maxVmin"     = "The Index column ranks attributes by their MaxVmin score — the best-minus-worst outcome range.",
    "mi"          = "The Index column ranks attributes by their mutual information with the outcome.",
    "none"        = NULL,
    NULL
  )
  if (!is.null(index_note)) .write_para(index_note)

  if (!is.null(min_base_for_lift)) {
    .write_para(
      paste0(
        "Brand-specific lift values require at least ", min_base_for_lift,
        " respondents in that brand x subgroup slice. Smaller slices appear blank rather than unreliable."
      )
    )
  }

  if (has_simulator && !is.null(min_base_for_sim)) {
    .write_para(
      paste0(
        "The Simulator applies the same floor: brand x subgroup slices below ",
        min_base_for_sim,
        " respondents are shown in the dashboard with a red warning and blank results."
      )
    )
  }

  # ---------------------------------------------------------------------------
  # Technical appendix — statistician-friendly details
  # ---------------------------------------------------------------------------
  r <- r + 1L
  .write_h2("Technical Appendix")

  # Helper to write a labelled technical paragraph: bold label in column B,
  # body in column C on the same row. No merge.
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

  .write_tech(
    "Model structure",
    paste(
      "A discrete Bayesian network is learned over the attributes and the",
      "dependent variable. Structure learning uses a score-based search",
      "(hill-climbing or tabu) with a penalized likelihood criterion (BIC or",
      "AIC). Given the learned structure, the conditional probability tables",
      "are estimated as Bayesian posterior means under a Dirichlet prior. Each",
      "subgroup model shares the same structure as the overall model but is",
      "re-fitted on the subgroup's rows, so the conditional probability tables",
      "reflect within-subgroup distributions. Before fitting, factor levels",
      "that do not appear in the subgroup are removed so every level in the",
      "model is backed by observed data."
    )
  )

  .write_tech(
    "MaxVmin",
    paste(
      "For each attribute X, MaxVmin is the difference in the outcome between",
      "its best and worst observed level, under exact Bayesian inference on",
      "the fitted network. In top-box mode it is the probability of the",
      "maximum DV level given X at its maximum, minus the same probability",
      "given X at its minimum. In mean mode it is the expected DV given X at",
      "its maximum, minus the expected DV given X at its minimum. This",
      "captures the theoretical ceiling of the attribute's influence,",
      "independent of where respondents actually sit on X."
    )
  )

  .write_tech(
    "Probability Lift",
    paste(
      "Lift accounts for the observed distribution of respondents on the",
      "attribute. Let p_obs be the attribute's current (optionally weighted)",
      "frequency distribution, and p_shift be the same distribution after an",
      "exponential tilt that shifts its mean by the requested percentage",
      "(proportional) or by a fixed number of scale points (absolute). Let",
      "q(x) be the conditional target: the probability of the top DV level",
      "given X = x, or the expected DV given X = x in mean mode, computed by",
      "exact inference on the network. Lift is then the difference between",
      "the expected target under the shifted distribution and the expected",
      "target under the observed distribution: sum over x of q(x) * p_shift(x)",
      "minus sum over x of q(x) * p_obs(x). When the requested lift is zero,",
      "the reported value is a symmetric sensitivity: the difference between",
      "a +5% and a -5% tilt. When a brand is specified, p_obs and p_shift are",
      "computed from the brand's rows while q(x) is shared across brands",
      "because the brand variable does not enter the network."
    )
  )

  .write_tech(
    "Weighting",
    paste(
      "When a weight variable is provided, the observed frequency distribution",
      "p_obs is computed as the sum of weights within each level rather than",
      "the raw count. Weighting therefore propagates into Lift, which depends",
      "on p_obs. It does not propagate into MaxVmin or the conditional target",
      "q(x), because those come from the network's conditional probability",
      "tables, which are fitted as unweighted Bayesian posteriors. Mutual",
      "information and its p-values depend on joint sample counts and are",
      "also unweighted. The Weight control in the dashboard reflects this:",
      "it is gated to lift-type metrics."
    )
  )

  .write_tech(
    "Mutual information and p-values",
    paste(
      "For individual attributes, mutual information between the attribute",
      "and the DV is computed from unconditional sample counts, and its",
      "significance is assessed with the standard chi-squared approximation",
      "to the likelihood-ratio test statistic. For communities, attributes are",
      "tested sequentially using a chained conditional mutual information",
      "decomposition: the MI between the first attribute and the DV, plus",
      "the MI between the second attribute and the DV conditional on the",
      "first, plus the MI between the third and the DV conditional on the",
      "first two, and so on. The likelihood-ratio test statistics and degrees of freedom are",
      "summed across steps and referred to a single chi-squared distribution.",
      "This avoids constructing a joint community factor with too many levels",
      "relative to the sample size, which would inflate degrees of freedom",
      "and break the asymptotic approximation. Optionally, the chi-squared",
      "p-value can be replaced by a bootstrap p-value: the community MI is",
      "recomputed on B resamples of the rows and p is reported as the",
      "proportion of resamples where the MI is at or below zero."
    )
  )

  # Bootstrap — only described when it actually ran. Two independent forms:
  # (a) parameter bootstrap (n_boot > 1), (b) community MI bootstrap.
  if (boot_applied) {
    .write_tech(
      "Parameter bootstrap",
      paste0(
        "Parameter uncertainty for this run was estimated by resampling the",
        " rows ", n_boot, " times with replacement. For each resample, the",
        " conditional probability tables were re-estimated on the fixed",
        " network structure and the full impact calculation was re-run.",
        " Reported means, standard errors, confidence intervals and p-values",
        " are bootstrap summaries: the replicate mean; standard error as the",
        " replicate standard deviation divided by the square root of the",
        " number of replicates; a 95% t-based confidence interval; and a",
        " two-sided p-value against zero. Because the network structure is",
        " held fixed across replicates, these intervals reflect parameter",
        " uncertainty conditional on the learned structure, not structural",
        " uncertainty."
      )
    )
  } else {
    .write_tech(
      "Parameter bootstrap",
      paste(
        "Parameter bootstrap was not applied to this run. The reported impact",
        "values are point estimates from the fitted network. When enabled,",
        "this step resamples rows with replacement, re-estimates the",
        "conditional probability tables on the fixed structure, and",
        "summarizes the resulting distribution of impact values with means,",
        "standard errors and confidence intervals."
      )
    )
  }

  if (mi_boot_applied) {
    .write_tech(
      "Community mutual-information bootstrap",
      paste0(
        "Community-level mutual information significance was assessed via ",
        mi_boot, " row-resamples. For each resample, the community's joint",
        " mutual information with the outcome was recomputed; the reported",
        " p-value is the proportion of resamples in which the community MI",
        " was at or below zero. This replaces the chi-squared approximation",
        " for the community tests, which becomes unreliable when the",
        " composite community factor has many levels relative to sample size."
      )
    )
  }

  .write_tech(
    "Indexing",
    paste(
      "The Index column rescales a chosen metric so the top-ranked attribute",
      "in the current view is anchored at 100 and all others are proportional.",
      "Possible anchors are the primary or secondary lift value (with mutual",
      "information as a tie-breaker), MaxVmin, or mutual information directly.",
      "The choice is a presentation decision and does not affect the",
      "underlying estimates. Indexing is applied within each subgroup",
      "independently."
    )
  )

  if (has_simulator) {
    .write_tech(
      "Simulator precomputation",
      paste(
        "In 'Current Level' mode, the Simulator displays precomputed posterior",
        "distributions of every target variable given the selected attribute",
        "at each of its observed levels, obtained by exact inference on the",
        "subgroup's network. In 'Percent Change' mode, it displays precomputed",
        "expected values of every target under the attribute's shifted",
        "distribution, where the shift is an exponential tilt over a range of",
        "percentages, and the input distribution is either the whole market's",
        "or a specific brand's. Stored values are rounded to four decimal",
        "places to keep the workbook compact; the dashboard formats to one or",
        "two decimals, so this rounding is not visible in outputs."
      )
    )
  }

  .write_tech(
    "Base-size thresholds",
    paste0(
      "Brand-by-subgroup slices with fewer than ", min_base_for_lift,
      " respondents are blanked in the main dashboard. Lift values return",
      " missing rather than being computed on sparse frequency distributions,",
      " which would otherwise produce estimates dominated by the Bayesian",
      " prior rather than the observed data.",
      if (has_simulator && !is.null(min_base_for_sim)) {
        paste0(
          " The Simulator applies the same logic with a minimum of ",
          min_base_for_sim,
          " respondents; offending slices are shown with a red warning next",
          " to the Focus control and blank values in the dashboard."
        )
      } else ""
    )
  )

  # Close the final section with its medium outer box
  .close_section()

  # Column widths: A narrow (indent), B label (narrow-ish), C content (wide)
  openxlsx::setColWidths(wb, guide_sheet, cols = 1, widths = 3)
  openxlsx::setColWidths(wb, guide_sheet, cols = col_left, widths = 28)
  openxlsx::setColWidths(wb, guide_sheet, cols = col_right, widths = 95)

  invisible(wb)
}
