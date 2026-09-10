#' Resondex Impact Framework: Probability Lift + Information Evidence
#'
#' Quantifies and prioritizes business drivers using two complementary dimensions:
#' **Impact Magnitude** (probability lift) and **Evidence Strength** (mutual information significance).
#' This produces an executive-ready ranking of variables that balances business materiality
#' with statistical credibility.
#'
#' @section Resondex Impact Framework:
#' The **Resondex Impact Framework** separates *effect size* from *evidence quality*.
#'
#' **1) Impact Magnitude — Probability Lift**
#' \itemize{
#'   \item Resondex defines effect size in probability terms:
#'   \item \emph{Probability Lift} = P(Outcome \| Driver = High) − P(Outcome \| Driver = Low)
#'   \item This yields an interpretable percentage-point change aligned to conversion, risk, or uplift.
#' }
#'
#' **2) Evidence Strength — Mutual Information (MI) Significance**
#' \itemize{
#'   \item Resondex evaluates dependency between each driver and the outcome using Mutual Information (MI).
#'   \item MI captures linear and non-linear relationships and supports categorical and continuous variables.
#'   \item The MI p-value reflects the statistical credibility of the observed relationship.
#' }
#'
#' **3) Decision Logic — Impact × Evidence**
#' \itemize{
#'   \item High Impact + High Evidence: primary strategic levers (scale and invest).
#'   \item High Impact + Lower Evidence: promising opportunities (validate via testing or more data).
#'   \item Low Impact + High Evidence: reliable but minor effects (incremental optimization).
#'   \item Low Impact + Low Evidence: de-prioritized signals.
#' }
#'
#' **Executive outcome**
#' A ranked set of drivers that clearly answers: *what matters most*, *which effects are trustworthy*,
#' and *where to focus investment for maximum ROI*.
#'
#' @section Resondex Impact Score (optional):
#' In addition to reporting Probability Lift and MI p-values directly, teams may compute a single
#' prioritization score for dashboards:
#' \itemize{
#'   \item \emph{Impact Score} = abs(Probability Lift)
#'   \item \emph{Evidence Score} = −log10(p_value)
#'   \item \emph{Resondex Impact Score} = Impact Score × Evidence Score
#' }
#' This combined score is intended for ranking and triage; reporting should still display the
#' underlying Probability Lift and p-value for transparency.
#'
#' @section Client-facing summary (drop-in language):
#' Resondex measures each driver two ways: (1) **Probability Lift** to quantify how much the outcome moves
#' in business terms, and (2) **Mutual Information significance** to quantify how confident we are the
#' relationship is real and not noise. We prioritize drivers that are both **material in effect** and
#' **statistically credible**, producing clear, defensible focus areas for investment.
#'
#' @section Board-level bullets (ultra concise):
#' \itemize{
#'   \item Quantify impact as percentage-point probability lift (business interpretable).
#'   \item Validate reliability using MI significance (captures non-linear relationships).
#'   \item Prioritize using an Impact × Evidence framework to guide investment decisions.
#' }
#'
#' @param obj A Bayesian network object or (when \code{process_subgroups = TRUE})
#'   a named list of Bayesian network objects. Names must correspond to subgroup
#'   indicator columns in \code{df} (values equal to 1) used to filter each
#'   subgroup.
#' @param df A data frame containing variables used for impact estimation. When
#'   \code{process_subgroups = TRUE}, it must also contain the subgroup indicator
#'   columns matching \code{names(obj)}.
#' @param dictionary Optional. A dictionary object (or named vector) for
#'   variable labels. Joined to output via \code{work::dictionary_from_named_object()}.
#'   Adds a \code{Label} column after \code{Variable}.
#' @param dv Optional. Dependent/target variable name (character scalar). If
#'   \code{NULL}, \code{bn_impact_engine()} determines the target.
#' @param ivs Optional. Independent variable names (character vector). If
#'   \code{NULL}, \code{bn_impact_engine()} determines which variables to
#'   evaluate.
#' @param process_subgroups Logical. If \code{TRUE}, iterate over subgroup
#'   models and filter \code{df} to rows where \code{df[[subgroup_name]] == 1}.
#'   If \code{FALSE}, compute once on the full \code{df}. Default TRUE.
#' @param do_community Logical. Whether to compute community-level summaries
#'   (passed through to the engine). Default FALSE.
#' @param community_assignment Optional. Community assignment object used when
#'   \code{do_community = TRUE}.
#' @param community_impact_attributes Character vector or NULL. Battery names
#'   (variable-name prefixes, e.g. \code{"q14a"}) whose attributes are included
#'   when computing community-level impacts. Default NULL includes all
#'   attributes. Errors if a declared battery matches no IV. Applies to every
#'   community metric across all subgroups and shift variants; ignored when
#'   \code{do_community = FALSE}. See \code{\link{bn_impact_engine}}.
#' @param impact_readoff Character. \code{"empirical"} (default) reads
#'   E\[DV | IV = level\] for the lift metrics directly from the data;
#'   \code{"model"} uses the fitted network's conditionals - the methodology
#'   used prior to 2026-07-28. See \code{\link{bn_impact_engine}}.
#'   \code{"empirical_brand_control"}: empirical with the \code{brand}
#'   column's composition held fixed - the back-door adjustment for
#'   stacked designs, where exposure-type IVs double as markers for which
#'   brand a row describes. Adjusts lifts, observed-anchor maxVmin, and
#'   pins brand margins in the joint community rake; MI unaffected.
#'   Requires \code{brand}. Global switch - also removes the brand-level
#'   component of perception batteries. See \code{\link{bn_impact_engine}}.
#' @param community_lift Character. \code{"joint"} (default) computes
#'   community lift columns by raking (IPF) to all member targets at once -
#'   the theme effect; \code{"average"} takes the arithmetic mean of member
#'   lifts - the methodology used prior to 2026-07-28. The joint rake's
#'   read-off is inherently empirical; combined with
#'   \code{impact_readoff = "model"}, attribute lifts stay model-based
#'   while community lifts are empirical (warning issued). See
#'   \code{\link{bn_impact_engine}}.
#' @param max_impact_anchor Character. \code{"observed"} (default) anchors
#'   the Best-vs-Worst (maxVmin) family at observed, support-guarded anchors;
#'   \code{"theoretical"} uses hypothetical all-max / all-min evidence - the
#'   methodology used prior to 2026-07-28. See \code{\link{bn_impact_engine}}.
#' @param max_impact_min_support Integer. Minimum respondent count for an
#'   observed anchor candidate. Default 5.
#' @param max_impact_shrinkage Numeric >= 0. Empirical-Bayes prior weight
#'   (pseudo-respondents toward the scope mean) guarding observed anchors
#'   against winner's curse. \code{0} disables. Default 20. See
#'   \code{\link{bn_impact_engine}}.
#' @param min_boot_coverage Numeric in (0, 1]. Minimum share of bootstrap
#'   replicates that must yield a value for a metric cell to be reported;
#'   cells below it are blanked and passing cells use the feasible count
#'   for the df. Default 0.9. See \code{\link{bn_impact_engine}}.
#' @param boot_inference_legacy Logical. \code{TRUE} reproduces the
#'   pre-2026-07-29 bootstrap bookkeeping (nominal df, no coverage
#'   blackout, NA rare-level replicates silently excluded) for
#'   replicating historical deliverables; known to overstate
#'   significance. Default \code{FALSE}. See \code{\link{bn_impact_engine}}.
#' @param lift Numeric vector. Target lift(s) for the shifted-distribution
#'   metric (both proportional and absolute shift variants are precomputed).
#'   Default \code{c(0, 0.1)}.
#' @param min_base_for_lift Integer. Minimum sample size required for a brand
#'   subgroup to compute lift estimates. Brands below this threshold are
#'   excluded. Counted as distinct respondents when \code{id} is set, otherwise
#'   as stacked records - see \code{\link{bn_impact_engine}}. Default 75.
#' @param override_min_base_for_lift Character vector or NULL. Subgroup names
#'   exempted from \code{min_base_for_lift}: every lift cell inside them is
#'   computed however thin its base, market and brand focus alike. For a
#'   must-report audience cut whose base the client has already accepted.
#'   Matched against the names of \code{obj} when \code{process_subgroups =
#'   TRUE}; when \code{FALSE} the single run is treated as \code{"Total"}.
#'   Truly empty scopes still return \code{NA} - there is no distribution to
#'   shift. Default NULL (no exemptions).
#' @param type Character. Engine type: \code{"gr"} (gRain exact inference,
#'   default), \code{"cp"} (cpdist sampling), or \code{"mi"} (mutual
#'   information).
#' @param dv_metric Character. How the DV is summarized: \code{"mean"}
#'   (default) computes the expected value across all DV levels;
#'   \code{"top_box"} uses the probability of the highest DV level.
#' @section Shift-type variants:
#'   Both proportional and absolute IV-shift variants are always precomputed
#'   as separate columns in the returned table. Lift columns carry
#'   `_propshift_` or `_absshift_` tags that interact with the outcome-
#'   display variants (`_propdisplay` / `_absdisplay`). The dashboard built
#'   by \code{bn_impact_write()} exposes runtime dropdowns for both. No
#'   parameter controls this any longer — both shifts are always emitted.
#'   maxVmin / mi columns are shift-independent and only emit the outcome-
#'   display variants.
#' @section Outcome-display variants:
#'   Both proportional and absolute outcome-display variants of every DV-change
#'   metric (\code{lift}, \code{maxVmin}) are always precomputed and stored
#'   as separate columns in the returned table. The dashboard built by
#'   \code{bn_impact_write()} exposes a runtime Outcome Display dropdown to
#'   switch between them. No parameter controls this — both are always
#'   emitted.
#' @param include_base Logical. Whether to include the base (overall/unfiltered)
#'   estimate alongside brand-specific estimates. Default \code{TRUE}.
#' @param index_by Character. How to compute the final impact index:
#'   \code{"lift_first"} (default) ranks by lift then breaks ties by MI,
#'   \code{"lift_second"} ranks by MI then lift, \code{"maxVmin"} uses the
#'   max-minus-min DV range, \code{"mi"} uses MI only, \code{"none"} omits
#'   the index column.
#' @param n_boot Integer. Number of bootstrap replicates for the MI
#'   significance test (passed through to the engine). Default 1.
#' @param n_querry Integer. Number of Monte Carlo samples used by the
#'   \code{"cp"} engine for \code{cpdist} queries. Ignored for \code{"gr"} and
#'   \code{"mi"} types. Default 1e4.
#' @param brand Character or NULL. Column name in \code{df} containing a brand
#'   or group variable. When provided, impact is computed separately for each
#'   brand level, producing brand-prefixed output columns.
#' @param brand_names Character vector or NULL. When provided, only compute
#'   brand-specific lift for these brand levels. Brands not in this vector are
#'   skipped. Market-level lift is always computed. Default NULL (all brands).
#' @param id Character or \code{NULL}. Column in \code{df} identifying the
#'   respondent. Default \code{"uuid"}. Bases are reported as DISTINCT
#'   RESPONDENTS rather than stacked records; errors if the column is absent.
#'   \code{NULL} restores stacked-record counts. See
#'   \code{\link{bn_impact_engine}}.
#' @param weight Character or NULL. Column name in \code{df} containing
#'   observation weights. When provided, frequency distributions used for
#'   lift calculations are weighted. Default NULL.
#' @param mi_boot Integer or NULL. Number of bootstrap replicates specifically
#'   for the mutual information significance test. Overrides \code{n_boot} for
#'   MI computation when set. Default NULL (uses \code{n_boot}).
#' @param verbose Logical. Show a progress bar across subgroups. Default
#'   \code{TRUE}. Set to \code{FALSE} when called from a higher-level wrapper
#'   that provides its own progress indication.
#' @param use_parallel Logical. Whether to parallelize subgroup processing
#'   via \code{work::imap_progress()}. Default \code{TRUE}.
#' @param boot_nonzero Logical. Default \code{FALSE}: classical bootstrap
#'   inference - the SD of the bootstrap replicates is used directly as the
#'   standard error, so p-values are invariant to \code{n_boot}. \code{TRUE}
#'   restores the legacy behavior (\code{se = sd/sqrt(n_boot)}), which tests
#'   whether the mean of the boot distribution is nonzero and shrinks
#'   p-values as \code{n_boot} grows.
#' @param seed Integer. Random seed passed through to the engine for
#'   reproducibility. Default 1.
#'
#' @return A \code{data.frame} (tibble-compatible) with one row per variable and one or more impact
#'   summary columns. When \code{process_subgroups = TRUE}, columns are subgroup-prefixed and the
#'   variable column is named \code{Variable}.
#'
#' @details
#' Output shaping:
#' \itemize{
#'   \item When \code{process_subgroups = TRUE}, results are computed per subgroup, renamed with
#'     subgroup-prefixed columns, and column-bound. Columns ending in \code{"_variable"} are removed
#'     after producing the unified \code{Variable} field.
#'   \item When \code{process_subgroups = FALSE}, the engine's \code{variable} column is renamed to
#'     \code{Variable}.
#' }
#'
#' @examples
#' \dontrun{
#' # --- Single model (no subgroups) ---
#' # Example assumes you have bn_impact_engine() and a compatible BN object (bn_fit).
#' out <- bn_impact(
#'   obj = bn_fit,
#'   df = iris,
#'   dv = "Species",
#'   ivs = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   type = "mi",
#'   process_subgroups = FALSE,
#'   n_boot = 10,
#'   n_querry = 1e4,
#'   seed = 1
#' )
#'
#' # --- Subgroup mode ---
#' # obj is a named list, and df has 0/1 indicator columns matching names(obj).
#' bn_list <- list(grp_a = bn_fit_a, grp_b = bn_fit_b)
#' iris2 <- iris
#' iris2[["grp_a"]] <- as.integer(iris2[["Species"]] == "setosa")
#' iris2[["grp_b"]] <- as.integer(iris2[["Species"]] != "setosa")
#'
#' out2 <- bn_impact(
#'   obj = bn_list,
#'   df = iris2,
#'   dv = "Species",
#'   type = "cp",
#'   process_subgroups = TRUE,
#'   n_boot = 5,
#'   n_querry = 1e4,
#'   seed = 1
#' )
#' }
#'
#' @export
bn_impact <- function(
    obj,
    df,
    dictionary = NULL,
    dv = NULL,
    ivs = NULL,
    process_subgroups = TRUE,
    do_community = FALSE,
    community_assignment = NULL,
    community_impact_attributes = NULL,
    impact_readoff = c("empirical", "model", "empirical_brand_control"),
    community_lift = c("joint", "average"),
    max_impact_anchor = c("observed", "theoretical"),
    max_impact_min_support = 5,
    max_impact_shrinkage = 20,
    min_boot_coverage = 0.9,
    boot_inference_legacy = FALSE,
    lift = c(0, 0.1),
    min_base_for_lift = 75,
    override_min_base_for_lift = NULL,
    type = c("gr", "cp", "mi"),
    dv_metric = c("mean", "top_box"),
    include_base = TRUE,
    index_by = c("lift_first", "lift_second", "maxVmin", "mi", "none"),
    n_boot = 1,
    n_querry = 1e4,
    brand = NULL,
    brand_names = NULL,
    weight = NULL,
    id = "uuid",
    mi_boot = NULL,
    verbose = TRUE,
    use_parallel = TRUE,
    scale_ranges = NULL,
    boot_nonzero = FALSE,
    seed = 1
){

  type <- match.arg(type)
  index_by <- match.arg(index_by)
  dv_metric <- match.arg(dv_metric)
  impact_readoff <- match.arg(impact_readoff)
  community_lift <- match.arg(community_lift)
  max_impact_anchor <- match.arg(max_impact_anchor)

  # Preserve named dv for meta, strip for bnlearn
  dv_original <- dv
  dv <- unname(dv)

  # An exempted subgroup gets the engine's threshold dropped to 0 rather than a
  # flag threaded through the engine: the gate is a single `base_n <
  # min_base_for_lift` test, so 0 disables it outright while the engine's
  # separate empty-scope guard still blanks scopes with nothing to shift.
  .min_base_for <- function(sg_name) {
    if (!is.null(override_min_base_for_lift) &&
        sg_name %in% override_min_base_for_lift) 0 else min_base_for_lift
  }

  # Run the engine ONCE with all four shift variants (proportional +
  # absolute + headroom + range). Since 2026-07-28 the engine computes every
  # variant in a single pass and emits lift columns already tagged with
  # `_propshift_` / `_absshift_` / `_headshift_` / `_rangeshift_`, so the
  # bootstrap loop, MI, maxVmin, and base are computed once instead of once
  # per variant (previously this wrapper ran the engine four times and
  # merged the lift columns afterwards).
  .dual_engine_call <- function(engine_args) {
    do.call(bn_impact_engine, c(engine_args, list(
      impact_shift_type = c("proportional", "absolute", "headroom", "range")
    )))
  }

  if(process_subgroups){

    first_subgroup <- names(obj)[[1]]

    n_subgroups <- length(obj)
    subgroup_names <- names(obj)
    show_subgroup_progress <- isTRUE(n_boot > 1)
    if (show_subgroup_progress) {
      progress_prefix <- if (do_community) "community impact" else "attribute impact"
      weight_tag <- if (is.null(weight)) "unweighted" else "weighted"
      mode_tag <- if (use_parallel) "parallel" else "serial"
      progress_suffix <- glue::glue("({weight_tag}, {n_boot} boots, {mode_tag})")
    }

    .engine_call <- function(.x, .y){
      if (show_subgroup_progress) {
        i <- match(.y, subgroup_names)
        sg <- .y
        cli::cli_alert_info(
          "Running {progress_prefix} - Subgroup {i} of {n_subgroups} - {sg} {progress_suffix}"
        )
      }
      .dual_engine_call(list(
        obj = .x,
        df = df %>%
          dplyr::filter(.data[[.y]] == 1) %>%
          droplevels() %>%
          as.data.frame(),
        dv = dv,
        ivs = ivs,
        do_community = do_community,
        community_assignment = community_assignment,
        community_impact_attributes = community_impact_attributes,
        impact_readoff = impact_readoff,
        community_lift = community_lift,
        max_impact_anchor = max_impact_anchor,
        max_impact_min_support = max_impact_min_support,
        max_impact_shrinkage = max_impact_shrinkage,
        min_boot_coverage = min_boot_coverage,
        boot_inference_legacy = boot_inference_legacy,
        id = id,
        type = type,
        index_by = index_by,
        n_boot = n_boot,
        n_querry = n_querry,
        lift = lift,
        brand = brand,
        brand_names = brand_names,
        min_base_for_lift = .min_base_for(.y),
        include_base = include_base,
        dv_metric = dv_metric,
        weight = weight,
        mi_boot = mi_boot,
        scale_ranges = scale_ranges,
        boot_nonzero = boot_nonzero,
        seed = seed
      )) %>%
        setNames(glue::glue("{.y}_{names(.)}"))
    }

    if (verbose) {
      output <- imap_progress(obj, .engine_call, .parallel = use_parallel)
    } else if (use_parallel) {
      output <- furrr::future_imap(
        obj, .engine_call,
        .options = furrr::furrr_options(seed = TRUE, scheduling = Inf)
      )
    } else {
      output <- purrr::imap(obj, .engine_call)
    }

    output <- output %>%
      dplyr::bind_cols() %>%
      dplyr::rename(Variable = !!paste0(first_subgroup, "_variable")) %>%
      dplyr::select(-dplyr::ends_with("_variable"))

    names(output) <- names(output) %>% gsub("_index$", "", .)

  }else{

    output <- .dual_engine_call(list(
      obj = obj,
      df = df,
      dv = dv,
      ivs = ivs,
      do_community = do_community,
      community_assignment = community_assignment,
      community_impact_attributes = community_impact_attributes,
      impact_readoff = impact_readoff,
      community_lift = community_lift,
      max_impact_anchor = max_impact_anchor,
      max_impact_min_support = max_impact_min_support,
      max_impact_shrinkage = max_impact_shrinkage,
      min_boot_coverage = min_boot_coverage,
      boot_inference_legacy = boot_inference_legacy,
      id = id,
      type = type,
      index_by = index_by,
      n_boot = n_boot,
      n_querry = n_querry,
      lift = lift,
      brand = brand,
      brand_names = brand_names,
      min_base_for_lift = .min_base_for("Total"),
      include_base = include_base,
      dv_metric = dv_metric,
      weight = weight,
      mi_boot = mi_boot,
      scale_ranges = scale_ranges,
      boot_nonzero = boot_nonzero,
      seed = seed
    )) %>%
      dplyr::rename(Variable = variable)

  }


  if(!do_community && !is.null(dictionary)){

    dictionary <- work::dictionary_from_named_object(dictionary)

    output <- output %>%
      dplyr::left_join(
        dictionary,
        by = dplyr::join_by(Variable == var)
      ) %>%
      dplyr::relocate(label, .after = "Variable") %>%
      dplyr::rename("Label" = "label")

  }



  if(!do_community && !is.null(community_assignment)){
    output <- output %>%
      dplyr::left_join(
        community_assignment %>%
          dplyr::mutate(community_name = as.character(community_name)) %>%
          dplyr::select(id, community_name),
        by = dplyr::join_by(Variable == id)
      ) %>%
      dplyr::rename(Community = community_name)

    if("Label" %in% names(output)){
      output <- output %>% dplyr::relocate(Community, .before = Label)
    } else {
      output <- output %>% dplyr::relocate(Community, .after = Variable)
    }
  }


  if(do_community){
    output <- output %>% dplyr::rename("Community" = "Variable")
  }



  # Resolve brand names
  brand_names_resolved <- if (!is.null(brand) && brand %in% names(df)) {
    all_brands <- sort(unique(as.character(df[[brand]])))
    if (!is.null(brand_names)) intersect(all_brands, brand_names) else all_brands
  } else {
    NULL
  }

  # Round numeric metric columns to 6 decimals so both output paths (Excel
  # data sheet + bn_report JSON) carry identical floats. Without this, the
  # Excel SUMPRODUCT and the JS Array.reduce paths can return means that
  # differ by a single ULP, which pushes some index values across an integer
  # rounding boundary and produces ±1 discrepancies between the two outputs.
  # 6 decimals is ample precision for impact values (typically ~0.01 range)
  # and doesn't affect any downstream computation meaningfully.
  .num_metric_cols <- setdiff(
    names(output)[vapply(output, is.numeric, logical(1))],
    c("index")  # engine-level index is already integer-scale, leave alone
  )
  if (length(.num_metric_cols) > 0) {
    output[.num_metric_cols] <- lapply(output[.num_metric_cols], round, digits = 6)
  }

  list(
    table = output,
    meta = list(
      type = type,
      index_by = index_by,
      lift = lift,
      subgroups = if (process_subgroups) names(obj) else NULL,
      dv = dv_original,
      dv_metric = dv_metric,
      # "Outcome is dichotomous" is broader than "DV has 2 levels":
      # a top_box analysis always produces values in [0, 1] regardless
      # of how many DV levels the underlying variable has, because the
      # dashboard reads P(DV = top). True binary DVs under "mean" also
      # produce [0, 1] probabilities. Either case → Point Change reads
      # more naturally than % Change.
      # Downstream dashboards (bn_report, bn_impact_write) use this flag
      # to auto-default the Outcome Display dropdown.
      is_dichotomous_dv = {
        if (identical(dv_metric, "top_box")) {
          TRUE
        } else {
          col <- df[[unname(dv)]]
          if (is.null(col)) FALSE
          else if (is.factor(col)) length(levels(col)) == 2L
          else length(unique(stats::na.omit(col))) == 2L
        }
      },
      brand = brand,
      brand_names = brand_names_resolved,
      min_base_for_lift = min_base_for_lift,
      # Subgroups the threshold above was waived for, so writers can caveat
      # their cells rather than presenting them as having cleared the base.
      override_min_base_for_lift = override_min_base_for_lift,
      # Respondent-level id column (or NULL). Governs whether min_base_for_lift
      # and the reported bases count respondents or stacked records.
      id = id,
      # Survey-weight column name (or NULL). Retained so downstream writers
      # (e.g. append_bn_simulator) can use the same weight without having
      # to re-thread it through every caller.
      weight = weight,
      # Bootstrap tracking — set when bootstrap was actually applied so
      # downstream writers (e.g., bn_impact_write guide) can describe what
      # ran rather than what could have run.
      n_boot = n_boot,
      boot_applied = isTRUE(n_boot > 1),
      mi_boot = mi_boot,
      mi_boot_applied = isTRUE(!is.null(mi_boot) && do_community)
    )
  )
}
