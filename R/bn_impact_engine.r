#' Estimate Variable Impact in a Bayesian Network
#'
#' @description
#' Computes the estimated impact of independent variables (IVs) on a dependent
#' variable (DV) in a Bayesian network. Supports three estimation methods:
#' exact inference via \code{gRain} (\code{"gr"}), Monte Carlo conditional
#' probability queries (\code{"cp"}), and mutual information (\code{"mi"}).
#' Optionally applies bootstrapping for uncertainty estimation.
#'
#' @param obj A fitted Bayesian network object. Accepts:
#'   \itemize{
#'     \item A list from \code{bn_engine()} (auto-extracts DV, IVs, fitted model)
#'     \item A \code{bnlearn::bn.fit} object
#'     \item A \code{bnlearn::bn} structure (will be fitted with \code{method = "bayes"})
#'   }
#' @param df A data frame containing the DV and IV columns.
#' @param dv Character scalar. Dependent variable name. Optional if \code{obj}
#'   is a \code{bn_engine()} result with metadata.
#' @param ivs Character vector. Independent variable names. Optional if \code{obj}
#'   is a \code{bn_engine()} result with metadata.
#' @param do_community Logical. If \code{TRUE}, computes impact at the community
#'   level by jointly setting all IVs within each community. Default FALSE.
#' @param community_assignment Optional data frame with \code{id} and
#'   \code{community_name} columns mapping IVs to communities.
#' @param community_impact_attributes Character vector or NULL. Battery names
#'   (variable-name prefixes, e.g. \code{"q14a"}) whose attributes are included
#'   when computing community-level impacts. An IV's battery is its variable
#'   name with the trailing \code{"_<number>"} suffix removed
#'   (\code{q14a_1} -> \code{"q14a"}). Default NULL includes all attributes.
#'   Errors if a declared battery matches no IV in the community assignment.
#'   Applies to every community metric (lift, maxVmin, MI, base); ignored when
#'   \code{do_community = FALSE}.
#' @param impact_readoff Character. Where the per-level DV expectation
#'   E\[DV | IV = level\] used by the lift metrics comes from.
#'   \code{"empirical"} (default): weighted conditional means computed
#'   directly from the (resampled) data - deterministic, no inference
#'   queries. \code{"model"}: the fitted network's conditional via
#'   \code{gRain::querygrain()} / \code{bnlearn::cpquery()} - this was the
#'   methodology used prior to 2026-07-28. In networks built with direct
#'   DV connections (\code{all_ivs_connect_to_dv = TRUE}) the two are
#'   numerically near-identical; they can diverge when IVs connect to the
#'   DV only through other nodes. Does not affect the maxVmin family
#'   (see \code{max_impact_anchor}) or MI.
#' @param community_lift Character. How community-level lift columns are
#'   computed. \code{"joint"} (default): theme effect - rake (IPF) the
#'   observed rows' weights so every member attribute's marginal matches its
#'   shifted target simultaneously, then read the DV change under the raked
#'   weights; preserves the observed correlation structure among members and
#'   only ever places mass on observed joint profiles. The rake's read-off
#'   is inherently empirical; paired with \code{impact_readoff = "model"}
#'   the attribute lifts and anchor values stay model-based while the
#'   community lift columns are empirical (\code{bn_impacts()} warns once
#'   about the mix). \code{"average"}: arithmetic mean
#'   of the members' individually-computed lifts - this was the methodology
#'   used prior to 2026-07-28. Attribute-level runs and the maxVmin / MI /
#'   base columns are unaffected.
#' @param max_impact_anchor Character. How the Best-vs-Worst (maxVmin) family
#'   (\code{dv_max_value}, \code{dv_min_value}, \code{maxVmin_*}) is
#'   anchored. \code{"observed"} (default): anchors must be observed with at
#'   least \code{max_impact_min_support} respondents - for a single
#'   attribute, the max/min scale levels among supported levels (sign
#'   semantics preserved); for a community, the observed joint member
#'   profiles with the highest/lowest empirical weighted E\[DV\]. Anchor
#'   selection is always empirical; the anchor VALUES are then read per
#'   \code{impact_readoff} (\code{"model"} queries the network at the two
#'   chosen observed anchors). \code{"theoretical"}: hypothetical
#'   all-max / all-min evidence configurations - this was the methodology
#'   used prior to 2026-07-28; for multi-member communities those
#'   configurations are mostly unobserved, so values there lean on CPT
#'   smoothing (extrapolation).
#' @param max_impact_min_support Integer. Minimum respondent count for an
#'   anchor candidate under \code{max_impact_anchor = "observed"}. Falls
#'   back to all observed candidates if fewer than two clear the threshold.
#'   Default 5.
#' @param max_impact_shrinkage Numeric >= 0. Empirical-Bayes prior weight
#'   used under \code{max_impact_anchor = "observed"}: each anchor
#'   candidate's E\[DV\] is shrunk toward the scope mean with this many
#'   pseudo-respondents before ranking (and, under
#'   \code{impact_readoff = "empirical"}, before reading the anchor
#'   values). Guards the Best-vs-Worst argmax against winner's curse -
#'   with a binary DV and raw means, the best observed profile is
#'   otherwise routinely a small all-top-box cell at exactly 1.0.
#'   \code{0} disables shrinkage. Default 20.
#' @param min_boot_coverage Numeric in (0, 1]. Minimum share of bootstrap
#'   replicates that must yield a value for a metric cell to be reported.
#'   Cells below the threshold - e.g. joint community shifts that are
#'   infeasible in most resamples of a small subgroup, or brand scopes
#'   flickering around \code{min_base_for_lift} - have their value, sd, CI,
#'   and p-value blanked rather than silently averaging the surviving
#'   replicates (a selection-biased subset). Cells that pass use the
#'   feasible-replicate count for the se / t degrees of freedom. Only
#'   applies when \code{n_boot > 1}. Default 0.9.
#' @param lift Numeric scalar or vector. Target percentage lift(s) for
#'   distribution-aware impact. Uses \code{bn_freq_prob_shift()} to shift each
#'   IV's observed distribution by each fraction (e.g., 0.10 = 10 percent),
#'   then computes the resulting DV probability change. Computed when
#'   \code{type = "gr"} (exact) or \code{type = "cp"} (Monte Carlo).
#'   When a lift value is 0, computes symmetric sensitivity: E_(+5 percent) -
#'   E_(-5 percent). A scalar produces a single \code{lift} column; a vector
#'   (e.g., \code{c(0, 0.05, 0.10)}) produces \code{lift_0}, \code{lift_5},
#'   \code{lift_10}. Default \code{c(0, 0.1)}.
#' @param min_base_for_lift Integer. Minimum sample size (frequency count)
#'   required to compute a lift value. If the distribution has fewer than this
#'   many observations, the lift cell returns \code{NA}. Applied per-brand when
#'   \code{brand} is set. Default \code{75}.
#' @param type Character. Estimation method:
#'   \itemize{
#'     \item \code{"gr"} (default): exact junction-tree inference via \code{gRain}.
#'       Deterministic, fast, no Monte Carlo noise.
#'     \item \code{"cp"}: Monte Carlo conditional probability via
#'       \code{bnlearn::cpquery()} with likelihood weighting.
#'     \item \code{"mi"}: mutual information test only (no probability lift).
#'   }
#' @param dv_metric Character. What to compute from the DV distribution per
#'   IV level: \code{"mean"} (default) uses \code{E[DV | IV=v]} — the expected
#'   value across all DV levels (works with any scale: 3-point, 5-point,
#'   7-point, etc.). \code{"top_box"} uses \code{P(DV_max | IV=v)} — the
#'   probability of the highest DV level.
#' @param impact_shift_type Character vector. How \code{lift} values are
#'   interpreted when shifting IV distributions: \code{"proportional"}
#'   (fraction of the current mean), \code{"absolute"} (fixed scale points),
#'   \code{"headroom"} (fraction of the room toward the boundary), and
#'   \code{"range"} (fraction of the theoretical scale range) — see
#'   \code{bn_freq_prob_shift()}. When several values are supplied (the
#'   default: all four), a single pass computes every variant and the lift
#'   columns carry \code{_propshift_} / \code{_absshift_} /
#'   \code{_headshift_} / \code{_rangeshift_} tags; the boot loop, MI,
#'   maxVmin, and base are computed once. Supplying a single value
#'   reproduces the legacy untagged single-variant output (prior to
#'   2026-07-28 \code{bn_impact()} ran the engine once per variant and
#'   merged afterwards).
#' @section Outcome-display variants:
#'   Both proportional and absolute variants of the DV-outcome metrics are
#'   always emitted as separate columns, so downstream writers (e.g., the
#'   dashboard built by \code{bn_impact_write()}) can switch between them
#'   at runtime without re-running the engine. Column names:
#'   \itemize{
#'     \item \code{maxVmin_propdisplay} = \code{(p1 - p0) / p0} (relative
#'       ratio, unbounded above for binary DVs)
#'     \item \code{maxVmin_absdisplay} = \code{p1 - p0} (probability-point
#'       difference, bounded in \code{[-1, 1]} for binary DVs)
#'     \item \code{lift_<N>_propdisplay} = lift shifted / observed
#'     \item \code{lift_<N>_absdisplay} = lift (raw probability-point change)
#'   }
#'   (Previously one \code{impact_outcome_display_type} parameter controlled
#'   which variant was emitted; it has been retired in favor of emitting
#'   both unconditionally.)
#' @param include_base Logical. Whether to include base-size columns alongside
#'   brand-specific lift columns. Default \code{TRUE}.
#' @param index_by Character. Which metric to use for the index column:
#'   \code{"lift_first"} (default), \code{"lift_second"}, \code{"maxVmin"},
#'   \code{"mi"}, or \code{"none"} (no index column).
#'   \code{"lift_first"} and \code{"lift_second"} select the first or second
#'   lift column when \code{lift} is a vector.
#' @param n_boot Integer. Number of bootstrap replicates. If \code{n_boot = 1}
#'   (recommended default), computes a single point estimate without bootstrapping.
#'   See \strong{When to bootstrap} below.
#' @param n_querry Integer. Number of Monte Carlo samples per \code{cpquery()} call.
#'   Only used when \code{type = "cp"}. Default \code{1e4}.
#' @param brand Character scalar or \code{NULL}. Column name in \code{df}
#'   containing brand (or segment) labels. When provided, the lift metric is
#'   computed using brand-specific frequency distributions rather than the
#'   overall distribution. Produces separate lift columns per brand (e.g.,
#'   \code{lift_Apex}, \code{lift_Vero}). The DV probability queries are shared
#'   across brands; only the observed distribution changes. Default \code{NULL}.
#' @param brand_names Character vector or NULL. When provided, only compute
#'   brand-specific lift for these brand levels. Brands not in this vector are
#'   skipped. Market-level lift is always computed. Default NULL (all brands).
#' @param weight Character or NULL. Column name in \code{df} containing
#'   observation weights. When provided, frequency distributions used for
#'   lift calculations are weighted. Default NULL.
#' @param mi_boot Integer or NULL. When set and \code{do_community = TRUE},
#'   bootstraps the MI calculation this many times to derive a p-value from the
#'   bootstrap distribution instead of the chi-squared approximation. The
#'   chi-squared p-value is unreliable for community-level MI because the
#'   composite factor has too many levels relative to sample size. Bootstrap
#'   p-value is the proportion of replicates with MI at or below zero.
#'   Default NULL (use analytic p-value).
#' @param boot_nonzero Logical. Default \code{FALSE}: classical bootstrap
#'   inference - the SD of the bootstrap replicates is used directly as the
#'   standard error of each metric, so p-values are invariant to
#'   \code{n_boot}. \code{TRUE} restores the legacy behavior
#'   (\code{se = sd/sqrt(n_boot)}), which tests whether the mean of the
#'   boot distribution is nonzero and mechanically shrinks p-values as
#'   \code{n_boot} grows.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @details
#' For each IV (or community of IVs when \code{do_community = TRUE}), the function:
#' \enumerate{
#'   \item Fits (or reuses) a Bayesian network via \code{bnlearn::bn.fit()}.
#'   \item \strong{maxVmin}: estimates the probability of the DV at its maximum
#'     level when the IV is set to its observed max vs min.
#'     \code{maxVmin = P(DV_max | IV_max) - P(DV_max | IV_min)}.
#'     This is the theoretical maximum effect of the IV on the DV.
#'   \item \strong{lift} (types \code{"gr"} and \code{"cp"}): shifts the IV's observed
#'     frequency distribution by \code{lift} percent using
#'     \code{bn_freq_prob_shift()}, then computes
#'     \code{lift = sum(P(DV_max | IV=v) * p_shifted(v)) - sum(P(DV_max | IV=v) * p_observed(v))}.
#'     This is the distribution-aware impact: it accounts for where respondents
#'     currently sit on the IV, so IVs with little headroom produce small lift
#'     values even if maxVmin is large.
#'   \item Computes mutual information (MI) between each IV and the DV. For
#'     individual attributes, this is a standard unconditional MI test via
#'     \code{bnlearn::ci.test()}. For communities (\code{do_community = TRUE}),
#'     a conditional MI chain is used: each attribute is tested sequentially,
#'     conditioning on all previously tested attributes in the community.
#'     \code{MI(IV1; DV) + MI(IV2; DV | IV1) + MI(IV3; DV | IV1, IV2) + ...}
#'     The G-statistics and degrees of freedom are summed across steps to
#'     produce a single chi-squared test. This avoids the dimensionality
#'     problem of testing all community attributes jointly (which creates a
#'     composite factor with too many levels relative to sample size) and
#'     prevents double-counting shared information between correlated
#'     attributes.
#'   \item Optionally bootstraps steps 1-4 to produce standard errors, confidence
#'     intervals, and p-values.
#' }
#'
#' Bootstrap replicates resample rows and refit parameters to the fixed network
#' structure, capturing parameter uncertainty without conflating it with
#' structural uncertainty.
#'
#' \strong{When to bootstrap}
#'
#' For most use cases (driver ranking, executive dashboards), \code{n_boot = 1}
#' with \code{type = "gr"} is sufficient. The MI p-value already provides
#' evidence strength (whether the IV-DV relationship is statistically real),
#' and the \code{"gr"} probability lift is deterministic with no Monte Carlo
#' noise, so the point estimate is stable without averaging across replicates.
#' Together, the lift and MI p-value deliver both dimensions of the
#' Impact x Evidence framework without any bootstrapping.
#'
#' Use \code{n_boot > 1} when you need:
#' \itemize{
#'   \item Confidence intervals on the probability lift itself (e.g., for
#'     technical reports or publications).
#'   \item A formal test of whether the \emph{lift} is distinguishable from
#'     zero (conceptually different from MI's test of \emph{association}).
#'   \item To assess whether two drivers' effects significantly differ
#'     (via overlapping confidence intervals).
#' }
#'
#' @return
#' A tibble with one row per variable (or community) containing:
#' \itemize{
#'   \item \code{variable}: IV or community name
#'   \item \code{dv_max_value}: P(DV_max | IV_max) (types \code{"gr"} and \code{"cp"})
#'   \item \code{maxVmin}: max-vs-min probability lift (types \code{"gr"} and \code{"cp"})
#'   \item \code{lift}: distribution-aware DV change from a \code{lift} percent
#'     shift in the IV (types \code{"gr"} and \code{"cp"})
#'   \item \code{mi}: normalized mutual information (all types)
#'   \item \code{p_val}: MI chi-squared p-value (all types)
#'   \item \code{index}: relative impact index based on \code{index_by}
#'     (omitted when \code{index_by = "none"})
#' }
#'
#' When \code{n_boot > 1}, maxVmin, lift, and MI columns are expanded with
#' bootstrap summary statistics: \code{_mean}, \code{_sd}, \code{_se},
#' \code{_t}, \code{_ci_low}, \code{_ci_high}, \code{_p_value}.
#'
#' @examples
#' \dontrun{
#' # --- From a bn_engine() result ---
#' bn_obj <- work::bn_engine(df = my_data, dv = "satisfaction", ivs = iv_names)
#' bn_impact_engine(obj = bn_obj, df = my_data, type = "gr", n_boot = 500)
#'
#' # --- From a bare bnlearn object ---
#' bn_struct <- bnlearn::hc(my_data)
#' bn_impact_engine(
#'   obj = bn_struct,
#'   df = my_data,
#'   dv = "satisfaction",
#'   ivs = c("quality", "price", "service"),
#'   type = "gr",
#'   n_boot = 1
#' )
#'
#' # --- MI only (no probability lift) ---
#' bn_impact_engine(obj = bn_obj, df = my_data, type = "mi", n_boot = 200)
#' }
#'
#' @seealso [bn_impact()], [bnlearn::bn.fit()], [gRain::querygrain()]
#'
#' @export
bn_impact_engine <- function(
    obj,
    df,
    dv = NULL,
    ivs = NULL,
    do_community = FALSE,
    community_assignment = NULL,
    community_impact_attributes = NULL,
    impact_readoff = c("empirical", "model"),
    community_lift = c("joint", "average"),
    max_impact_anchor = c("observed", "theoretical"),
    max_impact_min_support = 5,
    max_impact_shrinkage = 20,
    min_boot_coverage = 0.9,
    lift = c(0, 0.1),
    min_base_for_lift = 75,
    type = c("gr", "cp", "mi"),
    dv_metric = c("mean", "top_box"),
    impact_shift_type = c("proportional", "absolute", "headroom", "range"),
    include_base = TRUE,
    index_by = c("lift_first", "lift_second", "maxVmin", "mi", "none"),
    n_boot = 1,
    n_querry = 1e4,
    brand = NULL,
    brand_names = NULL,
    weight = NULL,
    mi_boot = NULL,
    scale_ranges = NULL,
    boot_nonzero = FALSE,
    seed = 1
){

  type <- match.arg(type)
  index_by <- match.arg(index_by)
  impact_shift_type <- match.arg(impact_shift_type, several.ok = TRUE)
  dv_metric <- match.arg(dv_metric)
  impact_readoff <- match.arg(impact_readoff)
  community_lift <- match.arg(community_lift)
  max_impact_anchor <- match.arg(max_impact_anchor)
  work::assert_positive_integer(max_impact_min_support, "max_impact_min_support")
  if (!is.numeric(max_impact_shrinkage) || length(max_impact_shrinkage) != 1 ||
      is.na(max_impact_shrinkage) || max_impact_shrinkage < 0) {
    stop("'max_impact_shrinkage' must be a single non-negative number.")
  }
  if (!is.numeric(min_boot_coverage) || length(min_boot_coverage) != 1 ||
      is.na(min_boot_coverage) || min_boot_coverage <= 0 || min_boot_coverage > 1) {
    stop("'min_boot_coverage' must be a single number in (0, 1].")
  }

  ivs <- ivs %>% unlist() %>% setNames(NULL)

  # ---------------------------
  # Validate required arguments
  # ---------------------------
  if (!is.data.frame(df)) stop("'df' must be a data frame.")
  work::assert_positive_integer(n_boot, "n_boot")
  if (!is.null(seed)) work::assert_numeric_scalar(seed, "seed")
  if (!is.null(brand) && !brand %in% names(df)) {
    stop("'brand' column '", brand, "' not found in df. Available columns: ",
         paste(head(names(df), 20), collapse = ", "))
  }
  if (!is.null(weight) && !weight %in% names(df)) {
    stop("'weight' column '", weight, "' not found in df. Available columns: ",
         paste(head(names(df), 20), collapse = ", "))
  }

  # Ensure DV and IV(s) are provided
  if (is.null(dv) && !"meta" %in% names(obj)) {
    stop("'dv' (dependent variable) must be specified.")
  }

  # Column existence checks
  work::assert_cols_exist(df, dv, "data frame for bn_impact_engine()")


  if (!is.null(ivs)) {
    work::assert_cols_exist(df, ivs %>% unlist(), "data frame for bn_impact_engine() [ivs]")
  }


  # BN structure check (if user passed a bn_engine() result)
  if ("meta" %in% names(obj)) {
    work::assert_list_elements_exist(obj, c("bn", "fit", "summary"), "bn object from bn_engine()")
  }


  # ---------------------------
  # Set up
  # ---------------------------

  if("meta" %in% names(obj)){
    if(obj[["meta"]][["analysis"]] == "bn_model_single"){
      if(is.null(dv)) dv <- obj[["meta"]][["dv"]]
      if(is.null(ivs)) ivs <- obj[["meta"]][["ivs"]] %>% unlist() %>% setNames(NULL)
      fit <- obj[["fit"]]
      bn <- obj[["bn"]]

      if(do_community && is.null(community_assignment)){
        community_assignment <- obj[["viz_prep"]][["attribute_viz_prep"]][["nodes"]]
      }
    }
  } else if (inherits(obj, "bn.fit")) {
    # Extract underlying bn structure if obj is a bn.fit
    bn <- bnlearn::bn.net(obj)
    fit <- obj
  } else if (inherits(obj, "bn")) {
    bn <- obj
    # Brand/weight columns are not network nodes (weight may be numeric, which
    # the "bayes" estimator rejects), so exclude them from the fitting data
    exclude_cols <- c(brand, weight)
    fit_data <- if (length(exclude_cols) > 0) df[, setdiff(names(df), exclude_cols), drop = FALSE] else df
    fit <- bnlearn::bn.fit(bn, fit_data, method = "bayes")
  } else {
    stop("'obj' must be a 'bnlearn::bn', 'bnlearn::bn.fit', or a list returned from bn_engine().")
  }


  df <- df %>%
    dplyr::select(dplyr::all_of(
      c(dv, ivs, brand, weight) %>% unlist() %>% setNames(NULL)
    )) %>%
    as.data.frame()


  if(type == "cp") dv_max <- df[[dv]] %>% as.character() %>% as.numeric() %>% max(na.rm = TRUE) else dv_max <- NULL

  ivs_max <- NULL
  ivs_min <- NULL
  if(type != "mi"){
    ivs_max <- df %>% dplyr::summarise(dplyr::across(dplyr::all_of(ivs), ~as.character(.x) %>% as.numeric() %>% max(na.rm = TRUE))) %>% as.list()
    ivs_min <- df %>% dplyr::summarise(dplyr::across(dplyr::all_of(ivs), ~as.character(.x) %>% as.numeric() %>% min(na.rm = TRUE))) %>% as.list()
  }



  if(do_community){
    community_assignment <- community_assignment %>%
      dplyr::filter(id %in% ivs)

    # Restrict community membership to the declared batteries. Runs BEFORE
    # the list conversion below so every community metric downstream (lift,
    # maxVmin, MI, base) sees the same filtered membership.
    if(!is.null(community_impact_attributes)){
      keep_ids <- .bn_community_impact_ids(
        community_assignment[["id"]], community_impact_attributes
      )
      community_assignment <- community_assignment %>%
        dplyr::filter(id %in% keep_ids)
    }

    community_assignment <- community_assignment %>%
      dplyr::select(community_name, id) %>%
      dplyr::group_split(community_name) %>%
      setNames(
        purrr::map(., ~ unique(.x[["community_name"]]))
      ) %>%
      purrr::map(~ .x[["id"]])

  }else if(!do_community){
    community_assignment <- NULL
  }else{
    stop("Unknown do_community value.")
  }



  # ---------------------------
  # Helper function
  # ---------------------------
  list_to_text_each <- function(x) {
    if (!is.list(x)) stop("Input must be a list.")

    lapply(seq_along(x), function(i) {
      el <- x[[i]]

      # Case 1: element is a single value
      if (length(el) == 1) {
        nm <- names(x)[i]
        val <- el
        return(paste0("list(", nm, " = '", val, "')"))
      }

      # Case 2: element is a named vector (e.g. c(q14a_3='2', q14a_4='1'))
      if (!is.null(names(el))) {
        inner <- paste0(names(el), " = '", el, "'", collapse = ", ")
        return(paste0("list(", inner, ")"))
      }
    }) %>%
      unlist() %>%
      setNames(NULL)
  }


  # ---------------------------
  # Core difference function
  # ---------------------------
  engine_diff_single_attribute <- function(
    fit_boot, grain_bn = NULL, dv = NULL, iv, attr_iv_boot_max, attr_iv_boot_min, attr_dv_boot_max = NULL,
    type = c("cp", "gr"), n_querry = 1e5, dv_metric = "top_box", seed = NULL
  ){

    if(type == "gr"){

      if (dv_metric == "top_box") {
        p1 <- gRain::querygrain(grain_bn, nodes = dv, evidence = attr_iv_boot_max, simplify = TRUE) %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
        p0 <- gRain::querygrain(grain_bn, nodes = dv, evidence = attr_iv_boot_min, simplify = TRUE) %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
      } else {
        dist1 <- gRain::querygrain(grain_bn, nodes = dv, evidence = attr_iv_boot_max, simplify = TRUE)
        p1 <- sum(as.numeric(names(dist1)) * as.numeric(dist1))
        dist0 <- gRain::querygrain(grain_bn, nodes = dv, evidence = attr_iv_boot_min, simplify = TRUE)
        p0 <- sum(as.numeric(names(dist0)) * as.numeric(dist0))
      }

    }else if(type == "cp"){

      if (dv_metric == "top_box") {
        if (!is.null(seed)) set.seed(seed)
        p1 <- eval(parse(text = glue::glue(
          "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{attr_dv_boot_max}'), evidence = {attr_iv_boot_max}, n = {n_querry}, method = 'lw')"
        )))

        if (!is.null(seed)) set.seed(seed)
        p0 <- eval(parse(text = glue::glue(
          "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{attr_dv_boot_max}'), evidence = {attr_iv_boot_min}, n = {n_querry}, method = 'lw')"
        )))
      } else {
        dv_scale <- fit_boot[[dv]] %>% dimnames() %>% .[[1]] %>% as.numeric()

        if (!is.null(seed)) set.seed(seed)
        p1 <- purrr::map_dbl(dv_scale, function(d) {
          eval(parse(text = glue::glue(
            "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{d}'), evidence = {attr_iv_boot_max}, n = {n_querry}, method = 'lw')"
          )))
        }) %>% { sum(dv_scale * .) }

        if (!is.null(seed)) set.seed(seed)
        p0 <- purrr::map_dbl(dv_scale, function(d) {
          eval(parse(text = glue::glue(
            "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{d}'), evidence = {attr_iv_boot_min}, n = {n_querry}, method = 'lw')"
          )))
        }) %>% { sum(dv_scale * .) }
      }
    }

    # Emit both outcome-display variants. Both are trivial derivations of
    # p1 and p0, so we always compute and store both — the dashboard's
    # Outcome Display dropdown then picks which one to read.
    maxVmin_propdisplay <- (p1 - p0) / p0
    maxVmin_absdisplay  <- p1 - p0

    data.frame(
      variable            = iv,
      dv_max_value        = p1,
      dv_min_value        = p0,
      maxVmin_propdisplay = maxVmin_propdisplay,
      maxVmin_absdisplay  = maxVmin_absdisplay
    )
  }



  # ---------------------------
  # Multiple  difference function
  # ---------------------------

  engine_diff_multiple <- function(
    data, indices = NULL, bn, ivs, ivs_max, ivs_min, dv_max = NULL,
    community_assignment = NULL,
    add_mi = TRUE, type = c("cp", "gr", "mi"),
    n_querry = 1e5, lift = c(0, 0.1),
    impact_shift_type = "proportional",
    brand = NULL,
    min_base_for_lift = 75, include_base = TRUE, dv_metric = "mean", weight = NULL, fit = NULL, mi_boot = NULL, seed = 1
  ){
    type <- match.arg(type)

    if(!is.null(indices)) dat_boot <- data[indices, , drop = FALSE] else dat_boot <- data

    # Exclude brand column from model fitting data
    exclude_cols <- c(brand, weight)
    fit_data <- if (length(exclude_cols) > 0) dat_boot[, setdiff(names(dat_boot), exclude_cols), drop = FALSE] else dat_boot

    if(type != "mi"){

      # The fitted network / compiled junction tree are only needed when a
      # model read-off or theoretical anchoring is in play. Under the
      # defaults (empirical read-off + observed anchors) both are skipped -
      # the main per-replicate cost of the bootstrap.
      need_model <- impact_readoff == "model" || max_impact_anchor == "theoretical"
      need_fit   <- need_model || type == "cp"

      if(is.null(fit)) {
        fit_boot <- if (need_fit) bnlearn::bn.fit(bn, fit_data, method = "bayes") else NULL
      } else {
        fit_boot <- fit
      }

      if(!all(purrr::map_lgl(ivs, ~ ivs_max[[.x]] %in% dat_boot[[.x]]))){
        iv_boot_max <- dat_boot %>% dplyr::summarise(dplyr::across(dplyr::all_of(ivs), ~as.character(.x) %>% as.numeric() %>% max(na.rm = TRUE))) %>% as.list()
      }else{
        iv_boot_max <- ivs_max
      }

      if(!all(purrr::map_lgl(ivs, ~ ivs_min[[.x]] %in% dat_boot[[.x]]))){
        iv_boot_min <- dat_boot %>% dplyr::summarise(dplyr::across(dplyr::all_of(ivs), ~as.character(.x) %>% as.numeric() %>% min(na.rm = TRUE))) %>% as.list()
      }else{
        iv_boot_min <- ivs_min
      }


      if(type == "cp"){

        iv_boot_max <- setNames(list_to_text_each(iv_boot_max), names(iv_boot_max))
        iv_boot_min <- setNames(list_to_text_each(iv_boot_min), names(iv_boot_min))


        if(!dv_max %in% dat_boot[[dv]]){
          dv_boot_max <- dat_boot[[dv]] %>% as.character() %>% as.numeric() %>% max(na.rm = TRUE)
        }else{
          dv_boot_max <- dv_max
        }

        dv_boot_max <- dv_boot_max %>% as.character()
        grain_bn <- NULL

      }else if(type == "gr"){

        iv_boot_max <- iv_boot_max %>% lapply(as.character)
        iv_boot_min <- iv_boot_min %>% lapply(as.character)

        grain_bn <- if (need_model) bnlearn::as.grain(fit_boot) %>% gRain:::compile.grain() else NULL

      }
    }


    if(type != "mi"){

      if(!is.null(community_assignment)) temp_ivs <- community_assignment else temp_ivs <- ivs %>% setNames(ivs)

      if (max_impact_anchor == "observed") {

        # Observed anchoring ("theoretical" all-max/all-min evidence was the
        # pre-2026-07-28 methodology). Anchors are restricted to what the
        # (resampled) data actually contains with >= max_impact_min_support
        # respondents: single attributes keep their max/min SCALE levels
        # (so negative relationships keep their sign) but only among
        # supported levels; communities anchor at the observed joint member
        # profiles with the highest/lowest empirical weighted E[DV] - the
        # place where theoretical configurations are mostly unobserved and
        # values would otherwise lean on CPT smoothing. Anchor selection is
        # empirical; anchor values are read per impact_readoff.
        w_anchor <- if (!is.null(weight)) dat_boot[[weight]] else rep(1, nrow(dat_boot))
        dv_anchor_num <- dat_boot[[dv]] %>% as.character() %>% as.numeric()
        dv_anchor_y <- if (dv_metric == "top_box") {
          as.numeric(dv_anchor_num == max(dv_anchor_num, na.rm = TRUE))
        } else {
          dv_anchor_num
        }
        scope_mean <- stats::weighted.mean(dv_anchor_y, w_anchor)

        # Empirical-Bayes shrunk E[DV] for one anchor candidate: prior =
        # scope mean with max_impact_shrinkage pseudo-respondents. Used for
        # ranking and for the empirical read-off, so a 5-person all-top-box
        # cell can't win the argmax at a saturated 1.0.
        shrunk_mean <- function(m) {
          (sum(dv_anchor_y[m] * w_anchor[m]) + max_impact_shrinkage * scope_mean) /
            (sum(w_anchor[m]) + max_impact_shrinkage)
        }

        model_read <- function(ev) {
          if (type == "gr") {
            dist <- gRain::querygrain(grain_bn, nodes = dv, evidence = ev, simplify = TRUE)
            if (dv_metric == "top_box") {
              dist %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
            } else {
              sum(as.numeric(names(dist)) * as.numeric(dist))
            }
          } else {
            ev_txt <- paste0("list(", paste0(names(ev), " = '", unlist(ev), "'", collapse = ", "), ")")
            if (dv_metric == "top_box") {
              if (!is.null(seed)) set.seed(seed)
              eval(parse(text = glue::glue(
                "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{dv_boot_max}'), evidence = {ev_txt}, n = {n_querry}, method = 'lw')"
              )))
            } else {
              dv_scale <- fit_boot[[dv]] %>% dimnames() %>% .[[1]] %>% as.numeric()
              purrr::map_dbl(dv_scale, function(d) {
                if (!is.null(seed)) set.seed(seed)
                eval(parse(text = glue::glue(
                  "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{d}'), evidence = {ev_txt}, n = {n_querry}, method = 'lw')"
                )))
              }) %>% { sum(dv_scale * .) }
            }
          }
        }

        results <- temp_ivs %>%
          purrr::imap(function(iv_vars, iv_name) {

            key <- if (length(iv_vars) == 1) {
              as.character(dat_boot[[iv_vars]])
            } else {
              apply(dat_boot[iv_vars], 1, paste0, collapse = "\r")
            }
            counts <- table(key)
            eligible <- names(counts)[counts >= max_impact_min_support]
            if (length(eligible) < 2) eligible <- names(counts)

            if (length(iv_vars) == 1) {
              lev_num <- suppressWarnings(as.numeric(eligible))
              anchor_max <- eligible[which.max(lev_num)]
              anchor_min <- eligible[which.min(lev_num)]
            } else {
              prof_means <- vapply(eligible, function(k) shrunk_mean(key == k), numeric(1))
              anchor_max <- eligible[which.max(prof_means)]
              anchor_min <- eligible[which.min(prof_means)]
            }

            read_at <- function(k) {
              m <- key == k
              if (impact_readoff == "empirical") {
                shrunk_mean(m)
              } else {
                ridx <- which(m)[1]
                ev <- lapply(dat_boot[ridx, iv_vars, drop = FALSE], as.character)
                model_read(ev)
              }
            }
            p1 <- read_at(anchor_max)
            p0 <- read_at(anchor_min)

            data.frame(
              variable            = iv_name,
              dv_max_value        = p1,
              dv_min_value        = p0,
              maxVmin_propdisplay = (p1 - p0) / p0,
              maxVmin_absdisplay  = p1 - p0
            )
          }) %>%
          dplyr::bind_rows() %>%
          dplyr::as_tibble()

      } else {

        results <- temp_ivs %>%
          purrr::imap(
            ~engine_diff_single_attribute(
              fit_boot = fit_boot,
              grain_bn = grain_bn,
              dv = dv,
              iv = .y,
              attr_iv_boot_max = iv_boot_max[.x],
              attr_iv_boot_min = iv_boot_min[.x],
              attr_dv_boot_max = dv_boot_max,
              type = type,
              n_querry = n_querry,
              dv_metric = dv_metric,
              seed = seed
            )
          ) %>%
          dplyr::bind_rows() %>%
          dplyr::as_tibble()
      }

    }


    # ---------------------------
    # Lift: distribution-aware impact via bn_freq_prob_shift
    # When lift value != 0: E_shifted - E_observed
    # When lift value == 0: E_(+5%) - E_(-5%)
    # Supports both gr (exact gRain) and cp (Monte Carlo cpquery)
    # Accepts scalar or vector of lift values
    # When brand != NULL, computes lift per brand using brand-specific frequencies
    # ---------------------------
    if(type %in% c("gr", "cp") && !is.null(lift)){

      if(!is.null(community_assignment)) temp_ivs_r <- community_assignment else temp_ivs_r <- ivs %>% setNames(ivs)

      # impact_readoff = "empirical": the DV as a per-respondent numeric
      # outcome (scale value, or top-box indicator), read once for the whole
      # lift block. Conditional means over this vector replace the
      # querygrain/cpquery model conditionals ("model" = pre-2026-07-28
      # methodology). The joint community rake always needs this vector -
      # its read-off is inherently empirical - so it is also built when a
      # community run pairs community_lift = "joint" with the model
      # read-off (attribute lifts and anchor values stay model-based;
      # bn_impacts() warns once about the mixed semantics).
      if (impact_readoff == "empirical" ||
          (!is.null(community_assignment) && community_lift == "joint")) {
        dv_emp_num <- dat_boot[[dv]] %>% as.character() %>% as.numeric()
        dv_emp_y <- if (dv_metric == "top_box") {
          as.numeric(dv_emp_num == max(dv_emp_num, na.rm = TRUE))
        } else {
          dv_emp_num
        }
      }

      multi_lift <- length(lift) > 1
      lift_labels_base <- if (multi_lift) paste0("lift_", round(lift * 100)) else "lift"
      # One-pass shift variants: when impact_shift_type carries several
      # values (the default since 2026-07-28), a single engine pass emits
      # every requested shift variant with its `_propshift_`-style tag baked
      # into the column names, so the boot loop, MI, maxVmin, and base are
      # computed once instead of once per variant. A single value reproduces
      # the legacy untagged output (previously the wrapper ran the engine
      # once per variant and tagged/merged afterwards).
      shift_key_map <- c(proportional = "propshift", absolute = "absshift",
                         headroom = "headshift", range = "rangeshift")
      one_pass_shifts <- length(impact_shift_type) > 1
      shift_tag <- function(st) if (one_pass_shifts) paste0("_", shift_key_map[[st]]) else ""
      # Emit both outcome-display variants per lift percent, interleaved as
      # (propdisplay, absdisplay) — must line up with compute_lift_vals.
      # Within each shift block market columns come first, then per-brand
      # columns; name format "lift_N_{brand}_{shift}_{display}" — brand goes
      # BEFORE the shift tag (the Excel / HTML dashboards' column-name
      # formulas assume `{sg}_{lift_N}_{brand}_{shift}_{display}`). Blocks
      # concatenate in impact_shift_type order, mirroring the legacy
      # prop/abs/head/range merge order.
      labels_for <- function(st, b = NULL) {
        mid <- if (is.null(b)) "" else paste0("_", b)
        as.vector(rbind(
          paste0(lift_labels_base, mid, shift_tag(st), "_propdisplay"),
          paste0(lift_labels_base, mid, shift_tag(st), "_absdisplay")
        ))
      }
      brand_levels <- if (!is.null(brand)) sort(unique(as.character(dat_boot[[brand]]))) else NULL
      if (!is.null(brand_levels) && !is.null(brand_names)) {
        brand_levels <- intersect(brand_levels, brand_names)
        if (length(brand_levels) == 0) brand_levels <- NULL
      }
      lift_col_names <- unlist(lapply(impact_shift_type, function(st) {
        c(labels_for(st),
          unlist(lapply(brand_levels, function(b) labels_for(st, b))))
      }))

      # Helper: weighted frequency table (falls back to table() when weight is NULL)
      wtd_table <- function(x, w = NULL, levels = NULL) {
        if (!is.null(levels)) x <- factor(x, levels = levels)
        if (is.null(w)) return(table(x))
        tapply(w, x, sum) %>% { ifelse(is.na(.), 0, .) }
      }

      # Helper: compute lift values for a single freq distribution.
      # Returns a numeric vector of length 2 * length(lift), interleaved as
      # (propdisplay, absdisplay) for each lift percent. Order must match
      # `lift_labels` above so the final column naming lines up.
      # `iv_name` is used to look up an optional scale_range override.
      compute_lift_vals <- function(freq, dv_probs, iv_name = NULL, st,
                                    unsupported = NULL) {
        if (sum(freq) < min_base_for_lift) return(rep(NA_real_, length(lift) * 2L))
        p_observed <- as.numeric(freq) / sum(freq)
        observed_expected <- sum(dv_probs * p_observed)
        sr <- if (!is.null(scale_ranges) && !is.null(iv_name)) scale_ranges[[iv_name]] else NULL
        # `unsupported` marks levels whose empirical DV conditional does not
        # exist (zero rows in this resample - e.g. a rare bottom level the
        # draw missed; dv_probs was zero-filled there). bn_freq_prob_shift
        # floors empty cells at ~1e-6, so negligible mass there is stripped
        # and the target renormalized; a shift that demands REAL mass (> 1%)
        # on an unsupported level is empirically unestimable for this
        # replicate - clean_shift returns NULL and the lift records NA.
        clean_shift <- function(p) {
          p <- as.numeric(p)
          if (is.null(unsupported) || !any(unsupported)) return(p)
          excess <- sum(p[unsupported])
          if (!is.finite(excess) || excess > 0.01) return(NULL)
          p[unsupported] <- 0
          p / sum(p)
        }
        purrr::map(lift, function(l) {
          use_sym <- (l == 0)
          if (use_sym) {
            p_up   <- clean_shift(bn_freq_prob_shift(freq, type = "exponential", lift = 0.05,
              impact_shift_type = st, scale_range = sr))
            p_down <- clean_shift(bn_freq_prob_shift(freq, type = "exponential", lift = -0.05,
              impact_shift_type = st, scale_range = sr))
            if (is.null(p_up) || is.null(p_down)) return(c(NA_real_, NA_real_))
            lift_abs <- sum(dv_probs * p_up) - sum(dv_probs * p_down)
          } else {
            p_shifted <- clean_shift(bn_freq_prob_shift(freq, type = "exponential", lift = l,
              impact_shift_type = st, scale_range = sr))
            if (is.null(p_shifted)) return(c(NA_real_, NA_real_))
            lift_abs <- sum(dv_probs * p_shifted) - observed_expected
          }
          # Proportional display = absolute lift scaled by the observed
          # baseline expectation. NA when the baseline is zero to avoid
          # division blow-up.
          lift_prop <- if (is.finite(observed_expected) && observed_expected != 0) {
            lift_abs / observed_expected
          } else {
            NA_real_
          }
          c(lift_prop, lift_abs)   # (propdisplay, absdisplay)
        }) %>% unlist()
      }

      # community_lift = "joint": theme effect for one community and one
      # focus scope (market rows or a brand's rows). Rake the scope's weights
      # so every member's marginal hits its bn_freq_prob_shift target
      # simultaneously, then read the DV change empirically under the raked
      # weights. Targets are zeroed and renormalized on levels the scope
      # never observes, so mass only moves across observed joint profiles
      # (support blackout). Requires impact_readoff = "empirical" (enforced
      # up front); "average" below was the pre-2026-07-28 methodology.
      compute_joint_lift_vals <- function(iv_vars, mask) {
        w_all <- if (!is.null(weight)) dat_boot[[weight]] else rep(1, nrow(dat_boot))
        w_s <- w_all[mask]
        if (sum(w_s) < min_base_for_lift) {
          return(lapply(impact_shift_type, function(st) rep(NA_real_, length(lift) * 2L)))
        }
        members <- dat_boot[mask, iv_vars, drop = FALSE]
        freqs <- lapply(iv_vars, function(v) {
          wtd_table(members[[v]], w = if (is.null(weight)) NULL else w_s)
        }) %>% setNames(iv_vars)
        dv_s <- dv_emp_y[mask]
        e_obs <- stats::weighted.mean(dv_s, w_s)

        # Level indices are identical for every rake in this scope (targets
        # are always named by names(freqs[[v]])) - build once, reuse across
        # all shift variants and lifts.
        rake_idx <- lapply(iv_vars, function(v) {
          match(as.character(members[[v]]), names(freqs[[v]]))
        }) %>% setNames(iv_vars)

        # Returns NULL when any member's shifted target is undefined for
        # this (resampled) scope - e.g. bn_freq_prob_shift returns NA for an
        # unshiftable distribution, or all target mass lands on unsupported
        # levels. The scope's joint lift is then NA for this replicate,
        # mirroring how the averaging path degrades to NA member lifts.
        targets_for <- function(l, st) {
          out <- lapply(iv_vars, function(v) {
            fr <- freqs[[v]]
            sr <- if (!is.null(scale_ranges)) scale_ranges[[v]] else NULL
            tgt <- suppressWarnings(as.numeric(
              bn_freq_prob_shift(fr, type = "exponential", lift = l,
                impact_shift_type = st, scale_range = sr)
            ))
            if (length(tgt) != length(fr) || anyNA(tgt)) return(NULL)
            tgt <- setNames(tgt, names(fr))
            tgt[as.numeric(fr) <= 0] <- 0
            tot <- sum(tgt)
            if (!is.finite(tot) || tot <= 0) return(NULL)
            tgt / tot
          }) %>% setNames(iv_vars)
          if (any(vapply(out, is.null, logical(1)))) return(NULL)
          out
        }
        e_raked <- function(l, st) {
          tg <- targets_for(l, st)
          if (is.null(tg)) return(NA_real_)
          r <- .bn_ipf_rake(members, w_s, tg, idx = rake_idx)
          if (is.null(r)) return(NA_real_)
          stats::weighted.mean(dv_s, r)
        }

        # One list element per shift variant (freqs / dv_s / e_obs shared);
        # each element interleaves (propdisplay, absdisplay) per lift.
        lapply(impact_shift_type, function(st) {
          purrr::map(lift, function(l) {
            lift_abs <- if (l == 0) e_raked(0.05, st) - e_raked(-0.05, st) else e_raked(l, st) - e_obs
            lift_prop <- if (is.finite(e_obs) && e_obs != 0) lift_abs / e_obs else NA_real_
            c(lift_prop, lift_abs)   # (propdisplay, absdisplay)
          }) %>% unlist()
        })
      }

      lift_results <- temp_ivs_r %>%
        purrr::imap(function(iv_vars, iv_name) {

          if (!is.null(community_assignment) && community_lift == "joint") {
            # Rake each focus scope once (per shift variant, inside), then
            # assemble shift-block-major to match lift_col_names order.
            scope_masks <- c(
              list(rep(TRUE, nrow(dat_boot))),
              if (is.null(brand_levels)) NULL else
                lapply(brand_levels, function(b) dat_boot[[brand]] %in% b)
            )
            per_scope <- lapply(scope_masks, function(m) compute_joint_lift_vals(iv_vars, m))
            vals <- unlist(lapply(seq_along(impact_shift_type), function(si) {
              unlist(lapply(per_scope, function(ps) ps[[si]]))
            }))
            return(dplyr::bind_cols(
              data.frame(variable = iv_name),
              as.data.frame(t(setNames(vals, lift_col_names)))
            ))
          }

          # Matrix: rows = iv_vars, cols = lift_col_names
          per_iv_mat <- purrr::map(iv_vars, function(single_iv) {
            w_vec <- if (!is.null(weight)) dat_boot[[weight]] else NULL
            freq_full <- wtd_table(dat_boot[[single_iv]], w = w_vec)
            levels_v <- names(freq_full)

            # DV expectation per IV level (shared across brands)
            # dv_metric = "top_box": P(DV_max | IV=v)
            # dv_metric = "mean":    E[DV | IV=v] = Σ d × P(DV=d | IV=v)
            if (impact_readoff == "empirical") {
              dv_probs <- purrr::map_dbl(levels_v, function(v) {
                mask <- dat_boot[[single_iv]] == v
                if (is.null(w_vec)) {
                  mean(dv_emp_y[mask])
                } else {
                  stats::weighted.mean(dv_emp_y[mask], w_vec[mask])
                }
              })
              # A retained factor level with zero rows in this resample
              # (rare bottom levels under bootstrap) yields mean(empty) =
              # NaN, and 0 * NaN would poison every lift sum even though
              # the level carries no mass. Zero-fill and remember which
              # levels are unsupported; compute_lift_vals NAs only the
              # shifts that put real target mass on them. Model read-off
              # never hits this (smoothed conditionals exist everywhere).
              unsupported_lv <- !is.finite(dv_probs)
              dv_probs[unsupported_lv] <- 0
            } else if (type == "gr") {
              dv_probs <- purrr::map_dbl(levels_v, function(v) {
                ev <- stats::setNames(list(v), single_iv)
                dist <- gRain::querygrain(grain_bn, nodes = dv, evidence = ev, simplify = TRUE)
                if (dv_metric == "top_box") {
                  dist %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
                } else {
                  dv_levels <- as.numeric(names(dist))
                  dv_vals <- as.numeric(dist)
                  sum(dv_levels * dv_vals)
                }
              })
            } else if (type == "cp") {
              if (dv_metric == "top_box") {
                dv_probs <- purrr::map_dbl(levels_v, function(v) {
                  if (!is.null(seed)) set.seed(seed)
                  eval(parse(text = glue::glue(
                    "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{dv_boot_max}'), evidence = list({single_iv} = '{v}'), n = {n_querry}, method = 'lw')"
                  )))
                })
              } else {
                dv_scale <- fit_boot[[dv]] %>% dimnames() %>% .[[1]] %>% as.numeric()
                dv_probs <- purrr::map_dbl(levels_v, function(v) {
                  if (!is.null(seed)) set.seed(seed)
                  purrr::map_dbl(dv_scale, function(d) {
                    eval(parse(text = glue::glue(
                      "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{d}'), evidence = list({single_iv} = '{v}'), n = {n_querry}, method = 'lw')"
                    )))
                  }) -> level_probs
                  sum(dv_scale * level_probs)
                })
              }
            }

            # Brand frequency tables are shift-independent — build once,
            # reuse across every shift variant.
            brand_freqs <- if (is.null(brand_levels)) NULL else lapply(brand_levels, function(b) {
              brand_mask <- dat_boot[[brand]] == b
              w_b <- if (!is.null(weight)) dat_boot[[weight]][brand_mask] else NULL
              wtd_table(dat_boot[[single_iv]][brand_mask], w = w_b, levels = levels_v)
            })

            # Per shift variant: market lift on the full distribution, then
            # per-brand lifts — matching lift_col_names block order.
            unsup <- if (impact_readoff == "empirical") unsupported_lv else NULL
            unlist(lapply(impact_shift_type, function(st) {
              market_vals <- compute_lift_vals(freq_full, dv_probs, iv_name = single_iv,
                                               st = st, unsupported = unsup)
              if (is.null(brand_levels)) return(market_vals)
              c(market_vals, unlist(lapply(brand_freqs, function(freq_b) {
                compute_lift_vals(freq_b, dv_probs, iv_name = single_iv,
                                  st = st, unsupported = unsup)
              })))
            }))
          }) %>%
            do.call(rbind, .)

          # Average across IVs (for communities), produce named vector
          avg_vals <- colMeans(per_iv_mat) %>% setNames(lift_col_names)
          dplyr::bind_cols(data.frame(variable = iv_name), as.data.frame(t(avg_vals)))
        }) %>%
        dplyr::bind_rows()

      results <- results %>%
        dplyr::left_join(lift_results, by = "variable")

      # Base sizes per cell
      if (include_base) {
        base_results <- temp_ivs_r %>%
          purrr::imap(function(iv_vars, iv_name) {
            per_iv_bases <- purrr::map(iv_vars, function(single_iv) {
              market_base <- c(base = sum(table(dat_boot[[single_iv]])))
              if (is.null(brand_levels)) {
                market_base
              } else {
                brand_bases <- purrr::map_dbl(brand_levels, function(b) {
                  sum(dat_boot[[brand]] == b, na.rm = TRUE)
                }) %>%
                  setNames(paste0("base_", brand_levels))
                c(market_base, brand_bases)
              }
            }) %>%
              do.call(rbind, .)
            avg_base <- colMeans(per_iv_bases)
            dplyr::bind_cols(data.frame(variable = iv_name), as.data.frame(t(avg_base)))
          }) %>%
          dplyr::bind_rows()

        results <- results %>%
          dplyr::left_join(base_results, by = "variable")
      }
    }


    if(type == "mi" || add_mi){

      if(!is.null(community_assignment)) temp_ivs <- community_assignment else temp_ivs <- ivs %>% setNames(ivs)

      results_mi <- temp_ivs %>%
        purrr::imap(
          ~{
            composite <- apply(fit_data[.x], 1, paste0, collapse = "_") %>% as.factor()

            # A bootstrap resample can leave the composite (or the DV) with a
            # single observed level - e.g. a near-constant binary q19a item in
            # a skewed subgroup, where the resample misses the handful of rows
            # on the rare level. as.factor() on the pasted values keeps only
            # OBSERVED levels, so ci.test()'s check.data() hard-errors with
            # "variable x in the data must have at least two levels". A
            # constant variable carries exactly zero mutual information, so
            # return mi = 0 for this draw instead of crashing the boot.
            if (nlevels(composite) < 2 || dplyr::n_distinct(fit_data[[dv]]) < 2) {
              dplyr::tibble(
                "variable" = .y,
                "mi" = 0,
                "p_val" = 1
              )
            } else {
              xmi <- bnlearn::ci.test(composite, fit_data[[dv]], test = "mi")

              dplyr::tibble(
                "variable" = .y,
                "mi" = xmi$statistic / (2 * nrow(dat_boot)),
                "p_val" = xmi$p.value
              )
            }
          }
        ) %>%
        dplyr::bind_rows()


      # Bootstrap MI for communities to get CI-based p-value
      if (!is.null(mi_boot) && !is.null(community_assignment)) {
        n_mi_boot <- as.integer(mi_boot)
        n_obs <- nrow(fit_data)

        mi_boot_results <- temp_ivs %>%
          purrr::imap(function(iv_vars, comm_name) {
            # Paste the community composite ONCE over the outer replicate's
            # rows; each nested resample just indexes into it. The row-wise
            # apply(paste) was rebuilt per nested replicate and dominated the
            # community boot cost. RNG-neutral (only sample() draws), so
            # mi_boot values are bit-identical to the per-replicate paste.
            composite_full <- apply(fit_data[iv_vars], 1, paste0, collapse = "_")
            dv_full <- fit_data[[dv]]
            boot_mi <- replicate(n_mi_boot, {
              boot_idx <- sample(n_obs, replace = TRUE)
              composite <- factor(composite_full[boot_idx])
              dv_b <- dv_full[boot_idx]
              # Suppress bnlearn "variable X has levels that are not observed
              # in the data" warnings from ci.test().
              #
              # Why they fire: bootstrap resampling (with replacement) from a
              # subgroup can occasionally draw samples that miss a rare factor
              # level (e.g., DV = 1 in a low-prevalence subgroup). bnlearn's
              # check.data() flags that the factor carries a level with zero
              # rows, even though the underlying data is fine.
              #
              # Why it's safe to suppress: ci.test() still returns a valid MI
              # statistic (levels with zero observations contribute zero to
              # the sum). Across n_mi_boot replicates, any individual resample
              # missing a level just shifts one draw slightly — the
              # distribution of MI values the bootstrap CI is built from is
              # unaffected in the aggregate. The warning is purely cosmetic
              # at this scope, and with n_mi_boot >> 1 it fires repeatedly
              # for the same root cause and drowns out anything meaningful.
              if (nlevels(composite) < 2 || dplyr::n_distinct(dv_b) < 2) {
                # A replicate where the composite (or the DV) is constant
                # carries zero mutual information by definition - contribute 0
                # rather than letting check.data() error out (same guard as
                # the attribute-MI block above).
                0
              } else {
                xmi <- suppressWarnings(
                  bnlearn::ci.test(composite, dv_b, test = "mi")
                )
                xmi$statistic / (2 * n_obs)
              }
            })

            # P-value: proportion of bootstrap replicates at or below zero
            boot_p <- mean(boot_mi <= 0)

            dplyr::tibble(
              variable = comm_name,
              mi_boot_p = boot_p,
              mi_boot_lower = stats::quantile(boot_mi, 0.025),
              mi_boot_upper = stats::quantile(boot_mi, 0.975)
            )
          }) %>%
          dplyr::bind_rows()

        results_mi <- results_mi %>%
          dplyr::left_join(mi_boot_results, by = "variable") %>%
          dplyr::mutate(p_val = mi_boot_p) %>%
          dplyr::select(-mi_boot_p)
      }

      if(type != "mi"){
        results <- results %>%
          dplyr::left_join(results_mi, by = dplyr::join_by(variable))
      }else if(type == "mi"){
        results <- results_mi
      }else{
        stop("Unknown type: ", type)
      }

    }


    return(results)
  }


  # ---------------------------
  # Bootstrap
  # ---------------------------

  if (n_boot > 1) {
    # Seed the resample draw - without this the boot indices depend on ambient
    # RNG state and boot p-values are not reproducible across runs, despite
    # the documented `seed` parameter.
    if (!is.null(seed)) set.seed(seed)
    index_sets <- replicate(n_boot, sample(seq_len(nrow(df)), replace = TRUE), simplify = FALSE)

    result <- index_sets %>%
      purrr::map(
        ~engine_diff_multiple(
          indices = .x, data = df,
          bn = bn,
          ivs = ivs, ivs_max = ivs_max, ivs_min = ivs_min, dv_max = dv_max,
          add_mi = TRUE, type = type, n_querry = n_querry, lift = lift,
          impact_shift_type = impact_shift_type,
          brand = brand, min_base_for_lift = min_base_for_lift,
          include_base = include_base, dv_metric = dv_metric, weight = weight, fit = NULL, mi_boot = mi_boot, community_assignment = community_assignment
        ) %>%
          dplyr::select(-dplyr::any_of("p_val"))
      ) %>%
      purrr::list_rbind() %>%
      tidyr::pivot_longer(cols = !variable, names_to = "metric") %>%
      dplyr::group_by(variable, metric) %>%
      dplyr::summarise(
        n_ok = sum(!is.na(value)),
        mean = mean(value, na.rm = TRUE),
        sd   = sd(value,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        # Coverage blackout: a cell whose value exists in fewer than
        # min_boot_coverage of the replicates (infeasible joint rakes in
        # small subgroups, brand scopes flickering around
        # min_base_for_lift) is blanked entirely - the surviving
        # replicates are a selection-biased subset, and averaging them
        # silently would report a conditional estimand with an overstated
        # df. Cells that pass use n_ok for the effective df, so fully
        # feasible cells (n_ok == n_boot) reproduce the previous
        # statistics exactly.
        blackout = n_ok < min_boot_coverage * n_boot,
        mean     = ifelse(blackout, NA_real_, mean),
        sd       = ifelse(blackout, NA_real_, sd),
        # boot_nonzero = FALSE (default): classical bootstrap inference - the
        # SD of the bootstrap replicates IS the standard-error estimate of the
        # statistic, so p-values are invariant to n_boot.
        # boot_nonzero = TRUE: legacy behavior - se = sd/sqrt(n_boot), which
        # tests whether the MEAN of the boot distribution is nonzero and
        # therefore mechanically shrinks p-values as n_boot grows.
        se      = if (boot_nonzero) sd / sqrt(pmax(n_ok, 1)) else sd,
        t       = mean / se,
        tcrit   = stats::qt(0.975, df = pmax(n_ok - 1, 1)),
        ci_low  = mean - tcrit * se,
        ci_high = mean + tcrit * se,
        p_value = 2 * stats::pt(-abs(t), df = pmax(n_ok - 1, 1)) %>% round(4)
      ) %>%
      dplyr::select(-tcrit, -n_ok, -blackout) %>%   # housekeeping
      tidyr::pivot_wider(
        id_cols = variable,
        names_from = metric,
        values_from = c(mean, sd, se, t, ci_low, ci_high, p_value),
        names_glue = "{metric}_{.value}"
      ) %>%
      dplyr::select(
        variable,
        tidyselect::matches("^dv_max_value"),
        tidyselect::matches("^dv_min_value"),
        tidyselect::matches("^mi"),
        tidyselect::matches("^maxVmin_"),
        tidyselect::matches("^lift"),
        # Base counts (overall + per-brand). These don't vary across
        # bootstrap iterations but the engine's pivot still emits
        # `base_mean`, `base_<brand>_mean`, etc. The dashboard's Base
        # row reads `<sg>_base` / `<sg>_base_<focus>`, so we need these
        # here so the rename-_mean step below produces `<sg>_base`.
        tidyselect::matches("^base")
      ) %>%
      # The dashboard formulas key off bare-name VALUE columns (e.g.
      # "lift_0_propdisplay") to compute the displayed index, and off
      # `<col>_p_value` for the bootstrap blackout. The pivot above only
      # emits `<metric>_<stat>` columns, so rename `_mean` (the bootstrap
      # point estimate) to no-suffix and drop stats the dashboards don't
      # use to keep the table from ballooning into thousands of columns.
      # Kept stats: <metric> (= mean), <metric>_p_value, <metric>_ci_low,
      # <metric>_ci_high. Dropped: _sd, _se, _t (internal computations
      # not surfaced in any UI).
      dplyr::select(
        -tidyselect::ends_with("_sd"),
        -tidyselect::ends_with("_se"),
        -tidyselect::ends_with("_t")
      ) %>%
      dplyr::rename_with(
        .fn = ~ sub("_mean$", "", .),
        .cols = tidyselect::ends_with("_mean")
      )


  } else {

    result <- engine_diff_multiple(
      indices = NULL, data = df,
      bn = bn,
      ivs = ivs, ivs_max = ivs_max, ivs_min = ivs_min, dv_max = dv_max,
      add_mi = TRUE, type = type, n_querry = n_querry, lift = lift,
      impact_shift_type = impact_shift_type,
      brand = brand, min_base_for_lift = min_base_for_lift,
      include_base = include_base, dv_metric = dv_metric, weight = weight, fit = fit, mi_boot = mi_boot, community_assignment = community_assignment
    )

  }


  if(index_by != "none"){

    # Resolve lift_first / lift_second to market lift columns. After the
    # outcome-display expansion, market lift columns look like
    # `lift_absdisplay`, `lift_0_absdisplay`, `lift_10_absdisplay`, etc. Use
    # the absolute-display variant for indexing (raw probability-point
    # change — the natural scale for ranking drivers).
    if (index_by %in% c("lift_first", "lift_second")) {
      lift_cols <- grep("^lift", names(result), value = TRUE)
      lift_cols <- lift_cols[!grepl("_mean$|_sd$|_ci_lo$|_ci_hi$", lift_cols)]
      # Market lift only (no brand suffix), absolute-display variant.
      market_lift_cols <- lift_cols[grep("^lift(_\\d+)?_absdisplay$", lift_cols)]
      if (length(market_lift_cols) == 0) {
        # One-pass tagged output: index on the propshift variant (matches
        # the legacy behavior of indexing on the proportional-shift pass).
        market_lift_cols <- lift_cols[grep("^lift(_\\d+)?_propshift_absdisplay$", lift_cols)]
      }
      if (length(market_lift_cols) == 0) market_lift_cols <- lift_cols
      if (length(market_lift_cols) == 0) {
        warning("index_by = '", index_by, "': no lift columns found. Skipping index.")
        return(result)
      }
      idx <- if (index_by == "lift_first") 1L else min(2L, length(market_lift_cols))
      resolved_col <- market_lift_cols[idx]
    } else if (index_by == "maxVmin") {
      # Prefer the absolute-display maxVmin (bounded, natural for indexing).
      resolved_col <- if ("maxVmin_absdisplay" %in% names(result)) {
        "maxVmin_absdisplay"
      } else {
        "maxVmin"  # legacy — shouldn't occur after the split
      }
    } else {
      resolved_col <- index_by
    }

    # Bootstrap pivot appends _mean suffix; single run uses bare name
    index_col <- if (resolved_col %in% names(result)) resolved_col else paste0(resolved_col, "_mean")

    if (!index_col %in% names(result)) {
      warning("index_by = '", index_by, "' not found in results. Skipping index.")
    } else {
      result <- result %>%
        dplyr::mutate(
          index = (abs(.data[[index_col]]) / mean(abs(.data[[index_col]]))) * 100
        )
    }

  }

  return(result)
}


#' Resolve community-impact attribute ids from declared battery names
#'
#' An IV's battery is its variable name with the trailing "_<number>" suffix
#' removed (q14a_1 -> "q14a"); ids without a numeric suffix are their own
#' battery. Returns the subset of `ids` whose battery is declared. Stops if
#' any declared battery matches no id, listing the batteries that are
#' available.
#'
#' @noRd
.bn_community_impact_ids <- function(ids, community_impact_attributes) {

  if (!is.character(community_impact_attributes) ||
      length(community_impact_attributes) == 0 ||
      anyNA(community_impact_attributes)) {
    stop("'community_impact_attributes' must be a character vector of battery names (or NULL).")
  }

  id_batteries <- sub("_[0-9]+$", "", ids)
  available <- unique(id_batteries)
  unidentified <- setdiff(community_impact_attributes, available)

  if (length(unidentified) > 0) {
    stop(
      "'community_impact_attributes' declares unidentified batter",
      if (length(unidentified) > 1) "ies: " else "y: ",
      paste0("'", unidentified, "'", collapse = ", "),
      ". Available batteries: ",
      paste0("'", sort(available), "'", collapse = ", "), "."
    )
  }

  ids[id_batteries %in% community_impact_attributes]
}


#' Iterative proportional fitting of row weights to per-member marginal targets
#'
#' member_df holds one column per community member (factor/character), w the
#' starting weights, targets a named list (per member) of target proportions
#' named by that member's observed levels. Cycles members, scaling weights so
#' each member's weighted marginal matches its target, until the worst
#' marginal gap is below tol or max_iter is hit (correlated members can make
#' the margins jointly infeasible - the weights then sit at the closest
#' reachable point, which is the intended support-respecting behavior).
#' Deterministic; no RNG.
#'
#' @noRd
.bn_ipf_rake <- function(member_df, w, targets, tol = 1e-4, max_iter = 50L,
                         idx = NULL) {

  r <- as.numeric(w)
  # idx: precomputed per-member level indices (match of each row's level into
  # that member's target names). Callers raking the same members repeatedly
  # (one rake per shift variant x lift) should build this once and pass it -
  # rebuilding match() per rake was a measurable share of the rake cost.
  if (is.null(idx)) {
    idx <- lapply(names(targets), function(v) {
      match(as.character(member_df[[v]]), names(targets[[v]]))
    }) %>% setNames(names(targets))
  }

  for (i in seq_len(max_iter)) {
    max_gap <- 0
    for (v in names(targets)) {
      # Saturated targets on several correlated members can zero out every
      # row (no observed joint profile carries the demanded mass). That is
      # an infeasible joint shift for this scope - return NULL and let the
      # caller record NA rather than dividing by a zero total.
      sr <- sum(r)
      if (!is.finite(sr) || sr <= 0) return(NULL)
      tgt <- targets[[v]]
      ix <- idx[[v]]
      cur <- numeric(length(tgt))
      rs <- rowsum(r, ix)
      cur[as.integer(rownames(rs))] <- rs[, 1]
      cur_p <- cur / sr
      max_gap <- max(max_gap, max(abs(cur_p - tgt)))
      ratio <- ifelse(cur_p > 0, tgt / cur_p, 0)
      r <- r * ratio[ix]
    }
    if (!is.finite(max_gap)) return(NULL)
    if (max_gap < tol) break
  }
  if (!is.finite(sum(r)) || sum(r) <= 0) return(NULL)

  r
}

