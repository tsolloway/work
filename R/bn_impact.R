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
#' @param lift Numeric vector. Target lift(s) for the shifted-distribution
#'   metric (both proportional and absolute shift variants are precomputed).
#'   Default \code{c(0, 0.1)}.
#' @param min_base_for_lift Integer. Minimum sample size required for a brand
#'   subgroup to compute lift estimates. Brands below this threshold are
#'   excluded. Default 75.
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
    lift = c(0, 0.1),
    min_base_for_lift = 75,
    type = c("gr", "cp", "mi"),
    dv_metric = c("mean", "top_box"),
    include_base = TRUE,
    index_by = c("lift_first", "lift_second", "maxVmin", "mi", "none"),
    n_boot = 1,
    n_querry = 1e4,
    brand = NULL,
    brand_names = NULL,
    weight = NULL,
    mi_boot = NULL,
    verbose = TRUE,
    use_parallel = TRUE,
    seed = 1
){

  type <- match.arg(type)
  index_by <- match.arg(index_by)
  dv_metric <- match.arg(dv_metric)

  # Preserve named dv for meta, strip for bnlearn
  dv_original <- dv
  dv <- unname(dv)

  # Helper: insert a shift-type tag (propshift / absshift) into lift column
  # names, right after the propdisplay / absdisplay token. Handles both:
  #   - value cols ending in propdisplay/absdisplay
  #   - bootstrap stat cols of the form <lift>_<display>_<stat>
  #     (where <stat> ∈ mean | sd | se | t | ci_low | ci_high | p_value)
  # so the dashboard's metric-keyed bootstrap p-value lookup resolves to a
  # consistently-shaped col name. maxVmin / mi / p_val / dv_max_value /
  # dv_min_value / index don't start with `lift` and are NOT renamed.
  .insert_shift_suffix <- function(cols, shift_key) {
    # POSIX regex (no perl=TRUE): R's perl backref handling treats `\\1`
    # as the byte 0x01 (octal escape) and emits literal control chars
    # instead of backrefs. Plain POSIX gives reliable backref behavior.
    sub("^(lift.*)_(propdisplay|absdisplay)(_.*)?$",
        paste0("\\1_", shift_key, "_\\2\\3"), cols)
  }

  # Helper: merge two engine outputs produced with impact_shift_type =
  # "proportional" and "absolute". Keep everything from the prop run; from
  # the abs run take all lift-related columns (values + bootstrap stats)
  # and bind alongside, each tagged _absshift_.
  .merge_shift_variants <- function(prop_tbl, abs_tbl) {
    # Rename lift columns in the prop run to include _propshift_ tag.
    prop_names <- names(prop_tbl)
    renamed_prop <- .insert_shift_suffix(prop_names, "propshift")
    names(prop_tbl) <- renamed_prop

    # From abs run, extract all lift cols (values + boot stats) and tag.
    abs_lift_cols <- grep("^lift.*_(propdisplay|absdisplay)(_.*)?$",
      names(abs_tbl), value = TRUE)
    if (length(abs_lift_cols) == 0) return(prop_tbl)
    abs_lift_only <- abs_tbl[, abs_lift_cols, drop = FALSE]
    names(abs_lift_only) <- .insert_shift_suffix(
      names(abs_lift_only), "absshift"
    )

    dplyr::bind_cols(prop_tbl, abs_lift_only)
  }

  # Run the engine once per shift_type (proportional + absolute). maxVmin /
  # mi / index values don't depend on shift_type and would be duplicates, so
  # only the lift columns are retained from the absolute run.
  .dual_engine_call <- function(engine_args) {
    prop_tbl <- do.call(bn_impact_engine,
      c(engine_args, list(impact_shift_type = "proportional")))
    abs_tbl  <- do.call(bn_impact_engine,
      c(engine_args, list(impact_shift_type = "absolute")))
    .merge_shift_variants(prop_tbl, abs_tbl)
  }

  if(process_subgroups){

    first_subgroup <- names(obj)[[1]]

    .engine_call <- function(.x, .y){
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
        type = type,
        index_by = index_by,
        n_boot = n_boot,
        n_querry = n_querry,
        lift = lift,
        brand = brand,
        brand_names = brand_names,
        min_base_for_lift = min_base_for_lift,
        include_base = include_base,
        dv_metric = dv_metric,
        weight = weight,
        mi_boot = mi_boot,
        seed = seed
      )) %>%
        setNames(glue::glue("{.y}_{names(.)}"))
    }

    if (verbose) {
      output <- imap_progress(obj, .engine_call, .parallel = use_parallel)
    } else if (use_parallel) {
      output <- furrr::future_imap(
        obj, .engine_call,
        .options = furrr::furrr_options(seed = TRUE)
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
      type = type,
      index_by = index_by,
      n_boot = n_boot,
      n_querry = n_querry,
      lift = lift,
      brand = brand,
      brand_names = brand_names,
      min_base_for_lift = min_base_for_lift,
      include_base = include_base,
      dv_metric = dv_metric,
      weight = weight,
      mi_boot = mi_boot,
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
      brand = brand,
      brand_names = brand_names_resolved,
      min_base_for_lift = min_base_for_lift,
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
