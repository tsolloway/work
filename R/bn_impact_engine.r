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
#'   level by jointly setting all IVs within each community.
#' @param community_assignment Optional data frame with \code{id} and
#'   \code{community_name} columns mapping IVs to communities.
#' @param type Character. Estimation method:
#'   \itemize{
#'     \item \code{"gr"} (default): exact junction-tree inference via \code{gRain}.
#'       Deterministic, fast, no Monte Carlo noise.
#'     \item \code{"cp"}: Monte Carlo conditional probability via
#'       \code{bnlearn::cpquery()} with likelihood weighting.
#'     \item \code{"mi"}: mutual information test only (no probability lift).
#'   }
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
#' @param lift Numeric scalar or vector. Target percentage lift(s) for
#'   distribution-aware impact. Uses \code{bn_freq_prob_shift()} to shift each
#'   IV's observed distribution by each fraction (e.g., 0.10 = 10 percent),
#'   then computes the resulting DV probability change. Computed when
#'   \code{type = "gr"} (exact) or \code{type = "cp"} (Monte Carlo).
#'   When a lift value is 0, computes symmetric sensitivity: E_(+5 percent) -
#'   E_(-5 percent). A scalar produces a single \code{lift} column; a vector
#'   (e.g., \code{c(0, 0.05, 0.10)}) produces \code{lift_0}, \code{lift_5},
#'   \code{lift_10}. Default \code{0}.
#' @param impact_metric_type Character. How \code{lift} values are interpreted:
#'   \code{"proportional"} (default) shifts the mean by a fraction of its
#'   current value (e.g., 0.10 = 10 percent of current mean);
#'   \code{"absolute"} shifts the mean by a fixed number of scale points
#'   (e.g., 0.10 = add 0.10 to the mean regardless of starting value).
#' @param min_base_for_lift Integer. Minimum sample size (frequency count) required
#'   to compute a lift value. If the distribution has fewer than this many
#'   observations, the lift cell returns \code{NA}. Applied per-brand when
#'   \code{brand} is set. Default \code{60}.
#' @param dv_metric Character. What to compute from the DV distribution per IV level:
#'   \code{"top_box"} (default) uses \code{P(DV_max | IV=v)} — probability of the
#'   highest DV level. \code{"mean"} uses \code{E[DV | IV=v]} — the expected value
#'   across all DV levels (works with any scale: 3-point, 5-point, 7-point, etc.).
#' @param brand Character scalar or \code{NULL}. Column name in \code{df}
#'   containing brand (or segment) labels. When provided, the lift metric is
#'   computed using brand-specific frequency distributions rather than the
#'   overall distribution. Produces separate lift columns per brand (e.g.,
#'   \code{lift_Apex}, \code{lift_Vero}). The DV probability queries are shared
#'   across brands; only the observed distribution changes. Default \code{NULL}.
#' @param mi_boot Integer or NULL. When set and \code{do_community = TRUE},
#'   bootstraps the MI calculation this many times to derive a p-value from the
#'   bootstrap distribution instead of the chi-squared approximation. The
#'   chi-squared p-value is unreliable for community-level MI because the
#'   composite factor has too many levels relative to sample size. Bootstrap
#'   p-value is the proportion of replicates with MI at or below zero.
#'   Default NULL (use analytic p-value).
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
    type = c("gr", "cp", "mi"),
    index_by = c("lift_first", "lift_second", "maxVmin", "mi", "none"),
    n_boot = 1,
    n_querry = 1e4,
    lift = 0,
    impact_metric_type = c("proportional", "absolute"),
    brand = NULL,
    min_base_for_lift = 60,
    include_base = TRUE,
    dv_metric = c("top_box", "mean"),
    weight = NULL,
    mi_boot = NULL,
    seed = 1
){

  type <- match.arg(type)
  index_by <- match.arg(index_by)
  impact_metric_type <- match.arg(impact_metric_type)
  dv_metric <- match.arg(dv_metric)
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
    fit <- bnlearn::bn.fit(bn, df, method = "bayes")
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
      dplyr::filter(id %in% ivs) %>%
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
    type = c("cp", "gr"), n_querry = 1e5, dv_metric = "top_box", impact_metric_type = "proportional", seed = NULL
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

    if (impact_metric_type == "proportional") {
      maxVmin_val <- (p1 - p0) / p0
    } else {
      maxVmin_val <- p1 - p0
    }

    data.frame(variable = iv, dv_max_value = p1, dv_min_value = p0, maxVmin = maxVmin_val)
  }



  # ---------------------------
  # Multiple  difference function
  # ---------------------------

  engine_diff_multiple <- function(
    data, indices = NULL, bn, ivs, ivs_max, ivs_min, dv_max = NULL,
    community_assignment = NULL,
    add_mi = TRUE, type = c("cp", "gr", "mi"),
    n_querry = 1e5, lift = 0, impact_metric_type = "proportional", brand = NULL,
    min_base_for_lift = 60, include_base = TRUE, dv_metric = "top_box", weight = NULL, fit = NULL, mi_boot = NULL, seed = 1
  ){
    type <- match.arg(type)

    if(!is.null(indices)) dat_boot <- data[indices, , drop = FALSE] else dat_boot <- data

    # Exclude brand column from model fitting data
    exclude_cols <- c(brand, weight)
    fit_data <- if (length(exclude_cols) > 0) dat_boot[, setdiff(names(dat_boot), exclude_cols), drop = FALSE] else dat_boot

    if(type != "mi"){

      if(is.null(fit)) fit_boot <- bnlearn::bn.fit(bn, fit_data, method = "bayes") else fit_boot <- fit

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

        grain_bn <- bnlearn::as.grain(fit_boot) %>% gRain:::compile.grain()

      }
    }


    if(type != "mi"){

      if(!is.null(community_assignment)) temp_ivs <- community_assignment else temp_ivs <- ivs %>% setNames(ivs)


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
            impact_metric_type = impact_metric_type,
            seed = seed
          )
        ) %>%
        dplyr::bind_rows() %>%
        dplyr::as_tibble()

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

      multi_lift <- length(lift) > 1
      lift_labels <- if (multi_lift) paste0("lift_", round(lift * 100)) else "lift"
      brand_levels <- if (!is.null(brand)) sort(unique(as.character(dat_boot[[brand]]))) else NULL

      # Build column names — market lift always included
      market_lift_col_names <- lift_labels
      if (is.null(brand_levels)) {
        lift_col_names <- lift_labels
      } else if (!multi_lift) {
        lift_col_names <- c(lift_labels, paste0("lift_", brand_levels))
      } else {
        brand_lift_col_names <- expand.grid(
          lift_label = lift_labels,
          brand = brand_levels,
          stringsAsFactors = FALSE
        ) %>%
          with(paste(lift_label, brand, sep = "_"))
        lift_col_names <- c(lift_labels, brand_lift_col_names)
      }

      # Helper: weighted frequency table (falls back to table() when weight is NULL)
      wtd_table <- function(x, w = NULL, levels = NULL) {
        if (!is.null(levels)) x <- factor(x, levels = levels)
        if (is.null(w)) return(table(x))
        tapply(w, x, sum) %>% { ifelse(is.na(.), 0, .) }
      }

      # Helper: compute lift values for a single freq distribution
      compute_lift_vals <- function(freq, dv_probs) {
        if (sum(freq) < min_base_for_lift) return(rep(NA_real_, length(lift)))
        p_observed <- as.numeric(freq) / sum(freq)
        purrr::map_dbl(lift, function(l) {
          use_sym <- (l == 0)
          if (use_sym) {
            p_up   <- bn_freq_prob_shift(freq, type = "exponential", lift = 0.05, impact_metric_type = impact_metric_type)
            p_down <- bn_freq_prob_shift(freq, type = "exponential", lift = -0.05, impact_metric_type = impact_metric_type)
            sum(dv_probs * p_up) - sum(dv_probs * p_down)
          } else {
            p_shifted <- bn_freq_prob_shift(freq, type = "exponential", lift = l, impact_metric_type = impact_metric_type)
            sum(dv_probs * p_shifted) - sum(dv_probs * p_observed)
          }
        })
      }

      lift_results <- temp_ivs_r %>%
        purrr::imap(function(iv_vars, iv_name) {

          # Matrix: rows = iv_vars, cols = lift_col_names
          per_iv_mat <- purrr::map(iv_vars, function(single_iv) {
            w_vec <- if (!is.null(weight)) dat_boot[[weight]] else NULL
            freq_full <- wtd_table(dat_boot[[single_iv]], w = w_vec)
            levels_v <- names(freq_full)

            # Query DV distribution per IV level (shared across brands)
            # dv_metric = "top_box": P(DV_max | IV=v)
            # dv_metric = "mean":    E[DV | IV=v] = Σ d × P(DV=d | IV=v)
            if (type == "gr") {
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

            # Market lift: always computed on full distribution
            market_vals <- compute_lift_vals(freq_full, dv_probs)

            if (is.null(brand_levels)) {
              market_vals
            } else {
              # Per-brand lift appended after market lift
              brand_vals <- purrr::map(brand_levels, function(b) {
                brand_mask <- dat_boot[[brand]] == b
                w_b <- if (!is.null(weight)) dat_boot[[weight]][brand_mask] else NULL
                freq_b <- wtd_table(dat_boot[[single_iv]][brand_mask], w = w_b, levels = levels_v)
                compute_lift_vals(freq_b, dv_probs)
              }) %>%
                unlist()
              c(market_vals, brand_vals)
            }
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
            xmi <- bnlearn::ci.test(
              apply(fit_data[.x], 1, paste0, collapse = "_") %>% as.factor(),
              fit_data[[dv]], test = "mi"
            )

            dplyr::tibble(
              "variable" = .y,
              "mi" = xmi$statistic / (2 * nrow(dat_boot)),
              "p_val" = xmi$p.value
            )
          }
        ) %>%
        dplyr::bind_rows()


      # Bootstrap MI for communities to get CI-based p-value
      if (!is.null(mi_boot) && !is.null(community_assignment)) {
        n_mi_boot <- as.integer(mi_boot)
        n_obs <- nrow(fit_data)

        mi_boot_results <- temp_ivs %>%
          purrr::imap(function(iv_vars, comm_name) {
            boot_mi <- replicate(n_mi_boot, {
              boot_idx <- sample(n_obs, replace = TRUE)
              boot_data <- fit_data[boot_idx, , drop = FALSE]
              composite <- apply(boot_data[iv_vars], 1, paste0, collapse = "_") %>% as.factor()
              xmi <- bnlearn::ci.test(composite, boot_data[[dv]], test = "mi")
              xmi$statistic / (2 * n_obs)
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
    index_sets <- replicate(n_boot, sample(seq_len(nrow(df)), replace = TRUE), simplify = FALSE)

    result <- index_sets %>%
      purrr::map(
        ~engine_diff_multiple(
          indices = .x, data = df,
          bn = bn,
          ivs = ivs, ivs_max = ivs_max, ivs_min = ivs_min, dv_max = dv_max,
          add_mi = TRUE, type = type, n_querry = n_querry, lift = lift,
          impact_metric_type = impact_metric_type, brand = brand, min_base_for_lift = min_base_for_lift,
          include_base = include_base, dv_metric = dv_metric, weight = weight, fit = NULL, mi_boot = mi_boot, community_assignment = community_assignment
        ) %>%
          dplyr::select(-dplyr::any_of("p_val"))
      ) %>%
      purrr::list_rbind() %>%
      tidyr::pivot_longer(cols = !variable, names_to = "metric") %>%
      dplyr::group_by(variable, metric) %>%
      dplyr::summarise(
        mean = mean(value, na.rm = TRUE),
        sd   = sd(value,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        se      = sd / sqrt(pmax(n_boot, 1)),
        t       = mean / se,
        tcrit   = stats::qt(0.975, df = pmax(n_boot - 1, 1)),
        ci_low  = mean - tcrit * se,
        ci_high = mean + tcrit * se,
        p_value = 2 * stats::pt(-abs(t), df = pmax(n_boot - 1, 1)) %>% round(4)
      ) %>%
      dplyr::select(-tcrit) %>%   # housekeeping
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
        tidyselect::matches("^maxVmin"),
        tidyselect::matches("^lift")
      )


  } else {

    result <- engine_diff_multiple(
      indices = NULL, data = df,
      bn = bn,
      ivs = ivs, ivs_max = ivs_max, ivs_min = ivs_min, dv_max = dv_max,
      add_mi = TRUE, type = type, n_querry = n_querry, lift = lift,
      impact_metric_type = impact_metric_type, brand = brand, min_base_for_lift = min_base_for_lift,
      include_base = include_base, dv_metric = dv_metric, weight = weight, fit = fit, mi_boot = mi_boot, community_assignment = community_assignment
    )

  }


  if(index_by != "none"){

    # Resolve lift_first / lift_second to market lift columns (bare lift / lift_0 / lift_10)
    if (index_by %in% c("lift_first", "lift_second")) {
      lift_cols <- grep("^lift", names(result), value = TRUE)
      lift_cols <- lift_cols[!grepl("_mean$|_sd$|_ci_lo$|_ci_hi$", lift_cols)]
      # Use market lift only (bare names without brand suffix)
      market_lift_cols <- lift_cols[grep("^lift$|^lift_\\d+$", lift_cols)]
      if (length(market_lift_cols) == 0) market_lift_cols <- lift_cols
      if (length(market_lift_cols) == 0) {
        warning("index_by = '", index_by, "': no lift columns found. Skipping index.")
        return(result)
      }
      idx <- if (index_by == "lift_first") 1L else min(2L, length(market_lift_cols))
      resolved_col <- market_lift_cols[idx]
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

