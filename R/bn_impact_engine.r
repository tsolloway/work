#' Estimate Variable Impact in a Bayesian Network using gRain Inference
#'
#' @description
#' Computes the estimated impact of an independent variable (IV) on a dependent
#' variable (DV) in a fitted Bayesian network by comparing conditional
#' probabilities using `gRain` inference. Optionally applies bootstrapping for
#' uncertainty estimation and parallel execution.
#'
#' @param obj A fitted Bayesian network object or a list returned from
#'   `bn_engine()`. If a list from `bn_engine()` is supplied and includes metadata
#'   (`meta = "bn_model_single"`), the function will automatically extract the
#'   DV, IVs, and fitted model.
#' @param df A data frame used to fit or query the Bayesian network. Must contain
#'   the DV and IV columns.
#' @param iv Character scalar or vector. Independent variable(s) to evaluate.
#' @param dv Character scalar. Dependent variable to query within the network.
#' @param ivs Optional character vector. Full list of IVs in the network; used if
#'   `bn` is a list from `bn_engine()`.
#' @param n_boot Integer. Number of bootstrap replicates to perform.
#'   If `n_boot = 1`, the function computes a single estimate without bootstrapping.
#' @param return_dv_estimate Logical. If `TRUE`, includes the estimated probability
#'   of the DV at the upper level of IV(s) in the output.
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @details
#' For each independent variable, this function:
#' 1. Fits (or reuses) a Bayesian network using `bnlearn::bn.fit()`.
#' 2. Converts the network to a compiled `gRain` object for inference.
#' 3. Estimates the difference in conditional probabilities of the DV when the IV
#'    is set to its observed minimum versus maximum values.
#' 4. Optionally bootstraps this difference to derive standard errors,
#'    confidence intervals, and approximate p-values.
#'
#' Bootstrapping is performed using `boot::boot()`, where each replicate resamples
#' rows from the data and refits the conditional probabilities to the fixed
#' network structure. The effect size is defined as:
#' \deqn{P(DV | IV_{max}) - P(DV | IV_{min})}
#'
#' Parallelization is supported on Unix-like systems via the `"multicore"` method.
#'
#' @return
#' A tibble or data frame containing:
#' \itemize{
#'   \item `dv` — Dependent variable name.
#'   \item `iv` — Independent variable(s) tested.
#'   \item `estimate` — Mean difference in conditional probability.
#'   \item `std_error`, `t_statistic`, `p_value` — Available when `n_boot > 1`.
#'   \item `ci_lower`, `ci_upper` — Bootstrap percentile confidence interval bounds.
#'   \item `dv_estimate` — (Optional) Estimated DV probability at maximum IV value.
#' }
#'
#' @examples
#' \dontrun{
#' library(bnlearn)
#' library(gRain)
#'
#' # Fit a simple Bayesian network
#' bn <- hc(learning.test)
#' fit <- bn.fit(bn, learning.test)
#'
#' # Estimate impact of variable "A" on "B"
#' bn_impact_grain_engine(
#'   obj = fit,
#'   df = learning.test,
#'   dv = "B",
#'   iv = "A",
#'   n_boot = 50,
#'   use_parallel = FALSE
#' )
#' }
#'
#' @seealso [bnlearn::bn.fit()], [gRain::querygrain()], [boot::boot()]
#'
#' @export
bn_impact_engine <- function(
    obj,
    df,
    dv = NULL,
    ivs = NULL,
    do_community = FALSE,
    community_assignment = NULL,
    type = c("cp", "gr", "mi"),
    add_index = TRUE,
    n_boot = 1,
    n_querry = 1e5,
    seed = 1
){

  type <- match.arg(type)
  ivs <- ivs %>% unlist() %>% setNames(NULL)

  # ---------------------------
  # Validate required arguments
  # ---------------------------
  if (!is.data.frame(df)) stop("'df' must be a data frame.")
  work::assert_positive_integer(n_boot, "n_boot")
  if (!is.null(seed)) work::assert_numeric_scalar(seed, "seed")

  # Ensure DV and IV(s) are provided
  if (is.null(dv) && !"meta" %in% names(obj)) {
    stop("'dv' (dependent variable) must be specified.")
  }

  # Column existence checks
  work::assert_cols_exist(df, dv, "data frame for bn_impact_grain_engine()")


  if (!is.null(ivs)) {
    work::assert_cols_exist(df, ivs %>% unlist(), "data frame for bn_impact_grain_engine() [ivs]")
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
      c(dv, ivs) %>% unlist() %>% setNames(NULL)
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
    type = c("cp", "gr"), n_querry = 1e5, seed = NULL
  ){

    if(type == "gr"){
      p1 <- gRain::querygrain(grain_bn, nodes = dv, evidence = attr_iv_boot_max, simplify = TRUE) %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)
      p0 <- gRain::querygrain(grain_bn, nodes = dv, evidence = attr_iv_boot_min, simplify = TRUE) %>% dplyr::select(dplyr::last_col()) %>% unlist() %>% setNames(NULL)

    }else if(type == "cp"){

      # Compute p1 and p0
      if (!is.null(seed)) set.seed(seed)
      p1 <- eval(parse(text = glue::glue(
        "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{attr_dv_boot_max}'), evidence = {attr_iv_boot_max}, n = {n_querry}, method = 'lw')"
      )))

      if (!is.null(seed)) set.seed(seed)
      p0 <- eval(parse(text = glue::glue(
        "bnlearn::cpquery(fitted = fit_boot, event = ({dv} == '{attr_dv_boot_max}'), evidence = {attr_iv_boot_min}, n = {n_querry}, method = 'lw')"
      )))
    }

    data.frame(variable = iv, dv_estimate = p1, effect = p1 - p0)
  }



  # ---------------------------
  # Multiple  difference function
  # ---------------------------

  engine_diff_multiple <- function(
    data, indices = NULL, bn, ivs, ivs_max, ivs_min, dv_max = NULL,
    community_assignment = NULL,
    add_mi = TRUE, type = c("cp", "gr", "mi"),
    n_querry = 1e5, fit = NULL, seed = 1
  ){
    type <- match.arg(type)

    if(!is.null(indices)) dat_boot <- data[indices, , drop = FALSE] else dat_boot <- data

    if(type != "mi"){

      if(is.null(fit)) fit_boot <- bnlearn::bn.fit(bn, dat_boot, method = "bayes") else fit_boot <- fit

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
            seed = seed
          )
        ) %>%
        dplyr::bind_rows() %>%
        dplyr::as_tibble()

    }



    if(type == "mi" || add_mi){

      if(!is.null(community_assignment)) temp_ivs <- community_assignment else temp_ivs <- ivs %>% setNames(ivs)

      results_mi <- temp_ivs %>%
        purrr::imap(
          ~{
            xmi = bnlearn::ci.test(apply(dat_boot[.x], 1, paste0, collapse = "_") %>% as.factor(), dat_boot[[dv]], test = "mi")

            dplyr::tibble(
              "variable" = .y,
              "mi" = xmi$statistic / (2*nrow(dat_boot)),
              "p_val" = xmi$p.value
            )
          }
        ) %>%
        dplyr::bind_rows()


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
          add_mi = TRUE, type = type, n_querry = n_querry, fit = NULL,
          community_assignment = community_assignment
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
        tidyselect::matches("^dv_estimate"),
        tidyselect::matches("^mi"),
        tidyselect::matches("^effect")
      )


  } else {

    result <- engine_diff_multiple(
      indices = NULL, data = df,
      bn = bn,
      ivs = ivs, ivs_max = ivs_max, ivs_min = ivs_min, dv_max = dv_max,
      add_mi = TRUE, type = type, n_querry = n_querry, fit = fit,
      community_assignment = community_assignment
    )

  }


  if(add_index){

    if(type != "mi"){
      result <- result %>%
        dplyr::mutate(
          index = (abs(effect) / mean(abs(effect))) * 100
        )
    }else if(type == "mi"){
      result <- result %>%
        dplyr::mutate(
          index = (abs(mi) / mean(abs(mi))) * 100
        )
    }else{
      stop("Unknown type for index: ", type)
    }

  }

  return(result)
}

