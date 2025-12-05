#' Build Layered Bayesian Network Whitelists with Tabu and Bootstrap
#'
#' @description
#' Iteratively builds layered whitelists of arcs using tabu search and
#' bootstrapped arc strength evaluation. Only bootstraps arcs that are new
#' in each layer to reduce redundant computation.
#'
#' @param df Data frame of variables including the dependent variable.
#' @param dv Optional name of the dependent variable (column in `df`).
#' @param white_list_base Initial whitelist of arcs (data frame with `from` and `to`).
#' @param black_list Optional blacklist of arcs (data frame with `from` and `to`).
#' @param dv_arcs Optional arcs to exclude from the first layer (typically arcs involving DV).
#' @param connections_max Maximum number of parents per node (maxp) for iterative layers.
#' @param bootstrap_reps Number of bootstrap replicates for `bn_boot_strength()`.
#' @param algorithm Structure learning algorithm: `"tabu"`, `"hc"`, `"tree.bayes"`.
#' @param score Scoring function for structure learning: `"bic"` or `"aic"`.
#' @param threshold Minimum strength to retain arcs or `"auto"`.
#' @param auto_threshold_ratio Proportion of arcs to retain if threshold = "auto".
#' @param strength_min Minimum bootstrap strength below which arcs are removed.
#' @param align_direction Logical; if TRUE, arcs are oriented to match majority direction.
#' @param return_strength Logical; if TRUE, include `strength` column in each layer output.
#' @param seed Optional integer for reproducibility.
#' @param ... Additional arguments passed to `bn_boot_strength()`.
#'
#' @return A list of whitelists, one per layer. Each data frame optionally includes `strength`.
#' @export
bn_increase_complexity <- function(
    df,
    dv = NULL,
    white_list_base,
    black_list = NULL,
    dv_arcs = NULL,
    connections_max = 3,
    bootstrap_reps = 100,
    algorithm = c("tabu", "hc", "tree.bayes"),
    score = c("bic", "aic"),
    threshold = NULL,
    auto_threshold_ratio = 1/4,
    strength_min = 0.01,
    align_direction = TRUE,
    return_strength = FALSE,
    seed = 1,
    ...
) {

  algorithm <- match.arg(algorithm)
  score <- match.arg(score)


  # ---------------------------
  # Validate required inputs
  # ---------------------------
  if (missing(df) || !is.data.frame(df)) {
    stop("'df' must be provided as a data frame.")
  }

  if (is.null(dv)) {
    stop("'dv' must be specified.")
  }

  if (is.null(white_list_base) || nrow(white_list_base) == 0) {
    stop("'white_list_base' must contain at least one arc. Cannot expand complexity on an empty structure.")
  }

  if (!dv %in% names(df)) {
    stop(glue::glue("Dependent variable '{dv}' not found in the data frame."))
  }

  if (connections_max < 1) {
    stop("'connections_max' must be at least 1.")
  }

  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1)) {
    stop("'seed' must be a single numeric value or NULL.")
  }



  # remove dv column for candidate searches if provided
  if (!is.null(dv)) {
    if (!dv %in% colnames(df)) stop("dv not found in df")
    df <- df %>% dplyr::select(-dplyr::all_of(dv)) %>% as.data.frame()
  } else {
    df <- as.data.frame(df)
  }


  if (is.null(dv_arcs)) {
    dv_arcs <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  }


  # Initialize layer list
  white_list_layer <- list()
  white_list_layer[[1]] <- white_list_base %>% dplyr::setdiff(dv_arcs) %>% as.data.frame()


  # Iteratively build layers
  for (i in seq(from = 2, to = connections_max)) {

    temp_white_list <- white_list_layer[[i - 1]]


    # Run tabu search to generate candidate arcs
    if (!is.null(seed)) set.seed(seed + i)
    tabu_arcs <- bnlearn::tabu(
      x = df,
      maxp = i,
      score = score,
      blacklist = black_list,
      whitelist = temp_white_list
    ) %>%
      bnlearn::arcs() %>%
      as.data.frame()


    # Only keep truly new arcs
    temp_new_white_list <- dplyr::setdiff(tabu_arcs, temp_white_list) %>% as.data.frame()


    # Skip bootstrap if no new arcs
    if (nrow(temp_new_white_list) == 0) {
      white_list_layer[[i]] <- temp_white_list
      next
    }


    # Bootstrap evaluation of new arcs
    if (!is.null(seed)) set.seed(seed + i)
    temp_iv_arc_boot <- work::bn_boot_strength(
      df = df,
      bootstrap_reps = bootstrap_reps,
      algorithm = algorithm,
      score = score,
      maxp = i,
      black_list = black_list,
      white_list = temp_white_list,
      set_dif_arcs_df = temp_white_list,
      evaluate_only_arcs_df = temp_new_white_list,
      align_direction = align_direction,
      threshold = threshold,
      auto_threshold_ratio = auto_threshold_ratio,
      strength_min = strength_min,
      return_direction = return_strength
    )


    # Combine previous whitelist with new arcs
    if (return_strength) {

      # Previous arcs get NA strength
      white_list_layer[[i]] <- dplry::bind_rows(
        temp_white_list %>% dplyr::mutate(strength = NA_real_),
        temp_iv_arc_boot
      ) %>%
        dplyr::distinct() %>%
        as.data.frame()

    } else {

      white_list_layer[[i]] <- dplyr::bind_rows(
        temp_white_list,
        temp_iv_arc_boot %>% dplyr::select(-strength)
      ) %>%
        dplyr::distinct() %>%
        as.data.frame()

    }
  }


  return(white_list_layer)
}
