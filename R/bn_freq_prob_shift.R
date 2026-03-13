#' Shift a Frequency Distribution to Achieve a Target Mean Lift
#'
#' @description
#' Given a named frequency table (or raw probabilities + values), computes a new
#' probability distribution whose mean is shifted by a specified percentage lift.
#' Three shift methods are available, each making different assumptions about how
#' probability mass redistributes:
#'
#' \itemize{
#'   \item \strong{Exponential} (default): Maximum-entropy tilting. The most neutral
#'     redistribution that achieves the target mean, minimizing KL-divergence from
#'     the original distribution.
#'   \item \strong{Quadratic}: Minimizes the sum of squared differences from the
#'     original probabilities (L2 distance) subject to the mean constraint. Uses
#'     \code{CVXR} for convex optimization.
#'   \item \strong{Linear}: Fastest but can produce negative probabilities for large
#'     shifts. Best suited for small perturbations.
#' }
#'
#' @param freq A named numeric vector of counts (e.g., from \code{table()}).
#'   Names are interpreted as numeric scale values. Either \code{freq} or both
#'   \code{p_orig} and \code{values} must be provided.
#' @param type Character. Shift method: \code{"exponential"} (default),
#'   \code{"linear"}, or \code{"quadratic"}.
#' @param lift Numeric. Target lift for the mean. Interpretation depends on
#'   \code{impact_metric_type}: proportional (0.1 = 10 percent of current mean) or
#'   absolute (0.1 = add 0.1 scale points to mean).
#' @param impact_metric_type Character. How \code{lift} is interpreted:
#'   \code{"proportional"} (default) shifts by a fraction of the current mean;
#'   \code{"absolute"} shifts by a fixed number of scale points.
#' @param return_actual_lift Logical. If \code{TRUE}, returns a list with the shifted
#'   probabilities plus diagnostic info (original/new/target means, actual lift).
#'   If \code{FALSE} (default), returns just the shifted probability vector.
#' @param p_orig Numeric vector. Original probabilities (alternative to \code{freq}).
#'   Must sum to 1. Requires \code{values} to also be provided.
#' @param values Numeric vector. Scale values corresponding to each probability in
#'   \code{p_orig}. Required when using \code{p_orig} instead of \code{freq}.
#'
#' @return
#' If \code{return_actual_lift = FALSE}: a named numeric vector of shifted probabilities.
#'
#' If \code{return_actual_lift = TRUE}: a list containing:
#' \itemize{
#'   \item \code{p_new_val}: shifted probabilities
#'   \item \code{p_orig_val}: original probabilities
#'   \item \code{mean_orig}: original weighted mean
#'   \item \code{mean_new}: achieved weighted mean
#'   \item \code{mean_target}: target weighted mean
#'   \item \code{mean_shift}: absolute mean change
#'   \item \code{lift_target}: requested lift
#'   \item \code{lift_actual}: achieved lift
#' }
#'
#' @examples
#' \dontrun{
#' freq <- c("1" = 50, "2" = 120, "3" = 200, "4" = 100, "5" = 30)
#'
#' # Exponential tilting (default, most principled)
#' bn_freq_prob_shift(freq, lift = 0.1)
#'
#' # With diagnostics
#' bn_freq_prob_shift(freq, lift = 0.1, return_actual_lift = TRUE)
#'
#' # Using raw probabilities instead of counts
#' bn_freq_prob_shift(p_orig = c(0.1, 0.24, 0.4, 0.2, 0.06), values = 1:5, lift = 0.1)
#' }
#'
#' @export
bn_freq_prob_shift <- function(
    freq = NULL,
    type = c("exponential", "linear", "quadratic"),
    lift = 0.1,
    impact_metric_type = c("proportional", "absolute"),
    return_actual_lift = FALSE,
    p_orig = NULL,
    values = NULL
){

  type <- match.arg(type)
  impact_metric_type <- match.arg(impact_metric_type)
  normalize <- function(x) x / sum(x)


  # ---------------------------
  # Input handling
  # ---------------------------
  if (!is.null(freq)) {
    values <- freq %>% names() %>% as.numeric() %>% setNames(NULL)
    counts <- freq %>% as.numeric() %>% setNames(NULL)
    p_orig <- counts / sum(counts)
  } else if (!is.null(p_orig) && !is.null(values)) {
    if (length(p_orig) != length(values)) stop("'p_orig' and 'values' must have the same length.")
    p_orig <- p_orig / sum(p_orig)
  } else {
    stop("Provide either 'freq' or both 'p_orig' and 'values'.")
  }


  # Original and target means
  orig_mean <- sum(values * p_orig)
  if (impact_metric_type == "proportional") {
    target_mean <- orig_mean * (1 + lift)
  } else {
    target_mean <- orig_mean + lift
  }

  # Clamp target to feasible range (can't exceed min/max of scale)
  val_min <- min(values)
  val_max <- max(values)
  target_mean <- max(val_min + 1e-6, min(val_max - 1e-6, target_mean))


  # ---------------------------
  # Shift engines
  # ---------------------------

  quadratic_shift_type <- function(p_orig, values, target_mean) {
    p_new <- CVXR::Variable(length(p_orig))
    objective <- CVXR::sum_squares(p_new - p_orig)
    constraints <- list(
      sum(p_new) == 1,
      sum(values * p_new) == target_mean,
      p_new >= 1e-6
    )

    prob <- CVXR::Problem(CVXR::Minimize(objective), constraints)
    result <- CVXR::solve(prob)

    result$getValue(p_new) %>% as.numeric() %>% setNames(values)
  }


  exponential_shift_type <- function(p_orig, values, target_mean) {
    tilted_mean <- function(lambda) {
      w <- p_orig * exp(lambda * values)
      w <- normalize(w)
      sum(values * w)
    }

    # Adaptive interval: widen until endpoints bracket the root
    lo <- -10; hi <- 10
    f_lo <- tilted_mean(lo) - target_mean
    f_hi <- tilted_mean(hi) - target_mean

    for (i in 1:10) {
      if (sign(f_lo) != sign(f_hi)) break
      lo <- lo * 2; hi <- hi * 2
      f_lo <- tilted_mean(lo) - target_mean
      f_hi <- tilted_mean(hi) - target_mean
    }

    if (sign(f_lo) == sign(f_hi)) {
      warning("bn_freq_prob_shift: could not bracket root for target_mean = ",
              round(target_mean, 4), " (orig = ", round(orig_mean, 4), "). Returning NA.")
      return(NA_real_)
    }

    lambda <- uniroot(
      function(l) tilted_mean(l) - target_mean,
      interval = c(lo, hi)
    )$root

    p_new <- p_orig * exp(lambda * values)
    normalize(p_new)
  }


  linear_shift_type <- function(p_orig, values, target_mean) {
    num <- sum(values * p_orig) - target_mean
    den <- target_mean * sum(values * p_orig) - sum(values^2 * p_orig)
    alpha <- num / den

    p_new <- p_orig * (1 + alpha * values)
    normalize(p_new)
  }


  # ---------------------------
  # Dispatch
  # ---------------------------
  if (type == "quadratic") {
    p_new_val <- quadratic_shift_type(p_orig, values, target_mean)
  } else if (type == "exponential") {
    p_new_val <- exponential_shift_type(p_orig, values, target_mean)
  } else if (type == "linear") {
    p_new_val <- linear_shift_type(p_orig, values, target_mean)
  } else {
    stop("Unknown shift type: ", type)
  }


  # NA means the shift failed — propagate immediately

  if (length(p_new_val) == 1 && is.na(p_new_val)) {
    if (return_actual_lift) {
      return(list(
        p_new_val = NA_real_, p_orig_val = p_orig,
        mean_orig = orig_mean, mean_new = NA_real_,
        mean_target = target_mean, mean_shift = NA_real_,
        lift_target = lift, lift_actual = NA_real_
      ))
    } else {
      return(NA_real_)
    }
  }

  # Floor any near-zero or negative probabilities and re-normalize
  if (any(p_new_val < 1e-6)) {
    p_new_val <- pmax(p_new_val, 1e-6)
    p_new_val <- normalize(p_new_val)
  }

  new_mean <- sum(values * p_new_val)


  # ---------------------------
  # Return
  # ---------------------------
  if (return_actual_lift) {
    list(
      p_new_val = p_new_val,
      p_orig_val = p_orig,
      mean_orig = orig_mean,
      mean_new = new_mean,
      mean_target = target_mean,
      mean_shift = new_mean - orig_mean,
      lift_target = lift,
      lift_actual = (new_mean / orig_mean) - 1
    )
  } else {
    p_new_val
  }

}
