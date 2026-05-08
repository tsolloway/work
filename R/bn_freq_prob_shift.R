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
#'   \code{impact_shift_type}: proportional (0.1 = 10 percent of current mean) or
#'   absolute (0.1 = add 0.1 scale points to mean).
#' @param impact_shift_type Character. How \code{lift} is interpreted:
#'   \code{"proportional"} shifts by a fraction of the current mean;
#'   \code{"absolute"} shifts by a fixed number of scale points;
#'   \code{"range"} shifts by \code{lift} times the IV's full range
#'   (\code{max - min}) — symmetric in both directions, scale-fair
#'   across IVs;
#'   \code{"headroom"} (default) shifts by \code{lift} times the available
#'   room in the requested direction — \code{(max - mean)} for positive
#'   lift, \code{(mean - min)} for negative lift. Use \code{"range"} for
#'   symmetric local sensitivity questions ("what's the response to a
#'   small jitter around the current state"). Use \code{"headroom"} for
#'   asymmetric improvement-direction questions ("how does the DV move
#'   if we close X% of each IV's gap to the top"). Both are
#'   apples-to-apples on mixed-scale batteries.
#'   (Formerly \code{impact_metric_type}, which was retired because it
#'   conflated this IV-shift choice with the DV-outcome display choice
#'   in \code{bn_impact_engine}.)
#' @param return_actual_lift Logical. If \code{TRUE}, returns a list with the shifted
#'   probabilities plus diagnostic info (original/new/target means, actual lift).
#'   If \code{FALSE} (default), returns just the shifted probability vector.
#' @param p_orig Numeric vector. Original probabilities (alternative to \code{freq}).
#'   Must sum to 1. Requires \code{values} to also be provided.
#' @param values Numeric vector. Scale values corresponding to each probability in
#'   \code{p_orig}. Required when using \code{p_orig} instead of \code{freq}.
#' @param scale_range Optional numeric vector of length 2, \code{c(min, max)}.
#'   When provided, overrides the observed min/max of \code{values} for the
#'   purpose of the \code{"absolute"} / \code{"headroom"} / \code{"range"}
#'   shift calculations and the saturation check. Use this when the IV's
#'   theoretical scale extends beyond what's observed in the data — e.g.,
#'   a 1–5 Likert that happens to have only levels 2–5 in a subgroup, but
#'   should still treat the range as 4. \code{NULL} (default) uses observed
#'   min/max.
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
    impact_shift_type = c("headroom", "proportional", "absolute", "range"),
    return_actual_lift = FALSE,
    p_orig = NULL,
    values = NULL,
    scale_range = NULL
){

  type <- match.arg(type)
  impact_shift_type <- match.arg(impact_shift_type)
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
  # Two pairs of scale extents:
  # - val_min / val_max define the SHIFT MAGNITUDE for "absolute" /
  #   "headroom" / "range". Comes from `scale_range` override when
  #   provided, else observed levels.
  # - obs_min / obs_max define the SATURATION CAP — the actual achievable
  #   mean given the observed levels. We can't redistribute probability
  #   to a level that isn't in the freq table, so the cap is always the
  #   observed extremes regardless of the override.
  obs_min <- min(values)
  obs_max <- max(values)
  if (!is.null(scale_range)) {
    if (!is.numeric(scale_range) || length(scale_range) != 2) {
      stop("`scale_range` must be a length-2 numeric vector c(min, max).")
    }
    val_min <- scale_range[1]
    val_max <- scale_range[2]
  } else {
    val_min <- obs_min
    val_max <- obs_max
  }

  if (impact_shift_type == "proportional") {
    target_mean <- orig_mean * (1 + lift)
  } else if (impact_shift_type == "absolute") {
    target_mean <- orig_mean + lift
  } else if (impact_shift_type == "range") {
    # "range" — symmetric: lift × (max − min) added regardless of sign.
    # Every IV gets the same nominal jitter (in scale-fair terms) up
    # AND down, so it's the right tool for symmetric local-sensitivity
    # questions. Doesn't bias toward improvement (unlike headroom).
    target_mean <- orig_mean + lift * (val_max - val_min)
  } else {
    # "headroom" — directional: positive lift closes a fraction of the gap
    # to val_max, negative lift closes a fraction of the gap to val_min.
    # Every IV moves the same proportional distance toward its own
    # boundary, regardless of scale, so rankings on mixed batteries
    # reflect DV responsiveness rather than scale geometry. Self-saturating:
    # at lift = ±1 the target equals the boundary exactly.
    headroom_dir <- if (lift >= 0) (val_max - orig_mean) else (orig_mean - val_min)
    target_mean <- orig_mean + lift * headroom_dir
  }

  # Saturation: when the requested shift would push the mean past the
  # scale's boundary, return a point-mass distribution at that boundary
  # instead of the near-boundary clamped distribution the optimizer would
  # otherwise produce. Matches the natural interpretation of "+0.5
  # absolute shift" on a saturated IV — if everyone would have to be at
  # the highest level for the requested mean to be reached, return
  # exactly that. Headroom / range are also subject to saturation when
  # the requested shift exceeds available room.
  if (impact_shift_type %in% c("absolute", "headroom", "range")) {
    if (target_mean >= obs_max) {
      p_sat <- numeric(length(values))
      p_sat[which.max(values)] <- 1
      names(p_sat) <- names(p_orig)
      if (isTRUE(return_actual_lift)) {
        return(list(
          p_new_val = p_sat, p_orig_val = p_orig,
          mean_orig = orig_mean, mean_new = obs_max, target_mean = target_mean,
          actual_lift = obs_max - orig_mean, saturated = "max"
        ))
      }
      return(p_sat)
    }
    if (target_mean <= obs_min) {
      p_sat <- numeric(length(values))
      p_sat[which.min(values)] <- 1
      names(p_sat) <- names(p_orig)
      if (isTRUE(return_actual_lift)) {
        return(list(
          p_new_val = p_sat, p_orig_val = p_orig,
          mean_orig = orig_mean, mean_new = obs_min, target_mean = target_mean,
          actual_lift = obs_min - orig_mean, saturated = "min"
        ))
      }
      return(p_sat)
    }
  }

  # Clamp target to the achievable range (bounded by the observed levels;
  # optimizer can't redistribute probability to a level not in the freq
  # table). Proportional shifts keep the old clamp — optimizer still runs
  # toward near-boundary target rather than pure point-mass, since
  # proportional shifts rarely hit the boundary anyway and the smooth
  # distribution is more informative when they don't.
  target_mean <- max(obs_min + 1e-6, min(obs_max - 1e-6, target_mean))


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
