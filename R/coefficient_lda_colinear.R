#' coefficient_lda_colinear
#'
#' @description Build the per-segment linear discriminant coefficient table
#'   directly from a fitted `MASS::lda` object's `scaling` matrix. Output
#'   shape matches [coefficient_lda()] exactly (same column order, same
#'   `variable` column with a `constant` row), so the typing tool can consume
#'   either function interchangeably.
#'
#' @details Uses MASS::lda's pre-computed scaling — the SVD-based
#'   discriminant directions — instead of inverting the within-class
#'   covariance matrix. Mathematically equivalent to [coefficient_lda()]
#'   for classification (the linear discriminant boundaries are identical;
#'   the two formulations differ only by a class-invariant offset that
#'   cancels under `argmax_k(score_k)`).
#'
#'   **Use this** when:
#'   - `coefficient_lda()` errors with a "system is computationally singular"
#'     message,
#'   - your LDA inputs include perfectly or near-perfectly collinear
#'     predictors,
#'   - or sample size is small relative to the number of predictors.
#'
#'   MASS::lda computes the scaling matrix via SVD with rank reduction,
#'   so it tolerates rank-deficient inputs that the closed-form
#'   `Sigma_W^{-1}` inversion in [coefficient_lda()] cannot handle.
#'
#'   Per-segment coefficients are computed as
#'   \deqn{\beta_k = L L^\top (\mu_k - \bar\mu)}
#'   where `L = fit$scaling` and `\bar\mu` is the prior-weighted grand mean.
#'   Per-segment constants are
#'   \deqn{const_k = \log \pi_k - \tfrac{1}{2}\| L^\top(\mu_k - \bar\mu) \|^2 - \bar\mu^\top \beta_k.}
#'   These produce identical class predictions to [coefficient_lda()];
#'   numeric coefficient values may differ but the typing tool's downstream
#'   `argmax(score_k)` lookup is unchanged.
#'
#' @param fit A fitted `MASS::lda` object.
#'
#' @return A tibble with column `variable` (first row `"constant"`,
#'   remaining rows = predictor names) and one numeric column per segment
#'   named `seg_<level>`. Output shape matches [coefficient_lda()].
#'
#' @seealso [coefficient_lda()] — the closed-form `Sigma_W^{-1}` version.
#'   Faster when inputs are well-conditioned, but errors on collinearity.
#'
#' @export
coefficient_lda_colinear <- function(fit){

  stopifnot(inherits(fit, "lda"))

  scaling     <- fit[["scaling"]]
  group_means <- fit[["means"]]
  priors      <- fit[["prior"]]
  predictors  <- rownames(scaling)
  segments    <- rownames(group_means)

  grand_mean <- colSums(priors * group_means)
  group_ld   <- sweep(group_means, 2, grand_mean) %*% scaling   # k x ndisc

  # Per-segment coefficient vector: scaling %*% scaling^T %*% (mu_k - mu_bar)
  coefs <- scaling %*% t(group_ld)                              # p x k

  # Per-segment constant
  centroid_sq <- rowSums(group_ld^2)                            # k
  gm_term     <- as.numeric(grand_mean %*% coefs)               # k
  consts      <- log(priors) - 0.5 * centroid_sq - gm_term      # k

  # Match coefficient_lda() output exactly:
  #   col 1 = "variable" with "constant" first then predictor names
  #   col 2..k+1 = "seg_<segment_name>" with constants in row 1 then betas
  result <- rbind(consts, coefs) %>%
    as.data.frame() %>%
    setNames(glue::glue("seg_{segments}")) %>%
    dplyr::mutate(
      variable = c("constant", predictors)
    ) %>%
    dplyr::relocate(variable, .before = 1) %>%
    tibble::as_tibble()

  result

}
