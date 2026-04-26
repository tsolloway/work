#' coefficient_lda
#'
#' @description Build the per-segment linear discriminant coefficient table
#'   directly from the within-class covariance matrix. Returns a tidy tibble
#'   with one row per predictor (plus a `constant` row) and one column per
#'   segment, suitable for plugging into the typing tool's scoring formula
#'   (`score_k = constant_k + sum(beta_jk * x_j)`).
#'
#' @details Uses the closed-form linear-discriminant formulation:
#'   \deqn{\beta_k = \Sigma_W^{-1} \mu_k}
#'   \deqn{const_k = -\tfrac{1}{2} \mu_k^\top \Sigma_W^{-1} \mu_k + \log \pi_k}
#'   This requires the raw `input` data and group labels so it can re-derive
#'   the pooled within-class covariance and invert it. **Use this when the
#'   inputs are well-conditioned** (no perfect collinearity, sample size
#'   comfortably larger than predictor count). It will fail or produce
#'   numerically unstable coefficients when `Sigma_W` is singular or
#'   near-singular — for those cases use [coefficient_lda_colinear()] instead.
#'
#' @param fit A fitted `MASS::lda` object.
#' @param input Optional data frame of predictor variables (rows = respondents,
#'   columns = predictors). If `NULL`, recovered from `fit$call`.
#' @param grp Optional factor (or vector coerced to factor) of class labels
#'   for each row of `input`. If `NULL`, recovered from `fit$call`.
#'
#' @return A tibble with column `variable` (first row `"constant"`, remaining
#'   rows = predictor names) and one numeric column per segment named
#'   `seg_<level>`.
#'
#' @seealso [coefficient_lda_colinear()] — the same output structure computed
#'   via `fit$scaling` (SVD-based), robust to collinear inputs.
#'
#' @export
coefficient_lda <- function(fit, input = NULL, grp = NULL){


  cov_within <- function(input, grp){

    n <- nrow(input)
    glevs <- levels(grp)
    ng <- nlevels(grp)
    Within <- matrix(0, ncol(input), ncol(input))

    for (k in 1:ng) {
      tmp <- grp == glevs[k]
      nk <- sum(tmp)

      Wk <- ((nk - 1)/(n - ng)) * var(input[tmp, ])

      Within <- Within + Wk
    }

    Within
  }

  prior <- fit[["prior"]]

  ng <- prior %>% length()

  GM <- fit[["means"]]


  if(is.null(input)){
    input <- fit[["call"]] %>% paste0() %>% tail(2) %>% head(1) %>% parse(text = .) %>% eval.parent()
  }


  if(is.null(grp)){
    grp <- fit[["call"]] %>% paste0() %>% tail(1) %>% parse(text = .) %>% eval.parent()
  }


  if(!is.factor(grp)){
    grp <- grp %>% as.factor()
  }


  W_inv <- cov_within(input, grp) %>% solve()

  cons <- rep(0, ng)

  Betas <- matrix(0, nrow(W_inv), ng)


  for (k in 1:ng) {
    cons[k] <- (-1/2) * GM[k, ] %*% W_inv %*% GM[k, ] + log(prior[k])
    Betas[, k] <- t(GM[k, ]) %*% W_inv
  }


  result <- rbind(cons, Betas) %>%
    as.data.frame() %>%
    setNames(glue::glue("seg_{names(prior)}")) %>%
    dplyr::mutate(
      variable = c("constant", colnames(GM))
    ) %>%
    dplyr::relocate(variable, .before = 1) %>%
    tibble::as_tibble()


  result

}
