#' driver_engine_linear
#'
#' @description Fits univariate OLS regressions for each IV predicting the DV.
#'   Returns a clean summary tibble and a named list of fit objects.
#'
#' @param df Data frame.
#' @param dv Character. Name of the dependent variable column.
#' @param ivs Character vector. Names of independent variable columns.
#' @param weight Character or NULL. Name of the weight column. If NULL, unweighted.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{table}: tibble with columns:
#'       \describe{
#'         \item{variable}{IV name}
#'         \item{n}{number of complete observations}
#'         \item{coefficient}{OLS slope (unstandardized)}
#'         \item{r2}{R-squared (proportion of variance explained)}
#'         \item{p}{model p-value (F-test)}
#'         \item{index}{relative importance: \code{abs(coefficient) / mean(abs(coefficient)) * 100}}
#'       }
#'     \item \code{fits}: named list of \code{lm} objects (keyed by IV name).
#'       Failed fits are \code{NA}.
#'   }
#'
#' @export
driver_engine_linear <- function(
    df,
    dv,
    ivs,
    weight = NULL
){

  safe_lm <- purrr::possibly(function(iv) {
    fml <- stats::reformulate(iv, response = dv)
    if (!is.null(weight)) {
      stats::lm(fml, data = df, weights = df[[weight]])
    } else {
      stats::lm(fml, data = df)
    }
  }, otherwise = NA)

  fits <- purrr::map(rlang::set_names(ivs), safe_lm)

  summaries <- purrr::imap_dfr(fits, function(fit, iv) {
    if (identical(fit, NA)) {
      return(tibble::tibble(
        variable = iv,
        n = NA_integer_,
        coefficient = NA_real_,
        r2 = NA_real_,
        p = NA_real_
      ))
    }

    gl <- broom::glance(fit)

    tibble::tibble(
      variable = iv,
      n = as.integer(gl[["nobs"]]),
      coefficient = stats::coefficients(fit) %>% utils::tail(1) %>% unname(),
      r2 = gl[["r.squared"]],
      p = gl[["p.value"]]
    )
  })

  summaries <- summaries %>%
    dplyr::mutate(
      index = (abs(.data[["coefficient"]]) / mean(abs(.data[["coefficient"]]), na.rm = TRUE)) * 100
    )

  list(
    table = summaries,
    fits = fits
  )
}



#' driver_engine_logistic
#'
#' @description Fits univariate logistic regressions (via \code{rms::lrm()}) for
#'   each IV predicting the DV. Computes a probability shift metric based on
#'   shifting each IV by \code{shift_percentage} of its range, centered on its
#'   mean. For example, with \code{shift_percentage = 0.05}, the IV is evaluated
#'   at \code{mean +/- 0.025 * range} (a 5 percent total window) and the
#'   probability shift is the difference in predicted P(DV=1) between those
#'   two points.
#'
#' @param df Data frame.
#' @param dv Character. Name of the dependent variable column.
#' @param ivs Character vector. Names of independent variable columns.
#' @param shift_percentage Numeric. Total fraction of IV range to shift,
#'   centered on the IV mean (default 0.05 = 5 percent of range).
#' @param weight Character or NULL. Name of the weight column. If NULL, unweighted.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{table}: tibble with columns:
#'       \describe{
#'         \item{variable}{IV name}
#'         \item{n}{number of observations}
#'         \item{coefficient}{logistic regression slope (log-odds per unit IV)}
#'         \item{prob_shift}{change in predicted probability for a
#'           \code{shift_percentage} shift in IV around its mean}
#'         \item{r2}{Nagelkerke pseudo-R-squared (scaled 0-1)}
#'         \item{p}{model p-value (likelihood ratio test)}
#'         \item{Dxy}{Somers Dxy concordance index (-1 to 1, where 0 = random)}
#'         \item{index}{relative importance: abs(prob_shift) / mean(abs(prob_shift)) * 100}
#'       }
#'     \item \code{fits}: named list of \code{lrm} objects (keyed by IV name).
#'       Failed fits are \code{NA}.
#'   }
#'
#' @export
driver_engine_logistic <- function(
    df,
    dv,
    ivs,
    shift_percentage = 0.05,
    weight = NULL
){

  prob_success <- function(x, beta0, beta1) {
    (exp(x * beta1) * exp(beta0)) / (1 + exp(x * beta1) * exp(beta0))
  }

  extract_stat <- function(fit, path) {
    val <- purrr::pluck(fit, !!!path)
    if (is.null(val)) NA_real_ else val
  }

  safe_lrm <- purrr::possibly(function(iv) {
    fml <- stats::reformulate(iv, response = dv)
    if (!is.null(weight)) {
      rms::lrm(fml, data = df, weights = df[[weight]]) %>%
        suppressWarnings() %>%
        suppressMessages()
    } else {
      rms::lrm(fml, data = df) %>%
        suppressWarnings() %>%
        suppressMessages()
    }
  }, otherwise = NA)

  fits <- purrr::map(rlang::set_names(ivs), safe_lrm)

  summaries <- purrr::imap_dfr(fits, function(fit, iv) {
    if (identical(fit, NA)) {
      return(tibble::tibble(
        variable = iv,
        n = NA_integer_,
        coefficient = NA_real_,
        prob_shift = NA_real_,
        r2 = NA_real_,
        p = NA_real_,
        Dxy = NA_real_
      ))
    }

    beta0 <- extract_stat(fit, list("coefficients", 1L))
    beta1 <- extract_stat(fit, list("coefficients")) %>% utils::tail(1) %>% unname()

    iv_vals <- df[[iv]]
    iv_mean <- mean(iv_vals, na.rm = TRUE)
    iv_range <- range(iv_vals, na.rm = TRUE)
    iv_shift <- shift_percentage * (iv_range[2] - iv_range[1])

    prob_low <- prob_success(iv_mean - 0.5 * iv_shift, beta0, beta1)
    prob_high <- prob_success(iv_mean + 0.5 * iv_shift, beta0, beta1)

    tibble::tibble(
      variable = iv,
      n = as.integer(extract_stat(fit, list("stats", "Obs"))),
      coefficient = beta1,
      prob_shift = prob_high - prob_low,
      r2 = extract_stat(fit, list("stats", "R2")),
      p = extract_stat(fit, list("stats", "P")),
      Dxy = extract_stat(fit, list("stats", "Dxy"))
    )
  })

  summaries <- summaries %>%
    dplyr::mutate(
      index = (abs(.data[["prob_shift"]]) / mean(abs(.data[["prob_shift"]]), na.rm = TRUE)) * 100
    )

  list(
    table = summaries,
    fits = fits
  )
}
