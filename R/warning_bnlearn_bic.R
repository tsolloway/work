#' Display Clarification About BIC Interpretation in bnlearn
#'
#' @description
#' Prints a warning reminding users that in **bnlearn**, higher BIC values
#' indicate better model fit because the score is internally rescaled by \(-2\).
#'
#' @details
#' This function provides a simple, standardized reminder when interpreting
#' model comparisons or summaries involving BIC within the `bnlearn` package.
#' It is primarily a helper to prevent confusion with the conventional
#' interpretation of BIC (where lower is better).
#'
#' @return
#' Invisibly returns `NULL` after displaying a warning message.
#'
#' @examples
#' bn_bnlearn_bic_warning()
#'
#' @export
warning_bnlearn_bic <- function() {
  warning("Higher BIC values are better in bnlearn, as it's rescaled by -2")
  invisible(NULL)
}
