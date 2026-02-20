#' Compute Arc-Level Mutual Information and Chi-Square Tests
#'
#' @description
#' For each arc in a Bayesian network, computes the mutual information (MI)
#' and chi-square test statistics using `bnlearn::ci.test()`.
#' Accepts either a fitted Bayesian network (`bn` object) or a data frame of arcs.
#'
#' @details
#' If a `bn` object is provided, its arcs are extracted via `bnlearn::arcs()`.
#' Each arc is then evaluated for conditional independence (MI-based) and tested
#' with the mutual information chi-square test (`test = "mi"`).
#'
#' @param obj A Bayesian network object (`bnlearn` class) or data frame of arcs with `from` and `to` columns.
#' @param df Data frame containing all variables in the network.
#' @param dv Optional dependent variable name. If supplied, output is split into
#'   `all`, `dv` (arcs from dv), and `ivs` (all other arcs).
#' @param round_to Optional integer. If specified, rounds numeric columns to this number of digits.
#'
#' @return
#' If `dv` is `NULL`, returns a tibble with:
#' \describe{
#'   \item{from, to}{Arc endpoints.}
#'   \item{mi}{Mutual information.}
#'   \item{test_statistic}{Chi-square test statistic.}
#'   \item{p_value}{Associated p-value (bounded below at 0.0001).}
#' }
#'
#' If `dv` is supplied, returns a list with:
#' \describe{
#'   \item{all}{Full arc-level results.}
#'   \item{dv}{Subset of arcs where `from == dv`.}
#'   \item{ivs}{Subset of arcs where `from != dv`.}
#' }
#'
#' @examples
#' \dontrun{
#' bn <- bnlearn::hc(learning.test)
#' bn_arc_chisq(bn, learning.test, dv = "A")
#' }
#'
#' @importFrom dplyr mutate across case_when filter rowwise ungroup
#' @importFrom entropy mi.plugin
#' @export
bn_arc_chisq <- function(
    obj,
    df,
    dv = NULL,
    round_to = NULL
){

  ##############################
  # Setup
  ##############################

  if (inherits(obj, "bn")) {
    obj <- bnlearn::arcs(obj)
  }

  if (!all(c("from", "to") %in% colnames(obj))) {
    stop("Input must contain columns 'from' and 'to', or be a bnlearn 'bn' object.")
  }

  arcs_tbl <- as_tibble(obj)

  ##############################
  # Compute mutual information and chi-square test
  ##############################

  output <- obj %>%
    dplyr::as_tibble() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      test_result = list(bnlearn::ci.test(from, to, data = df, test = "mi")),
      mi = entropy::mi.plugin(table(df[[from]], df[[to]])),
      test_statistic = test_result[["statistic"]],
      p_value = test_result[["p.value"]]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-test_result)



  ##############################
  # Post-processing
  ##############################

  # Bound small p-values
  output <- output %>%
    dplyr::mutate(
      p_value = dplyr::case_when(
        p_value < 0.0001 ~ 0.0001,
        .default = p_value
      )
    )

  # Optional rounding
  if (!is.null(round_to) && is.numeric(round_to)) {
    output <- output %>%
      dplyr::mutate_if(is.numeric, round, round_to)
  }



  ##############################
  # Optional DV split
  ##############################

  if (!is.null(dv)) {
    output <- list(
      all = output %>% as.data.frame(),
      dv = output %>% dplyr::filter(from == dv) %>% as.data.frame(),
      ivs = output %>% dplyr::filter(from != dv) %>% as.data.frame()
    )
  }



  ##############################
  # Return
  ##############################

  return(output)
}


