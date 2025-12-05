#' Generate a Blacklist from a White List of Nodes
#'
#' @description
#' Given a list or vector of node names (a white list), this function generates
#' all possible arcs between the nodes and then removes the arcs that are in the
#' white list, producing a blacklist suitable for `bnlearn` structure learning.
#'
#' @param x A list or vector of node names representing the whitelist.
#' @return A tibble with columns \code{from} and \code{to} representing the blacklist arcs.
#' @export
bn_black_list_from_white_list <- function(x) {
  result <- x %>%
    unlist() %>%
    unique() %>%
    work::make_arcs() %>%
    setdiff(x) %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    setNames(c("from", "to"))

  return(result)
}
