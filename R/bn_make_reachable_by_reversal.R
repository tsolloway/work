#' bn_make_reachable_by_reversal
#' @description bn_make_reachable_by_reversal
#'
#' @param bn A `bnlearn` bn object.
#' @param dv Character. Dependent variable name.
#' @param protected_arcs Optional data frame with `from`/`to` columns. Arcs in
#'   this set are skipped when looking for an arc to reverse, so user-supplied
#'   whitelist directions are preserved. Default `NULL` (no protection).
#'
#' @export
bn_make_reachable_by_reversal <- function(bn, dv, protected_arcs = NULL) {

  # find unreachable nodes
  unreachable_nodes <- bn_get_unreachable(bn, dv)


  # build a fast lookup for protected arcs
  is_protected <- function(from, to) {
    if (is.null(protected_arcs) || nrow(protected_arcs) == 0) return(FALSE)
    any(protected_arcs[["from"]] == from & protected_arcs[["to"]] == to)
  }


  for (u in unreachable_nodes) {
    # find edges involving the unreachable node
    arcs_u <- bnlearn::arcs(bn) %>%
      as.data.frame() %>%
      dplyr::filter(from == u | to == u) %>%
      as.data.frame()

    for (i in seq_len(nrow(arcs_u))) {
      a <- arcs_u[i, ]

      # skip protected arcs
      if (is_protected(a$from, a$to)) next

      # reverse the arc
      bn_rev <- bnlearn::reverse.arc(bn, from = a$from, to = a$to)

      # check if it fixed reachability and didn’t introduce a cycle
      reachable_now <- bn_get_reachable(bn_rev, dv)

      if (u %in% reachable_now) {
        bn <- bn_rev
        break
      }
    }
  }

  bn
}
