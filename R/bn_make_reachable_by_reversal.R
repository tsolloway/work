#' bn_make_reachable_by_reversal
#' @description bn_make_reachable_by_reversal
#' @export
bn_make_reachable_by_reversal <- function(bn, dv) {

  # find unreachable nodes
  unreachable_nodes <- bn_get_unreachable(bn, dv)

  for (u in unreachable_nodes) {
    # find edges involving the unreachable node
    arcs_u <- bnlearn::arcs(bn) %>%
      as.data.frame() %>%
      dplyr::filter(from == u | to == u) %>%
      as.data.frame()

    for (i in seq_len(nrow(arcs_u))) {
      a <- arcs_u[i, ]

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
