#' bn_ensure_reachability
#' @description Function to ensure all nodes reach DV via directed paths
#' @export
bn_ensure_reachability <- function(bn, dv, max_iter = 10) {

  iter <- 0

  unreachable_nodes <- bn_get_unreachable(bn, dv)

  # Only run if there are unreachable nodes
  while (length(unreachable_nodes) > 0 && iter < max_iter) {
    iter <- iter + 1
    bn <- bn_make_reachable_by_reversal(bn, dv)
    unreachable_nodes <- bn_get_unreachable(bn, dv)
  }

  # Warning if still unreachable nodes
  if (length(unreachable_nodes) > 0) {
    warning(
      glue::glue(
        "{dv} reachability: After {iter} iteration(s), the following nodes are still unreachable from {dv}: {paste(unreachable_nodes, collapse = ', ')}"
      )

    )
  } else {
    message(
      glue::glue("{dv} reachability: All nodes reachable after {iter} iteration(s).")
    )
  }

  return(bn)
}
