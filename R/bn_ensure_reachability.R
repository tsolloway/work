#' bn_ensure_reachability
#' @description Function to ensure all nodes reach DV via directed paths
#'
#' @param bn A `bnlearn` bn object.
#' @param dv Character. Dependent variable name.
#' @param max_iter Integer. Max passes of reversal to attempt.
#' @param protected_arcs Optional data frame with `from`/`to` columns. Arcs in
#'   this set are never reversed during reachability enforcement. When supplied,
#'   if any nodes remain unreachable after reversal, a warning is emitted listing
#'   them so the caller can edit the whitelist.
#'
#' @export
bn_ensure_reachability <- function(bn, dv, max_iter = 10, protected_arcs = NULL) {

  iter <- 0

  unreachable_nodes <- bn_get_unreachable(bn, dv)

  # Only run if there are unreachable nodes
  while (length(unreachable_nodes) > 0 && iter < max_iter) {
    iter <- iter + 1
    bn <- bn_make_reachable_by_reversal(bn, dv, protected_arcs = protected_arcs)
    new_unreachable <- bn_get_unreachable(bn, dv)
    if (identical(new_unreachable, unreachable_nodes)) break  # no progress, stop
    unreachable_nodes <- new_unreachable
  }

  # Warning if still unreachable nodes
  if (length(unreachable_nodes) > 0) {

    if (!is.null(protected_arcs) && nrow(protected_arcs) > 0) {
      warning(
        glue::glue(
          "{dv} reachability: {length(unreachable_nodes)} node(s) remain unreachable after {iter} iteration(s) because their only available arcs are protected by the whitelist. ",
          "Unreachable: {paste(unreachable_nodes, collapse = ', ')}. ",
          "Edit the whitelist or set `force_white_list_direction = FALSE` to allow reversal."
        ),
        call. = FALSE
      )
    } else {
      warning(
        glue::glue(
          "{dv} reachability: After {iter} iteration(s), the following nodes are still unreachable from {dv}: {paste(unreachable_nodes, collapse = ', ')}"
        ),
        call. = FALSE
      )
    }

  } else {
    message(
      glue::glue("{dv} reachability: All nodes reachable after {iter} iteration(s).")
    )
  }

  return(bn)
}
