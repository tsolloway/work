#' Stop all active future cluster workers
#'
#' @description
#' Stops all registered cluster workers in the `future` package.
#' This is useful to clean up resources after using a parallel `future` plan.
#'
#' @return Invisibly returns the result of `future:::ClusterRegistry("stop")`.
#'
#' @examples
#' \dontrun{
#' future_stop()
#' }
#'
#' @export
future_stop <- function() {
  invisible(future:::ClusterRegistry("stop"))
}
