#' future_stop
#' @description future_stop
#' @export
future_stop <- function(){
  future:::ClusterRegistry("stop")
}
