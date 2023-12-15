#' update_pkgs
#' @description Updates R libraries
#' @inherit install_pkg
#' @export
update_pkgs <- function(ask = FALSE, upgrade = FALSE, on_exit_restart = TRUE){

  install_pkg(update_pkgs = TRUE, on_exit_restart = on_exit_restart)

}
