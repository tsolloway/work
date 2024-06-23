#' update_pkgs
#' @description Updates R libraries
#' @inherit install_pkg
#' @export
update_pkgs <- function(ask = FALSE, upgrade = TRUE){

  old.packages() %>% as.data.frame() %>% .[["Package"]] %>% pak::pkg_install(ask = ask, upgrade = upgrade)

}
