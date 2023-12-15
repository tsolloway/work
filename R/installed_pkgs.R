
#' installed_pkgs
#' @description Checks if package is installed.
#' @param pkg Character vector of packages to check to see if installed
#' @export
installed_pkgs <- function(pkg){
  pkg %in% (
    installed.packages() %>% as.data.frame() %>% purrr::pluck("Package")
    )
}
