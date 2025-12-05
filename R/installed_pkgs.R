#' installed_pkgs
#'
#' @description
#' Checks whether one or more R packages are installed.
#'
#' @param pkg Character vector of package names to check.
#'
#' @return
#' Logical vector of the same length as `pkg`, where each element indicates
#' whether the corresponding package is installed (`TRUE` or `FALSE`).
#'
#' @examples
#' # Check a single package
#' installed_pkgs("ggplot2")
#'
#' # Check multiple packages
#' installed_pkgs(c("dplyr", "bnlearn", "fakepackage"))
#'
#' @export
installed_pkgs <- function(pkg) {

  stopifnot(is.character(pkg))

  installed <- installed.packages()[, "Package"]
  pkg %in% installed
}
