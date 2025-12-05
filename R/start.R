#' start
#' @description Loads common libraries for work. Stops if a required package is not installed.
#' @param lib_sales_force Logical; include salesforcer.
#' @param lib_dev Logical; include dev packages.
#' @param lib_oxl Logical; include openxlsx.
#' @param lib_future Logical; include future and future.apply.
#' @param lib_azure Logical; include AzureStor.
#' @param lib_viz Logical; include highcharter.
#' @param lib_shiny_reporter Logical; include Shiny + supporting packages.
#' @param .quietly Logical; suppress messages from library().
#' @export
start <- function(
    lib_sales_force = FALSE,
    lib_dev = FALSE,
    lib_oxl = FALSE,
    lib_future = FALSE,
    lib_azure = FALSE,
    lib_viz = FALSE,
    lib_shiny_reporter = FALSE,
    .quietly = TRUE
) {

  # Core packages
  pkgs_to_load <- c("work", "dplyr", "purrr", "magrittr", "glue")

  if (lib_sales_force) {
    pkgs_to_load <- c(pkgs_to_load, "salesforcer")
  }

  if (lib_dev) {
    pkgs_to_load <- c(pkgs_to_load, "tictoc")
  }

  if (lib_oxl) {
    pkgs_to_load <- c(pkgs_to_load, "openxlsx")
  }

  if (lib_future) {
    pkgs_to_load <- c(pkgs_to_load, "future", "future.apply")
  }

  if (lib_azure) {
    pkgs_to_load <- c(pkgs_to_load, "AzureStor")
  }

  if (lib_viz) {
    pkgs_to_load <- c(pkgs_to_load, "highcharter")
  }

  if (lib_shiny_reporter) {
    shiny_pkgs <- c("shiny", "bs4Dash", "bslib", "auth0", "fresh", "waiter", "highcharter", "DT")
    pkgs_to_load <- c(pkgs_to_load, shiny_pkgs)
  }


  purrr::walk(pkgs_to_load, load_or_stop)


  if (lib_sales_force) {
    salesforcer::sf_auth()
  }


  invisible(pkgs_to_load)
}
