#' start
#' @description loads common libraries
#' @param lib_sales_force logical on whether to include salesforcer
#' @param lib_dev logical on whether to include dev packages
#' @param lib_future logical on whether to include future packages
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
){

  require(work, quietly = .quietly, warn.conflicts = !.quietly)
  require(dplyr, quietly = .quietly, warn.conflicts = !.quietly)
  require(purrr, quietly = .quietly, warn.conflicts = !.quietly)
  require(magrittr, quietly = .quietly, warn.conflicts = !.quietly)
  require(glue, quietly = .quietly, warn.conflicts = !.quietly)


  if(lib_sales_force){
    require(salesforcer, quietly = .quietly, warn.conflicts = !.quietly)
    salesforcer::sf_auth()
  }


  if(lib_dev){
    require(tictoc, quietly = .quietly, warn.conflicts = !.quietly)
  }

  if(lib_oxl){
    require(openxlsx, quietly = .quietly, warn.conflicts = !.quietly)
  }

  if(lib_future){
    require(future, quietly = .quietly, warn.conflicts = !.quietly)
    require(future.apply, quietly = .quietly, warn.conflicts = !.quietly)
  }

  if(lib_azure){
    require(AzureStor, quietly = .quietly, warn.conflicts = !.quietly)
  }

  if(lib_viz){
    require(highcharter, quietly = .quietly, warn.conflicts = !.quietly)
  }

  if(lib_shiny_reporter){
    require(shiny, quietly = .quietly, warn.conflicts = !.quietly)
    require(bs4Dash, quietly = .quietly, warn.conflicts = !.quietly)
    require(bslib, quietly = .quietly, warn.conflicts = !.quietly)
    require(auth0, quietly = .quietly, warn.conflicts = !.quietly)
    require(fresh, quietly = .quietly, warn.conflicts = !.quietly)
    require(waiter, quietly = .quietly, warn.conflicts = !.quietly)
    require(highcharter, quietly = .quietly, warn.conflicts = !.quietly)
    require(DT, quietly = .quietly, warn.conflicts = !.quietly)
  }

}

