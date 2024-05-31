#' start
#' @description loads common libraries
#' @param lib_sales_force logical on whether to include salesforcer
#' @param lib_dev logical on whether to include dev packages
#' @param lib_future logical on whether to include future packages
#' @export
start <- function(
    lib_sales_force = FALSE,
    lib_dev = FALSE,
    lib_future = FALSE
){

  require(work)
  require(dplyr)
  require(purrr)
  require(magrittr)
  require(glue)


  if(lib_sales_force){
    require(salesforcer)
    sf_auth()
  }


  if(lib_dev){
    require(tictoc)
  }


  if(lib_future){
    require(future)
    require(future.apply)
  }

}
