#' start
#' @description loads common libraries
#' @param sales_force logical on whether to include esalesforcer
#' @export
start <- function(sales_force = FALSE){

  require(work)
  require(dplyr)
  require(purrr)
  require(magrittr)
  require(glue)

  if(sales_force) require(salesforcer)

}
