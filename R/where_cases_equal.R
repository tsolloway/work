#' where_cases_equal
#' @description returns sql / nosql syntax for case logic, typically used in where command.
#' @param x value vector
#' @param api_field name of api field
#' @export
where_cases_equal <- function(x, api_field = "Needles_CaseID__c"){
  x <- x %>% unlist() %>% unique()

  x <- paste0(api_field, " IN (", paste0("'", x, "'", collapse = ","), ")")

  return(x)
}
