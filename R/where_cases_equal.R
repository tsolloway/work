#' where_cases_equal
#' @description returns sql / nosql syntax for case logic, typically used in where command.
#' @param x value vector
#' @param api_field name of api field
#' @export
where_cases_equal <- function(x, api_field = "Needles_CaseID__c", separator = " OR ", logical = "=", return_vector = FALSE){
  x <- x %>% unlist() %>% unique()

  x <- glue::glue("{api_field} {logical} '{x}'") %>% glue::glue_collapse(sep = separator)

  if(return_vector){
    x <- x %>% strsplit(separator) %>% unlist()
  }

  return(x)
}
