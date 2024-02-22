#' lit_get_matter_team
#' @description queries litify for matter team data
#' @param cases vector of case numbers
#' @param cases_field field type the cases are referring to
#' @param limit Limit for the query
#' @export
lit_get_case_type <- function(
    cases,
    cases_field = c("id_case_type", "id_intake", "id_matter", "case", "case_type"),
    limit = NULL){


  cases_field <- match.arg(cases_field)

  if( cases_field != "id_case_type") stop("case_field not programmed yet in lit_get_case_type.")

  # cases_field <- switch(
  #   cases_field,
  #   "case" = "Needles_CaseID__c",
  #   "id_intake" = "litify_pm__Intake__c.Id",
  #   "id_matter" = "Id",
  #   "id_case_type" = "litify_pm__Case_Type__c",
  #   "case_type" = "litify_pm__Case_Type__r.Name"
  # )



  lit_get_data(
    from_object = "litify_pm__Case_Type__c",
    select_object = c("Id", "Name"),
    from_object_child = "litify_pm__Intakes__r",
    cases = cases,
    cases_field = "Id",
    predetermined_names = c("id_case_type", "case_type"),
    sort_predetermined_names = c("id_case_type", "case_type"),
    limit = limit
  )

}

