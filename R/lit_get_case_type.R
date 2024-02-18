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


  cases_field <- switch(
    cases_field,
    "case" = "Needles_CaseID__c",
    "id_intake" = "litify_pm__Intake__c.Id",
    "id_matter" = "Id",
    "id_case_type" = "litify_pm__Case_Type__c",
    "case_type" = "litify_pm__Case_Type__r.Name"
  )



  lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c("Id", "Needles_CaseID__c","litify_pm__Case_Type__c", "litify_pm__Case_Type__r.Name"),
    from_object_child = "litify_pm__Intakes__r",
    select_object_child = "Id",
    cases = cases,
    cases_field = cases_field,
    predetermined_names = c("id_matter", "id_case_type","case", "case_type", "id_intake"),
    sort_predetermined_names = c("id_intake", "id_matter", "case", "id_case_type", "case_type"),
    limit = limit
  )

}

