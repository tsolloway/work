#' lit_get_matter
#' @description queries litify for matter data
#' @export
lit_get_matter <- function(
    cases = NULL, limit = NULL,
    cases_field = c("case", "id_intake", "id_matter", NULL)
){

  cases_field <- match.arg(cases_field)

  if( !is.null(cases_field) ){

    cases_field <- switch(
      cases_field,
      "case" = "Needles_CaseID__c",
      "id_intake" = "litify_pm__Intakes__r.Id",
      "id_matter" = "Id",
      "custom" = custom_field
    )
  }


  matter <- work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      "Id", "Needles_CaseID__c", "litify_pm__Client__r.Name", "litify_pm__Incident_date__c",
      "Policy_Limit__c", "Received_Signed_Agreement__c", "Practice_Area__c",
      "litify_pm__Case_Type__r.Name", "Case_Severity__c", "litify_pm__Source__r.Name",
      "litify_pm__Primary_Intake__c", "Government_Case__c"),
    cases = cases,
    cases_field = cases_field,
    limit = limit,
    predetermined_names = c(
      "id_matter", "severity", "government", "date_incident", "id_intake",
      "case", "policy_limit", "practice_area", "date_agreement_signed", "case_type", "client_name", "source"),
    sort_predetermined_names = c(
      "id_matter", "id_intake", "case", "client_name",
      "practice_area", "case_type", "severity", "government", "policy_limit", "source",
      "date_incident","date_agreement_signed"
      )
  )


  matter_date_filed <- work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c("Id", "litify_pm__Filed_Date__c"),
    cases = cases,
    cases_field = cases_field
  )


  if( ncol(matter_date_filed) == 2 ){
    names(matter_date_filed) <- c("id_matter", "date_filed")

    matter <- dplyr::left_join(matter, matter_date_filed, by = "id_matter")

  }else if( ncol(matter_date_filed) == 1 ){

    matter <- matter %>% mutate(
      date_filed = NA
    )
  }


  matter <- matter %>% mutate(
    government = government %>% work::str_scrub() %>% dplyr::recode("yes" = TRUE, "no" = FALSE, .default = NA),
    policy_limit = policy_limit %>% work::translate_limits(),
    date_incident = date_incident %>% as.Date(),
    date_filed = date_filed %>% as.Date(),
    date_agreement_signed = date_agreement_signed %>% as.Date()
  )


  return(matter)
}
