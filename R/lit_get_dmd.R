#' lit_get_dmd
#' @description queries litify for dmd data
#' @export
lit_get_dmd <- function(
    cases = NULL
){

  dmd <- work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      "Id", "Needles_CaseID__c", "litify_pm__Client__r.Name", "litify_pm__Incident_date__c",
      "Policy_Limit__c", "Received_Signed_Agreement__c", "Practice_Area__c",
      "litify_pm__Case_Type__r.Name", "Case_Severity__c", "litify_pm__Source__r.Name",
      "Government_Case__c"),
    from_object_child = "litify_pm__Intakes__r",
    select_object_child = c(
      "Id", "Commercial__c", "Source_Name__c"
    ),
    cases = cases,
    cases_field = "Needles_CaseID__c",
    predetermined_names = c(
      "id_matter", "severity", "government",
      "date_incident", "case", "policy_limit",
      "practice_area", "date_agreement_signed", "case_type",
      "client_name", "commercial", "id_intake",
      "source_from_intake", "source_from_matter"
    ),
    sort_predetermined_names = c(
      "id_matter", "id_intake", "case", "client_name",
      "practice_area", "case_type", "severity",
      "commercial", "government", "policy_limit",
      "source_from_intake", "source_from_matter",
      "date_incident","date_agreement_signed"
    )
  )



questionnaire <- work::lit_get_intake_questionnaire(
  cases = dmd[["id_intake"]],
  cases_field = "id_intake",
  clean_cols = FALSE,
  question_filter = c('Type of Accident.', 'Type of Accident')
  )


  if( ncol(questionnaire) > 2 ){

    dmd <- dplyr::left_join(
      dmd,
      questionnaire %>% select(-id_intake),
      by = "id_matter")

  }else if( ncol(questionnaire) <= 2 ){
    matter <- matter %>% mutate(
      type_of_accident = NA
    )
  }





  matter_date_filed <- work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c("Id", "litify_pm__Filed_Date__c"),
    cases = dmd[["id_matter"]],
    cases_field = "Id"
  )



  if( ncol(matter_date_filed) == 2 ){
    names(matter_date_filed) <- c("id_matter", "date_filed")

    dmd <- dplyr::left_join(dmd, matter_date_filed, by = "id_matter")

  }else if( ncol(matter_date_filed) == 1 ){
    dmd <- dmd %>% mutate(
      date_filed = NA
    )
  }



  dmd <- dmd %>% mutate(
    commercial = commercial %>% work::str_scrub() %>% recode("commercial" = TRUE, "non_commercial" = FALSE, .default = NA),
    government = government %>% work::str_scrub() %>% dplyr::recode("yes" = TRUE, "no" = FALSE, .default = NA),
    policy_limit = policy_limit %>% work::translate_limits(),
    date_incident = date_incident %>% as.Date(),
    date_filed = date_filed %>% as.Date(),
    date_agreement_signed = date_agreement_signed %>% as.Date()
  )


return(dmd)

}
