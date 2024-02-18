

lit_get_intake_questionnaire <- function(
    cases = NULL, limit = NULL,
    cases_field = c(NULL, "case", "id_intake", "id_matter", "custom"),
    custom_field = NULL
){

  cases_field <- match.arg(cases_field)

  if( !is.null(cases_field) ){

    cases_field <- switch(
      cases_field,
      "case" = "litify_pm__Matter__r.Needles_CaseID__c",
      "id_intake" = "Id",
      "id_matter" = "litify_pm__Matter__r.Id",
      "custom" = custom_field
    )

  }

  questionnaire  <- lit_get_data(
    from_object = "litify_pm__Intake__c",
    select_object = "Id",
    from_object_child = "litify_pm__Question_Answers__r",
    select_object_child = "litify_pm__Answer__c",
    from_object_parent = "litify_pm__Matter__r",
    select_object_parent = c("Id", "Needles_CaseID__c"),
    from_object_child_parent = "litify_pm__Question__r",
    select_object_child_parent = "litify_pm__Question_Label__c",
    cases = cases,
    cases_field = cases_field,
    limit = NULL,
    predetermined_names = c("id_intake", "id_matter", "case", "answer", "question"),
    sort_predetermined_names = c("id_intake", "id_matter", "case", "question", "answer")
  ) %>%
  tidyr::pivot_wider(names_from  = "question", values_from = "answer", id_cols = "id_intake") %>%
  work::names_clean()


  questionnaire[["incident_date"]] <- questionnaire[["incident_date"]] %>% as.Date()

  questionnaire[["case_type"]]
  lit_get_data(
    from_object = "litify_pm__Case_Type__c",
    select_object = c("Id", "Name"),
    cases = "a03f400000SCf86AAD",
    cases_field = "Id",col_name_clean = F
  )


  questionnaire[["phone_number"]] %>% dialr::phone("USA")





}

