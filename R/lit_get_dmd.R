#' lit_get_dmd
#' @description queries litify for dmd data
#' @export
lit_get_dmd <- function(
    cases = NULL,
    limit_clean = TRUE
){


  work::start(TRUE)


  dmd <- lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      "Id", "Needles_CaseID__c", "litify_pm__Client__r.Name", "litify_pm__Incident_date__c",
      "Policy_Limit__c", "Received_Signed_Agreement__c", "Practice_Area__c",
      "litify_pm__Case_Type__r.Name", "Case_Severity__c", "litify_pm__Source__r.Name",
      "Government_Case__c",
      "litify_pm__Filed_Date__c", "Date_Complaint_Was_Filed__c",
      "X3P_Lawsuit_Filed__c", "Government_Claim_Filed__c"
    ),
    from_object_child = "litify_pm__Intakes__r",
    select_object_child = c(
      "Id", "Commercial__c", "Source_Name__c"
    ),
    cases = cases,
    cases_field = "Needles_CaseID__c",
    col_name_clean = FALSE

  ) %>% work::rename_col(
    id_matter = Id,
    severity = Case_Severity__c,
    government = Government_Case__c,
    date_incident = litify_pm__Incident_date__c,
    case = Needles_CaseID__c,
    policy_limit = Policy_Limit__c,
    practice_area = Practice_Area__c,
    date_agreement_signed = Received_Signed_Agreement__c,
    case_type = litify_pm__Case_Type__r.Name,
    client_name = litify_pm__Client__r.Name,
    commercial = litify_pm__Intake__c.Commercial__c,
    id_intake = litify_pm__Intake__c.Id,
    source_from_intake = litify_pm__Intake__c.Source_Name__c,
    source_from_matter = litify_pm__Source__r.Name,
    lit_date_filed = litify_pm__Filed_Date__c,
    lit_date_complaint_was_filed = Date_Complaint_Was_Filed__c,
    lit_date_x3p_filed = X3P_Lawsuit_Filed__c,
    lit_date_government_claim_filed = Government_Claim_Filed__c
  ) %>%
    work:::lit_correct_date_filed_mutate() %>%
    select(
      id_matter, id_intake, case, client_name,
      practice_area, case_type, severity,
      commercial, government, policy_limit,
      source_from_intake, source_from_matter, date_incident, date_agreement_signed,
      date_filed
    ) %>%
    mutate(
      commercial = commercial %>% work::str_scrub() %>% recode("commercial" = TRUE, "non_commercial" = FALSE, .default = NA),
      government = government %>% work::str_scrub() %>% dplyr::recode("yes" = TRUE, "no" = FALSE, .default = NA),

      date_incident = date_incident %>% as.Date(),
      date_filed = date_filed %>% as.Date(),
      date_agreement_signed = date_agreement_signed %>% as.Date()
    )



  if(limit_clean){
    dmd <- dmd %>% mutate(
      policy_limit = policy_limit %>% work::translate_limits()
    )
  }



  questionnaire <- work::lit_get_intake_questionnaire(
    cases = dmd[["id_intake"]],
    cases_field = "id_intake",
    clean_cols = FALSE,
    question_filter = c('Type of Accident.', 'Type of Accident')
  ) %>%
    dplyr::select(-id_intake) %>%
    dplyr::distinct()



  matter_team <- cases %>% work::lit_get_matter_team() %>%
    dplyr::select(-case, -case_type, -contains("_count"))



  resolutions <- cases %>% work::lit_get_resolutions() %>%
    dplyr::select(-case)



  dmd <- dmd %>%
    dplyr::left_join(questionnaire, by = "id_matter", relationship = "many-to-one") %>%
    dplyr::full_join(matter_team, by = "id_matter", relationship = "many-to-one") %>%
    dplyr::full_join(resolutions, by = "id_matter", relationship = "many-to-one")



  return(dmd)
}
