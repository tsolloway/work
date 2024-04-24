#' lit_get_dmd
#' @description queries litify for dmd data
#' @export
lit_get_dmd <- function(
    cases = NULL,
    remove_med_pay = FALSE,
    limit_clean = TRUE
){


  work::start(lib_sales_force = TRUE)


  if( is.null(cases) ){

    cases_field <- NULL
    chunks <- 700

  } else {

    cases_field <- "Needles_CaseID__c"
    chunks <- 1000
  }



  dmd <- lit_get_data(

    from_object = "litify_pm__Matter__c",
    select_object = c(
      Id, Needles_CaseID__c, litify_pm__Client__r.Name, litify_pm__Incident_date__c,
      Policy_Limit__c, Received_Signed_Agreement__c, Practice_Area__c,
      litify_pm__Case_Type__r.Name, Case_Severity__c, litify_pm__Source__r.Name,
      Government_Case__c,
      litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c,
      X3P_Lawsuit_Filed__c, Government_Claim_Filed__c,
      litify_pm__Client__r.BillingPostalCode,
      litify_pm__Primary_Intake__c
    ),

    from_object_child = "litify_pm__Intakes__r",
    select_object_child = c(
      Id, Commercial__c, Source_Name__c
    ),

    cases = cases,
    cases_field = cases_field,
    chunks = chunks

  ) %>% work::rename_col(
    id_matter = Id,
    id_intake_primary = litify_pm__Primary_Intake__c,
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
    lit_date_government_claim_filed = Government_Claim_Filed__c,
    client_zip = litify_pm__Client__r.BillingPostalCode
  ) %>%
    work:::lit_correct_date_filed_mutate() %>%
    select(
      id_matter, id_intake, id_intake_primary, case, client_name,
      practice_area, case_type, severity,
      commercial, government, policy_limit,
      source_from_intake, source_from_matter, date_incident, date_agreement_signed,
      date_filed, client_zip
    ) %>%
    mutate(
      case = case %>% as.numeric(),
      commercial = commercial %>% work::str_scrub() %>% recode("commercial" = TRUE, "non_commercial" = FALSE, .default = NA),
      government = government %>% work::str_scrub() %>% dplyr::recode("yes" = TRUE, "no" = FALSE, .default = NA),

      date_incident = date_incident %>% as.Date(),
      date_filed = date_filed %>% as.Date(),
      date_agreement_signed = date_agreement_signed %>% as.Date()
    )


  dmd <- dmd %>%
    group_by(id_matter) %>%
    mutate(
      matter_count = n()
    ) %>%
    ungroup() %>%
    filter(
      matter_count == 1 |
        (matter_count > 1 & id_intake == id_intake_primary)
    ) %>%
    select(-matter_count)



  if(limit_clean){
    dmd <- dmd %>% mutate(
      policy_limit = policy_limit %>% translate_limits()
    )
  }



  questionnaire <- lit_get_intake_questionnaire(
    cases = dmd[["id_intake"]],
    cases_field = "id_intake",
    clean_cols = FALSE,
    question_filter = c('Type of Accident.', 'Type of Accident')
  ) %>%
    dplyr::select(-id_intake) %>%
    dplyr::distinct()



  questionnaire <- questionnaire %>%
    group_by(id_matter) %>%
    mutate(
      type_of_accident = ifelse( n() == 1, type_of_accident, paste0(type_of_accident, collapse = "|"))
    ) %>%
    ungroup() %>%
    distinct()



  matter_team <- cases %>%
    lit_get_matter_team_DEPRECATE() %>%
    dplyr::select(-case, -case_type, -contains("_count"))



  resolutions <- cases %>%
    lit_get_resolutions() %>%
    dplyr::select(-case)


  dmd <- dmd %>%
    dplyr::left_join(questionnaire, by = "id_matter", relationship = "many-to-one") %>%
    dplyr::full_join(matter_team, by = "id_matter", relationship = "many-to-one") %>%
    dplyr::full_join(resolutions, by = "id_matter", relationship = "many-to-one")


  if(remove_med_pay){
    dmd <- dmd %>% mutate(
      resolution_data = resolution_data %>% map(function(x)x %>% filter(
        !resolution_type %in% c("Intact Med Exp", "Med-Pay Settlement", "MEDPAY") &
          !is.na(id_resolution)
      )),
      resolution_count = map(resolution_data, nrow) %>% unlist()
    )
  }else{
    dmd <- dmd %>% mutate(
      resolution_data = resolution_data %>% map(function(x)x %>% filter(!is.na(id_resolution))),
      resolution_count = map(resolution_data, nrow) %>% unlist()
    )
  }


  return(dmd)
}
