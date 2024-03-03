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
      "litify_pm__Primary_Intake__c", "Government_Case__c",
      "litify_pm__Filed_Date__c", "Date_Complaint_Was_Filed__c",
      "X3P_Lawsuit_Filed__c", "Government_Claim_Filed__c"
    ),
    cases = cases,
    cases_field = cases_field,
    limit = limit,
    col_name_clean = FALSE
  ) %>% work::rename_col(
    id_matter = Id,
    case = Needles_CaseID__c,
    client_name = litify_pm__Client__r.Name,
    date_incident = litify_pm__Incident_date__c,
    policy_limit = Policy_Limit__c,
    date_agreement_signed = Received_Signed_Agreement__c,
    practice_area = Practice_Area__c,
    case_type = litify_pm__Case_Type__r.Name,
    severity = Case_Severity__c,
    source = litify_pm__Source__r.Name,
    id_intake = litify_pm__Primary_Intake__c,
    government = Government_Case__c,
    lit_date_filed = litify_pm__Filed_Date__c,
    lit_date_complaint_was_filed = Date_Complaint_Was_Filed__c,
    lit_date_x3p_filed = X3P_Lawsuit_Filed__c,
    lit_date_government_claim_filed = Government_Claim_Filed__c
  ) %>% dplyr::select(
    id_matter, id_intake, case, client_name,
    practice_area, case_type, severity, government, policy_limit, source,
    date_incident,date_agreement_signed,
    lit_date_filed,
    lit_date_complaint_was_filed,
    lit_date_x3p_filed,
    lit_date_government_claim_filed
  ) %>%
    rowwise() %>%
    mutate(
      lit_date_filed = lit_date_filed %>% as.Date(),
      lit_date_complaint_was_filed  = lit_date_complaint_was_filed  %>% as.Date(),
      lit_date_x3p_filed = lit_date_x3p_filed %>% as.Date(),
      lit_date_government_claim_filed = lit_date_government_claim_filed %>% as.Date(),

      date_filed = ifelse(
        all(
          is.na(
            base::c(lit_date_filed, lit_date_complaint_was_filed, lit_date_x3p_filed, lit_date_government_claim_filed)
          )
        ),
        as.Date(NA),
        min(
          lit_date_filed, lit_date_complaint_was_filed, lit_date_x3p_filed, lit_date_government_claim_filed
          , na.rm = TRUE)
      ) %>% as.Date(),

      date_filed = ifelse(is.infinite(date_filed), NA, date_filed) %>% as.Date()
    ) %>%
    ungroup() %>%
    mutate(
      government = government %>% work::str_scrub() %>% dplyr::recode("yes" = TRUE, "no" = FALSE, .default = NA),
      policy_limit = policy_limit %>% work::translate_limits(),
      date_incident = date_incident %>% as.Date(),
      date_filed = date_filed %>% as.Date(),
      date_agreement_signed = date_agreement_signed %>% as.Date()
    )


  return(matter)
}
