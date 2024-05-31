#' lit_get_roi_append
#' @description queries litify for roi append data
#' @export
lit_get_roi_append <- function(
    cases = NULL,
    limit_clean = TRUE
){

  work::start(TRUE)


  roi_append <- lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      Id, Name, Needles_CaseID__c, litify_pm__Case_Type__r.Name, Case_Severity__c,
      litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c, X3P_Lawsuit_Filed__c, Government_Claim_Filed__c,
    ),
    from_object_child = "Parties__r",
    select_object_child = c(BillingAddress, ShippingAddress),
    cases = cases,
    cases_field = "Needles_CaseID__c",
    col_name_clean = FALSE
  ) %>%
    work::rename_col(
      id_matter = Id,
      severity = Case_Severity__c,
      case_type = litify_pm__Case_Type__r.Name,
      matter_name = Name,
      case = Needles_CaseID__c,
      billing_zip = Account.BillingAddress.postalCode,
      shipping_zip = Account.ShippingAddress.postalCode,

      lit_date_filed = litify_pm__Filed_Date__c,
      lit_date_complaint_was_filed = Date_Complaint_Was_Filed__c,
      lit_date_x3p_filed = X3P_Lawsuit_Filed__c,
      lit_date_government_claim_filed = Government_Claim_Filed__c
    ) %>%
    work:::lit_correct_date_filed_mutate() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      zip = ifelse(is.na(billing_zip), shipping_zip, billing_zip)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      id_matter, matter_name, case, case_type, severity, zip, date_filed
    )


  # questionnaire <- work::lit_get_intake_questionnaire(
  #   cases = roi_append[["case"]],
  #   cases_field = "case",
  #   clean_cols = FALSE,
  #   question_filter = c('Type of Accident.', 'Type of Accident')
  # ) %>%
  #   dplyr::select(-id_intake) %>%
  #   dplyr::distinct()
  #
  # roi_append <- roi_append %>%
  #   dplyr::left_join(questionnaire, by = "id_matter", relationship = "one-to-one")


  roi_append

}
