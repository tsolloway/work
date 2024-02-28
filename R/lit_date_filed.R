#' lit_date_filed
#' @description lit_date_filed
#' @export
lit_date_filed <- function(
    cases = NULL,
    cases_field = c("case", "id_matter", "cutom")
){

  cases_field <- match.arg(cases_field)

  cases_field <- switch(
    cases_field,
    "case" = "Needles_CaseID__c",
    # "id_intake" = "litify_pm__Intakes__r.Id",
    "id_matter" = "Id",
    "custom" = custom_field
  )



  work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      "Id", "Needles_CaseID__c", "litify_pm__Filed_Date__c", "Date_Complaint_Was_Filed__c",
      "X3P_Lawsuit_Filed__c", "Government_Claim_Filed__c"
    ),
    cases = cases,
    cases_field = cases_field,
    col_name_clean = FALSE
  ) %>% rename(
    id_matter = Id,
    case = Needles_CaseID__c,
    date_filed = litify_pm__Filed_Date__c,
    date_complaint_was_filed = Date_Complaint_Was_Filed__c,
    date_x3p_filed = X3P_Lawsuit_Filed__c,
    date_government_claim_filed = Government_Claim_Filed__c
  )

}










date_filed <-  "
SELECT Id,
 Date_Complaint_Was_Filed__c, litify_pm__Filed_Date__c, X3P_Lawsuit_Filed__c, Government_Claim_Filed__c
FROM litify_pm__Matter__c
limit 1000" %>% salesforcer::sf_query()
