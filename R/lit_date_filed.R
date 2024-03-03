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


  df <- work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      "Id", "Needles_CaseID__c", "litify_pm__Filed_Date__c", "Date_Complaint_Was_Filed__c",
      "X3P_Lawsuit_Filed__c", "Government_Claim_Filed__c"
    ),
    cases = cases,
    cases_field = cases_field,
    col_name_clean = FALSE
  ) %>% work::rename_col(
    id_matter = Id,
    case = Needles_CaseID__c,
    lit_date_filed = litify_pm__Filed_Date__c,
    lit_date_complaint_was_filed = Date_Complaint_Was_Filed__c,
    lit_date_x3p_filed = X3P_Lawsuit_Filed__c,
    lit_date_government_claim_filed = Government_Claim_Filed__c
  ) %>% dplyr::select(
    case, id_matter,
    lit_date_filed,
    lit_date_complaint_was_filed,
    lit_date_x3p_filed,
    lit_date_government_claim_filed
  )



  df <- df %>% rowwise() %>% mutate(

    lit_date_filed = lit_date_filed %>% as.Date(),
    lit_date_complaint_was_filed  = lit_date_complaint_was_filed  %>% as.Date(),
    lit_date_x3p_filed = lit_date_x3p_filed %>% as.Date(),
    lit_date_government_claim_filed = lit_date_government_claim_filed %>% as.Date(),

    date_filed = min(
      base::c(lit_date_filed, lit_date_complaint_was_filed, lit_date_x3p_filed, lit_date_government_claim_filed),
      na.rm = TRUE
    ) %>% suppressWarnings(),

    date_filed = ifelse(is.infinite(date_filed), NA, date_filed) %>% as.Date()
  )


  return(df)
}


