#' lit_get_matter
#' @description queries litify for matter data
#' @export
lit_get_matter <- function(attach_accident_type = FALSE){

  work::start(lib_sales_force = TRUE)

  df <- lit_get_data(
    from_object = "litify_pm__Matter__c",

    select_object = work:::c(
      Id, Name, Needles_CaseID__c, litify_pm__Status__c, Display_Name2__c, Litigation_At__c,

      Received_Signed_Agreement__c, litify_pm__Incident_date__c,

      Practice_Area__c, litify_pm__Case_Type__r.Name, Policy_Limit__c, Case_Severity__c,
      Government_Case__c,

      Drop_Date__c, Dropped_At__c, Drop_Or_Pending_Drop_At__c,
      Drop_Subout_Date__c, Subout_Date__c, Subout_At__c,
      litify_pm__Closed_Reason__c, Sub_Status__c,

      Lead_Case__c, Lead_Matter__c,
      litify_pm__Companion__r.Lead_Case__c,

      litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c,
      X3P_Lawsuit_Filed__c, Government_Claim_Filed__c,

      Closed_At__c, litify_pm__Closed_Date__c
    ),
  ) %>%

    filter(
      Display_Name2__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) %>% work::if_na_return(TRUE)
    ) %>%

    filter(
      litify_pm__Status__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) %>% work::if_na_return(TRUE)
    ) %>%

    filter(
      litify_pm__Closed_Reason__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) %>% work::if_na_return(TRUE)
    ) %>%

    filter(
      litify_pm__Closed_Reason__c %>% stringr::str_detect(stringr::coll("duplicate", ignore_case = TRUE), negate = TRUE) %>% work::if_na_return(TRUE)
    ) %>%

    mutate(
      litify_pm__Filed_Date__c = litify_pm__Filed_Date__c %>% as.Date(),
      Date_Complaint_Was_Filed__c = Date_Complaint_Was_Filed__c %>% as.Date(),
      X3P_Lawsuit_Filed__c = X3P_Lawsuit_Filed__c %>% as.Date(),
      Government_Claim_Filed__c = Government_Claim_Filed__c %>% as.Date(),
      date_filed = ifelse(
        all(is.na(base::c(litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c, X3P_Lawsuit_Filed__c, Government_Claim_Filed__c))),
        as.Date(NA),
        min(litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c, X3P_Lawsuit_Filed__c, Government_Claim_Filed__c, na.rm = TRUE)
      ) %>% as.Date()
    ) %>%
    ungroup() %>%
    select(-litify_pm__Filed_Date__c, -Date_Complaint_Was_Filed__c, -X3P_Lawsuit_Filed__c, -Government_Claim_Filed__c) %>%

    rename_col(
      .select = TRUE,
      .distinct = TRUE,

      id_matter = Id,
      case = Needles_CaseID__c,
      case_name = Display_Name2__c,
      name_matter = Name,

      practice_area = Practice_Area__c,
      case_severity = Case_Severity__c,
      case_type = litify_pm__Case_Type__r.Name,
      policy_limit = Policy_Limit__c,
      government = Government_Case__c,

      source_type = litify_pm__Source_Type__c,
      source_category = litify_pm__Source__r.Category__c,
      marketing_details = Marketing_Details__c,

      date_litigation_at = Litigation_At__c,
      date_signed_agreement = Received_Signed_Agreement__c,
      date_incident = litify_pm__Incident_date__c,
      date_filed = date_filed,

      status = litify_pm__Status__c,
      closed_reason = litify_pm__Closed_Reason__c,

      date_drop_c = Drop_Date__c,
      date_drop_or_pending_drop_c = Drop_Or_Pending_Drop_At__c,
      date_drop_subout_c = Drop_Subout_Date__c,
      date_dropped_at_c = Dropped_At__c,

      sub_status = Sub_Status__c,
      date_subout_at_c = Subout_At__c,
      date_subout_c = Subout_Date__c,

      lead_case = Lead_Case__c,
      lead_matter = Lead_Matter__c,
      companion_lead_case = litify_pm__Companion__r.Lead_Case__c,

      date_closed_at = Closed_At__c,
      date_closed_date = litify_pm__Closed_Date__c
    ) %>%
    mutate(

      case = case %>% as.numeric(),

      date_signed_agreement = date_signed_agreement %>% as.Date(),
      date_litigation_at = date_litigation_at %>% as.Date(),
      date_incident = date_incident %>% as.Date(),

      date_closed_at = date_closed_at %>% as.Date(),
      date_closed_date = date_closed_date %>% as.Date(),

      lead_case = lead_case %>% recode("Yes" = TRUE, "No" = FALSE),
      litigation = date_litigation_at %>% is.na() %>% not(),

      practice_area = practice_area %>% recode("Class Action" = "Class-Action"),
      practice_area = ifelse(practice_area == "PI" & litigation == TRUE, "PI-Litigation",
                             ifelse(practice_area == "PI" & litigation == FALSE, "PI-Pre-Litigation", practice_area)
      ),

      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Class Action - Consumer Litigation"), "Class-Action-Consumer", practice_area),
      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Consumer"), "Class-Action-Consumer", practice_area),

      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Class Action - Wage & Hour"), "Class-Action-W&H", practice_area),
      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Wage & Hour (Class Action)"), "Class-Action-W&H", practice_area),
      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Wage & Hour"), "Class-Action-W&H", practice_area),


      government = government %>% work::str_scrub() %>% dplyr::recode("yes" = TRUE, "no" = FALSE, .default = NA),
      policy_limit_cleaned = policy_limit %>% translate_limits(),

      date_drop_c = date_drop_c %>% as.Date(),
      date_drop_or_pending_drop_c = date_drop_or_pending_drop_c %>% as.Date(),
      date_drop_subout_c = date_drop_subout_c %>% as.Date(),
      date_dropped_at_c = date_dropped_at_c %>% as.Date(),

      date_subout_at_c = date_subout_at_c %>% as.Date(),
      date_subout_c = date_subout_c %>% as.Date(),

      signed_month = date_signed_agreement %>% zoo::as.yearmon() %>% zoo::as.Date(),
      litigation_start_month = date_litigation_at %>% zoo::as.yearmon() %>% zoo::as.Date(),

      litigation_same_month = (signed_month == litigation_start_month) %>% work::if_na_return(FALSE),


      closed_reason = closed_reason %>% recode(
        "Dropped" = "dropped",
        "Referred Out" = "referred_out",
        "Settled" = "settled",
        "Subbed-Out" = "subout",
        "Dropped by client" = "dropped",
        "Verdict" = "verdict"
      ),

      status = status %>% recode(
        "Dropped (No Lien)" = "dropped",
        "Dropped" = "dropped",
        "Dropped (Lien )" = "dropped",
        "Dropped In Pro Per (Lien)" = "dropped",
        "Dropped In Pro Per (No Lien)"  = "dropped",
        "Dropped (Lien)"  = "dropped",
        "Dropped MTBR (Lien)"  = "dropped",
        "Dropped MTBR (No Lien)"  = "dropped",

        "Pending Drop (Lien)" = "pending_drop",
        "Pending Drop (No Lien)" = "pending_drop",
        "Pending Drop In Pro Per (Lien)" = "pending_drop",
        "Pending Drop In Pro Per (No Lien) " = "pending_drop",
        "Pending Drop MTBR (Lien)" = "pending_drop",
        "Pending Drop" = "pending_drop",
        "Pending Drop In Pro Per (No Lien)" = "pending_drop",
        "Pending Drop MTBR (No Lien)" = "pending_drop",

        "Sub Out (Lien)" = "subout",
        "Sub Out (No Lien)" = "subout",
        "Subout" = "subout",

        "Pending Sub Out" = "pending_subout",
        "Pending Sub Out (Lien)" = "pending_subout",
        "Pending Sub Out (No Lien)" = "pending_subout",

        "Referral Initiated" = "referral_initiated",
        "Referral Rejected"  = "referral_rejected",
        "Referral Requested" = "referral_requested",
        "Referred Out" = "referred_out",
        "Settled" = "settled",
        "Liens" = "liens",

        "Litigation" = "litigation",
        "Pre-Litigation" = "pre-litigation",
        "Attorney Review" = "attorney_review",
        "Closed" = "closed"
      )
    ) %>%
    rowwise() %>%
    mutate(
      date_drop = ifelse(status == "dropped" | (status == "closed" & closed_reason == "dropped"), date_drop_subout_c, NA) %>% as.Date(),
      date_subout = date_subout_c,
      date_subout_unauditted = ifelse(status == "subout" | (status == "closed" & closed_reason == "subout"), date_drop_subout_c, NA) %>% as.Date(),

    ) %>%
    mutate(
      date_subout = ifelse(is.na(date_subout), date_subout_unauditted, date_subout) %>% as.Date(),
      date_drop = ifelse(
        (status == "dropped" | (status == "closed" & closed_reason == "dropped")) & is.na(date_drop),
        date_dropped_at_c, date_drop) %>% as.Date(),
    ) %>%
    ungroup() %>%
    mutate(

      drop_month = date_drop %>% zoo::as.yearmon() %>% zoo::as.Date(),

      subout_month = date_subout %>% zoo::as.yearmon() %>% zoo::as.Date(),

      drop_same_month = (signed_month == drop_month),
      subout_same_month = (signed_month == subout_month),

      drop_days = (date_drop - date_signed_agreement) %>% as.numeric(),
      subout_days = (date_subout - date_signed_agreement) %>% as.numeric(),
    )


  if(attach_accident_type){

    questionnaire <- lit_get_intake_questionnaire(
      cases = df[["id_matter"]],
      cases_field = "id_matter",
      clean_cols = FALSE,
      question_filter = c('Type of Accident.', 'Type of Accident')
    ) %>%
      select(-id_intake) %>%
      distinct() %>%
      group_by(id_matter) %>%
      mutate(
        type_of_accident = ifelse( n() == 1, type_of_accident, paste0(type_of_accident, collapse = "|"))
      ) %>%
      ungroup() %>%
      distinct()

    df <- left_join(df, questionnaire, by = "id_matter")
  }


  return(df)
}





#' lit_get_matter_DEPRECATE
#' @description queries litify for matter data
#' @export
lit_get_matter_DEPRECATE <- function(
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
