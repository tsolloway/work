#' lit_get_intake
#' @description queries litify for intake data
#' @export
lit_get_intake <- function(
    cases = NULL, limit = NULL,
    cases_field = c("case", "id_intake", "id_matter", NULL),
    question_filter = c('Type of Accident.', 'Type of Accident')
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



  intake <- work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c("Id", "Needles_CaseID__c"),
    from_object_child = "litify_pm__Intakes__r",
    select_object_child = c("Id", "Commercial__c", "Severity__c", "Source_Name__c", "litify_pm__Source_Type__c", "litify_pm__Case_Type__r.Name"),
    cases = cases,
    cases_field = cases_field,
    limit = limit,
    predetermined_names = c("id_matter", "case", "commercial", "id_intake", "case_type", "severity", "source"),
    sort_predetermined_names = c("id_intake", "id_matter", "case", "commercial", "case_type", "severity", "source")
  )


  intake <- intake %>% mutate(
    commercial = commercial %>% work::str_scrub() %>% recode("commercial" = TRUE, "non_commercial" = FALSE, .default = NA)
  )


  if( !is.null(question_filter) | !is.null(question_filter) & length(question_filter) > 0 & length(intake[["id_intake"]]) > 0 ){

    questionnaire <- work::lit_get_intake_questionnaire(
      cases = intake[["id_intake"]],
      cases_field = "id_intake",
      question_filter = question_filter,
      clean_cols = FALSE
    )

    if( ncol(questionnaire) > 2 ){
      intake <- dplyr::left_join(
        intake,
        dplyr::select(questionnaire, -id_matter),
        by = dplyr::join_by(id_intake))
    }
  }


  return(intake)
}
