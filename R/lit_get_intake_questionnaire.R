#' lit_get_intake_questionnaire
#' @description queries litify for intake questionnaire
#' @export
lit_get_intake_questionnaire <- function(
    cases = NULL, limit = NULL,
    cases_field = c(NULL, "case", "id_intake", "id_matter"),
    question_filter = NULL,
    clean_cols = TRUE,
    chunks = 700
){

  cases_field <- match.arg(cases_field)

  if( !is.null(cases_field) ){

    cases_field <- switch(
      cases_field,
      "case" = "litify_pm__Matter__r.Needles_CaseID__c",
      "id_intake" = "Id",
      "id_matter" = "litify_pm__Matter__r.Id"
    )

  }



  if( !is.null(question_filter) ){
    additional_where_child_constant <- where_cases_equal(
      question_filter,
      "litify_pm__Question__r.litify_pm__Question_Label__c"
    )
  }else if( is.null(question_filter) ){
    additional_where_child_constant <- NULL
  }



  questionnaire <- work::lit_get_data(
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
    limit = limit,
    predetermined_names = c("id_intake", "id_matter", "case", "answer", "question"),
    sort_predetermined_names = c("id_intake", "id_matter", "case", "question", "answer"),
    additional_where_child_constant = additional_where_child_constant,
    chunks = chunks
  ) %>%
    dplyr::filter(!is.na(answer)) %>%
    dplyr::distinct()


  dedupe <- questionnaire %>%
    dplyr::summarise(n = dplyr::n(), .by = c(id_intake, question)) %>%
    dplyr::filter(n > 1L)



  if( nrow(dedupe) > 0 ){
    for(i in seq(nrow(dedupe)) ){
      dedupe[["id_intake"]][i]
      dedupe[["question"]][i]

      new_response <- questionnaire[
        questionnaire[["id_intake"]] == dedupe[["id_intake"]][i] &
          questionnaire[["question"]] == dedupe[["question"]][i],
        "answer"
      ] %>% unlist() %>% unique()

      if( length(new_response) > 1 ) new_response <- new_response %>% paste0(collapse = " ***||*** ")

      questionnaire[
        questionnaire[["id_intake"]] == dedupe[["id_intake"]][i] &
          questionnaire[["question"]] == dedupe[["question"]][i],
        "answer"
      ] <- new_response
    };rm(i, new_response)
  }



  questionnaire <- questionnaire %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from  = "question", values_from = "answer", id_cols = c("id_intake", "id_matter")) %>%
    work::names_clean()


  if( clean_cols ){
    questionnaire <- questionnaire %>% mutate(
      incident_date = incident_date %>% as.Date(),
      phone_number = phone_number %>% dialr::phone("US", show_progress = FALSE) %>% format()
    )

  }

  # cols <- c("did_you_have_passengers", "have_you_gone_to_the_hospital",
  #   "was_anyone_transported_to_hospital_by_ambulance", "did_the_police_come_to_the_scene", "was_a_police_report_made",
  #   "was_your_vehicle_drivable_from_the_scene", "did_the_airbags_deploy", "have_you_spoken_with_the_defendants_insurance",
  #   "were_you_working_during_the_time_of_the_accident", "were_you_transported_by_ambulance", "have_you_signed_a_waiver",
  #   "do_you_have_photos_of_the_dog_and_or_injuries_please_obtain")


  if( !is.null(questionnaire[["case_type"]]) ){

    case_types_to_fix <- questionnaire[["case_type"]][grepl("***||***", questionnaire[["case_type"]], fixed = TRUE)]

    if( length(case_types_to_fix) == 0 ){
      case_types <- questionnaire[["case_type"]] %>% work::lit_get_case_type("id_case_type")

    }else if( length(case_types_to_fix) > 0 ){
      case_types <- questionnaire[["case_type"]][!grepl("***||***", questionnaire[["case_type"]], fixed = T)] %>% work::lit_get_case_type("id_case_type")

      case_types <- questionnaire[["case_type"]][!grepl("***||***", questionnaire[["case_type"]], fixed = T)] %>% work::lit_get_case_type("id_case_type")

      case_types_to_fix_return <- case_types_to_fix %>% strsplit("***||***", fixed = TRUE) %>% unlist() %>% trimws() %>% work::lit_get_case_type("id_case_type")

      case_types_fixed <- case_types_to_fix
      for(i in seq(nrow(case_types_to_fix_return))){
        case_types_fixed <- gsub(
          case_types_to_fix_return[["id_case_type"]][i],
          case_types_to_fix_return[["case_type"]][i],
          case_types_fixed)
      }; rm(i)

      for( i in seq(length(case_types_fixed)) ){
        new <- case_types_fixed[i] %>% strsplit("***||***", fixed = TRUE) %>% unlist() %>% unique()
        if( length(new) == 1){
          case_types_fixed[i] <- new
        }
      };rm(i, new)


      case_types <- bind_rows(
        case_types,
        tibble(
          id_case_type = case_types_to_fix,
          case_type = case_types_fixed
        )
      )

    }

    questionnaire[["case_type"]] <- case_types[["case_type"]][ match(questionnaire[["case_type"]], case_types[["id_case_type"]]) ]
  }


  return(questionnaire)
}

