#' lit_get_lead_gen_source
#' @description queries litify for lead gen source data
#' @export
lit_get_lead_gen_source <- function(){

  work::start(lib_sales_force = TRUE)


  sources <-
    lit_get_data(
      from_object = "litify_pm__Matter__c",
      select_object = c(Id, Needles_CaseID__c,
                        CreatedDate, Practice_Area__c,
                        litify_pm__Source__r.Name, litify_pm__Source__r.Id,
                        Marketing_Details__c, litify_pm__Primary_Intake__c,

                        Lead_Case__c,
                        litify_pm__Companion__r.Lead_Case__c,

                        Display_Name2__c, litify_pm__Status__c, litify_pm__Closed_Reason__c
      ),
      from_object_child = "litify_pm__Intakes__r",
      select_object_child = c(Id, litify_pm__Source__r.Name, litify_pm__Source__r.Id, Marketing_Note__c),
      chunks = 700,
      parallel_process = FALSE
    ) %>%
    filter(
      Display_Name2__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) |
        litify_pm__Status__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) |
        litify_pm__Closed_Reason__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) |
        litify_pm__Closed_Reason__c %>% stringr::str_detect(stringr::coll("duplicate", ignore_case = TRUE), negate = TRUE)
    ) %>%
    rename_col(
      .select = T,
      id_matter = Id,
      id_intake = litify_pm__Intake__c.Id,
      case = Needles_CaseID__c,
      id_primary_intake = litify_pm__Primary_Intake__c,
      date_created = CreatedDate,
      practice_area = Practice_Area__c,
      matter_details = Marketing_Details__c,
      matter_source = litify_pm__Source__r.Name,
      intake_source = litify_pm__Intake__c.litify_pm__Source__r.Name,
      intake_details = litify_pm__Intake__c.Marketing_Note__c,
      lead_case = Lead_Case__c,
      companion_lead_case = litify_pm__Companion__r.Lead_Case__c
    ) %>%
    add_count(id_matter) %>%
    filter(
      not(
        n > 1 & (id_intake != id_primary_intake)
      )
    ) %>%
    select(-n) %>%
    mutate(
      date_created = date_created %>% as.Date()
    )



  missing_intake_ids <- sources %>%
    filter(
      is.na(id_intake) & !is.na(id_primary_intake)
    ) %>%
    select(id_primary_intake) %>%
    unlist() %>%
    unique()



  missing_intakes <-
    lit_get_data(
      from_object = "litify_pm__Intake__c",
      select_object = c(Id, litify_pm__Source__r.Name, Marketing_Note__c),
      cases = missing_intake_ids,
      cases_field = "Id",
      chunks = 700,
      parallel_process = FALSE
    ) %>%
    rename_col(
      .select = T,
      id_primary_intake = Id,
      intake_source = litify_pm__Source__r.Name,
      intake_details = Marketing_Note__c
    )



  sources[is.na(sources$id_intake) & !is.na(sources$id_primary_intake), ]  <-
    left_join(
      sources[is.na(sources$id_intake) & !is.na(sources$id_primary_intake), ],
      missing_intakes,
      by = "id_primary_intake",
      relationship = "many-to-one"
    ) %>%
    select(-intake_source.x, -intake_details.x) %>%
    rename(
      intake_source = intake_source.y,
      intake_details = intake_details.y
    ) %>%
    select(all_of(names(sources)))



  df_find_lead_cases <-
    lit_get_data(
      from_object = "litify_pm__Matter__c",
      select_object = c(Id),
      from_object_child = "Companions__r",
      select_object_child = c(Lead_Case__c),
      cases = sources %>%
        filter(
          lead_case == "No" & is.na(companion_lead_case)
        ) %>%
        select(id_matter),
      cases_field = "Id",
      chunks = 700,
      parallel_process = FALSE
    ) %>%
    rename_col(
      id_matter = Id,
      companion_lead_case = litify_pm__Companion__c.Lead_Case__c
    )



  sources_lead <- sources %>% filter(lead_case == "Yes" | is.na(lead_case))



  sources_child <-
    sources %>%
    filter(lead_case == "No") %>%
    left_join(
      df_find_lead_cases,
      by = "id_matter"
    ) %>%
    mutate(
      companion_lead_case.x = ifelse(is.na(companion_lead_case.x), companion_lead_case.y, companion_lead_case.x)
    ) %>%
    select(-companion_lead_case.y) %>%
    rename(
      companion_lead_case = companion_lead_case.x
    ) %>%
    left_join(
      sources_lead %>% select(id_matter, id_primary_intake, matter_details, matter_source, intake_source, intake_details),
      by = join_by(companion_lead_case == id_matter)
    ) %>%
    mutate(

      id_primary_intake.x = ifelse(
        id_primary_intake.x != id_primary_intake.y & practice_area == "PI",
        id_primary_intake.y, id_primary_intake.x
      ),

      intake_source.x = ifelse(
        id_primary_intake.x != id_primary_intake.y & practice_area == "PI",
        intake_source.y, intake_source.x
      ),

      intake_details.x = ifelse(
        id_primary_intake.x != id_primary_intake.y & practice_area == "PI",
        intake_details.y, intake_details.x
      ),

      matter_source.x = ifelse(
        practice_area == "PI" & (!is.na(matter_source.y) | !is.na(matter_details.y)),
        matter_source.y, matter_source.x
      ),

      matter_details.x = ifelse(
        practice_area == "PI" & (!is.na(matter_source.y) | !is.na(matter_details.y)),
        matter_details.y, matter_details.x
      )
    ) %>%
    select(
      -ends_with(".y")
    ) %>%
    setNames(names(sources))



  sources <-
    bind_rows(sources_lead, sources_child) %>%
    mutate(
      source = ifelse(is.na(intake_source), matter_source, intake_source),
      source_original = source,
      source = source %>% work::str_scrub(keep="-"),
      details = ifelse(is.na(intake_details), matter_details, intake_details),
      details_original = details,
      details = details %>% work::str_scrub(keep="-")
    )



  df_translate_lead_gen_details <- work::df_translate_lead_gen %>%
    filter(!is.na(details)) %>%
    select(lead_gen_source, sub_lead_gen, source, details, match)



  df_translate_lead_gen_source <- work::df_translate_lead_gen %>%
    filter(is.na(details)) %>%
    select(lead_gen_source, sub_lead_gen, source, match)



  df_source_and_details <- fuzzyjoin::fuzzy_left_join(
    sources,
    df_translate_lead_gen_details,
    by = c(source, details),
    match_fun = list(equals, stringr::str_detect)
  ) %>%
    group_by(id_matter) %>%
    filter(
      row_number() == 1
    ) %>%
    ungroup() %>%
    select(-source.y, -details.y) %>%
    rename(
      source = source.x,
      details = details.x
    )



  df_source <- df_source_and_details %>%
    filter(
      is.na(match)
    ) %>%
    select(
      all_of(names(sources))
    ) %>%
    left_join(
      df_translate_lead_gen_source,
      by = "source",
      relationship = "many-to-one"
    ) %>% bind_rows(
      df_source_and_details %>% filter(match == 1)
    ) %>%
    mutate(
      lead_gen_source = ifelse(is.na(lead_gen_source) & is.na(match), "Unknown - Historic Intakes", lead_gen_source)
    )


  return(df_source)
}

