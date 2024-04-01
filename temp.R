


work::restart()

work::start(lib_sales_force = TRUE, lib_dev = T, lib_future = T)
cases = NULL
parallel_process = TRUE

if( parallel_process ){
  start(lib_future = TRUE)

  old_plan <- plan(multisession)

  on.exit(plan(old_plan), add = TRUE)
}


options(future.rng.onMisuse = "ignore")


######################

dedupe <- function(x){

  duplicate_dates <-  x[["date_created"]] %>%
    duplicated() %>%
    x[["date_created"]][.] %>%
    unique()

  if( length(duplicate_dates) > 0 ){

    for(i in duplicate_dates){

      duplicate_date_rows <- x[x[["date_created"]] == i, ]

      duplicate_dates_fields <- duplicate_date_rows[["field"]] %>%
        duplicated() %>%
        duplicate_date_rows[["field"]][.] %>%
        unique()


      if( length(duplicate_dates_fields) > 0 ){


        for(fld in duplicate_dates_fields){

          duplicate_date_fields_rows <- x[x[["date_created"]] == i & x[["field"]] == fld, ]

          duplicate_date_fields_rows <- duplicate_date_fields_rows %>%
            mutate(
              value = ifelse(value == "Missing", NA, value),
              value_cat = ifelse(value_cat == "Missing", NA, value_cat),
            )

          duplicate_date_fields_rows[['value']] <- duplicate_date_fields_rows[['value']] %>% tail(1)

          temp_value_cat <- duplicate_date_fields_rows[['value_cat']] %>% unique()

          if(length(temp_value_cat) == 1){
            duplicate_date_fields_rows[['value_cat']] <- temp_value_cat
          }else if(length(temp_value_cat) > 1){
            duplicate_date_fields_rows[['value_cat']] <- temp_value_cat %>% paste0(collapse = "|")
          }

          x[x[["date_created"]] == i & x[["field"]] == fld, ] <- duplicate_date_fields_rows

          x <- x %>% distinct()
        }
      }
    }
  }
  x
}



matter %<-% {
    df <- lit_get_data(
      from_object = "litify_pm__Matter__c",

      select_object = c(
        Id, Needles_CaseID__c,

        litify_pm__Case_Type__r.Name, Practice_Area__c, Case_Severity__c,

        Litigation_At__c,

        Lead_Case__c, litify_pm__Companion__r.Id, litify_pm__Companion__r.Lead_Case__c,

        Case_Manager__r.Name, Assigned_Attorney__r.Name, Litigation_Attorney__r.Name, litify_pm__Principal_Attorney__r.Name,

      ),
      cases = cases
    ) %>% rename_col(

      .select = TRUE,
      .distinct = TRUE,

      id_matter = Id,
      case = Needles_CaseID__c,

      practice_area = Practice_Area__c,
      case_type = litify_pm__Case_Type__r.Name,
      case_severity = Case_Severity__c,

      lead_case = Lead_Case__c,
      id_companion = litify_pm__Companion__r.Id,
      companion_lead_case = litify_pm__Companion__r.Lead_Case__c,

      case_manager = Case_Manager__r.Name,
      pre_lit_attorney = Assigned_Attorney__r.Name,
      litigation_attorney = Litigation_Attorney__r.Name,
      principal_attorney = litify_pm__Principal_Attorney__r.Name
    )


    for (i in c(case_manager, pre_lit_attorney, litigation_attorney, principal_attorney)){

      df[[i]] <- df[[i]] %>% case_match(
        c('System Automation', 'Litify Services', 'Wilshire Law Firm', 'Jesse Test2', 'Natalie Yunus Automation User (FE)') ~ NA,
        .default = df[[i]]
      )

      df <- df %>% distinct()
    }


    return(df)
  }



matter_team %<-% {
  df <- lit_get_data(
    from_object = "litify_pm__Matter_Team_Member__c",
    select_object = c(litify_pm__Matter__c, CreatedDate, litify_pm__User__r.Name, litify_pm__Role__r.Name)
  ) %>%
    filter(

      !is.na(CreatedDate) &

        litify_pm__Role__r.Name %in% c(
          'Attorney', 'Associate Attorney',
          '2nd Associate Attorney', 'Junior Associate Attorney',
          'Negotiator', 'Case Manager',
          'Litigation', 'Litigation Attorney', 'Managing Attorney',
          'Pre-Lit Attorney', 'Principal Attorney',
          'Secondary Litigation Attorney',
          'Senior Trial Attorney'
        )
    ) %>%
    filter(
      !is.na(CreatedDate) &
        !is.na(litify_pm__Matter__c)
    ) %>%
    rename_col(
      .distinct = TRUE,
      .select = TRUE,
      id_matter = litify_pm__Matter__c,
      date_created = CreatedDate,
      field = litify_pm__Role__r.Name,
      value = litify_pm__User__r.Name
    ) %>%
    mutate(
      date_created = date_created %>% as.POSIXct(format="%Y-%m-%dT%H:%M:%OS"),
      value = value %>% case_match(
        c(
          'System Automation', 'Litify Services', 'Wilshire Law Firm',
          'Jesse Test2', 'Natalie Yunus Automation User (FE)'
        ) ~ NA,
        .default = value
      ),
      value = ifelse(value == "Missing", NA, value),
      value_cat = value
    ) %>%
    distinct() %>%
    arrange(date_created) %>%
    group_split(id_matter)



  df <- future_lapply(df, dedupe, future.seed = NULL) %>%
    bind_rows() %>%
    tidyr::pivot_wider(
      names_from = 'field',
      values_from = c("value", "value_cat"),
      id_cols = c("id_matter", 'date_created')
    ) %>%
    arrange(id_matter, date_created)


  names(df) <- names(df) %>% str_scrub() %>% gsub("value_", "", .)


  df <- df %>%
    mutate(
      litigation_attorney = ifelse(is.na(litigation_attorney), litigation, litigation_attorney),
      cat_litigation_attorney = ifelse(is.na(cat_litigation_attorney), cat_litigation, cat_litigation_attorney)
    ) %>%
    select(-litigation, -cat_litigation)

  return(df)
}



matter_history %<-% {
  df <- lit_get_matter_history() %>%
    rename_col(
      .distinct = TRUE,
      .select = TRUE,
      id_matter = ParentId,
      date_created = CreatedDate,
      field = Field,
      value = NewValue
    ) %>%
    filter(
      !is.na(date_created)
    ) %>%
    mutate(
      date_created = date_created %>% as.POSIXct(format="%Y-%m-%dT%H:%M:%OS"),
      value = value %>% case_match(
        c(
          'System Automation', 'Litify Services', 'Wilshire Law Firm',
          'Jesse Test2', 'Natalie Yunus Automation User (FE)'
        ) ~ NA,
        .default = value
      ),
      # value = ifelse(value == "Missing", NA, value),
      value_cat = value
    ) %>%
    distinct() %>%
    arrange(date_created) %>%
    group_split(id_matter)


  df <- future_lapply(df, dedupe, future.seed = NULL) %>%
    bind_rows() %>%
    tidyr::pivot_wider(
      names_from = 'field',
      values_from = c("value", "value_cat"),
      id_cols = c("id_matter", 'date_created')
    ) %>%
    rename_col(
      .select = TRUE,
      .distinct = TRUE,

      id_matter = id_matter,
      date_created = date_created,

      case_manager = value_Case_Manager__c,
      pre_lit_attorney = value_Assigned_Attorney__c,
      principal_attorney = value_litify_pm__Principal_Attorney__c,
      team_member_ids = value_Matter_Team_Member_Ids__c,

      source = value_litify_pm__Source__c,
      case_type = value_litify_pm__Case_Type__c,
      matter_stage = value_litify_pm__Matter_Stage_Activity__c,
      status = value_litify_pm__Status__c,
      closed_reason = value_litify_pm__Closed_Reason__c,

      cat_case_manager = value_cat_Case_Manager__c,
      cat_pre_lit_attorney = value_cat_Assigned_Attorney__c,
      cat_principal_attorney = value_cat_litify_pm__Principal_Attorney__c,
      cat_team_member_ids = value_cat_Matter_Team_Member_Ids__c,

      cat_source = value_cat_litify_pm__Source__c,
      cat_case_type = value_cat_litify_pm__Case_Type__c,
      cat_matter_stage = value_cat_litify_pm__Matter_Stage_Activity__c,
      cat_status = value_cat_litify_pm__Status__c,
      cat_closed_reason = value_cat_litify_pm__Closed_Reason__c
    ) %>%
    arrange(id_matter, date_created)


  return(df)
}




matter_historical %>% names


matter_historical <- full_join(
  matter_history,
  matter_team,
  by = join_by(
    id_matter == id_matter,
    date_created == date_created
  )
) %>%
  mutate(
    case_manager.x = ifelse(is.na(case_manager.x), case_manager.y, case_manager.x),
    case_manager.x = ifelse(case_manager.x != case_manager.y, glue("{case_manager.x}|{case_manager.y}"), case_manager.x),

    cat_case_manager.x = ifelse(is.na(cat_case_manager.x), cat_case_manager.y, cat_case_manager.x),
    cat_case_manager.x = ifelse(cat_case_manager.x != cat_case_manager.y, glue("{cat_case_manager.x}|{cat_case_manager.y}"), cat_case_manager.x),

    pre_lit_attorney.x = ifelse(is.na(pre_lit_attorney.x), pre_lit_attorney.y, pre_lit_attorney.x),
    pre_lit_attorney.x = ifelse(pre_lit_attorney.x != pre_lit_attorney.y, glue("{pre_lit_attorney.x}|{pre_lit_attorney.y}"), pre_lit_attorney.x),

    cat_pre_lit_attorney.x = ifelse(is.na(cat_pre_lit_attorney.x), cat_pre_lit_attorney.y, cat_pre_lit_attorney.x),
    cat_pre_lit_attorney.x = ifelse(cat_pre_lit_attorney.x != cat_pre_lit_attorney.y, glue("{cat_pre_lit_attorney.x}|{cat_pre_lit_attorney.y}"), cat_pre_lit_attorney.x),
  ) %>%
  rename(
    case_manager = case_manager.x,
    cat_case_manager = cat_case_manager.x,
    pre_lit_attorney = pre_lit_attorney.x,
    cat_pre_lit_attorney = cat_pre_lit_attorney.x
  ) %>%
  select(
    -case_manager.y,
    -cat_case_manager.y,
    -pre_lit_attorney.y,
    -cat_pre_lit_attorney.y
  ) %>%
  mutate(
    date_created = date_created %>% as.Date(),
  ) %>%
  group_by(id_matter, date_created) %>%
  summarise_all(last) %>%
  group_by(id_matter) %>%
  tidyr::fill(names(.), .direction = "down") %>%
  ungroup() %>%
  mutate_if(is.character, function(x)ifelse(x == "Missing", NA, x))




matter


matter_team %>%
  arrange(date_created) %>%
  group_by(id_matter, date_created) %>%
  summarise_all(last) %>%
  ungroup() %>%
















matter %>% names





names(temp)


group_by(id_matter) %>%
  tidyr::fill(names(.), .direction = "down") %>%
  ungroup()


  mutate(
    date_created = date_created %>% as.Date(),
  ) %>%
  group_by(id_matter, date_created) %>%
  summarise_all(last) %>%
  ungroup()



temp %>% names

temp %>% filter(
  cat_case_manager.x != cat_case_manager.y
) %>% select(id_matter, date_created, case_manager.x, case_manager.y, cat_case_manager.x, cat_case_manager.y) %>% View


temp %>% filter(
  cat_pre_lit_attorney.x != cat_pre_lit_attorney.y
) %>% select(id_matter, date_created, pre_lit_attorney.x, pre_lit_attorney.y, cat_pre_lit_attorney.x, cat_pre_lit_attorney.y) %>% View




matter %>% names

matter_history[[1]]$id_matter

a0L5G00000Ccp4gUAB

matter_history[[1]] %>% tidyr::fill(names(.), .direction = "down")

matter_history %>% bind_rows() %>% group_by(id_matter) %>% tidyr::fill(names(.), .direction = "down")


matter_team %>% names











matter_team %>%
  tidyr::pivot_wider(
    names_from = 'field',
    values_from = "value",
    id_cols = c("id_matter", 'date_created')
  )


{matter_team} |>
  dplyr::summarise(n = dplyr::n(), .by = c(id_matter, date_created, role)) |>
  dplyr::filter(n > 1L)




matter_history <- matter_history %>% arrange((date_created))

temp <- matter_history %>%
  arrange(date_created) %>%
  group_split(id_matter) %>%
  lapply(
    function(x){

      duplicate_dates <-  x[["date_created"]] %>%
        duplicated() %>%
        x[["date_created"]][.] %>%
        unique()

      if( length(duplicate_dates) > 0 ){

        for(i in duplicate_dates){

          duplicate_date_rows <- x[x[["date_created"]] == i, ]

          duplicate_dates_roles <- duplicate_date_rows[["role"]] %>%
            duplicated() %>%
            duplicate_date_rows[["role"]][.] %>%
            unique()


          if( length(duplicate_dates_roles) > 0 ){


            for(r in duplicate_dates_roles){

              duplicate_date_role_rows <- x[x[["date_created"]] == i & x[["role"]] == r, ]

              duplicate_date_role_rows_names <- duplicate_date_role_rows[['name']] %>%
                unique() %>% work::remove_na()

              if( length(duplicate_date_role_rows_names) == 1 ){
                duplicate_date_role_rows[['name']] <- duplicate_date_role_rows_names
              }else if( length(duplicate_date_role_rows_names) > 1 ){
                duplicate_date_role_rows[['name']] <- duplicate_date_role_rows_names %>% paste0(collapse = "|")
              }

              x[x[["date_created"]] == i & x[["role"]] == r, ] <- duplicate_date_role_rows

              x <- x %>% distinct()
            }
          }
        }
      }
      x
    }
  ) %>%
  bind_rows() %>%
  tidyr::pivot_wider(
  names_from = 'field',
  values_from = "value",
  id_cols = c("id_matter", 'date_created')
)

{matter_history} |>
  dplyr::summarise(n = dplyr::n(), .by = c(id_matter, date_created, field)) |>
  dplyr::filter(n > 1L) %>% View


matter_history %>% value()


litify_pm__Status__c
litify_pm__Closed_Reason__c




matter

matter

matter %>% names


matter_history








matter_history <- matter_history %>% value







matter_team

matter_team_history$Field %>% table

matter_team_history %>% filter(Field == "Name")
toc()





litify_pm__Matter_Teams__r
Where Role_Name__c IN ('Attorney', 'Associate Attorney',
                       '2nd Associate Attorney', 'Junior Associate Attorney',
                       'Negotiator', 'Case Manager',
                       'Litigation', 'Litigation Attorney', 'Managing Attorney',
                       'Pre-Lit Attorney', 'Principal Attorney',
                       'Secondary Litigation Attorney',
                       'Senior Trial Attorney')"







date_created, role, name
