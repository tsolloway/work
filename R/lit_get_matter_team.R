#' lit_get_matter_team
#' @description queries litify for matter team data
#' @param cases vector of case numbers
#' @export
lit_get_matter_team <- function(
    cases = NULL,
    cases_field = "Needles_CaseID__c",
    add_links = FALSE,
    parallel_process = TRUE
){


  work::start(TRUE)


  if( parallel_process ){
    start(lib_future = TRUE)

    old_plan <- plan(multisession)

    on.exit(plan(old_plan), add = TRUE)
  }



  clean_role_type <- function(df, type){

    temp <- df %>% dplyr::select( names(df)[ df %>% names() %>% data.table::like(type) ] )

    temp[["na_count"]] <- apply(temp, 1, function(x) sum(!is.na(x)))

    temp[["target"]] <- NA

    temp[temp[["na_count"]] == 1, "target"] <- temp[temp[["na_count"]]==1, 1:(ncol(temp)-2)] %>% apply(1, function(x)x[!is.na(x)])

    temp[temp[["na_count"]] > 1, "target"] <- temp[temp[["na_count"]] > 1, ] %>% apply(1, function(x){

      x <- x %>% unlist()

      x <- head(x, length(x)-2)

      if( all(is.na(x)) ){

        return(NA)

      }else{

        x <- x[!is.na(x)]

        y <- x %>% unique()

        if( length(y) == 1 ){

          return(y)

        }else if(type == "attorney"){

          x <- c(
            tryCatch(x[["principal_attorney"]], error = function(e)NA),
            tryCatch(x[["litigation_attorney"]], error = function(e)NA),
            tryCatch(x[["pre_lit_attorney"]], error = function(e)NA)
          )

          y <- x[!is.na(x)] %>% unique() %>% paste0(collapse = "|")

        }else if( any(grepl("\\(fe\\)", y, ignore.case = TRUE)) ){

          y <- c(y[-grep("\\(fe\\)", y, ignore.case = TRUE)], x[grep("\\(fe\\)", x, ignore.case=T)])

          y <- y %>% paste0(collapse = "|")

        }else if( length(y) > 1 ){

          y <- y %>% rev() %>% paste0(collapse = "|")
        }
      }

      y
    })

    temp[["target"]]
  }



  matter <- lit_get_data(
    from_object = "litify_pm__Matter__c",

    select_object = c(
      Id, Needles_CaseID__c,
      litify_pm__Case_Type__r.Name,
      Practice_Area__c, Litigation_At__c, Lead_Case__c,
      litify_pm__Companion__r.Id, litify_pm__Companion__r.Lead_Case__c,
      Case_Manager__r.Name, Assigned_Attorney__r.Name,
      Litigation_Attorney__r.Name, litify_pm__Principal_Attorney__r.Name
    ),

    from_object_child = "litify_pm__Matter_Teams__r
    Where Role_Name__c IN ('Attorney', 'Associate Attorney',
  '2nd Associate Attorney', 'Junior Associate Attorney',
  'Negotiator', 'Case Manager',
  'Litigation', 'Litigation Attorney', 'Managing Attorney',
  'Pre-Lit Attorney', 'Principal Attorney',
  'Secondary Litigation Attorney',
  'Senior Trial Attorney')",

  select_object_child = c("CreatedDate", "Team_Member__c", "Role_Name__c"),
  cases = cases,
  cases_field = cases_field,
  chunks = 700,
  parallel_process = parallel_process
  ) %>%
    work::rename_col(
      id_matter = Id,
      case = Needles_CaseID__c,

      lead_case = Lead_Case__c,
      id_companion = litify_pm__Companion__r.Id,
      companion_lead_case = litify_pm__Companion__r.Lead_Case__c,

      practice_area = Practice_Area__c,
      case_type = litify_pm__Case_Type__r.Name,

      matter_case_manager = Case_Manager__r.Name,
      matter_pre_lit_attorney = Assigned_Attorney__r.Name,
      matter_litigation_attorney = Litigation_Attorney__r.Name,
      matter_principal_attorney = litify_pm__Principal_Attorney__r.Name,

      date_created = litify_pm__Matter_Team_Member__c.CreatedDate,
      role = litify_pm__Matter_Team_Member__c.Role_Name__c,
      name = litify_pm__Matter_Team_Member__c.Team_Member__c
    ) %>% dplyr::select(
      id_matter, case,
      lead_case, id_companion, companion_lead_case,
      practice_area, case_type,
      matter_case_manager, matter_pre_lit_attorney, matter_litigation_attorney, matter_principal_attorney,
      date_created, role, name
    ) %>%
    distinct()



  for (i in c(matter_case_manager, matter_pre_lit_attorney, matter_litigation_attorney, matter_principal_attorney, name)){

    df[[i]] <- df[[i]] %>% case_match(
      c('System Automation', 'Litify Services', 'Wilshire Law Firm', 'Jesse Test2', 'Natalie Yunus Automation User (FE)') ~ NA,
      .default = df[[i]]
    )

    df <- df %>% distinct()

  };rm(i)



  matter_current <- df %>% select(
    id_matter, case,
    lead_case, id_companion, companion_lead_case,
    practice_area, case_type,
    matter_case_manager, matter_pre_lit_attorney, matter_litigation_attorney, matter_principal_attorney
  ) %>%
    distinct()



  matter_team <- df %>% select(
    id_matter, case,
    date_created, role, name
  ) %>%
    filter(!is.na(date_created)) %>%
    distinct()



  matter_team_by_date <- matter_team %>%
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
    names_from = 'role',
    values_from = "name",
    id_cols = c("id_matter", "case", 'date_created')
  ) %>%
    setNames(., names(.) %>% work::str_scrub()) %>%
    group_split(id_matter)



  if( parallel_process ){

    matter_team_by_date <- matter_team_by_date %>%
      future_lapply(function(x){
        x_names <- x %>% work::other_col_names(id_matter, case, date_created)

        x %>%
          tidyr::fill(
            all_of(x_names),
            .direction = "down"
          )
      })
  }else if( !parallel_process ){

    matter_team_by_date <- matter_team_by_date %>%
      lapply(function(x){

        x_names <- x %>% work::other_col_names(id_matter, case, date_created)

        x %>%
          tidyr::fill(
            all_of(x_names),
            .direction = "down"
          )
      })
  }



  matter_team_by_date <- matter_team_by_date %>%
    bind_rows() %>%
    mutate(
      date_temp = date_created,
      date_created = date_created %>% as.Date(),
    ) %>%
    group_by(id_matter, date_created) %>%
    filter(date_temp == max(date_temp)) %>%
    select(-date_temp) %>%
    ungroup()


  # matter_team_by_date %>% write("desktop")


  # save.image("temp.rdata")

  # load("temp.rdata")


  matter_history <- work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      "Id", "Needles_CaseID__c",
      "(
      SELECT Field, CreatedDate, DataType, NewValue
      FROM Histories
      Where DataType = 'EntityId' AND Field IN ('Assigned_Attorney__c', 'Case_Manager__c', 'litify_pm__Principal_Attorney__c')
    )"
    ),
    cases = df %>% select(id_matter),
    cases_field = "Id",
    col_name_clean = FALSE,
    chunks = 750,
    parallel_process = parallel_process
  ) %>%
    work::rename_col(
      id_matter = Id,
      case = Needles_CaseID__c,

      date_created = litify_pm__Matter__History.CreatedDate,
      field = litify_pm__Matter__History.Field,
      new_value = litify_pm__Matter__History.NewValue,
    ) %>%
    select(-litify_pm__Matter__History.DataType) %>%
    filter(!is.na(date_created))



  users <- work::lit_get_data(
    from_object = "User",
    select_object = c(Id, Name),
    cases = matter_history %>% select(new_value),
    cases_field = "Id",
    col_name_clean = FALSE
  )



  matter_history <- left_join(
    matter_history, users,
    by = join_by(new_value == Id)
  ) %>%
    mutate(
      new_value = ifelse(is.na(Name), "Missing", Name)
    ) %>%
    select(-Name)


  matter_history_by_date <- matter_history %>%
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

            duplicate_dates_roles <- duplicate_date_rows[["field"]] %>%
              duplicated() %>%
              duplicate_date_rows[["field"]][.] %>%
              unique()


            if( length(duplicate_dates_roles) > 0 ){


              for(r in duplicate_dates_roles){

                duplicate_date_role_rows <- x[x[["date_created"]] == i & x[["field"]] == r, ]

                duplicate_date_role_rows[['new_value']] <- duplicate_date_role_rows[['new_value']] %>% tail(1)

                x[x[["date_created"]] == i & x[["field"]] == r, ] <- duplicate_date_role_rows

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
      values_from = "new_value",
      id_cols = c("id_matter", "case", 'date_created')
    ) %>%
    arrange(date_created) %>%
    work::rename_col(
      case_manager = Case_Manager__c,
      pre_lit_attorney = Assigned_Attorney__c,
      principal_attorney = litify_pm__Principal_Attorney__c
    ) %>%
    group_split(id_matter)



  if( parallel_process ){

    matter_history_by_date <- matter_history_by_date %>%
      future_lapply(function(x){
        x %>%
          tidyr::fill(
            case_manager, pre_lit_attorney, principal_attorney,
            .direction = "down"
          )
      })
  }else if( !parallel_process ){

    matter_history_by_date <- matter_history_by_date %>%
      lapply(function(x){
        x %>%
          tidyr::fill(
            case_manager, pre_lit_attorney, principal_attorney,
            .direction = "down"
          )
      })
  }



  matter_history_by_date <- matter_history_by_date %>%
    bind_rows() %>%
    mutate(
      case_manager = ifelse(case_manager == "Missing", NA, case_manager),
      pre_lit_attorney = ifelse(pre_lit_attorney == "Missing", NA, pre_lit_attorney),
      principal_attorney = ifelse(principal_attorney == "Missing", NA, principal_attorney),
      date_temp = date_created,
      date_created = date_created %>% as.Date(),
    ) %>%
    group_by(id_matter, date_created) %>%
    filter(date_temp == max(date_temp)) %>%
    select(-date_temp) %>%
    ungroup()


  temp <- full_join(
    matter_team_by_date,
    matter_history_by_date,
    by = join_by(
      id_matter, case, date_created
    ),
    suffix = c("", "_matter")
  ) %>%
    arrange(date_created)


  temp %>% filter(case_manager != case_manager_matter) %>%
    select(
      case, date_created,
      case_manager, pre_lit_attorney, principal_attorney,
      case_manager_matter, pre_lit_attorney_matter, principal_attorney_matter
    )















  if( nrow(df) == 0 ){

    # df <- df_matter %>% dplyr::select(-case_type)

  }else if( (ncol(df) == 2) && identical(names(df), c("id_matter", "case")) ){
    #do nothing
  }else{

    df <- df %>%
      mutate(
        role = role %>% work::str_scrub()
      ) %>%
      dplyr::group_split(id_matter, case) %>%
      purrr::map(function(x){
        x[["role"]] <- x[["role"]] %>% paste0("_1") %>% make.unique(sep="_") %>%
          gsub("1_1", "2", .) %>% gsub("1_2", "3", .)  %>% gsub("1_3", "4", .) %>%
          gsub("1_4", "5", .) %>% gsub("1_5", "6", .)  %>% gsub("1_6", "7", .)
        x
      }) %>%
      dplyr::bind_rows() %>%
      tidyr::pivot_wider(
        names_from = 'role',
        values_from = 'name',
        id_cols = c(
          'id_matter', 'case', "case_type", "matter_case_manager", "matter_pre_lit_attorney",
          "matter_litigation_attorney", "matter_principal_attorney"
        ))


    # work::other_col_names(
    #   df,
    #   work::c(
    #     id_matter, case, case_type, matter_case_manager, matter_pre_lit_attorney,
    #     matter_litigation_attorney, matter_principal_attorney
    #   )
    # )
    #
    # work::c(
    #   id_matter, case, case_type, matter_case_manager, matter_pre_lit_attorney,
    #   matter_litigation_attorney, matter_principal_attorney
    # ) %>% {
    #   x = work::other_col_names(df, !!.) %>% sort()
    #   base::c(., x)
    # }


    df <- df %>% dplyr::select(
      work::c(
        id_matter, case, case_type, matter_case_manager, matter_pre_lit_attorney,
        matter_litigation_attorney, matter_principal_attorney
      ) %>% {
        x = work::other_col_names(df, !!.) %>% sort()
        base::c(., x)
      }
    )

  }



  output <- df %>% select(id_matter, case, case_type)


  for( i in c("case_manager", "pre_lit_attorney", "litigation_attorney", "principal_attorney", "negotiator") ){

    select_cols <- names(df)[!data.table::like(names(df), "matter")]

    select_cols <- select_cols[!data.table::like(select_cols, "assistant")]

    output[[i]] <- df[, select_cols] %>% clean_role_type(i)
  }


  output <- dplyr::full_join(
    output,
    df %>% select(
      id_matter, case, case_type, matter_case_manager, matter_pre_lit_attorney,
      matter_litigation_attorney, matter_principal_attorney
    ),
    by = c("id_matter", "case", "case_type"))


  output <- output %>%
    rowwise() %>%
    mutate(
      case_manager = ifelse(is.na(case_manager), matter_case_manager, case_manager),
      pre_lit_attorney = ifelse(is.na(pre_lit_attorney), matter_pre_lit_attorney, pre_lit_attorney),
      litigation_attorney = ifelse(is.na(litigation_attorney), matter_litigation_attorney, litigation_attorney),
      principal_attorney = ifelse(is.na(principal_attorney), matter_principal_attorney, principal_attorney)
    ) %>%
    ungroup() %>%
    select(
      -matter_case_manager, -matter_pre_lit_attorney, -matter_litigation_attorney, -matter_principal_attorney
    )



  output <- output %>%
    mutate(
      attorney = output %>% clean_role_type("attorney")
    )


  for( i in c("case_manager", "pre_lit_attorney", "litigation_attorney", "principal_attorney", "attorney", "negotiator") ){
    output[[paste0(i, "_count")]] <- (stringr::str_count(output[[i]], stringr::fixed("|")) + 1) %>% work::if_na_return(0)
  }


  if( add_links ) output[["link_matter"]] <- output[["id_matter"]] %>% work::lit_add_id_link()


  output
}

