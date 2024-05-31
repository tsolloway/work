#' lit_get_matter_team_DEPRECATE
#' @description queries litify for matter team data
#' @param cases vector of case numbers
#' @export
lit_get_matter_team_DEPRECATE <- function(cases = NULL, limit = NULL, add_link = FALSE){


  work::start(TRUE)


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

          x <- base::c(
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


  df <- work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      "Id", "Needles_CaseID__c", "Case_Manager__r.Name",
      "litify_pm__Case_Type__r.Name",
      "Assigned_Attorney__r.Name", "Litigation_Attorney__r.Name",
      "litify_pm__Principal_Attorney__r.Name"),
    from_object_child = "litify_pm__Matter_Teams__r",
    select_object_child = c("Team_Member__c", "Role_Name__c"),
    cases = cases,
    limit = limit,
    chunks = 700
  ) %>%
    work::rename_col(
      .select = TRUE,
      id_matter = Id,
      case = Needles_CaseID__c,
      case_type = litify_pm__Case_Type__r.Name,
      matter_case_manager = Case_Manager__r.Name,
      matter_pre_lit_attorney = Assigned_Attorney__r.Name,
      matter_litigation_attorney = Litigation_Attorney__r.Name,
      matter_principal_attorney = litify_pm__Principal_Attorney__r.Name,
      role = litify_pm__Matter_Team_Member__c.Role_Name__c,
      name = litify_pm__Matter_Team_Member__c.Team_Member__c
    ) %>%
    filter(
      role == "Case Manager" |
        role == "Pre-Lit Attorney" |
        role == "Litigation Attorney" |
        role == "Principal Attorney" |
        role == "Negotiator"
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
    output[[i]] <- df[, names(df)[!data.table::like(names(df), "matter")]] %>% clean_role_type(i)
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


  if( add_link ) output[["link_matter"]] <- output[["id_matter"]] %>% work::lit_add_id_link()


  output
}

