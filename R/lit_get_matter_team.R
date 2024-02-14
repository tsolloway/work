#' lit_get_matter_team
#' @description queries litify for matter team data
#' @param cases vector of case numbers
#' @export
lit_get_matter_team <- function(cases = NULL, limit = NULL){

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

          y <- x[!is.na(x)] %>% paste0(collapse = "|")

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
    select_object = c("Id", "Needles_CaseID__c"),
    from_object_child = "litify_pm__Matter_Teams__r",
    select_object_child = c("Team_Member__c", "Role_Name__c"),
    cases = cases,
    limit = NULL,
    predetermined_names = c("id_matter", "case", "role", "name"),
    sort_predetermined_names = NULL,
    filter_syntax = 'role == "Case Manager" | role == "Pre-Lit Attorney" | role == "Litigation Attorney" | role == "Principal Attorney" | role == "Negotiator"',
    apply_translate_limits = FALSE,
    add_links = NULL
  )

  if( nrow(df) == 0 ){
    df <- work::lit_get_data(
      from_object = "litify_pm__Matter__c",
      select_object = "Id",
      predetermined_names = "id_matter",
      cases = cases,
      add_links = FALSE
    )
  }else{

    df[["role"]] <- df[["role"]] %>% work::str_scrub()


    df <- df %>% dplyr::group_split(id_matter) %>%
      purrr::map(function(x){
        x[["role"]] <- x[["role"]] %>% paste0("_1") %>% make.unique(sep="_") %>%
          gsub("1_1", "2", .) %>% gsub("1_2", "3", .)  %>% gsub("1_3", "4", .) %>%
          gsub("1_4", "5", .) %>% gsub("1_5", "6", .)  %>% gsub("1_6", "7", .)
        x
      }) %>%
      dplyr::bind_rows() %>%
      tidyr::pivot_wider(names_from = 'role', values_from = 'name', id_cols = 'id_matter')


    df <- df %>% dplyr::select( names(df)[-1] %>% sort() %>% c(names(df)[1], .) )

  }

  output <- df[, "id_matter"]

  for( i in c("case_manager", "pre_lit_attorney", "litigation_attorney", "principal_attorney", "negotiator") ){
    output[[i]] <- df %>% clean_role_type(i)
  }

  output[["attorney"]] <- output %>% clean_role_type("attorney")

  for( i in c("case_manager", "pre_lit_attorney", "litigation_attorney", "principal_attorney", "attorney", "negotiator") ){
    output[[paste0(i, "_count")]] <- (stringr::str_count(output[[i]], stringr::fixed("|")) + 1) %>% work::if_na_return(0)
  }


  output[["link_matter"]] <- paste0("https://wilshirelawfirm.lightning.force.com/lightning/r/", df[["id_matter"]], "/view")

  output
}

