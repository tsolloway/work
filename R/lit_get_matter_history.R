#' lit_get_matter_history
#' @description queries litify for matter history
#' @param id_matter vector of matter ids
#' @export
lit_get_matter_history <- function(
    id_matter = NULL,
    parallel_process = TRUE
){

  work::start(lib_sales_force = TRUE)



  if( is.null(id_matter) ){
    cases_field <- NULL
  }else{
    cases_field <- "ParentId"
  }



  if( parallel_process ){
    start(lib_future = TRUE)

    old_plan <- plan(multisession)

    on.exit(plan(old_plan), add = TRUE)
  }



  get_id_names <- function(df, x, obj = NULL, cases = NULL, do_join = TRUE){

    if(is.null(obj)) obj <- x

    if(is.null(cases)){
      cases <- df %>%
        filter(Field %in% x) %>%
        select(NewValue)
    }

    temp <- lit_get_data(
      from_object = obj,
      select_object = c(Id, Name),

      cases = cases,

      cases_field = "Id",
      chunks = 700
    )


    if(do_join){
      temp <- left_join(
        df[ df[["Field"]] %in% x, ],
        temp,
        by = dplyr::join_by(NewValue == Id)
      ) %>%
        mutate(
          NewValue = Name
        ) %>%
        select(-Name)

      df[ df[["Field"]] %in% x, ] <- temp

      return(df)

    }else{
      return(temp)
    }
  }



  df <- lit_get_data(
    from_object = "litify_pm__Matter__History",
    select_object = c(ParentId, CreatedDate, DataType, Field, NewValue),
    cases = id_matter,
    cases_field = cases_field,
    additional_where_constant = "DataType = 'DynamicEnum' or DataType = 'Text' or DataType = 'EntityId'"
  ) %>%
    filter(
      (Field == "Assigned_Attorney__c" & DataType == "EntityId") |
        (Field == "Case_Manager__c" & DataType == "EntityId") |
        (Field == "litify_pm__Principal_Attorney__c" & DataType == "EntityId") |
        (Field == "Matter_Team_Member_Ids__c") |
        (Field == "litify_pm__Matter_Stage_Activity__c" & DataType == "EntityId") |
        (Field == "litify_pm__Status__c") |
        (Field == "litify_pm__Closed_Reason__c") |
        (Field == "litify_pm__Source__c" & DataType == "EntityId") |
        (Field == "litify_pm__Case_Type__c" & DataType == "EntityId")
    ) %>%

    get_id_names(c(Case_Manager__c, Assigned_Attorney__c, litify_pm__Principal_Attorney__c), "User") %>%
    get_id_names("litify_pm__Case_Type__c") %>%
    get_id_names("litify_pm__Matter_Stage_Activity__c") %>%
    get_id_names("litify_pm__Source__c") %>%

    mutate(
      CreatedDate = CreatedDate %>% as.POSIXct(format="%Y-%m-%dT%H:%M:%OS")
    )


  team_member_ids <- df %>%
    filter(Field == "Matter_Team_Member_Ids__c") %>%
    select(NewValue) %>%
    filter(!is.na(NewValue)) %>%
    mutate(
      nv_split = NewValue %>%
        strsplit(":") %>%
        purrr::map(~{
          stringr::str_subset(.x,stringr::fixed("..."), negate = TRUE)
        })
    ) %>%
    ungroup() %>%
    distinct()


  team_member_names <- get_id_names(
    df,
    "Matter_Team_Member_Ids__c",
    "User",
    team_member_ids[["nv_split"]] %>% unlist() %>% unique(),
    FALSE
  )


  team_member_ids[["name"]] <- future_lapply(team_member_ids[["nv_split"]], function(x){
    team_member_names %>%
      slice(match(x, Id)) %>%
      select(Name) %>%
      unlist() %>%
      paste0(collapse = ":")
  },
  future.seed = NULL) %>%
    unlist()


  df <- left_join(
    df,
    team_member_ids,
    by = join_by(NewValue == NewValue)
  ) %>%
    mutate(
      NewValue = ifelse(Field == "Matter_Team_Member_Ids__c", name, NewValue)
    ) %>%
    select(-nv_split, -name)


  return(df)
}
