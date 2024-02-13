#' lit_get_resolutions
#' @description queries litify for resolution data
#' @param cases vector of case numbers
#' @param select_resolution fields from litify_pm__LitifyResolutions__c
#' @param select_matter fields from litify_pm__Matter__c
#' @param select_client fields from litify_pm__Client__c
#' @param select_payor fields from litify_pm__Payor__c
#' @param predetermined_names character vector of predetermined col_names.  Tricky to use unless with defaults
#' @export
lit_get_resolutions <- function(
    cases = NULL, limit = NULL,
    select_resolution = c(
      "Id", "litify_pm__Resolution_Type__c", "litify_pm__Settlement_Verdict_Amount__c",
      "litify_pm__Resolution_Date__c", "Resolution_Group__c", "Resolution_Reason__c"
    ),
    select_matter = c(
      "Id", "Needles_CaseID__c", "Policy_Limit__c", "litify_pm__Incident_date__c", "Received_Signed_Agreement__c"
    ),
    select_client = c("Name"),
    select_payor = c("Name"),
    predetermined_names = c(
      "id_matter", "date_incident","case","policy_limit",  "date_signed_agreement",
      "client_name", "id_resolution", "id_payor", "date_resolution", "resolution_type",
      "resolution_amount", "resolution_group", "resolution_reason"
    ),
    sort_predetermined_names = TRUE,
    apply_translate_limits = TRUE,
    nested_structure = TRUE
){

  require(glue)
  require(salesforcer)

  if( is.null(predetermined_names) && nested_structure){
    warning("can't have nested structure within function without predetermined names / structure")
    nested_structure <- FALSE
  }

  select_client <- glue("litify_pm__Client__r.{select_client}")
  select_payor <- glue("litify_pm__Payor__r.{select_payor}")

  if( length(select_resolution)>1 ) select_resolution <- select_resolution %>% glue_sql_collapse(",")
  if( length(select_matter)>1 ) select_matter <- select_matter %>% glue_sql_collapse(",")
  if( length(select_client)>1 ) select_client <- select_client %>% glue_sql_collapse(",")
  if( length(select_payor)>1 ) select_payor <- select_payor %>% glue_sql_collapse(",")


  if(length(select_matter) > 0 && length(select_client) > 0){
    first_line <- glue("SELECT {select_matter},{select_client}")
  }else if(length(select_matter) > 0 && length(select_client) == 0){
    first_line <- glue("SELECT {select_matter}")
  }else{
    stop("unsupported matter & client select combination")
  }

  if(length(select_resolution) > 0 && length(select_payor) > 0){
    second_line <- glue("(SELECT {select_resolution},{select_payor} FROM litify_pm__LitifyResolutions__r)")
  }else if(length(select_resolution) > 0 && length(select_payor) == 0){
    second_line <- glue("(SELECT {select_resolution} FROM litify_pm__LitifyResolutions__r)")
  }else{
    stop("unsupported resolution & payor select combination")
  }

  querry <- glue("{first_line},
                 {second_line}
                 FROM litify_pm__Matter__c")


  if (!is.null(cases) ){

    cases <- cases %>% work::where_cases_equal()

    querry <- glue("{querry}
                   WHERE {cases}")
  }


  if( !is.null(limit) && is.numeric(limit) && limit > 0 ){

    cases <- cases %>% work::where_cases_equal()

    querry <- glue("{querry}
                   LIMIT {limit}")
  }


  df <- querry %>% sf_query()

  df <- set_attr(df, "api_names", names(df))

  df <- df %>% work::names_clean()


  if( !is.null(predetermined_names) ){

    df <- df %>% stats::setNames(predetermined_names)

    df <- df %>% dplyr::mutate(
      resolution_link = glue::glue("https://wilshirelawfirm.lightning.force.com/lightning/r/litify_pm__Resolution__c/{id_resolution}/view")
    )

    if( sort_predetermined_names ){
      df <- df %>% dplyr::select(
        case, client_name, date_incident, resolution_amount, policy_limit, date_signed_agreement, date_resolution,
        resolution_type, resolution_group, resolution_group, resolution_reason, id_payor, id_matter, id_resolution, resolution_link
      )
    }

    if(apply_translate_limits) df[["policy_limit"]] <- df[["policy_limit"]] %>% work::translate_limits()

  }else{
    df <- df %>% dplyr::mutate(
      resolution_link = glue::glue("https://wilshirelawfirm.lightning.force.com/lightning/r/litify_pm__Resolution__c/{litify_pm_resolution_c_id}/view")
    )

    if(apply_translate_limits) df[["policy_limit_c"]] <- df[["policy_limit_c"]] %>% work::translate_limits()
  }


  if(nested_structure){

    df <- df %>%
      dplyr::group_split(id_matter) %>%
      tibble::tibble(
        resolution_data = .,
        "id_matter" = resolution_data %>% purrr::map_vec(function(x)x[1,"id_matter"]) %>% unlist(),
        "case" = resolution_data %>% purrr::map_vec(function(x)x[1,"case"]) %>% unlist(),
        resolution_count = resolution_data %>% purrr::map_vec(nrow)
      ) %>%
      dplyr::select(id_matter, case, resolution_count, resolution_data)

  }


  return(df)
}
