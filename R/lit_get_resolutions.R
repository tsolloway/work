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
    apply_translate_limits = TRUE,
    add_links = list(
      link_resolution = "id_resolution",
      link_matter = "id_matter"
    ),
    nested_structure = list(
      by = "id_matter",
      data_name = "resolution_data",
      id_matter = 'resolution_data %>% purrr::map_vec(function(x)x[1,"id_matter"]) %>% unlist()',
      case = 'resolution_data %>% purrr::map_vec(function(x)x[1,"case"]) %>% unlist()',
      resolution_count = 'resolution_data %>% purrr::map_vec(nrow)'
    )
){


  df <- lit_get_data(

    from_object = "litify_pm__Matter__c",
    select_object = c(
      Id, Needles_CaseID__c, Policy_Limit__c,
      litify_pm__Incident_date__c, Received_Signed_Agreement__c
    ),

    from_object_child = "litify_pm__LitifyResolutions__r",
    select_object_child = c(
      Id, litify_pm__Resolution_Type__c, litify_pm__Settlement_Verdict_Amount__c,
      litify_pm__Resolution_Date__c, Resolution_Group__c, Resolution_Reason__c
    ),

    from_object_parent = "litify_pm__Client__r",
    select_object_parent = c(Name),

    from_object_child_parent = "litify_pm__Payor__r",
    select_object_child_parent = c(Name),

    cases = cases,
    limit = limit,

    apply_translate_limits = apply_translate_limits,
    add_links = add_links
  ) %>% rename_col(
    id_matter = Id,
    date_incident = litify_pm__Incident_date__c,
    case = Needles_CaseID__c,
    policy_limit = Policy_Limit__c,
    date_signed_agreement = Received_Signed_Agreement__c,
    client_name = litify_pm__Client__r.Name,
    id_resolution = litify_pm__Resolution__c.Id,
    id_payor = litify_pm__Resolution__c.litify_pm__Payor__r.Name,
    date_resolution = litify_pm__Resolution__c.litify_pm__Resolution_Date__c,
    resolution_type = litify_pm__Resolution__c.litify_pm__Resolution_Type__c,
    resolution_amount = litify_pm__Resolution__c.litify_pm__Settlement_Verdict_Amount__c,
    resolution_group = litify_pm__Resolution__c.Resolution_Group__c,
    resolution_reason = litify_pm__Resolution__c.Resolution_Reason__c,
    link_resolution = link_resolution,
    link_matter = link_matter
  )



  if( !is.null(nested_structure) && is.list(nested_structure) ){

    df <- df %>%
      dplyr::group_split( eval(parse(text = nested_structure[["by"]])) ) %>%
      tibble::tibble() %>%
      setNames(
        nested_structure[["data_name"]]
        )

    assign(
      nested_structure[["data_name"]],
      df[[nested_structure[["data_name"]]]]
    )


    for(i in names(nested_structure[-base::c(1:2)])){
      df <- df %>%
        dplyr::bind_cols(
          !!i := eval(parse(text=nested_structure[[i]]))
        )
    }


    df <- df %>%
      dplyr::select(
        names(nested_structure[-base::c(1:2)]), nested_structure[["data_name"]]
      )
  }



}
