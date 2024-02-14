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
      "litify_pm__Resolution_Date__c", "Resolution_Group__c", "Resolution_Reason__c"),
    select_matter = c(
      "Id", "Needles_CaseID__c", "Policy_Limit__c",
      "litify_pm__Incident_date__c", "Received_Signed_Agreement__c"),
    select_client = "Name",
    select_payor = "Name",
    predetermined_names = c(
      "id_matter", "date_incident","case","policy_limit",  "date_signed_agreement",
      "client_name", "id_resolution", "id_payor", "date_resolution", "resolution_type",
      "resolution_amount", "resolution_group", "resolution_reason"),
    sort_predetermined_names = c(
      "case", "client_name", "date_incident", "resolution_amount", "policy_limit",
      "date_signed_agreement", "date_resolution", "resolution_type", "resolution_group",
      "resolution_group", "resolution_reason", "id_payor", "id_matter", "id_resolution"),
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

  work::lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = select_matter,
    from_object_child = "litify_pm__LitifyResolutions__r",
    select_object_child = select_resolution,
    from_object_parent = "litify_pm__Client__r",
    select_object_parent = select_client,
    from_object_child_parent = "litify_pm__Payor__r",
    select_object_child_parent = select_payor,
    cases = cases,
    limit = limit,
    predetermined_names = predetermined_names,
    sort_predetermined_names = sort_predetermined_names,
    apply_translate_limits = apply_translate_limits,
    add_links = add_links,
    nested_structure = nested_structure
  )

}
