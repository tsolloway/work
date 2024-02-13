#' lit_get_resolutions
#' @description queries litify for resolution data
#' @param cases vector of case numbers
#' @param select_object fields from from_object
#' @param select_object_child fields from from_object_child
#' @param select_object_parent fields from from_object_parent
#' @param select_object_child_parent fields from from_object_child_parent
#' @param predetermined_names character vector of predetermined col_names.  Tricky to use unless with defaults
#' @export
lit_get_data <- function(
    from_object,
    select_object,
    from_object_child = NULL,
    select_object_child = NULL,
    from_object_parent = NULL,
    select_object_parent = NULL,
    from_object_child_parent = NULL,
    select_object_child_parent = NULL,
    cases = NULL,
    limit = NULL,
    predetermined_names = NULL,
    sort_predetermined_names = NULL,
    apply_translate_limits = TRUE,
    add_links = TRUE,
    nested_structure = TRUE
){

  require(glue)
  require(salesforcer)

  if( is.null(predetermined_names) && nested_structure){
    warning("can't have nested structure within function without predetermined names / structure")
    nested_structure <- FALSE
  }



  if( !is.null(from_object_parent) && !is.null(select_object_parent) ){
    select_object_parent <- glue("{from_object_parent}.{select_object_parent}")
  }

  if( !is.null(from_object_child_parent) && !is.null(select_object_child_parent) ){
    select_object_child_parent <- glue("{from_object_child_parent}.{select_object_child_parent}")
  }



  if( length(select_object)>1 ) select_object <- select_object %>% glue_sql_collapse(",")
  if( length(select_object_child)>1 ) select_object_child <- select_object_child %>% glue_sql_collapse(",")
  if( length(select_object_parent)>1 ) select_object_parent <- select_object_parent %>% glue_sql_collapse(",")
  if( length(select_object_child_parent)>1 ) select_object_child_parent <- select_object_child_parent %>% glue_sql_collapse(",")



  if(length(select_object) > 0 && length(select_object_parent) > 0){

    first_line <- glue("SELECT {select_object},{select_object_parent}")

  }else if(length(select_object) > 0 && length(select_object_parent) == 0){

    first_line <- glue("SELECT {select_object}")

  }else{

    stop("unsupported select_object & select_object_parent combination")
  }




  if(length(select_object_child) > 0 && length(select_object_child_parent) > 0){

    second_line <- glue("(SELECT {select_object_child},{select_object_child_parent} FROM litify_pm__LitifyResolutions__r)")

  }else if(length(select_object_child) > 0 && length(select_object_child_parent) == 0){

    second_line <- glue("(SELECT {select_object_child} FROM litify_pm__LitifyResolutions__r)")

  }else if(length(select_object_child) == 0 || is.na(select_object_child) || is.null(select_object_child) ){

    second_line <- NULL

  }else{

    stop("unsupported resolution & payor select combination")
  }




  if( !is.null(second_line) ){
    querry <- glue("{first_line},
                 {second_line}
                 FROM {from_object}")
  }else{
    querry <- glue("{first_line}
                 FROM {from_object}")
  }




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

    if( !is.null(sort_predetermined_names) ){
      df <- df %>% dplyr::select(sort_predetermined_names)
    }
  }




  if( !is.null(apply_translate_limits) && is.character(apply_translate_limits) && length(apply_translate_limits) == 1  ){

    df[[apply_translate_limits]] <- df[[apply_translate_limits]] %>% work::translate_limits()

  }else if( !is.null(apply_translate_limits) && is.character(apply_translate_limits) && length(apply_translate_limits) > 1  ){

    for(i in apply_translate_limits){
      df[[i]] <- df[[i]] %>% work::translate_limits()
    }

  }else if( isTRUE(apply_translate_limits) ){

    apply_translate_limits <- names(df)[grep("limit", names(df), ignore.case = TRUE)]

    for(i in apply_translate_limits){
      df[[i]] <- df[[i]] %>% work::translate_limits()
    }
  }


  if( isTRUE(add_links) ){

    warning("add_links == TRUE parameter is over inclusive")

    add_links <- names(df)[grep("id", names(df), ignore.case = TRUE)]

    for(i in add_links){
      df[[paste0("link_",i)]] <- paste0("https://wilshirelawfirm.lightning.force.com/lightning/r/", df[[add_links[[i]]]], "/view")
    }

  }else if( is.list(add_links) && length(add_links) > 0 ){

    for( i in names(add_links) ){
      df[[i]] <- paste0("https://wilshirelawfirm.lightning.force.com/lightning/r/", df[[add_links[[i]]]], "/view")
    }

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
