#' lit_get_resolutions
#' @description queries litify for resolution data
#' @param cases vector of case numbers
#' @param select_object fields from from_object
#' @param select_object_child fields from from_object_child
#' @param select_object_parent fields from from_object_parent
#' @param select_object_child_parent fields from from_object_child_parent
#' @param predetermined_names character vector of predetermined col_names.  Tricky to use unless with defaults
#' @param filter_syntax syntax to go into dplyr::filter
#' @param nested_structure listed instructions for nested tibble list(by = , data_name = ...)
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
    cases_field = "Needles_CaseID__c",
    limit = NULL,
    predetermined_names = NULL,
    sort_predetermined_names = NULL,
    filter_syntax = NULL,
    apply_translate_limits = FALSE,
    add_links = FALSE,
    nested_structure = NULL,
    col_name_clean = TRUE,
    chunks = 1000,
    additional_where_constant = NULL,
    additional_where_child_constant = NULL
){

  require(glue)
  require(dplyr)
  require(salesforcer)

  if( is.null(predetermined_names) && (!is.null(nested_structure) || isFALSE(predetermined_names)) ){
    warning("can't have nested structure within function without predetermined names / structure")
    nested_structure <- FALSE
  }


  #######################
  ##  prep the selects
  #######################

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



  #######################
  ##  write query
  #######################

  if(length(select_object) > 0 && length(select_object_parent) > 0){

    first_line <- glue("SELECT {select_object},{select_object_parent}")

  }else if(length(select_object) > 0 && length(select_object_parent) == 0){

    first_line <- glue("SELECT {select_object}")

  }else{

    stop("unsupported select_object & select_object_parent combination")
  }




  if(length(select_object_child) > 0 && length(select_object_child_parent) > 0){


    second_line <- glue("SELECT {select_object_child},{select_object_child_parent} FROM {from_object_child}")

    if( !is.null(additional_where_child_constant) ){
      second_line <- glue("{second_line} WHERE {additional_where_child_constant}")
    }

    second_line <- glue("({second_line})")


  }else if(length(select_object_child) > 0 && length(select_object_child_parent) == 0){


    second_line <- glue("SELECT {select_object_child} FROM {from_object_child}")

    if( !is.null(additional_where_child_constant) ){
      second_line <- glue("{second_line} WHERE {additional_where_child_constant}")
    }

    second_line <- glue("({second_line})")


  }else if(length(select_object_child) == 0 || is.na(select_object_child) || is.null(select_object_child) ){

    second_line <- NULL

  }else{

    stop("unsupported resolution & payor select combination")
  }




  if( !is.null(second_line) ){
    query <- glue("{first_line},
                 {second_line}
                 FROM {from_object}")
  }else{
    query <- glue("{first_line}
                 FROM {from_object}")
  }



  #######################
  ##  add where & limit
  #######################

  if( !is.null(cases) ){

    cases <- cases %>% unlist() %>% unique() %>% work::remove_na()

    cases_list <- split(cases, rep(seq(ceiling(length(cases)/chunks)), length.out = length(cases), each = chunks))

    cases_list <- cases_list %>% lapply(work::where_cases_equal, api_field = cases_field)


    if( !is.null(additional_where_constant) ){

      cases_list <- cases_list %>% purrr::map(~paste0(additional_where_constant, " AND ", .x))

      }


    query <- cases_list %>% lapply(function(x)glue::glue(
      "{query}
      WHERE {x}"
      ))

    if( is.list(query) && length(query) == 1 ){
      query <- query %>% unlist()
    }

  }



  if( !is.null(limit) && is.numeric(limit) && limit > 0 ){

    if( is.character(query) && !is.list(query) ){
      query <- glue("{query}
                   LIMIT {limit}")
    }else if( is.list(query) ){

      query <- query %>% lapply(function(x)glue::glue(
        "{x}
      LIMIT {limit}"
      ))
    }

  }



  #######################
  ##  do query
  #######################

  if( is.character(query) && !is.list(query) ){

    df <- query %>% sf_query()

    }else if( is.list(query) && length(query) > 1 ){

    df <- query %>% lapply(sf_query)

    df <- dplyr::bind_rows(df)
  }

  df <- set_attr(df, "api_names", names(df))

  if(col_name_clean) df <- df %>% work::names_clean()

  if( !is.null(predetermined_names) ){

    df <- df %>% stats::setNames(predetermined_names)

    if( !is.null(sort_predetermined_names) ){
      df <- df %>% dplyr::select(dplyr::all_of(sort_predetermined_names))
    }
  }


  #######################
  ##  apply filter
  #######################

  if( !is.null(filter_syntax) ){
    df <- tryCatch(df %>% dplyr::filter( eval(parse(text = filter_syntax)) ), error = function(e)df)
  }


  #######################
  ##  translate limits
  #######################
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


  #######################
  ##  add linkts
  #######################
  if( isTRUE(add_links) ){

    warning("add_links == TRUE parameter is over inclusive")

    add_links <- names(df)[grep("id", names(df), ignore.case = TRUE)]

    for(i in add_links){
      df[[paste0("link_",i)]] <- paste0("https://wilshirelawfirm.lightning.force.com/lightning/r/", df[[i]], "/view")
    }

  }else if( is.list(add_links) && length(add_links) > 0 ){

    for( i in names(add_links) ){
      df[[i]] <- paste0("https://wilshirelawfirm.lightning.force.com/lightning/r/", df[[add_links[[i]]]], "/view")
    }

  }


  #######################
  ##  nest the data
  #######################
  if( !is.null(nested_structure) && is.list(nested_structure) ){

    df <- df %>%
      dplyr::group_split( eval(parse(text = nested_structure[["by"]])) ) %>%
      tibble::tibble() %>%
      setNames(nested_structure[["data_name"]])

    assign(nested_structure[["data_name"]], df[[nested_structure[["data_name"]]]])

    for(i in names(nested_structure[-base::c(1:2)])){
      df <- df %>% dplyr::bind_cols(
        !!i := eval(parse(text=nested_structure[[i]]))
      )
    }

    df <- df %>% dplyr::select(
      names(nested_structure[-base::c(1:2)]), nested_structure[["data_name"]]
    )
  }


  #######################
  ##  return
  #######################

  return(df)
}
