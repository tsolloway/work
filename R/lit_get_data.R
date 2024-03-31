#' lit_get_resolutions
#' @description queries litify for resolution data
#' @param cases vector of case numbers
#' @param select_object fields from from_object
#' @param select_object_child fields from from_object_child
#' @param select_object_parent fields from from_object_parent
#' @param select_object_child_parent fields from from_object_child_parent
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
    apply_translate_limits = FALSE,
    add_links = FALSE,
    nested_structure = NULL,
    chunks = 1000,
    additional_where_constant = NULL,
    additional_where_child_constant = NULL,
    use_bulk = c("auto", TRUE, FALSE),
    parallel_process = TRUE,
    verbose = FALSE
){

  work::start(lib_sales_force = TRUE)


  sf_auth()


  if( !is.logical(use_bulk) || is.na(use_bulk) ){
    use_bulk <- match.arg(use_bulk)
  }



  if( !is.null(cases) ){
    cases <- cases %>% unlist() %>% unique() %>% work::remove_na()
  }



  if( use_bulk == "auto" ){

    if( !is.null(from_object_child) || !is.null(from_object_child) ){
      use_bulk <- FALSE
    }else if( is.null(cases) ){
      use_bulk <- TRUE
    }else{
      use_bulk <- FALSE
    }

    if(verbose) message( glue("'use_bulk' switched to {use_bulk}") )
  }



  if( use_bulk ){ parallel_process <- FALSE }




  if( parallel_process ){
    start(lib_future = TRUE)

    old_plan <- plan(multisession)

    on.exit(plan(old_plan), add = TRUE)
  }




  if( parallel_process && is.null(cases) ){


    cases <- lit_get_data(
      from_object = from_object,
      select_object = "Id",
      use_bulk = TRUE) %>%
      select(Id) %>%
      distinct() %>%
      unlist()

    cases_field <- "Id"
    select_object <- base::c(cases_field, select_object) %>% unique()
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


    if( !use_bulk ){


      cases_list <- split(cases, rep(seq(ceiling(length(cases)/chunks)), length.out = length(cases), each = chunks))

      cases_list <- cases_list %>% lapply(work::where_cases_equal, api_field = cases_field)


      if( !is.null(additional_where_constant) ){

        cases_list <- cases_list %>% purrr::map(~paste0(additional_where_constant, " AND ", .x))

      }


      query <- cases_list %>% lapply(function(x)glue(
        "{query}
      WHERE {x}"
      ))


      if( is.list(query) && length(query) == 1 ){
        query <- query %>% unlist()
      }
    }

  }else if(
    ( is.null(cases) && !is.null(additional_where_constant) ) ||
    ( use_bulk && !is.null(additional_where_constant) )
  ){
      query <- glue::glue(
        "{query}
      WHERE {additional_where_constant}"
      )

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


    if( use_bulk ){
      df <- sf_query(query, api_type = "Bulk 2.0", guess_types = FALSE)
    }else if(!use_bulk){
      df <- sf_query(query, guess_types = FALSE)
    }



    if( !is.null(cases) ){
      df <- df %>% filter( df[[cases_field]] %in% cases )
    }



  }else if( is.list(query) ){


    if( parallel_process ){

      df <- future_lapply(query, sf_query, guess_types = FALSE, future.seed = NULL)

    }else if( !parallel_process ){

      df <- lapply(query, sf_query, guess_types = FALSE)
    }


    df_classes <- df %>% purrr::map_df(~{.x %>% purrr::map_df(class)})


    df_classes_not_same <- df_classes %>% purrr::map_lgl(~{
      .x %>% remove_na() %>% unique() %>% length() %>% equals(1) %>% not()
    })


    if( !all(df_classes_not_same) ){

      df_classes_not_same <- df_classes %>% select( names(df_classes_not_same)[df_classes_not_same] )

      for(i in names(df_classes_not_same)){
        differnet_classes <- df_classes_not_same %>% select(!!i) %>% unique() %>% unlist()

        if(
          all(grepl("character|numeric", differnet_classes))
        ){
          warning(glue("column {i} has {glue_collapse(differnet_classes, sep = ', ')}. Changing to only character."))

          df <- df %>% purrr::map(~{
            .x <- .x %>% work::insert_missing_column(i)
            .x[[i]] <- .x[[i]] %>% as.character()
            .x
          })

        }else if( all(grepl("POSIXct|POSIXt", differnet_classes)) ){

          # do nothing, it'll row bind.

        }else{
          stop(glue("column {i} has {glue_collapse(differnet_classes, sep = ', ')}. This scenario is not coded."))
        }
      }
    }


    df <- dplyr::bind_rows(df)
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
