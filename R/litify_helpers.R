#' lit_get_dictionary_from
#' @description Returns dictionaries from litify objects
#' @param from ojbects in litify that are serviced in work::litify_dict
#' @param what fields srom the object, relationships, or both
#' @export
lit_get_dictionary_from <- function(
    from = work::litify_dict[["object"]],
    what = c("dictionary", "relationships")
){

  from <- match.arg(from)
  what <- match.arg(what)

  temp <- work::litify_dict

  temp %>% subset(temp[["object"]] == from) %>%
    dplyr::select(what) %>%
    tidyr::unnest(cols = what)
}



#' lit_get_fields_from
#' @description Returns fields from litify objects
#' @inheritParams lit_get_dictionary_from
#' @export
lit_get_fields_from <- function(
    from = work::litify_dict[["object"]]
){

  from <- match.arg(from)

  work::lit_get_dictionary_from(from = from, what = "dictionary")[["api_name"]]

}



#' lit_get_clip
#' @description Gets clipboard clip from litify soql all field extension
#' @param write logical
#' @param write_path character to write folder path
#' @export
lit_get_clip <- function(write = TRUE, write_path = "~/Downloads/"){

  original <- clipr::read_clip()

  temp <- original %>% stringr::str_remove("SELECT") %>% stringr::str_split("FROM") %>% unlist()
  vars <- temp %>% head(1) %>% stringr::str_squish() %>%  stringr::str_split(",") %>% unlist()
  obj <- temp %>% tail(1) %>% stringr::str_split("WHERE") %>% unlist() %>% head(1) %>% stringr::str_squish()

  write_path %>% normalizePath(mustWork = TRUE)

  if(write) write.csv(vars, glue::glue("{write_path}{obj}.csv"))

  if(!write) return(list(original = original, fields = vars, object = obj))
}



#' lit_put_dictionary_in_pkg
#' @description Writes litify dictionary to work package.
#' @param where character to write workbook path. Must be xlsx file.
#' @export
lit_put_dictionary_in_pkg <- function(where = "~/Downloads/Map.xlsx", overwrite = TRUE){

  if( "~/Downloads/map.xlsx" %>% tools::file_ext() %>% equals("xlsx") %>% not() ){
    stop("Must be xlsx input file")
  }

  object_names <- openxlsx::getSheetNames(where)

  litify_dict <- object_names %>%
    purrr::map(function(x){openxlsx::read.xlsx(where, sheet = x)}) %>%
    purrr::map(function(x){
      list(
        dictionary = x %>% dplyr::select(api_name, api_label, type, value) %>% na.omit(),
        relationships = x %>% dplyr::select(relationship, child_object, field, label) %>% na.omit()
      )
    }) %>% tibble::tibble(
      temp = .,
      object = object_names,
      dictionary = purrr::map(temp, purrr::pluck("dictionary") %>% dplyr::select(-relationships)),
      relationships = purrr::map(temp, purrr::pluck("relationships") %>% dplyr::select(-dictionary))
    ) %>% dplyr::select(-temp)

  usethis::use_data(litify_dict, overwrite = overwrite)
}



#' lit_create_query
#' @description Returns a soql querry
#' @inheritParams lit_get_dictionary_from
#' @param select fields in the litify object. If NULL, all fields selected
#' @param where list with where conditions
#' @param limit numeric that limits the rows returned in the query
#' @export
lit_create_query <- function(
    from = work::litify_dict[["object"]],
    select = NULL,
    where = NULL, limit = NULL,
    check_select = TRUE,
    check_from = TRUE
){

  if(!is.null(limit) && !is.numeric(limit)) stop("limit must be numeric")


  if(check_from) {

    from <- match.arg(from)

    valid_fields <- work::lit_get_fields_from(from)

    if( is.null(select) ){
      select <- valid_fields
    }else if( !is.null(select) ){

      if(check_select){
        not_valid <- select[!select %in% valid_fields]

        if(length(not_valid) > 0 ){
          stop( glue::glue("Could not find these fields in the from object: ", glue::glue_collapse(not_valid, , sep = ", ")) )
        }
      }

      select <- select[select %in% valid_fields]
    }
  }


  select <- glue::glue("SELECT {glue::glue_collapse(select, sep = ', ')}")

  from <- glue::glue("FROM {from}")


  if( !is.null(where) ){
    where <- glue::glue("WHERE {where}")
  }


  if( !is.null(limit) ){
    limit <- limit %>% round(0)

    limit <- glue::glue("LIMIT {limit}")
  }

  nosql_syntax <- c()

  nosql_syntax <- c(nosql_syntax, select)

  nosql_syntax <- c(nosql_syntax, from)

  if( !is.null(where) ) nosql_syntax <- c(nosql_syntax, where)

  if( !is.null(limit) ) nosql_syntax <- c(nosql_syntax, limit)


  nosql_syntax <- nosql_syntax %>% paste0(collapse = "\n")

  return(nosql_syntax)
}




#' lit_run_query
#' @description Returns query from litify objects
#' @inheritParams lit_create_query
#' @param query noql query string
#' @param return_dictionary logical.  Will not return if you use the query parameter
#' @export
lit_run_query <- function(
    from = work::litify_dict[["object"]],
    select = NULL,
    where = NULL, limit = NULL,
    check_select = TRUE,
    check_from = TRUE,
    query = NULL,
    return_dictionary = FALSE
){

  if( !is.null(query) ){

    output <- query %>% work::lit_create_query() %>% salesforcer::sf_query()

  }else if( is.null(query) ){

    if(check_from) from <- match.arg(from)

    output <- work::lit_create_query(
      from = from, select = select, where = where, limit = limit, check_select = check_select
    ) %>% salesforcer::sf_query()

    if( return_dictionary ){
      output <- list(
        data = output,
        dictionary = work::lit_get_dictionary_from(from)
      )
    }
  }

  return(output)
}











