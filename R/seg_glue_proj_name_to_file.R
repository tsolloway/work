#' seg_glue_proj_name_to_file
#'
#' @description Prepends the project name and/or number from the seg object
#'   metadata to a base file name, e.g. `"ProjectName (123) - Shell"`.
#'
#' @param seg A seg object with `meta$project_name` and/or `meta$project_number`.
#' @param file_name Character. The base file name to prepend to.
#'
#' @return Character. The formatted file name string.
#'
#' @export
seg_glue_proj_name_to_file <- function(seg, file_name){

  project_name <- seg[["meta"]][["project_name"]] %>% stringr::str_squish()
  project_number <- seg[["meta"]][["project_number"]] %>% stringr::str_squish()

  if(!is.null(project_name) && !is.null(project_number)){
    file_name <- glue("{project_name} ({project_number}) - {file_name}")
  }else if(!is.null(project_name) && is.null(project_number)){
    file_name <- glue("{project_name} - {file_name}")
  }else if(is.null(project_name) && !is.null(project_number)){
    file_name <- glue("{file_name} ({project_number})")
  }

  return(file_name)
}
