#' translate_names
#' @description returns scrubbed names vector
#' @param x character vector to translate
#' @export
translate_names <- function(x, df_names, return_dict = FALSE){

  df_names <- work::df_translate_employee_names

  df_names <- df_names %>% dplyr::add_row(
    input = df_names[["translation"]],
    translation = df_names[["translation"]],
    cm = rep("", length(df_names[["translation"]])),
    attorney = rep("", length(df_names[["translation"]])),
    negotiator = rep("", length(df_names[["translation"]]))
  )

  df_names <- df_names %>% dplyr::add_row(
    input = df_names[["translation"]] %>% work::str_scrub(" ") %>% gsub(" fe | fe", "", .),
    translation = df_names[["translation"]],
    cm = rep("", length(df_names[["translation"]])),
    attorney = rep("", length(df_names[["translation"]])),
    negotiator = rep("", length(df_names[["translation"]]))
  )

  df_names <- df_names %>% dplyr::add_row(
    input = df_names[["translation"]] %>% work::str_scrub(" "),
    translation = df_names[["translation"]],
    cm = rep("", length(df_names[["translation"]])),
    attorney = rep("", length(df_names[["translation"]])),
    negotiator = rep("", length(df_names[["translation"]]))
  )

  df_names <- df_names %>% dplyr::distinct()

  df_names[["input"]] <- df_names[["input"]] %>% stringr::str_squish()
  df_names[["translation"]] <- df_names[["translation"]] %>% stringr::str_squish()

  x <- x %>% work::str_scrub(" ") %>% stringr::str_squish()
  y <- df_names[["translation"]][ match(x, df_names[["input"]]) ]

  result <- ifelse(is.na(y) & !is.na(x),x, y)

  if(return_dict){
    return(df_names)
  }else{
    return(result)
  }
}

