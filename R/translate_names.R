#' translate_names
#' @description returns scrubbed names vector
#' @param x character vector to translate
#' @export
translate_names <- function(x, type = c("cm", "attorney", "negotiator")){

  work::start()

  type <- match.arg(type)


  df_names <- work::df_translate_employee_names %>%
    bind_rows(
      mutate(., input = translation)
    ) %>%
    bind_rows(
      mutate(., input = translation %>% work::str_scrub(" ") %>% gsub(" fe | fe", "", .) )
    ) %>%
    bind_rows(
      mutate(., input = translation %>% work::str_scrub(" ") )
    ) %>%
    mutate(
      input = input %>% stringr::str_squish(),
      translation = translation %>% stringr::str_squish()
    ) %>%
    distinct()


  df_names <- df_names %>% filter(df_names[[type]])


  x <- x %>% work::str_scrub(" ") %>% stringr::str_squish()
  y <- df_names[["translation"]][ match(x, df_names[["input"]]) ]


  result <- ifelse(is.na(y) & !is.na(x),x, y)


  return(result)
}

