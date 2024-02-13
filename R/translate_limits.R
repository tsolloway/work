#' translate_limits
#' @description returns scrubbed limit vector
#' @param x character vector to translate
#' @export
translate_limits <- function(x){

  df_limit <- work::df_translate_limtis

  df_limit <- df_limit %>% dplyr::add_row(
    input =  df_limit[["translation"]],
    translation =  df_limit[["translation"]],
    valid_limits = rep("", length( df_limit[["translation"]]))
  )

  df_limit[["input"]] <- df_limit[["input"]] %>% stringr::str_squish()
  df_limit[["translation"]] <- df_limit[["translation"]] %>% stringr::str_squish()

  x <- x %>% stringr::str_squish()
  y <- df_limit[["translation"]][ match(x, df_limit[["input"]]) ]

  ifelse(is.na(y) & !is.na(x),x, y)
}
