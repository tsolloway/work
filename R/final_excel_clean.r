#' final_excel_clean
#' @description final_excel_clean
#' @export
final_excel_clean <- function(x){
  x %>%
    mutate_if(is.numeric, round, 4) %>%
    setNames(
      .,
      names(.) %>%
        gsub("_", " ", .) %>%
        stringr::str_to_title()
    )
}
