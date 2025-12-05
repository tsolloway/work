#' Clean and format a data frame for Excel export
#'
#' @description
#' Rounds numeric columns to 4 decimal places, formats column names for readability
#' (replaces underscores with spaces and converts to title case), and optionally
#' cleans character columns for Excel.
#'
#' @param x A data frame or tibble.
#' @param remove_non_ascii Logical; if TRUE, removes non-ASCII characters from character columns. Default TRUE
#' @param remove_ctrl_chars Logical; if TRUE, removes tabs, newlines, and carriage returns from character columns. Default TRUE
#' @param squish_spaces Logical; if TRUE, trims and collapses extra spaces in character columns. Default TRUE
#'
#' @return A cleaned data frame suitable for Excel export.
#'
#' @examples
#' final_excel_clean(iris)
#' final_excel_clean(iris, remove_ctrl_chars = TRUE)
#'
#' @export
final_excel_clean <- function(
    x,
    remove_non_ascii = TRUE,
    remove_ctrl_chars = TRUE,
    squish_spaces = TRUE
) {

  x_clean <- x %>%
    dplyr::mutate_if(is.numeric, ~round(., 4))

  if (remove_non_ascii) {
    x_clean <- x_clean %>%
      dplyr::mutate_if(is.character, ~gsub("[^\x20-\x7E]", "", .))
  }

  if (remove_ctrl_chars) {
    x_clean <- x_clean %>%
      dplyr::mutate_if(is.character, ~gsub("[\r\n\t]", " ", .))
  }

  if (squish_spaces) {
    x_clean <- x_clean %>%
      dplyr::mutate_if(is.character, ~stringr::str_squish(.))
  }

  setNames(
    x_clean,
    names(x_clean) %>%
      gsub("_", " ", .) %>%
      stringr::str_to_title()
  )
}
