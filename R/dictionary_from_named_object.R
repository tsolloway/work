#' Create a standard dictionary data frame from various named objects
#'
#' @description
#' Converts a named object (list, character vector, or data frame/tibble) into a standardized
#' dictionary data frame with columns `var` (variable name) and `label` (human-readable label).
#'
#' This is useful for preparing variable labels for plotting, tables, or network visualization.
#'
#' @param dictionary A named object to convert. Can be:
#'   - `NULL`: returns `NULL`
#'   - `data.frame` or `tibble`: should contain a `label` column; labels are trimmed
#'   - `list` or `named vector`: converts to a data frame with names as `var` and values as `label`
#'   - `character vector`: names become `var`, values become `label`; unnamed vectors get names assigned automatically
#'
#' @return A tibble with columns `var` and `label`, or `NULL` if input is `NULL`.
#'
#' @examples
#' # Named character vector
#' dict_char <- c(Sepal.Length = "Sepal Length", Petal.Width = "Petal Width")
#' dictionary_from_named_object(dict_char)
#'
#' # Unnamed character vector
#' dict_unnamed <- c("A", "B")
#' dictionary_from_named_object(dict_unnamed)
#'
#' # List
#' dict_list <- list(Sepal.Length = "Sepal Length", Petal.Width = "Petal Width")
#' dictionary_from_named_object(dict_list)
#'
#' # Data frame / tibble
#' dict_df <- tibble::tibble(var = c("Sepal.Length", "Petal.Width"),
#'                           label = c("Sepal Length", "Petal Width"))
#' dictionary_from_named_object(dict_df)
#'
#' # NULL input
#' dictionary_from_named_object(NULL)
#'
#' @export
dictionary_from_named_object <- function(dictionary){


  if(is.null(dictionary)){
    return(NULL)
  }


  if(is.data.frame(dictionary) || tibble::is_tibble(dictionary)){
    dictionary <- dictionary %>%
      dplyr::mutate(label = stringr::str_squish(label))
    return(dictionary)
  }


  if(is.list(dictionary) && !tibble::is_tibble(dictionary)){
    dictionary <- dictionary %>% purrr::map_chr(c)
  }


  if(is.character(dictionary)){
    if(is.null(names(dictionary))){
      names(dictionary) <- dictionary
    }
    dictionary <- tibble::tibble(var = names(dictionary), label = dictionary)
  }


  dictionary <- dictionary %>%
    dplyr::mutate(
      label = stringr::str_squish(label)
    )



  return(dictionary)
}
