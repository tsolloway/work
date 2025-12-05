#' str_scrub
#'
#' @description
#' Cleans and standardizes a character vector by removing punctuation, extra spaces,
#' UTF-8 characters, and optionally converting to lowercase. Spaces can be replaced
#' with a custom string. Supports filling NA or NULL values.
#'
#' @param x Character vector to scrub.
#' @param replacement Character string used to replace spaces. Default "_".
#' @param make_lowercase Logical; if TRUE, converts text to lowercase. Default TRUE.
#' @param return_on_error Logical; if TRUE, returns input with warning on error, else stops.
#' @param fill_na Optional value to replace NA elements.
#' @param fill_null Optional value to replace NULL elements.
#' @param keep Optional character to retain even if it is punctuation.
#' @param remove_all_spaces Logical; if TRUE, replaces spaces with `replacement`. Default TRUE.
#' @param remove_utf8 Logical; if TRUE, removes non-ASCII characters. Default TRUE.
#'
#' @return
#' A character vector with cleaned/scrubbed elements.
#'
#' @examples
#' str_scrub("Hello, World!")
#' str_scrub(c("A B", "C&D"), replacement = "-")
#' str_scrub("Äccent ütf8", remove_utf8 = TRUE)
#'
#' @export
str_scrub <- function(
    x, replacement = "_", make_lowercase = TRUE,
    return_on_error = TRUE, fill_na = NULL, fill_null = NULL,
    keep = NULL, remove_all_spaces = TRUE, remove_utf8 = TRUE
){


  do_this <- function(x, replacement, keep = NULL, remove_all_spaces = TRUE, remove_utf8 = TRUE){

    if( !is_truthy(x) ) return(x)


    if( is.null(keep) ){

      x <- x %>% gsub('[[:punct:] ]+',' ', .)

    }else if( !is.null(keep) ){

      x <- gsub("[[:punct:]]", function(m) ifelse(m == keep, keep, ""), x)

    }


    if( remove_utf8 ){
      x <- x %>% gsub('[^ -~]', '', .)
    }


    x <- x %>% stringr::str_squish()


    if( remove_all_spaces ){
      x <- x %>% gsub(" ", replacement, .)
    }

    return(x)
  }



  # Fill NA / NULL
  if( !is.null(fill_na) ) x[is.na(x)] <- fill_na
  if( !is.null(fill_null) ) x[is.null(x)] <- fill_null


  if( make_lowercase ) x <- x %>% tolower()


  # Apply function
  if( length(x) == 1 ){
    x %>% do_this(replacement, keep, remove_all_spaces, remove_utf8)
  }else if( length(x) > 1 ){
    x %>% purrr::map_vec(do_this, replacement, keep, remove_all_spaces, remove_utf8)
  }else{
    if( return_on_error ){
      warning("input to str_scrub needs a length")
      x
    }else{
      stop("input to str_scrub needs a length")
    }
  }
}

