#' str_scrub
#' @description Returns a scrubbed string
#' @param x Character / string vector
#' @param replacement Integer of how many n characters to return
#' @export
str_scrub <- function(x, replacement = "_", make_lowercase = TRUE,
                      return_on_error = TRUE, fill_na = NULL, fill_null = NULL,
                      keep = NULL, remove_all_spaces = TRUE, remove_utf8 = TRUE){


  do_this <- function(x, replacement, keep = NULL, remove_all_spaces = TRUE, remove_utf8 = TRUE){

    if( !work::is_truthy(x) ) return(x)

    if( is.null(keep) ){

      x <- x %>% gsub('[[:punct:] ]+',' ', .)

    }else if( !is.null(keep) ){

      x <- gsubfn::gsubfn(
        pattern = "[[:punct:]]", engine = "R",
        replacement = function(x) ifelse(x == keep, keep, ""),
        x)
    }



    if( remove_utf8 ) x <- x %>% gsub('[^ -~]', '', .)


    x <- x %>% stringr::str_squish()


    if( remove_all_spaces ) x <- x %>% gsub(" ", replacement, .)


    return(x)
  }



  if( make_lowercase ) x <- x %>% tolower()
  if( !is.null(fill_na) ) x[is.na(x)] <- fill_na
  if( !is.null(fill_null) ) x[is.null(x)] <- fill_null



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

