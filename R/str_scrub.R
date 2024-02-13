#' str_scrub
#' @description Returns a scrubbed string
#' @param x Character / string vector
#' @param repacement Integer of how many n characters to return
#' @export
str_scrub <- function(x, repacement = "_", make_lowercase = FALSE,
                      return_on_error = TRUE, fill_na = NULL, fill_null = NULL){

  do_this <- function(x, repacement){
    x <- x %>% gsub('[[:punct:] ]+',' ', .) %>% stringr::str_squish() %>% trimws()
    x %>% gsub(" ", repacement, .)
  }

  x <- x %>% tolower()

  if( !is.null(fill_na) ) x[is.na(x)] <- fill_na
  if( !is.null(fill_null) ) x[is.null(x)] <- fill_null

  if( length(x) == 1 ){
    x %>% do_this(repacement)
  }else if( length(x) > 1 ){
    x %>% purrr::map_vec(do_this, repacement)
  }else{
    if( return_on_error ){
      warning("input to str_scrub needs a length")
      x
    }else{
      stop("input to str_scrub needs a length")
    }
  }
}

