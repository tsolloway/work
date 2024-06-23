#' names_clean
#' @description Returns dictionaries from litify objects
#' @param x named df or character vector
#' @export
names_clean <- function(x, make_lowercase = TRUE){

  if( tibble::is_tibble(x) || is.data.frame(x) ){
    names(x) <- names(x) %>% str_scrub(make_lowercase = make_lowercase)
    return(x)
  }else if(is.character(x)){
    return(x %>% str_scrub(make_lowercase = make_lowercase))
  }else{
    stop("class not yet supported in 'names_clean'")
  }
}
