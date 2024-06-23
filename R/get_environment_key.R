#' get_environment_key
#' @description get_environment_key
#' @export
get_environment_key <- function(x){

  key <- Sys.getenv(x)

  if(!is_truthy(key)){
    key <- rstudioapi::askForPassword("Enter the key, please")
  }

  return(key)
}

