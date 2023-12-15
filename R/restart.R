#' restart
#' @description Custom restart function that exectues common tasks
#' @param keep Logical or character vector. TRUE is keep all objects on restart. FALSE is drop all objects on restart. Character vector specifies which objects to keep.
#' @param clean Logical. If TRUE, gc() is executed after restart. Note that the gc() is executed before the session is restarted.
#' @param restart_session Logical. IF true, the R session will be restarted
#' @export
restart <- function(keep = FALSE, clean = TRUE, restart_session = TRUE){

  code_to_eval <- ""

  environment_objects <- ls(envir = .GlobalEnv)


  if( isFALSE(keep) ){

    code_to_eval <- "rm(list = environment_objects, envir = .GlobalEnv)"

  }else if( isTRUE(keep) ){

    #intentionally blank

  }else if( is.character(keep) ){

    environment_objects <- setdiff(environment_objects, keep)
    code_to_eval <- "rm(list = environment_objects, envir = .GlobalEnv)"

  }else{
    stop("parameter keep isn't logical or character")
  }


  if( clean ){
    code_to_eval <- c(code_to_eval, "gc(verbose = FALSE, reset = TRUE)")
  }


  if( restart_session ){
    code_to_eval <- c(code_to_eval, ".rs.restartR()")
  }


  code_to_eval <- code_to_eval %>% stringi::stri_remove_empty()


  return(
    eval(parse(text=code_to_eval))
  )
}
