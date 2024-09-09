#' restart
#' @description Custom restart function that exectues common tasks
#' @param keep Logical or character vector. TRUE is keep all objects on restart. FALSE is drop all objects on restart. Character vector specifies which objects to keep.
#' @param run_gc Logical. If TRUE, gc() is executed after restart. Note that the gc() is executed before the session is restarted.
#' @param restart_session Logical. IF true, the R session will be restarted
#' @export
restart <- function(
    keep = FALSE, run_gc = TRUE, clean = FALSE, restart_session = TRUE,
    start_after = TRUE, restart_after_command = NULL
){


  code_to_eval <- "purrr::possibly(~detach('package:work', unload = TRUE))()"

  environment_objects <- ls(envir = .GlobalEnv)


  if( isFALSE(keep) ){

    code_to_eval <- c(code_to_eval, "rm(list = environment_objects, envir = .GlobalEnv)")

  }else if( isTRUE(keep) ){

    #intentionally blank

  }else if( is.character(keep) ){

    environment_objects <- setdiff(environment_objects, keep)
    code_to_eval <- c(code_to_eval, "rm(list = environment_objects, envir = .GlobalEnv)")

  }else{
    stop("parameter keep isn't logical or character")
  }


  if( run_gc ){
    code_to_eval <- c(code_to_eval, "gc(verbose = FALSE, reset = TRUE)")
  }



  if(is.null(restart_after_command) && start_after){
    restart_after_command <- "work::start()"
  }else if(!is.null(restart_after_command) && start_after){
    restart_after_command <- c(restart_after_command, "work::start()")
  }else if(!start_after){
    # do nothing
  }



  if( restart_session && is.null(restart_after_command)){
    code_to_eval <- c(code_to_eval, glue::glue(".rs.restartR(clean = {clean})"))
  }else if( restart_session && !is.null(restart_after_command)){
    code_to_eval <- c(code_to_eval, glue::glue(".rs.restartR(afterRestartCommand = {restart_after_command}, clean = {clean})"))
  }


  code_to_eval <- code_to_eval %>% stringi::stri_remove_empty()


  return(
    eval(parse(text=code_to_eval))
  )
}
