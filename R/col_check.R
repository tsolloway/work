#' col_check
#' @description col_check
#' @export
col_check <- function(df, check_names, hard_stop = TRUE){

  col_names <- colnames(df)

  if(!all(check_names %in% col_names)){

    missing_names <- check_names[!check_names %in% col_names]

    if(hard_stop){
      stop(
        message_collapse("Could not find: ", missing_names)
      )
    }


    return(FALSE)

  }else{

    return(TRUE)

  }
}

