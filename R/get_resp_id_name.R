#' get_resp_id_name
#' @description get_resp_id_name
#' @export
get_resp_id_name <- function(obj, .default = "seg_uuid"){

  if(
    !is.null(obj[["meta"]][["respondent_id_name"]])
  ){

    result <- obj[["meta"]][["respondent_id_name"]]

  }else{

    result <- .default

  }

  return(result)
}




#' set_resp_id_name
#' @description set_resp_id_name
#' @export
set_resp_id_name <- function(obj, resp_id_name = "seg_uuid"){


  obj[["meta"]][["respondent_id_name"]] <- resp_id_name


  return(obj)

}
