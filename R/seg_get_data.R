#' seg_get_data
#' @description seg_get_data
#' @export
seg_get_data <- function(seg, data_path, weight = NULL, id_name = "seg_uuid"){


  data_path <- data_path %>% normalizePath(mustWork = TRUE)


  seg[["data"]][["original"]] <- data_path %>%
    read(
      clean_col_names = FALSE,
      hard_stop = TRUE
    ) %>%
    add_uuid(id_name)


  seg[["paths"]][["files"]][["data"]] <- data_path


  if(!is.null(weight)){
    seg[["meta"]][["weight_variable"]] <- weight
  }


  if(!is.null(weight)){
    seg[["meta"]][["id_variable"]] <- id_name
  }


  return(seg)
}
