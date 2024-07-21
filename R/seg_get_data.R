#' seg_get_data
#' @description seg_get_data
#' @export
seg_get_data <- function(seg, data_path, weight = NULL){

  seg[["paths"]][["files"]][["data"]] <- data_path %>% normalizePath(mustWork = TRUE)

  seg[["df"]] <- read_xl(data_path, clean_col_names = FALSE)

  if(!is.null(weight)){
    seg[["meta"]][["weight_variable"]] <- weight
  }

  seg
}
