#' seg_fa
#' @description seg_fa
#' @export
seg_fa <- function(
    seg,
    clean_max = .25,
    where = NULL,
    file_name = "Factor Analysis",
    method = "pa",
    rotation = c(
      "equamax", "varimax", "quartimax", "bentlerT", "varimin", "geominT", "bifactor",
      "Promax", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin",
      "none"
    ),
    return_object = FALSE,
    add_proj_name_to_file = TRUE
){

  if(is.null(where)){

    where <- seg[["paths"]][["folders"]][["process"]]

    if(is.null(where)){
      where <- getwd()
    }

  }


  fa_analysis_object <- fa_analysis(
    df = seg[["data"]][["with_shell"]],
    vars = seg[["spec"]][["polars_table"]][["rs_var"]] %>% unlist(),
    labels = seg[["spec"]][["polars_table"]] %>% select(rs_var, source_label) %>% set_names(c("variable", "label")),
    method = method,
    rotation = rotation
  )



  if(add_proj_name_to_file){
    file_name <- seg_glue_proj_name_to_file(seg, file_name)
  }



  file_location <-  fa_analysis_object %>%
    fa_write(where = where, clean_max = clean_max, file_name = file_name, return_location = TRUE)



  seg[["paths"]][["files"]][["fa"]] <- file_location


  if(return_object){
    seg[["fa_analysis"]] <- fa_analysis_object
  }


  return(seg)
}
