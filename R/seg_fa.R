#' seg_fa
#' @description seg_fa
#' @export
seg_fa <- function(seg, where = NULL){

  if(is.null(where)) where <- seg[["paths"]][["folders"]][["process"]]

  fa_analysis(
    df = seg[["df"]],
    vars = seg[["input_table"]][["factor_var"]] %>% unlist(),
    labels = seg[["input_table"]] %>% select(factor_var, source_label) %>% set_names(c("variable", "label"))
  ) %>%
    fa_write(where = where)
}
