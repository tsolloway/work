#' seg_pca
#'
#' @description Runs principal components analysis on the polar shell variables,
#'   writes the results to an Excel workbook, and stores the file path in the
#'   seg object.
#'
#' @param seg A seg object with shell data populated.
#' @param vars Character vector of variable names to analyze. If `NULL`
#'   (default), uses all RS polar variables from the spec.
#' @param clean_max Numeric. Loading threshold below which variables are
#'   considered clean (default: `0.25`).
#' @param where Character. Output directory. Defaults to the process folder in
#'   the seg object.
#' @param file_name Character. Base file name (default: `"Factor Analysis"`).
#' @param rotation Character. Rotation method passed to [psych::principal()]
#'   (default: `"equamax"`).
#' @param return_object Logical. If `TRUE`, stores the full PCA analysis object
#'   in `seg[["pca_analysis"]]` (default: `FALSE`).
#' @param add_proj_name_to_file Logical. If `TRUE` (default), prepends the
#'   project name to the file name.
#'
#' @return The seg object with `seg[["paths"]][["files"]][["pca"]]` set.
#'
#' @export
seg_pca <- function(
    seg,
    vars = NULL,
    polar_type = c("rs", "source"),
    clean_max = .25,
    where = NULL,
    file_name = "Factor Analysis",
    rotation = c(
      "equamax", "varimax", "quartimax", "bentlerT", "varimin", "geominT", "bifactor",
      "Promax", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin",
      "none"
    ),
    return_object = FALSE,
    add_proj_name_to_file = TRUE
){

  polar_type <- match.arg(polar_type)

  if(is.null(where)){
    where <- seg[["paths"]][["folders"]][["process"]]

    if(is.null(where)){
      where <- getwd()
    }
  }


  var_col <- if (polar_type == "rs") "rs_var" else "source_var"

  if(is.null(vars)){
    vars = seg[["spec"]][["polars_table"]][[var_col]] %>% unlist()
  }


  pca_analysis_object <- pca_analysis(
    df = seg[["data"]][["with_shell"]],
    vars = vars,
    labels = seg[["spec"]][["polars_table"]] %>% select(all_of(var_col), source_label) %>% set_names(c("variable", "label")),
    rotation = rotation
  )



  if(add_proj_name_to_file){
    file_name <- seg_glue_proj_name_to_file(seg, file_name)
  }



  file_location <-  pca_analysis_object %>%
    pca_write(where = where, clean_max = clean_max, file_name = file_name, return_location = TRUE)



  seg[["paths"]][["files"]][["pca"]] <- file_location


  if(return_object){
    seg[["pca_analysis"]] <- pca_analysis_object
  }


  return(seg)
}
