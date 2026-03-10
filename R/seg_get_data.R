#' seg_get_data
#'
#' @description Reads a data file into the seg object, generates a unique
#'   respondent ID column, and optionally stores dictionary metadata.
#'
#' @param seg A seg object (from [seg_init()]).
#' @param data_path Character. Path to the data file (`.sav`, `.csv`, `.xlsx`).
#' @param weight Character. Name of the weight variable column, or `NULL` for
#'   unweighted (default).
#' @param id_name Character. Name of the auto-generated respondent ID column
#'   (default: `"seg_uuid"`).
#'
#' @return The seg object with `seg[["data"]][["original"]]` populated.
#'
#' @export
seg_get_data <- function(seg, data_path, weight = NULL, id_name = "seg_uuid"){


  data_path <- data_path %>% normalizePath(mustWork = TRUE)


  seg[["data"]][["original"]] <- list()


  dfx <- data_path %>%
    read(
      clean_col_names = FALSE,
      hard_stop = TRUE
    )


  if(!is.null(dfx[["dictionary"]])){
    seg[["data"]][["original_dictionary"]] <- dfx[["dictionary"]] %>%
      bind_rows(
        tibble(
          pos = 0, "variable" = !!id_name, "label" = !!id_name,
          "col_type" = "chr", "missing" = 0
        ),
        .
      )
  }


  if(!is.null(dfx[["df"]])){
    seg[["data"]][["original"]] <- dfx[["df"]] %>% add_uuid(id_name)
  }else{
    seg[["data"]][["original"]] <- dfx %>% add_uuid(id_name)
  }



  seg[["paths"]][["files"]][["data"]] <- data_path


  if(!is.null(weight)){
    seg[["meta"]][["weight_variable"]] <- weight
  }


  if(!is.null(id_name)){
    seg[["meta"]][["id_variable"]] <- id_name
  }


  return(seg)
}
