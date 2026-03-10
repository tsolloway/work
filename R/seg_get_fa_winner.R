#' seg_get_fa_winner
#'
#' @description Reads the winning PCA factor solution from the PCA Excel
#'   workbook, extracts variable-to-factor assignments and loadings, and stores
#'   the result in the seg object for input sheet generation.
#'
#' @param seg A seg object with spec and PCA file path populated.
#' @param winner Integer. The winning factor solution number (sheet name in the
#'   PCA workbook).
#' @param row_header Integer. Row where the data header starts in the PCA sheet
#'   (default: `4`).
#' @param file_location Character. Path to the PCA Excel file. Defaults to
#'   `seg[["paths"]][["files"]][["pca"]]`.
#'
#' @return The seg object with `seg[["input_sheet"]][["input_fa_table"]]`
#'   populated.
#'
#' @export
seg_get_fa_winner <- function(seg, winner, row_header = 4, file_location = NULL, polar_type = c("rs", "source")){

  polar_type <- match.arg(polar_type)
  polars_table <- seg[["spec"]][["polars_table"]]


  if(is.null(file_location)){
    file_location <- seg[["paths"]][["files"]][["pca"]]
  }


  df <- openxlsx::read.xlsx(
    xlsxFile = file_location,
    sheet = as.character(winner),
    startRow = row_header
  ) %>%
    as_tibble() %>%
    tidyr::fill(Name) %>%
    slice(., -nrow(.))


  factor_colnames <- df %>%
    select(starts_with("F")) %>%
    select(-Factor) %>%
    names()


  for(i in seq(nrow(df))){
    df[i, "loading"] <- df[[i, factor_colnames[df[[i, "Factor"]]]]]
  }


  df <- df %>%
    group_by(Factor) %>%
    summarise(
      max = max(abs(loading))
    ) %>%
    ungroup() %>%
    left_join(df, ., by = join_by(Factor)) %>%
    mutate(
      solution_a = ifelse(abs(loading) == abs(max), "x", NA)
    ) %>%
    rename_col(
      .select = TRUE,
      fa_var = Variable,
      fa_name = Name,
      fa_n = Factor,
      loading = loading,
      solution_a = solution_a
    ) %>%
    left_join(
      polars_table,
      .,
      by = join_by(!!rlang::sym(if (polar_type == "rs") "rs_var" else "source_var") == fa_var)
    )


  seg[["input_sheet"]][["input_fa_table"]] <- df


  return(seg)
}
