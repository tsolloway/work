#' seg_get_fa_winner
#' @description seg_get_fa_winner
#' @export
seg_get_fa_winner <- function(seg, winner, row_header = 4, file_location = NULL){

  require(openxlsx)


  polars_table <- seg[["spec"]][["polars_table"]]


  if(is.null(file_location)){
    file_location <- seg[["paths"]][["files"]][["fa"]]
  }


  df <- read.xlsx(
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
      by = join_by(rs_var == fa_var)
    )


  seg[["input_sheet"]][["input_fa_table"]] <- df


  return(seg)
}
