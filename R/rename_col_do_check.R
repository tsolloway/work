#' rename_col_do_check
#' @description check that a column was renames
#' @export
rename_col_do_check <- function(df, y, x, hard_stop = TRUE){

  if( !col_check(df, y, hard_stop = FALSE) && col_check(df, x, hard_stop = FALSE) ){

    df <- df %>% rename_col(!!y := !!x)
  }

  col_check(df, y, hard_stop = hard_stop)

  return(df)
}
