#' rename_col
#' @description rename_col
#' @examples
#' iris %>% rename_col(
#' s_len = Sepal.Length,
#' tom = mom,
#' p_len = Petal.Length,
#' foo = boo
#' ) %>% head()
#' @export
rename_col <- function(.data, ...){

  .data %>% insert_missing_column(...) %>% dplyr::rename(...)

}
