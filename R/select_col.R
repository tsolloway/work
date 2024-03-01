#' select
#' @description select
#' @examples
#' iris %>% select_col(Species, hi, Petal.Length) %>% head()
#' @export
select_col <- function(.data, ...){

  .data %>% insert_missing_column(...) %>% dplyr::select(...)

}
