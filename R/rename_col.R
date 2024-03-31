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
rename_col <- function(.data, ..., .select = FALSE, .distinct = FALSE){

  x <- .data %>% insert_missing_column(...) %>% dplyr::rename(...)


  if(.select){
    x_order <- rlang::enquos(...) %>% purrr::map(rlang::quo_get_expr) %>% names()

    x <- x %>% dplyr::select(dplyr::all_of(x_order))
  }


  if(.distinct){
    x <- x %>% dplyr::distinct()
  }


  return(x)
}
