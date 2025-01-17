#' insert_missing_column
#' @description insert_missing_column
#' @examples
#' iris %>% insert_missing_column(Sepal.Length, mom, boo) %>% head()
#' @export
insert_missing_column <- function(.data, ...){

  x <- rlang::enquos(...)

  old_columns <- purrr::map(x, rlang::quo_get_expr) %>% unlist()

  missing_columns <- old_columns[!old_columns %in% names(.data)]

  if( length(missing_columns) > 0 ){
    for(i in missing_columns) .data[[i]] <- NA
  }

  .data
}
