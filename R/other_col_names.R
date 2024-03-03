#' other_col_names
#' @description other_col_names
#' @examples
#' iris %>% other_col_names(Species, hi, Petal.Length)
#' @export
other_col_names <- function(.data, ...){

  x <- rlang::enquos(...)

  not_col_names <- purrr::map(x, rlang::quo_get_expr) %>% unlist()

  col_names <- .data %>% colnames()

  col_names[!col_names %in% not_col_names]
}

