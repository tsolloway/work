#' create_NA_rows
#'
#' @description Returns a data frame / tibble with n rows of NA matching the columns of df.
#' @param df Data frame or tibble.
#' @param n Number of rows to create.
#' @return A data frame with n rows of NA.
#' @examples
#' create_NA_rows(iris, 2)
#' @export
create_NA_rows <- function(df, n) {
  if (n <= 0) return(df[0, , drop = FALSE])

  replicate(
    n,
    as.list(rep(NA, ncol(df))),
    simplify = FALSE
  ) %>%
    purrr::map_dfr(~set_names(.x, colnames(df)))
}



#' add_NA_rows
#'
#' @description Appends n empty rows (NA) to a data frame / tibble.
#' @param df Data frame or tibble.
#' @param n Number of rows to append.
#' @return Original data frame with n NA rows added at the bottom.
#' @examples
#' add_NA_rows(iris, 3)
#' @export
add_NA_rows <- function(df, n) {
  dplyr::bind_rows(df, create_NA_rows(df, n))
}
