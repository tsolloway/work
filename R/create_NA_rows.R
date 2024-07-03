#' create_NA_rows
#' @description Returns empty rows
#' @param df data.frame / tibble
#' @param n number of rows
#' @export
create_NA_rows <- function(df, n){
  purrr::map(seq(n), function(x)set_names(rep(NA, ncol(df)), colnames(df))) %>%
    dplyr::bind_rows()
}


#' add_NA_rows
#' @description Returns empty rows
#' @param df data.frame / tibble
#' @param n number of rows
#' @export
add_NA_rows <- function(df, n){
  dplyr::bind_rows(
    df,
    create_NA_rows(df, n)
  )
}
