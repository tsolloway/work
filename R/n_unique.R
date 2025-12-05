#' top_n_unique
#'
#' @description Returns the n largest unique, non-NA values of a vector.
#'
#' @param x A numeric or character vector.
#' @param n Number of top values to return.
#'
#' @return A vector of length up to `n` with the largest unique values.
#'
#' @examples
#' top_n_unique(c(3, 1, 4, 4, NA, 2), n = 3)
#' top_n_unique(c("b", "a", "c", "c"), n = 2)
#'
#' @export
top_n_unique <- function(x, n = 2) {
  x %>%
    .[!is.na(.)] %>%
    unique() %>%
    sort() %>%
    tail(n)
}



#' bottom_n_unique
#'
#' @description Returns the n smallest unique, non-NA values of a vector.
#'
#' @param x A numeric or character vector.
#' @param n Number of bottom values to return.
#'
#' @return A vector of length up to `n` with the smallest unique values.
#'
#' @examples
#' bottom_n_unique(c(3, 1, 4, 4, NA, 2), n = 3)
#' bottom_n_unique(c("b", "a", "c", "c"), n = 2)
#'
#' @export
bottom_n_unique <- function(x, n = 2) {
  x %>%
    .[!is.na(.)] %>%
    unique() %>%
    sort() %>%
    head(n)
}



#' top2
#'
#' @description Returns the two largest unique, non-NA values of a vector.
#'
#' @param x A numeric or character vector.
#'
#' @return A vector of length up to 2 with the largest unique values.
#'
#' @examples
#' top2(c(3, 1, 4, 4, NA, 2))
#' top2(c("b", "a", "c", "c"))
#'
#' @export
top2 <- function(x) {
  top_n_unique(x, n = 2)
}




#' bottom2
#'
#' @description Returns the two smallest unique, non-NA values of a vector.
#'
#' @param x A numeric or character vector.
#'
#' @return A vector of length up to 2 with the smallest unique values.
#'
#' @examples
#' bottom2(c(3, 1, 4, 4, NA, 2))
#' bottom2(c("b", "a", "c", "c"))
#'
#' @export
bottom2 <- function(x) {
  bottom_n_unique(x, n = 2)
}

