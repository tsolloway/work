#' names_clean
#'
#' @description
#' Cleans names of a data frame, tibble, or character vector using `str_scrub()`.
#' Optionally converts names to lowercase.
#'
#' @param x A data frame, tibble, or character vector whose names you want to clean.
#' @param make_lowercase Logical; if TRUE, converts names to lowercase. Default is TRUE.
#'
#' @return
#' - If `x` is a data frame or tibble, returns `x` with cleaned column names.
#' - If `x` is a character vector, returns the cleaned vector.
#'
#' @examples
#' df <- data.frame("Column A" = 1:3, "COLUMN B" = 4:6)
#' names_clean(df)
#' names_clean(c("Column A", "COLUMN B"))
#'
#' @export
names_clean <- function(x, make_lowercase = TRUE) {

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required for 'names_clean'. Please install it.")
  }

  if (tibble::is_tibble(x) || is.data.frame(x)) {
    names(x) <- str_scrub(names(x), make_lowercase = make_lowercase)
    return(x)
  }

  if (is.character(x)) {
    return(str_scrub(x, make_lowercase = make_lowercase))
  }

  stop("Class not supported in 'names_clean'. Only data.frame, tibble, or character vector are allowed.")
}
