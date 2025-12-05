#' object_name
#'
#' @description Returns the name of the object passed to a function, pipe-safe.
#' Handles direct calls, magrittr pipes, and character literals.
#'
#' @param x Any R object (variable, literal, or piped object).
#'
#' @return A character string with the name of the object or value.
#'
#' @examples
#' df <- iris
#' object_name(df)
#' #> "df"
#'
#' object_name("text")
#' #> "text"
#'
#' df %>% object_name()
#' #> "df"
#'
#' iris %>% head() %>% object_name()
#' #> "iris"
#'
#' mtcars %>% head() %>% object_name()
#' #> "mtcars"
#'
#' @export
object_name <- function(x) {
  nm <- deparse(substitute(x))  # Capture expression as string

  # Handle magrittr pipe
  if (nm %in% c(".", "")) {
    sc <- sys.calls()
    prev <- tail(sc, n = 2)[[1]]  # Call before this function
    if (length(prev) > 1) {
      nm <- deparse(prev[[2]])
    } else {
      nm <- "."
    }
  }

  # Remove surrounding quotes for literals
  if (startsWith(nm, "\"") && endsWith(nm, "\"")) {
    nm <- substr(nm, 2, nchar(nm) - 1)
  }

  # If nm contains a pipe, keep only the first part
  if (grepl("%>%", nm)) {
    nm <- strsplit(nm, "%>%")[[1]][1] %>% trimws()
  }

  nm
}
