#' Load a package or optionally return FALSE if not installed
#'
#' @param pkg Character. Package name to load.
#' @param quietly Logical. Passed to `library()`. Default TRUE.
#' @param stop_if_missing Logical. If TRUE (default), stops if the package is missing.
#'   If FALSE, returns FALSE instead of stopping.
#' @return Invisibly returns TRUE if the package is loaded. Returns FALSE if missing and stop_if_missing = FALSE.
#' @examples
#' \dontrun{
#' load_or_stop("dplyr")
#' load_or_stop("nonexistentpkg", stop_if_missing = FALSE)
#' }
load_or_stop <- function(pkg, quietly = TRUE, stop_if_missing = TRUE) {

  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (stop_if_missing) {
      stop(glue::glue("Package '{pkg}' is not installed. Please install it."))
    } else {
      return(FALSE)
    }
  }

  # Load the package
  library(pkg, character.only = TRUE, quietly = quietly, warn.conflicts = !quietly)

  invisible(TRUE)
}
