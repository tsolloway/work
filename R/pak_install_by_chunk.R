#' Install packages in chunks
#'
#' @description Installs a vector of package names in chunks to avoid long install commands.
#' @param pkgs Character vector of package names.
#' @param nchunk Integer. Number of packages per chunk (default = 10).
#' @return NULL, called for side effects.
#' @export
#' @examples
#' pak_install_by_chunk(c("dplyr", "ggplot2", "purrr"), nchunk = 2)
pak_install_by_chunk <- function(pkgs, nchunk = 10) {
  if (length(pkgs) == 0) return(invisible(NULL))

  chunks <- split(pkgs, ceiling(seq_along(pkgs)/nchunk))

  purrr::walk(chunks, ~ tryCatch(
    pak::pkg_install(.x),
    error = function(e) message("Failed to install chunk: ", paste(.x, collapse = ", "))
  ))

  invisible(NULL)
}
