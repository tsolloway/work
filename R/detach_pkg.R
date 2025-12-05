#' Safely detach one or more packages
#'
#' @description
#' Detaches the specified packages if they are currently loaded.
#' Uses a safe approach that won't throw errors if a package is not attached.
#'
#' @param pkgs Character vector of package names to detach.
#' @export
detach_pkg <- function(pkgs) {
  stopifnot(is.character(pkgs))  # ensure input is character vector

  for (pkg in pkgs) {
    pkg_search <- paste0("package:", pkg)

    if (pkg_search %in% search()) {
      try(
        detach(pkg_search, character.only = TRUE, unload = TRUE, force = TRUE),
        silent = TRUE
      )
    }
  }

  invisible(NULL)
}
