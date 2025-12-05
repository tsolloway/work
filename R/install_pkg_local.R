#' install_pkg_local
#'
#' @description Installs a local R package from a specified path or git folder.
#' Documents the package before installation. Falls back to devtools if pak fails.
#' @param pkg Character. Name or path of local package. If path does not exist, function looks for it in the git folder.
#' @param ask Logical. Ask confirmation when installing a different version of a package already installed (default FALSE).
#' @param upgrade Logical. Controls upgrade behavior in pak (default FALSE).
#' @param on_exit_restart Logical. Restart R session after installation (default TRUE).
#' @return Invisibly TRUE if installation attempted.
#' @export
install_pkg_local <- function(
    pkg = NULL, ask = FALSE, upgrade = FALSE,
    on_exit_restart = TRUE
) {

  if (on_exit_restart) on.exit(if (interactive()) rstudioapi::restartSession(TRUE))

  install_pak()

  # Check if pkg is a valid path
  pkg_path <- tryCatch(normalizePath(pkg, mustWork = FALSE), error = function(e) NA_character_)
  pkg_found <- !is.na(pkg_path) && dir.exists(pkg_path)

  # If not found, try in git folder
  if (!pkg_found) {
    path_git <- get_path("git")
    pkg_path <- file.path(path_git, pkg)
    pkg_path <- tryCatch(normalizePath(pkg_path, mustWork = FALSE), error = function(e) NA_character_)
    pkg_found <- !is.na(pkg_path) && dir.exists(pkg_path)
  }

  if (!pkg_found) {
    message("Package path not found: ", pkg)
    return(invisible(FALSE))
  }

  # Document package
  devtools::document(pkg_path)

  # Try pak first, fallback to devtools
  tryCatch(
    pak::local_install(pkg_path, ask = ask, upgrade = upgrade),
    error = function(e) {
      message("pak installation failed, falling back to devtools...")
      devtools::install(pkg_path, upgrade = upgrade, dependencies = TRUE)
    }
  )

  invisible(TRUE)
}
