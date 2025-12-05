#' install_pak
#' @description Checks if pak is installed. If not, installs it. Optionally updates pak if already installed.
#' @param update Logical. If TRUE, updates pak if installed.
#' @return Logical: TRUE if pak is installed successfully, FALSE otherwise.
#' @export
install_pak <- function(update = TRUE) {

  yes_installed <- suppressMessages(requireNamespace("pak", quietly = TRUE))

  if (yes_installed && update) {
    tryCatch(
      suppressMessages(pak::pak_update(stream = "stable")),
      error = function(e) message("pak update failed: ", e$message)
    )
  } else if (!yes_installed) {
    install.packages("pak", dependencies = TRUE)
    yes_installed <- suppressMessages(requireNamespace("pak", quietly = TRUE))
  }

  yes_installed
}
