
#' install_pak
#' @description Checks for if pak is installed. If not, it installs it.
#' @param update Logical value. If pak is installed, should it be updated if update is available?
#' @export
install_pak <- function(update = TRUE){

  yes_installed <- suppressMessages(library("pak", logical.return = TRUE))

  if( yes_installed && update ){

    tryCatch(
      suppressMessages(pak::pak_update(stream = "stable")), error = function(e) TRUE
    )

  }else if( !yes_installed ){

    install.packages("pak", type = "binary", update = TRUE, dependencies = TRUE)

    yes_installed <- suppressMessages(library("pak", logical.return = TRUE))

  }

  return( yes_installed )
}
