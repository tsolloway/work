#' install_pkg_local
#' @description Install local R library
#' @param pkg Character. Name or path of local package to install. If characters don't lead to a valid path, the function checks for that package in the set git folder.
#' @param ask Logical. Whether to ask for confirmation when installing a different version of a package that is already installed. Installations that only add new packages never require confirmation.
#' @param upgrade Logical. When FALSE, the default, pak does the minimum amount of work to give you the latest version(s) of pkg. It will only upgrade dependent packages if pkg, or one of their dependencies explicitly require a higher version than what you currently have. It will also prefer a binary package over to source package, even it the binary package is older.
#' @param on_exit_restart Logical. Restart R session of install.
#' @export
install_pkg_local <- function(
    pkg = NULL, ask = FALSE, upgrade = FALSE,
    on_exit_restart = TRUE
){

  if(on_exit_restart) on.exit(work::restart(keep = TRUE))


  work::install_pak()


  pkg_found <- pkg %>% normalizePath() %>% dir.exists() %>% suppressWarnings()



  if( !pkg_found ){
    path_git <- work::get_path("git")

    pkg <- glue::glue("{path_git}/{pkg}")

    pkg_found <- pkg %>% normalizePath() %>% dir.exists() %>% suppressWarnings()
  }



  if( pkg_found ){

    pkg %>% devtools::document()

    tryCatch(
      {
        pak::local_install(pkg, ask = ask, upgrade = upgrade)
      },
      error = function(e){
        pkg %>% devtools::install(upgrade = upgrade)
      }
    )

  }
}
