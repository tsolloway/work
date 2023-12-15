#' install_pkg
#' @description Install R libraries
#' @param x Character vector. Vector of libraries to install
#' @param ask Logical. Whether to ask for confirmation when installing a different version of a package that is already installed. Installations that only add new packages never require confirmation.
#' @param upgrade Logical. When FALSE, the default, pak does the minimum amount of work to give you the latest version(s) of pkg. It will only upgrade dependent packages if pkg, or one of their dependencies explicitly require a higher version than what you currently have. It will also prefer a binary package over to source package, even it the binary package is older.
#' @param update_pkgs Logical. Update all current packages.
#' @param on_exit_restart Logical. Restart R session of install.
#' @export
install_pkg <- function(
    x = NULL, ask = FALSE, upgrade = FALSE,
    update_pkgs = FALSE, on_exit_restart = TRUE
){

  if(on_exit_restart) on.exit(work::restart(keep = TRUE))

  work::install_pak()


  # package organization
  if( update_pkgs && is.null(x) ){

    x <- old.packages() %>% as.data.frame() %>% .[["Package"]]

  }else if( update_pkgs && !is.null(x) ){

    pkgs_to_update <- old.packages() %>% as.data.frame() %>% .[["Package"]]

    x <- c(x, pkgs_to_update) %>% unique()

  }else if( !update_pkgs && is.null(x)  ){

    stop("No packages identified")

  }


  # internal function
  try_install_pkg <- function(x, ask, upgrade){

    tryCatch(

      {
        pak::pkg_install(x, ask = ask, upgrade = upgrade)
        return(TRUE)
      },

      error = function(e){

        error_message <- e[["parent"]][["message"]]

        if( grepl("from", error_message) ){

          error_pkg <- error_message %>% strsplit("from") %>% unlist() %>%
            head(1) %>% trimws() %>% strsplit(" ") %>% unlist() %>% tail(1) %>%
            gsub("'", "", .)

        }else if( grepl("compilation failed for package", error_message) ){

          error_pkg <- error_message %>% strsplit("compilation failed for package") %>%
            unlist() %>% tail(1) %>% strsplit("/") %>% unlist() %>% tail(1) %>%
            gsub("[[:punct:]]", "", .)

        }else if( grepl("Can't find package called", error_message) ){

          error_pkg <- error_message %>% strsplit("Can't find package called") %>%
            unlist() %>% tail(1) %>% trimws() %>% substr(1, nchar(.)-1)

        }else if( grepl("rJava", error_message) ){

          warning("Install rJava outside of this wrapper")

        }

        tryCatch(
          error_pkg %>% install.packages(error_pkg, type = "source", update = TRUE, dependencies = TRUE) ,
          error = tryCatch(
            install.packages(error_pkg, type = "binary", update = TRUE, dependencies = TRUE) ,
            error = return(error_pkg)
          )
        )
      }
    )
  }


  #setup
  install_done <- FALSE
  failed_pkgs <- c()
  next_try_pkgs <- x


  if( length(next_try_pkgs) == 0 || all(is.na(next_try_pkgs)) ){
    install_done <- TRUE
  }


  # continue to try pak through errors
  while( !isTRUE(install_done) ){

    this_try_pkgs <- next_try_pkgs

    install_done <- try_install_pkg(x = this_try_pkgs, ask = ask, upgrade = upgrade)

    if( !isTRUE(install_done) ){

      failed_pkgs <- c(failed_pkgs, this_try_pkgs[this_try_pkgs %in% install_done]) %>% unique()
      next_try_pkgs <- this_try_pkgs[!this_try_pkgs %in% install_done]


      if( identical(next_try_pkgs, failed_pkgs) ){

        install_done <- TRUE

      }else if( length(next_try_pkgs) == 0 || all(is.na(next_try_pkgs)) || is.null(next_try_pkgs) ){

        install_done <- TRUE

      }else if( identical(next_try_pkgs, this_try_pkgs) ){

        failed_pkgs <- c(failed_pkgs, next_try_pkgs[1]) %>% unique()
        next_try_pkgs <- next_try_pkgs[-1]


        if( length(next_try_pkgs) == 0 ){

          install_done <- TRUE

        }else if( length(next_try_pkgs) > 0 ){

          install_done <- FALSE

        }

      }else{
        install_done <- FALSE
      }
    }
  }


  # attempt our try one more time, but this time one at a time.
  if( length(failed_pkgs) > 0 ){

    second_attempt_failure_pkgs <- c()

    for(i in failed_pkgs){

      second_attempt_success <- try_install_pkg(x = i, ask = ask, upgrade = upgrade)

      if( !isTRUE(second_attempt_success) ) second_attempt_failure_pkgs <- c(second_attempt_failure_pkgs, i) %>% unique()

    }

    if( length(second_attempt_failure_pkgs) > 0 ){

      warning_message <- glue::glue("The following packages didn't install: {glue::glue_collapse(second_attempt_failure_pkgs, sep = ', ', last = ' and ')}") %>% as.character()

      warning(warning_message)

      return(second_attempt_failure_pkgs)

    }else{
      return(TRUE)
    }

  }else{
    return(TRUE)
  }
}


