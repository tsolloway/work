#' set_r_environment
#' @description Set the R environment
#' @param x Character vector. Lines to add to the R environment file.
#' @param type Type of set to do
#' @export
set_r_environment <- function(
    type = c("custom", "git_local_dir"), x = NULL, overwrite = FALSE, on_exit_restart = TRUE
){

  type <- match.arg(type)

  if( on_exit_restart ) on.exit( work::restart(keep = TRUE) )

  path <- work::get_path("environment")

  if( is.null(path) ) stop("Couldn't find Renviron path")

  previous_lines <- readLines(path) %>% stringi::stri_remove_empty()

  previous_lines_vars <- previous_lines %>% strsplit("=") %>% purrr::map(head(1)) %>% unlist()


  if( type == "custom" && !is_truthy(x) ){
    stop("Custom type while parameter x is empty.")
  }


  if( type == "git_local_dir"){

    git_dir_exists <- FALSE

    if( is_truthy(x) ){

      git_dir_exists <- x %>% normalizePath(winslash = "/") %>% dir.exists() %>% suppressWarnings()

      if( git_dir_exists ){

        x <- x %>% normalizePath(winslash = "/")

      }else if( !git_dir_exists ){

        warning("Provided local git path does not exist.")
      }
    }


    if( !git_dir_exists ){

      x <- rstudioapi::showPrompt("Local git dir", "Path to local git respository") %>%
        normalizePath(winslash = "/") %>% suppressWarnings()

      git_dir_exists <- x %>% normalizePath(winslash = "/") %>% dir.exists() %>% suppressWarnings()
    }


    if( git_dir_exists ){
      x <- glue::glue("path_git_directory={x}") %>% as.character()
    }
  }


  new_lines <- x %>% stringi::stri_remove_empty()


  if( overwrite ){

    new_vars <- new_lines %>% strsplit("=") %>% purrr::map(head(1)) %>% unlist()
    previous_lines <- previous_lines[!previous_lines_vars %in% new_vars]
    new_lines <- base::c(previous_lines, new_lines)

  }else if( !overwrite ){

    new_vars <- new_lines %>% strsplit("=") %>% purrr::map(head(1)) %>% unlist()
    new_lines <- new_lines[!new_vars %in% previous_lines_vars]
    new_lines <- base::c(previous_lines, new_lines)

  }

  writeLines(new_lines, path)
}
