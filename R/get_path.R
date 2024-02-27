#' get_path
#' @description Returns R system utility paths
#' @param typer Character. Path type to be returned.
#' @param edit_file Logical. If true, opens a file for editing in RStudio.
#' @export
get_path <- function(
    type = c(
      "home", "r_home", "environment", "profile", "makevars",
      "preference_rstudio", "snippets_r", "git",
      "onedrive", "downloads", "desktop"
    ),
    edit_file = FALSE,
    scope = c("user", "project")
){

  type <- match.arg(type)

  scope <- match.arg(scope)


  path <- switch(
    type,
    "home" = Sys.getenv("HOME"),
    "r_home" = Sys.getenv("R_HOME"),
    "environment" = usethis:::scoped_path_r(scope, ".Renviron", envvar = "R_ENVIRON_USER"),
    "profile" = usethis:::scoped_path_r(scope, ".Rprofile", envvar = "R_ENVIRON_USER"),
    "makevars" = usethis:::scoped_path_r(scope, ".R", "Makevars"),
    "preference_rstudio" = usethis:::rstudio_config_path("rstudio-prefs.json"),
    "snippets_r" = usethis:::rstudio_config_path("snippets", "r.snippets"),
    "git" = Sys.getenv("path_git_directory"),

    "onedrive" = {

      if( get_os() == "windows" ){
        temp <- shell('Dir "%OneDrive%"', intern = TRUE)

        temp[grepl("Directory of", temp, ignore.case = TRUE)] %>%
          gsub("Directory of", "", ., ignore.case = TRUE) %>%
          trimws()
      }
    },

    "downloads" = {
      if( get_os() == "windows" ){

        temp <- shell('Dir "%userprofile%/Downloads"', intern = TRUE)

        temp[grepl("Directory of", temp, ignore.case = TRUE)] %>%
          gsub("Directory of", "", ., ignore.case = TRUE) %>%
          trimws()
      }
    },

    "desktop" = {
      if( get_os() == "windows" ){

        temp <- shell('Dir "%OneDrive%/Desktop"', intern = TRUE)

        temp[grepl("Directory of", temp, ignore.case = TRUE)] %>%
          gsub("Directory of", "", ., ignore.case = TRUE) %>%
          trimws()
      }
    }
  )


  if(
    (
      type == "environment" || type == "profile" ||
      type == "preference_rstudio" || type == "snippets_r"
    ) &&
    !file.exists(path)
  ){

    path %>% file.create()

  }



  if( type == "git" && ( !is_truthy(path) || !dir.exists(path) ) ){

    stop("git path not set or is not valid. Please use `work::set_r_environment('git_local_dir')`")

  }else{

    path <- path %>% normalizePath(mustWork = TRUE, winslash = "/")
  }



  if(edit_file && type != "home" && type != "r_home"){

    path %>% usethis::edit_file()

  }else{

    return(path)
  }
}
