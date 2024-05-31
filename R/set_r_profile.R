#' set_r_profile
#' @description setup the local r profile file
#' @export
set_r_profile <- function(
    type = c("init", "my","custom"), x = NULL,
    overwrite = TRUE, on_exit_restart = TRUE
){

  if(on_exit_restart) on.exit(work::restart(keep = TRUE))

  type <- match.arg(type)

  path <- work::get_path("profile")

  if( is.null(path) ) stop("Couldn't find Rprofile path")

  old_lines <- readLines(path) %>% work::remove_empty()

  init_lines <- base::c(
    ".First <- function(){",
    "library(magrittr)",
    "}"
  )

  my_lines <- base::c(
    ".First <- function(){",
    "library(magrittr)",
    "library(work)",
    "}"
  )

  custom_lines <- base::c(
    ".First <- function(){",
    x,
    "}"
  )

  new_lines <- switch(
    type,
    init = init_lines,
    my = my_lines,
    custom = custom_lines
  )

  if(!overwrite){
    new_lines <- base::c(old_lines, "", new_lines)
  }

  writeLines(new_lines, path)
}
