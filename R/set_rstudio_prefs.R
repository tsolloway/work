#' set_rstudio_prefs
#' @description setup the local rstudio preferences file
#' @export
set_rstudio_prefs <- function(path = NULL, on_exit_restart = TRUE){

  if(on_exit_restart) on.exit(work::restart(keep = TRUE))

  if( is.null(path) ){
    path <- "rstudio-prefs.json" %>% usethis:::rstudio_config_path() %>% normalizePath(mustWork = TRUE)
  }else if( !is.null(path) ){
    path <- path %>% normalizePath(mustWork = TRUE)
  }


  old_lines <- readLines(path) %>% suppressWarnings() %>% jsonlite::fromJSON()

  init_lines <- list(
    jobs_tab_visibility = "shown",
    initial_working_directory = "~/Desktop",
    editor_theme = "Vibrant Ink",
    source_with_echo = TRUE,
    auto_detect_indentation = TRUE,
    margin_column = 80,
    highlight_r_function_calls =TRUE,
    rainbow_parentheses = TRUE
  )


  old_lines <- old_lines[!names(old_lines) %in% names(init_lines)]

  new_lines <- base::c(old_lines, init_lines) %>% jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  writeLines(new_lines, path)
}
