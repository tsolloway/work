#' find_file_in_dir
#' @description find_file_in_dir
#' @export
find_file_in_dir <- function(x, type = NULL){
  all_files <- list.files(recursive = TRUE, full.names = TRUE)

  all_files <- all_files %>% grep(x, ., ignore.case = T) %>% all_files[.] %>% normalizePath()

  if(!is.null(type)){
    all_files %>% grep(type, ., ignore.case = T) %>% all_files[.]
  }
}
