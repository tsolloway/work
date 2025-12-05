#' find_file_in_dir
#'
#' @description
#' Recursively searches for files in a specified directory (or the current working directory by default),
#' optionally filtering by a filename pattern and/or file type (e.g., file extension).
#' Returns either normalized absolute paths or relative paths.
#'
#' @param x Character string pattern to match in file names.
#' @param type Optional character string pattern to further filter matched files (e.g., file extension like "csv").
#' @param path Directory path to search. Defaults to current working directory.
#' @param normalize_paths Logical; if TRUE (default), returns absolute normalized paths.
#' @param must_work Logical; passed to `normalizePath()`. If TRUE (default), throws an error if a file does not exist. If FALSE, allows empty or missing files.
#' @return Character vector of file paths matching the patterns.
#' @examples
#' # Find all R scripts in the current directory
#' find_file_in_dir("script", type = "R$")
#'
#' # Find all CSV files containing "data" in the current directory
#' find_file_in_dir("data", type = "csv$")
#' @export
find_file_in_dir <- function(x, type = NULL, path = ".", normalize_paths = TRUE, must_work = TRUE) {

  # List all files recursively
  all_files <- list.files(path = path, recursive = TRUE, full.names = TRUE)

  # Filter by main pattern
  all_files <- all_files[grep(x, all_files, ignore.case = TRUE)]

  # Filter by type if provided
  if(!is.null(type)){
    all_files <- all_files[grep(type, all_files, ignore.case = TRUE)]
  }

  # Normalize paths safely
  if(normalize_paths){
    normalizePath(all_files, mustWork = must_work)
  } else {
    all_files
  }
}
