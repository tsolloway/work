#' get_os
#' @description Returns OS
#' @export
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "windows"
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    "macos"
  } else if (Sys.info()[["sysname"]] == "Linux") {
    "linux"
  } else {
    "unknown"
  }
}
