#' get_os
#' @description Returns the operating system type as "windows", "macos", "linux", or "unknown"
#' @export
get_os <- function() {
  sys <- Sys.info()[["sysname"]]
  if (.Platform$OS.type == "windows") {
    "windows"
  } else if (sys == "Darwin") {
    "macos"
  } else if (sys == "Linux") {
    "linux"
  } else {
    "unknown"
  }
}
