#' Retrieve an environment key with optional interactive prompt
#'
#' @description
#' Attempts to retrieve a key from the system environment using `Sys.getenv()`.
#' If the key is not found or is empty, it prompts the user to enter it securely via `rstudioapi::askForPassword()`.
#'
#' @param x Character. The name of the environment variable to retrieve.
#'
#' @return Character string containing the key.
#'
#' @examples
#' \dontrun{
#' api_key <- get_environment_key("MY_API_KEY")
#' }
#'
#' @export
get_environment_key <- function(x) {

  key <- Sys.getenv(x)

  if (!is_truthy(key)) {
    if (!requireNamespace("rstudioapi", quietly = TRUE)) {
      stop("Package 'rstudioapi' is required to prompt for a key interactively.")
    }
    key <- rstudioapi::askForPassword(paste0("Enter the key for '", x, "': "))
  }

  return(key)
}
