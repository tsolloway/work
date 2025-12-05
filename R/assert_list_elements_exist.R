#' Assert that required elements exist in a list
#'
#' @description
#' Checks whether all specified element names exist within a given list.
#' Stops with an informative message if any required elements are missing.
#'
#' @param x A list to check.
#' @param elements Character vector of element names expected to be present in `x`.
#' @param context Optional character string providing context for the error message.
#'
#' @return Invisibly returns `TRUE` if all elements are present.
#'
#' @examples
#' lst <- list(a = 1, b = 2)
#' assert_list_elements_exist(lst, c("a", "b"))
#'
#' \dontrun{
#' assert_list_elements_exist(lst, c("a", "c"))  # Throws error
#' }
#'
#' @export
assert_list_elements_exist <- function(x, elements, context = "list") {
  if (!is.list(x)) {
    stop(glue::glue("Expected a list for {context}, got {class(x)[1]} instead."))
  }

  missing <- setdiff(elements, names(x))
  if (length(missing) > 0) {
    stop(glue::glue(
      "The following required element(s) are missing from {context}: {paste(missing, collapse = ', ')}"
    ))
  }

  invisible(TRUE)
}
