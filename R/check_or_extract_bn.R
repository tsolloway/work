#' Validate or Extract a bnlearn Network from an Object or Nested List
#'
#' @description
#' Ensures the input is a `bnlearn::bn` object, or extracts the first `bn`
#' object found in a list (optionally nested). This is useful for safely
#' passing user inputs to functions that require a fitted Bayesian network.
#'
#' @param obj Object to check or search.
#' @param recursive Logical; if `TRUE`, searches nested lists recursively
#'   for a `bn` object. Default = `FALSE`.
#'
#' @return
#' A `bnlearn::bn` object if found; otherwise, an error is raised.
#'
#' @details
#' - If `obj` is already a `bnlearn::bn` object, it is returned as-is.
#' - If `obj` is a list, the function uses `find_recursive()` to locate
#'   the first element that inherits from `"bn"`.
#' - If no Bayesian network object is found, the function stops with
#'   an informative error message.
#'
#' This function acts as a validation and extraction utility for any function
#' that expects a Bayesian network as input, making it robust to nested
#' return structures (e.g., lists of results from bootstrapping or modeling pipelines).
#'
#' @examples
#' \dontrun{
#' library(bnlearn)
#' bn <- bnlearn::hc(iris)
#'
#' # Case 1: Direct BN
#' check_or_extract_bn(bn)
#'
#' # Case 2: BN inside a simple list
#' check_or_extract_bn(list(model = bn))
#'
#' # Case 3: Deeply nested BN
#' check_or_extract_bn(list(a = list(b = list(model = bn))), recursive = TRUE)
#' }
#'
#' @export
check_or_extract_bn <- function(obj, recursive = FALSE) {

  # ---- case 1: already a bn object ----
  if (inherits(obj, "bn")) return(obj)

  # ---- case 2: list input ----
  if (!is.list(obj)) {
    stop("Input must be a bnlearn::bn object or a list containing one.")
  }

  # ---- find bnlearn network ----
  bn_obj <- work::find_recursive(
    obj,
    x_class = "bn",
    max_depth = if (recursive) Inf else 1
  )

  if (is.null(bn_obj)) {
    stop("No bnlearn::bn object found in the object or nested lists.")
  }

  return(bn_obj)
}
