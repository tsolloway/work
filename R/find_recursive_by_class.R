#' Recursively Search Nested Lists for a Specific Object Type
#'
#' @description
#' Recursively searches through a (potentially deeply nested) list
#' to find the first element that inherits from a specified class.
#' This is useful for extracting a specific object (e.g., `"bn"`, `"bn.fit"`,
#' `"igraph"`) from complex or layered result structures.
#'
#' @param x Object or list to search through.
#' @param what Character scalar. Class name to search for
#'   (e.g., `"bn"` or `"bn.fit"`).
#' @param max_depth Positive integer or `Inf`. Maximum recursion depth.
#'   Prevents infinite recursion in self-referential or excessively
#'   nested lists. Defaults to `Inf`, meaning no limit.
#' @param depth Internal parameter used during recursion (do not modify).
#'
#' @details
#' The function performs a **depth-first search**, exploring each element
#' of the list recursively until it finds an object whose class inherits
#' from the specified value of `what`.
#'
#' - **Class-based search:** The function matches objects by class
#'   (via `inherits()`), *not* by name. For example, if an element is named
#'   `"bn"`, that alone does not qualify; it must actually be an object of
#'   class `"bn"`.
#'
#' - **Depth control:** The `max_depth` argument controls how far the function
#'   can recurse. This prevents infinite recursion in cases of cyclic references
#'   or extremely deep nesting. If `max_depth` is larger than the actual list
#'   depth, the function behaves normally and simply completes without hitting
#'   the limit.
#'
#' - **Error safety:** Each recursive call is wrapped in a `tryCatch()` to avoid
#'   stopping on malformed or inaccessible elements.
#'
#' If no matching object is found, the function returns `NULL`.
#'
#' @return
#' The first object found that inherits from the specified class,
#' or `NULL` if no such object exists within the specified `max_depth`.
#'
#' @examples
#' \dontrun{
#' library(bnlearn)
#'
#' bn <- bnlearn::hc(iris)
#' nested <- list(a = list(b = list(model = bn)))
#'
#' # Find the first bn object in the nested list
#' find_recursive_by_class(nested, what = "bn")
#'
#' # Limit search to depth 1 (will return NULL)
#' find_recursive_by_class(nested, what = "bn", max_depth = 1)
#'
#' # This will NOT match by name only
#' fake <- list(bn = "not_a_bn_object")
#' find_recursive_by_class(fake, what = "bn") # returns NULL
#' }
#'
#' @export
find_recursive_by_class <- function(x, what = "bn", max_depth = Inf, depth = 0) {

  # ---- validate inputs ----
  if (!is.character(what) || length(what) != 1) {
    stop("`what` must be a single character string specifying a class name.")
  }
  if (!is.numeric(max_depth) || length(max_depth) != 1 || max_depth <= 0) {
    stop("`max_depth` must be a positive number or Inf.")
  }

  # ---- stop if depth limit reached ----
  if (depth > max_depth) return(NULL)

  # ---- base case ----
  if (inherits(x, what)) return(x)

  # ---- recursive case ----
  if (is.list(x)) {
    for (el in x) {
      res <- tryCatch(
        find_recursive_by_class(el, what = what, max_depth = max_depth, depth = depth + 1),
        error = function(e) NULL
      )
      if (!is.null(res)) return(res)
    }
  }

  # ---- not found ----
  return(NULL)
}
