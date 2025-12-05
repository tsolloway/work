#' Recursively Search Nested Lists for an Object by Name
#'
#' @description
#' Recursively searches through a (potentially deeply nested) list
#' to find the first element whose **name** matches a specified value.
#' This is useful for locating a named object (e.g., `"model"`, `"bn"`, `"network"`)
#' in layered or complex result structures, regardless of its class.
#'
#' @param x Object or list to search through.
#' @param name Character scalar. Name to search for (case-sensitive).
#' @param max_depth Positive integer or `Inf`. Maximum recursion depth.
#'   Prevents infinite recursion in self-referential or excessively nested lists.
#'   Defaults to `Inf`, meaning no limit.
#' @param depth Internal parameter used during recursion (do not modify).
#'
#' @details
#' The function performs a **depth-first search**, inspecting the names
#' of list elements at each level until it finds one matching `name`.
#'
#' - **Name-based search:** The function matches by element **name**, not by class.
#'   For example, if a list has `list(model = bnlearn::hc(iris))`,
#'   you can find it with `find_recursive_by_name(x, "model")` even if
#'   the object class is not `"bn"`.
#'
#' - **Depth control:** The `max_depth` argument prevents infinite recursion
#'   in self-referential or highly nested lists. If `max_depth` exceeds the
#'   actual nesting level, the function behaves normally.
#'
#' - **Error safety:** Each recursive call is wrapped in `tryCatch()` to ensure
#'   the function does not stop on malformed elements.
#'
#' If no element with the specified name is found, the function returns `NULL`.
#'
#' @return
#' The first object whose name matches `name`, or `NULL` if not found
#' within the specified `max_depth`.
#'
#' @examples
#' \dontrun{
#' bn <- bnlearn::hc(iris)
#' nested <- list(a = list(b = list(model = bn)))
#'
#' # Find the element named "model"
#' find_recursive_by_name(nested, name = "model")
#'
#' # Limit search to depth 1 (will return NULL)
#' find_recursive_by_name(nested, name = "model", max_depth = 1)
#'
#' # Will NOT match by class
#' find_recursive_by_name(nested, name = "bn")
#' }
#'
#' @export
find_recursive_by_name <- function(x, name, max_depth = Inf, depth = 0) {

  # ---- validate inputs ----
  if (!is.character(name) || length(name) != 1) {
    stop("`name` must be a single character string specifying the element name to find.")
  }
  if (!is.numeric(max_depth) || length(max_depth) != 1 || max_depth <= 0) {
    stop("`max_depth` must be a positive number or Inf.")
  }

  # ---- stop if depth limit reached ----
  if (depth > max_depth) return(NULL)

  # ---- base case: check if current level contains the name ----
  if (is.list(x) && !is.null(names(x)) && name %in% names(x)) {
    return(x[[name]])
  }

  # ---- recursive case ----
  if (is.list(x)) {
    for (el in x) {
      res <- tryCatch(
        find_recursive_by_name(el, name = name, max_depth = max_depth, depth = depth + 1),
        error = function(e) NULL
      )
      if (!is.null(res)) return(res)
    }
  }

  # ---- not found ----
  return(NULL)
}
