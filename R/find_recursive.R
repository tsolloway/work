#' Recursively Search Nested Lists for an Object by Class, Name, or Both
#'
#' @description
#' Recursively searches through a (potentially deeply nested) list
#' to find the first element that either:
#' - Inherits from a specified class (`x_class`),
#' - Has a specified name (`x_name`),
#' - Or satisfies both conditions.
#'
#' Optionally, it can return a logical indicator (`TRUE` / `FALSE`) rather
#' than the object itself, which is useful for validation checks.
#'
#' @param x Object or list to search through.
#' @param x_class Optional character scalar. Class name to search for
#'   (e.g., `"bn"`, `"bn.fit"`). If `NULL`, class is ignored.
#' @param x_name Optional character scalar. Element name to search for.
#'   If `NULL`, name is ignored.
#' @param require_both Logical; if `TRUE`, both class and name must match
#'   for an element to be returned. If `FALSE` (default), either condition
#'   can match.
#' @param max_depth Positive integer or `Inf`. Maximum recursion depth.
#'   Prevents infinite recursion in self-referential or excessively nested lists.
#'   Defaults to `Inf`, meaning no limit.
#' @param return_logical Logical; if `TRUE`, returns `TRUE` if a match is found,
#'   or `FALSE` otherwise, instead of returning the object itself.
#'   Default = `FALSE`.
#' @param depth Internal parameter used during recursion (do not modify).
#'
#' @details
#' The function performs a **depth-first search**, inspecting each list
#' element recursively until it finds one that matches the specified
#' criteria.
#'
#' - **Search by class:** If `x_class` is provided, the function checks whether
#'   each element inherits from that class via `inherits()`.
#'
#' - **Search by name:** If `x_name` is provided, the function checks whether
#'   the current list level contains an element with that name.
#'
#' - **Combined search:** If both `x_class` and `x_name` are supplied, the argument
#'   `require_both` determines whether both conditions must be true (`TRUE`)
#'   or if either suffices (`FALSE`).
#'
#' - **Depth control:** The `max_depth` parameter limits recursion to prevent
#'   infinite loops in cyclic or malformed structures. If it exceeds the actual
#'   depth, the function behaves normally.
#'
#' - **Logical output:** When `return_logical = TRUE`, the function returns
#'   a logical flag instead of the object. This is useful in conditional
#'   checks or validation pipelines.
#'
#' - **Error safety:** Recursive calls are wrapped in `tryCatch()` to avoid
#'   interruption by malformed or inaccessible elements.
#'
#' If no match is found, the function returns either `NULL` or `FALSE`,
#' depending on the value of `return_logical`.
#'
#' @return
#' By default, the first matching object found according to the specified
#' criteria, or `NULL` if none are found within the specified `max_depth`.
#'
#' If `return_logical = TRUE`, returns `TRUE` if a match exists,
#' or `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' library(bnlearn)
#' bn <- bnlearn::hc(iris)
#' nested <- list(a = list(b = list(model = bn)))
#'
#' # Search by class only
#' find_recursive(nested, x_class = "bn")
#'
#' # Search by name only
#' find_recursive(nested, x_name = "model")
#'
#' # Search by both name and class (must satisfy both)
#' find_recursive(nested, x_class = "bn", x_name = "model", require_both = TRUE)
#'
#' # Logical return mode
#' find_recursive(nested, x_class = "bn", return_logical = TRUE)
#' find_recursive(nested, x_class = "bn.fit", return_logical = TRUE)
#' }
#'
#' @export
find_recursive <- function(
    x,
    x_class = NULL,
    x_name = NULL,
    require_both = FALSE,
    max_depth = Inf,
    return_logical = FALSE,
    depth = 0
) {

  # ---- validate inputs ----
  if (!is.null(x_class) && (!is.character(x_class) || length(x_class) != 1)) {
    stop("`x_class` must be NULL or a single character string specifying a class name.")
  }
  if (!is.null(x_name) && (!is.character(x_name) || length(x_name) != 1)) {
    stop("`x_name` must be NULL or a single character string specifying an element name.")
  }
  if (!is.numeric(max_depth) || length(max_depth) != 1 || max_depth <= 0) {
    stop("`max_depth` must be a positive number or Inf.")
  }
  if (is.null(x_class) && is.null(x_name)) {
    stop("You must specify either `x_class`, `x_name`, or both.")
  }

  # ---- stop if depth limit reached ----
  if (depth > max_depth) return(if (return_logical) FALSE else NULL)

  # ---- base cases ----
  class_match <- if (!is.null(x_class)) inherits(x, x_class) else FALSE
  name_match <- FALSE

  # search by name
  if (!is.null(x_name) && is.list(x) && !is.null(names(x)) && x_name %in% names(x)) {
    name_match <- TRUE
    if (require_both) {
      target <- x[[x_name]]
      if (!is.null(target) && inherits(target, x_class)) {
        return(if (return_logical) TRUE else target)
      }
    } else {
      return(if (return_logical) TRUE else x[[x_name]])
    }
  }

  # search by class only
  if (!is.null(x_class) && isTRUE(class_match) && (!require_both || is.null(x_name))) {
    return(if (return_logical) TRUE else x)
  }

  # ---- recursive case ----
  if (is.list(x)) {
    for (el in x) {
      res <- tryCatch(
        find_recursive(
          el,
          x_class = x_class,
          x_name = x_name,
          require_both = require_both,
          max_depth = max_depth,
          return_logical = return_logical,
          depth = depth + 1
        ),
        error = function(e) if (return_logical) FALSE else NULL
      )
      if (return_logical && isTRUE(res)) return(TRUE)
      if (!return_logical && !is.null(res)) return(res)
    }
  }

  # ---- not found ----
  return(if (return_logical) FALSE else NULL)
}
