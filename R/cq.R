#' cq
#'
#' @description Concatenate values or bare symbols, preserving names and converting numeric-like inputs to numeric if possible.
#' @param ... Values, expressions, or bare symbols.
#' @return A vector (numeric if all inputs are numeric-like, otherwise character) with names preserved.
#' @examples
#' cq(!!(1:4))
#' cq(!!(1:4), foo)
#' cq(!!(1:4), hi = "5")
#' cq(hi, there)
#' @export
cq <- function(...) {
  quos <- rlang::enquos(...)

  # Convert symbols and literals to character
  x <- purrr::map(quos, function(q) {
    expr <- rlang::quo_get_expr(q)
    if (rlang::is_symbol(expr)) {
      as.character(expr)
    } else {
      expr
    }
  }) %>% unlist()

  # Preserve names
  nms <- names(x)

  # Detect numeric-like
  numeric_like <- suppressWarnings(!is.na(as.numeric(as.character(x))))

  if (all(numeric_like)) {
    x <- as.numeric(x)
  } else {
    x <- as.character(x)
  }

  setNames(x, nms)
}
