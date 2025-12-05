#' message_collapse
#'
#' @description
#' Concatenates a character vector into a single message string,
#' optionally adding a prefix (`pre`) and suffix (`post`), with optional pluralization.
#'
#' @param pre Optional character prefix added before the collapsed message.
#'   Adds an "s" automatically if length(x) != 1.
#' @param x Character vector to be collapsed.
#' @param post Optional character suffix added after the collapsed message.
#' @param collapse_sep Character string used to separate elements when collapsing.
#'   Defaults to `", "`.
#'
#' @return
#' A single character string combining `pre`, the collapsed elements of `x`,
#' and `post`.
#'
#' @examples
#' message_collapse(pre = "Item", x = "A")
#' message_collapse(pre = "Item", x = c("A", "B", "C"))
#' message_collapse(pre = "Task", x = c("X", "Y"), post = " completed")
#'
#' @export
message_collapse <- function(pre = NULL, x, post = NULL, collapse_sep = ", ") {
  if (missing(x)) stop("`x` must be provided.")
  if (!is.character(x)) stop("`x` must be a character vector.")
  if (length(x) == 0) return("")

  collapsed <- paste(x, collapse = collapse_sep)

  # Handle pluralization: add "s" if pre is provided and length(x) != 1
  prefix <- if (!is.null(pre)) {
    if (length(x) == 1) pre else paste0(pre, "s")
  } else {
    ""
  }

  paste0(prefix, collapsed, ifelse(is.null(post), "", post))
}
