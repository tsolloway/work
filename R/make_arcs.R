#' make_arcs
#'
#' @description
#' Generate all possible directed arcs between two sets of nodes.
#'
#' Given two sets of nodes (`x` and `y`), this function returns all combinations
#' of directed arcs from each element of `x` to each element of `y`. When
#' `bidirectional = TRUE`, it also adds the reverse arcs.
#'
#' @param x Character vector of source node names.
#' @param y Optional character vector of target node names. Defaults to `x`.
#' @param bidirectional Logical; if `TRUE`, includes arcs in both directions.
#'
#' @return
#' A tibble with columns:
#' \describe{
#'   \item{from}{Source node.}
#'   \item{to}{Target node.}
#' }
#'
#' @examples
#' # Simple one-way arcs
#' make_arcs(c("A", "B", "C"), bidirectional = FALSE)
#'
#' # Bidirectional arcs within a single set
#' make_arcs(c("A", "B"), bidirectional = TRUE)
#'
#' # Directed arcs between two distinct sets
#' make_arcs(x = c("Input1", "Input2"), y = c("Output1", "Output2"), bidirectional = FALSE)
#'
#' @export
make_arcs <- function(x, y = NULL, bidirectional = TRUE) {

  # Validate inputs
  if (!is.character(x)) stop("`x` must be a character vector.")
  if (!is.null(y) && !is.character(y)) stop("`y` must be a character vector or NULL.")
  if (!is.logical(bidirectional) || length(bidirectional) != 1) {
    stop("`bidirectional` must be a single logical value.")
  }


  # Default to x if y not provided
  if (is.null(y)) y <- x


  # One-way arcs
  arcs <- expand.grid(x = x, y = y, stringsAsFactors = FALSE) %>%
    dplyr::rename(from = x, to = y)


  # Optionally add reverse arcs
  if (bidirectional) {
    reverse_arcs <- expand.grid(x = y, y = x, stringsAsFactors = FALSE) %>%
      dplyr::rename(from = x, to = y)

    arcs <- dplyr::bind_rows(arcs, reverse_arcs)
  }


  # Remove duplicates and self-loops
  arcs <- arcs %>%
    dplyr::filter(from != to) %>%
    dplyr::distinct()


  tibble::as_tibble(arcs)
}
