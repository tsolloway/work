#' seg_create_polar_block
#'
#' @description Build a polar block definition for segmentation spec generation.
#'
#' @param prefix character, variable prefix (e.g. "KG")
#' @param name character, block display name
#' @param source character, source variable pattern (e.g. "B1")
#' @param left character vector, left-side labels
#' @param right character vector, right-side labels
#' @param lead character vector, side lead ("L" or "R") per pair. Defaults to all "L".
#' @return A list with elements `prefix`, `block_name`, `source_pattern`, and `pairs` (data.frame).
#' @export
seg_create_polar_block <- function(prefix, name, source, left, right, lead = NULL) {
  if (is.null(lead)) lead <- rep("L", length(left))
  list(
    prefix = prefix,
    block_name = name,
    source_pattern = source,
    pairs = data.frame(
      left  = trimws(left),
      right = trimws(right),
      lead  = lead,
      stringsAsFactors = FALSE
    )
  )
}
