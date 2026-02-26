#' seg_create_profile_block
#'
#' @description Build a profile block definition for segmentation spec generation.
#'
#' @param prefix character, variable prefix (e.g. "AC")
#' @param name character, block display name
#' @param label character vector, item labels
#' @param source_var character vector, SPSS variable names
#' @param value character vector, value coding ("1", ">= 4", "mean", etc.)
#' @return A list with elements `prefix`, `block_name`, and `items` (data.frame).
#' @export
seg_create_profile_block <- function(prefix, name, label, source_var, value) {
  list(
    prefix = prefix,
    block_name = name,
    items = data.frame(
      label      = label,
      source_var = source_var,
      value      = value,
      stringsAsFactors = FALSE
    )
  )
}
