#' seg_create_profile_block
#'
#' @description Build a profile block definition for segmentation spec generation.
#'
#' @param prefix character, variable prefix (e.g. "AC")
#' @param name character, block display name
#' @param label character vector, item labels
#' @param source_var character vector, SPSS variable names
#' @param value character vector, value coding ("1", ">= 4", "mean", etc.)
#' @param zero_filled Zero-filled flag written to column I, recycled over the
#'   items. `1` turns NAs into 0; `0` lets them propagate. Default `NULL` picks
#'   per item: `0` where `value` is blank, `1` otherwise.
#'
#'   A blank `value` sends the row down the mean / direct-copy branch of the
#'   Syntax formula. Zero-filling there scores an unanswered source as 0 rather
#'   than leaving it missing, which silently drags a mean down by however many
#'   sources the respondent was never shown - on a looped battery that is most
#'   of them. Blank-value rows therefore default to 0.
#'
#' @return A list with elements `prefix`, `block_name`, and `items` (data.frame).
#' @export
seg_create_profile_block <- function(prefix, name, label, source_var, value, zero_filled = NULL) {

  n <- max(length(label), length(source_var), length(value))

  if (is.null(zero_filled)) {
    zero_filled <- ifelse(is.na(value) | trimws(value) == "", 0L, 1L)
  }

  zero_filled <- as.integer(rep(zero_filled, length.out = n))

  if (any(!zero_filled %in% c(0L, 1L))) {
    stop("zero_filled must be 0 or 1.", call. = FALSE)
  }

  list(
    prefix = prefix,
    block_name = name,
    items = data.frame(
      label       = label,
      source_var  = source_var,
      value       = value,
      zero_filled = zero_filled,
      stringsAsFactors = FALSE
    )
  )
}
