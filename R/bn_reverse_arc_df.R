#' Reverse arc directions in a data frame
#'
#' @description Takes a two-column arc data frame (from, to) and returns a new data frame
#'   with all arcs reversed (to → from becomes from → to).
#' @param df A data frame with at least columns `from` and `to`.
#' @return A data frame with reversed arcs.
#' @export
bn_reverse_arc_df <- function(df) {

  stopifnot(all(c("from", "to") %in% names(df)))

  df %>%
    dplyr::transmute(from = .data$to, to = .data$from)

}
