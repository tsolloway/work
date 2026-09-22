#' rescale_unit
#'
#' @description Maps a scale onto 0-1 linearly: `(x - lo) / (hi - lo)`. On a
#'   3-point importance scale with `range = 1:3` that is 1 -> 0, 2 -> 0.5,
#'   3 -> 1.
#'
#'   The bounds are given rather than taken from the data on purpose. A spec is
#'   a record of how a shell variable is built, so the same spec row must
#'   produce the same value on a partial file and on the final one - and an
#'   item nobody rated at the floor would otherwise rescale differently from
#'   its neighbours on the same battery.
#'
#' @param x Numeric vector.
#' @param range Numeric vector whose min and max are the scale bounds, e.g.
#'   `1:3` or `c(1, 5)`.
#'
#' @return `x` on 0-1. Values outside `range` are returned outside 0-1 rather
#'   than clipped, so a miscoded answer is visible instead of silently absorbed.
#'
#' @export
rescale_unit <- function(x, range){

  lo <- min(range)
  hi <- max(range)

  if(hi <= lo) stop("range must span more than one value.", call. = FALSE)

  (x - lo) / (hi - lo)
}
