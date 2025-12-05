#' oxl_colorscale_grey
#'
#' @description Returns shades of grey for ordinal scales (1-4).
#' @param x Character or numeric, one of 1,2,3,4.
#' @return Hex color code as character.
#' @examples
#' oxl_colorscale_grey(1)
#' oxl_colorscale_grey("3")
#' @export
oxl_colorscale_grey <- function(x = c("1","2","3","4")) {
  x <- as.character(x)
  x <- match.arg(x, choices = c("1","2","3","4"))

  switch(
    x,
    "1" = "#f2f2f2",
    "2" = "#d9d9d9",
    "3" = "#bfbfbf",
    "4" = "#808080"
  )
}

#' oxl_colorscale_good
#'
#' @description Returns “good” colors (green scale) for ordinal scales (1-2).
#' @param x Character or numeric, one of 1,2.
#' @return Hex color code as character.
#' @examples
#' oxl_colorscale_good(1)
#' oxl_colorscale_good("2")
#' @export
oxl_colorscale_good <- function(x = c("1","2")) {
  x <- as.character(x)
  x <- match.arg(x, choices = c("1","2"))

  switch(
    x,
    "1" = "#c6eecf",
    "2" = "#006100"
  )
}

#' oxl_colorscale_bad
#'
#' @description Returns “bad” colors (red scale) for ordinal scales (1-2).
#' @param x Character or numeric, one of 1,2.
#' @return Hex color code as character.
#' @examples
#' oxl_colorscale_bad(1)
#' oxl_colorscale_bad("2")
#' @export
oxl_colorscale_bad <- function(x = c("1","2")) {
  x <- as.character(x)
  x <- match.arg(x, choices = c("1","2"))

  switch(
    x,
    "1" = "#ffc8cd",
    "2" = "#9c0406"
  )
}

#' oxl_colorscale_neutral
#'
#' @description Returns neutral colors (yellow/orange) for ordinal scales (1-2).
#' @param x Character or numeric, one of 1,2.
#' @return Hex color code as character.
#' @examples
#' oxl_colorscale_neutral(1)
#' oxl_colorscale_neutral("2")
#' @export
oxl_colorscale_neutral <- function(x = c("1","2")) {
  x <- as.character(x)
  x <- match.arg(x, choices = c("1","2"))

  switch(
    x,
    "1" = "#feeb9b",
    "2" = "#9c5800"
  )
}
