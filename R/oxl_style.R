#' Horizontal alignment options
#'
#' @description Returns available horizontal alignment options for Excel cells.
#' @return Character vector of horizontal alignment options.
#' @export
oxl_opt_halign <- function() {
  c("center", "left", "right", "justify")
}



#' Vertical alignment options
#'
#' @description Returns available vertical alignment options for Excel cells.
#' @return Character vector of vertical alignment options.
#' @export
oxl_opt_valign <- function() {
  c("top", "center", "bottom")
}



#' Centered cell style
#'
#' @description Creates a cell style with horizontal alignment centered.
#' @param ... Additional arguments passed to `openxlsx::createStyle`.
#' @return An `openxlsx` style object.
#' @examples
#' openxlsx::createWorkbook() %>%
#'   oxl_style_center()
#' @export
oxl_style_center <- function(...) {
  oxl_style_halign(halign = "center", ...)
}



#' Horizontal alignment cell style
#'
#' @description Creates a cell style with specified horizontal alignment.
#' @param halign Horizontal alignment. One of `oxl_opt_halign()`.
#' @param ... Additional arguments passed to `openxlsx::createStyle`.
#' @return An `openxlsx` style object.
#' @examples
#' oxl_style_halign("left")
#' @export
oxl_style_halign <- function(halign = oxl_opt_halign(), ...) {
  halign <- match.arg(halign)
  openxlsx::createStyle(halign = halign, ...)
}



#' Percent cell style
#'
#' @description Creates a percentage cell style with specified decimal places and alignment.
#' @param decimal Number of decimal places.
#' @param halign Horizontal alignment.
#' @param ... Additional arguments passed to `openxlsx::createStyle`.
#' @return An `openxlsx` style object.
#' @examples
#' oxl_style_percent(1)
#' @export
oxl_style_percent <- function(decimal = 0, halign = oxl_opt_halign(), ...) {
  halign <- match.arg(halign)
  openxlsx::createStyle(
    halign = halign,
    numFmt = glue::glue("{format(0, nsmall = decimal)}%"),
    ...
  )
}



#' Numeric cell style
#'
#' @description Creates a numeric cell style with specified decimal places and alignment.
#' @param decimal Number of decimal places.
#' @param halign Horizontal alignment.
#' @param ... Additional arguments passed to `openxlsx::createStyle`.
#' @return An `openxlsx` style object.
#' @examples
#' oxl_style_number(2)
#' @export
oxl_style_number <- function(decimal = 0, halign = oxl_opt_halign(), ...) {

  halign <- match.arg(halign)

  openxlsx::createStyle(
    halign = halign,
    numFmt = format(0, nsmall = decimal),
    ...
  )

}



#' Good cell style
#'
#' @description Creates a “good” colored cell style. Can be conditional (bgFill) or not (fgFill).
#' @param halign Horizontal alignment.
#' @param conditional Logical; if TRUE, uses `bgFill` instead of `fgFill`.
#' @param ... Additional arguments passed to `openxlsx::createStyle`.
#' @return An `openxlsx` style object.
#' @examples
#' oxl_style_cell_good()
#' @export
oxl_style_cell_good <- function(halign = oxl_opt_halign(), conditional = FALSE, ...) {

  halign <- match.arg(halign)

  if (conditional) {

    openxlsx::createStyle(
      halign = halign,
      fontColour = oxl_colorscale_good(2),
      bgFill = oxl_colorscale_good(1),
      ...
    )

  } else {

    openxlsx::createStyle(
      halign = halign,
      fontColour = oxl_colorscale_good(2),
      fgFill = oxl_colorscale_good(1),
      ...
    )

  }

}




#' Good black-white style
#'
#' @description Creates a “good” black/white cell style.
#' @param halign Horizontal alignment.
#' @param conditional Logical; if TRUE, uses `bgFill`.
#' @param ... Additional arguments passed to `openxlsx::createStyle`.
#' @return An `openxlsx` style object.
#' @examples
#' oxl_style_cell_good_bw()
#' @export
oxl_style_cell_good_bw <- function(halign = oxl_opt_halign(), conditional = FALSE, ...) {

  halign <- match.arg(halign)

  if (conditional) {

    openxlsx::createStyle(
      halign = halign,
      fontColour = "white",
      bgFill = "black",
      ...
    )

  } else {

    openxlsx::createStyle(
      halign = halign,
      fontColour = "white",
      fgFill = "black",
      ...
    )

  }

}



#' Bad cell style
#'
#' @description Creates a “bad” colored cell style.
#' @param halign Horizontal alignment.
#' @param conditional Logical; if TRUE, uses `bgFill`.
#' @param ... Additional arguments passed to `openxlsx::createStyle`.
#' @return An `openxlsx` style object.
#' @examples
#' oxl_style_cell_bad()
#' @export
oxl_style_cell_bad <- function(halign = oxl_opt_halign(), conditional = FALSE, ...) {

  halign <- match.arg(halign)

  if (conditional) {

    openxlsx::createStyle(
      halign = halign,
      fontColour = oxl_colorscale_bad(2),
      bgFill = oxl_colorscale_bad(1),
      ...
    )

  } else {
    openxlsx::createStyle(

      halign = halign,
      fontColour = oxl_colorscale_bad(2),
      fgFill = oxl_colorscale_bad(1),
      ...
    )

  }
}



#' Bad black-white cell style
#'
#' @description Creates a “bad” black/white cell style.
#' @param halign Horizontal alignment.
#' @param conditional Logical; if TRUE, uses `bgFill`.
#' @param ... Additional arguments passed to `openxlsx::createStyle`.
#' @return An `openxlsx` style object.
#' @examples
#' oxl_style_cell_bad_bw()
#' @export
oxl_style_cell_bad_bw <- function(halign = oxl_opt_halign(), conditional = FALSE, ...) {

  halign <- match.arg(halign)

  if (conditional) {

    openxlsx::createStyle(
      halign = halign,
      fontColour = "black",
      bgFill = oxl_colorscale_grey(2),
      ...
    )

  } else {

    openxlsx::createStyle(
      halign = halign,
      fontColour = "black",
      fgFill = oxl_colorscale_grey(2),
      ...
    )

  }

}



#' Neutral cell style
#'
#' @description Creates a “neutral” colored cell style.
#' @param halign Horizontal alignment.
#' @param conditional Logical; if TRUE, uses `bgFill`.
#' @param ... Additional arguments passed to `openxlsx::createStyle`.
#' @return An `openxlsx` style object.
#' @examples
#' oxl_style_cell_neutral()
#' @export
oxl_style_cell_neutral <- function(halign = oxl_opt_halign(), conditional = FALSE, ...) {

  halign <- match.arg(halign)

  if (conditional) {

    openxlsx::createStyle(
      halign = halign,
      fontColour = oxl_colorscale_neutral(2),
      bgFill = oxl_colorscale_neutral(1),
      ...
    )

  } else {

    openxlsx::createStyle(
      halign = halign,
      fontColour = oxl_colorscale_neutral(2),
      fgFill = oxl_colorscale_neutral(1),
      ...
    )

  }

}

