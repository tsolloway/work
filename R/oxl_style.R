#' oxl_opt_halign
#' @description oxl_opt_halign
#' @export
oxl_opt_halign <- function(){
  c("center", "left", 'right', "justify")
}


#' oxl_opt_valign
#' @description oxl_opt_valign
#' @export
oxl_opt_valign <- function(){
  c("center", "left", 'right')
}


#' oxl_style_center
#' @description oxl_style_center
#' @export
oxl_style_center <- function(...){
  oxl_style_halign(halign = "center", ...)
}


#' oxl_style_halign
#' @description oxl_style_halign
#' @export
oxl_style_halign <- function(
    halign = oxl_opt_halign(), ...
){
  halign <- match.arg(halign)

  createStyle(halign = halign, ...)
}


#' oxl_style_percent
#' @description oxl_style_percent
#' @export
oxl_style_percent <- function(
    deciminal = 0,
    halign = oxl_opt_halign(),
    ...
){
  halign <- match.arg(halign)

  createStyle(
    halign = halign,
    numFmt = glue("{format(0, nsmall = deciminal)}%"),
    ...
  )
}


#' oxl_style_number
#' @description oxl_style_number
#' @export
oxl_style_number <- function(
    deciminal = 0,
    halign = oxl_opt_halign(),
    ...
){
  halign <- match.arg(halign)

  createStyle(
    halign = halign,
    numFmt = format(0, nsmall = deciminal),
    ...
  )
}


#' oxl_style_cell_good
#' @description oxl_style_cell_good
#' @export
oxl_style_cell_good <- function(
    halign = oxl_opt_halign(),
    conditional = FALSE,
    ...
){
  halign <- match.arg(halign)

  if(conditional){
    createStyle(
      halign = halign,
      fontColour = oxl_colorscale_good(2),
      bgFill = oxl_colorscale_good(1),
      ...
    )
  }else if(!conditional){
    createStyle(
      halign = halign,
      fontColour = oxl_colorscale_good(2),
      fgFill = oxl_colorscale_good(1),
      ...
    )
  }
}


#' oxl_style_cell_good_bw
#' @description oxl_style_cell_good_bw
#' @export
oxl_style_cell_good_bw <- function(
    halign = oxl_opt_halign(),
    conditional = FALSE,
    ...
){
  halign <- match.arg(halign)

  if(conditional){
    createStyle(
      halign = halign,
      fontColour = "white",
      bgFill = "black",
      ...
    )
  }else if(!conditional){
    createStyle(
      halign = halign,
      fontColour = "white",
      fgFill = "black",
      ...
    )
  }
}


#' oxl_style_cell_bad
#' @description oxl_style_cell_bad
#' @export
oxl_style_cell_bad <- function(
    halign = oxl_opt_halign(),
    conditional = FALSE,
    ...
){
  halign <- match.arg(halign)

  if(conditional){
    createStyle(
      halign = halign,
      fontColour = oxl_colorscale_bad(2),
      bgFill = oxl_colorscale_bad(1),
      ...
    )
  }else if(!conditional){
    createStyle(
      halign = halign,
      fontColour = oxl_colorscale_bad(2),
      fgFill = oxl_colorscale_bad(1),
      ...
    )
  }
}


#' oxl_style_cell_bad_bw
#' @description oxl_style_cell_bad_bw
#' @export
oxl_style_cell_bad_bw <- function(
    halign = oxl_opt_halign(),
    conditional = FALSE,
    ...
){
  halign <- match.arg(halign)

  if(conditional){
    createStyle(
      halign = halign,
      fontColour = "black",
      bgFill = oxl_colorscale_grey(2),
      ...
    )
  }else if(!conditional){
    createStyle(
      halign = halign,
      fontColour = "black",
      fgFill = oxl_colorscale_grey(2),
      ...
    )
  }
}


#' oxl_style_cell_neurtal
#' @description oxl_style_cell_neurtal
#' @export
oxl_style_cell_neurtal <- function(
    halign = oxl_opt_halign(),
    conditional = FALSE,
    ...
){
  halign <- match.arg(halign)

  if(conditional){
    createStyle(
      halign = halign,
      fontColour = oxl_colorscale_neurtal(2),
      bgFill = oxl_colorscale_neurtal(1),
      ...
    )
  }else if(!conditional){
    createStyle(
      halign = halign,
      fontColour = oxl_colorscale_neurtal(2),
      fgFill = oxl_colorscale_neurtal(1),
      ...
    )
  }
}


