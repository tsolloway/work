#' oxl_colorscale_grey
#' @description oxl_colorscale_grey
#' @export
oxl_colorscale_grey <- function(
    x = as.character(c(1,2,3,4))
){
  x <- x %>% as.character()
  x <- match.arg(x)

  switch(
    x,
    "1" = "#f2f2f2",
    "2" = "#d9d9d9",
    "3" = "#bfbfbf",
    "4" = "#808080"
  )
}



#' oxl_colorscale_good
#' @description oxl_colorscale_good
#' @export
oxl_colorscale_good <- function(
    x = as.character(c(1,2))
){
  x <- x %>% as.character()
  x <- match.arg(x)

  switch(
    x,
    "1" = "#c6eecf",
    "2" = "#006100"
  )
}


#' oxl_colorscale_bad
#' @description oxl_colorscale_bad
#' @export
oxl_colorscale_bad <- function(
    x = as.character(c(1,2))
){
  x <- x %>% as.character()
  x <- match.arg(x)

  switch(
    x,
    "1" = "#ffc8cd",
    "2" = "#9c0406"
  )
}


#' oxl_colorscale_neurtal
#' @description oxl_colorscale_neurtal
#' @export
oxl_colorscale_neurtal <- function(
    x = as.character(c(1,2))
){
  x <- x %>% as.character()
  x <- match.arg(x)

  switch(
    x,
    "1" = "#feeb9b",
    "2" = "#9c5800"
  )
}


