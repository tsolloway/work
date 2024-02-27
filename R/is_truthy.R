#' is_truthy
#' @description is_truthy
#' @export
is_truthy <- function (x){

  if (is.null(x))
    return(FALSE)
  if (inherits(x, "try-error"))
    return(FALSE)
  if (!is.atomic(x))
    return(TRUE)
  if (length(x) == 0)
    return(FALSE)
  if (all(is.na(x)))
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x))))
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x)))
    return(FALSE)

  return(TRUE)
}
