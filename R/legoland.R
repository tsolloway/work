#' legoland
#'
#' @description
#' Returns Legoland passholder codes for specified individuals or services.
#'
#' @param x Character string specifying whose code to return.
#'   Options: `"all"`, `"sean"`, `"alina"`, `"tyler"`, `"guestservices"`, `"services"`.
#'   Defaults to `"all"`.
#'
#' @return
#' A named character vector of passholder codes when `x = "all"`,
#' or a single character/number corresponding to the requested entry.
#'
#' @examples
#' # Return all codes
#' legoland("all")
#'
#' # Return a single passholder code
#' legoland("alina")
#'
#' # Return the Guest Services number
#' legoland("guestservices")
#'
#' @export
legoland <- function(
    x = c("all", "sean", "alina", "tyler", "guestservices", "services")
) {
  x <- match.arg(x)

  output <- c(
    "sean"  = "305 081 229 156 563 376",
    "mia"   = "308 050 130 652 355 432",
    "alina" = "305 081 229 161 716 186",
    "tyler" = "305 081 229 110 083 017"
  )

  if (x == "all") {
    return(output)
  }

  if (x %in% c("guestservices", "services")) {
    return("760 786 0034")
  }

  if (!x %in% names(output)) {
    stop("Invalid name: ", x, ". Must be one of ", paste(names(output), collapse = ", "))
  }

  return(output[[x]])
}
