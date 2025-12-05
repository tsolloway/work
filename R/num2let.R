#' num2let
#'
#' @description
#' Converts positive integers to letters in Excel column format.
#' For example, 1 -> "A", 27 -> "AA".
#'
#' @param n Positive integer or integer vector to convert.
#' @param lets Character vector of letters to use (default is `LETTERS`).
#'
#' @return Character vector of Excel-style column names.
#'
#' @examples
#' num2let(1)       # "A"
#' num2let(26)      # "Z"
#' num2let(27)      # "AA"
#' num2let(52:55)   # "AZ" "BA" "BB" "BC"
#'
#' @export
num2let <- function(n, lets = LETTERS) {
  base <- length(lets)

  if (any(n <= 0)) stop("All numbers must be positive integers.")

  if (length(n) > 1) return(sapply(n, num2let, lets = lets))

  out <- ""

  repeat {
    if (n > base) {
      rem <- (n - 1) %% base
      n <- (n - 1) %/% base
      out <- paste0(lets[rem + 1], out)
    } else {
      return(paste0(lets[n], out))
    }
  }
}
