#' if_numeric_make_numeric
#' @description Convert numeric-like characters to numeric. Handles percentages.
#' @param x Input vector (numeric or character; factors not allowed).
#' @param if_perc_divide Logical. If TRUE, divides numeric percentages by `divide_by_number`.
#' @param divide_by_number Numeric. Divisor for percentages (default = 100).
#' @return A numeric vector if conversion is successful; otherwise original input.
#' @export
if_numeric_make_numeric <- function(x, if_perc_divide = TRUE, divide_by_number = 100){

  if( is.factor(x) ) stop("if_numeric_make_numeric does not assume factors")

  if( !is.numeric(x) ){
    output <- x %>% as.numeric() %>% suppressWarnings()


    if( any(is.na(output)) && any(work::right(x) == "%") ){

      output <- x %>% purrr::map_vec(~{
        flag <- FALSE
        if( work::right(.x)=="%" ){
          flag <- TRUE
          y <- work::left(.x, nchar(.x) - 1)
        }else{
          y <- .x
        }
        y <- y %>% as.numeric() %>% suppressWarnings()

        if( is.na(y) ){
          y <- .x
        }else if( is.numeric(y) && flag && if_perc_divide ){
          y <- y %>% magrittr::divide_by(divide_by_number)
        }

        y
      })
    }

    if( all(is.na(output)) ){
      output <- x
    }

  }else if( is.numeric(x) ){
    output <- x
  }

  return(output)
}
