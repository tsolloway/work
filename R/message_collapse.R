#' message_collapse
#' @description message_collapse
#' @export
message_collapse <- function(pre = NULL, x, post = NULL, collapse_sep = ", "){

  if( length(x) == 1 ){

    paste0(pre, x, post)

  }else if( length(x) > 1 ){

    paste0(pre, paste0(x, collapse = collapse_sep), post)

  }

}
