#' bn_black_list_from_white_list
#' @description bn_black_list_from_white_list
#' @export
bn_black_list_from_white_list <- function(x){
  result <- x %>%
    unlist() %>%
    setNames(NULL) %>%
    unique() %>%
    work::make_arcs() %>%
    setdiff(x) %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    setNames(c("from", "to"))

  return(result)
}


