#' drivers_flat_means
#' @description drivers_flat_means
#' @export
drivers_flat_means <- function(
    df,
    vars
){
  df %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    apply(2, function(x){

      xmean <- mean(x, na.rm = TRUE)
      xbase <- sum(!is.na(x))

      tibble::tibble(
        mean = xmean,
        base = xbase
      )

    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(names = vars) %>%
    dplyr::select(names, mean, base)
}
