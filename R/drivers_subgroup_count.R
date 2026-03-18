#' drivers_subgroup_count
#' @description drivers_subgroup_count
#' @export
drivers_subgroup_count <- function(
    df_flat,
    subgroups
){

  df_flat[, subgroups] %>%
    apply(2, function(x){

      xsum <- sum(x, na.rm = TRUE)
      xbase <- sum(!is.na(x))

      tibble::tibble(
        count = xsum,
        base = xbase
      )

    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(names = subgroups) %>%
    dplyr::select(names, count, base)
}
