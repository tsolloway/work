#' drivers_subgroup_count
#' @description drivers_subgroup_count
#' @export
drivers_subgroup_count <- function(
    df_flat,
    subgroups
){

  df[, subgroups] %>%
    apply(2, function(x){

      xsum = x %>% sum(na.rm = T)
      xbase = x %>% is.na() %>% not() %>% sum()

      tibble::tibble(
        count = xsum,
        base = xbase
      )

    }) %>%
    bind_rows() %>%
    mutate(names = subgroups) %>%
    select(names, count, base)
}
