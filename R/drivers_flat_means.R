#' drivers_subgroup_count
#' @description drivers_subgroup_count
#' @export
drivers_flat_means <- function(
    df,
    vars
){
  df %>%
    select(all_of(vars)) %>%
    apply(2, function(x){

      xmean = x %>% mean(na.rm = T)
      xbase = x %>% is.na() %>% not() %>% sum()

      tibble::tibble(
        mean = xmean,
        base = xbase
      )

    }) %>%
    bind_rows() %>%
    mutate(names = vars) %>%
    select(names, mean, base)
}
