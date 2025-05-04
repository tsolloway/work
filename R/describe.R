#' describe
#' @description describe
#' @export
describe <- function(
    df,
    vars = NULL
){

  if(!is.null(vars)){
    vars <- unlist(vars) %>% as.character()
    df <- df %>% select(all_of(vars))
  }

  df %>%
    psych::describe() %>%
    as_tibble() %>%
    mutate(
      vars = vars,

      missing = df %>%
        summarise(
          across(everything(), ~sum(is.na(.)))
        ) %>%
        unlist(),

      complete = n / nrow(df),

      hc_histogram = df %>%
        imap(
          ~as.character(.x) %>%
            as.numeric() %>%
            hc_histogram(
              title = glue("Histogram of {stringr::str_to_title(.y)}")
            )
        )
    ) %>%
    relocate(
      missing, .after = n
    ) %>%
    relocate(
      complete, .after = missing
    )

}
