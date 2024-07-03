
#' means_check_flat
#' @description means_check_flat
#' @export
means_check_flat <- function(df, vars){

  df %>%
    select(all_of(vars)) %>%
    apply(2, function(x){

      xsum = x %>% magrittr::is_greater_than(0) %>% sum(na.rm = T)
      xmean = x %>% mean(na.rm = T)
      xbase = x %>% is.na() %>% not() %>% sum()

      tibble::tibble(
        greater_than_zero_count = xsum,
        mean = xmean,
        base = xbase
      )

    }) %>%
    bind_rows() %>%
    mutate(variable = vars) %>%
    select(variable, greater_than_zero_count, mean, base)

}



#' means_check_single_categorical
#' @description means_check_single_categorical
#' @export
means_check_single_categorical <- function(df, var){

  df %>%
    group_by({{var}}) %>%
    summarise(
      greater_than_zero_count = n()
    ) %>%
    ungroup() %>%
    set_colnames(
      c("variable", "greater_than_zero_count")
    ) %>%
    mutate(
      base = greater_than_zero_count %>% sum(),
      mean = greater_than_zero_count / base
    ) %>%
    select(variable, greater_than_zero_count, mean, base)

}


#' means_check_by_dictionary
#' @description means_check_by_dictionary
#' @export
means_check_by_dictionary <- function(df, ...){

  x <- work:::cq(...)

  output <- purrr::map(x, ~{

    .x <- eval(parse(text = .x))

    .x %>% col_check("variable", hard_stop = TRUE)

    do_recode <- .x %>% col_check("variable_recode", hard_stop = FALSE)

    if(!do_recode){
      left_join(
        .x,
        means_check_flat(df, .x$variable),
        by = join_by(variable)
      )
    }else if(do_recode){
      bind_rows(
        left_join(
          .x,
          means_check_flat(df, .x$variable),
          by = join_by(variable)
        ) %>%
          select(-variable_recode, -label_recode),

        left_join(
          .x,
          means_check_flat(df, .x$variable_recode),
          by = join_by(variable_recode == variable)
        ) %>%
          mutate(
            variable = variable_recode,
            label = label_recode
          ) %>%
          select(-variable_recode, -label_recode)
      )
    }
  })

  if(length(output) == 1){
    output[[1]]
  }else if(length(output) > 1){
    output %>% bind_rows()
  }
}



