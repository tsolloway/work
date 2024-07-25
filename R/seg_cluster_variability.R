#' seg_cluster_variability
#' @description seg_cluster_variability
#' @export
seg_cluster_variability <- function(df, vars, vary_percent = .1, side_bias_percent = .1){

  if(!is.null(vary_percent)){

    df[, "variability"] <- df %>%
      select(vars) %>%
      apply(1, sd, na.rm = TRUE)


    variability_cut_offs <- df[, "variability"] %>%
      unlist() %>%
      quantile(c(vary_percent/2, 1-(vary_percent/2)))


    df <- df %>% mutate(
      ok_variability = case_when(
        variability >= tail(variability_cut_offs, 1) ~ FALSE,
        variability <= head(variability_cut_offs, 1) ~ FALSE,
        .default = TRUE)
    )
  }


  if(!is.null(side_bias_percent)){
    polar_values <- df[, vars] %>% unlist() %>% unique() %>% sort()


    side_bias_threshold <- mean(polar_values)


    df <- df %>%
      mutate(
        across(
          vars,
          ~case_when(.x < side_bias_threshold ~ 1, NA~NA, .default = 0),
          .names = "side_bias_{.col}"
        )
      )


    df[, "side_bias_sum"] <- df %>%
      select(all_of(glue("side_bias_{vars}"))) %>%
      rowSums()


    side_bias_cut_offs <- df[, "side_bias_sum"] %>%
      unlist() %>%
      quantile(c(side_bias_percent/2, 1-(side_bias_percent/2)))


    df <- df %>% mutate(
      ok_side_bias = case_when(
        side_bias_sum >= tail(side_bias_cut_offs, 1) ~ FALSE,
        side_bias_sum <= head(side_bias_cut_offs, 1) ~ FALSE,
        .default = TRUE)
    )
  }


  if(!is.null(side_bias_percent) || !is.null(vary_percent)){
    ok_filter_exists <- TRUE

    df[, "okay_filter"] <- df %>%
      select(any_of(c("ok_variability", "ok_side_bias"))) %>%
      rowSums() %>% equals(., max(.))

  }else{
    ok_filter_exists <- FALSE
  }

  result <- list(
    df = df,
    ok_filter_exists = ok_filter_exists
  )

  return(result)
}

