#' seg_organize_input_sheet
#' @description seg_organize_input_sheet
#' @export
seg_organize_input_sheet <- function(seg, range_predictors, solution_items = 14){


  df <- seg[["data"]][["with_shell"]]


  input_table <- seg[["input_sheet"]][["input_fa_table"]] %>%
    mutate(
      .,
      order = seq(nrow(.))
    )


  vars <- input_table %>% select(source_var) %>% unlist() %>% setNames(NULL)
  vars_rs <- input_table %>% select(rs_var) %>% unlist() %>% setNames(NULL)


  input_table <- df %>%
    select(all_of(vars)) %>%
    psych::describe() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("source_var") %>%
    select(source_var, mean, sd) %>%
    left_join(input_table, ., by = join_by(source_var)) %>%
    relocate(solution_a, .after = last_col())


  prototype_table <- bind_rows(
    seg[["shell"]][["polars"]],
    seg[["shell"]][["profiles"]]
  ) %>%
    tidyr::unnest(vars) %>%
    select(var, label)


  vars_shell <- prototype_table %>%
    select(var) %>%
    unlist() %>%
    setNames(NULL)


  prototype_table <- df %>%
    select(all_of(vars_shell)) %>%
    psych::describe() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("var") %>%
    select(var, mean, sd) %>%
    left_join(prototype_table, ., by = join_by(var))


  range_predictors_table <- range_predictors %>% map(
    ~ df %>%
      select(all_of(c(.x, vars_rs))) %>%
      group_by(.data[[.x]]) %>%
      summarise(
        across(vars_rs, \(x) mean(x, na.rm = TRUE))
      ) %>%
      ungroup() %>%
      summarise(
        across(vars_rs, \(x) abs(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      ) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("var") %>%
      setNames(c("var", .x))
  ) %>%
    reduce(left_join, by = 'var') %>%
    as_tibble()


  range_predictors_table_threshold <- range_predictors_table %>%
    select(-var) %>% summarise(
      across(all_of(range_predictors), \(x) quantile(x, 2/3, na.rm = TRUE))
    )


  range_predictor_threshold_sum <- map2(
    range_predictors_table %>% select(-var),
    range_predictors_table_threshold,
    ~{.x>=.y}
  ) %>%
    as_tibble() %>%
    rowSums() %>%
    tibble(
      range_predictors_table %>% select(var),
      range_predictor_threshold = .
    ) %>%
    mutate(
      range_predictor_rank = dense_rank(desc(range_predictor_threshold))
    ) %>%
    arrange(-range_predictor_threshold)


  range_predictor_threshold_sum <- range_predictor_threshold_sum %>%
    mutate(
      .,
      solution_b = ifelse(range_predictor_rank <= .[[solution_items, "range_predictor_rank"]], "x", NA)
    ) %>%
    select(-range_predictor_rank)


  range_predictor_rank_mean <- range_predictors_table %>%
    mutate(
      across(all_of(range_predictors), \(x) dense_rank(desc(x)))
    ) %>%
    mutate(
      range_predictor_rank_mean = rowMeans(pick(where(is.numeric)))
    ) %>%
    select(var, range_predictor_rank_mean) %>%
    mutate(
      range_predictor_rank = dense_rank(range_predictor_rank_mean)
    ) %>%
    arrange(range_predictor_rank)


  range_predictor_rank_mean <- range_predictor_rank_mean %>%
    mutate(
      .,
      solution_c = ifelse(range_predictor_rank <= .[[solution_items, "range_predictor_rank"]], "x", NA)
    ) %>%
    select(-range_predictor_rank)


  input_table <- left_join(
    input_table,
    range_predictor_threshold_sum,
    by = join_by(rs_var == var)
  ) %>%
    left_join(
      range_predictor_rank_mean,
      by = join_by(rs_var == var)
    ) %>%
    rename_col(
      .select = TRUE,
      order = order,
      source_var = source_var,
      profile_var = profile_var,
      rs_var = rs_var,
      mean = mean,
      sd = sd,
      fa_name = fa_name,
      fa_n = fa_n,
      loading = loading,
      rp_threshold = range_predictor_threshold,
      rp_rank = range_predictor_rank_mean,
      source_label = source_label,
      label = label,
      solution_a = solution_a,
      solution_b = solution_b,
      solution_c = solution_c
    )


  input_table <- input_table %>%
    arrange(-sd) %>%
    mutate(
      solution_d = ifelse(sd >= .[[solution_items, "sd"]], "x", NA)
    ) %>%
    arrange(order)


  rational <- c(
    "Max factor loadings",
    "Range predictor - top tercile threshold",
    "Range predictor - top ranking",
    "Top standard deviation"
  )


  solution_rational <- tibble(
    solution = LETTERS,
    rational = c(rational, rep(NA, 26 - length(rational)))
  )


  seg[["input_sheet"]][["input_table"]] <- input_table
  seg[["input_sheet"]][["prototype_table"]] <- prototype_table
  seg[["input_sheet"]][["solution_rational"]] <- solution_rational

  return(seg)
}
