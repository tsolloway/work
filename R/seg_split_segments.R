#' seg_split_segments
#' @description seg_split_segments
#' @export
seg_split_segments <- function(
    seg, solution_name, seg_splits, new_solution_name,
    split_into = 2, id_name = "seg_uuid",
    method = c("kmeans", "medoid"),
    return_append_only = FALSE
){

  method <- match.arg(method)

  df <- seg[["data"]][["with_solutions"]]


  if(is.null(df) || all(is.na(df))){

    warning("Returning append only")

    return_append_only <- TRUE

    df <- seg[["data"]][["with_shell"]]
  }


  mean_inf_zero <- function(x){
    ifelse(is.infinite(max(x, na.rm = TRUE)), 0, max(x, na.rm = TRUE)) %>% suppressWarnings()
  }



  if(method == "kmeans"){

    df_append <- map(
      seg_splits,
      ~cluster_kmeans(
        df = df %>% filter(.data[[solution_name]] == .x),
        vars = seg[["input_sheet"]][["input_table"]][["rs_var"]],
        vars_profiles = seg[["input_sheet"]][["input_table"]][["profile_var"]],
        solution_name = "dummy", n_min = split_into, n_max = split_into, id_name = "seg_uuid"
      ) %>%
        keep_at("all_inputs") %>%
        flatten() %>%
        pluck("cluster_seed")
    )

  }else if(method == "medoid"){

    df_append <- map(
      seg_splits,
      ~cluster_medoid(
        df = df %>% filter(.data[[solution_name]] == .x),
        vars = seg[["input_sheet"]][["input_table"]][["rs_var"]],
        vars_profiles = seg[["input_sheet"]][["input_table"]][["profile_var"]],
        solution_name = "dummy", n_min = split_into, n_max = split_into, id_name = "seg_uuid"
      ) %>%
        keep_at("all_inputs") %>%
        flatten() %>%
        pluck("cluster_seed")
    )

  }


  df_append <- df_append %>%
    flatten() %>%
    set_names(seg_splits) %>%
    imap(
      ~.x %>% setNames(c("id", glue("new_cut_{.y}")))
    ) %>%
    reduce(full_join, by = "id") %>%
    full_join(
      df %>% select(all_of(c("seg_uuid", solution_name))),
      .,
      by = join_by(seg_uuid == id)
    ) %>% mutate(
      "seed_{new_solution_name}" := !!sym(solution_name) %>% case_match(seg_splits ~ NA, .default = !!sym(solution_name))
    ) %>%
    select(-all_of(solution_name))


  for(i in seg_splits){
    y <- glue("seed_{new_solution_name}")
    x <- glue("new_cut_{i}")

    df_append <- df_append %>%
      mutate(
        "{y}" := ifelse(is.na(!!sym(y)), !!sym(x) + mean_inf_zero(!!sym(y)), !!sym(y))
      ) %>%
      select(-all_of(x))
  };rm(y,x,i)



  seg[["data"]][["with_solutions"]] <- full_join(
    seg[["data"]][["with_solutions"]],
    df_append,
    by = join_by(seg_uuid)
  )


  if(return_append_only){
    return(df_append)
  }else if(!return_append_only){
    return(seg)
  }

}

