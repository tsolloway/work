#' seg_split_segments
#' @description seg_split_segments
#' @export
seg_split_segments <- function(
    seg,
    solution_name,
    seg_splits,
    new_solution_name,
    vars = NULL,
    vars_profiles = NULL,
    split_into = 2,
    resp_id_name = NULL,
    use_greedy = TRUE,
    use_top_n_polars = 20,
    method = c("kmeans", "medoid"),
    priors = c("equal", "size"),
    return_append_only = FALSE
){

  method <- match.arg(method)

  df <- seg[["data"]][["with_solutions"]]

  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }


  if(is.null(df) || all(is.na(df))){

    warning("Returning append only")

    return_append_only <- TRUE

    df <- seg[["data"]][["with_shell"]]
  }



  max_inf_zero <- function(x){
    ifelse(is.infinite(max(x, na.rm = TRUE)), 0, max(x, na.rm = TRUE)) %>% suppressWarnings()
  }


  min_inf_zero <- function(x){
    ifelse(is.infinite(min(x, na.rm = TRUE)), 0, min(x, na.rm = TRUE)) %>% suppressWarnings()
  }


  if(use_greedy && is.null(vars)){
    vars <- seg %>% get_greedy_vars(
      df = df %>% filter(.data[[solution_name]] %in% seg_splits),
      top = use_top_n_polars)
  }


  if(is.null(vars)){
    vars <- seg %>% seg_get_vars_polars(.return = "rs")
    vars_profiles <- seg %>% seg_get_vars_polars(.return = "profiles")
  }


  if(is.null(vars_profiles)){
    vars_profiles <- seg_get_vars_polars(seg, .return = "all") %>%
      dplyr::filter(rs_var %in% vars) %>%
      dplyr::select(profile_var) %>%
      unlist() %>%
      setNames(NULL)
  }


  if(method == "kmeans"){

    df_append <- map(
      seg_splits,
      ~cluster_kmeans(
        df = df %>% filter(.data[[solution_name]] == .x),
        vars = vars,
        vars_profiles = vars_profiles,
        priors = priors,
        solution_name = "dummy", n_min = split_into, n_max = split_into, resp_id_name = resp_id_name
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
        vars = vars,
        vars_profiles = vars_profiles,
        priors = priors,
        solution_name = "dummy", n_min = split_into, n_max = split_into, resp_id_name = resp_id_name
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
      "seed_{new_solution_name}" := !!sym(solution_name) %>% replace_values(seg_splits ~ NA)
    ) #%>%
    # select(-all_of(solution_name))


  for(i in seg_splits){
    y <- glue("seed_{new_solution_name}")
    x <- glue("new_cut_{i}")


    if(i == seg_splits[1]){

      ymin <- min_inf_zero(df_append[[y]])

      if(ymin > 1){

        df_append <- df_append %>%
          mutate(
            "{y}" := !!sym(y) - ymin + 1
          )
      }
      rm(ymin)
    }


    df_append <- df_append %>%
      mutate(
        "{y}" := ifelse(is.na(!!sym(y)), !!sym(x) + max_inf_zero(!!sym(y)), !!sym(y))
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

