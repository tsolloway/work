#' seg_split_segments
#'
#' @description Splits one or more segments from an existing solution into
#'   sub-segments via k-means or PAM, then renumbers the combined result into
#'   a new solution column. This is the primary tool for refining a prototype
#'   solution — e.g., splitting a large, heterogeneous segment into two more
#'   homogeneous ones.
#'
#'   **Workflow:**
#'   1. For each segment in `seg_splits`, subsets the data to that segment's
#'      respondents and runs [cluster_kmeans()] or [cluster_medoid()] to
#'      partition them into `split_into` sub-groups.
#'   2. Reassembles the full solution by keeping unsplit segments as-is and
#'      inserting the new sub-segments, renumbered sequentially.
#'   3. Stores the result as a `seed_{new_solution_name}` column on
#'      `seg$data$with_solutions`.
#'   4. If `prototype_seed = TRUE` (default), calls
#'      [seg_cluster_prototype_seed()] to fit the LDA typing tool across
#'      all segments in the new seed.
#'
#'   For example, splitting segments 3 and 5 from a 7-segment solution each
#'   into 2 produces a 9-segment seed (5 original + 2 + 2).
#'
#' @param seg A seg object with `seg$data$with_solutions` populated.
#' @param solution_name Character. Column name of the existing solution to
#'   split from (e.g. `"LDA_opt_kmeans_A7"`).
#' @param seg_splits Integer vector. Which segment numbers to split
#'   (e.g. `c(3, 5)`).
#' @param new_solution_name Character. Base name for the new solution column.
#'   The output column will be `seed_{new_solution_name}`.
#' @param vars Character vector or `NULL`. Polar input variables for the
#'   sub-clustering. If `NULL` and `use_greedy = TRUE`, selected via
#'   [get_greedy_vars()]. If `NULL` and `use_greedy = FALSE`, uses all RS
#'   polars.
#' @param vars_profiles Character vector or `NULL`. Profile variables
#'   corresponding to `vars`. If `NULL`, derived from the spec.
#' @param split_into Integer. Number of sub-segments to create from each
#'   split segment (default: `2`).
#' @param resp_id_name Character or `NULL`. Respondent ID column. If `NULL`,
#'   auto-detected via [get_resp_id_name()].
#' @param use_greedy Logical. If `TRUE` (default), select polar inputs via
#'   greedy Wilks' Lambda when `vars = NULL`.
#' @param use_top_n_polars Integer. Number of top polars for greedy selection
#'   (default: `20`).
#' @param method Character. Clustering method for the split: `"kmeans"`
#'   (default) or `"medoid"` (PAM).
#' @param priors Character. LDA prior method: `"equal"` (default) or `"size"`.
#' @param filter_logical_vector Logical vector or character column name used to
#'   subset respondents before clustering. When character, the named column is
#'   evaluated as a logical filter (e.g. `"missing_polars"`). Filtered-out
#'   respondents receive `NA` in the output seed column.
#' @param reduced_inputs_max Integer, character vector, or `NULL`. Cap on the
#'   number of reduced input variables passed to
#'   [seg_cluster_prototype_seed()]. If numeric, truncates the greedy-selected
#'   variables to this many. If character, uses the supplied variable names
#'   directly. If `NULL`, no cap is applied (default: `14`).
#' @param prototype_seed Logical. If `TRUE` (default), automatically calls
#'   [seg_cluster_prototype_seed()] after sub-clustering to fit a unified LDA
#'   typing tool across the new seed. If `FALSE`, only the seed column is
#'   written and LDA must be run separately.
#' @param return_append_only Logical. If `TRUE`, returns just the append
#'   data frame instead of the full seg object. Useful when chaining into
#'   [seg_prototype_split_segments()] (default: `FALSE`).
#'
#' @return The seg object with `seg$data$with_solutions` updated to include
#'   the new `seed_{new_solution_name}` column. If `prototype_seed = TRUE`,
#'   also includes the full LDA solution in `seg$solutions$analysis`,
#'   updated summary table, and segment append columns. If
#'   `return_append_only = TRUE`, returns the append data frame directly
#'   (skips prototype seed).
#'
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
    filter_logical_vector = NULL,
    reduced_inputs_max = 14,
    prototype_seed = TRUE,
    return_append_only = FALSE
){

  method <- match.arg(method)
  priors <- match.arg(priors)

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


  # filter data if available
  if(!is.null(filter_logical_vector)){

    if(is.character(filter_logical_vector)){

      df <- df %>% dplyr::filter(.data[[filter_logical_vector]])

    }else if(is.logical(filter_logical_vector)){

      df <- df %>% dplyr::filter(filter_logical_vector)
    }
  }


  if(use_greedy && is.null(vars)){
    vars <- seg %>% get_greedy_vars(
      df = df %>% dplyr::filter(.data[[solution_name]] %in% seg_splits),
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

    df_append <- purrr::map(
      seg_splits,
      ~cluster_kmeans(
        df = df %>% dplyr::filter(.data[[solution_name]] == .x),
        vars = vars,
        vars_profiles = vars_profiles,
        priors = priors,
        solution_name = "dummy", n_min = split_into, n_max = split_into, resp_id_name = resp_id_name
      ) %>%
        purrr::keep_at("all_inputs") %>%
        purrr::flatten() %>%
        purrr::pluck("cluster_seed")
    )

  }else if(method == "medoid"){

    df_append <- purrr::map(
      seg_splits,
      ~cluster_medoid(
        df = df %>% dplyr::filter(.data[[solution_name]] == .x),
        vars = vars,
        vars_profiles = vars_profiles,
        priors = priors,
        solution_name = "dummy", n_min = split_into, n_max = split_into, resp_id_name = resp_id_name
      ) %>%
        purrr::keep_at("all_inputs") %>%
        purrr::flatten() %>%
        purrr::pluck("cluster_seed")
    )

  }


  df_append <- df_append %>%
    purrr::flatten() %>%
    rlang::set_names(seg_splits) %>%
    purrr::imap(
      ~.x %>% setNames(c("id", glue::glue("new_cut_{.y}")))
    ) %>%
    purrr::reduce(dplyr::full_join, by = "id") %>%
    dplyr::full_join(
      df %>% dplyr::select(dplyr::all_of(c("seg_uuid", solution_name))),
      .,
      by = dplyr::join_by(seg_uuid == id)
    ) %>% dplyr::mutate(
      "seed_{new_solution_name}" := !!rlang::sym(solution_name) %>% dplyr::replace_values(seg_splits ~ NA)
    ) #%>%
    # dplyr::select(-dplyr::all_of(solution_name))


  for(i in seg_splits){
    y <- glue::glue("seed_{new_solution_name}")
    x <- glue::glue("new_cut_{i}")


    if(i == seg_splits[1]){

      ymin <- min_inf_zero(df_append[[y]])

      if(ymin > 1){

        df_append <- df_append %>%
          dplyr::mutate(
            "{y}" := !!rlang::sym(y) - ymin + 1
          )
      }
      rm(ymin)
    }


    df_append <- df_append %>%
      dplyr::mutate(
        "{y}" := ifelse(is.na(!!rlang::sym(y)), !!rlang::sym(x) + max_inf_zero(!!rlang::sym(y)), !!rlang::sym(y))
      ) %>%
      dplyr::select(-dplyr::all_of(x))


  };rm(y,x,i)



  # drop solution_name to avoid .x/.y duplication — it already exists in with_solutions
  df_append <- df_append %>%
    dplyr::select(-dplyr::all_of(solution_name))

  # drop any existing seed column from with_solutions so re-runs don't create .x/.y

  seed_col <- glue::glue("seed_{new_solution_name}")
  seg[["data"]][["with_solutions"]] <- seg[["data"]][["with_solutions"]] %>%
    dplyr::select(-dplyr::any_of(seed_col))

  seg[["data"]][["with_solutions"]] <- dplyr::full_join(
    seg[["data"]][["with_solutions"]],
    df_append,
    by = dplyr::join_by(seg_uuid)
  )


  if(return_append_only){
    return(df_append)
  }

  if(prototype_seed){
    seg <- seg %>%
      seg_cluster_prototype_seed(
        solution_family_name = new_solution_name,
        seed_name            = glue::glue("seed_{new_solution_name}"),
        vars                 = vars,
        vars_profiles        = vars_profiles,
        filter_name          = if(is.character(filter_logical_vector)) filter_logical_vector else NULL,
        use_greedy           = use_greedy,
        use_top_n_polars     = use_top_n_polars,
        priors               = priors,
        resp_id_name         = resp_id_name,
        reduced_inputs_max   = reduced_inputs_max
      )
  }

  return(seg)

}

