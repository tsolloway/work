#' seg_prototype_split_segments
#' @description seg_prototype_split_segments
#' @export
seg_prototype_split_segments <- function(
    seg,
    solution_to_split,
    seg_splits,
    new_solution_family_name,
    split_into = 2,
    split_vars = NULL,
    split_vars_profiles = NULL,
    lda_vars = NULL,
    lda_vars_profiles = NULL,
    use_greedy = TRUE,
    use_top_n_polars = 20,
    reduced_inputs_max = 14,
    priors = c("size", "equal"),
    method = c("kmeans", "medoid"),
    resp_id_name = NULL
){


  # solution_to_split = "kmeans_cluster_A4"
  # seg_splits = 4
  # new_solution_family_name = "Foo"
  # split_into = 2
  # split_vars = NULL
  # split_vars_profiles = NULL
  # lda_vars = NULL
  # lda_vars_profiles = NULL
  # use_greedy = TRUE
  # use_top_n_polars = 20
  # reduced_inputs_max = 14
  # priors = "size"
  # method = "kmeans"
  # resp_id_name = NULL


  priors <- match.arg(priors)
  method <- match.arg(method)


  if(is.null(resp_id_name)){
    resp_id_name <- seg %>% get_resp_id_name()
  }


  created_seed <- seg_split_segments(
    seg = seg, solution_name = solution_to_split,
    seg_splits = seg_splits, new_solution_name = new_solution_family_name,
    vars = split_vars,
    vars_profiles = split_vars_profiles,
    use_greedy = use_greedy,
    use_top_n_polars = use_top_n_polars,
    split_into = split_into, resp_id_name = resp_id_name, method = method,
    priors = priors,
    return_append_only = TRUE
  )


  seg <- cluster_prototype_seed(
    seg = seg, solution_family_name = new_solution_family_name,
    seed_name = glue("seed_{new_solution_family_name}"), seed = created_seed,
    vars = lda_vars, vars_profiles = lda_vars_profiles,
    use_greedy = use_greedy,
    use_top_n_polars = use_top_n_polars,
    reduced_inputs_max = reduced_inputs_max, priors = priors, resp_id_name = resp_id_name
  )

  return(seg)
}
