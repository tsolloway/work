#' seg_prototype_split_segments
#' @description seg_prototype_split_segments
#' @export
seg_prototype_split_segments <- function(
    seg, solution_to_split, seg_splits, new_solution_family_name, split_into = 2,
    vars = NULL, vars_profiles = NULL,
    reduced_inputs_max = 14,
    priors = c("size", "equal"),
    method = c("kmeans", "medoid"),
    id_name = "seg_uuid"
){

  priors <- match.arg(priors)
  method <- match.arg(method)

  created_seed <- seg_split_segments(
    seg = seg, solution_name = solution_to_split, seg_splits = seg_splits, new_solution_name = new_solution_family_name,
    split_into = split_into, id_name = id_name, method = method, return_append_only = TRUE
  )


  seg <- cluster_prototype_seed(
    seg = seg, solution_family_name = new_solution_family_name,
    seed_name = glue("seed_{new_solution_family_name}"), seed = created_seed,
    vars = vars, vars_profiles = vars_profiles,
    reduced_inputs_max = reduced_inputs_max, priors = priors, id_name = id_name
  )

  return(seg)
}
