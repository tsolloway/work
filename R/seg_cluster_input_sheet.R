#' seg_cluster_input_sheet
#' @description seg_cluster_input_sheet
#' @export
seg_cluster_input_sheet <- function(
    seg, id_name = "seg_uuid", filter_logical_vector = NULL,
    n_min = 4, n_max = 7,
    vary_percent = .1, side_bias_percent = .1,
    priors = c("equal", "size"), iter_max = 100000, nstart = 10,
    do_kmeans = TRUE, do_medoid = TRUE, do_gaus_mix = TRUE, do_hierarchical = TRUE
){

  imap(
    seg[["solutions"]][["inputs"]],
    ~cluster_solution_family(
      seg = seg,
      inputs = .x,
      solution_name = .y,
      id_name = id_name,
      filter_logical_vector = filter_logical_vector,
      n_min = n_min, n_max = n_max,
      vary_percent = vary_percent,
      side_bias_percent = side_bias_percent,
      priors = priors, iter_max = iter_max, nstart = nstart,
      do_kmeans = do_kmeans, do_medoid = do_medoid,
      do_gaus_mix = do_gaus_mix, do_hierarchical = do_hierarchical
    )
  )


}
