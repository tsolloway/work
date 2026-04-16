#' bn_get_group_names
#'
#' @description Extract the current community group names from a Bayesian
#'   Network results object. Returns a tibble (single engine) or a named list
#'   of tibbles (multiple engines) with columns \code{group} and
#'   \code{group_name}.
#'
#' @param results A BN results object from [bn_initial_networks()] or
#'   [bn_engine()]. Can be a single engine result, an unsupervised result,
#'   or the full supervised output from \code{bn_initial_networks()}.
#'
#' @return A tibble with columns \code{group} and \code{group_name} when
#'   \code{results} contains a single engine. When multiple engines are
#'   detected, a named list of tibbles keyed by the engine path.
#'
#' @seealso [bn_name_groups()]
#'
#' @export
bn_get_group_names <- function(results) {

  engine_paths <- .bn_name_find_engines(results)
  if (length(engine_paths) == 0) {
    cli::cli_warn("No engine results with viz_prep found. Returning NULL.")
    return(NULL)
  }

  extract_one <- function(path) {
    engine <- .bn_name_get_engine(results, path)
    nodes <- engine$viz_prep$attribute_viz_prep$nodes
    nodes %>%
      dplyr::distinct(group, community_name) %>%
      dplyr::arrange(group) %>%
      dplyr::rename(group_name = community_name) %>%
      tibble::as_tibble()
  }

  if (length(engine_paths) == 1) {
    return(extract_one(engine_paths[[1]]))
  }

  # Multiple engines → named list keyed by path
  path_labels <- purrr::map_chr(engine_paths, ~ paste(.x, collapse = "$"))
  purrr::map(engine_paths, extract_one) %>%
    rlang::set_names(path_labels)
}
