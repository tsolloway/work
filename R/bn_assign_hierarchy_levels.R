#' bn_assign_hierarchy_levels
#'
#' @description
#' Joins a hierarchy `level` column onto the nodes table of a Bayesian network
#' object, so `visNetwork::visHierarchicalLayout()` can use it. Pair with
#' `bn_get_hierarchy_levels()` to compute the levels first.
#'
#' Accepts an `attribute_viz_prep` (with `nodes`/`edges`), a `viz_prep` (with
#' `attribute_viz_prep` nested), or a full `bn` engine result. The returned
#' object matches the input shape with `nodes` updated.
#'
#' @param obj An `attribute_viz_prep`, `viz_prep`, or `bn` engine result.
#' @param levels A data frame with columns `id` and `level` (typically the
#'   output of `bn_get_hierarchy_levels()`).
#'
#' @return The input `obj` with `level` joined onto the nodes data frame.
#'
#' @export
bn_assign_hierarchy_levels <- function(obj, levels){

  # obj = bn
  # levels = bn_hierarchy_levels


  if (!is.data.frame(levels) || !all(c("id", "level") %in% names(levels))) {
    stop("`levels` must be a data frame with columns `id` and `level`.")
  }


  # --- detect input shape and path to attribute_viz_prep ---
  has_avp <- function(x) is.list(x) && all(c("nodes", "edges") %in% names(x))

  candidate_paths <- list(
    attribute_viz_prep_bare = character(0),
    viz_prep                = "attribute_viz_prep",
    engine                  = c("viz_prep", "attribute_viz_prep"),
    finalized               = c("bn", "viz_prep", "attribute_viz_prep")
  )

  avp_path <- NULL
  for (p in candidate_paths) {
    sub <- if (length(p) == 0) obj else tryCatch(obj[[p]], error = function(e) NULL)
    if (has_avp(sub)) {
      avp_path <- p
      break
    }
  }

  if (is.null(avp_path)) {
    stop("Could not locate an `attribute_viz_prep` (with `nodes` and `edges`) in `obj`. Pass an `attribute_viz_prep`, `viz_prep`, or `bn` engine result.")
  }

  avp <- if (length(avp_path) == 0) obj else obj[[avp_path]]


  # --- join levels onto nodes (overwrite if column already there) ---
  avp[["nodes"]] <- avp[["nodes"]] %>%
    dplyr::select(-dplyr::any_of("level")) %>%
    dplyr::left_join(levels %>% dplyr::select(id, level), by = "id")


  # --- write back into the original shape ---
  if (length(avp_path) == 0) {
    avp
  } else {
    obj[[avp_path]] <- avp
    obj
  }
}
