#' bn_assign_lone_nodes
#'
#' @description
#' Assigns nodes that fell outside the manual_groups join (i.e. flagged with
#' the unassigned color) to the group, community_name, and color of their
#' nearest neighbor. Nearest is the connected node with the strongest edge
#' weight (`value` column on edges). Iterates so chains of unassigned nodes
#' resolve once any one reaches an assigned anchor.
#'
#' Accepts an `attribute_viz_prep` (with `nodes`/`edges`), a `viz_prep` (with
#' `attribute_viz_prep` nested), or a full `bn` engine result. By default the
#' returned object matches the input shape; override with `return_type`.
#'
#' @param obj An `attribute_viz_prep`, `viz_prep`, or `bn` engine result.
#' @param return_type One of `"input"`, `"node_table"`, or
#'   `"attribute_viz_prep"`. Default `"input"` returns the same shape as `obj`
#'   with `nodes` updated. `"node_table"` returns just the reassigned `nodes`
#'   data frame, ready to pass as `manual_groups` to `bn_to_netviz_prep()`.
#'   `"attribute_viz_prep"` returns the inner list with updated `nodes`.
#' @param unassigned_color Color flagging unassigned nodes. Defaults to
#'   `"#FF0000"` (the fallback color in `bn_to_netviz_prep()`).
#' @param max_iter Safeguard for the propagation loop.
#'
#' @return An object whose shape is determined by `return_type`.
#'
#' @export
bn_assign_lone_nodes <- function(
    obj,
    return_type = c("input", "node_table", "attribute_viz_prep"),
    unassigned_color = "#FF0000",
    max_iter = 50
){

  # obj = bn
  # return_type = "input"
  # unassigned_color = "#FF0000"
  # max_iter = 50

  return_type <- match.arg(return_type)


  # --- detect input shape and path to attribute_viz_prep ---
  has_avp <- function(x) is.list(x) && all(c("nodes", "edges") %in% names(x))

  candidate_paths <- list(
    attribute_viz_prep_bare = character(0),
    viz_prep                = "attribute_viz_prep",
    engine                  = c("viz_prep", "attribute_viz_prep"),
    finalized               = c("bn", "viz_prep", "attribute_viz_prep")
  )

  avp_path <- NULL
  for (name in names(candidate_paths)) {
    p <- candidate_paths[[name]]
    sub <- if (length(p) == 0) obj else tryCatch(obj[[p]], error = function(e) NULL)
    if (has_avp(sub)) {
      avp_path <- p
      input_type <- name
      break
    }
  }

  if (is.null(avp_path)) {
    stop("Could not locate an `attribute_viz_prep` (with `nodes` and `edges`) in `obj`. Pass an `attribute_viz_prep`, `viz_prep`, or `bn` engine result.")
  }

  avp <- if (length(avp_path) == 0) obj else obj[[avp_path]]


  # --- propagate group assignment along strongest edge ---
  nodes <- avp[["nodes"]]
  edges <- avp[["edges"]]

  edges_sym <- dplyr::bind_rows(
    edges %>% dplyr::select(from, to, value),
    edges %>% dplyr::select(from = to, to = from, value)
  )

  for (i in seq_len(max_iter)) {

    unassigned_ids <- nodes %>%
      dplyr::filter(color == unassigned_color) %>%
      dplyr::pull(id)

    if (length(unassigned_ids) == 0) break

    nearest <- edges_sym %>%
      dplyr::filter(from %in% unassigned_ids) %>%
      dplyr::left_join(
        nodes %>% dplyr::select(id, n_group = group, n_name = community_name, n_color = color),
        by = dplyr::join_by(to == id)
      ) %>%
      dplyr::filter(n_color != unassigned_color) %>%
      dplyr::group_by(from) %>%
      dplyr::slice_max(value, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()

    if (nrow(nearest) == 0) break

    nodes <- nodes %>%
      dplyr::rows_update(
        nearest %>% dplyr::select(id = from, group = n_group, community_name = n_name, color = n_color),
        by = "id"
      )
  }

  avp[["nodes"]] <- nodes


  # --- write back into the original shape ---
  obj_out <- if (length(avp_path) == 0) {
    avp
  } else {
    obj[[avp_path]] <- avp
    obj
  }


  # --- return ---
  if (return_type == "node_table")          return(nodes)
  if (return_type == "attribute_viz_prep")  return(avp)
  obj_out
}
