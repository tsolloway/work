#' bn_assign_lone_nodes
#'
#' @description
#' Assigns nodes that fell outside the manual_groups join (i.e. flagged with
#' the unassigned color) to the group, community_name, and color of their
#' strongest neighbor (by the `value` column on edges). A node resolves only
#' once its strongest neighbor overall is itself assigned, so chains of
#' unassigned nodes inherit along their strongest path rather than flooding
#' from whichever community the chain touches first. Deadlocks (mutually
#' strongest unassigned pairs) are broken one node at a time via the
#' strongest edge to any assigned neighbor.
#'
#' When `enforce_contiguity = TRUE` (default), assigned communities that are
#' split across disconnected fragments of the network (e.g. groups carried
#' over from a previous network) are repaired first: the largest connected
#' component anchors the community and the minority fragments are flagged
#' unassigned, so the propagation re-homes them. Every community in the
#' result is then a single connected region.
#'
#' Accepts an `attribute_viz_prep` (with `nodes`/`edges`), a `viz_prep` (with
#' `attribute_viz_prep` nested), or a full `bn` engine result. By default the
#' returned object matches the input shape; override with `return_type`.
#'
#' @param obj An `attribute_viz_prep`, `viz_prep`, or `bn` engine result.
#' @param enforce_contiguity Logical; if `TRUE` (default), minority fragments
#'   of assigned communities are also reassigned so each community ends up
#'   as one connected region.
#' @param return_type One of `"input"`, `"node_table"`, or
#'   `"attribute_viz_prep"`. Default `"input"` returns the same shape as `obj`
#'   with `nodes` updated. `"node_table"` returns just the reassigned `nodes`
#'   data frame, ready to pass as `manual_groups` to `bn_to_netviz_prep()`.
#'   `"attribute_viz_prep"` returns the inner list with updated `nodes`.
#' @param unassigned_color Color flagging unassigned nodes. Defaults to
#'   `"#FF0000"` (the fallback color in `bn_to_netviz_prep()`).
#' @param max_iter Safeguard for the propagation loop. Deadlocks resolve one
#'   node per iteration, so allow at least as many iterations as there are
#'   unassigned nodes.
#'
#' @return An object whose shape is determined by `return_type`.
#'
#' @export
bn_assign_lone_nodes <- function(
    obj,
    return_type = c("input", "node_table", "attribute_viz_prep"),
    enforce_contiguity = TRUE,
    unassigned_color = "#FF0000",
    max_iter = 200
){

  # obj = bn
  # return_type = "input"
  # unassigned_color = "#FF0000"
  # max_iter = 200

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


  # --- flag minority fragments of split communities as unassigned ---
  # A community carried over from another network can land on this one as
  # several disconnected fragments; the propagation below can only grow
  # fragments, never merge them. Keep the largest component as the anchor
  # and let the stranded nodes be re-homed like any other lone node.
  if (enforce_contiguity) {

    g <- igraph::graph_from_data_frame(
      edges %>% dplyr::select(from, to),
      directed = FALSE,
      vertices = nodes %>% dplyr::select(id)
    )

    assigned_groups <- nodes %>%
      dplyr::filter(color != unassigned_color) %>%
      dplyr::distinct(group) %>%
      dplyr::pull(group)

    for (grp in assigned_groups) {
      grp_ids <- nodes %>%
        dplyr::filter(group == grp, color != unassigned_color) %>%
        dplyr::pull(id)
      if (length(grp_ids) < 2) next

      comps <- igraph::components(
        igraph::induced_subgraph(g, vids = igraph::V(g)[name %in% grp_ids])
      )
      if (comps$no < 2) next

      keep <- which.max(comps$csize)
      stranded <- names(comps$membership)[comps$membership != keep]
      nodes <- nodes %>%
        dplyr::mutate(color = ifelse(id %in% stranded, unassigned_color, color))
    }
  }


  for (i in seq_len(max_iter)) {

    unassigned_ids <- nodes %>%
      dplyr::filter(color == unassigned_color) %>%
      dplyr::pull(id)

    if (length(unassigned_ids) == 0) break

    # a node resolves only when its strongest neighbor overall is already
    # assigned — chains inherit along their strongest path instead of
    # flooding from whichever community the chain happens to touch first
    nearest <- edges_sym %>%
      dplyr::filter(from %in% unassigned_ids) %>%
      dplyr::group_by(from) %>%
      dplyr::slice_max(value, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(
        nodes %>% dplyr::select(id, n_group = group, n_name = community_name, n_color = color),
        by = dplyr::join_by(to == id)
      ) %>%
      dplyr::filter(n_color != unassigned_color)

    # stalled: every remaining unassigned node's strongest neighbor is itself
    # unassigned (e.g. two lone nodes strongest-linked to each other). Break
    # the deadlock for the single node with the strongest edge to any
    # assigned neighbor, then resume the strict rule.
    if (nrow(nearest) == 0) {
      nearest <- edges_sym %>%
        dplyr::filter(from %in% unassigned_ids) %>%
        dplyr::left_join(
          nodes %>% dplyr::select(id, n_group = group, n_name = community_name, n_color = color),
          by = dplyr::join_by(to == id)
        ) %>%
        dplyr::filter(n_color != unassigned_color) %>%
        dplyr::slice_max(value, n = 1, with_ties = FALSE)
    }

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
