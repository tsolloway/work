#' bn_get_hierarchy_levels
#'
#' @description
#' Computes hierarchy levels for every node in a Bayesian network so they can
#' be passed as a `level` column to `visNetwork::visHierarchicalLayout()`.
#' Roots (nodes with no incoming arcs) get level 0; every other node's level
#' is `max(parent_level) + 1`. This produces a clean top-down layout even when
#' the underlying DAG isn't a strict tree.
#'
#' Accepts an `attribute_viz_prep` (with `nodes`/`edges`), a `viz_prep` (with
#' `attribute_viz_prep` nested), or a full `bn` engine result.
#'
#' @param obj An `attribute_viz_prep`, `viz_prep`, or `bn` engine result.
#' @param root_nodes Optional character vector forcing a specific root set. If
#'   `NULL` (default) any node with no incoming arcs is treated as a root.
#'
#' @return A tibble with columns `id` and `level`.
#'
#' @export
bn_get_hierarchy_levels <- function(obj, root_nodes = NULL){

  # obj = bn
  # root_nodes = NULL


  # --- detect input shape and pull edges/nodes ---
  has_avp <- function(x) is.list(x) && all(c("nodes", "edges") %in% names(x))

  candidate_paths <- list(
    attribute_viz_prep_bare = character(0),
    viz_prep                = "attribute_viz_prep",
    engine                  = c("viz_prep", "attribute_viz_prep"),
    finalized               = c("bn", "viz_prep", "attribute_viz_prep")
  )

  avp <- NULL
  for (p in candidate_paths) {
    sub <- if (length(p) == 0) obj else tryCatch(obj[[p]], error = function(e) NULL)
    if (has_avp(sub)) {
      avp <- sub
      break
    }
  }

  if (is.null(avp)) {
    stop("Could not locate an `attribute_viz_prep` (with `nodes` and `edges`) in `obj`.")
  }

  edges <- avp[["edges"]]
  all_ids <- avp[["nodes"]][["id"]] %>% unique()


  # --- build parent map ---
  parents <- split(edges[["from"]], edges[["to"]])
  in_degree <- vapply(all_ids, function(id) length(parents[[id]] %||% character(0)), integer(1))
  names(in_degree) <- all_ids


  # --- determine roots ---
  if (is.null(root_nodes)) {
    root_nodes <- names(in_degree)[in_degree == 0]
  }

  if (length(root_nodes) == 0) {
    stop("No root nodes found (every node has at least one incoming arc). Pass `root_nodes` explicitly to break the cycle.")
  }


  # --- BFS / longest-path level assignment ---
  level_map <- stats::setNames(rep(NA_integer_, length(all_ids)), all_ids)
  level_map[root_nodes] <- 0L

  queue <- root_nodes
  while (length(queue)) {
    children <- edges %>%
      dplyr::filter(from %in% queue) %>%
      dplyr::pull(to) %>%
      unique()

    if (!length(children)) break

    next_queue <- character(0)
    for (child in children) {
      ps <- parents[[child]]
      parent_levels <- level_map[ps]
      if (any(is.na(parent_levels))) next  # wait until all parents resolved
      proposed <- max(parent_levels, na.rm = TRUE) + 1L
      if (is.na(level_map[[child]]) || proposed > level_map[[child]]) {
        level_map[[child]] <- proposed
        next_queue <- c(next_queue, child)
      }
    }
    queue <- unique(next_queue)
  }


  # --- assemble output ---
  tibble::tibble(
    id = names(level_map),
    level = unname(level_map)
  )
}


`%||%` <- function(a, b) if (is.null(a)) b else a
