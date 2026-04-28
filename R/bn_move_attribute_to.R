#' bn_move_attribute_to
#'
#' @description
#' Reassigns one or more attributes (nodes) to new communities and produces
#' the inputs needed to re-fit the network with those moves enforced.
#'
#' For each moved attribute, the function:
#' 1. Strips every arc touching that attribute from the current edges, leaving
#'    a `white_list` of surviving arcs that the next fit will preserve.
#' 2. Adds blacklist entries banning the attribute from connecting to any node
#'    outside the target community, forcing the engine to re-discover arcs
#'    only within that community.
#' 3. Updates the `manual_groups` table so the attribute carries the new
#'    `community_name`, `group`, and `color`.
#'
#' Pass either `community_name` or `group` (or both) in `moves` — the missing
#' column is resolved against the existing community lookup in `obj`.
#'
#' @param obj A `bn` engine result (or anything containing an
#'   `attribute_viz_prep` with `nodes` and `edges`).
#' @param moves A data frame with column `id` plus either `community_name`,
#'   `group`, or both, naming where each attribute should move.
#'
#' @section Validation:
#'
#' - **Unknown `id`** — hard error listing the offending ids. The function will
#'   not silently drop a node it cannot find in the network.
#' - **Unknown `community_name`** — `cli::cli_warn()` listing each invalid
#'   `(id → community_name)` pair alongside the valid set; those rows are
#'   skipped and the rest of the moves still apply.
#' - **Unknown `group`** — hard error (group numbers are unambiguous, so a
#'   typo there is treated as a programming bug rather than a stakeholder one).
#' - **All moves invalid** — emits a warning and returns the original
#'   `manual_groups`, an empty `black_list`, and `white_list` equal to the
#'   current edges (i.e. a no-op).
#'
#' @return A list with three elements:
#'   - `white_list`: data frame of surviving arcs (no row touches a moved id).
#'   - `black_list`: data frame of arcs to forbid (each moved id paired with
#'     every node outside its target community, in both directions).
#'   - `manual_groups`: updated nodes data frame with new community assignments
#'     applied to the moved ids.
#'
#' @examples
#' \dontrun{
#' moves <- tibble::tribble(
#'   ~id,        ~community_name,
#'   "q14b_46",  "Right Format",
#'   "q14a_12",  "Parental Support",
#'   "q14b_8",   "Brand Trust"
#' )
#'
#' out <- bn_previous$cb_direct$brand_preference_tb %>%
#'   bn_move_attribute_to(moves)
#'
#' bn_initial_networks(
#'   df            = df_impute,
#'   dv            = "brand_preference_tb",
#'   ivs           = iv_batteries,
#'   white_list    = out$white_list,
#'   black_list    = out$black_list,
#'   manual_groups = out$manual_groups
#' )
#' }
#'
#' @export
bn_move_attribute_to <- function(obj, moves){

  # obj = bn_previous_to_assigned$cb_direct$brand_preference_tb
  # moves = NULL


  # --- locate attribute_viz_prep ---
  has_avp <- function(x) is.list(x) && all(c("nodes", "edges") %in% names(x))

  candidate_paths <- list(
    character(0),
    "attribute_viz_prep",
    c("viz_prep", "attribute_viz_prep"),
    c("bn", "viz_prep", "attribute_viz_prep")
  )

  avp <- NULL
  for (p in candidate_paths) {
    sub <- if (length(p) == 0) obj else tryCatch(obj[[p]], error = function(e) NULL)
    if (has_avp(sub)) { avp <- sub; break }
  }

  if (is.null(avp)) {
    stop("Could not locate an `attribute_viz_prep` (with `nodes` and `edges`) in `obj`.")
  }


  # --- pull current state ---
  manual_groups <- avp[["nodes"]] %>% as.data.frame()
  edges <- avp[["edges"]] %>% as.data.frame()


  # --- validate moves ---
  if (!"id" %in% names(moves)) {
    stop("`moves` must have an `id` column.")
  }

  has_name  <- "community_name" %in% names(moves)
  has_group <- "group" %in% names(moves)

  if (!has_name && !has_group) {
    stop("`moves` must have `community_name`, `group`, or both.")
  }

  unknown_ids <- setdiff(moves[["id"]], manual_groups[["id"]])
  if (length(unknown_ids) > 0) {
    stop("These `id`s are not in the network: ", paste(unknown_ids, collapse = ", "))
  }


  # --- resolve target community lookup ---
  community_lookup <- manual_groups %>%
    dplyr::distinct(community_name, group, color)

  warn_unknown_community <- function(moves) {
    unknown <- moves %>%
      dplyr::filter(!community_name %in% community_lookup[["community_name"]])
    if (nrow(unknown) > 0) {
      cli::cli_warn(c(
        "!" = "{nrow(unknown)} move{?s} reference{?s/} community name{?s} not in the current network — skipping {?it/them}.",
        "i" = "Valid: {.val {community_lookup[['community_name']]}}",
        "x" = "Unknown: {paste0(unknown[['id']], ' → ', unknown[['community_name']])}"
      ))
    }
    moves %>% dplyr::filter(community_name %in% community_lookup[["community_name"]])
  }


  if (has_name && !has_group) {
    moves <- warn_unknown_community(moves)
    moves_resolved <- moves %>%
      dplyr::select(id, community_name) %>%
      dplyr::left_join(community_lookup, by = "community_name")

  } else if (has_group && !has_name) {
    unknown <- setdiff(moves[["group"]], community_lookup[["group"]])
    if (length(unknown) > 0) {
      stop("Unknown target group(s): ", paste(unknown, collapse = ", "))
    }
    moves_resolved <- moves %>%
      dplyr::select(id, group) %>%
      dplyr::left_join(community_lookup, by = "group")

  } else {
    moves <- warn_unknown_community(moves)
    moves_resolved <- moves %>%
      dplyr::select(id, community_name) %>%
      dplyr::left_join(community_lookup, by = "community_name")
  }


  if (nrow(moves_resolved) == 0) {
    cli::cli_warn("No valid moves remain after filtering. Returning the network unchanged.")
    return(list(
      white_list    = edges %>% dplyr::select(from, to) %>% dplyr::distinct() %>% as.data.frame(),
      black_list    = data.frame(from = character(0), to = character(0)),
      manual_groups = manual_groups
    ))
  }


  # --- apply moves to manual_groups ---
  manual_groups_new <- manual_groups %>%
    dplyr::rows_update(
      moves_resolved %>% dplyr::select(id, community_name, group, color),
      by = "id"
    )


  # --- white_list = surviving edges (none touching a moved id) ---
  moved_ids <- moves_resolved[["id"]]

  white_list <- edges %>%
    dplyr::filter(!from %in% moved_ids & !to %in% moved_ids) %>%
    dplyr::select(from, to) %>%
    dplyr::distinct() %>%
    as.data.frame()


  # --- black_list = ban each moved id from non-target-community nodes ---
  black_list <- purrr::map_dfr(seq_len(nrow(moves_resolved)), function(i) {

    this_id     <- moves_resolved[["id"]][[i]]
    this_target <- moves_resolved[["community_name"]][[i]]

    target_members <- manual_groups_new[["id"]][manual_groups_new[["community_name"]] == this_target]
    non_members    <- setdiff(manual_groups_new[["id"]], c(target_members, this_id))

    dplyr::bind_rows(
      tibble::tibble(from = this_id,     to = non_members),
      tibble::tibble(from = non_members, to = this_id)
    )

  }) %>%
    dplyr::distinct() %>%
    as.data.frame()


  list(
    white_list    = white_list,
    black_list    = black_list,
    manual_groups = manual_groups_new
  )
}
