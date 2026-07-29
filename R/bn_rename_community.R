#' Rename communities in a finalized network object
#'
#' @description Renames one or more communities everywhere the name is stored
#'   in a \code{bn_finalize_network()} result, so downstream writers
#'   (\code{bn_write()}, \code{bn_report()}) stay consistent. Community names
#'   live in several places - the attribute node table, the community network's
#'   nodes and edges, and the impact tables - and editing only one of them
#'   desyncs the workbook (e.g. a renamed Community Drivers sheet against an
#'   old community map image).
#'
#'   Renaming after finalizing is safe because the community name is a label
#'   only: membership, impacts, and the maps are keyed off the attribute
#'   assignments, which are untouched. To change membership instead, use
#'   \code{bn_move_attribute_to()} and re-finalize.
#'
#' @param obj A list returned by \code{bn_finalize_network()}.
#' @param rename Named character vector of \code{c("old name" = "new name")}
#'   pairs. Every old name must exist in the object; a new name that collides
#'   with a community not itself being renamed is rejected (that would merge
#'   two communities' labels while leaving their rows separate).
#' @param verbose Logical. Report which slots were updated. Default TRUE.
#'
#' @return \code{obj} with the community names replaced.
#'
#' @examples
#' \dontrun{
#' bn_final <- bn_final %>%
#'   bn_rename_community(c("Modern Expertise" = "Premium Signals"))
#' }
#'
#' @seealso [bn_finalize_network()], [bn_move_attribute_to()]
#'
#' @export
bn_rename_community <- function(obj, rename, verbose = TRUE) {

  if (!is.list(obj)) stop("'obj' must be a bn_finalize_network() result.")
  if (!is.character(rename) || length(rename) == 0 || is.null(names(rename)) ||
      any(!nzchar(names(rename))) || anyNA(rename) || any(!nzchar(rename))) {
    stop("'rename' must be a named character vector, e.g. ",
         "c(\"Modern Expertise\" = \"Premium Signals\").")
  }

  existing <- obj[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]][["community_name"]]
  if (is.null(existing)) {
    stop("Could not find community assignments at ",
         "obj$bn$viz_prep$attribute_viz_prep$nodes$community_name - is 'obj' a ",
         "bn_finalize_network() result?")
  }
  existing <- unique(as.character(existing))

  missing_old <- setdiff(names(rename), existing)
  if (length(missing_old) > 0) {
    stop("'rename' names communities that do not exist: ",
         paste0("'", missing_old, "'", collapse = ", "),
         ". Available: ", paste0("'", sort(existing), "'", collapse = ", "), ".")
  }
  # A new name landing on a community that is not itself being renamed would
  # give two separate community rows the same label.
  collide <- intersect(unname(rename), setdiff(existing, names(rename)))
  if (length(collide) > 0) {
    stop("'rename' would duplicate the label of existing communit",
         if (length(collide) > 1) "ies: " else "y: ",
         paste0("'", collide, "'", collapse = ", "),
         ". Rename that community in the same call, or use ",
         "bn_move_attribute_to() to merge membership.")
  }

  n_changed <- 0L
  touched <- character(0)

  # Replace values while preserving the column's class - viz_prep columns are
  # `glue` vectors, the impact tables are plain character, and manual-group
  # tables can carry factors.
  .map <- function(x) {
    if (is.null(x)) return(x)
    if (is.factor(x)) {
      lv <- levels(x)
      hit <- lv %in% names(rename)
      if (any(hit)) {
        n_changed <<- n_changed + sum(as.character(x) %in% names(rename))
        levels(x)[hit] <- unname(rename[lv[hit]])
      }
      return(x)
    }
    cls <- class(x)
    chr <- as.character(x)
    hit <- chr %in% names(rename)
    if (!any(hit)) return(x)
    n_changed <<- n_changed + sum(hit)
    chr[hit] <- unname(rename[chr[hit]])
    class(chr) <- cls
    chr
  }

  # Apply .map to obj[[path]][[col]] when both exist, recording the slot.
  .apply_at <- function(o, path, cols) {
    node <- o
    for (p in path) {
      if (is.null(node[[p]])) return(o)
      node <- node[[p]]
    }
    if (!is.data.frame(node)) return(o)
    before <- n_changed
    for (cl in intersect(cols, names(node))) node[[cl]] <- .map(node[[cl]])
    if (n_changed > before) {
      touched <<- c(touched, paste0(paste(path, collapse = "$"),
                                    " (", n_changed - before, ")"))
    }
    .assign_in(o, path, node)
  }
  .assign_in <- function(o, path, value) {
    if (length(path) == 1) { o[[path]] <- value; return(o) }
    o[[path[1]]] <- .assign_in(o[[path[1]]], path[-1], value)
    o
  }

  # Attribute + community node/edge tables (also inside per-subgroup objects,
  # which carry a viz_prep when built from a bn_engine() result).
  viz_roots <- list(c("bn", "viz_prep"))
  if (!is.null(obj[["bn_subgroups"]])) {
    for (sg in names(obj[["bn_subgroups"]])) {
      viz_roots <- c(viz_roots, list(c("bn_subgroups", sg, "viz_prep")))
    }
  }
  for (root in viz_roots) {
    obj <- .apply_at(obj, c(root, "attribute_viz_prep", "nodes"),
                     c("community_name"))
    obj <- .apply_at(obj, c(root, "community_viz_prep", "nodes"),
                     c("id", "label", "community_name"))
    obj <- .apply_at(obj, c(root, "community_viz_prep", "edges"),
                     c("from", "to"))
  }

  # Impact tables: attribute tables carry a Community label column, community
  # tables key on it.
  for (tb in c("table_attribute", "table_attribute_weighted",
               "table_community", "table_community_weighted")) {
    obj <- .apply_at(obj, c("impacts", tb), c("Community", "Variable"))
  }

  # Prioritizations, when present, label community-level runs the same way.
  if (!is.null(obj[["prioritizations"]])) {
    for (tb in names(obj[["prioritizations"]])) {
      obj <- .apply_at(obj, c("prioritizations", tb), c("Community", "Variable"))
    }
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "Renamed {length(rename)} communit{?y/ies} in {length(touched)} slot{?s} ({n_changed} value{?s})."
    )
    for (t in touched) cli::cli_alert_info(t)
  }

  obj
}
