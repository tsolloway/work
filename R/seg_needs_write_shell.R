#' seg_needs_write_shell
#'
#' @description Wraps [seg_write_shell()] for a needs project, where there are
#'   two frames and therefore two possible bases. Picks the frame from `level`,
#'   and stamps the base into the file name so a grid-level shell can never be
#'   mistaken for a person-level one.
#'
#'   This is the means check. Run it right after the stack, before any
#'   structure or typing work: cut the grid frame by context and the person
#'   frame by a stable demographic, and read the two shells against the raw
#'   data. If the stack is wrong, it shows up here as nonsense means long
#'   before it shows up as a strange solution.
#'
#'   \strong{Base.} Every count on a grid-level shell is grids, not people. A
#'   respondent contributing three contexts is in the base three times. Say so
#'   on the deliverable.
#'
#' @param seg A seg object after [seg_get_spec()] has executed the spec.
#' @param solution_var Character. The variable to cut by. On the grid frame
#'   `"context"` is the natural means check; on the person frame use a
#'   demographic.
#' @param level Character. `"grid"` (default) or `"person"`.
#' @param verbose Logical. Report the column and block bookkeeping behind the
#'   level choice. Off by default - the one-line summary says what the shell
#'   contains, and the detail only matters when something is out of step.
#' @param ... Passed through to [seg_write_shell()].
#'
#' @return Whatever [seg_write_shell()] returns, invisibly.
#'
#' @export
seg_needs_write_shell <- function(seg, solution_var, level = c("grid", "person"),
                                  verbose = FALSE, ...){

  level <- match.arg(level)

  df <- seg[["data"]][["with_shell"]]

  if(!is.data.frame(df)){
    stop("No executed spec found. Run seg_get_spec() before seg_needs_write_shell().",
         call. = FALSE)
  }

  if(!"person_id" %in% names(df)){
    stop("person_id is not on the executed frame - was the stacked file loaded?",
         call. = FALSE)
  }

  seg_local <- seg

  # A column is GRID-level when it differs down a respondent's rows. That is the
  # whole distinction: such a column has no single value for a person, so it can
  # neither be collapsed to a person row nor read as a person attribute.
  # Detected rather than listed, so it stays correct if the blocks change.
  varying <- vapply(
    setdiff(names(df), "person_id"),
    function(v) any(tapply(df[[v]], df$person_id, function(x) dplyr::n_distinct(x) > 1)),
    logical(1)
  )
  grid_cols <- names(varying)[varying]

  # A block is grid-level when every one of its variables is
  is_grid_block <- function(tbl){
    if(is.null(tbl) || !"vars" %in% names(tbl)) return(logical(0))
    vapply(tbl[["vars"]], function(v){
      if(is.null(v) || !"var" %in% names(v)) return(FALSE)
      vv <- stats::na.omit(v[["var"]])
      length(vv) > 0 && all(vv %in% grid_cols)
    }, logical(1))
  }

  prof_spec  <- seg[["spec"]][["profiles"]]
  prof_shell <- seg[["shell"]][["profiles"]]
  gb         <- is_grid_block(prof_shell)
  grid_blocks <- if(length(gb)) prof_shell[["prefix"]][gb] else character(0)

  n_polar <- if(is.null(seg[["shell"]][["polars"]])) 0L else nrow(seg[["shell"]][["polars"]])
  n_prof  <- if(is.null(prof_shell)) 0L else nrow(prof_shell)

  if(level == "person"){

    # Drop the grid-level columns and the blocks made of them, from the frame,
    # the spec and the shell - all three hold a copy of the variable list and
    # seg_write_shell() fails if they disagree.
    df <- dplyr::distinct(df[setdiff(names(df), grid_cols)], .data$person_id, .keep_all = TRUE)

    if(length(gb))                 seg_local[["shell"]][["profiles"]] <- prof_shell[!gb, , drop = FALSE]
    if(length(is_grid_block(prof_spec))) seg_local[["spec"]][["profiles"]] <- prof_spec[!is_grid_block(prof_spec), , drop = FALSE]

    kept <- n_polar + sum(!gb)

    message(
      "Person shell: ", kept, " of ", n_polar + n_prof, " blocks",
      if(length(grid_blocks))
        paste0(". Excludes ", paste(grid_blocks, collapse = ", "),
               " - these vary by occasion and have no person-level value")
      else "",
      ". Base = ", format(nrow(df), big.mark = ","), " respondents."
    )

    if(verbose){
      message("  columns removed: ", length(grid_cols),
              " (e.g. ", paste(utils::head(grid_cols, 4), collapse = ", "), ")")
    }

  } else {

    # Grid shell keeps everything, but reads better with the occasion-specific
    # blocks up front next to the polars rather than trailing after
    # demographics.
    if(length(gb) && any(gb)){
      seg_local[["shell"]][["profiles"]] <- rbind(prof_shell[gb, , drop = FALSE],
                                                  prof_shell[!gb, , drop = FALSE])
      gs <- is_grid_block(prof_spec)
      if(length(gs) && any(gs)){
        seg_local[["spec"]][["profiles"]] <- rbind(prof_spec[gs, , drop = FALSE],
                                                    prof_spec[!gs, , drop = FALSE])
      }
    }

    message(
      "Grid shell: ", n_polar + n_prof, " blocks",
      if(length(grid_blocks))
        paste0(", with ", paste(grid_blocks, collapse = ", "), " ordered first")
      else "",
      ". Base = ", format(nrow(df), big.mark = ","), " occasion-grids, not people."
    )
  }

  # Guard: the frame, the spec and the shell must name the same variables.
  # seg_write_shell() selects with all_of(), which is strict by design, but
  # fails ~30 frames deep naming variables rather than the block they came from.
  gaps <- unlist(lapply(
    list(seg_local[["shell"]][["polars"]], seg_local[["shell"]][["profiles"]]),
    function(tbl){
      if(is.null(tbl) || !"vars" %in% names(tbl)) return(NULL)
      Map(function(v, pref){
        if(is.null(v) || !"var" %in% names(v)) return(NULL)
        m <- setdiff(stats::na.omit(v[["var"]]), names(df))
        if(length(m)) stats::setNames(list(m), pref) else NULL
      }, tbl[["vars"]], tbl[["prefix"]])
    }
  ), recursive = FALSE)
  gaps <- gaps[!vapply(gaps, is.null, logical(1))]

  if(length(gaps) > 0){
    stop(
      sum(lengths(gaps)), " variable(s) in ", length(gaps), " block(s) are named by ",
      "the shell but absent from the ", level, " frame:\n",
      paste0("  ", names(gaps), ": ",
             vapply(gaps, function(v)
               paste0(paste(utils::head(v, 3), collapse = ", "),
                      if(length(v) > 3) paste0(" ... (", length(v), " total)") else ""),
               character(1)),
             collapse = "\n"),
      "\nThe frame, the spec and the shell are out of step.",
      call. = FALSE
    )
  }

  seg_local[["data"]][["with_shell"]] <- df
  seg_local[["meta"]][["shell_level"]] <- level

  invisible(
    seg_write_shell(seg_local, solution_var = solution_var,
                    file_label = if(level == "grid") "Grid" else "Person", ...)
  )
}
