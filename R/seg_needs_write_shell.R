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
#' @param ... Passed through to [seg_write_shell()].
#'
#' @return Whatever [seg_write_shell()] returns, invisibly.
#'
#' @export
seg_needs_write_shell <- function(seg, solution_var, level = c("grid", "person"), ...){

  level <- match.arg(level)

  # Subset the SPEC-EXECUTED frame, not the raw stacked one - with_shell is
  # what carries the spec variables the shell is built from. Falling back to
  # the stacked frame would silently drop every spec column.
  df <- seg[["data"]][["with_shell"]]

  if(!is.data.frame(df)){
    stop(
      "No executed spec found. Run seg_get_spec() before seg_needs_write_shell().",
      call. = FALSE
    )
  }

  if(level == "person"){
    if(!"person_id" %in% names(df)){
      stop("person_id is not on the executed frame - was the stacked file loaded?",
           call. = FALSE)
    }
    df <- dplyr::distinct(df, .data$person_id, .keep_all = TRUE)
  }

  if(!solution_var %in% names(df)){
    stop("solution_var '", solution_var, "' is not on the ", level, " frame.", call. = FALSE)
  }

  base_label <- if(level == "grid") "grids" else "respondents"

  message(
    "Writing ", level, "-level shell cut by ", solution_var,
    " (base = ", nrow(df), " ", base_label, ")."
  )

  # seg_write_shell reads seg$data$with_shell; point it at the chosen frame and
  # keep the original so the object is unchanged on return
  seg_local <- seg
  seg_local[["data"]][["with_shell"]] <- df
  seg_local[["meta"]][["shell_level"]] <- level

  invisible(
    seg_write_shell(seg_local, solution_var = solution_var, ...)
  )
}
