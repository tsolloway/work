#' seg_needs_set_unit
#'
#' @description Sets which frame is the unit of analysis, i.e. which one the
#'   spec executes against.
#'
#'   A needs project carries two frames: the person frame loaded by
#'   [seg_get_data()], and the person-context grid built by
#'   [seg_needs_stack()]. Everything downstream of the spec - [seg_do_spec()],
#'   [seg_write_shell()], the cluster functions - reads `seg$data$original`, so
#'   whichever frame should be the unit has to be promoted into that slot.
#'
#'   Call it after [seg_needs_prepare()], since prepare adds the basis columns
#'   to the stacked frame.
#'
#'   Promotion is reversible: the person frame is kept at `seg$data$person`, so
#'   `level = "person"` puts it back exactly as [seg_get_data()] loaded it -
#'   raw, without the basis columns [seg_needs_prepare()] wrote onto the
#'   stacked frame. That path is for re-stacking, not for person-level
#'   analysis. For a person-level READ of an executed spec use
#'   [seg_needs_write_shell()] with `level = "person"`, which subsets the
#'   executed frame and so keeps every derived column. A fresh `seg_uuid` is issued each time
#'   because the row means something different - one per grid at grid level,
#'   one per respondent at person level. `person_id` is untouched either way,
#'   so a grid-level solution can always be rolled back up.
#'
#' @param seg A seg object after [seg_needs_prepare()].
#' @param level Character. `"grid"` (default) makes the person-context grid the
#'   unit; `"person"` restores the raw respondent frame.
#'
#' @return The seg object with `seg$data$original` set to the chosen frame.
#'
#' @export
seg_needs_set_unit <- function(seg, level = c("grid", "person")){

  level <- match.arg(level)

  if(!inherits(seg, "analytic_needs")){
    stop("Run seg_needs_init() first.", call. = FALSE)
  }

  id_name <- seg[["meta"]][["id_variable"]]
  if(is.null(id_name) || is.na(id_name)) id_name <- "seg_uuid"

  # keep the person frame the first time we move off it
  if(is.null(seg[["data"]][["person"]]) || !is.data.frame(seg[["data"]][["person"]])){
    seg[["data"]][["person"]] <- seg[["data"]][["original"]]
  }

  if(level == "grid"){

    stacked <- seg[["data"]][["stacked"]]
    if(!is.data.frame(stacked)){
      stop("No stacked frame. Run seg_needs_stack() first.", call. = FALSE)
    }
    df <- stacked

  } else {

    df <- seg[["data"]][["person"]]
    if(!is.data.frame(df)){
      stop("No person frame stored.", call. = FALSE)
    }
  }

  # drop any previous id so add_uuid does not collide with itself
  df[[id_name]] <- NULL
  df <- df %>% add_uuid(id_name)

  seg[["data"]][["original"]]  <- df
  seg[["meta"]][["unit"]]      <- level

  message(
    "Unit of analysis: ", level, " (", nrow(df), " rows). ",
    "The spec will execute against this frame."
  )

  seg
}
