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

  # Subset the SPEC-EXECUTED frame, not the raw stacked one - with_shell is what
  # carries the spec variables the shell is built from.
  df <- seg[["data"]][["with_shell"]]

  if(!is.data.frame(df)){
    stop("No executed spec found. Run seg_get_spec() before seg_needs_write_shell().",
         call. = FALSE)
  }

  seg_local <- seg

  if(level == "person"){

    if(!"person_id" %in% names(df)){
      stop("person_id is not on the executed frame - was the stacked file loaded?",
           call. = FALSE)
    }

    # Collapsing to one row per person is only valid for columns that are
    # CONSTANT within a person. A grid-level column varies down a respondent's
    # rows, so collapsing would report whichever context sorted first as though
    # it were a person attribute. Detect them rather than keep a list.
    varying <- vapply(
      setdiff(names(df), "person_id"),
      function(v) any(tapply(df[[v]], df$person_id,
                             function(x) dplyr::n_distinct(x) > 1)),
      logical(1)
    )

    dropped <- names(varying)[varying]

    if(length(dropped) > 0){
      message("Dropping ", length(dropped), " grid-level column(s) with no ",
              "person-level meaning (e.g. ",
              paste(utils::head(dropped, 4), collapse = ", "), ")")
      df <- df[setdiff(names(df), dropped)]
    }

    df <- dplyr::distinct(df, .data$person_id, .keep_all = TRUE)

    # seg_write_shell() builds its tables from seg$shell, which seg_do_spec()
    # derives from the spec - so both have to be trimmed to what survives on the
    # person frame, or it asks for columns that were deliberately removed.
    # Blocks left with nothing are dropped whole.
    trim <- function(tbl, what){

      if(is.null(tbl) || !"vars" %in% names(tbl)) return(tbl)

      tbl[["vars"]] <- lapply(tbl[["vars"]], function(v){
        if(is.null(v) || !"var" %in% names(v)) return(v)
        v[is.na(v[["var"]]) | v[["var"]] %in% names(df), , drop = FALSE]
      })

      keep <- vapply(tbl[["vars"]], function(v) nrow(v) > 0, logical(1))

      if(any(!keep)){
        message("Dropping ", sum(!keep), " grid-level block(s) from the person ",
                what, ": ", paste(tbl[["prefix"]][!keep], collapse = ", "))
      }

      tbl[keep, , drop = FALSE]
    }

    seg_local[["spec"]][["profiles"]]  <- trim(seg[["spec"]][["profiles"]],  "spec")
    seg_local[["shell"]][["profiles"]] <- trim(seg[["shell"]][["profiles"]], "shell")
  }

  # ---- guard: frame, spec and shell must name the same variables -----------
  # seg_write_shell() takes its variable list from seg$shell and selects it with
  # all_of(), which is strict by design - a shell silently losing rows because a
  # column vanished would be worse than a crash. But it fails ~30 frames deep,
  # naming the variables and not the block they came from, and there are three
  # copies of that list (the frame, seg$spec, seg$shell) so knowing NS01 is
  # absent does not say which pair is out of step. Check it here instead.
  missing_from_frame <- function(tbl){
    if(is.null(tbl) || !"vars" %in% names(tbl)) return(NULL)
    Map(function(v, pref){
      if(is.null(v) || !"var" %in% names(v)) return(NULL)
      m <- stats::na.omit(v[["var"]])
      m <- setdiff(m, names(df))
      if(length(m)) stats::setNames(list(m), pref) else NULL
    }, tbl[["vars"]], tbl[["prefix"]]) |> unlist(recursive = FALSE)
  }

  gaps <- c(
    missing_from_frame(seg_local[["shell"]][["polars"]]),
    missing_from_frame(seg_local[["shell"]][["profiles"]])
  )

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
      "\nThe frame, the spec and the shell are out of step. At ", level,
      " level the grid-level blocks should be dropped from all three.",
      call. = FALSE
    )
  }

  base_label <- if(level == "grid") "grids" else "respondents"

  message("Writing ", level, "-level shell cut by ", solution_var,
          " (base = ", nrow(df), " ", base_label, ").")

  seg_local[["data"]][["with_shell"]] <- df
  seg_local[["meta"]][["shell_level"]] <- level

  invisible(seg_write_shell(seg_local, solution_var = solution_var, ...))
}
