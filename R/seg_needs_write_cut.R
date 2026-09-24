#' seg_needs_write_cut
#'
#' @description Writes the needs solution as a deliverable at both units:
#'
#'   \describe{
#'     \item{person}{the person-level shell with one column per cell of
#'       [seg_needs_cut()] - base respondents, grid-level blocks left off}
#'     \item{grid}{the grid-level shell with one column per theme (or need
#'       state) - base occasion-grids, the occasion blocks first so each theme
#'       reads with the occasions it happens in}
#'   }
#'
#'   Each workbook opens on a key sheet. The shell numbers segments 1..N; the
#'   key is what turns "Seg 7" into "Escape + Smart Utility", so it is not
#'   optional. It also carries the caveats a reader would not otherwise see:
#'   breadth is censored (a "single" respondent is single on the contexts
#'   observed), how near-ties and flat grids were treated, how many could not
#'   be classified, and - on the grid shell - that the base is occasions, not
#'   people.
#'
#'   To write every solution side by side - the themes and each cluster
#'   solution - use [seg_needs_write_solutions()], which calls this for each.
#'
#' @param seg A seg object after [seg_needs_cut()] and [seg_get_spec()].
#' @param file_label Character. Stamped into the file names, followed by
#'   "Person" / "Grid" (default `"Needs Cut"`).
#' @param level Character. `"both"` (default), `"person"` or `"grid"`.
#' @param min_n Integer. Person cells smaller than this are warned about - they
#'   are still written (default `30`).
#' @param ... Passed through to [seg_write_shell()] (e.g. `where`, `truncate`).
#'
#' @return The paths of the written workbooks, invisibly, named `person` /
#'   `grid`.
#'
#' @export
seg_needs_write_cut <- function(seg, file_label = "Needs Cut", level = c("both", "person", "grid"),
                                min_n = 30, ...){

  level <- match.arg(level)

  cut <- seg[["needs"]][["cut"]]
  if(!is.list(cut) || !is.data.frame(cut[["persons"]])){
    stop("No cut found. Run seg_needs_cut() first.", call. = FALSE)
  }
  df <- seg[["data"]][["with_shell"]]
  if(!is.data.frame(df)){
    stop("No executed spec found. Run seg_get_spec() first.", call. = FALSE)
  }

  typing    <- seg[["needs"]][["typing"]]
  is_states <- identical(typing[["source"]], "states")
  unit      <- if(is_states) "state" else "theme"
  Unit      <- if(is_states) "Need state" else "Theme"

  tie_note <- paste0(
    "Near-ties (",
    if(is_states) paste0("within ", sprintf("%.1f", 100 * typing[["tie_margin"]]),
                         "% of equidistant between two states")
    else paste0("top two themes within ", sprintf("%.2f", typing[["tie_margin"]])),
    if(!is.null(typing[["tie_rule"]])) paste0(", ", typing[["tie_rule"]]) else "",
    "): "
  )

  paths <- c()


  # ---- person: the cut ----------------------------------------------------------
  if(level %in% c("both", "person")){

    cells   <- cut[["cells"]]
    persons <- cut[["persons"]]

    # seg_write_shell() numbers segments seq(max()), so an empty cell in the
    # middle would still get a column - of nothing. Renumber to the cells that
    # are populated, keeping their order, and let the key carry the mapping.
    kept <- cells[cells$n > 0, ]
    kept$segment <- seq_len(nrow(kept))
    persons$needs_cut <- kept$segment[match(persons$cell, kept$code)]

    seg_p <- seg
    df_p  <- df
    df_p[["needs_cut"]] <- NULL
    seg_p[["data"]][["with_shell"]] <- dplyr::left_join(df_p, persons[c("person_id", "needs_cut")],
                                                        by = "person_id")

    small <- kept$label[kept$n < min_n]
    if(length(small)){
      warning(length(small), " cell(s) under ", min_n, " respondents are in the shell: ",
              paste(small, collapse = ", "), ". Read them as directional.", call. = FALSE)
    }

    label_p <- paste(file_label, "Person")
    seg_needs_write_shell(seg_p, solution_var = "needs_cut", level = "person",
                          file_label = label_p, ...)
    path_p <- .needs_shell_path(seg, label_p, "needs_cut", list(...))

    n_class <- sum(!is.na(persons$needs_cut))
    n_uncl  <- sum(is.na(persons$needs_cut))

    key <- data.frame(
      Segment        = paste("Seg", kept$segment),
      Cell           = kept$label,
      Type           = kept$type,
      Respondents    = kept$n,
      `% classified` = round(100 * kept$n / n_class, 1),
      check.names = FALSE
    )
    defs <- data.frame(typing[["theme_names"]], typing[["describe"]])
    names(defs) <- if(is_states) c("Need state", "Over-indexes on") else c("Theme", "Needs")

    notes <- c(
      paste0("Base: ", format(n_class, big.mark = ","), " respondents classified. ",
             format(n_uncl, big.mark = ","), " not classified - fewer than 2 of their contexts could be typed."),
      if(is_states)
        paste0("Each context a respondent rated is assigned to the nearest of ", typing[["k"]],
               " need states, clustered on the shape of the need profile. A respondent's cell is the set of states across their typed contexts.")
      else
        "Each context a respondent rated is typed to the theme its needs lean toward most. A respondent's cell is the set of themes across their typed contexts.",
      paste0("Breadth is censored: respondents rated a few of the contexts they do, so 'only' means one ", unit,
             " across the contexts observed - they show at least that many ", unit, "s, not exactly that many."),
      paste0(tie_note,
             if(isTRUE(cut[["decisive_only"]])) "left out of the cut - only clearly typed contexts count." else
               switch(typing[["ties"]],
                      flag    = paste0("assigned to the leading ", unit, "."),
                      exclude = "left untyped.",
                      first   = paste0("assigned to the leading ", unit, "."))),
      paste0("Flat contexts (every need rated the same): ",
             if(identical(typing[["flat"]], "type")) "typed like any other." else
               paste0("not typed - they carry no lean toward any ", unit, "."),
             if(any(cells$type == "flat"))
               paste0(" Respondents flat in every context form the ",
                      paste(cells$label[cells$type == "flat"], collapse = " and "),
                      " cells, split by the level they rated at - a rating level rather than a need.")
             else "")
    )

    .needs_write_key(path_p, "Cut Key", "Needs cut - segment key", list(key, defs), notes,
                     widths = c(12, 55, 10, 13, 13))
    paths["person"] <- path_p
  }


  # ---- grid: the themes / states ------------------------------------------------
  if(level %in% c("both", "grid")){

    if(!"need_theme" %in% names(df)){
      stop("The grid typing is not on the executed frame - set the unit to grid with ",
           "seg_needs_set_unit('grid') before seg_get_spec(), then re-type.", call. = FALSE)
    }

    g      <- typing[["grids"]]
    K      <- length(typing[["theme_names"]])
    typed  <- !is.na(g$theme)
    n_grid <- tabulate(g$theme[typed], nbins = K)

    label_g <- paste(file_label, "Grid")
    seg_needs_write_shell(seg, solution_var = "need_theme", level = "grid", file_label = label_g, ...)
    path_g <- .needs_shell_path(seg, label_g, "need_theme", list(...))

    key <- data.frame(
      Segment       = paste("Seg", seq_len(K)),
      x             = typing[["theme_names"]],
      Grids         = n_grid,
      `% of typed`  = round(100 * n_grid / sum(n_grid), 1),
      `% near-tie`  = round(100 * vapply(seq_len(K), function(k) mean(g$near_tie[typed & g$theme == k]),
                                         numeric(1)), 1),
      y             = typing[["describe"]],
      check.names = FALSE
    )
    names(key)[c(2, 6)] <- c(Unit, if(is_states) "Over-indexes on" else "Needs")

    notes <- c(
      paste0("Base: ", format(sum(typed), big.mark = ","), " occasion-grids, not people - a respondent ",
             "who rated three contexts is in the base up to three times."),
      if(is_states)
        paste0("Each grid is assigned to the nearest of ", K, " need states, clustered on the shape of its need profile.")
      else
        "Each grid is typed to the theme its needs lean toward most, on needs standardised across all grids.",
      paste0(tie_note, "included, typed to their leading ", unit, "."),
      paste0("Flat grids (every need rated the same): ", sum(g$flat), " left out - they carry no lean toward any ",
             unit, ".")
    )

    .needs_write_key(path_g, "Grid Key", paste0("Needs by grid - ", unit, " key"), list(key), notes,
                     widths = c(10, 30, 10, 12, 12, 90))
    paths["grid"] <- path_g
  }

  message("Needs solution written (", paste(names(paths), collapse = " + "), "). Keys on the first sheet.\n  ",
          paste(paths, collapse = "\n  "))

  invisible(paths)
}


# Rebuild the path seg_write_shell() writes to; it does not return it.
.needs_shell_path <- function(seg, file_label, solution_var, dots){
  where <- dots[["where"]]
  if(is.null(where) || is.na(where)) where <- seg[["paths"]][["folders"]][["solution"]]
  if(is.null(where) || is.na(where)) where <- getwd()
  path <- file.path(where, paste0("Solution ", file_label, " - ", solution_var,
                                  if(isTRUE(dots[["truncate"]])) " (Truncate)" else "", ".xlsx"))
  if(!file.exists(path)){
    stop("Shell was written but not found at ", path, " - cannot add the key.", call. = FALSE)
  }
  path
}


# Put a key sheet first in a written shell: a title, one or more tables, notes.
# The shell's segment numbers mean nothing without it.
.needs_write_key <- function(path, sheet, title, tables, notes, widths){

  wb <- openxlsx::loadWorkbook(path)
  if(sheet %in% names(wb)) openxlsx::removeWorksheet(wb, sheet)
  openxlsx::addWorksheet(wb, sheet)

  bold <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::writeData(wb, sheet, title, startRow = 1)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(textDecoration = "bold", fontSize = 13), rows = 1, cols = 1)

  r <- 3
  for(tbl in tables){
    openxlsx::writeData(wb, sheet, tbl, startRow = r, headerStyle = bold)
    r <- r + nrow(tbl) + 2
  }
  openxlsx::writeData(wb, sheet, "Notes", startRow = r)
  openxlsx::addStyle(wb, sheet, bold, rows = r, cols = 1)
  openxlsx::writeData(wb, sheet, notes, startRow = r + 1)
  openxlsx::setColWidths(wb, sheet, cols = seq_along(widths), widths = widths)

  n_sheets <- length(names(wb))
  openxlsx::worksheetOrder(wb) <- c(n_sheets, seq_len(n_sheets - 1))
  openxlsx::activeSheet(wb) <- 1
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  invisible(path)
}
