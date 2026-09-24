#' seg_needs_write_cut
#'
#' @description Writes the needs cut as a deliverable: the person-level shell
#'   with one column per cell of [seg_needs_cut()], plus a `Cut Key` sheet up
#'   front saying what each numbered segment is and how the cut was built.
#'
#'   The shell itself is [seg_needs_write_shell()] at `level = "person"`, so the
#'   base is respondents and every grid-level block is left off. The shell
#'   numbers segments 1..N; the key is what turns "Seg 7" into
#'   "Escape + Smart Utility", so it is not optional.
#'
#'   The key also carries the caveats a reader of this cut needs and would not
#'   otherwise see: that breadth is censored (a "single" respondent is single on
#'   the contexts observed), how near-ties and flat grids were treated, and how
#'   many respondents could not be classified.
#'
#' @param seg A seg object after [seg_needs_cut()] and [seg_get_spec()].
#' @param file_label Character. Stamped into the file name
#'   (default `"Needs Cut"`).
#' @param min_n Integer. Cells smaller than this are warned about - they will
#'   still be written (default `30`).
#' @param ... Passed through to [seg_write_shell()] (e.g. `where`, `truncate`).
#'
#' @return The path of the written workbook, invisibly.
#'
#' @export
seg_needs_write_cut <- function(seg, file_label = "Needs Cut", min_n = 30, ...){

  cut <- seg[["needs"]][["cut"]]
  if(!is.list(cut) || !is.data.frame(cut[["persons"]])){
    stop("No cut found. Run seg_needs_cut() first.", call. = FALSE)
  }
  df <- seg[["data"]][["with_shell"]]
  if(!is.data.frame(df)){
    stop("No executed spec found. Run seg_get_spec() first.", call. = FALSE)
  }

  cells   <- cut[["cells"]]
  persons <- cut[["persons"]]
  typing  <- seg[["needs"]][["typing"]]

  # seg_write_shell() numbers segments seq(max()), so an empty cell in the
  # middle would still get a column - of nothing. Renumber to the cells that
  # are populated, keeping their order, and let the key carry the mapping.
  kept <- cells[cells$n > 0, ]
  kept$segment <- seq_len(nrow(kept))
  persons$needs_cut <- kept$segment[match(persons$cell, kept$code)]

  df[["needs_cut"]] <- NULL
  seg[["data"]][["with_shell"]] <- dplyr::left_join(df, persons[c("person_id", "needs_cut")],
                                                    by = "person_id")

  small <- kept$label[kept$n < min_n]
  if(length(small)){
    warning(length(small), " cell(s) under ", min_n, " respondents are in the shell: ",
            paste(small, collapse = ", "), ". Read them as directional.", call. = FALSE)
  }

  seg_needs_write_shell(seg, solution_var = "needs_cut", level = "person",
                        file_label = file_label, ...)


  # ---- the key ----------------------------------------------------------------
  # Rebuild the path the way seg_write_shell() does; it does not return it.
  dots  <- list(...)
  where <- dots[["where"]]
  if(is.null(where) || is.na(where)) where <- seg[["paths"]][["folders"]][["solution"]]
  if(is.null(where) || is.na(where)) where <- getwd()
  path <- file.path(where, paste0("Solution ", file_label, " - needs_cut",
                                  if(isTRUE(dots[["truncate"]])) " (Truncate)" else "", ".xlsx"))
  if(!file.exists(path)){
    stop("Shell was written but not found at ", path, " - cannot add the key.", call. = FALSE)
  }

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

  themes <- seg[["needs"]][["themes"]]
  theme_key <- data.frame(
    Theme = typing[["theme_names"]],
    Needs = vapply(seq_along(typing[["theme_names"]]), function(k){
      paste(themes$label[!is.na(themes$theme) & themes$theme == k], collapse = "; ")
    }, character(1))
  )

  notes <- c(
    paste0("Base: ", format(n_class, big.mark = ","), " respondents classified. ",
           format(n_uncl, big.mark = ","), " not classified - fewer than 2 of their contexts could be typed."),
    "Each context a respondent rated is typed to the theme its needs lean toward most. A respondent's cell is the set of themes across their typed contexts.",
    paste0("Breadth is censored: respondents rated a few of the contexts they do, so 'only' means one theme across the contexts observed ",
           "- they show at least that many themes, not exactly that many."),
    paste0("Near-ties (top two themes within ", typing[["tie_margin"]], "): ",
           if(isTRUE(cut[["decisive_only"]])) "left out of the cut - only clearly typed contexts count." else
             switch(typing[["ties"]],
                    flag    = "typed to the leading theme.",
                    exclude = "left untyped.",
                    first   = "typed to the leading theme.")),
    paste0("Flat contexts (every need rated the same): ",
           if(identical(typing[["flat"]], "type")) "typed like any other." else "not typed - they carry no lean toward any theme.")
  )

  wb <- openxlsx::loadWorkbook(path)
  sheet <- "Cut Key"
  if(sheet %in% names(wb)) openxlsx::removeWorksheet(wb, sheet)
  openxlsx::addWorksheet(wb, sheet)

  bold <- openxlsx::createStyle(textDecoration = "bold")
  r <- 1
  openxlsx::writeData(wb, sheet, "Needs cut - segment key", startRow = r)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(textDecoration = "bold", fontSize = 13), rows = r, cols = 1)
  r <- r + 2
  openxlsx::writeData(wb, sheet, key, startRow = r, headerStyle = bold)
  r <- r + nrow(key) + 2
  openxlsx::writeData(wb, sheet, theme_key, startRow = r, headerStyle = bold)
  r <- r + nrow(theme_key) + 2
  openxlsx::writeData(wb, sheet, "Notes", startRow = r)
  openxlsx::addStyle(wb, sheet, bold, rows = r, cols = 1)
  openxlsx::writeData(wb, sheet, notes, startRow = r + 1)
  openxlsx::setColWidths(wb, sheet, cols = 1:5, widths = c(12, 55, 10, 13, 13))

  # the key opens first - without it the shell's segment numbers mean nothing
  n_sheets <- length(names(wb))
  openxlsx::worksheetOrder(wb) <- c(n_sheets, seq_len(n_sheets - 1))
  openxlsx::activeSheet(wb) <- 1
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  message("Needs cut written: ", nrow(kept), " segments, ", format(n_class, big.mark = ","),
          " respondents. Key on the first sheet.\n  ", path)

  invisible(path)
}
