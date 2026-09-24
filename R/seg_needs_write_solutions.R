#' seg_needs_write_solutions
#'
#' @description Writes every candidate needs solution to the solutions folder
#'   so they can be reviewed side by side: the factor solution (themes, from
#'   [seg_needs_type()]) and each cluster solution (need states, from
#'   [seg_needs_cluster()]). For each it writes the person and grid shells with
#'   [seg_needs_write_cut()], and it writes one comparison workbook that puts
#'   the solutions head to head on the measures that decide between them.
#'
#'   Each solution is built on its own copy of the seg object, so the typing
#'   and cut on `seg` are left exactly as they were - reviewing the clusters
#'   does not switch the project onto them. Adopt the winner deliberately
#'   afterwards (`seg_needs_cluster(adopt = )`, or keep the themes).
#'
#'   Every solution is cut the same way (`cut_args`), and the cluster solutions
#'   flag the same share of close calls as the themes, so the numbers compare
#'   like with like.
#'
#' @param seg A seg object after [seg_needs_themes()] and [seg_get_spec()],
#'   with the grid as the unit.
#' @param k Integer vector of cluster solutions to include. Default: every k
#'   stored by the last [seg_needs_cluster()] run; none if it has not been
#'   run.
#' @param cut_args List of arguments for [seg_needs_cut()], applied to every
#'   solution (default: its defaults).
#' @param file_label Character. Prefix for the shell file names
#'   (default `"Needs"`), giving e.g. "Needs Themes Person", "Needs Clusters
#'   k4 Grid".
#' @param comparison_file Character. File name of the comparison workbook.
#' @param ... Passed through to [seg_write_shell()] (e.g. `where`).
#'
#' @return The seg object, invisibly, unchanged except for the comparison in
#'   `seg[["needs"]][["reports"]][["solutions"]]`. Prints the comparison.
#'
#' @export
seg_needs_write_solutions <- function(seg, k = NULL, cut_args = list(), file_label = "Needs",
                                      comparison_file = "Needs Solutions Comparison", ...){

  if(!is.data.frame(seg[["needs"]][["themes"]])){
    stop("No theme assignment. Run seg_needs_themes() first.", call. = FALSE)
  }

  quiet <- function(expr){
    out <- NULL
    utils::capture.output(out <- suppressMessages(expr))
    out
  }


  # ---- the factor solution, as the project has it typed ---------------------------
  typing <- seg[["needs"]][["typing"]]
  base <- if(is.list(typing) && identical(typing[["source"]], "themes")){
    seg
  } else {
    # re-type from the stored theme settings, so the comparison uses the
    # themes as they were set rather than the defaults
    prev <- seg[["needs"]][["typing_themes"]]
    args <- list(seg)
    if(is.list(prev)){
      args$ties <- prev[["ties"]]
      args$flat <- prev[["flat"]]
      if(!is.null(prev[["flat_sd"]])) args$flat_sd <- prev[["flat_sd"]]
      if(identical(prev[["tie_rule"]], "set")) args$tie_margin <- prev[["tie_margin"]]
    }
    quiet(do.call(seg_needs_type, args))
  }

  solutions <- list("Themes" = base)

  stored <- seg[["needs"]][["states"]]
  if(is.null(k)) k <- if(is.list(stored)) stored[["k"]] else integer(0)
  if(length(k) == 0){
    message("No cluster solutions - run seg_needs_cluster(k = ) first to compare them. Writing the themes only.")
  }
  for(kk in k){
    solutions[[paste0("Clusters k", kk)]] <- quiet(seg_needs_cluster(base, adopt = kk))
  }


  # ---- build, measure and write each ----------------------------------------------
  theme_top <- base[["needs"]][["typing"]][["grids"]]$top
  where_dots <- list(...)

  measures <- lapply(names(solutions), function(nm){

    s  <- quiet(do.call(seg_needs_cut, c(list(solutions[[nm]]), cut_args)))
    s  <- quiet(seg_needs_state_lock(s))
    ty <- s[["needs"]][["typing"]]
    g  <- ty[["grids"]]
    ct <- s[["needs"]][["cut"]]
    lk <- s[["needs"]][["reports"]][["state_lock"]]

    fragile <- NA_real_
    if("varied" %in% ct[["cells"]]$type && any(ct[["cells"]]$n[ct[["cells"]]$type == "varied"] > 0)){
      tv <- quiet(seg_needs_test_varied(s))
      fragile <- mean(tv[["needs"]][["reports"]][["test_varied"]][["fragile"]]$fragile)
    }

    paths <- quiet(seg_needs_write_cut(s, file_label = paste(file_label, nm), ...))

    typed  <- !is.na(g$theme)
    shares <- tabulate(g$theme[typed], nbins = length(ty[["theme_names"]])) / sum(typed)
    per    <- ct[["persons"]]
    cls    <- !is.na(per$cell)
    type_share <- function(t) mean(per$cell_type[cls] == t)
    needs_cells <- ct[["cells"]][ct[["cells"]]$type != "flat", ]

    accuracy <- if(identical(ty[["source"]], "states")){
      stored[["fits"]]$accuracy[match(ty[["k"]], stored[["fits"]]$n)]
    } else NA_real_

    list(
      metrics = c(
        groups              = length(ty[["theme_names"]]),
        smallest_group_pct  = 100 * min(shares),
        largest_group_pct   = 100 * max(shares),
        intensity_explained = 100 * ty[["eta2"]],
        near_tie_pct        = 100 * mean(g$near_tie[typed]),
        typing_accuracy_pct = 100 * accuracy,
        agree_with_themes   = needs_ari(g$top, theme_top),
        person_lock         = lk[["lock"]],
        occasion_link       = lk[["context_v"]],
        single_pct          = 100 * type_share("single"),
        pair_pct            = 100 * type_share("pair"),
        varied_pct          = 100 * type_share("varied"),
        varied_fragile_pct  = 100 * fragile,
        classified          = sum(cls),
        smallest_cell       = min(needs_cells$n[needs_cells$n > 0])
      ),
      groups = data.frame(solution = nm, group = ty[["theme_names"]],
                          pct_of_grids = round(100 * shares, 1), defined_by = ty[["describe"]]),
      paths = paths
    )
  })
  names(measures) <- names(solutions)


  # ---- the comparison ---------------------------------------------------------------
  what <- data.frame(
    measure = c("Groups", "Smallest group (% of grids)", "Largest group (% of grids)",
                "Rating level explaining the grouping (%)", "Close calls (% of grids)",
                "Raw answers recover the group (%)", "Agreement with the themes (ARI)",
                "Person lock", "Occasion link (Cramer's V)",
                "Single (% of classified)", "Pair (% of classified)", "Varied (% of classified)",
                "Varied that is fragile (%)", "Respondents classified", "Smallest needs cell (n)"),
    better = c("interpretability decides", "higher - no tiny groups", "lower - no dominant group",
               "lower - groups are needs, not generosity", "lower - fewer arbitrary calls",
               "higher - a usable typing tool (clusters only)", "1 = same as themes; low = a different read",
               "higher - groups are about people", "depends: low = a person story, high = an occasion story",
               "context", "context", "context", "lower - the varied cell is real",
               "higher", "higher - no directional-only cells"),
    stringsAsFactors = FALSE
  )
  vals <- sapply(measures, function(m) m$metrics)
  cmp  <- cbind(what, round(vals, 2))
  rownames(cmp) <- NULL
  groups <- dplyr::bind_rows(lapply(measures, `[[`, "groups"))

  where <- where_dots[["where"]]
  if(is.null(where) || is.na(where)) where <- seg[["paths"]][["folders"]][["solution"]]
  if(is.null(where) || is.na(where)) where <- getwd()
  cmp_path <- file.path(where, paste0(seg_glue_proj_name_to_file(seg, comparison_file), ".xlsx"))

  wb <- openxlsx::createWorkbook()
  bold <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::addWorksheet(wb, "Comparison")
  openxlsx::writeData(wb, "Comparison", "Needs solutions - factor (themes) vs cluster (need states)", startRow = 1)
  openxlsx::addStyle(wb, "Comparison", openxlsx::createStyle(textDecoration = "bold", fontSize = 13), rows = 1, cols = 1)
  openxlsx::writeData(wb, "Comparison", cmp, startRow = 3, headerStyle = bold)
  notes <- c(
    "Every solution is cut the same way and flags the same share of close calls, so the rows compare like with like.",
    "Themes are defined by fixed lists of needs from the factor analysis and reproduce exactly; clusters are defined by whole-profile centroids and depend on k.",
    "Person lock is how often two of a respondent's occasions share a group, beyond what their occasions alone would give (kappa).",
    "Each solution's shells are in this folder: '<file_label> <solution> Person' (the cut) and '... Grid' (by group)."
  )
  openxlsx::writeData(wb, "Comparison", "Notes", startRow = nrow(cmp) + 5)
  openxlsx::addStyle(wb, "Comparison", bold, rows = nrow(cmp) + 5, cols = 1)
  openxlsx::writeData(wb, "Comparison", notes, startRow = nrow(cmp) + 6)
  openxlsx::setColWidths(wb, "Comparison", cols = 1:(2 + ncol(vals)), widths = c(42, 48, rep(14, ncol(vals))))
  openxlsx::addWorksheet(wb, "Groups")
  openxlsx::writeData(wb, "Groups", groups, headerStyle = bold)
  openxlsx::setColWidths(wb, "Groups", cols = 1:4, widths = c(16, 40, 14, 100))
  openxlsx::saveWorkbook(wb, cmp_path, overwrite = TRUE)


  cat("\n=== Needs solutions: ", paste(names(solutions), collapse = " | "), " ===\n\n", sep = "")
  print(cmp[, c("measure", names(solutions))], row.names = FALSE)
  cat("\n  Written to ", where, ":\n", sep = "")
  cat("    ", basename(cmp_path), "\n", sep = "")
  for(nm in names(measures)) cat("    ", paste(basename(measures[[nm]]$paths), collapse = "\n    "), "\n", sep = "")
  cat("\n  Nothing on seg was switched - adopt the winner deliberately.\n\n")

  seg[["needs"]][["reports"]][["solutions"]] <- list(comparison = cmp, groups = groups, path = cmp_path)

  invisible(seg)
}
