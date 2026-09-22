#' seg_needs_stack_report
#'
#' @description The gate on a stacked loop. Reconciles what came out of
#'   [seg_needs_stack()] against what went in, reports fill by context against
#'   the eligibility pool, and quantifies how much of each respondent's
#'   repertoire was actually observed.
#'
#'   The load-bearing check is the shape of the fill. A loop keyed by context
#'   IDENTITY produces an uneven fill that tracks eligibility. A loop keyed by
#'   POSITION - where slot 1 holds whatever the respondent happened to see
#'   first - produces a suspiciously flat fill regardless of eligibility, and
#'   stacks into a clean-looking file of nonsense. This function tests for that
#'   rather than relying on someone noticing.
#'
#' @param seg A seg object after [seg_needs_stack()].
#' @param eligibility_stem Character. Stem of the multi-punch question naming
#'   which contexts a respondent was eligible for, e.g. `"Occasions"` for
#'   columns `Occasionsr1` ... `Occasionsr13`. Optional; without it the
#'   eligibility and censoring sections are skipped.
#' @param flat_fill_cv Numeric. Coefficient of variation of context fill below
#'   which the fill is flagged as suspiciously flat (default `0.15`).
#'
#' @return The seg object, invisibly, with the report stored in
#'   `seg[["needs"]][["reports"]][["stack"]]`. Prints a verdict.
#'
#' @export
seg_needs_stack_report <- function(seg, eligibility_stem = NULL, flat_fill_cv = 0.15){

  stacked <- seg[["data"]][["stacked"]]
  person  <- seg[["data"]][["original"]]
  loop    <- seg[["needs"]][["loop"]]
  items   <- seg[["needs"]][["vars"]][["raw"]]

  if(!is.data.frame(stacked)){
    stop("No stacked frame. Run seg_needs_stack() first.", call. = FALSE)
  }

  ok <- TRUE
  say <- function(pass, msg){
    cat(if(pass) "  OK   " else "  FAIL ", msg, "\n", sep = "")
    if(!pass) ok <<- FALSE
  }

  cat("\n=== 1. Reconciliation ===\n")

  rows_per <- table(stacked$person_id)
  n_person <- dplyr::n_distinct(stacked$person_id)

  cat("  grids: ", nrow(stacked), " | respondents: ", n_person,
      " of ", nrow(person), " on the person frame\n", sep = "")
  cat("  grids per respondent: ",
      paste(names(table(rows_per)), "->", table(rows_per), collapse = "; "), "\n", sep = "")

  say(!anyNA(stacked[items]), "no missing items on any emitted grid")
  say(n_person <= nrow(person), "no respondent gained rows from nowhere")
  say(max(rows_per) <= loop[["n_contexts"]], "no respondent exceeds the context count")


  cat("\n=== 2. Fill by context ===\n")

  fill <- stacked %>%
    dplyr::count(.data$context, .data$context_label, name = "assigned")

  if(!is.null(eligibility_stem)){

    elig_cols <- paste0(eligibility_stem, "r", seq_len(loop[["n_contexts"]]))
    have <- intersect(elig_cols, names(person))

    if(length(have) != length(elig_cols)){
      warning("eligibility_stem columns not all found - skipping eligibility.", call. = FALSE)
    } else {
      elig <- vapply(elig_cols, function(v) sum(person[[v]] %in% 1), numeric(1))
      fill$eligible <- elig[fill$context]
      fill$pct_of_eligible <- round(100 * fill$assigned / pmax(fill$eligible, 1), 1)
    }
  }

  print(as.data.frame(fill), row.names = FALSE)

  cv <- stats::sd(fill$assigned) / mean(fill$assigned)
  cat("\n  fill CV: ", round(cv, 3), "\n", sep = "")
  say(
    cv >= flat_fill_cv,
    paste0(
      "fill varies by context (flat fill would suggest the loop is keyed by ",
      "POSITION, not context identity)"
    )
  )


  cat("\n=== 3. Censoring ===\n")

  if(!is.null(eligibility_stem) && "eligible" %in% names(fill)){

    punched <- rowSums(
      sapply(paste0(eligibility_stem, "r", seq_len(loop[["n_contexts"]])),
             function(v) person[[v]] %in% 1)
    )
    asked <- as.numeric(rows_per[as.character(person[[loop[["id_var"]]]])])
    asked[is.na(asked)] <- 0

    cov_pct <- round(100 * mean(asked / pmax(punched, 1)), 1)

    cat("  contexts punched per respondent (median): ", stats::median(punched), "\n", sep = "")
    cat("  contexts asked per respondent   (median): ", stats::median(asked[asked > 0]), "\n", sep = "")
    cat("  repertoire observed: ", cov_pct, "%\n", sep = "")
    cat("\n  -> breadth is censored. Report it as 'at least N themes', never 'exactly N'.\n")

    short <- names(rows_per)[rows_per < max(rows_per)]
    if(length(short) > 0){
      idx <- match(short, as.character(person[[loop[["id_var"]]]]))
      exhausted <- sum(punched[idx] <= rows_per[short], na.rm = TRUE)
      cat("\n  short rows: ", length(short), " respondent(s); ", exhausted,
          " explained by an exhausted eligibility pool", sep = "")
      cat(if(exhausted == length(short)) " (benign)\n" else " - the rest are DATA LOSS\n")
      say(exhausted == length(short), "every short row is explained by pool exhaustion")
    }

  } else {
    cat("  (supply eligibility_stem to quantify censoring)\n")
  }


  cat("\n=== Verdict: ", if(ok) "PASS" else "FAIL - do not build on this frame", " ===\n\n", sep = "")

  seg[["needs"]][["reports"]][["stack"]] <- list(fill = fill, rows_per = rows_per, pass = ok)

  invisible(seg)
}
