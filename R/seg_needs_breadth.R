#' seg_needs_breadth
#'
#' @description How many distinct themes each respondent shows across the
#'   contexts they were asked about, against what chance would give them.
#'
#'   The baseline is a reshuffle: theme labels are permuted across grids,
#'   keeping every respondent's grid count, and breadth is recounted. Two
#'   versions are reported. `global` shuffles across all grids and asks whether
#'   people are more consistent than a random draw. `context` shuffles only
#'   within a context, so each respondent keeps the mix of contexts they had,
#'   and asks whether people are more consistent than their contexts alone
#'   would make them - the tougher test, and the one that says the typology is
#'   about people.
#'
#'   \strong{Breadth is censored.} A respondent is asked about a few of the
#'   contexts they do - on Kadro 3 of a median 6, about half their repertoire.
#'   Someone typed to one theme is single-theme on the contexts observed, so
#'   report "at least N themes", never "exactly N". Supply `eligibility_stem`
#'   to have the censoring quantified.
#'
#'   \strong{Near-ties.} With `decisive_only = TRUE` only decisively typed
#'   grids count. That drops grids, so fewer respondents have enough grids to
#'   show breadth, and those who remain are observed on fewer contexts - which
#'   by itself pushes toward single-theme. The reshuffle baseline is rebuilt on
#'   the same grids, so the comparison with chance stays fair; the raw
#'   single-theme share does not. Both readings are printed either way.
#'
#' @param seg A seg object after [seg_needs_type()].
#' @param decisive_only Logical. Count only grids that are not near-ties
#'   (default `FALSE`).
#' @param eligibility_stem Character. Stem of the multi-punch question naming
#'   which contexts a respondent does, e.g. `"Occasions"` for `Occasionsr1` ...
#'   Optional; quantifies the censoring.
#' @param n_perm Integer. Reshuffles for the chance baseline (default `200`).
#' @param seed Integer. Seed for the reshuffle (default `1`).
#'
#' @return The seg object, invisibly, with the tables in
#'   `seg[["needs"]][["reports"]][["breadth"]]`. Prints a verdict.
#'
#' @export
seg_needs_breadth <- function(seg,
                              decisive_only    = FALSE,
                              eligibility_stem = NULL,
                              n_perm           = 200,
                              seed             = 1){

  run <- function(decisive){

    g <- needs_typed_grids(seg, decisive_only = decisive)

    # breadth needs at least two typed contexts to be anything but 1
    n_typed <- table(g$person_id)
    keep_ids <- names(n_typed)[n_typed >= 2]
    g <- g[as.character(g$person_id) %in% keep_ids, , drop = FALSE]

    pid <- match(g$person_id, keep_ids)
    P   <- length(keep_ids)
    obs <- needs_n_distinct(pid, g$theme, P)

    max_d <- max(table(pid))
    levels_d <- seq_len(max_d)

    dist <- function(d) tabulate(d, nbins = max_d) / P

    set.seed(seed)
    perm_global  <- matrix(0, n_perm, max_d)
    perm_context <- matrix(0, n_perm, max_d)
    ctx_rows <- split(seq_len(nrow(g)), g$context)

    for(b in seq_len(n_perm)){
      perm_global[b, ] <- dist(needs_n_distinct(pid, sample(g$theme), P))
      th <- g$theme
      for(r in ctx_rows) th[r] <- th[r][sample.int(length(r))]
      perm_context[b, ] <- dist(needs_n_distinct(pid, th, P))
    }

    list(
      n_person = P,
      n_grids  = nrow(g),
      # against everyone stacked, so a respondent with no typed grid at all -
      # flat in every context - is counted as missing rather than vanishing
      n_short  = dplyr::n_distinct(seg[["data"]][["stacked"]]$person_id) - P,
      table = data.frame(
        themes         = levels_d,
        observed       = round(100 * dist(obs), 1),
        chance_global  = round(100 * colMeans(perm_global), 1),
        chance_context = round(100 * colMeans(perm_context), 1)
      ),
      # how often chance reaches the observed single-theme share - the p-value
      # of the lock, from the permutations themselves
      p_single_global  = mean(perm_global[, 1]  >= dist(obs)[1]),
      p_single_context = mean(perm_context[, 1] >= dist(obs)[1]),
      per_person = dplyr::tibble(person_id = keep_ids, n_typed = as.integer(n_typed[keep_ids]),
                                 n_themes = obs)
    )
  }

  typing <- seg[["needs"]][["typing"]]
  if(!is.list(typing) || !is.data.frame(typing[["grids"]])){
    stop("No typing found. Run seg_needs_type() first.", call. = FALSE)
  }
  can_split <- !identical(typing[["ties"]], "first")

  main  <- run(decisive_only)
  other <- if(can_split) run(!decisive_only) else NULL


  # ---- report -------------------------------------------------------------
  basis <- if(decisive_only) "decisively typed grids only" else "all typed grids"
  cat("\n=== Needs breadth: ", format(main$n_person, big.mark = ","), " respondents with 2+ typed contexts (",
      basis, ") ===\n\n", sep = "")
  tbl <- main$table
  tbl$themes <- paste(tbl$themes, ifelse(tbl$themes == 1, "theme", "themes"))
  print(tbl, row.names = FALSE)

  if(main$n_short > 0){
    cat("\n  ", main$n_short, " respondent(s) have fewer than 2 typed contexts and are not counted",
        if(decisive_only) " - lost to near-ties and flat grids" else " - lost to flat or untyped grids",
        ".\n", sep = "")
  }

  s_obs <- main$table$observed[1]
  s_glo <- main$table$chance_global[1]
  s_ctx <- main$table$chance_context[1]

  cat("\n  single-theme: ", s_obs, "% vs ", s_glo, "% by chance (", s_ctx,
      "% keeping each respondent's contexts)\n", sep = "")

  if(!is.null(other)){
    cat("  on ", if(decisive_only) "all typed grids" else "decisive grids only", ": ",
        other$table$observed[1], "% single-theme vs ", other$table$chance_global[1],
        "% by chance, ", format(other$n_person, big.mark = ","), " respondents\n", sep = "")
  }


  # ---- censoring ------------------------------------------------------------
  censoring <- NULL
  if(!is.null(eligibility_stem)){

    # the raw person frame when set_unit() has stored it; otherwise the stacked
    # frame carries the same person columns on every row
    person <- seg[["data"]][["person"]]
    id     <- seg[["needs"]][["loop"]][["id_var"]]
    if(!is.data.frame(person)){
      person <- dplyr::distinct(seg[["data"]][["stacked"]], .data$person_id, .keep_all = TRUE)
      id     <- "person_id"
    }
    elig_cols <- paste0(eligibility_stem, "r", seq_len(seg[["needs"]][["loop"]][["n_contexts"]]))
    if(!all(elig_cols %in% names(person))){
      stop("eligibility_stem columns not found, e.g. ", elig_cols[1], call. = FALSE)
    }

    punched <- rowSums(as.matrix(person[elig_cols]) == 1, na.rm = TRUE)
    names(punched) <- as.character(person[[id]])

    asked <- table(seg[["data"]][["stacked"]]$person_id)
    typed <- table(needs_typed_grids(seg, decisive_only)$person_id)
    ids   <- names(asked)

    typed_n <- as.numeric(typed[ids]); typed_n[is.na(typed_n)] <- 0

    censoring <- data.frame(
      measure = c("contexts punched (median)", "contexts asked (median)",
                  "contexts typed (median)", "repertoire observed", "repertoire typed"),
      value   = c(stats::median(punched[ids]), stats::median(as.numeric(asked)),
                  stats::median(typed_n),
                  paste0(round(100 * mean(as.numeric(asked) / pmax(punched[ids], 1))), "%"),
                  paste0(round(100 * mean(typed_n / pmax(punched[ids], 1))), "%"))
    )
    cat("\n=== Censoring ===\n")
    print(censoring, row.names = FALSE)
  }


  cat("\n=== Verdict ===\n")
  if(main$p_single_context < 0.01){
    cat("  People are LOCKED to their themes: ", s_obs, "% single-theme against ", s_ctx,
        "% if their contexts alone\n  decided it (reshuffle p < 0.01). The typology is about people, not occasions.\n",
        sep = "")
  } else if(main$p_single_global < 0.01){
    cat("  More consistent than a random draw, but no more than their contexts explain -\n",
        "  the lock is the context mix, not the person.\n", sep = "")
  } else {
    cat("  No person lock: single-theme share is what chance gives. Themes move with context.\n")
  }
  cat("  Breadth is censored - report it as 'at least N themes', never 'exactly N'.\n\n")

  seg[["needs"]][["reports"]][["breadth"]] <- list(
    decisive_only = decisive_only,
    main          = main,
    other         = other,
    censoring     = censoring
  )

  invisible(seg)
}
