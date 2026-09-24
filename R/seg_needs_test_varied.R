#' seg_needs_test_varied
#'
#' @description Tests what the varied cell of the needs cut actually is. A
#'   respondent whose contexts land in three different themes could be three
#'   different things, and each calls for a different treatment of the cell:
#'
#'   \describe{
#'     \item{versatility}{they genuinely want different things in different
#'       contexts. Their themes track their contexts, and they do more of them.
#'       A real segment.}
#'     \item{low engagement}{they answered carelessly, so their grids are noise
#'       and land anywhere. Flat grids, fast completes. Not a segment.}
#'     \item{near-tie artifact}{their grids were close calls, and resolving the
#'       ties scattered them. Narrow margins. A by-product of the typing.}
#'   }
#'
#'   Each hypothesis is a named set of indicators with a predicted direction.
#'   The built-in indicators are computed from the typing; any person-level
#'   column can be added. The defaults use whatever is available:
#'
#'   \preformatted{
#'   versatility       context_fit +, contexts_punched +
#'   low_engagement    grid_spread -, duration -
#'   near_tie_artifact near_tie_share +, mean_margin -
#'   }
#'
#'   \describe{
#'     \item{`context_fit`}{mean, over the respondent's typed grids, of how
#'       common that grid's theme is in that context across the sample}
#'     \item{`grid_spread`}{mean within-grid SD of the answers}
#'     \item{`intensity`}{mean rating across the respondent's grids}
#'     \item{`near_tie_share`, `mean_margin`}{how close their typing calls were}
#'     \item{`contexts_punched`}{contexts they do (needs `eligibility_stem`)}
#'     \item{`duration`}{the column named by `duration_var`}
#'   }
#'
#'   The varied group is compared only with respondents who COULD have been
#'   varied - enough typed contexts to show more than `max_themes` themes.
#'   Anyone asked about fewer contexts cannot land in the cell, and including
#'   them would make "does more contexts" look like a finding about the cell.
#'   Comparisons are rank-based (Wilcoxon), so a skewed duration or a few
#'   extreme values cannot carry a verdict; the effect is the probability that a
#'   varied respondent outranks a comparison respondent (0.5 = no difference).
#'
#'   Alongside the hypotheses it reports how many varied respondents are only
#'   varied because of how their near-ties fell: if letting each near-tie grid
#'   take its runner-up theme could bring them within `max_themes`, their
#'   membership is fragile.
#'
#' @param seg A seg object after [seg_needs_cut()].
#' @param hypotheses Named list of named character vectors, indicator ->
#'   `"+"` or `"-"`, e.g.
#'   `list(tech_breadth = c(CD05 = "+", CD06 = "+"))`. Replaces the defaults;
#'   use [seg_needs_test_varied_defaults()] to extend them.
#' @param duration_var Character. Person-level interview duration column, e.g.
#'   `"qtime"`.
#' @param eligibility_stem Character. Stem of the contexts-done multi-punch,
#'   e.g. `"Occasions"`.
#' @param alpha Numeric. Significance level (default `0.05`).
#'
#' @return The seg object, invisibly, with the indicator table and verdicts in
#'   `seg[["needs"]][["reports"]][["test_varied"]]`. Prints a verdict per
#'   hypothesis.
#'
#' @export
seg_needs_test_varied <- function(seg, hypotheses = NULL, duration_var = NULL,
                                  eligibility_stem = NULL, alpha = 0.05){

  cut <- seg[["needs"]][["cut"]]
  if(!is.list(cut) || !is.data.frame(cut[["persons"]])){
    stop("No cut found. Run seg_needs_cut() first.", call. = FALSE)
  }
  if(!"varied" %in% cut[["cells"]]$type){
    stop("This cut has no varied cell - max_themes covers every combination respondents can show.",
         call. = FALSE)
  }

  persons    <- cut[["persons"]]
  max_themes <- cut[["max_themes"]]
  grids      <- seg[["needs"]][["typing"]][["grids"]]
  stacked    <- seg[["data"]][["stacked"]]


  # ---- built-in indicators ----------------------------------------------------
  M <- as.matrix(stacked[seg[["needs"]][["vars"]][["raw"]]])
  gl <- dplyr::tibble(
    person_id = stacked$person_id,
    context   = stacked$context,
    spread    = apply(M, 1, stats::sd),
    intensity = rowMeans(M)
  ) %>% dplyr::left_join(grids, by = c("person_id", "context"))

  # how typical each theme is of each context, from all typed grids
  typed <- gl[!is.na(gl$theme), ]
  fit_tbl <- typed %>%
    dplyr::count(.data$context, .data$theme) %>%
    dplyr::group_by(.data$context) %>%
    dplyr::mutate(p_theme = .data$n / sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"n")

  # Spread and fit are read on typed grids only: flat grids are untyped, and a
  # respondent who is varied has none among their typed contexts by definition,
  # so counting flat grids would hand the comparison group a low spread for
  # free.
  metrics <- typed %>%
    dplyr::left_join(fit_tbl, by = c("context", "theme")) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      context_fit    = mean(.data$p_theme),
      grid_spread    = mean(.data$spread),
      intensity      = mean(.data$intensity),
      near_tie_share = mean(.data$near_tie),
      mean_margin    = mean(.data$margin),
      .groups = "drop"
    )

  pf <- if(is.data.frame(seg[["data"]][["with_shell"]])) needs_person_frame(seg) else
    dplyr::distinct(stacked, .data$person_id, .keep_all = TRUE)

  if(!is.null(eligibility_stem)){
    elig_cols <- paste0(eligibility_stem, "r", seq_len(seg[["needs"]][["loop"]][["n_contexts"]]))
    if(!all(elig_cols %in% names(pf))) stop("eligibility_stem columns not found, e.g. ", elig_cols[1], call. = FALSE)
    metrics$contexts_punched <- rowSums(as.matrix(pf[match(metrics$person_id, pf$person_id), elig_cols]) == 1,
                                        na.rm = TRUE)
  }
  if(!is.null(duration_var)){
    if(!duration_var %in% names(pf)) stop("duration_var '", duration_var, "' not found.", call. = FALSE)
    metrics$duration <- pf[[duration_var]][match(metrics$person_id, pf$person_id)]
  }


  # ---- hypotheses ---------------------------------------------------------------
  if(is.null(hypotheses)) hypotheses <- seg_needs_test_varied_defaults()

  if(isTRUE(cut[["decisive_only"]]) && "near_tie_artifact" %in% names(hypotheses)){
    message("The cut was built from decisive grids only, so no one in it has a near-tie ",
            "to be an artifact of. Skipping near_tie_artifact.")
    hypotheses[["near_tie_artifact"]] <- NULL
  }

  wanted <- unique(unlist(lapply(hypotheses, names)))
  from_pf <- setdiff(wanted, names(metrics))
  builtin_optional <- c("contexts_punched", "duration")

  unknown <- setdiff(from_pf, c(names(pf), builtin_optional))
  if(length(unknown)){
    stop("Indicator(s) not found as a built-in or a person-level column: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  for(v in intersect(from_pf, names(pf))){
    metrics[[v]] <- pf[[v]][match(metrics$person_id, pf$person_id)]
  }

  # optional built-ins without their source are dropped from the set, not failed
  skipped <- setdiff(intersect(wanted, builtin_optional), names(metrics))
  hypotheses <- lapply(hypotheses, function(h) h[!names(h) %in% skipped])
  hypotheses <- hypotheses[lengths(hypotheses) > 0]


  # ---- compare ------------------------------------------------------------------
  eligible <- persons[!is.na(persons$cell) & persons$n_typed > max_themes, ]
  eligible$varied <- eligible$cell_type == "varied"
  d <- dplyr::left_join(eligible[c("person_id", "varied")], metrics, by = "person_id")

  test_one <- function(v){
    x <- d[[v]][d$varied];  y <- d[[v]][!d$varied]
    x <- x[!is.na(x)];      y <- y[!is.na(y)]
    wt <- stats::wilcox.test(x, y, exact = FALSE)
    auc <- unname(wt$statistic) / (length(x) * length(y))
    data.frame(indicator = v, varied = signif(stats::median(x), 3), rest = signif(stats::median(y), 3),
               varied_mean = signif(mean(x), 3), rest_mean = signif(mean(y), 3),
               outranks = round(auc, 3), p_value = signif(wt$p.value, 3))
  }

  rows <- lapply(names(hypotheses), function(h){
    pred <- hypotheses[[h]]
    out <- dplyr::bind_rows(lapply(names(pred), test_one))
    out$hypothesis <- h
    out$predicted  <- unname(pred)
    out$observed   <- ifelse(out$outranks > 0.5, "+", "-")
    out$result     <- ifelse(out$p_value >= alpha, "no difference",
                             ifelse(out$observed == out$predicted, "as predicted", "AGAINST"))
    out
  })
  tbl <- dplyr::bind_rows(rows)
  tbl <- tbl[c("hypothesis", "indicator", "predicted", "varied", "rest", "varied_mean", "rest_mean",
               "outranks", "p_value", "result")]

  verdict <- vapply(names(hypotheses), function(h){
    r <- tbl$result[tbl$hypothesis == h]
    if(all(r == "as predicted"))            "SUPPORTED"
    else if(any(r == "AGAINST") && any(r == "as predicted")) "MIXED"
    else if(any(r == "AGAINST"))            "CONTRADICTED"
    else if(any(r == "as predicted"))       "PARTLY SUPPORTED"
    else                                    "NOT SUPPORTED"
  }, character(1))


  # ---- fragility: varied only because of how the near-ties fell -----------------
  vg <- grids[grids$person_id %in% eligible$person_id[eligible$varied] & !is.na(grids$theme), ]
  if(isTRUE(cut[["decisive_only"]])) vg <- vg[!vg$near_tie, ]

  fragile <- vapply(split(vg, vg$person_id), function(p){
    fixed <- p$theme[!p$near_tie]
    open  <- p[p$near_tie, ]
    if(nrow(open) == 0) return(FALSE)
    options <- expand.grid(lapply(seq_len(nrow(open)), function(i) c(open$top[i], open$second[i])))
    any(apply(options, 1, function(o) length(unique(c(fixed, o))) <= max_themes))
  }, logical(1))


  # ---- report -------------------------------------------------------------------
  cat("\n=== What is the varied cell? ", sum(eligible$varied), " varied vs ",
      sum(!eligible$varied), " respondents who could have been ===\n\n", sep = "")
  shown <- tbl[c("hypothesis", "indicator", "predicted", "varied", "rest", "outranks", "p_value", "result")]
  print(shown, row.names = FALSE)
  cat("\n  (varied / rest are medians; outranks = chance a varied respondent scores higher)\n")
  if(length(skipped)){
    cat("  skipped for want of a source: ", paste(skipped, collapse = ", "),
        " - supply eligibility_stem / duration_var\n", sep = "")
  }

  cat("\n  fragile membership: ", sum(fragile), " of ", length(fragile), " (",
      round(100 * mean(fragile)), "%) varied respondents would fall within ", max_themes,
      " themes\n  if their near-tie grids took the runner-up theme\n", sep = "")

  cat("\n=== Verdict ===\n")
  for(h in names(verdict)) cat(sprintf("  %-20s %s\n", h, verdict[[h]]))

  v_of   <- function(h) if(h %in% names(verdict)) verdict[[h]] else ""
  is_art <- v_of("near_tie_artifact")
  is_low <- v_of("low_engagement")
  is_ver <- v_of("versatility")
  # Artifact outranks versatility: if most of the cell only exists because of
  # how close calls fell, "versatile" describes a minority of it at best.
  yes <- c("SUPPORTED", "PARTLY SUPPORTED")
  artifact <- is_art %in% yes || mean(fragile) > 0.5
  cat("\n")
  if(is_low %in% yes){
    cat("  The varied cell shows signs of low engagement - check it before presenting it as a segment.\n")
  } else if(artifact){
    cat("  The varied cell is mostly a near-tie artifact: ", round(100 * mean(fragile)),
        "% of it would not be varied if\n  its close calls fell the other way.",
        if(is_low %in% c("CONTRADICTED", "NOT SUPPORTED")) " It is not disengagement." else "",
        "\n  Re-cut with decisive_only = TRUE to find the core that is varied on clear calls,\n",
        "  and test that.\n", sep = "")
  } else if(is_ver %in% yes){
    cat("  The varied cell reads as genuinely versatile - keep it as a segment.\n")
  } else {
    cat("  No single reading dominates - describe the cell by its profile, not by a story.\n")
  }
  cat("\n")

  seg[["needs"]][["reports"]][["test_varied"]] <- list(
    indicators = tbl,
    verdict    = verdict,
    fragile    = dplyr::tibble(person_id = names(fragile), fragile = unname(fragile))
  )

  invisible(seg)
}


#' seg_needs_test_varied_defaults
#'
#' @description The default hypothesis sets for [seg_needs_test_varied()], for
#'   extending rather than replacing them.
#'
#' @return A named list of named character vectors.
#'
#' @export
seg_needs_test_varied_defaults <- function(){
  list(
    versatility       = c(context_fit = "+", contexts_punched = "+"),
    low_engagement    = c(grid_spread = "-", duration = "-"),
    near_tie_artifact = c(near_tie_share = "+", mean_margin = "-")
  )
}
