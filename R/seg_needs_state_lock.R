#' seg_needs_state_lock
#'
#' @description Whether states belong to people or to contexts. Two readings,
#'   on whatever typing is current - adopted states or themes - and, when
#'   [seg_needs_cluster()] has been run, across every stored k so the lock can
#'   inform the choice of k.
#'
#'   \describe{
#'     \item{person lock}{take any two grids from the same respondent: how often
#'       are they in the same state, against two baselines? `chance` is two
#'       grids drawn at random from the sample. `context` is two grids drawn
#'       from the same pair of contexts the respondent was asked about, so it
#'       credits whatever the contexts alone would produce. The lock is the
#'       kappa against the context baseline: 0 = no more consistent than their
#'       contexts, 1 = always the same state.}
#'     \item{state x context}{Cramer's V between state and context, and for
#'       each context the state it over-indexes on most.}
#'   }
#'
#'   Pair-based, so the baselines are exact expectations rather than a
#'   reshuffle. A strong person lock with a weak context association is the
#'   premise of a needs segmentation - two people doing the same thing want
#'   different things, and each wants much the same thing wherever they are.
#'
#' @param seg A seg object after [seg_needs_type()] or [seg_needs_cluster()].
#' @param decisive_only Logical. Use only grids that are not near-ties
#'   (default `FALSE`).
#' @param top_contexts Integer. Contexts to list in the over-index table
#'   (default: all).
#'
#' @return The seg object, invisibly, with the tables in
#'   `seg[["needs"]][["reports"]][["state_lock"]]`. Prints a verdict.
#'
#' @export
seg_needs_state_lock <- function(seg, decisive_only = FALSE, top_contexts = NULL){

  typing <- seg[["needs"]][["typing"]]
  if(!is.list(typing) || !is.data.frame(typing[["grids"]])){
    stop("No typing found. Run seg_needs_type() or seg_needs_cluster(adopt = ) first.", call. = FALSE)
  }
  ctx_labels <- seg[["needs"]][["loop"]][["context_labels"]]

  lock_stats <- function(person_id, context, state){

    ok <- !is.na(state)
    person_id <- person_id[ok]; context <- context[ok]; state <- state[ok]

    # every within-person pair of grids
    idx   <- split(seq_along(state), person_id)
    idx   <- idx[lengths(idx) >= 2]
    pairs <- do.call(rbind, lapply(idx, function(i) t(utils::combn(i, 2))))

    same <- mean(state[pairs[, 1]] == state[pairs[, 2]])

    p_state <- tabulate(state) / length(state)
    chance  <- sum(p_state^2)

    # P(state | context), then the chance two grids from these two contexts agree
    tab <- table(factor(context, sort(unique(context))), factor(state, seq_along(p_state)))
    pc  <- prop.table(tab, 1)
    rows_a <- match(context[pairs[, 1]], rownames(pc))
    rows_b <- match(context[pairs[, 2]], rownames(pc))
    ctx_chance <- mean(rowSums(pc[rows_a, , drop = FALSE] * pc[rows_b, , drop = FALSE]))

    chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
    v   <- sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))

    list(
      n_pairs    = nrow(pairs),
      same       = same,
      chance     = chance,
      ctx_chance = ctx_chance,
      kappa      = (same - ctx_chance) / (1 - ctx_chance),
      v          = v,
      table      = tab
    )
  }

  g <- needs_typed_grids(seg, decisive_only = decisive_only)
  cur <- lock_stats(g$person_id, g$context, g$theme)
  what <- if(identical(typing[["source"]], "states")) "state" else "theme"
  names_cur <- typing[["theme_names"]]


  # ---- across stored k -------------------------------------------------------
  by_k <- NULL
  stored <- seg[["needs"]][["states"]]
  if(is.list(stored) && length(stored[["k"]]) > 1){
    stacked <- seg[["data"]][["stacked"]]
    keep <- !stored[["flat"]]
    by_k <- lapply(stored[["k"]], function(kk){
      cl <- stored[["fits"]]$cluster_fit[[match(kk, stored[["fits"]]$n)]]$cluster
      s <- lock_stats(stacked$person_id[keep], stacked$context[keep], cl)
      data.frame(k = kk, same = round(s$same, 3), chance = round(s$chance, 3),
                 context_chance = round(s$ctx_chance, 3), lock = round(s$kappa, 3),
                 context_v = round(s$v, 3))
    }) %>% dplyr::bind_rows()
  }


  # ---- report ------------------------------------------------------------------
  cat("\n=== ", tools::toTitleCase(what), " lock: ", format(cur$n_pairs, big.mark = ","),
      " within-person pairs of grids", if(decisive_only) " (decisive only)" else "", " ===\n\n", sep = "")
  cat("  same ", what, ": ", sprintf("%.1f%%", 100 * cur$same),
      "  vs chance ", sprintf("%.1f%%", 100 * cur$chance),
      "  vs same contexts ", sprintf("%.1f%%", 100 * cur$ctx_chance), "\n", sep = "")
  cat("  person lock (kappa over the context baseline): ", sprintf("%.2f", cur$kappa), "\n", sep = "")
  cat("  ", what, " x context association (Cramer's V): ", sprintf("%.2f", cur$v), "\n", sep = "")

  pc   <- prop.table(cur$table, 1)
  base <- colSums(cur$table) / sum(cur$table)
  lift <- sweep(pc, 2, base, `/`)
  over <- data.frame(
    context = ctx_labels[as.integer(rownames(lift))],
    grids   = as.integer(rowSums(cur$table)),
    leans   = names_cur[apply(lift, 1, which.max)],
    lift    = round(apply(lift, 1, max), 2),
    share   = round(100 * pc[cbind(seq_len(nrow(pc)), apply(lift, 1, which.max))], 1)
  )
  over <- over[order(-over$lift), ]
  if(!is.null(top_contexts)) over <- utils::head(over, top_contexts)
  cat("\n  Each context's most over-indexed ", what, ":\n", sep = "")
  print(over, row.names = FALSE)

  if(!is.null(by_k)){
    cat("\n  Across the stored state solutions:\n")
    print(by_k, row.names = FALSE)
  }

  cat("\n=== Verdict ===\n")
  if(cur$kappa >= 0.2 && cur$v < 0.15){
    cat("  ", tools::toTitleCase(what), "s belong to PEOPLE: a respondent's grids agree well beyond what their\n",
        "  contexts explain (lock ", sprintf("%.2f", cur$kappa), "), and context barely moves the ", what,
        " (V ", sprintf("%.2f", cur$v), ").\n", sep = "")
  } else if(cur$kappa < 0.1 && cur$v >= 0.15){
    cat("  ", tools::toTitleCase(what), "s belong to CONTEXTS: respondents are no more consistent than\n",
        "  their contexts, and context moves the ", what, " (V ", sprintf("%.2f", cur$v), ").\n", sep = "")
  } else {
    cat("  Both matter: lock ", sprintf("%.2f", cur$kappa), ", context V ", sprintf("%.2f", cur$v),
        ". Profile the cut by context as well as by person.\n", sep = "")
  }
  if(!is.null(by_k)){
    best <- by_k$k[which.max(by_k$lock)]
    cat("  Across k the lock peaks at k = ", best, ". More states always split people more finely,\n",
        "  so a lock that holds up as k rises is the better sign than its maximum.\n", sep = "")
  }
  cat("\n")

  seg[["needs"]][["reports"]][["state_lock"]] <- list(
    source    = typing[["source"]],
    same      = cur$same,
    chance    = cur$chance,
    ctx_chance = cur$ctx_chance,
    lock      = cur$kappa,
    context_v = cur$v,
    over      = over,
    by_k      = by_k
  )

  invisible(seg)
}
