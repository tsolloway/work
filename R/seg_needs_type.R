#' seg_needs_type
#'
#' @description Types every grid to its dominant theme, and records by how
#'   much. Each theme is scored as the mean of its items, the grid goes to the
#'   highest-scoring theme, and the margin to second place is kept alongside.
#'
#'   \strong{The basis is item-scaled, always.} Each item is z-scored across
#'   grids before the themes are scored, and this function does that itself
#'   from the survey answers rather than trusting whatever
#'   [seg_needs_prepare()] was run with. Unscaled, a theme built from consensus
#'   items wins by default: on the Kadro hearables data comfort, secure fit and
#'   battery are simply the highest-rated needs, so an unscaled Frictionless
#'   theme took 47% of grids and the niche Smart Utility theme 2% - for that
#'   reason alone. Nothing in the output looks wrong when that happens, which is
#'   why it is not an option.
#'
#'   \strong{There is no centring option.} Grid-centring subtracts the same
#'   row constant from every theme score, so it cannot change which theme wins -
#'   and combined with scaling it doubled the association between rating style
#'   and theme (eta-squared 0.08 -> 0.16).
#'
#'   \strong{Near-ties are a first-class outcome.} On Kadro a third of grids
#'   have their top two themes within 0.15 of each other. Those grids do not
#'   have a dominant theme; they have two. Counting them or not moves the
#'   single-theme share of respondents from 39% to 56%, so how they are handled
#'   has to be a decision, not a side effect of `which.max()`:
#'
#'   \describe{
#'     \item{`"flag"`}{(default) type to the top theme and mark the grid as a
#'       near-tie. The person layer can then be read both ways with
#'       `decisive_only`.}
#'     \item{`"exclude"`}{leave near-ties untyped. They drop out of everything
#'       downstream, which deepens the censoring on breadth.}
#'     \item{`"first"`}{take the top theme however slim the margin. Kept for
#'       comparison; the person layer cannot then separate the two readings.}
#'   }
#'
#'   Every grid keeps its margin and runner-up whatever `ties` says.
#'
#'   \strong{Flat grids are not typed.} A grid rated the same on every need
#'   has no dominant theme, but item-scaling gives it one anyway: a constant
#'   row lands highest on the lowest-mean items, so every straight-lined grid
#'   goes to the most niche theme, and by a margin wide enough to pass as
#'   decisive. On Kadro that was 649 grids (8.6%), all of them Smart Utility,
#'   and 133 respondents flat in every context - who would otherwise read as
#'   the most single-minded people in the file. It is the mirror image of the
#'   consensus bias the scaling removes, so it is handled the same way as a
#'   near-tie: recorded, and left untyped unless `flat = "type"`.
#'
#' @param seg A seg object after [seg_needs_themes()].
#' @param ties Character. How to treat near-ties - see above.
#' @param tie_margin Numeric. Top-two gap, in theme-score units (mean item
#'   z-score), below which a grid is a near-tie (default `0.15`).
#' @param flat Character. `"exclude"` (default) leaves grids with no spread
#'   untyped; `"type"` types them like any other, for comparison only.
#' @param flat_sd Numeric. Within-grid SD of the answers at or below which a
#'   grid is flat (default `0`, i.e. identical answers throughout).
#'
#' @return The seg object. The typing is in `seg[["needs"]][["typing"]]` and
#'   joined, by `person_id` and `context`, onto the stacked frame and onto
#'   `seg$data$original` / `seg$data$with_shell` when those are grid-level, as
#'   `need_theme`, `need_theme_second`, `need_theme_margin`,
#'   `need_theme_near_tie`, `need_theme_flat` and one `need_theme_score<k>` per
#'   theme. Prints a verdict.
#'
#' @export
seg_needs_type <- function(seg,
                           ties       = c("flag", "exclude", "first"),
                           tie_margin = 0.15,
                           flat       = c("exclude", "type"),
                           flat_sd    = 0){

  ties <- match.arg(ties)
  flat <- match.arg(flat)

  themes <- seg[["needs"]][["themes"]]
  if(!is.data.frame(themes)){
    stop("No theme assignment. Run seg_needs_themes() first.", call. = FALSE)
  }

  stacked <- seg[["data"]][["stacked"]]
  items   <- seg[["needs"]][["vars"]][["raw"]]
  tnames  <- seg[["needs"]][["theme_names"]]
  K       <- length(tnames)

  M <- as.matrix(stacked[items])
  sds <- apply(M, 2, stats::sd)
  if(any(sds == 0)){
    stop("Item(s) with no variance across grids cannot be scaled: ",
         paste(themes$label[sds == 0], collapse = ", "), call. = FALSE)
  }

  # item-scaled across ALL grids - see the description for why this is fixed
  Z <- base::scale(M)

  S <- vapply(seq_len(K), function(k){
    rows <- which(!is.na(themes$theme) & themes$theme == k)
    rowMeans(sweep(Z[, rows, drop = FALSE], 2, themes$sign[rows], `*`))
  }, numeric(nrow(Z)))
  S <- matrix(S, ncol = K)

  top    <- max.col(S, ties.method = "first")
  S2     <- S
  S2[cbind(seq_len(nrow(S)), top)] <- -Inf
  second <- max.col(S2, ties.method = "first")
  margin <- S[cbind(seq_len(nrow(S)), top)] - S2[cbind(seq_len(nrow(S)), second)]

  spread  <- apply(M, 1, stats::sd)
  is_flat <- spread <= flat_sd
  # a flat grid's margin measures the item means, not the respondent, so it
  # is never a near-tie - it is its own outcome
  near    <- margin < tie_margin & !is_flat

  theme <- top
  if(ties == "exclude") theme[near]    <- NA_integer_
  if(flat == "exclude") theme[is_flat] <- NA_integer_

  score_names <- paste0("need_theme_score", seq_len(K))

  grids <- dplyr::tibble(
    person_id           = stacked$person_id,
    context             = stacked$context,
    need_theme          = theme,
    need_theme_top      = top,
    need_theme_second   = second,
    need_theme_margin   = round(margin, 4),
    need_theme_near_tie = as.integer(near),
    need_theme_flat     = as.integer(is_flat)
  ) %>%
    dplyr::bind_cols(as.data.frame(S) %>% rlang::set_names(score_names))

  if(anyDuplicated(grids[c("person_id", "context")])){
    stop("person_id + context does not identify a grid on the stacked frame.", call. = FALSE)
  }


  # ---- report -------------------------------------------------------------
  # shares, margins and the intensity association are read on the grids that
  # carry information about need shape; flat grids are reported on their own
  inf <- !is_flat
  intensity <- rowMeans(M)
  eta2 <- summary(stats::lm(intensity[inf] ~ factor(top[inf])))$r.squared

  share_all <- tabulate(top[inf], nbins = K) / sum(inf)
  share_dec <- tabulate(top[inf & !near], nbins = K) / sum(inf & !near)

  cat("\n=== Needs typing: ", format(nrow(grids), big.mark = ","), " grids, ",
      K, " themes, item-scaled basis ===\n\n", sep = "")
  print(data.frame(
    theme        = tnames,
    items        = tabulate(stats::na.omit(themes$theme), nbins = K),
    pct_typed    = round(100 * share_all, 1),
    pct_decisive = round(100 * share_dec, 1),
    pct_near_tie = round(100 * vapply(seq_len(K), function(k) mean(near[inf & top == k]), numeric(1)), 1)
  ), row.names = FALSE)

  cat("\n  flat grids (no spread): ", sum(is_flat), " (", round(100 * mean(is_flat), 1), "%) -> ",
      if(flat == "exclude") "left untyped" else "typed like any other", sep = "")
  if(any(is_flat)){
    ft <- tabulate(top[is_flat], nbins = K)
    cat("; scoring alone would put ", round(100 * max(ft) / sum(ft)), "% of them in ",
        tnames[which.max(ft)], sep = "")
  }
  cat("\n  margin to second place (quartiles): ",
      paste(sprintf("%.2f", stats::quantile(margin[inf], c(.25, .5, .75))), collapse = " / "), "\n", sep = "")
  cat("  near-ties (< ", tie_margin, "): ", round(100 * mean(near[inf]), 1), "% of differentiated grids -> ",
      switch(ties,
             flag    = "typed to the top theme and flagged",
             exclude = "left untyped",
             first   = "typed to the top theme as if decisive"),
      "\n", sep = "")
  cat("  rating intensity explained by theme (eta-squared): ", sprintf("%.3f", eta2), "\n", sep = "")


  cat("\n=== Verdict ===\n")
  if(max(share_all) > 0.5){
    cat("  CHECK - ", tnames[which.max(share_all)], " takes ", round(100 * max(share_all)),
        "% of grids. A theme winning outright usually means its items are the\n",
        "  highest-rated rather than the most distinctive - check the theme assignment.\n", sep = "")
  } else {
    cat("  No theme wins by default (largest ", round(100 * max(share_all)), "%, smallest ",
        round(100 * min(share_all)), "%).\n", sep = "")
  }

  # The flat-grid problem shades into grids that are NEARLY flat: one or two
  # answers off a straight line still leave the row constant in charge of the
  # scores. Check the least-spread decile of the differentiated grids for the
  # same pull toward one theme.
  low <- inf & spread <= stats::quantile(spread[inf], 0.10)
  if(sum(low) > 0){
    lt <- tabulate(top[low], nbins = K) / sum(low)
    if(max(lt) > 0.6){
      cat("  CHECK - ", round(100 * max(lt)), "% of the least-spread tenth of grids go to ",
          tnames[which.max(lt)], ". A grid one or two\n",
          "  answers off a straight line is typed mostly by its rating level, not its need shape.\n",
          "  flat_sd = 0.25 leaves those untyped on a 3-point scale; either way, read theme sizes\n",
          "  with grid intensity in view.\n", sep = "")
    }
  }

  if(mean(near[inf]) >= 0.15){
    cat("  ", round(100 * mean(near[inf])), "% of grids are near-ties - enough to move every person-level\n",
        "  number. ",
        if(ties == "first") "ties = 'first' hides this; re-type with 'flag'." else
          "Read the person layer with decisive_only both ways before reporting it.",
        "\n", sep = "")
  }

  cat("  Rating intensity explains ", round(100 * eta2), "% of the variation across themes. That is\n",
      "  real - intensity and need-shape are correlated in the population - so profile grid\n",
      "  intensity alongside the themes rather than removing it.\n\n", sep = "")


  # ---- write back -----------------------------------------------------------
  describe <- vapply(seq_len(K), function(k){
    paste(themes$label[!is.na(themes$theme) & themes$theme == k], collapse = "; ")
  }, character(1))

  seg <- needs_attach_typing(seg, grids)

  seg[["needs"]][["typing"]] <- list(
    grids       = needs_typing_table(grids),
    source      = "themes",
    ties        = ties,
    tie_margin  = tie_margin,
    flat        = flat,
    theme_names = tnames,
    describe    = describe,
    eta2        = eta2
  )

  seg
}
