#' seg_needs_states
#'
#' @description The richer alternative to theme typing: cluster the grids into
#'   k need states on their whole need profile, rather than typing each grid to
#'   the one theme it leans toward most. A state can be a mixture - high on
#'   Escape and Frictionless together - which theme typing can only call a
#'   near-tie.
#'
#'   Runs [cluster_kmeans()] over a range of k and prints a comparison to
#'   choose from. Nothing downstream changes until a k is adopted:
#'   `adopt = 5` makes that solution the typing, in the same slot and the same
#'   shape as [seg_needs_type()] writes, so [seg_needs_breadth()],
#'   [seg_needs_cut()] and the rest run on states unchanged. Re-running
#'   [seg_needs_type()] puts the themes back.
#'
#'   \strong{The basis is grid-centred by default.} Clustering is
#'   distance-based, so rating generosity is an axis the clusters will split on
#'   unless it is removed. On Kadro, k-means on item-scaled needs produced
#'   states that were largely rating bands - intensity explained 71% of state
#'   membership at k = 4 - against 7.5% on the grid-centred basis, level with
#'   the theme typing. This is the one place in the needs path where centring
#'   earns its keep.
#'
#'   \strong{Flat grids are left out} for the same reason as in
#'   [seg_needs_type()]: centred, they are all the zero vector, so they would
#'   land together in whichever state sits nearest the origin.
#'
#'   \strong{Near-ties.} A grid almost equidistant between two centroids is
#'   the state analogue of a near-tie. The margin is `1 - d1 / d2`, the nearest
#'   centroid distance over the second-nearest: 0 is equidistant, 1 is on the
#'   centroid. Below `tie_margin` a grid is flagged, and `decisive_only`
#'   downstream reads the same flag as it does for themes.
#'
#'   There is no natural k. On the Kadro partial, silhouette beat a null with
#'   the same covariance by a flat margin at every k from 2 to 8, so k is chosen
#'   on interpretability, cell balance and [seg_needs_state_lock()], not on fit.
#'
#' @param seg A seg object after [seg_needs_prepare()].
#' @param k Integer vector of state counts to compare (default `4:8`).
#' @param adopt Integer. Make this k's solution the typing. Uses the stored
#'   run if it covers this k and basis, otherwise runs it.
#' @param state_names Character, one per state, used with `adopt`. Defaults to
#'   a placeholder naming the need each state over-indexes on most.
#' @param basis Character. `"centred"` (default) grid-centres the answers;
#'   `"centred_scaled"` also z-scores each item; `"scaled"` only z-scores -
#'   kept for comparison, and it clusters on rating level.
#' @param tie_margin Numeric. Relative margin below which a grid is a near-tie
#'   (default `0.10`).
#' @param nstart,iter_max,seed Passed to [cluster_kmeans()].
#'
#' @return The seg object with the run in `seg[["needs"]][["states"]]` and,
#'   with `adopt`, the typing replaced. Prints a comparison and a verdict.
#'
#' @export
seg_needs_states <- function(
    seg,
    k           = 4:8,
    adopt       = NULL,
    state_names = NULL,
    basis       = c("centred", "centred_scaled", "scaled"),
    tie_margin  = 0.10,
    nstart      = 20,
    iter_max    = 100,
    seed        = 1
){

  basis <- match.arg(basis)

  stacked <- seg[["data"]][["stacked"]]
  items   <- seg[["needs"]][["vars"]][["raw"]]
  labs    <- seg[["needs"]][["loop"]][["item_labels"]]

  if(!is.data.frame(stacked) || anyNA(items)){
    stop("No stacked frame. Run seg_needs_stack() and seg_needs_prepare() first.", call. = FALSE)
  }

  M    <- as.matrix(stacked[items])
  flat <- apply(M, 1, stats::sd) == 0

  X <- switch(basis,
    centred        = M - rowMeans(M),
    centred_scaled = base::scale(M - rowMeans(M)),
    scaled         = base::scale(M)
  )
  X <- unname(as.matrix(X))
  colnames(X) <- sprintf("nstate%02d", seq_along(items))


  # ---- cluster, reusing a stored run when it already covers what is asked ----
  stored <- seg[["needs"]][["states"]]
  want   <- if(is.null(adopt)) k else adopt
  reuse  <- is.list(stored) && identical(stored[["basis"]], basis) && all(want %in% stored[["k"]])

  if(!reuse){

    # seg_uuid is what cluster_add_lda() predicts against, so the grid row
    # number goes in under that name. Flat grids are dropped beforehand rather
    # than via filter_name, because the LDA accuracy is only computed unfiltered.
    df <- dplyr::bind_cols(
      dplyr::tibble(seg_uuid = seq_len(nrow(stacked))),
      as.data.frame(X),
      stacked[items]
    )[!flat, , drop = FALSE]

    # Hartigan-Wong hitting its transfer limit on a few starts is noise here:
    # nstart keeps the best of the starts that did converge.
    res <- withCallingHandlers(
      cluster_kmeans(df, vars = colnames(X), vars_profiles = items, solution_name = "N",
                     n_min = min(want), n_max = max(want), nstart = nstart,
                     iter_max = iter_max, seed = seed),
      warning = function(w){
        if(grepl("Quick-TRANSfer", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    )[["all_inputs"]]

    stored <- list(basis = basis, k = res$n, fits = res, flat = flat, X = X)
    seg[["needs"]][["states"]] <- stored
  }

  fits <- stored[["fits"]]
  keep <- !stored[["flat"]]
  Xk   <- stored[["X"]][keep, , drop = FALSE]
  intensity <- rowMeans(M)[keep]

  # theme typing to compare against: the current typing if it is themes, else
  # the copy kept when states were adopted
  themes_typing <- if(is.list(seg[["needs"]][["typing"]]) &&
                      identical(seg[["needs"]][["typing"]][["source"]], "themes")){
    seg[["needs"]][["typing"]]
  } else {
    seg[["needs"]][["typing_themes"]]
  }

  assignment <- function(kk){
    fit <- fits$cluster_fit[[match(kk, fits$n)]]
    D <- vapply(seq_len(nrow(fit$centers)), function(j){
      sqrt(rowSums(sweep(Xk, 2, fit$centers[j, ])^2))
    }, numeric(nrow(Xk)))
    ord <- t(apply(D, 1, order))
    d1  <- D[cbind(seq_len(nrow(D)), ord[, 1])]
    d2  <- D[cbind(seq_len(nrow(D)), ord[, 2])]
    list(fit = fit, state = ord[, 1], second = ord[, 2], margin = 1 - d1 / pmax(d2, 1e-12), D = D)
  }


  # ---- comparison across k -----------------------------------------------------
  if(is.null(adopt)){

    cmp <- lapply(k, function(kk){
      a <- assignment(kk)
      shares <- tabulate(a$state, nbins = kk) / length(a$state)
      row <- data.frame(
        k             = kk,
        smallest_pct  = round(100 * min(shares), 1),
        largest_pct   = round(100 * max(shares), 1),
        near_tie_pct  = round(100 * mean(a$margin < tie_margin), 1),
        intensity_eta = round(summary(stats::lm(intensity ~ factor(a$state)))$r.squared, 3),
        lda_accuracy  = round(fits$accuracy[match(kk, fits$n)], 3)
      )
      if(is.list(themes_typing)){
        th <- themes_typing[["grids"]]$top[keep]
        row$agree_themes <- round(needs_ari(a$state, th), 2)
      }
      row
    }) %>% dplyr::bind_rows()

    cat("\n=== Need states: ", format(sum(keep), big.mark = ","), " grids, ", basis, " basis",
        " (", sum(!keep), " flat grids left out) ===\n\n", sep = "")
    print(cmp, row.names = FALSE)
    cat("\n  intensity_eta = share of rating intensity explained by state; lda_accuracy = how well a\n",
        "  grid's raw answers recover its state (a typing tool); agree_themes = adjusted Rand\n",
        "  index with the theme typing.\n", sep = "")

    cat("\n=== Verdict ===\n")
    if(any(cmp$intensity_eta > 0.3)){
      cat("  CHECK - at k = ", paste(cmp$k[cmp$intensity_eta > 0.3], collapse = ", "),
          " the states are largely rating levels (eta > 0.3). Use the centred basis.\n", sep = "")
    }
    small <- cmp$k[cmp$smallest_pct < 5]
    if(length(small)){
      cat("  k = ", paste(small, collapse = ", "), " leave a state under 5% of grids.\n", sep = "")
    }
    cat("  There is no natural k in needs data - choose on interpretability and balance, then\n",
        "  read seg_needs_state_lock() for the k you are weighing. Adopt one with adopt = <k>.\n\n",
        sep = "")

    seg[["needs"]][["reports"]][["states"]] <- cmp
    return(seg)
  }


  # ---- adopt one k as the typing ---------------------------------------------------
  if(length(adopt) != 1) stop("adopt takes a single k.", call. = FALSE)
  a  <- assignment(adopt)
  kk <- adopt

  # what each state over-indexes on, in the basis units, against the grid mean
  centre <- colMeans(Xk)
  lift   <- sweep(a$fit$centers, 2, centre)
  describe <- vapply(seq_len(kk), function(j){
    paste(labs[order(-lift[j, ])[1:4]], collapse = "; ")
  }, character(1))

  if(is.null(state_names)){
    # One need, not two joined with "+" - the cut joins states with "+" to
    # label its pairs, so a compound name would make those unreadable.
    state_names <- paste0("S", seq_len(kk), " ", labs[apply(lift, 1, which.max)])
  }
  if(length(state_names) != kk){
    stop("state_names has ", length(state_names), " names for ", kk, " states.", call. = FALSE)
  }

  n <- nrow(stacked)
  full <- function(x, fill = NA) { out <- rep(fill, n); out[keep] <- x; out }

  near <- full(a$margin < tie_margin, FALSE)
  D_full <- matrix(NA_real_, n, kk); D_full[keep, ] <- a$D

  grids <- dplyr::tibble(
    person_id           = stacked$person_id,
    context             = stacked$context,
    need_theme          = full(a$state, NA_integer_),
    need_theme_top      = full(a$state, NA_integer_),
    need_theme_second   = full(a$second, NA_integer_),
    need_theme_margin   = round(full(a$margin, NA_real_), 4),
    need_theme_near_tie = as.integer(near),
    need_theme_flat     = as.integer(!keep)
  ) %>%
    # nearer is better, so the score is the negative distance
    dplyr::bind_cols(as.data.frame(-D_full) %>% rlang::set_names(paste0("need_theme_score", seq_len(kk))))

  # keep the theme typing so it can be compared with, and restored
  if(is.list(seg[["needs"]][["typing"]]) && identical(seg[["needs"]][["typing"]][["source"]], "themes")){
    seg[["needs"]][["typing_themes"]] <- seg[["needs"]][["typing"]]
  }

  seg <- needs_attach_typing(seg, grids)

  eta2 <- summary(stats::lm(intensity ~ factor(a$state)))$r.squared

  seg[["needs"]][["typing"]] <- list(
    grids       = needs_typing_table(grids),
    source      = "states",
    k           = kk,
    basis       = basis,
    centers     = a$fit$centers,
    ties        = "flag",
    tie_margin  = tie_margin,
    flat        = "exclude",
    theme_names = state_names,
    describe    = describe,
    eta2        = eta2
  )

  shares <- tabulate(a$state, nbins = kk) / length(a$state)
  cat("\n=== Need states adopted: k = ", kk, " (", basis, " basis) ===\n\n", sep = "")
  print(data.frame(
    state        = state_names,
    pct          = round(100 * shares, 1),
    near_tie_pct = round(100 * vapply(seq_len(kk), function(j) mean(a$margin[a$state == j] < tie_margin),
                                      numeric(1)), 1),
    over_indexes = describe
  ), row.names = FALSE)
  cat("\n  near-ties (margin < ", tie_margin, "): ", round(100 * mean(a$margin < tie_margin), 1),
      "% of grids | intensity explained by state: ", sprintf("%.3f", eta2), "\n", sep = "")
  if(is.list(themes_typing)){
    cat("  agreement with the theme typing (adjusted Rand): ",
        sprintf("%.2f", needs_ari(a$state, themes_typing[["grids"]]$top[keep])), "\n", sep = "")
  }
  cat("\n  The person layer now reads states. Name them with state_names = before the cut;\n",
      "  seg_needs_type() restores the themes.\n\n", sep = "")

  seg
}
