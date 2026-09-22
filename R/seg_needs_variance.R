#' seg_needs_variance
#'
#' @description The decision gate. Decomposes the needs variance into person
#'   and context components and reports whether a needs segmentation is a
#'   person-level or a context-level exercise - before anything is built on it.
#'
#'   Four readings:
#'
#'   \describe{
#'     \item{ICC}{share of each item's variance that is between-person. High =
#'       the item is a trait. Low = it moves with context.}
#'     \item{reliability}{how well a respondent's mean over their observed
#'       contexts estimates their true mean, given how few contexts they saw.}
#'     \item{interaction}{size of the context-by-item effect against the person
#'       effect, with each item's own level removed so a uniformly low item
#'       cannot masquerade as a context effect.}
#'     \item{top interactions}{the context-by-item cells that actually move,
#'       so tautologies (sleeping raises rest/sleep) are visible as such.}
#'   }
#'
#'   A low median ICC with a large interaction says segment contexts. A
#'   moderate-to-high ICC with a small interaction says segment people, and the
#'   contexts belong in the profile. Note that a small interaction is NOT a
#'   failure of the technique: the premise of a needs segmentation is that two
#'   people doing the SAME thing want different things.
#'
#' @param seg A seg object after [seg_needs_prepare()].
#' @param top_n Integer. How many context-by-item interactions to print.
#'
#' @return The seg object, invisibly, with the tables in
#'   `seg[["needs"]][["reports"]][["variance"]]`. Prints a verdict.
#'
#' @export
seg_needs_variance <- function(seg, top_n = 12){

  stacked <- seg[["data"]][["stacked"]]
  items   <- seg[["needs"]][["vars"]][["raw"]]
  labs    <- seg[["needs"]][["loop"]][["item_labels"]]
  ctx     <- seg[["needs"]][["loop"]][["context_labels"]]

  if(!is.data.frame(stacked)) stop("Run seg_needs_stack() first.", call. = FALSE)

  pid <- stacked$person_id
  M   <- as.matrix(stacked[items])
  n_per_person <- mean(table(pid))

  decomp <- lapply(seq_along(items), function(k){

    x  <- M[, k]
    pm <- tapply(x, pid, mean)

    within  <- mean(tapply(x, pid, function(v) if(length(v) < 2) NA else stats::var(v)), na.rm = TRUE)
    between <- max(stats::var(pm) - within / n_per_person, 0)

    data.frame(
      item        = labs[k],
      ICC         = round(between / (between + within), 2),
      reliability = round(between / (between + within / n_per_person), 2),
      sd_person   = round(sqrt(between), 3),
      sd_context  = round(sqrt(within), 3)
    )
  }) %>% dplyr::bind_rows()


  # context x item interaction, each item's own level removed
  C   <- as.matrix(stacked[seg[["needs"]][["vars"]][["centred"]]])
  dev <- t(vapply(
    sort(unique(stacked$context)),
    function(j) colMeans(C[stacked$context == j, , drop = FALSE]),
    numeric(length(items))
  ))
  inter <- sweep(dev, 2, colMeans(dev))
  dimnames(inter) <- list(ctx[sort(unique(stacked$context))], labs)

  person_sd <- stats::sd(sweep(
    as.matrix(stacked[seg[["needs"]][["vars"]][["person"]]]), 2,
    colMeans(as.matrix(stacked[seg[["needs"]][["vars"]][["person"]]]))
  ))
  ratio <- person_sd / stats::sd(inter)


  cat("\n=== Variance decomposition (", nrow(stacked), " grids / ",
      dplyr::n_distinct(pid), " respondents, ", round(n_per_person, 1),
      " contexts each) ===\n\n", sep = "")
  print(decomp[order(-decomp$ICC), ], row.names = FALSE)

  cat("\n  median ICC         : ", stats::median(decomp$ICC), "\n", sep = "")
  cat("  median reliability : ", stats::median(decomp$reliability), "\n", sep = "")
  cat("  person effect SD   : ", round(person_sd, 3), "\n", sep = "")
  cat("  context x item SD  : ", round(stats::sd(inter), 3),
      "   (person effect is ", round(ratio, 1), "x larger)\n", sep = "")

  cat("\n=== Largest context x item interactions ===\n")
  hit <- which(abs(inter) > stats::quantile(abs(inter), 1 - top_n / length(inter)), arr.ind = TRUE)
  tops <- data.frame(
    context = rownames(inter)[hit[, 1]],
    item    = colnames(inter)[hit[, 2]],
    effect  = round(inter[hit], 2)
  )
  print(utils::head(tops[order(-abs(tops$effect)), ], top_n), row.names = FALSE)

  cat("\n=== Verdict ===\n")
  med <- stats::median(decomp$ICC)

  if(ratio >= 2 && med >= 0.35){
    cat("  PERSON-level. Needs are traits; contexts belong in the profile,\n",
        "  not as the unit of analysis. Segment people.\n", sep = "")
  } else if(ratio < 1.5){
    cat("  CONTEXT-level. Needs move with the situation more than with the\n",
        "  person. A grid-level state solution is warranted.\n", sep = "")
  } else {
    cat("  MIXED. Build both and compare - neither unit is clearly dominant.\n")
  }
  cat("  (a small interaction is expected and desirable: the premise is that\n",
      "   two people in the SAME context want different things)\n\n", sep = "")

  seg[["needs"]][["reports"]][["variance"]] <-
    list(items = decomp, interaction = inter, ratio = ratio)

  invisible(seg)
}
