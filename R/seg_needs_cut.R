#' seg_needs_cut
#'
#' @description The person-level typology: one cut that carries both what a
#'   respondent needs and how consistently they need it. Each respondent is
#'   placed by the set of themes their typed contexts fall into:
#'
#'   \describe{
#'     \item{single}{every typed context in one theme - one cell per theme}
#'     \item{pair}{exactly two themes - one cell per pair}
#'     \item{...}{and so on up to `max_themes`}
#'     \item{varied}{more than `max_themes` themes - one cell}
#'   }
#'
#'   With 3 contexts asked and the default `max_themes = 2`, that is singles +
#'   pairs + fully varied: 4 themes give 4 + 6 + 1 = 11 cells. More contexts
#'   per respondent would let people show three themes without being fully
#'   varied, and `max_themes = 3` names those triples - but the cell count
#'   grows combinatorially (4 themes: 14 cells before the varied group; 5
#'   themes: 25), so check the sizes this prints before committing.
#'
#'   A respondent with fewer than two typed contexts cannot show consistency
#'   either way and is left unclassified (code `NA`), which is how
#'   [seg_write_shell()] drops a respondent from a solution.
#'
#'   Breadth is censored: a "single" respondent is single on the contexts
#'   observed. Label deliverables accordingly.
#'
#' @param seg A seg object after [seg_needs_type()].
#' @param decisive_only Logical. Build the cut from decisively typed grids only
#'   (default `FALSE`). Near-tie grids then do not count toward a respondent's
#'   themes, so more respondents fall to unclassified - read
#'   [seg_needs_breadth()] both ways first.
#' @param max_themes Integer. Largest theme combination given its own cells;
#'   anything broader is "Varied" (default `2`).
#' @param min_n Integer. Cells smaller than this are flagged (default `30`).
#'
#' @return The seg object with `seg[["needs"]][["cut"]]`: `persons` (one row
#'   per respondent) and `cells` (the cell key). Prints the cell sizes and a
#'   verdict.
#'
#' @export
seg_needs_cut <- function(seg, decisive_only = FALSE, max_themes = 2, min_n = 30){

  g      <- needs_typed_grids(seg, decisive_only = decisive_only)
  all_g  <- seg[["needs"]][["typing"]][["grids"]]
  tnames <- seg[["needs"]][["typing"]][["theme_names"]]
  K      <- length(tnames)

  if(max_themes < 1) stop("max_themes must be at least 1.", call. = FALSE)

  # nobody can show more themes than they have typed contexts, so combinations
  # beyond that - and a Varied cell above them - would be empty by construction
  max_reach  <- min(K, max(table(g$person_id)))
  max_themes <- min(max_themes, max_reach)


  # ---- the cell key: every theme combination up to max_themes, then Varied --
  combos <- unlist(lapply(seq_len(max_themes), function(m) utils::combn(K, m, simplify = FALSE)),
                   recursive = FALSE)

  type_name <- function(m) if(m <= 4) c("single", "pair", "triple", "quad")[m] else paste0(m, "-theme")

  cells <- dplyr::tibble(
    code   = seq_along(combos),
    key    = vapply(combos, paste, character(1), collapse = ","),
    type   = vapply(combos, function(x) type_name(length(x)), character(1)),
    label  = vapply(combos, function(x){
      if(length(x) == 1) paste(tnames[x], "only") else paste(tnames[x], collapse = " + ")
    }, character(1))
  )

  if(max_reach > max_themes){
    cells <- dplyr::bind_rows(cells, dplyr::tibble(
      code  = nrow(cells) + 1L,
      key   = "varied",
      type  = "varied",
      label = paste0("Varied (", max_themes + 1, "+ themes)")
    ))
  }


  # ---- place each respondent ------------------------------------------------
  per <- g %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      n_typed  = dplyr::n(),
      n_themes = dplyr::n_distinct(.data$theme),
      key      = paste(sort(unique(.data$theme)), collapse = ","),
      .groups = "drop"
    )

  shares <- g %>%
    dplyr::count(.data$person_id, .data$theme) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::mutate(share = .data$n / sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"n") %>%
    tidyr::pivot_wider(names_from = "theme", values_from = "share",
                       names_prefix = "share_theme", values_fill = 0)

  for(k in seq_len(K)){
    if(!paste0("share_theme", k) %in% names(shares)) shares[[paste0("share_theme", k)]] <- 0
  }
  shares <- shares[c("person_id", paste0("share_theme", seq_len(K)))]

  diag <- all_g %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      n_asked    = dplyr::n(),
      n_near_tie = sum(.data$near_tie),
      n_flat     = sum(.data$flat),
      .groups = "drop"
    )

  persons <- diag %>%
    dplyr::left_join(per,    by = "person_id") %>%
    dplyr::left_join(shares, by = "person_id") %>%
    dplyr::mutate(
      n_typed  = dplyr::coalesce(.data$n_typed, 0L),
      n_themes = dplyr::coalesce(.data$n_themes, 0L),
      key      = dplyr::case_when(
        .data$n_typed < 2             ~ NA_character_,
        .data$n_themes > max_themes   ~ "varied",
        .default                      = .data$key
      )
    ) %>%
    dplyr::left_join(cells[c("key", "code", "label", "type")], by = "key") %>%
    dplyr::rename(cell = "code", cell_label = "label", cell_type = "type") %>%
    dplyr::mutate(
      cell_label = dplyr::coalesce(.data$cell_label, "Unclassified (fewer than 2 typed contexts)"),
      cell_type  = dplyr::coalesce(.data$cell_type, "unclassified")
    ) %>%
    dplyr::select(-"key")

  cells$n   <- tabulate(stats::na.omit(persons$cell), nbins = nrow(cells))
  cells$pct <- round(100 * cells$n / sum(!is.na(persons$cell)), 1)
  cells     <- cells[c("code", "type", "label", "n", "pct", "key")]


  # ---- report ---------------------------------------------------------------
  n_class <- sum(!is.na(persons$cell))
  n_uncl  <- sum(is.na(persons$cell))

  cat("\n=== Needs cut: ", nrow(cells), " cells, ", format(n_class, big.mark = ","),
      " respondents classified (",
      if(decisive_only) "decisive grids only" else "all typed grids", ") ===\n\n", sep = "")

  shown <- cells[c("code", "type", "label", "n", "pct")]
  shown$flag <- ifelse(shown$n < min_n, paste0("< ", min_n), "")
  print(as.data.frame(shown), row.names = FALSE)

  by_type <- tapply(cells$n, factor(cells$type, unique(cells$type)), sum)
  cat("\n  ", paste0(names(by_type), " ", round(100 * by_type / n_class, 1), "%", collapse = " | "),
      "\n", sep = "")
  if(n_uncl > 0){
    cat("  unclassified: ", n_uncl, " respondent(s) with fewer than 2 typed contexts",
        " (", sum(persons$n_flat[is.na(persons$cell)] > 0), " of them because of flat grids)\n", sep = "")
  }

  small <- sum(cells$n < min_n)
  cat("\n=== Verdict ===\n")
  if(small == 0){
    cat("  All ", nrow(cells), " cells hold at least ", min_n, " respondents.\n", sep = "")
  } else {
    cat("  ", small, " of ", nrow(cells), " cells hold fewer than ", min_n,
        " respondents - read them as directional,\n  or lower max_themes.\n", sep = "")
  }
  cat("  'single' means one theme across the contexts OBSERVED - breadth is censored.\n\n")

  seg[["needs"]][["cut"]] <- list(
    persons       = persons,
    cells         = cells,
    decisive_only = decisive_only,
    max_themes    = max_themes,
    theme_names   = tnames
  )

  seg
}
