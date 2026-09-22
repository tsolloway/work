#' seg_needs_prepare
#'
#' @description Builds the derived bases on the stacked frame and the person
#'   means that profile them. Writes four families of columns:
#'
#'   \describe{
#'     \item{`need01..`}{the survey answers, untouched - the spec rescales these}
#'     \item{`needc01..`}{grid-centred - each item minus its own grid's mean}
#'     \item{`needs01..`}{item-scaled - each item z-scored across all grids}
#'     \item{`pneed01..`}{the respondent's mean per item, on every one of their rows}
#'     \item{`ptop01..`}{1 where the respondent gave that item the top code in
#'       any context they saw}
#'   }
#'
#'   plus `grid_intensity`, the grid's mean across items.
#'
#'   \strong{Item-scaling is the typing basis and the default.} Without it a
#'   theme built from consensus items wins by default: on the Kadro hearables
#'   data the Frictionless theme took 60% of grids purely because comfort,
#'   secure fit and battery are the highest-rated items in absolute terms.
#'
#'   \strong{Grid-centring cannot change a theme typing.} It subtracts the same
#'   row constant from every theme score, so the ranking is fixed by
#'   construction - verified to floating-point zero. It is off by default. It
#'   earns its place only for distance-based work, where rater generosity would
#'   otherwise be an axis the clusters split on.
#'
#'   \strong{`grid_intensity` is a real variable, not a nuisance.} Rating
#'   generosity stays associated with theme assignment under every
#'   transformation tested (best case eta-squared ~0.08), because intensity and
#'   need-shape are genuinely correlated in the population. Report it rather
#'   than laundering it out of the basis.
#'
#' @param seg A seg object after [seg_needs_stack()].
#' @param scale Character. `"item"` (default) z-scores each item across grids.
#'   `"none"` leaves the raw values.
#' @param center Character. `"none"` (default), or `"grid"` to also grid-centre
#'   before scaling. Only meaningful for the clustering path.
#' @param rescale Character. `"unit"` (default) puts the ANALYSIS quantities -
#'   the centred and scaled bases, the person means, the grid intensity - on
#'   0-1 across all items together. It never rewrites the item columns
#'   themselves: those are the survey answers the spec names as its source, and
#'   the spec does its own rescale via `Value = "unit 1:3"`. Preparing
#'   variables for analysis and preparing them for the client shell are
#'   separate jobs.
#'
#' @return The seg object with the derived columns on
#'   `seg[["data"]][["stacked"]]` and their names in `seg[["needs"]][["vars"]]`.
#'
#' @export
seg_needs_prepare <- function(seg,
                              scale   = c("item", "none"),
                              center  = c("none", "grid"),
                              rescale = c("unit", "none")){

  scale   <- match.arg(scale)
  center  <- match.arg(center)
  rescale <- match.arg(rescale)

  stacked <- seg[["data"]][["stacked"]]
  items   <- seg[["needs"]][["vars"]][["raw"]]

  if(!is.data.frame(stacked)){
    stop("No stacked frame. Run seg_needs_stack() first.", call. = FALSE)
  }

  n <- length(items)
  M <- as.matrix(stacked[items])

  # Map the answer codes onto 0-1 before anything else, so every downstream
  # quantity - the item values, the person means, the grid intensity - is
  # bounded and reads as a proportion of the scale rather than as a raw code.
  # On a 3-point scale that is 1 -> 0, 2 -> 0.5, 3 -> 1.
  #
  # The range is taken across ALL items together, not per item: the 20 needs
  # share one scale, so rescaling each on its own observed range would stretch
  # a need nobody rated at the floor and destroy comparability between them.
  # NEVER write back to the item columns. need01..N are the survey answers and
  # the spec's source column points at them, so mutating them here would mean
  # the spec documents a recode of something other than what it names. The
  # rescale for the SHELL belongs in the spec (Value = "unit 1:3"); this one is
  # for the analysis quantities below and stays inside this function.
  if(rescale == "unit"){
    lo <- min(M, na.rm = TRUE)
    hi <- max(M, na.rm = TRUE)
    if(hi <= lo) stop("Items have no range to rescale.", call. = FALSE)
    M <- rescale_unit(M, c(lo, hi))
    message("Analysis quantities on 0-1 (", lo, "->0 ... ", hi, "->1). ",
            "Item columns keep the raw codes - the spec rescales those itself.")
  }

  row_mu <- rowMeans(M)

  centred <- M - row_mu
  colnames(centred) <- sprintf("needc%02d", seq_len(n))

  basis <- if(center == "grid") centred else M
  basis <- if(scale == "item") base::scale(basis) else basis
  colnames(basis) <- sprintf("needs%02d", seq_len(n))

  stacked <- stacked %>%
    dplyr::mutate(grid_intensity = row_mu) %>%
    dplyr::bind_cols(as.data.frame(centred), as.data.frame(basis))

  # person-level summaries of the loop, carried onto every row. These profile
  # the grids, they never define them. Two families:
  #   pneed*  the respondent's mean per item across the contexts they saw
  #   ptop*   1 if they gave the item the top code in ANY context they saw
  # ptop is what lets a single spec carry both the continuous and the top-box
  # person read, so a needs project needs only one workbook.
  pnames <- sprintf("pneed%02d", seq_len(n))
  tnames <- sprintf("ptop%02d",  seq_len(n))
  top_code <- max(M, na.rm = TRUE)   # 1 after a unit rescale

  # built from M (rescaled), not from the raw item columns
  summ_src <- as.data.frame(M) %>%
    rlang::set_names(items) %>%
    dplyr::mutate(person_id = stacked$person_id)

  person_summary <- summ_src %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(items), mean,                              .names = "mean_{.col}"),
      dplyr::across(dplyr::all_of(items), ~ as.integer(any(.x == top_code)), .names = "top_{.col}"),
      .groups = "drop"
    ) %>%
    rlang::set_names(c("person_id", pnames, tnames))

  stacked <- stacked %>% dplyr::left_join(person_summary, by = "person_id")


  stopifnot(
    max(abs(rowSums(centred))) < 1e-9,
    !anyNA(stacked[colnames(basis)])
  )

  seg[["data"]][["stacked"]] <- stacked

  seg[["needs"]][["vars"]][["centred"]]   <- colnames(centred)
  seg[["needs"]][["vars"]][["scaled"]]    <- colnames(basis)
  seg[["needs"]][["vars"]][["person"]]    <- pnames
  seg[["needs"]][["vars"]][["person_top"]] <- tnames
  seg[["needs"]][["vars"]][["intensity"]] <- "grid_intensity"

  message(
    "Basis prepared (rescale = ", rescale, ", center = ", center,
    ", scale = ", scale, "). Items: ", items[1], "..", items[n],
    if(scale != "none" || center != "none")
      paste0(" | transformed copy: ", colnames(basis)[1], "..", colnames(basis)[n]) else ""
  )

  seg
}
