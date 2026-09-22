#' seg_needs_prepare
#'
#' @description Builds the derived bases on the stacked frame and the person
#'   means that profile them. Writes four families of columns:
#'
#'   \describe{
#'     \item{`need01..`}{as answered (already present from the stack)}
#'     \item{`needc01..`}{grid-centred - each item minus its own grid's mean}
#'     \item{`needs01..`}{item-scaled - each item z-scored across all grids}
#'     \item{`pneed01..`}{the respondent's mean per item, on every one of their rows}
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
#'
#' @return The seg object with the derived columns on
#'   `seg[["data"]][["stacked"]]` and their names in `seg[["needs"]][["vars"]]`.
#'
#' @export
seg_needs_prepare <- function(seg, scale = c("item", "none"), center = c("none", "grid")){

  scale  <- match.arg(scale)
  center <- match.arg(center)

  stacked <- seg[["data"]][["stacked"]]
  items   <- seg[["needs"]][["vars"]][["raw"]]

  if(!is.data.frame(stacked)){
    stop("No stacked frame. Run seg_needs_stack() first.", call. = FALSE)
  }

  n       <- length(items)
  M       <- as.matrix(stacked[items])
  row_mu  <- rowMeans(M)

  centred <- M - row_mu
  colnames(centred) <- sprintf("needc%02d", seq_len(n))

  basis <- if(center == "grid") centred else M
  basis <- if(scale == "item") base::scale(basis) else basis
  colnames(basis) <- sprintf("needs%02d", seq_len(n))

  stacked <- stacked %>%
    dplyr::mutate(grid_intensity = row_mu) %>%
    dplyr::bind_cols(as.data.frame(centred), as.data.frame(basis))

  # person mean per item, carried onto every row - profiles the grids, never
  # defines them
  pnames <- sprintf("pneed%02d", seq_len(n))

  person_mean <- stacked %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(items), mean), .groups = "drop") %>%
    rlang::set_names(c("person_id", pnames))

  stacked <- stacked %>% dplyr::left_join(person_mean, by = "person_id")


  stopifnot(
    max(abs(rowSums(centred))) < 1e-9,
    !anyNA(stacked[colnames(basis)])
  )

  seg[["data"]][["stacked"]] <- stacked

  seg[["needs"]][["vars"]][["centred"]]   <- colnames(centred)
  seg[["needs"]][["vars"]][["scaled"]]    <- colnames(basis)
  seg[["needs"]][["vars"]][["person"]]    <- pnames
  seg[["needs"]][["vars"]][["intensity"]] <- "grid_intensity"

  message(
    "Basis prepared (center = ", center, ", scale = ", scale, "). ",
    "Typing basis: ", colnames(basis)[1], "..", colnames(basis)[n]
  )

  seg
}
