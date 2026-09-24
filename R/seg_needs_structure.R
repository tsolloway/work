#' seg_needs_structure
#'
#' @description Factors the needs and writes the rotation workbook for review,
#'   using the same engine and output format as the rest of the suite
#'   ([pca_analysis()] / [pca_write()]), so the winner is picked the same way as
#'   on a standard segmentation. It is read back with [seg_needs_themes()]
#'   (`winner =`); [seg_get_fa_winner()] is the polar path.
#'
#'   Two things it adds over calling [seg_pca()] directly.
#'
#'   First, [seg_pca()] is hard-wired to the polars: it takes its labels from
#'   `seg$spec$polars_table` and its data from `seg$data$with_shell`. A needs
#'   basis has neither, so this passes the needs frame and needs labels into the
#'   same underlying engine instead.
#'
#'   Second, it runs the structure at BOTH units and reports Tucker congruence
#'   between them. If the grid-level and person-level structures are the same
#'   factors, the themes are a property of the needs rather than an artifact of
#'   how the loop was aggregated, and either unit can be used downstream. On the
#'   Kadro hearables data congruence was 0.98-1.00 across all four factors.
#'
#'   \strong{PCA only.} Both the workbook and the congruence check run
#'   [psych::principal()], the suite's engine, so what is reviewed and what is
#'   compared are the same model. The choice is not cosmetic: on Kadro,
#'   PCA-equamax and FA-oblimin agreed at only ARI 0.61 at the person level and
#'   disagreed about whether a focus/motivation theme exists at all - so the
#'   rotation is worth reviewing, and a factor that only appears under one
#'   rotation is a fragile one.
#'
#'   \strong{Only the requested solutions are written.} `nfactors` sets both
#'   the congruence check and the sheets in the workbook, so the review covers
#'   exactly the solutions in contention.
#'
#' @param seg A seg object after [seg_needs_prepare()].
#' @param level Character. `"grid"` (default) or `"person"` - which unit to
#'   write the review workbook for.
#' @param nfactors Integer vector of factor counts (default `4`). Each gets a
#'   sheet in the workbook and a line in the congruence check. Must lie between
#'   3 and the number of needs minus 2 - the range [pca_analysis()] fits.
#' @param rotation Rotation passed through to the engine.
#' @param congruence Logical. Also run the other unit and report Tucker
#'   congruence (default `TRUE`).
#' @param where,file_name,clean_max Passed to [pca_write()].
#'
#' @return The seg object with the workbook path in
#'   `seg[["paths"]][["files"]][["pca"]]`.
#'
#' @export
seg_needs_structure <- function(
    seg,
    level      = c("grid", "person"),
    nfactors   = 4,
    rotation   = "equamax",
    congruence = TRUE,
    where      = NULL,
    file_name  = "Needs Factor Analysis",
    clean_max  = .25
){

  level <- match.arg(level)

  labs <- seg[["needs"]][["loop"]][["item_labels"]]

  # pca_analysis() fits 3 .. (items - 2); anything outside that has no sheet
  bad <- nfactors[nfactors < 3 | nfactors > length(labs) - 2]
  if(length(bad)){
    stop("nfactors must lie between 3 and ", length(labs) - 2, " for ", length(labs),
         " needs; got ", paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  nfactors <- sort(unique(as.integer(nfactors)))

  frames <- list(
    grid   = list(df = seg[["data"]][["stacked"]], vars = seg[["needs"]][["vars"]][["raw"]]),
    person = list(
      df   = dplyr::distinct(seg[["data"]][["stacked"]], .data$person_id, .keep_all = TRUE),
      vars = seg[["needs"]][["vars"]][["person"]]
    )
  )

  if(is.null(where)){
    where <- seg[["paths"]][["folders"]][["process"]]
    if(is.null(where) || is.na(where)) where <- getwd()
  }

  label_tbl <- dplyr::tibble(
    variable = frames[[level]][["vars"]],
    label    = labs
  )


  # ---- congruence across units -------------------------------------------
  if(isTRUE(congruence)){

    fit <- function(which_level, nf){
      M <- as.matrix(frames[[which_level]][["df"]][frames[[which_level]][["vars"]]])
      colnames(M) <- labs
      psych::principal(M, nfactors = nf, rotate = rotation)
    }

    cat("\n=== Structure congruence: grid level vs person level ===\n")
    for(nf in nfactors){
      cg <- psych::factor.congruence(fit("grid", nf), fit("person", nf))
      best <- apply(abs(cg), 1, max)
      cat("  ", nf, " factors: best match per factor = ",
          paste(sprintf("%.2f", sort(best, decreasing = TRUE)), collapse = " "), "\n", sep = "")
      if(min(best) >= 0.95){
        cat("    -> same factors at both units; either can be used downstream\n")
      } else if(min(best) >= 0.85){
        cat("    -> fair similarity; check the weakest factor before relying on it\n")
      } else {
        cat("    -> structures DIFFER by unit; the themes are aggregation-dependent\n")
      }
    }
    cat("\n")
  }


  # ---- the suite's own review workbook ------------------------------------
  obj <- pca_analysis(
    df          = frames[[level]][["df"]],
    vars        = frames[[level]][["vars"]],
    labels      = label_tbl,
    rotation    = rotation,
    max_factors = max(nfactors)
  )

  # Keep only the requested solutions. Subset AFTER the fit rather than asking
  # for fewer: the variance sheet's proportion_var is each solution's cumulative
  # variance minus the one below it, so dropping solution 3 before the fit would
  # hand solution 4 its whole cumulative share instead of its increment.
  keep <- as.character(nfactors)
  obj[["pca_tables"]]         <- obj[["pca_tables"]][keep]
  obj[["variance_explained"]] <- obj[["variance_explained"]][
    obj[["variance_explained"]][["solution"]] %in% paste("Solution", keep), , drop = FALSE]

  file_location <- obj %>%
    pca_write(
      where           = where,
      clean_max       = clean_max,
      file_name       = seg_glue_proj_name_to_file(seg, paste(file_name, level)),
      return_location = TRUE
    )

  seg[["paths"]][["files"]][["pca"]] <- file_location

  message("Rotation workbook written (", paste(nfactors, collapse = ", "), " factors): ", file_location)
  message("Pick the winner, then read it with seg_needs_themes(winner = ).")

  seg
}
