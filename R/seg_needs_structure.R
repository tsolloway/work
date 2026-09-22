#' seg_needs_structure
#'
#' @description Factors the needs and writes the rotation workbook for review,
#'   using the same engine and output format as the rest of the suite
#'   ([pca_analysis()] / [pca_write()]), so the winner is picked the same way as
#'   on a standard segmentation and read back with [seg_get_fa_winner()].
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
#'   \strong{On `model`.} The suite's default is [psych::principal()] with
#'   equamax, and that is kept as the default so a needs project reviews the
#'   same way as any other. Be aware the choice is not cosmetic for theme
#'   assignment: on Kadro, PCA-equamax and FA-oblimin agreed at only ARI 0.61 at
#'   the person level, and they disagreed about whether a focus/motivation theme
#'   exists at all. Run both and pick on interpretability.
#'
#' @param seg A seg object after [seg_needs_prepare()].
#' @param level Character. `"grid"` (default) or `"person"` - which unit to
#'   write the review workbook for.
#' @param model Character. `"pca"` (default, matches the suite) or `"fa"`.
#' @param nfactors Integer vector of factor counts to compare in the congruence
#'   check (default `4`).
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
    model      = c("pca", "fa"),
    nfactors   = 4,
    rotation   = "equamax",
    congruence = TRUE,
    where      = NULL,
    file_name  = "Needs Factor Analysis",
    clean_max  = .25
){

  level <- match.arg(level)
  model <- match.arg(model)

  labs <- seg[["needs"]][["loop"]][["item_labels"]]

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
      if(model == "pca"){
        psych::principal(M, nfactors = nf, rotate = rotation)
      } else {
        psych::fa(M, nfactors = nf, rotate = rotation, fm = "minres")
      }
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
    df       = frames[[level]][["df"]],
    vars     = frames[[level]][["vars"]],
    labels   = label_tbl,
    rotation = rotation
  )

  file_location <- obj %>%
    pca_write(
      where           = where,
      clean_max       = clean_max,
      file_name       = seg_glue_proj_name_to_file(seg, paste(file_name, level)),
      return_location = TRUE
    )

  seg[["paths"]][["files"]][["pca"]] <- file_location

  message("Rotation workbook written: ", file_location)
  message("Pick the winner, then read it back with seg_get_fa_winner().")

  seg
}
