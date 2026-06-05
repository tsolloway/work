#' resondex_assets_sync
#' @description
#' Copy the latest brand SVG sources from a local checkout of the
#' \code{resondex/brand-assets} repo into this package's
#' \code{inst/brand-assets/} tree. Use after editing files in brand-assets
#' to refresh what \code{\link{resondex_assets}} returns — then commit the
#' diff in work in the usual way.
#'
#' The full source directory structure is preserved, so identical filenames
#' in different folders (e.g. \code{icon/analyses/previews/network.svg}
#' vs. \code{icon/analyses/line/network.svg}) are handled correctly by
#' construction — each lands at its own relative path. SVGs only — skips
#' \code{index.html}, README, \code{.command} launchers, \code{.git/}, etc.
#'
#' Compares files by MD5 so identical-content files aren't needlessly
#' recopied (cleaner git diffs). On any change, clears the rasterizer's
#' session cache (\code{tempdir()/resondex-assets/}) so the next
#' \code{resondex_assets(..., "png")} call rebuilds from the new SVG.
#'
#' Safe by default: never deletes files in \code{dest} that aren't in
#' \code{source} — if you remove an asset from brand-assets, delete it
#' from \code{inst/brand-assets/} by hand so the intent is explicit in
#' the work repo's commit history.
#'
#' @param source Path to a local checkout of the brand-assets repo.
#'   Defaults to \code{~/Documents/GitHub/brand-assets}.
#' @param dest Path to work's \code{inst/brand-assets/} in a source checkout
#'   (the one git tracks, not the installed copy). Defaults to
#'   \code{~/Documents/GitHub/work/inst/brand-assets}.
#' @param export_png Also write a parallel \code{<source>/png/} mirror by
#'   rasterizing every \strong{curated catalog} asset (i.e. blessed
#'   deliverables, not exploration sketches). Each PNG lives at
#'   \code{<source>/png/<same-relative-path>.png}. SVGs are still the
#'   masters — you edit those; PNGs are derived. Skipped silently if
#'   \pkg{rsvg} isn't installed. Default \code{TRUE}.
#' @param min_png_width Floor for PNG width when \code{export_png = TRUE}.
#'   The actual width per asset is \code{max(svg_intrinsic_width,
#'   min_png_width)} — so a 64\eqn{\times}64 mark becomes a 512\eqn{\times}512
#'   PNG (useful for slides/docs), while a 1200\eqn{\times}630 OG card
#'   stays at its native 1200. Default 512.
#' @param quiet Suppress the printed summary.
#' @return Invisibly: a list with three character vectors of relative paths
#'   (\code{added}, \code{updated}, \code{identical}) plus the resolved
#'   \code{source} and \code{dest}; when \code{export_png = TRUE} a
#'   \code{png} sub-list with parallel \code{added}/\code{updated}/\code{identical}
#'   vectors for the PNG mirror.
#' @export
resondex_assets_sync <- function(
    source        = "~/Documents/GitHub/brand-assets",
    dest          = "~/Documents/GitHub/work/inst/brand-assets",
    export_png    = TRUE,
    min_png_width = 512L,
    quiet         = FALSE) {

  source <- normalizePath(source, mustWork = TRUE)
  dest   <- normalizePath(dest,   mustWork = TRUE)

  # Every SVG in the brand-assets source. Recursive so the analyses/
  # previews/ + line/ subtrees come along; the relative-path preservation
  # below is what makes identical filenames-in-different-folders safe.
  src_rel <- list.files(source, pattern = "\\.svg$",
                        recursive = TRUE, full.names = FALSE)

  added <- character()
  updated <- character()
  identical_files <- character()

  for (rel in src_rel) {
    s <- file.path(source, rel)
    d <- file.path(dest,   rel)
    dst_dir <- dirname(d)
    if (!dir.exists(dst_dir)) {
      dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!file.exists(d)) {
      file.copy(s, d, overwrite = FALSE)
      added <- c(added, rel)
    } else if (!.resondex_assets_same(s, d)) {
      file.copy(s, d, overwrite = TRUE)
      updated <- c(updated, rel)
    } else {
      identical_files <- c(identical_files, rel)
    }
  }

  changes <- c(added, updated)

  # On any change, drop the in-session PNG cache so stale rasters don't
  # get served by the next resondex_assets(..., "png") call.
  if (length(changes) > 0L) {
    cache <- file.path(tempdir(), "resondex-assets")
    if (dir.exists(cache)) {
      cached <- list.files(cache, full.names = TRUE)
      if (length(cached) > 0L) file.remove(cached)
    }
  }

  # ---- PNG mirror under <source>/png/ ----
  # For every curated catalog asset, write a PNG sibling at the same
  # relative path under <source>/png/. SVGs stay the masters; PNGs are
  # derived deliverables for non-R consumers (LinkedIn uploads, OG meta
  # tags, drag-into-slide). Re-rasterizes only when the SVG is newer than
  # the PNG, so repeat sync runs are cheap.
  png_out <- list(added = character(),
                  updated = character(),
                  identical = character())
  png_skipped <- FALSE
  if (isTRUE(export_png)) {
    if (!requireNamespace("rsvg", quietly = TRUE)) {
      png_skipped <- TRUE
    } else {
      png_out <- .resondex_assets_export_png_mirror(source, min_png_width)
    }
  }

  if (!isTRUE(quiet)) {
    cat("Sync: ", source, " -> ", dest, "\n", sep = "")
    cat("  added:     ", length(added), "\n", sep = "")
    if (length(added))   cat("    ", paste(added,   collapse = "\n    "), "\n", sep = "")
    cat("  updated:   ", length(updated), "\n", sep = "")
    if (length(updated)) cat("    ", paste(updated, collapse = "\n    "), "\n", sep = "")
    cat("  identical: ", length(identical_files), "\n", sep = "")
    if (length(changes)) cat("Cleared PNG cache (tempdir()/resondex-assets/).\n")
    if (isTRUE(export_png)) {
      if (png_skipped) {
        cat("PNG mirror: skipped (install rsvg to enable).\n")
      } else {
        cat("PNG mirror -> ", file.path(source, "png"), "\n", sep = "")
        cat("  added:     ", length(png_out$added), "\n", sep = "")
        if (length(png_out$added))
          cat("    ", paste(png_out$added, collapse = "\n    "), "\n", sep = "")
        cat("  updated:   ", length(png_out$updated), "\n", sep = "")
        if (length(png_out$updated))
          cat("    ", paste(png_out$updated, collapse = "\n    "), "\n", sep = "")
        cat("  identical: ", length(png_out$identical), "\n", sep = "")
      }
    }
  }

  invisible(list(
    added = added, updated = updated, identical = identical_files,
    png = png_out,
    source = source, dest = dest
  ))
}

# Byte-equivalence via MD5. Fast for the brand-asset sizes (a few KB to
# tens of KB) and avoids needlessly rewriting files (= cleaner work git diffs).
.resondex_assets_same <- function(a, b) {
  unname(tools::md5sum(a)) == unname(tools::md5sum(b))
}

# For every curated catalog asset, rasterize its SVG to a sibling PNG
# under <source>/png/. SVG-newer-than-PNG → re-rasterize; otherwise skip.
# Uses each SVG's intrinsic width, floored at min_width so small marks
# come out at a usable size (e.g. 64 → 512).
.resondex_assets_export_png_mirror <- function(source, min_width) {
  catalog <- .resondex_assets_catalog()
  png_root <- file.path(source, "png")

  added    <- character()
  updated  <- character()
  identical_files <- character()

  for (key in names(catalog)) {
    svg_rel  <- unname(catalog[key])
    svg_path <- file.path(source, svg_rel)
    if (!file.exists(svg_path)) next  # missing master SVG; skip silently

    png_rel  <- sub("\\.svg$", ".png", svg_rel)
    png_path <- file.path(png_root, png_rel)
    png_dir  <- dirname(png_path)
    if (!dir.exists(png_dir)) {
      dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)
    }

    width <- max(.resondex_assets_svg_width(svg_path), as.integer(min_width))

    if (!file.exists(png_path)) {
      rsvg::rsvg_png(svg_path, file = png_path, width = width)
      added <- c(added, png_rel)
    } else if (file.info(svg_path)$mtime > file.info(png_path)$mtime) {
      # SVG was modified after the PNG was generated -> regenerate.
      rsvg::rsvg_png(svg_path, file = png_path, width = width)
      updated <- c(updated, png_rel)
    } else {
      identical_files <- c(identical_files, png_rel)
    }
  }
  list(added = added, updated = updated, identical = identical_files)
}
