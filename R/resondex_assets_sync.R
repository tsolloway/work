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
#' @param quiet Suppress the printed summary.
#' @return Invisibly: a list with three character vectors of relative paths
#'   (\code{added}, \code{updated}, \code{identical}) plus the resolved
#'   \code{source} and \code{dest}.
#' @export
resondex_assets_sync <- function(
    source = "~/Documents/GitHub/brand-assets",
    dest   = "~/Documents/GitHub/work/inst/brand-assets",
    quiet  = FALSE) {

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

  if (!isTRUE(quiet)) {
    cat("Sync: ", source, " -> ", dest, "\n", sep = "")
    cat("  added:     ", length(added), "\n", sep = "")
    if (length(added))   cat("    ", paste(added,   collapse = "\n    "), "\n", sep = "")
    cat("  updated:   ", length(updated), "\n", sep = "")
    if (length(updated)) cat("    ", paste(updated, collapse = "\n    "), "\n", sep = "")
    cat("  identical: ", length(identical_files), "\n", sep = "")
    if (length(changes)) cat("Cleared PNG cache (tempdir()/resondex-assets/).\n")
  }

  invisible(list(
    added = added, updated = updated, identical = identical_files,
    source = source, dest = dest
  ))
}

# Byte-equivalence via MD5. Fast for the brand-asset sizes (a few KB to
# tens of KB) and avoids needlessly rewriting files (= cleaner work git diffs).
.resondex_assets_same <- function(a, b) {
  unname(tools::md5sum(a)) == unname(tools::md5sum(b))
}
