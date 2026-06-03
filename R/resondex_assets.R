#' resondex_assets
#' @description
#' Fetch a Resondex brand asset (mark, logo, lockup, favicon, icon, social
#' card, or analyses preview/line icon) for use in Shiny apps, reports,
#' rmarkdown, or any HTML output. SVG masters are shipped inside the work
#' package at \code{inst/brand-assets/} so they resolve anywhere work is
#' installed — no external dependency on the brand-assets repo at runtime.
#'
#' Companion to \code{\link{resondex_brand}} (the design tokens) — those
#' tokens dictate the surface; these are the brand marks placed on it.
#'
#' Call with no arguments to list every available key. Pass an unknown key
#' and the error message echoes the catalog so discovery stays cheap.
#'
#' @param name Asset key. Two-tier resolution: (1) the curated catalog of
#'   stable, short keys listed below; (2) for any SVG synced into
#'   \code{inst/brand-assets/} that isn't yet catalogued (e.g. exploration
#'   sketches, in-progress favicon variants), the relative path inside that
#'   tree — with or without the \code{.svg} extension — works too. So
#'   \code{resondex_assets("mark")} (curated) and
#'   \code{resondex_assets("icon/resondex-favicon-bullseye-dark")}
#'   (path-fallback) both resolve. Call \code{resondex_assets()} with no
#'   arguments to list every callable key (curated first, then extras).
#'   Examples of curated keys:
#'   \itemize{
#'     \item Marks: \code{"mark"}, \code{"mark-dark"}, \code{"mark-mono"},
#'           \code{"mark-white"}, \code{"mark-black"}
#'     \item Lockups: \code{"logo-horizontal"}, \code{"logo-horizontal-dark"},
#'           \code{"logo-stacked"}, \code{"logo-stacked-dark"}, \code{"wordmark"}
#'     \item Icons: \code{"favicon"}, \code{"favicon-16"}, \code{"icon-tile"},
#'           \code{"app-icon"}, \code{"icon-expressive"}
#'     \item Analyses previews (rich): \code{"analyses/network"},
#'           \code{"analyses/network2"}, \code{"analyses/segment"},
#'           \code{"analyses/turf"}, \code{"analyses/dashboard"},
#'           \code{"analyses/modeling"}, \code{"analyses/scoring"},
#'           \code{"analyses/linkage"}, \code{"analyses/tracker"},
#'           \code{"analyses/foundation"}
#'     \item Analyses line icons: same keys with \code{-line} suffix, e.g.
#'           \code{"analyses/network-line"}
#'     \item Social: \code{"og"}, \code{"avatar"}
#'   }
#' @param format Return format:
#'   \itemize{
#'     \item \code{"svg"} (default) — raw inline SVG markup (a character
#'           scalar). Drop into \code{htmltools::HTML()}, \code{shiny::HTML()},
#'           an inline R expression in a Quarto/Rmd chunk with
#'           \code{results = "asis"}, or any string-templating context.
#'     \item \code{"html"} — the SVG wrapped in \code{htmltools::HTML()} so
#'           it slots straight into a tag tree
#'           (e.g. \code{div(resondex_assets("mark", "html"))}).
#'     \item \code{"path"} — absolute file path to the SVG on disk. Useful
#'           for \code{img(src = …)} via Shiny resource paths, for embedding
#'           in openxlsx2 workbooks via \code{wb_add_image}, or for copying
#'           the master out of the package.
#'     \item \code{"png"} — rasterizes the SVG to a PNG via \pkg{rsvg} and
#'           returns the absolute path to the cached file. Use for things
#'           SVG can't reliably do: OG \code{og:image} meta tags, email
#'           signatures, Slack/Teams uploads, social uploads, embedded
#'           images in \code{openxlsx} reports. Width is set by \code{width};
#'           PNGs are cached in \code{tempdir()/resondex-assets/} for the R
#'           session so repeat calls with the same \code{(name, width)} are
#'           free. Requires the \pkg{rsvg} package.
#'   }
#' @param width Pixel width for \code{format = "png"}. Defaults to the SVG's
#'   intrinsic width when not set (e.g. 64 for the mark, 1200 for the OG
#'   card). Ignored by every other format. Height follows the SVG's aspect
#'   ratio automatically.
#' @return One of: the asset (character SVG, \code{htmltools::HTML}, or
#'   file path) when \code{name} is supplied, or a character vector of all
#'   available keys when \code{name} is NULL.
#'
#' @examples
#' \dontrun{
#'   resondex_assets()                                 # list every key
#'   resondex_assets("mark")                           # inline SVG string
#'   resondex_assets("mark-dark", format = "html")     # htmltools::HTML
#'   resondex_assets("favicon", format = "path")       # /path/to/...svg
#'   resondex_assets("og", format = "png")             # /tmp/.../og-1200.png
#'   resondex_assets("favicon", "png", width = 32)     # 32px PNG for ICO
#'
#'   # Shiny app header
#'   htmltools::div(
#'     class = "app-header",
#'     resondex_assets("logo-horizontal", "html"),
#'     htmltools::tags$h1("Network Drivers")
#'   )
#'
#'   # Quarto / Rmd inline (chunk option `results = "asis"`)
#'   cat(resondex_assets("analyses/network"))
#'
#'   # Copy a rasterized OG card into a website's public folder
#'   file.copy(resondex_assets("og", "png"), "public/og.png", overwrite = TRUE)
#' }
#' @export
resondex_assets <- function(name = NULL,
                            format = c("svg", "html", "path", "png"),
                            width = NULL) {
  format <- match.arg(format)

  # ---- Catalog ----
  # Explicit key → relative path under inst/brand-assets/ so the public
  # keys are stable even if the on-disk filenames change. Edit here when
  # adding a new asset to the kit.
  catalog <- c(
    # Marks — the canonical 4-colour glyph in every finish.
    "mark"                  = "logo/resondex-mark.svg",
    "mark-dark"             = "logo/resondex-mark-dark.svg",
    "mark-mono"             = "logo/resondex-mark-mono.svg",
    "mark-white"            = "logo/resondex-mark-white.svg",
    "mark-black"            = "logo/resondex-mark-black.svg",
    # Lockups & wordmark.
    "logo-horizontal"       = "logo/resondex-logo-horizontal.svg",
    "logo-horizontal-dark"  = "logo/resondex-logo-horizontal-dark.svg",
    "logo-stacked"          = "logo/resondex-logo-stacked.svg",
    "logo-stacked-dark"     = "logo/resondex-logo-stacked-dark.svg",
    "wordmark"              = "logo/resondex-wordmark.svg",
    # Icons (favicon family + app/tile/expressive).
    "favicon"               = "icon/resondex-favicon.svg",
    "favicon-16"            = "icon/resondex-favicon-16.svg",
    "icon-tile"             = "icon/resondex-icon-tile.svg",
    "app-icon"              = "icon/resondex-app-icon.svg",
    "icon-expressive"       = "icon/resondex-icon-expressive.svg",
    # Analyses — productized preview illustrations (rich, brand-coloured).
    "analyses/network"      = "icon/analyses/previews/network.svg",
    "analyses/network2"     = "icon/analyses/previews/network2.svg",
    "analyses/segment"      = "icon/analyses/previews/segment.svg",
    "analyses/turf"         = "icon/analyses/previews/turf.svg",
    "analyses/dashboard"    = "icon/analyses/previews/dashboard.svg",
    "analyses/modeling"     = "icon/analyses/previews/modeling.svg",
    "analyses/scoring"      = "icon/analyses/previews/scoring.svg",
    "analyses/linkage"      = "icon/analyses/previews/linkage.svg",
    "analyses/tracker"      = "icon/analyses/previews/tracker.svg",
    "analyses/foundation"   = "icon/analyses/previews/foundation.svg",
    # Analyses — uniform line icon set (24×24, accent grey).
    "analyses/network-line"    = "icon/analyses/line/network.svg",
    "analyses/segment-line"    = "icon/analyses/line/segment.svg",
    "analyses/turf-line"       = "icon/analyses/line/turf.svg",
    "analyses/dashboard-line"  = "icon/analyses/line/dashboard.svg",
    "analyses/modeling-line"   = "icon/analyses/line/modeling.svg",
    "analyses/scoring-line"    = "icon/analyses/line/scoring.svg",
    "analyses/linkage-line"    = "icon/analyses/line/linkage.svg",
    "analyses/tracker-line"    = "icon/analyses/line/tracker.svg",
    "analyses/foundation-line" = "icon/analyses/line/foundation.svg",
    # Social.
    "og"     = "social/resondex-og.svg",
    "avatar" = "social/resondex-avatar.svg"
  )

  root <- system.file("brand-assets", package = "work")

  if (is.null(name)) {
    # List every callable key: curated catalog first (in the stable order
    # above), then any extras present on disk under inst/brand-assets/ that
    # the catalog hasn't yet adopted (synced files like exploration sketches,
    # in-progress favicons, etc.). Extras are listed as their relative path
    # minus the .svg extension so they read like keys.
    extras <- character()
    if (nzchar(root) && dir.exists(root)) {
      on_disk <- list.files(root, pattern = "\\.svg$", recursive = TRUE)
      catalog_paths <- unname(catalog)
      extras <- sub("\\.svg$", "",
                    setdiff(on_disk, catalog_paths))
    }
    return(c(names(catalog), extras))
  }

  if (length(name) != 1L || !is.character(name)) {
    stop("`name` must be a single character key. ",
         "Call resondex_assets() to list available keys.", call. = FALSE)
  }

  # Resolution order: (1) curated catalog key, (2) path-fallback for any
  # synced SVG under inst/brand-assets/. The fallback accepts the relative
  # path with or without the .svg suffix.
  path <- if (name %in% names(catalog)) {
    file.path(root, unname(catalog[name]))
  } else {
    .resondex_assets_resolve_path(name, root)
  }

  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop("Unknown Resondex brand asset: '", name, "'.\n",
         "Call resondex_assets() to list available keys.", call. = FALSE)
  }

  switch(
    format,
    path = path,
    svg  = paste(readLines(path, warn = FALSE), collapse = "\n"),
    html = htmltools::HTML(paste(readLines(path, warn = FALSE), collapse = "\n")),
    png  = .resondex_assets_rasterize(path, name, width)
  )
}

# Resolve a non-catalogued key as a relative path under inst/brand-assets/.
# Accepts both "icon/resondex-favicon-bullseye-dark.svg" and the same
# without the extension. Returns NULL if neither form lands on a real file,
# so the caller can surface a clean "unknown asset" error. Guards against
# path traversal — anything containing ".." is rejected up front.
.resondex_assets_resolve_path <- function(name, root) {
  if (grepl("\\.\\.", name, fixed = FALSE)) return(NULL)
  candidates <- c(name, paste0(name, ".svg"))
  for (rel in candidates) {
    p <- file.path(root, rel)
    if (file.exists(p) && !dir.exists(p)) return(p)
  }
  NULL
}

# Rasterize an SVG asset to a cached PNG.
#
# Cache lives in tempdir()/resondex-assets/ for the R session. Keyed on the
# asset key + width, so repeated calls (e.g. across screens / chunks) reuse
# the rasterization. Width defaults to the SVG's intrinsic width attribute
# (so the OG card comes out at 1200×630, the mark at 64×64, etc.); pass
# `width` to override (e.g. 32 for a small favicon, 1200 for a larger mark).
.resondex_assets_rasterize <- function(svg_path, name, width = NULL) {
  if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop("PNG output requires the 'rsvg' package. ",
         "Install with install.packages('rsvg').", call. = FALSE)
  }
  if (is.null(width)) width <- .resondex_assets_svg_width(svg_path)

  cache_dir <- file.path(tempdir(), "resondex-assets")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  # name may contain "/" (e.g. "analyses/network") — flatten for the cache.
  safe <- gsub("/", "__", name, fixed = TRUE)
  png_path <- file.path(cache_dir, sprintf("%s-%d.png", safe, width))

  if (!file.exists(png_path)) {
    rsvg::rsvg_png(svg_path, file = png_path, width = width)
  }
  png_path
}

# Pull the SVG's intrinsic width from its <svg width="..."> attribute. Falls
# back to the viewBox's width, then to 512 if neither is parseable — never
# fails (a sensible default beats erroring on a hand-edited SVG).
.resondex_assets_svg_width <- function(svg_path) {
  head_txt <- paste(readLines(svg_path, n = 5, warn = FALSE), collapse = " ")
  m <- regmatches(head_txt, regexpr('width="\\d+', head_txt))
  if (length(m) > 0) {
    n <- suppressWarnings(as.integer(sub('width="', "", m, fixed = TRUE)))
    if (!is.na(n) && n > 0) return(n)
  }
  m <- regmatches(head_txt,
                  regexpr('viewBox="\\s*-?\\d+\\s+-?\\d+\\s+\\d+\\s+\\d+', head_txt))
  if (length(m) > 0) {
    nums <- as.integer(strsplit(sub('viewBox="\\s*', "", m), "\\s+")[[1]])
    if (length(nums) >= 3L && !is.na(nums[3L]) && nums[3L] > 0L) return(nums[3L])
  }
  512L
}
