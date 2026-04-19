#' append_bn_network_maps
#'
#' @description Internal helper that renders Bayesian network maps as PNG
#'   images and inserts them into an Excel workbook on separate sheets. Uses
#'   \code{bn_visual(save_to_png = TRUE)} to capture visNetwork widgets.
#'
#' @param wb An openxlsx workbook object.
#' @param bn_full The full output of \code{bn_finalize_network()}.
#' @param network_type Character. Layout type passed to \code{bn_visual()}.
#'   One of \code{"none"} (default), \code{"gravity"}, \code{"charge"}, or
#'   \code{"hierarchy"}.
#' @param width Numeric. Image width in inches. Default 10.
#' @param height Numeric. Image height in inches. Default 8.
#' @param defer_images Logical. When FALSE (default), images are inserted
#'   directly via \code{openxlsx::insertImage}. When TRUE, the sheets are
#'   created and PNGs are rendered to a temp dir, but images are NOT
#'   inserted — metadata is attached to the workbook under attribute
#'   \code{"network_map_images"} so the caller can insert via openxlsx2
#'   after a save/reload. Used by \code{bn_write()}.
#' @param attribute_font_size Numeric or NULL. Node-label font size (in pixels)
#'   for the Attribute Network PNG. NULL leaves vis.js's default 14 px.
#' @param community_font_size Numeric or NULL. Node-label font size (in pixels)
#'   for the Community Network PNG. NULL leaves vis.js's default 14 px.
#'
#' @return The modified workbook object (invisibly).
#'
#' @keywords internal
append_bn_network_maps <- function(
    wb,
    bn_full,
    network_type = "none",
    width = 10,
    height = 8,
    defer_images = FALSE,
    attribute_font_size = NULL,
    community_font_size = NULL
) {

  if (!requireNamespace("webshot2", quietly = TRUE)) {
    warning("webshot2 package required for network maps. Install with install.packages('webshot2')")
    return(invisible(wb))
  }

  px_width <- round(width * 96)
  px_height <- round(height * 96)

  tmp_dir <- tempfile("bn_maps_")
  dir.create(tmp_dir, recursive = TRUE)
  existing_tmp <- attr(wb, "tmp_dirs") %||% character(0)
  attr(wb, "tmp_dirs") <- c(existing_tmp, tmp_dir)

  title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 18)

  # Parallel-safe: render ONE PNG, return its path (or NA on failure).
  # No workbook mutation — safe to run in a future worker.
  .render_map_png <- function(sheet_name, do_community, font_size) {
    if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)
    png_base <- file.path(tmp_dir, gsub(" ", "_", sheet_name))
    err <- NULL
    tryCatch({
      bn_visual(
        obj = bn_full[["bn"]],
        type = network_type,
        do_community = do_community,
        physics = TRUE,
        interactive = FALSE,
        save_to_png = TRUE,
        save_file_name = png_base,
        png_width = px_width,
        png_height = px_height,
        save_visuals = FALSE,
        font_size = font_size
      )
    }, error = function(e) {
      err <<- conditionMessage(e)
    })
    list(
      sheet_name = sheet_name,
      png_path   = paste0(png_base, ".png"),
      error      = err
    )
  }

  # Serial: attach a rendered PNG to the workbook (creates the sheet,
  # writes the title, and either inserts or defers the image).
  .attach_map_sheet <- function(wb, rendered) {
    sheet_name <- rendered$sheet_name
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::addStyle(wb, sheet_name,
      style = openxlsx::createStyle(fgFill = "#FFFFFF"),
      rows = 1:200, cols = 1:50, gridExpand = TRUE, stack = TRUE)

    if (!is.null(rendered$error)) {
      warning(sheet_name, " render failed: ", rendered$error)
    }

    png_path <- rendered$png_path
    if (file.exists(png_path) && file.info(png_path)$size > 1000) {
      openxlsx::writeData(wb, sheet_name, sheet_name,
        startRow = 2, startCol = 2)
      openxlsx::addStyle(wb, sheet_name, style = title_style,
        rows = 2, cols = 2)

      if (isTRUE(defer_images)) {
        existing <- attr(wb, "network_map_images") %||% list()
        existing[[length(existing) + 1L]] <- list(
          sheet = sheet_name,
          file  = png_path,
          start_row = 3,
          start_col = 2,
          width  = width,
          height = height
        )
        attr(wb, "network_map_images") <- existing
      } else {
        openxlsx::insertImage(wb, sheet_name, file = png_path,
          startRow = 3, startCol = 2,
          width = width, height = height, units = "in")
      }
    } else {
      openxlsx::writeData(wb, sheet_name,
        paste(sheet_name, "could not be rendered."),
        startRow = 3, startCol = 2)
    }

    wb
  }

  # --- Render both PNGs concurrently -----------------------------------------
  # Each render is a webshot2 headless-Chrome session (~10-30 s). With two
  # maps to render, parallelizing cuts this stage ~in half.
  #
  # Use whatever future plan the caller has set up. If they're on the default
  # sequential plan, spin up a short-lived 2-worker multisession just for
  # this step and restore the old plan afterwards, so users who haven't
  # thought about parallelism still get the speedup transparently.
  render_specs <- list(
    list(sheet_name = "Attribute Network",
         do_community = FALSE, font_size = attribute_font_size),
    list(sheet_name = "Community Network",
         do_community = TRUE,  font_size = community_font_size)
  )

  can_parallel <- requireNamespace("furrr", quietly = TRUE) &&
                  requireNamespace("future", quietly = TRUE)

  render_sequential <- function() {
    lapply(render_specs, function(spec) {
      .render_map_png(spec$sheet_name, spec$do_community, spec$font_size)
    })
  }

  renders <- NULL
  if (can_parallel) {
    old_plan <- future::plan()
    if (inherits(old_plan, "sequential")) {
      future::plan(future::multisession, workers = 2)
      on.exit(future::plan(old_plan), add = TRUE)
    }
    # Fall back to sequential on worker-side failures — the common case is
    # devtools::load_all() development where "work" isn't installed and so
    # library(work) fails inside the worker.
    #
    # withCallingHandlers muffles just the "added, removed, or modified
    # connections" warning that future emits because webshot2/chromote leaks
    # supervisor FIFO handles inside each worker. Harmless (PNGs save fine),
    # but noisy — muffling only this specific pattern keeps real warnings
    # from bn_visual() / webshot2 visible.
    renders <- tryCatch(
      withCallingHandlers(
        furrr::future_map(render_specs, function(spec) {
          .render_map_png(spec$sheet_name, spec$do_community, spec$font_size)
        }, .options = furrr::furrr_options(seed = TRUE, packages = "work")),
        warning = function(w) {
          if (grepl("added, removed, or modified connections",
                    conditionMessage(w), fixed = TRUE)) {
            invokeRestart("muffleWarning")
          }
        }
      ),
      error = function(e) {
        warning("Parallel PNG render failed, falling back to sequential: ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }
  if (is.null(renders)) renders <- render_sequential()

  # Attach serially on the main thread (openxlsx workbooks aren't
  # cross-process-safe to mutate).
  for (rendered in renders) wb <- .attach_map_sheet(wb, rendered)

  invisible(wb)
}
