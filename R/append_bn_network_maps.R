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
    defer_images = FALSE
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

  # Helper: render one map to a sheet
  .add_map_sheet <- function(wb, sheet_name, do_community) {
    if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::addStyle(wb, sheet_name,
      style = openxlsx::createStyle(fgFill = "#FFFFFF"),
      rows = 1:200, cols = 1:50, gridExpand = TRUE, stack = TRUE)

    png_base <- file.path(tmp_dir, gsub(" ", "_", sheet_name))
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
        save_visuals = FALSE
      )
    }, error = function(e) {
      warning(sheet_name, " render failed: ", conditionMessage(e))
    })

    png_path <- paste0(png_base, ".png")
    if (file.exists(png_path) && file.info(png_path)$size > 1000) {
      openxlsx::writeData(wb, sheet_name, sheet_name,
        startRow = 2, startCol = 2)
      openxlsx::addStyle(wb, sheet_name, style = title_style,
        rows = 2, cols = 2)

      if (isTRUE(defer_images)) {
        # Record image metadata — caller will insert via openxlsx2 after
        # the workbook is saved and reloaded. Do NOT insert here.
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

  # Attribute network sheet
  wb <- .add_map_sheet(wb, "Attribute Network", do_community = FALSE)

  # Community network sheet
  wb <- .add_map_sheet(wb, "Community Network", do_community = TRUE)

  invisible(wb)
}
