#' correspondence_write
#'
#' @description Takes the output of [correspondence_analysis()] and writes an
#'   Excel workbook containing a native, editable scatter-chart perceptual map.
#'   Point labels are attached with Excel's "Value from Cells" mechanism, so
#'   users can click and drag any label to de-clutter the map before pasting it
#'   into a report. Brands and attributes are separate series, and supplementary
#'   points (thin bases projected onto the map — see
#'   [correspondence_analysis()]) get their own lighter-colored series.
#'
#'   Each map also gets a rotation control: a spin button (plus an editable
#'   angle cell) that rotates the whole solution 0-360 degrees in 1-degree
#'   increments. The chart plots rotated coordinate columns computed by cell
#'   formulas, so the map redraws live as the angle changes. Rotation is
#'   orthogonal — distances between points are preserved.
#'
#' @param ca_result Output of [correspondence_analysis()], or a named list of
#'   such results (one map per element, e.g.
#'   \code{list("Platform" = ca1, "Creator" = ca2)}).
#' @param file_name Character. Prefix for the output file. Saved as
#'   \code{{file_name} - Correspondence Maps.xlsx}. If NULL, inherits from
#'   \code{sub_title}; if both are NULL, \code{Correspondence Maps.xlsx}.
#' @param sub_title Character. Subtitle text (e.g. project name). Default NULL.
#' @param path Directory to write the workbook to (default \code{"."}).
#' @param brand_color Hex color (no \code{#}) for active brand points.
#' @param brand_supp_color Hex color for supplementary brand points.
#' @param attribute_color Hex color for active attribute points.
#' @param attribute_supp_color Hex color for supplementary attribute points.
#' @param hide_data Logical. If TRUE (default), the per-map Data sheets are set
#'   to Excel's "veryHidden" state — invisible and absent from the right-click
#'   unhide list (recoverable only via VBA or openxlsx2). The charts, rotation
#'   formulas, and spinner links keep working off the hidden sheets.
#' @param save Logical. If TRUE (default) the workbook is saved to disk.
#'
#' @return The file path (invisibly).
#'
#' @export
correspondence_write <- function(
    ca_result,
    file_name = NULL,
    sub_title = NULL,
    path = ".",
    brand_color = "1A73E8",
    brand_supp_color = "9FC5F8",
    attribute_color = "595959",
    attribute_supp_color = "BFBFBF",
    hide_data = TRUE,
    save = TRUE
){

  if (is.null(file_name)) file_name <- sub_title
  if (is.null(sub_title)) sub_title <- file_name

  # Accept a single result or a named list of results
  is_single <- all(c("coordinates", "inertia", "matrix", "bases") %in% names(ca_result))
  maps <- if (is_single) list(Map = ca_result) else ca_result
  if (is.null(names(maps)) || any(names(maps) == "")) {
    names(maps) <- paste("Map", seq_along(maps))
  }

  colors <- c(
    brand_active         = brand_color,
    brand_supplementary  = brand_supp_color,
    attr_active          = attribute_color,
    attr_supplementary   = attribute_supp_color
  )

  # --- layout constants -------------------------------------------------
  row_title    <- 2L
  row_subtitle <- 3L
  row_inertia  <- 4L
  row_rotation <- 5L   # Map sheet: rotation label / angle cell / spinner
  angle_col    <- 4L   # Map sheet column D holds the angle value
  chart_dims   <- "B7:R41"
  row_footer   <- 43L
  data_start   <- 5L   # Data sheet: header row of the coordinates table

  styles <- list(
    title     = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header    = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE),
    note      = openxlsx::createStyle(fontColour = "#7A7A7A", fontSize = 10),
    angle     = openxlsx::createStyle(
      textDecoration = "bold", halign = "center", border = "TopBottomLeftRight",
      fgFill = "#F2F2F2"
    )
  )

  wb <- oxl_create_workbook()

  # Per-map registry of chart / control metadata, injected post-save via openxlsx2
  chart_registry <- list()

  for (map_name in names(maps)) {

    res <- maps[[map_name]]

    map_sheet  <- .corr_sheet_name(map_name, "Map")
    data_sheet <- .corr_sheet_name(map_name, "Data")
    angle_ref  <- paste0("'", map_sheet, "'!$", num2let(angle_col), "$", row_rotation)

    supplemental_base <- res[["meta"]][["supplemental_base"]]
    exclude_base      <- res[["meta"]][["exclude_base"]]

    # --- order coordinates into contiguous series blocks -----------------
    coords <- res[["coordinates"]] %>%
      dplyr::mutate(
        series = dplyr::case_when(
          type == "brand"     & !supplementary ~ "brand_active",
          type == "brand"     &  supplementary ~ "brand_supplementary",
          type == "attribute" & !supplementary ~ "attr_active",
          type == "attribute" &  supplementary ~ "attr_supplementary"
        )
      ) %>%
      dplyr::arrange(match(series, names(colors)))

    n_pts <- nrow(coords)

    # Fixed, rotation-invariant axis limits: the largest point radius, padded.
    r_max    <- max(sqrt(coords[["dim_1"]]^2 + coords[["dim_2"]]^2))
    axis_lim <- ceiling(r_max * 1.2 * 1000) / 1000

    # =====================================================================
    # Map sheet (title block, rotation control, chart, footer)
    # =====================================================================
    openxlsx::addWorksheet(wb, map_sheet, gridLines = FALSE)

    # NOTE: text passed to writeData must be plain character — a glue object
    # is written as a one-column data frame (an "x" header lands in the cell).
    openxlsx::writeData(wb, map_sheet, paste0("Perceptual Map - ", map_name),
                        startRow = row_title, startCol = 2)
    openxlsx::addStyle(wb, map_sheet, styles$title, rows = row_title, cols = 2, stack = TRUE)

    if (!is.null(sub_title)) {
      openxlsx::writeData(wb, map_sheet, as.character(sub_title),
                          startRow = row_subtitle, startCol = 2)
      openxlsx::addStyle(wb, map_sheet, styles$sub_title,
                         rows = row_subtitle, cols = 2, stack = TRUE)
    }

    openxlsx::writeData(
      wb, map_sheet,
      paste0(
        "Dim 1 = ", round(res$inertia$inertia_pct[1], 1), "% | ",
        "Dim 2 = ", round(res$inertia$inertia_pct[2], 1), "% | ",
        "cumulative = ", round(res$inertia$inertia_cumulative[2], 1), "% of inertia"
      ),
      startRow = row_inertia, startCol = 2
    )
    openxlsx::addStyle(wb, map_sheet, styles$note, rows = row_inertia, cols = 2, stack = TRUE)

    # Rotation control row: label (B), angle value (D), spinner patched in at (E)
    openxlsx::writeData(wb, map_sheet, "Rotation (degrees):",
                        startRow = row_rotation, startCol = 2)
    openxlsx::writeData(wb, map_sheet, 0L, startRow = row_rotation, startCol = angle_col)
    openxlsx::addStyle(wb, map_sheet, styles$angle,
                       rows = row_rotation, cols = angle_col, stack = TRUE)
    openxlsx::dataValidation(
      wb, map_sheet, cols = angle_col, rows = row_rotation,
      type = "whole", operator = "between", value = c(0, 360)
    )

    # Footer
    excluded <- res[["bases"]] %>% dplyr::filter(status == "excluded")
    footer_lines <- as.character(c(
      paste0("Lighter points are supplementary (base < ", supplemental_base, "): ",
             "shown on the map but excluded when constructing the axes."),
      if (nrow(excluded) > 0) {
        paste0("Excluded entirely (base < ", exclude_base, "): ",
               paste(excluded$point, collapse = ", "), ".")
      },
      "Click any point label to select it, then drag to reposition.",
      paste("Use the spinner (or type 0-360 in the rotation cell) to rotate the map;",
            "distances between points are preserved.")
    ))
    for (i in seq_along(footer_lines)) {
      openxlsx::writeData(wb, map_sheet, footer_lines[[i]],
                          startRow = row_footer + i - 1L, startCol = 2)
      openxlsx::addStyle(wb, map_sheet, styles$note,
                         rows = row_footer + i - 1L, cols = 2, stack = TRUE)
    }

    # =====================================================================
    # Data sheet (title block, coordinates + rotated columns, inertia, matrix)
    # =====================================================================
    openxlsx::addWorksheet(wb, data_sheet)

    openxlsx::writeData(wb, data_sheet, paste0("Perceptual Map - ", map_name),
                        startRow = row_title, startCol = 1)
    openxlsx::addStyle(wb, data_sheet, styles$title, rows = row_title, cols = 1, stack = TRUE)
    if (!is.null(sub_title)) {
      openxlsx::writeData(wb, data_sheet, as.character(sub_title),
                          startRow = row_subtitle, startCol = 1)
      openxlsx::addStyle(wb, data_sheet, styles$sub_title,
                         rows = row_subtitle, cols = 1, stack = TRUE)
    }

    coords_out <- coords %>%
      dplyr::select(point, type, base, supplementary, dim_1, dim_2) %>%
      dplyr::mutate(dim_1_rotated = NA_real_, dim_2_rotated = NA_real_)

    openxlsx::writeData(wb, data_sheet, coords_out,
                        startRow = data_start, headerStyle = styles$header)
    openxlsx::setColWidths(wb, data_sheet, cols = 1, widths = 42)
    openxlsx::setColWidths(wb, data_sheet, cols = 2:8, widths = 14)

    # Rotated coordinates: x' = x cos - y sin ; y' = x sin + y cos
    data_rows <- (data_start + 1L):(data_start + n_pts)
    openxlsx::writeFormula(
      wb, data_sheet, startRow = data_start + 1L, startCol = 7,
      x = glue::glue("=$E{data_rows}*COS(RADIANS({angle_ref}))",
                     "-$F{data_rows}*SIN(RADIANS({angle_ref}))")
    )
    openxlsx::writeFormula(
      wb, data_sheet, startRow = data_start + 1L, startCol = 8,
      x = glue::glue("=$E{data_rows}*SIN(RADIANS({angle_ref}))",
                     "+$F{data_rows}*COS(RADIANS({angle_ref}))")
    )

    inertia_col <- ncol(coords_out) + 2   # one spacer column
    openxlsx::writeData(wb, data_sheet, res[["inertia"]],
                        startRow = data_start, startCol = inertia_col,
                        headerStyle = styles$header)

    matrix_row <- data_start + n_pts + 2L
    openxlsx::writeData(wb, data_sheet, "Input matrix (means):",
                        startRow = matrix_row, startCol = 1)
    openxlsx::addStyle(wb, data_sheet, styles$sub_title,
                       rows = matrix_row, cols = 1, stack = TRUE)
    openxlsx::writeData(wb, data_sheet,
                        tibble::as_tibble(res[["matrix"]], rownames = "brand"),
                        startRow = matrix_row + 1L, headerStyle = styles$header)

    # --- chart series metadata (sheet row ranges per block) ---------------
    series_meta <- coords %>%
      dplyr::mutate(row = dplyr::row_number() + data_start) %>%
      dplyr::group_by(series) %>%
      dplyr::summarise(
        r1 = min(row), r2 = max(row),
        labels = list(point), .groups = "drop"
      ) %>%
      dplyr::mutate(series = factor(series, levels = names(colors))) %>%
      dplyr::arrange(series)

    chart_registry[[map_sheet]] <- list(
      data_sheet  = data_sheet,
      series_meta = series_meta,
      dims        = chart_dims,
      axis_lim    = axis_lim,
      angle_ref   = angle_ref
    )
  }

  # Very-hide the data sheets (assign only "veryHidden" — mixing logical FALSE
  # and "veryHidden" in the visibility vector corrupts openxlsx state). Map
  # sheets stay visible, so the workbook always has a visible first sheet.
  if (isTRUE(hide_data)) {
    sheet_names <- openxlsx::sheets(wb)
    for (reg in chart_registry) {
      openxlsx::sheetVisibility(wb)[match(reg[["data_sheet"]], sheet_names)] <- "veryHidden"
    }
  }

  # ---------------------------------------------------------------------
  # Save via openxlsx, then reload with openxlsx2 to inject the chart XML
  # and rotation spinners (openxlsx can write neither natively).
  # ---------------------------------------------------------------------
  fname <- if (!is.null(file_name)) {
    paste0(file_name, " - Correspondence Maps.xlsx")
  } else {
    "Correspondence Maps.xlsx"
  }
  file_path <- file.path(path, fname)

  if (!isTRUE(save)) return(invisible(wb))

  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  wb2 <- openxlsx2::wb_load(file_path)

  for (map_sheet in names(chart_registry)) {
    reg <- chart_registry[[map_sheet]]

    # Rotation spinner: add a placeholder control (creates the VML part,
    # relationships, and content types), then patch it into a Spin button —
    # openxlsx2 only offers Checkbox/Radio/Drop directly.
    wb2 <- .corr_add_spinner(
      wb2       = wb2,
      sheet     = map_sheet,
      dims      = paste0("E", 5),
      angle_ref = reg[["angle_ref"]]
    )

    chart_xml <- .correspondence_chart_xml(
      data_sheet  = reg[["data_sheet"]],
      series_meta = reg[["series_meta"]],
      colors      = colors,
      axis_lim    = reg[["axis_lim"]]
    )
    wb2$add_chart_xml(sheet = map_sheet, dims = reg[["dims"]], xml = chart_xml)
  }

  wb2$save(file_path, overwrite = TRUE)

  cli::cli_alert_success("Correspondence map workbook saved: {file_path}")
  invisible(file_path)
}


# Sheet names: "{map} - Map" / "{map} - Data", truncated to Excel's 31 chars
.corr_sheet_name <- function(map_name, suffix) {
  base <- substr(map_name, 1, 31 - nchar(suffix) - 3)
  paste(base, "-", suffix)
}


# Add a 0-360 (1-degree increment) spin button linked to the angle cell.
# openxlsx2 has no native spin-button type, so we add a Checkbox (which
# creates the legacy-VML part, worksheet relationship, and content types)
# and rewrite its ClientData / formControlPr into a Spin control. Excel
# reads form controls from the legacy VML ClientData, so this is the same
# structure Excel itself writes. Excel orients a spin button by its aspect
# ratio, so the control is anchored two columns wide by one row tall
# (wider than tall) to get left/right arrows instead of up/down.
.corr_add_spinner <- function(wb2, sheet, dims, angle_ref) {

  n_vml  <- length(wb2$vml)
  n_ctrl <- length(wb2$ctrlProps)

  wb2$add_form_control(sheet = sheet, dims = dims, type = "Checkbox",
                       text = "", link = angle_ref)

  # --- patch the ctrlProps entry ----------------------------------------
  idx_ctrl <- n_ctrl + 1L
  wb2$ctrlProps[[idx_ctrl]] <- paste0(
    '<formControlPr xmlns="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main" ',
    'objectType="Spin" dx="16" fmlaLink="', angle_ref,
    '" inc="1" max="360" min="0" page="10" val="0"/>'
  )

  # --- patch the VML shape ------------------------------------------------
  idx_vml <- if (length(wb2$vml) > n_vml) length(wb2$vml) else n_vml
  vml <- wb2$vml[[idx_vml]]

  # Horizontal anchor: from the dims cell, span 2 columns x 1 row
  # (zero-based: col, colOff, row, rowOff, col2, colOff2, row2, rowOff2)
  col0 <- match(gsub("[0-9]", "", dims), LETTERS) - 1L
  row0 <- as.integer(gsub("[A-Z]", "", dims)) - 1L
  anchor <- paste0(
    "<x:Anchor>",
    col0, ", 0, ", row0, ", 0, ", col0 + 2L, ", 0, ", row0 + 1L, ", 0",
    "</x:Anchor>"
  )

  client_data <- paste0(
    '<x:ClientData ObjectType="Spin">',
    '<x:MoveWithCells/><x:SizeWithCells/>',
    anchor,
    '<x:AutoFill>False</x:AutoFill><x:AutoLine>False</x:AutoLine>',
    '<x:FmlaLink>', angle_ref, '</x:FmlaLink>',
    '<x:Val>0</x:Val><x:Min>0</x:Min><x:Max>360</x:Max><x:Inc>1</x:Inc>',
    '<x:Page>10</x:Page>',
    '</x:ClientData>'
  )

  vml <- sub("<v:textbox.*?</v:textbox>", "", vml)
  vml <- sub("<x:ClientData ObjectType=\"Checkbox\">.*?</x:ClientData>",
             client_data, vml)
  # Shape style must agree with the anchor (wider than tall), or Excel
  # falls back to the placeholder's tall box and draws up/down arrows.
  vml <- sub('style="[^"]*"',
             'style="position:absolute;width:96pt;height:16pt;z-index:1"',
             vml)
  wb2$vml[[idx_vml]] <- vml

  wb2
}


# Build chartSpace XML for the perceptual map: an XY scatter with one series
# per block (brand/attribute x active/supplementary). Labels use the c15
# "Value from Cells" extension, which renders each point's name as a
# draggable data label. The chart plots the *rotated* coordinate columns
# (G/H on the data sheet) with fixed axis limits so rotation doesn't rescale.
.correspondence_chart_xml <- function(data_sheet, series_meta, colors, axis_lim) {

  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    gsub(">", "&gt;",  x, fixed = TRUE)
  }

  ref <- function(col, r1, r2) {
    paste0("'", data_sheet, "'!$", col, "$", r1, ":$", col, "$", r2)
  }

  series_names <- c(
    brand_active        = "Brands",
    brand_supplementary = "Brands (supplementary)",
    attr_active         = "Attributes",
    attr_supplementary  = "Attributes (supplementary)"
  )

  # marker + label styling per block
  marker_symbol <- c(
    brand_active = "circle",   brand_supplementary = "circle",
    attr_active  = "triangle", attr_supplementary  = "triangle"
  )
  marker_size <- c(
    brand_active = 9, brand_supplementary = 9,
    attr_active  = 6, attr_supplementary  = 6
  )
  label_size <- c(   # font size in hundredths of a point
    brand_active = 1100, brand_supplementary = 1100,
    attr_active  = 800,  attr_supplementary  = 800
  )
  label_bold <- c(
    brand_active = 1, brand_supplementary = 1,
    attr_active  = 0, attr_supplementary  = 0
  )

  sers <- purrr::imap_chr(
    rlang::set_names(seq_len(nrow(series_meta)), as.character(series_meta[["series"]])),
    function(i, key) {

      r1     <- series_meta[["r1"]][i]
      r2     <- series_meta[["r2"]][i]
      labels <- series_meta[["labels"]][[i]]
      color  <- colors[[key]]
      n_pts  <- length(labels)

      label_cache <- paste0(
        '<c15:dlblRangeCache><c:ptCount val="', n_pts, '"/>',
        paste0(
          '<c:pt idx="', seq_len(n_pts) - 1L, '"><c:v>', esc(labels), '</c:v></c:pt>',
          collapse = ""
        ),
        '</c15:dlblRangeCache>'
      )

      paste0(
        '<c:ser>',
        '<c:idx val="', i - 1L, '"/><c:order val="', i - 1L, '"/>',
        '<c:tx><c:v>', esc(series_names[[key]]), '</c:v></c:tx>',
        '<c:spPr><a:ln><a:noFill/></a:ln></c:spPr>',

        '<c:marker>',
        '<c:symbol val="', marker_symbol[[key]], '"/>',
        '<c:size val="', marker_size[[key]], '"/>',
        '<c:spPr>',
        '<a:solidFill><a:srgbClr val="', color, '"/></a:solidFill>',
        '<a:ln><a:solidFill><a:srgbClr val="', color, '"/></a:solidFill></a:ln>',
        '</c:spPr>',
        '</c:marker>',

        # Data labels: value-from-cells (draggable), colored to match series
        '<c:dLbls>',
        '<c:spPr><a:noFill/><a:ln><a:noFill/></a:ln></c:spPr>',
        '<c:txPr><a:bodyPr/><a:lstStyle/>',
        '<a:p><a:pPr><a:defRPr sz="', label_size[[key]], '" b="', label_bold[[key]], '">',
        '<a:solidFill><a:srgbClr val="', color, '"/></a:solidFill>',
        '<a:latin typeface="Calibri"/><a:cs typeface="Calibri"/>',
        '</a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p>',
        '</c:txPr>',
        '<c:dLblPos val="r"/>',
        '<c:showLegendKey val="0"/><c:showVal val="0"/><c:showCatName val="0"/>',
        '<c:showSerName val="0"/><c:showPercent val="0"/><c:showBubbleSize val="0"/>',
        '<c:extLst>',
        '<c:ext uri="{CE6537A1-D6FC-4f65-9D91-7224C49458BB}" ',
        'xmlns:c15="http://schemas.microsoft.com/office/drawing/2012/chart">',
        '<c15:showDataLabelsRange val="1"/>',
        '</c:ext>',
        '</c:extLst>',
        '</c:dLbls>',

        # Rotated coordinates: G = dim_1_rotated, H = dim_2_rotated
        '<c:xVal><c:numRef><c:f>', ref("G", r1, r2), '</c:f></c:numRef></c:xVal>',
        '<c:yVal><c:numRef><c:f>', ref("H", r1, r2), '</c:f></c:numRef></c:yVal>',
        '<c:smooth val="0"/>',

        # Series extension: the label range that feeds value-from-cells
        '<c:extLst>',
        '<c:ext uri="{02D57815-91ED-43cb-92C2-25804820EDAC}" ',
        'xmlns:c15="http://schemas.microsoft.com/office/drawing/2012/chart">',
        '<c15:datalabelsRange>',
        '<c15:f>', ref("A", r1, r2), '</c15:f>',
        label_cache,
        '</c15:datalabelsRange>',
        '</c:ext>',
        '</c:extLst>',
        '</c:ser>'
      )
    }
  )

  axis <- function(ax_id, cross_id, pos) {
    paste0(
      '<c:valAx>',
      '<c:axId val="', ax_id, '"/>',
      '<c:scaling><c:orientation val="minMax"/>',
      '<c:max val="', axis_lim, '"/><c:min val="-', axis_lim, '"/>',
      '</c:scaling>',
      '<c:delete val="0"/>',
      '<c:axPos val="', pos, '"/>',
      '<c:numFmt formatCode="0.000" sourceLinked="0"/>',
      '<c:majorTickMark val="none"/><c:minorTickMark val="none"/>',
      '<c:tickLblPos val="none"/>',
      '<c:spPr><a:ln><a:solidFill><a:srgbClr val="D9D9D9"/></a:solidFill></a:ln></c:spPr>',
      '<c:crossAx val="', cross_id, '"/>',
      '</c:valAx>'
    )
  }

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<c:roundedCorners val="0"/>',
    '<c:chart>',
    '<c:autoTitleDeleted val="1"/>',
    '<c:plotArea>',
    '<c:layout/>',
    '<c:scatterChart>',
    '<c:scatterStyle val="marker"/>',
    '<c:varyColors val="0"/>',
    paste0(sers, collapse = ""),
    '<c:axId val="111111111"/>',
    '<c:axId val="222222222"/>',
    '</c:scatterChart>',
    axis("111111111", "222222222", "b"),
    axis("222222222", "111111111", "l"),
    '</c:plotArea>',
    '<c:legend>',
    '<c:legendPos val="b"/>',
    '<c:overlay val="0"/>',
    '<c:txPr><a:bodyPr/><a:lstStyle/>',
    '<a:p><a:pPr><a:defRPr sz="900">',
    '<a:latin typeface="Calibri"/><a:cs typeface="Calibri"/>',
    '</a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p>',
    '</c:txPr>',
    '</c:legend>',
    # 0 = plot data even when its source sheet/rows are hidden — required for
    # the charts to render when hide_data very-hides the Data sheets.
    '<c:plotVisOnly val="0"/>',
    '<c:dispBlanksAs val="gap"/>',
    '</c:chart>',
    '</c:chartSpace>'
  )
}
