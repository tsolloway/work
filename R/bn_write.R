#' bn_write
#'
#' @description Takes the output of \code{bn_finalize_network()} and writes
#'   both the impacts and prioritization dashboards to a single combined
#'   Excel workbook. Flexibly handles cases where only impacts or only
#'   prioritizations are present (i.e., when \code{do_impacts = FALSE} or
#'   \code{do_prioritizations = FALSE} was passed to
#'   \code{bn_finalize_network()}). Prioritization tabs always appear at the
#'   end of the workbook. A single unified Guide tab is added last when
#'   both sections are present; otherwise the individual write function's
#'   Guide is used.
#'
#' @param obj Output of \code{bn_finalize_network()}. Expected to contain
#'   \code{impacts}, \code{prioritizations}, or both, along with
#'   \code{bn_subgroups} (used by the simulator).
#' @param df Data frame used to fit the network. Required when the simulator
#'   is enabled.
#' @param dictionary Optional. A data frame or named object for variable
#'   labels.
#' @param title,sub_title,file_name Shared header / file name controls. If
#'   \code{file_name} is NULL, inherits from \code{sub_title}.
#' @param brand_names Character vector or NULL. Brand levels to include.
#' @param shift_range,shift_step Simulator frequency-shift range and step.
#' @param wb_type \code{"dynamic"} (default) or \code{"standard"} — impact
#'   portion only; prioritization is always dynamic.
#' @param add_simple_simulator Logical. Adds the Simulator sheet when TRUE.
#' @param sim_dv_only Logical. Restricts simulator to DV-only targets.
#' @param variable_width,community_width,label_width,combo_width Column widths.
#' @param network_type Character. Layout type for the network map sheets.
#'   One of \code{"gravity"} (default), \code{"none"} (skips network maps),
#'   \code{"charge"}, or \code{"hierarchy"}. See \code{bn_visual()} for
#'   layout details.
#' @param attribute_map_font_size Numeric or NULL. Node-label font size (in
#'   pixels) for the Attribute Network PNG. Default 30. Set NULL to fall
#'   back to vis.js's built-in 14 px.
#' @param community_map_font_size Numeric or NULL. Node-label font size (in
#'   pixels) for the Community Network PNG. Default 30. Set NULL to fall
#'   back to vis.js's built-in 14 px.
#' @param very_hide_all Logical. Use veryHidden (TRUE) or hidden (FALSE) for
#'   helper sheets.
#' @param min_base_for_lift,min_base_for_sim,min_base_for_boot Base-size
#'   thresholds — fallbacks inherit from meta.
#' @param sig_threshold,marginal_threshold P-value thresholds used in the
#'   prioritization dashboard. If NULL, inherits from
#'   \code{prioritizations$meta} (set by \code{bn_finalize_network()} /
#'   \code{bn_prioritizations()}); falls back to 0.05 / 0.10.
#' @param lift Lift fraction used in the prioritization analysis (for footer).
#' @param path Character. Directory to write the workbook to.
#'
#' @return The full path of the written `.xlsx` file (invisibly). Side
#'   effect: writes the xlsx file.
#'
#' @seealso [bn_finalize_network()], [bn_impact_write()],
#'   [bn_prioritize_write()]
#'
#' @export
bn_write <- function(
    obj,
    df = NULL,
    dictionary = NULL,
    title = NULL,
    sub_title = NULL,
    file_name = NULL,
    brand_names = NULL,
    shift_range = c(-0.25, 0.25),
    shift_step = 0.05,
    wb_type = c("dynamic", "standard"),
    add_simple_simulator = TRUE,
    sim_dv_only = FALSE,
    variable_width = 20,
    community_width = 20,
    label_width = "auto",
    combo_width = 40,
    network_type = "gravity",
    attribute_map_font_size = 30,
    community_map_font_size = 30,
    very_hide_all = TRUE,
    min_base_for_lift = NULL,
    min_base_for_sim = NULL,
    min_base_for_boot = NULL,
    sig_threshold = NULL,
    marginal_threshold = NULL,
    lift = 0.10,
    path = "."
) {

  wb_type <- match.arg(wb_type)

  impacts <- obj[["impacts"]]
  prioritizations <- obj[["prioritizations"]]

  has_impacts <- !is.null(impacts)
  has_prioritizations <- !is.null(prioritizations)

  if (!has_impacts && !has_prioritizations) {
    cli::cli_abort(
      "{.arg obj} must contain {.field impacts} or {.field prioritizations}."
    )
  }

  # Shared file naming: fall back logic mirroring the individual writers
  if (is.null(file_name)) file_name <- sub_title
  if (is.null(sub_title)) sub_title <- file_name

  # Figure out DV for filename / guide
  meta_for_dv <- if (has_impacts) impacts[["meta"]] else prioritizations[["meta"]]
  dv <- meta_for_dv[["dv"]]
  dv_display <- if (!is.null(dv) && !is.null(names(dv))) {
    names(dv)
  } else if (!is.null(dv)) dv else NULL

  # File name logic:
  #   both present OR impacts only  → "{file_name} - Network Drivers of {dv}.xlsx"
  #   prioritize only               → "{file_name} - Prioritization.xlsx"
  if (has_impacts) {
    dv_suffix <- if (!is.null(dv_display)) {
      paste("Network Drivers of", dv_display)
    } else {
      "Network Drivers"
    }
  } else {
    dv_suffix <- "Prioritization"
  }

  fname <- if (!is.null(file_name)) {
    paste0(file_name, " - ", dv_suffix, ".xlsx")
  } else {
    paste0(dv_suffix, ".xlsx")
  }
  file_path <- file.path(path, fname)

  # ---------------------------------------------------------------------------
  # 1. Create the shared workbook
  # ---------------------------------------------------------------------------
  wb <- oxl_create_workbook()

  # ---------------------------------------------------------------------------
  # 2. Impacts (when present) — always first
  # ---------------------------------------------------------------------------
  # Whether each portion gets its OWN guide or not: when both are present we
  # build a single unified guide at the end; when only one is present we let
  # that portion's standalone guide render.
  unified_guide <- has_impacts && has_prioritizations

  if (has_impacts) {
    wb <- bn_impact_write(
      bn_impact_result   = impacts,
      bn_obj             = obj,
      df                 = df,
      dictionary         = dictionary,
      title              = title,
      sub_title          = sub_title,
      file_name          = file_name,
      brand_names        = brand_names,
      shift_range        = shift_range,
      shift_step         = shift_step,
      wb_type            = wb_type,
      add_simple_simulator = add_simple_simulator,
      sim_dv_only        = sim_dv_only,
      variable_width     = variable_width,
      community_width    = community_width,
      label_width        = label_width,
      network_type       = network_type,
      attribute_map_font_size = attribute_map_font_size,
      community_map_font_size = community_map_font_size,
      very_hide_all      = very_hide_all,
      min_base_for_lift  = min_base_for_lift,
      min_base_for_sim   = min_base_for_sim,
      path               = path,
      wb                 = wb,
      save               = FALSE,
      add_guide          = !unified_guide,
      add_images         = FALSE     # deferred — added post-prioritize below
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Prioritizations (when present)
  # ---------------------------------------------------------------------------
  if (has_prioritizations) {
    wb <- bn_prioritize_write(
      result             = prioritizations,
      title              = title,
      sub_title          = sub_title,
      file_name          = file_name,
      variable_width     = variable_width,
      community_width    = community_width,
      label_width        = if (identical(label_width, "auto")) 20 else label_width,
      combo_width        = combo_width,
      sig_threshold      = sig_threshold,
      marginal_threshold = marginal_threshold,
      lift               = lift,
      min_base_for_boot  = min_base_for_boot,
      very_hide_all      = very_hide_all,
      path               = path,
      wb                 = wb,
      save               = FALSE,
      add_guide          = !unified_guide
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Network maps — build sheets + render PNGs, but DEFER image insertion.
  # Images are actually inserted by openxlsx2 in the post-save step so they
  # survive openxlsx2's save step (which drops openxlsx-embedded images).
  # ---------------------------------------------------------------------------
  if (has_impacts && !is.null(obj) && "bn_subgroups" %in% names(obj)) {
    wb <- append_bn_network_maps(
      wb = wb, bn_full = obj, network_type = network_type,
      defer_images = TRUE,
      attribute_font_size = attribute_map_font_size,
      community_font_size = community_map_font_size
    )
  }

  # ---------------------------------------------------------------------------
  # 5. Unified Guide (only when both portions present)
  # ---------------------------------------------------------------------------
  if (unified_guide) {
    wb <- append_bn_unified_guide(
      wb = wb,
      impacts = impacts,
      prioritizations = prioritizations,
      dv_display = dv_display,
      has_weights = !is.null(impacts[["table_attribute_weighted"]]),
      has_community = !is.null(impacts[["table_community"]]),
      has_simulator = isTRUE(add_simple_simulator) && !is.null(obj[["bn_subgroups"]]),
      has_brands = !is.null(impacts[["meta"]][["brand"]]) ||
        !is.null(impacts[["meta"]][["brand_names"]]),
      wb_type = wb_type,
      sim_dv_only = sim_dv_only,
      sig_threshold = sig_threshold,
      marginal_threshold = marginal_threshold,
      lift = lift,
      min_base_for_lift = min_base_for_lift %||% impacts[["meta"]][["min_base_for_lift"]] %||% 100L,
      min_base_for_sim = min_base_for_sim %||% min_base_for_lift %||%
        impacts[["meta"]][["min_base_for_lift"]] %||% 100L,
      min_base_for_boot = min_base_for_boot %||%
        prioritizations[["meta"]][["min_base_for_boot"]] %||% 100L
    )
  }

  # ---------------------------------------------------------------------------
  # 6. Save — two paths depending on whether we need openxlsx2's capabilities.
  #
  # The only reasons we ever route through openxlsx2 are (a) to inject the
  # prioritization waterfall chart XML, and (b) to attach deferred network
  # map images (we defer them because openxlsx2's wb_load drops images that
  # openxlsx embedded). When neither is present — e.g. network_type = "none"
  # AND no prioritization chart — we can save straight through openxlsx and
  # skip the wb_load/wb2$save round-trip (saves ~3-10 s for medium sheets).
  # ---------------------------------------------------------------------------
  chart_xml  <- attr(wb, "priorit_chart_xml")
  chart_dims <- attr(wb, "priorit_chart_dims")
  net_images <- attr(wb, "network_map_images")

  needs_openxlsx2 <- (!is.null(chart_xml) && !is.null(chart_dims)) ||
                    (length(net_images) > 0)

  if (needs_openxlsx2) {
    # openxlsx -> temp file -> openxlsx2 -> add chart/images -> final file.
    # openxlsx2's native load/save preserves sheet visibility, so hidden
    # helper sheets stay hidden in the final output.
    tmp_path <- tempfile(fileext = ".xlsx")
    openxlsx::saveWorkbook(wb, tmp_path, overwrite = TRUE)

    wb2 <- openxlsx2::wb_load(tmp_path)

    # Images go in BEFORE the chart XML so openxlsx2 gives them the natural
    # rId sequence.
    for (img in net_images) {
      if (!file.exists(img$file)) next
      wb2$add_image(
        sheet = img$sheet,
        file  = img$file,
        dims  = paste0(num2let(img$start_col), img$start_row),
        width = img$width,
        height = img$height,
        units = "in"
      )
    }

    if (!is.null(chart_xml) && !is.null(chart_dims)) {
      wb2$add_chart_xml(sheet = "Prioritization",
        dims = chart_dims, xml = chart_xml)
    }

    wb2$save(file_path, overwrite = TRUE)
    if (file.exists(tmp_path)) unlink(tmp_path)
  } else {
    # Fast path: no images, no chart — just save openxlsx directly.
    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  }

  # Cleanup
  for (td in attr(wb, "tmp_dirs") %||% character(0)) {
    unlink(td, recursive = TRUE)
  }

  cli::cli_alert_success("Combined workbook saved: {file_path}")

  invisible(file_path)
}
