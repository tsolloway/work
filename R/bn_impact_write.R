#' bn_impact_write
#'
#' @description Takes the output of \code{bn_impact()} and writes a formatted
#'   Excel workbook. Reads metadata (type, index_by, subgroups) from the result
#'   object.
#'
#' @param bn_impact_result The output of \code{bn_impact()} or
#'   \code{bn_impacts()}. When from \code{bn_impacts()}, community and weighted
#'   tables are automatically included. When weighted tables are present, a
#'   Weight control is added to the dynamic dashboard.
#' @param bn_obj The BN object or NULL. Accepts either the full output of
#'   \code{bn_finalize_network()} or just the \code{bn_subgroups} element.
#'   When the full object is provided, network maps are automatically added
#'   and \code{bn_subgroups} is extracted for the simulator. Required when
#'   \code{add_simple_simulator = TRUE}.
#' @param df Data frame or NULL. The original data frame used for impact
#'   estimation. Passed to the simulator for frequency-based shift calculations.
#'   Required when \code{add_simple_simulator = TRUE} and frequency shifts are
#'   desired.
#' @param dictionary Optional. A data frame or named object for variable labels.
#'   Passed to the simulator for labeling target variables.
#' @param title Character or NULL. Title displayed in the sheet header. If NULL,
#'   defaults to \code{"Attribute Drivers of {dv}"} using the DV display name.
#' @param sub_title Character or NULL. Subtitle text displayed in the sheet
#'   header (e.g. project name). If NULL, inherits from \code{file_name}.
#'   Default NULL.
#' @param file_name Character or NULL. Prefix for output file name. File is
#'   saved as \code{{file_name} - Network Drivers of {dv}.xlsx}. If NULL,
#'   inherits from \code{sub_title}. If both are NULL, file is saved as
#'   \code{Network Drivers of {dv}.xlsx}.
#' @param brand_names Character vector or NULL. Brand level names to use in the
#'   simulator. If NULL, auto-detected from \code{meta$brand_names} in the
#'   impact result.
#' @param shift_range Numeric vector of length 2. Min and max shift values for
#'   the simulator's frequency shift sliders. Default \code{c(-0.25, 0.25)}.
#' @param shift_step Numeric. Step size for the simulator's frequency shift
#'   sliders. Default 0.05.
#' @param wb_type Character. Workbook format: \code{"dynamic"} (default)
#'   writes an interactive dashboard with dropdown filters; \code{"standard"}
#'   writes a static formatted table.
#' @param add_simple_simulator Logical. If TRUE (default), adds a Simulator
#'   sheet with interactive dropdowns for exploring conditional probability
#'   distributions. Requires \code{bn_obj} to be provided.
#' @param sim_dv_only Logical. When TRUE, the Simulator only provides results
#'   for the DV as the target (a single row on the dashboard). Dramatically
#'   reduces stored simulator data by dropping all non-DV target columns and
#'   rows. Default FALSE (all nodes queryable as targets).
#' @param variable_width Numeric. Column width for the Variable column.
#'   Default 20.
#' @param community_width Numeric. Column width for the Community column when
#'   community results are present. Default 20.
#' @param label_width Numeric or \code{"auto"}. Column width for the Label
#'   column. Default \code{"auto"}.
#' @param network_type Character. Layout type for the network map sheets
#'   (rendered via \code{bn_visual()}). One of \code{"gravity"} (default),
#'   \code{"none"} (skips network maps), \code{"charge"}, or
#'   \code{"hierarchy"}. Only used when \code{bn_obj} is the full
#'   \code{bn_finalize_network()} output.
#' @param attribute_map_font_size Numeric or NULL. Node-label font size (in
#'   pixels) for the Attribute Network PNG. Default 30. Set NULL to fall
#'   back to vis.js's built-in 14 px.
#' @param community_map_font_size Numeric or NULL. Node-label font size (in
#'   pixels) for the Community Network PNG. Default 30. Set NULL to fall
#'   back to vis.js's built-in 14 px.
#' @param very_hide_all Logical. If TRUE (default), all hidden sheets are set to
#'   veryHidden. If FALSE, they are simply hidden.
#' @param min_base_for_lift Integer or NULL. Minimum sample size used for
#'   footnote text about when lift metrics are calculated. If NULL, inherits
#'   from \code{meta$min_base_for_lift} in the impact result.
#' @param min_base_for_sim Integer or NULL. Minimum sample size required in a
#'   \code{brand x subgroup} slice for the simulator to compute frequency-shift
#'   outputs for that focus. Slices below this threshold are blanked and listed
#'   on a warning sheet. If NULL, inherits from \code{min_base_for_lift} (which
#'   itself falls back to \code{meta$min_base_for_lift} or 75).
#' @param path Character. Directory to write workbook to. Default \code{"."}.
#' @param wb openxlsx workbook object or NULL. When NULL (default), a new
#'   workbook is created. When provided, impact sheets are appended to the
#'   existing workbook — used by \code{bn_write()} to assemble a combined
#'   workbook.
#' @param save Logical. When TRUE (default), the workbook is saved to disk.
#'   When FALSE, the workbook is returned without saving — used by
#'   \code{bn_write()}.
#' @param add_guide Logical. When TRUE (default), a Guide tab is appended.
#'   When FALSE, no Guide is added — used by \code{bn_write()} which builds
#'   a single unified Guide for the combined workbook.
#' @param add_images Logical. When TRUE (default), network map images are
#'   rendered and inserted via \code{append_bn_network_maps()}. When FALSE,
#'   the network map step is skipped entirely — used by \code{bn_write()},
#'   which calls \code{append_bn_network_maps()} separately so the tab
#'   ordering places network maps after the prioritization section.
#'
#' @return Workbook object (invisibly).
#'
#' @export
bn_impact_write <- function(
    bn_impact_result,
    bn_obj = NULL,
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
    network_type = "gravity",
    attribute_map_font_size = 30,
    community_map_font_size = 30,
    very_hide_all = TRUE,
    min_base_for_lift = NULL,
    min_base_for_sim = NULL,
    path = ".",
    wb = NULL,
    save = TRUE,
    add_guide = TRUE,
    add_images = TRUE
){

  wb_type <- match.arg(wb_type)

  # Accept either the full bn_finalize_network() object or just bn_subgroups
  if (!is.null(bn_obj) && "bn_subgroups" %in% names(bn_obj)) {
    bn_full <- bn_obj
    bn_obj <- bn_full[["bn_subgroups"]]
  } else {
    bn_full <- NULL
  }

  if (is.null(file_name)) file_name <- sub_title
  if (is.null(sub_title)) sub_title <- file_name

  # Auto-detect: bn_impact() returns list(table, meta)
  #              bn_impacts() returns list(table_attribute, table_community, ..., meta)
  if ("table_attribute" %in% names(bn_impact_result)) {
    # bn_impacts format
    table <- bn_impact_result[["table_attribute"]]
    table_weighted <- bn_impact_result[["table_attribute_weighted"]]
    community_result <- bn_impact_result[["table_community"]]
    community_weighted <- bn_impact_result[["table_community_weighted"]]
    meta <- bn_impact_result[["meta"]]
  } else {
    # bn_impact format
    table <- bn_impact_result[["table"]]
    table_weighted <- NULL
    community_result <- NULL
    community_weighted <- NULL
    meta <- bn_impact_result[["meta"]]
  }

  has_weights <- !is.null(table_weighted)
  subgroups        <- meta[["subgroups"]]
  index_by         <- meta[["index_by"]]
  type             <- meta[["type"]]
  dv               <- meta[["dv"]]
  if (is.null(min_base_for_lift)) min_base_for_lift <- meta[["min_base_for_lift"]]
  # Simulator threshold: inherit from min_base_for_lift when not supplied
  if (is.null(min_base_for_sim)) min_base_for_sim <- min_base_for_lift %||% 75

  # Use name of dv if named, otherwise the value itself
dv_display <- if (!is.null(names(dv))) names(dv) else dv

  # Title
  if (is.null(title)) {
    title <- if (!is.null(dv_display)) paste("Attribute Drivers of", dv_display) else "Attribute Drivers"
  }

  # Footer based on estimation type
  engine_footer <- switch(type,
    "gr" = "Impact estimated with exact conditional probability distributions",
    "cp" = "Impact estimated with Monte Carlo conditional probability",
    "mi" = "Impact estimated with mutual information",
    "Impact estimated with Bayesian network inference"
  )

  lift <- meta[["lift"]]

  index_footer <- if (index_by %in% c("lift_first", "lift_second")) {
    lift_idx <- if (index_by == "lift_first") 1L else min(2L, length(lift))
    lift_val <- lift[lift_idx]
    if (lift_val == 0) {
      "Indexed by average market lift. Average lift measures the overall influence of each attribute on the outcome by shifting each attribute level up by 5% and averaging the resulting changes"
    } else {
      pct <- round(lift_val * 100)
      paste0("Indexed by ", pct, "% market lift. ", pct, "% lift measures how much the outcome changes when ", pct, "% of respondents for each attribute shift up by one level")
    }
  } else {
    switch(index_by,
      "maxVmin" = "Indexed by max vs min impact. Max vs min measures the difference in the outcome between the best-case and worst-case scenario for each attribute",
      "mi"      = "Indexed by mutual information. Mutual information measures the strength of the relationship between each attribute and the outcome",
      "none"    = NULL
    )
  }

  footer <- paste(c(engine_footer, index_footer), collapse = ". ")

  # Sheet name (tab) — kept generic; in-sheet title carries the DV
  sheet_name <- "Attribute Drivers"

  # Flags for the Guide sheet — known before any sheets are created
  guide_has_community <- !is.null(community_result)
  guide_has_simulator <- isTRUE(add_simple_simulator) && !is.null(bn_obj)
  guide_has_brands <- !is.null(meta[["brand"]]) ||
    !is.null(meta[["brand_names"]]) ||
    !is.null(brand_names)

  if (wb_type == "standard") {

    if (is.null(wb)) wb <- oxl_create_workbook()
    pre_sheets <- names(wb)

    wb <- append_bn_impact(
      analysis_table = table,
      subgroups = subgroups,
      wb = wb,
      sheet_name = sheet_name,
      title = title,
      sub_title = sub_title,
      footer = footer,
      variable_width = variable_width,
      label_width = label_width,
      index_by = index_by
    )

    # Community sheet (optional)
    if (!is.null(community_result)) {
      comm_table <- community_result
      comm_sheet <- "Community Drivers"

      comm_title <- if (!is.null(dv_display)) {
        paste("Community Drivers of", dv_display)
      } else {
        "Community Drivers"
      }

      wb <- append_bn_impact(
        analysis_table = comm_table,
        subgroups = subgroups,
        wb = wb,
        sheet_name = comm_sheet,
        title = comm_title,
        sub_title = sub_title,
        footer = footer,
        variable_width = variable_width,
        label_width = label_width,
        index_by = index_by
      )
    }

    # File name: "{file_name} - Network Drivers of {dv}.xlsx" or "Network Drivers of {dv}.xlsx"
    dv_suffix <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
    fname <- if (!is.null(file_name)) {
      paste0(file_name, " - ", dv_suffix, ".xlsx")
    } else {
      paste0(dv_suffix, ".xlsx")
    }
    file_path <- file.path(path, fname)

    if (add_simple_simulator) {
      if (is.null(bn_obj)) stop("bn_obj is required when add_simple_simulator = TRUE")
      # Extract community nodes from bn_full if available
      comm_nodes <- tryCatch(
        bn_full[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]],
        error = function(e) NULL
      )
      sim_df <- df
      # Resolve brand info for simulator
      sim_brand <- meta[["brand"]]
      sim_brand_names <- if (!is.null(brand_names)) brand_names else meta[["brand_names"]]

      wb <- append_bn_simulator(
        wb = wb, obj = bn_obj, df = sim_df, dv = dv,
        subgroups = subgroups, dictionary = dictionary,
        community_nodes = comm_nodes,
        brand = sim_brand, brand_names = sim_brand_names,
        add_freq_shifts = !is.null(sim_df),
        shift_range = shift_range,
        shift_step = shift_step,
        min_base_for_sim = min_base_for_sim,
        sim_dv_only = sim_dv_only,
        weight = meta[["weight"]]
      )
      # _sim_data and _sim_lookup sheets added — hide them. Only touch
      # visibility on sheets WE added (so external callers like bn_write
      # don't see their own sheets' visibility clobbered).
      sheet_names <- names(wb)
      hide_these <- c("_sim_data", "_sim_pct_data", "_sim_lookup", "_sim_base")
      cur_vis <- openxlsx::sheetVisibility(wb)
      for (sn in setdiff(sheet_names, pre_sheets)) {
        idx <- match(sn, sheet_names)
        cur_vis[idx] <- if (sn %in% hide_these) {
          if (very_hide_all) "veryHidden" else FALSE
        } else TRUE
      }
      openxlsx::sheetVisibility(wb) <- cur_vis
    }

    # Network maps (when full bn object provided). Skipped when
    # add_images = FALSE so bn_write() can add them itself after the
    # prioritization section has been added.
    if (isTRUE(add_images) && !is.null(bn_full)) {
      wb <- append_bn_network_maps(wb = wb, bn_full = bn_full,
        network_type = network_type,
        attribute_font_size = attribute_map_font_size,
        community_font_size = community_map_font_size)
    }

    # Guide tab — added last so it appears as the final tab
    if (isTRUE(add_guide)) {
      wb <- append_bn_impact_guide(
        wb = wb, wb_type = "standard",
        dv_display = dv_display,
        has_weights = has_weights,
        has_community = guide_has_community,
        has_simulator = guide_has_simulator,
        has_brands = guide_has_brands,
        index_by = index_by,
        type = type,
        min_base_for_lift = min_base_for_lift,
        min_base_for_sim = min_base_for_sim,
        boot_applied = isTRUE(meta[["boot_applied"]]),
        n_boot = meta[["n_boot"]],
        mi_boot_applied = isTRUE(meta[["mi_boot_applied"]]),
        mi_boot = meta[["mi_boot"]]
      )
    }

    if (isTRUE(save)) {
      openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
      # Clean up temp dirs from network map PNGs only when we actually saved —
      # otherwise we'd delete files the caller still needs.
      for (td in attr(wb, "tmp_dirs") %||% character(0)) {
        unlink(td, recursive = TRUE)
      }
    }

    invisible(wb)

  } else if (wb_type == "dynamic") {

    if (is.null(wb)) wb <- oxl_create_workbook()
    pre_sheets <- names(wb)

    # Attribute dynamic dashboard
    wb <- append_bn_impact_dynamic(
      wb = wb, table = table, subgroups = subgroups,
      dash_sheet = sheet_name, results_sheet = "Results", lookup_sheet = "_lookup",
      title = title, sub_title = sub_title, engine_footer = engine_footer,
      variable_width = variable_width, community_width = community_width,
      label_width = label_width,
      has_weights = has_weights,
      weighted_results_sheet = if (has_weights) "Results_Weighted" else NULL,
      min_base_for_lift = min_base_for_lift
    )

    # Community dynamic dashboard (optional)
    if (!is.null(community_result)) {
      comm_table <- community_result

      comm_title <- if (!is.null(dv_display)) {
        paste("Community Drivers of", dv_display)
      } else {
        "Community Drivers"
      }
      comm_sheet <- "Community Drivers"

      wb <- append_bn_impact_dynamic(
        wb = wb, table = comm_table, subgroups = subgroups,
        dash_sheet = comm_sheet, results_sheet = "Results_Community",
        lookup_sheet = "_lookup_community",
        title = comm_title, sub_title = sub_title, engine_footer = engine_footer,
        variable_width = variable_width, community_width = community_width,
        label_width = label_width,
        has_weights = !is.null(community_weighted),
        weighted_results_sheet = if (!is.null(community_weighted)) "Results_Community_Weighted" else NULL,
        min_base_for_lift = min_base_for_lift
      )
    }

    # Write weighted Results sheets after dashboards (so dashboards are first)
    if (has_weights) {
      openxlsx::addWorksheet(wb, "Results_Weighted", gridLines = FALSE)
      openxlsx::writeData(wb, "Results_Weighted", table_weighted, startRow = 1, startCol = 1)
    }
    if (!is.null(community_weighted) && !"Results_Community_Weighted" %in% names(wb)) {
      openxlsx::addWorksheet(wb, "Results_Community_Weighted", gridLines = FALSE)
      openxlsx::writeData(wb, "Results_Community_Weighted", community_weighted, startRow = 1, startCol = 1)
    }

    # File name
    dv_suffix <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
    fname <- if (!is.null(file_name)) {
      paste0(file_name, " - ", dv_suffix, ".xlsx")
    } else {
      paste0(dv_suffix, ".xlsx")
    }
    file_path <- file.path(path, fname)

    if (add_simple_simulator) {
      if (is.null(bn_obj)) stop("bn_obj is required when add_simple_simulator = TRUE")
      # Extract community nodes from bn_full if available
      comm_nodes <- tryCatch(
        bn_full[["bn"]][["viz_prep"]][["attribute_viz_prep"]][["nodes"]],
        error = function(e) NULL
      )
      sim_df <- df
      # Resolve brand info for simulator
      sim_brand <- meta[["brand"]]
      sim_brand_names <- if (!is.null(brand_names)) brand_names else meta[["brand_names"]]

      wb <- append_bn_simulator(
        wb = wb, obj = bn_obj, df = sim_df, dv = dv,
        subgroups = subgroups, dictionary = dictionary,
        community_nodes = comm_nodes,
        brand = sim_brand, brand_names = sim_brand_names,
        add_freq_shifts = !is.null(sim_df),
        shift_range = shift_range,
        shift_step = shift_step,
        min_base_for_sim = min_base_for_sim,
        sim_dv_only = sim_dv_only,
        weight = meta[["weight"]]
      )
    }

    # Network maps (when full bn object provided). Skipped when
    # add_images = FALSE so bn_write() can add them itself after the
    # prioritization section has been added.
    if (isTRUE(add_images) && !is.null(bn_full)) {
      wb <- append_bn_network_maps(wb = wb, bn_full = bn_full,
        network_type = network_type,
        attribute_font_size = attribute_map_font_size,
        community_font_size = community_map_font_size)
    }

    # Guide tab — added last so it appears as the final tab
    if (isTRUE(add_guide)) {
      wb <- append_bn_impact_guide(
        wb = wb, wb_type = "dynamic",
        dv_display = dv_display,
        has_weights = has_weights,
        has_community = guide_has_community,
        has_simulator = guide_has_simulator,
        has_brands = guide_has_brands,
        index_by = index_by,
        type = type,
        min_base_for_lift = min_base_for_lift,
        min_base_for_sim = min_base_for_sim,
        boot_applied = isTRUE(meta[["boot_applied"]]),
        n_boot = meta[["n_boot"]],
        mi_boot_applied = isTRUE(meta[["mi_boot_applied"]]),
        mi_boot = meta[["mi_boot"]]
      )
    }

    # Hide helper sheets — only touch visibility on sheets WE added.
    sheet_names <- names(wb)
    hide_sheets <- c("Results", "Results_Community", "Results_Weighted",
      "Results_Community_Weighted", "_sim_data", "_sim_pct_data",
      "_lookup", "_lookup_community", "_sim_lookup", "_sim_base")
    cur_vis <- openxlsx::sheetVisibility(wb)
    for (sn in setdiff(sheet_names, pre_sheets)) {
      idx <- match(sn, sheet_names)
      cur_vis[idx] <- if (sn %in% hide_sheets) {
        if (very_hide_all) "veryHidden" else FALSE
      } else TRUE
    }
    openxlsx::sheetVisibility(wb) <- cur_vis

    if (isTRUE(save)) {
      openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
      # Clean up temp dirs from network map PNGs only when we actually saved —
      # otherwise we'd delete files the caller still needs.
      for (td in attr(wb, "tmp_dirs") %||% character(0)) {
        unlink(td, recursive = TRUE)
      }
    }

    invisible(wb)
  }
}
