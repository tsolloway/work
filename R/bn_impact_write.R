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
#' @param file_name Character. Prefix for output file name. File is saved as
#'   \code{{file_name} - Network Drivers of {dv}.xlsx}. If NULL, inherits from
#'   \code{sub_title}. If both are NULL, file is saved as
#'   \code{Network Drivers of {dv}.xlsx}.
#' @param sub_title Character. Subtitle text (e.g. project name). If NULL,
#'   inherits from \code{file_name}. Default NULL.
#' @param title Character. Title displayed in the sheet header. If NULL,
#'   defaults to \code{"Network Drivers of {dv}"} using the DV display name.
#' @param variable_width Column width for the Variable column (default 20).
#' @param label_width Column width for the Label column (default \code{"auto"}).
#' @param wb_type Character. Workbook type: \code{"standard"} (default) or
#'   \code{"dynamic"}.
#' @param add_simple_simulator Logical. If TRUE, adds a Simulator sheet with
#'   interactive dropdowns for exploring conditional probability distributions.
#'   Requires \code{bn_obj} to be provided. Default FALSE.
#' @param bn_obj The BN object. Accepts either the full output of
#'   \code{bn_finalize_network()} or just the \code{bn_subgroups} element.
#'   When the full object is provided, network maps are automatically added
#'   and \code{bn_subgroups} is extracted for the simulator.
#' @param dictionary Optional. A data frame or named object for variable labels.
#'   Passed to the simulator for labeling target variables.
#' @param path Directory to write workbook to (default \code{"."}).
#'
#' @return Workbook object (invisibly).
#'
#' @export
bn_impact_write <- function(
    bn_impact_result,
    file_name = NULL,
    sub_title = NULL,
    title = NULL,
    variable_width = 20,
    community_width = 20,
    label_width = "auto",
    wb_type = c("standard", "dynamic"),
    add_simple_simulator = FALSE,
    bn_obj = NULL,
    df = NULL,
    brand_names = NULL,
    shift_range = c(-0.50, 0.50),
    shift_step = 0.025,
    network_type = "none",
    dictionary = NULL,
    path = "."
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
  subgroups <- meta[["subgroups"]]
  index_by  <- meta[["index_by"]]
  type      <- meta[["type"]]
  dv        <- meta[["dv"]]

  # Use name of dv if named, otherwise the value itself
dv_display <- if (!is.null(names(dv))) names(dv) else dv

  # Title
  if (is.null(title)) {
    title <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
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

  # Sheet name
  sheet_name <- if (!is.null(dv_display)) paste("Network Drivers of", dv_display) else "Network Drivers"
  if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)

  if (wb_type == "standard") {

    wb <- oxl_create_workbook()

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
      comm_sheet <- if (!is.null(dv_display)) {
        paste("Community Drivers of", dv_display)
      } else {
        "Community Drivers"
      }
      if (nchar(comm_sheet) > 31) comm_sheet <- substr(comm_sheet, 1, 31)

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
        shift_step = shift_step
      )
      # _sim_data and _sim_lookup sheets added — hide them
      n_sheets <- length(names(wb))
      vis <- rep(TRUE, n_sheets)
      sheet_names <- names(wb)
      for (si in seq_along(sheet_names)) {
        if (sheet_names[si] %in% c("_sim_data", "_sim_pct_data", "_sim_lookup")) vis[si] <- FALSE
      }
      openxlsx::sheetVisibility(wb) <- vis
    }

    # Network maps (when full bn object provided)
    if (!is.null(bn_full)) {
      wb <- append_bn_network_maps(wb = wb, bn_full = bn_full, network_type = network_type)
    }

    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

    # Clean up temp dirs from network map PNGs
    for (td in attr(wb, "tmp_dirs") %||% character(0)) {
      unlink(td, recursive = TRUE)
    }

    invisible(wb)

  } else if (wb_type == "dynamic") {

    wb <- oxl_create_workbook()

    # Attribute dynamic dashboard
    wb <- append_bn_impact_dynamic(
      wb = wb, table = table, subgroups = subgroups,
      dash_sheet = sheet_name, results_sheet = "Results", lookup_sheet = "_lookup",
      title = title, sub_title = sub_title, engine_footer = engine_footer,
      variable_width = variable_width, community_width = community_width,
      label_width = label_width,
      has_weights = has_weights,
      weighted_results_sheet = if (has_weights) "Results_Weighted" else NULL
    )

    # Community dynamic dashboard (optional)
    if (!is.null(community_result)) {
      comm_table <- community_result

      comm_title <- if (!is.null(dv_display)) {
        paste("Community Drivers of", dv_display)
      } else {
        "Community Drivers"
      }
      comm_sheet <- comm_title
      if (nchar(comm_sheet) > 31) comm_sheet <- substr(comm_sheet, 1, 31)

      wb <- append_bn_impact_dynamic(
        wb = wb, table = comm_table, subgroups = subgroups,
        dash_sheet = comm_sheet, results_sheet = "Results_Community",
        lookup_sheet = "_lookup_community",
        title = comm_title, sub_title = sub_title, engine_footer = engine_footer,
        variable_width = variable_width, community_width = community_width,
        label_width = label_width,
        has_weights = !is.null(community_weighted),
        weighted_results_sheet = if (!is.null(community_weighted)) "Results_Community_Weighted" else NULL
      )
    }

    # Write weighted Results sheets after dashboards (so dashboards are first)
    if (has_weights) {
      openxlsx::addWorksheet(wb, "Results_Weighted")
      openxlsx::writeData(wb, "Results_Weighted", table_weighted, startRow = 1, startCol = 1)
    }
    if (!is.null(community_weighted) && !"Results_Community_Weighted" %in% names(wb)) {
      openxlsx::addWorksheet(wb, "Results_Community_Weighted")
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
        shift_step = shift_step
      )
    }

    # Network maps (when full bn object provided)
    if (!is.null(bn_full)) {
      wb <- append_bn_network_maps(wb = wb, bn_full = bn_full, network_type = network_type)
    }

    # Hide helper sheets — set visibility for all sheets
    # Set visibility — all helper sheets hidden (no veryHidden to avoid coercion)
    sheet_names <- names(wb)
    hide_sheets <- c("Results", "Results_Community", "Results_Weighted",
      "Results_Community_Weighted", "_sim_data", "_sim_pct_data",
      "_lookup", "_lookup_community", "_sim_lookup")

    vis <- rep(TRUE, length(sheet_names))
    for (si in seq_along(sheet_names)) {
      if (sheet_names[si] %in% hide_sheets) vis[si] <- FALSE
    }
    openxlsx::sheetVisibility(wb) <- vis

    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

    # Clean up temp dirs from network map PNGs
    for (td in attr(wb, "tmp_dirs") %||% character(0)) {
      unlink(td, recursive = TRUE)
    }

    invisible(wb)
  }
}
