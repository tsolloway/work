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
#' @param impact_outcome_display Character or NULL. Initial value of the
#'   impact dashboard's Outcome dropdown — either `"Point Change"` or
#'   `"% Change"`. When NULL (default), auto-detects from the DV type:
#'   dichotomous outcomes get `"Point Change"`, continuous outcomes get
#'   `"% Change"`. Pass an explicit string to override.
#' @param add_impacts_by_battery Logical. When TRUE (default), per-battery
#'   `AD - <name>` dashboards are emitted alongside the main Attribute
#'   Drivers tab — one per battery (and one per battery group) with
#'   within-battery indexing. When FALSE, only the main Attribute Drivers
#'   tab is written. Has no effect when no batteries are defined upstream.
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
    # Static-write display variants. Only consulted when wb_type = "standard"
    # — they pick which precomputed lift column drives the displayed index.
    # `impact_outcome_display = NULL` (default) auto-detects from the DV
    # type: dichotomous → "Point Change"; continuous → "% Change". Explicit
    # values ("Point Change" / "% Change") are respected. Dynamic dashboards
    # still expose these as dropdowns; the initial selected value follows
    # the same auto-detection.
    impact_outcome_display = NULL,
    shift_type      = c("absolute", "proportional", "headroom", "range"),
    path = ".",
    wb = NULL,
    save = TRUE,
    add_guide = TRUE,
    add_images = TRUE,
    # When TRUE (default), per-battery `AD - <name>` dashboards are emitted
    # alongside the main Attribute Drivers tab. When FALSE, only the main
    # tab is written. Has no effect when no batteries are defined upstream.
    add_impacts_by_battery = TRUE,
    # When TRUE (default), impact cell colour-scale uses the brand
    # semantic palette (--ndr-danger / white / --ndr-success) so Excel
    # matches the in-app reactable and bn_report HTML. FALSE falls back
    # to the legacy red / yellow / green Material scale — pass FALSE for
    # legacy workbooks where stakeholders expect the old treatment.
    color_gradient_resondex = TRUE
){

  wb_type <- match.arg(wb_type)
  shift_type <- match.arg(shift_type)

  # Resolve impact_outcome_display: NULL auto-detects from DV type;
  # explicit "Point Change" / "% Change" override. Translate to the
  # internal "absolute" / "proportional" vocabulary used by the engine
  # and downstream column-name plumbing.
  if (is.null(impact_outcome_display)) {
    is_dichotomous <- isTRUE(bn_impact_result[["meta"]][["is_dichotomous_dv"]])
    outcome_display <- if (is_dichotomous) "absolute" else "proportional"
  } else {
    impact_outcome_display <- match.arg(impact_outcome_display,
      c("Point Change", "% Change"))
    outcome_display <- if (impact_outcome_display == "Point Change")
      "absolute" else "proportional"
  }

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
  batteries        <- meta[["batteries"]]
  battery_groups   <- meta[["battery_groups"]]
  has_battery_groups <- !is.null(battery_groups) && length(battery_groups) > 0L
  # Resolve a group name to its union of IVs across the component batteries.
  .group_ivs <- function(group_name) {
    comp <- battery_groups[[group_name]]
    unique(unlist(batteries[comp], use.names = FALSE))
  }
  if (is.null(min_base_for_lift)) min_base_for_lift <- meta[["min_base_for_lift"]]
  # Simulator threshold: inherit from min_base_for_lift when not supplied
  if (is.null(min_base_for_sim)) min_base_for_sim <- min_base_for_lift %||% 75

  # ---------------------------------------------------------------------------
  # Battery helpers (used by both static and dynamic write paths)
  # ---------------------------------------------------------------------------
  has_batteries <- !is.null(batteries) && length(batteries) > 0L

  # IV → battery name lookup. Built once for reuse.
  iv_to_battery <- if (has_batteries) {
    iv2b <- character(0)
    for (b_name in names(batteries)) {
      ivs_in_b <- batteries[[b_name]]
      iv2b <- c(iv2b, rlang::set_names(rep(b_name, length(ivs_in_b)), ivs_in_b))
    }
    iv2b
  } else NULL

  # Inject a Battery column into a table just before any subgroup column.
  # Position: after Community when present, otherwise after Variable.
  .inject_battery_col <- function(tbl) {
    if (!has_batteries || !"Variable" %in% names(tbl)) return(tbl)
    tbl$Battery <- ifelse(
      tbl$Variable %in% names(iv_to_battery),
      iv_to_battery[tbl$Variable],
      ""
    )
    if ("Community" %in% names(tbl)) {
      tbl <- tbl %>% dplyr::relocate(Battery, .after = "Community")
    } else {
      tbl <- tbl %>% dplyr::relocate(Battery, .after = "Variable")
    }
    tbl
  }

  # Resolve the raw-metric column suffix for the current index_by, scoped
  # by the user's outcome_display / shift_type choices. Returned suffix is
  # what we'd append to a subgroup name to land on the precomputed raw
  # column, e.g. "_lift_0_absshift_absdisplay" for lift indexes.
  display_tag <- if (outcome_display == "absolute") "absdisplay" else "propdisplay"
  shift_tag   <- if (shift_type      == "absolute") "absshift"   else "propshift"

  .resolve_metric_suffix <- function(tbl) {
    sg1 <- subgroups[[1]]
    if (index_by %in% c("lift_first", "lift_second")) {
      # Discover which lift_N levels exist in the table (e.g. lift_0, lift_10).
      # We match against the trailing display tag because that's universal —
      # every lift column ends with `_propdisplay` or `_absdisplay` regardless
      # of shift tag presence.
      lift_pat <- paste0("^", sg1, "_lift_(\\d+)_(propshift|absshift)_(propdisplay|absdisplay)$")
      m <- regmatches(names(tbl), regexpr(lift_pat, names(tbl)))
      if (length(m) == 0) {
        # Fallback to old (pre-Pass-B) shape: lift_N_propdisplay/absdisplay.
        legacy_pat <- paste0("^", sg1, "_lift_(\\d+)_(propdisplay|absdisplay)$")
        m <- regmatches(names(tbl), regexpr(legacy_pat, names(tbl)))
        if (length(m) == 0) return("_maxVmin_absdisplay")
        ns <- sort(unique(as.integer(sub(legacy_pat, "\\1", m))))
        target_n <- if (index_by == "lift_first") ns[1] else ns[min(2L, length(ns))]
        return(paste0("_lift_", target_n, "_", display_tag))
      }
      ns <- sort(unique(as.integer(sub(lift_pat, "\\1", m))))
      target_n <- if (index_by == "lift_first") ns[1] else ns[min(2L, length(ns))]
      paste0("_lift_", target_n, "_", shift_tag, "_", display_tag)
    } else if (index_by == "maxVmin") {
      # maxVmin is shift-invariant — only display variants exist.
      paste0("_maxVmin_", display_tag)
    } else if (index_by == "mi") {
      "_mi"
    } else {
      "_maxVmin"
    }
  }

  # For the static "main" sheet we also need to overwrite each subgroup's
  # `index`-aliased display column so it reflects the user's outcome /
  # shift choices. The engine bakes in a default index (prop-shift,
  # abs-display) — this rewrites it to match `outcome_display` x `shift_type`.
  .reindex_main_table <- function(tbl) {
    suffix <- .resolve_metric_suffix(tbl)
    for (sg in subgroups) {
      raw_col <- paste0(sg, suffix)
      if (!raw_col %in% names(tbl)) next
      raw_vals <- tbl[[raw_col]]
      denom <- mean(abs(raw_vals), na.rm = TRUE)
      tbl[[sg]] <- if (is.finite(denom) && denom > 0) {
        abs(raw_vals) / denom * 100
      } else {
        NA_real_
      }
    }
    tbl
  }

  # For a given battery: filter to its rows, then recompute each subgroup's
  # display column as within-battery index. Returns a fresh table ready to
  # hand to append_bn_impact.
  .per_battery_table <- function(tbl, battery_ivs, metric_suffix) {
    sub_tbl <- tbl %>%
      dplyr::filter(.data[["Variable"]] %in% battery_ivs)
    if (nrow(sub_tbl) == 0L) return(NULL)
    for (sg in subgroups) {
      raw_col <- paste0(sg, metric_suffix)
      if (!raw_col %in% names(sub_tbl)) next
      raw_vals <- sub_tbl[[raw_col]]
      denom <- mean(abs(raw_vals), na.rm = TRUE)
      sub_tbl[[sg]] <- if (is.finite(denom) && denom > 0) {
        abs(raw_vals) / denom * 100
      } else {
        NA_real_
      }
    }
    sub_tbl
  }

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
      "Indexed by average effect. Measures the outcome's sensitivity to a small symmetric perturbation around each attribute's current state"
    } else {
      pct <- round(lift_val * 100)
      paste0(
        "Indexed by ", pct, "% improvement. Measures how much the outcome changes when each attribute's distribution shifts by ", pct, "%"
      )
    }
  } else {
    switch(index_by,
      "maxVmin" = "Indexed by best-vs-worst effect. Measures the outcome difference between the top of each attribute versus the bottom",
      "mi"      = "Indexed by explanatory value. Measures the statistical strength of the relationship between each attribute and the outcome (mutual information), independent of intervention direction or shift type",
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

    # Inject Battery column into the main table when batteries are defined.
    # The static main sheet keeps GLOBAL indexing — battery is just a label
    # the user can sort/filter by. Per-battery within-battery-indexed sheets
    # come below.
    main_table <- .inject_battery_col(table)
    # Override the engine-baked index with one keyed off the user's
    # outcome_display × shift_type choice.
    main_table <- .reindex_main_table(main_table)

    wb <- append_bn_impact(
      analysis_table = main_table,
      subgroups = subgroups,
      wb = wb,
      sheet_name = sheet_name,
      title = title,
      sub_title = sub_title,
      footer = footer,
      variable_width = variable_width,
      label_width = label_width,
      index_by = index_by,
      color_gradient_resondex = color_gradient_resondex
    )

    # Per-battery sheets (optional, static path only). One sheet per battery
    # showing only that battery's rows, with the subgroup index columns
    # recomputed against the within-battery mean. Sheet names are truncated
    # to Excel's 31-character limit to avoid silent truncation collisions.
    if (has_batteries) {
      metric_suffix <- .resolve_metric_suffix(table)
      for (b_name in names(batteries)) {
        b_tbl <- .per_battery_table(main_table, batteries[[b_name]], metric_suffix)
        if (is.null(b_tbl)) next
        # "Attribute Drivers - <battery>" trimmed to 31 chars.
        prefix <- "AD - "
        max_name_len <- 31L - nchar(prefix)
        b_short <- if (nchar(b_name) > max_name_len) {
          substr(b_name, 1L, max_name_len)
        } else b_name
        b_sheet <- paste0(prefix, b_short)
        b_title <- if (!is.null(dv_display)) {
          paste0("Attribute Drivers of ", dv_display, " — ", b_name, " (within-battery index)")
        } else {
          paste0("Attribute Drivers — ", b_name, " (within-battery index)")
        }
        wb <- append_bn_impact(
          analysis_table = b_tbl,
          subgroups = subgroups,
          wb = wb,
          sheet_name = b_sheet,
          title = b_title,
          sub_title = sub_title,
          footer = footer,
          variable_width = variable_width,
          label_width = label_width,
          index_by = index_by
        )
      }
    }

    # Per-group sheets — same shape as per-battery sheets, but the IV set
    # is the union of the group's component batteries.
    if (has_battery_groups) {
      metric_suffix <- .resolve_metric_suffix(table)
      for (g_name in names(battery_groups)) {
        g_tbl <- .per_battery_table(main_table, .group_ivs(g_name), metric_suffix)
        if (is.null(g_tbl)) next
        prefix <- "AD - "
        max_name_len <- 31L - nchar(prefix)
        g_short <- if (nchar(g_name) > max_name_len) substr(g_name, 1L, max_name_len) else g_name
        g_sheet <- paste0(prefix, g_short)
        g_title <- if (!is.null(dv_display)) {
          paste0("Attribute Drivers of ", dv_display, " — ", g_name, " (within-group index)")
        } else {
          paste0("Attribute Drivers — ", g_name, " (within-group index)")
        }
        wb <- append_bn_impact(
          analysis_table = g_tbl,
          subgroups = subgroups,
          wb = wb,
          sheet_name = g_sheet,
          title = g_title,
          sub_title = sub_title,
          footer = footer,
          variable_width = variable_width,
          label_width = label_width,
          index_by = index_by
        )
      }
    }

    # Community sheet (optional). Same outcome_display / shift_type
    # override applies — keeps the community-level index aligned with
    # the attribute-level one.
    if (!is.null(community_result)) {
      comm_table <- .reindex_main_table(community_result)
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
      # Simulator helper sheets added — hide them. Only touch
      # visibility on sheets WE added (so external callers like bn_write
      # don't see their own sheets' visibility clobbered).
      sheet_names <- names(wb)
      hide_these <- c("_sim_data_wide", "_sim_data_prior", "_sim_shifted_probs",
                      "_sim_lookup", "_sim_base")
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

    # Attribute dynamic dashboard (main, all rows, global index). Pass
    # `battery_groups` here too so the BatteryGroup_<name> helper columns
    # are injected onto the Results sheet on this first call (subsequent
    # per-group tabs reference those columns and won't get a chance to
    # write them themselves — write_helper_sheets = FALSE there).
    wb <- append_bn_impact_dynamic(
      wb = wb, table = table, subgroups = subgroups,
      dash_sheet = sheet_name, results_sheet = "Results", lookup_sheet = "_lookup",
      title = title, sub_title = sub_title, engine_footer = engine_footer,
      variable_width = variable_width, community_width = community_width,
      label_width = label_width,
      has_weights = has_weights,
      weighted_results_sheet = if (has_weights) "Results_Weighted" else NULL,
      min_base_for_lift = min_base_for_lift,
      batteries = batteries,
      battery_groups = battery_groups,
      outcome_display = outcome_display, shift_type = shift_type,
      color_gradient_resondex = color_gradient_resondex
    )

    # Per-battery dashboards. Each tab shows only its battery's IVs and
    # indexes within that battery; they share the Results/_lookup sheets
    # written by the main call above (write_helper_sheets = FALSE). Sheet
    # name truncated to Excel's 31-character limit. Gated by
    # `add_impacts_by_battery`.
    if (isTRUE(add_impacts_by_battery) &&
        !is.null(batteries) && length(batteries) > 0L) {
      for (b_name in names(batteries)) {
        prefix <- "AD - "
        max_name_len <- 31L - nchar(prefix)
        b_short <- if (nchar(b_name) > max_name_len) substr(b_name, 1L, max_name_len) else b_name
        b_sheet <- paste0(prefix, b_short)
        b_title <- if (!is.null(dv_display)) {
          paste0("Attribute Drivers of ", dv_display, " — ", b_name, " (within-battery index)")
        } else {
          paste0("Attribute Drivers — ", b_name, " (within-battery index)")
        }
        wb <- append_bn_impact_dynamic(
          wb = wb, table = table, subgroups = subgroups,
          dash_sheet = b_sheet, results_sheet = "Results", lookup_sheet = "_lookup",
          title = b_title, sub_title = sub_title, engine_footer = engine_footer,
          variable_width = variable_width, community_width = community_width,
          label_width = label_width,
          has_weights = has_weights,
          weighted_results_sheet = if (has_weights) "Results_Weighted" else NULL,
          min_base_for_lift = min_base_for_lift,
          batteries = batteries,
          battery_groups = battery_groups,
          battery_filter = b_name,
          write_helper_sheets = FALSE,
          outcome_display = outcome_display, shift_type = shift_type,
          color_gradient_resondex = color_gradient_resondex
        )
      }
    }

    # Per-group dashboards. Each tab shows the union of its component
    # batteries' IVs and indexes within that union. Same shared Results
    # /_lookup sheets as the per-battery tabs. Gated by
    # `add_impacts_by_battery` — same toggle controls both per-battery
    # and per-group tabs (they're conceptually the same family of views).
    if (isTRUE(add_impacts_by_battery) && has_battery_groups) {
      for (g_name in names(battery_groups)) {
        prefix <- "AD - "
        max_name_len <- 31L - nchar(prefix)
        g_short <- if (nchar(g_name) > max_name_len) substr(g_name, 1L, max_name_len) else g_name
        g_sheet <- paste0(prefix, g_short)
        g_title <- if (!is.null(dv_display)) {
          paste0("Attribute Drivers of ", dv_display, " — ", g_name, " (within-group index)")
        } else {
          paste0("Attribute Drivers — ", g_name, " (within-group index)")
        }
        wb <- append_bn_impact_dynamic(
          wb = wb, table = table, subgroups = subgroups,
          dash_sheet = g_sheet, results_sheet = "Results", lookup_sheet = "_lookup",
          title = g_title, sub_title = sub_title, engine_footer = engine_footer,
          variable_width = variable_width, community_width = community_width,
          label_width = label_width,
          has_weights = has_weights,
          weighted_results_sheet = if (has_weights) "Results_Weighted" else NULL,
          min_base_for_lift = min_base_for_lift,
          batteries = batteries,
          battery_groups = battery_groups,
          battery_filter = g_name,
          write_helper_sheets = FALSE,
          outcome_display = outcome_display, shift_type = shift_type,
          color_gradient_resondex = color_gradient_resondex
        )
      }
    }

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
        min_base_for_lift = min_base_for_lift,
        outcome_display = outcome_display, shift_type = shift_type,
        color_gradient_resondex = color_gradient_resondex
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
      "Results_Community_Weighted",
      "_sim_data_wide", "_sim_data_prior", "_sim_shifted_probs",
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
