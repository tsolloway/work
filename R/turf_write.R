#' turf_write
#' @description Writes TURF analysis results to an interactive macro-enabled
#'   Excel workbook (.xlsm) with multi-sheet architecture:
#'   - Dashboard: controls + greedy build chart + items panel
#'   - Best Combos: linked controls + combo results table
#'   - Greedy (hidden): greedy results table + chart staging data
#'   - _controls (hidden): single source of truth for dropdown states
#'   Requires a VBA template (turf_template.xlsm) with Dashboard + Best Combos
#'   sheets and all VBA modules pre-loaded.
#'
#' @param best_combo_results Optional. Output from \code{turf_best_combo()}.
#'   Accepts any of the 4 return shapes (single tibble, named list by n,
#'   named list by subgroup, nested list). If \code{NULL}, the Best Combos
#'   sheet is created with dummy controls and hidden — the Dashboard and
#'   greedy functionality still work without it.
#' @param raw Data frame. The original respondent-level data frame containing
#'   binary item columns, weight column, subgroup columns, and respondent IDs.
#' @param vars Character vector. Names of the binary item columns in \code{raw}.
#' @param subgroups Character vector. Names of subgroup columns in \code{raw}
#'   (binary 0/1 indicators). If NULL and turf_best_combo() was run without subgroups,
#'   a "Total" column of all 1s is created.
#' @param weight Character. Name of the weight column in \code{raw}. NULL if
#'   unweighted.
#' @param respondent_id Character. Name of the respondent ID column in
#'   \code{raw}. If NULL, row numbers are used.
#' @param labels Data frame with columns \code{variable} and \code{label}, or
#'   a named character vector mapping variable names to labels.
#' @param top Integer. Maximum number of combos to write per subgroup x combo
#'   size. Default \code{50000}.
#' @param file_name Character. File name (without extension). Default
#'   \code{"TURF_Analysis"}.
#' @param project_name Character. Project name displayed in row 3 of Dashboard
#'   and Best Combos sheets. Default \code{"Project Name - (#xxxxxxx)"}.
#' @param where Character. Directory to save to. Default \code{getwd()}.
#' @param template Character. Path to the VBA template .xlsm file. Default
#'   uses the bundled template from the work package, falling back to
#'   \code{turf_template.xlsm} in the working directory.
#' @param return_location Logical. If \code{TRUE}, returns the file path.
#'
#' @return File path (invisibly) if \code{return_location = TRUE}.
#'
#' @examples
#' \dontrun{
#' turf_results <- turf_best_combo(
#'   df        = example_data_ice_cream,
#'   vars      = example_data_ice_cream_dictionary$variable,
#'   n         = 1:3,
#'   subgroups = c("Total", "Gen_Z", "Millennials", "Gen_X"),
#'   labels    = example_data_ice_cream_dictionary,
#'   weight    = "weight"
#' )
#'
#' turf_write(
#'   best_combo_results = turf_results,
#'   raw                = example_data_ice_cream,
#'   vars               = example_data_ice_cream_dictionary$variable,
#'   subgroups          = c("Total", "Gen_Z", "Millennials", "Gen_X"),
#'   weight             = "weight",
#'   labels             = example_data_ice_cream_dictionary,
#'   file_name          = "example_ice_cream_turf"
#' )
#' }
#'
#' @export
turf_write <- function(
    best_combo_results = NULL, raw, vars,
    subgroups = NULL, weight = NULL, respondent_id = NULL,
    labels = NULL, top = 50000,
    file_name = "TURF_Analysis",
    project_name = "Project Name - (#xxxxxxx)",
    where = NULL, template = NULL,
    sig_threshold = 0.10,
    marginal_threshold = 0.20,
    very_hidden = FALSE,
    return_location = TRUE
) {

  # best_combo_results = foo
  # raw = df_turf
  # vars = df_turf %>% select(starts_with(c("PAExp_TravelExperience", ...))) %>% names()
  # subgroups = c("Total", "Male", "Female")
  # weight = "weight"
  # respondent_id = NULL
  # labels = NULL
  # top = 50000
  # file_name = "TURF_Analysis"
  # where = NULL
  # template = NULL
  # return_location = TRUE

  require(openxlsx2)

  if(is.null(where)) where <- getwd()


  # ---- Resolve label lookup ----
  label_lookup <- .turf_build_label_lookup(vars, labels)


  # ---- Normalize turf_best_combo() output (if provided) ----
  has_combos <- !is.null(best_combo_results)

  if(has_combos){
    normalized <- .turf_normalize(best_combo_results)
    subgroup_names <- names(normalized)
    n_keys <- names(normalized[[1]])
    n_values <- as.integer(gsub("^n_", "", n_keys))

    # Detect features from first tibble
    first_tbl <- normalized[[1]][[1]]
    col_info <- .turf_detect_columns(first_tbl)

    # Detect if subgroups were actually provided (vs auto-wrapped "Total")
    has_subgroups <- !(.turf_detect_shape(best_combo_results) %in% c("single", "multi_n"))
  } else {
    # No combo results — derive subgroup names from the subgroups param
    if(!is.null(subgroups)){
      subgroup_names <- subgroups
      has_subgroups <- TRUE
    } else {
      subgroup_names <- "Total"
      has_subgroups <- FALSE
    }
    n_values <- integer(0)
    col_info <- list(has_labels = FALSE, has_weights = !is.null(weight),
                     item_cols = character(0), label_cols = character(0), n_items = 0)
  }

  # weight param overrides: if weight = NULL, hide weight controls
  if(is.null(weight)) col_info$has_weights <- FALSE

  n_items <- length(vars)


  # ---- Compute base sizes per subgroup ----
  base_sizes <- .turf_compute_bases(raw, subgroups, subgroup_names)


  # ---- Create workbook (load from VBA template if available) ----
  # Strategy: wb_load() preserves VBA project bindings correctly.
  # Template must have "Dashboard" (Sheet1) and "Best Combos" (Sheet2)
  # with their respective Worksheet_Change event handlers.
  if(is.null(template)){
    template <- system.file("turf_template.xlsm", package = "work")
    if(template == ""){
      template <- file.path(getwd(), "turf_template.xlsm")
    }
  }

  has_vba <- FALSE
  if(file.exists(template)){
    wb <- tryCatch({
      loaded <- wb_load(template)
      has_vba <- TRUE
      cli::cli_alert_info("VBA template loaded: {template}")
      loaded
    }, error = function(e){
      cli::cli_alert_warning("Failed to load template: {e$message}. Creating without macros.")
      wb_workbook()
    })
  } else {
    cli::cli_alert_warning("VBA template not found at {template}. Saving without macros.")
    wb <- wb_workbook()
  }


  # ---- Write _raw sheet ----
  .turf_write_raw_sheet(wb, raw, vars, subgroups, subgroup_names, weight, respondent_id)


  # ---- Write hidden combo data sheets ----
  if(has_combos){
    for(sg in subgroup_names){
      for(nk in n_keys){
        tbl <- normalized[[sg]][[nk]]
        n_val <- gsub("^n_", "", nk)

        for(sort_by in c("reach", "freq")){
          sheet_name <- paste0("d_", sg, "_", n_val, "_", sort_by)
          prepared <- .turf_prepare_sheet(tbl, top, col_info, sort_by = sort_by)

          wb$add_worksheet(sheet_name)
          wb$add_data(sheet_name, x = prepared)
        }
      }
    }
  }


  # ---- Write _config sheet ----
  .turf_write_config(wb, subgroup_names, n_values, col_info, has_subgroups,
                     top, n_items, vars, label_lookup, base_sizes,
                     sig_threshold = sig_threshold,
                     marginal_threshold = marginal_threshold)


  # ---- Write _controls sheet ----
  .turf_write_controls(wb, subgroup_names, n_values, col_info, base_sizes)


  # ---- Write Greedy sheet (hidden) ----
  .turf_write_greedy_sheet(wb, n_items)


  # ---- Write _best_combo_charts sheet (hidden, chart staging data) ----
  # ---- Write _best_combo_charts sheet (hidden, chart staging data) ----
  wb$add_worksheet("_best_combo_charts")
  wb$add_data("_best_combo_charts", x = "label", dims = "A1")
  wb$add_data("_best_combo_charts", x = "value", dims = "B1")
  wb$add_data("_best_combo_charts", x = "value2", dims = "C1")


  # ---- Build Dashboard sheet ----
  if(!"Dashboard" %in% wb$get_sheet_names()){
    wb$add_worksheet("Dashboard", grid_lines = FALSE)
  } else {
    wb$set_sheetview(sheet = "Dashboard", showGridLines = FALSE)
  }

  .turf_write_dashboard(
    wb, subgroup_names, n_values, col_info,
    n_items, vars, label_lookup, base_sizes,
    project_name = project_name,
    sig_threshold = sig_threshold,
    marginal_threshold = marginal_threshold
  )


  # ---- Build Best Combos sheet ----
  if(!"Best Combos" %in% wb$get_sheet_names()){
    wb$add_worksheet("Best Combos", grid_lines = FALSE)
  } else {
    wb$set_sheetview(sheet = "Best Combos", showGridLines = FALSE)
  }

  .turf_write_best_combos(
    wb, subgroup_names, n_values, col_info, base_sizes,
    project_name = project_name
  )


  # ---- Reorder sheets: Dashboard first, Best Combos second (if present), then everything else ----
  sheet_names <- wb$get_sheet_names()
  dash_idx <- which(sheet_names == "Dashboard")
  bc_idx <- which(sheet_names == "Best Combos")
  others <- setdiff(seq_along(sheet_names), c(dash_idx, bc_idx))
  new_order <- c(dash_idx, bc_idx, others)
  wb$set_order(new_order)


  # ---- Hide non-user sheets ----
  hide_level <- if(very_hidden) "veryHidden" else "hidden"
  sheet_names <- wb$get_sheet_names()
  for(sn in sheet_names){
    if(grepl("^d_|^_config$|^_raw$|^_controls$|^Greedy$|^_best_combo_charts$", sn)){
      wb$set_sheet_visibility(sheet = sn, value = hide_level)
    }
  }

  # Hide Best Combos if no combo results provided
  if(!has_combos){
    wb$set_sheet_visibility(sheet = "Best Combos", value = hide_level)
  }


  # ---- Save ----
  ext <- if(has_vba) "xlsm" else "xlsx"
  file_location <- glue::glue("{where}/{file_name}.{ext}")
  wb$save(file_location)

  cli::cli_alert_success("TURF workbook saved: {file_location}")

  if(return_location) return(invisible(file_location))
}


# =============================================================================
# Input normalization helpers
# =============================================================================

.turf_detect_shape <- function(df){
  if(is.data.frame(df)) return("single")

  first <- df[[1]]

  if(is.data.frame(first) && all(grepl("^n_\\d+$", names(df)))) return("multi_n")

  if(is.data.frame(first)) return("subgroup_single_n")

  if(is.list(first)) return("subgroup_multi_n")

  stop("Unrecognized turf_best_combo() output structure.")
}


.turf_infer_n_key <- function(tbl){
  n_items <- sum(grepl("^item_\\d+$", names(tbl)))
  paste0("n_", n_items)
}


.turf_normalize <- function(df){
  shape <- .turf_detect_shape(df)

  switch(shape,
    "single" = {
      nk <- .turf_infer_n_key(df)
      list(Total = setNames(list(df), nk))
    },
    "multi_n" = list(Total = df),
    "subgroup_single_n" = purrr::map(df, function(tbl){
      nk <- .turf_infer_n_key(tbl)
      setNames(list(tbl), nk)
    }),
    "subgroup_multi_n" = df
  )
}


.turf_detect_columns <- function(tbl){
  nms <- names(tbl)
  list(
    has_labels  = any(grepl("^label_\\d+$", nms)),
    has_weights = "w_reach_pct" %in% nms,
    item_cols   = grep("^item_\\d+$", nms, value = TRUE),
    label_cols  = grep("^label_\\d+$", nms, value = TRUE),
    n_items     = sum(grepl("^item_\\d+$", nms))
  )
}


.turf_build_label_lookup <- function(vars, labels){
  if(is.null(labels)){
    return(setNames(vars, vars))
  }
  if(is.data.frame(labels)){
    lookup <- setNames(labels$label, labels$variable)
  } else if(is.character(labels) && !is.null(names(labels))){
    lookup <- labels
  } else {
    return(setNames(vars, vars))
  }
  missing <- setdiff(vars, names(lookup))
  if(length(missing) > 0){
    lookup[missing] <- missing
  }
  lookup[vars]
}


.turf_compute_bases <- function(raw, subgroups, subgroup_names){
  if(is.null(subgroups) || length(subgroups) == 0){
    return(c(Total = nrow(raw)))
  }
  bases <- purrr::map_int(subgroup_names, function(sg){
    if(sg == "Total"){
      nrow(raw)
    } else if(sg %in% names(raw)){
      sum(raw[[sg]] == 1, na.rm = TRUE)
    } else {
      nrow(raw)
    }
  })
  setNames(bases, subgroup_names)
}


# =============================================================================
# Data preparation
# =============================================================================

.turf_prepare_sheet <- function(tbl, top, col_info, sort_by = "reach"){

  if(sort_by == "freq"){
    tbl <- tbl %>% dplyr::arrange(-freq_avg, -reach_pct)
  } else {
    tbl <- tbl %>% dplyr::arrange(-reach_pct, -freq_avg)
  }

  tbl <- tbl %>%
    dplyr::slice_head(n = top) %>%
    dplyr::mutate(rank = dplyr::row_number())

  if(col_info$has_labels){
    label_cols <- grep("^label_\\d+$", names(tbl), value = TRUE)
    tbl$item_label <- apply(tbl[, label_cols, drop = FALSE], 1, function(x){
      paste(x[!is.na(x)], collapse = " + ")
    })
  } else {
    item_cols <- grep("^item_\\d+$", names(tbl), value = TRUE)
    tbl$item_label <- apply(tbl[, item_cols, drop = FALSE], 1, function(x){
      paste(x[!is.na(x)], collapse = " + ")
    })
  }

  out <- tbl %>% dplyr::select(rank, item_label, reach_pct, freq_avg)

  if(col_info$has_weights){
    out$w_reach_pct <- tbl$w_reach_pct
    out$w_freq_avg <- tbl$w_freq_avg
  }

  item_cols <- grep("^item_\\d+$", names(tbl), value = TRUE)
  for(ic in item_cols){
    out[[ic]] <- tbl[[ic]]
  }

  out
}


# =============================================================================
# Hidden sheet writers
# =============================================================================

.turf_write_raw_sheet <- function(wb, raw, vars, subgroups, subgroup_names, weight, respondent_id){

  wb$add_worksheet("_raw")

  n_resp <- nrow(raw)
  raw_list <- list()

  if(!is.null(respondent_id) && respondent_id %in% names(raw)){
    raw_list[["resp_id"]] <- raw[[respondent_id]]
  } else {
    raw_list[["resp_id"]] <- seq_len(n_resp)
  }

  if(!is.null(weight) && weight %in% names(raw)){
    raw_list[["weight"]] <- raw[[weight]]
  } else {
    raw_list[["weight"]] <- rep(1, n_resp)
  }

  for(sg in subgroup_names){
    if(sg == "Total"){
      raw_list[[paste0("sg_", sg)]] <- rep(1L, n_resp)
    } else if(sg %in% names(raw)){
      raw_list[[paste0("sg_", sg)]] <- as.integer(raw[[sg]])
    } else {
      raw_list[[paste0("sg_", sg)]] <- rep(1L, n_resp)
    }
  }

  for(v in vars){
    raw_list[[v]] <- as.integer(raw[[v]])
  }

  raw_out <- as.data.frame(raw_list, stringsAsFactors = FALSE)

  wb$add_data("_raw", x = raw_out)
}


.turf_write_config <- function(wb, subgroup_names, n_values, col_info,
                                has_subgroups, top, n_items, vars,
                                label_lookup, base_sizes,
                                sig_threshold = 0.10, marginal_threshold = 0.20){

  wb$add_worksheet("_config")

  # ---- Key-value config pairs: title in A, value in B ----
  config_titles <- c("subgroup_names", "n_values", "has_weights", "has_labels",
                     "has_subgroups", "top", "n_items", "sig_threshold",
                     "marginal_threshold")
  config_values <- c(
    paste(subgroup_names, collapse = ","),
    paste(n_values, collapse = ","),
    as.character(col_info$has_weights),
    as.character(col_info$has_labels),
    as.character(has_subgroups),
    as.character(top),
    as.character(n_items),
    as.character(sig_threshold),
    as.character(marginal_threshold)
  )
  for (i in seq_along(config_titles)) {
    wb$add_data("_config", x = config_titles[i], dims = paste0("A", i))
    wb$add_data("_config", x = config_values[i], dims = paste0("B", i))
  }
  # Write numeric values so VBA reads them as numbers
  wb$add_data("_config", x = as.integer(top), dims = "B6")
  wb$add_data("_config", x = n_items, dims = "B7")
  wb$add_data("_config", x = sig_threshold, dims = "B8")
  wb$add_data("_config", x = marginal_threshold, dims = "B9")

  # ---- Subgroups + base sizes: F/G starting row 1 ----
  base_df <- data.frame(
    subgroup = names(base_sizes),
    base = unname(base_sizes),
    stringsAsFactors = FALSE
  )
  wb$add_data("_config", x = base_df, start_col = 6, start_row = 1)

  # ---- Item variable/label: A/B starting row 20 ----
  item_ref <- data.frame(
    variable = vars,
    label = unname(label_lookup[vars]),
    stringsAsFactors = FALSE
  )
  wb$add_data("_config", x = item_ref, start_row = 20)
}


.turf_write_controls <- function(wb, subgroup_names, n_values, col_info, base_sizes) {
  # _controls: single source of truth for dropdown states
  # Layout: title in A, value in B
  wb$add_worksheet("_controls")

  # Row 1-5: dropdown state defaults
  wb$add_data("_controls", x = "subgroup", dims = "A1")
  wb$add_data("_controls", x = subgroup_names[1], dims = "B1")

  wb$add_data("_controls", x = "combo", dims = "A2")
  combo_default <- if(length(n_values) > 0) (if(2L %in% n_values) 2L else n_values[1]) else 2L
  wb$add_data("_controls", x = combo_default, dims = "B2")

  wb$add_data("_controls", x = "optimize", dims = "A3")
  wb$add_data("_controls", x = "Reach", dims = "B3")

  if(col_info$has_weights){
    wb$add_data("_controls", x = "weighted", dims = "A4")
    wb$add_data("_controls", x = "Yes", dims = "B4")
  }

  wb$add_data("_controls", x = "chart_label", dims = "A5")
  wb$add_data("_controls", x = "Label", dims = "B5")

  # Row 6: base count formula (references new _config subgroup layout in F/G)
  n_sg <- length(subgroup_names)
  base_f <- paste0(
    'INDEX(_config!G2:G', 1 + n_sg, ',MATCH(B1,_config!F2:F', 1 + n_sg, ',0))'
  )
  wb$add_data("_controls", x = "base", dims = "A6")
  wb$add_formula("_controls", x = base_f, dims = "B6")

  # Row 7: lowercase sort key
  wb$add_data("_controls", x = "sort_key", dims = "A7")
  wb$add_formula("_controls", x = 'IF(B3="Reach","reach","freq")', dims = "B7")

  # Row 8: data sheet name
  wb$add_data("_controls", x = "data_sheet", dims = "A8")
  wb$add_formula("_controls", x = '"d_"&B1&"_"&B2&"_"&B7', dims = "B8")

  # Row 9: invalidate best combos flag (1 = needs rebuild, 0 = current)
  wb$add_data("_controls", x = "invalidate_bc", dims = "A9")
  wb$add_data("_controls", x = 1L, dims = "B9")
}


.turf_write_greedy_sheet <- function(wb, n_items){
  # Greedy: hidden sheet for greedy results + chart staging data
  wb$add_worksheet("Greedy")

  # Title
  wb$add_data("Greedy", x = "Greedy TURF Results", dims = "A1")
  wb$add_font("Greedy", dims = "A1", bold = "true", size = 14)

  # Table headers at row 2 (A-F)
  headers <- c("#", "Item", "Cumul", "Incr", "Avg Freq", "Abs")
  cols <- c("A", "B", "C", "D", "E", "F")
  for(i in seq_along(headers)){
    dims <- paste0(cols[i], "2")
    wb$add_data("Greedy", x = headers[i], dims = dims)
    wb$add_font("Greedy", dims = dims, bold = "true", color = wb_color("white"))
    wb$add_fill("Greedy", dims = dims, color = wb_color("4472C4"))
    wb$add_cell_style("Greedy", dims = dims, horizontal = "center")
  }

  # Staging headers (H-K row 1) — chart reads from here
  wb$add_data("Greedy", x = "Step", dims = "H1")
  wb$add_data("Greedy", x = "Item", dims = "I1")
  wb$add_data("Greedy", x = "Cumul", dims = "J1")
  wb$add_data("Greedy", x = "Incr", dims = "K1")

  # Column widths
  wb$set_col_widths("Greedy", cols = 1, widths = 6)   # A: step #
  wb$set_col_widths("Greedy", cols = 2, widths = 55)  # B: item label
  wb$set_col_widths("Greedy", cols = 3, widths = 10)  # C: cumul
  wb$set_col_widths("Greedy", cols = 4, widths = 10)  # D: incr
  wb$set_col_widths("Greedy", cols = 5, widths = 10)  # E: avg frq
  wb$set_col_widths("Greedy", cols = 6, widths = 10)  # F: abs
}


# =============================================================================
# Dashboard builder (controls + chart + items panel only)
# =============================================================================

.turf_write_dashboard <- function(
    wb, subgroup_names, n_values, col_info,
    n_items, vars, label_lookup, base_sizes,
    project_name = "Project Name - (#xxxxxxx)",
    sig_threshold = 0.10, marginal_threshold = 0.20
) {

  sheet <- "Dashboard"

  # Light grey fill for headers/dropdowns (≈ D9D9D9)
  grey_fill <- wb_color("D9D9D9")

  # ---- Row 2: Title ----
  wb$add_data(sheet, x = "TURF Analysis", dims = "B2")
  wb$add_font(sheet, dims = "B2", bold = "true", size = 18)
  wb$set_row_heights(sheet, rows = 2, heights = 24)

  # ---- Row 3: Project name placeholder ----
  wb$add_data(sheet, x = project_name, dims = "B3")
  wb$add_font(sheet, dims = "B3", bold = "true", size = 14)
  wb$set_row_heights(sheet, rows = 3, heights = 19)

  # ---- Row 4: thin separator ----
  wb$set_row_heights(sheet, rows = 4, heights = 5)

  # ---- Row 5: Controls ----
  .turf_write_controls_row(wb, sheet, subgroup_names, n_values, col_info, base_sizes,
                            is_dashboard = TRUE)
  wb$set_row_heights(sheet, rows = 5, heights = 17)

  # ---- Row 6: separator ----
  wb$set_row_heights(sheet, rows = 6, heights = 17)

  # ---- Row 7: Section headers (merged, centered, grey fill) ----
  wb$add_data(sheet, x = "TURF Chart", dims = "B7")
  wb$add_font(sheet, dims = "B7", bold = "true", size = 12)
  wb$add_cell_style(sheet, dims = "B7", horizontal = "center", vertical = "center")
  wb$add_fill(sheet, dims = "B7:O8", color = grey_fill)
  wb$merge_cells(sheet, dims = "B7:O8")

  wb$add_data(sheet, x = "TURF Results", dims = "Q7")
  wb$add_font(sheet, dims = "Q7", bold = "true", size = 12)
  wb$add_cell_style(sheet, dims = "Q7", horizontal = "center")
  wb$add_fill(sheet, dims = "Q7", color = grey_fill)
  wb$merge_cells(sheet, dims = "Q7:X7")

  wb$add_data(sheet, x = "Item Controls", dims = "Z7")
  wb$add_font(sheet, dims = "Z7", bold = "true", size = 14)
  wb$add_cell_style(sheet, dims = "Z7", horizontal = "center")
  wb$add_fill(sheet, dims = "Z7", color = grey_fill)
  wb$merge_cells(sheet, dims = "Z7:AB7")

  wb$set_row_heights(sheet, rows = 7, heights = 20)

  # ---- Row 7-8: Outer border on TURF Chart header ----
  wb$add_border(sheet, dims = "B7:O8",
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium",
                inner_hgrid = NULL, inner_vgrid = NULL)

  # ---- Row 8: Greedy table headers (Q-X, centered) ----
  greedy_headers <- c("#", "Item", "Label", "Cumul", "Incr", "Avg Freq", "Abs", "p-value")
  greedy_cols <- c("Q", "R", "S", "T", "U", "V", "W", "X")
  for(i in seq_along(greedy_headers)){
    dims <- paste0(greedy_cols[i], "8")
    wb$add_data(sheet, x = greedy_headers[i], dims = dims)
    wb$add_font(sheet, dims = dims, bold = "true")
    wb$add_fill(sheet, dims = dims, color = grey_fill)
    wb$add_cell_style(sheet, dims = dims, horizontal = "center")
  }

  # ---- Row 8: Items panel headers (Z-AB, centered) ----
  items_headers <- c("Item", "Label", "Include")
  items_cols <- c("Z", "AA", "AB")
  for(i in seq_along(items_headers)){
    dims <- paste0(items_cols[i], "8")
    wb$add_data(sheet, x = items_headers[i], dims = dims)
    wb$add_font(sheet, dims = dims, bold = "true")
    wb$add_fill(sheet, dims = dims, color = grey_fill)
    wb$add_cell_style(sheet, dims = dims, horizontal = "center")
  }
  wb$set_row_heights(sheet, rows = 8, heights = 17)

  # ---- Outer border: TURF Results header (Q7:X8) ----
  last_data_row <- 8 + n_items  # row 9 + n_items - 1
  wb$add_border(sheet, dims = "Q7:X8",
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium",
                inner_hgrid = NULL, inner_vgrid = NULL)

  # ---- Outer border: Item Controls header (Z7:AB8) ----
  wb$add_border(sheet, dims = "Z7:AB8",
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium",
                inner_hgrid = NULL, inner_vgrid = NULL)

  # ---- Outer border: TURF Results data (Q9:X + last data row) ----
  turf_data_dims <- paste0("Q9:X", last_data_row)
  wb$add_border(sheet, dims = turf_data_dims,
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium",
                inner_hgrid = NULL, inner_vgrid = NULL)

  # ---- Outer border: Item Controls data (Z9:AB + last item row) ----
  items_data_dims <- paste0("Z9:AB", last_data_row)
  wb$add_border(sheet, dims = items_data_dims,
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium",
                inner_hgrid = NULL, inner_vgrid = NULL)

  # ---- Row 9+: Item variable names, labels + checkboxes ----
  for(i in seq_along(vars)){
    row <- 8 + i  # row 9, 10, 11, ...
    item_label <- unname(label_lookup[vars[i]])
    wb$add_data(sheet, x = vars[i], dims = paste0("Z", row))
    wb$add_data(sheet, x = item_label, dims = paste0("AA", row))
    wb$add_data(sheet, x = TRUE, dims = paste0("AB", row))
  }


  # ---- Column widths ----
  wb$set_col_widths(sheet, cols = 1, widths = 2)      # A: gutter
  wb$set_col_widths(sheet, cols = 2, widths = 10)     # B: labels
  wb$set_col_widths(sheet, cols = 3, widths = 8.5)    # C: subgroup dropdown
  wb$set_col_widths(sheet, cols = 4, widths = 3)      # D: spacer
  wb$set_col_widths(sheet, cols = 5, widths = 10)     # E: optimize label
  wb$set_col_widths(sheet, cols = 6, widths = 8.5)    # F: optimize dropdown
  wb$set_col_widths(sheet, cols = 7, widths = 3)      # G: spacer
  wb$set_col_widths(sheet, cols = 8, widths = 10)     # H: chart label label
  wb$set_col_widths(sheet, cols = 9, widths = 8.5)    # I: chart label dropdown
  wb$set_col_widths(sheet, cols = 10, widths = 3)     # J: spacer
  wb$set_col_widths(sheet, cols = 11, widths = 12)    # K: weighted label
  wb$set_col_widths(sheet, cols = 12, widths = 8.5)   # L: weighted dropdown
  wb$set_col_widths(sheet, cols = 13, widths = 3)     # M: spacer
  wb$set_col_widths(sheet, cols = 14, widths = 8.5)   # N: base label
  wb$set_col_widths(sheet, cols = 15, widths = 8.5)   # O: base value
  wb$set_col_widths(sheet, cols = 16, widths = 5)     # P: spacer
  wb$set_col_widths(sheet, cols = 17, widths = 6)     # Q: greedy #
  wb$set_col_widths(sheet, cols = 18, widths = 20)    # R: greedy item (variable)
  wb$set_col_widths(sheet, cols = 19, widths = 35)    # S: greedy label
  wb$set_col_widths(sheet, cols = 20, widths = 10)    # T: greedy cumul
  wb$set_col_widths(sheet, cols = 21, widths = 8.5)   # U: greedy incr
  wb$set_col_widths(sheet, cols = 22, widths = 8.5)   # V: greedy avg freq
  wb$set_col_widths(sheet, cols = 23, widths = 8.5)   # W: greedy abs
  wb$set_col_widths(sheet, cols = 24, widths = 10)    # X: p-value
  wb$set_col_widths(sheet, cols = 25, widths = 3)     # Y: spacer
  wb$set_col_widths(sheet, cols = 26, widths = 20)    # Z: item variable
  wb$set_col_widths(sheet, cols = 27, widths = 35)    # AA: item label
  wb$set_col_widths(sheet, cols = 28, widths = 10)    # AB: include

  # Hide weighted controls if no weights
  if(!col_info$has_weights){
    wb$set_col_widths(sheet, cols = 11, hidden = TRUE)   # K: weighted label
    wb$set_col_widths(sheet, cols = 12, hidden = TRUE)   # L: weighted dropdown
  }


  # ---- Footer: metric definitions (VBA overwrites at runtime) ----
  footer_start <- last_data_row + 2  # one blank row then definitions start

  footer_lines <- c(
    "# \u2014 Greedy step number (order in which items were selected)",
    "Cumul \u2014 Cumulative unduplicated reach (% of respondents reached through this step)",
    "Incr \u2014 Incremental reach (% points added by this item beyond previous step)",
    "Avg Freq \u2014 Average frequency among reached respondents (mean items selected per reached respondent)",
    "Abs \u2014 Absolute/standalone reach (% who selected this item regardless of others)",
    "p-value \u2014 Binomial exact test for incremental reach significance",
    paste0("Method: P(X >= n_new | n_unreached, p0 = mean rate of remaining items). ",
           "Green < ", sig_threshold * 100, "%",
           ", Orange < ", marginal_threshold * 100, "%",
           ", Red >= ", marginal_threshold * 100, "%")
  )

  for(fi in seq_along(footer_lines)){
    r <- footer_start + fi - 1
    dims_cell <- paste0("Q", r)
    dims_merge <- paste0("Q", r, ":X", r)

    wb$add_data(sheet, x = footer_lines[fi], dims = dims_cell)
    wb$merge_cells(sheet, dims = dims_merge)
    wb$add_font(sheet, dims = dims_cell, size = 9, italic = "true",
                color = wb_color("595959"))
    wb$add_cell_style(sheet, dims = dims_cell, horizontal = "left")
  }


  # ---- Freeze pane (below row 8 headers) ----
  wb$freeze_pane(sheet, first_active_row = 9, first_active_col = 2)

  invisible(NULL)
}


# =============================================================================
# Best Combos builder (controls + combo results table)
# =============================================================================

.turf_write_best_combos <- function(wb, subgroup_names, n_values, col_info, base_sizes,
                                    project_name = "Project Name - (#xxxxxxx)"){

  sheet <- "Best Combos"
  grey_fill <- wb_color("D9D9D9")
  max_n <- if(length(n_values) > 0) max(n_values) else 2L

  # Last item column letter (E, F, G, ... depending on max combo size)
  last_item_col <- openxlsx2::int2col(4 + max_n)

  # ---- Row 2: Title ----
  wb$add_data(sheet, x = "Optimized TURF Results", dims = "B2")
  wb$add_font(sheet, dims = "B2", bold = "true", size = 18)
  wb$set_row_heights(sheet, rows = 2, heights = 24)

  # ---- Row 3: Project name placeholder ----
  wb$add_data(sheet, x = project_name, dims = "B3")
  wb$add_font(sheet, dims = "B3", bold = "true", size = 14)
  wb$set_row_heights(sheet, rows = 3, heights = 19)

  # ---- Row 4: thin separator ----
  wb$set_row_heights(sheet, rows = 4, heights = 5)

  # ---- Row 5-12: Controls (stacked vertically, label in B, value in C) ----
  .turf_write_bc_controls(wb, sheet, subgroup_names, n_values, col_info, base_sizes)

  # ---- Row 14: separator ----
  wb$set_row_heights(sheet, rows = 14, heights = 10)

  # ---- Row 15: Section header (merged, grey fill, centered) ----
  merge_dims <- paste0("B15:", last_item_col, "15")
  wb$add_data(sheet, x = "Combo Results", dims = "B15")
  wb$add_font(sheet, dims = "B15", bold = "true", size = 12)
  wb$add_cell_style(sheet, dims = "B15", horizontal = "center")
  wb$add_fill(sheet, dims = "B15", color = grey_fill)
  wb$merge_cells(sheet, dims = merge_dims)
  wb$set_row_heights(sheet, rows = 15, heights = 20)

  # Outer border on section header + header row combined (rows 15-16)
  full_header_dims <- paste0("B15:", last_item_col, "16")
  wb$add_border(sheet, dims = full_header_dims,
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium",
                inner_hgrid = NULL, inner_vgrid = NULL)

  # ---- Row 16: Combo table headers ----
  combo_headers <- c("Rank", "Reach", "Freq")
  combo_cols <- c("B", "C", "D")
  for(i in seq_len(max_n)){
    combo_headers <- c(combo_headers, paste0("Item ", i))
    combo_cols <- c(combo_cols, openxlsx2::int2col(4 + i))
  }

  for(i in seq_along(combo_headers)){
    dims <- paste0(combo_cols[i], "16")
    wb$add_data(sheet, x = combo_headers[i], dims = dims)
    wb$add_font(sheet, dims = dims, bold = "true")
    wb$add_fill(sheet, dims = dims, color = grey_fill)
    wb$add_cell_style(sheet, dims = dims, horizontal = "center")
  }
  wb$set_row_heights(sheet, rows = 16, heights = 17)


  # ---- Column widths ----
  wb$set_col_widths(sheet, cols = 1, widths = 2)    # A: gutter
  wb$set_col_widths(sheet, cols = 2, widths = 12)   # B: rank / control label
  wb$set_col_widths(sheet, cols = 3, widths = 12)   # C: reach / control value
  wb$set_col_widths(sheet, cols = 4, widths = 10)   # D: freq
  for(i in seq_len(max_n)){
    wb$set_col_widths(sheet, cols = 4 + i, widths = 45)  # E+: item labels
  }


  # ---- Freeze pane (below header row 16) ----
  wb$freeze_pane(sheet, first_active_row = 17, first_active_col = 2)

  invisible(NULL)
}


# =============================================================================
# Best Combos controls: stacked vertically, label in B, value in C
# =============================================================================

.turf_write_bc_controls <- function(wb, sheet, subgroup_names, n_values,
                                     col_info, base_sizes){

  grey_fill <- wb_color("D9D9D9")

  # Helper: style a stacked control row (label in B, value in C)
  .style_bc_control <- function(row){
    b_dims <- paste0("B", row)
    c_dims <- paste0("C", row)
    bc_dims <- paste0("B", row, ":C", row)

    wb$add_fill(sheet, dims = b_dims, color = grey_fill)
    wb$add_fill(sheet, dims = c_dims, color = grey_fill)
    wb$add_cell_style(sheet, dims = b_dims, horizontal = "right", vertical = "center")
    wb$add_cell_style(sheet, dims = c_dims, horizontal = "center", vertical = "center")
    wb$add_border(sheet, dims = b_dims,
                  top_border = "medium", bottom_border = "medium",
                  left_border = "medium", right_border = NULL)
    wb$add_border(sheet, dims = c_dims,
                  top_border = "medium", bottom_border = "medium",
                  left_border = NULL, right_border = "medium")
  }

  # Row 5: Subgroup
  wb$add_data(sheet, x = "Subgroup:", dims = "B5")
  wb$add_font(sheet, dims = "B5", bold = "true", size = 11)
  wb$add_data(sheet, x = subgroup_names[1], dims = "C5")
  wb$add_data_validation(
    sheet, dims = "C5", type = "list",
    value = paste0('"', paste(subgroup_names, collapse = ","), '"')
  )
  wb$add_font(sheet, dims = "C5", size = 11)
  .style_bc_control(5)

  # Row 6: Combo Size (default to 2 if available, else first)
  wb$add_data(sheet, x = "Combo Size:", dims = "B6")
  wb$add_font(sheet, dims = "B6", bold = "true", size = 11)
  default_combo <- if(length(n_values) > 0) (if(2L %in% n_values) 2L else n_values[1]) else 2L
  wb$add_data(sheet, x = default_combo, dims = "C6")
  if(length(n_values) > 0){
    wb$add_data_validation(
      sheet, dims = "C6", type = "list",
      value = paste0('"', paste(n_values, collapse = ","), '"')
    )
  }
  wb$add_font(sheet, dims = "C6", size = 11)
  .style_bc_control(6)

  # Row 7: Display top N
  wb$add_data(sheet, x = "Display:", dims = "B7")
  wb$add_font(sheet, dims = "B7", bold = "true", size = 11)
  wb$add_data(sheet, x = 1000L, dims = "C7")
  wb$add_font(sheet, dims = "C7", size = 11)
  .style_bc_control(7)

  # Row 8: Optimize
  wb$add_data(sheet, x = "Optimize:", dims = "B8")
  wb$add_font(sheet, dims = "B8", bold = "true", size = 11)
  wb$add_data(sheet, x = "Reach", dims = "C8")
  wb$add_data_validation(
    sheet, dims = "C8", type = "list",
    value = '"Reach,Freq"'
  )
  wb$add_font(sheet, dims = "C8", size = 11)
  .style_bc_control(8)

  # Row 9: Weighted (only if weights exist)
  if(col_info$has_weights){
    wb$add_data(sheet, x = "Weighted:", dims = "B9")
    wb$add_font(sheet, dims = "B9", bold = "true", size = 11)
    wb$add_data(sheet, x = "Yes", dims = "C9")
    wb$add_data_validation(
      sheet, dims = "C9", type = "list",
      value = '"Yes,No"'
    )
    wb$add_font(sheet, dims = "C9", size = 11)
    .style_bc_control(9)
  } else {
    wb$set_row_heights(sheet, rows = 9, heights = 0)
  }

  # Row 10: Autofit
  wb$add_data(sheet, x = "Autofit:", dims = "B10")
  wb$add_font(sheet, dims = "B10", bold = "true", size = 11)
  wb$add_data(sheet, x = "No", dims = "C10")
  wb$add_data_validation(
    sheet, dims = "C10", type = "list",
    value = '"Yes,No"'
  )
  wb$add_font(sheet, dims = "C10", size = 11)
  .style_bc_control(10)

  # Row 11: Chart
  wb$add_data(sheet, x = "Chart:", dims = "B11")
  wb$add_font(sheet, dims = "B11", bold = "true", size = 11)
  wb$add_data(sheet, x = "Top Reach", dims = "C11")
  wb$add_data_validation(
    sheet, dims = "C11", type = "list",
    value = '"Top Reach,Reach vs Freq,Item Frequency,None"'
  )
  wb$add_font(sheet, dims = "C11", size = 11)
  .style_bc_control(11)

  # Row 12: Chart Location
  wb$add_data(sheet, x = "Chart Loc:", dims = "B12")
  wb$add_font(sheet, dims = "B12", bold = "true", size = 11)
  wb$add_data(sheet, x = "Top", dims = "C12")
  wb$add_data_validation(
    sheet, dims = "C12", type = "list",
    value = '"Top,Right"'
  )
  wb$add_font(sheet, dims = "C12", size = 11)
  .style_bc_control(12)

  # Row 13: Base
  wb$add_data(sheet, x = "Base:", dims = "B13")
  wb$add_font(sheet, dims = "B13", bold = "true", size = 11)
  n_sg <- length(subgroup_names)
  base_f <- paste0(
    'INDEX(_config!G2:G', 1 + n_sg, ',MATCH(C5,_config!F2:F', 1 + n_sg, ',0))'
  )
  wb$add_formula(sheet, x = base_f, dims = "C13")
  wb$add_font(sheet, dims = "C13", bold = "true", size = 11)
  .style_bc_control(13)
}


# =============================================================================
# Shared helper: write controls row to any sheet (Dashboard only now)
# =============================================================================

.turf_write_controls_row <- function(wb, sheet, subgroup_names, n_values,
                                      col_info, base_sizes,
                                      is_dashboard = FALSE){
  # Row 5 controls layout:
  # B5:C5 — Subgroup (both sheets)
  # E5:F5 — Optimize (Dashboard) or Combo Size (Best Combos)
  # H5:I5 — Chart Label (Dashboard) or Optimize (Best Combos)
  # K5:L5 — Weighted (both sheets, hidden if no weights)
  # N5:O5 — Base (both sheets)

  grey_fill <- wb_color("D9D9D9")

  # Helper: style a control pair (label_dims + value_dims)
  .style_control_pair <- function(label_dims, value_dims, pair_dims){
    wb$add_fill(sheet, dims = label_dims, color = grey_fill)
    wb$add_fill(sheet, dims = value_dims, color = grey_fill)
    wb$add_cell_style(sheet, dims = label_dims, horizontal = "center", vertical = "top")
    wb$add_cell_style(sheet, dims = value_dims, horizontal = "center", vertical = "center")

    wb$add_border(sheet, dims = label_dims,
                  top_border = "medium", bottom_border = "medium",
                  left_border = "medium", right_border = NULL)
    wb$add_border(sheet, dims = value_dims,
                  top_border = "medium", bottom_border = "medium",
                  left_border = NULL, right_border = "medium")
  }

  # Subgroup (same on both sheets)
  wb$add_data(sheet, x = "Subgroup:", dims = "B5")
  wb$add_font(sheet, dims = "B5", bold = "true", size = 12)
  wb$add_data(sheet, x = subgroup_names[1], dims = "C5")
  wb$add_data_validation(
    sheet, dims = "C5", type = "list",
    value = paste0('"', paste(subgroup_names, collapse = ","), '"')
  )
  wb$add_font(sheet, dims = "C5", size = 12)
  .style_control_pair("B5", "C5", "B5:C5")

  # E5:F5 — Dashboard: Optimize / Best Combos: Combo Size
  if(is_dashboard){
    wb$add_data(sheet, x = "Optimize:", dims = "E5")
    wb$add_font(sheet, dims = "E5", bold = "true", size = 12)
    wb$add_data(sheet, x = "Reach", dims = "F5")
    wb$add_data_validation(
      sheet, dims = "F5", type = "list",
      value = '"Reach,Freq"'
    )
    wb$add_font(sheet, dims = "F5", size = 12)
    .style_control_pair("E5", "F5", "E5:F5")
  } else {
    wb$add_data(sheet, x = "Combo Size:", dims = "E5")
    wb$add_font(sheet, dims = "E5", bold = "true", size = 12)
    wb$add_data(sheet, x = n_values[1], dims = "F5")
    wb$add_data_validation(
      sheet, dims = "F5", type = "list",
      value = paste0('"', paste(n_values, collapse = ","), '"')
    )
    wb$add_font(sheet, dims = "F5", size = 12)
    .style_control_pair("E5", "F5", "E5:F5")
  }

  # H5:I5 — Dashboard: Chart Label / Best Combos: Optimize
  if(is_dashboard){
    wb$add_data(sheet, x = "Chart Label:", dims = "H5")
    wb$add_font(sheet, dims = "H5", bold = "true", size = 12)
    wb$add_data(sheet, x = "Label", dims = "I5")
    wb$add_data_validation(
      sheet, dims = "I5", type = "list",
      value = '"Variable,Variable - Label,Label"'
    )
    wb$add_font(sheet, dims = "I5", size = 12)
    .style_control_pair("H5", "I5", "H5:I5")
  } else {
    wb$add_data(sheet, x = "Optimize:", dims = "H5")
    wb$add_font(sheet, dims = "H5", bold = "true", size = 12)
    wb$add_data(sheet, x = "Reach", dims = "I5")
    wb$add_data_validation(
      sheet, dims = "I5", type = "list",
      value = '"Reach,Freq"'
    )
    wb$add_font(sheet, dims = "I5", size = 12)
    .style_control_pair("H5", "I5", "H5:I5")
  }

  # Weighted toggle (same on both sheets)
  if(col_info$has_weights){
    wb$add_data(sheet, x = "Weighted:", dims = "K5")
    wb$add_font(sheet, dims = "K5", bold = "true", size = 12)
    wb$add_data(sheet, x = "Yes", dims = "L5")
    wb$add_data_validation(
      sheet, dims = "L5", type = "list",
      value = '"Yes,No"'
    )
    wb$add_font(sheet, dims = "L5", size = 12)
    .style_control_pair("K5", "L5", "K5:L5")
  }

  # Base count (same on both sheets)
  wb$add_data(sheet, x = "Base:", dims = "N5")
  wb$add_font(sheet, dims = "N5", bold = "true", size = 12)
  n_sg <- length(subgroup_names)
  base_f <- paste0(
    'INDEX(_config!G2:G', 1 + n_sg, ',MATCH(C5,_config!F2:F', 1 + n_sg, ',0))'
  )
  wb$add_formula(sheet, x = base_f, dims = "O5")
  wb$add_font(sheet, dims = "O5", bold = "true", size = 12)
  .style_control_pair("N5", "O5", "N5:O5")

  # Items: All/None toggle (Dashboard only)
  if(is_dashboard){
    wb$add_data(sheet, x = "Select:", dims = "AB3")
    wb$add_font(sheet, dims = "AB3", bold = "true", size = 12)
    wb$add_cell_style(sheet, dims = "AB3", horizontal = "center")
    wb$add_data(sheet, x = "-", dims = "AB5")
    wb$add_data_validation(
      sheet, dims = "AB5", type = "list",
      value = '"-,All,None"'
    )
    wb$add_font(sheet, dims = "AB5", size = 12)
    grey_fill <- wb_color("D9D9D9")
    wb$add_fill(sheet, dims = "AB5", color = grey_fill)
    wb$add_cell_style(sheet, dims = "AB5", horizontal = "center", vertical = "center")
    wb$add_border(sheet, dims = "AB5",
                  top_border = "medium", bottom_border = "medium",
                  left_border = "medium", right_border = "medium")
  }
}
