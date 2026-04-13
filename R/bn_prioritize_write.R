#' bn_prioritize_write
#'
#' @description Takes the output of \code{bn_prioritize()} or
#'   \code{bn_prioritizations()} and writes a formatted Excel workbook with
#'   a dynamic dashboard. Dropdown controls for strategy, search, subgroup,
#'   focus (brand), and weight switch between hidden data sheets. Controls
#'   are only shown when multiple options exist for that dimension.
#'
#' @param result Output of \code{bn_prioritize()} (a single tibble) or
#'   \code{bn_prioritizations()} (a list of results with meta).
#' @param file_name Character or NULL. Prefix for output file name. If NULL,
#'   inherits from \code{sub_title}. Default NULL.
#' @param sub_title Character or NULL. Subtitle text displayed in the sheet
#'   header (e.g. project name). Default NULL.
#' @param title Character or NULL. Title displayed in the sheet header. If NULL,
#'   defaults to \code{"Prioritization Analysis"}. Default NULL.
#' @param variable_width Numeric. Column width for the Variable column.
#'   Default 20.
#' @param label_width Numeric. Column width for the Label column. Default 20.
#' @param combo_width Numeric. Column width for the Combo column.
#'   Default 40.
#' @param sig_threshold Numeric. P-value threshold for green (significant).
#'   Default 0.05.
#' @param marginal_threshold Numeric. P-value threshold for orange (marginal).
#'   Default 0.10.
#' @param lift Numeric. The lift fraction used in the prioritization analysis,
#'   displayed in the footer. Default 0.10 (10\%).
#' @param path Character. Directory to write workbook to. Default \code{"."}.
#'
#' @return Workbook object (invisibly).
#'
#' @seealso [bn_prioritize()], [bn_prioritizations()]
#'
#' @export
bn_prioritize_write <- function(
    result,
    file_name = NULL,
    sub_title = NULL,
    title = NULL,
    variable_width = 20,
    label_width = 20,
    combo_width = 40,
    sig_threshold = 0.05,
    marginal_threshold = 0.10,
    lift = 0.10,
    path = "."
) {

  if (is.null(file_name)) file_name <- sub_title
  if (is.null(sub_title)) sub_title <- file_name
  if (is.null(title)) title <- "Prioritization Analysis"

  wb <- oxl_create_workbook()

  # ---------------------------------------------------------------------------
  # Build registry: flat list of tagged entries
  # ---------------------------------------------------------------------------
  registry <- .prioritize_build_registry(result)

  has_p <- any(purrr::map_lgl(registry, ~"p_value" %in% names(.x$tbl)))
  max_rows <- max(purrr::map_int(registry, ~nrow(.x$tbl)))
  n_entries <- length(registry)

  # Determine which dimensions have multiple values
  dims <- list(
    strategy = unique(purrr::map_chr(registry, "strategy")),
    search   = unique(purrr::map_chr(registry, "search")),
    subgroup = unique(purrr::map_chr(registry, "subgroup")),
    focus    = unique(purrr::map_chr(registry, "focus")),
    weight   = unique(purrr::map_chr(registry, "weight"))
  )
  active_dims <- names(dims)[purrr::map_int(dims, length) > 1]

  # ---------------------------------------------------------------------------
  # 1. Dashboard sheet (created first)
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "Dashboard", tabColour = "#FFFFFF")
  openxlsx::addStyle(wb, "Dashboard",
    style = openxlsx::createStyle(fgFill = "#FFFFFF"),
    rows = 1:200, cols = 1:50, gridExpand = TRUE, stack = TRUE)

  # ---------------------------------------------------------------------------
  # 2. Write hidden data sheets
  # ---------------------------------------------------------------------------
  for (entry in registry) {
    sn <- entry$sheet_name
    openxlsx::addWorksheet(wb, sn)
    openxlsx::writeData(wb, sn, entry$tbl, startRow = 1, startCol = 1)
  }

  # ---------------------------------------------------------------------------
  # 3. Write _lookup sheet
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "_lookup")

  # Columns: Strategy | Search | Subgroup | Focus | Weight | Sheet | Base | Key
  lookup_headers <- c("Strategy", "Search", "Subgroup", "Focus", "Weight",
                       "Sheet", "Base", "Key")
  for (ci in seq_along(lookup_headers)) {
    openxlsx::writeData(wb, "_lookup", lookup_headers[ci], startRow = 1, startCol = ci)
  }

  for (i in seq_along(registry)) {
    r <- i + 1
    e <- registry[[i]]
    openxlsx::writeData(wb, "_lookup", e$strategy, startRow = r, startCol = 1)
    openxlsx::writeData(wb, "_lookup", e$search, startRow = r, startCol = 2)
    openxlsx::writeData(wb, "_lookup", e$subgroup, startRow = r, startCol = 3)
    openxlsx::writeData(wb, "_lookup", e$focus, startRow = r, startCol = 4)
    openxlsx::writeData(wb, "_lookup", e$weight, startRow = r, startCol = 5)
    openxlsx::writeData(wb, "_lookup", e$sheet_name, startRow = r, startCol = 6)
    openxlsx::writeData(wb, "_lookup", if (!is.null(e$n_obs)) e$n_obs else NA_integer_,
      startRow = r, startCol = 7)
  }

  # Key column: concatenation of all 5 dims
  for (i in seq_along(registry)) {
    r <- i + 1
    key_formula <- paste0("A", r, "&\"|\"&B", r, "&\"|\"&C", r, "&\"|\"&D", r, "&\"|\"&E", r)
    openxlsx::writeFormula(wb, "_lookup", x = key_formula, startRow = r, startCol = 8)
  }

  # Unique option lists for dropdowns (columns I onward)
  opt_col <- 9L
  dim_names <- c("strategy", "search", "subgroup", "focus", "weight")
  dim_labels <- c("Strategy", "Search", "Subgroup", "Focus", "Weight")
  dim_opt_cols <- list()  # track which lookup column has options for each dim

  for (di in seq_along(dim_names)) {
    dn <- dim_names[di]
    if (dn %in% active_dims) {
      vals <- dims[[dn]]
      openxlsx::writeData(wb, "_lookup", dim_labels[di], startRow = 1, startCol = opt_col)
      for (vi in seq_along(vals)) {
        openxlsx::writeData(wb, "_lookup", vals[vi], startRow = vi + 1, startCol = opt_col)
      }
      dim_opt_cols[[dn]] <- list(col = opt_col, n = length(vals))
      opt_col <- opt_col + 1L
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Build Dashboard
  # ---------------------------------------------------------------------------
  styles <- list(
    title      = openxlsx::createStyle(textDecoration = "bold", fontSize = 18),
    sub_title  = openxlsx::createStyle(textDecoration = c("bold", "italic"), fontSize = 14),
    header     = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE),
    center_int = openxlsx::createStyle(numFmt = "0", halign = "center"),
    center_2dp = openxlsx::createStyle(numFmt = "0.00", halign = "center"),
    center_pct = openxlsx::createStyle(numFmt = "0.00%", halign = "center"),
    left       = openxlsx::createStyle(halign = "left"),
    baseline   = openxlsx::createStyle(textDecoration = "italic", fgFill = "#F2F2F2"),
    p_green    = openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE"),
    p_orange   = openxlsx::createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C"),
    p_red      = openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"),
    dropdown_label = openxlsx::createStyle(textDecoration = "bold", halign = "right"),
    dropdown_cell  = openxlsx::createStyle(border = "Bottom", borderStyle = "thin", halign = "left")
  )

  dash <- "Dashboard"
  col_data_start <- 2L

  # Title
  openxlsx::writeData(wb, dash, title, startRow = 2L, startCol = col_data_start)
  openxlsx::addStyle(wb, dash, style = styles$title,
    rows = 2L, cols = col_data_start, stack = TRUE)

  # Subtitle
  if (!is.null(sub_title)) {
    openxlsx::writeData(wb, dash, sub_title, startRow = 3L, startCol = col_data_start)
    openxlsx::addStyle(wb, dash, style = styles$sub_title,
      rows = 3L, cols = col_data_start, stack = TRUE)
  }

  # --- Dropdown controls (stacked vertically in C/D) ---
  row_controls_start <- if (!is.null(sub_title)) 5L else 4L
  label_col <- 3L  # Column C
  cell_col <- 4L   # Column D

  dropdown_refs <- list()
  current_row <- row_controls_start

  dim_display_labels <- c(
    strategy = "Strategy:",
    search   = "Search:",
    subgroup = "Subgroup:",
    focus    = "Focus:",
    weight   = "Weight:"
  )

  for (dn in active_dims) {
    info <- dim_opt_cols[[dn]]
    default_val <- dims[[dn]][1]

    # Label in C
    openxlsx::writeData(wb, dash, dim_display_labels[[dn]],
      startRow = current_row, startCol = label_col)
    openxlsx::addStyle(wb, dash, style = styles$dropdown_label,
      rows = current_row, cols = label_col, stack = TRUE)

    # Cell in D
    openxlsx::writeData(wb, dash, default_val,
      startRow = current_row, startCol = cell_col)
    openxlsx::addStyle(wb, dash, style = styles$dropdown_cell,
      rows = current_row, cols = cell_col, stack = TRUE)

    # Data validation
    opt_range <- paste0("_lookup!$", num2let(info$col), "$2:$",
      num2let(info$col), "$", info$n + 1)
    openxlsx::dataValidation(wb, dash,
      col = cell_col, rows = current_row,
      type = "list", value = opt_range)

    dropdown_refs[[dn]] <- paste0("$", num2let(cell_col), "$", current_row)

    current_row <- current_row + 1L
  }

  openxlsx::setColWidths(wb, dash, cols = label_col, widths = 10)
  openxlsx::setColWidths(wb, dash, cols = cell_col, widths = 14)

  last_control_row <- current_row - 1L

  # --- Build key formula from dropdown refs ---
  # For dimensions with only one value, hardcode that value in the key
  key_parts <- purrr::map_chr(dim_names, function(dn) {
    if (dn %in% active_dims) {
      paste0("Dashboard!", dropdown_refs[[dn]])
    } else {
      paste0("\"", dims[[dn]], "\"")
    }
  })
  key_formula <- paste(key_parts, collapse = "&\"|\"&")

  # Sheet lookup: INDEX(Sheet col, MATCH(key, Key col, 0))
  sheet_match <- paste0(
    "MATCH(", key_formula, ",_lookup!$H$2:$H$", n_entries + 1, ",0)"
  )
  sheet_lookup <- paste0(
    "INDEX(_lookup!$F$2:$F$", n_entries + 1, ",", sheet_match, ")"
  )

  # --- Data table ---
  # Dashboard columns (no combo) and their source column letters in the data sheets
  # Source: A=priority, B=variable, C=label, D=combo(skip), E=dv_estimate,
  #         F=marginal_gain, G=marginal_gain_pct, H=p_value
  display_headers <- c("Step", "Variable", "Label",
                       "DV Estimate", "Marginal Gain", "Marginal Gain %")
  source_letters <- c("A", "B", "C", "E", "F", "G")
  if (has_p) {
    display_headers <- c(display_headers, "p-value")
    source_letters <- c(source_letters, "H")
  }

  n_display_cols <- length(display_headers)
  row_data_start <- last_control_row + 2L
  cols_all <- seq(col_data_start, col_data_start + n_display_cols - 1)

  # Write headers
  for (ci in seq_along(display_headers)) {
    openxlsx::writeData(wb, dash, display_headers[ci],
      startRow = row_data_start, startCol = col_data_start + ci - 1)
  }
  openxlsx::addStyle(wb, dash, style = styles$header,
    rows = row_data_start, cols = cols_all, gridExpand = TRUE, stack = TRUE)

  # Write INDIRECT formulas for each data row
  data_rows <- seq(row_data_start + 1, row_data_start + max_rows)

  for (ri in seq_along(data_rows)) {
    row <- data_rows[ri]
    source_row <- ri + 1  # row 2+ in data sheet

    for (ci in seq_len(n_display_cols)) {
      src_let <- source_letters[ci]
      src_ref <- paste0(sheet_lookup, "&\"!$", src_let, "$", source_row, "\"")
      cell_formula <- paste0(
        "IF(ISBLANK(INDIRECT(", src_ref, ")),\"\",IFERROR(INDIRECT(", src_ref, "),\"\"))"
      )
      openxlsx::writeFormula(wb, dash, x = cell_formula,
        startRow = row, startCol = col_data_start + ci - 1)
    }
  }

  # --- Column widths ---
  col_step <- col_data_start
  col_variable <- col_data_start + 1
  col_label <- col_data_start + 2
  col_estimate <- col_data_start + 3
  col_gain <- col_data_start + 4
  col_gain_pct <- col_data_start + 5
  col_pvalue <- if (has_p) col_data_start + 6 else NULL

  openxlsx::setColWidths(wb, dash, cols = col_step, widths = 6)
  openxlsx::setColWidths(wb, dash, cols = col_variable, widths = variable_width)
  openxlsx::setColWidths(wb, dash, cols = col_label, widths = label_width)
  openxlsx::setColWidths(wb, dash, cols = col_estimate, widths = 14)
  openxlsx::setColWidths(wb, dash, cols = col_gain, widths = 14)
  openxlsx::setColWidths(wb, dash, cols = col_gain_pct, widths = 14)
  if (!is.null(col_pvalue)) {
    openxlsx::setColWidths(wb, dash, cols = col_pvalue, widths = 10)
  }

  # --- Data styles ---
  # Step = integer
  openxlsx::addStyle(wb, dash, style = styles$center_int,
    rows = data_rows, cols = col_step, gridExpand = TRUE, stack = TRUE)

  # DV Estimate, Marginal Gain = 2 decimals
  openxlsx::addStyle(wb, dash, style = styles$center_2dp,
    rows = data_rows, cols = c(col_estimate, col_gain), gridExpand = TRUE, stack = TRUE)

  # Marginal Gain % = percentage
  openxlsx::addStyle(wb, dash, style = styles$center_pct,
    rows = data_rows, cols = col_gain_pct, gridExpand = TRUE, stack = TRUE)

  # Left-align text columns
  text_cols <- c(col_variable, col_label)
  openxlsx::addStyle(wb, dash, style = styles$left,
    rows = data_rows, cols = text_cols, gridExpand = TRUE, stack = TRUE)

  # White-to-green colour scale for DV Estimate, Marginal Gain, Marginal Gain %
  for (cc in c(col_estimate, col_gain, col_gain_pct)) {
    openxlsx::conditionalFormatting(wb, dash,
      cols = cc, rows = data_rows,
      style = c("#FFFFFF", "#66bd7d"), type = "colourScale")
  }

  # --- P-value conditional formatting ---
  if (has_p && !is.null(col_pvalue)) {
    step_rows <- data_rows[-1]

    openxlsx::addStyle(wb, dash, style = styles$center_2dp,
      rows = data_rows, cols = col_pvalue, gridExpand = TRUE, stack = TRUE)

    if (length(step_rows) > 0) {
      p_let <- num2let(col_pvalue)
      step_let <- num2let(col_step)

      openxlsx::conditionalFormatting(wb, dash,
        cols = col_pvalue, rows = step_rows,
        style = styles$p_red, type = "expression",
        rule = paste0(p_let, step_rows[1], ">=", marginal_threshold))

      openxlsx::conditionalFormatting(wb, dash,
        cols = col_pvalue, rows = step_rows,
        style = styles$p_orange, type = "expression",
        rule = paste0("AND(", p_let, step_rows[1], ">=", sig_threshold,
                       ",", p_let, step_rows[1], "<", marginal_threshold, ")"))

      openxlsx::conditionalFormatting(wb, dash,
        cols = col_pvalue, rows = step_rows,
        style = styles$p_green, type = "expression",
        rule = paste0(p_let, step_rows[1], "<", sig_threshold))

      # White when step is blank or 0 (last = highest priority in Excel)
      openxlsx::conditionalFormatting(wb, dash,
        cols = col_pvalue, rows = step_rows,
        style = openxlsx::createStyle(bgFill = "#FFFFFF"), type = "expression",
        rule = paste0("OR(", step_let, step_rows[1], "=\"\",",
                       step_let, step_rows[1], "=0)"))
    }
  }

  # --- Conditional borders (only on rows with data) ---
  step_let <- num2let(col_step)
  border_rule <- paste0(step_let, data_rows[1], "<>\"\"")

  # Left border on first column
  openxlsx::conditionalFormatting(wb, dash,
    cols = min(cols_all), rows = data_rows,
    style = openxlsx::createStyle(border = "Left", borderStyle = "medium"),
    type = "expression", rule = border_rule)

  # Right border on last column
  openxlsx::conditionalFormatting(wb, dash,
    cols = max(cols_all), rows = data_rows,
    style = openxlsx::createStyle(border = "Right", borderStyle = "medium"),
    type = "expression", rule = border_rule)

  # Bottom border only on last data row (this row has data, next row is blank)
  bottom_rule <- paste0("AND(", step_let, data_rows[1], "<>\"\",",
                         step_let, data_rows[1] + 1, "=\"\")")
  openxlsx::conditionalFormatting(wb, dash,
    cols = cols_all, rows = data_rows,
    style = openxlsx::createStyle(border = "Bottom", borderStyle = "medium"),
    type = "expression", rule = bottom_rule)

  # Header row — outer box only
  openxlsx::addStyle(wb, dash,
    style = openxlsx::createStyle(border = "TopBottom", borderStyle = "medium"),
    rows = row_data_start, cols = cols_all, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, dash,
    style = openxlsx::createStyle(border = "TopBottomLeft", borderStyle = "medium"),
    rows = row_data_start, cols = min(cols_all), stack = TRUE)
  openxlsx::addStyle(wb, dash,
    style = openxlsx::createStyle(border = "TopBottomRight", borderStyle = "medium"),
    rows = row_data_start, cols = max(cols_all), stack = TRUE)

  # --- Footer ---
  footer_row <- max(data_rows) + 2

  # Dynamic base size
  base_formula <- paste0(
    "\"Base: \"&INDEX(_lookup!$G$2:$G$", n_entries + 1, ",", sheet_match, ")"
  )
  openxlsx::writeFormula(wb, dash, x = base_formula,
    startRow = footer_row, startCol = col_data_start)
  openxlsx::addStyle(wb, dash, style = openxlsx::createStyle(textDecoration = "bold"),
    rows = footer_row, cols = col_data_start, stack = TRUE)
  footer_row <- footer_row + 1

  footer_lines <- c(
    "Step \u2014 Priority step number (order in which attributes were selected)",
    "DV Estimate \u2014 Expected DV value with all selected attributes shifted",
    "Marginal Gain \u2014 Absolute increase in DV estimate from adding this attribute",
    "Marginal Gain % \u2014 Percentage increase relative to the previous step"
  )

  if (has_p) {
    footer_lines <- c(footer_lines,
      "p-value \u2014 Noise-floor test: proportion of bootstraps where this step's gain \u2264 the noise floor",
      paste0("Green < ", sig_threshold * 100, "%, ",
             "Orange < ", marginal_threshold * 100, "%, ",
             "Red \u2265 ", marginal_threshold * 100, "%")
    )
  }

  lift_pct <- round(lift * 100)
  footer_lines <- c(footer_lines,
    "",
    paste0("Lift \u2014 Shifts each IV's distribution upward by ", lift_pct,
           "%, reflecting a realistic improvement scenario"),
    "Max \u2014 Sets each IV to its highest level as hard evidence, representing the theoretical ceiling"
  )

  for (fi in seq_along(footer_lines)) {
    openxlsx::writeData(wb, dash, footer_lines[fi],
      startRow = footer_row + fi - 1, startCol = col_data_start)
  }

  # ---------------------------------------------------------------------------
  # 5. Hide data + lookup sheets
  # ---------------------------------------------------------------------------
  sheet_names <- names(wb)
  vis <- rep(TRUE, length(sheet_names))
  for (si in seq_along(sheet_names)) {
    if (sheet_names[si] != "Dashboard") vis[si] <- FALSE
  }
  openxlsx::sheetVisibility(wb) <- vis

  # ---------------------------------------------------------------------------
  # 6. Save
  # ---------------------------------------------------------------------------
  fname <- if (!is.null(file_name)) {
    paste0(file_name, " - Prioritization.xlsx")
  } else {
    "Prioritization.xlsx"
  }
  file_path <- file.path(path, fname)
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  cli::cli_alert_success("Prioritization workbook saved: {file_path}")

  invisible(wb)
}


# =============================================================================
# Internal: build flat registry with dimension tags
# =============================================================================
.prioritize_build_registry <- function(result) {

  registry <- list()
  sheet_counter <- 1L

  .add <- function(tbl, strategy, search, subgroup, focus, weight) {
    sn <- paste0("_d", sheet_counter)
    n_obs <- attr(tbl, "n_obs")
    registry[[length(registry) + 1]] <<- list(
      strategy = strategy,
      search = search,
      subgroup = subgroup,
      focus = focus,
      weight = weight,
      sheet_name = sn,
      tbl = tbl,
      n_obs = n_obs
    )
    sheet_counter <<- sheet_counter + 1L
  }

  # Single bn_prioritize() tibble
  if (is.data.frame(result)) {
    .add(result, strategy = "Greedy", search = "Greedy",
         subgroup = "Total", focus = "Market", weight = "Unweighted")
    return(registry)
  }

  meta <- result[["meta"]]
  has_subgroups <- !is.null(meta[["subgroups"]])

  # Helper to iterate subgroups or treat as single
  .iter <- function(variant_data, strategy, search, focus, weight) {
    if (has_subgroups && is.list(variant_data) && !is.data.frame(variant_data)) {
      for (sg_name in names(variant_data)) {
        .add(variant_data[[sg_name]], strategy = strategy, search = search,
             subgroup = sg_name, focus = focus, weight = weight)
      }
    } else {
      sg <- if (has_subgroups) meta[["subgroups"]][1] else "Total"
      .add(variant_data, strategy = strategy, search = search,
           subgroup = sg, focus = focus, weight = weight)
    }
  }

  # greedy_lift
  if (!is.null(result[["greedy_lift"]])) {
    .iter(result[["greedy_lift"]], strategy = "Lift", search = "Greedy",
          focus = "Market", weight = "Unweighted")
  }

  # greedy_max
  if (!is.null(result[["greedy_max"]])) {
    .iter(result[["greedy_max"]], strategy = "Max", search = "Greedy",
          focus = "Market", weight = "Unweighted")
  }

  # greedy_lift_weighted
  if (!is.null(result[["greedy_lift_weighted"]])) {
    .iter(result[["greedy_lift_weighted"]], strategy = "Lift", search = "Greedy",
          focus = "Market", weight = "Weighted")
  }

  # greedy_lift_brand
  if (!is.null(result[["greedy_lift_brand"]])) {
    for (brand_name in names(result[["greedy_lift_brand"]])) {
      brand_result <- result[["greedy_lift_brand"]][[brand_name]]
      .iter(brand_result, strategy = "Lift", search = "Greedy",
            focus = brand_name, weight = "Unweighted")
    }
  }

  # greedy_lift_brand_weighted
  if (!is.null(result[["greedy_lift_brand_weighted"]])) {
    for (brand_name in names(result[["greedy_lift_brand_weighted"]])) {
      brand_result <- result[["greedy_lift_brand_weighted"]][[brand_name]]
      .iter(brand_result, strategy = "Lift", search = "Greedy",
            focus = brand_name, weight = "Weighted")
    }
  }

  registry
}
