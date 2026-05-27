#' seg_shell_add_crosstab
#'
#' @description Appends a `Crosstabs` sheet to a solution workbook containing a
#'   pairwise crosstab block for every unique pair of solution columns in
#'   `variables`. Each block has three vertically stacked panels — raw counts,
#'   row percentages, and column percentages — and blocks are arranged
#'   side-by-side across the sheet. Styling (border weight, font size, header
#'   fill, alignment) matches [seg_write_shell()] conventions.
#'
#' @param seg A seg object with `seg$data$with_solutions` populated.
#' @param solution Character. The solution whose workbook will receive the new
#'   sheet. Used to resolve `file_location` when `file_location` is `NULL`.
#' @param variables Character vector of column names in
#'   `seg$data$with_solutions`. Every unique pair (combinations of 2) is
#'   crosstabbed and written as its own block.
#' @param file_location Character. Path to the solution Excel file. Defaults to
#'   `{seg$paths$folders$solution}/{solution}/Solution - {solution}.xlsx`.
#' @param sheet_name Character. Sheet name (default `"Crosstabs"`). If a sheet
#'   with this name already exists it is removed and re-added.
#' @param quietly Logical. If `TRUE` (default), suppress console feedback other
#'   than the final write-confirmation message.
#'
#' @return The seg object (invisibly).
#'
#' @export
seg_shell_add_crosstab <- function(
    seg,
    solution,
    variables,
    file_location = NULL,
    sheet_name    = "Crosstabs",
    quietly       = TRUE
){


  if(length(variables) < 2){
    cli::cli_abort("Need at least 2 entries in {.arg variables} to form a pair.")
  }


  # Tolerate accidental "Solution - " prefix on solution / variables — that's a
  # file-naming convention, not part of the column / folder name.
  solution  <- sub("^Solution - ", "", solution)
  variables <- sub("^Solution - ", "", variables)


  if(is.null(file_location)){
    file_location <- .crosstab_resolve_path(seg = seg, solution = solution)
  }


  if(is.null(file_location) || !file.exists(file_location)){
    cli::cli_abort(c(
      "Solution workbook not found for {.val {solution}}.",
      "i" = "Pass {.arg file_location} explicitly, or check {.arg solution}."
    ))
  }


  df <- seg[["data"]][["with_solutions"]]

  missing_vars <- setdiff(variables, names(df))
  if(length(missing_vars) > 0){
    cli::cli_abort(
      "Variable{?s} not found in {.field seg$data$with_solutions}: {.val {missing_vars}}"
    )
  }


  # If `solution` is one of the variables, anchor it as the row variable in
  # the first set of pairs so its blocks render on the left of the sheet,
  # then fall back to combn ordering for the remaining pairs. Otherwise just
  # use combn over `variables` as-is.
  if(solution %in% variables){
    others        <- setdiff(variables, solution)
    primary_pairs <- lapply(others, function(v) c(solution, v))
    other_pairs   <- if(length(others) >= 2){
      utils::combn(others, 2, simplify = FALSE)
    }else{
      list()
    }
    pairs <- c(primary_pairs, other_pairs)
  }else{
    pairs <- utils::combn(variables, 2, simplify = FALSE)
  }


  if(!quietly){
    cli::cli_h2("seg_shell_add_crosstab")
    cli::cli_alert_info("Writing {length(pairs)} crosstab block{?s} to {.val {sheet_name}}")
  }


  # ---- open workbook, replace target sheet ----
  wb <- openxlsx::loadWorkbook(file_location)

  if(sheet_name %in% openxlsx::sheets(wb)){
    openxlsx::removeWorksheet(wb, sheet_name)
  }
  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)


  styles <- .crosstab_styles()


  # ---- render each pair side-by-side ----
  col_cursor <- 2
  row_start  <- 2
  gap_cols   <- 1

  for(i in seq_along(pairs)){

    pair      <- pairs[[i]]
    row_var   <- pair[[1]]
    col_var   <- pair[[2]]
    block_w   <- .crosstab_write_pair(
      wb         = wb,
      sheet_name = sheet_name,
      df         = df,
      row_var    = row_var,
      col_var    = col_var,
      row_start  = row_start,
      col_start  = col_cursor,
      styles     = styles
    )

    col_cursor <- col_cursor + block_w + gap_cols


    if(!quietly){
      cli::cli_bullets(c("v" = "{.val {row_var}}  x  {.val {col_var}}"))
    }
  }


  openxlsx::saveWorkbook(wb, file_location, overwrite = TRUE)

  cli::cli_alert_success("Added {.val {sheet_name}} sheet to {.path {file_location}}")

  invisible(seg)
}


# ---- internal helpers --------------------------------------------------------

#' Locate the `Solution - {solution}.xlsx` workbook. First tries the
#' conventional path (`{seg$paths$folders$solution}/{solution}/...`), then
#' falls back to a recursive search under the solutions folder so the function
#' works regardless of whether solutions are organised by solution name or by
#' family folder. Mirrors the fallback logic in [seg_describe_solutions()].
#' @keywords internal
.crosstab_resolve_path <- function(seg, solution){

  where <- seg[["paths"]][["folders"]][["solution"]]

  if(is.null(where) || is.na(where) || !nzchar(where)){
    where <- getwd()
  }


  sol_filename <- glue::glue("Solution - {solution}.xlsx")


  conventional <- file.path(where, solution, sol_filename)
  if(file.exists(conventional)){
    return(conventional)
  }


  candidates <- list.files(
    where,
    pattern    = paste0("^Solution - ", solution, "\\.xlsx$"),
    recursive  = TRUE,
    full.names = TRUE
  )

  candidates <- candidates[!grepl("~\\$", candidates)]
  candidates <- candidates[!grepl("previous|archive|old|backup", candidates, ignore.case = TRUE)]


  if(length(candidates) == 0){
    return(NULL)
  }

  candidates[[1]]
}


#' Style set used across the Crosstabs sheet. Mirrors the conventions used by
#' [seg_write_shell()] — default 11pt body font, bold + `#e0e0e0` fill for
#' headers, `medium` outer borders applied via [oxl_outer_box()].
#' @keywords internal
.crosstab_styles <- function(){
  list(
    label       = openxlsx::createStyle(
      textDecoration = "Bold",
      halign         = "center",
      valign         = "center",
      wrapText       = TRUE
    ),
    header      = openxlsx::createStyle(textDecoration = "Bold", halign = "center"),
    count       = openxlsx::createStyle(halign = "center", numFmt = "0"),
    percent     = openxlsx::createStyle(halign = "center", numFmt = "0%"),
    total       = openxlsx::createStyle(textDecoration = "Bold", halign = "center"),
    color_scale = c("#f8696a", "#feea84", "#63be7b")   # low -> mid -> high (red -> yellow -> green)
  )
}


#' Build an Excel A1-style cell reference (e.g. `B5`, `$B5`, `B$5`, `$B$5`).
#' @keywords internal
.cell_ref <- function(row, col, abs_row = FALSE, abs_col = FALSE){
  paste0(
    if(abs_col) "$" else "",
    num2let(col),
    if(abs_row) "$" else "",
    row
  )
}


#' Build an Excel A1-style range string (e.g. `B5:H5`).
#' @keywords internal
.cell_range <- function(r1, c1, r2, c2, abs_row = FALSE, abs_col = FALSE){
  paste0(
    .cell_ref(r1, c1, abs_row = abs_row, abs_col = abs_col), ":",
    .cell_ref(r2, c2, abs_row = abs_row, abs_col = abs_col)
  )
}


#' Compute panel coordinate landmarks for a block starting at
#' (`row_start`, `col_start`) with `n_row_cats` row segments and `n_col_cats`
#' column segments. Each panel reserves: 1 col-solution-label row, 1
#' col-header row, `n_row_cats` data rows, and 1 Total row (and analogous
#' columns).
#' @keywords internal
.crosstab_panel_coords <- function(row_start, col_start, n_row_cats, n_col_cats){
  list(
    r_col_label       = row_start,
    r_col_header      = row_start + 1,
    r_data_first      = row_start + 2,
    r_data_last_inner = row_start + 2 + n_row_cats - 1,
    r_total           = row_start + 2 + n_row_cats,
    c_row_label       = col_start,
    c_row_header      = col_start + 1,
    c_data_first      = col_start + 2,
    c_data_last_inner = col_start + 2 + n_col_cats - 1,
    c_total           = col_start + 2 + n_col_cats
  )
}


#' Write one (row_var, col_var) crosstab block: three panels (counts, row %,
#' col %) stacked vertically with a 2-row gap between panels. Counts are
#' hardcoded for the inner cells; both percentage panels are entirely
#' formulaic and reference the counts panel. Returns the block width in
#' columns so the caller can advance the cursor.
#' @keywords internal
.crosstab_write_pair <- function(wb, sheet_name, df, row_var, col_var, row_start, col_start, styles){


  d <- df %>%
    dplyr::select(dplyr::all_of(c(row_var, col_var))) %>%
    dplyr::filter(!is.na(.data[[row_var]]) & !is.na(.data[[col_var]]))


  # Numeric segment categories — used as headers (stored as numerics in Excel)
  # and as factor levels so `table()` honours sort order and complete coverage.
  row_cats <- sort(unique(d[[row_var]]))
  col_cats <- sort(unique(d[[col_var]]))


  counts <- table(
    factor(d[[row_var]], levels = row_cats),
    factor(d[[col_var]], levels = col_cats)
  )
  storage.mode(counts) <- "integer"


  n_row_cats <- length(row_cats)
  n_col_cats <- length(col_cats)


  # All three panels share the same shape.
  panel_height <- 2 + (n_row_cats + 1)   # col-label + header + data + Total
  panel_gap    <- 2                      # blank rows between panels
  block_width  <- 2 + (n_col_cats + 1)   # row-label + row-cat + data + Total


  # Panel anchors.
  r_counts  <- row_start
  r_row_pct <- row_start + (panel_height + panel_gap)
  r_col_pct <- row_start + 2 * (panel_height + panel_gap)


  counts_coords  <- .crosstab_panel_coords(r_counts , col_start, n_row_cats, n_col_cats)
  row_pct_coords <- .crosstab_panel_coords(r_row_pct, col_start, n_row_cats, n_col_cats)
  col_pct_coords <- .crosstab_panel_coords(r_col_pct, col_start, n_row_cats, n_col_cats)


  # ---- panel 1: counts (hardcoded inner data, formulaic totals) ----
  .crosstab_write_chrome(
    wb, sheet_name, row_var, col_var, row_cats, col_cats,
    coords = counts_coords, styles = styles, value_style = styles$count
  )

  .crosstab_write_counts_data(
    wb, sheet_name, counts = counts, coords = counts_coords
  )


  # ---- panel 2: row % (entirely formulaic, refs counts panel) ----
  .crosstab_write_chrome(
    wb, sheet_name, row_var, col_var, row_cats, col_cats,
    coords = row_pct_coords, styles = styles, value_style = styles$percent
  )

  .crosstab_write_pct_data(
    wb, sheet_name, panel_type = "row_pct",
    coords = row_pct_coords, counts_coords = counts_coords
  )


  # ---- panel 3: col % (entirely formulaic, refs counts panel) ----
  .crosstab_write_chrome(
    wb, sheet_name, row_var, col_var, row_cats, col_cats,
    coords = col_pct_coords, styles = styles, value_style = styles$percent
  )

  .crosstab_write_pct_data(
    wb, sheet_name, panel_type = "col_pct",
    coords = col_pct_coords, counts_coords = counts_coords
  )


  # Auto-fit the row-solution label column so the full solution name shows
  # without truncation. Applied once per block (col_start = c_row_label for
  # every panel in this block).
  openxlsx::setColWidths(
    wb, sheet_name,
    cols   = col_start,
    widths = "auto"
  )


  block_width
}


#' Write the shared panel chrome — solution labels, numeric segment headers
#' (with the trailing "Total" cell written as text), Total row/col bolding,
#' value styling, outer border, and the green-yellow-red color scale on the
#' inner data rectangle. Does NOT write the data values themselves.
#' @keywords internal
.crosstab_write_chrome <- function(wb, sheet_name, row_var, col_var, row_cats, col_cats,
                                    coords, styles, value_style){


  # 1) Column solution label — merged across the data cols only (NOT the
  # Total col). The outer-panel box drawn at the end forms the visible
  # rectangle around the label.
  openxlsx::writeData(
    wb, sheet_name,
    x        = col_var,
    startRow = coords$r_col_label,
    startCol = coords$c_data_first,
    colNames = FALSE
  )
  openxlsx::mergeCells(
    wb, sheet_name,
    rows = coords$r_col_label,
    cols = coords$c_data_first:coords$c_data_last_inner
  )
  openxlsx::addStyle(
    wb, sheet_name,
    style      = styles$label,
    rows       = coords$r_col_label,
    cols       = coords$c_data_first:coords$c_data_last_inner,
    gridExpand = TRUE,
    stack      = TRUE
  )


  # 2a) Column category headers — segment numbers stored as numerics
  openxlsx::writeData(
    wb, sheet_name,
    x        = matrix(as.numeric(col_cats), nrow = 1),
    startRow = coords$r_col_header,
    startCol = coords$c_data_first,
    colNames = FALSE
  )

  # 2b) "Total" header at the end (text)
  openxlsx::writeData(
    wb, sheet_name,
    x        = "Total",
    startRow = coords$r_col_header,
    startCol = coords$c_total,
    colNames = FALSE
  )

  openxlsx::addStyle(
    wb, sheet_name,
    style      = styles$header,
    rows       = coords$r_col_header,
    cols       = coords$c_data_first:coords$c_total,
    gridExpand = TRUE,
    stack      = TRUE
  )


  # 3) Row solution label — merged down the data rows only (NOT the Total
  # row). The outer-panel box drawn at the end forms the visible rectangle
  # around the label.
  openxlsx::writeData(
    wb, sheet_name,
    x        = row_var,
    startRow = coords$r_data_first,
    startCol = coords$c_row_label,
    colNames = FALSE
  )
  openxlsx::mergeCells(
    wb, sheet_name,
    rows = coords$r_data_first:coords$r_data_last_inner,
    cols = coords$c_row_label
  )
  openxlsx::addStyle(
    wb, sheet_name,
    style      = styles$label,
    rows       = coords$r_data_first:coords$r_data_last_inner,
    cols       = coords$c_row_label,
    gridExpand = TRUE,
    stack      = TRUE
  )


  # 4a) Row category labels — segment numbers stored as numerics
  openxlsx::writeData(
    wb, sheet_name,
    x        = matrix(as.numeric(row_cats), ncol = 1),
    startRow = coords$r_data_first,
    startCol = coords$c_row_header,
    colNames = FALSE
  )

  # 4b) "Total" row label at the bottom (text)
  openxlsx::writeData(
    wb, sheet_name,
    x        = "Total",
    startRow = coords$r_total,
    startCol = coords$c_row_header,
    colNames = FALSE
  )

  openxlsx::addStyle(
    wb, sheet_name,
    style      = styles$header,
    rows       = coords$r_data_first:coords$r_total,
    cols       = coords$c_row_header,
    gridExpand = TRUE,
    stack      = TRUE
  )


  # 5) Value style across the whole data + totals rectangle
  openxlsx::addStyle(
    wb, sheet_name,
    style      = value_style,
    rows       = coords$r_data_first:coords$r_total,
    cols       = coords$c_data_first:coords$c_total,
    gridExpand = TRUE,
    stack      = TRUE
  )


  # 6) Bold the Total row and Total column
  openxlsx::addStyle(
    wb, sheet_name,
    style      = styles$total,
    rows       = coords$r_total,
    cols       = coords$c_row_header:coords$c_total,
    gridExpand = TRUE,
    stack      = TRUE
  )

  openxlsx::addStyle(
    wb, sheet_name,
    style      = styles$total,
    rows       = coords$r_data_first:coords$r_total,
    cols       = coords$c_total,
    gridExpand = TRUE,
    stack      = TRUE
  )


  # 7) Green-yellow-red color scale on the inner data rectangle (excludes
  # the Total row + Total col so the gradient isn't dominated by totals).
  openxlsx::conditionalFormatting(
    wb, sheet_name,
    rows  = coords$r_data_first:coords$r_data_last_inner,
    cols  = coords$c_data_first:coords$c_data_last_inner,
    type  = "colourScale",
    style = styles$color_scale
  )


  # 8a) Inner values box: medium border around the segment headers + data +
  # Total row/col (excludes the merged solution labels).
  oxl_outer_box(
    wb, sheet_name,
    row_start   = coords$r_col_header,
    row_end     = coords$r_total,
    col_start   = coords$c_row_header,
    col_end     = coords$c_total,
    borderStyle = "medium"
  )


  # 8b) Full outer box: medium border around the entire panel including both
  # solution labels. Closes the perimeter past the (now-empty) Total cells
  # in the label row/col so the panel reads as a single bounded unit.
  oxl_outer_box(
    wb, sheet_name,
    row_start   = coords$r_col_label,
    row_end     = coords$r_total,
    col_start   = coords$c_row_label,
    col_end     = coords$c_total,
    borderStyle = "medium"
  )


  invisible(NULL)
}


#' Write the counts panel data: hardcoded inner cells + SUM formulas for the
#' Total row, Total col, and grand total — so a manual edit to any inner cell
#' propagates through totals and into the row-% / col-% panels.
#' @keywords internal
.crosstab_write_counts_data <- function(wb, sheet_name, counts, coords){


  n_row_cats <- nrow(counts)
  n_col_cats <- ncol(counts)


  # Strip class / dimnames completely. Passing a `table` object (or even a
  # named matrix produced by `cbind(table, ...)`) to openxlsx::writeData
  # triggers a long-format `as.data.frame.table` coercion that shifts the
  # block right by one column. Building a fresh integer matrix avoids that.
  counts_mx <- matrix(
    as.integer(counts),
    nrow = n_row_cats,
    ncol = n_col_cats
  )


  # Inner data — write each row as a 1-row matrix to keep the writeData
  # path on a code path we know is offset-safe.
  for(i in seq_len(n_row_cats)){
    openxlsx::writeData(
      wb, sheet_name,
      x        = matrix(counts_mx[i, ], nrow = 1),
      startRow = coords$r_data_first + i - 1,
      startCol = coords$c_data_first,
      colNames = FALSE
    )
  }


  # Total col (per-row sums)
  for(i in seq_len(n_row_cats)){
    r <- coords$r_data_first + i - 1
    openxlsx::writeFormula(
      wb, sheet_name,
      x        = paste0("=SUM(",
                        .cell_range(r, coords$c_data_first, r, coords$c_data_last_inner),
                        ")"),
      startRow = r,
      startCol = coords$c_total
    )
  }


  # Total row (per-col sums)
  for(j in seq_len(n_col_cats)){
    c <- coords$c_data_first + j - 1
    openxlsx::writeFormula(
      wb, sheet_name,
      x        = paste0("=SUM(",
                        .cell_range(coords$r_data_first, c, coords$r_data_last_inner, c),
                        ")"),
      startRow = coords$r_total,
      startCol = c
    )
  }


  # Grand total (sum of Total column)
  openxlsx::writeFormula(
    wb, sheet_name,
    x        = paste0("=SUM(",
                      .cell_range(coords$r_data_first, coords$c_total,
                                  coords$r_data_last_inner, coords$c_total),
                      ")"),
    startRow = coords$r_total,
    startCol = coords$c_total
  )


  invisible(NULL)
}


#' Write a percentage panel (row % or col %) entirely as formulas that
#' reference the counts panel at `counts_coords`. Inner cells divide a counts
#' cell by the appropriate marginal total; Total rows/cols are computed via
#' SUM of the inner percentages (= 1) or via the counts marginals (= marginal
#' share of grand total) depending on direction.
#' @keywords internal
.crosstab_write_pct_data <- function(wb, sheet_name, panel_type, coords, counts_coords){


  panel_type <- match.arg(panel_type, c("row_pct", "col_pct"))


  n_row_cats <- counts_coords$r_data_last_inner - counts_coords$r_data_first + 1
  n_col_cats <- counts_coords$c_data_last_inner - counts_coords$c_data_first + 1


  for(i in seq_len(n_row_cats + 1)){
    for(j in seq_len(n_col_cats + 1)){

      # Target cell in this panel.
      r_target <- coords$r_data_first + i - 1
      c_target <- coords$c_data_first + j - 1

      # Corresponding counts cell (same i, j).
      r_counts <- counts_coords$r_data_first + i - 1
      c_counts <- counts_coords$c_data_first + j - 1

      is_total_row <- (i == n_row_cats + 1)
      is_total_col <- (j == n_col_cats + 1)


      f <- if(panel_type == "row_pct"){

        if(!is_total_row && !is_total_col){
          # inner: count / row total
          paste0("=",
                 .cell_ref(r_counts, c_counts),
                 "/",
                 .cell_ref(r_counts, counts_coords$c_total, abs_col = TRUE))

        }else if(!is_total_row && is_total_col){
          # Total col: sum of inner row %s in this panel = 1
          paste0("=SUM(",
                 .cell_range(r_target, coords$c_data_first,
                             r_target, coords$c_data_last_inner),
                 ")")

        }else if(is_total_row && !is_total_col){
          # Total row: col marginal share = col total / grand total
          paste0("=",
                 .cell_ref(counts_coords$r_total, c_counts, abs_row = TRUE),
                 "/",
                 .cell_ref(counts_coords$r_total, counts_coords$c_total,
                           abs_row = TRUE, abs_col = TRUE))

        }else{
          # bottom-right: sum of bottom row inner = 1
          paste0("=SUM(",
                 .cell_range(r_target, coords$c_data_first,
                             r_target, coords$c_data_last_inner),
                 ")")
        }

      }else{   # col_pct

        if(!is_total_row && !is_total_col){
          # inner: count / col total
          paste0("=",
                 .cell_ref(r_counts, c_counts),
                 "/",
                 .cell_ref(counts_coords$r_total, c_counts, abs_row = TRUE))

        }else if(is_total_row && !is_total_col){
          # Total row: sum of inner col %s in this panel = 1
          paste0("=SUM(",
                 .cell_range(coords$r_data_first, c_target,
                             coords$r_data_last_inner, c_target),
                 ")")

        }else if(!is_total_row && is_total_col){
          # Total col: row marginal share = row total / grand total
          paste0("=",
                 .cell_ref(r_counts, counts_coords$c_total, abs_col = TRUE),
                 "/",
                 .cell_ref(counts_coords$r_total, counts_coords$c_total,
                           abs_row = TRUE, abs_col = TRUE))

        }else{
          # bottom-right: sum of right col inner = 1
          paste0("=SUM(",
                 .cell_range(coords$r_data_first, c_target,
                             coords$r_data_last_inner, c_target),
                 ")")
        }
      }


      openxlsx::writeFormula(
        wb, sheet_name,
        x        = f,
        startRow = r_target,
        startCol = c_target
      )
    }
  }


  invisible(NULL)
}
