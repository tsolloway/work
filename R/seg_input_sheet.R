#' seg_input_sheet
#'
#' @description Generates the segmentation input sheet Excel workbook with three
#'   tabs: Inputs (variable selection matrix), Rational (solution rationale), and
#'   Prototype (segment prototypes with formula-driven recoding).
#'
#' @param seg A seg object containing spec, data, and path information.
#' @param fa_winner Integer. The winning PCA/factor solution number to use.
#' @param range_predictors Numeric vector of length 2. The min and max number of
#'   predictor variables to evaluate.
#' @param where Character. Output directory path. Defaults to the process folder
#'   in the seg object, or the working directory if not set.
#' @param file_name Character. Base file name for the output Excel file
#'   (default: `"Input Sheet"`).
#' @param add_proj_name_to_file Logical. If `TRUE` (default), prepends the
#'   project name from the seg object to the file name.
#'
#' @return The seg object with `seg[["paths"]][["files"]][["input"]]` set to the
#'   saved file path.
#'
#' @export
seg_input_sheet <- function(
    seg,
    fa_winner,
    range_predictors,
    polar_type = c("rs", "source"),
    where = NULL,
    file_name = "Input Sheet",
    add_proj_name_to_file = TRUE
){

  polar_type <- match.arg(polar_type)

  if(is.null(where)){

    where <- seg[["paths"]][["folders"]][["process"]]

    if(is.null(where)){
      where <- getwd()
    }
  }


  color_scale_colors <- c("#f8696a", "#feea84", "#63be7b")
  color_scale_colors_rev <- color_scale_colors %>% rev()

  styles <- list(
    header_grey    = openxlsx::createStyle(textDecoration = "bold", fgFill = "#BFBFBF", halign = "center", valign = "center", wrapText = TRUE),
    header_grey_nw = openxlsx::createStyle(textDecoration = "bold", fgFill = "#BFBFBF", halign = "center", valign = "center"),
    header_orange  = openxlsx::createStyle(textDecoration = "bold", halign = "center", fgFill = "#FCD5B4"),
    header_blue    = openxlsx::createStyle(fgFill = "#B7DEE8", border = "TopBottomLeftRight", borderStyle = "thick"),
    center         = openxlsx::createStyle(halign = "center"),
    center_bold    = openxlsx::createStyle(halign = "center", valign = "center", textDecoration = "bold"),
    center_dec     = openxlsx::createStyle(numFmt = "0.00"),
    center_pct     = openxlsx::createStyle(numFmt = "0.0%", halign = "center"),
    center_int     = openxlsx::createStyle(numFmt = "0", halign = "center", border = "TopBottomLeftRight"),
    center_dec_c   = openxlsx::createStyle(numFmt = "0.00", halign = "center"),
    wrap_center    = openxlsx::createStyle(valign = "center", wrapText = TRUE),
    grey_fill      = openxlsx::createStyle(bgFill = "#e0e0e0"),
    bold_orange    = openxlsx::createStyle(textDecoration = "bold", bgFill = "#FCD5B4"),
    bold_header    = openxlsx::createStyle(textDecoration = "bold", fgFill = "#BFBFBF", halign = "center"),
    color_scale    = color_scale_colors,
    color_scale_rev = color_scale_colors_rev
  )


  seg <- seg %>% seg_get_fa_winner(winner = fa_winner, polar_type = polar_type)


  seg <- seg %>% seg_organize_input_sheet(range_predictors = range_predictors)


  wb <- oxl_create_workbook()


  .input_append_input_sheet(wb, seg_input_table = seg[["input_sheet"]][["input_table"]], styles = styles, sheet_name = "Inputs", row_start = 6, col_start = 2)


  .input_append_rational_sheet(wb, rational_table = seg[["input_sheet"]][["solution_rational"]], styles = styles, sheet_name = "Rational", row_start = 2, col_start = 2)


  .input_append_prototype_sheet(wb, prototype_table = seg[["input_sheet"]][["prototype_table"]], styles = styles, sheet_name = "Prototype", row_start = 2,  col_start = 2, segs_max = 10)



  if(add_proj_name_to_file){
    file_name <- seg_glue_proj_name_to_file(seg, file_name)
  }


  file_location <- glue("{where}/{file_name}.xlsx")


  openxlsx::saveWorkbook(wb, file_location, overwrite = TRUE)


  seg[["paths"]][["files"]][["input"]] <- file_location


  return(seg)
}


#' @keywords internal
.input_append_input_sheet <- function(
    wb, seg_input_table, styles, sheet_name = "Inputs", row_start = 6, col_start = 2
){

  input_table <- seg_input_table %>%
    mutate(.,
           " " = rep("", nrow(.))
    ) %>%
    relocate(" ", .before = "solution_a")


  for(i in letters[5:26]){
    input_table[[glue("solution_{i}")]] <- NA
  }


  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)


  rows_all <- seq(row_start + 1, row_start + nrow(input_table))
  rows_first <- rows_all %>% head(1)
  rows_end <- rows_all %>% tail(1)

  cols_all <- seq(col_start, col_start + ncol(input_table) - 1)
  cols_solutions <- seq(col_start + 14, cols_all %>% tail(1))
  cols_first_letter <- cols_all %>% num2let() %>% head(1)
  cols_end_letter <- cols_all %>% num2let() %>% tail(1)

  cell_solution <- glue("${num2let(col_start + 2)}${row_start - 2}")
  cell_var_type <- glue("${num2let(col_start + 2)}${row_start - 3}")
  cell_quote <- glue("${num2let(col_start + 2)}${row_start - 4}")

  range_table <- glue('${cols_first_letter}${rows_first}:${cols_end_letter}${rows_end}')
  range_header <- glue('${cols_first_letter}${row_start}:${cols_end_letter}${row_start}')


  names(input_table) <- names(input_table) %>%
    gsub("_", " ", .) %>%
    stringr::str_to_title() %>%
    gsub("Solution", "", .) %>%
    gsub("Rs", "RS", .) %>%
    gsub("Sd", "SD", .) %>%
    gsub("Fa N", "FA #", .) %>%
    gsub("Loading", "Load", .) %>%
    gsub("FA #ame", "FA Name", .) %>%
    gsub("Rp", "RP", .) %>%
    gsub("Threshold", "Thresh", .) %>%
    gsub("Var", "", .) %>%
    stringr::str_squish()


  walk(
    seq(4,2),
    ~ {
      openxlsx::mergeCells(wb, sheet_name, cols = c(col_start, col_start + 1), rows = row_start - .x)
      if(.x == 2){
        openxlsx::mergeCells(wb, sheet_name, cols = c(col_start + 3, col_start + 13), rows = row_start - .x)
        oxl_outer_box(
          wb, sheet_name,
          row_start = row_start - .x, row_end = row_start - .x,
          col_start = col_start + 3, col_end = col_start + 13,
          borderStyle = "thick"
        )
      }
    }
  )


  openxlsx::writeData(
    wb, sheet_name,
    x = data.frame(
      x = c("Quote", "Var Type", "Solution"),
      foo = rep(NA, 3),
      y = c(1, "RS", "A")
    ),
    startRow = row_start - 4,
    startCol = col_start,
    colNames = FALSE,
    borders = "all",
    borderStyle = "thick"
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$header_orange,
    rows = row_start - 4:2, cols = col_start + 0:2,
    gridExpand = TRUE, stack = TRUE
  )


  openxlsx::writeFormula(
    wb, sheet_name,
    startCol = col_start + 3,
    startRow = row_start - 2,
    array = TRUE,
    x = glue('=CONCAT(
  IF({cell_quote}*1 = 1, "foo_quote_foo", ""),
  TEXTJOIN(
  IF({cell_quote}*1 = 1, "foo_quote_foo, foo_quote_foo", ", "),
  TRUE,
  IF(INDEX({range_table},,MATCH({cell_solution},{range_header},0)) = "x",INDEX({range_table},,MATCH({cell_var_type},{range_header},0)),"")
  ),
  IF(D2*1 = 1, "foo_quote_foo", "")
  )
') %>%
      gsub("foo_quote_foo", "'", .)
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$header_blue,
    rows = row_start - 2, cols = col_start + 3,
    gridExpand = TRUE, stack = TRUE
  )


  openxlsx::writeData(
    wb, sheet_name,
    x = input_table,
    startRow = row_start,
    startCol = col_start,
    colNames = TRUE,
    borders = "all",
    headerStyle = styles$header_grey
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = row_start,
    col_start = col_start, col_end = col_start + ncol(input_table) - 1,
    borderStyle = "thick"
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = row_start + nrow(input_table),
    col_start = col_start, col_end = col_start + ncol(input_table) - 1,
    borderStyle = "thick"
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$center,
    rows = rows_all, cols = cols_all[-c(12, 13)],
    gridExpand = TRUE, stack = TRUE
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$center_dec,
    rows = rows_all, cols = col_start + c(4, 5, 8, 10),
    gridExpand = TRUE, stack = TRUE
  )


  walk(
    cols_solutions,
    ~openxlsx::writeFormula(
      wb, sheet_name,
      startRow = row_start - 1, startCol = .x,
      x = glue('=COUNTIF({num2let(.x)}${rows_first}:{num2let(.x)}${rows_end},"x")')
    )
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$center_int,
    rows = row_start - 1, cols = cols_solutions,
    gridExpand = TRUE, stack = TRUE
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start - 1, row_end = row_start - 1,
    col_start = cols_solutions %>% head(1), col_end = cols_solutions %>% tail(1),
    borderStyle = "thick"
  )


  openxlsx::conditionalFormatting(
    wb, sheet_name,
    cols = cols_all, rows = rows_all,
    rule = glue("=ISEVEN(${num2let(col_start)}{head(rows_all, 1)})"),
    style = styles$grey_fill
  )


  openxlsx::conditionalFormatting(
    wb, sheet_name,
    cols = cols_solutions, rows = rows_all,
    rule = glue('=AND(
              UPPER(TRIM({cell_solution})) = UPPER(TRIM({cols_solutions %>% head(1) %>% num2let()}${row_start})),
              UPPER(TRIM({cols_solutions %>% head(1) %>% num2let()}{rows_first})) = "X"
              )'),
    style = styles$bold_orange
  )



  walk(
    col_start + c(4:5, 7:9),
    ~openxlsx::conditionalFormatting(
      wb, sheet_name,
      rows = rows_all, cols = .x,
      type = "colourScale",
      style = styles$color_scale
    )
  )


  openxlsx::conditionalFormatting(
    wb, sheet_name,
    rows = rows_all, cols = col_start + 10,,
    type = "colourScale",
    style = styles$color_scale_rev
  )


  openxlsx::setColWidths(wb, sheet_name, cols = c(col_start - 1, cols_all[length(cols_all)] + 1), widths = 1)
  openxlsx::setColWidths(wb, sheet_name, cols = cols_all[14], widths = .1)
  openxlsx::setColWidths(wb, sheet_name, cols = cols_all[c(1:3, 5:6, 8:11)], widths = 6)
  openxlsx::setColWidths(wb, sheet_name, cols = cols_all[15:length(cols_all)], widths = 3)
  openxlsx::setColWidths(wb, sheet_name, cols = cols_all[c(4, 7)], widths = 8)
  openxlsx::setColWidths(wb, sheet_name, cols = cols_all[c(12, 13)], widths = 80)

  openxlsx::setRowHeights(wb, sheet_name, rows = row_start, heights = 55)

  openxlsx::addFilter(wb, sheet_name, rows = row_start, cols = cols_all)

  openxlsx::groupRows(wb, sheet_name, rows = c(row_start - 4:3), hidden = TRUE) %>% suppressWarnings()
  openxlsx::groupColumns(wb, sheet_name, cols = cols_all[13], hidden = TRUE) %>% suppressWarnings()

  openxlsx::freezePane(
    wb, sheet_name,
    firstActiveRow = rows_first,
    firstActiveCol = cols_solutions %>% head(1),
  )

}


#' @keywords internal
.input_append_rational_sheet <- function(
    wb, rational_table, styles, sheet_name = "Rational", row_start = 2, col_start = 2
){

  names(rational_table) <- names(rational_table) %>% stringr::str_to_title()


  rows_all <- seq(row_start + 1, row_start + nrow(rational_table))


  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)


  openxlsx::writeData(
    wb, sheet_name,
    x = rational_table,
    startRow = row_start,
    startCol = col_start,
    colNames = TRUE,
    borders = "all",
    headerStyle = styles$header_grey_nw
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = row_start,
    col_start = col_start, col_end = col_start + ncol(rational_table) - 1,
    borderStyle = "thick"
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = rows_all %>% head(1), row_end = rows_all %>% tail(1),
    col_start = col_start, col_end = col_start + ncol(rational_table) - 1,
    borderStyle = "thick"
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$center_bold,
    rows = rows_all, cols = col_start,
    gridExpand = TRUE, stack = TRUE
  )


  openxlsx::addStyle(
    wb, sheet_name,
    style = styles$wrap_center,
    rows = rows_all, cols = col_start + 1,
    gridExpand = TRUE, stack = TRUE
  )


  openxlsx::setColWidths(wb, sheet_name, cols = c(col_start - 1, col_start + 2), widths = 1)
  openxlsx::setColWidths(wb, sheet_name, cols = col_start, widths = 10)
  openxlsx::setColWidths(wb, sheet_name, cols = col_start + 1, widths = 100)

  openxlsx::setRowHeights(wb, sheet_name, rows = rows_all, heights = 20)

  openxlsx::freezePane(wb, sheet_name, firstActiveRow = row_start + 1)

}


#' @keywords internal
.input_append_prototype_sheet <- function(
    wb, prototype_table, styles, sheet_name = "Prototype", row_start = 2,  col_start = 2, segs_max = 10
){

  names(prototype_table) <- names(prototype_table) %>%
    stringr::str_to_title() %>%
    gsub("Sd", "SD", .)


  for(i in glue("Seg {seq(segs_max)}")){
    prototype_table <-  prototype_table %>% insert_missing_column(!!i)
  }

  cols_seg <- seq(ncol(prototype_table))[-c(1:4)] + col_start - 1

  for(i in glue("Func Seg {seq(segs_max)}")){
    prototype_table <-  prototype_table %>% insert_missing_column(!!i)
  }

  cols_seg_func <- seq(ncol(prototype_table))[-c(1:14)] + col_start - 1

  rows_all <- seq(row_start + 1, row_start + nrow(prototype_table))


  openxlsx::addWorksheet(wb, sheet_name, gridLines = TRUE)


  openxlsx::writeData(
    wb, sheet_name,
    x = prototype_table,
    startRow = row_start,
    startCol = col_start,
    colNames = TRUE,
    borders = "surrounding",
    borderStyle = "thick",
    headerStyle = styles$bold_header
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = row_start,
    col_start = col_start, col_end = cols_seg_func %>% tail(1),
    borderStyle = "thick"
  )


  openxlsx::addStyle(
    wb, sheet_name,
    rows = rows_all, cols = col_start + 1,
    style = styles$center,
    gridExpand = TRUE, stack = TRUE
  )


  openxlsx::addStyle(
    wb, sheet_name,
    rows = rows_all, cols = col_start + 2,
    style = styles$center_pct,
    gridExpand = TRUE, stack = TRUE
  )


  openxlsx::addStyle(
    wb, sheet_name,
    rows = rows_all, cols = col_start + 3,
    style = styles$center_dec_c,
    gridExpand = TRUE, stack = TRUE
  )


  openxlsx::addStyle(
    wb, sheet_name,
    rows = rows_all, cols = c(cols_seg, cols_seg_func),
    style = styles$center,
    gridExpand = TRUE, stack = TRUE
  )


  walk(
    2:3,
    ~openxlsx::conditionalFormatting(
      wb, sheet_name,
      rows = rows_all, cols = col_start + .x,
      type = "colourScale",
      style = styles$color_scale
    )
  )


  openxlsx::writeData(
    wb, sheet_name,
    x = data.frame(
      Solution = glue("seed_seg_{seq(segs_max)}"),
      Variables = rep(NA, segs_max)
    ),
    startRow = row_start,
    startCol = cols_seg_func %>% tail(1) + 2,
    colNames = TRUE,
    borders = "surrounding",
    borderStyle = "thick",
    headerStyle = styles$bold_header
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = row_start,
    col_start = cols_seg_func %>% tail(1) + 2, col_end = cols_seg_func %>% tail(1) + 3,
    borderStyle = "thick"
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = rows_all %>% tail(1),
    col_start = cols_seg_func %>% head(1), col_end = cols_seg_func %>% tail(1),
    borderStyle = "thick"
  )


  oxl_outer_box(
    wb, sheet_name,
    row_start = row_start, row_end = rows_all %>% tail(1),
    col_start = cols_seg %>% head(1), col_end = cols_seg %>% tail(1),
    borderStyle = "thick"
  )


  var_letter <- num2let(col_start)

  for(i in seq(segs_max)){
    seg_letter <- num2let(cols_seg[i])

    formulas <- vapply(rows_all, function(r) {
      cell_seg <- glue('{seg_letter}{r}')
      cell_var <- glue('${var_letter}{r}')
      as.character(glue(
        '=IF(ISBLANK({cell_seg}), "",',
        'IF({cell_seg} = 0, "(1-" & {cell_var} & ")",',
        'IF({cell_seg} = "n", "(1-" & {cell_var} & ")",',
        'IF({cell_seg} = 1, {cell_var},',
        'IF({cell_seg} = "y", {cell_var},',
        '"")))))'
      ))
    }, character(1))

    openxlsx::writeFormula(
      wb, sheet_name,
      startCol = cols_seg_func[i],
      startRow = rows_all[1],
      x = formulas
    )

    openxlsx::writeFormula(
      wb, sheet_name,
      startCol = cols_seg_func %>% tail(1) + 3,
      startRow = rows_all[i],
      array = TRUE,
      x = glue('=TEXTJOIN(",", TRUE, ${num2let(cols_seg_func[i])}${rows_all %>% head(1)}:${num2let(cols_seg_func[i])}${rows_all %>% tail(1)})')
    )
  }


  openxlsx::setColWidths(wb, sheet_name, cols = cols_seg, widths = 6)

  openxlsx::setColWidths(wb, sheet_name, cols = cols_seg_func, widths = 9)

  openxlsx::groupColumns(wb, sheet_name, cols = cols_seg_func, hidden = TRUE) %>% suppressWarnings()

  openxlsx::setColWidths(wb, sheet_name, cols = c(col_start - 1, cols_seg_func %>% tail(1) + 1), widths = 1)

  openxlsx::freezePane(
    wb, sheet_name,
    firstActiveCol =  cols_seg %>% head(1),
    firstActiveRow = rows_all %>% head(1)
  )

}
