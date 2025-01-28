#' seg_input_sheet
#' @description seg_input_sheet
#' @export
seg_input_sheet <- function(
    seg,
    fa_winner,
    range_predictors,
    where = NULL,
    file_name = "Input Sheet",
    add_proj_name_to_file = TRUE
){

  require(openxlsx)


  if(is.null(where)){

    where <- seg[["paths"]][["folders"]][["process"]]

    if(is.null(where)){
      where <- getwd()
    }
  }


  color_scale_colors <- c("#f8696a", "#feea84", "#63be7b")


  color_scale_colors_rev <- color_scale_colors %>% rev()


  append_input_sheet <- function(
    wb, seg_input_table, sheet_name = "Inputs", row_start = 6, col_start = 2
  ){

    input_table <- seg_input_table %>%
      mutate(.,
             " " = rep("", nrow(.))
      ) %>%
      relocate(" ", .before = "solution_a")


    for(i in letters[5:26]){
      input_table[[glue("solution_{i}")]] <- NA
    }


    addWorksheet(wb, sheet_name, gridLines = FALSE)


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
        mergeCells(wb, sheet_name, cols = c(col_start, col_start + 1), rows = row_start - .x)
        if(.x == 2){
          mergeCells(wb, sheet_name, cols = c(col_start + 3, col_start + 13), rows = row_start - .x)
          oxl_outer_box(
            wb, sheet_name,
            row_start = row_start - .x, row_end = row_start - .x,
            col_start = col_start + 3, col_end = col_start + 13,
            borderStyle = "thick"
          )
        }
      }
    )


    writeData(
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


    addStyle(
      wb, sheet_name,
      style = createStyle(textDecoration = "bold", halign = "center", fgFill = "#FCD5B4"),
      rows = row_start - 4:2, cols = col_start + 0:2,
      gridExpand = TRUE, stack = TRUE
    )


    writeFormula(
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


    addStyle(
      wb, sheet_name,
      style = createStyle(fgFill = "#B7DEE8", border = "TopBottomLeftRight", borderStyle = "thick"),
      rows = row_start - 2, cols = col_start + 3,
      gridExpand = TRUE, stack = TRUE
    )


    writeData(
      wb, sheet_name,
      x = input_table,
      startRow = row_start,
      startCol = col_start,
      colNames = TRUE,
      borders = "all",
      headerStyle = createStyle(
        textDecoration = "bold", fgFill = "#BFBFBF",
        halign = "center", valign = "center", wrapText = TRUE
      )
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


    addStyle(
      wb, sheet_name,
      style = createStyle(halign = "center"),
      rows = rows_all, cols = cols_all[-c(12, 13)],
      gridExpand = TRUE, stack = TRUE
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(numFmt = "0.00"),
      rows = rows_all, cols = col_start + c(4, 5, 8, 10),
      gridExpand = TRUE, stack = TRUE
    )


    walk(
      cols_solutions,
      ~writeFormula(
        wb, sheet_name,
        startRow = row_start - 1, startCol = .x,
        x = glue('=COUNTIF({num2let(.x)}${rows_first}:{num2let(.x)}${rows_end},"x")')
      )
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(numFmt = "0", halign = "center", border = "TopBottomLeftRight"),
      rows = row_start - 1, cols = cols_solutions,
      gridExpand = TRUE, stack = TRUE
    )


    oxl_outer_box(
      wb, sheet_name,
      row_start = row_start - 1, row_end = row_start - 1,
      col_start = cols_solutions %>% head(1), col_end = cols_solutions %>% tail(1),
      borderStyle = "thick"
    )


    conditionalFormatting(
      wb, sheet_name,
      cols = cols_all, rows = rows_all,
      rule = glue("=ISEVEN(${num2let(col_start)}{head(rows_all, 1)})"),
      style = createStyle(bgFill = "#e0e0e0")
    )


    conditionalFormatting(
      wb, sheet_name,
      cols = cols_solutions, rows = rows_all,
      rule = glue('=AND(
              UPPER(TRIM({cell_solution})) = UPPER(TRIM({cols_solutions %>% head(1) %>% num2let()}${row_start})),
              UPPER(TRIM({cols_solutions %>% head(1) %>% num2let()}{rows_first})) = "X"
              )'),
      style = createStyle(textDecoration = "bold", bgFill = "#FCD5B4")
    )



    walk(
      col_start + c(4:5, 7:9),
      ~conditionalFormatting(
        wb, sheet_name,
        rows = rows_all, cols = .x,
        type = "colourScale",
        style = color_scale_colors
      )
    )


    conditionalFormatting(
      wb, sheet_name,
      rows = rows_all, cols = col_start + 10,,
      type = "colourScale",
      style = color_scale_colors_rev
    )


    setColWidths(wb, sheet_name, cols = c(col_start - 1, cols_all[length(cols_all)] + 1), widths = 1)
    setColWidths(wb, sheet_name, cols = cols_all[14], widths = .1)
    setColWidths(wb, sheet_name, cols = cols_all[c(1:3, 5:6, 8:11)], widths = 6)
    setColWidths(wb, sheet_name, cols = cols_all[15:length(cols_all)], widths = 3)
    setColWidths(wb, sheet_name, cols = cols_all[c(4, 7)], widths = 8)
    setColWidths(wb, sheet_name, cols = cols_all[c(12, 13)], widths = 80)#, hidden = c(FALSE, TRUE))

    setRowHeights(wb, sheet_name, rows = row_start, heights = 55)

    addFilter(wb, sheet_name, rows = row_start, cols = cols_all)

    groupRows(wb, sheet_name, rows = c(row_start - 4:3), hidden = TRUE) %>% suppressWarnings()
    groupColumns(wb, sheet_name, cols = cols_all[13], hidden = TRUE) %>% suppressWarnings()

    freezePane(
      wb, sheet_name,
      firstActiveRow = rows_first,
      firstActiveCol = cols_solutions %>% head(1),
    )

  }


  append_rational_sheet <- function(
    wb, rational_table, sheet_name = "Rational", row_start = 2, col_start = 2
  ){

    names(rational_table) <- names(rational_table) %>% stringr::str_to_title()


    rows_all <- seq(row_start + 1, row_start + nrow(rational_table))


    addWorksheet(wb, sheet_name, gridLines = FALSE)


    writeData(
      wb, sheet_name,
      x = rational_table,
      startRow = row_start,
      startCol = col_start,
      colNames = TRUE,
      borders = "all",
      headerStyle = createStyle(
        textDecoration = "bold", fgFill = "#BFBFBF",
        halign = "center", valign = "center"
      )
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


    addStyle(
      wb, sheet_name,
      style = createStyle(halign = "center", valign = "center", textDecoration = "bold"),
      rows = rows_all, cols = col_start,
      gridExpand = TRUE, stack = TRUE
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(valign = "center", wrapText = TRUE),
      rows = rows_all, cols = col_start + 1,
      gridExpand = TRUE, stack = TRUE
    )


    setColWidths(wb, sheet_name, cols = c(col_start - 1, col_start + 2), widths = 1)
    setColWidths(wb, sheet_name, cols = col_start, widths = 10)
    setColWidths(wb, sheet_name, cols = col_start + 1, widths = 100)

    setRowHeights(wb, sheet_name, rows = rows_all, heights = 20)

    freezePane(wb, sheet_name, firstActiveRow = row_start + 1)

  }


  append_prototype_sheet <- function(
    wb, prototype_table, sheet_name = "Prototype", row_start = 2,  col_start = 2, segs_max = 10
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


    addWorksheet(wb, sheet_name, gridLines = TRUE)


    writeData(
      wb, sheet_name,
      x = prototype_table,
      startRow = row_start,
      startCol = col_start,
      colNames = TRUE,
      borders = "surrounding",
      borderStyle = "thick",
      headerStyle = createStyle(
        textDecoration = "bold", fgFill = "#BFBFBF", halign = "center"
      )
    )


    oxl_outer_box(
      wb, sheet_name,
      row_start = row_start, row_end = row_start,
      col_start = col_start, col_end = cols_seg_func %>% tail(1),
      borderStyle = "thick"
    )


    addStyle(
      wb, sheet_name,
      rows = rows_all, cols = col_start + 1,
      style = createStyle(halign = "center"),
      gridExpand = TRUE, stack = TRUE
    )


    addStyle(
      wb, sheet_name,
      rows = rows_all, cols = col_start + 2,
      style = createStyle(numFmt = "0.0%", halign = "center"),
      gridExpand = TRUE, stack = TRUE
    )


    addStyle(
      wb, sheet_name,
      rows = rows_all, cols = col_start + 3,
      style = createStyle(numFmt = "0.00", halign = "center"),
      gridExpand = TRUE, stack = TRUE
    )


    addStyle(
      wb, sheet_name,
      rows = rows_all, cols = c(cols_seg, cols_seg_func),
      style = createStyle(halign = "center"),
      gridExpand = TRUE, stack = TRUE
    )


    walk(
      2:3,
      ~conditionalFormatting(
        wb, sheet_name,
        rows = rows_all, cols = col_start + .x,
        type = "colourScale",
        style = color_scale_colors
      )
    )


    writeData(
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
      headerStyle = createStyle(
        textDecoration = "bold", fgFill = "#BFBFBF", halign = "center"
      )
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


    for(i in seq(segs_max)){
      walk(
        rows_all,
        ~{
          cell_seg <- glue('{num2let(cols_seg[i])}{.x}')
          cell_var <- glue('${num2let(col_start)}{.x}')
          writeFormula(
            wb, sheet_name,
            startCol = cols_seg_func[i],
            startRow = .x,
            x = glue('=IF(ISBLANK({cell_seg}), "",
                   IF({cell_seg} = 0, "(1-" & {cell_var} & ")",
                   IF({cell_seg} = "n", "(1-" & {cell_var} & ")",
                   IF({cell_seg} = 1, {cell_var},
                   IF({cell_seg} = "y", {cell_var},
                   "")))))')
          )
        }
      )

      writeFormula(
        wb, sheet_name,
        startCol = cols_seg_func %>% tail(1) + 3,
        startRow = rows_all[i],
        array = TRUE,
        x = glue('=TEXTJOIN(",", TRUE, ${num2let(cols_seg_func[i])}${rows_all %>% head(1)}:${num2let(cols_seg_func[i])}${rows_all %>% tail(1)})')
      )
    }


    setColWidths(wb, sheet_name, cols = cols_seg, widths = 6)

    setColWidths(wb, sheet_name, cols = cols_seg_func, widths = 9)

    groupColumns(wb, sheet_name, cols = cols_seg_func, hidden = TRUE) %>% suppressWarnings()

    setColWidths(wb, sheet_name, cols = c(col_start - 1, cols_seg_func %>% tail(1) + 1), widths = 1)

    freezePane(
      wb, sheet_name,
      firstActiveCol =  cols_seg %>% head(1),
      firstActiveRow = rows_all %>% head(1)
    )

  }



  seg <- seg %>% seg_get_fa_winner(winner = fa_winner)


  seg <- seg %>% seg_organize_input_sheet(range_predictors = range_predictors)


  wb <- oxl_create_workbook()


  append_input_sheet(wb, seg_input_table = seg[["input_sheet"]][["input_table"]], sheet_name = "Inputs", row_start = 6, col_start = 2)


  append_rational_sheet(wb, rational_table = seg[["input_sheet"]][["solution_rational"]], sheet_name = "Rational", row_start = 2, col_start = 2)


  append_prototype_sheet(wb, prototype_table = seg[["input_sheet"]][["prototype_table"]], sheet_name = "Prototype", row_start = 2,  col_start = 2, segs_max = 10)



  if(add_proj_name_to_file){
    file_name <- seg_glue_proj_name_to_file(seg, file_name)
  }


  file_location <- glue("{where}/{file_name}.xlsx")


  saveWorkbook(wb, file_location, overwrite = TRUE)


  seg[["paths"]][["files"]][["input"]] <- file_location


  return(seg)
}
