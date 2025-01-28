#' fa_write
#' @description fa_write
#' @export
fa_write <- function(
    fa_analysis_object, clean_max = .25, file_name = "Factor Analysis",  where = NULL, return_location = TRUE
){

  require(openxlsx)


  if(is.null(where)){
    where <- getwd()
  }


  append_analysis <- function(wb, fa_table, sheet_name, footer = NULL, clean_max = .25, row_start = 4, col_start = 2){

    addWorksheet(wb, sheet_name, gridLines = FALSE)

    col_fa <- fa_table %>% select(starts_with("F", ignore.case = FALSE)) %>% names()

    fa_table[, col_fa][abs(fa_table[, col_fa]) <= clean_max] <- NA

    col_fa <- which(names(fa_table) %in% col_fa) + col_start -1
    col_max <- col_fa %>% head(1) - 1

    names(fa_table) <- fa_table %>% names() %>% gsub("_", " ", .) %>% stringr::str_to_title()

    rows_all <- seq(row_start, row_start + nrow(fa_table))


    writeData(
      wb, sheet_name,
      x = glue("FA {sheet_name}"),
      startRow = row_start - 2,
      startCol = col_start,
      colNames = FALSE,
      borders = "none"
    )


    if(!is.null(footer)){
      writeData(
        wb, sheet_name,
        x = footer,
        startRow = rows_all %>% tail(1) + 1,
        startCol = col_start,
        colNames = FALSE,
        borders = "none"
      )
    }


    addStyle(
      wb, sheet_name,
      style = createStyle(textDecoration = "bold", fontSize = 16),
      rows = row_start - 2,
      cols = col_start,
      stack = T
    )


    writeData(
      wb, sheet_name,
      x = fa_table,
      startRow = row_start,
      startCol = col_start,
      colNames = TRUE,
      borders = "all",
      headerStyle = createStyle(textDecoration = "bold", halign = "center")
    )


    oxl_outer_box(
      wb, sheet_name,
      row_start = row_start, row_end = row_start,
      col_start = col_start, col_end = col_start + ncol(fa_table) - 1,
      borderStyle = "thick"
    )


    oxl_outer_box(
      wb, sheet_name,
      row_start = row_start, row_end = row_start + nrow(fa_table),
      col_start = col_start, col_end = col_start + ncol(fa_table) - 1,
      borderStyle = "thick"
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(textDecoration = "bold", halign = "center", fgFill = "#e0e0e0"),
      cols = seq(col_start, col_fa %>% tail(1)),
      rows = row_start,
      gridExpand = T,
      stack = T
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(halign = "center"),
      rows = rows_all,
      cols = c(col_start, col_start + 1, col_start + 3),
      gridExpand = T,
      stack = T
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(halign = "center", numFmt = "0.00"),
      rows = rows_all,
      cols = c(col_max, col_fa),
      gridExpand = T,
      stack = T
    )


    conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_start, col_fa %>% tail(1)),
      rows = rows_all,
      rule = glue('=ISEVEN(${num2let(col_start + 3)}{row_start})'),
      style = createStyle(bgFill = "#e0e0e0")
    )


    conditionalFormatting(
      wb, sheet_name,
      cols = col_fa,
      rows = rows_all,
      rule = glue('${num2let(col_start + 4)}{row_start} == {num2let(col_fa %>% head(1))}{row_start}'),
      style = createStyle(textDecoration = "bold")
    )


    setColWidths(wb, sheet_name, cols = 1, widths = 2)
    setColWidths(wb, sheet_name, cols = col_start, widths = 10)
    setColWidths(wb, sheet_name, cols = col_start + 1, widths = 15)
    setColWidths(wb, sheet_name, cols = col_start + 2, widths = 70)
    setColWidths(wb, sheet_name, cols = c(col_start + 3, col_max, col_fa), widths = 5)

    freezePane(wb, sheet_name, firstActiveRow = row_start + 1, firstActiveCol = "G")
  }


  append_variance_explained <- function(wb, variance_explained, sheet_name = "variance_explained", row_start = 4, col_start = 2){
    rows_all <- seq(row_start, row_start + nrow(variance_explained))

    names(variance_explained) <- names(variance_explained) %>% gsub("_", " ", .) %>% stringr::str_to_title()

    sheet_name <- "variance_explained"

    addWorksheet(wb, sheet_name, gridLines = FALSE)


    writeData(
      wb, sheet_name,
      x = "Variance Explained",
      startRow = row_start - 2,
      startCol = col_start,
      colNames = FALSE,
      borders = "none"
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(textDecoration = "bold", fontSize = 16),
      rows = row_start - 2,
      cols = col_start,
      stack = T
    )


    writeData(
      wb, sheet_name,
      x = variance_explained,
      startRow = row_start,
      startCol = col_start,
      colNames = TRUE,
      borders = "all",
      headerStyle = createStyle(textDecoration = "bold", halign = "center")
    )


    oxl_outer_box(
      wb, sheet_name,
      row_start = row_start, row_end = row_start,
      col_start = col_start, col_end = col_start + ncol(variance_explained) - 1,
      borderStyle = "thick"
    )


    oxl_outer_box(
      wb, sheet_name,
      row_start = row_start, row_end = row_start + nrow(variance_explained),
      col_start = col_start, col_end = col_start + ncol(variance_explained) - 1,
      borderStyle = "thick"
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(textDecoration = "bold", halign = "center", fgFill = "#e0e0e0"),
      cols = seq(col_start, col_start + 2),
      rows = row_start,
      gridExpand = T,
      stack = T
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(halign = "center"),
      rows = rows_all,
      cols = col_start,
      gridExpand = T,
      stack = T
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(halign = "center", numFmt = "0%"),
      rows = rows_all,
      cols = c(col_start + 1, col_start + 2),
      gridExpand = T,
      stack = T
    )


    setColWidths(wb, sheet_name, cols = seq(col_start, col_start + 2), widths = 15)

    freezePane(wb, sheet_name, firstActiveRow = row_start + 1, firstActiveCol = "C")
  }


  wb <- oxl_create_workbook()


  rotation <- fa_analysis_object[["parameters"]][["rotation"]]


  variance_explained <- fa_analysis_object[["variance_explained"]]


  append_variance_explained(wb, variance_explained = variance_explained)


  iwalk(
    fa_analysis_object[["fa_tables"]],
    ~ append_analysis(
      wb, fa_table = .x,
      sheet_name = .y,
      footer = glue("Rotation: {rotation}"),
      clean_max = clean_max
    )
  )


  file_location <- glue("{where}/{file_name}.xlsx")


  saveWorkbook(wb, file_location, overwrite = TRUE)


  if(return_location){
    return(file_location)
  }

}

