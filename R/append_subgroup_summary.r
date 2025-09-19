#' append_subgroup_summary
#' @description append_subgroup_summary
#' @export
append_subgroup_summary <- function(
    df_subgroup,
    wb = NULL,
    sheet_name = NULL,
    title = "ProjectName (Number)",
    sub_title = "Means Check",
    label_width = "auto",
    write_file = TRUE
){

  work::start()
  require(openxlsx)

  if( is.null(wb) ) wb <- oxl_create_workbook()
  if( is.null(sheet_name) ) sheet_name <- "subgroup_count"



  #############################
  # set up
  #############################

  row_data_start <- 5
  col_data_start <- 2

  col_all <- seq(ncol(df_subgroup)) + col_data_start - 1

  col_var <- col_data_start
  col_count <- col_var + 1


  row_header <- row_data_start
  row_data_all <- seq(nrow(df_subgroup)) + row_data_start
  row_data_start <- row_data_all %>% head(1)
  row_data_end <- row_data_all %>% tail(1)

  row_title <- row_data_start - 4
  row_subtitle <- row_title + 1

  addWorksheet(wb, sheet_name)


  #############################
  # add data
  #############################

  writeData(wb, sheet_name, title, startRow = row_title, startCol = col_var)
  writeData(wb, sheet_name, sub_title, startRow = row_subtitle, startCol = col_var)

  writeData(wb, sheet_name, df_subgroup, startRow = row_header, startCol = col_var)



  #############################
  # format data
  #############################

  addStyle(wb, sheet_name, style = createStyle(halign = "center", textDecoration = "bold"), rows = row_header, cols = col_all, gridExpand = TRUE)

  addStyle(wb, sheet_name, style = createStyle(numFmt = "0", halign = "center"), rows = row_data_all, cols = col_count, gridExpand = TRUE)
  addStyle(wb, sheet_name, style = createStyle(halign = "left"), rows = row_data_all, cols = col_var, gridExpand = TRUE)

  conditionalFormatting(wb, sheet_name, cols = col_count, rows = row_data_all, style = c("#f66a6e","#feea8a","#66bd7d"), type = "colourScale", stack = TRUE)

  addStyle(wb, sheet_name, style = createStyle(fontSize = 16, textDecoration = "bold"), rows = row_title, cols = col_var, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, style = createStyle(fontSize = 14, textDecoration = c("bold", "italic")), rows = row_subtitle, cols = col_var, gridExpand = TRUE, stack = TRUE)

  setColWidths(wb, sheet_name, cols = col_var, widths = label_width)

  oxl_outer_box(
    wb, sheet_name,
    row_start = row_header, row_end = row_header,
    col_start = col_all %>% head(1), col_end = col_all %>% tail(1),
    borderStyle = "thick"
  )

  oxl_outer_box(
    wb, sheet_name,
    row_start = row_data_start, row_end = row_data_end,
    col_start = col_all %>% head(1), col_end = col_all %>% tail(1),
    borderStyle = "thick"
  )


  freezePane(
    wb,
    sheet_name,
    firstActiveRow = row_data_start,
    firstActiveCol = col_var + 1
  )


  addFilter(wb, sheet_name, row = row_header, col_all)


  if(write_file){
    saveWorkbook(wb, glue("{title} - Subgroup Count.xlsx"), overwrite = TRUE)
  }


  return(wb)
}
