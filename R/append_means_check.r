#' append_means_check
#' @description append_means_check
#' @export
append_means_check <- function(
    df_means,
    wb = NULL,
    sheet_name = NULL,
    title = "ProjectName (Number)",
    sub_title = "Means Check",
    variable_width = "auto",
    label_width = "auto",
    write_file = TRUE
){

  work::start()
  require(openxlsx)

  if( is.null(wb) ) wb <- oxl_create_workbook()
  if( is.null(sheet_name) ) sheet_name <- "means_check"


  #############################
  # set up
  #############################

  row_data_start <- 5
  col_data_start <- 2

  col_all <- seq(ncol(df_means)) + col_data_start - 1

  col_var <- col_data_start
  col_label <- col_var + 1
  col_count <- grep("- N", names(df_means)) + col_data_start - 1
  col_mean <- setdiff(col_all, c(col_var, col_label, col_count))

  row_header <- row_data_start
  row_data_all <- seq(nrow(df_means)) + row_data_start
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

  writeData(wb, sheet_name, df_means, startRow = row_header, startCol = col_var)


  #############################
  # format data
  #############################

  addStyle(wb, sheet_name, style = createStyle(numFmt = "0.00", halign = "center"), rows = row_data_all, cols = col_mean, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, style = createStyle(numFmt = "0", halign = "center"), rows = row_data_all, cols = col_count, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, style = createStyle(halign = "center"), rows = row_data_all, cols = col_var, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, style = createStyle(halign = "left"), rows = row_data_all, cols = col_label, gridExpand = TRUE, stack = TRUE)

  setColWidths(wb, sheet_name, cols = col_label, widths = label_width)
  setColWidths(wb, sheet_name, cols = col_var, widths = variable_width)

  addStyle(wb, sheet_name, style = createStyle(fontSize = 16, textDecoration = "bold"), rows = row_title, cols = col_var, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, style = createStyle(fontSize = 14, textDecoration = c("bold", "italic")), rows = row_subtitle, cols = col_var, gridExpand = TRUE, stack = TRUE)


  for(i in col_mean){
    conditionalFormatting(wb, sheet_name, cols = i, rows = row_data_all, style = c("#f66a6e","#feea8a","#66bd7d"), type = "colourScale", stack = TRUE)
  }


  addStyle(
    wb, sheet_name,
    style = createStyle(
      textDecoration = "bold", halign = "center", wrapText = TRUE,
      border = "TopBottom", borderStyle = "thick", borderColour = "black"
    ),
    rows = row_header, cols = col_all, gridExpand = TRUE, stack = TRUE
  )


  addStyle(
    wb, sheet_name,
    style = createStyle(
      border = "left", borderStyle = "thick", borderColour = "black"
    ),
    rows = c(row_header, row_data_all), cols = col_var, gridExpand = TRUE, stack = TRUE
  )


  addStyle(
    wb, sheet_name,
    style = createStyle(
      border = "right", borderStyle = "thick", borderColour = "black"
    ),
    rows = c(row_header, row_data_all), cols = col_all %>% tail(1), gridExpand = TRUE, stack = TRUE
  )


  addStyle(
    wb, sheet_name,
    style = createStyle(
      border = "bottom", borderStyle = "thick", borderColour = "black"
    ),
    rows = row_data_end, cols = col_all, gridExpand = TRUE, stack = TRUE
  )


  addStyle(
    wb, sheet_name,
    style = createStyle(
      border = "right", borderStyle = "thick", borderColour = "black"
    ),
    rows = row_data_all, cols = col_label, gridExpand = TRUE, stack = TRUE
  )


  freezePane(
    wb,
    sheet_name,
    firstActiveRow = row_data_start,
    firstActiveCol = col_label + 1
  )


  addFilter(wb, sheet_name, row = row_header, col_all)


  if(write_file){
    saveWorkbook(wb, glue("{title} - Means Check.xlsx"), overwrite = TRUE)
  }


  return(wb)
}







