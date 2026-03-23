#' @keywords internal
.oxl2_batch_new <- function() {
  # Accumulates openxlsx2 style operations for deferred flush.
  # cell_style/numfmt/font: comma-separated dims (works with openxlsx2).
  # border: stored as individual calls (comma dims breaks outer-box logic).
  ops <- list()
  border_ops <- list()

  list(
    add_cell_style = function(sheet, dims, horizontal = NULL) {
      key <- paste0("cs|", sheet, "|", horizontal %||% "")
      ops[[key]] <<- if (is.null(ops[[key]])) dims else paste0(ops[[key]], ",", dims)
    },

    add_numfmt = function(sheet, dims, numfmt) {
      key <- paste0("nf|", sheet, "|", numfmt)
      ops[[key]] <<- if (is.null(ops[[key]])) dims else paste0(ops[[key]], ",", dims)
    },

    add_font = function(sheet, dims, bold = NULL, size = NULL) {
      key <- paste0("fn|", sheet, "|", bold %||% "", "|", size %||% "")
      ops[[key]] <<- if (is.null(ops[[key]])) dims else paste0(ops[[key]], ",", dims)
    },

    add_border = function(sheet, dims, top_border = NULL, bottom_border = NULL,
                          left_border = NULL, right_border = NULL,
                          inner_hgrid = NULL, inner_vgrid = NULL) {
      border_ops[[length(border_ops) + 1L]] <<- list(
        sheet = sheet, dims = dims,
        top_border = top_border, bottom_border = bottom_border,
        left_border = left_border, right_border = right_border,
        inner_hgrid = inner_hgrid, inner_vgrid = inner_vgrid
      )
    },

    flush = function(wb) {
      ne <- function(x) if (is.na(x) || x == "") NULL else x

      for (key in names(ops)) {
        dims <- ops[[key]]
        parts <- strsplit(key, "\\|", fixed = TRUE)[[1]]
        type <- parts[1]
        sheet <- parts[2]

        if (type == "cs") {
          wb$add_cell_style(sheet = sheet, dims = dims, horizontal = ne(parts[3]))
        } else if (type == "nf") {
          wb$add_numfmt(sheet = sheet, dims = dims, numfmt = parts[3])
        } else if (type == "fn") {
          wb$add_font(sheet = sheet, dims = dims, bold = ne(parts[3]), size = ne(parts[4]))
        }
      }

      for (bd in border_ops) {
        wb$add_border(sheet = bd$sheet, dims = bd$dims,
                      top_border = bd$top_border, bottom_border = bd$bottom_border,
                      left_border = bd$left_border, right_border = bd$right_border,
                      inner_hgrid = bd$inner_hgrid, inner_vgrid = bd$inner_vgrid)
      }

      ops <<- list()
      border_ops <<- list()
    }
  )
}


#' @keywords internal
.seg_add_spec_table3 <- function(
    wb,
    sheet_name,
    row_data_start,
    row_start,
    col_start,
    data_table,
    header,
    seg_count = NULL,
    segment_specific = TRUE,
    version = "traditional",
    do_italic = TRUE,
    do_seg_bw = TRUE,
    hide_pvalue = FALSE,
    dxf_styles = NULL,
    batch = NULL
){

  sheet_name_summary <- "summary"

  row_end <- (row_start + nrow(data_table) - 1)
  rows_all <- seq(row_start, row_end)


  # column / data settings

  col_seg_summary_first_number <- col_start + 5
  col_seg_summary_last_number <- col_start + 5 + seg_count - 1

  if(!segment_specific){

    col_rule <- "H"

    col_label_last <- col_start + 3

    col_seg_first_number <- col_seg_summary_first_number
    col_seg_last_number <- col_seg_summary_last_number

    col_range_number <- col_start + 5 + seg_count + 1
    col_pvalue_number <- col_start + 5 + seg_count + 2

    col_dynamic_number <- col_start + 5 + seg_count + 4
    col_type_number <- col_start + 5 + seg_count + 6

    xdf_label <- data_table %>% dplyr::select(var, label, count, mean) %>% dplyr::mutate(label = label %>% gsub(" || ",  "   ||   ", ., fixed = TRUE))
    xdf_seg <- data_table %>% dplyr::select(-c(var, label, count, mean, range, p_value, type))
    xdf_eval <- data_table %>% dplyr::select(range, p_value)

  }else if(segment_specific){

    col_rule <- "F"

    col_label_last <- col_start + 1

    col_seg_first_number <- col_start + 3
    col_seg_last_number <- col_start + 4

    col_range_number <- col_start + 6
    col_pvalue_number <- col_start + 7
    col_dynamic_number <- col_start + 9
    col_type_number <- col_start + 11


    xdf_label <- data_table %>% dplyr::select(var, label) %>% dplyr::mutate(label = label %>% gsub(" || ",  "   ||   ", ., fixed = TRUE))
    xdf_seg <- data_table %>% dplyr::select(target, others)
    xdf_eval <- data_table %>% dplyr::select(Diff, p_value)


    if(version == "both"){

      Rcol_seg_first_number <- col_start + 11
      Rcol_seg_last_number <- col_start + 12

      Rcol_range_number <- col_start + 14
      Rcol_pvalue_number <- col_start + 15
      Rcol_dynamic_number <- col_start + 17

      col_type_number <- col_start + 19


      Rxdf_seg <- 1 - xdf_seg

      Rxdf_eval <- xdf_eval %>%
        dplyr::mutate(
          Diff = Rxdf_seg[["target"]] - Rxdf_seg[["others"]]
        )

    }else if(version == "both_profile"){
      col_type_number <- col_start + 19
    }

  }


  col_first_letter <- col_start %>% num2let()
  col_second_letter <- (col_start + 1) %>% num2let()
  cell_rule_polar <- glue::glue("${col_rule}$2")
  cell_rule_profile <- glue::glue("${col_rule}$3")
  cell_rule_tolerance <- glue::glue("${col_rule}$4")
  cell_rule_pvalue <- glue::glue("${col_rule}$5")
  cell_rule_diff <- glue::glue("${col_rule}$6")
  cell_rule_type <- glue::glue("${col_rule}$7")
  cell_rule_color <- glue::glue("${col_rule}$8")

  col_seg_first <- col_seg_first_number %>% num2let()
  col_seg_last <- col_seg_last_number %>% num2let()

  col_seg_number_all <- seq(col_seg_first_number, col_seg_last_number)
  col_eval_number_all <- seq(col_range_number, col_pvalue_number)
  col_dynamic_number_all <- col_dynamic_number

  col_seg_summary_first <- col_seg_summary_first_number %>% num2let()
  col_seg_summary_last <- col_seg_summary_last_number %>% num2let()

  col_range <- col_range_number %>% num2let()
  col_pvalue <- col_pvalue_number %>% num2let()
  col_dynamic <- col_dynamic_number %>% num2let()
  col_type <- col_type_number %>% num2let()


  if(version == "both"){

    col_seg_number_all <- c(
      col_seg_number_all,
      seq(Rcol_seg_first_number, Rcol_seg_last_number)
    )

    col_dynamic_number_all <- c(col_dynamic_number_all, Rcol_dynamic_number)

    Rcol_seg_first <- Rcol_seg_first_number %>% num2let()
    Rcol_seg_last <- Rcol_seg_last_number %>% num2let()

    Rcol_range <- Rcol_range_number %>% num2let()
    Rcol_pvalue <- Rcol_pvalue_number %>% num2let()
    Rcol_dynamic <- Rcol_dynamic_number %>% num2let()

  }


  ## header

  wb$add_data(sheet = sheet_name, x = header, start_row = row_start - 1,
              start_col = col_start + 1, col_names = FALSE)
  batch$add_font(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = row_start - 1, cols = col_start + 1), bold = "true")



  ## label block

  wb$add_data(sheet = sheet_name, x = xdf_label, start_row = row_start,
              start_col = col_start, col_names = FALSE)

  # thin inner borders on label
  batch$add_border(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = rows_all, cols = seq(col_start, col_label_last)),
                inner_hgrid = "thin", inner_vgrid = "thin",
                top_border = NULL, bottom_border = NULL, left_border = NULL, right_border = NULL)

  if(segment_specific){
    wb$add_data(sheet = sheet_name,
                x = data.frame(foo = rep(" ", nrow(xdf_label))),
                start_row = row_start, start_col = col_start + 2, col_names = FALSE)
  }

  # outer boxes
  batch$add_border(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = rows_all, cols = col_start),
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium")
  batch$add_border(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = rows_all, cols = seq(col_start + 1, col_label_last)),
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium")


  if(!segment_specific){
    batch$add_numfmt(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_start + 2), numfmt = "0")
    batch$add_cell_style(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_start + 2), horizontal = "center")
    batch$add_numfmt(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_start + 3), numfmt = "0%")
    batch$add_cell_style(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_start + 3), horizontal = "center")
  }

  batch$add_cell_style(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_start), horizontal = "center")



  ## seg block

  wb$add_data(sheet = sheet_name, x = xdf_seg, start_row = row_start,
              start_col = col_seg_first_number, col_names = FALSE)
  batch$add_border(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = rows_all, cols = col_seg_number_all[col_seg_number_all <= col_seg_last_number]),
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium",
                inner_hgrid = "thin", inner_vgrid = "thin")


  if(version == "both"){
    wb$add_data(sheet = sheet_name, x = Rxdf_seg, start_row = row_start,
                start_col = Rcol_seg_first_number, col_names = FALSE)
    batch$add_border(sheet = sheet_name,
                  dims = openxlsx2::wb_dims(rows = rows_all, cols = seq(Rcol_seg_first_number, Rcol_seg_last_number)),
                  top_border = "medium", bottom_border = "medium",
                  left_border = "medium", right_border = "medium",
                  inner_hgrid = "thin", inner_vgrid = "thin")
  }


  batch$add_numfmt(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_seg_number_all), numfmt = "0%")
  batch$add_cell_style(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_seg_number_all), horizontal = "center")



  ## eval block

  wb$add_data(sheet = sheet_name, x = xdf_eval, start_row = row_start,
              start_col = col_range_number, col_names = FALSE)
  batch$add_border(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = rows_all, cols = col_eval_number_all),
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium",
                inner_hgrid = "thin", inner_vgrid = "thin")

  if(hide_pvalue){
    batch$add_border(sheet = sheet_name,
                  dims = openxlsx2::wb_dims(rows = rows_all, cols = col_range_number),
                  top_border = "medium", bottom_border = "medium",
                  left_border = "medium", right_border = "medium")
  }


  if(version == "both"){
    wb$add_data(sheet = sheet_name, x = Rxdf_eval, start_row = row_start,
                start_col = Rcol_range_number, col_names = FALSE)
    batch$add_border(sheet = sheet_name,
                  dims = openxlsx2::wb_dims(rows = rows_all, cols = seq(Rcol_range_number, Rcol_pvalue_number)),
                  top_border = "medium", bottom_border = "medium",
                  left_border = "medium", right_border = "medium",
                  inner_hgrid = "thin", inner_vgrid = "thin")

    if(hide_pvalue){
      batch$add_border(sheet = sheet_name,
                    dims = openxlsx2::wb_dims(rows = rows_all, cols = Rcol_range_number),
                    top_border = "medium", bottom_border = "medium",
                    left_border = "medium", right_border = "medium")
    }

    col_eval_number_all <- c(
      col_eval_number_all,
      seq(Rcol_range_number, Rcol_pvalue_number)
    )
  }


  batch$add_numfmt(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_eval_number_all), numfmt = "0%")
  batch$add_cell_style(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_eval_number_all), horizontal = "center")



  # type

  wb$add_data(sheet = sheet_name, x = data_table %>% dplyr::select(type),
              start_row = row_start, start_col = col_type_number, col_names = FALSE)
  batch$add_border(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = rows_all, cols = col_type_number),
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium")


  if(segment_specific){
    wb$add_formula(
      sheet = sheet_name,
      x = glue::glue("=VLOOKUP(${col_first_letter}{rows_all},
               {sheet_name_summary}!${col_first_letter}:$AL,
               MATCH(${col_type}${row_data_start - 4}, {sheet_name_summary}!${col_first_letter}${row_data_start - 4}:$AL${row_data_start - 4}, 0),
               FALSE
               )"),
      start_row = row_start,
      start_col = col_type_number,
      dims = NULL
    )
  }


  batch$add_cell_style(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_type_number), horizontal = "center")



  # conditional formatting

  if(segment_specific){

    all_cond_cols <- c(col_start + 1, seq(col_seg_first_number, col_seg_last_number), seq(col_range_number, col_pvalue_number))
    cond_dims <- openxlsx2::wb_dims(rows = rows_all, cols = seq(min(all_cond_cols), max(all_cond_cols)))

    cond_params <- list(
      list(x = 1, y = ">=", z = 1, s = dxf_styles$pos),
      list(x = 0, y = ">=", z = 1, s = dxf_styles$seg_pos),
      list(x = 1, y = "<=", z = -1, s = dxf_styles$neg),
      list(x = 0, y = "<=", z = -1, s = dxf_styles$seg_neg)
    )

    for (cp in cond_params) {
      wb$add_conditional_formatting(
        sheet = sheet_name,
        dims = cond_dims,
        rule = glue::glue('OR(
      AND(
      {cell_rule_color} = {cp$x}, {cell_rule_type} = 1, ${col_type}{row_start} = "polar", ${col_range}{row_start} {cp$y} {cell_rule_polar} * {cp$z}
      ),
      AND(
      {cell_rule_color} = {cp$x}, {cell_rule_type} = 1, ${col_type}{row_start} = "profile", ${col_range}{row_start} {cp$y} {cell_rule_profile} * {cp$z}
      ),
      AND(
      {cell_rule_color} = {cp$x}, {cell_rule_type} = 2, ${col_pvalue}{row_start} <= {cell_rule_pvalue}, ${col_range}{row_start} {left(cp$y)} 0
      )
                  )'),
        style = cp$s
      )
    }


    if(version == "both"){

      all_cond_cols_r <- c(seq(Rcol_seg_first_number, Rcol_seg_last_number), seq(Rcol_range_number, Rcol_pvalue_number))
      cond_dims_r <- openxlsx2::wb_dims(rows = rows_all, cols = seq(min(all_cond_cols_r), max(all_cond_cols_r)))

      cond_params_r <- list(
        list(x = 1, y = ">=", z = 1, s = dxf_styles$neg),
        list(x = 0, y = ">=", z = 1, s = dxf_styles$seg_neg),
        list(x = 1, y = "<=", z = -1, s = dxf_styles$pos),
        list(x = 0, y = "<=", z = -1, s = dxf_styles$seg_pos)
      )

      for (cp in cond_params_r) {
        wb$add_conditional_formatting(
          sheet = sheet_name,
          dims = cond_dims_r,
          rule = glue::glue('OR(
      AND(
      {cell_rule_color} = {cp$x}, {cell_rule_type} = 1, ${col_type}{row_start} = "polar", ${col_range}{row_start} {cp$y} {cell_rule_polar} * {cp$z}
      ),
      AND(
      {cell_rule_color} = {cp$x}, {cell_rule_type} = 1, ${col_type}{row_start} = "profile", ${col_range}{row_start} {cp$y} {cell_rule_profile} * {cp$z}
      ),
      AND(
      {cell_rule_color} = {cp$x}, {cell_rule_type} = 2, ${col_pvalue}{row_start} <= {cell_rule_pvalue}, ${col_range}{row_start} {left(cp$y)} 0
      )
                  )'),
          style = cp$s
        )
      }
    }


    wb$add_formula(
      sheet = sheet_name,
      start_col = col_dynamic_number,
      start_row = row_start,
      dims = NULL,
      x = glue::glue('IFERROR(
                IF(
                ${col_seg_first}{rows_all} >= MAX(INDEX(summary!${col_seg_summary_first}${row_start}:${col_seg_summary_last}${row_end},MATCH(${col_second_letter}{rows_all},summary!${col_second_letter}${row_start}:${col_second_letter}${row_end},0),)) - {cell_rule_tolerance},
                "High",
                IF(
                ${col_seg_first}{rows_all} <= MIN(INDEX(summary!${col_seg_summary_first}${row_start}:${col_seg_summary_last}${row_end},MATCH(${col_second_letter}{rows_all},summary!${col_second_letter}${row_start}:${col_second_letter}${row_end},0),)) + {cell_rule_tolerance},
                "Low","")
                ),
                IF(
                1 - ${col_seg_first}{rows_all} <= MIN(INDEX(summary!${col_seg_summary_first}${row_start}:${col_seg_summary_last}${row_end},MATCH(${col_first_letter}{rows_all},summary!${col_first_letter}${row_start}:${col_first_letter}${row_end},0),)) + {cell_rule_tolerance},
                "High",
                IF(
                1 - ${col_seg_first}{rows_all} >= MAX(INDEX(summary!${col_seg_summary_first}${row_start}:${col_seg_summary_last}${row_end},MATCH(${col_first_letter}{rows_all},summary!${col_first_letter}${row_start}:${col_first_letter}${row_end},0),)) - {cell_rule_tolerance},
                "Low", "")
                )
                )')
    )


    if(version == "both"){
      wb$add_formula(
        sheet = sheet_name,
        start_col = Rcol_dynamic_number,
        start_row = row_start,
        dims = NULL,
        x = glue::glue('
                IF(
                ${col_dynamic}{rows_all} = "High",
                "Low",
                IF(
                ${col_dynamic}{rows_all} = "Low",
                "High", "")
                )
                ')
      )
    }


    batch$add_cell_style(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_dynamic_number_all), horizontal = "center")


  }else if(!segment_specific){

    cond_params_summary <- list(
      list(x = 1, y = ">= max", z = "-", s = dxf_styles$pos),
      list(x = 0, y = ">= max", z = "-", s = dxf_styles$pos_bw),
      list(x = 1, y = "<= min", z = "+", s = dxf_styles$neg),
      list(x = 0, y = "<= min", z = "+", s = dxf_styles$neg_bw)
    )

    seg_dims <- openxlsx2::wb_dims(rows = rows_all, cols = col_seg_number_all[col_seg_number_all <= col_seg_last_number])

    for (cp in cond_params_summary) {
      wb$add_conditional_formatting(
        sheet = sheet_name,
        dims = seg_dims,
        rule = glue::glue('AND(
                      {cell_rule_color} = {cp$x}, {col_seg_first}{row_start} {cp$y}(${col_seg_first}{row_start}:${col_seg_last}{row_start}) {cp$z} {cell_rule_tolerance},
                      OR(
                      AND({cell_rule_type} = 1, ${col_type}{row_start} = "polar", ${col_range}{row_start} >= {cell_rule_polar}),
                      AND({cell_rule_type} = 1, ${col_type}{row_start} = "profile", ${col_range}{row_start} >= {cell_rule_profile}),
                      AND({cell_rule_type} = 2, ${col_pvalue}{row_start} <= {cell_rule_pvalue})
                      )
                      )'),
        style = cp$s
      )
    }


    # add Diff x/o formula
    wb$add_formula(
      sheet = sheet_name,
      start_col = col_dynamic_number,
      start_row = row_start,
      dims = NULL,
      x = glue::glue('IFERROR(
               AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=x", {col_seg_first}{rows_all}:{col_seg_last}{rows_all}),
               0) -
               IFERROR(
               AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=o", {col_seg_first}{rows_all}:{col_seg_last}{rows_all}),
               0)')
    )


    batch$add_numfmt(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_dynamic_number), numfmt = "0%")
    batch$add_cell_style(sheet = sheet_name, dims = openxlsx2::wb_dims(rows = rows_all, cols = col_dynamic_number), horizontal = "center")


    dyn_dims <- openxlsx2::wb_dims(rows = rows_all, cols = col_dynamic_number)

    cond_params_dyn <- list(
      list(x = 1, y = ">=", s = dxf_styles$pos),
      list(x = 0, y = ">=", s = dxf_styles$pos_bw),
      list(x = 1, y = "<=-", s = dxf_styles$neg),
      list(x = 0, y = "<=-", s = dxf_styles$neg_bw)
    )

    for (cp in cond_params_dyn) {
      wb$add_conditional_formatting(
        sheet = sheet_name,
        dims = dyn_dims,
        rule = glue::glue('OR(AND({cell_rule_color} = {cp$x}, {col_dynamic}{row_start} {cp$y} {cell_rule_diff}), )'),
        style = cp$s
      )
    }

    wb$add_conditional_formatting(
      sheet = sheet_name,
      dims = dyn_dims,
      rule = "== 0",
      style = dxf_styles$white_font
    )

  }

}



#' @keywords internal
.seg_append_sheet3 <- function(
    wb,
    shell_tables,
    solution_var,
    add_key = FALSE,
    seg_n = NULL,
    row_data_start = 15,
    col_start = 2,
    row_block_gap = 2,
    label_width = 75,
    hide_pvalue = FALSE,
    truncate = FALSE,
    truncate_polar_threshold = .15,
    truncate_profile_threshold = .1,
    version = "traditional",
    do_italic = TRUE,
    do_seg_bw = TRUE,
    setting_polar_threshold = .2,
    setting_profile_threshold = .15, setting_tolerance = .05,
    setting_pvalue = .1, setting_diff = .1,
    setting_type = c("diff", "pvalue"), setting_color = c("bw", "color"),
    dxf_styles = NULL,
    batch = NULL
){

  setting_type <- match.arg(setting_type)
  setting_color <- match.arg(setting_color)


  summary_sheet_name <-  "summary"
  key_sheet_name <- "key"


  row_start <- row_data_start


  solution_frequency <- shell_tables[["solution_frequency"]]
  seg_names <- solution_frequency %>% names()
  seg_count <- length(seg_names)


  col_first_letter <- num2let(col_start)
  col_second_letter <- num2let(col_start + 1)



  if(is.null(seg_n)){

    segment_specific <- FALSE
    col_pvalue_number <- col_start + 5 + seg_count + 2

  }else if(!is.null(seg_n)){

    segment_specific <- TRUE
    col_pvalue_number <- col_start + 7

    if(version == "both"){
      Rcol_pvalue_number <- col_start + 15
    }

  }


  col_width_75 <- col_start + 1


  if(segment_specific){

    sheet_name <- seg_names[seg_n]

    if(version == "traditional"){
      col_width_1 <- col_start + c(-1, 2, 5, 8, 10, 12)
      col_width_7 <- col_start + c(0, 3, 4, 6, 7, 9, 11)
    }else if(version == "both"){
      col_width_1 <- col_start + c(-1, 2, 5, 8, 13, 16, 18, 20)
      col_width_7 <- col_start + c(0, 3, 4, 6, 7, 9, 10, 11, 12, 14, 15, 17, 19)
    }

    xtable <- shell_tables[["segment_tables"]][[sheet_name]]


  }else if(!segment_specific){

    col_seg_first <- num2let(col_start + 5)
    col_seg_last <- num2let(col_start + 5 + seg_count - 1)

    col_width_1 <- col_start + c(-1, 4, 5 + seg_count, 8 + seg_count, 10 + seg_count, 12 + seg_count)
    col_width_7 <- col_start + c(0, seq(2,3), seq(5, 4 + seg_count), seq(6 + seg_count, 7 + seg_count), 9 + seg_count, 11 + seg_count)


    if(add_key){
      sheet_name <- key_sheet_name
      xtable <- shell_tables[["summary_key"]]
    }else if(!add_key){
      sheet_name <- summary_sheet_name
      xtable <- shell_tables[["summary_table"]]
    }

  }


  wb$add_worksheet(sheet_name)


  if(truncate){
    rows_to_truncate <- c()
  }


  for(i in seq(nrow(xtable))){

    temp_type <- xtable[["type"]][[i]]
    temp_header <- xtable[["block_header"]][[i]]

    temp <- xtable %>%
      dplyr::select(by, type) %>%
      dplyr::slice(i) %>%
      tidyr::unnest(cols = by) %>%
      as.data.frame()


    if(segment_specific){
      for(y in c("Diff", "p_value", "target", "others")){
        class(temp[, y]) <- "percentage"
      }
    }else if(!segment_specific){
      for(y in c("mean", "range", "p_value", str_scrub(seg_names))){
        class(temp[, y]) <- "percentage"
      }
    }


    temp_version <- version
    if(temp_type != "polar" && version == "both") temp_version <- "both_profile"


    .seg_add_spec_table3(
      wb, sheet_name, row_data_start = row_data_start, row_start = row_start, col_start = col_start,
      data_table = temp, header = temp_header, seg_count = seg_count, segment_specific = segment_specific,
      version = temp_version, do_italic = do_italic, do_seg_bw = do_seg_bw, hide_pvalue = hide_pvalue,
      dxf_styles = dxf_styles,
      batch = batch
    )


    if(truncate){

      type <- temp %>% dplyr::select(dplyr::any_of("type")) %>% unlist() %>% unique()

      diff <- temp %>% dplyr::select(dplyr::any_of(c("Diff", "range"))) %>% unlist() %>% abs()


      truncate_threshold <- ifelse(type == "polar", truncate_polar_threshold, truncate_profile_threshold)


      temp_rows_to_truncate <- seq(row_start, row_start + nrow(temp) - 1)[diff < truncate_threshold]


      if(length(temp_rows_to_truncate) == nrow(temp)){
        temp_rows_to_truncate <- c(seq(row_start - 3, row_start - 1), temp_rows_to_truncate)
      }

      rows_to_truncate <- c(rows_to_truncate, temp_rows_to_truncate) %>% unique()
    }


    row_start <- row_start + nrow(temp) + row_block_gap + 1
  }



  if(truncate && length(rows_to_truncate) >= 1){
    wb$set_row_heights(sheet = sheet_name, rows = rows_to_truncate, heights = 0)
  }



  # sheet formatting

  wb$set_sheetview(sheet = sheet_name, show_grid_lines = FALSE)

  # establish spacer/gap columns (openxlsx2 drops empty cols unlike openxlsx)
  # only write within the actual data column range, at a hidden row (row 2 is in grouped/hidden area)
  max_data_col <- if(segment_specific) col_start + 11 else col_start + 5 + seg_count + 6
  if(version == "both" && segment_specific) max_data_col <- col_start + 19
  gap_cols <- col_width_1[col_width_1 >= col_start & col_width_1 <= max_data_col]
  for (sc in gap_cols) {
    wb$add_data(sheet = sheet_name, x = " ", start_row = 2, start_col = sc, col_names = FALSE)
  }

  wb$set_col_widths(sheet = sheet_name, cols = col_width_75, widths = label_width)
  wb$set_col_widths(sheet = sheet_name, cols = col_width_1, widths = 1)
  wb$set_col_widths(sheet = sheet_name, cols = col_width_7, widths = 7)


  # add title

  if(segment_specific){

    col_summary_seg_first <- num2let(col_start + 4 + seg_n)

    wb$add_formula(
      sheet = sheet_name,
      dims = NULL,
      x = glue::glue('=trim({summary_sheet_name}!{col_summary_seg_first}{row_data_start - 4}) & " (" & trim({summary_sheet_name}!{col_second_letter}{row_data_start - 4}) & ")"'),
      start_row = row_data_start - 4,
      start_col = col_start + 1
    )

  }else if(!segment_specific){
    wb$add_data(sheet = sheet_name,
                x = glue::glue("Solution - {solution_var}"),
                col_names = FALSE,
                start_row = row_data_start - 4,
                start_col = col_start + 1)
  }

  batch$add_font(sheet = sheet_name,
              dims = openxlsx2::wb_dims(rows = row_data_start - 4, cols = col_start + 1),
              bold = "true", size = "18")


  # add header / freq

  if(segment_specific){

    cols_header <- seq((col_start + 3), (col_start + 4))
    cols_header_bold <- seq((col_start + 3), (col_start + 2 + 9))

    col_first_box_end <- col_start + 4


    if(version == "both"){
      cols_header_bold <- seq((col_start + 3), (col_start + 2 + 17))
    }


    wb$add_formula(
      sheet = sheet_name,
      dims = NULL,
      x = glue::glue('=trim({summary_sheet_name}!{col_summary_seg_first}{row_data_start - 4})'),
      start_row = row_data_start - 4,
      start_col = col_start + 3
    )

    if(version == "traditional"){
      header_temp <- c("Others", NA, "Diff", "P Value", NA, NA, NA, "Type") %>% matrix(nrow = 1)
    }else if(version == "both"){
      header_temp <- c("Others", NA, "Diff", "P Value", NA, NA, NA, "Seg", "Others", NA, "Diff", "P Value", NA, NA, NA, "Type") %>% matrix(nrow = 1)
    }

    wb$add_data(sheet = sheet_name, x = header_temp, col_names = FALSE,
                start_row = row_data_start - 4, start_col = col_start + 4)


    if(version == "both"){

      wb$add_formula(
        sheet = sheet_name,
        dims = NULL,
        x = glue::glue('=trim({summary_sheet_name}!{col_summary_seg_first}{row_data_start - 4})'),
        start_row = row_data_start - 4,
        start_col = col_start + 11
      )

      wb$merge_cells(sheet = sheet_name,
                     dims = openxlsx2::wb_dims(rows = row_data_start - 5, cols = seq(col_start + 3, col_start + 7)))
      wb$merge_cells(sheet = sheet_name,
                     dims = openxlsx2::wb_dims(rows = row_data_start - 5, cols = seq(col_start + 11, col_start + 15)))

      wb$add_data(sheet = sheet_name, x = "First Statement", col_names = FALSE,
                  start_row = row_data_start - 5, start_col = col_start + 3)
      wb$add_data(sheet = sheet_name, x = "Second Statement", col_names = FALSE,
                  start_row = row_data_start - 5, start_col = col_start + 11)

      batch$add_font(sheet = sheet_name,
                  dims = openxlsx2::wb_dims(rows = row_data_start - 5, cols = c(col_start + 3, col_start + 11)),
                  bold = "true")
      batch$add_cell_style(sheet = sheet_name,
                        dims = openxlsx2::wb_dims(rows = row_data_start - 5, cols = c(col_start + 3, col_start + 11)),
                        horizontal = "center")
      wb$add_fill(sheet = sheet_name,
                  dims = openxlsx2::wb_dims(rows = row_data_start - 5, cols = c(col_start + 3, col_start + 11)),
                  color = openxlsx2::wb_color("e0e0e0"))
    }


    wb$add_data(
      sheet = sheet_name,
      x = matrix(
        c(
          solution_frequency[1 ,seg_n], solution_frequency[1 ,seg_n] / sum(solution_frequency[1 , ]),
          sum(solution_frequency[1 , -seg_n]), sum(solution_frequency[1 , -seg_n]) / sum(solution_frequency[1 , ])
        ), nrow = 2),
      col_names = FALSE,
      start_row = row_data_start - 3,
      start_col = col_start + 3
    )

  }else if(!segment_specific){

    cols_header <- seq((col_start + 2), (col_start + 2 + seg_count + 5))
    cols_header_bold <- seq((col_start + 2), (col_start + 2 + seg_count + 9))

    col_first_box_end <- col_start + 3



    if(!add_key){

      wb$add_data(sheet = sheet_name,
                  x = c("N", "Total", NA, names(solution_frequency), NA, "Range", "P Value", NA, "Diff", NA, "Type") %>% matrix(nrow = 1),
                  col_names = FALSE,
                  start_row = row_data_start - 4,
                  start_col = col_start + 2)

      wb$add_data(sheet = sheet_name, x = solution_frequency, col_names = FALSE,
                  start_row = row_data_start - 3, start_col = col_start + 5)

      wb$add_data(sheet = sheet_name, x = c(sum(solution_frequency[1,]), 1),
                  col_names = FALSE, start_row = row_data_start - 3, start_col = col_start + 3)

    }else if(add_key){

      for(i in cols_header_bold){
        if(!i %in% c(
          col_start + c(4, 4 + seg_count + 1, 4 + seg_count + 4, 4 + seg_count + 6)
        )
        ){

          wb$add_formula(
            sheet = sheet_name,
            dims = NULL,
            x = glue::glue('=trim({summary_sheet_name}!{num2let(i)}{row_data_start - 4})'),
            start_row = row_data_start - 4,
            start_col = i
          )

          if(i %in% seq(col_start + 3, col_start + 5 + seg_count - 1)){
            for(xr in seq(0,1)){
              wb$add_formula(
                sheet = sheet_name,
                dims = NULL,
                x = glue::glue('={summary_sheet_name}!{num2let(i)}{row_data_start - 3 + xr}'),
                start_row = row_data_start - 3 + xr,
                start_col = i
              )
            }
          }
        }
      }
    }


    batch$add_border(sheet = sheet_name,
                  dims = openxlsx2::wb_dims(rows = (row_data_start - 3):(row_data_start - 2),
                                            cols = seq(col_start + 5, col_start + 5 + seg_count - 1)),
                  top_border = "thick", bottom_border = "thick",
                  left_border = "thick", right_border = "thick")

  }


  batch$add_numfmt(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = row_data_start - 3, cols = cols_header),
                numfmt = "0")
  batch$add_cell_style(sheet = sheet_name,
                    dims = openxlsx2::wb_dims(rows = row_data_start - 3, cols = cols_header),
                    horizontal = "center")

  batch$add_numfmt(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = row_data_start - 2, cols = cols_header),
                numfmt = "0%")
  batch$add_cell_style(sheet = sheet_name,
                    dims = openxlsx2::wb_dims(rows = row_data_start - 2, cols = cols_header),
                    horizontal = "center")

  batch$add_font(sheet = sheet_name,
              dims = openxlsx2::wb_dims(rows = row_data_start - 4, cols = cols_header_bold),
              bold = "true")
  batch$add_cell_style(sheet = sheet_name,
                    dims = openxlsx2::wb_dims(rows = row_data_start - 4, cols = cols_header_bold),
                    horizontal = "center")

  batch$add_border(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = (row_data_start - 3):(row_data_start - 2),
                                          cols = seq(col_start + 3, col_first_box_end)),
                top_border = "thick", bottom_border = "thick",
                left_border = "thick", right_border = "thick")


  ## add controls

  if(segment_specific){
    col_controls <- col_start + 3
  }else if(!segment_specific){
    col_controls <- col_start + 5
  }


  if(setting_type == "diff"){
    setting_type <- 1
  }else if(setting_type == "pvalue"){
    setting_type <- 2
  }


  if(setting_color == "bw"){
    setting_color <- 0
  }else if(setting_color == "color"){
    setting_color <- 1
  }


  wb$add_data(
    sheet = sheet_name,
    x = data.frame(
      x = c("Polar", "Profile", "Tolerance", "P Value", "Diff", "Type", "Color"),
      y = c(
        setting_polar_threshold, setting_profile_threshold, setting_tolerance,
        setting_pvalue, setting_diff, setting_type, setting_color
      )
    ),
    col_names = FALSE,
    start_row = 2,
    start_col = col_controls
  )
  batch$add_border(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = 2:8, cols = col_controls:(col_controls + 1)),
                top_border = "medium", bottom_border = "medium",
                left_border = "medium", right_border = "medium")


  if(segment_specific || add_key){

    if(segment_specific) col_temp <- col_start + 4
    if(add_key) col_temp <- col_start + 6

    for(i in seq(2, 8)){

      if(i <= 3 && segment_specific){
        xrule <- glue::glue("={summary_sheet_name}!$H${i} - .05")
      }else if(i == 4 && segment_specific){
        xrule <- glue::glue("={summary_sheet_name}!$H${i} / 10")
      }else if(i >= 5 || add_key){
        xrule <- glue::glue("={summary_sheet_name}!$H${i}")
      }

      wb$add_formula(sheet = sheet_name, dims = NULL,
                     x = xrule, start_row = i, start_col = col_temp)
    }
  }



  ## add dynamic x/o controls

  if(!segment_specific){
    col_dynamic_number <- col_start + 5 + seg_count - 1 + 5

    col_seg_first <- num2let(col_start + 5)
    col_seg_last <- num2let(col_start + 5 + seg_count - 1)

    wb$add_data(sheet = sheet_name, x = "X/O", col_names = FALSE,
                start_row = row_data_start - 5, start_col = col_start + 3)

    batch$add_font(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = row_data_start - 5, cols = col_start + 3),
                bold = "true")
    batch$add_cell_style(sheet = sheet_name,
                      dims = openxlsx2::wb_dims(rows = row_data_start - 5, cols = col_start + 3),
                      horizontal = "center")

    wb$add_fill(sheet = sheet_name,
                dims = openxlsx2::wb_dims(rows = row_data_start - 5,
                                          cols = seq(col_start + 5, col_start + 2 + seg_count + 2)),
                color = openxlsx2::wb_color("e0e0e0"))
    batch$add_cell_style(sheet = sheet_name,
                      dims = openxlsx2::wb_dims(rows = row_data_start - 5,
                                                cols = seq(col_start + 5, col_start + 2 + seg_count + 2)),
                      horizontal = "center")


    for(i in c(row_data_start - 3, row_data_start - 2)){
      wb$add_formula(
        sheet = sheet_name,
        start_col = col_dynamic_number,
        start_row = i,
        dims = NULL,
        x = glue::glue('IFERROR(
                 AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=x", {col_seg_first}{i}:{col_seg_last}{i}), 0
                 ) -
                 IFERROR(
                 AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=o", {col_seg_first}{i}:{col_seg_last}{i}), 0
                 )')
      )

      wb$add_conditional_formatting(
        sheet = sheet_name,
        dims = openxlsx2::wb_dims(rows = i, cols = col_dynamic_number),
        rule = "== 0",
        style = dxf_styles$white_font
      )
    }

    batch$add_numfmt(sheet = sheet_name,
                  dims = openxlsx2::wb_dims(rows = row_data_start - 3, cols = col_dynamic_number),
                  numfmt = "0")
    batch$add_cell_style(sheet = sheet_name,
                      dims = openxlsx2::wb_dims(rows = row_data_start - 3, cols = col_dynamic_number),
                      horizontal = "center")

    batch$add_numfmt(sheet = sheet_name,
                  dims = openxlsx2::wb_dims(rows = row_data_start - 2, cols = col_dynamic_number),
                  numfmt = "0%")
    batch$add_cell_style(sheet = sheet_name,
                      dims = openxlsx2::wb_dims(rows = row_data_start - 2, cols = col_dynamic_number),
                      horizontal = "center")
  }


  # final formatting

  if(segment_specific){

    if(version == "traditional"){
      cols_to_hide <- c(col_start, col_start + 2, col_start + 11)
    }else if(version == "both"){
      cols_to_hide <- c(col_start, col_start + 2, col_start + 19)
    }

  }else if(!segment_specific){
    cols_to_hide <- c(col_start, col_start + 2, col_start + seg_count + 11)
  }


  if(hide_pvalue){
    cols_to_hide <- c(cols_to_hide, col_pvalue_number)
    if(version == "both"){
      cols_to_hide <- c(cols_to_hide, Rcol_pvalue_number)
    }
  }


  wb$freeze_pane(sheet = sheet_name, first_active_row = row_data_start - 1, first_active_col = 4)
  wb$set_col_widths(sheet = sheet_name, cols = cols_to_hide, hidden = TRUE)

  wb$group_rows(sheet = sheet_name, rows = seq(2, row_data_start - 6), collapsed = TRUE)
  wb$set_row_heights(sheet = sheet_name, rows = row_data_start - 6, heights = 0)

}


#' seg_write_shell3
#'
#' @description openxlsx2-based version of [seg_write_shell()]. Uses R6 class
#'   dispatch instead of S4, eliminating the main performance bottleneck.
#'
#' @inheritParams seg_write_shell
#'
#' @return Invisibly returns `NULL`. Writes the solution workbook to disk.
#'
#' @export
seg_write_shell3 <- function(
    seg,
    solution_var,
    key = NULL,
    add_key = FALSE,
    label_width = 75,
    hide_pvalue = FALSE,
    truncate = FALSE,
    truncate_polar_threshold = .15,
    truncate_profile_threshold = .1,
    var_weight = NULL,
    use_weight = TRUE,
    version = c("traditional", "both"),
    do_seg_bw = TRUE,
    do_italic = TRUE,
    switched_polars = FALSE,
    setting_polar_threshold = .2,
    setting_profile_threshold = .15,
    setting_tolerance = .05,
    setting_pvalue = .1,
    setting_diff = .1,
    setting_type = c("diff", "pvalue"),
    setting_color = c("bw", "color"),
    where = NULL,
    verbose = FALSE
){


  work::start()

  version <- match.arg(version)
  setting_type <- match.arg(setting_type)
  setting_color <- match.arg(setting_color)

  if(is.null(where) || is.na(where)){
    where <- seg[["paths"]][["folders"]][["solution"]]
  }

  if(is.null(where) || is.na(where)){
    where <- getwd()
  }


  df_solutions <- seg[["data"]][["with_solutions"]]


  if(all(is.na(df_solutions)) || is.null(df_solutions)){
    df <- seg[["data"]][["with_shell"]]
  }else{
    df <- df_solutions
  }

  # drop respondents with no segment assignment (filtered during clustering)
  df <- df %>% dplyr::filter(!is.na(.data[[solution_var]]))


  if( !is.null(seg[["meta"]][["weight_variable"]]) && use_weight && is.null(var_weight)){
    var_weight <- seg[["meta"]][["weight_variable"]]
  }



  #########################
  # do analytics
  #########################

  shell_tables <- .seg_do_shell_tables(
    seg = seg, solution_var = solution_var,
    df = df, key = key, var_weight = var_weight
  )


  if(switched_polars){

    spec_polars <- seg[["spec"]][["polars"]] %>% tidyr::unnest(vars)

    shell_tables[["segment_tables"]] <- shell_tables[["segment_tables"]] %>%
      purrr::map(
        function(x){
          temp <- x %>%
            dplyr::filter(type == "polar") %>%
            tidyr::unnest(by) %>%
            dplyr::left_join(
              spec_polars %>% dplyr::select(var, opposite_label),
              by = dplyr::join_by(var)
            ) %>%
            dplyr::mutate(
              opposite_label = glue::glue("{var} - {opposite_label}"),
              label = ifelse(Diff < 0, opposite_label, label),
              target = ifelse(Diff < 0, 1 - target, target),
              others = ifelse(Diff < 0, 1 - others, others),
              Diff = ifelse(Diff < 0, Diff * -1, Diff)
            ) %>%
            dplyr::select(-opposite_label) %>%
            tidyr::nest(by = -c(block_header, type))

          temp[["by"]] <- temp[["by"]] %>% purrr::map(~{.x %>% dplyr::arrange(-Diff)})

          dplyr::bind_rows(
            temp,
            x %>% dplyr::filter(type == "profile")
          )
        })

  }



  #########################
  # write workbook
  #########################


  wb <- openxlsx2::wb_workbook()

  # pre-create and REGISTER DXF styles for conditional formatting
  # openxlsx2 requires styles registered via styles_mgr for correct dxfId assignment
  s_pos_bw_xml <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_color("FFFFFF"), bg_fill = openxlsx2::wb_color("000000"))
  s_neg_bw_xml <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_color("000000"), bg_fill = openxlsx2::wb_color("e0e0e0"))

  if(do_seg_bw){
    s_seg_pos_xml <- s_pos_bw_xml
    s_seg_neg_xml <- s_neg_bw_xml
  }else{
    s_seg_pos_xml <- openxlsx2::create_dxfs_style(font_bold = "true", bg_fill = openxlsx2::wb_color("e0e0e0"))
    s_seg_neg_xml <- openxlsx2::create_dxfs_style(font_bold = "true", font_italic = "true", bg_fill = openxlsx2::wb_color("e0e0e0"))
  }

  if(!do_italic) s_seg_neg_xml <- s_seg_pos_xml

  s_pos_xml <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_color("006100"), bg_fill = openxlsx2::wb_color("C6EFCE"))
  s_neg_xml <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_color("9C0006"), bg_fill = openxlsx2::wb_color("FFC7CE"))
  s_white_xml <- openxlsx2::create_dxfs_style(font_color = openxlsx2::wb_color("FFFFFF"))

  wb$styles_mgr$add(s_pos_xml, "dxf_pos")
  wb$styles_mgr$add(s_pos_bw_xml, "dxf_pos_bw")
  wb$styles_mgr$add(s_neg_xml, "dxf_neg")
  wb$styles_mgr$add(s_neg_bw_xml, "dxf_neg_bw")
  wb$styles_mgr$add(s_seg_pos_xml, "dxf_seg_pos")
  wb$styles_mgr$add(s_seg_neg_xml, "dxf_seg_neg")
  wb$styles_mgr$add(s_white_xml, "dxf_white_font")

  # dxf_styles holds registered names (not XML) for CF calls
  dxf_styles <- list(
    pos = "dxf_pos",
    pos_bw = "dxf_pos_bw",
    neg = "dxf_neg",
    neg_bw = "dxf_neg_bw",
    seg_pos = "dxf_seg_pos",
    seg_neg = "dxf_seg_neg",
    white_font = "dxf_white_font"
  )


  batch <- .oxl2_batch_new()

  .seg_append_sheet3(
    wb = wb,
    shell_tables = shell_tables,
    solution_var = solution_var,
    setting_polar_threshold = setting_polar_threshold,
    setting_profile_threshold = setting_profile_threshold,
    setting_tolerance = setting_tolerance,
    setting_pvalue = setting_pvalue,
    setting_diff = setting_diff,
    setting_type = setting_type,
    setting_color = setting_color,
    label_width = label_width,
    hide_pvalue = hide_pvalue,
    dxf_styles = dxf_styles,
    batch = batch
  )



  if(add_key){
    .seg_append_sheet3(
      wb = wb,
      shell_tables = shell_tables,
      solution_var = solution_var,
      add_key = TRUE,
      label_width = label_width,
      hide_pvalue = hide_pvalue,
      dxf_styles = dxf_styles,
      batch = batch
    )

    wb$set_order(2:1)
  }



  purrr::walk(
    shell_tables[["segment_tables"]] %>% length() %>% seq(),
    ~.seg_append_sheet3(
      wb = wb,
      shell_tables = shell_tables,
      solution_var = solution_var,
      seg_n = .x,
      truncate = truncate,
      truncate_polar_threshold = truncate_polar_threshold,
      truncate_profile_threshold = truncate_profile_threshold,
      version = version,
      do_italic = do_italic,
      do_seg_bw = do_seg_bw,
      label_width = label_width,
      hide_pvalue = hide_pvalue,
      dxf_styles = dxf_styles,
      batch = batch
    ) %>%
      suppressWarnings()
  )



  # flush all batched styles at once
  batch$flush(wb)


  if(truncate){
    file_name <- glue::glue("{where}/Solution - {solution_var} (Truncate).xlsx")
  }else{
    file_name <- glue::glue("{where}/Solution - {solution_var}.xlsx")
  }



  wb$save(file_name, overwrite = TRUE)


  if(verbose) message(glue::glue("Written: {solution_var}"))

}
