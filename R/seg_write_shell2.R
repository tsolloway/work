#' @keywords internal
.style_batch_new <- function() {
  ops <- list()
  box_ops <- list()

  list(
    add = function(sheet, style, rows, cols, stack = TRUE) {
      ops[[length(ops) + 1L]] <<- list(
        sheet = sheet, style = style,
        rows = rows, cols = cols, stack = stack
      )
    },

    add_box = function(sheet, row_start, row_end, col_start, col_end,
                       borderStyle = "thick") {
      box_ops[[length(box_ops) + 1L]] <<- list(
        sheet = sheet,
        row_start = row_start, row_end = row_end,
        col_start = col_start, col_end = col_end,
        borderStyle = borderStyle
      )
    },

    add_grid = function(sheet, row_start, row_end, col_start, col_end,
                        borderStyle = "thin") {
      # Store as a grid op — expanded in flush
      box_ops[[length(box_ops) + 1L]] <<- list(
        sheet = sheet,
        row_start = row_start, row_end = row_end,
        col_start = col_start, col_end = col_end,
        borderStyle = borderStyle,
        grid = TRUE
      )
    },

    flush = function(wb) {

      # --- Phase 1: collect per-cell style ATTRIBUTES as named lists ---
      # Key = "sheet|row|col", value = list(sheet, row, col, attrs)
      # attrs is a named list of raw createStyle args to be merged
      cell_attrs <- new.env(hash = TRUE, parent = emptyenv())

      .add_attrs <- function(sheet, row, col, attrs) {
        k <- paste0(sheet, "|", row, "|", col)
        if (is.null(cell_attrs[[k]])) {
          cell_attrs[[k]] <- list(sheet = sheet, row = row, col = col, attrs = attrs)
        } else {
          # merge: later attrs override, except borders are additive
          existing <- cell_attrs[[k]]$attrs
          for (nm in names(attrs)) {
            existing[[nm]] <- attrs[[nm]]
          }
          cell_attrs[[k]]$attrs <- existing
        }
      }

      # Convert a Style object to a named list of createStyle-compatible args
      .style_to_attrs <- function(s) {
        a <- list()
        if (length(s$fontColour) > 0) a$fontColour <- s$fontColour$rgb
        if (length(s$fontDecoration) > 0 && nzchar(paste(s$fontDecoration, collapse = "")))
          a$fontDecoration <- s$fontDecoration
        if (length(s$fontSize) > 0) a$fontSize <- s$fontSize$val
        if (length(s$halign) > 0 && nzchar(s$halign)) a$halign <- s$halign
        if (length(s$numFmt) > 0) a$numFmt <- s$numFmt
        if (length(s$fill) > 0) a$fill <- s$fill
        # borders stored individually
        if (length(s$borderTop) > 0 && nzchar(s$borderTop)) a$borderTop <- s$borderTop
        if (length(s$borderBottom) > 0 && nzchar(s$borderBottom)) a$borderBottom <- s$borderBottom
        if (length(s$borderLeft) > 0 && nzchar(s$borderLeft)) a$borderLeft <- s$borderLeft
        if (length(s$borderRight) > 0 && nzchar(s$borderRight)) a$borderRight <- s$borderRight
        a
      }

      # --- expand style ops into per-cell attributes ---
      for (op in ops) {
        attrs <- .style_to_attrs(op$style)
        grid <- expand.grid(r = op$rows, c = op$cols, KEEP.OUT.ATTRS = FALSE)
        for (i in seq_len(nrow(grid))) {
          .add_attrs(op$sheet, grid$r[i], grid$c[i], attrs)
        }
      }

      # --- expand border box/grid ops into per-cell border attributes ---
      for (bop in box_ops) {
        sh <- bop$sheet
        bs <- bop$borderStyle
        rs <- bop$row_start; re <- bop$row_end
        cs <- bop$col_start; ce <- bop$col_end
        is_grid <- isTRUE(bop$grid)

        if (is_grid) {
          # Grid: all 4 borders on every cell
          all_borders <- list(borderTop = bs, borderBottom = bs, borderLeft = bs, borderRight = bs)
          for (rr in seq(rs, re)) {
            for (cc in seq(cs, ce)) {
              .add_attrs(sh, rr, cc, all_borders)
            }
          }
        } else {
          # Box: borders only on perimeter
          for (cc in seq(cs, ce)) {
            .add_attrs(sh, rs, cc, list(borderTop = bs))
            .add_attrs(sh, re, cc, list(borderBottom = bs))
          }
          for (rr in seq(rs, re)) {
            .add_attrs(sh, rr, cs, list(borderLeft = bs))
            .add_attrs(sh, rr, ce, list(borderRight = bs))
          }
        }
      }

      # --- Phase 2: convert attr lists to fingerprints, group cells ---
      # Build one createStyle per unique fingerprint, inject directly into wb$styleObjects

      groups <- new.env(hash = TRUE, parent = emptyenv())

      for (k in ls(cell_attrs)) {
        cell <- cell_attrs[[k]]
        # Serialize attrs to a stable fingerprint — handles nested lists (e.g., numFmt, fill)
        fp_parts <- vapply(names(cell$attrs), function(nm) {
          v <- cell$attrs[[nm]]
          if (is.list(v)) {
            paste0(nm, "=", paste(vapply(v, as.character, character(1L)), collapse = ","))
          } else {
            paste0(nm, "=", paste(v, collapse = ","))
          }
        }, character(1L))
        fp <- paste0(cell$sheet, "||", paste(sort(fp_parts), collapse = "|"))

        if (is.null(groups[[fp]])) {
          groups[[fp]] <- list(
            sheet = cell$sheet, attrs = cell$attrs,
            rows = cell$row, cols = cell$col
          )
        } else {
          g <- groups[[fp]]
          g$rows <- c(g$rows, cell$row)
          g$cols <- c(g$cols, cell$col)
          groups[[fp]] <- g
        }
      }

      # --- Phase 3: create Style objects and inject into wb$styleObjects ---

      # Deduplicate numFmt IDs: createStyle always assigns numFmtId=165 for custom
      # formats, causing collisions when different formatCodes exist. Assign unique IDs.
      numfmt_map <- list()  # formatCode -> numFmtId
      next_id <- 165L

      for (fp in ls(groups)) {
        g <- groups[[fp]]
        a <- g$attrs

        # Build createStyle args
        border <- c()
        borderStyle <- c()
        for (side in c("Top", "Bottom", "Left", "Right")) {
          bname <- paste0("border", side)
          if (!is.null(a[[bname]])) {
            border <- c(border, side)
            borderStyle <- c(borderStyle, a[[bname]])
          }
        }

        # Decode fontColour back from "FFRRGGBB" to "#RRGGBB"
        fc <- a$fontColour
        if (!is.null(fc) && nchar(fc) == 8 && startsWith(fc, "FF"))
          fc <- paste0("#", substring(fc, 3))

        # Decode fill
        bg <- NULL
        fg <- NULL
        if (!is.null(a$fill)) {
          if (!is.null(a$fill$fillBg$rgb)) {
            v <- a$fill$fillBg$rgb
            if (nchar(v) == 8 && startsWith(v, "FF")) bg <- paste0("#", substring(v, 3)) else bg <- v
          }
          if (!is.null(a$fill$fillFg$rgb)) {
            v <- a$fill$fillFg$rgb
            if (nchar(v) == 8 && startsWith(v, "FF")) fg <- paste0("#", substring(v, 3)) else fg <- v
          }
        }

        # Decode numFmt
        nf <- NULL
        if (!is.null(a$numFmt)) nf <- a$numFmt$formatCode

        # Decode fontDecoration
        td <- NULL
        if (!is.null(a$fontDecoration)) {
          td <- a$fontDecoration
          td <- gsub("BOLD", "Bold", td)
          td <- gsub("ITALIC", "italic", td)
        }

        args <- list()
        if (!is.null(fc)) args$fontColour <- fc
        if (!is.null(a$fontSize)) args$fontSize <- a$fontSize
        if (!is.null(a$halign)) args$halign <- a$halign
        if (!is.null(nf)) args$numFmt <- nf
        if (!is.null(td)) args$textDecoration <- td
        if (!is.null(fg)) args$fgFill <- fg
        if (!is.null(bg)) args$bgFill <- bg
        if (length(border) > 0) {
          args$border <- border
          args$borderStyle <- borderStyle
        }

        style <- do.call(openxlsx::createStyle, args)

        # Fix numFmtId collision: assign unique ID per distinct formatCode
        if (!is.null(style$numFmt) && length(style$numFmt) > 0) {
          fc_key <- style$numFmt$formatCode
          if (is.null(numfmt_map[[fc_key]])) {
            numfmt_map[[fc_key]] <- next_id
            next_id <- next_id + 1L
          }
          style$numFmt$numFmtId <- numfmt_map[[fc_key]]
        }

        # Direct injection into wb$styleObjects — bypasses addStyle S4 dispatch
        wb$styleObjects[[length(wb$styleObjects) + 1L]] <- list(
          style = style,
          sheet = g$sheet,
          rows = as.integer(g$rows),
          cols = as.integer(g$cols)
        )
      }

      ops <<- list()
      box_ops <<- list()
    }
  )
}


#' @keywords internal
.seg_add_spec_table2 <- function(
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
    styles = NULL,
    batch = NULL
){

  sheet_name_summary <- "summary"

  style_center <- styles$style_center
  style_percent <- styles$style_percent
  style_number <- styles$style_number
  style_bold <- styles$style_bold
  style_white_font <- styles$style_white_font
  pos_style <- styles$pos_style
  pos_style_bw <- styles$pos_style_bw
  neg_style <- styles$neg_style
  neg_style_bw <- styles$neg_style_bw
  seg_pos_style_bw <- styles$seg_pos_style_bw
  seg_neg_style_bw <- styles$seg_neg_style_bw

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

  openxlsx::writeData(
    wb, sheet_name,
    x = header,
    startRow = row_start - 1,
    startCol = col_start + 1,
    colNames = FALSE
  )

  batch$add(sheet_name, style_bold, rows = row_start - 1, cols = col_start + 1)



  ## label block

  openxlsx::writeData(
    wb, sheet_name,
    x = xdf_label,
    startRow = row_start,
    startCol = col_start,
    colNames = FALSE
  )

  batch$add_grid(sheet_name, row_start, row_end, col_start, col_label_last)


  if(segment_specific){

    openxlsx::writeData(
      wb, sheet_name,
      x = xdf_label %>% dplyr::mutate(foo = " ") %>% dplyr::select(foo),
      startRow = row_start,
      startCol = col_start + 2,
      colNames = FALSE
    )
  }


  batch$add_box(sheet_name, row_start, row_end, col_start, col_start, borderStyle = "medium")
  batch$add_box(sheet_name, row_start, row_end, col_start + 1, col_label_last, borderStyle = "medium")


  if(!segment_specific){
    batch$add(sheet_name, style_number, rows = rows_all, cols = col_start + 2)
    batch$add(sheet_name, style_percent, rows = rows_all, cols = col_start + 3)
  }

  batch$add(sheet_name, style_center, rows = rows_all, cols = col_start)



  ## seg block

  openxlsx::writeData(
    wb, sheet_name,
    x = xdf_seg,
    startRow = row_start,
    startCol = col_seg_first_number,
    colNames = FALSE
  )

  batch$add_grid(sheet_name, row_start, row_end, col_seg_first_number, col_seg_last_number)
  batch$add_box(sheet_name, row_start, row_end, col_seg_first_number, col_seg_last_number, borderStyle = "medium")



  if(version == "both"){

    openxlsx::writeData(
      wb, sheet_name,
      x = Rxdf_seg,
      startRow = row_start,
      startCol = Rcol_seg_first_number,
      colNames = FALSE
    )

    batch$add_grid(sheet_name, row_start, row_end, Rcol_seg_first_number, Rcol_seg_last_number)
    batch$add_box(sheet_name, row_start, row_end, Rcol_seg_first_number, Rcol_seg_last_number, borderStyle = "medium")
  }


  batch$add(sheet_name, style_percent, rows = rows_all, cols = col_seg_number_all)



  ## last block

  openxlsx::writeData(
    wb, sheet_name,
    x = xdf_eval,
    startRow = row_start,
    startCol = col_range_number,
    colNames = FALSE
  )

  batch$add_grid(sheet_name, row_start, row_end, col_range_number, col_pvalue_number)
  batch$add_box(sheet_name, row_start, row_end, col_range_number, col_pvalue_number, borderStyle = "medium")

  if(hide_pvalue){
    batch$add_box(sheet_name, row_start, row_end, col_range_number, col_range_number, borderStyle = "medium")
  }


  if(version == "both"){

    openxlsx::writeData(
      wb, sheet_name,
      x = Rxdf_eval,
      startRow = row_start,
      startCol = Rcol_range_number,
      colNames = FALSE
    )

    batch$add_grid(sheet_name, row_start, row_end, Rcol_range_number, Rcol_pvalue_number)
    batch$add_box(sheet_name, row_start, row_end, Rcol_range_number, Rcol_pvalue_number, borderStyle = "medium")

    if(hide_pvalue){
      batch$add_box(sheet_name, row_start, row_end, Rcol_range_number, Rcol_range_number, borderStyle = "medium")
    }

    col_eval_number_all <- c(
      col_eval_number_all,
      seq(Rcol_range_number, Rcol_pvalue_number)
    )

  }


  batch$add(sheet_name, style_percent, rows = rows_all, cols = col_eval_number_all)



  # type

  openxlsx::writeData(
    wb, sheet_name,
    x = data_table %>% dplyr::select(type),
    startRow = row_start,
    startCol = col_type_number,
    colNames = FALSE
  )

  batch$add_box(sheet_name, row_start, row_end, col_type_number, col_type_number, borderStyle = "medium")


  if(segment_specific){
    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue("=VLOOKUP(${col_first_letter}{rows_all},
               {sheet_name_summary}!${col_first_letter}:$AL,
               MATCH(${col_type}${row_data_start - 4}, {sheet_name_summary}!${col_first_letter}${row_data_start - 4}:$AL${row_data_start - 4}, 0),
               FALSE
               )"),
      startRow = row_start,
      startCol = col_type_number
    )
  }


  batch$add(sheet_name, style_center, rows = rows_all, cols = col_type_number)



  # conditional formatting (stays direct — can't batch these)

  if(segment_specific){

    temp_func <- function(x,y,z,s, xc){
      openxlsx::conditionalFormatting(
        wb, sheet_name,
        cols = xc,
        rows = rows_all,
        rule = glue::glue('OR(
      AND(
      {cell_rule_color} = {x}, {cell_rule_type} = 1, ${col_type}{row_start} = "polar", ${col_range}{row_start} {y} {cell_rule_polar} * {z}
      ),
      AND(
      {cell_rule_color} = {x}, {cell_rule_type} = 1, ${col_type}{row_start} = "profile", ${col_range}{row_start} {y} {cell_rule_profile} * {z}
      ),
      AND(
      {cell_rule_color} = {x}, {cell_rule_type} = 2, ${col_pvalue}{row_start} <= {cell_rule_pvalue}, ${col_range}{row_start} {left(y)} 0
      )
                  )'),
        style = s
      )
    }


    all_cond_cols <- c(col_start + 1, seq(col_seg_first_number, col_seg_last_number), seq(col_range_number, col_pvalue_number))

    conditional_coloring_instructions <- list(
      x = c(1, 0, 1, 0),
      y = c(">=", ">=", "<=", "<="),
      z = c(1, 1, -1, -1),
      s = c(pos_style, seg_pos_style_bw, neg_style, seg_neg_style_bw)
    )


    purrr::pwalk(
      conditional_coloring_instructions,
      function(x,y,z,s){
        temp_func(x, y, z, s, xc = all_cond_cols)
      }
    )


    if(version == "both"){

      all_cond_cols_r <- c(seq(Rcol_seg_first_number, Rcol_seg_last_number), seq(Rcol_range_number, Rcol_pvalue_number))

      conditional_coloring_instructions[["s"]] <- c(neg_style, seg_neg_style_bw, pos_style, seg_pos_style_bw)

      purrr::pwalk(
        conditional_coloring_instructions,
        function(x,y,z,s){
          temp_func(x, y, z, s, xc = all_cond_cols_r)
        }
      )
    }


    openxlsx::writeFormula(
      wb, sheet_name,
      startCol = col_dynamic_number,
      startRow = row_start,
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
      openxlsx::writeFormula(
        wb, sheet_name,
        startCol = Rcol_dynamic_number,
        startRow = row_start,
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


    batch$add(sheet_name, style_center, rows = rows_all, cols = col_dynamic_number_all)


  }else if(!segment_specific){

    purrr::pwalk(
      list(
        x = c(1, 0, 1, 0),
        y = c(">= max", ">= max", "<= min", "<= min"),
        z = c("-", "-", "+", "+"),
        s = c(pos_style, pos_style_bw, neg_style, neg_style_bw)
      ),
      function(x,y,z,s){
        openxlsx::conditionalFormatting(
          wb, sheet_name,
          cols = col_seg_number_all,
          rows = rows_all,
          rule = glue::glue('AND(
                      {cell_rule_color} = {x}, {col_seg_first}{row_start} {y}(${col_seg_first}{row_start}:${col_seg_last}{row_start}) {z} {cell_rule_tolerance},
                      OR(
                      AND({cell_rule_type} = 1, ${col_type}{row_start} = "polar", ${col_range}{row_start} >= {cell_rule_polar}),
                      AND({cell_rule_type} = 1, ${col_type}{row_start} = "profile", ${col_range}{row_start} >= {cell_rule_profile}),
                      AND({cell_rule_type} = 2, ${col_pvalue}{row_start} <= {cell_rule_pvalue})
                      )
                      )'),
          style = s
        )
      }
    )


    # add Diff x/o formula
    openxlsx::writeFormula(
      wb, sheet_name,
      startCol = col_dynamic_number,
      startRow = row_start,
      x = glue::glue('IFERROR(
               AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=x", {col_seg_first}{rows_all}:{col_seg_last}{rows_all}),
               0) -
               IFERROR(
               AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=o", {col_seg_first}{rows_all}:{col_seg_last}{rows_all}),
               0)')
    )


    batch$add(sheet_name, style_percent, rows = rows_all, cols = col_dynamic_number)

    purrr::pwalk(
      list(
        x = c(1, 0, 1, 0),
        y = c(">=", ">=", "<=-", "<=-"),
        z = c(pos_style, pos_style_bw, neg_style, neg_style_bw)
      ),
      function(x, y, z){
        openxlsx::conditionalFormatting(
          wb, sheet_name,
          cols = col_dynamic_number,
          rows = rows_all,
          rule = glue::glue('OR(AND({cell_rule_color} = {x}, {col_dynamic}{row_start} {y} {cell_rule_diff}), )'),
          style = z
        )
      })

    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = col_dynamic_number,
      rows = rows_all,
      rule = "== 0",
      style = style_white_font
    )

  }

}



#' @keywords internal
.seg_append_sheet2 <- function(
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


  openxlsx::addWorksheet(wb, sheet_name)


  # pre-create styles once for all blocks
  spec_styles <- {
    s_pos_bw <- openxlsx::createStyle(fontColour = "white", bgFill = "black")
    s_neg_bw <- openxlsx::createStyle(fontColour = "black", bgFill = "#e0e0e0")
    s_seg_pos <- if(do_seg_bw) s_pos_bw else openxlsx::createStyle(textDecoration = "bold", bgFill = "#e0e0e0")
    s_seg_neg <- if(do_seg_bw) s_neg_bw else openxlsx::createStyle(textDecoration = c("bold", "italic"), bgFill = "#e0e0e0")
    if(!do_italic) s_seg_neg <- s_seg_pos
    list(
      style_center = openxlsx::createStyle(halign = "center"),
      style_percent = openxlsx::createStyle(halign = "center", numFmt = "0%"),
      style_number = openxlsx::createStyle(halign = "center", numFmt = "0"),
      style_bold = openxlsx::createStyle(textDecoration = "Bold"),
      style_white_font = openxlsx::createStyle(fontColour = "white"),
      pos_style = openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE"),
      pos_style_bw = s_pos_bw,
      neg_style = openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"),
      neg_style_bw = s_neg_bw,
      seg_pos_style_bw = s_seg_pos,
      seg_neg_style_bw = s_seg_neg
    )
  }


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


    .seg_add_spec_table2(
      wb, sheet_name, row_data_start = row_data_start, row_start = row_start, col_start = col_start,
      data_table = temp, header = temp_header, seg_count = seg_count, segment_specific = segment_specific,
      version = temp_version, do_italic = do_italic, do_seg_bw = do_seg_bw, hide_pvalue = hide_pvalue,
      styles = spec_styles, batch = batch
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
    openxlsx::setRowHeights(wb, sheet = sheet_name, rows = rows_to_truncate, heights = 0)
  }



  # sheet formatting

  openxlsx::showGridLines(wb, sheet_name, showGridLines = FALSE)

  openxlsx::setColWidths(wb, sheet_name, cols = col_width_75, widths = label_width)
  openxlsx::setColWidths(wb, sheet_name, cols = col_width_1, widths = 1)
  openxlsx::setColWidths(wb, sheet_name, cols = col_width_7, widths = 7)


  # add title

  if(segment_specific){

    col_summary_seg_first <- num2let(col_start + 4 + seg_n)

    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue('=trim({summary_sheet_name}!{col_summary_seg_first}{row_data_start - 4}) & " (" & trim({summary_sheet_name}!{col_second_letter}{row_data_start - 4}) & ")"'),
      startRow = row_data_start - 4,
      startCol = col_start + 1
    )

  }else if(!segment_specific){
    openxlsx::writeData(
      wb, sheet_name,
      x = glue::glue("Solution - {solution_var}"),
      colNames = FALSE,
      startRow = row_data_start - 4,
      startCol = col_start + 1
    )
  }

  batch$add(
    sheet_name,
    openxlsx::createStyle(textDecoration = "Bold", fontSize = 18),
    rows = row_data_start - 4,
    cols = col_start + 1
  )


  # add header / freq

  if(segment_specific){

    cols_header <- seq((col_start + 3), (col_start + 4))
    cols_header_bold <- seq((col_start + 3), (col_start + 2 + 9))

    col_first_box_end <- col_start + 4


    if(version == "both"){
      cols_header_bold <- seq((col_start + 3), (col_start + 2 + 17))
    }


    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue('=trim({summary_sheet_name}!{col_summary_seg_first}{row_data_start - 4})'),
      startRow = row_data_start - 4,
      startCol = col_start + 3
    )

    if(version == "traditional"){
      header_temp <- c("Others", "", "Diff", "P Value", "", "", "", "Type") %>% matrix(nrow = 1)
    }else if(version == "both"){
      header_temp <- c("Others", "", "Diff", "P Value", "", "", "", "Seg", "Others", "", "Diff", "P Value", "", "", "", "Type") %>% matrix(nrow = 1)
    }

    openxlsx::writeData(
      wb, sheet_name,
      x = header_temp,
      colNames = FALSE,
      startRow = row_data_start - 4,
      startCol = col_start + 4
    )


    if(version == "both"){

      openxlsx::writeFormula(
        wb, sheet_name,
        x = glue::glue('=trim({summary_sheet_name}!{col_summary_seg_first}{row_data_start - 4})'),
        startRow = row_data_start - 4,
        startCol = col_start + 11
      )


      openxlsx::mergeCells(
        wb, sheet_name,
        cols = seq(col_start + 3, col_start + 7),
        rows = row_data_start - 5
      )


      openxlsx::mergeCells(
        wb, sheet_name,
        cols = seq(col_start + 11, col_start + 15),
        rows = row_data_start - 5
      )


      openxlsx::writeData(
        wb, sheet_name,
        x = "First Statement",
        colNames = FALSE,
        startRow = row_data_start - 5,
        startCol = col_start + 3
      )


      openxlsx::writeData(
        wb, sheet_name,
        x = "Second Statement",
        colNames = FALSE,
        startRow = row_data_start - 5,
        startCol = col_start + 11
      )


      batch$add(
        sheet_name,
        openxlsx::createStyle(textDecoration = "Bold", halign = "center", fgFill = "#e0e0e0"),
        rows = row_data_start - 5,
        cols = c(col_start + 3, col_start + 11)
      )

    }


    openxlsx::writeData(
      wb, sheet_name,
      x = matrix(
        c(
          solution_frequency[1 ,seg_n], solution_frequency[1 ,seg_n] / sum(solution_frequency[1 , ]),
          sum(solution_frequency[1 , -seg_n]), sum(solution_frequency[1 , -seg_n]) / sum(solution_frequency[1 , ])
        ), nrow = 2),
      colNames = FALSE,
      startRow = row_data_start - 3,
      startCol = col_start + 3
    )

  }else if(!segment_specific){

    cols_header <- seq((col_start + 2), (col_start + 2 + seg_count + 5))
    cols_header_bold <- seq((col_start + 2), (col_start + 2 + seg_count + 9))

    col_first_box_end <- col_start + 3



    if(!add_key){

      openxlsx::writeData(
        wb, sheet_name,
        x = c("N", "Total", "", names(solution_frequency), "", "Range", "P Value", "", "Diff", "", "Type") %>% matrix(nrow = 1),
        colNames = FALSE,
        startRow = row_data_start - 4,
        startCol = col_start + 2
      )

      openxlsx::writeData(
        wb, sheet_name,
        x = solution_frequency,
        colNames = FALSE,
        startRow = row_data_start - 3,
        startCol = col_start + 5
      )

      openxlsx::writeData(
        wb, sheet_name,
        x = c(sum(solution_frequency[1,]), 1),
        colNames = FALSE,
        startRow = row_data_start - 3,
        startCol = col_start + 3
      )

    }else if(add_key){

      for(i in cols_header_bold){
        if(!i %in% c(
          col_start + c(4, 4 + seg_count + 1, 4 + seg_count + 4, 4 + seg_count + 6)
        )
        ){

          openxlsx::writeFormula(
            wb, sheet_name,
            x = glue::glue('=trim({summary_sheet_name}!{num2let(i)}{row_data_start - 4})'),
            startRow = row_data_start - 4,
            startCol = i
          )

          if(i %in% seq(col_start + 3, col_start + 5 + seg_count - 1)){
            for(xr in seq(0,1)){
              openxlsx::writeFormula(
                wb, sheet_name,
                x = glue::glue('={summary_sheet_name}!{num2let(i)}{row_data_start - 3 + xr}'),
                startRow = row_data_start - 3 + xr,
                startCol = i
              )
            }
          }
        }
      }
    }


    batch$add_box(sheet_name, row_data_start - 3, row_data_start - 2, col_start + 5, col_start + 5 + seg_count - 1)

  }


  batch$add(
    sheet_name,
    openxlsx::createStyle(numFmt = "0", halign = "center"),
    rows = row_data_start - 3,
    cols = cols_header
  )

  batch$add(
    sheet_name,
    openxlsx::createStyle(numFmt = "0%", halign = "center"),
    rows = row_data_start - 2,
    cols = cols_header
  )

  batch$add(
    sheet_name,
    openxlsx::createStyle(textDecoration = "Bold", halign = "center"),
    rows = row_data_start - 4,
    cols = cols_header_bold
  )

  batch$add_box(sheet_name, row_data_start - 3, row_data_start - 2, col_start + 3, col_first_box_end)


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


  openxlsx::writeData(
    wb, sheet_name,
    x = data.frame(
      x = c("Polar", "Profile", "Tolerance", "P Value", "Diff", "Type", "Color"),
      y = c(
        setting_polar_threshold, setting_profile_threshold, setting_tolerance,
        setting_pvalue, setting_diff, setting_type, setting_color
      )
    ),
    colNames = FALSE,
    startRow = 2,
    startCol = col_controls
  )

  batch$add_box(sheet_name, 2, 8, col_controls, col_controls + 1, borderStyle = "medium")


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

      openxlsx::writeFormula(
        wb, sheet_name,
        x = xrule,
        startRow = i,
        startCol = col_temp,
      )
    }
  }



  ## add dynamic x/o controls

  if(!segment_specific){
    col_dynamic_number <- col_start + 5 + seg_count - 1 + 5

    col_seg_first <- num2let(col_start + 5)
    col_seg_last <- num2let(col_start + 5 + seg_count - 1)

    openxlsx::writeData(
      wb, sheet_name,
      x = "X/O",
      colNames = FALSE,
      startRow = row_data_start - 5,
      startCol = col_start + 3
    )

    batch$add(
      sheet_name,
      openxlsx::createStyle(textDecoration = "Bold", halign = "center"),
      rows = row_data_start - 5,
      cols = col_start + 3
    )

    batch$add(
      sheet_name,
      openxlsx::createStyle(fgFill = "#e0e0e0", halign = "center"),
      rows = row_data_start - 5,
      cols = seq((col_start + 5), (col_start + 2 + seg_count + 2))
    )


    for(i in c(row_data_start - 3, row_data_start - 2)){
      openxlsx::writeFormula(
        wb, sheet_name,
        startCol = col_dynamic_number,
        startRow = i,
        x = glue::glue('IFERROR(
                 AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=x", {col_seg_first}{i}:{col_seg_last}{i}), 0
                 ) -
                 IFERROR(
                 AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=o", {col_seg_first}{i}:{col_seg_last}{i}), 0
                 )')
      )

      openxlsx::conditionalFormatting(
        wb, sheet_name,
        cols = col_dynamic_number,
        rows = i,
        rule = "== 0",
        style = openxlsx::createStyle(fontColour = "white")
      )
    }

    batch$add(
      sheet_name,
      openxlsx::createStyle(halign = "center", numFmt = "0"),
      rows = row_data_start - 3,
      cols = col_dynamic_number
    )

    batch$add(
      sheet_name,
      openxlsx::createStyle(halign = "center", numFmt = "0%"),
      rows = row_data_start - 2,
      cols = col_dynamic_number
    )
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


  openxlsx::freezePane(wb, sheet_name, firstActiveRow = row_data_start - 1, firstActiveCol = "D")
  openxlsx::setColWidths(wb, sheet_name, hidden = TRUE, cols = cols_to_hide)

  openxlsx::groupRows(wb, sheet = sheet_name, rows = seq(2, row_data_start - 6), hidden = TRUE)
  openxlsx::setRowHeights(wb, sheet = sheet_name, rows = row_data_start - 6, heights = 0)

}


#' seg_write_shell2
#'
#' @description Performance-optimized version of [seg_write_shell()]. Batches
#'   `addStyle` calls to reduce S4 dispatch overhead. Produces identical output.
#'
#' @inheritParams seg_write_shell
#'
#' @return Invisibly returns `NULL`. Writes the solution workbook to disk.
#'
#' @export
seg_write_shell2 <- function(
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

  batch <- .style_batch_new()

  wb <- oxl_create_workbook()


  .seg_append_sheet2(
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
    batch = batch
  )



  if(add_key){
    .seg_append_sheet2(
      wb = wb,
      shell_tables = shell_tables,
      solution_var = solution_var,
      add_key = TRUE,
      label_width = label_width,
      hide_pvalue = hide_pvalue,
      batch = batch
    )

    openxlsx::worksheetOrder(wb) <- 2:1
  }



  purrr::walk(
    shell_tables[["segment_tables"]] %>% length() %>% seq(),
    ~.seg_append_sheet2(
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



  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)


  if(verbose) message(glue::glue("Written: {solution_var}"))

}
