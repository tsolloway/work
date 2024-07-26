#' seg_write_shell
#' @description seg_write_shell
#' @export
seg_write_shell <- function(seg, solution_var, where = c("solutions", "here"), verbose = FALSE){

  where <- match.arg(where)

  if(where == "solutions"){
    where = seg[["paths"]][["folders"]][["solution"]]
  }else if(where == "here"){
    where = getwd()
  }


  #########################
  # analytic helpers
  #########################

  do_summary_means <- function(
    df, solution_var, shell_all, add_desc = c("range", "diff"),
    remove_total = FALSE, seg_names = NULL, sort_table = FALSE
  ){

    add_desc <- match.arg(add_desc)

    vars <- shell_all[["var"]]


    if(!remove_total){
      total <- df %>%
        select(all_of(vars)) %>%
        psych::describe() %>%
        select(n, mean) %>%
        as.data.frame() %>%
        round(10) %>%
        setNames(c("count", "mean")) %>%
        tibble::rownames_to_column("var")
    }


    table_summary <- df %>%
      select(all_of(solution_var)) %>%
      unlist() %>%
      unique() %>%
      sort() %>%
      map(
        ~ df %>%
          filter(.data[[solution_var]] == .x) %>%
          select(all_of(vars)) %>%
          psych::describe() %>%
          select(mean) %>%
          round(10) %>%
          as_tibble()
      ) %>%
      bind_cols() %>%
      suppressMessages() %>%
      setNames(., glue("seg_{seq(length(.))}"))


    if(!is.null(seg_names)){
      names(table_summary) <- seg_names
    }


    if(add_desc == "range"){
      table_summary[, "range"] <- table_summary %>% apply(1, function(x){max(x, na.rm=T)-min(x, na.rm=T)})
    }else if(add_desc == "diff"){
      table_summary <- table_summary %>%
        mutate(
          Diff = target - others
        ) %>%
        suppressMessages()
    }


    table_summary[, "p_value"] <- sapply(
      vars,
      function(x){
        possibly(
          ~ chisq.test(table(df[[x]], df[[solution_var]]))
          , otherwise = NA)() %>%
          pluck("p.value") %>%
          suppressWarnings() %>%
          round(10)
      }) %>% sapply(function(x){
        ifelse(is.null(x) || is.nan(x), NA, x)
      }) %>%
      sapply(function(x){
        ifelse(x == 0, 0.0000000001, x)
      })


    if(!remove_total){
      table_summary <- bind_cols(total, table_summary)
    }else if(remove_total){
      table_summary <- bind_cols(var = vars, table_summary)
    }


    table_summary <- left_join(
      shell_all,
      table_summary,
      by = join_by(var)
    ) %>%
      select(-var) %>%
      tidyr::nest(by=-c(block_header, type))


    if(sort_table){
      if(add_desc == "range"){
        table_summary[["by"]] <- table_summary[["by"]] %>% map(~{.x %>% arrange(-range)})
      }else if(add_desc == "diff"){
        table_summary[["by"]] <- table_summary[["by"]] %>% map(~{.x %>% arrange(-Diff)})
      }
    }


    return(table_summary)
  }


  do_all_segment_means <- function(solution_var, df, shell_all){

    seg_max <- df %>% select(all_of(solution_var)) %>% unlist() %>% max()

    seq(seg_max) %>%
      map(~{

        temp <- df %>%
          mutate(
            temp_solution = .data[[solution_var]] %>% case_match(.x ~ 1, NA ~ NA, .default = 2)
          )

        do_summary_means(
          df = temp, solution_var = "temp_solution", shell_all = shell_all,
          add_desc = "diff", remove_total = TRUE,
          seg_names = c("target", "others"), sort_table = TRUE
        )
      })
  }


  do_shell_tables <- function(seg, solution_var){

    df <- seg[["data"]][["with_shell"]]

    shell_polars <- seg[["shell"]][["polars"]] %>%
      tidyr::unnest(cols = vars)

    shell_profiles <- seg[["shell"]][["profiles"]] %>%
      tidyr::unnest(cols = vars)

    shell_all <- bind_rows(shell_polars, shell_profiles) %>%
      mutate(
        label = glue("{var} - {label}") %>% stringr::str_squish(),
        block_header = glue("{prefix} - {block_label}"),
        type = ifelse(prefix %in% seg[["spec"]][["polars"]][["prefix"]], "polar", "profile")
      ) %>%
      select(block_header, type, var, label)


    summary_table <- df %>% do_summary_means(solution_var = solution_var, shell_all = shell_all)


    segment_tables <- do_all_segment_means(solution_var = solution_var, df = df, shell_all = shell_all)


    solution_frequency <- df %>%
      select(all_of(solution_var)) %>%
      table() %>%
      as.numeric() %>%
      tibble(
        n = .,
        r = ./nrow(df)
      ) %>%
      t() %>%
      data.frame()


    names(solution_frequency) <- glue("Seg {seq(ncol(solution_frequency))}")

    names(segment_tables) <- names(solution_frequency)

    segment_tables
    return(
      list(
        "summary_table" = summary_table,
        "segment_tables" = segment_tables,
        "solution_frequency" = solution_frequency
      )
    )


  }


  #########################
  # do analytics
  #########################


  shell_tables <- seg %>% do_shell_tables(solution_var = solution_var)



  #########################
  # formatted helpers
  #########################


  require(openxlsx)

  add_spec_table <- function(
    wb, sheet_name, row_data_start, row_start, col_start, data_table, header,
    seg_count = NULL, segment_specific = TRUE
  ){

    require(openxlsx)


    if(!segment_specific){
      sheet_name <- "summary"
    }else if(segment_specific){
      sheet_name_summary <- "summary"
    }


    style_percent <- createStyle(halign = "center", numFmt = "0%")
    style_number <- createStyle(halign = "center", numFmt = "0")

    pos_style <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
    pos_style_bw <- createStyle(fontColour = "white", bgFill = "black")

    neg_style <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
    neg_style_bw <- createStyle(fontColour = "black", bgFill = "#e0e0e0")

    seg_pos_style_bw <- createStyle(textDecoration = "bold",  bgFill = "#e0e0e0")
    seg_neg_style_bw <- createStyle(textDecoration = c("bold", "italic"),  bgFill = "#e0e0e0")

    row_end <- (row_start + nrow(data_table) - 1)
    rows_all <- seq(row_start, row_end)


    # column / data settings

    col_seg_summary_first_number <- col_start + 4
    col_seg_summary_last_number <- col_start + 4 + seg_count - 1

    if(!segment_specific){

      col_rule <- "G"

      col_label_last <- col_start + 2

      col_seg_first_number <- col_seg_summary_first_number
      col_seg_last_number <- col_seg_summary_last_number

      col_range_number <- col_start + 4 + seg_count + 1
      col_pvalue_number <- col_start + 4 + seg_count + 2

      col_dynamic_number <- col_start + 4 + seg_count + 4
      col_type_number <- col_start + 4 + seg_count + 6

      xdf_label <- data_table %>% select(label, count, mean)
      xdf_seg <- data_table %>% select(-c(label, count, mean, range, p_value, type))
      xdf_eval <- data_table %>% select(range, p_value)

    }else if(segment_specific){

      col_rule <- "E"

      col_label_last <- col_start

      col_seg_first_number <- col_start + 2
      col_seg_last_number <- col_start + 3

      col_range_number <- col_start + 5
      col_pvalue_number <- col_start + 6
      col_dynamic_number <- col_start + 8
      col_type_number <- col_start + 10


      xdf_label <- data_table %>% select(label)
      xdf_seg <- data_table %>% select(target, others)
      xdf_eval <- data_table %>% select(Diff, p_value)
    }


    col_first_letter <- col_start %>% num2let()
    cell_rule_polar <- glue("${col_rule}$2")
    cell_rule_profile <- glue("${col_rule}$3")
    cell_rule_tolerance <- glue("${col_rule}$4")
    cell_rule_pvalue <- glue("${col_rule}$5")
    cell_rule_diff <- glue("${col_rule}$6")
    cell_rule_type <- glue("${col_rule}$7")
    cell_rule_color <- glue("${col_rule}$8")

    col_seg_first <- col_seg_first_number %>% num2let()
    col_seg_last <- col_seg_last_number %>% num2let()
    col_seg_number_all <- seq(col_seg_first_number, col_seg_last_number)

    col_seg_summary_first <- col_seg_summary_first_number %>% num2let()
    col_seg_summary_last <- col_seg_summary_last_number %>% num2let()

    col_range <- col_range_number %>% num2let()
    col_pvalue <- col_pvalue_number %>% num2let()
    col_dynamic <- col_dynamic_number %>% num2let()
    col_type <- col_type_number %>% num2let()



    ## header

    writeData(
      wb, sheet_name,
      x = header,
      startRow = row_start - 1,
      startCol = col_start,
      colNames = FALSE
    )

    addStyle(
      wb, sheet_name,
      style = createStyle(textDecoration = "Bold"),
      rows = row_start - 1,
      cols = col_start,
      gridExpand = T
    )



    ## label block

    writeData(
      wb, sheet_name,
      x = xdf_label,
      startRow = row_start,
      startCol = col_start,
      colNames = FALSE,
      borders = "all"
    )

    if(segment_specific){
      writeData(
        wb, sheet_name,
        x = xdf_label %>% mutate(foo = " ") %>% select(foo),
        startRow = row_start,
        startCol = col_start + 1,
        colNames = FALSE
      )
    }


    oxl_outer_box(
      wb, sheet_name,
      row_start = row_start, row_end = row_end,
      col_start = col_start, col_end = col_label_last,
      borderStyle = "thick"
    )


    if(!segment_specific){
      addStyle(
        wb, sheet_name, style = style_number,
        rows = rows_all,
        cols = col_start + 1,
        gridExpand = TRUE, stack = TRUE
      )

      addStyle(
        wb, sheet_name, style = style_percent,
        rows = rows_all,
        cols = col_start + 2,
        gridExpand = TRUE, stack = TRUE
      )
    }



    ## seg block

    writeData(
      wb, sheet_name,
      x = xdf_seg,
      startRow = row_start,
      startCol = col_seg_first_number,
      colNames = FALSE,
      borders = "all"
    )

    oxl_outer_box(
      wb, sheet_name,
      row_start = row_start, row_end = row_end,
      col_start = col_seg_first_number, col_end = col_seg_last_number,
      borderStyle = "thick"
    )

    addStyle(
      wb, sheet_name, style = style_percent,
      rows = rows_all,
      cols = col_seg_number_all,
      gridExpand = TRUE, stack = TRUE
    )



    ## last block

    writeData(
      wb, sheet_name,
      x = xdf_eval,
      startRow = row_start,
      startCol = col_range_number,
      colNames = FALSE,
      borders = "all"
    )

    oxl_outer_box(
      wb, sheet_name,
      row_start = row_start, row_end = row_end,
      col_start = col_range_number,
      col_end = col_pvalue_number,
      borderStyle = "thick"
    )

    addStyle(
      wb, sheet_name, style = style_percent,
      rows = rows_all,
      cols = seq(col_range_number, col_pvalue_number),
      gridExpand = TRUE, stack = TRUE
    )


    # type

    writeData(
      wb, sheet_name,
      x = data_table %>% select(type),
      startRow = row_start,
      startCol = col_type_number,
      colNames = FALSE,
      borders = "surrounding",
      borderStyle = "thick"
    )


    if(segment_specific){
      for(i in rows_all){
        writeFormula(
          wb, sheet_name,
          x = glue("=VLOOKUP(${col_first_letter}{i},
                   {sheet_name_summary}!${col_first_letter}:$AL,
                   MATCH($L${row_data_start - 4}, {sheet_name_summary}!${col_first_letter}${row_data_start - 4}:$AL${row_data_start - 4}, 0),
                   FALSE
                   )"),
          startRow = i,
          startCol = col_type_number,
        )
      }
    }


    addStyle(
      wb, sheet_name, style = createStyle(halign = "center"),
      rows = rows_all,
      cols = col_type_number,
      gridExpand = TRUE, stack = TRUE
    )



    # conditional formatting

    if(segment_specific){

      temp_func <- function(x,y,z,s, xc){
        conditionalFormatting(
          wb, sheet_name,
          cols = xc,
          rows = rows_all,
          rule = glue('OR(
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

      pwalk(
        list(
          x = c(1, 0, 1, 0),
          y = c(">=", ">=", "<=", "<="),
          z = c(1, 1, -1, -1),
          s = c(pos_style, seg_pos_style_bw, neg_style, seg_neg_style_bw)
        ),
        function(x,y,z,s){
          temp_func(x,y,z,s, xc = 2)
          temp_func(x,y,z,s, xc = 4:5)
          temp_func(x,y,z,s, xc = 7:8)
          temp_func(x,y,z,s, xc = 10)
        }
      )

      for(i in rows_all){
        writeFormula(
          wb, sheet_name,
          startCol = col_dynamic,
          startRow = i,
          x = glue('
          if(
          ${col_seg_first}{i} >= MAX(INDEX(summary!${col_seg_summary_first}${row_start}:${col_seg_summary_last}${row_end},MATCH(${col_first_letter}{i},summary!${col_first_letter}${row_start}:${col_first_letter}${row_end},0),)) - {cell_rule_tolerance},
          "High",
          if(
          ${col_seg_first}{i} <= MIN(INDEX(summary!${col_seg_summary_first}${row_start}:${col_seg_summary_last}${row_end},MATCH(${col_first_letter}{i},summary!${col_first_letter}${row_start}:${col_first_letter}${row_end},0),)) + {cell_rule_tolerance},
          "Low","")
          )')
        )
      }

      addStyle(
        wb, sheet_name, style = createStyle(halign = "center"),
        cols = col_dynamic,
        rows = rows_all,
        gridExpand = TRUE, stack = TRUE
      )


    }else if(!segment_specific){

      pwalk(
        list(
          x = c(1, 0, 1, 0),
          y = c(">= max", ">= max", "<= min", "<= min"),
          z = c("-", "-", "+", "+"),
          s = c(pos_style, pos_style_bw, neg_style, neg_style_bw)
        ),
        function(x,y,z,s){
          conditionalFormatting(
            wb, sheet_name,
            cols = col_seg_number_all,
            rows = rows_all,
            rule = glue('AND(
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

      for(i in rows_all){
        writeFormula(
          wb, sheet_name,
          startCol = col_dynamic,
          startRow = i,
          x = glue('IFERROR(
                   AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=x", {col_seg_first}{i}:{col_seg_last}{i}),
                   0) -
                   IFERROR(
                   AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=o", {col_seg_first}{i}:{col_seg_last}{i}),
                   0)')
        )
      }


      addStyle(
        wb, sheet_name, style = style_percent,
        cols = col_dynamic,
        rows = rows_all,
        gridExpand = TRUE, stack = TRUE
      )

      pwalk(
        list(
          x = c(1, 0, 1, 0),
          y = c(">=", ">=", "<=-", "<=-"),
          z = c(pos_style, pos_style_bw, neg_style, neg_style_bw)
        ),
        function(x, y, z){
          conditionalFormatting(
            wb, sheet_name,
            cols = col_dynamic,
            rows = rows_all,
            rule = glue('OR(AND({cell_rule_color} = {x}, {col_dynamic}{row_start} {y} {cell_rule_diff}), )'),
            style = z
          )
        })

      conditionalFormatting(
        wb, sheet_name,
        cols = col_dynamic,
        rows = rows_all,
        rule = "== 0",
        style = createStyle(fontColour = "white")
      )

    }

  }



  append_sheet <- function(
    wb, shell_tables, seg_n = NULL, row_data_start = 15, col_start = 2, row_block_gap = 2
  ){

    summary_sheet_name <-  "summary"

    row_start <- row_data_start

    solution_frequency <- shell_tables[["solution_frequency"]]
    seg_names <- solution_frequency %>% names()
    seg_count <- length(seg_names)

    col_first_letter <- num2let(col_start)


    if(is.null(seg_n)){
      segment_specific <- FALSE
    }else if(!is.null(seg_n)){
      segment_specific <- TRUE
    }


    col_width_75 <- 2

    if(segment_specific){

      sheet_name <- seg_names[seg_n]

      col_width_1 <- c(1, 3, 6, 9, 11, 13)
      col_width_7 <- c(4, 5, 7, 8, 12)

      xtable <- shell_tables[["segment_tables"]][[sheet_name]]

    }else if(!segment_specific){
      sheet_name <- summary_sheet_name

      col_seg_first <- num2let(col_start + 4)
      col_seg_last <- num2let(col_start + 4 + seg_count - 1)

      col_width_1 <- c(1, 5, 6 + seg_count, 9 + seg_count, 11 + seg_count, 13 + seg_count)
      col_width_7 <- c(seq(3,4), seq(6, 5 + seg_count), seq(7 + seg_count, 8 + seg_count), 10 + seg_count, 12 + seg_count)

      xtable <- shell_tables[["summary_table"]]
    }


    addWorksheet(wb, sheet_name)


    for(i in seq(nrow(xtable))){

      temp_header <- xtable %>%
        slice(i) %>%
        select(block_header) %>%
        unlist()

      temp <- xtable %>%
        select(by, type) %>%
        slice(i) %>%
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

      add_spec_table(
        wb, sheet_name, row_data_start = row_data_start, row_start = row_start, col_start = col_start,
        data_table = temp, header = temp_header, seg_count = seg_count, segment_specific = segment_specific
      )

      row_start <- row_start + nrow(temp) + row_block_gap + 1
    }


    # sheet formatting

    showGridLines(wb, sheet_name, showGridLines = FALSE)

    setColWidths(wb, sheet_name, cols = col_width_75, widths = 75)
    setColWidths(wb, sheet_name, cols = col_width_1, widths = 1)
    setColWidths(wb, sheet_name, cols = col_width_7, widths = 7)


    # add title

    if(segment_specific){

      col_summary_seg_first <- num2let(col_start + 3 + seg_n)

      writeFormula(
        wb, sheet_name,
        x = glue('=trim({summary_sheet_name}!{col_summary_seg_first}{row_data_start - 4}) & " (" & trim({summary_sheet_name}!{col_first_letter}{row_data_start - 4}) & ")"'),
        startRow = row_data_start - 4,
        startCol = col_start
      )
    }else if(!segment_specific){
      writeData(
        wb, sheet_name,
        x = glue("Solution - {solution_var}"),
        colNames = FALSE,
        startRow = row_data_start - 4,
        startCol = col_start
      )
    }

    addStyle(
      wb, sheet_name,
      style = createStyle(textDecoration = "Bold", fontSize = 16),
      rows = row_data_start - 4,
      cols = col_start,
      stack = T
    )


    # add header / freq

    if(segment_specific){

      cols_header <- seq((col_start + 2), (col_start + 3))
      cols_header_bold <- seq((col_start + 1), (col_start + 1 + 9))

      col_first_box_end <- col_start + 3

      writeFormula(
        wb, sheet_name,
        x = glue('=trim({summary_sheet_name}!{col_summary_seg_first}{row_data_start - 4})'),
        startRow = row_data_start - 4,
        startCol = col_start + 2
      )

      writeData(
        wb, sheet_name,
        x = c("Others", "", "Diff", "P Value", "", "", "", "Type") %>% matrix(nrow = 1),
        colNames = FALSE,
        startRow = row_data_start - 4,
        startCol = col_start + 3
      )

      writeData(
        wb, sheet_name,
        x =     matrix(
          c(
            solution_frequency[1 ,seg_n], solution_frequency[1 ,seg_n] / sum(solution_frequency[1 , ]),
            sum(solution_frequency[1 , -seg_n]), sum(solution_frequency[1 , -seg_n]) / sum(solution_frequency[1 , ])
          ), nrow = 2),
        colNames = FALSE,
        startRow = row_data_start - 3,
        startCol = col_start + 2
      )

    }else if(!segment_specific){

      cols_header <- seq((col_start + 1), (col_start + 1 + seg_count + 5))
      cols_header_bold <- seq((col_start + 1), (col_start + 1 + seg_count + 9))

      col_first_box_end <- col_start + 2

      writeData(
        wb, sheet_name,
        x = c("N", "Total", "", names(solution_frequency), "", "Range", "P Value", "", "Diff", "", "Type") %>% matrix(nrow = 1),
        colNames = FALSE,
        startRow = row_data_start - 4,
        startCol = col_start + 1
      )

      writeData(
        wb, sheet_name,
        x = solution_frequency,
        colNames = FALSE,
        startRow = row_data_start - 3,
        startCol = col_start + 4
      )

      writeData(
        wb, sheet_name,
        x = c(sum(solution_frequency[1,]), 1),
        colNames = FALSE,
        startRow = row_data_start - 3,
        startCol = col_start + 2
      )

      oxl_outer_box(
        wb, sheet_name,
        row_start = row_data_start - 3 , row_end = row_data_start - 2,
        col_start = col_start + 4, col_end = col_start + 4 + seg_count - 1
      )
    }


    addStyle(
      wb, sheet_name,
      style = createStyle(numFmt = "0", halign = "center"),
      rows = row_data_start - 3,
      cols = cols_header,
      gridExpand = T, stack = T
    )

    addStyle(
      wb, sheet_name,
      style = createStyle(numFmt = "0%", halign = "center"),
      rows = row_data_start - 2,
      cols = cols_header,
      gridExpand = T, stack = T
    )

    addStyle(
      wb, sheet_name,
      style = createStyle(textDecoration = "Bold", halign = "center"),
      rows = row_data_start - 4,
      cols = cols_header_bold,
      gridExpand = T, stack = T
    )

    oxl_outer_box(
      wb, sheet_name,
      row_start = row_data_start - 3 , row_end = row_data_start - 2,
      col_start = col_start + 2, col_end = col_first_box_end
    )


    ## add controls

    if(segment_specific){
      col_controls <- col_start + 2
    }else if(!segment_specific){
      col_controls <- col_start + 4
    }

    writeData(
      wb, sheet_name,
      x =     data.frame(
        x = c("Polar", "Profile", "Tolerance", "P Value", "Diff", "Type", "Color"),
        y = c(.2, .15, .05, .1, .1, 1, 0)
      ),
      colNames = FALSE,
      startRow = 2,
      startCol = col_controls,
      borders = "surrounding",
      borderStyle = "thick"
    )

    if(segment_specific){
      for(i in seq(2, 8)){

        if(i <= 3){
          xrule <- glue("={summary_sheet_name}!$G${i} - .05")
        }else if(i == 4){
          xrule <- glue("={summary_sheet_name}!$G${i} / 10")
        }else if(i >= 5){
          xrule <- glue("={summary_sheet_name}!$G${i}")
        }

        writeFormula(
          wb, sheet_name,
          x = xrule,
          startRow = i,
          startCol = col_start + 3,
        )
      }
    }



    ## add dynamic x/o contorls

    if(!segment_specific){
      col_dynamic <- col_start + 4 + seg_count - 1 + 5

      writeData(
        wb, sheet_name,
        x = "X/O",
        colNames = FALSE,
        startRow = row_data_start - 5,
        startCol = col_start + 2
      )

      addStyle(
        wb, sheet_name,
        style = createStyle(textDecoration = "Bold", halign = "center"),
        rows = row_data_start - 5,
        cols = col_start + 2,
        gridExpand = T, stack = T
      )

      addStyle(
        wb, sheet_name,
        style = createStyle(fgFill = "#e0e0e0", halign = "center"),
        rows = row_data_start - 5,
        cols = seq((col_start + 4), (col_start + 1 + seg_count + 2)),
        gridExpand = T, stack = T
      )


      for(i in c(row_data_start - 3, row_data_start - 2)){
        writeFormula(
          wb, sheet_name,
          startCol = col_dynamic,
          startRow = i,
          x = glue('IFERROR(
                   AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=x", {col_seg_first}{i}:{col_seg_last}{i}), 0
                   ) -
                   IFERROR(
                   AVERAGEIF(${col_seg_first}${row_data_start - 5}:${col_seg_last}${row_data_start - 5}, "=o", {col_seg_first}{i}:{col_seg_last}{i}), 0
                   )')
        )

        conditionalFormatting(
          wb, sheet_name,
          cols = col_dynamic,
          rows = i,
          rule = "== 0",
          style = createStyle(fontColour = "white")
        )
      }

      addStyle(
        wb, sheet_name,
        style = createStyle(halign = "center", numFmt = "0"),
        cols = col_dynamic,
        rows = row_data_start - 3,
        gridExpand = T, stack = T
      )

      addStyle(
        wb, sheet_name,
        style = createStyle(halign = "center", numFmt = "0%"),
        cols = col_start + 4 + seg_count - 1 + 5,
        rows = row_data_start - 2,
        gridExpand = T, stack = T
      )
    }


    # final formatting

    if(segment_specific){
      cols_to_hide <- c(col_start + 1, col_start + 10)

    }else if(!segment_specific){
      cols_to_hide <- c(col_start + 1, col_start + seg_count + 10)
    }

    freezePane(wb, sheet_name, firstActiveRow = row_data_start - 1, firstActiveCol = "D")
    setColWidths(wb, sheet_name, hidden = TRUE, cols = cols_to_hide)

    groupRows(wb, sheet = sheet_name, rows = seq(2, row_data_start - 6), hidden = TRUE)
    setRowHeights(wb, sheet = sheet_name, rows = row_data_start - 6, heights = 0)

  }


  wb <- createWorkbook()

  append_sheet(wb, shell_tables) %>% suppressWarnings()

  purrr::walk(
    shell_tables[["segment_tables"]] %>% length() %>% seq(),
    ~append_sheet(wb, shell_tables, seg_n = .x) %>% suppressWarnings()
  )

  saveWorkbook(wb, glue("{where}/Solution - {solution_var}.xlsx"), overwrite = TRUE)


  if(verbose) message(glue("Written: {solution_var}"))

}
