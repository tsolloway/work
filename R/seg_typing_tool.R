#' seg_typing_tool
#' @description seg_typing_tool
#' @export
seg_typing_tool <- function(
    seg,
    solution_name,
    survey_respondent_id = "uuid",
    qualification_instructions = rep("qualification goes here", 8),
    overwrite_left_label = NULL,
    overwrite_right_label = NULL,
    segment_names = NULL,
    file_name = "Typing Tool",
    where = NULL,
    start_row = 2,
    start_col = 2,
    polar_label_width = 65,
    doc_label_cell_merge = 6
){

  # survey_respondent_id = "uuid"
  # qualification_instructions = rep("qualification goes here", 8)
  # overwrite_left_label = NULL
  # overwrite_right_label = NULL
  # segment_names = NULL
  # file_name = "Typing Tool"
  # where = NULL
  # start_row = 2
  # start_col = 2
  # polar_label_width = 65
  # doc_label_cell_merge = 6

  work::start(lib_oxl = TRUE)

  row_title <- start_row

  where <- seg[["paths"]][["folders"]][["solution"]]

  df <- seg[["data"]][["with_solutions"]]


  #######################
  # setup styles
  #######################

  style_header <- createStyle(
    textDecoration ="bold",
    halign = "center",
    fgFill = oxl_colorscale_grey(2),
    wrapText = TRUE
  )


  style_header2 <- createStyle(
    textDecoration ="bold",
    halign = "center",
    fgFill = oxl_colorscale_grey(3),
    wrapText = TRUE
  )


  style_title <- createStyle(
    textDecoration ="bold",
    halign = "left",
    fontSize = 16
  )


  style_table <- createStyle(
    fgFill = NULL,
    halign = "center"
  )


  #######################
  # setup objections
  #######################

  inputs <- seg[["solutions"]][["summary_table"]] %>%
    filter(lda_name == !!solution_name) %>%
    select(lda_inputs) %>%
    unlist() %>%
    setNames(NULL)


  polars_table <- seg[["spec"]][["polars_table"]]

  profile_table <- seg[["spec"]][["profiles"]] %>%
    tidyr::unnest(vars)


  if(all(inputs %in% polars_table[["rs_var"]]) && all(!inputs %in% profile_table[["var"]])){
    inputs_are_rs <- TRUE
    inputs_are_profile <- FALSE
  }else if(all(inputs %in% polars_table[["source_var"]]) && all(!inputs %in% profile_table[["var"]])){
    inputs_are_rs <- FALSE
    inputs_are_profile <- FALSE
  }else if(all(inputs %in% profile_table[["source_var"]])){
    inputs_are_rs <- FALSE
    inputs_are_profile <- TRUE
    inputs_are_profile_dichot <- FALSE
  }else if(all(inputs %in% profile_table[["var"]])){
    inputs_are_rs <- FALSE
    inputs_are_profile <- TRUE
    inputs_are_profile_dichot <- TRUE
  }else{
    stop("Script can't find the inputs in the polars, or we mixed raw and rs polar vars.")
  }



  if(inputs_are_rs){

    inputs_table <- polars_table %>% filter(rs_var %in% inputs)

    inputs <- inputs_table %>% select(rs_var) %>% unlist() %>% setNames(NULL)
    inputs_raw <- inputs_table %>% select(source_var) %>% unlist() %>% setNames(NULL)

  }else if(!inputs_are_rs && !inputs_are_profile){

    inputs_table <- polars_table %>% filter(source_var %in% inputs)

    inputs <- inputs_table %>% select(source_var) %>% unlist() %>% setNames(NULL)
    inputs_raw <- inputs

  }else if(inputs_are_profile && !inputs_are_profile_dichot){

    inputs_table <- profile_table %>% filter(source_var %in% inputs)

    inputs <- inputs_table %>% select(source_var) %>% unlist() %>% setNames(NULL)
    inputs_raw <- inputs
  }else if(inputs_are_profile && inputs_are_profile_dichot){

    inputs_table <- profile_table %>% filter(var %in% inputs)

    inputs <- inputs_table %>% select(var) %>% unlist() %>% setNames(NULL)
    inputs_raw <- inputs_table %>% select(source_var) %>% unlist() %>% setNames(NULL)
  }


  if(!inputs_are_profile){
    if(!is.null(overwrite_left_label)) inputs_table[["left_label"]] <- overwrite_left_label
    if(!is.null(overwrite_right_label)) inputs_table[["right_label"]] <- overwrite_right_label
  }else if(inputs_are_profile){
    if(!is.null(overwrite_left_label)) inputs_table[["label"]] <- overwrite_left_label
  }



  coef_func <- seg[["solutions"]][["summary_table"]] %>%
    filter(lda_name == !!solution_name) %>%
    select(lda_coefficient_function) %>%
    flatten_dfc() %>%
    slice(
      match(
        c(inputs, "constant"),
        .[["variable"]]
      )
    ) %>%
    setNames(., stringr::str_to_title(names(.)))


  data_solution_check <- seg[["solutions"]][["summary_table"]] %>%
    filter(lda_name == !!solution_name) %>%
    select(lda_predict) %>%
    flatten_dfc()


  segments <- data_solution_check %>% select(seg) %>% unlist() %>% unique() %>% sort() %>% as.character() %>% as.numeric()


  if(is.null(segment_names)){
    segment_names <- glue("Segment Name {segments}")
  }else{
    segment_names <- segment_names %>% stringr::str_squish()
  }


  data_inputs <- seg[["data"]][["with_solutions"]] %>% select(all_of(c(survey_respondent_id, inputs_raw)))


  polar_points <- data_inputs %>% select(-!!survey_respondent_id) %>% unlist() %>% unique() %>% length() %>% seq()


  if(inputs_are_profile && inputs_are_profile_dichot){
    polar_points <- seq(0,1)
  }


  ind_response <- data_inputs %>%
    select(-!!survey_respondent_id) %>%
    slice(1) %>%
    unlist() %>%
    map(
      ~{
        answer <- (polar_points %in% .x) %>%
          ifelse("x", NA)

        matrix(answer, nrow = 1) %>%
          data.frame()
      }
    ) %>%
    bind_rows() %>%
    setNames(polar_points)


  clean_variable_names <- glue("Q{seq(nrow(inputs_table))}")



  #######################
  # internal functions
  #######################

  ## creating individual ui

  individual_typing_tool <- function(
    wb = wb,
    sheet_name = sheet_name,
    row_title = start_row,
    start_col = start_col,
    polar_label_width = polar_label_width
  ){

    ind_ui_response_instructions <- "Please read each pair of statements and decide which side you agree with more. Indicate your response with an x"
    ind_doc_response_instructions <- "Please read each pair of statements and decide which side you agree with more."

    #######################
    # create objects
    #######################

    recode_values <- table(
      df[, inputs_raw] %>% unlist(),
      df[, inputs] %>% unlist()) %>%
      colnames() %>%
      as.numeric() %>%
      matrix(nrow = 1)


    ind_ui <- tibble(
      "Question" = clean_variable_names,
      "Survey Variable" = inputs_table %>% select(source_var) %>% flatten_chr() %>% stringr::str_squish(),
      "Side A" = inputs_table %>% select(left_label) %>% flatten_chr() %>% stringr::str_squish(),
      matrix(nrow = nrow(inputs_table), ncol = length(polar_points)) %>% data.frame(),
      "Side B" = inputs_table %>% select(right_label) %>% flatten_chr() %>% stringr::str_squish()
    ) %>%
      setNames(c("Side A", rep("", length(polar_points)), "Side B"))


    if(length(polar_points) == 4){
      ind_ui <- ind_ui %>%
        setNames(
          c(
            "Question", "Survey Variable",
            "Side A",
            "Agree Much \nMore \n<<", "Agree Somewhat \nMore \n<<",
            "Agree Somewhat \nMore \n>>", "Agree Much \nMore \n>>",
            "Side B"
          )
        )
    }else if(length(polar_points) == 2){
      ind_ui <- ind_ui %>%
        setNames(
          c(
            "Question", "Survey Variable",
            "Side A",
            "Agree \nMore \n<<",
            "Agree \nMore \n>>",
            "Side B"
          )
        )
    }


    #######################
    # reference constants
    #######################

    row_ind_score <- row_title + 2
    row_instructions <- row_ind_score + 1
    row_header <- row_instructions + 1

    row_ind_recode <- row_instructions + 1
    row_header <- row_ind_recode + 1
    row_ind_first <- row_header + 1
    row_ind_last <- row_ind_first + nrow(coef_func) - 2

    row_ind_engine_header <- row_ind_last + 5
    row_ind_engine_first <- row_ind_engine_header + 1
    row_ind_engine_last <- row_ind_engine_first + nrow(coef_func) - 1
    row_ind_engine_prob <- row_ind_engine_last + 2

    row_eng_coef_diff <- row_ind_engine_first - row_ind_first

    row_ind_engine_seg <- row_ind_engine_prob + 4
    row_ind_engine_seg_name <- row_ind_engine_seg + 2
    row_ind_engine_seg_prob <- row_ind_engine_seg_name + 2
    row_ind_engine_seg_qc <- row_ind_engine_seg_prob + 2

    row_ind_control_feedback_style <- row_ind_engine_seg
    row_ind_control_prob_show <- row_ind_engine_seg_name
    row_ind_control_prob_dec <- row_ind_engine_seg_prob
    row_ind_control_q_feedback <- row_ind_engine_seg_qc

    col_ind_clean_var_number <- start_col
    col_ind_survey_var_number <- col_ind_clean_var_number + 1
    col_ind_label_left_number <- col_ind_survey_var_number + 1
    col_ind_label_point_first_number <- col_ind_label_left_number + 1
    col_ind_label_point_last_number <- col_ind_label_point_first_number + length(polar_points) - 1
    col_ind_label_right_number <- col_ind_label_point_last_number + 1

    col_ind_engine_clean_var_number <- col_ind_label_right_number + 2
    col_ind_engine_survey_var_number <- col_ind_engine_clean_var_number + 1
    col_ind_engine_answer_number <- col_ind_engine_survey_var_number + ncol(coef_func)
    col_ind_engine_recode_number <- col_ind_engine_answer_number + 1
    col_ind_engine_qc_number <- col_ind_engine_recode_number + 1

    col_ind_engine_controls_number <- col_ind_engine_clean_var_number + 4

    range_ind_prob <- glue('${num2let(col_ind_engine_survey_var_number + 1)}${row_ind_engine_prob}:${num2let(col_ind_engine_answer_number - 1)}${row_ind_engine_prob}')


    row_doc_intro <- row_title + 3
    row_doc_qualification <- row_doc_intro + 3
    row_doc_qualification_last <- row_doc_qualification + length(qualification_instructions)

    row_doc_seg_name_header <- row_doc_qualification
    row_doc_seg_name_first <- row_doc_seg_name_header + 1
    row_doc_seg_name_last <- row_doc_seg_name_header + length(segments)


    if(row_doc_qualification_last >= row_doc_seg_name_last){
      row_doc_questions_header <- row_doc_qualification_last + 5
    }else{
      row_doc_questions_header <- row_doc_seg_name_last + 5
    }


    doc_row_gap <- 2

    row_doc_questions_first <- row_doc_questions_header + doc_row_gap + 1
    row_doc_questions_last <- row_doc_questions_first + nrow(coef_func) - 2

    row_doc_function_header <- row_doc_questions_last + 4
    row_doc_function_last <- row_doc_function_header + nrow(coef_func) + 1


    row_doc_steps <- row_doc_function_last + 2


    col_doc_start <- col_ind_engine_qc_number + 2
    col_doc_qualification_last <- col_doc_start + doc_label_cell_merge + 2

    col_doc_seg_name_header <- col_doc_start + ncol(ind_ui) + doc_label_cell_merge - 2
    col_doc_seg_name_value <- col_doc_seg_name_header + 1

    row_diff_ind_ui_doc <- row_doc_questions_header - row_header
    col_diff_ind_ui_doc <- col_doc_start - col_ind_clean_var_number

    row_diff_ind_eng_doc <- row_doc_function_header - row_ind_engine_header
    col_diff_ind_eng_doc <- col_doc_start - col_ind_engine_clean_var_number

    col_last <- col_doc_start + ncol(ind_ui) + (doc_label_cell_merge * 2) - 1



    #######################
    # write indi ui
    #######################

    writeData(
      wb, sheet_name,
      x = "Individual Typing Tool",
      startRow = row_title,
      startCol = col_ind_clean_var_number,
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_title,
      rows = row_title,
      cols = col_ind_clean_var_number,
      stack = TRUE
    )


    writeData(
      wb, sheet_name,
      x = "Response Recode",
      startRow = row_ind_recode,
      startCol = col_ind_label_point_first_number - 1,
      colNames = FALSE
    )


    writeData(
      wb, sheet_name,
      x = recode_values,
      startRow = row_ind_recode,
      startCol = col_ind_label_point_first_number,
      colNames = FALSE
    )


    range_recode <- glue('${num2let(col_ind_label_point_first_number)}${row_ind_recode}:${num2let(col_ind_label_point_first_number + ncol(recode_values) - 1)}${row_ind_recode}')


    addStyle(
      wb, sheet_name,
      style = oxl_style_cell_neurtal(textDecoration = "Bold"),
      rows = row_ind_recode,
      cols = seq(col_ind_label_point_first_number - 1, col_ind_label_point_last_number),
      gridExpand = TRUE
    )


    diff_question <- function(
    place = c("ui", "doc"),
    type = c("row", "col"),
    add_doc_row_gap = FALSE,
    doc_label_cell_merge = 0
    ){
      place <- match.arg(place)
      type <- match.arg(type)

      if(place == "ui" && type == "row") return(0)
      if(place == "ui" && type == "col") return(0)
      if(place == "doc" && type == "row" && !add_doc_row_gap) return(row_diff_ind_ui_doc)
      if(place == "doc" && type == "row" && add_doc_row_gap) return(row_diff_ind_ui_doc + doc_row_gap)
      if(place == "doc" && type == "col") return(col_diff_ind_ui_doc + doc_label_cell_merge)
    }


    for(i in c("ui", "doc")){

      setColWidths(wb, sheet_name, cols = seq(col_ind_clean_var_number, col_ind_label_point_last_number) + diff_question(i, "col"), widths = 10)
      setColWidths(wb, sheet_name, cols = col_ind_survey_var_number + diff_question(i, "col"), hidden = TRUE)
      groupRows(wb, sheet_name, rows = row_ind_recode, hidden = TRUE)


      if(i == "ui"){

        setColWidths(wb, sheet_name, cols = c(col_ind_label_left_number, col_ind_label_right_number) + diff_question(i, "col"), widths = polar_label_width)

        writeData(
          wb, sheet_name,
          x = ind_ui,
          startRow = row_header + diff_question(i, "row"),
          startCol = col_ind_clean_var_number + diff_question(i, "col"),
          colNames = TRUE,
          headerStyle = style_header,
          borders = "all",
        )

        writeData(
          wb, sheet_name,
          x = ind_response,
          startRow = row_ind_first,
          startCol = col_ind_label_point_first_number,
          colNames = FALSE
        )

      }else if(i == "doc"){

        for(y in seq(col_ind_clean_var_number, col_ind_label_right_number + (doc_label_cell_merge * 2)) + diff_question(i, "col")){

          if(
            (
              y %in% seq(
                col_ind_clean_var_number + diff_question(i, "col") + 2,
                col_ind_clean_var_number + diff_question(i, "col") + 2 + doc_label_cell_merge
              )
            ) ||
            (
              y %in% seq(
                col_ind_label_right_number + diff_question(i, "col") + doc_label_cell_merge,
                col_ind_label_right_number + diff_question(i, "col") + (doc_label_cell_merge * 2)
              )
            )
          ){

            if(y %in% c(
              col_ind_clean_var_number + diff_question(i, "col") + 2,
              col_ind_label_right_number + diff_question(i, "col") + doc_label_cell_merge
            )){

              mergeCells(
                wb, sheet_name,
                rows = seq(row_header + diff_question(i, "row"), row_header + diff_question(i, "row") + doc_row_gap),
                cols = seq(y, y + doc_label_cell_merge)
              )
            }

          }else{
            mergeCells(
              wb, sheet_name,
              rows = seq(row_header + diff_question(i, "row"), row_header + diff_question(i, "row") + doc_row_gap),
              cols = y
            )
          }
        }

        for(tc in c(col_ind_label_left_number, col_ind_label_right_number + doc_label_cell_merge) + diff_question(i, "col")){
          for(tr in seq(row_ind_first, row_ind_last) + diff_question(i, "row", TRUE)){
            mergeCells(
              wb, sheet_name,
              rows = tr,
              cols = seq(tc, tc + doc_label_cell_merge)
            )
          }
        }


        temp_doc_ui <- ind_ui %>%
          bind_rows(create_NA_rows(., doc_row_gap), .) %>%
          mutate(" " = NA) %>%
          setNames(., names(.) %>% trimws())

        temp_doc_ui <- temp_doc_ui[
          ,
          c(
            1:3,
            rep(ncol(temp_doc_ui), doc_label_cell_merge),
            4:ncol(temp_doc_ui),
            rep(ncol(temp_doc_ui), doc_label_cell_merge - 1)
          )]

        writeData(
          wb, sheet_name,
          x = temp_doc_ui,
          startRow = row_header + diff_question(i, "row"),
          startCol = col_ind_clean_var_number + diff_question(i, "col"),
          colNames = TRUE,
          headerStyle = style_header,
          borders = "all",
        )


        # write values into questionaire doc instructions

        writeData(
          wb, sheet_name,
          x = map(
            ind_response %>%
              nrow() %>%
              seq(),
            ~ recode_values %>%
              length() %>%
              seq() %>%
              matrix(nrow = 1) %>%
              data.frame()
          ) %>%
            bind_rows(),
          startRow = row_header + diff_question(i, "row") + 3,
          startCol = col_ind_clean_var_number + diff_question(i, "col") + doc_label_cell_merge + 3,
          colNames = FALSE
        )


        addStyle(
          wb, sheet_name,
          style = style_header,
          rows = seq(
            row_header + diff_question(i, "row"),
            row_header + diff_question(i, "row") + doc_row_gap
          ),
          cols = seq(
            col_ind_clean_var_number + diff_question(i, "col"),
            col_ind_label_right_number + diff_question(i, "col", doc_label_cell_merge = doc_label_cell_merge)
          ),
          gridExpand = TRUE, stack = FALSE
        )


        mergeCells(
          wb, sheet_name,
          rows = row_header + diff_question(i, "row") - 2,
          cols = seq(col_doc_start, col_last)
        )


        writeData(
          wb, sheet_name,
          x = "Solution Questions",
          startRow = row_header + diff_question(i, "row") - 2,
          startCol = col_doc_start,
          colNames = FALSE
        )


        addStyle(
          wb, sheet_name,
          style = style_header2,
          rows = row_header + diff_question(i, "row") - 2,
          cols = seq(col_doc_start, col_last),
          gridExpand = TRUE, stack = TRUE
        )


        oxl_outer_box(
          wb, sheet_name,
          borderStyle = "medium",
          row_start = row_header + diff_question(i, "row") - 2,
          row_end = row_header + diff_question(i, "row") - 2,
          col_start = col_doc_start,
          col_end = col_last
        )


      }


      addStyle(
        wb, sheet_name,
        style = createStyle(fontSize = 8),
        rows = row_header + diff_question(i, "row"),
        cols = seq(
          col_ind_label_point_first_number,
          col_ind_label_point_last_number
        ) +
          diff_question(i, "col", doc_label_cell_merge = doc_label_cell_merge),
        gridExpand = TRUE, stack = TRUE
      )


      addStyle(
        wb, sheet_name,
        style = style_table,
        rows = seq(row_ind_first, row_ind_last) + diff_question(i, "row", TRUE),
        cols = seq(
          col_ind_clean_var_number + diff_question(i, "col"),
          col_ind_label_right_number + diff_question(i, "col", doc_label_cell_merge = doc_label_cell_merge * 2)
        ),
        gridExpand = TRUE, stack = TRUE
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = row_header + diff_question(i, "row"),
        row_end = row_header + diff_question(i, "row", TRUE),
        col_start = col_ind_clean_var_number + diff_question(i, "col"),
        col_end = col_ind_label_right_number + diff_question(i, "col", doc_label_cell_merge = doc_label_cell_merge * 2)
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = row_ind_first + diff_question(i, "row", TRUE),
        row_end = row_ind_last + diff_question(i, "row", TRUE),
        col_start = col_ind_clean_var_number + diff_question(i, "col"),
        col_end = col_ind_label_right_number + diff_question(i, "col", doc_label_cell_merge = doc_label_cell_merge * 2)
      )


      if(i == "ui") temp_row_instructions <- row_instructions
      if(i == "doc") temp_row_instructions <- row_instructions + diff_question(i, "row") + 1

      mergeCells(
        wb, sheet_name,
        rows = temp_row_instructions,
        cols = seq(
          col_ind_label_left_number + diff_question(i, "col"),
          col_ind_label_right_number + diff_question(i, "col", doc_label_cell_merge = doc_label_cell_merge * 2)
        )
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = temp_row_instructions,
        row_end = temp_row_instructions,
        col_start = col_ind_clean_var_number + diff_question(i, "col"),
        col_end = col_ind_label_right_number + diff_question(i, "col", doc_label_cell_merge = doc_label_cell_merge * 2)
      )


      if(i == "ui") temp_response_instructions <- ind_ui_response_instructions
      if(i == "doc") temp_response_instructions <- ind_doc_response_instructions

      writeData(
        wb, sheet_name,
        x = temp_response_instructions,
        startRow = temp_row_instructions,
        startCol = col_ind_label_left_number + diff_question(i, "col"),
        colNames = FALSE
      )


      addStyle(
        wb, sheet_name,
        style = style_header,
        rows = temp_row_instructions,
        cols = seq(
          col_ind_clean_var_number + diff_question(i, "col"),
          col_ind_label_right_number + diff_question(i, "col", doc_label_cell_merge = doc_label_cell_merge * 2)
        ),
        gridExpand = TRUE, stack = TRUE
      )

    }



    mergeCells(
      wb, sheet_name,
      rows = row_ind_score,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number)
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_ind_score, row_end = row_ind_score,
      col_start = col_ind_label_point_first_number, col_end = col_ind_label_point_last_number
    )


    addStyle(
      wb, sheet_name,
      style = style_header,
      rows = row_ind_score,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number),
      gridExpand = TRUE, stack = TRUE
    )


    temp_col_result <- num2let(col_ind_engine_survey_var_number)
    temp_col_control <- num2let(col_ind_engine_controls_number)
    writeFormula(
      wb, sheet_name,
      x = glue(
        '=IF(
    {temp_col_result}{row_ind_engine_seg_qc},
    IF({temp_col_control}{row_ind_control_feedback_style} = 1, "Segment " & {temp_col_result}{row_ind_engine_seg}, IF({temp_col_control}{row_ind_control_feedback_style} = 2, TRIM({temp_col_result}{row_ind_engine_seg_name}), "Segment " & {temp_col_result}{row_ind_engine_seg} & " - " & TRIM({temp_col_result}{row_ind_engine_seg_name}))) &
    IF({temp_col_control}{row_ind_control_prob_show}, " (Prob. " & ROUND({temp_col_result}{row_ind_engine_seg_prob}, {num2let(col_ind_engine_controls_number)}{row_ind_control_prob_dec} + 2) * 100 & "%)", ""),
    "Score pending"
    )'
      ),
      startRow = row_ind_score,
      startCol = col_ind_label_point_first_number
    )

    conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number),
      rows = row_ind_score,
      rule = glue('{temp_col_result}{row_ind_engine_seg_qc} = TRUE'),
      style = oxl_style_cell_good(textDecoration = "bold", conditional = TRUE)
    )

    conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number),
      rows = row_ind_score,
      rule = glue('{temp_col_result}{row_ind_engine_seg_qc} = FALSE'),
      style = oxl_style_cell_neurtal(textDecoration = "bold", conditional = TRUE)
    )


    conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number),
      rows = seq(row_ind_first, row_ind_last),
      rule = glue('AND(${num2let(col_ind_engine_controls_number)}${row_ind_engine_seg_qc} = TRUE, COUNTIF(${num2let(col_ind_label_point_first_number)}{row_ind_first}:${num2let(col_ind_label_point_last_number)}{row_ind_first}, "x") > 1)'),
      style = oxl_style_cell_neurtal(textDecoration = "bold", conditional = TRUE)
    )


    #######################
    # write indi engine
    #######################

    writeData(
      wb, sheet_name,
      x = "Individual Typing Tool - Engine",
      startRow = row_title,
      startCol = col_ind_engine_clean_var_number,
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_title,
      rows = row_title,
      cols = col_ind_engine_clean_var_number,
      stack = TRUE
    )


    diff_function <- function(
    place,
    type = c("row", "col")
    ){
      type <- match.arg(type)

      if((place == row_header || place == row_ind_engine_header) && type == "row") return(0)
      if((place == row_header || place == row_ind_engine_header) && type == "col") return(0)
      if(place == row_doc_function_header && type == "row") return(row_diff_ind_eng_doc)
      if(place == row_doc_function_header && type == "col") return(col_diff_ind_eng_doc)
    }


    range_coef <- map(
      segments - 1,
      function(xc){
        glue('{num2let(col_ind_engine_clean_var_number + diff_function(row_doc_function_header, "col") + 2 + xc)}${row_doc_function_header + 1}:{num2let(col_ind_engine_clean_var_number + diff_function(row_doc_function_header, "col") + 2 + xc)}${row_doc_function_header + 1 + nrow(coef_func) - 2}')
      }
    )


    range_constant <- map(
      segments - 1,
      function(xc){
        glue('{num2let(col_ind_engine_clean_var_number + diff_function(row_doc_function_header, "col") + 2 + xc)}${row_doc_function_header + 1 + nrow(coef_func) - 1}')
      }
    )


    for(i in c(row_header, row_ind_engine_header, row_doc_function_header)){

      temp_coef_func <- tibble(
        "Question" = c(clean_variable_names, NA)
      ) %>%
        bind_cols(coef_func)

      temp_coef_func[nrow(temp_coef_func), "Variable"] <- "Constant"


      if(i == row_header){

        temp_coef_func <- temp_coef_func %>%
          mutate(
            "Answer" = NA,
            "Recode" = NA,
            "QC Check" = NA
          )

        temp_cols <- seq(col_ind_engine_clean_var_number, col_ind_engine_qc_number)

        temp_row_header <- seq(i - 2, i - 1)

        temp_header_text <- "Solution Coefficient Function"

        mergeCells(
          wb, sheet_name,
          rows = temp_row_header,
          cols = seq(col_ind_engine_answer_number, col_ind_engine_qc_number)
        )


        writeData(
          wb, sheet_name,
          x = "Response Processing",
          startRow = temp_row_header %>% head(1),
          startCol = col_ind_engine_answer_number,
          colNames = FALSE
        )


        addStyle(
          wb, sheet_name,
          style = style_header2,
          rows = temp_row_header,
          cols = seq(col_ind_engine_answer_number, col_ind_engine_qc_number),
          gridExpand = TRUE, stack = TRUE
        )


        oxl_outer_box(
          wb, sheet_name,
          borderStyle = "medium",
          row_start = temp_row_header %>% head(1), row_end = temp_row_header %>% tail(1),
          col_start = col_ind_engine_answer_number, col_end = col_ind_engine_qc_number
        )

        oxl_outer_box(
          wb, sheet_name,
          borderStyle = "medium",
          row_start = temp_row_header %>% head(1), row_end = i + nrow(temp_coef_func),
          col_start = col_ind_engine_answer_number, col_end = col_ind_engine_qc_number
        )


      }else if(i == row_ind_engine_header){

        temp_coef_func <- temp_coef_func %>%
          mutate_if(
            is.numeric, list(~NA)
          ) %>%
          add_NA_rows(2)

        temp_cols <- seq(col_ind_engine_clean_var_number, col_ind_engine_answer_number - 1)

        temp_row_header <- i - 1

        temp_header_text <- "Response Calculations"

      }else if(i == row_doc_function_header){

        temp_coef_func[nrow(temp_coef_func), "Question"] <- "Constant"

        temp_cols <- seq(col_ind_engine_clean_var_number, col_ind_engine_answer_number - 1) + col_diff_ind_eng_doc

        temp_row_header <- i - 1

        temp_header_text <- "Solution Coefficient Function"
      }


      writeData(
        wb, sheet_name,
        x = temp_coef_func,
        startRow = i,
        startCol = col_ind_engine_clean_var_number + diff_function(i, "col"),
        headerStyle = style_header,
        borders = "all",
        colNames = TRUE
      )


      addStyle(
        wb, sheet_name,
        style = style_table,
        rows = seq(i, i + nrow(temp_coef_func)),
        cols = temp_cols,
        gridExpand = TRUE, stack = TRUE
      )


      mergeCells(
        wb, sheet_name,
        rows = temp_row_header,
        cols = seq(col_ind_engine_clean_var_number, col_ind_engine_answer_number - 1) + diff_function(i, "col")
      )


      writeData(
        wb, sheet_name,
        x = temp_header_text,
        startRow = temp_row_header %>% head(1),
        startCol = col_ind_engine_clean_var_number + diff_function(i, "col"),
        colNames = FALSE
      )


      addStyle(
        wb, sheet_name,
        style = style_header2,
        rows = temp_row_header,
        cols = seq(col_ind_engine_clean_var_number, col_ind_engine_answer_number - 1) + diff_function(i, "col"),
        gridExpand = TRUE, stack = TRUE
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = temp_row_header %>% head(1),
        row_end = temp_row_header %>% tail(1),
        col_start = col_ind_engine_clean_var_number + diff_function(i, "col"),
        col_end = col_ind_engine_answer_number - 1 + diff_function(i, "col")
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = i,
        row_end = i,
        col_start = temp_cols %>% head(1),
        col_end = temp_cols %>% tail(1)
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = i + 1,
        row_end = i + nrow(temp_coef_func),
        col_start = temp_cols %>% head(1),
        col_end = temp_cols %>% tail(1)
      )

      if(i == row_header){
        oxl_outer_box(
          wb, sheet_name,
          borderStyle = "medium",
          row_start = temp_row_header %>% head(1), row_end = i + nrow(temp_coef_func),
          col_start = col_ind_engine_answer_number, col_end = col_ind_engine_qc_number
        )
      }

    }



    for(i in seq(row_ind_first, row_ind_last)){

      writeFormula(
        wb, sheet_name,
        x = glue('MATCH("x", {num2let(col_ind_label_point_first_number)}{i}:{num2let(col_ind_label_point_last_number)}{i}, 0)'),
        startRow = i,
        startCol = col_ind_engine_answer_number,
      )

      writeFormula(
        wb, sheet_name,
        x = glue('INDEX(${num2let(col_ind_label_point_first_number)}${row_ind_recode}:${num2let(col_ind_label_point_last_number)}${row_ind_recode},,{num2let(col_ind_engine_answer_number)}{i})'),
        startRow = i,
        startCol = col_ind_engine_recode_number,
        array = T
      )

      writeFormula(
        wb, sheet_name,
        x = glue('COUNTIF({num2let(col_ind_label_point_first_number)}{i}:{num2let(col_ind_label_point_last_number)}{i},"x")'),
        startRow = i,
        startCol = col_ind_engine_qc_number
      )

      conditionalFormatting(
        wb, sheet_name,
        rows = i,
        cols = col_ind_engine_qc_number,
        rule = "== 1",
        style = oxl_style_cell_good(conditional = TRUE)
      )

      conditionalFormatting(
        wb, sheet_name,
        rows = i,
        cols = col_ind_engine_qc_number,
        rule = "!= 1",
        style = oxl_style_cell_bad(conditional = TRUE)
      )

      if(i == row_ind_last){
        writeData(
          wb, sheet_name,
          x = rep(1, 2) %>% matrix(nrow = 1),
          startRow = i + 1,
          startCol = col_ind_engine_answer_number,
          colNames = FALSE
        )
      }

    }



    for(i in seq(row_ind_engine_first, row_ind_engine_last)){

      for(x in seq(col_ind_engine_survey_var_number + 1, col_ind_engine_answer_number - 1)){
        writeFormula(
          wb, sheet_name,
          x = glue('{num2let(x)}{i - row_eng_coef_diff} * ${num2let(col_ind_engine_recode_number)}{i - row_eng_coef_diff}'),
          startRow = i,
          startCol = x,
        )

        if(i == row_ind_engine_last){
          writeData(
            wb, sheet_name,
            x = c("Score", "Probability") %>% matrix(ncol = 1),
            startRow = i + 1,
            startCol = col_ind_engine_survey_var_number,
            colNames = FALSE
          )

          writeFormula(
            wb, sheet_name,
            x = glue('EXP(SUM({num2let(x)}${row_ind_engine_first}:{num2let(x)}${row_ind_engine_last}))'),
            startRow = i + 1,
            startCol = x,
          )

          writeFormula(
            wb, sheet_name,
            x = glue('{num2let(x)}{i+1} / SUM(${num2let(col_ind_engine_survey_var_number + 1)}${i+1}:${num2let(col_ind_engine_answer_number - 1)}${i+1})'),
            startRow = row_ind_engine_prob,
            startCol = x,
          )
        }
      }
    }



    addStyle(
      wb, sheet_name,
      style = oxl_style_percent(2),
      rows = row_ind_engine_prob,
      cols = seq(col_ind_engine_survey_var_number + 1, col_ind_engine_answer_number - 1),
      gridExpand = TRUE, stack = TRUE
    )


    for(i in c(
      row_ind_engine_seg, row_ind_engine_seg_name,
      row_ind_engine_seg_prob, row_ind_engine_seg_qc
    )){
      for(xc in c(col_ind_engine_survey_var_number, col_ind_engine_controls_number)){
        mergeCells(
          wb, sheet_name,
          rows = seq(i, i + 1),
          cols = xc - 1
        )
        mergeCells(
          wb, sheet_name,
          rows = seq(i, i + 1),
          cols = xc
        )
      }
    }


    writeData(
      wb, sheet_name,
      x = data.frame(
        Results = c("Segment", NA, "Segment Name", NA, "Segment Probability", NA, "QC Check", NA),
        y = NA
      ),
      startRow = row_ind_engine_seg - 1,
      startCol = col_ind_engine_clean_var_number,
      borders = "all",
      colNames = TRUE,
      headerStyle = style_header2
    )

    writeData(
      wb, sheet_name,
      x = data.frame(
        Controls = c("Feedback Style", NA, "Show Probability", NA, "Probability Decimals", NA, "Question Feedback", NA),
        y = c(1, NA, "TRUE", NA, 1, NA, "TRUE", NA)
      ),
      startRow = row_ind_control_feedback_style - 1,
      startCol = col_ind_engine_controls_number - 1,
      borders = "all",
      colNames = TRUE,
      headerStyle = style_header2
    )

    for(i in c(row_ind_control_feedback_style, row_ind_control_prob_dec)){
      writeData(
        wb, sheet_name,
        x = 1,
        startRow = i,
        startCol = col_ind_engine_controls_number,
        colNames = FALSE
      )
    }


    for(xc in c(col_ind_engine_clean_var_number, col_ind_engine_controls_number - 1)){

      mergeCells(
        wb, sheet_name,
        rows = row_ind_engine_seg - 1,
        cols = seq(xc, xc + 1)
      )


      addStyle(
        wb, sheet_name,
        style = style_header,
        rows = seq(row_ind_engine_seg, row_ind_engine_seg_qc + 1),
        cols = xc,
        gridExpand = TRUE, stack = TRUE
      )


      addStyle(
        wb, sheet_name,
        style = oxl_style_center(valign = "center", wrapText = TRUE),
        rows = seq(row_ind_engine_seg, row_ind_engine_seg_qc + 1),
        cols = seq(xc, xc + 1),
        gridExpand = TRUE, stack = TRUE
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = row_ind_engine_seg - 1, row_end = row_ind_engine_seg - 1,
        col_start = xc, col_end = xc + 1
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = row_ind_engine_seg, row_end = row_ind_engine_seg_qc + 1,
        col_start = xc, col_end = xc
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = row_ind_engine_seg, row_end = row_ind_engine_seg_qc + 1,
        col_start = xc, col_end = xc + 1
      )
    }


    writeFormula(
      wb, sheet_name,
      x = glue('MATCH(MAX({range_ind_prob}), {range_ind_prob}, 0)'),
      startRow = row_ind_engine_seg,
      startCol = col_ind_engine_survey_var_number,
    )


    range_seg_name <- glue("${num2let(col_doc_seg_name_value)}${row_doc_seg_name_first}:${num2let(col_doc_seg_name_value)}${row_doc_seg_name_last}")

    writeFormula(
      wb, sheet_name,
      x = glue("INDEX({range_seg_name}, {num2let(col_ind_engine_survey_var_number)}{row_ind_engine_seg}, 1)"),
      startRow = row_ind_engine_seg_name,
      startCol = col_ind_engine_survey_var_number,
    )


    writeFormula(
      wb, sheet_name,
      x = glue('MAX({range_ind_prob})'),
      startRow = row_ind_engine_seg_prob,
      startCol = col_ind_engine_survey_var_number,
    )


    writeFormula(
      wb, sheet_name,
      x = glue('=COUNTIF({num2let(col_ind_engine_qc_number)}{row_ind_first}:{num2let(col_ind_engine_qc_number)}{row_ind_last},1) = {length(clean_variable_names)}'),
      startRow = row_ind_engine_seg_qc,
      startCol = col_ind_engine_survey_var_number,
    )


    conditionalFormatting(
      wb, sheet_name,
      rows = seq(row_ind_engine_seg_qc, row_ind_engine_seg_qc + 1),
      cols = col_ind_engine_survey_var_number,
      rule = "== TRUE",
      style = oxl_style_cell_good(conditional = TRUE)
    )


    conditionalFormatting(
      wb, sheet_name,
      rows = seq(row_ind_engine_seg_qc, row_ind_engine_seg_qc + 1),
      cols = col_ind_engine_survey_var_number,
      rule = "== FALSE",
      style = oxl_style_cell_bad(conditional = TRUE)
    )


    addStyle(
      wb, sheet_name,
      style = oxl_style_percent(2, valign = "center"),
      rows = row_ind_engine_seg_prob,
      cols = col_ind_engine_survey_var_number,
      stack = TRUE
    )


    writeComment(
      wb, sheet_name,
      row = row_ind_control_feedback_style,
      col = col_ind_engine_controls_number,
      comment = createComment(
        comment = "1 = Segment number\n2 = Segment name\n3 = Both",
        author = "Analytic Provider",
        style = createStyle(fontSize = 11),
        visible = TRUE,
        width = 1, height = 1
      )
    )


    #######################
    # write documentation
    #######################

    writeData(
      wb, sheet_name,
      x = "Typing Tool Documentation",
      startRow = row_title,
      startCol = col_doc_start,
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_title,
      rows = row_title,
      cols = col_doc_start,
      stack = TRUE
    )


    writeData(
      wb, sheet_name,
      x = glue("These instructions create the {xfun::numbers_to_words(length(segments))} segment solution from {xfun::numbers_to_words(length(clean_variable_names))} items"),
      startRow = row_doc_intro,
      startCol = col_doc_start,
      colNames = FALSE
    )


    for(xr in seq(row_doc_qualification, row_doc_qualification_last)){
      mergeCells(
        wb, sheet_name,
        cols = seq(col_doc_start, col_doc_qualification_last),
        rows = xr
      )
    }


    qualification_instructions_table <- tibble(
      "Qualifications" = glue("{seq(qualification_instructions)}. {qualification_instructions}"),
      " " = NA
    ) %>% setNames(., names(.) %>% trimws())

    qualification_instructions_table <- qualification_instructions_table[, c(1, rep(2, col_doc_qualification_last - col_doc_start))]

    writeData(
      wb, sheet_name,
      x = qualification_instructions_table,
      startRow = row_doc_qualification,
      startCol = col_doc_start,
      colNames = TRUE,
      headerStyle = style_header2
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(fgFill = "white"),
      rows = seq(row_doc_qualification + 1, row_doc_qualification_last),
      cols = seq(col_doc_start, col_doc_qualification_last),
      gridExpand = TRUE, stack = TRUE
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_doc_qualification, row_end = row_doc_qualification,
      col_start = col_doc_start, col_end = col_doc_qualification_last
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_doc_qualification, row_end = row_doc_qualification_last,
      col_start = col_doc_start, col_end = col_doc_qualification_last
    )



    # create seg name tables

    temp_seg_names <- tibble(
      "Segment Names" = glue("Segment {segments}"),
      " " = segment_names
    ) %>% setNames(., names(.) %>% trimws())

    temp_seg_names <- temp_seg_names[, c(1, rep(2, length(segments) + 1))]


    writeData(
      wb, sheet_name,
      x = temp_seg_names,
      startRow = row_doc_seg_name_header,
      startCol = col_doc_seg_name_header,
      colNames = TRUE,
      headerStyle = style_header2,
      borders = "all",
    )


    for(tr in seq(row_doc_seg_name_header, row_doc_seg_name_last)){
      if(tr == row_doc_seg_name_header){
        mergeCells(
          wb, sheet_name,
          rows = tr,
          cols = seq(col_doc_seg_name_header, col_last)
        )
      }else{
        mergeCells(
          wb, sheet_name,
          rows = tr,
          cols = seq(col_doc_seg_name_value, col_last)
        )
      }
    }


    addStyle(
      wb, sheet_name,
      style = style_header,
      rows = seq(row_doc_seg_name_first, row_doc_seg_name_last),
      cols = col_doc_seg_name_header,
      gridExpand = TRUE, stack = TRUE
    )



    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_doc_seg_name_header,
      row_end = row_doc_seg_name_header,
      col_start = col_doc_seg_name_header,
      col_end = col_last
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_doc_seg_name_header,
      row_end = row_doc_seg_name_last,
      col_start = col_doc_seg_name_header,
      col_end = col_last
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_doc_seg_name_header,
      row_end = row_doc_seg_name_last,
      col_start = col_doc_seg_name_header,
      col_end = col_doc_seg_name_value
    )



    # create step-by-step instructions

    calulation_steps <- tibble(
      "Solution Calculation Step-by-step Instructions" = c(
        NA,
        "Segment membership is calculated by doing the following steps:",
        NA,
        "Step 1 - Questionnaire",
        "          Ask respondents the Solution Questions, recording their answer for each item.  Respondents must respond to each and all items for their score to be valid.",
        NA,
        "          We do not recommend randomizing item order.  Do not randomize label sides.  Splitting the item questions into multiple (more than one) question blocks is a subjective choice.",
        NA,
        "          The Typing Tool was designed to be asked among respondents who meet the stated qualifications.",
        NA, NA,
        "Step 2 - Recoding",
        glue("          Rescale the solution questions ({head(clean_variable_names, 1)} through {tail(clean_variable_names, 1)}) using the following rules:"),
        NA,
        "          1     ->     -4",
        "          2     ->     -2",
        "          3     ->     2",
        "          4     ->     4 (no change)",
        NA, NA,
        "Step 3 - Multiply recoded responses with coefficient function",
        "          For each participant, multiply the rescaled item responses with the coefficeints for each segment. Sum the products for each segment and add the constant.  This will create a score for each segment.",
        NA,
        glue('                    Segment 1 Score = ( rs{head(clean_variable_names, 1)} * {coef_func[1, "Seg_1"]} ) + ... + ( rs{tail(clean_variable_names, 1)} * {coef_func[nrow(coef_func)-1, "Seg_1"]} ) + ( {coef_func[nrow(coef_func), "Seg_1"]} )'),
        NA,
        "                              ...     (Repeat this process for each segment)",
        NA,
        glue('                    Segment {max(segments)} Score = ( rs{head(clean_variable_names, 1)} * {coef_func[1, ncol(coef_func)]} ) + ... + ( rs{tail(clean_variable_names, 1)} * {coef_func[nrow(coef_func)-1, ncol(coef_func)]} ) + ( {coef_func[nrow(coef_func), ncol(coef_func)]} )'),
        NA,
        "          The segment with the highest score is the one the respondent is most likely to be a member of.",
        NA, NA,
        "Step 4 - Calculate the probability (optional)",
        NA,
        "          1. Compute the exponetial value for each segment score",
        NA,
        "                    Segment 1 Exponential Score = EXP( Segment 1 Score )",
        "                              OR",
        glue('                    Segment 1 Exponential Score = EXP( ( rs{head(clean_variable_names, 1)} * {coef_func[1, "Seg_1"]} ) + ... + ( rs{tail(clean_variable_names, 1)} * {coef_func[nrow(coef_func)-1, "Seg_1"]} ) + ( {coef_func[nrow(coef_func), "Seg_1"]} ) )'),
        NA,
        "                              ...     (Repeat this process for each segment)",
        NA,
        glue("                    Segment {max(segments)} Exponential Score = EXP( Segment {max(segments)} Score )"),
        "                              OR",
        glue('                    Segment {max(segments)} Exponential Score = EXP( ( rs{head(clean_variable_names, 1)} * {coef_func[1, ncol(coef_func)]} ) + ... + ( rs{tail(clean_variable_names, 1)} * {coef_func[nrow(coef_func)-1, ncol(coef_func)]} ) + ( {coef_func[nrow(coef_func), ncol(coef_func)]} ) )'),
        NA, NA,
        "          2. Divide each segment's exponetial value with the sum of all expoential values",
        NA,
        glue("                    Segment 1 Probability = Segment 1 Exponential Score / ( Segment 1 Exponential Score + ... + Segment {max(segments)} Exponential Score )"),
        "                              OR",
        glue("                    Segment 1 Probability = EXP( Segment 1 Score ) / ( EXP( Segment 1 Score ) + ... + EXP( Segment {max(segments)} Score ))"),
        NA,
        "                              ...     (Repeat this process for each segment)",
        NA,
        glue("                    Segment {max(segments)} Probability = Segment {max(segments)} Exponential Score / ( Segment 1 Exponential Score + ... + Segment {max(segments)} Exponential Score )"),
        "                              OR",
        glue("                    Segment {max(segments)} Probability = EXP( Segment {max(segments)} Score ) / ( EXP( Segment 1 Score ) + ... + EXP( Segment {max(segments)} Score ))"),
        NA, NA,
        "Step 5 - QC Check",
        NA,
        "          Perform your calculations on a respondent. Then check your results using the Individual UI to the left of this document (starting cell B2).",
        NA,
        "          Note, you can expand Individual Typing Tool Engine, which is currently hidden, if you need additional guidance on step-by-step calculations.",
        NA
      ),
      " " = NA
    ) %>% setNames(., names(.) %>% trimws())


    calulation_steps_bold <- calulation_steps[[1]] %>%
      left(4) %>%
      equals("Step") %>%
      if_na_return()


    calulation_steps <- calulation_steps[, c(1, rep(2, col_last - col_doc_start))]


    for(xr in seq(row_doc_steps, row_doc_steps + nrow(calulation_steps))){
      mergeCells(
        wb, sheet_name,
        cols = seq(col_doc_start, col_last),
        rows = xr
      )
    }


    writeData(
      wb, sheet_name,
      x = calulation_steps,
      startRow = row_doc_steps,
      startCol = col_doc_start,
      colNames = TRUE,
      headerStyle = style_header2
    )


    addStyle(
      wb, sheet_name,
      style = createStyle(fgFill = "white"),
      rows = seq(row_doc_steps + 1, row_doc_steps + nrow(calulation_steps)),
      cols = seq(col_doc_start, col_last),
      gridExpand = TRUE, stack = TRUE
    )


    for(i in seq(row_doc_steps + 1, row_doc_steps + nrow(calulation_steps))[calulation_steps_bold]){
      addStyle(
        wb, sheet_name,
        style = createStyle(textDecoration = "bold"),
        rows = i,
        cols = seq(col_doc_start, col_last),
        gridExpand = TRUE, stack = TRUE
      )
    }


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_doc_steps, row_end = row_doc_steps,
      col_start = col_doc_start, col_end = col_last
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_doc_steps, row_end = row_doc_steps + nrow(calulation_steps),
      col_start = col_doc_start, col_end = col_last
    )



    #######################
    # final column group
    #######################

    groupColumns(
      wb, sheet_name,
      cols = seq(col_ind_engine_clean_var_number, col_ind_engine_qc_number),
      hidden = TRUE
    )


    groupColumns(
      wb, sheet_name,
      cols = seq(col_doc_start, col_last),
      hidden = TRUE
    )

    #######################
    # return
    #######################

    return(
      list(
        "col_last" = col_last,
        "range_recode" = range_recode,
        "range_coef" = range_coef,
        "range_constant" = range_constant,
        "range_seg_name" = range_seg_name
      )
    )
  }




  bulk_typing_tool <- function(
    wb = wb,
    sheet_name = sheet_name,
    row_title = start_row,
    start_col = range_returns[["col_last"]] + 2,
    range_recode = range_returns[["range_recode"]],
    range_coef = range_returns[["range_coef"]],
    range_constant = range_returns[["range_constant"]],
    range_seg_name = range_returns[["range_seg_name"]]
  ){

    #######################
    # reference constants
    #######################

    row_header <- row_title + 5

    row_data_first <- row_header + 1
    row_data_last <- row_data_first + nrow(data_inputs)

    ammount_of_input_rows <- row_data_last %>% divide_by(1000) %>% ceiling() %>% multiply_by(1000)

    row_last <- row_data_first + ammount_of_input_rows

    col_input_first <- start_col + 1
    col_input_last <- start_col + length(clean_variable_names)

    col_calculation_qc <- col_input_last + 1

    col_recode_first <- col_input_first
    col_recode_last <- col_input_last

    if(inputs_are_rs){
      col_recode_first <- col_input_first + length(clean_variable_names) + 1
      col_recode_last <- col_input_last + length(clean_variable_names) + 1
    }

    col_score_first <- col_recode_last + 1
    col_score_last <- col_score_first + length(segments) - 1

    col_prob_first <- col_score_last + 1
    col_prob_last <- col_prob_first + length(segments) - 1
    col_seg <- col_prob_last + 1
    col_seg_name <- col_seg + 1

    col_bulk_qc_first <- col_seg_name + 3
    col_bulk_qc_last <- col_bulk_qc_first + ncol(data_solution_check) - 1

    col_bulk_qc_formula_first <- col_bulk_qc_last + 1
    col_bulk_qc_formula_last <- col_bulk_qc_formula_first + ncol(data_solution_check) - 1

    col_last <- col_bulk_qc_formula_last

    #######################
    # bulk title
    #######################

    writeData(
      wb, sheet_name,
      x = "Bulk Typing Tool",
      startRow = row_title,
      startCol = start_col,
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_title,
      rows = row_title,
      cols = start_col,
      stack = TRUE
    )


    #######################
    # input data
    #######################

    writeData(
      wb, sheet_name,
      x = data.frame(
        y = c("Original Questionnaire", inputs_raw),
        x = c("Respondent", clean_variable_names)
      ) %>% t() %>% data.frame(),
      startRow = row_header - 1,
      startCol = start_col,
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_header,
      rows = seq(row_header - 1, row_header),
      cols = seq(start_col, col_input_last),
      gridExpand = TRUE, stack = TRUE
    )


    mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = seq(start_col, col_input_last)
    )


    writeData(
      wb, sheet_name,
      x = "Data Input",
      startRow = row_header - 2,
      startCol = start_col,
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_header2,
      rows = row_header - 2,
      cols = seq(start_col, col_input_last),
      gridExpand = TRUE, stack = TRUE
    )


    for(i in seq(row_header - 2, row_header)){
      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = i,
        row_end = i,
        col_start = start_col,
        col_end = col_input_last
      )
    }


    temp_data_inputs <- data_inputs %>%
      add_NA_rows(
        ammount_of_input_rows - nrow(data_inputs) + 1
      )

    temp_data_inputs_empty <- matrix(
      NA,
      nrow = nrow(temp_data_inputs),
      ncol = length(clean_variable_names)
    ) %>%
      data.frame() %>%
      set_names(clean_variable_names)


    writeData(
      wb, sheet_name,
      x = temp_data_inputs,
      startRow = row_data_first,
      startCol = start_col,
      borders = "all",
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_data_first, row_last),
      cols = seq(start_col, col_input_last),
      gridExpand = TRUE, stack = TRUE
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_data_first,
      row_end = row_last,
      col_start = start_col,
      col_end = col_input_last
    )



    #######################
    # QC calculation
    #######################

    temp_qc <- tibble(
      "Calculation Ready" = glue('COUNTIFS(${num2let(col_input_first)}{seq(row_data_first, row_last)}:${num2let(col_input_last)}{seq(row_data_first, row_last)},">={min(polar_points)}", ${num2let(col_input_first)}{seq(row_data_first, row_last)}:${num2let(col_input_last)}{seq(row_data_first, row_last)},"<={max(polar_points)}") = {length(inputs)}')
    )
    class(temp_qc[[1]]) <- "formula"


    writeData(
      wb, sheet_name,
      x = temp_qc,
      startRow = row_header,
      startCol = col_calculation_qc,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    writeData(
      wb, sheet_name,
      x = "QC",
      startRow = row_header - 2,
      startCol = col_calculation_qc,
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_data_first, row_last),
      cols = col_calculation_qc,
      gridExpand = TRUE, stack = TRUE
    )

    addStyle(
      wb, sheet_name,
      style = style_header2,
      rows = row_header - 2,
      cols = col_calculation_qc,
      gridExpand = TRUE, stack = TRUE
    )


    for(i in c(row_header - 2, row_header)){
      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = i,
        row_end = i,
        col_start = col_calculation_qc,
        col_end = col_calculation_qc
      )
    }


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_header,
      row_end = row_last,
      col_start = col_calculation_qc,
      col_end = col_calculation_qc
    )

    conditionalFormatting(
      wb, sheet_name,
      cols = col_calculation_qc,
      rows = seq(row_data_first, row_last),
      rule = glue('${num2let(col_calculation_qc)}{row_data_first} = TRUE'),
      style = oxl_style_cell_good(textDecoration = "bold", conditional = TRUE)
    )


    conditionalFormatting(
      wb, sheet_name,
      cols = col_calculation_qc,
      rows = seq(row_data_first, row_last),
      rule = glue('AND(${num2let(col_calculation_qc)}{row_data_first} = FALSE, COUNTIFS(${num2let(col_input_first)}{row_data_first}:${num2let(col_input_last)}{row_data_first},">={min(polar_points)}", ${num2let(col_input_first)}{row_data_first}:${num2let(col_input_last)}{row_data_first},"<={max(polar_points)}") = 0)'),
      style = createStyle(fontColour = "white")
    )


    conditionalFormatting(
      wb, sheet_name,
      cols = col_calculation_qc,
      rows = seq(row_data_first, row_last),
      rule = glue('AND(${num2let(col_calculation_qc)}{row_data_first} = FALSE, COUNTIF(${num2let(col_input_first)}{row_data_first}:${num2let(col_input_last)}{row_data_first},">= 1") > 0)'),
      style = oxl_style_cell_bad(textDecoration = "bold", conditional = TRUE)
    )



    #######################
    # recode data
    #######################

    if(inputs_are_rs){

      temp <- map(
        seq(col_input_first, col_input_last),
        function(xc){
          glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)}, IF(ISBLANK({num2let(xc)}{seq(row_data_first, row_last)}), "", INDEX({range_recode},1,{num2let(xc)}{seq(row_data_first, row_last)})), "")')
        }
      ) %>%
        bind_cols() %>%
        suppressMessages() %>%
        setNames(
          glue("recode_{clean_variable_names}")
        )

      for(i in names(temp)){
        class(temp[[i]]) <- "formula"
      }


      writeData(
        wb, sheet_name,
        x = temp,
        startRow = row_header,
        startCol = col_recode_first,
        borders = "all",
        headerStyle = style_header,
        colNames = TRUE
      )


      mergeCells(
        wb, sheet_name,
        rows = row_header - 2,
        cols = seq(col_recode_first, col_recode_last)
      )


      writeData(
        wb, sheet_name,
        x = "Recode Inputs",
        startRow = row_header - 2,
        startCol = col_recode_first,
        colNames = FALSE
      )


      addStyle(
        wb, sheet_name,
        style = style_header2,
        rows = row_header - 2,
        cols = seq(col_recode_first, col_recode_last),
        gridExpand = TRUE, stack = TRUE
      )


      for(i in c(row_header - 2, row_header)){
        oxl_outer_box(
          wb, sheet_name,
          borderStyle = "medium",
          row_start = i,
          row_end = i,
          col_start = col_recode_first,
          col_end = col_recode_last
        )
      }


      addStyle(
        wb, sheet_name,
        style = style_table,
        rows = seq(row_data_first, row_last),
        cols = seq(col_recode_first, col_recode_last),
        gridExpand = TRUE, stack = TRUE
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = row_data_first,
        row_end = row_last,
        col_start = col_recode_first,
        col_end = col_recode_last
      )

    }


    #######################
    # calculate scores
    #######################

    temp <- map2(
      range_coef,
      range_constant,
      ~glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)}, EXP(MMULT(${num2let(col_recode_first)}{seq(row_data_first, row_last)}:${num2let(col_recode_last)}{seq(row_data_first, row_last)}, {.x}) + {.y}), "")')
    ) %>%
      bind_cols() %>%
      suppressMessages() %>%
      setNames(
        glue("exp_score_seg_{segments}")
      )


    for(i in names(temp)){
      class(temp[[i]]) <- "formula"
    }


    writeData(
      wb, sheet_name,
      x = temp,
      startRow = row_header,
      startCol = col_score_first,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = seq(col_score_first, col_score_last)
    )


    writeData(
      wb, sheet_name,
      x = "Calculate Exponential Scores",
      startRow = row_header - 2,
      startCol = col_score_first,
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_header2,
      rows = row_header - 2,
      cols = seq(col_score_first, col_score_last),
      gridExpand = TRUE, stack = TRUE
    )


    for(i in c(row_header - 2, row_header)){
      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = i,
        row_end = i,
        col_start = col_score_first,
        col_end = col_score_last
      )
    }


    addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_data_first, row_last),
      cols = seq(col_score_first, col_score_last),
      gridExpand = TRUE, stack = TRUE
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_data_first,
      row_end = row_last,
      col_start = col_score_first,
      col_end = col_score_last
    )



    #######################
    # calculate prob
    #######################

    temp <- map(
      seq(col_score_first, col_score_last) %>% num2let(),
      ~glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)}, {.x}{seq(row_data_first, row_last)} / SUM(${num2let(col_score_first)}{seq(row_data_first, row_last)}:${num2let(col_score_last)}{seq(row_data_first, row_last)}), "")')
    ) %>%
      bind_cols() %>%
      suppressMessages() %>%
      setNames(
        glue("prob_seg_{segments}")
      )


    for(i in names(temp)){
      class(temp[[i]]) <- c("formula")
    }


    writeData(
      wb, sheet_name,
      x = temp,
      startRow = row_header,
      startCol = col_prob_first,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = seq(col_prob_first, col_prob_last)
    )


    writeData(
      wb, sheet_name,
      x = "Calculate Probabilities",
      startRow = row_header - 2,
      startCol = col_prob_first,
      colNames = FALSE
    )


    addStyle(
      wb, sheet_name,
      style = style_header2,
      rows = row_header - 2,
      cols = seq(col_prob_first, col_prob_last),
      gridExpand = TRUE, stack = TRUE
    )


    for(i in c(row_header - 2, row_header)){
      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = i,
        row_end = i,
        col_start = col_prob_first,
        col_end = col_prob_last
      )
    }


    addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_data_first, row_last),
      cols = seq(col_prob_first, col_prob_last),
      gridExpand = TRUE, stack = TRUE
    )


    addStyle(
      wb, sheet_name,
      style = oxl_style_percent(2),
      rows = seq(row_data_first, row_last),
      cols = seq(col_prob_first, col_prob_last),
      gridExpand = TRUE, stack = TRUE
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_data_first,
      row_end = row_last,
      col_start = col_prob_first,
      col_end = col_prob_last
    )


    #######################
    # calculate membership
    #######################

    temp <- data.frame(
      "Classification" = glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)}, MATCH(MAX({num2let(col_prob_first)}{seq(row_data_first, row_last)}:{num2let(col_prob_last)}{seq(row_data_first, row_last)}), {num2let(col_prob_first)}{seq(row_data_first, row_last)}:{num2let(col_prob_last)}{seq(row_data_first, row_last)}, 0), "")'),
      "Name" = glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)}, INDEX({range_seg_name}, ${num2let(col_seg)}{seq(row_data_first, row_last)}, 1), "")')
    )
    class(temp[["Classification"]]) <- "formula"
    class(temp[["Name"]]) <- "formula"


    writeData(
      wb, sheet_name,
      x = temp,
      startRow = row_header,
      startCol = col_seg,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    writeData(
      wb, sheet_name,
      x = "Membership",
      startRow = row_header - 2,
      startCol = col_seg,
      colNames = FALSE
    )


    mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = c(col_seg, col_seg_name)
    )


    addStyle(
      wb, sheet_name,
      style = style_header2,
      rows = row_header - 2,
      cols = c(col_seg, col_seg_name),
      gridExpand = TRUE, stack = TRUE
    )


    for(i in c(row_header - 2, row_header)){
      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = i,
        row_end = i,
        col_start = col_seg,
        col_end = col_seg_name
      )
    }


    addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_data_first, row_last),
      cols = c(col_seg, col_seg_name),
      gridExpand = TRUE, stack = TRUE
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_data_first,
      row_end = row_last,
      col_start = col_seg,
      col_end = col_seg_name
    )


    #######################
    # bulk QC
    #######################

    writeData(
      wb, sheet_name,
      x = data_solution_check,
      startRow = row_header,
      startCol = col_bulk_qc_first,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    data_solution_check_formulas <- map2(
      seq(col_bulk_qc_first, col_bulk_qc_last - 1),
      seq(col_prob_first, col_prob_last),
      ~glue('ROUND({num2let(.x)}{seq(row_header + 1, row_header + nrow(data_solution_check))}, 4) = ROUND({num2let(.y)}{seq(row_header + 1, row_header + nrow(data_solution_check))}, 4)')
    ) %>%
      bind_cols() %>%
      suppressMessages() %>%
      setNames(
        glue('Prob Check {names(data_solution_check)[-ncol(data_solution_check)]}')
      ) %>%
      mutate(
        "Class Check" = glue('{num2let(col_bulk_qc_last)}{seq(row_header + 1, row_header + nrow(data_solution_check))} = {num2let(col_seg)}{seq(row_header + 1, row_header + nrow(data_solution_check))}')
      )

    for(i in names(data_solution_check_formulas)){
      class(data_solution_check_formulas[[i]]) <- c("formula")
    }


    writeData(
      wb, sheet_name,
      x = data_solution_check_formulas,
      startRow = row_header,
      startCol = col_bulk_qc_formula_first,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    writeData(
      wb, sheet_name,
      x = "Bulk QC Check",
      startRow = row_header - 2,
      startCol = col_bulk_qc_first,
      colNames = FALSE
    )


    mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = seq(col_bulk_qc_first, col_bulk_qc_formula_last)
    )


    addStyle(
      wb, sheet_name,
      style = style_header2,
      rows = row_header - 2,
      cols = seq(col_bulk_qc_first, col_bulk_qc_formula_last),
      gridExpand = TRUE, stack = TRUE
    )


    for(i in c(row_header - 2, row_header)){
      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = i,
        row_end = i,
        col_start = col_bulk_qc_first,
        col_end = col_bulk_qc_formula_last
      )
    }


    addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_header + 1, row_header + nrow(data_solution_check)),
      cols = seq(col_bulk_qc_first, col_bulk_qc_formula_last),
      gridExpand = TRUE, stack = TRUE
    )


    addStyle(
      wb, sheet_name,
      style = oxl_style_percent(4),
      rows = seq(row_header + 1, row_header + nrow(data_solution_check)),
      cols = seq(col_bulk_qc_first, col_bulk_qc_last - 1),
      gridExpand = TRUE, stack = TRUE
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_header + 1,
      row_end = row_header + nrow(data_solution_check),
      col_start = col_bulk_qc_first,
      col_end = col_bulk_qc_formula_last
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_header,
      row_end = row_header + nrow(data_solution_check),
      col_start = col_bulk_qc_first,
      col_end = col_bulk_qc_last
    )


    conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_bulk_qc_formula_first, col_bulk_qc_formula_last),
      rows = seq(row_header + 1, row_header + nrow(data_solution_check)),
      rule = glue('{num2let(col_bulk_qc_formula_first)}{row_header + 1} = TRUE'),
      style = oxl_style_cell_good(textDecoration = "bold", conditional = TRUE)
    )


    conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_bulk_qc_formula_first, col_bulk_qc_formula_last),
      rows = seq(row_header + 1, row_header + nrow(data_solution_check)),
      rule = glue('{num2let(col_bulk_qc_formula_first)}{row_header + 1} = FALSE'),
      style = oxl_style_cell_bad(textDecoration = "bold", conditional = TRUE)
    )


    #######################
    # final column group
    #######################

    setColWidths(wb, sheet_name, cols = col_seg, widths = 15)
    setColWidths(wb, sheet_name, cols = c(start_col, col_seg_name), widths = 25)

    groupColumns(
      wb, sheet_name,
      cols = seq(col_recode_first, col_score_last),
      hidden = TRUE
    )

  }



  #######################
  # create workbook
  #######################

  wb <- oxl_create_workbook()

  sheet_name <- "Typing Tool"

  addWorksheet(wb, sheet_name)

  row_last_formatting <- (start_row + 6) + ((start_row + 6 + nrow(data_inputs)) %>% divide_by(1000) %>% ceiling() %>% multiply_by(1000)) + 1

  addStyle(
    wb, sheet_name,
    style = createStyle(fgFill = oxl_colorscale_grey(1)),
    rows = seq(1, row_last_formatting),
    cols = seq(1, 200),
    gridExpand = TRUE
  )


  range_returns <- individual_typing_tool(
    wb = wb,
    sheet_name = sheet_name,
    row_title = start_row,
    start_col = start_col,
    polar_label_width = polar_label_width
  )


  bulk_typing_tool(
    wb = wb,
    sheet_name = sheet_name,
    row_title = start_row,
    start_col = range_returns[["col_last"]] + 2,
    range_recode = range_returns[["range_recode"]],
    range_coef = range_returns[["range_coef"]],
    range_constant = range_returns[["range_constant"]],
    range_seg_name = range_returns[["range_seg_name"]]
  )


  #######################
  # create workbook
  #######################

  if(is.null(where)){
    where <- seg[["paths"]][["folders"]][["solution"]]
  }
  if(is.null(where)){
    where <- getwd()
  }

  file_name <- glue("{where}/{file_name}.xlsx")

  saveWorkbook(wb, file_name, overwrite = TRUE)

}
