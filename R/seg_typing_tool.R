#' seg_typing_tool
#'
#' @description Builds a 3-sheet "Typing Tool" Excel workbook for a given LDA
#'   solution: an interactive Individual UI sheet with a scoring engine, a
#'   printable Documentation sheet (qualification instructions, solution
#'   questions, coefficient function, step-by-step calculation guide), and a
#'   Bulk sheet for scoring many respondents at once. The Individual UI and
#'   Bulk sheets pull Survey Variable / Side A / Side B labels and the
#'   coefficient function from the Documentation sheet via cross-sheet
#'   formulas, so edits to the Documentation sheet propagate.
#'
#' @param seg A seg object with `solutions`, `data$with_solutions`, and
#'   `spec` populated.
#' @param solution_name Character. The `lda_name` of the solution to build the
#'   typing tool for.
#' @param survey_respondent_id Character. Column name of the respondent ID in
#'   `seg$data$with_solutions`. Default `"uuid"`.
#' @param qualification_instructions Character vector. Qualification bullet
#'   lines shown on the Documentation sheet.
#' @param overwrite_left_label,overwrite_right_label Optional character vectors
#'   to override the polar side A / side B labels pulled from the spec.
#' @param segment_names Optional character vector of segment display names. If
#'   `NULL`, placeholder names (`"Segment Name 1"`, ...) are used.
#' @param file_name Character. Output file basename (no extension).
#' @param where Optional character. Directory to save into. If `NULL`, uses
#'   `seg$paths$folders$solution`, else the working directory.
#' @param start_row,start_col Integers. Top-left anchor for content on each
#'   sheet. Default `(2, 2)`.
#' @param polar_label_width Numeric. Column width for the side-A / side-B
#'   label columns on the Individual UI sheet.
#' @param additional_questions Optional named list of post-LDA reassignment
#'   rules using survey variables that are **not** LDA inputs. Each element's
#'   name is the survey variable, and the value is a tibble with columns
#'   `from_seg`, `value`, `to_seg` (same shape as `additional_logic`). These
#'   variables get their own input blocks rendered on the Individual UI,
#'   Documentation, and Bulk sheets so respondents can supply answers, then
#'   matching rules redistribute probability the same way `additional_logic`
#'   does. Names must exist in `seg$data$with_solutions` and must not overlap
#'   with the LDA inputs (use `additional_logic` for those). Any new segments
#'   introduced via `to_seg` must be sequential (combined with new segments
#'   from `additional_logic`).
#' @param apply_additional_logic_to_qc Logical. When `TRUE`, the
#'   `additional_logic` redistribution is also applied to `data_solution_check`
#'   (the LDA reference probabilities and predicted seg) so the Bulk QC check
#'   compares post-redistribution values on both sides. When `FALSE` (default),
#'   `data_solution_check` stays at the LDA's raw output and the Bulk QC check
#'   will mark reassigned respondents as mismatches — useful for verifying the
#'   LDA scoring itself rather than the reassigned classification.
#' @param additional_logic Optional named list of post-LDA reassignment rules.
#'   Each element's name must be the variable name of an LDA input (polar or
#'   non-polar) and the value is a tibble with columns `from_seg`, `value`,
#'   `to_seg`. After the LDA picks an initial segment, any matching rule
#'   reassigns the respondent to `to_seg` for the final classification. Rules
#'   may introduce new segments beyond the LDA-derived count, but only as the
#'   next sequential integers (e.g., if the LDA produced segs 1-6, new
#'   segments must be 7, 8, ...; gaps raise an error). All variable names must
#'   exist as LDA inputs; mismatches raise an error.
#' @param use_colinear_lda Optional logical. Controls which coefficient
#'   function the typing tool embeds:
#'   * `NULL` (default) — auto-pick: if the solution's `collinear` flag is
#'     `TRUE`, recompute the coefficient table from `lda_fit` via
#'     [coefficient_lda_colinear()]. Otherwise use the pre-computed
#'     `lda_coefficient_function` stored on the solution.
#'   * `TRUE` — always recompute via [coefficient_lda_colinear()].
#'   * `FALSE` — always use the pre-computed `lda_coefficient_function`.
#'
#' @return Invisibly writes the workbook to disk. Returns `NULL`.
#'
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
    additional_questions = NULL,
    additional_logic = NULL,
    apply_additional_logic_to_qc = FALSE,
    use_colinear_lda = NULL
){

  row_title <- start_row

  where <- seg[["paths"]][["folders"]][["solution"]]

  df <- seg[["data"]][["with_solutions"]]

  doc_label_cell_merge <- 6
  doc_row_gap          <- 2


  #######################
  # setup styles
  #######################

  style_header <- openxlsx::createStyle(
    textDecoration ="bold",
    halign = "center",
    fgFill = oxl_colorscale_grey(2),
    wrapText = TRUE
  )


  style_header2 <- openxlsx::createStyle(
    textDecoration ="bold",
    halign = "center",
    fgFill = oxl_colorscale_grey(3),
    wrapText = TRUE
  )


  style_title <- openxlsx::createStyle(
    textDecoration ="bold",
    halign = "left",
    fontSize = 16
  )


  style_table <- openxlsx::createStyle(
    fgFill = NULL,
    halign = "center"
  )


  #######################
  # setup objections
  #######################

  inputs <- seg[["solutions"]][["summary_table"]] %>%
    dplyr::filter(lda_name == !!solution_name) %>%
    dplyr::select(lda_inputs) %>%
    unlist() %>%
    setNames(NULL)


  polars_table <- seg[["spec"]][["polars_table"]]

  profile_table <- seg[["spec"]][["profiles"]] %>%
    tidyr::unnest(vars)


  #######################
  # value-label parser (reads seg$data$original_dictionary)
  #######################

  parse_value_labels <- function(var_name){
    dict <- seg[["data"]][["original_dictionary"]]
    data_vals <- seg[["data"]][["with_solutions"]][[var_name]] %>%
      unique() %>% stats::na.omit() %>% sort()

    fallback <- tibble::tibble(
      code  = as.numeric(data_vals),
      label = paste0("value ", as.character(data_vals))
    )

    if(is.null(dict) || !"variable" %in% names(dict) || !var_name %in% dict$variable){
      return(fallback)
    }

    row <- dict %>% dplyr::filter(variable == var_name)
    vl_str <- row[["value_labels"]][1]

    if(is.null(vl_str) || is.na(vl_str) || !nzchar(vl_str)){
      return(fallback)
    }

    pairs <- strsplit(vl_str, ", ")[[1]]
    parsed <- purrr::map_dfr(pairs, function(p){
      parts <- strsplit(p, " = ", fixed = TRUE)[[1]]
      if(length(parts) < 2) return(NULL)
      tibble::tibble(
        code  = suppressWarnings(as.numeric(parts[1])),
        label = paste(parts[-1], collapse = " = ")
      )
    })

    parsed <- parsed %>% dplyr::filter(!is.na(code)) %>% dplyr::arrange(code)

    if(nrow(parsed) == 0) return(fallback)

    parsed
  }


  #######################
  # classify each input
  #######################

  polar_rs_hit     <- inputs %in% polars_table[["rs_var"]]
  polar_source_hit <- (inputs %in% polars_table[["source_var"]]) & !polar_rs_hit
  polar_hit        <- polar_rs_hit | polar_source_hit

  profile_hit      <- (inputs %in% profile_table[["var"]]) |
                      (inputs %in% profile_table[["source_var"]])
  profile_hit      <- profile_hit & !polar_hit

  data_hit         <- (inputs %in% names(seg[["data"]][["with_solutions"]])) &
                      !polar_hit & !profile_hit

  unresolved <- inputs[!polar_hit & !profile_hit & !data_hit]
  if(length(unresolved) > 0){
    stop(glue::glue("Could not resolve these inputs against polars, profiles, or data columns: {paste(unresolved, collapse = ', ')}"))
  }

  polar_inputs     <- inputs[polar_hit]
  non_polar_inputs <- inputs[!polar_hit]

  if(length(polar_inputs) > 0){
    n_rs  <- sum(polar_rs_hit)
    n_src <- sum(polar_source_hit)
    if(n_rs > 0 && n_src > 0){
      stop("Polar inputs mix `rs_var` and `source_var` — must be one or the other.")
    }
    inputs_are_rs <- n_rs > 0
  }else{
    inputs_are_rs <- FALSE
  }


  #######################
  # non-polar inputs table (scaffolding for pass #2 — not yet rendered)
  #######################

  non_polar_inputs_table <- NULL

  if(length(non_polar_inputs) > 0){
    spec_profile_lookup <- tryCatch(
      seg[["spec"]][["profiles"]] %>%
        tidyr::unnest(vars) %>%
        dplyr::select(var, label),
      error = function(e) NULL
    )
    dict_tbl <- seg[["data"]][["original_dictionary"]]

    non_polar_inputs_table <- tibble::tibble(
      var = non_polar_inputs
    ) %>%
      dplyr::mutate(
        label = purrr::map_chr(var, function(v){
          # 1) prefer the spec profiles label
          if(!is.null(spec_profile_lookup) && nrow(spec_profile_lookup) > 0){
            row <- spec_profile_lookup %>% dplyr::filter(var == v)
            if(nrow(row) > 0){
              lbl <- row$label[1]
              if(!is.na(lbl) && nzchar(lbl)) return(lbl)
            }
          }
          # 2) fall back to the original-dictionary label
          if(!is.null(dict_tbl) && "variable" %in% names(dict_tbl) && "label" %in% names(dict_tbl)){
            d_row <- dict_tbl %>% dplyr::filter(variable == v)
            if(nrow(d_row) > 0){
              lbl <- d_row$label[1]
              if(!is.na(lbl) && nzchar(lbl)) return(lbl)
            }
          }
          # 3) last resort — return the variable name itself
          v
        }),
        value_table = purrr::map(var, parse_value_labels)
      )
  }


  #######################
  # polar-only homogeneous detection (existing behavior preserved)
  #######################

  inputs_are_profile <- FALSE
  inputs_are_profile_dichot <- FALSE

  if(length(polar_inputs) == 0 && length(non_polar_inputs) > 0){
    # Pure non-polar / profile path
    if(all(inputs %in% profile_table[["source_var"]])){
      inputs_are_profile <- TRUE
      inputs_are_profile_dichot <- FALSE
    }else if(all(inputs %in% profile_table[["var"]])){
      inputs_are_profile <- TRUE
      inputs_are_profile_dichot <- TRUE
    }
    # else: pure non-polar (no profiles either) — handled via non_polar_inputs_table downstream
  }



  if(length(polar_inputs) > 0){

    if(inputs_are_rs){
      inputs_table <- polars_table %>% dplyr::filter(rs_var %in% polar_inputs)
      polar_reordered     <- inputs_table %>% dplyr::pull(rs_var)
      polar_reordered_raw <- inputs_table %>% dplyr::pull(source_var)
    }else{
      inputs_table <- polars_table %>% dplyr::filter(source_var %in% polar_inputs)
      polar_reordered     <- inputs_table %>% dplyr::pull(source_var)
      polar_reordered_raw <- polar_reordered
    }

    # polars first, non-polars after
    inputs     <- c(polar_reordered,     non_polar_inputs)
    inputs_raw <- c(polar_reordered_raw, non_polar_inputs)

  }else if(inputs_are_profile && !inputs_are_profile_dichot){

    inputs_table <- profile_table %>% dplyr::filter(source_var %in% inputs)

    inputs <- inputs_table %>% dplyr::select(source_var) %>% unlist() %>% setNames(NULL)
    inputs_raw <- inputs

  }else if(inputs_are_profile && inputs_are_profile_dichot){

    inputs_table <- profile_table %>% dplyr::filter(var %in% inputs)

    inputs <- inputs_table %>% dplyr::select(var) %>% unlist() %>% setNames(NULL)
    inputs_raw <- inputs_table %>% dplyr::select(source_var) %>% unlist() %>% setNames(NULL)

  }else{

    # Pure non-polar (no polars, no profile matches) — use non_polar_inputs_table
    inputs_table <- NULL
    inputs       <- non_polar_inputs
    inputs_raw   <- non_polar_inputs
  }


  if(!inputs_are_profile){
    if(!is.null(overwrite_left_label)) inputs_table[["left_label"]] <- overwrite_left_label
    if(!is.null(overwrite_right_label)) inputs_table[["right_label"]] <- overwrite_right_label
  }else if(inputs_are_profile){
    if(!is.null(overwrite_left_label)) inputs_table[["label"]] <- overwrite_left_label
  }



  # Decide whether to (re)compute the coefficient function via
  # coefficient_lda_colinear() or use the pre-computed one on the solution.
  analysis_solution_row <- seg[["solutions"]][["analysis"]] %>%
    purrr::map(purrr::pluck, "solution_table") %>%
    dplyr::bind_rows() %>%
    dplyr::filter(lda_name == !!solution_name) %>%
    dplyr::slice(1)

  collinear_flag <- isTRUE(analysis_solution_row[["collinear"]][[1]])

  resolved_use_colinear <- if(is.null(use_colinear_lda)){
    collinear_flag
  }else{
    isTRUE(use_colinear_lda)
  }

  if(resolved_use_colinear){
    lda_fit_obj <- analysis_solution_row[["lda_fit"]][[1]]
    if(is.null(lda_fit_obj) || (length(lda_fit_obj) == 1 && is.na(lda_fit_obj))){
      stop(glue::glue("`use_colinear_lda` requested but `lda_fit` is unavailable for solution '{solution_name}'."))
    }
    coef_func <- coefficient_lda_colinear(lda_fit_obj) %>%
      dplyr::slice(
        match(
          c(inputs, "constant"),
          .[["variable"]]
        )
      ) %>%
      setNames(., stringr::str_to_title(names(.)))
  }else{
    coef_func <- seg[["solutions"]][["summary_table"]] %>%
      dplyr::filter(lda_name == !!solution_name) %>%
      dplyr::select(lda_coefficient_function) %>%
      purrr::flatten_dfc() %>%
      dplyr::slice(
        match(
          c(inputs, "constant"),
          .[["variable"]]
        )
      ) %>%
      setNames(., stringr::str_to_title(names(.)))
  }


  data_solution_check <- seg[["solutions"]][["analysis"]] %>%
    purrr::map(purrr::pluck, "solution_table") %>%
    dplyr::bind_rows() %>%
    dplyr::filter(lda_name == !!solution_name) %>%
    dplyr::pull(lda_predict) %>%
    purrr::pluck(1) %>%
    dplyr::select(-dplyr::any_of("seg_uuid")) %>%
    dplyr::mutate(seg = as.numeric(as.character(seg)))


  segments <- seg[["solutions"]][["summary_table"]] %>%
    dplyr::filter(lda_name == !!solution_name) %>%
    dplyr::pull(n) %>%
    unlist() %>%
    seq()


  if(is.null(segment_names)){
    segment_names <- glue::glue("Segment Name {segments}")
  }else{
    segment_names <- segment_names %>% stringr::str_squish()
  }

  lda_segments <- segments


  #######################
  # additional_logic + additional_questions — validate, extend segments,
  # extend coef_func, and (optionally) redistribute data_solution_check
  #######################

  additional_logic_info     <- NULL
  additional_questions_info <- NULL
  combined_new_segs         <- integer(0)

  validate_logic_list <- function(arg_value, arg_name, must_be_in_inputs){
    if(is.null(arg_value)) return(invisible(NULL))
    if(!is.list(arg_value) || is.null(names(arg_value)) || any(names(arg_value) == "")){
      stop(glue::glue("`{arg_name}` must be a named list of tibbles."))
    }
    required_cols <- c("from_seg", "value", "to_seg")
    for(var_name in names(arg_value)){
      rules <- arg_value[[var_name]]
      if(!is.data.frame(rules) || !all(required_cols %in% names(rules))){
        stop(glue::glue("`{arg_name}[['{var_name}']]` must be a tibble with columns: {paste(required_cols, collapse = ', ')}."))
      }
    }
    if(must_be_in_inputs){
      bad <- names(arg_value)[!names(arg_value) %in% inputs]
      if(length(bad) > 0){
        stop(glue::glue(
          "`{arg_name}` variable(s) not found in LDA inputs: {paste(bad, collapse = ', ')}. ",
          "Names must match LDA inputs ({paste(inputs, collapse = ', ')})."
        ))
      }
    }else{
      bad_in_inputs <- names(arg_value)[names(arg_value) %in% inputs]
      if(length(bad_in_inputs) > 0){
        stop(glue::glue(
          "`{arg_name}` variable(s) overlap with LDA inputs: {paste(bad_in_inputs, collapse = ', ')}. ",
          "Use `additional_logic` for LDA-input variables; reserve `additional_questions` for non-LDA survey vars."
        ))
      }
      bad_in_data <- names(arg_value)[!names(arg_value) %in% names(seg[["data"]][["with_solutions"]])]
      if(length(bad_in_data) > 0){
        stop(glue::glue(
          "`{arg_name}` variable(s) not found in seg$data$with_solutions: {paste(bad_in_data, collapse = ', ')}."
        ))
      }
    }
  }

  validate_logic_list(additional_logic,     "additional_logic",     must_be_in_inputs = TRUE)
  validate_logic_list(additional_questions, "additional_questions", must_be_in_inputs = FALSE)

  if(!is.null(additional_logic)) additional_logic_info <- additional_logic

  if(!is.null(additional_questions)){
    spec_profile_lookup_aq <- tryCatch(
      seg[["spec"]][["profiles"]] %>% tidyr::unnest(vars) %>% dplyr::select(var, label),
      error = function(e) NULL
    )
    dict_tbl_aq <- seg[["data"]][["original_dictionary"]]

    additional_questions_info <- purrr::imap(additional_questions, function(rules, var_name){
      label_val <- var_name
      if(!is.null(spec_profile_lookup_aq) && nrow(spec_profile_lookup_aq) > 0){
        row <- spec_profile_lookup_aq %>% dplyr::filter(var == var_name)
        if(nrow(row) > 0){
          v <- row$label[1]
          if(!is.na(v) && nzchar(v)) label_val <- v
        }
      }
      if(label_val == var_name && !is.null(dict_tbl_aq) && "variable" %in% names(dict_tbl_aq) && "label" %in% names(dict_tbl_aq)){
        d <- dict_tbl_aq %>% dplyr::filter(variable == var_name)
        if(nrow(d) > 0){
          v <- d$label[1]
          if(!is.na(v) && nzchar(v)) label_val <- v
        }
      }

      list(
        var         = var_name,
        label       = label_val,
        value_table = parse_value_labels(var_name),
        rules       = rules
      )
    })
  }

  # combine new segs from both sources for one sequential check
  current_max_seg <- max(as.integer(segments))
  all_to_segs <- c(
    unlist(purrr::map(additional_logic,     ~ as.integer(.x$to_seg))),
    unlist(purrr::map(additional_questions, ~ as.integer(.x$to_seg)))
  )
  new_segs <- sort(unique(all_to_segs[all_to_segs > current_max_seg]))

  if(length(new_segs) > 0){
    expected_new <- seq.int(current_max_seg + 1, current_max_seg + length(new_segs))
    if(!identical(as.integer(new_segs), as.integer(expected_new))){
      stop(glue::glue(
        "`additional_logic` / `additional_questions` introduce new segment(s) ({paste(new_segs, collapse = ', ')}) ",
        "but they must be sequential after the LDA segments (max = {current_max_seg}); ",
        "expected {paste(expected_new, collapse = ', ')}."
      ))
    }

    combined_new_segs <- as.integer(new_segs)
    segments <- seq.int(min(as.integer(segments)), max(as.integer(new_segs)))

    if(length(segment_names) < length(segments)){
      new_seg_indices <- (length(segment_names) + 1):length(segments)
      segment_names <- c(segment_names, glue::glue("Segment Name {new_seg_indices}"))
    }
  }

  # extend coef_func with zero coefs + very-negative constant for the new segments
  if(length(combined_new_segs) > 0){
    new_col_names <- glue::glue("Seg_{combined_new_segs}")
    new_cols <- as.data.frame(matrix(0, nrow = nrow(coef_func), ncol = length(combined_new_segs)))
    names(new_cols) <- new_col_names
    new_cols[nrow(coef_func), ] <- -1e6
    coef_func <- dplyr::bind_cols(coef_func, new_cols)
  }

  if(isTRUE(apply_additional_logic_to_qc) && (!is.null(additional_logic_info) || !is.null(additional_questions_info))){
    # redistribute probs in data_solution_check + update seg via argmax
    seg_prob_cols <- glue::glue("seg_{lda_segments}")
    new_prob_cols <- glue::glue("seg_{combined_new_segs}")

    if(length(combined_new_segs) > 0){
      for(c in new_prob_cols){
        data_solution_check[[c]] <- 0
      }
    }

    all_prob_cols <- c(seg_prob_cols, new_prob_cols)

    combined_logic_for_qc <- c(
      additional_logic_info,
      if(!is.null(additional_questions_info)){
        setNames(
          purrr::map(additional_questions_info, ~ .x$rules),
          purrr::map_chr(additional_questions_info, "var")
        )
      }else NULL
    )

    for(i in seq_len(nrow(data_solution_check))){
      probs <- as.numeric(data_solution_check[i, all_prob_cols])
      probs[is.na(probs)] <- 0

      for(var_name in names(combined_logic_for_qc)){
        rules    <- combined_logic_for_qc[[var_name]]
        resp_val <- seg[["data"]][["with_solutions"]][[var_name]][i]
        if(is.na(resp_val)) next

        for(j in seq_len(nrow(rules))){
          if(rules$value[j] == resp_val){
            from_idx <- match(rules$from_seg[j], segments)
            to_idx   <- match(rules$to_seg[j], segments)
            if(!is.na(from_idx) && !is.na(to_idx)){
              probs[to_idx]   <- probs[to_idx] + probs[from_idx]
              probs[from_idx] <- 0
            }
          }
        }
      }

      for(k in seq_along(all_prob_cols)){
        data_solution_check[[all_prob_cols[k]]][i] <- probs[k]
      }

      if(any(probs > 0)){
        data_solution_check[["seg"]][i] <- as.numeric(segments[which.max(probs)])
      }
    }

    data_solution_check <- data_solution_check[, c(all_prob_cols, "seg")]
  }

  # back-compat name for downstream code that already references this variable
  additional_logic_new_segs <- combined_new_segs


  data_inputs <- seg[["data"]][["with_solutions"]] %>% dplyr::select(dplyr::all_of(c(survey_respondent_id, inputs_raw)))


  # Split raw inputs into polar and non-polar for downstream handling
  polar_inputs_raw     <- if(length(polar_inputs) > 0) head(inputs_raw, length(polar_inputs)) else character(0)
  non_polar_inputs_raw <- non_polar_inputs


  polar_info <- seg %>% seg_get_vars_polars()

  has_recoding <- length(polar_inputs) > 0 && inputs[1] %in% polar_info$rs_var

  if(has_recoding){
    first_polar_row <- polar_info %>% dplyr::filter(rs_var == inputs[1])
    recode_source_col <- first_polar_row$source_var[1]
    recode_rs_col     <- first_polar_row$rs_var[1]

    recode_mapping <- seg[["data"]][["with_solutions"]] %>%
      dplyr::select(dplyr::all_of(c(recode_source_col, recode_rs_col))) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(.data[[recode_source_col]]), !is.na(.data[[recode_rs_col]])) %>%
      dplyr::arrange(.data[[recode_source_col]])
  }


  if(length(polar_inputs) > 0){
    polar_points <- data_inputs %>%
      dplyr::select(dplyr::all_of(polar_inputs_raw)) %>%
      unlist() %>% unique() %>% dplyr::setdiff(NA) %>% length() %>% seq()
  }else{
    polar_points <- integer(0)
  }


  if(inputs_are_profile && inputs_are_profile_dichot){
    polar_points <- seq(0, 1)
  }


  if(length(polar_inputs) > 0){
    ind_response <- data_inputs %>%
      dplyr::select(dplyr::all_of(polar_inputs_raw)) %>%
      dplyr::slice(1) %>%
      unlist() %>%
      purrr::map(
        ~{
          answer <- (polar_points %in% .x) %>%
            ifelse("x", NA)

          matrix(answer, nrow = 1) %>%
            data.frame()
        }
      ) %>%
      dplyr::bind_rows() %>%
      setNames(polar_points)
  }else{
    ind_response <- NULL
  }


  clean_variable_names <- glue::glue("Q{seq_len(length(inputs))}")


  #######################
  # build ind_ui (polar block only — shared by individual and documentation tools)
  #######################

  has_non_polar_outer <- !is.null(non_polar_inputs_table)

  if(length(polar_inputs) > 0 && !is.null(inputs_table)){
    if(has_non_polar_outer){
      ind_ui <- tibble::tibble(
        "Question" = head(clean_variable_names, nrow(inputs_table)),
        "Survey Variable" = inputs_table %>% dplyr::select(source_var) %>% purrr::flatten_chr() %>% stringr::str_squish(),
        "Side A" = inputs_table %>% dplyr::select(left_label) %>% purrr::flatten_chr() %>% stringr::str_squish(),
        " " = NA,
        matrix(nrow = nrow(inputs_table), ncol = length(polar_points)) %>% data.frame(),
        "Side B" = inputs_table %>% dplyr::select(right_label) %>% purrr::flatten_chr() %>% stringr::str_squish()
      ) %>%
        setNames(c("Question", "Survey Variable", "Side A", " ", rep("", length(polar_points)), "Side B"))
    }else{
      ind_ui <- tibble::tibble(
        "Question" = head(clean_variable_names, nrow(inputs_table)),
        "Survey Variable" = inputs_table %>% dplyr::select(source_var) %>% purrr::flatten_chr() %>% stringr::str_squish(),
        "Side A" = inputs_table %>% dplyr::select(left_label) %>% purrr::flatten_chr() %>% stringr::str_squish(),
        matrix(nrow = nrow(inputs_table), ncol = length(polar_points)) %>% data.frame(),
        "Side B" = inputs_table %>% dplyr::select(right_label) %>% purrr::flatten_chr() %>% stringr::str_squish()
      ) %>%
        setNames(c("Question", "Survey Variable", "Side A", rep("", length(polar_points)), "Side B"))
    }
  }else{
    ind_ui <- NULL
  }

  if(!is.null(ind_ui)){
    if(length(polar_points) == 4){
      ind_ui <- ind_ui %>%
        setNames(c(
          "Question", "Survey Variable", "Side A",
          if(has_non_polar_outer) " " else NULL,
          "Agree Much \nMore \n<<", "Agree Somewhat \nMore \n<<",
          "Agree Somewhat \nMore \n>>", "Agree Much \nMore \n>>",
          "Side B"
        ))
    }else if(length(polar_points) == 2){
      ind_ui <- ind_ui %>%
        setNames(c(
          "Question", "Survey Variable", "Side A",
          if(has_non_polar_outer) " " else NULL,
          "Agree \nMore \n<<", "Agree \nMore \n>>",
          "Side B"
        ))
    }
  }


  #######################
  # non-polar info bundle for rendering (names + rows)
  #######################

  if(!is.null(non_polar_inputs_table)){
    non_polar_inputs_info <- purrr::map(
      seq_len(nrow(non_polar_inputs_table)),
      function(i){
        nv_count    <- nrow(non_polar_inputs_table$value_table[[i]])
        clean_idx   <- length(polar_inputs) + i
        list(
          var          = non_polar_inputs_table$var[i],
          q_label      = clean_variable_names[clean_idx],
          text         = non_polar_inputs_table$label[i],
          value_table  = non_polar_inputs_table$value_table[[i]],
          n_values     = nv_count
        )
      }
    )
  }else{
    non_polar_inputs_info <- NULL
  }


  #######################
  # internal functions
  #######################

  ## creating individual ui

  individual_ui_tool <- function(
    wb = wb,
    sheet_name = sheet_name,
    row_title = start_row,
    start_col = start_col,
    polar_label_width = polar_label_width
  ){

    ind_ui_response_instructions <- "Please read each pair of statements and decide which side you agree with more. Indicate your response with an x"

    #######################
    # create objects
    #######################

    if(has_recoding){
      recode_values <- recode_mapping[[2]] %>% as.numeric() %>% matrix(nrow = 1)
    }else{
      recode_values <- polar_points %>% matrix(nrow = 1)
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
    # add a spacer column between Side A and the polar point cols only when
    # non-polar blocks need a hidden Code column at that position
    polar_spacer_offset <- if(!is.null(non_polar_inputs_info)) 2L else 1L
    col_ind_label_point_first_number <- col_ind_label_left_number + polar_spacer_offset
    col_ind_label_point_last_number <- col_ind_label_point_first_number + length(polar_points) - 1
    col_ind_label_right_number <- col_ind_label_point_last_number + 1

    col_ind_engine_clean_var_number <- col_ind_label_right_number + 2
    col_ind_engine_survey_var_number <- col_ind_engine_clean_var_number + 1
    col_ind_engine_answer_number <- col_ind_engine_survey_var_number + ncol(coef_func)
    col_ind_engine_recode_number <- col_ind_engine_answer_number + 1
    col_ind_engine_qc_number <- col_ind_engine_recode_number + 1

    col_ind_engine_controls_number <- col_ind_engine_clean_var_number + 4

    range_ind_prob <- glue::glue('${num2let(col_ind_engine_survey_var_number + 1)}${row_ind_engine_prob}:${num2let(col_ind_engine_answer_number - 1)}${row_ind_engine_prob}')


    #######################
    # write indi ui
    #######################

    openxlsx::writeData(
      wb, sheet_name,
      x = "Individual Typing Tool",
      startRow = row_title,
      startCol = col_ind_clean_var_number,
      colNames = FALSE
    )


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_title,
      rows = row_title,
      cols = col_ind_clean_var_number,
      stack = TRUE
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = "Response Recode",
      startRow = row_ind_recode,
      startCol = col_ind_label_point_first_number - 1,
      colNames = FALSE
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = recode_values,
      startRow = row_ind_recode,
      startCol = col_ind_label_point_first_number,
      colNames = FALSE
    )


    range_recode <- glue::glue('${num2let(col_ind_label_point_first_number)}${row_ind_recode}:${num2let(col_ind_label_point_first_number + ncol(recode_values) - 1)}${row_ind_recode}')


    openxlsx::addStyle(
      wb, sheet_name,
      style = oxl_style_cell_neutral(textDecoration = "Bold"),
      rows = row_ind_recode,
      cols = seq(col_ind_label_point_first_number - 1, col_ind_label_point_last_number),
      gridExpand = TRUE
    )


    openxlsx::setColWidths(wb, sheet_name, cols = seq(col_ind_clean_var_number, col_ind_label_point_last_number), widths = 10)
    openxlsx::setColWidths(wb, sheet_name, cols = col_ind_survey_var_number, hidden = TRUE)
    openxlsx::groupRows(wb, sheet_name, rows = row_ind_recode, hidden = TRUE)

    openxlsx::setColWidths(wb, sheet_name, cols = c(col_ind_label_left_number, col_ind_label_right_number), widths = polar_label_width)

    openxlsx::writeData(
      wb, sheet_name,
      x = ind_ui,
      startRow = row_header,
      startCol = col_ind_clean_var_number,
      colNames = TRUE,
      headerStyle = style_header,
      borders = "all",
    )

    openxlsx::writeData(
      wb, sheet_name,
      x = ind_response,
      startRow = row_ind_first,
      startCol = col_ind_label_point_first_number,
      colNames = FALSE
    )


    # pull Survey Variable, Side A, Side B from Documentation sheet
    doc_q_row_qualification_last <- row_title + 6 + length(qualification_instructions)
    doc_q_row_seg_name_last      <- row_title + 6 + length(segments)
    doc_q_row_questions_header   <- max(doc_q_row_qualification_last, doc_q_row_seg_name_last) + 5
    doc_q_row_first              <- doc_q_row_questions_header + doc_row_gap + 1

    doc_q_rows <- seq(doc_q_row_first, doc_q_row_first + nrow(ind_ui) - 1)
    doc_col_sv <- num2let(start_col + 1)
    doc_col_sa <- num2let(start_col + 2)
    doc_col_sb <- num2let(start_col + 3 + length(polar_points) + doc_label_cell_merge)

    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue("'{sheet_name_doc}'!{doc_col_sv}{doc_q_rows}"),
      startRow = row_ind_first,
      startCol = col_ind_survey_var_number
    )

    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue("'{sheet_name_doc}'!{doc_col_sa}{doc_q_rows}"),
      startRow = row_ind_first,
      startCol = col_ind_label_left_number
    )

    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue("'{sheet_name_doc}'!{doc_col_sb}{doc_q_rows}"),
      startRow = row_ind_first,
      startCol = col_ind_label_right_number
    )


    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(fontSize = 8),
      rows = row_header,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number),
      gridExpand = TRUE, stack = TRUE
    )


    row_polar_last <- if(!is.null(ind_ui)) row_ind_first + nrow(ind_ui) - 1 else row_ind_last

    openxlsx::addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_ind_first, row_polar_last),
      cols = seq(col_ind_clean_var_number, col_ind_label_right_number),
      gridExpand = TRUE, stack = TRUE
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_header,
      row_end = row_polar_last,
      col_start = col_ind_clean_var_number,
      col_end = col_ind_label_right_number
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_ind_first,
      row_end = row_polar_last,
      col_start = col_ind_clean_var_number,
      col_end = col_ind_label_right_number
    )


    openxlsx::mergeCells(
      wb, sheet_name,
      rows = row_instructions,
      cols = seq(col_ind_label_left_number, col_ind_label_right_number)
    )


    oxl_outer_box(
      wb, sheet_name,
      borderStyle = "medium",
      row_start = row_instructions,
      row_end = row_instructions,
      col_start = col_ind_clean_var_number,
      col_end = col_ind_label_right_number
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = ind_ui_response_instructions,
      startRow = row_instructions,
      startCol = col_ind_label_left_number,
      colNames = FALSE
    )


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_header,
      rows = row_instructions,
      cols = seq(col_ind_clean_var_number, col_ind_label_right_number),
      gridExpand = TRUE, stack = TRUE
    )


    #######################
    # non-polar LDA input blocks (below polar block)
    #######################

    np_block_bottom <- row_polar_last
    np_info_with_cells <- NULL

    if(!is.null(non_polar_inputs_info)){
      np_info_with_cells <- vector("list", length(non_polar_inputs_info))

      # column layout (maps to same sheet columns as polar block):
      #   B = Question, C = Survey Variable (hidden), D = Question Text,
      #   E = Code (hidden), F = Response, col_ind_label_right_number = Answer
      col_np_q        <- col_ind_clean_var_number
      col_np_var      <- col_ind_survey_var_number
      col_np_text     <- col_ind_label_left_number
      col_np_code     <- col_ind_label_left_number + 1
      col_np_response <- col_ind_label_point_first_number
      col_np_answer   <- col_ind_label_point_last_number

      style_np_cell <- openxlsx::createStyle(
        fgFill = "white", halign = "center", valign = "center",
        border = "TopBottomLeftRight", borderStyle = "thin"
      )
      style_np_text <- openxlsx::createStyle(
        fgFill = "white", halign = "center", valign = "center", wrapText = TRUE,
        border = "TopBottomLeftRight", borderStyle = "thin"
      )

      row_np_cursor <- row_polar_last + 3

      for(idx in seq_along(non_polar_inputs_info)){
        info        <- non_polar_inputs_info[[idx]]
        n_vals      <- info$n_values
        row_np_head <- row_np_cursor
        row_np_first <- row_np_head + 1
        row_np_last  <- row_np_head + n_vals

        # header row (Question Text header intentionally blank)
        header_tbl <- tibble::tibble(
          "Question"         = NA_character_,
          "Survey Variable"  = NA_character_,
          " "                = NA_character_,
          "Code"             = NA_character_,
          "Response"         = NA_character_,
          "Answer"           = NA_character_
        )

        openxlsx::writeData(
          wb, sheet_name,
          x = header_tbl[0, ],
          startRow = row_np_head,
          startCol = col_np_q,
          colNames = TRUE,
          headerStyle = style_header,
          borders = "all"
        )

        # Q label, survey var, question text — first value row
        openxlsx::writeData(wb, sheet_name, x = info$q_label,  startRow = row_np_first, startCol = col_np_q,    colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = info$var,      startRow = row_np_first, startCol = col_np_var,  colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = info$text,     startRow = row_np_first, startCol = col_np_text, colNames = FALSE)

        # merge Q, var, text vertically across all value rows
        if(n_vals > 1){
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_np_first, row_np_last), cols = col_np_q)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_np_first, row_np_last), cols = col_np_var)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_np_first, row_np_last), cols = col_np_text)
        }

        # determine first respondent's answer and mark x
        first_val <- seg[["data"]][["with_solutions"]][[info$var]][1]
        x_row_idx <- which(info$value_table$code == first_val)

        # write code + response per value row, and "x" if this row matches first respondent
        for(v in seq_len(n_vals)){
          r <- row_np_first + v - 1
          openxlsx::writeData(wb, sheet_name, x = info$value_table$code[v],  startRow = r, startCol = col_np_code,     colNames = FALSE)
          openxlsx::writeData(wb, sheet_name, x = info$value_table$label[v], startRow = r, startCol = col_np_response, colNames = FALSE)
          if(length(x_row_idx) > 0 && v == x_row_idx[1]){
            openxlsx::writeData(wb, sheet_name, x = "x", startRow = r, startCol = col_np_answer, colNames = FALSE)
          }
        }

        # white background for the question header columns
        openxlsx::addStyle(
          wb, sheet_name,
          style = style_np_cell,
          rows = seq(row_np_first, row_np_last),
          cols = seq(col_np_q, col_np_text),
          gridExpand = TRUE, stack = TRUE
        )

        # Code column — neutral (orange/yellow) highlight
        openxlsx::addStyle(
          wb, sheet_name,
          style = oxl_style_cell_neutral(textDecoration = "Bold", border = "TopBottomLeftRight"),
          rows = seq(row_np_first, row_np_last),
          cols = col_np_code,
          gridExpand = TRUE, stack = TRUE
        )

        # Response column — light gray with thin borders
        openxlsx::addStyle(
          wb, sheet_name,
          style = openxlsx::createStyle(
            fgFill = oxl_colorscale_grey(2),
            halign = "center", valign = "center",
            border = "TopBottomLeftRight",
            borderStyle = "thin"
          ),
          rows = seq(row_np_first, row_np_last),
          cols = col_np_response,
          gridExpand = TRUE, stack = TRUE
        )

        # Answer column — white with thin borders (matches polar block inner borders)
        openxlsx::addStyle(
          wb, sheet_name,
          style = openxlsx::createStyle(
            fgFill = "white",
            halign = "center", valign = "center",
            border = "TopBottomLeftRight",
            borderStyle = "thin"
          ),
          rows = seq(row_np_first, row_np_last),
          cols = col_np_answer,
          gridExpand = TRUE, stack = TRUE
        )

        oxl_outer_box(
          wb, sheet_name, borderStyle = "medium",
          row_start = row_np_head, row_end = row_np_last,
          col_start = col_np_q, col_end = col_np_answer
        )

        # second outer box around data rows only — medium line below the header
        oxl_outer_box(
          wb, sheet_name, borderStyle = "medium",
          row_start = row_np_first, row_end = row_np_last,
          col_start = col_np_q, col_end = col_np_answer
        )

        # multi-x warning (yellow) — same logic as polar response highlight
        np_answer_abs_range <- glue::glue("${num2let(col_np_answer)}${row_np_first}:${num2let(col_np_answer)}${row_np_last}")
        openxlsx::conditionalFormatting(
          wb, sheet_name,
          cols = col_np_answer,
          rows = seq(row_np_first, row_np_last),
          rule = glue::glue('AND(${num2let(col_ind_engine_controls_number)}${row_ind_engine_seg_qc} = TRUE, COUNTIF({np_answer_abs_range}, "x") > 1)'),
          style = oxl_style_cell_neutral(textDecoration = "bold", conditional = TRUE)
        )

        # hide Code column (col E)
        openxlsx::setColWidths(wb, sheet_name, cols = col_np_code, hidden = TRUE)

        np_info_with_cells[[idx]] <- c(info, list(
          row_first    = row_np_first,
          row_last     = row_np_last,
          col_code     = col_np_code,
          col_answer   = col_np_answer,
          answer_range = glue::glue("{num2let(col_np_answer)}{row_np_first}:{num2let(col_np_answer)}{row_np_last}"),
          code_range   = glue::glue("{num2let(col_np_code)}{row_np_first}:{num2let(col_np_code)}{row_np_last}")
        ))

        row_np_cursor <- row_np_last + 2
      }

      np_block_bottom <- row_np_cursor - 2
    }


    #######################
    # additional_questions blocks (post-LDA reassignment vars not in LDA inputs)
    #######################

    aq_info_with_cells <- NULL

    if(!is.null(additional_questions_info)){
      aq_info_with_cells <- vector("list", length(additional_questions_info))

      col_aq_q        <- col_ind_clean_var_number
      col_aq_var      <- col_ind_survey_var_number
      col_aq_text     <- col_ind_label_left_number
      col_aq_code     <- col_ind_label_left_number + 1
      col_aq_response <- col_ind_label_point_first_number
      col_aq_answer   <- col_ind_label_point_last_number

      style_aq_cell <- openxlsx::createStyle(
        fgFill = "white", halign = "center", valign = "center",
        border = "TopBottomLeftRight", borderStyle = "thin"
      )

      row_aq_cursor <- np_block_bottom + 3

      for(idx in seq_along(additional_questions_info)){
        info <- additional_questions_info[[idx]]
        n_vals <- nrow(info$value_table)
        row_aq_h <- row_aq_cursor
        row_aq_f <- row_aq_h + 1
        row_aq_l <- row_aq_h + n_vals

        # header row
        header_tbl_aq <- tibble::tibble(
          "Question"         = NA_character_,
          "Survey Variable"  = NA_character_,
          " "                = NA_character_,
          "Code"             = NA_character_,
          "Response"         = NA_character_,
          "Answer"           = NA_character_
        )

        openxlsx::writeData(
          wb, sheet_name,
          x = header_tbl_aq[0, ],
          startRow = row_aq_h,
          startCol = col_aq_q,
          colNames = TRUE,
          headerStyle = style_header,
          borders = "all"
        )

        # Q label, survey var, question text — first value row
        q_label_aq <- glue::glue("AQ{idx}")
        openxlsx::writeData(wb, sheet_name, x = q_label_aq,  startRow = row_aq_f, startCol = col_aq_q,    colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = info$var,    startRow = row_aq_f, startCol = col_aq_var,  colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = info$label,  startRow = row_aq_f, startCol = col_aq_text, colNames = FALSE)

        if(n_vals > 1){
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_aq_f, row_aq_l), cols = col_aq_q)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_aq_f, row_aq_l), cols = col_aq_var)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_aq_f, row_aq_l), cols = col_aq_text)
        }

        first_val_aq <- seg[["data"]][["with_solutions"]][[info$var]][1]
        x_row_idx_aq <- which(info$value_table$code == first_val_aq)

        for(v in seq_len(n_vals)){
          r <- row_aq_f + v - 1
          openxlsx::writeData(wb, sheet_name, x = info$value_table$code[v],  startRow = r, startCol = col_aq_code,     colNames = FALSE)
          openxlsx::writeData(wb, sheet_name, x = info$value_table$label[v], startRow = r, startCol = col_aq_response, colNames = FALSE)
          if(length(x_row_idx_aq) > 0 && v == x_row_idx_aq[1]){
            openxlsx::writeData(wb, sheet_name, x = "x", startRow = r, startCol = col_aq_answer, colNames = FALSE)
          }
        }

        # styles per column (matches non-polar block)
        openxlsx::addStyle(wb, sheet_name, style = style_aq_cell,
          rows = seq(row_aq_f, row_aq_l),
          cols = seq(col_aq_q, col_aq_text),
          gridExpand = TRUE, stack = TRUE
        )

        openxlsx::addStyle(wb, sheet_name,
          style = oxl_style_cell_neutral(textDecoration = "Bold", border = "TopBottomLeftRight"),
          rows = seq(row_aq_f, row_aq_l),
          cols = col_aq_code,
          gridExpand = TRUE, stack = TRUE
        )

        openxlsx::addStyle(wb, sheet_name,
          style = openxlsx::createStyle(
            fgFill = oxl_colorscale_grey(2),
            halign = "center", valign = "center",
            border = "TopBottomLeftRight", borderStyle = "thin"
          ),
          rows = seq(row_aq_f, row_aq_l),
          cols = col_aq_response,
          gridExpand = TRUE, stack = TRUE
        )

        openxlsx::addStyle(wb, sheet_name,
          style = openxlsx::createStyle(
            fgFill = "white",
            halign = "center", valign = "center",
            border = "TopBottomLeftRight", borderStyle = "thin"
          ),
          rows = seq(row_aq_f, row_aq_l),
          cols = col_aq_answer,
          gridExpand = TRUE, stack = TRUE
        )

        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_aq_h, row_end = row_aq_l,
          col_start = col_aq_q, col_end = col_aq_answer
        )

        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_aq_f, row_end = row_aq_l,
          col_start = col_aq_q, col_end = col_aq_answer
        )

        # multi-x warning (yellow)
        aq_answer_abs <- glue::glue("${num2let(col_aq_answer)}${row_aq_f}:${num2let(col_aq_answer)}${row_aq_l}")
        openxlsx::conditionalFormatting(wb, sheet_name,
          cols = col_aq_answer,
          rows = seq(row_aq_f, row_aq_l),
          rule = glue::glue('AND(${num2let(col_ind_engine_controls_number)}${row_ind_engine_seg_qc} = TRUE, COUNTIF({aq_answer_abs}, "x") > 1)'),
          style = oxl_style_cell_neutral(textDecoration = "bold", conditional = TRUE)
        )

        openxlsx::setColWidths(wb, sheet_name, cols = col_aq_code, hidden = TRUE)

        aq_info_with_cells[[idx]] <- c(info, list(
          row_first    = row_aq_f,
          row_last     = row_aq_l,
          col_code     = col_aq_code,
          col_answer   = col_aq_answer,
          answer_range = glue::glue("{num2let(col_aq_answer)}{row_aq_f}:{num2let(col_aq_answer)}{row_aq_l}"),
          code_range   = glue::glue("{num2let(col_aq_code)}{row_aq_f}:{num2let(col_aq_code)}{row_aq_l}"),
          answer_abs   = glue::glue("${num2let(col_aq_answer)}${row_aq_f}:${num2let(col_aq_answer)}${row_aq_l}"),
          code_abs     = glue::glue("${num2let(col_aq_code)}${row_aq_f}:${num2let(col_aq_code)}${row_aq_l}")
        ))

        row_aq_cursor <- row_aq_l + 2
      }
    }


    openxlsx::mergeCells(
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


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_header,
      rows = row_ind_score,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number),
      gridExpand = TRUE, stack = TRUE
    )


    temp_col_result <- num2let(col_ind_engine_survey_var_number)
    temp_col_control <- num2let(col_ind_engine_controls_number)
    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue(
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

    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number),
      rows = row_ind_score,
      rule = glue::glue('{temp_col_result}{row_ind_engine_seg_qc} = TRUE'),
      style = oxl_style_cell_good(textDecoration = "bold", conditional = TRUE)
    )

    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number),
      rows = row_ind_score,
      rule = glue::glue('{temp_col_result}{row_ind_engine_seg_qc} = FALSE'),
      style = oxl_style_cell_neutral(textDecoration = "bold", conditional = TRUE)
    )


    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_ind_label_point_first_number, col_ind_label_point_last_number),
      rows = seq(row_ind_first, row_polar_last),
      rule = glue::glue('AND(${num2let(col_ind_engine_controls_number)}${row_ind_engine_seg_qc} = TRUE, COUNTIF(${num2let(col_ind_label_point_first_number)}{row_ind_first}:${num2let(col_ind_label_point_last_number)}{row_ind_first}, "x") > 1)'),
      style = oxl_style_cell_neutral(textDecoration = "bold", conditional = TRUE)
    )


    #######################
    # write indi engine
    #######################

    openxlsx::writeData(
      wb, sheet_name,
      x = "Individual Typing Tool - Engine",
      startRow = row_title,
      startCol = col_ind_engine_clean_var_number,
      colNames = FALSE
    )


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_title,
      rows = row_title,
      cols = col_ind_engine_clean_var_number,
      stack = TRUE
    )


    for(i in c(row_header, row_ind_engine_header)){

      temp_coef_func <- tibble::tibble(
        "Question" = c(clean_variable_names, NA)
      ) %>%
        dplyr::bind_cols(coef_func)

      temp_coef_func[nrow(temp_coef_func), "Variable"] <- "Constant"


      if(i == row_header){

        temp_coef_func <- temp_coef_func %>%
          dplyr::mutate(
            "Answer" = NA,
            "Recode" = NA,
            "QC Check" = NA
          )

        temp_cols <- seq(col_ind_engine_clean_var_number, col_ind_engine_qc_number)

        temp_row_header <- seq(i - 2, i - 1)

        temp_header_text <- "Solution Coefficient Function"

        openxlsx::mergeCells(
          wb, sheet_name,
          rows = temp_row_header,
          cols = seq(col_ind_engine_answer_number, col_ind_engine_qc_number)
        )


        openxlsx::writeData(
          wb, sheet_name,
          x = "Response Processing",
          startRow = temp_row_header %>% head(1),
          startCol = col_ind_engine_answer_number,
          colNames = FALSE
        )


        openxlsx::addStyle(
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
          dplyr::mutate_if(
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


      openxlsx::writeData(
        wb, sheet_name,
        x = temp_coef_func,
        startRow = i,
        startCol = col_ind_engine_clean_var_number,
        headerStyle = style_header,
        borders = "all",
        colNames = TRUE
      )


      openxlsx::addStyle(
        wb, sheet_name,
        style = style_table,
        rows = seq(i, i + nrow(temp_coef_func)),
        cols = temp_cols,
        gridExpand = TRUE, stack = TRUE
      )


      openxlsx::mergeCells(
        wb, sheet_name,
        rows = temp_row_header,
        cols = seq(col_ind_engine_clean_var_number, col_ind_engine_answer_number - 1)      )


      openxlsx::writeData(
        wb, sheet_name,
        x = temp_header_text,
        startRow = temp_row_header %>% head(1),
        startCol = col_ind_engine_clean_var_number,
        colNames = FALSE
      )


      openxlsx::addStyle(
        wb, sheet_name,
        style = style_header2,
        rows = temp_row_header,
        cols = seq(col_ind_engine_clean_var_number, col_ind_engine_answer_number - 1),
        gridExpand = TRUE, stack = TRUE
      )


      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = temp_row_header %>% head(1),
        row_end = temp_row_header %>% tail(1),
        col_start = col_ind_engine_clean_var_number,
        col_end = col_ind_engine_answer_number - 1      )


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


    # pull coefficient function Variable and Seg_* from Documentation sheet
    np_doc_extra_rows <- 0
    if(!is.null(non_polar_inputs_info) && length(non_polar_inputs_info) > 0){
      N_np_ui          <- length(non_polar_inputs_info)
      total_vals_np_ui <- sum(purrr::map_int(non_polar_inputs_info, ~ as.integer(.x$n_values)))
      np_doc_extra_rows <- total_vals_np_ui + 2 * N_np_ui + 1
    }
    doc_coef_row_header <- doc_q_row_first + nrow(inputs_table) - 1 + np_doc_extra_rows + 4
    doc_coef_data_rows  <- seq(doc_coef_row_header + 1, doc_coef_row_header + nrow(coef_func))

    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue("'{sheet_name_doc}'!{num2let(start_col + 1)}{doc_coef_data_rows}"),
      startRow = row_header + 1,
      startCol = col_ind_engine_clean_var_number + 1
    )

    for(s in seq_len(length(segments))){
      openxlsx::writeFormula(
        wb, sheet_name,
        x = glue::glue("'{sheet_name_doc}'!{num2let(start_col + 1 + s)}{doc_coef_data_rows}"),
        startRow = row_header + 1,
        startCol = col_ind_engine_clean_var_number + 1 + s
      )
    }


    for(i in seq(row_ind_first, row_ind_last)){

      input_idx <- i - row_ind_first + 1
      is_polar  <- input_idx <= length(polar_inputs)

      if(is_polar){
        answer_formula <- glue::glue('MATCH("x", {num2let(col_ind_label_point_first_number)}{i}:{num2let(col_ind_label_point_last_number)}{i}, 0)')
        recode_formula <- glue::glue('INDEX(${num2let(col_ind_label_point_first_number)}${row_ind_recode}:${num2let(col_ind_label_point_last_number)}${row_ind_recode},,{num2let(col_ind_engine_answer_number)}{i})')
        qc_formula     <- glue::glue('COUNTIF({num2let(col_ind_label_point_first_number)}{i}:{num2let(col_ind_label_point_last_number)}{i},"x")')
        recode_array   <- TRUE
      }else{
        np <- np_info_with_cells[[input_idx - length(polar_inputs)]]
        answer_formula <- glue::glue('MATCH("x", {np$answer_range}, 0)')
        recode_formula <- glue::glue('INDEX({np$code_range}, {num2let(col_ind_engine_answer_number)}{i}, 1)')
        qc_formula     <- glue::glue('COUNTIF({np$answer_range},"x")')
        recode_array   <- FALSE
      }

      openxlsx::writeFormula(
        wb, sheet_name,
        x = answer_formula,
        startRow = i,
        startCol = col_ind_engine_answer_number,
      )

      openxlsx::writeFormula(
        wb, sheet_name,
        x = recode_formula,
        startRow = i,
        startCol = col_ind_engine_recode_number,
        array = recode_array
      )

      openxlsx::writeFormula(
        wb, sheet_name,
        x = qc_formula,
        startRow = i,
        startCol = col_ind_engine_qc_number
      )

      openxlsx::conditionalFormatting(
        wb, sheet_name,
        rows = i,
        cols = col_ind_engine_qc_number,
        rule = "== 1",
        style = oxl_style_cell_good(conditional = TRUE)
      )

      openxlsx::conditionalFormatting(
        wb, sheet_name,
        rows = i,
        cols = col_ind_engine_qc_number,
        rule = "!= 1",
        style = oxl_style_cell_bad(conditional = TRUE)
      )

      if(i == row_ind_last){
        openxlsx::writeData(
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
        openxlsx::writeFormula(
          wb, sheet_name,
          x = glue::glue('{num2let(x)}{i - row_eng_coef_diff} * ${num2let(col_ind_engine_recode_number)}{i - row_eng_coef_diff}'),
          startRow = i,
          startCol = x,
        )

        if(i == row_ind_engine_last){
          openxlsx::writeData(
            wb, sheet_name,
            x = c("Score", "Probability") %>% matrix(ncol = 1),
            startRow = i + 1,
            startCol = col_ind_engine_survey_var_number,
            colNames = FALSE
          )

          openxlsx::writeFormula(
            wb, sheet_name,
            x = glue::glue('EXP(SUM({num2let(x)}${row_ind_engine_first}:{num2let(x)}${row_ind_engine_last}))'),
            startRow = i + 1,
            startCol = x,
          )

          # adjusted probability with optional additional_logic redistribution
          seg_idx_ui <- x - col_ind_engine_survey_var_number
          seg_num_ui <- as.integer(segments[seg_idx_ui])

          score_cell_ui <- glue::glue("{num2let(x)}{i+1}")
          score_range_ui <- glue::glue("${num2let(col_ind_engine_survey_var_number + 1)}${i+1}:${num2let(col_ind_engine_answer_number - 1)}${i+1}")
          base_prob_ui  <- glue::glue("{score_cell_ui} / SUM({score_range_ui})")

          from_k_conds_ui <- character(0)
          added_parts_ui  <- character(0)

          # additional_logic — variables are LDA inputs, value cell is engine Answer/Recode
          if(!is.null(additional_logic_info)){
            for(var_name in names(additional_logic_info)){
              rules <- additional_logic_info[[var_name]]
              input_idx_v <- match(var_name, inputs)
              is_polar_v  <- input_idx_v <= length(polar_inputs)
              engine_row_v <- row_ind_first + input_idx_v - 1
              cell_v <- if(is_polar_v){
                glue::glue("{num2let(col_ind_engine_answer_number)}{engine_row_v}")
              }else{
                glue::glue("{num2let(col_ind_engine_recode_number)}{engine_row_v}")
              }

              from_subset <- rules %>% dplyr::filter(from_seg == seg_num_ui)
              for(j in seq_len(nrow(from_subset))){
                from_k_conds_ui <- c(from_k_conds_ui, glue::glue("{cell_v}={from_subset$value[j]}"))
              }

              to_subset <- rules %>% dplyr::filter(to_seg == seg_num_ui)
              for(j in seq_len(nrow(to_subset))){
                from_idx <- match(to_subset$from_seg[j], as.integer(segments))
                if(!is.na(from_idx)){
                  from_x <- col_ind_engine_survey_var_number + from_idx
                  from_score_cell <- glue::glue("{num2let(from_x)}{i+1}")
                  base_prob_from <- glue::glue("{from_score_cell} / SUM({score_range_ui})")
                  added_parts_ui <- c(added_parts_ui, glue::glue("IF({cell_v}={to_subset$value[j]}, {base_prob_from}, 0)"))
                }
              }
            }
          }

          # additional_questions — variables aren't LDA inputs; resolve value via INDEX/MATCH on AQ block
          if(!is.null(aq_info_with_cells)){
            for(aq_entry in aq_info_with_cells){
              cell_v_aq <- glue::glue('INDEX({aq_entry$code_abs}, MATCH("x", {aq_entry$answer_abs}, 0))')

              from_subset_aq <- aq_entry$rules %>% dplyr::filter(from_seg == seg_num_ui)
              for(j in seq_len(nrow(from_subset_aq))){
                from_k_conds_ui <- c(from_k_conds_ui, glue::glue("{cell_v_aq}={from_subset_aq$value[j]}"))
              }

              to_subset_aq <- aq_entry$rules %>% dplyr::filter(to_seg == seg_num_ui)
              for(j in seq_len(nrow(to_subset_aq))){
                from_idx <- match(to_subset_aq$from_seg[j], as.integer(segments))
                if(!is.na(from_idx)){
                  from_x <- col_ind_engine_survey_var_number + from_idx
                  from_score_cell <- glue::glue("{num2let(from_x)}{i+1}")
                  base_prob_from <- glue::glue("{from_score_cell} / SUM({score_range_ui})")
                  added_parts_ui <- c(added_parts_ui, glue::glue("IF({cell_v_aq}={to_subset_aq$value[j]}, {base_prob_from}, 0)"))
                }
              }
            }
          }

          zeroed_part_ui <- if(length(from_k_conds_ui) > 0){
            glue::glue("IF(OR({paste(from_k_conds_ui, collapse = ', ')}), 0, {base_prob_ui})")
          }else{
            base_prob_ui
          }

          ui_prob_formula <- if(length(added_parts_ui) > 0){
            paste(c(zeroed_part_ui, added_parts_ui), collapse = " + ")
          }else{
            zeroed_part_ui
          }

          openxlsx::writeFormula(
            wb, sheet_name,
            x = ui_prob_formula,
            startRow = row_ind_engine_prob,
            startCol = x,
          )
        }
      }
    }



    openxlsx::addStyle(
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
        openxlsx::mergeCells(
          wb, sheet_name,
          rows = seq(i, i + 1),
          cols = xc - 1
        )
        openxlsx::mergeCells(
          wb, sheet_name,
          rows = seq(i, i + 1),
          cols = xc
        )
      }
    }


    openxlsx::writeData(
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

    openxlsx::writeData(
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
      openxlsx::writeData(
        wb, sheet_name,
        x = 1,
        startRow = i,
        startCol = col_ind_engine_controls_number,
        colNames = FALSE
      )
    }


    for(xc in c(col_ind_engine_clean_var_number, col_ind_engine_controls_number - 1)){

      openxlsx::mergeCells(
        wb, sheet_name,
        rows = row_ind_engine_seg - 1,
        cols = seq(xc, xc + 1)
      )


      openxlsx::addStyle(
        wb, sheet_name,
        style = style_header,
        rows = seq(row_ind_engine_seg, row_ind_engine_seg_qc + 1),
        cols = xc,
        gridExpand = TRUE, stack = TRUE
      )


      openxlsx::addStyle(
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


    # Plain MATCH — additional_logic redistributes probabilities upstream
    # (in the row_ind_engine_prob formulas), so MAX over the adjusted probs
    # naturally lands on the correct (post-redistribution) segment.
    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue('MATCH(MAX({range_ind_prob}), {range_ind_prob}, 0)'),
      startRow = row_ind_engine_seg,
      startCol = col_ind_engine_survey_var_number,
    )


    doc_col_seg_name_value <- start_col + doc_label_cell_merge + 5
    doc_row_seg_name_first <- row_title + 7
    doc_row_seg_name_last  <- row_title + 6 + length(segments)
    range_seg_name <- glue::glue("'{sheet_name_doc}'!${num2let(doc_col_seg_name_value)}${doc_row_seg_name_first}:${num2let(doc_col_seg_name_value)}${doc_row_seg_name_last}")

    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue("INDEX({range_seg_name}, {num2let(col_ind_engine_survey_var_number)}{row_ind_engine_seg}, 1)"),
      startRow = row_ind_engine_seg_name,
      startCol = col_ind_engine_survey_var_number,
    )


    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue('MAX({range_ind_prob})'),
      startRow = row_ind_engine_seg_prob,
      startCol = col_ind_engine_survey_var_number,
    )


    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue('=COUNTIF({num2let(col_ind_engine_qc_number)}{row_ind_first}:{num2let(col_ind_engine_qc_number)}{row_ind_last},1) = {length(clean_variable_names)}'),
      startRow = row_ind_engine_seg_qc,
      startCol = col_ind_engine_survey_var_number,
    )


    openxlsx::conditionalFormatting(
      wb, sheet_name,
      rows = seq(row_ind_engine_seg_qc, row_ind_engine_seg_qc + 1),
      cols = col_ind_engine_survey_var_number,
      rule = "== TRUE",
      style = oxl_style_cell_good(conditional = TRUE)
    )


    openxlsx::conditionalFormatting(
      wb, sheet_name,
      rows = seq(row_ind_engine_seg_qc, row_ind_engine_seg_qc + 1),
      cols = col_ind_engine_survey_var_number,
      rule = "== FALSE",
      style = oxl_style_cell_bad(conditional = TRUE)
    )


    openxlsx::addStyle(
      wb, sheet_name,
      style = oxl_style_percent(2, valign = "center"),
      rows = row_ind_engine_seg_prob,
      cols = col_ind_engine_survey_var_number,
      stack = TRUE
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = matrix(c("1 = Segment number", "2 = Segment name", "3 = Both"), nrow = 1),
      startRow = row_ind_control_feedback_style,
      startCol = col_ind_engine_controls_number + 1,
      colNames = FALSE
    )


    #######################
    # final column group
    #######################

    openxlsx::groupColumns(
      wb, sheet_name,
      cols = seq(col_ind_engine_clean_var_number, col_ind_engine_qc_number),
      hidden = TRUE
    )


    #######################
    # return
    #######################

    return(
      list(
        "range_recode" = paste0("'", sheet_name, "'!", range_recode)
      )
    )
  }




  documentation_tool <- function(
    wb = wb,
    sheet_name = sheet_name,
    row_title = start_row,
    start_col = start_col,
    polar_label_width = polar_label_width
  ){

    ind_doc_response_instructions <- "Please read each pair of statements and decide which side you agree with more."


    #######################
    # reference constants
    #######################

    col_clean_var     <- start_col
    col_survey_var    <- col_clean_var + 1
    col_label_left    <- col_survey_var + 1
    col_label_point_first <- col_label_left + 1
    col_label_point_last  <- col_label_point_first + length(polar_points) - 1
    col_label_right   <- col_label_point_last + 1

    col_last <- col_label_right + (doc_label_cell_merge * 2)

    col_qualification_last <- col_clean_var + doc_label_cell_merge + 2

    col_seg_name_header <- col_qualification_last + 2
    col_seg_name_value  <- col_seg_name_header + 1


    row_doc_intro            <- row_title + 3
    row_doc_qualification    <- row_doc_intro + 3
    row_doc_qualification_last <- row_doc_qualification + length(qualification_instructions)

    row_doc_seg_name_header  <- row_doc_qualification
    row_doc_seg_name_first   <- row_doc_seg_name_header + 1
    row_doc_seg_name_last    <- row_doc_seg_name_header + length(segments)

    if(row_doc_qualification_last >= row_doc_seg_name_last){
      row_doc_questions_header <- row_doc_qualification_last + 5
    }else{
      row_doc_questions_header <- row_doc_seg_name_last + 5
    }

    row_doc_header_top       <- row_doc_questions_header + doc_row_gap
    row_doc_first            <- row_doc_header_top + 1
    row_doc_last             <- if(!is.null(inputs_table)) row_doc_first + nrow(inputs_table) - 1 else row_doc_header_top

    np_doc_total_rows <- 0
    if(!is.null(non_polar_inputs_info) && length(non_polar_inputs_info) > 0){
      N_np          <- length(non_polar_inputs_info)
      total_vals_np <- sum(purrr::map_int(non_polar_inputs_info, ~ as.integer(.x$n_values)))
      np_doc_total_rows <- total_vals_np + 2 * N_np + 1
    }

    aq_doc_total_rows <- 0
    if(!is.null(additional_questions_info) && length(additional_questions_info) > 0){
      N_aq           <- length(additional_questions_info)
      total_vals_aq  <- sum(purrr::map_int(additional_questions_info, ~ as.integer(nrow(.x$value_table))))
      aq_doc_total_rows <- total_vals_aq + 2 * N_aq + 1
    }

    row_doc_function_header  <- row_doc_last + np_doc_total_rows + aq_doc_total_rows + 4
    row_doc_function_last    <- row_doc_function_header + nrow(coef_func) + 1

    row_doc_steps            <- row_doc_function_last + 2


    #######################
    # column widths
    #######################

    openxlsx::setColWidths(
      wb, sheet_name,
      cols = seq(col_clean_var, col_last),
      widths = 10
    )
    openxlsx::setColWidths(wb, sheet_name, cols = col_survey_var, hidden = TRUE)


    #######################
    # title + intro
    #######################

    openxlsx::writeData(
      wb, sheet_name,
      x = "Typing Tool Documentation",
      startRow = row_title,
      startCol = col_clean_var,
      colNames = FALSE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = style_title,
      rows = row_title,
      cols = col_clean_var,
      stack = TRUE
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = glue::glue("These instructions create the {xfun::numbers_to_words(length(segments))} segment solution from {xfun::numbers_to_words(length(clean_variable_names))} items"),
      startRow = row_doc_intro,
      startCol = col_clean_var,
      colNames = FALSE
    )


    #######################
    # qualifications
    #######################

    for(xr in seq(row_doc_qualification, row_doc_qualification_last)){
      openxlsx::mergeCells(
        wb, sheet_name,
        cols = seq(col_clean_var, col_qualification_last),
        rows = xr
      )
    }

    qualification_instructions_table <- tibble::tibble(
      "Qualifications" = glue::glue("{seq(qualification_instructions)}. {qualification_instructions}"),
      " " = NA
    ) %>% setNames(., names(.) %>% trimws())

    qualification_instructions_table <- qualification_instructions_table[, c(1, rep(2, col_qualification_last - col_clean_var))]

    openxlsx::writeData(
      wb, sheet_name,
      x = qualification_instructions_table,
      startRow = row_doc_qualification,
      startCol = col_clean_var,
      colNames = TRUE,
      headerStyle = style_header2
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(fgFill = "white"),
      rows = seq(row_doc_qualification + 1, row_doc_qualification_last),
      cols = seq(col_clean_var, col_qualification_last),
      gridExpand = TRUE, stack = TRUE
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_qualification, row_end = row_doc_qualification,
      col_start = col_clean_var, col_end = col_qualification_last
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_qualification, row_end = row_doc_qualification_last,
      col_start = col_clean_var, col_end = col_qualification_last
    )


    #######################
    # segment names table
    #######################

    temp_seg_names <- tibble::tibble(
      "Segment Names" = glue::glue("Segment {segments}"),
      " " = segment_names
    ) %>% setNames(., names(.) %>% trimws())

    temp_seg_names <- temp_seg_names[, c(1, rep(2, col_last - col_seg_name_header))]

    openxlsx::writeData(
      wb, sheet_name,
      x = temp_seg_names,
      startRow = row_doc_seg_name_header,
      startCol = col_seg_name_header,
      colNames = TRUE,
      headerStyle = style_header2,
      borders = "all"
    )

    for(tr in seq(row_doc_seg_name_header, row_doc_seg_name_last)){
      if(tr == row_doc_seg_name_header){
        openxlsx::mergeCells(
          wb, sheet_name,
          rows = tr,
          cols = seq(col_seg_name_header, col_last)
        )
      }else{
        openxlsx::mergeCells(
          wb, sheet_name,
          rows = tr,
          cols = seq(col_seg_name_value, col_last)
        )
      }
    }

    openxlsx::addStyle(
      wb, sheet_name,
      style = style_header,
      rows = seq(row_doc_seg_name_first, row_doc_seg_name_last),
      cols = col_seg_name_header,
      gridExpand = TRUE, stack = TRUE
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_seg_name_header, row_end = row_doc_seg_name_header,
      col_start = col_seg_name_header, col_end = col_last
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_seg_name_header, row_end = row_doc_seg_name_last,
      col_start = col_seg_name_header, col_end = col_last
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_seg_name_header, row_end = row_doc_seg_name_last,
      col_start = col_seg_name_header, col_end = col_seg_name_value
    )


    #######################
    # Solution Questions (doc copy of ind_ui)
    #######################

    for(y in seq(col_clean_var, col_label_right + (doc_label_cell_merge * 2))){

      if(
        y %in% seq(col_clean_var + 2, col_clean_var + 2 + doc_label_cell_merge) ||
        y %in% seq(col_label_right + doc_label_cell_merge, col_label_right + (doc_label_cell_merge * 2))
      ){
        if(y %in% c(col_clean_var + 2, col_label_right + doc_label_cell_merge)){
          openxlsx::mergeCells(
            wb, sheet_name,
            rows = seq(row_doc_header_top - doc_row_gap, row_doc_header_top),
            cols = seq(y, y + doc_label_cell_merge)
          )
        }
      }else{
        openxlsx::mergeCells(
          wb, sheet_name,
          rows = seq(row_doc_header_top - doc_row_gap, row_doc_header_top),
          cols = y
        )
      }
    }

    for(tc in c(col_label_left, col_label_right + doc_label_cell_merge)){
      for(tr in seq(row_doc_first, row_doc_last)){
        openxlsx::mergeCells(
          wb, sheet_name,
          rows = tr,
          cols = seq(tc, tc + doc_label_cell_merge)
        )
      }
    }

    # drop the polar spacer column (col 4) before building the doc copy —
    # the spacer only exists in mixed solutions to align Individual UI with
    # the non-polar Code column. Polar-only solutions have no spacer.
    temp_doc_ui <- ind_ui
    if(!is.null(non_polar_inputs_info)){
      temp_doc_ui <- temp_doc_ui %>% dplyr::select(-4)
    }
    temp_doc_ui <- temp_doc_ui %>%
      dplyr::bind_rows(create_NA_rows(., doc_row_gap), .) %>%
      dplyr::mutate(" " = NA) %>%
      setNames(., names(.) %>% trimws())

    temp_doc_ui <- temp_doc_ui[
      ,
      c(
        1:3,
        rep(ncol(temp_doc_ui), doc_label_cell_merge),
        4:ncol(temp_doc_ui),
        rep(ncol(temp_doc_ui), doc_label_cell_merge - 1)
      )]

    openxlsx::writeData(
      wb, sheet_name,
      x = temp_doc_ui,
      startRow = row_doc_header_top - doc_row_gap,
      startCol = col_clean_var,
      colNames = TRUE,
      headerStyle = style_header,
      borders = "all"
    )

    openxlsx::writeData(
      wb, sheet_name,
      x = purrr::map(
        seq(nrow(inputs_table)),
        ~ seq(length(polar_points)) %>% matrix(nrow = 1) %>% data.frame()
      ) %>% dplyr::bind_rows(),
      startRow = row_doc_header_top - doc_row_gap + 3,
      startCol = col_clean_var + doc_label_cell_merge + 3,
      colNames = FALSE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = style_header,
      rows = seq(row_doc_header_top - doc_row_gap, row_doc_header_top),
      cols = seq(col_clean_var, col_label_right + doc_label_cell_merge),
      gridExpand = TRUE, stack = FALSE
    )

    openxlsx::mergeCells(
      wb, sheet_name,
      rows = row_doc_questions_header - 2,
      cols = seq(col_clean_var, col_last)
    )

    openxlsx::writeData(
      wb, sheet_name,
      x = "Solution Questions",
      startRow = row_doc_questions_header - 2,
      startCol = col_clean_var,
      colNames = FALSE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = style_header2,
      rows = row_doc_questions_header - 2,
      cols = seq(col_clean_var, col_last),
      gridExpand = TRUE, stack = TRUE
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_questions_header - 2, row_end = row_doc_questions_header - 2,
      col_start = col_clean_var, col_end = col_last
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(fontSize = 8),
      rows = row_doc_header_top - doc_row_gap,
      cols = seq(col_label_point_first, col_label_point_last) + doc_label_cell_merge,
      gridExpand = TRUE, stack = TRUE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_doc_first, row_doc_last),
      cols = seq(col_clean_var, col_label_right + (doc_label_cell_merge * 2)),
      gridExpand = TRUE, stack = TRUE
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_header_top - doc_row_gap, row_end = row_doc_last,
      col_start = col_clean_var, col_end = col_label_right + (doc_label_cell_merge * 2)
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_first, row_end = row_doc_last,
      col_start = col_clean_var, col_end = col_label_right + (doc_label_cell_merge * 2)
    )

    temp_row_instructions <- row_doc_header_top - doc_row_gap - 1
    openxlsx::mergeCells(
      wb, sheet_name,
      rows = temp_row_instructions,
      cols = seq(col_label_left, col_label_right + (doc_label_cell_merge * 2))
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = temp_row_instructions, row_end = temp_row_instructions,
      col_start = col_clean_var, col_end = col_label_right + (doc_label_cell_merge * 2)
    )

    openxlsx::writeData(
      wb, sheet_name,
      x = ind_doc_response_instructions,
      startRow = temp_row_instructions,
      startCol = col_label_left,
      colNames = FALSE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = style_header,
      rows = temp_row_instructions,
      cols = seq(col_clean_var, col_label_right + (doc_label_cell_merge * 2)),
      gridExpand = TRUE, stack = TRUE
    )


    #######################
    # non-polar question blocks (doc layout)
    #######################

    if(!is.null(non_polar_inputs_info)){

      col_doc_np_q        <- col_clean_var
      col_doc_np_var      <- col_survey_var
      col_doc_np_text     <- col_label_left
      col_doc_np_text_end <- col_label_left + doc_label_cell_merge
      col_doc_np_response <- col_doc_np_text_end + 1
      col_doc_np_answer   <- col_doc_np_response + 1

      row_np_doc_cursor <- row_doc_last + 3

      for(idx in seq_along(non_polar_inputs_info)){
        info        <- non_polar_inputs_info[[idx]]
        n_vals      <- info$n_values
        row_np_d_h  <- row_np_doc_cursor
        row_np_d_f  <- row_np_d_h + 1
        row_np_d_l  <- row_np_d_h + n_vals

        # header row
        openxlsx::writeData(wb, sheet_name, x = "Question",        startRow = row_np_d_h, startCol = col_doc_np_q,        colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = "Survey\nVariable", startRow = row_np_d_h, startCol = col_doc_np_var,      colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = "Response",         startRow = row_np_d_h, startCol = col_doc_np_response, colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = "Answer",           startRow = row_np_d_h, startCol = col_doc_np_answer,   colNames = FALSE)

        openxlsx::mergeCells(wb, sheet_name, rows = row_np_d_h,
          cols = seq(col_doc_np_text, col_doc_np_text_end))

        openxlsx::addStyle(wb, sheet_name, style = style_header,
          rows = row_np_d_h,
          cols = seq(col_doc_np_q, col_doc_np_answer),
          gridExpand = TRUE, stack = TRUE
        )

        # info on first data row
        openxlsx::writeData(wb, sheet_name, x = info$q_label, startRow = row_np_d_f, startCol = col_doc_np_q,    colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = info$var,     startRow = row_np_d_f, startCol = col_doc_np_var,  colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = info$text,    startRow = row_np_d_f, startCol = col_doc_np_text, colNames = FALSE)

        # merge Q + var vertically across all data rows
        if(n_vals > 1){
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_np_d_f, row_np_d_l), cols = col_doc_np_q)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_np_d_f, row_np_d_l), cols = col_doc_np_var)
        }
        # merge question text as one wide rectangle (rows x cols)
        openxlsx::mergeCells(wb, sheet_name,
          rows = seq(row_np_d_f, row_np_d_l),
          cols = seq(col_doc_np_text, col_doc_np_text_end))

        # response label + answer numeric per value
        for(v in seq_len(n_vals)){
          r <- row_np_d_f + v - 1
          openxlsx::writeData(wb, sheet_name, x = info$value_table$label[v], startRow = r, startCol = col_doc_np_response, colNames = FALSE)
          openxlsx::writeData(wb, sheet_name, x = info$value_table$code[v],  startRow = r, startCol = col_doc_np_answer,   colNames = FALSE)
        }

        # styles + borders
        openxlsx::addStyle(wb, sheet_name, style = style_table,
          rows = seq(row_np_d_f, row_np_d_l),
          cols = seq(col_doc_np_q, col_doc_np_answer),
          gridExpand = TRUE, stack = TRUE
        )

        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_np_d_h, row_end = row_np_d_l,
          col_start = col_doc_np_q, col_end = col_doc_np_answer
        )

        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_np_d_f, row_end = row_np_d_l,
          col_start = col_doc_np_q, col_end = col_doc_np_answer
        )

        row_np_doc_cursor <- row_np_d_l + 2
      }
    }


    #######################
    # additional_questions blocks (doc layout)
    #######################

    if(!is.null(additional_questions_info) && length(additional_questions_info) > 0){

      col_doc_aq_q        <- col_clean_var
      col_doc_aq_var      <- col_survey_var
      col_doc_aq_text     <- col_label_left
      col_doc_aq_text_end <- col_label_left + doc_label_cell_merge
      col_doc_aq_response <- col_doc_aq_text_end + 1
      col_doc_aq_answer   <- col_doc_aq_response + 1

      row_aq_doc_cursor <- if(!is.null(non_polar_inputs_info) && length(non_polar_inputs_info) > 0){
        row_doc_last + np_doc_total_rows + 1   # one row past the np-block region
      }else{
        row_doc_last + 3
      }

      for(idx in seq_along(additional_questions_info)){
        info       <- additional_questions_info[[idx]]
        n_vals     <- nrow(info$value_table)
        row_aq_d_h <- row_aq_doc_cursor
        row_aq_d_f <- row_aq_d_h + 1
        row_aq_d_l <- row_aq_d_h + n_vals

        openxlsx::writeData(wb, sheet_name, x = "Question",         startRow = row_aq_d_h, startCol = col_doc_aq_q,        colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = "Survey\nVariable", startRow = row_aq_d_h, startCol = col_doc_aq_var,      colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = "Response",         startRow = row_aq_d_h, startCol = col_doc_aq_response, colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = "Answer",           startRow = row_aq_d_h, startCol = col_doc_aq_answer,   colNames = FALSE)

        openxlsx::mergeCells(wb, sheet_name, rows = row_aq_d_h,
          cols = seq(col_doc_aq_text, col_doc_aq_text_end))

        openxlsx::addStyle(wb, sheet_name, style = style_header,
          rows = row_aq_d_h,
          cols = seq(col_doc_aq_q, col_doc_aq_answer),
          gridExpand = TRUE, stack = TRUE
        )

        q_label_aq_doc <- glue::glue("AQ{idx}")
        openxlsx::writeData(wb, sheet_name, x = q_label_aq_doc, startRow = row_aq_d_f, startCol = col_doc_aq_q,    colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = info$var,       startRow = row_aq_d_f, startCol = col_doc_aq_var,  colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = info$label,     startRow = row_aq_d_f, startCol = col_doc_aq_text, colNames = FALSE)

        if(n_vals > 1){
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_aq_d_f, row_aq_d_l), cols = col_doc_aq_q)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_aq_d_f, row_aq_d_l), cols = col_doc_aq_var)
        }
        openxlsx::mergeCells(wb, sheet_name,
          rows = seq(row_aq_d_f, row_aq_d_l),
          cols = seq(col_doc_aq_text, col_doc_aq_text_end))

        for(v in seq_len(n_vals)){
          r <- row_aq_d_f + v - 1
          openxlsx::writeData(wb, sheet_name, x = info$value_table$label[v], startRow = r, startCol = col_doc_aq_response, colNames = FALSE)
          openxlsx::writeData(wb, sheet_name, x = info$value_table$code[v],  startRow = r, startCol = col_doc_aq_answer,   colNames = FALSE)
        }

        openxlsx::addStyle(wb, sheet_name, style = style_table,
          rows = seq(row_aq_d_f, row_aq_d_l),
          cols = seq(col_doc_aq_q, col_doc_aq_answer),
          gridExpand = TRUE, stack = TRUE
        )

        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_aq_d_h, row_end = row_aq_d_l,
          col_start = col_doc_aq_q, col_end = col_doc_aq_answer
        )

        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_aq_d_f, row_end = row_aq_d_l,
          col_start = col_doc_aq_q, col_end = col_doc_aq_answer
        )

        row_aq_doc_cursor <- row_aq_d_l + 2
      }
    }


    #######################
    # Solution Coefficient Function (doc copy)
    #######################

    col_func_clean_var <- col_clean_var
    col_func_answer    <- col_func_clean_var + 2 + length(segments)

    temp_coef_func <- tibble::tibble(
      "Question" = c(clean_variable_names, NA)
    ) %>%
      dplyr::bind_cols(coef_func)

    temp_coef_func[nrow(temp_coef_func), "Variable"] <- "Constant"
    temp_coef_func[nrow(temp_coef_func), "Question"] <- "Constant"

    openxlsx::writeData(
      wb, sheet_name,
      x = temp_coef_func,
      startRow = row_doc_function_header,
      startCol = col_func_clean_var,
      headerStyle = style_header,
      borders = "all",
      colNames = TRUE
    )

    openxlsx::mergeCells(
      wb, sheet_name,
      rows = row_doc_function_header - 1,
      cols = seq(col_func_clean_var, col_func_answer - 1)
    )

    openxlsx::writeData(
      wb, sheet_name,
      x = "Solution Coefficient Function",
      startRow = row_doc_function_header - 1,
      startCol = col_func_clean_var,
      colNames = FALSE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = style_header2,
      rows = row_doc_function_header - 1,
      cols = seq(col_func_clean_var, col_func_answer - 1),
      gridExpand = TRUE, stack = TRUE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_doc_function_header, row_doc_function_header + nrow(temp_coef_func)),
      cols = seq(col_func_clean_var, col_func_answer - 1),
      gridExpand = TRUE, stack = TRUE
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_function_header - 1, row_end = row_doc_function_header - 1,
      col_start = col_func_clean_var, col_end = col_func_answer - 1
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_function_header, row_end = row_doc_function_header,
      col_start = col_func_clean_var, col_end = col_func_answer - 1
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_function_header + 1,
      row_end = row_doc_function_header + nrow(temp_coef_func),
      col_start = col_func_clean_var, col_end = col_func_answer - 1
    )


    #######################
    # ranges returned for bulk
    #######################

    range_coef <- purrr::map(
      segments - 1,
      function(xc){
        glue::glue('{num2let(col_func_clean_var + 2 + xc)}${row_doc_function_header + 1}:{num2let(col_func_clean_var + 2 + xc)}${row_doc_function_header + 1 + nrow(coef_func) - 2}')
      }
    )

    range_constant <- purrr::map(
      segments - 1,
      function(xc){
        glue::glue('{num2let(col_func_clean_var + 2 + xc)}${row_doc_function_header + 1 + nrow(coef_func) - 1}')
      }
    )

    range_seg_name <- glue::glue("${num2let(col_seg_name_value)}${row_doc_seg_name_first}:${num2let(col_seg_name_value)}${row_doc_seg_name_last}")


    #######################
    # step-by-step instructions
    #######################

    if(has_recoding){
      recode_rules <- purrr::map_chr(
        seq_len(nrow(recode_mapping)),
        function(i){
          src <- recode_mapping[[1]][i]
          rs  <- recode_mapping[[2]][i]
          suffix <- if(src == rs) " (no change)" else ""
          glue::glue("          {src}     ->     {rs}{suffix}")
        }
      )
    }

    has_logic_doc <- !is.null(additional_logic_info) || !is.null(additional_questions_info)

    s_questionnaire <- 1L
    s_recode        <- if(has_recoding) 2L else NA_integer_
    s_multiply      <- if(has_recoding) 3L else 2L
    s_probability   <- if(has_recoding) 4L else 3L
    s_reassign      <- if(has_logic_doc) (if(has_recoding) 5L else 4L) else NA_integer_
    s_qc            <- if(has_logic_doc){
      if(has_recoding) 6L else 5L
    }else{
      if(has_recoding) 5L else 4L
    }

    var_prefix <- if(has_recoding) "rs" else ""
    mult_adj   <- if(has_recoding) "recoded " else ""
    resp_adj   <- if(has_recoding) "rescaled item " else "item "

    has_non_polar <- length(non_polar_inputs) > 0
    polar_count   <- length(polar_inputs)

    polar_clean_first  <- if(polar_count > 0) clean_variable_names[1] else NA_character_
    polar_clean_last   <- if(polar_count > 0) clean_variable_names[polar_count] else NA_character_
    np_clean_first     <- if(has_non_polar) clean_variable_names[polar_count + 1] else NA_character_
    np_clean_last      <- if(has_non_polar) clean_variable_names[length(clean_variable_names)] else NA_character_

    questionnaire_extra <- if(has_non_polar) c(
      NA,
      glue::glue("          The solution questions include polar pairs ({polar_clean_first} through {polar_clean_last}) and additional single-answer questions ({np_clean_first} through {np_clean_last}).")
    ) else character(0)

    recode_step_content <- if(has_recoding) c(
      glue::glue("Step {s_recode} - Recoding"),
      glue::glue(
        "          Rescale the polar solution questions",
        if(has_non_polar) glue::glue(" ({polar_clean_first} through {polar_clean_last})") else glue::glue(" ({head(clean_variable_names, 1)} through {tail(clean_variable_names, 1)})"),
        " using the following rules:"
      ),
      NA,
      recode_rules,
      NA,
      if(has_non_polar) "          Non-polar question answers are NOT recoded — use the raw answer code for each non-polar question." else NA,
      NA, NA
    ) else character(0)

    reassign_step_content <- if(has_logic_doc){
      reassign_lines <- character(0)
      if(!is.null(additional_logic_info)){
        for(var_name in names(additional_logic_info)){
          rules_l <- additional_logic_info[[var_name]]
          for(j in seq_len(nrow(rules_l))){
            reassign_lines <- c(
              reassign_lines,
              glue::glue("          If {var_name} = {rules_l$value[j]}: set Segment {rules_l$from_seg[j]} probability to 0 and add it to Segment {rules_l$to_seg[j]} probability.")
            )
          }
        }
      }
      if(!is.null(additional_questions_info)){
        for(aq_entry in additional_questions_info){
          for(j in seq_len(nrow(aq_entry$rules))){
            reassign_lines <- c(
              reassign_lines,
              glue::glue("          If {aq_entry$var} = {aq_entry$rules$value[j]}: set Segment {aq_entry$rules$from_seg[j]} probability to 0 and add it to Segment {aq_entry$rules$to_seg[j]} probability.")
            )
          }
        }
      }
      c(
        glue::glue("Step {s_reassign} - Probability Reassignment"),
        "          After computing the base probabilities, apply the following adjustments:",
        NA,
        reassign_lines,
        NA,
        "          The segment with the highest probability after these adjustments is the final classification.",
        NA, NA
      )
    }else character(0)

    # Build per-segment score formula example pieces (split by polar vs non-polar)
    score_formula <- function(seg_col){
      polar_piece <- if(polar_count > 0){
        glue::glue("( {var_prefix}{polar_clean_first} * {coef_func[1, seg_col]} ) + ... + ( {var_prefix}{polar_clean_last} * {coef_func[polar_count, seg_col]} )")
      }else{
        ""
      }
      np_piece <- if(has_non_polar){
        plus_prefix <- if(polar_count > 0) " + " else ""
        glue::glue("{plus_prefix}( {np_clean_first} * {coef_func[polar_count + 1, seg_col]} ) + ... + ( {np_clean_last} * {coef_func[nrow(coef_func) - 1, seg_col]} )")
      }else{
        ""
      }
      glue::glue("{polar_piece}{np_piece} + ( {coef_func[nrow(coef_func), seg_col]} )")
    }

    last_seg_col_name <- names(coef_func)[ncol(coef_func)]

    calulation_steps <- tibble::tibble(
      "Solution Calculation Step-by-step Instructions" = c(
        NA,
        "Segment membership is calculated by doing the following steps:",
        NA,
        glue::glue("Step {s_questionnaire} - Questionnaire"),
        "          Ask respondents the Solution Questions, recording their answer for each item.  Respondents must respond to each and all items for their score to be valid.",
        NA,
        "          We do not recommend randomizing item order or label sides.",
        NA,
        "          The Typing Tool was designed to be asked among respondents who meet the stated qualifications.",
        questionnaire_extra,
        NA, NA,
        recode_step_content,
        glue::glue("Step {s_multiply} - Multiply {mult_adj}responses with coefficient function"),
        glue::glue(
          "          For each participant, multiply the {resp_adj}responses with the coefficients for each segment. Sum the products for each segment and add the constant.  This will create a score for each segment.",
          if(has_non_polar) " For polar questions, multiply the recoded value (rs prefix); for non-polar questions, multiply the raw answer code." else ""
        ),
        NA,
        glue::glue('                    Segment 1 Score = {score_formula("Seg_1")}'),
        NA,
        "                              ...     (Repeat this process for each segment)",
        NA,
        glue::glue('                    Segment {max(segments)} Score = {score_formula(last_seg_col_name)}'),
        NA,
        "          The segment with the highest score is the one the respondent is most likely to be a member of.",
        NA, NA,
        glue::glue("Step {s_probability} - Calculate the probability (optional)"),
        NA,
        "          1. Compute the exponential value for each segment score",
        NA,
        "                    Segment 1 Exponential Score = EXP( Segment 1 Score )",
        "                              OR",
        glue::glue('                    Segment 1 Exponential Score = EXP( {score_formula("Seg_1")} )'),
        NA,
        "                              ...     (Repeat this process for each segment)",
        NA,
        glue::glue("                    Segment {max(segments)} Exponential Score = EXP( Segment {max(segments)} Score )"),
        "                              OR",
        glue::glue('                    Segment {max(segments)} Exponential Score = EXP( {score_formula(last_seg_col_name)} )'),
        NA, NA,
        "          2. Divide each segment's exponential value with the sum of all exponential values",
        NA,
        glue::glue("                    Segment 1 Probability = Segment 1 Exponential Score / ( Segment 1 Exponential Score + ... + Segment {max(segments)} Exponential Score )"),
        "                              OR",
        glue::glue("                    Segment 1 Probability = EXP( Segment 1 Score ) / ( EXP( Segment 1 Score ) + ... + EXP( Segment {max(segments)} Score ))"),
        NA,
        "                              ...     (Repeat this process for each segment)",
        NA,
        glue::glue("                    Segment {max(segments)} Probability = Segment {max(segments)} Exponential Score / ( Segment 1 Exponential Score + ... + Segment {max(segments)} Exponential Score )"),
        "                              OR",
        glue::glue("                    Segment {max(segments)} Probability = EXP( Segment {max(segments)} Score ) / ( EXP( Segment 1 Score ) + ... + EXP( Segment {max(segments)} Score ))"),
        NA, NA,
        reassign_step_content,
        glue::glue("Step {s_qc} - QC Check"),
        NA,
        "          Perform your calculations on a respondent. Then check your results using the Individual UI sheet (starting cell B2).",
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

    calulation_steps <- calulation_steps[, c(1, rep(2, col_last - col_clean_var))]

    for(xr in seq(row_doc_steps, row_doc_steps + nrow(calulation_steps))){
      openxlsx::mergeCells(
        wb, sheet_name,
        cols = seq(col_clean_var, col_last),
        rows = xr
      )
    }

    openxlsx::writeData(
      wb, sheet_name,
      x = calulation_steps,
      startRow = row_doc_steps,
      startCol = col_clean_var,
      colNames = TRUE,
      headerStyle = style_header2
    )

    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(fgFill = "white"),
      rows = seq(row_doc_steps + 1, row_doc_steps + nrow(calulation_steps)),
      cols = seq(col_clean_var, col_last),
      gridExpand = TRUE, stack = TRUE
    )

    for(i in seq(row_doc_steps + 1, row_doc_steps + nrow(calulation_steps))[calulation_steps_bold]){
      openxlsx::addStyle(
        wb, sheet_name,
        style = openxlsx::createStyle(textDecoration = "bold"),
        rows = i,
        cols = seq(col_clean_var, col_last),
        gridExpand = TRUE, stack = TRUE
      )
    }

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_steps, row_end = row_doc_steps,
      col_start = col_clean_var, col_end = col_last
    )

    oxl_outer_box(
      wb, sheet_name, borderStyle = "medium",
      row_start = row_doc_steps, row_end = row_doc_steps + nrow(calulation_steps),
      col_start = col_clean_var, col_end = col_last
    )


    #######################
    # return
    #######################

    return(
      list(
        "range_coef"     = purrr::map(range_coef, ~paste0("'", sheet_name, "'!", .x)),
        "range_constant" = purrr::map(range_constant, ~paste0("'", sheet_name, "'!", .x)),
        "range_seg_name" = paste0("'", sheet_name, "'!", range_seg_name)
      )
    )
  }




  bulk_typing_tool <- function(
    wb = wb,
    sheet_name = sheet_name,
    row_title = start_row,
    start_col = start_col,
    range_recode = NULL,
    range_coef = NULL,
    range_constant = NULL,
    range_seg_name = NULL
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

    n_aq_cols  <- if(!is.null(additional_questions_info)) length(additional_questions_info) else 0
    aq_var_names <- if(n_aq_cols > 0) purrr::map_chr(additional_questions_info, "var") else character(0)
    aq_q_labels  <- if(n_aq_cols > 0) glue::glue("AQ{seq_len(n_aq_cols)}") else character(0)

    col_aq_input_first <- if(n_aq_cols > 0) col_input_last + 1 else NA_integer_
    col_aq_input_last  <- if(n_aq_cols > 0) col_input_last + n_aq_cols else col_input_last

    col_calculation_qc <- col_aq_input_last + 1

    col_recode_first <- col_input_first
    col_recode_last <- col_input_last

    if(inputs_are_rs){
      col_recode_first <- col_calculation_qc + 1
      col_recode_last <- col_recode_first + length(clean_variable_names) - 1
    }

    col_score_first <- if(inputs_are_rs) col_recode_last + 1 else col_calculation_qc + 1
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

    openxlsx::writeData(
      wb, sheet_name,
      x = "Bulk Typing Tool",
      startRow = row_title,
      startCol = start_col,
      colNames = FALSE
    )


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_title,
      rows = row_title,
      cols = start_col,
      stack = TRUE
    )


    #######################
    # input data
    #######################

    openxlsx::writeData(
      wb, sheet_name,
      x = data.frame(
        y = c("Original Questionnaire", inputs_raw),
        x = c("Respondent", clean_variable_names)
      ) %>% t() %>% data.frame(),
      startRow = row_header - 1,
      startCol = start_col,
      colNames = FALSE
    )


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_header,
      rows = seq(row_header - 1, row_header),
      cols = seq(start_col, col_input_last),
      gridExpand = TRUE, stack = TRUE
    )


    openxlsx::mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = seq(start_col, col_input_last)
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = "Data Input",
      startRow = row_header - 2,
      startCol = start_col,
      colNames = FALSE
    )


    openxlsx::addStyle(
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
      rlang::set_names(clean_variable_names)


    openxlsx::writeData(
      wb, sheet_name,
      x = temp_data_inputs,
      startRow = row_data_first,
      startCol = start_col,
      borders = "all",
      colNames = FALSE
    )


    openxlsx::addStyle(
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
    # additional_questions input columns
    #######################

    if(n_aq_cols > 0){
      aq_data <- seg[["data"]][["with_solutions"]] %>%
        dplyr::select(dplyr::all_of(aq_var_names))
      aq_data_padded <- aq_data %>% add_NA_rows(ammount_of_input_rows - nrow(aq_data) + 1)

      openxlsx::writeData(
        wb, sheet_name,
        x = data.frame(
          y = aq_var_names,
          x = aq_q_labels
        ) %>% t() %>% data.frame(),
        startRow = row_header - 1,
        startCol = col_aq_input_first,
        colNames = FALSE
      )

      openxlsx::addStyle(
        wb, sheet_name,
        style = style_header,
        rows = seq(row_header - 1, row_header),
        cols = seq(col_aq_input_first, col_aq_input_last),
        gridExpand = TRUE, stack = TRUE
      )

      openxlsx::mergeCells(
        wb, sheet_name,
        rows = row_header - 2,
        cols = seq(col_aq_input_first, col_aq_input_last)
      )

      openxlsx::writeData(
        wb, sheet_name,
        x = "Additional Questions",
        startRow = row_header - 2,
        startCol = col_aq_input_first,
        colNames = FALSE
      )

      openxlsx::addStyle(
        wb, sheet_name,
        style = style_header2,
        rows = row_header - 2,
        cols = seq(col_aq_input_first, col_aq_input_last),
        gridExpand = TRUE, stack = TRUE
      )

      for(i in c(row_header - 2, row_header)){
        oxl_outer_box(
          wb, sheet_name,
          borderStyle = "medium",
          row_start = i,
          row_end = i,
          col_start = col_aq_input_first,
          col_end = col_aq_input_last
        )
      }

      openxlsx::writeData(
        wb, sheet_name,
        x = aq_data_padded,
        startRow = row_data_first,
        startCol = col_aq_input_first,
        borders = "all",
        colNames = FALSE
      )

      openxlsx::addStyle(
        wb, sheet_name,
        style = style_table,
        rows = seq(row_data_first, row_last),
        cols = seq(col_aq_input_first, col_aq_input_last),
        gridExpand = TRUE, stack = TRUE
      )

      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = row_data_first,
        row_end = row_last,
        col_start = col_aq_input_first,
        col_end = col_aq_input_last
      )
    }



    #######################
    # Calculation Ready
    #######################

    polar_count_b <- length(polar_inputs)
    polar_input_first_let <- num2let(col_input_first)
    polar_input_last_let  <- if(polar_count_b > 0) num2let(col_input_first + polar_count_b - 1) else NA_character_
    all_input_first_let   <- num2let(col_input_first)
    all_input_last_let    <- num2let(col_aq_input_last)  # spans LDA inputs + AQ inputs

    has_non_polar_b <- length(non_polar_inputs) > 0
    has_aq_b        <- n_aq_cols > 0

    calc_ready_formulas <- purrr::map_chr(seq(row_data_first, row_last), function(r){
      parts <- character(0)

      if(polar_count_b > 0){
        parts <- c(parts, glue::glue(
          'COUNTIFS(${polar_input_first_let}{r}:${polar_input_last_let}{r},">={min(polar_points)}", ',
          '${polar_input_first_let}{r}:${polar_input_last_let}{r},"<={max(polar_points)}") = {polar_count_b}'
        ))
      }

      if(has_non_polar_b){
        for(i in seq_along(non_polar_inputs_info)){
          np_info_b <- non_polar_inputs_info[[i]]
          np_col_let <- num2let(col_input_first + polar_count_b + i - 1)
          np_vals    <- as.numeric(np_info_b$value_table$code)
          np_min     <- min(np_vals, na.rm = TRUE)
          np_max     <- max(np_vals, na.rm = TRUE)
          parts <- c(parts, glue::glue('AND(${np_col_let}{r}>={np_min}, ${np_col_let}{r}<={np_max})'))
        }
      }

      if(has_aq_b){
        for(i in seq_along(additional_questions_info)){
          aq_info_b  <- additional_questions_info[[i]]
          aq_col_let <- num2let(col_aq_input_first + i - 1)
          aq_vals    <- as.numeric(aq_info_b$value_table$code)
          aq_min     <- min(aq_vals, na.rm = TRUE)
          aq_max     <- max(aq_vals, na.rm = TRUE)
          parts <- c(parts, glue::glue('AND(${aq_col_let}{r}>={aq_min}, ${aq_col_let}{r}<={aq_max})'))
        }
      }

      check_combined <- if(length(parts) > 1){
        glue::glue('AND({paste(parts, collapse = ", ")})')
      }else if(length(parts) == 1){
        parts
      }else{
        "TRUE"
      }

      glue::glue(
        'IF(COUNTA(${all_input_first_let}{r}:${all_input_last_let}{r}) = 0, "", {check_combined})'
      )
    })

    temp_qc <- tibble::tibble("Calculation Ready" = calc_ready_formulas)
    class(temp_qc[[1]]) <- "formula"


    openxlsx::writeData(
      wb, sheet_name,
      x = temp_qc,
      startRow = row_header,
      startCol = col_calculation_qc,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = "Calculation Ready",
      startRow = row_header - 2,
      startCol = col_calculation_qc,
      colNames = FALSE
    )


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_data_first, row_last),
      cols = col_calculation_qc,
      gridExpand = TRUE, stack = TRUE
    )

    openxlsx::addStyle(
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

    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = col_calculation_qc,
      rows = seq(row_data_first, row_last),
      rule = glue::glue('${num2let(col_calculation_qc)}{row_data_first} = TRUE'),
      style = oxl_style_cell_good(textDecoration = "bold", conditional = TRUE)
    )


    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = col_calculation_qc,
      rows = seq(row_data_first, row_last),
      rule = glue::glue('${num2let(col_calculation_qc)}{row_data_first} = FALSE'),
      style = oxl_style_cell_bad(textDecoration = "bold", conditional = TRUE)
    )



    #######################
    # recode data
    #######################

    if(inputs_are_rs){

      temp <- purrr::map(
        seq(col_input_first, col_input_last),
        function(xc){
          col_idx <- xc - col_input_first + 1
          is_polar_col <- col_idx <= polar_count_b
          if(is_polar_col){
            glue::glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)}, IF(ISBLANK({num2let(xc)}{seq(row_data_first, row_last)}), "", INDEX({range_recode},1,{num2let(xc)}{seq(row_data_first, row_last)})), "")')
          }else{
            glue::glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)}, IF(ISBLANK({num2let(xc)}{seq(row_data_first, row_last)}), "", {num2let(xc)}{seq(row_data_first, row_last)}), "")')
          }
        }
      ) %>%
        dplyr::bind_cols() %>%
        suppressMessages() %>%
        setNames(
          glue::glue("recode_{clean_variable_names}")
        )

      for(i in names(temp)){
        class(temp[[i]]) <- "formula"
      }


      openxlsx::writeData(
        wb, sheet_name,
        x = temp,
        startRow = row_header,
        startCol = col_recode_first,
        borders = "all",
        headerStyle = style_header,
        colNames = TRUE
      )


      openxlsx::mergeCells(
        wb, sheet_name,
        rows = row_header - 2,
        cols = seq(col_recode_first, col_recode_last)
      )


      openxlsx::writeData(
        wb, sheet_name,
        x = "Recode Inputs",
        startRow = row_header - 2,
        startCol = col_recode_first,
        colNames = FALSE
      )


      openxlsx::addStyle(
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


      openxlsx::addStyle(
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

    temp <- purrr::map2(
      range_coef,
      range_constant,
      ~glue::glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)} = TRUE, EXP(MMULT(${num2let(col_recode_first)}{seq(row_data_first, row_last)}:${num2let(col_recode_last)}{seq(row_data_first, row_last)}, {.x}) + {.y}), "")')
    ) %>%
      dplyr::bind_cols() %>%
      suppressMessages() %>%
      setNames(
        glue::glue("exp_score_seg_{segments}")
      )


    for(i in names(temp)){
      class(temp[[i]]) <- "formula"
    }


    openxlsx::writeData(
      wb, sheet_name,
      x = temp,
      startRow = row_header,
      startCol = col_score_first,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    openxlsx::mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = seq(col_score_first, col_score_last)
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = "Calculate Exponential Scores",
      startRow = row_header - 2,
      startCol = col_score_first,
      colNames = FALSE
    )


    openxlsx::addStyle(
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


    openxlsx::addStyle(
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
    # calculate prob (with additional_logic redistribution if any)
    #######################

    score_col_letters <- num2let(seq(col_score_first, col_score_last))

    prob_columns_list <- purrr::map(seq_along(segments), function(seg_idx){
      seg_num   <- as.integer(segments[seg_idx])
      score_let <- score_col_letters[seg_idx]

      purrr::map_chr(seq(row_data_first, row_last), function(r){
        score_range_r <- glue::glue("${num2let(col_score_first)}{r}:${num2let(col_score_last)}{r}")
        base_prob_k   <- glue::glue("{score_let}{r}/SUM({score_range_r})")

        from_k_conds <- character(0)
        added_parts  <- character(0)

        if(!is.null(additional_logic_info)){
          for(var_name in names(additional_logic_info)){
            rules <- additional_logic_info[[var_name]]
            input_idx_v <- match(var_name, inputs)
            cell        <- glue::glue("{num2let(col_input_first + input_idx_v - 1)}{r}")

            from_subset <- rules %>% dplyr::filter(from_seg == seg_num)
            for(j in seq_len(nrow(from_subset))){
              from_k_conds <- c(from_k_conds, glue::glue("{cell}={from_subset$value[j]}"))
            }

            to_subset <- rules %>% dplyr::filter(to_seg == seg_num)
            for(j in seq_len(nrow(to_subset))){
              from_idx <- match(to_subset$from_seg[j], as.integer(segments))
              if(!is.na(from_idx)){
                from_score_let <- score_col_letters[from_idx]
                base_prob_from <- glue::glue("{from_score_let}{r}/SUM({score_range_r})")
                added_parts <- c(added_parts, glue::glue("IF({cell}={to_subset$value[j]}, {base_prob_from}, 0)"))
              }
            }
          }
        }

        if(!is.null(additional_questions_info)){
          for(aq_idx_b in seq_along(additional_questions_info)){
            aq_entry <- additional_questions_info[[aq_idx_b]]
            rules    <- aq_entry$rules
            cell     <- glue::glue("{num2let(col_aq_input_first + aq_idx_b - 1)}{r}")

            from_subset <- rules %>% dplyr::filter(from_seg == seg_num)
            for(j in seq_len(nrow(from_subset))){
              from_k_conds <- c(from_k_conds, glue::glue("{cell}={from_subset$value[j]}"))
            }

            to_subset <- rules %>% dplyr::filter(to_seg == seg_num)
            for(j in seq_len(nrow(to_subset))){
              from_idx <- match(to_subset$from_seg[j], as.integer(segments))
              if(!is.na(from_idx)){
                from_score_let <- score_col_letters[from_idx]
                base_prob_from <- glue::glue("{from_score_let}{r}/SUM({score_range_r})")
                added_parts <- c(added_parts, glue::glue("IF({cell}={to_subset$value[j]}, {base_prob_from}, 0)"))
              }
            }
          }
        }

        zeroed_part <- if(length(from_k_conds) > 0){
          glue::glue("IF(OR({paste(from_k_conds, collapse = ', ')}), 0, {base_prob_k})")
        }else{
          base_prob_k
        }

        full_formula <- if(length(added_parts) > 0){
          paste(c(zeroed_part, added_parts), collapse = " + ")
        }else{
          zeroed_part
        }

        glue::glue('IF(${num2let(col_calculation_qc)}{r}=TRUE, {full_formula}, "")')
      })
    })

    temp <- setNames(prob_columns_list, glue::glue("prob_seg_{segments}")) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      tibble::as_tibble()


    for(i in names(temp)){
      class(temp[[i]]) <- c("formula")
    }


    openxlsx::writeData(
      wb, sheet_name,
      x = temp,
      startRow = row_header,
      startCol = col_prob_first,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    openxlsx::mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = seq(col_prob_first, col_prob_last)
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = "Calculate Probabilities",
      startRow = row_header - 2,
      startCol = col_prob_first,
      colNames = FALSE
    )


    openxlsx::addStyle(
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


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_data_first, row_last),
      cols = seq(col_prob_first, col_prob_last),
      gridExpand = TRUE, stack = TRUE
    )


    openxlsx::addStyle(
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

    # Plain argmax — additional_logic redistributes probabilities upstream
    # in the Calculate Probabilities block, so MAX over the adjusted probs
    # already reflects the reassignment.
    classification_core <- glue::glue('MATCH(MAX({num2let(col_prob_first)}{seq(row_data_first, row_last)}:{num2let(col_prob_last)}{seq(row_data_first, row_last)}), {num2let(col_prob_first)}{seq(row_data_first, row_last)}:{num2let(col_prob_last)}{seq(row_data_first, row_last)}, 0)')

    temp <- data.frame(
      "Classification" = glue::glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)} = TRUE, {classification_core}, "")'),
      "Name" = glue::glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)} = TRUE, INDEX({range_seg_name}, ${num2let(col_seg)}{seq(row_data_first, row_last)}, 1), "")')
    )
    class(temp[["Classification"]]) <- "formula"
    class(temp[["Name"]]) <- "formula"


    openxlsx::writeData(
      wb, sheet_name,
      x = temp,
      startRow = row_header,
      startCol = col_seg,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = "Membership",
      startRow = row_header - 2,
      startCol = col_seg,
      colNames = FALSE
    )


    openxlsx::mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = c(col_seg, col_seg_name)
    )


    openxlsx::addStyle(
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


    openxlsx::addStyle(
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

    openxlsx::writeData(
      wb, sheet_name,
      x = data_solution_check,
      startRow = row_header,
      startCol = col_bulk_qc_first,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    n_qc_pairs <- ncol(data_solution_check) - 1L
    data_solution_check_formulas <- purrr::map2(
      seq(col_bulk_qc_first, col_bulk_qc_last - 1),
      seq(col_prob_first, col_prob_first + n_qc_pairs - 1),
      ~glue::glue('ROUND({num2let(.x)}{seq(row_header + 1, row_header + nrow(data_solution_check))}, 4) = ROUND({num2let(.y)}{seq(row_header + 1, row_header + nrow(data_solution_check))}, 4)')
    ) %>%
      dplyr::bind_cols() %>%
      suppressMessages() %>%
      setNames(
        glue::glue('Prob Check {names(data_solution_check)[-ncol(data_solution_check)]}')
      ) %>%
      dplyr::mutate(
        "Class Check" = glue::glue('{num2let(col_bulk_qc_last)}{seq(row_header + 1, row_header + nrow(data_solution_check))} = {num2let(col_seg)}{seq(row_header + 1, row_header + nrow(data_solution_check))}')
      )

    for(i in names(data_solution_check_formulas)){
      class(data_solution_check_formulas[[i]]) <- c("formula")
    }


    openxlsx::writeData(
      wb, sheet_name,
      x = data_solution_check_formulas,
      startRow = row_header,
      startCol = col_bulk_qc_formula_first,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = "Bulk QC Check",
      startRow = row_header - 2,
      startCol = col_bulk_qc_first,
      colNames = FALSE
    )


    openxlsx::mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = seq(col_bulk_qc_first, col_bulk_qc_formula_last)
    )


    openxlsx::addStyle(
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


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_header + 1, row_header + nrow(data_solution_check)),
      cols = seq(col_bulk_qc_first, col_bulk_qc_formula_last),
      gridExpand = TRUE, stack = TRUE
    )


    openxlsx::addStyle(
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


    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_bulk_qc_formula_first, col_bulk_qc_formula_last),
      rows = seq(row_header + 1, row_header + nrow(data_solution_check)),
      rule = glue::glue('{num2let(col_bulk_qc_formula_first)}{row_header + 1} = TRUE'),
      style = oxl_style_cell_good(textDecoration = "bold", conditional = TRUE)
    )


    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_bulk_qc_formula_first, col_bulk_qc_formula_last),
      rows = seq(row_header + 1, row_header + nrow(data_solution_check)),
      rule = glue::glue('{num2let(col_bulk_qc_formula_first)}{row_header + 1} = FALSE'),
      style = oxl_style_cell_bad(textDecoration = "bold", conditional = TRUE)
    )


    #######################
    # final column group
    #######################

    openxlsx::setColWidths(wb, sheet_name, cols = col_seg, widths = 15)
    openxlsx::setColWidths(wb, sheet_name, cols = c(start_col, col_seg_name), widths = 25)

    openxlsx::groupColumns(
      wb, sheet_name,
      cols = seq(col_score_first, col_prob_last),
      hidden = FALSE
    )

  }



  #######################
  # create workbook
  #######################

  wb <- oxl_create_workbook()

  sheet_name_ui   <- "Individual UI"
  sheet_name_doc  <- "Documentation"
  sheet_name_bulk <- "Bulk"

  for(s in c(sheet_name_ui, sheet_name_doc, sheet_name_bulk)){
    openxlsx::addWorksheet(wb, s)
  }

  row_last_formatting <- (start_row + 6) + ((start_row + 6 + nrow(data_inputs)) %>% divide_by(1000) %>% ceiling() %>% multiply_by(1000)) + 1

  for(s in c(sheet_name_ui, sheet_name_doc, sheet_name_bulk)){
    openxlsx::addStyle(
      wb, s,
      style = openxlsx::createStyle(fgFill = oxl_colorscale_grey(1)),
      rows = seq(1, row_last_formatting),
      cols = seq(1, 200),
      gridExpand = TRUE
    )
  }


  ui_returns <- individual_ui_tool(
    wb = wb,
    sheet_name = sheet_name_ui,
    row_title = start_row,
    start_col = start_col,
    polar_label_width = polar_label_width
  )


  doc_returns <- documentation_tool(
    wb = wb,
    sheet_name = sheet_name_doc,
    row_title = start_row,
    start_col = start_col,
    polar_label_width = polar_label_width
  )


  bulk_typing_tool(
    wb = wb,
    sheet_name = sheet_name_bulk,
    row_title = start_row,
    start_col = start_col,
    range_recode = ui_returns[["range_recode"]],
    range_coef = doc_returns[["range_coef"]],
    range_constant = doc_returns[["range_constant"]],
    range_seg_name = doc_returns[["range_seg_name"]]
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

  file_name <- glue::glue("{where}/{file_name}.xlsx")

  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)

}
