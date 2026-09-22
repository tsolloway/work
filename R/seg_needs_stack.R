#' seg_needs_stack
#'
#' @description Unstacks an importance loop into one row per person-context.
#'   A loop written as `<stem>_<context>r<item>` holds a full item grid for each
#'   context a respondent was assigned, and NA for every context they were not.
#'   This turns that wide block into a long frame where the unit is the grid.
#'
#'   A row is emitted only where the grid was actually answered, so the result
#'   can never contain a grid of missing items. Person-level variables are
#'   carried through unchanged and therefore repeat across a respondent's rows;
#'   `id_var` is preserved so a grid-level solution can be rolled back up.
#'
#'   Always read [seg_needs_stack_report()] before trusting the result. A loop
#'   keyed by POSITION rather than by context identity will stack without
#'   complaint and produce a plausible file of nonsense.
#'
#' @param seg A seg object from [seg_needs_init()] with data loaded.
#' @param stem Character. The loop stem, e.g. `"NeedsxOccasion"` for columns
#'   named `NeedsxOccasion_3r7`.
#' @param id_var Character. Respondent identifier on the person frame, carried
#'   onto every grid row as `person_id`.
#' @param context_vars Named character vector of per-context variables to carry
#'   across, where the name is the output column and the value is a template
#'   containing `{context}`, e.g.
#'   `c(context_freq = "OccasionFrequencyr{context}")`.
#' @param sep Character between the stem and the context index (default `"_"`).
#' @param item_sep Character between the context index and the item index
#'   (default `"r"`).
#'
#' @return The seg object with `seg[["data"]][["stacked"]]` populated and the
#'   loop definition recorded in `seg[["needs"]][["loop"]]`.
#'
#' @export
seg_needs_stack <- function(
    seg,
    stem,
    id_var       = "record",
    context_vars = NULL,
    sep          = "_",
    item_sep     = "r"
){

  if(!inherits(seg, "analytic_needs")){
    stop("Run seg_needs_init() before seg_needs_stack().", call. = FALSE)
  }

  df <- seg[["data"]][["original"]]

  if(!is.data.frame(df)){
    stop("No person-level data found. Run seg_get_data() first.", call. = FALSE)
  }

  if(identical(seg[["meta"]][["unit"]], "grid")){
    stop(
      "The unit is already set to grid, so seg$data$original is the stacked ",
      "frame. Run seg_needs_set_unit('person') before stacking again.",
      call. = FALSE
    )
  }

  if(!id_var %in% names(df)){
    stop("id_var '", id_var, "' is not a column on the data.", call. = FALSE)
  }

  loop        <- seg[["needs"]][["loop"]]
  n_items     <- loop[["n_items"]]
  n_contexts  <- loop[["n_contexts"]]
  item_names  <- sprintf("need%02d", seq_len(n_items))

  grid_cols <- function(j){
    paste0(stem, sep, j, item_sep, seq_len(n_items))
  }

  # every context's grid must exist in full, or the loop is not what we think
  missing_cols <- setdiff(unlist(lapply(seq_len(n_contexts), grid_cols)), names(df))

  if(length(missing_cols) > 0){
    stop(
      "Loop columns not found on the data (", length(missing_cols), " missing), e.g. ",
      paste(utils::head(missing_cols, 3), collapse = ", "),
      ".\nCheck `stem`, `sep`, `item_sep`, and that work::read(clean_col_names = FALSE) was used - ",
      "the default lowercases every column name.",
      call. = FALSE
    )
  }


  # Keep the raw loop columns on the stacked frame. They are person-level (the
  # whole battery, all contexts) and the spec needs them to state a person-level
  # summary column-wise - rowMeans across a respondent's occasion columns. Drop
  # them and that summary can only be produced in R, outside the document.
  person_vars <- names(df)

  stacked <- lapply(seq_len(n_contexts), function(j){

    cols <- grid_cols(j)

    carried <- lapply(context_vars, function(tmpl){
      v <- gsub("\\{context\\}", j, tmpl)
      if(!v %in% names(df)) stop("context_var column '", v, "' not found.", call. = FALSE)
      rlang::sym(v)
    })

    df %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(cols), ~ !is.na(.x))) %>%
      dplyr::transmute(
        person_id      = .data[[id_var]],
        context        = j,
        context_label  = loop[["context_labels"]][j],
        !!!carried,
        !!!rlang::set_names(lapply(cols, rlang::sym), item_names),
        dplyr::across(dplyr::all_of(person_vars))
      )

  }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(.data$person_id, .data$context)


  # a partially answered grid is dropped by if_all above; say so rather than
  # silently losing it
  any_answered <- lapply(seq_len(n_contexts), function(j){
    rowSums(!is.na(df[grid_cols(j)])) > 0
  }) %>% Reduce(`+`, .) %>% sum()

  if(any_answered > nrow(stacked)){
    warning(
      any_answered - nrow(stacked),
      " grid(s) were partially answered and have been dropped. ",
      "Check the loop's own skip logic.",
      call. = FALSE
    )
  }


  seg[["data"]][["stacked"]]      <- stacked
  seg[["needs"]][["loop"]][["stem"]]   <- stem
  seg[["needs"]][["loop"]][["id_var"]] <- id_var
  seg[["needs"]][["vars"]][["raw"]]    <- item_names

  message(
    "Stacked ", nrow(stacked), " grids from ",
    dplyr::n_distinct(stacked$person_id), " respondents. ",
    "Run seg_needs_stack_report() before going further."
  )

  seg
}
