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
#' @param ui_groups Optional tibble describing how to merge multiple input
#'   variables into a single combined block on the Individual UI and
#'   Documentation sheets. Columns:
#'   \itemize{
#'     \item `label` — section header for the merged block.
#'     \item `type` — `"multi_select"` (multiple selections allowed) or
#'       `"mece"` (mutually exclusive — at most one selection across the group).
#'     \item `vars` — list-column; each cell is a character vector of variable
#'       names. Each variable must be either an LDA input or an
#'       `additional_questions` variable; otherwise validation errors.
#'     \item `no_selection_allowed` — logical; when `TRUE`, zero selections is
#'       valid in Calculation Ready; when `FALSE`, the group requires at least
#'       one selection (multi_select) or exactly one (mece).
#'   }
#'   Variables placed in a `ui_groups` row are skipped by their normal
#'   individual-block render and are rendered inside the merged block instead.
#' @param apply_logic_only_to_max Logical, default `TRUE`. Controls whether
#'   `additional_logic` / `additional_questions` reassignment rules fire
#'   conditionally on the LDA-predicted segment. When `TRUE`, a rule's
#'   redistribution only fires if `from_seg` is currently the max-scoring
#'   segment for that respondent (i.e., the LDA's predicted class is
#'   `from_seg`); when `FALSE`, the rule fires whenever its `value` matches
#'   regardless of which segment the respondent was originally classified as.
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
#' @param additional_bulk_qc_check Optional character vector of variable names
#'   from `seg$data$with_solutions`. Each named variable is appended as an
#'   extra column to the right of the Bulk QC Check block on the Bulk sheet,
#'   under the same merged "Bulk QC Check" title. Useful for spot-checking
#'   classification results against known reference variables (e.g. the
#'   original `seg` column from a prior solution). Errors if any name is not
#'   a column of `seg$data$with_solutions`.
#' @param additional_logic_advanced Optional tibble of sequential
#'   `case_when`-style reassignment rules, applied after the simple
#'   `additional_logic` / `additional_questions` redistributions. Columns:
#'   \itemize{
#'     \item `when` — either a character expression (e.g.
#'       `"(VT02 == 1 | VT03 == 1) & VT21 == 1"`) **or** a one-sided formula
#'       (e.g. `~ (VT02 == 1 | VT03 == 1) & VT21 == 1`) describing the
#'       variable condition. Strings let you use `tibble::tribble` for a
#'       compact row-major layout; formulas require `tibble::tibble` with an
#'       explicit list-column for `when` (since `tribble` parses every
#'       `~`-prefixed cell as a column-name spec). Supported operators in
#'       both forms: `==`, `!=`, `<`, `<=`, `>`, `>=`, `&`, `|`, `!`,
#'       `%in%`, plus parentheses. Variables must be LDA inputs or appear
#'       in `additional_questions` (an entry of
#'       `additional_questions = list(VAR = NULL)` declares a variable for
#'       conditions only).
#'     \item `from_lda` — `NA` for **hard reassignment** (when `when` fires,
#'       `to_seg`'s probability is clamped to 1 and every other segment's
#'       probability to 0; classification therefore lands on `to_seg`
#'       regardless of which LDA segment the respondent originally
#'       resolved to); a single integer or integer vector of LDA segments
#'       for **soft reassignment** (probability mass moves from those
#'       segments to `to_seg` when `when` fires — sum-to-1 preserved by
#'       redistribution; classification re-argmaxes).
#'     \item `to_seg` — single integer target segment. Can be an existing
#'       LDA seg or a new seg (extends `segments` like
#'       `additional_logic` / `additional_questions` do; new segs must be
#'       sequential).
#'   }
#'   Rules are applied in row order (case_when semantics: first matching
#'   hard rule overrides the argmax classification; soft rules all fire
#'   independently at the prob layer). Stacks on top of `additional_logic`
#'   and `additional_questions` — both can coexist.
#'
#' @return Invisibly writes the workbook to disk. Returns `NULL`.
#'
#' @examples
#' \dontrun{
#' # 1. Minimal call — just polars and non-polars from the solution.
#' seg %>% seg_typing_tool(solution_name = "LDA_kids_v11_seed")
#'
#' # 2. Simple post-LDA reassignment — when DM20 = 1, take Segment 6's
#' #    probability and add it to Segment 1 (LDA → LDA, mass conserved).
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_kids_v11_seed",
#'   additional_logic = list(
#'     DM20 = tibble::tribble(
#'       ~from_seg, ~value, ~to_seg,
#'       6,         1,      1
#'     )
#'   )
#' )
#'
#' # 3. additional_questions adds non-LDA vars with their own input blocks
#' #    in Individual UI / Documentation / Bulk and reassigns to a new seg
#' #    (Segment 7 here, which is auto-extended onto the segment set).
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_kids_v11_seed",
#'   additional_questions = list(
#'     DM26 = tibble::tribble(~from_seg, ~value, ~to_seg, 2, 1, 7),
#'     DM25 = tibble::tribble(~from_seg, ~value, ~to_seg, 3, 1, 7)
#'   )
#' )
#'
#' # 4. ui_groups merges multiple LDA inputs / additional_questions into a
#' #    single multi_select / mece block on the Individual UI and Doc sheets.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_kids_v11_seed",
#'   ui_groups = tibble::tribble(
#'     ~label,         ~type,           ~vars,                                     ~no_selection_allowed,
#'     "Activities",   "multi_select",  list(c("AP06", "PL03", "PL27")),            TRUE,
#'     "Child Age",    "mece",          list(c("DM20", "DM21", "DM22", "DM23")),    FALSE
#'   )
#' )
#'
#' # 5. additional_bulk_qc_check appends one or more reference columns from
#' #    seg$data$with_solutions to the Bulk QC Check, plus per-var
#' #    classification-comparison columns and matching freq tables.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_kids_v11_seed",
#'   additional_bulk_qc_check = c("solution_kids_krikor_fix", "seg_v8")
#' )
#'
#' # 6. Combine the simple mechanisms — additional_logic + additional_questions
#' #    + ui_groups + additional_bulk_qc_check at once. Note DM20-DM24 are LDA
#' #    inputs (folded into Child Age) while DM26/DM25 are additional_questions
#' #    (folded into Child Gender).
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_kids_v11_seed",
#'   additional_bulk_qc_check = "solution_kids_krikor_fix",
#'   additional_logic = list(
#'     DM20 = tibble::tribble(~from_seg, ~value, ~to_seg, 6, 1, 1)
#'   ),
#'   additional_questions = list(
#'     DM26 = tibble::tribble(~from_seg, ~value, ~to_seg, 2, 1, 7),
#'     DM25 = tibble::tribble(~from_seg, ~value, ~to_seg, 3, 1, 7)
#'   ),
#'   ui_groups = tibble::tribble(
#'     ~label,         ~type,           ~vars,                                            ~no_selection_allowed,
#'     "Activities",   "multi_select",  list(c("AP06", "PL03", "PL27")),                   TRUE,
#'     "Child Age",    "mece",          list(c("DM20", "DM21", "DM22", "DM23", "DM24")),   FALSE,
#'     "Child Gender", "mece",          list(c("DM26", "DM25")),                           FALSE
#'   )
#' )
#'
#' # 7. additional_logic_advanced — single hard rule (no LDA reference).
#' #    When VT02 or VT03 is 1 AND VT21 or VT22 is 1, Segment 9's probability
#' #    is set to 100% and every other segment to 0% (so classification lands
#' #    on Segment 9). Variables used in the condition that aren't LDA inputs
#' #    (VT21, VT22) are declared with `additional_questions = list(VAR = NULL)`
#' #    so they're pulled into Bulk + Individual UI without their own
#' #    reassignment rules. `when` uses the character form so the tribble
#' #    row-major layout works.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_opt_seed_v30",
#'   additional_questions = list(VT21 = NULL, VT22 = NULL),
#'   additional_logic_advanced = tibble::tribble(
#'     ~when,                                                       ~from_lda, ~to_seg,
#'     "(VT02 == 1 | VT03 == 1) & (VT21 == 1 | VT22 == 1)",         NA,        9
#'   )
#' )
#'
#' # 8. additional_logic_advanced — single soft rule (LDA gated). When
#' #    VT03 = 1 AND the respondent's LDA argmax is Segment 6 or 7,
#' #    probability mass moves from those segments to Segment 9.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_opt_seed_v30",
#'   additional_logic_advanced = tibble::tribble(
#'     ~when,           ~from_lda, ~to_seg,
#'     "VT03 == 1",     c(6, 7),   9
#'   )
#' )
#'
#' # 9. additional_logic_advanced — pure-LDA rename (no condition variables;
#' #    just rename Segment 9 to 10 and Segment 10 to 11). Use "TRUE" for
#' #    "no extra condition" — first match wins, so order matters.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_opt_seed_v30",
#'   additional_logic_advanced = tibble::tribble(
#'     ~when,   ~from_lda, ~to_seg,
#'     "TRUE",   9,         10,
#'     "TRUE",   10,        11
#'   )
#' )
#'
#' # 10. additional_logic_advanced — full case_when from a real project,
#' #     mixing hard + soft rules sequentially.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_opt_seed_v30",
#'   additional_questions = list(VT21 = NULL, VT22 = NULL),
#'   additional_logic_advanced = tibble::tribble(
#'     ~when,                                                       ~from_lda, ~to_seg,
#'     "(VT02 == 1 | VT03 == 1) & (VT21 == 1 | VT22 == 1)",         NA,        9,
#'     "VT03 == 1",                                                  c(6, 7),   9,
#'     "TRUE",                                                       9,         10,
#'     "TRUE",                                                       10,        11
#'   )
#' )
#'
#' # 11. %in% support inside conditions — equivalent to chained == / |.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_opt_seed_v30",
#'   additional_logic_advanced = tibble::tribble(
#'     ~when,                       ~from_lda, ~to_seg,
#'     "VT01 %in% c(1, 2, 3)",      c(4, 5),   8
#'   )
#' )
#'
#' # 12. Same rules as example 10, but using formula syntax via
#' #     `tibble::tibble` with an explicit list-column. Equivalent end result;
#' #     pick whichever style you prefer. (Strings + tribble = compact;
#' #     formulas + tibble + list-column = better IDE syntax highlighting.)
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_opt_seed_v30",
#'   additional_questions = list(VT21 = NULL, VT22 = NULL),
#'   additional_logic_advanced = tibble::tibble(
#'     when = list(
#'       ~ (VT02 == 1 | VT03 == 1) & (VT21 == 1 | VT22 == 1),
#'       ~ VT03 == 1,
#'       ~ TRUE,
#'       ~ TRUE
#'     ),
#'     from_lda = list(NA, c(6, 7), 9, 10),
#'     to_seg   = c(9, 9, 10, 11)
#'   )
#' )
#'
#' # 13. Stack additional_logic_advanced on top of additional_logic /
#' #     additional_questions — they coexist. Soft rules from both layers
#' #     redistribute mass at the prob layer; hard rules from advanced
#' #     override the argmax at the very end.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_opt_seed_v30",
#'   additional_questions = list(
#'     VT21 = NULL,
#'     VT22 = NULL,
#'     DM01 = tibble::tribble(~from_seg, ~value, ~to_seg, 5, 1, 8)
#'   ),
#'   additional_logic = list(
#'     VT01 = tibble::tribble(~from_seg, ~value, ~to_seg, 3, 1, 2)
#'   ),
#'   additional_logic_advanced = tibble::tribble(
#'     ~when,                                                  ~from_lda, ~to_seg,
#'     "(VT02 == 1 | VT03 == 1) & (VT21 == 1 | VT22 == 1)",    NA,        9,
#'     "TRUE",                                                  9,         10
#'   )
#' )
#'
#' # 13. apply_additional_logic_to_qc = TRUE so the Bulk QC Check compares
#' #     post-redistribution classifications on both sides (matching). Useful
#' #     once you trust the redistribution logic and want a clean Class Check.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_kids_v11_seed",
#'   additional_logic = list(
#'     DM20 = tibble::tribble(~from_seg, ~value, ~to_seg, 6, 1, 1)
#'   ),
#'   apply_additional_logic_to_qc = TRUE
#' )
#'
#' # 14. apply_logic_only_to_max = FALSE — rules fire whenever their
#' #     condition matches, regardless of which segment the respondent was
#' #     originally classified into. Useful for unconditional reassignments.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_kids_v11_seed",
#'   additional_questions = list(
#'     DM26 = tibble::tribble(~from_seg, ~value, ~to_seg, 2, 1, 7)
#'   ),
#'   apply_logic_only_to_max = FALSE
#' )
#'
#' # 15. use_colinear_lda — recompute the coefficient table from the LDA fit
#' #     via SVD-based scaling. Use when the closed-form Sigma_W^{-1} blows
#' #     up on collinear inputs.
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_collinear_inputs_solution",
#'   use_colinear_lda = TRUE
#' )
#'
#' # 16. overwrite_left_label / overwrite_right_label / segment_names — full
#' #     manual labeling override (e.g. when the spec hasn't been finalized).
#' seg %>% seg_typing_tool(
#'   solution_name = "LDA_kids_v11_seed",
#'   overwrite_left_label = c("I prefer X", "I prefer Y", "..."),
#'   overwrite_right_label = c("I prefer not X", "I prefer not Y", "..."),
#'   segment_names = c("Adventurers", "Caretakers", "Builders", "Explorers",
#'                     "Performers", "Strategists", "Free Spirits")
#' )
#' }
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
    apply_logic_only_to_max = TRUE,
    ui_groups = NULL,
    use_colinear_lda = NULL,
    additional_bulk_qc_check = NULL,
    additional_logic_advanced = NULL
){

  row_title <- start_row

  where <- seg[["paths"]][["folders"]][["solution"]]

  df <- seg[["data"]][["with_solutions"]]

  # Validate additional_bulk_qc_check: must be NULL or a character vector whose
  # entries name columns in seg$data$with_solutions. The bulk tool reads the
  # values from there and writes them next to the QC Check block.
  if(!is.null(additional_bulk_qc_check)){
    if(!is.character(additional_bulk_qc_check)){
      stop("`additional_bulk_qc_check` must be a character vector of variable names (or NULL).")
    }
    missing_qc_vars <- setdiff(additional_bulk_qc_check, names(df))
    if(length(missing_qc_vars) > 0){
      stop(glue::glue(
        "`additional_bulk_qc_check` references variable(s) not found in seg$data$with_solutions: ",
        "{paste(missing_qc_vars, collapse = ', ')}."
      ))
    }
  }

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
      # NULL / empty-tibble entries are allowed in additional_questions: they
      # declare "this var participates in additional_logic_advanced conditions
      # — pull it into Bulk + UI — but it has no own from/to/value rules".
      # additional_logic still requires real rule tibbles since LDA inputs are
      # already pulled in regardless.
      if(!must_be_in_inputs && (is.null(rules) || (is.data.frame(rules) && nrow(rules) == 0))){
        next
      }
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

      # NULL / empty-tibble entries (vars declared for advanced conditions
      # only) get an empty rules tibble so downstream code that iterates
      # rules still works.
      if(is.null(rules) || nrow(rules) == 0){
        rules <- tibble::tibble(from_seg = integer(0), value = numeric(0), to_seg = integer(0))
      }

      list(
        var         = var_name,
        label       = label_val,
        value_table = parse_value_labels(var_name),
        rules       = rules
      )
    })
  }


  #######################
  # additional_logic_advanced — validate, parse formulas, extract referenced
  # vars. The actual Excel-formula emission happens at render time inside
  # bulk_typing_tool() / individual_ui_tool() since they need the var-to-cell
  # mapper.
  #######################

  additional_logic_advanced_info <- NULL

  if(!is.null(additional_logic_advanced)){
    if(!is.data.frame(additional_logic_advanced)){
      stop("`additional_logic_advanced` must be a tibble (or NULL).")
    }
    required_adv_cols <- c("when", "from_lda", "to_seg")
    if(!all(required_adv_cols %in% names(additional_logic_advanced))){
      stop(glue::glue(
        "`additional_logic_advanced` must have columns: {paste(required_adv_cols, collapse = ', ')}."
      ))
    }

    # Recursive AST walker: extract every variable name (symbol) referenced
    # in a `when` formula. Used for validation (every var must exist in LDA
    # inputs or additional_questions) and later for downstream wiring.
    extract_when_vars_recursive <- function(expr){
      if(is.symbol(expr)){
        nm <- as.character(expr)
        if(nm %in% c("TRUE", "FALSE", "NA", "T", "F")) return(character(0))
        return(nm)
      }
      if(is.call(expr)){
        out <- character(0)
        for(arg in as.list(expr)[-1]){
          out <- c(out, extract_when_vars_recursive(arg))
        }
        return(out)
      }
      character(0)
    }
    extract_when_vars <- function(when_formula){
      if(!inherits(when_formula, "formula")) return(character(0))
      unique(extract_when_vars_recursive(rlang::f_rhs(when_formula)))
    }

    aq_var_names_for_when <- if(!is.null(additional_questions_info)){
      purrr::map_chr(additional_questions_info, "var")
    }else character(0)
    valid_when_vars <- c(inputs, aq_var_names_for_when)

    parsed_adv_rules <- purrr::imap(seq_len(nrow(additional_logic_advanced)), function(i, .) NULL)  # placeholder to ensure indices
    parsed_adv_rules <- vector("list", nrow(additional_logic_advanced))

    for(i in seq_len(nrow(additional_logic_advanced))){
      when_raw <- additional_logic_advanced[["when"]][[i]]
      # Accept either a one-sided formula (`~ expr`) or a character string
      # (`"expr"`). Strings make the row-major `tribble` form usable —
      # `tribble` parses every `~`-prefixed cell as a column-name spec, so
      # formulas can only live in a list-column built via `tibble::tibble`.
      when_i <- if(inherits(when_raw, "formula")){
        when_raw
      }else if(is.character(when_raw) && length(when_raw) == 1 && !is.na(when_raw)){
        stats::as.formula(paste("~", when_raw))
      }else{
        stop(glue::glue(
          "`additional_logic_advanced$when[[{i}]]` must be a one-sided formula ",
          "(e.g. `~ VT03 == 1`) or a character expression (e.g. `\"VT03 == 1\"`)."
        ))
      }

      from_lda_i <- additional_logic_advanced[["from_lda"]][[i]]
      # accept NA, single integer, integer vector. Validate values fall in LDA segs.
      if(length(from_lda_i) == 1 && is.na(from_lda_i)){
        from_lda_i <- NA_integer_
      }else{
        from_lda_i <- as.integer(from_lda_i)
        if(any(is.na(from_lda_i))){
          stop(glue::glue(
            "`additional_logic_advanced$from_lda[[{i}]]` contains NA among non-NA entries; ",
            "use a bare NA scalar for hard reassignment, or a single integer / integer vector ",
            "of LDA segments for soft reassignment."
          ))
        }
        bad_lda <- setdiff(from_lda_i, lda_segments)
        if(length(bad_lda) > 0){
          stop(glue::glue(
            "`additional_logic_advanced$from_lda[[{i}]]` references segment(s) ",
            "({paste(bad_lda, collapse = ', ')}) not in the LDA segment set ",
            "({paste(lda_segments, collapse = ', ')})."
          ))
        }
      }

      to_seg_i <- as.integer(additional_logic_advanced[["to_seg"]][[i]])
      if(length(to_seg_i) != 1 || is.na(to_seg_i)){
        stop(glue::glue("`additional_logic_advanced$to_seg[[{i}]]` must be a single integer."))
      }

      vars_in_when <- extract_when_vars(when_i)
      bad_vars <- setdiff(vars_in_when, valid_when_vars)
      if(length(bad_vars) > 0){
        stop(glue::glue(
          "`additional_logic_advanced$when[[{i}]]` references variable(s) not in LDA inputs ",
          "or additional_questions: {paste(bad_vars, collapse = ', ')}. ",
          "Add them to `additional_questions` (use `additional_questions = list({bad_vars[1]} = NULL, ...)` ",
          "to declare them for conditions only)."
        ))
      }

      parsed_adv_rules[[i]] <- list(
        when_formula = when_i,
        when_vars    = vars_in_when,
        from_lda     = from_lda_i,                            # NA_integer_ = hard, else soft
        to_seg       = to_seg_i,
        is_hard      = length(from_lda_i) == 1 && is.na(from_lda_i),
        rule_index   = i
      )
    }

    # Reorder for display: soft rules first (they redistribute prob mass —
    # all run independently), then hard rules (clamp the final prob vector;
    # sequential, first match wins). Within each kind, preserve the user's
    # tribble order. Assign display_index 1..N in this new order so the doc
    # step text and Reassignment Counts table read 1, 2, 3 top-to-bottom
    # regardless of how the user mixed soft/hard in their input. The original
    # rule_index is kept too in case any debugging back-reference to the
    # user's tribble row is needed.
    is_hard_flags  <- purrr::map_lgl(parsed_adv_rules, "is_hard")
    soft_first_idx <- c(which(!is_hard_flags), which(is_hard_flags))
    parsed_adv_rules <- parsed_adv_rules[soft_first_idx]
    for(i in seq_along(parsed_adv_rules)){
      parsed_adv_rules[[i]]$display_index <- i
    }

    additional_logic_advanced_info <- parsed_adv_rules
  }


  # combine new segs from all sources for one sequential check
  current_max_seg <- max(as.integer(segments))
  all_to_segs <- c(
    unlist(purrr::map(additional_logic,     ~ as.integer(.x$to_seg))),
    unlist(purrr::map(additional_questions, ~ as.integer(.x$to_seg))),
    if(!is.null(additional_logic_advanced_info)){
      purrr::map_int(additional_logic_advanced_info, ~ as.integer(.x$to_seg))
    }else integer(0)
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

  # extend coef_func with zero coefs + zero constant for the new segments.
  # The score-level redistribution in bulk_typing_tool() / individual_ui_tool()
  # overrides these zeros and computes new-seg scores from from_seg
  # contributions, so the displayed coefficients in the doc table can stay
  # cleanly zero for new segments.
  if(length(combined_new_segs) > 0){
    new_col_names <- glue::glue("Seg_{combined_new_segs}")
    new_cols <- as.data.frame(matrix(0, nrow = nrow(coef_func), ncol = length(combined_new_segs)))
    names(new_cols) <- new_col_names
    coef_func <- dplyr::bind_cols(coef_func, new_cols)
  }

  any_redistribution_logic_for_qc <- !is.null(additional_logic_info) ||
    !is.null(additional_questions_info) ||
    !is.null(additional_logic_advanced_info)

  if(isTRUE(apply_additional_logic_to_qc) && any_redistribution_logic_for_qc){
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

      # Snapshot of base argmax — the LDA-predicted segment for this respondent
      base_argmax_seg <- if(any(probs > 0)) as.integer(segments[which.max(probs)]) else NA_integer_

      for(var_name in names(combined_logic_for_qc)){
        rules    <- combined_logic_for_qc[[var_name]]
        resp_val <- seg[["data"]][["with_solutions"]][[var_name]][i]
        if(is.na(resp_val)) next

        for(j in seq_len(nrow(rules))){
          if(rules$value[j] == resp_val){
            if(isTRUE(apply_logic_only_to_max) && (is.na(base_argmax_seg) || base_argmax_seg != as.integer(rules$from_seg[j]))){
              next
            }
            from_idx <- match(rules$from_seg[j], segments)
            to_idx   <- match(rules$to_seg[j], segments)
            if(!is.na(from_idx) && !is.na(to_idx)){
              probs[to_idx]   <- probs[to_idx] + probs[from_idx]
              probs[from_idx] <- 0
            }
          }
        }
      }

      # Soft advanced rules — same prob redistribution shape as the simple
      # additional_logic, but with compound `when` conditions evaluated in
      # the respondent's data row, and from_lda allowed to be a vector of
      # LDA segs (any of which contributes when the rule fires).
      if(!is.null(additional_logic_advanced_info)){
        for(rule in additional_logic_advanced_info){
          if(rule$is_hard) next
          when_val <- tryCatch(
            isTRUE(rlang::eval_tidy(
              rlang::f_rhs(rule$when_formula),
              data = seg[["data"]][["with_solutions"]][i, , drop = FALSE]
            )),
            error = function(e) FALSE
          )
          if(!isTRUE(when_val)) next
          if(isTRUE(apply_logic_only_to_max)){
            if(is.na(base_argmax_seg) || !(base_argmax_seg %in% rule$from_lda)) next
          }
          to_idx <- match(rule$to_seg, segments)
          if(is.na(to_idx)) next
          for(fl in rule$from_lda){
            from_idx <- match(fl, segments)
            if(!is.na(from_idx)){
              probs[to_idx]   <- probs[to_idx] + probs[from_idx]
              probs[from_idx] <- 0
            }
          }
        }
      }

      for(k in seq_along(all_prob_cols)){
        data_solution_check[[all_prob_cols[k]]][i] <- probs[k]
      }

      # Final classification for this row: argmax of redistributed probs,
      # then overridden by the first matching hard advanced rule (case_when
      # semantics — sequential, first match wins, fall-through to argmax).
      final_seg_i <- if(any(probs > 0)) as.numeric(segments[which.max(probs)]) else NA_real_
      if(!is.null(additional_logic_advanced_info)){
        for(rule in additional_logic_advanced_info){
          if(!rule$is_hard) next
          when_val <- tryCatch(
            isTRUE(rlang::eval_tidy(
              rlang::f_rhs(rule$when_formula),
              data = seg[["data"]][["with_solutions"]][i, , drop = FALSE]
            )),
            error = function(e) FALSE
          )
          if(isTRUE(when_val)){
            final_seg_i <- as.numeric(rule$to_seg)
            break
          }
        }
      }
      if(!is.na(final_seg_i)){
        data_solution_check[["seg"]][i] <- final_seg_i
      }
    }

    data_solution_check <- data_solution_check[, c(all_prob_cols, "seg")]
  }

  # back-compat name for downstream code that already references this variable
  additional_logic_new_segs <- combined_new_segs


  #######################
  # ui_groups — validate (rendering happens in individual_ui_tool / documentation_tool)
  #######################

  ui_groups_info <- NULL
  ui_grouped_vars <- character(0)

  if(!is.null(ui_groups)){
    if(!is.data.frame(ui_groups)){
      stop("`ui_groups` must be a tibble / data.frame.")
    }
    required_cols_ui <- c("label", "type", "vars", "no_selection_allowed")
    missing_cols <- setdiff(required_cols_ui, names(ui_groups))
    if(length(missing_cols) > 0){
      stop(glue::glue("`ui_groups` is missing required column(s): {paste(missing_cols, collapse = ', ')}."))
    }

    valid_types <- c("multi_select", "mece")
    bad_types <- ui_groups$type[!ui_groups$type %in% valid_types]
    if(length(bad_types) > 0){
      stop(glue::glue("`ui_groups$type` must be one of {paste(valid_types, collapse = '/', sep = '')}; got {paste(unique(bad_types), collapse = ', ')}."))
    }

    if(!is.logical(ui_groups$no_selection_allowed) || any(is.na(ui_groups$no_selection_allowed))){
      stop("`ui_groups$no_selection_allowed` must be logical (TRUE/FALSE) with no NAs.")
    }

    valid_var_pool <- c(inputs, if(!is.null(additional_questions)) names(additional_questions) else character(0))

    for(g in seq_len(nrow(ui_groups))){
      g_vars <- ui_groups$vars[[g]]
      # tribble's list(c(...)) wraps the vector in a length-1 list — unwrap
      if(is.list(g_vars) && length(g_vars) == 1 && is.character(g_vars[[1]])){
        g_vars <- g_vars[[1]]
      }
      if(!is.character(g_vars) || length(g_vars) < 2){
        stop(glue::glue("`ui_groups[[{g}]]$vars` must be a character vector of at least 2 variable names."))
      }
      bad_vars <- g_vars[!g_vars %in% valid_var_pool]
      if(length(bad_vars) > 0){
        stop(glue::glue(
          "`ui_groups` row '{ui_groups$label[g]}' references variable(s) that aren't LDA inputs or `additional_questions` names: ",
          "{paste(bad_vars, collapse = ', ')}."
        ))
      }
      if(any(g_vars %in% ui_grouped_vars)){
        dup <- intersect(g_vars, ui_grouped_vars)
        stop(glue::glue("`ui_groups` variable(s) appear in more than one group: {paste(dup, collapse = ', ')}."))
      }
      ui_grouped_vars <- c(ui_grouped_vars, g_vars)
    }

    ui_groups_info <- purrr::map(seq_len(nrow(ui_groups)), function(g){
      g_vars <- ui_groups$vars[[g]]
      if(is.list(g_vars) && length(g_vars) == 1 && is.character(g_vars[[1]])){
        g_vars <- g_vars[[1]]
      }
      list(
        label                = ui_groups$label[g],
        type                 = ui_groups$type[g],
        vars                 = g_vars,
        no_selection_allowed = isTRUE(ui_groups$no_selection_allowed[g])
      )
    })
  }


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

  # Master list of every data-input variable rendered on Bulk and labeled with
  # a continuous Q numbering: LDA inputs first, then additional_questions vars.
  aq_var_names_master <- if(!is.null(additional_questions)) names(additional_questions) else character(0)
  all_data_input_vars <- c(inputs_raw, aq_var_names_master)
  all_q_names <- c(
    clean_variable_names,
    if(length(aq_var_names_master) > 0) glue::glue("Q{(length(inputs) + 1):(length(inputs) + length(aq_var_names_master))}") else character(0)
  )

  q_label_for_var <- function(v){
    idx_lda <- match(v, inputs)
    if(!is.na(idx_lda)) return(as.character(clean_variable_names[idx_lda]))
    idx_aq <- match(v, aq_var_names_master)
    if(!is.na(idx_aq)) return(as.character(all_q_names[length(inputs) + idx_aq]))
    v
  }


  #######################
  # additional_logic_advanced — formula → Excel translator
  #
  # Walks the rhs of a one-sided formula and emits an Excel formula string
  # (or vector of strings, when var_to_cell returns vectorized cell refs).
  # Supported subset:
  #   symbols (var names)              → var_to_cell(name)
  #   TRUE / FALSE / T / F             → "TRUE" / "FALSE"
  #   numeric / character literals     → as.character(value)
  #   ==, !=, <, <=, >, >=             → corresponding Excel comparators
  #   &, |                             → AND(...), OR(...)
  #   !                                → NOT(...)
  #   x %in% c(v1, v2, ...)            → OR(x=v1, x=v2, ...)
  #   parentheses                      → preserved structurally
  # Unsupported nodes raise an error rather than silently mis-translating.
  #######################

  expr_to_excel <- function(expr, var_to_cell){
    if(is.symbol(expr)){
      nm <- as.character(expr)
      if(nm %in% c("TRUE", "T")) return("TRUE")
      if(nm %in% c("FALSE", "F")) return("FALSE")
      return(var_to_cell(nm))
    }
    if(is.numeric(expr) || is.logical(expr)){
      return(as.character(expr))
    }
    if(is.character(expr)){
      return(glue::glue('"{expr}"'))
    }
    if(is.call(expr)){
      op <- as.character(expr[[1]])
      args <- as.list(expr[-1])

      if(op == "("){
        return(expr_to_excel(args[[1]], var_to_cell))
      }

      binary_ops <- c("==" = "=", "!=" = "<>", "<" = "<", "<=" = "<=", ">" = ">", ">=" = ">=")
      if(op %in% names(binary_ops)){
        lhs <- expr_to_excel(args[[1]], var_to_cell)
        rhs <- expr_to_excel(args[[2]], var_to_cell)
        return(glue::glue("{lhs}{binary_ops[op]}{rhs}"))
      }

      if(op == "&"){
        lhs <- expr_to_excel(args[[1]], var_to_cell)
        rhs <- expr_to_excel(args[[2]], var_to_cell)
        return(glue::glue("AND({lhs}, {rhs})"))
      }
      if(op == "|"){
        lhs <- expr_to_excel(args[[1]], var_to_cell)
        rhs <- expr_to_excel(args[[2]], var_to_cell)
        return(glue::glue("OR({lhs}, {rhs})"))
      }
      if(op == "!"){
        sub <- expr_to_excel(args[[1]], var_to_cell)
        return(glue::glue("NOT({sub})"))
      }
      if(op == "%in%"){
        lhs       <- expr_to_excel(args[[1]], var_to_cell)
        rhs_lit   <- tryCatch(eval(args[[2]]), error = function(e) NULL)
        if(is.null(rhs_lit)){
          stop("`%in%` rhs in additional_logic_advanced$when must be a literal vector (e.g. c(1, 2, 3)).")
        }
        conds <- purrr::map(rhs_lit, ~ glue::glue("{lhs}={.x}"))
        combined <- purrr::reduce(conds, function(a, b) paste0(a, ", ", b))
        return(glue::glue("OR({combined})"))
      }

      stop(glue::glue(
        "Unsupported operator `{op}` in additional_logic_advanced$when. ",
        "Supported: ==, !=, <, <=, >, >=, &, |, !, %in%, parentheses."
      ))
    }
    stop("Unsupported expression in additional_logic_advanced$when.")
  }

  when_to_excel <- function(when_formula, var_to_cell){
    expr_to_excel(rlang::f_rhs(when_formula), var_to_cell)
  }


  # Human-readable rendering of a `when` formula with Q labels substituted
  # for the underlying variable names — used in the Documentation step text
  # so readers see "Q24 == 1 | Q25 == 1" instead of "VT02 == 1 | VT03 == 1".
  expr_to_human_text <- function(expr){
    if(is.symbol(expr)){
      nm <- as.character(expr)
      if(nm %in% c("TRUE", "T"))  return("TRUE")
      if(nm %in% c("FALSE", "F")) return("FALSE")
      if(nm == "NA")              return("NA")
      return(q_label_for_var(nm))
    }
    if(is.numeric(expr) || is.logical(expr)) return(as.character(expr))
    if(is.character(expr))                   return(glue::glue('"{expr}"'))
    if(is.call(expr)){
      op   <- as.character(expr[[1]])
      args <- as.list(expr[-1])

      if(op == "("){
        return(glue::glue("({expr_to_human_text(args[[1]])})"))
      }
      binary_ops <- c("==", "!=", "<", "<=", ">", ">=", "&", "|", "&&", "||")
      if(op %in% binary_ops){
        lhs <- expr_to_human_text(args[[1]])
        rhs <- expr_to_human_text(args[[2]])
        return(glue::glue("{lhs} {op} {rhs}"))
      }
      if(op == "!"){
        return(glue::glue("!{expr_to_human_text(args[[1]])}"))
      }
      if(op == "%in%"){
        lhs     <- expr_to_human_text(args[[1]])
        rhs_lit <- tryCatch(eval(args[[2]]), error = function(e) NULL)
        if(is.null(rhs_lit)) return(glue::glue("{lhs} %in% <unknown>"))
        return(glue::glue("{lhs} %in% c({paste(rhs_lit, collapse = ', ')})"))
      }
      # fallback for unsupported nodes — preserves the raw expression
      return(paste(deparse(expr), collapse = " "))
    }
    as.character(expr)
  }

  when_to_human_text <- function(when_formula){
    expr_to_human_text(rlang::f_rhs(when_formula))
  }


  #######################
  # build ind_ui (polar block only — shared by individual and documentation tools)
  #######################

  has_non_polar_outer <- !is.null(non_polar_inputs_table)

  # The polar tibble reserves a spacer column between Side A and the polar
  # points whenever a non-polar OR additional_questions block will render as
  # its own individual block (those blocks reuse the spacer position for their
  # Code column). Vars folded into a ui_groups block do not render their own
  # block, so they don't need the spacer.
  has_ungrouped_non_polar_outer <- has_non_polar_outer &&
    length(setdiff(non_polar_inputs_table$var, ui_grouped_vars)) > 0
  has_ungrouped_aq_outer <- !is.null(additional_questions_info) &&
    length(additional_questions_info) > 0 &&
    any(!purrr::map_chr(additional_questions_info, "var") %in% ui_grouped_vars)
  polar_spacer_needed <- has_ungrouped_non_polar_outer || has_ungrouped_aq_outer

  if(length(polar_inputs) > 0 && !is.null(inputs_table)){
    if(polar_spacer_needed){
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
          if(polar_spacer_needed) " " else NULL,
          "Agree Much \nMore \n<<", "Agree Somewhat \nMore \n<<",
          "Agree Somewhat \nMore \n>>", "Agree Much \nMore \n>>",
          "Side B"
        ))
    }else if(length(polar_points) == 2){
      ind_ui <- ind_ui %>%
        setNames(c(
          "Question", "Survey Variable", "Side A",
          if(polar_spacer_needed) " " else NULL,
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
    # add a spacer column between Side A and the polar point cols only when at
    # least one non-polar OR additional_questions var is rendered as its own
    # individual block (those blocks reuse the spacer position for their hidden
    # Code column). Mirrors the polar_spacer_needed flag the outer tibble uses
    # so the writeData column shape and the col_* offsets stay in sync.
    polar_spacer_offset <- if(polar_spacer_needed) 2L else 1L
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
    # grey background canvas — fitted to this sheet's actual extent
    #
    # Eagerly compute upper-bound canvas dims from the inputs/layout constants
    # set above, then paint the canvas BEFORE any block writes so that
    # block-specific styles (white fills, header fills, etc.) override the
    # canvas where they apply.
    #######################

    # Left-side blocks: only count the ones that actually render as their own
    # individual block (vars folded into a ui_groups block don't render
    # individually — they appear inside the group block instead).
    polar_n_canvas <- if(!is.null(inputs_table)) nrow(inputs_table) else 0L
    ungrouped_np_canvas <- if(!is.null(non_polar_inputs_info)){
      purrr::keep(non_polar_inputs_info, ~ !.x$var %in% ui_grouped_vars)
    }else list()
    ungrouped_aq_canvas <- if(!is.null(additional_questions_info)){
      purrr::keep(additional_questions_info, ~ !.x$var %in% ui_grouped_vars)
    }else list()

    # Per-block height (header + n data rows for np/aq, title + header + n_vars
    # for groups). Inter-block gap = 2 empty rows.
    np_block_heights_canvas <- purrr::map_int(ungrouped_np_canvas, ~ as.integer(.x$n_values + 1))
    aq_block_heights_canvas <- purrr::map_int(ungrouped_aq_canvas, ~ as.integer(nrow(.x$value_table) + 1))
    g_block_heights_canvas  <- if(!is.null(ui_groups_info)){
      purrr::map_int(ui_groups_info, ~ as.integer(length(.x$vars) + 2))
    }else integer(0)

    n_indiv_blocks_canvas <- length(np_block_heights_canvas) + length(aq_block_heights_canvas) + length(g_block_heights_canvas)
    sum_block_heights_canvas <- sum(np_block_heights_canvas, aq_block_heights_canvas, g_block_heights_canvas)

    polar_last_canvas <- row_ind_first + polar_n_canvas - 1
    left_side_bottom_canvas <- if(n_indiv_blocks_canvas == 0){
      polar_last_canvas
    }else{
      # First block starts at polar_last + 3 (2 empty rows gap). Successive
      # blocks add their height + 2-row gap (= height + 2). The last block
      # contributes only its height (no trailing gap), which we get by
      # subtracting one trailing 2 from the inter-block sum.
      polar_last_canvas + 3 + sum_block_heights_canvas + (n_indiv_blocks_canvas - 1) * 2 - 1
    }

    # Right-side engine bottom: bottom row of the controls box (1 row past
    # row_ind_control_q_feedback = row_ind_engine_seg_qc).
    engine_bottom_canvas <- row_ind_engine_seg_qc + 1

    # Extend canvas exactly 1 row past the deepest content and 1 col past the
    # rightmost engine col (col_ind_engine_qc_number is the rightmost cell of
    # the engine main; the controls box sits to its left and finishes earlier).
    canvas_max_row <- max(left_side_bottom_canvas, engine_bottom_canvas) + 1
    canvas_max_col <- col_ind_engine_qc_number + 1

    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(fgFill = oxl_colorscale_grey(1)),
      rows = seq(1, canvas_max_row),
      cols = seq(1, canvas_max_col),
      gridExpand = TRUE
    )


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


    # Precompute each ungrouped non-polar / additional-questions block's
    # first-data-row in the Documentation sheet. We mirror the doc tool's
    # cursor math here so that the UI tool — which runs before the doc tool —
    # can build cross-sheet '='Documentation'!<col><row>' references that
    # resolve correctly when Excel opens the workbook.
    doc_row_doc_last  <- doc_q_row_first + (if(!is.null(inputs_table)) nrow(inputs_table) else 0L) - 1
    doc_col_q_letter        <- num2let(start_col)
    doc_col_var_letter      <- num2let(start_col + 1)
    doc_col_text_letter     <- num2let(start_col + 2)
    doc_col_response_letter <- num2let(start_col + 2 + doc_label_cell_merge + 1)
    doc_col_answer_letter   <- num2let(start_col + 2 + doc_label_cell_merge + 2)

    doc_block_first_row <- list()  # var_name -> first data row (= row_d_f) in doc
    doc_group_title_row <- list()  # group_label -> title row in doc
    {
      cursor_dq <- doc_row_doc_last + 3
      if(!is.null(non_polar_inputs_info)){
        for(info in non_polar_inputs_info){
          if(info$var %in% ui_grouped_vars) next
          doc_block_first_row[[info$var]] <- as.integer(cursor_dq + 1)
          cursor_dq <- cursor_dq + as.integer(info$n_values) + 3L
        }
      }
      if(!is.null(additional_questions_info)){
        for(info in additional_questions_info){
          if(info$var %in% ui_grouped_vars) next
          doc_block_first_row[[info$var]] <- as.integer(cursor_dq + 1)
          cursor_dq <- cursor_dq + as.integer(nrow(info$value_table)) + 3L
        }
      }
      # Group blocks: title at cursor_dq, header at cursor_dq + 1, first var
      # data row at cursor_dq + 2; each subsequent var data row +1. Per-block
      # advance = n_vars + 4 (title + header + n_vars + 2-row gap).
      if(!is.null(ui_groups_info)){
        for(g_info in ui_groups_info){
          doc_group_title_row[[g_info$label]] <- as.integer(cursor_dq)
          first_var_row <- as.integer(cursor_dq + 2)
          for(v_idx in seq_along(g_info$vars)){
            v_name <- g_info$vars[v_idx]
            doc_block_first_row[[v_name]] <- as.integer(first_var_row + v_idx - 1)
          }
          cursor_dq <- cursor_dq + length(g_info$vars) + 4L
        }
      }
    }


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
        if(info$var %in% ui_grouped_vars) next  # rendered inside a ui_groups block
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

        # Q label, survey var, question text pulled from Documentation so any
        # edits there propagate. doc_block_first_row[[var]] points at the
        # first data row of this block in the doc np layout.
        doc_first_row_np <- doc_block_first_row[[info$var]]
        openxlsx::writeFormula(
          wb, sheet_name,
          x = glue::glue("'{sheet_name_doc}'!{doc_col_q_letter}{doc_first_row_np}"),
          startRow = row_np_first, startCol = col_np_q
        )
        openxlsx::writeFormula(
          wb, sheet_name,
          x = glue::glue("'{sheet_name_doc}'!{doc_col_var_letter}{doc_first_row_np}"),
          startRow = row_np_first, startCol = col_np_var
        )
        openxlsx::writeFormula(
          wb, sheet_name,
          x = glue::glue("'{sheet_name_doc}'!{doc_col_text_letter}{doc_first_row_np}"),
          startRow = row_np_first, startCol = col_np_text
        )

        # merge Q, var, text vertically across all value rows
        if(n_vals > 1){
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_np_first, row_np_last), cols = col_np_q)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_np_first, row_np_last), cols = col_np_var)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_np_first, row_np_last), cols = col_np_text)
        }

        # determine first respondent's answer and mark x
        first_val <- seg[["data"]][["with_solutions"]][[info$var]][1]
        x_row_idx <- which(info$value_table$code == first_val)

        # Code + response label per value row pulled from Documentation. The
        # doc block writes "Answer" (= numeric code) at doc_col_answer_letter
        # and "Response" (= label) at doc_col_response_letter on rows
        # doc_first_row_np .. doc_first_row_np + n_vals - 1.
        # The "x" pre-fill at col_np_answer remains writeData since it's a
        # user-editable input, not metadata.
        for(v in seq_len(n_vals)){
          r <- row_np_first + v - 1
          doc_v_row <- doc_first_row_np + v - 1
          openxlsx::writeFormula(
            wb, sheet_name,
            x = glue::glue("'{sheet_name_doc}'!{doc_col_answer_letter}{doc_v_row}"),
            startRow = r, startCol = col_np_code
          )
          openxlsx::writeFormula(
            wb, sheet_name,
            x = glue::glue("'{sheet_name_doc}'!{doc_col_response_letter}{doc_v_row}"),
            startRow = r, startCol = col_np_response
          )
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

        row_np_cursor <- row_np_last + 3
      }

      np_block_bottom <- row_np_cursor - 3
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
        if(info$var %in% ui_grouped_vars) next  # rendered inside a ui_groups block
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

        # Q label, survey var, question text pulled from Documentation. The
        # AQ block in the doc layout follows the same Q/var/text/response/
        # answer columns as the non-polar block, so we reuse the same column
        # letters and the same first-data-row lookup.
        doc_first_row_aq <- doc_block_first_row[[info$var]]
        openxlsx::writeFormula(
          wb, sheet_name,
          x = glue::glue("'{sheet_name_doc}'!{doc_col_q_letter}{doc_first_row_aq}"),
          startRow = row_aq_f, startCol = col_aq_q
        )
        openxlsx::writeFormula(
          wb, sheet_name,
          x = glue::glue("'{sheet_name_doc}'!{doc_col_var_letter}{doc_first_row_aq}"),
          startRow = row_aq_f, startCol = col_aq_var
        )
        openxlsx::writeFormula(
          wb, sheet_name,
          x = glue::glue("'{sheet_name_doc}'!{doc_col_text_letter}{doc_first_row_aq}"),
          startRow = row_aq_f, startCol = col_aq_text
        )

        if(n_vals > 1){
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_aq_f, row_aq_l), cols = col_aq_q)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_aq_f, row_aq_l), cols = col_aq_var)
          openxlsx::mergeCells(wb, sheet_name, rows = seq(row_aq_f, row_aq_l), cols = col_aq_text)
        }

        first_val_aq <- seg[["data"]][["with_solutions"]][[info$var]][1]
        x_row_idx_aq <- which(info$value_table$code == first_val_aq)

        # Code + response label per value row pulled from Documentation; the
        # "x" pre-fill at col_aq_answer remains writeData (user input).
        for(v in seq_len(n_vals)){
          r <- row_aq_f + v - 1
          doc_v_row <- doc_first_row_aq + v - 1
          openxlsx::writeFormula(
            wb, sheet_name,
            x = glue::glue("'{sheet_name_doc}'!{doc_col_answer_letter}{doc_v_row}"),
            startRow = r, startCol = col_aq_code
          )
          openxlsx::writeFormula(
            wb, sheet_name,
            x = glue::glue("'{sheet_name_doc}'!{doc_col_response_letter}{doc_v_row}"),
            startRow = r, startCol = col_aq_response
          )
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

        row_aq_cursor <- row_aq_l + 3
      }
    }


    #######################
    # ui_groups blocks (merged multi_select / mece blocks)
    #######################

    ui_grouped_vars_cells <- list()

    if(!is.null(ui_groups_info) && length(ui_groups_info) > 0){
      col_g_q       <- col_ind_clean_var_number
      col_g_var     <- col_ind_survey_var_number
      col_g_text    <- col_ind_label_left_number
      col_g_answer  <- col_ind_label_right_number

      style_g_cell <- openxlsx::createStyle(
        fgFill = "white", halign = "center", valign = "center",
        border = "TopBottomLeftRight", borderStyle = "thin"
      )

      # cursor: bottom of whatever individual blocks rendered (np / aq), or row_polar_last
      np_max_row <- if(!is.null(np_info_with_cells)){
        rendered_np <- purrr::compact(np_info_with_cells)
        if(length(rendered_np) > 0){
          max(purrr::map_int(rendered_np, ~ as.integer(.x$row_last)))
        }else 0L
      }else 0L
      aq_max_row <- if(!is.null(aq_info_with_cells)){
        rendered_aq <- purrr::compact(aq_info_with_cells)
        if(length(rendered_aq) > 0){
          max(purrr::map_int(rendered_aq, ~ as.integer(.x$row_last)))
        }else 0L
      }else 0L
      row_g_cursor <- max(row_polar_last, np_block_bottom, np_max_row, aq_max_row) + 3

      for(g_idx in seq_along(ui_groups_info)){
        g_info  <- ui_groups_info[[g_idx]]
        n_g_vars <- length(g_info$vars)
        row_g_h  <- row_g_cursor
        row_g_f  <- row_g_h + 1
        row_g_l  <- row_g_h + n_g_vars

        # title + header row. Title text pulled from Documentation so that
        # editing the doc title (e.g. relabeling "Activities") propagates here.
        openxlsx::mergeCells(wb, sheet_name,
          rows = row_g_h - 1,
          cols = seq(col_g_q, col_g_answer)
        )
        doc_title_row_g <- doc_group_title_row[[g_info$label]]
        openxlsx::writeFormula(wb, sheet_name,
          x = glue::glue("'{sheet_name_doc}'!{doc_col_q_letter}{doc_title_row_g}"),
          startRow = row_g_h - 1,
          startCol = col_g_q
        )
        # Match the column-header row below (style_header) so the title +
        # column-header pair reads as a single grey band rather than two
        # different shades stacked on top of each other.
        openxlsx::addStyle(wb, sheet_name,
          style = style_header,
          rows = row_g_h - 1,
          cols = seq(col_g_q, col_g_answer),
          gridExpand = TRUE, stack = TRUE
        )

        header_tbl_g <- tibble::tibble(
          "Question"        = NA_character_,
          "Survey Variable" = NA_character_,
          " "               = NA_character_,
          "Answer"          = NA_character_
        )
        # write header at row_g_h spanning Q | SurveyVar | (merged label) | Answer
        openxlsx::writeData(wb, sheet_name,
          x = "Question",
          startRow = row_g_h, startCol = col_g_q, colNames = FALSE
        )
        openxlsx::writeData(wb, sheet_name,
          x = "Survey\nVariable",
          startRow = row_g_h, startCol = col_g_var, colNames = FALSE
        )
        openxlsx::writeData(wb, sheet_name,
          x = "Answer",
          startRow = row_g_h, startCol = col_g_answer, colNames = FALSE
        )
        openxlsx::mergeCells(wb, sheet_name,
          rows = row_g_h, cols = seq(col_g_text, col_g_answer - 1)
        )
        openxlsx::addStyle(wb, sheet_name,
          style = style_header,
          rows = row_g_h,
          cols = seq(col_g_q, col_g_answer),
          gridExpand = TRUE, stack = TRUE
        )

        # determine which vars to pre-fill with "x" for the first respondent.
        # For mece groups at most one will be 1 by design (so first-match is
        # fine); for multi_select groups multiple can be 1 and we must mark
        # every one — otherwise the engine recode for unmarked-but-actually-
        # selected vars will be 0 and the UI exp_score will diverge from
        # Bulk for the same respondent.
        first_resp_selected_vars <- character(0)
        for(v in g_info$vars){
          val <- seg[["data"]][["with_solutions"]][[v]][1]
          if(!is.na(val) && val == 1){
            first_resp_selected_vars <- c(first_resp_selected_vars, v)
            if(g_info$type == "mece") break
          }
        }

        for(v_idx in seq_along(g_info$vars)){
          v_name <- g_info$vars[v_idx]
          r      <- row_g_f + v_idx - 1

          # Q label, var, and var-question label all pulled from Documentation
          # so any edits there propagate. The doc tool resolves the v_label
          # itself (spec profiles → dictionary → var) and writes it at
          # doc_block_first_row[[v_name]] in the col_doc_g_text column.
          doc_var_row <- doc_block_first_row[[v_name]]
          openxlsx::writeFormula(wb, sheet_name,
            x = glue::glue("'{sheet_name_doc}'!{doc_col_q_letter}{doc_var_row}"),
            startRow = r, startCol = col_g_q
          )
          openxlsx::writeFormula(wb, sheet_name,
            x = glue::glue("'{sheet_name_doc}'!{doc_col_var_letter}{doc_var_row}"),
            startRow = r, startCol = col_g_var
          )
          openxlsx::writeFormula(wb, sheet_name,
            x = glue::glue("'{sheet_name_doc}'!{doc_col_text_letter}{doc_var_row}"),
            startRow = r, startCol = col_g_text
          )

          # merge text cells from col_g_text to col_g_answer - 1
          openxlsx::mergeCells(wb, sheet_name,
            rows = r, cols = seq(col_g_text, col_g_answer - 1)
          )

          # pre-fill x for every var the first respondent selected
          if(v_name %in% first_resp_selected_vars){
            openxlsx::writeData(wb, sheet_name, x = "x", startRow = r, startCol = col_g_answer, colNames = FALSE)
          }

          ui_grouped_vars_cells[[v_name]] <- list(
            group_idx     = g_idx,
            group_label   = g_info$label,
            group_type    = g_info$type,
            group_no_sel  = g_info$no_selection_allowed,
            var           = v_name,
            row           = r,
            answer_cell   = glue::glue("${num2let(col_g_answer)}${r}"),
            answer_range  = glue::glue("${num2let(col_g_answer)}${row_g_f}:${num2let(col_g_answer)}${row_g_l}")
          )
        }

        # styles per column
        openxlsx::addStyle(wb, sheet_name, style = style_g_cell,
          rows = seq(row_g_f, row_g_l),
          cols = seq(col_g_q, col_g_answer - 1),
          gridExpand = TRUE, stack = TRUE
        )
        openxlsx::addStyle(wb, sheet_name,
          style = openxlsx::createStyle(
            fgFill = "white",
            halign = "center", valign = "center",
            border = "TopBottomLeftRight", borderStyle = "thin"
          ),
          rows = seq(row_g_f, row_g_l),
          cols = col_g_answer,
          gridExpand = TRUE, stack = TRUE
        )

        # outer borders (whole block + data only)
        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_g_h - 1, row_end = row_g_l,
          col_start = col_g_q, col_end = col_g_answer
        )
        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_g_f, row_end = row_g_l,
          col_start = col_g_q, col_end = col_g_answer
        )

        # MECE multi-x warning (yellow)
        if(g_info$type == "mece"){
          group_answer_abs <- glue::glue("${num2let(col_g_answer)}${row_g_f}:${num2let(col_g_answer)}${row_g_l}")
          openxlsx::conditionalFormatting(wb, sheet_name,
            cols = col_g_answer,
            rows = seq(row_g_f, row_g_l),
            rule = glue::glue('AND(${num2let(col_ind_engine_controls_number)}${row_ind_engine_seg_qc} = TRUE, COUNTIF({group_answer_abs}, "x") > 1)'),
            style = oxl_style_cell_neutral(textDecoration = "bold", conditional = TRUE)
          )
        }

        row_g_cursor <- row_g_l + 3
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

      # Question col includes "Constant" on the last row so that downstream
      # INDEX/MATCH lookups against the Question column can find the constant
      # row by name (rather than relying on positional row alignment between
      # the engine grid and the Solution Coefficient Function block).
      temp_coef_func <- tibble::tibble(
        "Question" = c(clean_variable_names, "Constant")
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
    # Mirror the doc-sheet layout exactly: 3 empty rows after polar, 1 empty
    # row between regions, 1 empty row before the coef function label.
    np_advance_ui <- 0
    N_np_ui_eff   <- 0
    if(!is.null(non_polar_inputs_info) && length(non_polar_inputs_info) > 0){
      ungrouped_np_ui <- purrr::keep(non_polar_inputs_info, ~ !.x$var %in% ui_grouped_vars)
      N_np_ui_eff      <- length(ungrouped_np_ui)
      if(N_np_ui_eff > 0){
        total_vals_np_ui <- sum(purrr::map_int(ungrouped_np_ui, ~ as.integer(.x$n_values)))
        np_advance_ui    <- total_vals_np_ui + 3 * N_np_ui_eff
      }
    }
    aq_advance_ui <- 0
    N_aq_ui_eff   <- 0
    if(!is.null(additional_questions_info) && length(additional_questions_info) > 0){
      ungrouped_aq_ui <- purrr::keep(additional_questions_info, ~ !.x$var %in% ui_grouped_vars)
      N_aq_ui_eff      <- length(ungrouped_aq_ui)
      if(N_aq_ui_eff > 0){
        total_vals_aq_ui <- sum(purrr::map_int(ungrouped_aq_ui, ~ as.integer(nrow(.x$value_table))))
        aq_advance_ui    <- total_vals_aq_ui + 3 * N_aq_ui_eff
      }
    }
    g_advance_ui <- 0
    N_g_ui       <- 0
    if(!is.null(ui_groups_info) && length(ui_groups_info) > 0){
      N_g_ui          <- length(ui_groups_info)
      total_vars_g_ui <- sum(purrr::map_int(ui_groups_info, ~ as.integer(length(.x$vars))))
      g_advance_ui    <- total_vars_g_ui + 4 * N_g_ui
    }

    doc_row_doc_last <- doc_q_row_first + nrow(inputs_table) - 1
    any_doc_region_ui <- (N_np_ui_eff + N_aq_ui_eff + N_g_ui) > 0
    doc_coef_row_header <- if(any_doc_region_ui){
      doc_row_doc_last + 4 + np_advance_ui + aq_advance_ui + g_advance_ui
    }else{
      doc_row_doc_last + 4
    }
    doc_coef_data_rows <- seq(doc_coef_row_header + 1, doc_coef_row_header + nrow(coef_func))

    # Pull the UI's Solution Coefficient Function block (Variable + Seg cols)
    # from Documentation via 2D INDEX/MATCH — row matched against Doc's
    # Question column, col matched against Doc's column-header row. Robust
    # to edits or column re-orderings on the Doc sheet: as long as the
    # Question labels (e.g. "Q1", "Q2", ..., "Constant") and column headers
    # (e.g. "Variable", "Seg_1", ...) still exist, the UI resolves them.
    doc_q_col_letter      <- num2let(start_col)
    doc_first_value_col   <- start_col + 1                       # Variable col onward
    doc_last_value_col    <- start_col + 1 + length(segments)    # last Seg col
    doc_first_value_letter <- num2let(doc_first_value_col)
    doc_last_value_letter  <- num2let(doc_last_value_col)
    doc_data_first_row    <- doc_coef_data_rows[1]
    doc_data_last_row     <- doc_coef_data_rows[length(doc_coef_data_rows)]

    doc_q_range_abs <- glue::glue(
      "'{sheet_name_doc}'!${doc_q_col_letter}${doc_data_first_row}:",
      "${doc_q_col_letter}${doc_data_last_row}"
    )
    doc_value_2d_range <- glue::glue(
      "'{sheet_name_doc}'!${doc_first_value_letter}${doc_data_first_row}:",
      "${doc_last_value_letter}${doc_data_last_row}"
    )
    doc_header_row_range <- glue::glue(
      "'{sheet_name_doc}'!${doc_first_value_letter}${doc_coef_row_header}:",
      "${doc_last_value_letter}${doc_coef_row_header}"
    )

    ui_scf_q_letter <- num2let(col_ind_engine_clean_var_number)
    ui_scf_q_cells  <- glue::glue("${ui_scf_q_letter}{seq(row_header + 1, row_header + nrow(coef_func))}")

    # Variable column lookup: row keyed by Q label, col keyed by the UI's
    # own "Variable" header cell so a Doc rearrangement doesn't break it.
    ui_var_header_cell <- glue::glue("${num2let(col_ind_engine_clean_var_number + 1)}${row_header}")
    openxlsx::writeFormula(
      wb, sheet_name,
      x = glue::glue(
        "INDEX({doc_value_2d_range}, ",
        "MATCH({ui_scf_q_cells}, {doc_q_range_abs}, 0), ",
        "MATCH({ui_var_header_cell}, {doc_header_row_range}, 0))"
      ),
      startRow = row_header + 1,
      startCol = col_ind_engine_clean_var_number + 1
    )

    for(s in seq_len(length(segments))){
      ui_seg_header_cell <- glue::glue("${num2let(col_ind_engine_clean_var_number + 1 + s)}${row_header}")
      openxlsx::writeFormula(
        wb, sheet_name,
        x = glue::glue(
          "INDEX({doc_value_2d_range}, ",
          "MATCH({ui_scf_q_cells}, {doc_q_range_abs}, 0), ",
          "MATCH({ui_seg_header_cell}, {doc_header_row_range}, 0))"
        ),
        startRow = row_header + 1,
        startCol = col_ind_engine_clean_var_number + 1 + s
      )
    }


    for(i in seq(row_ind_first, row_ind_last)){

      input_idx <- i - row_ind_first + 1
      var_name_eng <- inputs[input_idx]
      is_grouped <- var_name_eng %in% names(ui_grouped_vars_cells)
      is_polar   <- !is_grouped && input_idx <= length(polar_inputs)

      if(is_grouped){
        gcell <- ui_grouped_vars_cells[[var_name_eng]]$answer_cell
        answer_formula <- glue::glue('IF({gcell}="x", 1, 0)')
        recode_formula <- glue::glue('IF({gcell}="x", 1, 0)')

        # QC for grouped vars: enforce the group-level constraint (matches the
        # bulk Calculation Ready logic).
        #   mece   + no_selection_allowed=TRUE  → fail when count > 1
        #   mece   + no_selection_allowed=FALSE → fail when count != 1
        #   multi  + no_selection_allowed=TRUE  → no constraint
        #   multi  + no_selection_allowed=FALSE → fail when count < 1
        g_info_for_var <- ui_grouped_vars_cells[[var_name_eng]]
        g_count_expr   <- glue::glue('COUNTIF({g_info_for_var$answer_range},"x")')
        qc_formula <- if(g_info_for_var$group_type == "mece"){
          if(isTRUE(g_info_for_var$group_no_sel)){
            glue::glue('IF({g_count_expr}<=1, 1, 0)')
          }else{
            glue::glue('IF({g_count_expr}=1, 1, 0)')
          }
        }else{  # multi_select
          if(isTRUE(g_info_for_var$group_no_sel)){
            "1"
          }else{
            glue::glue('IF({g_count_expr}>=1, 1, 0)')
          }
        }
        recode_array   <- FALSE
      }else if(is_polar){
        answer_formula <- glue::glue('MATCH("x", {num2let(col_ind_label_point_first_number)}{i}:{num2let(col_ind_label_point_last_number)}{i}, 0)')
        recode_formula <- glue::glue('INDEX(${num2let(col_ind_label_point_first_number)}${row_ind_recode}:${num2let(col_ind_label_point_last_number)}${row_ind_recode},,{num2let(col_ind_engine_answer_number)}{i})')
        qc_formula     <- glue::glue('COUNTIF({num2let(col_ind_label_point_first_number)}{i}:{num2let(col_ind_label_point_last_number)}{i},"x")')
        recode_array   <- TRUE
      }else{
        # locate the np_info_with_cells entry for this var. Some slots are
        # NULL because their var was folded into a ui_groups block at line
        # 1810 (the preallocated list is left NULL on skip). Use
        # `.default = NA_character_` so map_chr tolerates the gaps instead
        # of erroring "Result must be length 1, not 0".
        np_idx <- which(purrr::map_chr(np_info_with_cells, "var",
          .default = NA_character_) == var_name_eng)
        np <- np_info_with_cells[[np_idx[1]]]
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



    # Engine multiplication grid (per-cell coef * recoded-input products).
    # Each cell looks up the Solution Coefficient Function block by the
    # engine row's Question label (Q1, Q2, ..., Constant) using INDEX/MATCH
    # — robust to row-order changes in the SCF block.
    #   coef value  = INDEX(<seg_col_range_in_SCF>, MATCH(<engine_q_cell>, <q_range_in_SCF>, 0))
    #   recode mult = INDEX(<recode_col_range_in_SCF>, MATCH(<engine_q_cell>, <q_range_in_SCF>, 0))
    # Constant-row recode is 1 (written elsewhere) so the multiplication
    # naturally yields the seg constant on the engine constant row.
    seq_engine_rows <- seq(row_ind_engine_first, row_ind_engine_last)
    seq_engine_cols <- seq(col_ind_engine_survey_var_number + 1, col_ind_engine_answer_number - 1)

    scf_q_letter      <- num2let(col_ind_engine_clean_var_number)
    scf_var_letter    <- num2let(col_ind_engine_clean_var_number + 1)
    scf_recode_letter <- num2let(col_ind_engine_recode_number)
    scf_first_row     <- row_header + 1
    scf_last_row      <- row_header + nrow(coef_func)
    scf_q_range       <- glue::glue("${scf_q_letter}${scf_first_row}:${scf_q_letter}${scf_last_row}")
    scf_recode_range  <- glue::glue("${scf_recode_letter}${scf_first_row}:${scf_recode_letter}${scf_last_row}")

    # Make the Response Calculations block's Question and Variable columns
    # formulaic — each engine row mirrors the SCF row at the matching offset,
    # so the SCF block stays the single source of truth. Overwrites the
    # hardcoded values just written via temp_coef_func at row_ind_engine_header.
    scf_rows_for_engine <- scf_first_row + (seq_engine_rows - row_ind_engine_first)
    engine_q_formulas   <- glue::glue("={scf_q_letter}{scf_rows_for_engine}")
    engine_var_formulas <- glue::glue("={scf_var_letter}{scf_rows_for_engine}")

    engine_q_var_tbl <- tibble::tibble(
      "Question" = engine_q_formulas,
      "Variable" = engine_var_formulas
    )
    class(engine_q_var_tbl[["Question"]]) <- "formula"
    class(engine_q_var_tbl[["Variable"]]) <- "formula"

    openxlsx::writeData(
      wb, sheet_name, x = engine_q_var_tbl,
      startRow = row_ind_engine_first,
      startCol = col_ind_engine_clean_var_number,
      colNames = FALSE
    )

    engine_q_cells <- glue::glue("${scf_q_letter}{seq_engine_rows}")  # engine row's own Question cell

    # 2D INDEX/MATCH against the Solution Coefficient Function block. Row is
    # matched against SCF's Question column, col is matched against SCF's
    # column-header row. The recode lookup uses the literal "Recode" header
    # so it survives column reordering inside the SCF block.
    scf_first_col_letter <- num2let(col_ind_engine_clean_var_number)
    scf_last_col_letter  <- num2let(col_ind_engine_qc_number)
    scf_data_2d_range    <- glue::glue(
      "${scf_first_col_letter}${scf_first_row}:${scf_last_col_letter}${scf_last_row}"
    )
    scf_header_row_range <- glue::glue(
      "${scf_first_col_letter}${row_header}:${scf_last_col_letter}${row_header}"
    )
    recode_col_match <- glue::glue('MATCH("Recode", {scf_header_row_range}, 0)')

    engine_grid_tbl <- purrr::map(seq_engine_cols, function(x){
      # horizontal key: this column's own header cell (e.g. "Seg_1") on the
      # engine block's header row. Same value as the SCF block's header at
      # the matching column, but matching by header value rather than column
      # position keeps the formula valid even if either block's columns get
      # reordered.
      engine_seg_header_cell <- glue::glue("{num2let(x)}${row_ind_engine_header}")
      seg_col_match <- glue::glue("MATCH({engine_seg_header_cell}, {scf_header_row_range}, 0)")
      row_match     <- glue::glue("MATCH({engine_q_cells}, {scf_q_range}, 0)")
      glue::glue(
        "INDEX({scf_data_2d_range}, {row_match}, {seg_col_match}) * ",
        "INDEX({scf_data_2d_range}, {row_match}, {recode_col_match})"
      )
    })
    engine_grid_tbl <- setNames(
      as.data.frame(engine_grid_tbl, stringsAsFactors = FALSE),
      paste0("col_", seq_along(seq_engine_cols))
    ) %>% tibble::as_tibble()
    for(c in names(engine_grid_tbl)) class(engine_grid_tbl[[c]]) <- "formula"

    openxlsx::writeData(
      wb, sheet_name, x = engine_grid_tbl,
      startRow = row_ind_engine_first,
      startCol = col_ind_engine_survey_var_number + 1,
      colNames = FALSE
    )

    # Score / Probability row labels (written once, not per-cell)
    openxlsx::writeData(
      wb, sheet_name,
      x = c("Score", "Probability") %>% matrix(ncol = 1),
      startRow = row_ind_engine_last + 1,
      startCol = col_ind_engine_survey_var_number,
      colNames = FALSE
    )

    # Score + Probability cells — one pass over segs (no longer nested in the
    # row x col grid loop).
    n_lda_ui          <- length(segments) - length(combined_new_segs)
    col_lda_first_ui  <- col_ind_engine_survey_var_number + 1
    col_lda_last_ui   <- col_ind_engine_survey_var_number + n_lda_ui
    score_row_ui      <- row_ind_engine_last + 1
    score_range_lda_ui <- glue::glue("${num2let(col_lda_first_ui)}${score_row_ui}:${num2let(col_lda_last_ui)}${score_row_ui}")

    ui_max_check_for <- function(from_seg_int){
      pos <- match(from_seg_int, as.integer(segments[seq_len(n_lda_ui)]))
      glue::glue("MATCH(MAX({score_range_lda_ui}), {score_range_lda_ui}, 0)={pos}")
    }

    ui_value_cell_for <- function(var_name){
      if(var_name %in% names(ui_grouped_vars_cells)){
        gcell <- ui_grouped_vars_cells[[var_name]]$answer_cell
        return(list(cell = gcell, is_x = TRUE))
      }
      input_idx_v <- match(var_name, inputs)
      if(!is.na(input_idx_v)){
        is_polar_v  <- input_idx_v <= length(polar_inputs)
        engine_row_v <- row_ind_first + input_idx_v - 1
        if(is_polar_v){
          return(list(cell = glue::glue("{num2let(col_ind_engine_answer_number)}{engine_row_v}"), is_x = FALSE))
        }else{
          return(list(cell = glue::glue("{num2let(col_ind_engine_recode_number)}{engine_row_v}"), is_x = FALSE))
        }
      }
      if(!is.null(aq_info_with_cells)){
        for(aq_entry in aq_info_with_cells){
          if(aq_entry$var == var_name){
            return(list(cell = glue::glue('INDEX({aq_entry$code_abs}, MATCH("x", {aq_entry$answer_abs}, 0))'), is_x = FALSE))
          }
        }
      }
      list(cell = NA_character_, is_x = FALSE)
    }

    ui_grouped_or_normal_condition <- function(var_name, value, from_seg_int){
      ref <- ui_value_cell_for(var_name)
      if(is.na(ref$cell)) return(NA_character_)
      base <- if(isTRUE(ref$is_x)){
        if(as.numeric(value) == 1){
          glue::glue('{ref$cell}="x"')
        }else if(as.numeric(value) == 0){
          glue::glue('{ref$cell}<>"x"')
        }else{
          'FALSE'
        }
      }else{
        glue::glue('{ref$cell}={value}')
      }
      if(isTRUE(apply_logic_only_to_max)){
        glue::glue("AND({base}, {ui_max_check_for(from_seg_int)})")
      }else{
        base
      }
    }

    # additional_logic_advanced helpers — UI engine side. Mirrors the bulk
    # implementation but emits scalar cell refs (one cell per var) since the
    # UI engine resolves to a single respondent.
    ui_when_var_to_cell <- function(var_name){
      ref <- ui_value_cell_for(var_name)
      if(is.na(ref$cell)) return(NA_character_)
      # grouped vars hold "x" / blank — translate to 1 / 0 for numeric
      # comparisons inside the formula.
      if(isTRUE(ref$is_x)){
        return(glue::glue('IF({ref$cell}="x", 1, 0)'))
      }
      ref$cell
    }

    ui_advanced_soft_expanded <- list()
    ui_advanced_hard_rules    <- list()
    if(!is.null(additional_logic_advanced_info)){
      for(rule in additional_logic_advanced_info){
        when_excel_ui <- when_to_excel(rule$when_formula, ui_when_var_to_cell)
        if(rule$is_hard){
          ui_advanced_hard_rules[[length(ui_advanced_hard_rules) + 1]] <- list(
            when_excel = when_excel_ui,
            to_seg     = rule$to_seg,
            rule_index = rule$rule_index
          )
        }else{
          for(fl in rule$from_lda){
            cond_with_max <- if(isTRUE(apply_logic_only_to_max)){
              glue::glue("AND({when_excel_ui}, {ui_max_check_for(fl)})")
            }else{
              when_excel_ui
            }
            ui_advanced_soft_expanded[[length(ui_advanced_soft_expanded) + 1]] <- list(
              cond       = cond_with_max,
              from_lda   = fl,
              to_seg     = rule$to_seg,
              rule_index = rule$rule_index
            )
          }
        }
      }
    }

    for(x in seq_engine_cols){
      seg_idx_ui  <- x - col_ind_engine_survey_var_number
      seg_num_ui  <- as.integer(segments[seg_idx_ui])
      is_new_ui   <- seg_num_ui %in% combined_new_segs
      score_cell_ui <- glue::glue("{num2let(x)}{score_row_ui}")

      # Score formula
      if(!is_new_ui){
        ui_score_formula <- glue::glue('EXP(SUM({num2let(x)}${row_ind_engine_first}:{num2let(x)}${row_ind_engine_last}))')
      }else{
        added_parts_ui <- character(0)
        collect_to <- function(rules, var_name){
          ts <- rules %>% dplyr::filter(to_seg == seg_num_ui)
          for(j in seq_len(nrow(ts))){
            from_idx <- match(ts$from_seg[j], as.integer(segments))
            if(!is.na(from_idx)){
              from_x          <- col_ind_engine_survey_var_number + from_idx
              from_score_cell <- glue::glue("{num2let(from_x)}{score_row_ui}")
              cond <- ui_grouped_or_normal_condition(var_name, ts$value[j], ts$from_seg[j])
              if(!is.na(cond)) added_parts_ui <- c(added_parts_ui, glue::glue("IF({cond}, {from_score_cell}, 0)"))
            }
          }
          added_parts_ui
        }
        if(!is.null(additional_logic_info)){
          for(var_name in names(additional_logic_info)){
            added_parts_ui <- collect_to(additional_logic_info[[var_name]], var_name)
          }
        }
        if(!is.null(additional_questions_info)){
          for(aq_e in additional_questions_info){
            added_parts_ui <- collect_to(aq_e$rules, aq_e$var)
          }
        }

        # additional_logic_advanced (soft) — accumulate from_lda's score onto
        # this new seg when the rule's `when` condition fires.
        for(adv in ui_advanced_soft_expanded){
          if(adv$to_seg != seg_num_ui) next
          from_idx <- match(adv$from_lda, as.integer(segments))
          if(is.na(from_idx)) next
          from_x          <- col_ind_engine_survey_var_number + from_idx
          from_score_cell <- glue::glue("{num2let(from_x)}{score_row_ui}")
          added_parts_ui <- c(added_parts_ui, glue::glue("IF({adv$cond}, {from_score_cell}, 0)"))
        }

        ui_score_formula <- if(length(added_parts_ui) > 0){
          paste(added_parts_ui, collapse = " + ")
        }else{
          "0"
        }
      }

      openxlsx::writeFormula(
        wb, sheet_name,
        x = ui_score_formula,
        startRow = score_row_ui,
        startCol = x,
      )

      # Probability formula. For LDA segs we both zero on `from_seg = this`
      # rules AND add `IF(cond, base_prob_from, 0)` on `to_seg = this` rules
      # so LDA→LDA reassignment conserves mass at the prob layer (LDA→new
      # already conserves mass via the score block's accumulator).
      base_prob_ui <- glue::glue("{score_cell_ui} / SUM({score_range_lda_ui})")
      ui_prob_formula <- if(!is_new_ui){
        from_k_conds_ui <- character(0)
        added_parts_prob_ui <- character(0)

        prob_from_for_idx <- function(from_idx){
          from_x_prob   <- col_ind_engine_survey_var_number + from_idx
          from_score_pf <- glue::glue("{num2let(from_x_prob)}{score_row_ui}")
          glue::glue("{from_score_pf} / SUM({score_range_lda_ui})")
        }

        collect_for_var <- function(rules, var_name){
          fs <- rules %>% dplyr::filter(from_seg == seg_num_ui)
          for(j in seq_len(nrow(fs))){
            cond_str <- ui_grouped_or_normal_condition(var_name, fs$value[j], seg_num_ui)
            if(!is.na(cond_str)) from_k_conds_ui <<- c(from_k_conds_ui, cond_str)
          }
          ts <- rules %>% dplyr::filter(to_seg == seg_num_ui)
          for(j in seq_len(nrow(ts))){
            from_idx <- match(ts$from_seg[j], as.integer(segments))
            if(!is.na(from_idx)){
              cond_to <- ui_grouped_or_normal_condition(var_name, ts$value[j], ts$from_seg[j])
              if(!is.na(cond_to)){
                added_parts_prob_ui <<- c(
                  added_parts_prob_ui,
                  glue::glue("IF({cond_to}, {prob_from_for_idx(from_idx)}, 0)")
                )
              }
            }
          }
        }

        if(!is.null(additional_logic_info)){
          for(var_name in names(additional_logic_info)){
            collect_for_var(additional_logic_info[[var_name]], var_name)
          }
        }
        if(!is.null(additional_questions_info)){
          for(aq_e in additional_questions_info){
            collect_for_var(aq_e$rules, aq_e$var)
          }
        }

        # additional_logic_advanced (soft) — same pattern: zero on from rules,
        # add on to rules (LDA→LDA only).
        for(adv in ui_advanced_soft_expanded){
          if(adv$from_lda == seg_num_ui){
            from_k_conds_ui <- c(from_k_conds_ui, adv$cond)
          }
          if(adv$to_seg == seg_num_ui){
            from_idx <- match(adv$from_lda, as.integer(segments))
            if(!is.na(from_idx)){
              added_parts_prob_ui <- c(
                added_parts_prob_ui,
                glue::glue("IF({adv$cond}, {prob_from_for_idx(from_idx)}, 0)")
              )
            }
          }
        }

        zeroed_part_ui <- if(length(from_k_conds_ui) > 0){
          glue::glue("IF(OR({paste(from_k_conds_ui, collapse = ', ')}), 0, {base_prob_ui})")
        }else{
          base_prob_ui
        }

        if(length(added_parts_prob_ui) > 0){
          paste(c(zeroed_part_ui, added_parts_prob_ui), collapse = " + ")
        }else{
          zeroed_part_ui
        }
      }else{
        base_prob_ui
      }

      # Hard advanced rules — when one fires, the row's prob vector becomes
      # (1 for the rule's to_seg, 0 for all other segs), so the prob row
      # mirrors the classification override at row_ind_engine_seg.
      if(length(ui_advanced_hard_rules) > 0){
        ui_prob_formula <- purrr::reduce(
          rev(ui_advanced_hard_rules),
          function(acc, hard){
            target <- if(hard$to_seg == seg_num_ui) "1" else "0"
            glue::glue("IF({hard$when_excel}, {target}, {acc})")
          },
          .init = ui_prob_formula
        )
      }

      openxlsx::writeFormula(
        wb, sheet_name,
        x = ui_prob_formula,
        startRow = row_ind_engine_prob,
        startCol = x,
      )
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
    # additional_logic_advanced (hard) rules wrap this argmax in an IF chain
    # — first matching hard rule overrides the argmax (case_when semantics).
    ui_seg_core <- glue::glue('MATCH(MAX({range_ind_prob}), {range_ind_prob}, 0)')
    if(length(ui_advanced_hard_rules) > 0){
      ui_seg_core <- purrr::reduce(
        rev(ui_advanced_hard_rules),
        function(acc, hard){
          glue::glue('IF({hard$when_excel}, {hard$to_seg}, {acc})')
        },
        .init = ui_seg_core
      )
    }
    openxlsx::writeFormula(
      wb, sheet_name,
      x = ui_seg_core,
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




  # Shared cell style for non-polar / additional_questions / ui_groups data
  # rows in the documentation sheet: white fill with a thin grid border on
  # every side, matching the polar table's appearance.
  style_doc_data_cell <- openxlsx::createStyle(
    fgFill         = "white",
    halign         = "center",
    valign         = "center",
    border         = "TopBottomLeftRight",
    borderStyle    = "thin",
    wrapText       = TRUE
  )


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

    # Each *_advance = cursor advance from region-start row to next-region
    # cursor row, where the per-block height includes 2 trailing empty rows
    # (so consecutive blocks within and across regions sit 2 rows apart).
    #   non-polar / aq block: header(1) + n_vals + 2 empty   = n_vals + 3
    #   group block:          title(1) + header(1) + n_vars + 2 empty = n_vars + 4
    np_advance <- 0
    N_np_eff   <- 0
    if(!is.null(non_polar_inputs_info) && length(non_polar_inputs_info) > 0){
      ungrouped_np <- purrr::keep(non_polar_inputs_info, ~ !.x$var %in% ui_grouped_vars)
      N_np_eff      <- length(ungrouped_np)
      if(N_np_eff > 0){
        total_vals_np <- sum(purrr::map_int(ungrouped_np, ~ as.integer(.x$n_values)))
        np_advance    <- total_vals_np + 3 * N_np_eff
      }
    }

    aq_advance <- 0
    N_aq_eff   <- 0
    if(!is.null(additional_questions_info) && length(additional_questions_info) > 0){
      ungrouped_aq <- purrr::keep(additional_questions_info, ~ !.x$var %in% ui_grouped_vars)
      N_aq_eff       <- length(ungrouped_aq)
      if(N_aq_eff > 0){
        total_vals_aq <- sum(purrr::map_int(ungrouped_aq, ~ as.integer(nrow(.x$value_table))))
        aq_advance    <- total_vals_aq + 3 * N_aq_eff
      }
    }

    g_advance <- 0
    N_g       <- 0
    if(!is.null(ui_groups_info) && length(ui_groups_info) > 0){
      N_g          <- length(ui_groups_info)
      total_vars_g <- sum(purrr::map_int(ui_groups_info, ~ as.integer(length(.x$vars))))
      g_advance    <- total_vars_g + 4 * N_g
    }

    any_doc_region <- (N_np_eff + N_aq_eff + N_g) > 0

    # 2 empty rows after polar block, regions chain with 2 empty rows between,
    # 2 empty rows between the final region's last data row and the
    # "Solution Coefficient Function" label (which sits at
    # row_doc_function_header - 1; the data row is the column-header row of
    # the coef table itself, so we add one extra to land the LABEL — not the
    # data row — 2 rows below the last input).
    row_doc_function_header <- if(any_doc_region){
      row_doc_last + 4 + np_advance + aq_advance + g_advance
    }else{
      row_doc_last + 4
    }
    row_doc_function_last    <- row_doc_function_header + nrow(coef_func) + 1

    row_doc_steps            <- row_doc_function_last + 2


    #######################
    # grey background canvas — fitted to this sheet's actual extent
    #
    # Painted before any block writes so block-specific styles override the
    # canvas where they apply. The bottom is the step-by-step instructions
    # tibble's last row, computed eagerly here from the same inputs the tibble
    # build (further down) uses — keeping the two in sync.
    #######################

    # Mirror the calulation_steps c(...) construction further down so we know
    # nrow(calulation_steps) here, before any block writes happen.
    #
    # Fixed parts of the c():
    #   9 intro elements (NA, "Segment membership...", NA, Step Q + 5 lines)
    # + 2 NAs after intro
    # + 12 elements in the multiply step (incl. trailing NA, NA)
    # + 28 elements in the probability step (incl. trailing NA, NA)
    # + 6 elements in the QC step
    # = 57 fixed elements.
    #
    # Conditional sections:
    #   questionnaire_extra:    2 elements when has_non_polar
    #   recode_step_content:    7 + nrow(recode_mapping) when has_recoding
    #   reassign_step_content:  7 + n_reassign_rules when has_logic_doc
    has_logic_doc_canvas <- !is.null(additional_logic_info) ||
      !is.null(additional_questions_info) ||
      !is.null(additional_logic_advanced_info)
    has_non_polar_canvas <- length(non_polar_inputs) > 0
    n_recode_rows_canvas <- if(has_recoding) as.integer(nrow(recode_mapping)) else 0L
    # advanced rules contribute their N lines plus a preamble + spacer (2
    # lines) when at least one rule exists, plus an extra NA spacer (1
    # line) when both soft and hard rules are present.
    adv_n         <- if(!is.null(additional_logic_advanced_info)) length(additional_logic_advanced_info) else 0L
    adv_n_hard    <- if(adv_n > 0) sum(purrr::map_lgl(additional_logic_advanced_info, "is_hard")) else 0L
    adv_n_soft    <- adv_n - adv_n_hard
    adv_extra <- (if(adv_n > 0) 3L else 0L) + (if(adv_n_hard > 0L && adv_n_soft > 0L) 1L else 0L)

    n_reassign_rules_canvas <- 0L +
      (if(!is.null(additional_logic_info)) sum(purrr::map_int(additional_logic_info, ~ as.integer(nrow(.x)))) else 0L) +
      (if(!is.null(additional_questions_info)) sum(purrr::map_int(additional_questions_info, ~ as.integer(nrow(.x$rules)))) else 0L) +
      adv_n + adv_extra

    n_calc_steps_canvas <- 58L +
      (if(has_non_polar_canvas) 2L else 0L) +
      (if(has_recoding)         7L + n_recode_rows_canvas else 0L) +
      (if(has_logic_doc_canvas) 7L + n_reassign_rules_canvas else 0L)

    # writeData(... colNames = TRUE) writes the column header at row_doc_steps
    # and the data rows at row_doc_steps + 1 .. row_doc_steps + n_calc_steps,
    # so the table's last row sits at row_doc_steps + n_calc_steps. Extend the
    # canvas one row past that.
    canvas_max_row_doc <- row_doc_steps + n_calc_steps_canvas + 1
    canvas_max_col_doc <- col_last + 1

    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(fgFill = oxl_colorscale_grey(1)),
      rows = seq(1, canvas_max_row_doc),
      cols = seq(1, canvas_max_col_doc),
      gridExpand = TRUE
    )


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
    # ind_ui only carries that column when polar_spacer_needed is TRUE (i.e.
    # an individually-rendered non-polar or AQ block reuses that column for
    # its hidden Code column on the Individual UI). Documentation never wants
    # the spacer in its polar layout, so strip it whenever it's there.
    temp_doc_ui <- ind_ui
    if(polar_spacer_needed){
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
        if(info$var %in% ui_grouped_vars) next  # rendered inside a ui_groups block
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

        # white fill + thin inner grid (matches polar / coef table look)
        openxlsx::addStyle(wb, sheet_name, style = style_doc_data_cell,
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

        row_np_doc_cursor <- row_np_d_l + 3
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

      # Chain off the post-np cursor when np blocks rendered; otherwise start
      # 2 empty rows below the polar block. row_np_doc_cursor (when set) already
      # equals last_np_data_row + 3, so reusing it gives a 2-row inter-region gap.
      row_aq_doc_cursor <- if(exists("row_np_doc_cursor", inherits = FALSE)){
        row_np_doc_cursor
      }else{
        row_doc_last + 3
      }

      for(idx in seq_along(additional_questions_info)){
        info       <- additional_questions_info[[idx]]
        if(info$var %in% ui_grouped_vars) next  # rendered inside a ui_groups block
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

        q_label_aq_doc <- q_label_for_var(info$var)
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

        openxlsx::addStyle(wb, sheet_name, style = style_doc_data_cell,
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

        row_aq_doc_cursor <- row_aq_d_l + 3
      }
    }


    #######################
    # ui_groups blocks (doc layout)
    #######################

    if(!is.null(ui_groups_info) && length(ui_groups_info) > 0){

      col_doc_g_q       <- col_clean_var
      col_doc_g_var     <- col_survey_var
      col_doc_g_text    <- col_label_left
      col_doc_g_text_end <- col_label_left + doc_label_cell_merge
      col_doc_g_answer  <- col_doc_g_text_end + 1

      # Chain off the most recent post-region cursor: aq → np → polar fallback.
      # Each cursor variable, when set, equals last_block_row + 3, which yields
      # a 2-row inter-region gap. With no prior region, leave 2 empty rows
      # below the polar block.
      row_g_doc_cursor <- if(exists("row_aq_doc_cursor", inherits = FALSE)){
        row_aq_doc_cursor
      }else if(exists("row_np_doc_cursor", inherits = FALSE)){
        row_np_doc_cursor
      }else{
        row_doc_last + 3
      }

      for(g_idx in seq_along(ui_groups_info)){
        g_info  <- ui_groups_info[[g_idx]]
        n_vars  <- length(g_info$vars)
        row_g_d_t <- row_g_doc_cursor       # title row
        row_g_d_h <- row_g_d_t + 1          # header row
        row_g_d_f <- row_g_d_h + 1          # first var row
        row_g_d_l <- row_g_d_h + n_vars     # last var row

        # title (merged across block)
        openxlsx::mergeCells(wb, sheet_name,
          rows = row_g_d_t,
          cols = seq(col_doc_g_q, col_doc_g_answer)
        )
        openxlsx::writeData(wb, sheet_name,
          x = paste0(g_info$label, if(g_info$type == "mece") " (choose one)" else " (select all that apply)"),
          startRow = row_g_d_t,
          startCol = col_doc_g_q,
          colNames = FALSE
        )
        # Match the column-header row below (style_header) so the title +
        # column-header pair reads as a single grey band.
        openxlsx::addStyle(wb, sheet_name,
          style = style_header,
          rows = row_g_d_t,
          cols = seq(col_doc_g_q, col_doc_g_answer),
          gridExpand = TRUE, stack = TRUE
        )

        # header row
        openxlsx::writeData(wb, sheet_name, x = "Question",         startRow = row_g_d_h, startCol = col_doc_g_q,        colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = "Survey\nVariable", startRow = row_g_d_h, startCol = col_doc_g_var,      colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, x = "Answer",           startRow = row_g_d_h, startCol = col_doc_g_answer,   colNames = FALSE)
        openxlsx::mergeCells(wb, sheet_name,
          rows = row_g_d_h,
          cols = seq(col_doc_g_text, col_doc_g_text_end)
        )
        openxlsx::addStyle(wb, sheet_name,
          style = style_header,
          rows = row_g_d_h,
          cols = seq(col_doc_g_q, col_doc_g_answer),
          gridExpand = TRUE, stack = TRUE
        )

        spec_profile_lookup_g_doc <- tryCatch(
          seg[["spec"]][["profiles"]] %>% tidyr::unnest(vars) %>% dplyr::select(var, label),
          error = function(e) NULL
        )
        dict_lk_doc <- seg[["data"]][["original_dictionary"]]

        for(v_idx in seq_along(g_info$vars)){
          v_name <- g_info$vars[v_idx]
          r      <- row_g_d_f + v_idx - 1

          v_label <- v_name
          if(!is.null(spec_profile_lookup_g_doc) && nrow(spec_profile_lookup_g_doc) > 0){
            row_lk <- spec_profile_lookup_g_doc %>% dplyr::filter(var == v_name)
            if(nrow(row_lk) > 0){
              vv <- row_lk$label[1]
              if(!is.na(vv) && nzchar(vv)) v_label <- vv
            }
          }
          if(v_label == v_name && !is.null(dict_lk_doc) && "variable" %in% names(dict_lk_doc)){
            dr <- dict_lk_doc %>% dplyr::filter(variable == v_name)
            if(nrow(dr) > 0){
              vv <- dr$label[1]
              if(!is.na(vv) && nzchar(vv)) v_label <- vv
            }
          }

          q_label_g_doc <- q_label_for_var(v_name)
          openxlsx::writeData(wb, sheet_name, x = q_label_g_doc, startRow = r, startCol = col_doc_g_q,    colNames = FALSE)
          openxlsx::writeData(wb, sheet_name, x = v_name,        startRow = r, startCol = col_doc_g_var,  colNames = FALSE)
          openxlsx::writeData(wb, sheet_name, x = v_label,       startRow = r, startCol = col_doc_g_text, colNames = FALSE)

          openxlsx::mergeCells(wb, sheet_name,
            rows = r,
            cols = seq(col_doc_g_text, col_doc_g_text_end)
          )
        }

        openxlsx::addStyle(wb, sheet_name, style = style_doc_data_cell,
          rows = seq(row_g_d_f, row_g_d_l),
          cols = seq(col_doc_g_q, col_doc_g_answer),
          gridExpand = TRUE, stack = TRUE
        )

        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_g_d_t, row_end = row_g_d_l,
          col_start = col_doc_g_q, col_end = col_doc_g_answer
        )
        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_g_d_f, row_end = row_g_d_l,
          col_start = col_doc_g_q, col_end = col_doc_g_answer
        )

        row_g_doc_cursor <- row_g_d_l + 3
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

    has_logic_doc <- !is.null(additional_logic_info) ||
      !is.null(additional_questions_info) ||
      !is.null(additional_logic_advanced_info)

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
      max_clause_for <- function(from_seg){
        if(isTRUE(apply_logic_only_to_max)){
          glue::glue(" (only when Segment {from_seg} has the highest base probability)")
        }else{
          ""
        }
      }
      if(!is.null(additional_logic_info)){
        for(var_name in names(additional_logic_info)){
          rules_l <- additional_logic_info[[var_name]]
          for(j in seq_len(nrow(rules_l))){
            reassign_lines <- c(
              reassign_lines,
              glue::glue("          If {var_name} = {rules_l$value[j]}{max_clause_for(rules_l$from_seg[j])}: set Segment {rules_l$from_seg[j]} probability to 0 and add it to Segment {rules_l$to_seg[j]} probability.")
            )
          }
        }
      }
      if(!is.null(additional_questions_info)){
        for(aq_entry in additional_questions_info){
          for(j in seq_len(nrow(aq_entry$rules))){
            reassign_lines <- c(
              reassign_lines,
              glue::glue("          If {aq_entry$var} = {aq_entry$rules$value[j]}{max_clause_for(aq_entry$rules$from_seg[j])}: set Segment {aq_entry$rules$from_seg[j]} probability to 0 and add it to Segment {aq_entry$rules$to_seg[j]} probability.")
            )
          }
        }
      }

      # additional_logic_advanced — render rules in evaluation order: soft
      # rules first (they redistribute prob mass at the prob layer; all run
      # independently and compose with each other and with simpler logic),
      # then hard rules (clamp the final prob vector to 100/0 — sequential,
      # first match wins, always supersede soft work).
      if(!is.null(additional_logic_advanced_info)){
        soft_lines <- character(0)
        hard_lines <- character(0)

        for(rule in additional_logic_advanced_info){
          # render the when condition with question labels (Q1, Q2, ...)
          # substituted for the underlying variable names — easier to read
          # than the raw rs_* / VT* / DM* identifiers.
          when_text <- when_to_human_text(rule$when_formula)
          if(rule$is_hard){
            hard_lines <- c(
              hard_lines,
              glue::glue(
                "          [Advanced rule {rule$display_index}, hard reassignment] If ({when_text}): ",
                "set Segment {rule$to_seg}'s probability to 1 (100%) and every other segment's probability to 0; ",
                "the final classification becomes Segment {rule$to_seg}."
              )
            )
          }else{
            from_set <- paste(rule$from_lda, collapse = ", ")
            n_from   <- length(rule$from_lda)
            seg_word <- if(n_from == 1) "Segment" else "Segments"
            max_clause <- if(isTRUE(apply_logic_only_to_max)){
              if(n_from == 1){
                glue::glue(" (only when Segment {from_set} has the highest base probability)")
              }else{
                glue::glue(" (only when one of Segments {from_set} has the highest base probability)")
              }
            }else ""
            soft_lines <- c(
              soft_lines,
              glue::glue(
                "          [Advanced rule {rule$display_index}, soft reassignment] If ({when_text}){max_clause}: ",
                "add {seg_word} {from_set}'s probability to Segment {rule$to_seg}, ",
                "and set {seg_word} {from_set}'s probability to 0."
              )
            )
          }
        }

        has_soft <- length(soft_lines) > 0
        has_hard <- length(hard_lines) > 0

        preamble_line_1 <- paste0(
          "          The reassignments below run in two passes: soft rules first ",
          "(they move probability mass between segments)."
        )
        preamble_line_2 <- paste0(
          "          Then hard rules, which clamp the final probability vector to 100% ",
          "on 'to_seg' and 0% on every other segment - first matching hard rule wins. ",
          "Hard rules always supersede any soft redistribution that occurred for the same respondent."
        )

        if(has_soft || has_hard){
          reassign_lines <- c(reassign_lines, preamble_line_1, preamble_line_2, NA)
        }
        if(has_soft){
          reassign_lines <- c(reassign_lines, soft_lines)
          if(has_hard) reassign_lines <- c(reassign_lines, NA)
        }
        if(has_hard){
          reassign_lines <- c(reassign_lines, hard_lines)
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
        glue::glue("          For each participant, multiply the {resp_adj}responses with the coefficients for each segment. Sum the products for each segment and add the constant.  This will create a score for each segment."),
        if(has_non_polar) "          For polar questions, multiply the recoded value (rs prefix); for non-polar questions, multiply the raw answer code." else NA,
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
    # cached styles (build once, reuse) — avoids per-call createStyle alloc
    #######################

    bulk_style_good_bold <- oxl_style_cell_good(textDecoration = "bold", conditional = TRUE)
    bulk_style_bad_bold  <- oxl_style_cell_bad(textDecoration = "bold", conditional = TRUE)


    #######################
    # reference constants
    #######################

    row_header <- row_title + 5

    row_data_first <- row_header + 1
    row_data_last <- row_data_first + nrow(data_inputs)

    ammount_of_input_rows <- ceiling(row_data_last / 1000) * 1000

    row_last <- row_data_first + ammount_of_input_rows

    col_input_first <- start_col + 1
    # all data input cols (LDA inputs + AQ vars rendered in one continuous block)
    col_input_last <- start_col + length(all_data_input_vars)

    # legacy "lda-only" boundary for the recode/score range (recode block covers
    # only LDA inputs; AQ vars don't feed the LDA score)
    col_lda_input_last <- start_col + length(clean_variable_names)

    n_aq_cols    <- if(!is.null(additional_questions_info)) length(additional_questions_info) else 0
    aq_var_names <- if(n_aq_cols > 0) purrr::map_chr(additional_questions_info, "var") else character(0)

    col_calculation_qc <- col_input_last + 1

    col_recode_first <- col_input_first
    col_recode_last <- col_lda_input_last

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

    # "Prob Sum to 1" sanity-check column sits directly after the Class Check
    # column (= col_bulk_qc_formula_last). Always present.
    col_prob_sum_check <- col_bulk_qc_formula_last + 1L

    # Optional extra columns (additional_bulk_qc_check) appended directly to
    # the right of the prob-sum check, under the same merged title. Each
    # named variable contributes two columns: the variable's value (from
    # seg$data$with_solutions) followed by a TRUE/FALSE column comparing it
    # to the bulk Classification col (col_seg).
    n_additional_qc <- length(additional_bulk_qc_check)
    has_additional_qc <- n_additional_qc > 0
    col_additional_qc_first <- col_prob_sum_check + 1L
    col_additional_qc_last  <- col_prob_sum_check + 2L * n_additional_qc

    # Helpers: data col / check col for additional QC var i (1-indexed).
    col_additional_qc_data_for  <- function(i) col_additional_qc_first + (i - 1L) * 2L
    col_additional_qc_check_for <- function(i) col_additional_qc_first + (i - 1L) * 2L + 1L

    # Rightmost col of the QC Check block (used by title merge / borders /
    # canvas etc.). Always at least col_prob_sum_check; extends to
    # col_additional_qc_last when extras are supplied.
    col_bulk_qc_block_last <- if(has_additional_qc) col_additional_qc_last else col_prob_sum_check

    col_last <- col_bulk_qc_block_last


    #######################
    # grey background canvas — fitted to this sheet's actual extent
    #
    # Painted before any block writes so block-specific styles override the
    # canvas where they apply. The bulk table extends past the inputted data
    # to `row_last` (rounded up to the next 1000-row boundary so pre-baked
    # formulas can absorb additional respondents without re-rendering the
    # sheet); the canvas matches the table and extends 1 row beyond it.
    # Width = 1 col past col_qc_count (col_bulk_qc_block_last + 7), the
    # rightmost freq-table column written further down. col_bulk_qc_block_last
    # equals col_bulk_qc_formula_last unless additional_bulk_qc_check is
    # supplied — then it extends to include those extra columns.
    #######################

    # Width covers (in order): the QC block itself, a 2-col gap, then the
    # four base freq tables (Class Check / Prob Sum / Membership / QC Seg = 8
    # cols), plus 2 cols per additional QC var for that var's Seg Counts
    # table. Height matches the bulk table; the Check Counts tables sit a
    # few rows below row_header, well above row_last.
    has_advanced_rules <- !is.null(additional_logic_advanced_info) && length(additional_logic_advanced_info) > 0
    canvas_max_row_bulk <- row_last + 1
    canvas_max_col_bulk <- col_bulk_qc_block_last + 10 +
      (if(has_additional_qc) n_additional_qc * 2L else 0L) +
      (if(has_advanced_rules) 4L else 0L)

    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(fgFill = oxl_colorscale_grey(1)),
      rows = seq(1, canvas_max_row_bulk),
      cols = seq(1, canvas_max_col_bulk),
      gridExpand = TRUE
    )


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
        y = c("Original Questionnaire", all_data_input_vars),
        x = c("Respondent", as.character(all_q_names))
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


    # combined data input: respondent id + LDA inputs + AQ vars (one continuous block)
    combined_data_inputs <- seg[["data"]][["with_solutions"]] %>%
      dplyr::select(dplyr::all_of(c(survey_respondent_id, all_data_input_vars)))

    temp_data_inputs <- combined_data_inputs %>%
      add_NA_rows(
        ammount_of_input_rows - nrow(combined_data_inputs) + 1
      )


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


    # AQ vars are now part of the unified Data Input block (no separate header)



    #######################
    # Calculation Ready
    #######################

    polar_count_b <- length(polar_inputs)
    polar_input_first_let <- num2let(col_input_first)
    polar_input_last_let  <- if(polar_count_b > 0) num2let(col_input_first + polar_count_b - 1) else NA_character_
    all_input_first_let   <- num2let(col_input_first)
    all_input_last_let    <- num2let(col_input_last)  # spans LDA inputs + AQ inputs (unified)

    has_non_polar_b <- length(non_polar_inputs) > 0
    has_aq_b        <- n_aq_cols > 0

    seq_r_b <- seq(row_data_first, row_last)
    parts_list_b <- list()

    bulk_col_for_var <- function(v){
      idx <- match(v, all_data_input_vars)
      if(!is.na(idx)) return(col_input_first + idx - 1)
      NA_integer_
    }

    if(polar_count_b > 0){
      parts_list_b <- c(parts_list_b, list(glue::glue(
        'COUNTIFS(${polar_input_first_let}{seq_r_b}:${polar_input_last_let}{seq_r_b},">={min(polar_points)}", ',
        '${polar_input_first_let}{seq_r_b}:${polar_input_last_let}{seq_r_b},"<={max(polar_points)}") = {polar_count_b}'
      )))
    }

    if(has_non_polar_b){
      for(i in seq_along(non_polar_inputs_info)){
        np_info_b <- non_polar_inputs_info[[i]]
        np_col_let <- num2let(col_input_first + polar_count_b + i - 1)
        np_vals    <- as.numeric(np_info_b$value_table$code)
        np_min     <- min(np_vals, na.rm = TRUE)
        np_max     <- max(np_vals, na.rm = TRUE)
        parts_list_b <- c(parts_list_b, list(glue::glue('AND(${np_col_let}{seq_r_b}>={np_min}, ${np_col_let}{seq_r_b}<={np_max})')))
      }
    }

    if(has_aq_b){
      for(i in seq_along(additional_questions_info)){
        aq_info_b  <- additional_questions_info[[i]]
        aq_col_let <- num2let(bulk_col_for_var(aq_info_b$var))
        aq_vals    <- as.numeric(aq_info_b$value_table$code)
        aq_min     <- min(aq_vals, na.rm = TRUE)
        aq_max     <- max(aq_vals, na.rm = TRUE)
        parts_list_b <- c(parts_list_b, list(glue::glue('AND(${aq_col_let}{seq_r_b}>={aq_min}, ${aq_col_let}{seq_r_b}<={aq_max})')))
      }
    }

    if(!is.null(ui_groups_info) && length(ui_groups_info) > 0){
      for(g_info in ui_groups_info){
        bulk_cols <- purrr::map_int(g_info$vars, bulk_col_for_var)
        bulk_cols <- bulk_cols[!is.na(bulk_cols)]
        if(length(bulk_cols) == 0) next
        # build vectorized sum-of-cells expression per row (length = length(seq_r_b))
        cell_vecs <- lapply(bulk_cols, function(cc) glue::glue("${num2let(cc)}{seq_r_b}"))
        sum_terms <- purrr::reduce(cell_vecs, function(a, b) paste0(a, "+", b))
        if(g_info$type == "mece"){
          op <- if(isTRUE(g_info$no_selection_allowed)) "<=1" else "=1"
          parts_list_b <- c(parts_list_b, list(glue::glue("({sum_terms}){op}")))
        }else if(g_info$type == "multi_select"){
          if(!isTRUE(g_info$no_selection_allowed)){
            parts_list_b <- c(parts_list_b, list(glue::glue("({sum_terms})>=1")))
          }
        }
      }
    }

    check_combined <- if(length(parts_list_b) > 1){
      combined_inner <- purrr::reduce(parts_list_b, function(a, b) paste0(a, ", ", b))
      glue::glue("AND({combined_inner})")
    }else if(length(parts_list_b) == 1){
      parts_list_b[[1]]
    }else{
      rep("TRUE", length(seq_r_b))
    }

    calc_ready_formulas <- glue::glue(
      'IF(COUNTA(${all_input_first_let}{seq_r_b}:${all_input_last_let}{seq_r_b}) = 0, "", {check_combined})'
    )

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
      style = bulk_style_good_bold
    )


    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = col_calculation_qc,
      rows = seq(row_data_first, row_last),
      rule = glue::glue('${num2let(col_calculation_qc)}{row_data_first} = FALSE'),
      style = bulk_style_bad_bold
    )



    #######################
    # recode data
    #######################

    if(inputs_are_rs){

      # Recode only LDA inputs — AQ vars don't feed the LDA score so they
      # don't need a recode column. col_input_last spans LDA + AQ; clamping
      # to col_lda_input_last keeps the loop output length in sync with
      # clean_variable_names (LDA only) so setNames doesn't create NA names.
      temp <- purrr::map(
        seq(col_input_first, col_lda_input_last),
        function(xc){
          col_idx <- xc - col_input_first + 1
          is_polar_col <- col_idx <= polar_count_b
          if(is_polar_col){
            glue::glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)} = TRUE, IF(ISBLANK({num2let(xc)}{seq(row_data_first, row_last)}), "", INDEX({range_recode},1,{num2let(xc)}{seq(row_data_first, row_last)})), "")')
          }else{
            glue::glue('IF(${num2let(col_calculation_qc)}{seq(row_data_first, row_last)} = TRUE, IF(ISBLANK({num2let(xc)}{seq(row_data_first, row_last)}), "", {num2let(xc)}{seq(row_data_first, row_last)}), "")')
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

    # Shared row indexing + helpers used by both the score and prob blocks.
    seq_r_p           <- seq(row_data_first, row_last)
    score_col_letters <- num2let(seq(col_score_first, col_score_last))
    calc_qc_cell_v    <- glue::glue("${num2let(col_calculation_qc)}{seq_r_p}")

    # The score columns are laid out as [LDA segs] then [new segs] (matching
    # the bind_cols order in coef_func). All max-checks and the prob
    # denominator reference the LDA-only sub-range so:
    #   * new-seg score formulas (which sit in the score row) can use the
    #     max-check without creating a circular reference,
    #   * prob mass conserves: SUM(LDA scores) doesn't change when a new seg
    #     pulls mass from an LDA seg.
    n_lda_segs       <- length(segments) - length(combined_new_segs)
    col_lda_score_last <- col_score_first + n_lda_segs - 1
    score_range_lda  <- glue::glue("${num2let(col_score_first)}{seq_r_p}:${num2let(col_lda_score_last)}{seq_r_p}")

    bulk_max_check_for <- function(from_seg_int){
      pos <- match(from_seg_int, as.integer(segments[seq_len(n_lda_segs)]))
      glue::glue("MATCH(MAX({score_range_lda}), {score_range_lda}, 0)={pos}")
    }

    bulk_rule_condition <- function(cell_str, value, from_seg_int){
      base <- glue::glue("{cell_str}={value}")
      if(isTRUE(apply_logic_only_to_max)){
        glue::glue("AND({base}, {bulk_max_check_for(from_seg_int)})")
      }else{
        base
      }
    }

    # additional_logic_advanced helpers — Excel-formula side
    bulk_when_var_to_cell <- function(var_name){
      col <- bulk_col_for_var(var_name)
      glue::glue("{num2let(col)}{seq_r_p}")
    }

    # Pre-translate every soft advanced rule's `when` condition into Excel
    # (vectorized over data rows). Each soft rule is then expanded into one
    # entry per from_lda value so the score/prob blocks can iterate
    # uniformly. Hard rules go to a separate list used by the classification
    # override at the end of the bulk render.
    bulk_advanced_soft_expanded <- list()
    bulk_advanced_hard_rules    <- list()
    if(!is.null(additional_logic_advanced_info)){
      for(rule in additional_logic_advanced_info){
        when_excel <- when_to_excel(rule$when_formula, bulk_when_var_to_cell)
        if(rule$is_hard){
          bulk_advanced_hard_rules[[length(bulk_advanced_hard_rules) + 1]] <- list(
            when_excel = when_excel,
            to_seg     = rule$to_seg,
            rule_index = rule$rule_index
          )
        }else{
          for(fl in rule$from_lda){
            cond_with_max <- if(isTRUE(apply_logic_only_to_max)){
              glue::glue("AND({when_excel}, {bulk_max_check_for(fl)})")
            }else{
              when_excel
            }
            bulk_advanced_soft_expanded[[length(bulk_advanced_soft_expanded) + 1]] <- list(
              cond       = cond_with_max,
              from_lda   = fl,
              to_seg     = rule$to_seg,
              rule_index = rule$rule_index
            )
          }
        }
      }
    }

    # Per-seg base exp expression (vectorized over rows). LDA segs use the
    # closed-form EXP(MMULT(recodes, coefs) + const). New segs have zero
    # coefs/constant in coef_func, so we don't write that formula at all
    # — their score is built directly from from-seg score-cell references
    # below (this also keeps the workbook small since we're not inlining
    # the EXP(MMULT(...)) expression in multiple places).
    score_columns_list <- purrr::map(seq_along(segments), function(seg_idx){
      seg_num <- as.integer(segments[seg_idx])
      is_new  <- seg_num %in% combined_new_segs

      if(!is_new){
        # plain LDA score — no IF/MAX, no circular reference risk
        base_exp_k <- glue::glue('EXP(MMULT(${num2let(col_recode_first)}{seq_r_p}:${num2let(col_recode_last)}{seq_r_p}, {range_coef[seg_idx]}) + {range_constant[seg_idx]})')
        return(as.character(glue::glue('IF({calc_qc_cell_v} = TRUE, {base_exp_k}, "")')))
      }

      # new seg: pull from from_seg score cell when the rule condition fires
      added_parts_list <- list()

      collect_to_rules <- function(rules, cell_v){
        to_subset <- rules %>% dplyr::filter(to_seg == seg_num)
        for(j in seq_len(nrow(to_subset))){
          from_idx <- match(to_subset$from_seg[j], as.integer(segments))
          if(!is.na(from_idx)){
            from_score_let  <- score_col_letters[from_idx]
            from_score_cell <- glue::glue("{from_score_let}{seq_r_p}")
            cond <- bulk_rule_condition(cell_v, to_subset$value[j], to_subset$from_seg[j])
            added_parts_list[[length(added_parts_list) + 1]] <<- glue::glue("IF({cond}, {from_score_cell}, 0)")
          }
        }
      }

      if(!is.null(additional_logic_info)){
        for(var_name in names(additional_logic_info)){
          input_idx_v <- match(var_name, inputs)
          cell_v <- glue::glue("{num2let(col_input_first + input_idx_v - 1)}{seq_r_p}")
          collect_to_rules(additional_logic_info[[var_name]], cell_v)
        }
      }

      if(!is.null(additional_questions_info)){
        for(aq_entry in additional_questions_info){
          cell_v <- glue::glue("{num2let(bulk_col_for_var(aq_entry$var))}{seq_r_p}")
          collect_to_rules(aq_entry$rules, cell_v)
        }
      }

      # additional_logic_advanced (soft) — accumulate from_lda's score onto
      # this new seg when the rule's `when` condition fires.
      for(adv in bulk_advanced_soft_expanded){
        if(adv$to_seg != seg_num) next
        from_idx <- match(adv$from_lda, as.integer(segments))
        if(is.na(from_idx)) next
        from_score_let  <- score_col_letters[from_idx]
        from_score_cell <- glue::glue("{from_score_let}{seq_r_p}")
        added_parts_list[[length(added_parts_list) + 1]] <- glue::glue(
          "IF({adv$cond}, {from_score_cell}, 0)"
        )
      }

      inner <- if(length(added_parts_list) > 0){
        purrr::reduce(added_parts_list, function(a, b) paste0(a, " + ", b))
      }else{
        "0"
      }

      as.character(glue::glue('IF({calc_qc_cell_v} = TRUE, {inner}, "")'))
    })

    temp <- setNames(score_columns_list, glue::glue("exp_score_seg_{segments}")) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      tibble::as_tibble()


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
    # calculate prob
    #
    # base_prob_k = score_k / SUM(LDA scores). Using the LDA-only score range
    # as the denominator means new segs (which pull mass from an LDA seg via
    # their score cell) don't double-count in the total. Mass conservation:
    # when a from_seg=k rule fires we zero prob_k and add prob_k's value to
    # the corresponding to_seg below — total prob sums to 1.
    #######################

    # Hard-rule wrapper: when a hard advanced rule fires, the whole row's
    # probability vector becomes (1 for the rule's to_seg, 0 for everyone
    # else), so the prob layer mirrors the classification override at the
    # col_seg level. Sequential semantics — first matching hard rule wins.
    bulk_wrap_prob_with_hard <- function(inner, seg_num){
      if(length(bulk_advanced_hard_rules) == 0) return(inner)
      purrr::reduce(
        rev(bulk_advanced_hard_rules),
        function(acc, hard){
          target <- if(hard$to_seg == seg_num) "1" else "0"
          glue::glue("IF({hard$when_excel}, {target}, {acc})")
        },
        .init = inner
      )
    }

    prob_columns_list <- purrr::map(seq_along(segments), function(seg_idx){
      seg_num   <- as.integer(segments[seg_idx])
      is_new    <- seg_num %in% combined_new_segs
      score_let <- score_col_letters[seg_idx]

      base_prob_k <- glue::glue("{score_let}{seq_r_p}/SUM({score_range_lda})")

      if(is_new){
        # new seg's score is already redistributed; just emit score / SUM(LDA)
        wrapped <- bulk_wrap_prob_with_hard(base_prob_k, seg_num)
        return(as.character(glue::glue('IF({calc_qc_cell_v} = TRUE, {wrapped}, "")')))
      }

      # LDA seg: zero on from-rules + add on to-rules (LDA→LDA only — new seg
      # to-rules already accumulate at the score level upstream, and the
      # SUM(score_range_lda) denominator is unaffected).
      from_k_conds_list <- list()
      added_parts_list  <- list()

      collect_rules_for_lda <- function(rules, cell_v){
        from_subset <- rules %>% dplyr::filter(from_seg == seg_num)
        for(j in seq_len(nrow(from_subset))){
          from_k_conds_list[[length(from_k_conds_list) + 1]] <<- bulk_rule_condition(cell_v, from_subset$value[j], seg_num)
        }
        to_subset <- rules %>% dplyr::filter(to_seg == seg_num)
        for(j in seq_len(nrow(to_subset))){
          from_idx <- match(to_subset$from_seg[j], as.integer(segments))
          if(!is.na(from_idx)){
            from_score_let <- score_col_letters[from_idx]
            base_prob_from <- glue::glue("{from_score_let}{seq_r_p}/SUM({score_range_lda})")
            cond <- bulk_rule_condition(cell_v, to_subset$value[j], to_subset$from_seg[j])
            added_parts_list[[length(added_parts_list) + 1]] <<- glue::glue("IF({cond}, {base_prob_from}, 0)")
          }
        }
      }

      if(!is.null(additional_logic_info)){
        for(var_name in names(additional_logic_info)){
          input_idx_v <- match(var_name, inputs)
          cell_v <- glue::glue("{num2let(col_input_first + input_idx_v - 1)}{seq_r_p}")
          collect_rules_for_lda(additional_logic_info[[var_name]], cell_v)
        }
      }

      if(!is.null(additional_questions_info)){
        for(aq_entry in additional_questions_info){
          cell_v <- glue::glue("{num2let(bulk_col_for_var(aq_entry$var))}{seq_r_p}")
          collect_rules_for_lda(aq_entry$rules, cell_v)
        }
      }

      # additional_logic_advanced (soft) — same pattern: zero on from rules,
      # add on to rules (LDA→LDA only; LDA→new flows through the score block
      # above and inherits the SUM(LDA) denominator).
      for(adv in bulk_advanced_soft_expanded){
        if(adv$from_lda == seg_num){
          from_k_conds_list[[length(from_k_conds_list) + 1]] <- adv$cond
        }
        if(adv$to_seg == seg_num){
          from_idx <- match(adv$from_lda, as.integer(segments))
          if(!is.na(from_idx)){
            from_score_let <- score_col_letters[from_idx]
            base_prob_from <- glue::glue("{from_score_let}{seq_r_p}/SUM({score_range_lda})")
            added_parts_list[[length(added_parts_list) + 1]] <- glue::glue(
              "IF({adv$cond}, {base_prob_from}, 0)"
            )
          }
        }
      }

      zeroed_part <- if(length(from_k_conds_list) > 0){
        combined_conds <- purrr::reduce(from_k_conds_list, function(a, b) paste0(a, ", ", b))
        glue::glue("IF(OR({combined_conds}), 0, {base_prob_k})")
      }else{
        base_prob_k
      }

      full <- if(length(added_parts_list) > 0){
        combined_added <- purrr::reduce(added_parts_list, function(a, b) paste0(a, " + ", b))
        paste0(zeroed_part, " + ", combined_added)
      }else{
        zeroed_part
      }

      wrapped <- bulk_wrap_prob_with_hard(full, seg_num)
      as.character(glue::glue('IF({calc_qc_cell_v} = TRUE, {wrapped}, "")'))
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

    # additional_logic_advanced (hard) — wrap the argmax classification in a
    # sequential IF chain so the first matching hard rule overrides the
    # post-soft-redistribution argmax. Order preserved from the user's
    # `additional_logic_advanced` tibble.
    if(length(bulk_advanced_hard_rules) > 0){
      classification_core <- purrr::reduce(
        rev(bulk_advanced_hard_rules),
        function(acc, hard){
          glue::glue('IF({hard$when_excel}, {hard$to_seg}, {acc})')
        },
        .init = classification_core
      )
    }

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


    # Prob Sum to 1 sanity check — verifies that the row's probabilities sum
    # to 1 (rounded to 4 decimals to absorb floating-point noise). Sits
    # directly after Class Check inside the Bulk QC Check block. Blank when
    # the row is not Calculation Ready, matching the prob columns themselves.
    seq_qc_rows <- seq(row_header + 1, row_header + nrow(data_solution_check))
    prob_sum_check_formulas <- glue::glue(
      'IF(${num2let(col_calculation_qc)}{seq_qc_rows} = TRUE, ',
      'ROUND(SUM({num2let(col_prob_first)}{seq_qc_rows}:{num2let(col_prob_last)}{seq_qc_rows}), 4) = 1, "")'
    )
    prob_sum_tbl <- tibble::tibble("Prob Sum to 1" = prob_sum_check_formulas)
    class(prob_sum_tbl[[1]]) <- "formula"
    openxlsx::writeData(
      wb, sheet_name,
      x = prob_sum_tbl,
      startRow = row_header,
      startCol = col_prob_sum_check,
      borders = "all",
      headerStyle = style_header,
      colNames = TRUE
    )
    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = col_prob_sum_check,
      rows = seq_qc_rows,
      rule = glue::glue('{num2let(col_prob_sum_check)}{row_header + 1} = TRUE'),
      style = bulk_style_good_bold
    )
    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = col_prob_sum_check,
      rows = seq_qc_rows,
      rule = glue::glue('{num2let(col_prob_sum_check)}{row_header + 1} = FALSE'),
      style = bulk_style_bad_bold
    )


    openxlsx::writeData(
      wb, sheet_name,
      x = "Bulk QC Check",
      startRow = row_header - 2,
      startCol = col_bulk_qc_first,
      colNames = FALSE
    )


    # Optional additional QC check columns (additional_bulk_qc_check). Each
    # named variable produces two adjacent columns:
    #   1. the variable's value (pulled straight from seg$data$with_solutions)
    #   2. a TRUE/FALSE check formula = (var_value == Classification)
    # Both sit inside the merged "Bulk QC Check" title.
    if(has_additional_qc){
      col_seg_letter <- num2let(col_seg)
      for(i in seq_len(n_additional_qc)){
        var_i      <- additional_bulk_qc_check[i]
        col_data_i <- col_additional_qc_data_for(i)
        col_chk_i  <- col_additional_qc_check_for(i)

        # 1. data column — the var's values + a header
        openxlsx::writeData(
          wb, sheet_name,
          x = tibble::tibble(!!var_i := df[[var_i]][seq_len(nrow(data_solution_check))]),
          startRow = row_header,
          startCol = col_data_i,
          borders = "all",
          headerStyle = style_header,
          colNames = TRUE
        )

        # 2. check column — formula comparing the data col to Classification
        col_data_letter <- num2let(col_data_i)
        check_formulas <- glue::glue('{col_data_letter}{seq_qc_rows} = {col_seg_letter}{seq_qc_rows}')
        check_tbl <- tibble::tibble(!!glue::glue("{var_i} = Class") := check_formulas)
        class(check_tbl[[1]]) <- "formula"
        openxlsx::writeData(
          wb, sheet_name,
          x = check_tbl,
          startRow = row_header,
          startCol = col_chk_i,
          borders = "all",
          headerStyle = style_header,
          colNames = TRUE
        )

        # TRUE / FALSE conditional formatting on the check col, matching the
        # existing QC formula block styling.
        openxlsx::conditionalFormatting(
          wb, sheet_name,
          cols = col_chk_i,
          rows = seq_qc_rows,
          rule = glue::glue('{num2let(col_chk_i)}{row_header + 1} = TRUE'),
          style = bulk_style_good_bold
        )
        openxlsx::conditionalFormatting(
          wb, sheet_name,
          cols = col_chk_i,
          rows = seq_qc_rows,
          rule = glue::glue('{num2let(col_chk_i)}{row_header + 1} = FALSE'),
          style = bulk_style_bad_bold
        )
      }
    }


    openxlsx::mergeCells(
      wb, sheet_name,
      rows = row_header - 2,
      cols = seq(col_bulk_qc_first, col_bulk_qc_block_last)
    )


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_header2,
      rows = row_header - 2,
      cols = seq(col_bulk_qc_first, col_bulk_qc_block_last),
      gridExpand = TRUE, stack = TRUE
    )


    for(i in c(row_header - 2, row_header)){
      oxl_outer_box(
        wb, sheet_name,
        borderStyle = "medium",
        row_start = i,
        row_end = i,
        col_start = col_bulk_qc_first,
        col_end = col_bulk_qc_block_last
      )
    }


    openxlsx::addStyle(
      wb, sheet_name,
      style = style_table,
      rows = seq(row_header + 1, row_header + nrow(data_solution_check)),
      cols = seq(col_bulk_qc_first, col_bulk_qc_block_last),
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
      col_end = col_bulk_qc_block_last
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
      style = bulk_style_good_bold
    )


    openxlsx::conditionalFormatting(
      wb, sheet_name,
      cols = seq(col_bulk_qc_formula_first, col_bulk_qc_formula_last),
      rows = seq(row_header + 1, row_header + nrow(data_solution_check)),
      rule = glue::glue('{num2let(col_bulk_qc_formula_first)}{row_header + 1} = FALSE'),
      style = bulk_style_bad_bold
    )


    #######################
    # Bulk frequency tables (Class Check / Membership / QC Seg)
    #######################

    col_freq_first <- col_bulk_qc_block_last + 2

    col_cc_label  <- col_freq_first
    col_cc_count  <- col_freq_first + 1
    col_ps_label  <- col_freq_first + 2
    col_ps_count  <- col_freq_first + 3
    col_mem_label <- col_freq_first + 4
    col_mem_count <- col_freq_first + 5
    col_qc_label  <- col_freq_first + 6
    col_qc_count  <- col_freq_first + 7

    class_check_range <- glue::glue("${num2let(col_bulk_qc_formula_last)}${row_data_first}:${num2let(col_bulk_qc_formula_last)}${row_last}")
    prob_sum_range    <- glue::glue("${num2let(col_prob_sum_check)}${row_data_first}:${num2let(col_prob_sum_check)}${row_last}")
    seg_range_freq    <- glue::glue("${num2let(col_seg)}${row_data_first}:${num2let(col_seg)}${row_last}")
    qc_seg_range      <- glue::glue("${num2let(col_bulk_qc_last)}${row_header + 1}:${num2let(col_bulk_qc_last)}${row_header + nrow(data_solution_check)}")

    # Class Check Counts
    cc_tbl <- tibble::tibble(
      "Result" = c("TRUE", "FALSE"),
      "Count"  = c(
        glue::glue("COUNTIF({class_check_range}, TRUE)"),
        glue::glue("COUNTIF({class_check_range}, FALSE)")
      )
    )
    class(cc_tbl[["Count"]]) <- "formula"

    # Prob Sum to 1 Counts
    ps_tbl <- tibble::tibble(
      "Result" = c("TRUE", "FALSE"),
      "Count"  = c(
        glue::glue("COUNTIF({prob_sum_range}, TRUE)"),
        glue::glue("COUNTIF({prob_sum_range}, FALSE)")
      )
    )
    class(ps_tbl[["Count"]]) <- "formula"

    # Membership counts (col_seg = bulk's classification col)
    mem_tbl <- tibble::tibble(
      "Segment" = glue::glue("Seg {segments}"),
      "Count"   = glue::glue("COUNTIF({seg_range_freq}, {segments})")
    )
    class(mem_tbl[["Count"]]) <- "formula"

    # Bulk QC Seg counts (col_bulk_qc_last = LDA-side seg col)
    qc_tbl <- tibble::tibble(
      "Segment" = glue::glue("Seg {segments}"),
      "Count"   = glue::glue("COUNTIF({qc_seg_range}, {segments})")
    )
    class(qc_tbl[["Count"]]) <- "formula"


    # write the three tables side-by-side, each with its section header above
    write_freq_table <- function(tbl, col_label, col_count, title){
      openxlsx::mergeCells(wb, sheet_name,
        rows = row_header - 2,
        cols = c(col_label, col_count)
      )
      openxlsx::writeData(wb, sheet_name,
        x = title,
        startRow = row_header - 2,
        startCol = col_label,
        colNames = FALSE
      )
      openxlsx::addStyle(wb, sheet_name,
        style = style_header2,
        rows = row_header - 2,
        cols = c(col_label, col_count),
        gridExpand = TRUE, stack = TRUE
      )
      openxlsx::writeData(wb, sheet_name,
        x = tbl,
        startRow = row_header,
        startCol = col_label,
        borders = "all",
        headerStyle = style_header,
        colNames = TRUE
      )
      openxlsx::addStyle(wb, sheet_name,
        style = style_table,
        rows = seq(row_header + 1, row_header + nrow(tbl)),
        cols = c(col_label, col_count),
        gridExpand = TRUE, stack = TRUE
      )
      oxl_outer_box(wb, sheet_name, borderStyle = "medium",
        row_start = row_header - 2, row_end = row_header - 2,
        col_start = col_label, col_end = col_count
      )
      oxl_outer_box(wb, sheet_name, borderStyle = "medium",
        row_start = row_header, row_end = row_header + nrow(tbl),
        col_start = col_label, col_end = col_count
      )
    }

    write_freq_table(cc_tbl,  col_cc_label,  col_cc_count,  "Class Check Counts")
    write_freq_table(ps_tbl,  col_ps_label,  col_ps_count,  "Prob Sum to 1 Counts")
    write_freq_table(mem_tbl, col_mem_label, col_mem_count, "Membership Counts")
    write_freq_table(qc_tbl,  col_qc_label,  col_qc_count,  "QC Seg Counts")

    col_last <- col_qc_count

    # Per-additional-var freq tables: Seg Counts continue right of QC Seg
    # Counts (same row band), and Check Counts (TRUE/FALSE) sit one row below
    # the seg counts table at the same column.
    if(has_additional_qc){
      n_seg_rows <- length(segments)
      row_check_label <- row_header + n_seg_rows + 3   # 2 empty rows below the seg counts table
      row_check_data  <- row_check_label + 1

      for(i in seq_len(n_additional_qc)){
        var_i        <- additional_bulk_qc_check[i]
        col_var_lbl  <- col_qc_count + 1L + (i - 1L) * 2L
        col_var_cnt  <- col_var_lbl + 1L

        # Seg Counts: per-segment count of the additional var's values
        # (= COUNTIF(<var_data_range>, <seg>)). The data column was written
        # at col_additional_qc_data_for(i).
        var_data_letter <- num2let(col_additional_qc_data_for(i))
        var_data_range  <- glue::glue("${var_data_letter}${row_data_first}:${var_data_letter}${row_last}")

        var_seg_tbl <- tibble::tibble(
          "Segment" = glue::glue("Seg {segments}"),
          "Count"   = glue::glue("COUNTIF({var_data_range}, {segments})")
        )
        class(var_seg_tbl[["Count"]]) <- "formula"

        write_freq_table(var_seg_tbl, col_var_lbl, col_var_cnt, glue::glue("{var_i} Seg Counts"))

        # Check Counts: TRUE / FALSE counts over the matching `<var> = Class`
        # check column, written below the seg counts table.
        var_check_letter <- num2let(col_additional_qc_check_for(i))
        var_check_range  <- glue::glue("${var_check_letter}${row_data_first}:${var_check_letter}${row_last}")

        var_check_tbl <- tibble::tibble(
          "Result" = c("TRUE", "FALSE"),
          "Count"  = c(
            glue::glue("COUNTIF({var_check_range}, TRUE)"),
            glue::glue("COUNTIF({var_check_range}, FALSE)")
          )
        )
        class(var_check_tbl[["Count"]]) <- "formula"

        # write_freq_table anchors its row positions at row_header / row_header - 2,
        # so we inline the equivalent at row_check_label / row_check_data here.
        openxlsx::mergeCells(wb, sheet_name,
          rows = row_check_label,
          cols = c(col_var_lbl, col_var_cnt)
        )
        openxlsx::writeData(wb, sheet_name,
          x = glue::glue("{var_i} Check Counts"),
          startRow = row_check_label,
          startCol = col_var_lbl,
          colNames = FALSE
        )
        openxlsx::addStyle(wb, sheet_name,
          style = style_header2,
          rows = row_check_label,
          cols = c(col_var_lbl, col_var_cnt),
          gridExpand = TRUE, stack = TRUE
        )
        openxlsx::writeData(wb, sheet_name,
          x = var_check_tbl,
          startRow = row_check_data,
          startCol = col_var_lbl,
          borders = "all",
          headerStyle = style_header,
          colNames = TRUE
        )
        openxlsx::addStyle(wb, sheet_name,
          style = style_table,
          rows = seq(row_check_data + 1, row_check_data + nrow(var_check_tbl)),
          cols = c(col_var_lbl, col_var_cnt),
          gridExpand = TRUE, stack = TRUE
        )
        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_check_label, row_end = row_check_label,
          col_start = col_var_lbl, col_end = col_var_cnt
        )
        oxl_outer_box(wb, sheet_name, borderStyle = "medium",
          row_start = row_check_data, row_end = row_check_data + nrow(var_check_tbl),
          col_start = col_var_lbl, col_end = col_var_cnt
        )
      }

      col_last <- col_qc_count + n_additional_qc * 2L
    }


    # additional_logic_advanced — Reassignment Counts table
    #
    # One row per advanced rule, count = COUNTIF(col_seg, rule$to_seg). This
    # is an aggregate per to_seg, so multiple rules sharing a to_seg show the
    # same count (acceptable approximation — the per-respondent attribution
    # to a specific rule would require hidden helper columns and isn't worth
    # the workbook bloat for the QC use case).
    if(!is.null(additional_logic_advanced_info) && length(additional_logic_advanced_info) > 0){
      col_adv_label <- col_last + 2L
      col_adv_count <- col_adv_label + 1L

      adv_seg_range <- glue::glue("${num2let(col_seg)}${row_data_first}:${num2let(col_seg)}${row_last}")
      adv_rule_labels <- purrr::map_chr(additional_logic_advanced_info, function(rule){
        kind <- if(rule$is_hard) "hard" else "soft"
        glue::glue("Rule {rule$display_index} ({kind}) -> {rule$to_seg}")
      })
      adv_rule_counts <- purrr::map_chr(additional_logic_advanced_info, function(rule){
        as.character(glue::glue("COUNTIF({adv_seg_range}, {rule$to_seg})"))
      })

      adv_tbl <- tibble::tibble(
        "Rule" = adv_rule_labels,
        "Count" = adv_rule_counts
      )
      class(adv_tbl[["Count"]]) <- "formula"

      write_freq_table(adv_tbl, col_adv_label, col_adv_count, "Reassignment Counts")

      col_last <- col_adv_count
    }


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

  # Each inner tool function applies its own grey-background canvas internally,
  # sized to that sheet's actual extent (see "grey background canvas" sections
  # in individual_ui_tool / documentation_tool / bulk_typing_tool).


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
