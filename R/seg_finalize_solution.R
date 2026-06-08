#' seg_finalize_solution
#'
#' @description Finalizes the single selected solution out of a prepped family
#'   produced by [seg_finalize_prep_solution()]. Given a `final_solution_name`
#'   (the prepped family) and an `items` count, this:
#'   \enumerate{
#'     \item picks the prepped solution at that item count;
#'     \item regenerates the LDA independently from the seg object (data,
#'       inputs, cluster target, prior, seed) and verifies the resulting segment
#'       assignment matches the prepped solution exactly — stopping if it does
#'       not, so the shipped deliverable is provably reproducible;
#'     \item registers it as `"{final_solution_name}_Final"` on the seg object;
#'     \item writes a single shell and typing tool into
#'       `3. Solutions/FINAL-SOLUTIONS/{final_solution_name} - Final/`.
#'   }
#'
#' @param seg A seg object with a prepped family in
#'   `seg$solutions$analysis[[final_solution_name]]` (run
#'   [seg_finalize_prep_solution()] first) and a shell ([seg_do_spec()]).
#' @param final_solution_name Character. Name of the prepped family to finalize
#'   from (e.g. `"Solution_A"`). The finalized solution is named
#'   `"{final_solution_name}_Final"`.
#' @param items Integer. The item count to finalize — selects the prepped
#'   solution whose LDA input set has exactly this many variables.
#' @param overwrite Logical. If `FALSE` (default) and the finalized solution /
#'   output folder already exists, the function stops. If `TRUE`, the existing
#'   output folder and registered rows/columns are removed first.
#' @param segment_names,qualification_instructions Passed through to
#'   [seg_typing_tool()].
#' @param survey_respondent_id Character. Respondent ID column for the typing
#'   tool. Defaults to the seg object's respondent id ([get_resp_id_name()]).
#' @param truncate,weighted,version,strategy,workers Passed through to
#'   [seg_write_shell_parallel()] for the solution shell.
#' @param setting_polar_threshold,setting_profile_threshold Numeric (defaults
#'   `0.2` / `0.15`). Significance/colouring thresholds for polar and profile
#'   variables, passed through to [seg_write_shell_parallel()].
#' @param seed Numeric. Random seed for the independent LDA refit. Defaults to
#'   `1` (matching [seg_short_form_analysis()]).
#'
#' @return The seg object, invisibly, with the finalized solution registered
#'   under `seg$solutions$analysis[["{final_solution_name}_Final"]]` and
#'   `summary_table` / `with_solutions` rebuilt.
#'
#' @export
seg_finalize_solution <- function(
    seg,
    final_solution_name,
    items,
    overwrite = FALSE,
    segment_names = NULL,
    qualification_instructions = rep("qualification goes here", 8),
    survey_respondent_id = NULL,
    truncate = c("no", "yes", "both"),
    weighted = TRUE,
    version = c("traditional", "both"),
    strategy = c("multisession", "multicore", "sequential", "cluster"),
    workers = NULL,
    setting_polar_threshold = 0.2,
    setting_profile_threshold = 0.15,
    seed = 1
){

  truncate <- match.arg(truncate)
  version  <- match.arg(version)
  strategy <- match.arg(strategy)

  id_name <- get_resp_id_name(seg)
  survey_respondent_id <- survey_respondent_id %||% id_name

  final_name <- glue::glue("{final_solution_name}_Final")


  # ---- resolve output path ----
  solution_root <- seg[["paths"]][["folders"]][["solution"]]
  if (is.null(solution_root) || is.na(solution_root)) solution_root <- getwd()
  final_folder <- file.path(
    solution_root, "FINAL-SOLUTIONS", glue::glue("{final_solution_name} - Final")
  )


  # ---- locate the prepped family + the selected item-count row ----
  fam_tbl <- seg[["solutions"]][["analysis"]][[final_solution_name]][["solution_table"]]
  if (is.null(fam_tbl) || nrow(fam_tbl) == 0) {
    stop(glue::glue(
      "No prepped family '{final_solution_name}' found. Run ",
      "seg_finalize_prep_solution(final_solution_name = '{final_solution_name}') first."
    ), call. = FALSE)
  }

  fam_tbl <- fam_tbl %>% dplyr::mutate(.n_items = lengths(lda_inputs))
  selected <- fam_tbl %>% dplyr::filter(.n_items == !!items)

  if (nrow(selected) == 0) {
    stop(glue::glue(
      "No prepped solution with {items} items in '{final_solution_name}'. ",
      "Available item counts: {paste(sort(unique(fam_tbl[['.n_items']])), collapse = ', ')}."
    ), call. = FALSE)
  }
  selected <- dplyr::slice(selected, 1)

  selected_lda_name <- selected[["lda_name"]]
  inputs            <- selected[["lda_inputs"]][[1]] %>% unlist()
  cluster_name      <- selected[["cluster_name"]]
  collinear_flag    <- isTRUE(selected[["collinear"]][[1]])


  # ---- existence check (folder OR registered solution name) ----
  existing_names <- seg[["solutions"]][["summary_table"]][["lda_name"]] %||% character()
  name_exists    <- final_name %in% existing_names
  folder_exists  <- dir.exists(final_folder)

  if ((folder_exists || name_exists) && !overwrite) {
    stop(glue::glue(
      "'{final_name}' already exists",
      if (folder_exists) " (output folder)" else "",
      if (folder_exists && name_exists) " and" else "",
      if (name_exists) " (registered solution)" else "",
      ". Set `overwrite = TRUE` to replace it."
    ), call. = FALSE)
  }

  if (overwrite) {
    if (dir.exists(final_folder)) unlink(final_folder, recursive = TRUE)
    seg[["solutions"]][["analysis"]][[final_name]] <- NULL
    if (final_name %in% existing_names && !is.null(seg[["data"]][["with_solutions"]])) {
      seg[["data"]][["with_solutions"]] <- seg[["data"]][["with_solutions"]] %>%
        dplyr::select(-dplyr::any_of(final_name))
    }
  }


  # ---- regenerate the LDA independently from the seg object ----
  df <- seg[["data"]][["with_solutions"]]
  df_complete <- df %>% dplyr::filter(!is.na(.data[[cluster_name]]))
  grp <- df_complete[[cluster_name]] %>% unlist() %>% setNames(NULL)

  # prior: the prepped fit's prior (a hyperparameter input, not the learned
  # model), so the independent refit reproduces the same solution.
  prior <- tryCatch(as.numeric(selected[["lda_fit"]][[1]][["prior"]]), error = function(e) NULL)
  if (!is.null(prior) && length(prior) > 0) prior <- prior / sum(prior)

  set.seed(seed)
  fit_indep <- if (is.null(prior)) {
    MASS::lda(x = df_complete %>% dplyr::select(dplyr::all_of(inputs)), grouping = grp)
  } else {
    MASS::lda(x = df_complete %>% dplyr::select(dplyr::all_of(inputs)), grouping = grp, prior = prior)
  }

  coef_indep <- if (collinear_flag) {
    coefficient_lda_colinear(fit_indep)
  } else {
    coefficient_lda(fit = fit_indep, input = df_complete %>% dplyr::select(dplyr::all_of(inputs)), grp = grp)
  }


  # ---- verify the independent fit reproduces the prepped solution exactly ----
  stored <- selected[["df_solution"]][[1]] %>%
    dplyr::transmute(id, stored_seg = as.numeric(as.character(.data[[selected_lda_name]]))) %>%
    dplyr::filter(!is.na(stored_seg))

  score_df <- df %>% dplyr::filter(.data[[id_name]] %in% stored[["id"]])
  pred_cls <- stats::predict(fit_indep, score_df %>% dplyr::select(dplyr::all_of(inputs)))[["class"]]
  pred_tbl <- tibble::tibble(
    id        = score_df[[id_name]],
    indep_seg = as.numeric(as.character(pred_cls))
  )

  cmp <- dplyr::inner_join(stored, pred_tbl, by = "id")
  n_missing  <- nrow(stored) - nrow(cmp)
  n_mismatch <- sum(cmp[["stored_seg"]] != cmp[["indep_seg"]], na.rm = TRUE)

  if (n_missing > 0 || n_mismatch > 0) {
    cli::cli_abort(c(
      "Independent LDA does not match the prepped solution '{selected_lda_name}'.",
      "x" = "{n_mismatch} of {nrow(cmp)} respondents reclassified differently.",
      if (n_missing > 0) c("x" = "{n_missing} prepped respondents could not be scored (missing inputs).") else NULL,
      "i" = "The prepped solution is not reproducible from the current seg data — re-run seg_finalize_prep_solution."
    ))
  }
  cli::cli_alert_success(
    "Verified: independent LDA reproduces '{selected_lda_name}' exactly ({nrow(cmp)} respondents)."
  )


  # ---- assemble the finalized row (independent fit + verified assignment) ----
  df_solution_final <- pred_tbl %>% dplyr::transmute(id, !!final_name := indep_seg)

  complete_pred <- df %>% dplyr::filter(dplyr::if_all(dplyr::all_of(inputs), ~ !is.na(.x)))
  lda_predict_final <- dplyr::bind_cols(
    tibble::tibble(seg_uuid = complete_pred[[id_name]]),
    .finalize_lda_score(fit_indep, complete_pred %>% dplyr::select(dplyr::all_of(inputs)))
  )

  final_row <- selected %>%
    dplyr::select(-dplyr::any_of(".n_items")) %>%
    dplyr::mutate(
      solution_name            = final_name,
      lda_name                 = final_name,
      lda_fit                  = list(fit_indep),
      lda_coefficient_function = list(coef_indep),
      lda_predict              = list(lda_predict_final),
      df_solution              = list(df_solution_final)
    )


  # ---- register, rebuild lookup + wide data ----
  seg[["solutions"]][["analysis"]][[final_name]][["solution_table"]] <- final_row
  seg[["solutions"]][["summary_table"]] <- seg_bind_summary_tables(seg)
  seg <- seg_build_with_solutions(seg)


  # ---- create folder + write shell and typing tool ----
  dir.create(final_folder, recursive = TRUE, showWarnings = FALSE)

  cli::cli_alert_info("Writing final shell to {.file {final_folder}}")
  seg_write_shell_parallel(
    seg                       = seg,
    solution_vars             = final_name,
    where                     = final_folder,
    strategy                  = strategy,
    workers                   = workers,
    truncate                  = truncate,
    version                   = version,
    weighted                  = weighted,
    setting_polar_threshold   = setting_polar_threshold,
    setting_profile_threshold = setting_profile_threshold
  )

  cli::cli_alert_info("Writing final typing tool to {.file {final_folder}}")
  seg_typing_tool(
    seg                        = seg,
    solution_name              = final_name,
    survey_respondent_id       = survey_respondent_id,
    segment_names              = segment_names,
    qualification_instructions = qualification_instructions,
    file_name                  = glue::glue("Typing Tool - {final_name}"),
    where                      = final_folder
  )

  cli::cli_alert_success("Finalized '{final_name}' ({items} items) into {.file {final_folder}}")

  invisible(seg)
}
