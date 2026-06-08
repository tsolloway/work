#' seg_finalize_prep_solution
#'
#' @description Finalizes a chosen LDA solution into a complete, deliverable
#'   bundle. Given a solution and a `final_solution_name`, this:
#'   \enumerate{
#'     \item runs [seg_short_form_analysis()] to produce the full solution plus
#'       every shorter item-count reduction down to `min_items`;
#'     \item registers each finalized solution on the seg object under a new
#'       analysis family named `final_solution_name` (its rows also carry
#'       `solution_name = final_solution_name`). The full (max-item) solution
#'       takes the bare `final_solution_name`; each reduction is suffixed with
#'       its item count
#'       (e.g. `Solution_A`, `Solution_A_11`, `Solution_A_10`, ...,
#'       `Solution_A_{min}`). `Solution_A` is the short-form max-item refit row,
#'       so all sizes are produced by the same pipeline and nest cleanly;
#'     \item creates an output folder named `final_solution_name` under a
#'       shared `FINAL-SOLUTIONS` folder in the project's solution folder
#'       (i.e. `3. Solutions/FINAL-SOLUTIONS/{final_solution_name}`) and writes
#'       the short-form analysis workbook and a shell for every finalized
#'       solution (in parallel) into it;
#'     \item writes a typing tool for every finalized solution into a
#'       `"{final_solution_name} - Typing Tools"` subfolder;
#'     \item prints the source solution's LDA accuracy and reliability metrics.
#'   }
#'
#'   If `final_solution_name` already exists — either as an output folder or as
#'   a registered solution name — the function stops unless `overwrite = TRUE`.
#'
#' @param seg A seg object with solutions in `seg$solutions$summary_table` and
#'   `seg$data$with_solutions` populated.
#' @param solution Character. Name of the source solution to finalize. Matched
#'   against `lda_name` first, then `solution_name`. Its full input set seeds
#'   the short-form reduction.
#' @param final_solution_name Character. Base name for the finalized solution,
#'   its output folder, and the bare full-size solution (e.g. `"Solution_A"`).
#' @param overwrite Logical. If `FALSE` (default) and `final_solution_name`
#'   already exists as a folder or registered solution, the function stops. If
#'   `TRUE`, the existing output folder and any previously finalized rows /
#'   columns for this name are removed first.
#' @param method Character. How to reduce the item set: `"sequential"`
#'   (default) runs a stepwise greedy backward reduction; `"best_combo"`
#'   evaluates every variable combination at each item count and keeps the best
#'   per size (slower, exhaustive).
#' @param reduce_type,priors,remove_first,min_items,max_items,max_combos,seed
#'   Passed through to [seg_short_form_analysis()] to control the item
#'   reduction. `reduce_type` and `remove_first` apply to `method =
#'   "sequential"`; `max_combos` applies to `method = "best_combo"`.
#' @param segment_labels,segment_descriptions Passed through to
#'   [seg_write_short_form_analysis()] for the analysis workbook's column
#'   headers and legend.
#' @param segment_names,qualification_instructions Passed through to
#'   [seg_typing_tool()].
#' @param survey_respondent_id Character. Respondent ID column for the typing
#'   tool. Defaults to the seg object's respondent id ([get_resp_id_name()]).
#' @param truncate,weighted,version,strategy,workers Passed through to
#'   [seg_write_shell_parallel()] for the solution shells.
#' @param setting_polar_threshold,setting_profile_threshold Numeric (defaults
#'   `0.2` / `0.15`). Significance/colouring thresholds for polar and profile
#'   variables, passed through to [seg_write_shell_parallel()].
#' @param setting_diff,setting_pvalue Numeric (defaults `0.1`). Seed the master
#'   Diff / p-value thresholds in the [seg_compare_segments()] workbook, which
#'   is written into the output folder restricted to the finalized solutions.
#'
#' @return The seg object, invisibly, with the finalized solutions registered
#'   under `seg$solutions$analysis[[final_solution_name]]` and `summary_table` /
#'   `with_solutions` rebuilt.
#'
#' @export
seg_finalize_prep_solution <- function(
    seg,
    solution,
    final_solution_name,
    overwrite = FALSE,
    method = c("sequential", "best_combo"),
    reduce_type = c("greedy_step", "greedy", "step"),
    priors = c("equal", "size"),
    remove_first = NULL,
    min_items = NULL,
    max_items = NULL,
    max_combos = 5000,
    seed = 1,
    segment_labels = NULL,
    segment_descriptions = NULL,
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
    setting_diff = 0.1,
    setting_pvalue = 0.1
){

  method      <- match.arg(method)
  reduce_type <- match.arg(reduce_type)
  truncate    <- match.arg(truncate)
  version     <- match.arg(version)
  strategy    <- match.arg(strategy)
  if (is.character(priors)) priors <- match.arg(priors)

  # `method` selects how the item set is reduced: "sequential" is the
  # stepwise greedy backward reduction; "best_combo" evaluates every
  # combination at each item count and keeps the best per size.
  direction <- if (method == "best_combo") "best_combo" else "backward"

  id_name <- get_resp_id_name(seg)
  survey_respondent_id <- survey_respondent_id %||% id_name


  # ---- resolve output paths ----
  # finalized bundles live under a shared "FINAL-SOLUTIONS" folder inside the
  # project's solution folder, one subfolder per finalized solution.
  solution_root <- seg[["paths"]][["folders"]][["solution"]]
  if (is.null(solution_root) || is.na(solution_root)) {
    solution_root <- getwd()
  }
  final_root   <- file.path(solution_root, "FINAL-SOLUTIONS")
  final_folder <- file.path(final_root, final_solution_name)
  tt_folder    <- file.path(final_folder, glue::glue("{final_solution_name} - Typing Tools"))


  # ---- existence check (folder OR registered solution name) ----
  existing_names <- seg[["solutions"]][["summary_table"]][["lda_name"]] %||% character()
  name_pattern   <- glue::glue("^{stringr::str_escape(final_solution_name)}(_\\d+)?$")
  name_exists    <- any(stringr::str_detect(existing_names, name_pattern))
  folder_exists  <- dir.exists(final_folder)

  if ((folder_exists || name_exists) && !overwrite) {
    stop(glue::glue(
      "'{final_solution_name}' already exists",
      if (folder_exists) " (output folder)" else "",
      if (folder_exists && name_exists) " and" else "",
      if (name_exists) " (registered solution)" else "",
      ". Set `overwrite = TRUE` to replace it."
    ), call. = FALSE)
  }

  if (overwrite) {
    # remove prior output folder
    if (dir.exists(final_folder)) unlink(final_folder, recursive = TRUE)

    # purge any prior finalized rows for this name from EVERY analysis family
    # (not just the same-named one). This also clears rows left behind by older
    # code versions that keyed the finalized family differently — otherwise the
    # stale rows duplicate the freshly-registered lda_names in summary_table.
    seg[["solutions"]][["analysis"]] <- seg[["solutions"]][["analysis"]] %>%
      purrr::map(function(fam) {
        st <- fam[["solution_table"]]
        if (!is.null(st) && nrow(st) > 0) {
          fam[["solution_table"]] <- dplyr::filter(st, !stringr::str_detect(lda_name, name_pattern))
        }
        fam
      }) %>%
      # drop families that are now empty (held only this name's finalized rows)
      purrr::keep(function(fam) {
        st <- fam[["solution_table"]]
        is.null(st) || nrow(st) > 0
      })

    # drop the matching columns from with_solutions so they can be rebuilt
    stale_cols <- existing_names[stringr::str_detect(existing_names, name_pattern)]
    if (length(stale_cols) > 0 && !is.null(seg[["data"]][["with_solutions"]])) {
      seg[["data"]][["with_solutions"]] <- seg[["data"]][["with_solutions"]] %>%
        dplyr::select(-dplyr::any_of(stale_cols))
    }
  }


  # ---- locate the full source solution row (with lda_fit / metrics) ----
  source_row <- purrr::keep(seg[["solutions"]][["analysis"]], is.list) %>%
    purrr::map(purrr::pluck, "solution_table") %>%
    purrr::compact() %>%
    dplyr::bind_rows() %>%
    dplyr::filter(lda_name == !!solution | solution_name == !!solution)

  if (nrow(source_row) == 0) {
    stop(glue::glue(
      "Source solution '{solution}' not found in any seg$solutions$analysis ",
      "family's solution_table."
    ), call. = FALSE)
  }
  source_row <- dplyr::slice(source_row, 1)


  # ---- run short-form analysis (full solution + reductions) ----
  sfa <- seg_short_form_analysis(
    seg          = seg,
    solution     = solution,
    id_name      = id_name,
    priors       = priors,
    reduce_type  = reduce_type,
    direction    = direction,
    max_combos   = max_combos,
    remove_first = remove_first,
    min_items    = min_items,
    max_items    = max_items,
    seed         = seed
  )

  results <- sfa[["results"]]
  if (is.null(results) || nrow(results) == 0) {
    stop("Short-form analysis returned no solutions.", call. = FALSE)
  }

  # max item count -> bare name; everything else -> "{name}_{n}"
  max_n <- max(results[["n"]])
  final_names <- purrr::map_chr(results[["n"]], function(n) {
    if (n == max_n) final_solution_name else glue::glue("{final_solution_name}_{n}")
  })


  # ---- assemble a `final`-family row for each item count ----
  df_all       <- seg[["data"]][["with_solutions"]]
  cluster_name <- source_row[["cluster_name"]]

  cli::cli_alert_info("Recomputing reliability for {nrow(results)} item size{?s}...")

  final_rows <- purrr::imap(final_names, function(this_name, i) {

    inputs_i <- results[["inputs"]][[i]] %>% unlist()
    fit_i    <- results[["lda_fit"]][[i]]

    # df_solution: id + finalized segment column (drives with_solutions)
    df_solution_i <- results[["lda_seg"]][[i]]
    names(df_solution_i)[2] <- this_name

    # lda_predict: posteriors (seg_1..k + seg) for the Bulk QC reference,
    # recomputed on complete cases for this item set
    complete_i <- df_all %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(inputs_i), ~ !is.na(.x)))
    lda_predict_i <- dplyr::bind_cols(
      tibble::tibble(seg_uuid = complete_i[[id_name]]),
      .finalize_lda_score(fit_i, complete_i %>% dplyr::select(dplyr::all_of(inputs_i)))
    )

    # accuracy + kappa from this size's confusion matrix; cv + split-half
    # reliability recomputed on this item set (mirrors cluster_add_lda)
    conf_i  <- results[["confusion"]][[i]]
    kappa_i <- purrr::pluck(conf_i, "overall", "Kappa", .default = NA_real_)
    rel_i   <- .finalize_reliability(
      inputs       = inputs_i,
      df           = df_all,
      cluster_name = cluster_name,
      prior        = as.numeric(fit_i[["prior"]]),
      seed         = seed
    )

    source_row %>% dplyr::mutate(
      solution_name            = final_solution_name,
      lda_name                 = this_name,
      lda_inputs               = list(inputs_i),
      lda_coefficient_function = list(results[["lda_coefficient_function"]][[i]]),
      lda_fit                  = list(fit_i),
      lda_predict              = list(lda_predict_i),
      accuracy                 = results[["accuracy_overall"]][[i]],
      kappa                    = kappa_i,
      cv                       = rel_i[["cv"]],
      split_half               = rel_i[["split_half"]],
      df_solution              = list(df_solution_i)
    )
  }) %>% dplyr::bind_rows()


  # ---- register the finalized family (keyed by name), rebuild lookup + data ----
  seg[["solutions"]][["analysis"]][[final_solution_name]][["solution_table"]] <- dplyr::bind_rows(
    seg[["solutions"]][["analysis"]][[final_solution_name]][["solution_table"]],
    final_rows
  )
  seg[["solutions"]][["summary_table"]] <- seg_bind_summary_tables(seg)
  seg <- seg_build_with_solutions(seg)


  # ---- create output folders ----
  dir.create(final_folder, recursive = TRUE, showWarnings = FALSE)
  dir.create(tt_folder, recursive = TRUE, showWarnings = FALSE)


  # ---- write short-form analysis workbook ----
  seg_write_short_form_analysis(
    short_form_analysis  = sfa,
    where                = final_folder,
    segment_labels       = segment_labels,
    segment_descriptions = segment_descriptions
  )


  # ---- write LDA Metrics workbook ----
  pn   <- seg[["meta"]][["project_name"]]
  pnum <- seg[["meta"]][["project_number"]]
  project_name <- if (!is.null(pn) && !is.null(pnum)) glue::glue("{pn} ({pnum})") else (pn %||% pnum)

  metrics_df <- final_rows %>%
    dplyr::mutate(.n_items = lengths(lda_inputs)) %>%
    dplyr::arrange(dplyr::desc(.n_items)) %>%
    dplyr::transmute(solution_name, cluster_name, lda_name, accuracy, kappa, cv)

  .finalize_write_lda_metrics(
    metrics_df   = metrics_df,
    where        = final_folder,
    project_name = project_name
  )


  # ---- write shells in parallel (one per finalized solution) ----
  cli::cli_alert_info("Writing {length(final_names)} shell{?s} to {.file {final_folder}}")
  seg_write_shell_parallel(
    seg                       = seg,
    solution_vars             = final_names,
    where                     = rep(final_folder, length(final_names)),
    strategy                  = strategy,
    workers                   = workers,
    truncate                  = truncate,
    version                   = version,
    weighted                  = weighted,
    setting_polar_threshold   = setting_polar_threshold,
    setting_profile_threshold = setting_profile_threshold
  )


  # ---- write a typing tool per finalized solution ----
  cli::cli_alert_info("Writing {length(final_names)} typing tool{?s} to {.file {tt_folder}}")
  purrr::walk(final_names, function(this_name) {
    seg_typing_tool(
      seg                        = seg,
      solution_name              = this_name,
      survey_respondent_id       = survey_respondent_id,
      segment_names              = segment_names,
      qualification_instructions = qualification_instructions,
      file_name                  = glue::glue("Typing Tool - {this_name}"),
      where                      = tt_folder
    )
  })


  # ---- segment comparison workbook (finalized solutions only) ----
  cli::cli_alert_info("Writing segment comparison to {.file {final_folder}}")
  seg_compare_segments(
    seg            = seg,
    solutions      = final_names,
    where          = final_folder,
    weighted       = weighted,
    setting_diff   = setting_diff,
    setting_pvalue = setting_pvalue
  )


  # ---- print accuracy + reliability for the finalized full solution ----
  full_row <- final_rows %>% dplyr::filter(lda_name == !!final_solution_name) %>% dplyr::slice(1)
  .finalize_print_metrics(full_row, solution, final_solution_name, length(final_names))

  invisible(seg)
}


#' Score an LDA fit into posterior + class columns
#'
#' @description Internal helper for [seg_finalize_prep_solution()]. Mirrors the
#'   posterior layout used elsewhere in the seg suite: columns `seg_1..seg_k`
#'   hold posteriors and the final `seg` column holds the predicted class.
#'
#' @keywords internal
#' @noRd
.finalize_lda_score <- function(fit, newdata) {
  pred <- stats::predict(fit, newdata)
  out <- dplyr::bind_cols(
    tibble::as_tibble(pred[["posterior"]]),
    tibble::tibble(class = pred[["class"]])
  ) %>% suppressMessages()
  out <- rlang::set_names(out, glue::glue("seg_{seq_len(ncol(out))}"))
  names(out)[ncol(out)] <- "seg"
  out
}


#' Recompute LDA cross-validation + split-half reliability for an item set
#'
#' @description Internal helper for [seg_finalize_prep_solution()]. Mirrors the
#'   reliability logic in `cluster_add_lda()`: leave-one-out cross-validation
#'   accuracy and split-half agreement, recomputed for a specific reduced item
#'   set against the source solution's cluster assignments.
#'
#' @param inputs Character vector of input variable names for this item set.
#' @param df The wide respondent data (`seg$data$with_solutions`).
#' @param cluster_name Character. Column holding the cluster (training target)
#'   assignment.
#' @param prior Numeric prior vector (normalized) in class-level order.
#' @param seed Numeric random seed.
#'
#' @return A list with numeric `cv` and `split_half` (each `NA_real_` on
#'   failure).
#'
#' @keywords internal
#' @noRd
.finalize_reliability <- function(inputs, df, cluster_name, prior, seed = 1) {

  d <- df %>%
    dplyr::filter(!is.na(.data[[cluster_name]])) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(inputs), ~ !is.na(.x)))

  grp <- d[[cluster_name]] %>% unlist() %>% setNames(NULL)
  X   <- d %>% dplyr::select(dplyr::all_of(inputs))

  if (!is.null(prior) && length(prior) > 0) prior <- prior / sum(prior)

  cv <- tryCatch({
    set.seed(seed)
    cv_fit <- suppressWarnings(
      MASS::lda(x = X, grouping = grp, prior = prior, CV = TRUE)
    )
    round(mean(cv_fit[["class"]] == grp), 10)
  }, error = function(e) NA_real_)

  split_half <- tryCatch({
    set.seed(seed)
    n    <- nrow(X)
    idx  <- sample(n)
    half <- floor(n / 2)
    idx_a <- idx[seq_len(half)]
    idx_b <- idx[(half + 1):n]

    lda_a <- suppressWarnings(MASS::lda(x = X[idx_a, ], grouping = grp[idx_a], prior = prior))
    lda_b <- suppressWarnings(MASS::lda(x = X[idx_b, ], grouping = grp[idx_b], prior = prior))

    pred_a <- stats::predict(lda_a, X)[["class"]]
    pred_b <- stats::predict(lda_b, X)[["class"]]
    round(mean(pred_a == pred_b), 10)
  }, error = function(e) NA_real_)

  list(cv = cv, split_half = split_half)
}


#' Print finalized solution accuracy + reliability
#'
#' @description Internal helper for [seg_finalize_prep_solution()]. Prints the
#'   finalized full solution's LDA accuracy (accuracy, kappa) and reliability
#'   (cross-validation, split-half) metrics.
#'
#' @keywords internal
#' @noRd
.finalize_print_metrics <- function(metrics_row, solution, final_solution_name, n_sizes) {

  pct <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "—" else scales::percent(x, accuracy = 0.1)
  num <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "—" else format(round(x, 3), nsmall = 3)

  cli::cli_h2("Finalized '{final_solution_name}' ({n_sizes} item size{?s}) from '{solution}'")
  cli::cli_h3("Accuracy")
  cli::cli_ul()
  cli::cli_li("Overall accuracy: {.strong {pct(metrics_row[['accuracy']][[1]])}}")
  cli::cli_li("Kappa: {.strong {num(metrics_row[['kappa']][[1]])}}")
  cli::cli_end()
  cli::cli_h3("Reliability")
  cli::cli_ul()
  cli::cli_li("Cross-validation accuracy: {.strong {pct(metrics_row[['cv']][[1]])}}")
  cli::cli_li("Split-half reliability: {.strong {pct(metrics_row[['split_half']][[1]])}}")
  cli::cli_end()
}


#' Write the LDA Metrics workbook
#'
#' @description Internal helper for [seg_finalize_prep_solution()]. Writes a
#'   single-sheet "LDA Metrics" workbook (solution name, cluster name, LDA name,
#'   accuracy, kappa, cross-validation accuracy) formatted to match the suite's
#'   other deliverable tables (bold title, project subtitle, grey header row,
#'   medium outer box, percentage / decimal number formats).
#'
#' @param metrics_df Tibble with columns `solution_name`, `cluster_name`,
#'   `lda_name`, `accuracy`, `kappa`, `cv`.
#' @param where Character. Output directory.
#' @param project_name Character. Project name shown in the subtitle.
#'
#' @return Invisibly, the workbook path.
#'
#' @keywords internal
#' @noRd
.finalize_write_lda_metrics <- function(metrics_df, where, project_name = NULL) {

  sheet     <- "lda_metrics"
  col_start <- 2L

  row_title    <- 2L
  row_subtitle <- 3L
  row_header   <- 5L
  row_data     <- row_header + 1L
  row_data_end <- row_data + nrow(metrics_df) - 1L

  col_sol     <- col_start          # B  solution name
  col_cluster <- col_start + 1L     # C  cluster name
  col_lda     <- col_start + 2L     # D  lda name
  col_acc     <- col_start + 3L     # E  accuracy
  col_kappa   <- col_start + 4L     # F  kappa
  col_cv      <- col_start + 5L     # G  cross-validation accuracy
  col_end     <- col_cv

  headers <- c("Solution", "Cluster", "LDA Name", "Accuracy", "Kappa", "CV Accuracy")


  # -- workbook + styles (consistent with seg_write_short_form_analysis) --
  wb <- oxl_create_workbook()
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)

  s_title    <- openxlsx::createStyle(textDecoration = "Bold", fontSize = 18)
  s_subtitle <- openxlsx::createStyle(textDecoration = c("Bold", "italic"), fontSize = 14)
  s_header   <- openxlsx::createStyle(
    textDecoration = "Bold", fontSize = 12, halign = "center", valign = "center",
    wrapText = TRUE, fgFill = oxl_colorscale_grey("2")
  )
  s_text    <- oxl_style_halign("left")
  s_percent <- oxl_style_percent(1)
  s_number  <- oxl_style_number(3)


  # -- title + subtitle --
  openxlsx::writeData(wb, sheet, "LDA Metrics", startRow = row_title, startCol = col_start, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, s_title, rows = row_title, cols = col_start, stack = TRUE)

  if (!is.null(project_name)) {
    openxlsx::writeData(wb, sheet, project_name, startRow = row_subtitle, startCol = col_start, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, s_subtitle, rows = row_subtitle, cols = col_start, stack = TRUE)
  }


  # -- header row --
  for (i in seq_along(headers)) {
    openxlsx::writeData(wb, sheet, headers[i], startRow = row_header, startCol = col_start + i - 1L, colNames = FALSE)
  }
  openxlsx::addStyle(wb, sheet, s_header, rows = row_header, cols = seq(col_sol, col_end), gridExpand = TRUE, stack = TRUE)


  # -- data --
  openxlsx::writeData(wb, sheet, metrics_df[["solution_name"]], startRow = row_data, startCol = col_sol,     colNames = FALSE)
  openxlsx::writeData(wb, sheet, metrics_df[["cluster_name"]],  startRow = row_data, startCol = col_cluster, colNames = FALSE)
  openxlsx::writeData(wb, sheet, metrics_df[["lda_name"]],      startRow = row_data, startCol = col_lda,     colNames = FALSE)
  openxlsx::writeData(wb, sheet, metrics_df[["accuracy"]],      startRow = row_data, startCol = col_acc,     colNames = FALSE)
  openxlsx::writeData(wb, sheet, metrics_df[["kappa"]],         startRow = row_data, startCol = col_kappa,   colNames = FALSE)
  openxlsx::writeData(wb, sheet, metrics_df[["cv"]],            startRow = row_data, startCol = col_cv,      colNames = FALSE)

  data_rows <- seq(row_data, row_data_end)
  openxlsx::addStyle(wb, sheet, s_text,    rows = data_rows, cols = seq(col_sol, col_lda), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet, s_percent, rows = data_rows, cols = c(col_acc, col_cv),    gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet, s_number,  rows = data_rows, cols = col_kappa,             gridExpand = TRUE, stack = TRUE)


  # -- borders: outer box + header underline + vertical dividers --
  oxl_outer_box(wb, sheet,
    row_start = row_header, row_end = row_data_end,
    col_start = col_sol, col_end = col_end, borderStyle = "medium"
  )
  openxlsx::addStyle(wb, sheet,
    openxlsx::createStyle(border = "bottom", borderStyle = "medium"),
    rows = row_header, cols = seq(col_sol, col_end), gridExpand = TRUE, stack = TRUE
  )
  s_divider <- openxlsx::createStyle(border = "left", borderStyle = "medium")
  for (dc in c(col_acc)) {  # divide the identifier columns from the metric columns
    openxlsx::addStyle(wb, sheet, s_divider, rows = seq(row_header, row_data_end), cols = dc, gridExpand = TRUE, stack = TRUE)
  }


  # -- column widths --
  openxlsx::setColWidths(wb, sheet, cols = c(col_sol, col_cluster, col_lda), widths = 26)
  openxlsx::setColWidths(wb, sheet, cols = c(col_acc, col_kappa, col_cv), widths = 13)


  file_path <- file.path(where, "LDA Metrics.xlsx")
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  cli::cli_alert_success("Wrote LDA metrics to {.file {file_path}}")
  invisible(file_path)
}
