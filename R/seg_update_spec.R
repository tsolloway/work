#' seg_update_spec
#'
#' @description Re-reads and re-executes the segmentation spec, updating shell
#'   variables in `seg$data$with_shell` and `seg$data$with_solutions` without
#'   destroying other columns (e.g., solution assignments, okay_filter).
#'
#' @param seg A seg object that has already been through [seg_get_spec()] and
#'   optionally [seg_cluster_with_profiles()].
#' @param spec_path Character. Path to the spec Excel file. If `NULL`, uses the
#'   path stored in `seg$paths$files$spec`.
#' @param resp_id Character. Name of the respondent ID column. If `NULL`,
#'   falls back to `seg$meta$id_variable`.
#' @param execute_debug Logical. If `TRUE`, prints debug info during spec
#'   execution (default: `FALSE`).
#' @param verbose Logical. If `TRUE`, prints summary of changes (default: `TRUE`).
#'
#' @return The seg object with updated spec, shell, and data objects.
#'
#' @export
seg_update_spec <- function(seg, spec_path = NULL, resp_id = NULL, execute_debug = FALSE, verbose = TRUE) {

  work::start()

  # ---- resolve resp_id ----
  if (is.null(resp_id)) resp_id <- seg[["meta"]][["id_variable"]]
  if (is.null(resp_id)) stop("Cannot resolve respondent ID. Pass resp_id or ensure seg$meta$id_variable is set.", call. = FALSE)

  # ---- collect old shell var names ----
  old_shell_vars <- character(0)

  if (!is.null(seg[["spec"]][["polars"]])) {
    old_polar_vars <- seg[["spec"]][["polars"]] %>%
      tidyr::unnest(cols = vars) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::pull(var)

    old_rs_vars <- seg[["spec"]][["polars"]] %>%
      tidyr::unnest(cols = vars) %>%
      dplyr::filter(!is.na(source_var)) %>%
      dplyr::pull(source_var) %>%
      unique() %>%
      paste0("rs_", .)

    old_shell_vars <- c(old_shell_vars, old_polar_vars, old_rs_vars)
  }

  if (!is.null(seg[["spec"]][["profiles"]])) {
    old_profile_vars <- seg[["spec"]][["profiles"]] %>%
      tidyr::unnest(cols = vars) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::pull(var)

    old_shell_vars <- c(old_shell_vars, old_profile_vars)
  }

  # coalesced source vars
  if (!is.null(seg[["spec"]][["polars"]])) {
    all_source <- seg[["spec"]][["polars"]] %>%
      tidyr::unnest(cols = vars) %>%
      dplyr::pull(source_var)
    coalesced_mask <- grepl("_", all_source) & !all_source %in% names(seg[["data"]][["original"]])
    if (any(coalesced_mask)) {
      old_shell_vars <- c(old_shell_vars, unique(all_source[coalesced_mask]))
    }
  }

  old_shell_vars <- unique(old_shell_vars)
  n_old <- length(old_shell_vars)

  # ---- identify non-shell columns ----
  original_cols <- names(seg[["data"]][["original"]])
  has_with_shell <- !is.null(seg[["data"]][["with_shell"]])
  has_with_solutions <- !is.null(seg[["data"]][["with_solutions"]])

  extra_shell_cols <- character(0)
  extra_solution_cols <- character(0)

  # ---- stash extra columns before re-executing ----
  extra_shell_df <- NULL
  extra_solution_df <- NULL

  if (has_with_shell) {
    extra_shell_cols <- setdiff(names(seg[["data"]][["with_shell"]]), c(original_cols, old_shell_vars))
    if (length(extra_shell_cols) > 0) {
      extra_shell_df <- seg[["data"]][["with_shell"]] %>%
        dplyr::select(dplyr::all_of(c(resp_id, extra_shell_cols)))
    }
  }

  if (has_with_solutions) {
    extra_solution_cols <- setdiff(names(seg[["data"]][["with_solutions"]]), c(original_cols, old_shell_vars))
    solution_only_cols <- setdiff(extra_solution_cols, extra_shell_cols)
    if (length(solution_only_cols) > 0) {
      extra_solution_df <- seg[["data"]][["with_solutions"]] %>%
        dplyr::select(dplyr::all_of(c(resp_id, solution_only_cols)))
    }
  }

  # ---- re-read and re-execute spec ----
  seg <- seg %>% seg_get_spec(spec_path = spec_path, execute = TRUE, execute_debug = execute_debug)

  # ---- collect new shell var names ----
  new_polar_vars <- seg[["spec"]][["polars"]] %>%
    tidyr::unnest(cols = vars) %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::pull(var)

  new_rs_vars <- seg[["spec"]][["polars"]] %>%
    tidyr::unnest(cols = vars) %>%
    dplyr::filter(!is.na(source_var)) %>%
    dplyr::pull(source_var) %>%
    unique() %>%
    paste0("rs_", .)

  new_profile_vars <- seg[["spec"]][["profiles"]] %>%
    tidyr::unnest(cols = vars) %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::pull(var)

  new_shell_vars <- unique(c(new_polar_vars, new_rs_vars, new_profile_vars))
  n_new <- length(new_shell_vars)

  # ---- re-attach extra columns to with_shell ----
  if (!is.null(extra_shell_df)) {
    safe_extra <- setdiff(names(extra_shell_df), c(new_shell_vars, resp_id))
    if (length(safe_extra) > 0) {
      seg[["data"]][["with_shell"]] <- seg[["data"]][["with_shell"]] %>%
        dplyr::left_join(
          extra_shell_df %>% dplyr::select(dplyr::all_of(c(resp_id, safe_extra))),
          by = resp_id
        )
    }
  }

  # ---- rebuild with_solutions ----
  if (has_with_solutions) {
    df_solutions <- seg[["data"]][["with_shell"]]

    if (!is.null(extra_solution_df)) {
      safe_solution <- setdiff(names(extra_solution_df), c(names(df_solutions), resp_id))
      if (length(safe_solution) > 0) {
        df_solutions <- df_solutions %>%
          dplyr::left_join(
            extra_solution_df %>% dplyr::select(dplyr::all_of(c(resp_id, safe_solution))),
            by = resp_id
          )
      }
    }

    seg[["data"]][["with_solutions"]] <- df_solutions
  }

  # ---- report ----
  if (verbose) {
    added <- setdiff(new_shell_vars, old_shell_vars)
    removed <- setdiff(old_shell_vars, new_shell_vars)
    unchanged <- intersect(old_shell_vars, new_shell_vars)

    n_shell_extra <- if (!is.null(extra_shell_df)) length(setdiff(names(extra_shell_df), c(new_shell_vars, resp_id))) else 0L
    n_solution_extra <- if (!is.null(extra_solution_df)) length(setdiff(names(extra_solution_df), c(names(seg[["data"]][["with_shell"]]), resp_id))) else 0L

    cli::cli_h2("Spec updated")
    cli::cli_text("{n_old} old shell vars -> {n_new} new shell vars")

    if (length(added) > 0) {
      cli::cli_alert_success("{length(added)} added: {.val {utils::head(added, 10)}}{if (length(added) > 10) paste0(' ... +', length(added) - 10, ' more') else ''}")
    }
    if (length(removed) > 0) {
      cli::cli_alert_warning("{length(removed)} removed: {.val {utils::head(removed, 10)}}{if (length(removed) > 10) paste0(' ... +', length(removed) - 10, ' more') else ''}")
    }
    if (length(unchanged) > 0) {
      cli::cli_alert_info("{length(unchanged)} re-executed in place")
    }
    if (n_shell_extra > 0) {
      cli::cli_alert_info("{n_shell_extra} extra with_shell columns preserved")
    }
    if (has_with_solutions) {
      cli::cli_alert_info("{n_solution_extra} solution columns preserved on with_solutions")
    }
  }

  return(seg)
}
