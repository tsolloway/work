#' seg_analyze_solutions
#'
#' @description Analyzes all written solution files and ranks them under five
#'   perspectives. Each perspective returns a tibble of the top solutions with
#'   relevant metrics. The function reads the Excel files previously written by
#'   [seg_write_solutions()] and extracts summary-sheet and per-segment data.
#'
#'   ## Perspectives
#'
#'   **P1 — Actionable + Real-world.** Balanced segment sizes plus profile
#'   differentiation. Composite of segment balance (SD of proportions, inverted),
#'   minimum segment size, average profile range, count of profiles exceeding
#'   threshold, and LDA accuracy.
#'
#'   **P2 — Maximum Differentiation.** Broad spread across all variables.
#'   Composite of average spread (SD of segment means), median spread, count of
#'   vars with spread > 0.05, even-differentiation count (outlier ratio < 3),
#'   and LDA accuracy.
#'
#'   **P3 — Threshold Hits.** Every segment must be presentable. Reads per-segment
#'   sheets and counts hits (polar diff >= `polar_threshold`, profile diff >=
#'   `profile_threshold`). Ranks by minimum hits across segments, breaking ties
#'   with average hits. Includes LDA accuracy in composite.
#'
#'   **P4 — Typing Tool Quality.** LDA accuracy weighted heavily, combined with
#'   segment balance and total hits. Answers: which solutions can be deployed as
#'   a reliable typing tool?
#'
#'   **P5 — Polar vs Profile Balance.** Penalizes lopsided hit distributions
#'   within segments. A segment with 40 polar hits and 0 profile hits is hard to
#'   action on. Composite of minimum per-segment polar-profile balance ratio,
#'   average balance ratio, total hits, and LDA accuracy.
#'
#' @param seg A seg object with solutions written via [seg_write_solutions()].
#' @param where Character. Path to the solutions directory. Defaults to
#'   `seg[["paths"]][["folders"]][["solution"]]`.
#' @param polar_threshold Numeric. Minimum absolute diff for a polar variable
#'   to count as a "hit" (default: `0.20`).
#' @param profile_threshold Numeric. Minimum absolute diff for a profile
#'   variable to count as a "hit" (default: `0.15`).
#' @param top_n Integer. Number of top solutions to return per perspective
#'   (default: `10`).
#' @param only_opt Logical. If `TRUE`, only analyze reduced-inputs (LDA_opt_)
#'   solutions (default: `FALSE`).
#' @param verbose Logical. Print ranked tables to console (default: `TRUE`).
#'
#' @return A named list with elements `p1` through `p5` (tibbles), `metrics`
#'   (full metrics table), and `details` (per-solution parsed data).
#'
#' @export
seg_analyze_solutions <- function(
    seg,
    where = NULL,
    polar_threshold = 0.20,
    profile_threshold = 0.15,
    top_n = 10,
    only_opt = FALSE,
    verbose = TRUE
) {

  if (is.null(where)) {
    where <- seg[["paths"]][["folders"]][["solution"]]
  }
  if (is.null(where) || is.na(where)) {
    where <- getwd()
  }

  # ---- discover solution files ----
  all_files <- list.files(
    where, pattern = "^Solution - .*\\.xlsx$",
    recursive = TRUE, full.names = TRUE
  )
  all_files <- all_files[!grepl("~\\$", all_files)]

  if (length(all_files) == 0) {
    cli::cli_abort("No solution files found in {.path {where}}")
  }

  # ---- get accuracy from seg object ----
  summary_tbl <- seg[["solutions"]][["summary_table"]]

  if (only_opt) {
    summary_tbl <- summary_tbl %>%
      dplyr::filter(grepl("^LDA_opt_", lda_name))
  }

  accuracy_lookup <- summary_tbl %>%
    dplyr::select(lda_name, accuracy) %>%
    dplyr::mutate(accuracy = as.numeric(accuracy))

  # ---- polar prefixes from spec ----
  polar_prefixes <- seg[["spec"]][["polars"]][["prefix"]]

  # ---- parse one solution file ----
  .parse_solution <- function(filepath) {
    sol_name <- gsub("^Solution - ", "",
                     tools::file_path_sans_ext(basename(filepath)))

    tryCatch({
      sheets <- openxlsx::getSheetNames(filepath)
      summary_sheet <- sheets[tolower(sheets) == "summary"][1]   # tolerate "summary" (old) or "Summary" (new)
      df <- openxlsx::read.xlsx(filepath, sheet = summary_sheet, colNames = FALSE)

      # header row detection
      header_row <- df[9, ]
      seg_cols <- which(grepl("^Seg", as.character(header_row)))
      n_segs <- length(seg_cols)
      if (n_segs == 0) return(NULL)

      # segment sizes
      seg_ns   <- as.numeric(df[10, seg_cols])
      seg_pcts <- as.numeric(df[11, seg_cols])
      total_n  <- as.numeric(df[10, 4])

      # column positions
      range_col <- which(as.character(header_row) == "Range")
      type_col  <- which(as.character(header_row) == "Type")
      if (length(type_col) == 0 || length(range_col) == 0) return(NULL)

      range_col_name <- names(df)[range_col]
      type_col_name  <- names(df)[type_col]

      # variable rows
      var_rows <- which(!is.na(df[[1]]) & !is.na(df[[type_col_name]]))
      if (length(var_rows) == 0) return(NULL)

      var_data <- tibble::tibble(
        var    = as.character(df[var_rows, 1]),
        prefix = stringr::str_extract(as.character(df[var_rows, 1]), "^[A-Z]+"),
        type   = ifelse(
          stringr::str_extract(as.character(df[var_rows, 1]), "^[A-Z]+") %in%
            polar_prefixes, "polar", "profile"
        ),
        range  = as.numeric(df[var_rows, range_col_name])
      )

      # segment means per variable
      seg_means_mat <- as.data.frame(
        lapply(seg_cols, function(sc) as.numeric(df[var_rows, sc]))
      )
      names(seg_means_mat) <- paste0("seg_", seq_len(n_segs))
      var_data$spread_sd <- apply(seg_means_mat, 1, stats::sd, na.rm = TRUE)

      # ---- per-segment hits ----
      seg_sheets <- sheets[grepl("^Seg ", sheets)]

      seg_hits <- purrr::map_dfr(seq_len(n_segs), function(s) {
        sheet_name <- paste("Seg", s)
        if (!sheet_name %in% sheets) {
          return(tibble::tibble(
            seg = s, polar_hits = 0L, profile_hits = 0L, total_hits = 0L
          ))
        }

        seg_df <- openxlsx::read.xlsx(filepath, sheet = sheet_name,
                                       colNames = FALSE)
        seg_var_rows <- which(
          !is.na(seg_df[[1]]) & grepl("^[A-Z]{2}\\d", seg_df[[1]])
        )
        if (length(seg_var_rows) == 0) {
          return(tibble::tibble(
            seg = s, polar_hits = 0L, profile_hits = 0L, total_hits = 0L
          ))
        }

        seg_vars  <- as.character(seg_df[seg_var_rows, 1])
        seg_diffs <- as.numeric(seg_df[seg_var_rows, 7])
        seg_type  <- ifelse(
          stringr::str_extract(seg_vars, "^[A-Z]+") %in% polar_prefixes,
          "polar", "profile"
        )

        ph <- sum(abs(seg_diffs[seg_type == "polar"]) >= polar_threshold,
                  na.rm = TRUE)
        prh <- sum(abs(seg_diffs[seg_type == "profile"]) >= profile_threshold,
                   na.rm = TRUE)

        tibble::tibble(
          seg = s, polar_hits = as.integer(ph),
          profile_hits = as.integer(prh),
          total_hits = as.integer(ph + prh)
        )
      })

      # match accuracy from seg object
      acc <- accuracy_lookup %>%
        dplyr::filter(grepl(sol_name, lda_name, fixed = TRUE))
      acc_val <- if (nrow(acc) > 0) max(acc$accuracy, na.rm = TRUE) else NA_real_

      list(
        name       = sol_name,
        n_segs     = n_segs,
        total_n    = total_n,
        seg_ns     = seg_ns,
        seg_pcts   = seg_pcts,
        accuracy   = acc_val,
        var_data   = var_data,
        seg_hits   = seg_hits
      )

    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_warning("Error parsing {.file {basename(filepath)}}: {conditionMessage(e)}")
      }
      NULL
    })
  }

  # ---- process all files ----
  if (verbose) cli::cli_alert_info("Processing {length(all_files)} solution files")
  results <- purrr::map(
    all_files, .parse_solution,
    .progress = if (verbose) "Parsing solutions" else FALSE
  )
  results <- purrr::compact(results)

  if (length(results) == 0) {
    cli::cli_abort("No solutions could be parsed")
  }
  if (verbose) cli::cli_alert_success("{length(results)} solutions parsed")

  # ---- build metrics table ----
  metrics <- purrr::map_dfr(results, function(r) {
    vd  <- r$var_data
    sh  <- r$seg_hits
    pvd <- vd %>% dplyr::filter(type == "profile")

    # outlier ratio for even-differentiation
    or <- vd$range / (vd$spread_sd * sqrt(r$n_segs))

    # per-segment polar/profile balance ratio (min of polar/profile, profile/polar)
    balance_ratios <- purrr::map_dbl(seq_len(nrow(sh)), function(i) {
      ph  <- sh$polar_hits[i]
      prh <- sh$profile_hits[i]
      if (ph == 0 && prh == 0) return(0)
      min(ph, prh) / max(ph, prh)
    })

    tibble::tibble(
      solution              = r$name,
      n_segs                = r$n_segs,
      total_n               = r$total_n,
      accuracy              = r$accuracy,
      seg_sizes             = paste0(round(r$seg_pcts * 100, 1), "%",
                                     collapse = " / "),
      seg_balance_sd        = stats::sd(r$seg_pcts, na.rm = TRUE),
      min_seg_pct           = min(r$seg_pcts, na.rm = TRUE),
      avg_range_all         = mean(vd$range, na.rm = TRUE),
      avg_range_profile     = mean(pvd$range, na.rm = TRUE),
      n_profile_range_gt_t  = sum(pvd$range > profile_threshold, na.rm = TRUE),
      avg_spread_all        = mean(vd$spread_sd, na.rm = TRUE),
      median_spread_all     = stats::median(vd$spread_sd, na.rm = TRUE),
      n_spread_gt_05        = sum(vd$spread_sd > 0.05, na.rm = TRUE),
      n_even_diff           = sum(or < 3.0, na.rm = TRUE),
      total_hits            = sum(sh$total_hits),
      avg_hits_per_seg      = mean(sh$total_hits),
      min_hits_seg          = min(sh$total_hits),
      segs_with_30plus      = sum(sh$total_hits >= 30),
      min_balance_ratio     = min(balance_ratios),
      avg_balance_ratio     = mean(balance_ratios),
      min_polar_hits        = min(sh$polar_hits),
      min_profile_hits      = min(sh$profile_hits)
    )
  })

  # ---- normalize helper ----
  .norm <- function(x, invert = FALSE) {
    rng <- range(x, na.rm = TRUE)
    if (rng[2] == rng[1]) return(rep(0.5, length(x)))
    n <- (x - rng[1]) / (rng[2] - rng[1])
    if (invert) n <- 1 - n
    n
  }

  # fill NA accuracy with 0 for scoring
  acc_filled <- ifelse(is.na(metrics$accuracy), 0, metrics$accuracy)

  # ============================================================
  # P1 — Actionable + Real-world
  # ============================================================
  p1 <- metrics %>%
    dplyr::mutate(
      s_balance    = .norm(seg_balance_sd, invert = TRUE),
      s_min_size   = .norm(min_seg_pct),
      s_prof_range = .norm(avg_range_profile),
      s_prof_count = .norm(n_profile_range_gt_t),
      s_accuracy   = .norm(acc_filled),
      score = 0.25 * s_balance + 0.15 * s_min_size +
              0.20 * s_prof_range + 0.15 * s_prof_count +
              0.25 * s_accuracy
    ) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(rank, solution, n_segs, seg_sizes, accuracy,
                  seg_balance_sd, min_seg_pct, avg_range_profile,
                  n_profile_range_gt_t, score)

  # ============================================================
  # P2 — Maximum Differentiation
  # ============================================================
  p2 <- metrics %>%
    dplyr::mutate(
      s_spread     = .norm(avg_spread_all),
      s_med_spread = .norm(median_spread_all),
      s_count      = .norm(n_spread_gt_05),
      s_even       = .norm(n_even_diff),
      s_accuracy   = .norm(acc_filled),
      score = 0.25 * s_spread + 0.15 * s_med_spread +
              0.15 * s_count + 0.15 * s_even +
              0.30 * s_accuracy
    ) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(rank, solution, n_segs, seg_sizes, accuracy,
                  avg_spread_all, median_spread_all, n_spread_gt_05,
                  n_even_diff, score)

  # ============================================================
  # P3 — Threshold Hits
  # ============================================================
  p3 <- metrics %>%
    dplyr::mutate(
      s_min_hits = .norm(min_hits_seg),
      s_avg_hits = .norm(avg_hits_per_seg),
      s_total    = .norm(total_hits),
      s_accuracy = .norm(acc_filled),
      score = 0.30 * s_min_hits + 0.20 * s_avg_hits +
              0.20 * s_total + 0.30 * s_accuracy
    ) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(rank, solution, n_segs, seg_sizes, accuracy,
                  total_hits, avg_hits_per_seg, min_hits_seg,
                  segs_with_30plus, score)

  # ============================================================
  # P4 — Typing Tool Quality
  # ============================================================
  p4 <- metrics %>%
    dplyr::mutate(
      s_accuracy = .norm(acc_filled),
      s_balance  = .norm(seg_balance_sd, invert = TRUE),
      s_hits     = .norm(total_hits),
      score = 0.55 * s_accuracy + 0.20 * s_balance + 0.25 * s_hits
    ) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(rank, solution, n_segs, seg_sizes, accuracy,
                  seg_balance_sd, total_hits, score)

  # ============================================================
  # P5 — Polar vs Profile Balance
  # ============================================================
  p5 <- metrics %>%
    dplyr::mutate(
      s_min_bal  = .norm(min_balance_ratio),
      s_avg_bal  = .norm(avg_balance_ratio),
      s_hits     = .norm(total_hits),
      s_accuracy = .norm(acc_filled),
      score = 0.25 * s_min_bal + 0.20 * s_avg_bal +
              0.25 * s_hits + 0.30 * s_accuracy
    ) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(rank, solution, n_segs, seg_sizes, accuracy,
                  min_balance_ratio, avg_balance_ratio,
                  min_polar_hits, min_profile_hits, total_hits, score)

  # ---- console output ----
  if (verbose) {
    .print_header <- function(title, subtitle) {
      cli::cli_rule()
      cli::cli_h1(title)
      cli::cli_text(subtitle)
      cli::cli_text("")
    }

    .print_table <- function(tbl) {
      print(tbl, n = top_n, width = Inf)
      cli::cli_text("")
    }

    .print_header(
      "P1: Actionable + Real-world",
      "Balanced segments + profile differentiation + accuracy"
    )
    .print_table(p1)

    .print_header(
      "P2: Maximum Differentiation",
      "Broad spread across all variables + accuracy"
    )
    .print_table(p2)

    .print_header(
      "P3: Threshold Hits",
      glue::glue("Every segment presentable (polar >= {polar_threshold}, profile >= {profile_threshold}) + accuracy")
    )
    .print_table(p3)

    .print_header(
      "P4: Typing Tool Quality",
      "LDA accuracy weighted heavily + balance + hits"
    )
    .print_table(p4)

    .print_header(
      "P5: Polar vs Profile Balance",
      "Penalizes lopsided hit distributions + accuracy"
    )
    .print_table(p5)
  }

  # ---- return ----
  invisible(list(
    p1      = p1,
    p2      = p2,
    p3      = p3,
    p4      = p4,
    p5      = p5,
    metrics = metrics,
    details = results
  ))
}
