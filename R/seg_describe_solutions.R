#' seg_describe_solutions
#'
#' @description Generates AI-written segment descriptions by calling the
#'   Claude API. For each segment in a solution, extracts the top
#'   differentiating variables (polar and profile) from the written solution
#'   Excel file, sends the data to Claude, and returns a tibble with a short
#'   segment name and 3-sentence description per segment.
#'
#' @param seg A seg object with solutions written via [seg_write_solutions()].
#' @param solution Character. The `lda_name` of the solution to describe
#'   (e.g. `"LDA_opt_kmeans_source_eq_H7"`).
#' @param where Character. Path to the solutions directory. Defaults to
#'   `seg[["paths"]][["folders"]][["solution"]]`.
#' @param polar_threshold Numeric. Minimum absolute diff for a polar hit
#'   (default: `0.20`).
#' @param profile_threshold Numeric. Minimum absolute diff for a profile hit
#'   (default: `0.15`).
#' @param n_polar Integer. Number of top polar differentiators to include per
#'   segment in the prompt (default: `15`).
#' @param n_profile Integer. Number of top profile differentiators to include
#'   per segment in the prompt (default: `10`).
#' @param model Character. Claude model to use (default:
#'   `"claude-sonnet-4-20250514"`).
#' @param api_key Character. Anthropic API key. If `NULL` (default), retrieves
#'   via `get_environment_key("ANTHROPIC_API_KEY")`.
#' @param add_to_wb Logical. If `TRUE`, appends a "Descriptions" sheet to the
#'   solution Excel workbook (default: `FALSE`).
#' @param verbose Logical. Print descriptions to console (default: `TRUE`).
#'
#' @return A tibble with columns: `segment`, `name`, `description`, `n`, `pct`,
#'   `polar_hits`, `profile_hits`.
#'
#' @export
seg_describe_solutions <- function(
    seg,
    solution,
    where = NULL,
    polar_threshold = 0.20,
    profile_threshold = 0.15,
    n_polar = 15,
    n_profile = 10,
    model = "claude-sonnet-4-20250514",
    api_key = NULL,
    add_to_wb = FALSE,
    verbose = TRUE
) {

  # ---- resolve API key ----
  if (is.null(api_key)) {
    api_key <- get_environment_key("ANTHROPIC_API_KEY")
  }

  # ---- resolve solution file path ----
  if (is.null(where)) {
    where <- seg[["paths"]][["folders"]][["solution"]]
  }
  if (is.null(where) || is.na(where)) {
    where <- getwd()
  }

  # find the file — look in the expected subfolder first, then fall back to recursive search
  sol_filename <- paste0("Solution - ", solution, ".xlsx")
  sol_file <- file.path(where, solution, sol_filename)

  if (!file.exists(sol_file)) {
    # fall back to recursive search, excluding archive/previous folders
    candidates <- list.files(
      where, pattern = paste0("Solution - ", solution, "\\.xlsx$"),
      recursive = TRUE, full.names = TRUE
    )
    candidates <- candidates[!grepl("~\\$", candidates)]
    candidates <- candidates[!grepl("previous|archive|old|backup", candidates, ignore.case = TRUE)]

    if (length(candidates) == 0) {
      cli::cli_abort("No solution file found for {.val {solution}} in {.path {where}}")
    }
    sol_file <- candidates[1]
  }

  if (verbose) cli::cli_alert_info("Reading {.file {sol_file}}")

  # ---- build battery legend from spec ----
  polar_blocks <- seg[["spec"]][["polars"]] %>%
    dplyr::select(prefix, block_label) %>%
    dplyr::distinct()

  profile_blocks <- seg[["spec"]][["profiles"]]
  if (!is.null(profile_blocks) && "prefix" %in% names(profile_blocks) &&
      "block_label" %in% names(profile_blocks)) {
    profile_blocks <- profile_blocks %>%
      dplyr::select(prefix, block_label) %>%
      dplyr::distinct()
  } else {
    profile_blocks <- tibble::tibble(prefix = character(), block_label = character())
  }

  all_blocks <- dplyr::bind_rows(polar_blocks, profile_blocks) %>%
    dplyr::distinct(prefix, .keep_all = TRUE)

  battery_legend <- paste(
    purrr::map2_chr(all_blocks$prefix, all_blocks$block_label,
                    ~paste0(.x, " = ", .y)),
    collapse = "\n"
  )

  # ---- polar prefixes ----
  polar_prefixes <- seg[["spec"]][["polars"]][["prefix"]]

  # ---- parse solution file ----
  df <- openxlsx::read.xlsx(sol_file, sheet = "summary", colNames = FALSE)
  sheets <- openxlsx::getSheetNames(sol_file)

  header <- df[9, ]
  seg_cols <- which(grepl("^Seg", as.character(header)))
  n_segs <- length(seg_cols)

  seg_ns <- as.numeric(df[10, seg_cols])
  seg_pcts <- as.numeric(df[11, seg_cols])
  total_n <- as.numeric(df[10, 4])

  # ---- build per-segment data blocks ----
  seg_data_blocks <- character(n_segs)
  seg_meta <- vector("list", n_segs)

  for (s in seq_len(n_segs)) {
    sheet_name <- paste("Seg", s)
    if (!sheet_name %in% sheets) next

    seg_df <- openxlsx::read.xlsx(sol_file, sheet = sheet_name, colNames = FALSE)
    var_rows <- which(!is.na(seg_df[[1]]) & grepl("^[A-Z]{2}\\d", seg_df[[1]]))

    if (length(var_rows) == 0) next

    seg_vars   <- as.character(seg_df[var_rows, 1])
    seg_labels <- as.character(seg_df[var_rows, 2])
    seg_target <- as.numeric(seg_df[var_rows, 4])
    seg_others <- as.numeric(seg_df[var_rows, 5])
    seg_diffs  <- as.numeric(seg_df[var_rows, 7])

    prefixes <- gsub("[0-9]+.*$", "", seg_vars)
    seg_type <- ifelse(prefixes %in% polar_prefixes, "polar", "profile")

    # hits
    ph  <- sum(abs(seg_diffs[seg_type == "polar"]) >= polar_threshold, na.rm = TRUE)
    prh <- sum(abs(seg_diffs[seg_type == "profile"]) >= profile_threshold, na.rm = TRUE)

    seg_meta[[s]] <- list(
      n = round(seg_ns[s]), pct = round(seg_pcts[s] * 100, 1),
      polar_hits = ph, profile_hits = prh
    )

    # top polar differentiators
    polar_idx <- which(seg_type == "polar")
    polar_order <- polar_idx[order(abs(seg_diffs[polar_idx]), decreasing = TRUE)]
    polar_top <- utils::head(polar_order, n_polar)

    polar_lines <- purrr::map_chr(polar_top, function(i) {
      dir <- ifelse(seg_diffs[i] > 0, "+", "-")
      sprintf("  %s %s (%.3f vs %.3f, diff=%s%.3f) %s",
              dir, seg_vars[i], seg_target[i], seg_others[i],
              dir, abs(seg_diffs[i]), seg_labels[i])
    })

    # top profile differentiators
    prof_idx <- which(seg_type == "profile")
    prof_order <- prof_idx[order(abs(seg_diffs[prof_idx]), decreasing = TRUE)]
    prof_top <- utils::head(prof_order, n_profile)

    prof_lines <- purrr::map_chr(prof_top, function(i) {
      dir <- ifelse(seg_diffs[i] > 0, "+", "-")
      sprintf("  %s %s (%.3f vs %.3f, diff=%s%.3f) %s",
              dir, seg_vars[i], seg_target[i], seg_others[i],
              dir, abs(seg_diffs[i]), seg_labels[i])
    })

    seg_data_blocks[s] <- paste0(
      sprintf("## Segment %d (n=%d, %.1f%%)\n",
              s, round(seg_ns[s]), seg_pcts[s] * 100),
      "\nTop polar differentiators:\n",
      paste(polar_lines, collapse = "\n"),
      "\n\nTop profile differentiators:\n",
      paste(prof_lines, collapse = "\n")
    )
  }

  # ---- build prompt ----
  system_prompt <- paste0(
    "You are a segmentation research analyst. You will receive data about ",
    "segments from a consumer segmentation study. For each segment, provide:\n",
    "1. A short name (2-4 words, in quotes)\n",
    "2. Exactly 3 sentences describing the segment\n\n",
    "Focus on what makes each segment distinctive relative to the others. ",
    "Translate variable codes into plain language using the battery legend. ",
    "The first sentence should capture the core identity. The second should ",
    "elaborate on attitudes or behaviors. The third should note actionable ",
    "implications or notable contrasts.\n\n",
    "Format your response as:\n",
    "Segment 1: \"Name Here\"\n",
    "Description line 1. Description line 2. Description line 3.\n\n",
    "Segment 2: \"Name Here\"\n",
    "...\n\n",
    "Do not include any other text, headers, or commentary."
  )

  user_prompt <- paste0(
    "Battery legend (prefix = what it measures):\n",
    battery_legend,
    "\n\nSolution: ", solution,
    " (", n_segs, " segments, total N=", total_n, ")\n\n",
    paste(seg_data_blocks, collapse = "\n\n")
  )

  # ---- call Claude API ----
  if (verbose) cli::cli_alert_info("Calling Claude API ({model})...")

  body <- list(
    model = model,
    max_tokens = 2048,
    messages = list(
      list(role = "user", content = user_prompt)
    ),
    system = system_prompt
  )

  resp <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  if (httr::status_code(resp) != 200) {
    err_body <- httr::content(resp, as = "text", encoding = "UTF-8")
    stop(sprintf("Claude API returned status %d: %s",
                 httr::status_code(resp), err_body))
  }

  resp_json <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  response_text <- resp_json[["content"]][[1]][["text"]]

  # ---- parse response into tibble ----
  # Expected format:
  # Segment 1: "Name Here"
  # Description...
  #
  # Segment 2: "Name Here"
  # ...

  seg_pattern <- 'Segment\\s+(\\d+):\\s*"([^"]+)"\\s*\\n([^\\n].*?)(?=\\nSegment\\s+\\d+:|$)'
  matches <- stringr::str_match_all(
    response_text,
    stringr::regex(seg_pattern, dotall = FALSE, multiline = TRUE)
  )[[1]]

  # fallback: split by "Segment N:" if regex is too strict

  if (nrow(matches) == 0) {
    chunks <- stringr::str_split(response_text, "(?=Segment\\s+\\d+:)")[[1]]
    chunks <- chunks[nchar(trimws(chunks)) > 0]

    matches <- purrr::map_dfr(chunks, function(chunk) {
      num <- stringr::str_match(chunk, "Segment\\s+(\\d+):")[, 2]
      name <- stringr::str_match(chunk, '"([^"]+)"')[, 2]
      desc <- stringr::str_replace(chunk, 'Segment\\s+\\d+:\\s*"[^"]*"\\s*', "")
      desc <- trimws(desc)
      if (is.na(num)) return(NULL)
      tibble::tibble(seg_num = as.integer(num), name = name, description = desc)
    })
  } else {
    matches <- tibble::tibble(
      seg_num = as.integer(matches[, 2]),
      name = matches[, 3],
      description = trimws(matches[, 4])
    )
  }

  # build output tibble
  result <- tibble::tibble(
    segment = seq_len(n_segs),
    name = NA_character_,
    description = NA_character_,
    n = purrr::map_int(seg_meta, ~as.integer(.x$n)),
    pct = purrr::map_dbl(seg_meta, ~.x$pct),
    polar_hits = purrr::map_int(seg_meta, ~as.integer(.x$polar_hits)),
    profile_hits = purrr::map_int(seg_meta, ~as.integer(.x$profile_hits))
  )

  for (i in seq_len(nrow(matches))) {
    idx <- matches$seg_num[i]
    if (idx >= 1 && idx <= n_segs) {
      result$name[idx] <- matches$name[i]
      result$description[idx] <- matches$description[i]
    }
  }

  # ---- console output ----
  if (verbose) {
    cli::cli_text("")
    cli::cli_rule(left = solution)
    for (i in seq_len(nrow(result))) {
      cli::cli_h2("Seg {i}: \"{result$name[i]}\" (n={result$n[i]}, {result$pct[i]}%)")
      cli::cli_text(result$description[i])
      cli::cli_text("Hits: polar={result$polar_hits[i]}, profile={result$profile_hits[i]}")
      cli::cli_text("")
    }
  }

  # ---- add to workbook ----
  if (add_to_wb) {
    if (verbose) cli::cli_alert_info("Adding Descriptions sheet to {.file {sol_file}}")

    wb <- openxlsx::loadWorkbook(sol_file)

    # remove existing Descriptions sheet if present
    if ("Descriptions" %in% openxlsx::sheets(wb)) {
      openxlsx::removeWorksheet(wb, "Descriptions")
    }
    openxlsx::addWorksheet(wb, "Descriptions")

    # header style
    header_style <- openxlsx::createStyle(
      textDecoration = "bold", fontSize = 12
    )
    name_style <- openxlsx::createStyle(
      textDecoration = "bold", fontSize = 11
    )
    desc_style <- openxlsx::createStyle(
      wrapText = TRUE, valign = "top"
    )

    # title
    openxlsx::writeData(wb, "Descriptions", x = paste("Solution:", solution),
                        startRow = 1, startCol = 1)
    openxlsx::addStyle(wb, "Descriptions", style = header_style,
                       rows = 1, cols = 1)

    # write each segment
    row <- 3
    for (i in seq_len(nrow(result))) {
      seg_header <- sprintf("Segment %d: \"%s\" (n=%d, %.1f%%)",
                            i, result$name[i], result$n[i], result$pct[i])

      openxlsx::writeData(wb, "Descriptions", x = seg_header,
                          startRow = row, startCol = 1)
      openxlsx::addStyle(wb, "Descriptions", style = name_style,
                         rows = row, cols = 1)

      openxlsx::writeData(wb, "Descriptions", x = result$description[i],
                          startRow = row + 1, startCol = 1)
      openxlsx::addStyle(wb, "Descriptions", style = desc_style,
                         rows = row + 1, cols = 1)

      row <- row + 3
    }

    # set column width
    openxlsx::setColWidths(wb, "Descriptions", cols = 1, widths = 120)

    tryCatch(
      openxlsx::saveWorkbook(wb, sol_file, overwrite = TRUE),
      error = function(e) stop(sprintf("Failed to save workbook to %s: %s", sol_file, e$message), call. = FALSE)
    )
    if (verbose) cli::cli_alert_success("Saved to {.path {sol_file}}")
  }

  invisible(result)
}
