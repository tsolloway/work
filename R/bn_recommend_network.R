#' bn_recommend_network
#'
#' @description
#' Reads a set of candidate Bayesian Network engine results, builds a structured
#' comparison digest, and asks Anthropic's Claude API to produce a plain-language
#' recommendation about which network the client should adopt and why.
#'
#' Designed to consume the return value of [bn_initial_networks()] or any named
#' list of engine results (the same shape `bn_name_groups()` accepts). Names
#' on the list entries become the candidate labels shown to the client, so use
#' clear, presentation-ready names.
#'
#' @param networks Named list of engine results. Each entry must be an engine
#'   result with at least `$bn`, `$meta`, `$summary`, `$viz_prep`.
#' @param df Optional data frame (imputed, ready for scoring). When supplied,
#'   the function collects every distinct DV declared in `meta$dv` across the
#'   supplied networks and re-scores each unsupervised candidate against
#'   every one of those DVs (every attribute is forced to point at the DV,
#'   then the structure is refit and scored). Each unsupervised candidate's
#'   `performance` block then carries one entry per available DV, putting it
#'   on the same axis as each supervised candidate for the overall
#'   recommendation. Skipped silently when NULL.
#' @param context Optional character. Extra business context for the analyst
#'   (e.g. `"Client is Google; goal is to identify what drives Chrome
#'   consideration in Japan."`). When provided, the prompt incorporates it so
#'   the recommendation is grounded in the client's actual question.
#' @param format Character. Output format. One of `"email"` (default — a
#'   ready-to-send email with subject line, greeting, body, and sign-off) or
#'   `"memo"` (a full client-ready memo with summary + numbered sections).
#'   The reasoning is the same in either case; only the wrapping changes.
#' @param sign_off Optional character. Name to use on the email sign-off.
#'   Ignored when `format = "memo"`. Default `NULL` leaves a `[Your name]`
#'   placeholder.
#' @param file Character path, `TRUE` (default), or `NULL`. When non-NULL,
#'   the narrative is written to disk. With `format = "email"` it writes a
#'   `.eml` file (RFC 5322 minimal envelope; double-click to open in Mail
#'   / Outlook ready to edit + send); with `format = "memo"` it writes a
#'   `.md` file. Pass an explicit path to control the location; pass `TRUE`
#'   (the default) to drop a timestamped file in `getwd()`; pass `NULL` to
#'   skip the file step entirely. The extension is auto-corrected to
#'   `.eml` / `.md` if missing. The resolved path is returned in the
#'   result list as `$file`.
#' @param open_file Logical. When `TRUE` (default), open the saved file in
#'   the OS's default handler (Mail.app on macOS for `.eml`, Outlook on
#'   Windows, etc.). Implies `file = TRUE` when `file` is `NULL` — you
#'   can't open what wasn't written.
#' @param verbose Logical. When TRUE (default), prints the narrative to the
#'   console. The return value still carries the narrative regardless.
#' @param api_key Character or NULL. Anthropic API key. If NULL (default),
#'   loaded via `get_environment_key("ANTHROPIC_API_KEY")`.
#' @param model Character. Anthropic model id. Default `"claude-opus-4-5"`.
#' @param max_tokens Integer. Upper bound on the generated response length.
#'   Default 2000.
#' @param dry_run Logical. When TRUE, builds the digest and returns it WITHOUT
#'   calling the Anthropic API. Useful for inspecting exactly what would be
#'   sent to the model. Default FALSE.
#'
#' @return A list (invisibly) with elements:
#'   * `narrative` — the client-ready memo (character scalar).
#'   * `digest` — the structured comparison sent to the API.
#'   * `model`, `usage` — Anthropic response metadata.
#'
#' @examples
#' \dontrun{
#' initial_networks <- list(
#'   "Brand Most Considered (TB)"   = bn$cb_direct$brand_consideration_tb,
#'   "Brand Considered (Full Scale)" = bn$cb_direct$brand_consideration,
#'   "Unsupervised Network"          = bn$cb_unsupervised
#' ) %>% work::bn_name_groups(verbose = FALSE)
#'
#' # Defaults: format = "email", file = TRUE, open_file = TRUE
#' # — writes a timestamped .eml in getwd() and opens it for editing.
#' initial_networks %>% bn_recommend_network(
#'   df       = df_impute,
#'   context  = "Client is Google. Goal: identify what drives Chrome consideration in Japan.",
#'   sign_off = "Tyler"
#' )
#'
#' # Memo flavour, save to a specific Markdown path, don't auto-open:
#' initial_networks %>% bn_recommend_network(
#'   df        = df_impute,
#'   format    = "memo",
#'   file      = "~/Desktop/jp_chrome_recommendation.md",
#'   open_file = FALSE
#' )
#' }
#'
#' @export
bn_recommend_network <- function(
    networks,
    df = NULL,
    context = NULL,
    format = c("email", "memo"),
    sign_off = NULL,
    file = TRUE,
    open_file = TRUE,
    verbose = TRUE,
    api_key = NULL,
    model = "claude-opus-4-5",
    max_tokens = 2000L,
    dry_run = FALSE
) {

  format <- match.arg(format)

  # `open_file = TRUE` implies a file must exist on disk to open. If the
  # caller asked to open but didn't specify a destination, default to
  # `file = TRUE` (timestamped path in getwd()).
  if (isTRUE(open_file) && is.null(file)) {
    file <- TRUE
  }

  # --- input checks ----
  if (!is.list(networks) || length(networks) == 0) {
    cli::cli_abort("{.arg networks} must be a non-empty named list of engine results.")
  }
  if (is.null(names(networks)) || any(!nzchar(names(networks)))) {
    cli::cli_abort(c(
      "Every entry in {.arg networks} must have a non-empty name.",
      "i" = "Names are shown to the client verbatim — make them presentation-ready."
    ))
  }
  if (length(networks) < 2) {
    cli::cli_warn("Only one candidate supplied — there is nothing to compare.")
  }
  if (!isTRUE(dry_run) && !requireNamespace("httr2", quietly = TRUE)) {
    cli::cli_abort("{.pkg httr2} is required for {.fn bn_recommend_network}.")
  }

  # --- build per-candidate digests ----
  candidates <- purrr::imap(networks, .bn_recommend_network_digest)

  # --- collect every DV present in any supplied network ----
  # Returns a named character vector: names = display labels, values = variable
  # names. Empty if `df` is NULL or no supervised network was supplied.
  available_dvs <- if (!is.null(df)) {
    .bn_recommend_network_collect_dvs(networks)
  } else character(0)

  has_unsupervised <- any(vapply(
    candidates,
    function(c) identical(c$network_type, "unsupervised"),
    logical(1)
  ))
  if (length(available_dvs) > 0 && has_unsupervised) {
    n_dv <- length(available_dvs)
    cli::cli_alert_info(
      "Scoring unsupervised candidates against {n_dv} outcome{?s}: \\
       {.field {unname(available_dvs)}}."
    )
  }

  # --- inject implied hit-rate per available DV for unsupervised candidates ----
  if (length(available_dvs) > 0) {
    candidates <- purrr::imap(candidates, function(cand, nm) {
      if (!identical(cand$network_type, "unsupervised")) return(cand)
      net <- networks[[nm]]
      perf <- purrr::map(seq_along(available_dvs), function(i) {
        .bn_recommend_network_implied_perf(
          unsupervised_network = net,
          df                   = df,
          dv_var               = unname(available_dvs)[i],
          dv_label             = names(available_dvs)[i]
        )
      })
      perf <- purrr::compact(perf)
      cand$performance <- if (length(perf) > 0) perf else NULL
      cand
    })
  }

  digest <- list(
    n_candidates    = length(candidates),
    client_context  = context,
    available_dvs   = if (length(available_dvs) > 0) {
                        purrr::map2(names(available_dvs), unname(available_dvs),
                                    ~list(label = .x, variable = .y))
                      } else NULL,
    candidates      = candidates
  )

  # --- dry_run short-circuit ----
  if (isTRUE(dry_run)) {
    if (isTRUE(verbose)) {
      cat("--- dry_run: digest only, no API call ---\n\n")
      cat(jsonlite::toJSON(digest, auto_unbox = TRUE, pretty = TRUE,
                           null = "null", na = "null"))
      cat("\n")
    }
    return(invisible(list(
      digest = digest, narrative = NULL, model = NULL, usage = NULL
    )))
  }

  # --- API call ----
  if (is.null(api_key)) api_key <- get_environment_key("ANTHROPIC_API_KEY")

  response <- .bn_recommend_network_call_claude(
    digest     = digest,
    api_key    = api_key,
    model      = model,
    max_tokens = max_tokens,
    format     = format,
    sign_off   = sign_off
  )

  if (isTRUE(verbose)) cat(response$narrative, "\n", sep = "")

  # --- optionally write to disk + open ----
  out_path <- NULL
  if (!is.null(file)) {
    out_path <- .bn_recommend_network_write_file(
      narrative = response$narrative,
      file      = file,
      format    = format
    )
    open_ok <- if (isTRUE(open_file)) {
      .bn_recommend_network_open_path(out_path)
    } else NA
    if (isTRUE(verbose)) {
      cat("\n")  # visual break after the narrative
      cli::cli_rule("File saved")
      cli::cli_alert_success("{.field {format}} written to:")
      cli::cli_text("  {.path {out_path}}")
      if (isTRUE(open_file)) {
        if (isTRUE(open_ok)) {
          cli::cli_alert_success("Opened in default handler.")
        } else {
          cli::cli_alert_warning(
            "Could not open automatically — open it manually with: \\
             {.code system(\"open '{out_path}'\")}"
          )
        }
      }
      cli::cli_rule()
    }
  }

  invisible(list(
    narrative = response$narrative,
    digest    = digest,
    model     = response$model,
    usage     = response$usage,
    file      = out_path
  ))
}


# ---- per-candidate digest ------------------------------------------------

#' @noRd
.bn_recommend_network_digest <- function(network, name) {

  if (!is.list(network) || is.null(network[["bn"]])) {
    return(list(
      name  = name,
      error = "Entry is not a recognizable engine result (missing $bn)."
    ))
  }

  meta     <- network[["meta"]]    %||% list()
  summary  <- network[["summary"]] %||% list()
  bn_obj   <- network[["bn"]]
  viz      <- network[["viz_prep"]]

  # DV / target outcome
  dv <- meta[["dv"]]
  is_supervised <- !is.null(dv) && length(dv) > 0 && nzchar(as.character(dv)[1])
  dv_label <- if (is_supervised) {
    if (!is.null(names(dv)) && nzchar(names(dv)[1])) names(dv)[1] else as.character(dv)[1]
  } else NULL

  # arcs & node counts
  arcs <- tryCatch(
    dplyr::as_tibble(bn_obj$arcs),
    error = function(e) NULL
  )
  n_arcs <- if (!is.null(arcs)) nrow(arcs) else NA_integer_

  n_total_variables <- tryCatch(
    length(bnlearn::nodes(bn_obj)),
    error = function(e) NA_integer_
  )

  # how many attributes directly connect to the target?
  direct_drivers <- NA_integer_
  if (is_supervised && !is.null(arcs) && nrow(arcs) > 0) {
    dv_var <- as.character(dv)[1]
    direct_drivers <- sum(arcs$from == dv_var | arcs$to == dv_var)
  }

  # group / community structure
  groups <- NULL
  n_groups <- NA_integer_
  if (!is.null(viz) && !is.null(viz$attribute_viz_prep$nodes)) {
    nodes_df <- viz$attribute_viz_prep$nodes
    have_cols <- all(c("group", "label") %in% names(nodes_df))
    if (have_cols) {
      grp <- nodes_df %>%
        dplyr::filter(!is.na(.data$group)) %>%
        dplyr::group_by(.data$group) %>%
        dplyr::summarise(
          name = dplyr::first(if ("community_name" %in% names(nodes_df))
                              .data$community_name else NA_character_),
          n_attributes = dplyr::n(),
          attribute_labels = paste(.data$label, collapse = "; "),
          .groups = "drop"
        ) %>%
        dplyr::arrange(.data$group)

      n_groups <- nrow(grp)
      groups <- purrr::pmap(grp, function(group, name, n_attributes, attribute_labels) {
        list(
          group_number     = as.integer(group),
          name             = name %||% "(unnamed)",
          n_attributes     = as.integer(n_attributes),
          attribute_labels = attribute_labels
        )
      })
    }
  }

  # predictive performance — supervised only (one entry against own DV).
  # Wrapped in a list so the shape matches unsupervised candidates, which
  # carry one entry per available DV when implied scoring is enabled.
  performance <- NULL
  if (is_supervised && is.data.frame(summary[["model"]])) {
    m <- summary[["model"]]
    pick <- function(col) {
      if (col %in% names(m) && length(m[[col]]) > 0) m[[col]][1] else NA_real_
    }
    hit_rate          <- pick("accuracy")
    hit_rate_baseline <- pick("accuracy_naive")
    lift_pct          <- pick("accuracy_improve_perc")
    if (is.na(lift_pct) && !is.na(hit_rate) && !is.na(hit_rate_baseline) &&
        hit_rate_baseline > 0) {
      lift_pct <- (hit_rate - hit_rate_baseline) / hit_rate_baseline
    }
    performance <- list(list(
      target_outcome            = dv_label,
      reference_dv              = as.character(dv)[1],
      hit_rate                  = hit_rate,
      hit_rate_baseline_guess   = hit_rate_baseline,
      improvement_over_baseline = lift_pct,
      implied                   = FALSE
    ))
  }

  list(
    name              = name,
    network_type      = if (is_supervised) "supervised" else "unsupervised",
    target_outcome    = dv_label,
    n_total_variables = n_total_variables,
    n_connections     = n_arcs,
    n_groups          = n_groups,
    direct_drivers    = direct_drivers,
    performance       = performance,
    groups            = groups
  )
}


# ---- implied performance for unsupervised candidates ---------------------

# Collect every DV declared in `meta$dv` across the supplied networks.
# Returns a named character vector: names = display labels (from
# `names(meta$dv)` when populated, else the variable name), values =
# variable names. De-duplicated by variable name, preserving first-seen
# order. Empty character vector when no DV is found.
#' @noRd
.bn_recommend_network_collect_dvs <- function(networks) {
  vars   <- character(0)
  labels <- character(0)
  for (n in networks) {
    if (!is.list(n)) next
    dv <- n[["meta"]][["dv"]]
    if (is.null(dv) || length(dv) == 0) next
    for (i in seq_along(dv)) {
      v <- as.character(dv)[i]
      if (!nzchar(v)) next
      l <- if (!is.null(names(dv)) && nzchar(names(dv)[i])) names(dv)[i] else v
      vars   <- c(vars, v)
      labels <- c(labels, l)
    }
  }
  if (length(vars) == 0) return(character(0))
  keep <- !duplicated(vars)
  stats::setNames(vars[keep], labels[keep])
}

# Score an unsupervised network AS IF every attribute were a driver of the
# given DV. Delegates to bn_finalize_network() with do_impacts /
# do_prioritizations switched off so we only pay for the model-fitting +
# subgroup-summary step. Returns a list of performance fields with
# $implied = TRUE, or NULL if anything fails (warns; does not abort).
#' @noRd
.bn_recommend_network_implied_perf <- function(unsupervised_network, df, dv_var,
                                                dv_label = NULL) {
  if (is.null(unsupervised_network) || is.null(df) || is.null(dv_var)) return(NULL)
  if (!dv_var %in% names(df)) {
    cli::cli_warn(
      "Reference DV {.field {dv_var}} not found in {.arg df}; skipping implied scoring for this outcome."
    )
    return(NULL)
  }

  finalized <- tryCatch(
    suppressMessages(work::bn_finalize_network(
      obj                = unsupervised_network,
      df                 = df,
      dv                 = dv_var,
      subgroups          = NULL,
      do_impacts         = FALSE,
      do_prioritizations = FALSE,
      model_parallel     = FALSE,
      impact_parallel    = FALSE
    )),
    error = function(e) {
      cli::cli_warn(c(
        "Could not score implied supervised network against {.field {dv_var}}.",
        "x" = conditionMessage(e)
      ))
      NULL
    }
  )
  if (is.null(finalized)) return(NULL)

  smry <- finalized[["bn_subgroups_summary"]]
  if (!is.data.frame(smry) || nrow(smry) == 0) return(NULL)

  # Single-subgroup ("Total") run — take the first row.
  row <- if ("subgroup" %in% names(smry) && "Total" %in% smry[["subgroup"]]) {
    smry[smry[["subgroup"]] == "Total", , drop = FALSE][1, ]
  } else {
    smry[1, ]
  }
  pick <- function(col) {
    if (col %in% names(row) && length(row[[col]]) > 0) row[[col]][1] else NA_real_
  }
  hit_rate          <- pick("accuracy")
  hit_rate_baseline <- pick("accuracy_naive")
  lift_pct          <- pick("accuracy_improve_perc")
  if (is.na(lift_pct) && !is.na(hit_rate) && !is.na(hit_rate_baseline) &&
      hit_rate_baseline > 0) {
    lift_pct <- (hit_rate - hit_rate_baseline) / hit_rate_baseline
  }

  list(
    target_outcome            = dv_label %||% dv_var,
    reference_dv              = dv_var,
    hit_rate                  = hit_rate,
    hit_rate_baseline_guess   = hit_rate_baseline,
    improvement_over_baseline = lift_pct,
    implied                   = TRUE
  )
}


# ---- file output ---------------------------------------------------------

# Write the narrative to disk. For email, write a minimal RFC 5322 .eml so
# the file double-clicks open in Mail / Outlook. For memo, write Markdown.
# Returns the resolved absolute path.
#' @noRd
.bn_recommend_network_write_file <- function(narrative, file, format) {

  ext <- if (identical(format, "email")) ".eml" else ".md"

  # Resolve `file = TRUE` to a timestamped path in the current working
  # directory (much easier to find than tempdir()).
  if (isTRUE(file)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file <- file.path(
      getwd(),
      paste0("bn_recommendation_", format, "_", ts, ext)
    )
  }
  if (!is.character(file) || length(file) != 1L || !nzchar(file)) {
    cli::cli_abort(c(
      "{.arg file} must be a single non-empty path, {.code TRUE}, or {.code NULL}.",
      "x" = "Got: {.val {file}}"
    ))
  }

  # Auto-correct extension if missing or wrong.
  if (!grepl(paste0("\\", ext, "$"), file, ignore.case = TRUE)) {
    file <- paste0(tools::file_path_sans_ext(file), ext)
  }

  # Make sure the parent dir exists.
  parent <- dirname(file)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }

  content <- if (identical(format, "email")) {
    .bn_recommend_network_format_eml(narrative)
  } else {
    narrative
  }

  writeLines(content, con = file, useBytes = TRUE)
  normalizePath(file, mustWork = TRUE, winslash = "/")
}

# Convert the email body Claude produced (Subject: line + blank + body) into a
# minimal RFC 5322 envelope so the file opens cleanly in Mail clients.
#' @noRd
.bn_recommend_network_format_eml <- function(narrative) {
  lines <- strsplit(narrative, "\n", fixed = TRUE)[[1]]

  # First non-blank line should be `Subject: ...`. If Claude drifted, fall
  # back to a generic subject and treat the whole narrative as body.
  first_idx <- which(nzchar(trimws(lines)))[1]
  subject <- "Network recommendation"
  body_lines <- lines

  if (!is.na(first_idx) && grepl("^Subject:\\s*", lines[first_idx])) {
    subject <- trimws(sub("^Subject:\\s*", "", lines[first_idx]))
    body_lines <- lines[-seq_len(first_idx)]
    # Drop the blank line(s) that follow the Subject header.
    while (length(body_lines) > 0 && !nzchar(trimws(body_lines[1]))) {
      body_lines <- body_lines[-1]
    }
  }
  body <- paste(body_lines, collapse = "\n")

  # Minimal headers — no From/To so the mail client prompts for them on open.
  paste0(
    "Subject: ", subject, "\n",
    "MIME-Version: 1.0\n",
    "Content-Type: text/plain; charset=UTF-8\n",
    "Content-Transfer-Encoding: 8bit\n",
    "\n",
    body
  )
}

# Open a file in the OS default handler. Returns TRUE on success (exit code
# 0), FALSE otherwise. `system2()` returns an integer status code rather
# than throwing on failure, so we need to inspect the code itself — a
# tryCatch around it will report success even when the launch failed.
# Never aborts.
#' @noRd
.bn_recommend_network_open_path <- function(path) {
  os <- Sys.info()[["sysname"]]
  status <- tryCatch({
    if (identical(os, "Darwin")) {
      system2("open", shQuote(path), stdout = FALSE, stderr = FALSE)
    } else if (identical(os, "Windows")) {
      # shell.exec() returns invisible NULL on success and errors on failure.
      tryCatch({ shell.exec(path); 0L }, error = function(e) 1L)
    } else {
      system2("xdg-open", shQuote(path), stdout = FALSE, stderr = FALSE)
    }
  }, error = function(e) 1L, warning = function(w) 1L)
  isTRUE(identical(as.integer(status), 0L))
}


# ---- API call ------------------------------------------------------------

#' @noRd
.bn_recommend_network_call_claude <- function(digest, api_key, model, max_tokens,
                                               format = "memo", sign_off = NULL) {

  body <- list(
    model      = model,
    max_tokens = max_tokens,
    system     = .bn_recommend_network_system_prompt(),
    messages   = list(list(
      role    = "user",
      content = .bn_recommend_network_user_prompt(digest, format = format,
                                                   sign_off = sign_off)
    ))
  )

  req <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_retry(max_tries = 3, is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    }) |>
    httr2::req_timeout(120)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) cli::cli_abort(c(
      "Anthropic API request failed.",
      "x" = "{conditionMessage(e)}"
    ))
  )

  parsed <- httr2::resp_body_json(resp)
  text_blocks <- purrr::keep(parsed$content,
                             function(b) identical(b$type, "text"))
  narrative <- paste(purrr::map_chr(text_blocks, "text"), collapse = "\n\n")

  list(
    narrative = narrative,
    model     = parsed$model %||% model,
    usage     = parsed$usage
  )
}


#' @noRd
.bn_recommend_network_system_prompt <- function() {
  paste(
    "You are a senior market-research analyst writing for a non-technical",
    "client audience. You receive a structured digest comparing several",
    "candidate driver-analysis models built from the same survey, and you",
    "recommend ONE for the client to adopt.",
    "",
    "How to read the digest:",
    "- Each candidate is a different way of organizing the same survey data",
    "  into a driver story.",
    "- `target_outcome` is the survey question the model is built to explain.",
    "  Candidates with different `target_outcome` values are answering",
    "  different business questions — the recommendation must address WHICH",
    "  QUESTION to answer, not just which math is tighter.",
    "- `performance` is ALWAYS an array of entries — one entry per outcome",
    "  the candidate was scored against. Each entry carries `target_outcome`",
    "  (display label), `hit_rate`, `hit_rate_baseline_guess`,",
    "  `improvement_over_baseline` (all decimals — convert to percentages",
    "  when quoting), and `implied` (true/false).",
    "- `network_type = 'supervised'` means the model is built around a",
    "  specific outcome. Its `performance` array has exactly ONE entry,",
    "  scored against its own target outcome (`implied = false`).",
    "- `network_type = 'unsupervised'` means the model has no target of its",
    "  own — it just describes how attributes naturally cluster together.",
    "  Its `performance` array has ONE ENTRY PER AVAILABLE OUTCOME (every",
    "  entry has `implied = true`): every attribute was forced to point at",
    "  that outcome, and the whole thing was re-scored. The top-level",
    "  `available_dvs` field lists every outcome the unsupervised candidate",
    "  was scored against. When you quote an implied hit rate, make it",
    "  unambiguous that this is what would happen IF the client adopted the",
    "  exploratory structure as a driver framework for that outcome (use",
    "  phrasing like 'if you used this lens to predict X, you'd be right",
    "  Y% of the time') — it is not a property the model has on its own.",
    "- Compare apples-to-apples: when judging the unsupervised candidate",
    "  against a supervised candidate, line up the unsupervised entry whose",
    "  `target_outcome` matches the supervised candidate's `target_outcome`.",
    "  Do not compare across different target outcomes.",
    "- `direct_drivers` is how many attributes are directly connected to the",
    "  outcome. A small number tells a cleaner story; a large number is more",
    "  comprehensive but harder to act on.",
    "- `groups` shows the thematic clusters the model found, with names and",
    "  member attributes. Fewer, cleaner groups are easier for a client to",
    "  internalize than many small ones.",
    "",
    "Two-pass evaluation:",
    "- PASS 1 (groupings only) — Ignore `target_outcome`, `direct_drivers`,",
    "  and `performance` entirely. Look ONLY at `groups`. Ask: do the",
    "  attributes inside each theme genuinely belong together? Are the themes",
    "  meaningfully distinct from each other? Are there too many tiny themes,",
    "  or too few overstuffed ones? Is the theme name a fair label for its",
    "  members? Pick the candidate whose themes a client would find most",
    "  intuitive and easy to talk about.",
    "- PASS 2 (overall fit) — Now bring everything back in. For the",
    "  unsupervised candidate, evaluate it AS IF every attribute were a",
    "  driver of the SAME outcome the supervised candidates target (the most",
    "  prominent target_outcome among the supervised candidates). In other",
    "  words: treat the unsupervised theme structure as a candidate driver",
    "  framework on equal footing with the supervised ones — judge whether,",
    "  used that way, it would tell a sharper or fuzzier story than the",
    "  supervised candidates' direct-driver structures.",
    "",
    "Style rules:",
    "- Plain business language. ABSOLUTELY NO statistical jargon. Do not",
    "  use any of these words: Bayesian, network, accuracy, lift, p-value,",
    "  baseline, naive, BIC, AIC, log-likelihood, community detection,",
    "  bootstrap, significance, confidence interval, posterior, prior,",
    "  conditional probability, model fit, supervised, unsupervised.",
    "- Translate everything:",
    "    'hit_rate'                  -> 'how often the model picks the right",
    "                                    brand'",
    "    'improvement_over_baseline' -> 'how much better than just guessing",
    "                                    the most popular answer every time'",
    "    'direct_drivers'            -> 'the levers tied directly to the",
    "                                    outcome'",
    "    'groups'                    -> 'themes' or 'storylines'",
    "    'supervised'                -> 'aimed at a specific question'",
    "    'unsupervised'              -> 'exploratory / no target'",
    "- Convert decimals to percentages when quoting hit rates (0.62 -> 62%).",
    "- Be opinionated. Pick ONE recommended candidate by its exact `name`.",
    "  Do not hedge with 'it depends' or recommend two.",
    "- If a number isn't in the digest, don't make one up. Describe",
    "  qualitatively instead.",
    sep = "\n"
  )
}


#' @noRd
.bn_recommend_network_user_prompt <- function(digest, format = "memo",
                                               sign_off = NULL) {
  if (identical(format, "email")) {
    .bn_recommend_network_user_prompt_email(digest, sign_off = sign_off)
  } else {
    .bn_recommend_network_user_prompt_memo(digest)
  }
}


#' @noRd
.bn_recommend_network_user_prompt_memo <- function(digest) {

  digest_json <- jsonlite::toJSON(digest, auto_unbox = TRUE, null = "null",
                                  na = "null", pretty = TRUE)

  paste(
    "# Task",
    "",
    "Read the comparison digest below and write a client-ready memo with",
    "exactly these sections, in this order:",
    "",
    "0. **Summary** — Open the memo with a 3 to 4 sentence executive",
    "   summary, BEFORE any section heading. State (a) which candidate has",
    "   the cleanest groupings (Section 1 winner, in bold), (b) which",
    "   candidate the client should adopt overall (Section 2 winner, in",
    "   bold), and (c) a one-line reason. If the two picks differ, the",
    "   summary must say so explicitly and signal that the trade-off is",
    "   explained in Section 2. Keep it punchy — this is what a busy",
    "   stakeholder reads before deciding whether to read the rest.",
    "",
    "1. **Cleanest groupings** — Apply PASS 1 from the system instructions",
    "   (groupings only — ignore the target outcome, the direct levers, and",
    "   the hit-rate figures). Open with one sentence naming the candidate",
    "   whose themes are the most logically consistent, with its `name` in",
    "   bold. Follow with 2 to 3 bullets that justify the pick by",
    "   referencing actual theme names and members from the digest — call",
    "   out where another candidate's themes feel jumbled, overlap awkwardly,",
    "   or split a single idea across multiple themes.",
    "2. **Overall recommendation** — Apply PASS 2. Open with one sentence",
    "   naming the candidate the client should adopt going forward, with",
    "   its `name` in bold.",
    "   ",
    "   IF the Section 2 winner is the SAME as the Section 1 winner: follow",
    "   with 3 to 5 justifying bullets. At least one bullet must address",
    "   WHAT QUESTION this candidate answers (its target outcome). At least",
    "   one bullet must reference something concrete from the digest — a",
    "   hit rate, an improvement figure, a theme name, or the number of",
    "   direct levers.",
    "   ",
    "   IF the Section 2 winner DIFFERS from the Section 1 winner: you owe",
    "   the client a real explanation of the trade-off, not a passing",
    "   reference. Add a sub-section in this exact format:",
    "     **Why we're overriding the cleanest-groupings pick**",
    "       - Compare the two candidates' hit rates SIDE BY SIDE, against",
    "         the SAME target outcome (the Section 2 winner's target). For",
    "         the unsupervised candidate, use its implied entry whose",
    "         `target_outcome` matches. Quote both as percentages (e.g.,",
    "         '62% vs 48%') AND say in plain words how much better that",
    "         is at picking the right brand. NEVER compare hit rates",
    "         computed against different target outcomes.",
    "       - State what the Section 1 winner is *better* at (its themes",
    "         are cleaner — name one or two examples) so the client sees",
    "         what they're giving up.",
    "       - State what the Section 2 winner is *better* at (concrete:",
    "         the hit-rate gap, the question it answers, the actionability",
    "         of its direct drivers) so the client sees what they're",
    "         gaining.",
    "       - End with one sentence explaining why the gain outweighs the",
    "         loss for THIS client's question. Tie it back to the client",
    "         context if one was provided.",
    "   Then continue with 2 to 3 additional justifying bullets in the",
    "   same shape as the matched-winner case.",
    "3. **What the alternatives offer** — one short paragraph per",
    "   non-recommended candidate (relative to Section 2), naming it in",
    "   bold. Say what that candidate is good at and why you didn't pick",
    "   it. Be specific — vague contrasts are not acceptable.",
    "4. **Caveat** — one sentence flagging any limitation the client should",
    "   know about (e.g. small base for the strictest outcome, themes that",
    "   overlap, exploratory-only nature).",
    "",
    if (!is.null(digest$client_context)) {
      paste("# Client context",
            "",
            digest$client_context,
            "",
            sep = "\n")
    } else "",
    "# Comparison digest",
    "",
    "```json",
    digest_json,
    "```",
    "",
    "Respond in Markdown.",
    sep = "\n"
  )
}


#' @noRd
.bn_recommend_network_user_prompt_email <- function(digest, sign_off = NULL) {

  digest_json <- jsonlite::toJSON(digest, auto_unbox = TRUE, null = "null",
                                  na = "null", pretty = TRUE)

  sign_off_line <- if (!is.null(sign_off) && nzchar(sign_off)) {
    paste0("Sign off with `Best,\\n", sign_off, "` exactly.")
  } else {
    "Sign off with `Best,\\n[Your name]` (leave the placeholder for the user to fill in)."
  }

  paste(
    "# Task",
    "",
    "Read the comparison digest below and write a ready-to-send email to",
    "the client recommending which candidate network to adopt. The reasoning",
    "rules from the system prompt still apply (no jargon, percentage-",
    "formatted hit rates, the two-pass evaluation, apples-to-apples",
    "comparisons, opinionated single recommendation). The shape changes —",
    "this is an email, not a memo.",
    "",
    "Required structure, top to bottom:",
    "",
    "1. **Subject line** — On the very first line, write `Subject: <line>`",
    "   where `<line>` names the recommended candidate in plain language",
    "   (e.g. `Subject: Recommendation — adopt the Brand Most Considered (TB)",
    "   driver framework`). Keep it under ~80 characters.",
    "2. **Blank line**, then a greeting. Default to `Hi team,` unless the",
    "   client context names a specific stakeholder, in which case use their",
    "   first name (e.g. `Hi Priya,`).",
    "3. **Opening paragraph** (3 to 4 sentences) — the same executive summary",
    "   the memo would lead with: name the cleanest-groupings winner in",
    "   bold, name the overall recommendation in bold, give a one-line",
    "   reason. If the two picks differ, say so explicitly and flag that",
    "   the trade-off is explained below.",
    "4. **Body paragraph(s)** — 1 or 2 short paragraphs (NOT bullets — this",
    "   is an email body, not a memo) covering the justification for the",
    "   overall recommendation. Reference at least one concrete number",
    "   from the digest (a hit rate as a percentage, an improvement gap,",
    "   a theme name, or the number of direct levers). If the Section 1",
    "   and Section 2 winners DIFFER, one of these paragraphs must be the",
    "   trade-off explanation: quote both candidates' hit rates SIDE BY",
    "   SIDE against the SAME target outcome (for the unsupervised",
    "   candidate, use its implied entry whose `target_outcome` matches),",
    "   say plainly what's gained and what's given up, and tie it back to",
    "   the client's question. NEVER compare hit rates across different",
    "   target outcomes.",
    "5. **Alternatives paragraph** (1 short paragraph) — briefly acknowledge",
    "   what each non-recommended candidate offers and why it didn't win.",
    "   Keep it tight; this is an email, not an audit.",
    "6. **Caveat / next-steps sentence** (1 sentence) — flag one limitation",
    "   the client should know about, then offer to discuss further.",
    "7. **Sign-off** —", sign_off_line,
    "",
    "Constraints:",
    "- Tone: warm, professional, confident. Like a senior analyst writing",
    "  to a client they have a working relationship with. Not stiff,",
    "  not chatty.",
    "- No section headings inside the email body. No numbered or bulleted",
    "  lists unless absolutely necessary — paragraphs read more naturally",
    "  in email.",
    "- Convert all decimals to percentages when quoting hit rates.",
    "- No statistical jargon (rules from the system prompt apply).",
    "",
    if (!is.null(digest$client_context)) {
      paste("# Client context",
            "",
            digest$client_context,
            "",
            sep = "\n")
    } else "",
    "# Comparison digest",
    "",
    "```json",
    digest_json,
    "```",
    "",
    "Respond with ONLY the email — Subject line first, then a blank line,",
    "then greeting through sign-off. Do not wrap it in code fences. Do not",
    "add commentary before or after. Markdown bold (`**...**`) is",
    "acceptable for the two candidate names in the opening paragraph;",
    "otherwise plain text.",
    sep = "\n"
  )
}
