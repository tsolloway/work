#' bn_insights
#'
#' @description Produces a client-facing narrative analysis of a Bayesian
#'   network's results (impacts + prioritization) by extracting the key
#'   findings from the R objects and sending a structured digest to Anthropic's
#'   Claude API for plain-language interpretation. Designed to consume the
#'   return value of `bn_write()` directly.
#'
#' @param x One of:
#'   * The return value of `bn_write()` — a `bn_write_result` carrying `$obj`,
#'     `$title`, `$sub_title`, `$dv_display`. Recommended.
#'   * A `bn_finalize_network()` output directly. Title/subtitle/DV fall back
#'     to NULL and whatever is in `$impacts$meta`.
#'   * A list of either of the above, one per result. In that case each is
#'     analyzed independently and the narratives are concatenated.
#' @param verbose Logical. When TRUE (default) the narrative is printed to the
#'   console via `cat()`. When FALSE the function is silent; the return value
#'   still carries the narrative and the digest.
#' @param api_key Character or NULL. Anthropic API key. If NULL (default), it
#'   is loaded via `get_environment_key("ANTHROPIC_API_KEY")` — reads from the
#'   environment variable first, and falls back to an interactive
#'   `rstudioapi::askForPassword()` prompt if unset.
#' @param model Character. Anthropic model id. Default
#'   `"claude-sonnet-4-5-20250929"`.
#' @param max_tokens Integer. Upper bound on the generated response length.
#'   Default 2000.
#' @param top_n Integer. How many top attribute drivers / top communities /
#'   prioritization steps to include in the digest sent to the API. Default 10.
#' @param audience One of `"client"` (default — plain language, no methods
#'   jargon) or `"internal"` (uses metric names directly). Controls the prompt
#'   framing sent to the API.
#' @param format Character. Output format. One of `"email"` (default — a
#'   ready-to-send email with subject line, greeting, body paragraphs, and
#'   sign-off) or `"memo"` (a longer client-ready memo with sectioned
#'   analysis). The underlying digest and analytical rigor are identical;
#'   only the wrapping changes.
#' @param sign_off Optional character. Name to use on the email sign-off.
#'   Ignored when `format = "memo"`. Default `NULL` leaves a `[Your name]`
#'   placeholder in the email.
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
#' @param dry_run Logical. When TRUE, the function builds the digest and
#'   returns it WITHOUT calling the Anthropic API. Useful for inspecting what
#'   the LLM would see (and verifying top-N ordering) without spending
#'   tokens. Default FALSE.
#'
#' @return A list (invisibly) with elements:
#'   * `narrative` — the analyst-generated text (character scalar).
#'   * `digest` — the structured summary that was sent to the API (useful for
#'     auditing / regression testing).
#'   * `model`, `usage` — the Anthropic response metadata.
#'   * `file` — resolved path of the written file, or NULL when
#'     `file = NULL`.
#'
#' @details
#'   The API key is only read when the function is actually called, never at
#'   package load time. The call uses `httr2::request()` with a default retry
#'   policy on transient 5xx / rate-limit errors.
#'
#' @export
bn_insights <- function(
    x,
    verbose = TRUE,
    api_key = NULL,
    model = "claude-sonnet-4-5-20250929",
    max_tokens = 2000L,
    top_n = 10L,
    audience = c("client", "internal"),
    format = c("email", "memo"),
    sign_off = NULL,
    file = TRUE,
    open_file = TRUE,
    dry_run = FALSE
) {

  audience <- match.arg(audience)
  format   <- match.arg(format)

  # `open_file = TRUE` implies a file must exist on disk to open. If the
  # caller asked to open but didn't specify a destination, default to
  # `file = TRUE` (timestamped path in getwd()).
  if (isTRUE(open_file) && is.null(file)) {
    file <- TRUE
  }

  if (!isTRUE(dry_run) && !requireNamespace("httr2", quietly = TRUE)) {
    cli::cli_abort("{.pkg httr2} is required for {.fn bn_insights}.")
  }

  # If `x` is a list of bn_write_result / bn_final objects, recurse.
  if (.is_bn_insights_batch(x)) {
    res <- purrr::imap(x, function(xi, nm) {
      if (isTRUE(verbose)) {
        cat("\n\n============================================================\n")
        cat("Result: ", nm %||% "(unnamed)", "\n", sep = "")
        cat("============================================================\n\n")
      }
      bn_insights(xi, verbose = verbose, api_key = api_key,
        model = model, max_tokens = max_tokens, top_n = top_n,
        audience = audience, format = format, sign_off = sign_off,
        file = file, open_file = open_file, dry_run = dry_run)
    })
    return(invisible(res))
  }

  # Resolve the input down to an (obj, title, sub_title, dv_display) tuple.
  resolved <- .bn_insights_resolve_input(x)

  # Build the compact structured digest sent to the API.
  digest <- .bn_insights_build_digest(
    obj        = resolved$obj,
    title      = resolved$title,
    sub_title  = resolved$sub_title,
    dv_display = resolved$dv_display,
    top_n      = top_n
  )

  # Short-circuit when the caller just wants to audit the digest.
  if (isTRUE(dry_run)) {
    if (isTRUE(verbose)) {
      cat("--- dry_run: digest only, no API call ---\n\n")
      cat(jsonlite::toJSON(digest, auto_unbox = TRUE, pretty = TRUE,
        null = "null", na = "null"))
      cat("\n")
    }
    return(invisible(list(digest = digest, narrative = NULL,
                          model = NULL, usage = NULL, file = NULL)))
  }

  # Call the API.
  response <- .bn_insights_call_claude(
    digest     = digest,
    api_key    = api_key,
    model      = model,
    max_tokens = max_tokens,
    audience   = audience,
    format     = format,
    sign_off   = sign_off
  )

  narrative <- response$narrative

  if (isTRUE(verbose)) cat(narrative, "\n", sep = "")

  # Optionally write to disk + open.
  out_path <- NULL
  if (!is.null(file)) {
    out_path <- .bn_insights_write_file(
      narrative = narrative,
      file      = file,
      format    = format
    )
    open_ok <- if (isTRUE(open_file)) {
      .bn_insights_open_path(out_path)
    } else NA
    if (isTRUE(verbose)) {
      cat("\n")
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
    narrative = narrative,
    digest    = digest,
    model     = response$model,
    usage     = response$usage,
    file      = out_path
  ))
}


# -- input resolution ------------------------------------------------------

# Is x a container of multiple bn_write / bn_finalize results, as opposed to
# a single one? Heuristic: an unclassed list whose first element looks like a
# bn_write_result or a bn_finalize output.
.is_bn_insights_batch <- function(x) {
  if (inherits(x, "bn_write_result")) return(FALSE)
  if (!is.list(x)) return(FALSE)
  if (length(x) == 0) return(FALSE)
  # Single bn_finalize output (has $bn or $impacts at top level) -> not batch.
  if (!is.null(x[["bn"]]) || !is.null(x[["impacts"]])) return(FALSE)
  # Single bn_write_result masquerading as list — handled above.
  first <- x[[1L]]
  inherits(first, "bn_write_result") ||
    (is.list(first) && (!is.null(first[["obj"]]) ||
                        !is.null(first[["bn"]]) ||
                        !is.null(first[["impacts"]])))
}

# Coerce x into a list(obj, title, sub_title, dv_display). `obj` is the
# bn_finalize_network output carrying $impacts / $prioritizations / $bn.
.bn_insights_resolve_input <- function(x) {

  if (inherits(x, "bn_write_result")) {
    return(list(
      obj        = x[["obj"]],
      title      = x[["title"]],
      sub_title  = x[["sub_title"]],
      dv_display = x[["dv_display"]]
    ))
  }

  if (is.list(x) && !is.null(x[["obj"]])) {
    # Plain list that happens to carry $obj — treat like a bn_write_result.
    return(list(
      obj        = x[["obj"]],
      title      = x[["title"]],
      sub_title  = x[["sub_title"]],
      dv_display = x[["dv_display"]]
    ))
  }

  # A bare file path can't be turned into a digest — bn_insights needs the
  # structured R objects, not the serialized .xlsx. Guide the user to the
  # right call shape before we try to subscript a string as if it were a list.
  if (is.character(x)) {
    cli::cli_abort(c(
      "{.fn bn_insights} received a character value (looks like a file path).",
      "i" = "This input type is likely from an older {.fn bn_write} call that \\
             returned a path. Re-run {.code bn_final %>% bn_write(...)} after \\
             reloading the package, then pass the new return value in.",
      "i" = "Alternately, pass the {.fn bn_finalize_network} output directly: \\
             {.code bn_insights(bn_final)}."
    ))
  }

  if (!is.list(x)) {
    cli::cli_abort(c(
      "{.fn bn_insights} expects a {.cls bn_write_result}, a \\
       {.fn bn_finalize_network} output, or a list of either.",
      "x" = "Got an object of class {.cls {class(x)}}."
    ))
  }

  # Otherwise assume x is the bn_finalize_network output itself. Verify it
  # at least has the shape we expect before trying to dig into it.
  if (is.null(x[["impacts"]]) && is.null(x[["prioritizations"]]) &&
      is.null(x[["bn"]])) {
    cli::cli_abort(c(
      "{.fn bn_insights} could not recognize the input as a \\
       {.fn bn_finalize_network} output.",
      "i" = "Expected a list with at least one of {.field impacts}, \\
             {.field prioritizations}, or {.field bn}; got names: \\
             {.val {names(x)}}."
    ))
  }

  # Extract the DV display from its impacts/prioritizations meta if we can.
  meta <- x[["impacts"]][["meta"]] %||% x[["prioritizations"]][["meta"]]
  dv <- meta[["dv"]]
  dv_display <- if (!is.null(dv) && !is.null(names(dv))) names(dv) else dv

  list(
    obj        = x,
    title      = NULL,
    sub_title  = NULL,
    dv_display = dv_display
  )
}


# -- digest building -------------------------------------------------------

# Build a compact, JSON-friendly summary of the key results for the API.
.bn_insights_build_digest <- function(obj, title, sub_title, dv_display, top_n) {

  impacts <- obj[["impacts"]]
  priort  <- obj[["prioritizations"]]

  impact_meta <- impacts[["meta"]] %||% list()
  priort_meta <- priort[["meta"]]  %||% list()

  dv <- dv_display %||% impact_meta[["dv"]] %||% priort_meta[["dv"]]
  if (!is.null(dv) && !is.null(names(dv))) dv <- names(dv)

  list(
    study = list(
      title     = title,
      sub_title = sub_title,
      dv        = dv,
      subgroups = impact_meta[["subgroups"]] %||% priort_meta[["subgroups"]],
      brand     = impact_meta[["brand"]],
      n_brands  = length(impact_meta[["brand_names"]] %||% character(0))
    ),
    attribute_drivers = .digest_impact_table(
      impacts[["table_attribute"]],
      impacts[["table_attribute_weighted"]],
      top_n,
      grain = "attribute"
    ),
    community_drivers = .digest_impact_table(
      impacts[["table_community"]],
      impacts[["table_community_weighted"]],
      top_n,
      grain = "community"
    ),
    prioritization = .digest_priort(priort, top_n),
    thresholds = list(
      min_base_for_lift    = impact_meta[["min_base_for_lift"]],
      sig_threshold        = priort_meta[["sig_threshold"]],
      marginal_threshold   = priort_meta[["marginal_threshold"]],
      lift                 = impact_meta[["lift"]] %||% priort_meta[["lift"]],
      boot_applied         = isTRUE(impact_meta[["boot_applied"]]),
      mi_boot_applied      = isTRUE(impact_meta[["mi_boot_applied"]])
    ),
    weighting = list(
      weight_column        = impact_meta[["weight"]],
      weighted_available   = !is.null(impacts[["table_attribute_weighted"]])
    )
  )
}

# Convert an impact table (attribute or community) into a compact top-N list.
# Impact tables from bn_impact are WIDE: a row per variable/community with
# columns `Variable` / `Community`, optional `Label`, and per-subgroup metric
# columns like `Total_lift`, `Total_maxVmin`, `Total_mi`, `Total_p_val`,
# `Total_index`, then `Frequent_Google_User_lift` etc. for each subgroup.
# We pick the Total subgroup when present, prefer `lift` as the sort metric,
# and emit a compact list of rows for the API digest.
.digest_impact_table <- function(tbl, tbl_weighted, top_n, grain) {
  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)

  cols <- names(tbl)
  id_col <- if ("Variable" %in% cols) "Variable"
  else if ("Community" %in% cols) "Community"
  else if ("variable" %in% cols) "variable"
  else if ("community" %in% cols) "community"
  else cols[1]

  # Detect subgroups by looking at what comes before `_lift` / `_lift_N` /
  # `_maxVmin` / `_mi`. Most numeric impact columns follow the pattern
  # `{subgroup}_{metric}`.
  # bn_impact_engine produces lift columns named `lift_0`, `lift_10`, etc.
  # (one per element of the `lift` argument, defaulting to c(0, 0.1)), plus
  # bare `lift` when a single value was passed. Bootstrap runs add `_mean`,
  # `_sd`, `_se`, `_t`, `_ci_low`, `_ci_high`, `_p_value` to each.
  all_lift_cols <- grep("^[^_]+(_[^_]+)*_lift(_\\d+)?(_mean)?$", cols, value = TRUE)
  all_lift_cols <- all_lift_cols[!grepl("_sd$|_se$|_t$|_ci_low$|_ci_high$|_p_value$",
                                        all_lift_cols)]
  # Subgroup detection pattern: fallback to conventional metric suffixes too.
  metric_suffixes <- c("lift", "lift_0", "maxVmin", "mi", "p_val", "index",
                       "dv_max_value")
  detected_sgs <- unique(unlist(lapply(metric_suffixes, function(m) {
    hits <- grep(paste0("_", m, "$"), cols, value = TRUE)
    sub(paste0("_", m, "$"), "", hits)
  })))
  # Also infer subgroups from `{sg}_lift_N` / `{sg}_lift_N_mean`.
  for (lc in all_lift_cols) {
    sg_guess <- sub("_lift(_\\d+)?(_mean)?$", "", lc)
    if (nzchar(sg_guess)) detected_sgs <- c(detected_sgs, sg_guess)
  }
  detected_sgs <- unique(detected_sgs)

  # Prefer "Total" when present — that's the overall ranking.
  sort_sg <- if ("Total" %in% detected_sgs) "Total" else detected_sgs[1]
  if (is.na(sort_sg) || length(sort_sg) == 0) return(NULL)

  # Pick a sort metric. The Excel dashboard's default "Average Lift" is
  # backed by `{sg}_lift_0` (the 0% baseline-shift column) or `{sg}_lift`
  # in single-shift mode — NOT `{sg}_lift_mean` (which is bootstrap-average
  # and differs slightly from the point estimate). Match the dashboard.
  # Preference order:
  #   1. `{sg}_lift_0` (matches dashboard "Average Lift" with multi-shift)
  #   2. `{sg}_lift`   (matches dashboard "Average Lift" with single shift)
  #   3. Any other `{sg}_lift_N` (other % shifts, e.g. "10% Lift")
  #   4. maxVmin, then mi.
  other_lift_cands <- grep(paste0("^", sort_sg, "_lift_\\d+$"),
    all_lift_cols, value = TRUE)
  other_lift_cands <- setdiff(other_lift_cands, paste0(sort_sg, "_lift_0"))
  candidates <- c(
    paste0(sort_sg, "_lift_0"),
    paste0(sort_sg, "_lift"),
    other_lift_cands,
    paste0(sort_sg, "_maxVmin"),
    paste0(sort_sg, "_mi")
  )

  sort_col <- NULL
  for (c_ in candidates) {
    if (c_ %in% cols && is.numeric(tbl[[c_]])) { sort_col <- c_; break }
  }
  if (is.null(sort_col)) return(NULL)

  # Derive a short metric label: strip the subgroup prefix.
  sort_metric <- sub(paste0("^", sort_sg, "_"), "", sort_col)

  # p-value: prefer bootstrap-derived p_value for the chosen metric; else
  # fall back to MI chi-squared p_val.
  pval_boot <- paste0(sort_col, "_p_value")
  p_col <- if (pval_boot %in% cols) pval_boot
           else if (paste0(sort_sg, "_p_val") %in% cols) paste0(sort_sg, "_p_val")
           else NULL
  # Index column: bn_impact strips `_index` suffix, so the index column is
  # just the bare subgroup name (e.g. "Total" holds Total_index's values).
  index_col <- if (sort_sg %in% cols) sort_sg else NULL

  # Sort by |metric| descending; drop NA rows; pick top_n.
  vals <- tbl[[sort_col]]
  keep <- !is.na(vals)
  tbl  <- tbl[keep, , drop = FALSE]
  vals <- vals[keep]
  ord  <- order(abs(vals), decreasing = TRUE)
  tbl  <- tbl[ord, , drop = FALSE]
  tbl  <- tbl[seq_len(min(top_n, nrow(tbl))), , drop = FALSE]

  rows <- purrr::map(seq_len(nrow(tbl)), function(i) {
    out <- list(
      rank       = i,          # explicit 1-based rank so the LLM can anchor
                               # on a numeric field instead of list position.
      id         = as.character(tbl[[id_col]][i]),
      label      = if ("Label" %in% cols) as.character(tbl[["Label"]][i])
                   else if ("label" %in% cols) as.character(tbl[["label"]][i])
                   else NULL,
      community  = if ("Community" %in% cols && id_col != "Community")
                     as.character(tbl[["Community"]][i]) else NULL,
      metric     = sort_metric,
      subgroup   = sort_sg,
      value      = unname(tbl[[sort_col]][i]),
      direction  = if (!is.na(tbl[[sort_col]][i])) {
                     if (tbl[[sort_col]][i] >= 0) "positive" else "negative"
                   } else NA_character_,
      p_value    = if (!is.null(p_col)) unname(tbl[[p_col]][i]) else NULL,
      index      = if (!is.null(index_col)) unname(tbl[[index_col]][i]) else NULL
    )
    out[!vapply(out, is.null, logical(1))]
  })

  sort_metric_label <- if (sort_metric %in% c("lift", "lift_0")) {
    "Average Lift (0% baseline shift)"
  } else if (grepl("^lift_\\d+$", sort_metric)) {
    paste0(sub("lift_", "", sort_metric), "% Lift")
  } else if (sort_metric == "maxVmin") {
    "Max vs Min"
  } else if (sort_metric == "mi") {
    "Mutual Information"
  } else {
    sort_metric
  }

  list(
    grain              = grain,
    sort_metric        = sort_metric,
    sort_metric_label  = sort_metric_label,
    sort_subgroup      = sort_sg,
    n_subgroups        = length(detected_sgs),
    subgroups          = detected_sgs,
    rows               = rows
  )
}

# Convert a bn_prioritizations result into a compact top-N digest.
# Structure of the input: a list with at least $greedy_lift and $greedy_max,
# each of which is a named list keyed by subgroup -> tibble. Tibbles have
# columns: priority, variable, label, combo, dv_estimate, marginal_gain,
# marginal_gain_pct, and optionally p_value / ci columns when n_boot_final
# was set.
.digest_priort <- function(priort, top_n) {
  if (is.null(priort)) return(NULL)

  # Prefer the "lift" strategy (distributional-shift prioritization); fall
  # back to "max" (max-value prioritization) if lift isn't there.
  strategies <- list(
    greedy_lift = priort[["greedy_lift"]],
    greedy_max  = priort[["greedy_max"]]
  )
  strategy_name <- NULL
  strategy_list <- NULL
  for (s in names(strategies)) {
    if (!is.null(strategies[[s]]) && length(strategies[[s]]) > 0) {
      strategy_name <- s
      strategy_list <- strategies[[s]]
      break
    }
  }
  if (is.null(strategy_list)) return(NULL)

  # Pick the Total subgroup when present; otherwise first entry.
  sg_names <- names(strategy_list)
  primary_sg <- if ("Total" %in% sg_names) "Total" else sg_names[1]
  tbl <- strategy_list[[primary_sg]]
  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)

  if ("priority" %in% names(tbl)) {
    tbl <- tbl[order(tbl[["priority"]]), , drop = FALSE]
  }
  tbl <- tbl[seq_len(min(top_n, nrow(tbl))), , drop = FALSE]

  rows <- purrr::map(seq_len(nrow(tbl)), function(i) {
    out <- list(
      rank              = i,   # explicit 1-based rank for LLM anchoring
      priority          = if ("priority" %in% names(tbl)) unname(tbl[["priority"]][i]) else i,
      variable          = if ("variable" %in% names(tbl)) as.character(tbl[["variable"]][i]) else NULL,
      label             = if ("label" %in% names(tbl)) as.character(tbl[["label"]][i]) else NULL,
      community         = if ("community" %in% names(tbl)) as.character(tbl[["community"]][i]) else NULL,
      combo             = if ("combo" %in% names(tbl)) as.character(tbl[["combo"]][i]) else NULL,
      dv_estimate       = if ("dv_estimate" %in% names(tbl)) unname(tbl[["dv_estimate"]][i]) else NULL,
      marginal_gain     = if ("marginal_gain" %in% names(tbl)) unname(tbl[["marginal_gain"]][i]) else NULL,
      marginal_gain_pct = if ("marginal_gain_pct" %in% names(tbl)) unname(tbl[["marginal_gain_pct"]][i]) else NULL,
      p_value           = if ("p_value" %in% names(tbl)) unname(tbl[["p_value"]][i]) else NULL
    )
    out[!vapply(out, is.null, logical(1))]
  })

  # Precompute cumulative-lift summary figures so the LLM doesn't have to
  # derive them arithmetically (which it sometimes gets wrong — e.g. confusing
  # marginal_gain with marginal_gain_pct, or inflating percentage points).
  baseline_dv <- NULL
  if ("dv_estimate" %in% names(tbl) && nrow(tbl) >= 1) {
    # Baseline is either an explicit row where variable == "Baseline" (typical
    # bn_prioritize output) or the first row minus its own marginal_gain.
    bl_row <- which(tolower(as.character(tbl[["variable"]])) == "baseline")
    baseline_dv <- if (length(bl_row) >= 1) {
      tbl[["dv_estimate"]][bl_row[1]]
    } else if ("marginal_gain" %in% names(tbl)) {
      tbl[["dv_estimate"]][1] - (tbl[["marginal_gain"]][1] %||% 0)
    } else {
      NA_real_
    }
  }

  # Cumulative DV at cut points 1, 3, 5, and the end (post-baseline).
  make_cut <- function(k) {
    if (!("dv_estimate" %in% names(tbl))) return(NULL)
    # Find the k-th post-baseline row (skip the Baseline row if present).
    post_bl <- if ("variable" %in% names(tbl)) {
      which(tolower(as.character(tbl[["variable"]])) != "baseline")
    } else seq_len(nrow(tbl))
    if (length(post_bl) < k) return(NULL)
    idx <- post_bl[k]
    dv_at <- tbl[["dv_estimate"]][idx]
    list(
      step         = k,
      variable     = as.character(tbl[["variable"]][idx]),
      dv_estimate  = dv_at,
      cum_gain_abs = if (!is.null(baseline_dv) && !is.na(baseline_dv))
                       dv_at - baseline_dv else NA_real_,
      cum_gain_pp  = if (!is.null(baseline_dv) && !is.na(baseline_dv))
                       (dv_at - baseline_dv) * 100 else NA_real_,
      cum_gain_pct = if (!is.null(baseline_dv) && !is.na(baseline_dv) && baseline_dv != 0)
                       (dv_at - baseline_dv) / baseline_dv else NA_real_
    )
  }
  n_post_baseline <- sum(tolower(as.character(tbl[["variable"]])) != "baseline",
                         na.rm = TRUE)
  cuts <- purrr::compact(list(
    top_1   = make_cut(1),
    top_3   = make_cut(3),
    top_5   = make_cut(5),
    all     = make_cut(n_post_baseline)
  ))

  list(
    strategy              = strategy_name,
    subgroup              = primary_sg,
    subgroups_available   = sg_names,
    baseline_dv           = baseline_dv,
    cumulative_milestones = cuts,
    rows                  = rows
  )
}


# -- API call --------------------------------------------------------------

# Build the prompts and POST to Anthropic's Messages API. Returns a list with
# $narrative, $model, $usage. Any HTTP error is surfaced as a cli_abort.
.bn_insights_call_claude <- function(digest, api_key, model, max_tokens, audience,
                                     format = "memo", sign_off = NULL) {

  # Matches the key-loading convention used by bn_name_groups() and
  # seg_describe_solutions(): env var first, falls back to an interactive
  # rstudioapi::askForPassword() prompt if absent.
  if (is.null(api_key)) api_key <- get_environment_key("ANTHROPIC_API_KEY")

  system_prompt <- .bn_insights_system_prompt(audience)
  user_prompt <- .bn_insights_user_prompt(digest, audience, format = format,
                                          sign_off = sign_off)

  body <- list(
    model = model,
    max_tokens = max_tokens,
    system = system_prompt,
    messages = list(
      list(role = "user", content = user_prompt)
    )
  )

  req <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key"          = api_key,
      "anthropic-version"  = "2023-06-01",
      "content-type"       = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_retry(max_tries = 3, is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    }) |>
    httr2::req_timeout(120)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort(c(
        "Anthropic API request failed.",
        "x" = "{conditionMessage(e)}"
      ))
    }
  )

  parsed <- httr2::resp_body_json(resp)

  # Messages API returns content as a list of blocks; text blocks have
  # $type = "text" with a $text field.
  text_blocks <- purrr::keep(parsed$content, function(b) {
    identical(b$type, "text")
  })
  narrative <- paste(purrr::map_chr(text_blocks, "text"), collapse = "\n\n")

  list(
    narrative = narrative,
    model     = parsed$model %||% model,
    usage     = parsed$usage
  )
}

.bn_insights_system_prompt <- function(audience) {
  base <- paste(
    "You are a senior market-research analyst writing for a client audience.",
    "You receive a structured digest of a Bayesian-network driver-analysis",
    "study and produce a concise, actionable narrative about what it means",
    "for the business.",
    sep = " "
  )
  if (audience == "client") {
    paste(base,
      "",
      "Style rules:",
      "- Plain business language. Avoid statistical jargon: no mention of",
      "  p-values, lift, MaxVmin, mutual information, Bayesian networks,",
      "  conditional probabilities, bootstraps, or thresholds.",
      "- Speak in terms of drivers, priorities, and where to focus.",
      "- Short paragraphs and bulleted takeaways. No tables. No code.",
      "- Concrete recommendations tied to the driver rankings, not generic",
      "  advice.",
      "- If subgroups are present, call out where their drivers meaningfully",
      "  differ.",
      "- Close with one sentence noting any caveats.",
      "",
      "Ranking fidelity:",
      "- Every row in `attribute_drivers.rows` and `prioritization.rows`",
      "  carries an explicit integer `rank` field (1, 2, 3, ...). The rank",
      "  is authoritative. Iterate in ascending rank order. Do not re-rank",
      "  based on narrative fit, semantic similarity, or your priors.",
      "- When you paraphrase a driver, preserve its rank. The rank-1 driver",
      "  is the #1 driver in the output; the rank-2 driver is #2; and so on.",
      "- If rank-1 feels like a duplicate of rank-2 (e.g. 'ease of use' and",
      "  'effortless'), list them both separately in rank order — don't",
      "  merge them.",
      "",
      "Numeric fidelity — NEVER invent statistics:",
      "- Every number you cite (percentage points, lifts, percentages,",
      "  sample sizes) must come directly from a field in the digest.",
      "- Do NOT compute aggregates the digest doesn't already contain.",
      "  Specifically: do not sum `marginal_gain` values, do not multiply",
      "  `marginal_gain_pct` by 100 and call it percentage points, do not",
      "  compare values across rows arithmetically.",
      "- The digest already includes `prioritization.cumulative_milestones`",
      "  — a precomputed set of cut points (top_1, top_3, top_5, all) with",
      "  `cum_gain_pp` (percentage points over baseline), `cum_gain_pct`",
      "  (relative gain), and `dv_estimate` (absolute). Use these when you",
      "  want to cite a 'top N drivers together lift by X' figure. These",
      "  are the only cumulative-gain numbers that are safe to quote.",
      "- If an observation requires a number that isn't in the digest,",
      "  describe qualitatively ('steep early gains then flattens',",
      "  'diffuse with many small levers') rather than making one up.",
      sep = "\n"
    )
  } else {
    paste(base,
      "",
      "Style rules:",
      "- Internal-team audience; use the domain terminology directly.",
      "- Reference metric names (lift, p-value, MaxVmin, MI) when it adds",
      "  clarity.",
      "- Keep it actionable and concrete.",
      sep = "\n"
    )
  }
}

.bn_insights_user_prompt <- function(digest, audience, format = "memo",
                                     sign_off = NULL) {
  if (identical(format, "email")) {
    .bn_insights_user_prompt_email(digest, audience, sign_off = sign_off)
  } else {
    .bn_insights_user_prompt_memo(digest, audience)
  }
}

.bn_insights_user_prompt_memo <- function(digest, audience) {

  digest_json <- jsonlite::toJSON(digest, auto_unbox = TRUE, null = "null",
    na = "null", pretty = TRUE)

  sections <- paste(
    "# Task",
    "",
    if (audience == "client") {
      paste(
        "Read the study digest below and produce a client-ready narrative",
        "with these sections, in this order:",
        "",
        "1. **Headline** - one or two sentences naming the single most",
        "   important thing the client should take away. This MUST be the",
        "   #1 ranked driver from `attribute_drivers.rows[0]` in the digest,",
        "   not your own pick of 'most important'.",
        "2. **Why this matters** - 3-4 bullet points that SYNTHESIZE the",
        "   results into strategic implications — not restate them. Each",
        "   bullet should answer 'so what?' for the business. Look at the",
        "   top-ranked drivers TOGETHER and find the pattern: do they",
        "   cluster around a theme (e.g., ease-of-use, trust, speed)? Does",
        "   the prioritization sequence tell a story (one dominant lever,",
        "   or many small ones)? Do subgroups diverge in a way that",
        "   suggests segment-level strategy? Frame each bullet as a",
        "   business implication tied to competitive positioning, user",
        "   behavior, growth opportunities, or risks — not as a driver",
        "   rank restatement. Avoid phrases like 'the top driver is X',",
        "   'ranked #1', or 'scored highest' in this section.",
        "3. **Top drivers** - the top items from `attribute_drivers.rows`",
        "   in the SAME ORDER as the digest (the digest is pre-sorted by",
        "   the chosen metric, descending). Do NOT re-rank, do NOT skip",
        "   items, do NOT promote items you find more interesting. Use 5-7",
        "   items. For each, reference the `label` field in plain language,",
        "   and briefly say what it means for the business.",
        "4. **Where to focus first** - 3-5 numbered, concrete actions tied",
        "   to the `prioritization.rows` sequence in the digest, preserving",
        "   that order. Frame each as a recommendation.",
        "5. **Subgroup notes** - if the study has multiple subgroups, note",
        "   where their drivers diverge meaningfully. Skip this section if",
        "   only a Total / single subgroup is present.",
        "6. **Caveats** - one sentence. Flag any data-quality concerns (low",
        "   base, weakly significant findings, brands that were skipped).",
        sep = "\n"
      )
    } else {
      paste(
        "Read the study digest below and produce an internal analyst brief",
        "with: headline, strategic synthesis (3-4 bullets answering 'so",
        "what' for the business — cluster the top drivers thematically,",
        "identify whether prioritization is concentrated or diffuse, flag",
        "subgroup divergence patterns; DO NOT simply restate rank order),",
        "top drivers (with metric values, preserve digest order), recommended",
        "prioritization sequence, subgroup divergence, caveats.",
        sep = "\n"
      )
    },
    "",
    "# Study digest",
    "",
    "```json",
    digest_json,
    "```",
    "",
    "Respond in Markdown.",
    sep = "\n"
  )

  sections
}


# -- email variant ---------------------------------------------------------

.bn_insights_user_prompt_email <- function(digest, audience, sign_off = NULL) {

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
    "Read the study digest below and write a ready-to-send email to the",
    "client summarizing what the driver analysis found. Same analytical rigor",
    "as a memo — same ranking fidelity, same numeric fidelity, same jargon",
    "rules — but this is an EMAIL. Short paragraphs, no section headings, no",
    "bullet lists unless absolutely necessary.",
    "",
    "Required structure, top to bottom:",
    "",
    "1. **Subject line** — On the very first line, write `Subject: <line>`",
    "   where `<line>` names the study in plain language and hints at the",
    "   headline finding (e.g. `Subject: What drives Brand Consideration in",
    "   Japan — top-line findings`). Keep it under ~80 characters.",
    "2. **Blank line**, then a greeting. Default to `Hi team,` unless the",
    "   study context names a specific stakeholder.",
    "3. **Opening paragraph (2 to 3 sentences)** — name the study's target",
    "   outcome (from `study.dv`), and give the single most important",
    "   takeaway. This must be anchored on `attribute_drivers.rows[0]` — the",
    "   #1 ranked driver — with its `label` in bold. Do not re-rank.",
    "4. **Findings paragraph (1 short paragraph, ~4 to 6 sentences)** —",
    "   synthesize the top drivers into 2 or 3 strategic themes. Look at the",
    "   top 5-7 rows in `attribute_drivers.rows` in digest order and find the",
    "   pattern (ease-of-use, trust, quality, price, etc.). Reference at",
    "   least one concrete number from the digest — either an",
    "   `attribute_drivers.rows[i].value` or a",
    "   `prioritization.cumulative_milestones` figure — expressed as a",
    "   percentage where appropriate. Do NOT invent numbers or aggregate",
    "   raw fields; the cumulative_milestones are the only pre-computed",
    "   cumulative figures safe to quote.",
    "5. **Where-to-focus paragraph (1 short paragraph, ~3 to 5 sentences)** —",
    "   translate `prioritization.rows` (in that order) into 2 or 3 concrete",
    "   next steps. Prose form, not a numbered list.",
    "6. **Subgroup note (1 sentence, optional)** — only if `study.subgroups`",
    "   has more than a Total / single subgroup AND their drivers diverge",
    "   meaningfully. Otherwise skip.",
    "7. **Caveat + next-step sentence (1 sentence)** — flag one limitation and",
    "   offer to walk through the details.",
    "8. **Sign-off** —", sign_off_line,
    "",
    "Ranking fidelity: every `rank` field in the digest is authoritative.",
    "Iterate in ascending rank order. Preserve rank when paraphrasing.",
    "",
    "Numeric fidelity: every number cited must come directly from a digest",
    "field. Convert decimals to percentages when quoting. Use",
    "`prioritization.cumulative_milestones.cum_gain_pp` /",
    "`cum_gain_pct` for cumulative-gain figures — do not sum",
    "`marginal_gain` values yourself.",
    "",
    if (audience == "client") {
      paste(
        "Jargon rules (client audience): no p-values, lift, MaxVmin, mutual",
        "information, Bayesian networks, conditional probabilities,",
        "bootstraps, or thresholds. Speak in terms of drivers, priorities,",
        "and where to focus.",
        sep = "\n"
      )
    } else {
      paste(
        "Audience is internal — metric names (lift, MI, MaxVmin) are OK when",
        "they add clarity, but this is still an email, so use them sparingly.",
        sep = "\n"
      )
    },
    "",
    "Tone: warm, professional, confident. Like a senior analyst writing to a",
    "client they have a working relationship with. Not stiff, not chatty.",
    "",
    "# Study digest",
    "",
    "```json",
    digest_json,
    "```",
    "",
    "Respond with ONLY the email — Subject line first, then a blank line,",
    "then greeting through sign-off. Do not wrap it in code fences. Do not",
    "add commentary before or after. Markdown bold (`**...**`) is acceptable",
    "for the #1 driver label in the opening paragraph; otherwise plain text.",
    sep = "\n"
  )
}


# -- file output -----------------------------------------------------------

# Write the narrative to disk. For email, write a minimal RFC 5322 .eml so
# the file double-clicks open in Mail / Outlook. For memo, write Markdown.
# Returns the resolved absolute path.
.bn_insights_write_file <- function(narrative, file, format) {

  ext <- if (identical(format, "email")) ".eml" else ".md"

  # Resolve `file = TRUE` to a timestamped path in the current working
  # directory (easier to find than tempdir()).
  if (isTRUE(file)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file <- file.path(
      getwd(),
      paste0("bn_insights_", format, "_", ts, ext)
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
    .bn_insights_format_eml(narrative)
  } else {
    narrative
  }

  writeLines(content, con = file, useBytes = TRUE)
  normalizePath(file, mustWork = TRUE, winslash = "/")
}

# Convert the email body Claude produced (Subject: line + blank + body) into a
# minimal RFC 5322 envelope so the file opens cleanly in Mail clients.
.bn_insights_format_eml <- function(narrative) {
  lines <- strsplit(narrative, "\n", fixed = TRUE)[[1]]

  # First non-blank line should be `Subject: ...`. If Claude drifted, fall
  # back to a generic subject and treat the whole narrative as body.
  first_idx <- which(nzchar(trimws(lines)))[1]
  subject <- "Driver-analysis findings"
  body_lines <- lines

  if (!is.na(first_idx) && grepl("^Subject:\\s*", lines[first_idx])) {
    subject <- trimws(sub("^Subject:\\s*", "", lines[first_idx]))
    body_lines <- lines[-seq_len(first_idx)]
    while (length(body_lines) > 0 && !nzchar(trimws(body_lines[1]))) {
      body_lines <- body_lines[-1]
    }
  }
  body <- paste(body_lines, collapse = "\n")

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
# 0), FALSE otherwise. Never aborts.
.bn_insights_open_path <- function(path) {
  os <- Sys.info()[["sysname"]]
  status <- tryCatch({
    if (identical(os, "Darwin")) {
      system2("open", shQuote(path), stdout = FALSE, stderr = FALSE)
    } else if (identical(os, "Windows")) {
      tryCatch({ shell.exec(path); 0L }, error = function(e) 1L)
    } else {
      system2("xdg-open", shQuote(path), stdout = FALSE, stderr = FALSE)
    }
  }, error = function(e) 1L, warning = function(w) 1L)
  isTRUE(identical(as.integer(status), 0L))
}
