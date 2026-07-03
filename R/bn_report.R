#' bn_report
#'
#' @description
#' Generates a self-contained HTML report containing multiple Bayesian Network
#' visualizations across layout types (none, gravity, charge) and views
#' (attribute-level, community-level). Accepts one or more engine results
#' from `bn_engine()`, `bn_engine_unsupervised()`, or `bn_initial_networks()`.
#'
#' Results are shown in collapsible accordion sections. Within each section,
#' a dropdown selects the layout type and tabs switch between attribute and
#' community views.
#'
#' @param results A named list of engine results
#'   (e.g., `list(cb = bn_init$cb, ncb = bn_init$ncb)`),
#'   or a single engine result (auto-wrapped).
#'   Also accepts the full output of `bn_initial_networks()` directly.
#' @param types Character vector of layout types to render.
#'   Default `c("none", "gravity", "charge")`.
#' @param do_community Logical vector controlling which views to render.
#'   Default `c(TRUE, FALSE)` renders both attribute and community views
#'   (order doesn't matter — the tab UI is built from the presence of
#'   both `TRUE` and `FALSE` in the vector). Use `FALSE` for
#'   attribute-only or `TRUE` for community-only.
#' @param title Character. Report title displayed as H1 header.
#'   Default `"Network Analysis"`.
#' @param subtitle Character or `NULL`. Optional subtitle displayed below the
#'   title, above the border line. Default `"Project Name (123456789)"`;
#'   pass `NULL` to suppress.
#' @param interactive Logical. Passed through to `bn_visual()`.
#'   Default `TRUE`.
#' @param physics Logical. If `TRUE`, networks render with physics enabled
#'   (nodes repel/attract in real time). If `FALSE` (default), networks
#'   render with physics to compute layout, then physics is disabled after
#'   stabilization so nodes stay fixed.
#' @param default_type Character or `NULL`. Which layout type to show by
#'   default in the dropdown selector. Must be one of `types`. If `NULL`
#'   (default), uses the first element of `types`.
#' @param gravity_constant Numeric. Gravitational constant for the gravity
#'   layout. Passed through to `bn_visual()`. Default `-9000`.
#' @param central_gravity Numeric. Central gravity strength for the gravity
#'   layout. Passed through to `bn_visual()`. Default `0.2`.
#' @param charge_layout Character. igraph layout algorithm for the charge
#'   layout. Passed through to `bn_visual()`. Default `"layout_with_fr"`.
#' @param add_key Logical. If `TRUE` (default), adds a community color
#'   legend to each network. Pass `FALSE` to suppress (the legend can
#'   cause overflow in tight iframes).
#' @param self_contained Logical. If `TRUE` (default), embeds all widget
#'   JS/CSS into a single HTML file via base64 iframes. If `FALSE`, writes
#'   widget dependencies to a `lib/` folder alongside the HTML file (faster
#'   to generate, but not portable as a single file).
#' @param file Character or `NULL`. Output HTML file path. If `NULL` (default),
#'   auto-generates from `title` and `subtitle` (e.g., `"Network Analysis.html"`
#'   or `"Network Analysis - Feb 2026.html"`).
#' @param open Logical. If `TRUE` (default), opens the file in the
#'   browser via `utils::browseURL()`. Pass `FALSE` to suppress (useful
#'   for headless / server contexts).
#' @param save_name Character or `NULL`. Name portion of the Save Layout
#'   filename (without extension). If `NULL` (default), auto-generates from
#'   `title` and `subtitle`.
#' @param seed Numeric. Passed through to `bn_visual()`. Default `1`.
#' @param add_additional_results Logical. If `TRUE`, and a passed result
#'   (e.g., the output of `bn_finalize_network()`) contains `$impacts`
#'   and/or `$prioritizations`, those tables are rendered as extra tabs
#'   in each accordion section: "Attribute Impacts", "Community Impacts"
#'   (when community results are present), and "Prioritization". Styling
#'   matches the `bn_write()` dashboards (grey header fill, color-coded
#'   p-values). The Prioritization tab reads its `sig_threshold` and
#'   `marginal_threshold` from `prioritizations$meta`, so the colour bands
#'   stay consistent with whatever was set at `bn_finalize_network()` /
#'   `bn_prioritizations()` time. Default `FALSE`.
#' @param results_excel Optional. Path(s) to prebaked `.xlsx` file(s) (e.g.
#'   produced by `bn_write()`) to embed as **Download Report** buttons on
#'   each accordion. Accepted shapes:
#'   * `NULL` (default) — no Download Report buttons.
#'   * A single string — used for the (single) result.
#'   * A named list/vector — matched to `results` by name; missing or NULL
#'     slots get no button.
#'   * An unnamed list/vector — matched to `results` by position; trailing
#'     unmatched slots get no button.
#' @param add_prioritization_pvalue Logical. If `TRUE`, the prioritization
#'   dashboard table includes its `p-value` column (current behavior). If
#'   `FALSE` (default), the p-value column is removed from the prioritization
#'   table.
#' @param impact_outcome_display Character or `NULL`. Initial value of the
#'   impact dashboard's Outcome dropdown — `"Point Change"` or `"% Change"`.
#'   `NULL` (default) auto-detects from the DV type (dichotomous outcomes
#'   default to `"Point Change"`, continuous outcomes default to
#'   `"% Change"`). Pass an explicit string to override.
#' @param shift_type Character. Initial value of the impact dashboard's
#'   Shift Type dropdown. One of `"absolute"` (Fixed Step, default),
#'   `"proportional"` (% of Current Mean), `"headroom"` (% Toward Top),
#'   or `"range"` (% of Range). Only consulted when the underlying
#'   impact table emitted the corresponding shift variant.
#' @param prioritize_display Character or `NULL`. Initial value of the
#'   prioritization Display dropdown — `"Point Change"` or `"% Change"`.
#'   `NULL` (default) auto-detects from the DV type (dichotomous outcomes
#'   default to `"Point Change"`, continuous outcomes default to
#'   `"% Change"`). Pass an explicit string to override the auto-detection.
#' @param trim_wb Logical. When `TRUE` (default), strips the unused boot-stat
#'   columns (`_sd`, `_se`, `_t`, `_ci_low`, `_ci_high`) from the impact
#'   tables before embedding the JSON payload in the HTML. The dashboard
#'   only consumes `_mean` and `_p_value`; the other 5 stats are serialized
#'   but never read. With bootstrapping + many subgroups + brand levels,
#'   keeping all 7 stats inflates the payload (and the resulting HTML +
#'   browser parse time) by ~3.5×. The in-memory result is not mutated —
#'   trimming happens on a copy local to the renderer. When `FALSE`, the
#'   full tables are embedded as-is, and any table exceeding 16,384
#'   columns aborts with an error (same threshold as `bn_write()` — at
#'   that scale the payload is pathological even though HTML itself has
#'   no column limit).
#' @param qc_mode Logical. Enables QC-mode affordances in the HTML report
#'   — currently: hover tooltips on Impact dashboard cells that expose the
#'   underlying raw metric value and unrounded index (useful for validating
#'   that computed values line up with expectations, spotting outliers,
#'   cross-referencing Excel). Intentionally off for client-facing reports
#'   so the displayed integer indices aren't shadowed by scientific-notation
#'   hover text. Default `FALSE`.
#'
#' @return The file path (invisibly).
#'
#' @examples
#' \dontrun{
#' # Full report from bn_initial_networks output
#' bn_init <- work::bn_initial_networks(df = df_bn, dv = NULL, batteries)
#' work::bn_report(bn_init, file = "bn_exploratory.html")
#'
#' # Single result, attribute-only, charge layout only
#' work::bn_report(bn_init$cb, types = "charge", do_community = FALSE)
#'
#' # Named comparison
#' work::bn_report(
#'   list(Experimental = bn_init_exp$cb, Control = bn_init_ctrl$cb),
#'   title = "Kadro Millennials - BN Comparison"
#' )
#' }
#'
#' @export
bn_report <- function(
    results,
    types = c("none", "gravity", "charge", "hierarchy"),
    do_community = c(TRUE, FALSE),
    title = "Network Analysis",
    subtitle = "Project Name (123456789)",
    interactive = TRUE,
    physics = FALSE,
    default_type = "gravity",
    gravity_constant = -9000,
    central_gravity = 0.2,
    charge_layout = "layout_with_fr",
    add_key = TRUE,
    self_contained = TRUE,
    save_name = NULL,
    file = NULL,
    open = TRUE,
    seed = 1,
    add_additional_results = FALSE,
    results_excel = NULL,
    qc_mode = FALSE,
    impact_outcome_display = NULL,
    shift_type      = c("absolute", "proportional", "headroom", "range"),
    add_prioritization_pvalue = FALSE,
    prioritize_display = NULL,
    trim_wb = TRUE
){

  # NULL impact_outcome_display means "auto-detect by DV type" inside the
  # impact dashboard render. Explicit values ("Point Change" / "% Change")
  # are validated and translated to the engine's "absolute" / "proportional"
  # internal vocabulary that downstream code expects.
  outcome_display <- if (!is.null(impact_outcome_display)) {
    impact_outcome_display <- match.arg(impact_outcome_display,
      c("Point Change", "% Change"))
    if (impact_outcome_display == "Point Change") "absolute" else "proportional"
  } else NULL
  shift_type      <- match.arg(shift_type)

  # --- auto-name from title/subtitle ---
  auto_name <- if (!is.null(subtitle)) {
    paste(subtitle, title, sep = " - ")
  } else {
    title
  }

  if (is.null(save_name)) save_name <- auto_name
  if (is.null(file)) {
    file <- paste0(auto_name, ".html")
  } else if (!grepl("\\.html$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".html")
  }

  # --- default type ---
  if (is.null(default_type)) default_type <- types[1]
  default_type <- match.arg(default_type, types)

  # --- detect input shape and normalize to named list of engine results ---
  results <- .bn_report_normalize_results(results)

  # --- type labels ---
  type_labels <- purrr::map_chr(types, function(type) {
    switch(type,
      none = "Dynamic",
      gravity = "Gravity",
      charge = "Charge",
      hierarchy = "Hierarchy",
      type
    )
  })

  has_tabs <- length(do_community) > 1 && all(c(FALSE, TRUE) %in% do_community)

  # --- temp dir for individual widget html files ---
  tmp_dir <- tempfile("bn_report_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # --- normalize results_excel to a list aligned with `results` ---
  # Result: a named list of length(results); each slot is either a file path
  # (character) or NULL (no Download Report button for that accordion).
  results_excel <- .bn_report_normalize_results_excel(results_excel, results)

  # Read a prebaked .xlsx from disk and return base64 + filename. Returns
  # NULL when path is NULL or the file is missing — caller omits the button.
  .read_xlsx_b64 <- function(path) {
    if (is.null(path) || !nzchar(path)) return(NULL)
    if (!file.exists(path)) {
      warning("results_excel: file not found, skipping Download Report: ",
              path, call. = FALSE)
      return(NULL)
    }
    bytes <- readBin(path, what = "raw", n = file.info(path)$size)
    list(
      b64      = base64enc::base64encode(bytes),
      filename = basename(path)
    )
  }

  # ---- Per-bn_report state container for the extracted widget renderer ----
  # `state` is an environment because .bn_report_render_widget() needs to
  # mutate these counters across calls within a single bn_report
  # invocation; environments give us reference semantics without `<<-`
  # gymnastics across a function boundary.
  state <- new.env(parent = emptyenv())
  state$widget_counter   <- 0L
  state$dep_cache        <- NULL
  state$first_lib_prefix <- NULL
  state$shared_deps_b64  <- NULL

  # Immutable config bundle handed to .bn_report_render_widget() each call.
  cfg <- list(
    add_key          = add_key,
    interactive      = interactive,
    physics          = physics,
    gravity_constant = gravity_constant,
    central_gravity  = central_gravity,
    charge_layout    = charge_layout,
    seed             = seed,
    title            = title,
    subtitle         = subtitle,
    self_contained   = self_contained,
    tmp_dir          = tmp_dir
  )

  # Thin wrapper closures so call sites (and the type-panel builder)
  # don't have to thread cfg / state on every invocation.
  render_widget <- function(result, type, do_community_val, result_name = NULL) {
    .bn_report_render_widget(result, type, do_community_val, result_name,
                              cfg = cfg, state = state)
  }
  render_membership <- function(result, result_name) {
    .bn_report_render_membership(result, result_name)
  }


  # --- build html sections ---
  # structure: result (accordion) > type (dropdown) > view (tabs)

  result_counter <- 0L

  sections <- purrr::imap(results, function(result, name) {

    result_counter <<- result_counter + 1L
    rid <- glue::glue("r{result_counter}")

    # Pre-build shared impact / prioritization payloads ONCE per result.
    # Each payload is rendered as a single <script> at the result level;
    # every type-panel's dashboard then references it via
    # `data-impact-data-id` instead of carrying its own copy. Cuts file
    # size by ~(N-1) × payload-size for N layout types when
    # add_additional_results = TRUE.
    shared_scripts <- character(0)
    impacts_res <- result[["impacts"]]
    if (isTRUE(trim_wb)) {
      impacts_res <- .bn_impact_drop_unused_boot_stats(impacts_res)
    } else {
      .bn_impact_assert_column_cap(impacts_res, fn_label = "bn_report")
    }
    prioritizations_res <- result[["prioritizations"]]

    shared_attr_id <- NULL
    shared_comm_id <- NULL
    if (isTRUE(add_additional_results)) {
      if (!is.null(impacts_res) && !is.null(impacts_res[["table_attribute"]])) {
        shared_attr_id <- as.character(glue::glue("impact-data-{rid}-attr"))
        attr_payload <- .bn_report_render_attribute_impacts_dashboard(
          impacts_res, result_name = name, dashboard_id = "_payload_only",
          qc_mode = qc_mode,
          outcome_display = outcome_display, shift_type = shift_type
        )
        shared_scripts <- c(shared_scripts, paste0(
          '<script type="application/json" id="', shared_attr_id, '">',
          attr_payload$data_json, '</script>'
        ))
      }
      if (!is.null(impacts_res) && !is.null(impacts_res[["table_community"]])) {
        shared_comm_id <- as.character(glue::glue("impact-data-{rid}-comm"))
        comm_payload <- .bn_report_render_attribute_impacts_dashboard(
          impacts_res, result_name = name, dashboard_id = "_payload_only",
          is_community = TRUE, qc_mode = qc_mode,
          outcome_display = outcome_display, shift_type = shift_type
        )
        shared_scripts <- c(shared_scripts, paste0(
          '<script type="application/json" id="', shared_comm_id, '">',
          comm_payload$data_json, '</script>'
        ))
      }
    }

    # Build type panels — each contains tabs (or single view). Heavy
    # lifting lives in .bn_report_build_type_panel() (bn_helpers.R);
    # this loop just iterates and computes the per-panel id / visibility.
    type_panels <- purrr::map2_chr(types, type_labels, function(type, label) {
      panel_id <- glue::glue("{rid}_{type}")
      visible <- if (type == default_type) "block" else "none"
      .bn_report_build_type_panel(
        type = type, label = label, panel_id = panel_id, visible = visible,
        has_tabs = has_tabs, result = result, name = name, rid = rid,
        types = types, type_labels = type_labels,
        do_community = do_community,
        render_widget = render_widget, render_membership = render_membership,
        add_additional_results = add_additional_results,
        impacts_res = impacts_res, prioritizations_res = prioritizations_res,
        shared_attr_id = shared_attr_id, shared_comm_id = shared_comm_id,
        qc_mode = qc_mode,
        outcome_display = outcome_display, shift_type = shift_type,
        add_prioritization_pvalue = add_prioritization_pvalue,
        prioritize_display = prioritize_display
      )
    })

    type_panels_str <- paste(type_panels, collapse = "\n")
    shared_scripts_str <- paste(shared_scripts, collapse = "\n")

    open_attr <- if (result_counter == 1L) " open" else ""

    # If the user supplied a prebaked .xlsx for this result via results_excel,
    # embed its bytes (base64) in a hidden <script> and add a Download Report
    # button to the accordion summary. NULL slot = no button.
    prebake <- .read_xlsx_b64(results_excel[[name]])
    if (!is.null(prebake)) {
      xlsx_id <- glue::glue("xlsx_{rid}")
      esc_filename <- htmltools::htmlEscape(prebake$filename, attribute = TRUE)
      download_btn_html <- glue::glue(
        '<button class="report-btn accordion-download-btn" ',
        'onclick="event.stopPropagation();event.preventDefault();',
        'downloadAccordionReport(this);" ',
        'data-xlsx-id="{xlsx_id}" data-filename="{esc_filename}">',
        'Download Report</button>'
      )
      xlsx_script_html <- glue::glue(
        '<script type="application/octet-stream" id="{xlsx_id}" ',
        'class="prebake-xlsx">{prebake$b64}</script>'
      )
    } else {
      download_btn_html <- ""
      xlsx_script_html  <- ""
    }

    glue::glue(
      '<details class="result-accordion"{open_attr}>',
      '  <summary>',
      '    <span class="accordion-title">{name}</span>',
      '    {download_btn_html}',
      '  </summary>',
      '  {xlsx_script_html}',
      '  {shared_scripts_str}',
      '  <div class="accordion-body">',
      '    {type_panels_str}',
      '  </div>',
      '</details>'
    )
  })


  # --- assemble full html page ---
  html_body <- paste(sections, collapse = "\n")

  # subtitle html (empty string if NULL)
  subtitle_html <- if (!is.null(subtitle)) {
    glue::glue('<p class="subtitle">{subtitle}</p>')
  } else {
    ""
  }

  report_css <- .bn_report_css()
  report_js <- .bn_report_js(save_name)

  # shared deps variable for blob URL iframes (self_contained only)
  shared_deps_tag <- if (!is.null(state$shared_deps_b64)) {
    paste0('  <script>var __sharedDepsB64 = "', state$shared_deps_b64, '";</script>')
  } else {
    ""
  }

  full_html <- glue::glue('
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>{title}</title>
{shared_deps_tag}
  <style>
{report_css}
  </style>
  <script>
{report_js}
  </script>
</head>
<body>
  <div class="page-header">
    <div>
      <h1>{title}</h1>
      {subtitle_html}
    </div>
    <div class="header-actions">
      <button class="report-btn" onclick="location.reload()">Reset</button>
      <button class="report-btn" onclick="saveAllLayouts()">Save</button>
      <button class="report-btn" onclick="document.getElementById(&quot;globalFileInput&quot;).click()">Load</button>
      <input type="file" id="globalFileInput" accept=".resondex_bn,.json" style="display:none" onchange="loadAllLayouts(this)">
    </div>
  </div>
  {html_body}
</body>
</html>')

  # --- write ---
  writeLines(full_html, file)

  if (!self_contained) {
    lib_dir <- file.path(dirname(file), "lib")
    if (!dir.exists(lib_dir)) dir.create(lib_dir, recursive = TRUE)

    widget_files <- list.files(tmp_dir, pattern = "^widget_.*\\.html$", full.names = TRUE)
    file.copy(widget_files, lib_dir, overwrite = TRUE)

    widget_lib <- list.dirs(tmp_dir, recursive = TRUE, full.names = TRUE)
    widget_lib <- widget_lib[widget_lib != tmp_dir]
    for (d in widget_lib) {
      target <- file.path(lib_dir, basename(d))
      if (!dir.exists(target)) dir.create(target, recursive = TRUE)
      file.copy(list.files(d, full.names = TRUE), target, overwrite = TRUE, recursive = TRUE)
    }
  }

  if (open) utils::browseURL(file)

  invisible(file)
}

