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
#'   Default `c(FALSE, TRUE)` renders both attribute and community views.
#'   Use `FALSE` for attribute-only or `TRUE` for community-only.
#' @param title Character. Report title displayed as H1 header.
#'   Default `"Network Analysis"`.
#' @param subtitle Character or `NULL`. Optional subtitle displayed below the
#'   title, above the border line. Default `NULL` (no subtitle).
#' @param interactive Logical. Passed through to `bn_visual()`.
#'   Default `TRUE`.
#' @param physics Logical. If `TRUE` (default), networks render with physics
#'   enabled (nodes repel/attract in real time). If `FALSE`, networks render
#'   with physics to compute layout, then physics is disabled after
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
#' @param add_key Logical. If `TRUE`, adds a community color legend to each
#'   network. Default `FALSE` (legend can cause overflow in iframes).
#' @param self_contained Logical. If `TRUE` (default), embeds all widget
#'   JS/CSS into a single HTML file via base64 iframes. If `FALSE`, writes
#'   widget dependencies to a `lib/` folder alongside the HTML file (faster
#'   to generate, but not portable as a single file).
#' @param file Character or `NULL`. Output HTML file path. If `NULL` (default),
#'   auto-generates from `title` and `subtitle` (e.g., `"Network Analysis.html"`
#'   or `"Network Analysis - Feb 2026.html"`).
#' @param open Logical. If `TRUE`, opens the file in the browser.
#'   Default `FALSE`.
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
#' @param prioritize_display Character or `NULL`. Initial value of the
#'   prioritization Display dropdown — `"Point Change"` or `"% Change"`.
#'   `NULL` (default) auto-detects from the DV type (dichotomous outcomes
#'   default to `"Point Change"`, continuous outcomes default to
#'   `"% Change"`). Pass an explicit string to override the auto-detection.
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
    # Initial value for the impact dashboard's Outcome dropdown. NULL
    # (default) auto-detects from the DV type: dichotomous DVs land on
    # "Point Change"; continuous DVs land on "% Change". Pass "Point Change"
    # or "% Change" to override. Translated internally to the engine's
    # "absolute" / "proportional" vocabulary.
    impact_outcome_display = NULL,
    shift_type      = c("absolute", "proportional", "headroom", "range"),
    # When TRUE, the prioritization dashboard table includes a p-value column
    # (current behavior). When FALSE (default), the p-value column is hidden
    # entirely from the prioritization table.
    add_prioritization_pvalue = FALSE,
    # Initial Display value for the prioritization dashboard. NULL (default)
    # auto-detects from the DV type (dichotomous -> "Point Change",
    # continuous -> "% Change"). Pass an explicit string to override.
    prioritize_display = NULL
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

  widget_counter <- 0L

  # cache for shared widget dependency files (self_contained = TRUE only)
  # populated after first widget is saved; reused for all subsequent widgets
  dep_cache <- NULL
  first_lib_prefix <- NULL
  shared_deps_b64 <- NULL


  # --- helper: render one widget to iframe html ---
  render_widget <- function(result, type, do_community_val, result_name = NULL) {

    # no legend on community tabs
    use_key <- add_key && !do_community_val

    # build namespace key for report-level save/load
    view_name <- if (do_community_val) "community" else "attribute"
    ns <- if (!is.null(result_name)) paste(result_name, type, view_name, sep = "|") else NULL

    # build download prefix: {title} - {subtitle} - {accordion} - {tab}
    tab_label <- if (do_community_val) "Community" else "Attribute"
    dl_prefix <- .bn_report_download_prefix(title, subtitle, result_name, tab_label)

    viz <- tryCatch(
      bn_visual(
        obj = result,
        type = type,
        do_community = do_community_val,
        vs_height = "95vh",
        interactive = interactive,
        # always TRUE: vis.js needs physics ON to compute force-directed layout.
        # when user passes physics=FALSE, the __disablePhysicsAfterStabilize
        # flag (injected below) freezes nodes after stabilization.
        physics = TRUE,
        gravity_constant = gravity_constant,
        central_gravity = central_gravity,
        charge_layout = charge_layout,
        add_key = use_key,
        panel_ns = ns,
        download_prefix = dl_prefix,
        save_visuals = FALSE,
        seed = seed
      ),
      error = function(e) {
        warning("bn_report render_widget failed for [", result_name, " / ", type, " / ", view_name, "]: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(viz)) {
      return(glue::glue(
        '<div style="height: 100px; padding: 20px; color: #888;">',
        '<p>Could not render this view.</p>',
        '</div>'
      ))
    }

    widget_counter <<- widget_counter + 1L
    widget_file <- file.path(tmp_dir, glue::glue("widget_{widget_counter}.html"))
    widget_lib_prefix <- paste0("widget_", widget_counter, "_files")

    # always save non-self-contained to avoid redundant pandoc calls.
    # when self_contained = TRUE, we cache shared lib files from the first
    # widget and manually inline them for all subsequent widgets.
    htmlwidgets::saveWidget(
      viz,
      file = widget_file,
      selfcontained = FALSE
    )

    # read and inject iframe-level CSS overrides
    widget_html <- readLines(widget_file, warn = FALSE) %>% paste(collapse = "\n")
    inject_head <- paste0(
      "<head><style>",
      # Brand layer first (Inter @import must lead): the network iframe is
      # an isolated sandboxed document, so resondex_css() carries the
      # --ndr-* tokens + Inter into it. Without this the visNetwork
      # toolbar's var(--ndr-*) styles fall back to unstyled.
      resondex_css(include_import = TRUE),
      "body,html{margin:0!important;padding:0!important;height:100%!important;overflow:hidden!important;}",
      " .htmlwidget{height:100%!important;}",
      " #pngButton,#svgButton,#fontButton,#physicsButton{width:130px!important;height:30px!important;}",
      "</style>"
    )

    # when physics = FALSE, set a global flag the interactivity JS will read
    if (!physics) {
      inject_head <- paste0(inject_head, "<script>window.__disablePhysicsAfterStabilize=true;</script>")
    }

    widget_html <- sub("<head>", inject_head, widget_html)

    if (self_contained) {
      widget_lib_dir <- file.path(tmp_dir, widget_lib_prefix)

      # cache deps from the first widget; reuse for all subsequent
      if (is.null(dep_cache)) {
        dep_cache <<- .bn_report_cache_deps(widget_html, widget_lib_dir)
        first_lib_prefix <<- widget_lib_prefix
        # store shared deps as base64 once — JS injects into each iframe via blob URL
        deps_string <- .bn_report_shared_deps_string(dep_cache)
        shared_deps_b64 <<- base64enc::base64encode(charToRaw(deps_string))
      }

      # strip shared dep tags, leaving <!--SHARED_DEPS--> marker for JS injection
      widget_html <- .bn_report_strip_deps(
        widget_html, dep_cache, widget_lib_prefix, first_lib_prefix
      )
      widget_b64 <- base64enc::base64encode(charToRaw(widget_html))

      glue::glue(
        '<div class="iframe-wrap" data-widget="{widget_b64}">',
        '<div class="spinner-overlay"><div class="spinner"><div class="spinner-bar"></div><div class="spinner-bar"></div><div class="spinner-bar"></div></div></div>',
        '<iframe style="width: 100%; height: 70vh; border: none;" ',
        'sandbox="allow-scripts allow-downloads" allowfullscreen>',
        '</iframe></div>'
      )
    } else {
      widget_rel <- glue::glue("lib/widget_{widget_counter}.html")
      writeLines(widget_html, widget_file)

      glue::glue(
        '<div class="iframe-wrap">',
        '<div class="spinner-overlay"><div class="spinner"><div class="spinner-bar"></div><div class="spinner-bar"></div><div class="spinner-bar"></div></div></div>',
        '<iframe src="{widget_rel}" ',
        'style="width: 100%; height: 70vh; border: none;" ',
        'sandbox="allow-scripts allow-downloads" allowfullscreen>',
        '</iframe></div>'
      )
    }
  }


  # --- helper: render membership table + card views ---
  render_membership <- function(result, result_name) {
    nodes_df <- tryCatch(
      work::find_recursive(result, x_name = "attribute_viz_prep")$nodes,
      error = function(e) NULL
    )
    if (is.null(nodes_df)) return("")

    # group nodes by community
    groups <- nodes_df %>%
      dplyr::arrange(group) %>%
      dplyr::group_by(community_name, color) %>%
      dplyr::summarise(
        nodes = list(tibble::tibble(id = id, label = label)),
        .groups = "drop"
      )

    # --- table view ---
    table_rows <- purrr::pmap_chr(groups, function(community_name, color, nodes) {
      pills <- purrr::map_chr(seq_len(nrow(nodes)), function(i) {
        glue::glue('<span class="node-pill" data-node-id="{nodes$id[i]}">{nodes$label[i]}</span>')
      })
      pills_str <- paste(pills, collapse = "")
      n_nodes <- nrow(nodes)
      glue::glue(
        '<tr>',
        '<td><span class="membership-dot" style="background: {color};"></span>',
        '<span class="community-label" data-color="{color}" data-orig-comm="{community_name}">{community_name}</span>',
        '<span class="card-count">{n_nodes}</span></td>',
        '<td><div class="card-nodes">{pills_str}</div></td>',
        '</tr>'
      )
    })
    table_html <- paste0(
      '<table class="membership-table">',
      '<thead><tr><th>Community</th><th>Attributes</th></tr></thead>',
      '<tbody>', paste(table_rows, collapse = ""), '</tbody></table>'
    )

    # --- card view ---
    cards <- purrr::pmap_chr(groups, function(community_name, color, nodes) {
      pills <- purrr::map_chr(seq_len(nrow(nodes)), function(i) {
        glue::glue('<span class="node-pill" data-node-id="{nodes$id[i]}">{nodes$label[i]}</span>')
      })
      pills_str <- paste(pills, collapse = "")
      n_nodes <- nrow(nodes)
      glue::glue(
        '<div class="membership-card" style="border-left: 4px solid {color};">',
        '<div class="card-header"><span class="membership-dot" style="background: {color};"></span>',
        '<span class="community-label" data-color="{color}" data-orig-comm="{community_name}">{community_name}</span>',
        '<span class="card-count">{n_nodes}</span></div>',
        '<div class="card-nodes">{pills_str}</div>',
        '</div>'
      )
    })
    cards_html <- paste0('<div class="membership-cards">', paste(cards, collapse = ""), '</div>')

    # wrap both views with toggle
    paste0(
      '<div class="membership-wrap" data-result="', result_name, '">',
      '<div class="membership-toolbar">',
      '<button class="report-btn membership-toggle" onclick="toggleMembershipView(this)" title="Switch view">',
      '&#9776; Toggle View</button></div>',
      '<div class="membership-view membership-table-view" style="display:none;">', table_html, '</div>',
      '<div class="membership-view membership-card-view">', cards_html, '</div>',
      '</div>'
    )
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
    impacts_res         <- result[["impacts"]]
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

    # build type panels — each contains tabs (or single view)
    type_panels <- purrr::map2_chr(types, type_labels, function(type, label) {

      panel_id <- glue::glue("{rid}_{type}")
      visible <- if (type == default_type) "block" else "none"

      # Per-type layout dropdown — each type-panel carries its own copy with
      # its own type pre-selected, so when switchType swaps to this panel the
      # dropdown already reads the right value (no JS sync needed).
      type_options <- purrr::map2_chr(types, type_labels, function(t, l) {
        sel <- if (t == type) " selected" else ""
        glue::glue('<option value="{rid}_{t}"{sel}>{l}</option>')
      })
      type_options_str <- paste(type_options, collapse = "\n            ")
      layout_ctrl_html <- glue::glue(
        '<div class="layout-controls">',
        '<label for="{panel_id}_layout">Layout</label>',
        '<select id="{panel_id}_layout" class="layout-select" ',
        'onchange="switchType(\'{rid}\', this.value)">',
        '{type_options_str}',
        '</select>',
        '</div>'
      )

      if (has_tabs) {

        # Wrap the network views in .network-dashboard so they get the same
        # 20px outer padding as .impact-dashboard / .priort-dashboard /
        # .membership-wrap — keeps every tab\\u2019s controls box visually
        # identical (same edge spacing, same gap below to the content).
        tab_attr <- paste0(
          '<div class="network-dashboard">',
          layout_ctrl_html,
          render_widget(result, type, FALSE, result_name = name),
          '</div>'
        )
        tab_comm <- paste0(
          '<div class="network-dashboard">',
          layout_ctrl_html,
          render_widget(result, type, TRUE,  result_name = name),
          '</div>'
        )
        tab_memb <- render_membership(result, name)

        attr_id <- glue::glue("{panel_id}_attr")
        comm_id <- glue::glue("{panel_id}_comm")
        memb_id <- glue::glue("{panel_id}_memb")

        # Optional extra tabs: Attribute Impacts, Community Impacts, Prioritization
        extras_buttons <- character(0)
        extras_panels  <- character(0)

        if (isTRUE(add_additional_results)) {

          if (!is.null(impacts_res) && !is.null(impacts_res[["table_attribute"]])) {
            impact_attr_id <- glue::glue("{panel_id}_impact_attr")
            impact_attr_res <- .bn_report_render_attribute_impacts_dashboard(
              impacts_res, result_name = name, dashboard_id = impact_attr_id,
              qc_mode = qc_mode,
              outcome_display = outcome_display, shift_type = shift_type,
              shared_data_id = shared_attr_id
            )
            impact_attr_html <- impact_attr_res$html
            extras_buttons <- c(extras_buttons, glue::glue(
              '    <button class="tab-btn" onclick="switchTab(this, \'{impact_attr_id}\')">Attribute Impacts</button>'
            ))
            extras_panels <- c(extras_panels, glue::glue(
              '  <div id="{impact_attr_id}" class="tab-panel impact-panel" data-result="{name}" data-layout="{type}" data-view="impact_attr">{impact_attr_html}</div>'
            ))
          }

          if (!is.null(impacts_res) && !is.null(impacts_res[["table_community"]])) {
            impact_comm_id <- glue::glue("{panel_id}_impact_comm")
            impact_comm_res <- .bn_report_render_attribute_impacts_dashboard(
              impacts_res, result_name = name, dashboard_id = impact_comm_id,
              is_community = TRUE, qc_mode = qc_mode,
              outcome_display = outcome_display, shift_type = shift_type,
              shared_data_id = shared_comm_id
            )
            impact_comm_html <- impact_comm_res$html
            extras_buttons <- c(extras_buttons, glue::glue(
              '    <button class="tab-btn" onclick="switchTab(this, \'{impact_comm_id}\')">Community Impacts</button>'
            ))
            extras_panels <- c(extras_panels, glue::glue(
              '  <div id="{impact_comm_id}" class="tab-panel impact-panel" data-result="{name}" data-layout="{type}" data-view="impact_comm">{impact_comm_html}</div>'
            ))
          }

          if (!is.null(prioritizations_res)) {
            priort_id <- glue::glue("{panel_id}_priort")
            # Pull thresholds from the prioritizations meta (set at
            # bn_finalize_network / bn_prioritizations time). Fall back to
            # standard defaults if absent.
            priort_meta <- prioritizations_res[["meta"]] %||% list()
            priort_html <- .bn_report_render_prioritization_dashboard(
              prioritizations_res, result_name = name, dashboard_id = priort_id,
              sig_threshold = priort_meta[["sig_threshold"]] %||% 0.05,
              marginal_threshold = priort_meta[["marginal_threshold"]] %||% 0.10,
              add_prioritization_pvalue = add_prioritization_pvalue,
              prioritize_display = prioritize_display
            )
            extras_buttons <- c(extras_buttons, glue::glue(
              '    <button class="tab-btn" onclick="switchTab(this, \'{priort_id}\')">Prioritization</button>'
            ))
            extras_panels <- c(extras_panels, glue::glue(
              '  <div id="{priort_id}" class="tab-panel priort-panel" data-result="{name}" data-layout="{type}" data-view="prioritization">{priort_html}</div>'
            ))
          }
        }

        extras_buttons_str <- paste(extras_buttons, collapse = "\n")
        extras_panels_str  <- paste(extras_panels,  collapse = "\n")

        glue::glue(
          '<div id="{panel_id}" class="type-panel" style="display: {visible};">',
          '  <div class="tab-bar">',
          '    <button class="tab-btn active" onclick="switchTab(this, \'{attr_id}\')">Attribute</button>',
          '    <button class="tab-btn" onclick="switchTab(this, \'{comm_id}\')">Community</button>',
          '    <button class="tab-btn" onclick="switchTab(this, \'{memb_id}\')">Community Assignments</button>',
          '{extras_buttons_str}',
          '  </div>',
          '  <div id="{attr_id}" class="tab-panel active attr-panel" data-result="{name}" data-layout="{type}" data-view="attribute">{tab_attr}</div>',
          '  <div id="{comm_id}" class="tab-panel comm-panel" data-result="{name}" data-layout="{type}" data-view="community">{tab_comm}</div>',
          '  <div id="{memb_id}" class="tab-panel membership-panel" data-result="{name}" data-layout="{type}" data-view="membership">{tab_memb}</div>',
          '{extras_panels_str}',
          '</div>'
        )

      } else {

        panel_content <- render_widget(result, type, do_community[1], result_name = name)

        glue::glue(
          '<div id="{panel_id}" class="type-panel" style="display: {visible};">',
          '  <div class="network-dashboard">{layout_ctrl_html}{panel_content}</div>',
          '</div>'
        )
      }
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
  shared_deps_tag <- if (!is.null(shared_deps_b64)) {
    paste0('  <script>var __sharedDepsB64 = "', shared_deps_b64, '";</script>')
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





























