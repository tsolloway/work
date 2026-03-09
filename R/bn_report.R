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
    types = c("none", "gravity", "charge"),
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
    seed = 1
){

  # --- auto-name from title/subtitle ---
  auto_name <- if (!is.null(subtitle)) {
    paste(title, subtitle, sep = " - ")
  } else {
    title
  }

  if (is.null(save_name)) save_name <- auto_name
  if (is.null(file)) file <- paste0(auto_name, ".html")

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
      "body,html{margin:0!important;padding:0!important;height:100%!important;overflow:hidden!important;}",
      " .htmlwidget{height:100%!important;}",
      " #pngButton,#svgButton,#fontButton,#physicsButton{width:130px!important;height:34px!important;}",
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
        'sandbox="allow-scripts allow-downloads">',
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
        'sandbox="allow-scripts allow-downloads">',
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
        '<span class="community-label" data-color="{color}">{community_name}</span>',
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
        '<span class="community-label" data-color="{color}">{community_name}</span>',
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
      '<div class="membership-view membership-table-view">', table_html, '</div>',
      '<div class="membership-view membership-card-view" style="display:none;">', cards_html, '</div>',
      '</div>'
    )
  }

  # --- build html sections ---
  # structure: result (accordion) > type (dropdown) > view (tabs)

  result_counter <- 0L

  sections <- purrr::imap(results, function(result, name) {

    result_counter <<- result_counter + 1L
    rid <- glue::glue("r{result_counter}")

    # build dropdown options
    options_html <- purrr::map2_chr(types, type_labels, function(type, label) {
      sel <- if (type == default_type) " selected" else ""
      glue::glue('<option value="{rid}_{type}"{sel}>{label}</option>')
    })
    options_str <- paste(options_html, collapse = "\n          ")

    # build type panels — each contains tabs (or single view)
    type_panels <- purrr::map2_chr(types, type_labels, function(type, label) {

      panel_id <- glue::glue("{rid}_{type}")
      visible <- if (type == default_type) "block" else "none"

      if (has_tabs) {

        tab_attr <- render_widget(result, type, FALSE, result_name = name)
        tab_comm <- render_widget(result, type, TRUE, result_name = name)
        tab_memb <- render_membership(result, name)

        attr_id <- glue::glue("{panel_id}_attr")
        comm_id <- glue::glue("{panel_id}_comm")
        memb_id <- glue::glue("{panel_id}_memb")

        glue::glue(
          '<div id="{panel_id}" class="type-panel" style="display: {visible};">',
          '  <div class="tab-bar">',
          '    <button class="tab-btn active" onclick="switchTab(this, \'{attr_id}\')">Attribute</button>',
          '    <button class="tab-btn" onclick="switchTab(this, \'{comm_id}\')">Community</button>',
          '    <button class="tab-btn" onclick="switchTab(this, \'{memb_id}\')">Community Assignments</button>',
          '  </div>',
          '  <div id="{attr_id}" class="tab-panel active attr-panel" data-result="{name}" data-layout="{type}" data-view="attribute">{tab_attr}</div>',
          '  <div id="{comm_id}" class="tab-panel comm-panel" data-result="{name}" data-layout="{type}" data-view="community">{tab_comm}</div>',
          '  <div id="{memb_id}" class="tab-panel membership-panel" data-result="{name}" data-layout="{type}" data-view="membership">{tab_memb}</div>',
          '</div>'
        )

      } else {

        panel_content <- render_widget(result, type, do_community[1], result_name = name)

        glue::glue(
          '<div id="{panel_id}" class="type-panel" style="display: {visible};">',
          '  <div style="padding: 12px;">{panel_content}</div>',
          '</div>'
        )
      }
    })

    type_panels_str <- paste(type_panels, collapse = "\n")

    open_attr <- if (result_counter == 1L) " open" else ""

    glue::glue(
      '<details class="result-accordion"{open_attr}>',
      '  <summary>{name}</summary>',
      '  <div class="accordion-body">',
      '    <div class="controls-bar">',
      '      <label for="{rid}_select">Layout</label>',
      '      <select id="{rid}_select" onchange="switchType(\'{rid}\', this.value)">',
      '        {options_str}',
      '      </select>',
      '    </div>',
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



# --- internal: normalize results to named list of engine results ---
.bn_report_normalize_results <- function(results) {

  # single engine result (has $meta or $bn)
  if (!is.null(results[["meta"]]) || !is.null(results[["bn"]])) {
    return(list(Network = results))
  }

  # helper: does this object look like an engine result?
  .is_engine <- function(x) {
    is.list(x) && !is.null(x[["meta"]])
  }

  # bn_initial_networks output: mixed bare engines (unsupervised) and
  # nested engine lists (supervised, keyed by DV name)
  children_have_meta <- purrr::map_lgl(results, .is_engine)
  children_have_nested_engines <- purrr::map_lgl(results, function(x) {
    is.list(x) && !.is_engine(x) && any(purrr::map_lgl(x, .is_engine))
  })

  if (any(children_have_meta) || any(children_have_nested_engines)) {
    flat <- list()

    # bare engines (unsupervised): results$cb_unsupervised = <engine>
    for (nm in names(results)[children_have_meta]) {
      flat[[nm]] <- results[[nm]]
    }

    # nested engines (supervised): results$cb_direct$ltr = <engine>
    for (nm in names(results)[children_have_nested_engines]) {
      child <- results[[nm]]
      engines <- purrr::keep(child, .is_engine)
      for (dv_nm in names(engines)) {
        label <- if (length(engines) == 1) nm else paste(nm, dv_nm, sep = " - ")
        flat[[label]] <- engines[[dv_nm]]
      }
    }

    return(flat)
  }

  # already a named list of engine results
  if (is.null(names(results))) {
    names(results) <- paste("Network", seq_along(results))
  }

  results
}


# --- internal: report CSS ---
#' @noRd
.bn_report_css <- function() {
  paste(c(
    'body {',
    '  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;',
    '  margin: 20px 40px;',
    '  background: #fafafa;',
    '}',
    'h1 { margin: 0; }',
    '.subtitle {',
    '  margin: 2px 0 0 0;',
    '  font-size: 14px;',
    '  font-weight: 400;',
    '  color: #666;',
    '}',
    '.page-header {',
    '  border-bottom: 2px solid #333;',
    '  padding-bottom: 4px;',
    '  display: flex;',
    '  align-items: flex-end;',
    '  justify-content: space-between;',
    '}',
    '.header-actions {',
    '  display: flex;',
    '  gap: 8px;',
    '  flex-shrink: 0;',
    '}',
    '',
    '/* accordion */',
    '.result-accordion {',
    '  margin: 20px 0;',
    '  border: 1px solid #ddd;',
    '  border-radius: 8px;',
    '  background: #fff;',
    '  overflow: hidden;',
    '}',
    '.result-accordion summary {',
    '  padding: 14px 20px;',
    '  font-size: 18px;',
    '  font-weight: 700;',
    '  color: #333;',
    '  cursor: pointer;',
    '  user-select: none;',
    '  background: #f7f7f7;',
    '  border-bottom: 1px solid #eee;',
    '}',
    '.result-accordion summary:hover { background: #f0f0f0; }',
    '.accordion-body { padding: 0; }',
    '',
    '/* dropdown controls */',
    '.controls-bar {',
    '  display: flex;',
    '  align-items: center;',
    '  gap: 10px;',
    '  padding: 12px 20px;',
    '  background: #fafafa;',
    '  border-bottom: 1px solid #eee;',
    '}',
    '.controls-bar label {',
    '  font-size: 14px;',
    '  font-weight: 600;',
    '  color: #555;',
    '}',
    '.controls-bar select {',
    '  padding: 6px 12px;',
    '  font-size: 14px;',
    '  border: 1px solid #ccc;',
    '  border-radius: 4px;',
    '  background: #fff;',
    '  color: #333;',
    '  cursor: pointer;',
    '}',
    '.report-btn {',
    '  padding: 6px 12px;',
    '  font-size: 13px;',
    '  border: 1px solid #ccc;',
    '  border-radius: 4px;',
    '  background: #fff;',
    '  color: #333;',
    '  cursor: pointer;',
    '}',
    '.report-btn:hover { background: #f0f0f0; }',
    '',
    '/* type panels */',
    '.type-panel { padding: 0; }',
    '',
    '/* tabs */',
    '.tab-bar {',
    '  display: flex;',
    '  border-bottom: 2px solid #ddd;',
    '  background: #f9f9f9;',
    '}',
    '.tab-btn {',
    '  padding: 10px 24px;',
    '  border: none;',
    '  background: transparent;',
    '  font-size: 14px;',
    '  font-weight: 500;',
    '  color: #888;',
    '  cursor: pointer;',
    '  border-bottom: 2px solid transparent;',
    '  margin-bottom: -2px;',
    '  transition: color 0.15s, border-color 0.15s;',
    '}',
    '.tab-btn:hover { color: #444; }',
    '.tab-btn.active {',
    '  color: #222;',
    '  border-bottom-color: #333;',
    '}',
    '.tab-panel { display: none; padding: 0; }',
    '.tab-panel iframe { display: block; }',
    '.tab-panel.active { display: block; }',
    '',
    '/* membership tab */',
    '.membership-wrap { padding: 20px; }',
    '.membership-toolbar {',
    '  display: flex;',
    '  justify-content: flex-end;',
    '  margin-bottom: 12px;',
    '}',
    '.membership-toggle { font-size: 14px; padding: 4px 10px; }',
    '.membership-table {',
    '  width: 100%;',
    '  border-collapse: collapse;',
    '  font-size: 14px;',
    '}',
    '.membership-table th {',
    '  text-align: left;',
    '  padding: 10px 12px;',
    '  border-bottom: 2px solid #ddd;',
    '  font-weight: 600;',
    '  color: #555;',
    '}',
    '.membership-table th:first-child,',
    '.membership-table td:first-child {',
    '  min-width: 125px;',
    '  max-width: 150px;',
    '}',
    '.membership-table td {',
    '  padding: 10px 12px;',
    '  border-bottom: 1px solid #eee;',
    '  vertical-align: top;',
    '}',
    '.membership-table tr:hover { background: #f8f8f8; }',
    '.membership-dot {',
    '  display: inline-block;',
    '  width: 12px;',
    '  height: 12px;',
    '  border-radius: 50%;',
    '  margin-right: 8px;',
    '  vertical-align: middle;',
    '}',
    '.membership-cards {',
    '  display: grid;',
    '  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));',
    '  gap: 16px;',
    '}',
    '.membership-card {',
    '  background: #fff;',
    '  border: 1px solid #e0e0e0;',
    '  border-radius: 8px;',
    '  padding: 16px;',
    '}',
    '.card-header {',
    '  font-weight: 600;',
    '  font-size: 15px;',
    '  color: #333;',
    '  display: flex;',
    '  align-items: center;',
    '  margin-bottom: 12px;',
    '}',
    '.card-count {',
    '  margin-left: 8px;',
    '  font-size: 12px;',
    '  font-weight: 500;',
    '  color: #888;',
    '  background: #f0f0f0;',
    '  padding: 2px 8px;',
    '  border-radius: 10px;',
    '}',
    '.card-nodes { display: flex; flex-wrap: wrap; gap: 6px; }',
    '.node-pill {',
    '  display: inline-block;',
    '  padding: 4px 10px;',
    '  background: #f0f0f0;',
    '  border-radius: 12px;',
    '  font-size: 13px;',
    '  color: #444;',
    '}',
    '',
    '/* loading spinner */',
    '.iframe-wrap { position: relative; }',
    '.spinner-overlay {',
    '  position: absolute;',
    '  top: 0; left: 0; right: 0; bottom: 0;',
    '  display: flex;',
    '  align-items: center;',
    '  justify-content: center;',
    '  background: #fff;',
    '  z-index: 1;',
    '}',
    '.spinner {',
    '  display: flex;',
    '  gap: 16px;',
    '  align-items: flex-end;',
    '  height: 96px;',
    '}',
    '.spinner-bar {',
    '  width: 16px;',
    '  background: #333;',
    '  border-radius: 6px;',
    '  animation: bars 0.8s ease-in-out infinite;',
    '}',
    '.spinner-bar:nth-child(1) { height: 48px; animation-delay: 0s; }',
    '.spinner-bar:nth-child(2) { height: 72px; animation-delay: 0.15s; }',
    '.spinner-bar:nth-child(3) { height: 56px; animation-delay: 0.3s; }',
    '@keyframes bars {',
    '  0%, 100% { transform: scaleY(0.4); opacity: 0.3; }',
    '  50% { transform: scaleY(1); opacity: 1; }',
    '}'
  ), collapse = "\n")
}


# --- internal: report JS ---
#' @noRd
.bn_report_js <- function(save_name) {

  # membership sync snippet (applied to static HTML membership tab)
  membership_sync <- paste0(
    'var rEdits = legendEdits[rName] || {};\n',
    '        Object.keys(rEdits).forEach(function(color) {\n',
    '          panel.querySelectorAll(\'.community-label[data-color="\' + color + \'"]\').forEach(function(el) {\n',
    '            el.textContent = rEdits[color];\n',
    '          });\n',
    '        });\n',
    '        var nEdits = nodeLabelEdits;\n',
    '        Object.keys(nEdits).forEach(function(nodeId) {\n',
    '          panel.querySelectorAll(\'[data-node-id="\' + nodeId + \'"]\').forEach(function(el) {\n',
    '            el.textContent = nEdits[nodeId];\n',
    '          });\n',
    '        });'
  )

  # save download snippet
  save_download <- paste0(
    'var a = document.createElement("a");\n',
    '      a.href = URL.createObjectURL(blob);\n',
    '      a.download = "', save_name, '.resondex_bn";\n',
    '      a.click();'
  )

  paste(c(
    'function switchType(resultId, panelId) {',
    '  var accordion = document.getElementById(panelId).closest(".result-accordion");',
    '  accordion.querySelectorAll(".type-panel").forEach(function(p) {',
    '    p.style.display = "none";',
    '  });',
    '  var panel = document.getElementById(panelId);',
    '',
    '  var activePanel = panel.querySelector(".tab-panel.active");',
    '  var hasEdits = false;',
    '  if (activePanel) {',
    '    var rName = activePanel.getAttribute("data-result") || "_default";',
    '    var rEdits = legendEdits[rName] || {};',
    '    hasEdits = Object.keys(rEdits).length > 0 || Object.keys(nodeLabelEdits).length > 0;',
    '  }',
    '',
    '  panel.style.display = "block";',
    '',
    '  if (hasEdits) {',
    '    panel.style.opacity = "0";',
    '    requestAnimationFrame(function() {',
    '      var rEdits = legendEdits[rName] || {};',
    '      sendSyncEdits(activePanel, rEdits);',
    '    });',
    '  } else {',
    '    panel.querySelectorAll("iframe").forEach(function(iframe) {',
    '      try { iframe.contentWindow.postMessage({ type: "fitNetwork" }, "*"); } catch(e) {}',
    '    });',
    '  }',
    '',
    '  if (Object.keys(window.pendingLoads).length > 0) {',
    '    panel.querySelectorAll(".tab-panel[data-result]").forEach(function(tp) {',
    '      sendSnapshotToPanel(tp);',
    '    });',
    '  }',
    '}',
    '',
    'function sendSyncEdits(tabPanel, legend) {',
    '  var iframe = tabPanel.querySelector("iframe");',
    '  if (!iframe) return;',
    '  try {',
    '    iframe.contentWindow.postMessage({',
    '      type: "syncEdits",',
    '      legend: legend,',
    '      nodeLabels: nodeLabelEdits',
    '    }, "*");',
    '  } catch(e) {}',
    '  try { iframe.contentWindow.postMessage({ type: "fitNetwork" }, "*"); } catch(e) {}',
    '}',
    '',
    'function switchTab(btn, panelId) {',
    '  var bar = btn.parentElement;',
    '  bar.querySelectorAll(".tab-btn").forEach(function(b) { b.classList.remove("active"); });',
    '  btn.classList.add("active");',
    '  var typePanel = bar.parentElement;',
    '  typePanel.querySelectorAll(".tab-panel").forEach(function(p) { p.classList.remove("active"); });',
    '  document.getElementById(panelId).classList.add("active");',
    '}',
    '',
    'function toggleMembershipView(btn) {',
    '  var wrap = btn.closest(".membership-wrap");',
    '  var tbl = wrap.querySelector(".membership-table-view");',
    '  var crd = wrap.querySelector(".membership-card-view");',
    '  if (tbl.style.display === "none") {',
    '    tbl.style.display = ""; crd.style.display = "none";',
    '  } else {',
    '    tbl.style.display = "none"; crd.style.display = "";',
    '  }',
    '}',
    '',
    'var legendEdits = {};',
    'var nodeLabelEdits = {};',
    '',
    'function findSourceResult(evtSource) {',
    '  var result = { accordion: null, name: null };',
    '  document.querySelectorAll("iframe").forEach(function(f) {',
    '    if (f.contentWindow === evtSource) {',
    '      result.accordion = f.closest(".result-accordion");',
    '      var panel = f.closest(".tab-panel[data-result]");',
    '      if (panel) result.name = panel.getAttribute("data-result");',
    '    }',
    '  });',
    '  return result;',
    '}',
    '',
    'window.addEventListener("message", function(evt) {',
    '  if (!evt.data) return;',
    '  var src = findSourceResult(evt.source);',
    '  var rName = src.name || "_default";',
    '',
    '  if (evt.data.type === "legendUpdate") {',
    '    if (!legendEdits[rName]) legendEdits[rName] = {};',
    '    var legendEditsMap = {};',
    '    evt.data.keyData.forEach(function(item) {',
    '      legendEdits[rName][item.color] = item.label;',
    '      legendEditsMap[item.color] = item.label;',
    '    });',
    '    var scope = src.accordion || document;',
    '    scope.querySelectorAll(".comm-panel iframe").forEach(function(iframe) {',
    '      try { iframe.contentWindow.postMessage({ type: "legendUpdate", edits: legendEditsMap }, "*"); } catch(e) {}',
    '    });',
    '    scope.querySelectorAll(".attr-panel iframe").forEach(function(iframe) {',
    '      if (iframe.contentWindow !== evt.source) {',
    '        try { iframe.contentWindow.postMessage({ type: "nodeUpdate", edits: legendEditsMap }, "*"); } catch(e) {}',
    '      }',
    '    });',
    '  }',
    '  if (evt.data.type === "nodeUpdate") {',
    '    var edits = evt.data.edits || {};',
    '    if (!legendEdits[rName]) legendEdits[rName] = {};',
    '    Object.keys(edits).forEach(function(color) {',
    '      legendEdits[rName][color] = edits[color];',
    '    });',
    '    var scope = src.accordion || document;',
    '    scope.querySelectorAll(".attr-panel iframe").forEach(function(iframe) {',
    '      try { iframe.contentWindow.postMessage({ type: "nodeUpdate", edits: edits }, "*"); } catch(e) {}',
    '    });',
    '    scope.querySelectorAll(".comm-panel iframe").forEach(function(iframe) {',
    '      if (iframe.contentWindow !== evt.source) {',
    '        try { iframe.contentWindow.postMessage({ type: "legendUpdate", edits: edits }, "*"); } catch(e) {}',
    '      }',
    '    });',
    '  }',
    '  if (evt.data.type === "nodeLabelUpdate") {',
    '    nodeLabelEdits[evt.data.nodeId] = evt.data.label;',
    '    document.querySelectorAll(".attr-panel iframe").forEach(function(iframe) {',
    '      if (iframe.contentWindow !== evt.source) {',
    '        try { iframe.contentWindow.postMessage({ type: "nodeLabelUpdate", nodeId: evt.data.nodeId, label: evt.data.label }, "*"); } catch(e) {}',
    '      }',
    '    });',
    '  }',
    '',
    '  if (evt.data.type === "editsSynced") {',
    '    document.querySelectorAll("iframe").forEach(function(f) {',
    '      if (f.contentWindow === evt.source) {',
    '        var typePanel = f.closest(".type-panel");',
    '        if (typePanel) typePanel.style.opacity = "1";',
    '      }',
    '    });',
    '  }',
    '});',
    '',
    'var origSwitchTab = switchTab;',
    'switchTab = function(btn, panelId, skipToggle) {',
    '  if (!skipToggle) origSwitchTab(btn, panelId);',
    '  var panel = document.getElementById(panelId);',
    '  if (!panel) return;',
    '  var rName = panel.getAttribute("data-result") || "_default";',
    '',
    '  if (panel.classList.contains("membership-panel")) {',
    paste0('    ', membership_sync),
    '    return;',
    '  }',
    '',
    '  var rEdits = legendEdits[rName] || {};',
    '  sendSyncEdits(panel, rEdits);',
    '',
    '  if (Object.keys(window.pendingLoads).length > 0) {',
    '    sendSnapshotToPanel(panel);',
    '  }',
    '};',
    '',
    'var snapshotStore = {};',
    'window.pendingLoads = {};',
    '',
    'function dismissSpinner(source) {',
    '  document.querySelectorAll("iframe").forEach(function(f) {',
    '    if (f.contentWindow === source) {',
    '      var overlay = f.parentElement && f.parentElement.querySelector(".spinner-overlay");',
    '      if (overlay) overlay.style.display = "none";',
    '    }',
    '  });',
    '}',
    '',
    'window.addEventListener("message", function(evt) {',
    '  if (!evt.data) return;',
    '  if (evt.data.type === "snapshotPush" && evt.data.nsKey) {',
    '    snapshotStore[evt.data.nsKey] = evt.data.data;',
    '    dismissSpinner(evt.source);',
    '    delete window.pendingLoads[evt.data.nsKey];',
    '  }',
    '  if (evt.data.type === "iframeReady" && evt.data.nsKey) {',
    '    var nsKey = evt.data.nsKey;',
    '    var rName = nsKey.split("|")[0] || "_default";',
    '',
    '    // send pending snapshot load if any',
    '    var data = window.pendingLoads[nsKey];',
    '    if (data) {',
    '      var merged = mergeEditsIntoSnapshot(data, rName);',
    '      try { evt.source.postMessage({ type: "applyReportLoad", snapshot: merged }, "*"); } catch(e) {}',
    '    }',
    '',
    '    // send current legend/node edits so late-loading iframes get them',
    '    var rEdits = legendEdits[rName] || {};',
    '    if (Object.keys(rEdits).length > 0 || Object.keys(nodeLabelEdits).length > 0) {',
    '      try {',
    '        evt.source.postMessage({ type: "syncEdits", legend: rEdits, nodeLabels: nodeLabelEdits }, "*");',
    '      } catch(e) {}',
    '    }',
    '  }',
    '});',
    '',
    'function panelKey(panel) {',
    '  var r = panel.getAttribute("data-result") || "";',
    '  var l = panel.getAttribute("data-layout") || "";',
    '  var v = panel.getAttribute("data-view") || "";',
    '  return r + "|" + l + "|" + v;',
    '}',
    '',
    'function saveAllLayouts() {',
    '  var json = JSON.stringify({ panels: snapshotStore, legendEdits: legendEdits, nodeLabelEdits: nodeLabelEdits }, null, 2);',
    '  var blob = new Blob([json], { type: "application/json" });',
    paste0('  ', save_download),
    '}',
    '',
    'function mergeEditsIntoSnapshot(data, rName) {',
    '  var rEdits = legendEdits[rName] || {};',
    '  if (!data || !data.keyLabels || Object.keys(rEdits).length === 0) return data;',
    '  var copy = JSON.parse(JSON.stringify(data));',
    '  copy.keyLabels.forEach(function(item) {',
    '    if (rEdits[item.color]) item.label = rEdits[item.color];',
    '  });',
    '  return copy;',
    '}',
    '',
    'function sendSnapshotToPanel(panel) {',
    '  var key = panelKey(panel);',
    '  var data = window.pendingLoads[key];',
    '  if (!data) return;',
    '  var iframe = panel.querySelector("iframe");',
    '  if (!iframe) return;',
    '  try {',
    '    var rName = panel.getAttribute("data-result") || "_default";',
    '    var merged = mergeEditsIntoSnapshot(data, rName);',
    '    iframe.contentWindow.postMessage({ type: "applyReportLoad", snapshot: merged }, "*");',
    '  } catch(ex) {}',
    '}',
    '',
    'function loadAllLayouts(input) {',
    '  var file = input.files[0];',
    '  if (!file) return;',
    '  var reader = new FileReader();',
    '  reader.onload = function(e) {',
    '    try {',
    '      var parsed = JSON.parse(e.target.result);',
    '      var savedPanels = parsed.panels || {};',
    '',
    '      if (parsed.legendEdits) {',
    '        Object.keys(parsed.legendEdits).forEach(function(key) {',
    '          legendEdits[key] = parsed.legendEdits[key];',
    '        });',
    '      }',
    '      if (parsed.nodeLabelEdits) {',
    '        Object.keys(parsed.nodeLabelEdits).forEach(function(key) {',
    '          nodeLabelEdits[key] = parsed.nodeLabelEdits[key];',
    '        });',
    '      }',
    '',
    '      Object.keys(savedPanels).forEach(function(nsKey) {',
    '        window.pendingLoads[nsKey] = savedPanels[nsKey];',
    '      });',
    '',
    '      document.querySelectorAll(".tab-panel[data-result]").forEach(function(panel) {',
    '        sendSnapshotToPanel(panel);',
    '      });',
    '',
    '    } catch(err) {',
    '      alert("Invalid layout file.");',
    '    }',
    '  };',
    '  reader.readAsText(file);',
    '  input.value = "";',
    '}',
    '',
    '/* --- lazy srcdoc iframe initialization (self_contained mode) --- */',
    '/* only init iframes in open accordions; defer closed ones until toggled */',
    'document.addEventListener("DOMContentLoaded", function() {',
    '  if (typeof __sharedDepsB64 === "undefined" || !__sharedDepsB64) return;',
    '  var sharedBytes = Uint8Array.from(atob(__sharedDepsB64), function(c) { return c.charCodeAt(0); });',
    '  var sharedDeps = new TextDecoder().decode(sharedBytes);',
    '',
    '  function initIframes(container) {',
    '    container.querySelectorAll(".iframe-wrap[data-widget]").forEach(function(wrap) {',
    '      var wb64 = wrap.getAttribute("data-widget");',
    '      var wBytes = Uint8Array.from(atob(wb64), function(c) { return c.charCodeAt(0); });',
    '      var widgetHtml = new TextDecoder().decode(wBytes);',
    '      var fullHtml = widgetHtml.replace("<!--SHARED_DEPS-->", sharedDeps);',
    '      wrap.querySelector("iframe").srcdoc = fullHtml;',
    '      wrap.removeAttribute("data-widget");',
    '    });',
    '  }',
    '',
    '  // init iframes in accordions that are already open',
    '  document.querySelectorAll("details[open]").forEach(function(d) {',
    '    initIframes(d);',
    '  });',
    '',
    '  // lazy-load: init iframes when a closed accordion is opened',
    '  document.querySelectorAll("details.result-accordion").forEach(function(d) {',
    '    d.addEventListener("toggle", function() {',
    '      if (d.open) initIframes(d);',
    '    });',
    '  });',
    '});'
  ), collapse = "\n")
}


# --- internal: cache shared widget dependency files ---
#' @noRd
.bn_report_cache_deps <- function(widget_html, lib_dir) {
  # extract ordered <script src="widget_N_files/..."> tags
  script_pattern <- '<script[^>]+src="([^"]+_files/[^"]+)"[^>]*>\\s*</script>'
  script_tags <- regmatches(widget_html, gregexpr(script_pattern, widget_html, perl = TRUE))[[1]]
  script_srcs <- sub(script_pattern, "\\1", script_tags, perl = TRUE)

  # extract ordered <link href="widget_N_files/..."> tags
  link_pattern <- '<link[^>]+href="([^"]+_files/[^"]+)"[^>]*>'
  link_tags <- regmatches(widget_html, gregexpr(link_pattern, widget_html, perl = TRUE))[[1]]
  link_hrefs <- sub(link_pattern, "\\1", link_tags, perl = TRUE)

  inline_map <- list()

  for (i in seq_along(link_tags)) {
    file_path <- file.path(dirname(lib_dir), link_hrefs[i])
    if (file.exists(file_path)) {
      content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
      inline_map[[link_tags[i]]] <- paste0("<style>", content, "</style>")
    }
  }

  for (i in seq_along(script_tags)) {
    file_path <- file.path(dirname(lib_dir), script_srcs[i])
    if (file.exists(file_path)) {
      content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
      inline_map[[script_tags[i]]] <- paste0("<script>", content, "</script>")
    }
  }

  if (length(inline_map) == 0) {
    cli::cli_warn("No widget dependencies found to cache. Self-contained inlining may fail.")
  }

  inline_map
}


# --- internal: concatenate cached dep content into shared deps string ---
#' @noRd
.bn_report_shared_deps_string <- function(dep_cache) {
  paste(unlist(dep_cache, use.names = FALSE), collapse = "\n")
}


# --- internal: strip shared dep tags, leaving <!--SHARED_DEPS--> marker ---
#' @noRd
.bn_report_strip_deps <- function(widget_html, dep_cache, widget_lib_prefix, first_lib_prefix) {
  placeholder_inserted <- FALSE
  for (original_tag in names(dep_cache)) {
    this_tag <- gsub(first_lib_prefix, widget_lib_prefix, original_tag, fixed = TRUE)
    if (!placeholder_inserted) {
      widget_html <- sub(this_tag, "<!--SHARED_DEPS-->", widget_html, fixed = TRUE)
      placeholder_inserted <- TRUE
    } else {
      widget_html <- sub(this_tag, "", widget_html, fixed = TRUE)
    }
  }
  widget_html
}


# --- internal: build download filename prefix ---
#' @noRd
.bn_report_download_prefix <- function(title, subtitle, result_name, tab_label) {
  dl_parts <- c(title, subtitle, result_name, tab_label)
  paste(dl_parts[nchar(dl_parts) > 0], collapse = " - ")
}
