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
#'   Default `"Bayesian Network Report"`.
#' @param section_height Character. CSS height per network panel.
#'   Default `"800px"`.
#' @param interactive Logical. Passed through to `bn_visual()`.
#'   Default `TRUE`.
#' @param add_key Logical. If `TRUE`, adds a community color legend to each
#'   network. Default `FALSE` (legend can cause overflow in iframes).
#' @param self_contained Logical. If `TRUE` (default), embeds all widget
#'   JS/CSS into a single HTML file via base64 iframes. If `FALSE`, writes
#'   widget dependencies to a `lib/` folder alongside the HTML file (faster
#'   to generate, but not portable as a single file).
#' @param file Character. Output HTML file path. Default `"bn_report.html"`.
#' @param open Logical. If `TRUE`, opens the file in the browser.
#'   Default `TRUE`.
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
    do_community = c(FALSE, TRUE),
    title = "Network Analysis",
    section_height = "800px",
    interactive = TRUE,
    add_key = FALSE,
    self_contained = TRUE,
    file = "bn_report.html",
    open = TRUE,
    seed = 1
){

  # --- detect input shape and normalize to named list of engine results ---
  results <- .bn_report_normalize_results(results)

  # --- type labels ---
  type_labels <- purrr::map_chr(types, function(type) {
    switch(type,
      none = "None",
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


  # --- helper: render one widget to iframe html ---
  render_widget <- function(result, type, do_community_val) {

    viz <- tryCatch(
      bn_visual(
        obj = result,
        type = type,
        do_community = do_community_val,
        vs_height = "95vh",
        interactive = interactive,
        add_key = add_key,
        save_visuals = FALSE,
        seed = seed
      ),
      error = function(e) NULL
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

    htmlwidgets::saveWidget(
      viz,
      file = widget_file,
      selfcontained = self_contained
    )

    # strip default body margin/padding inside the iframe
    # reposition interactive controls to work inside iframe context
    widget_html <- readLines(widget_file, warn = FALSE) %>% paste(collapse = "\n")
    widget_html <- sub(
      "<head>",
      paste0(
        "<head><style>",
        "body,html{margin:0!important;padding:0!important;height:100%!important;overflow:hidden!important;}",
        " .htmlwidget{height:100%!important;}",
        # move font size button to bottom-left (away from visNetwork select-by-id at top)
        " #fontButton{top:auto!important;bottom:10px!important;left:10px!important;}",
        # keep download buttons at top-right
        " #pngButton{top:10px!important;right:10px!important;left:auto!important;}",
        " #svgButton{top:10px!important;right:160px!important;left:auto!important;}",
        " #saveLayoutButton{top:10px!important;right:300px!important;left:auto!important;}",
        " #loadLayoutButton{top:10px!important;right:440px!important;left:auto!important;}",
        "</style>"
      ),
      widget_html
    )



    if (self_contained) {
      widget_b64 <- base64enc::base64encode(charToRaw(widget_html))

      glue::glue(
        '<iframe src="data:text/html;base64,{widget_b64}" ',
        'style="width: 100%; height: 70vh; border: none;" ',
        'sandbox="allow-scripts allow-same-origin allow-downloads">',
        '</iframe>'
      )
    } else {
      widget_rel <- glue::glue("lib/widget_{widget_counter}.html")
      writeLines(widget_html, widget_file)

      glue::glue(
        '<iframe src="{widget_rel}" ',
        'style="width: 100%; height: 70vh; border: none;" ',
        'sandbox="allow-scripts allow-same-origin allow-downloads">',
        '</iframe>'
      )
    }
  }


  # --- build html sections ---
  # structure: result (accordion) > type (dropdown) > view (tabs)

  result_counter <- 0L

  sections <- purrr::imap(results, function(result, name) {

    result_counter <<- result_counter + 1L
    rid <- glue::glue("r{result_counter}")

    # build dropdown options
    options_html <- purrr::map2_chr(types, type_labels, function(type, label) {
      glue::glue('<option value="{rid}_{type}">{label}</option>')
    })
    options_str <- paste(options_html, collapse = "\n          ")

    # build type panels — each contains tabs (or single view)
    type_panels <- purrr::map2_chr(types, type_labels, function(type, label) {

      panel_id <- glue::glue("{rid}_{type}")
      first_type <- types[1]
      visible <- if (type == first_type) "block" else "none"

      if (has_tabs) {

        tab_attr <- render_widget(result, type, FALSE)
        tab_comm <- render_widget(result, type, TRUE)

        attr_id <- glue::glue("{panel_id}_attr")
        comm_id <- glue::glue("{panel_id}_comm")

        glue::glue(
          '<div id="{panel_id}" class="type-panel" style="display: {visible};">',
          '  <div class="tab-bar">',
          '    <button class="tab-btn active" onclick="switchTab(this, \'{attr_id}\')">Attribute</button>',
          '    <button class="tab-btn" onclick="switchTab(this, \'{comm_id}\')">Community</button>',
          '  </div>',
          '  <div id="{attr_id}" class="tab-panel active">{tab_attr}</div>',
          '  <div id="{comm_id}" class="tab-panel">{tab_comm}</div>',
          '</div>'
        )

      } else {

        panel_content <- render_widget(result, type, do_community[1])

        glue::glue(
          '<div id="{panel_id}" class="type-panel" style="display: {visible};">',
          '  <div style="padding: 12px;">{panel_content}</div>',
          '</div>'
        )
      }
    })

    type_panels_str <- paste(type_panels, collapse = "\n")

    glue::glue(
      '<details class="result-accordion" open>',
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

  full_html <- glue::glue('
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>{title}</title>
  <style>
    body {{
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      margin: 20px 40px;
      background: #fafafa;
    }}
    h1 {{
      border-bottom: 2px solid #333;
      padding-bottom: 10px;
    }}

    /* accordion */
    .result-accordion {{
      margin: 20px 0;
      border: 1px solid #ddd;
      border-radius: 8px;
      background: #fff;
      overflow: hidden;
    }}
    .result-accordion summary {{
      padding: 14px 20px;
      font-size: 18px;
      font-weight: 700;
      color: #333;
      cursor: pointer;
      user-select: none;
      background: #f7f7f7;
      border-bottom: 1px solid #eee;
    }}
    .result-accordion summary:hover {{
      background: #f0f0f0;
    }}
    .accordion-body {{
      padding: 0;
    }}

    /* dropdown controls */
    .controls-bar {{
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 12px 20px;
      background: #fafafa;
      border-bottom: 1px solid #eee;
    }}
    .controls-bar label {{
      font-size: 14px;
      font-weight: 600;
      color: #555;
    }}
    .controls-bar select {{
      padding: 6px 12px;
      font-size: 14px;
      border: 1px solid #ccc;
      border-radius: 4px;
      background: #fff;
      color: #333;
      cursor: pointer;
    }}

    /* type panels */
    .type-panel {{
      padding: 0;
    }}

    /* tabs */
    .tab-bar {{
      display: flex;
      border-bottom: 2px solid #ddd;
      background: #f9f9f9;
    }}
    .tab-btn {{
      padding: 10px 24px;
      border: none;
      background: transparent;
      font-size: 14px;
      font-weight: 500;
      color: #888;
      cursor: pointer;
      border-bottom: 2px solid transparent;
      margin-bottom: -2px;
      transition: color 0.15s, border-color 0.15s;
    }}
    .tab-btn:hover {{
      color: #444;
    }}
    .tab-btn.active {{
      color: #222;
      border-bottom-color: #333;
    }}
    .tab-panel {{
      display: none;
      padding: 0;
    }}
    .tab-panel iframe {{
      display: block;
    }}
    .tab-panel.active {{
      display: block;
    }}
  </style>
</head>
<body>
  <h1>{title}</h1>
  {html_body}

  <script>
    function switchType(resultId, panelId) {{
      // hide all type panels within this result
      var accordion = document.getElementById(panelId).closest(".result-accordion");
      accordion.querySelectorAll(".type-panel").forEach(function(p) {{
        p.style.display = "none";
      }});
      // show selected
      document.getElementById(panelId).style.display = "block";
    }}

    function switchTab(btn, panelId) {{
      // deactivate sibling buttons
      var bar = btn.parentElement;
      bar.querySelectorAll(".tab-btn").forEach(function(b) {{ b.classList.remove("active"); }});
      btn.classList.add("active");

      // hide sibling panels, show target
      var typePanel = bar.parentElement;
      typePanel.querySelectorAll(".tab-panel").forEach(function(p) {{ p.classList.remove("active"); }});
      document.getElementById(panelId).classList.add("active");
    }}
  </script>
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

  # bn_initial_networks output: has $summary at top level + named engine results
  # detect by checking if any child has $meta
  children_have_meta <- purrr::map_lgl(results, function(x) {
    is.list(x) && !is.null(x[["meta"]])
  })

  if (any(children_have_meta)) {
    # keep only the engine result children (drop $summary etc.)
    results <- results[children_have_meta]

    # ensure names
    if (is.null(names(results))) {
      names(results) <- paste("Network", seq_along(results))
    }

    return(results)
  }

  # already a named list of engine results
  if (is.null(names(results))) {
    names(results) <- paste("Network", seq_along(results))
  }

  results
}
