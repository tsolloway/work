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
    seed = 1,
    add_additional_results = FALSE
){

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

        # Optional extra tabs: Attribute Impacts, Community Impacts, Prioritization
        extras_buttons <- character(0)
        extras_panels  <- character(0)

        if (isTRUE(add_additional_results)) {
          impacts_res         <- result[["impacts"]]
          prioritizations_res <- result[["prioritizations"]]

          if (!is.null(impacts_res) && !is.null(impacts_res[["table_attribute"]])) {
            impact_attr_id <- glue::glue("{panel_id}_impact_attr")
            impact_attr_html <- .bn_report_render_attribute_impacts_dashboard(
              impacts_res, result_name = name, dashboard_id = impact_attr_id
            )
            extras_buttons <- c(extras_buttons, glue::glue(
              '    <button class="tab-btn" onclick="switchTab(this, \'{impact_attr_id}\')">Attribute Impacts</button>'
            ))
            extras_panels <- c(extras_panels, glue::glue(
              '  <div id="{impact_attr_id}" class="tab-panel impact-panel" data-result="{name}" data-layout="{type}" data-view="impact_attr">{impact_attr_html}</div>'
            ))
          }

          if (!is.null(impacts_res) && !is.null(impacts_res[["table_community"]])) {
            impact_comm_id <- glue::glue("{panel_id}_impact_comm")
            impact_comm_html <- .bn_report_render_attribute_impacts_dashboard(
              impacts_res, result_name = name, dashboard_id = impact_comm_id,
              is_community = TRUE
            )
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
              marginal_threshold = priort_meta[["marginal_threshold"]] %||% 0.10
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


# --- internal: full Impacts dashboard (HTML + inline JS) -------------------
# Mirrors the bn_impact_write dynamic dashboard: Metric, Focus, and Weight
# dropdowns; one Index column per subgroup; conditional formatting (green/
# yellow/red color scale on index, bold-italic for negative raw metric,
# blackout for p > 0.1, red warning next to Focus when base is below the
# minimum). Total Impact + Base rows recompute as the dropdowns change.
# Works for both attribute-level and community-level impact tables —
# pass is_community = TRUE for the latter (no Variable/Label columns; the
# leading column is "Community" instead).
#' @noRd
.bn_report_render_attribute_impacts_dashboard <- function(
    impacts, result_name, dashboard_id, is_community = FALSE
) {
  if (isTRUE(is_community)) {
    tbl   <- impacts[["table_community"]]
    tbl_w <- impacts[["table_community_weighted"]]
    id_col_name <- "Community"
    id_col_label <- "Community"
  } else {
    tbl   <- impacts[["table_attribute"]]
    tbl_w <- impacts[["table_attribute_weighted"]]
    id_col_name <- "Variable"
    id_col_label <- "Variable"
  }

  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
    return('<div class="extra-empty">No impact results.</div>')
  }

  meta <- impacts[["meta"]] %||% list()
  has_weights <- !is.null(tbl_w)
  min_base_for_lift <- meta[["min_base_for_lift"]] %||% 75L

  # --- Parse dimensions from column names (mirrors append_bn_impact_dynamic)
  all_cols <- names(tbl)
  sgs <- meta[["subgroups"]]
  if (is.null(sgs) || length(sgs) == 0) sgs <- "Total"
  sgs <- sgs[vapply(sgs, function(sg) {
    any(startsWith(all_cols, paste0(sg, "_")))
  }, logical(1))]
  if (length(sgs) == 0) sgs <- "Total"

  sg1 <- sgs[1]
  sg1_cols <- all_cols[startsWith(all_cols, paste0(sg1, "_"))]
  metric_suffixes <- sub(paste0("^", sg1, "_"), "", sg1_cols)

  all_lift_suffixes    <- grep("^lift", metric_suffixes, value = TRUE)
  market_lift_suffixes <- grep("^lift$|^lift_\\d+$", all_lift_suffixes, value = TRUE)
  brand_lift_suffixes  <- setdiff(all_lift_suffixes, market_lift_suffixes)

  brand_names <- if (length(brand_lift_suffixes) > 0) {
    unique(sub("^lift_\\d+_|^lift_", "", brand_lift_suffixes))
  } else character(0)
  focus_options <- c("Market", brand_names)

  metric_info <- list()
  for (ml in market_lift_suffixes) {
    if (ml %in% c("lift", "lift_0")) {
      metric_info[[length(metric_info) + 1]] <- list(label = "Average Lift", key = ml)
    } else {
      pct <- sub("lift_", "", ml)
      metric_info[[length(metric_info) + 1]] <- list(
        label = paste0(pct, "% Lift"), key = ml
      )
    }
  }
  if ("maxVmin" %in% metric_suffixes) {
    metric_info[[length(metric_info) + 1]] <- list(label = "Max vs Min", key = "maxVmin")
  }
  if ("mi" %in% metric_suffixes) {
    metric_info[[length(metric_info) + 1]] <- list(label = "Mutual Information", key = "mi")
  }

  # In community mode, Community IS the id column, so no secondary Community
  # column; there's also no Label.
  has_community <- (!isTRUE(is_community)) && ("Community" %in% names(tbl))
  has_label     <- (!isTRUE(is_community)) && ("Label"     %in% names(tbl))

  # --- Flatten one table (unweighted or weighted) into per-row JSON lists
  .flatten <- function(tt) {
    lapply(seq_len(nrow(tt)), function(i) {
      row <- list(
        id        = as.character(tt[[id_col_name]][i]),
        community = if (has_community) as.character(tt$Community[i]) else NULL,
        label     = if (has_label)     as.character(tt$Label[i])     else NULL,
        sg        = list()
      )
      for (sg in sgs) {
        sg_data <- list()
        sg_cols <- all_cols[startsWith(all_cols, paste0(sg, "_"))]
        for (col in sg_cols) {
          suf <- sub(paste0("^", sg, "_"), "", col)
          v <- tt[[col]][i]
          sg_data[[suf]] <- if (is.numeric(v) && is.finite(v)) as.numeric(v) else NA
        }
        row$sg[[sg]] <- sg_data
      }
      row
    })
  }

  data_obj <- list(
    subgroups         = as.list(sgs),
    focuses           = as.list(focus_options),
    metrics           = metric_info,
    has_weights       = has_weights,
    has_community     = has_community,
    has_label         = has_label,
    min_base_for_lift = as.integer(min_base_for_lift),
    rows_unweighted   = .flatten(tbl),
    rows_weighted     = if (has_weights) .flatten(tbl_w) else NULL
  )

  data_json <- jsonlite::toJSON(data_obj, auto_unbox = TRUE, null = "null", na = "null")

  # --- HTML scaffold
  # Controls row — Focus always shown. Metric always shown. Weight only if
  # weighted data is available. Subgroup is rendered as columns (not a dropdown).
  focus_options_html <- paste0(
    vapply(focus_options, function(f) {
      sprintf('<option value="%s">%s</option>',
        htmltools::htmlEscape(f), htmltools::htmlEscape(f))
    }, character(1)),
    collapse = "\n"
  )
  metric_options_html <- paste0(
    vapply(metric_info, function(m) {
      sprintf('<option value="%s">%s</option>',
        htmltools::htmlEscape(m$key), htmltools::htmlEscape(m$label))
    }, character(1)),
    collapse = "\n"
  )
  weight_options_html <- '<option value="Unweighted">Unweighted</option><option value="Weighted">Weighted</option>'

  weight_ctrl <- if (has_weights) {
    sprintf(paste0(
      '<label>Weight:</label>',
      '<select class="impact-ctrl" data-dim="weight">%s</select>',
      '<span class="impact-warning" data-for="weight"></span>'
    ), weight_options_html)
  } else ""

  # Header row: leading cols (sortable, text) + one metric column per
  # subgroup (sortable, numeric). Subgroup label "_" -> " " for display.
  leading_headers <- c(
    sprintf('<th class="sortable" data-sort="text" data-col="id">%s</th>',
      htmltools::htmlEscape(id_col_label)),
    if (has_community) '<th class="sortable" data-sort="text" data-col="community">Community</th>' else NULL,
    if (has_label)     '<th class="sortable" data-sort="text" data-col="label">Label</th>'         else NULL
  )
  subgroup_headers <- vapply(sgs, function(sg) {
    sprintf(
      '<th class="sg-col sortable metric-col" data-sort="num" data-sg="%s">%s</th>',
      htmltools::htmlEscape(sg),
      htmltools::htmlEscape(gsub("_", " ", sg, fixed = TRUE))
    )
  }, character(1))
  header_row <- paste0("<tr>",
    paste(c(leading_headers, subgroup_headers), collapse = ""),
    "</tr>")

  # Body row template — one <tr> per row; index cells populated by JS.
  body_rows <- vapply(seq_along(data_obj$rows_unweighted), function(i) {
    row <- data_obj$rows_unweighted[[i]]
    leading_cells <- c(
      sprintf('<td class="txt-col">%s</td>', htmltools::htmlEscape(row$id)),
      if (has_community) sprintf('<td class="txt-col">%s</td>',
        htmltools::htmlEscape(row$community %||% "")) else NULL,
      if (has_label)     sprintf('<td class="txt-col">%s</td>',
        htmltools::htmlEscape(row$label %||% "")) else NULL
    )
    sg_cells <- vapply(sgs, function(sg) {
      sprintf('<td class="idx-cell num-col" data-sg="%s" data-row="%d"></td>',
        htmltools::htmlEscape(sg), i - 1L)
    }, character(1))
    paste0("<tr>", paste(c(leading_cells, sg_cells), collapse = ""), "</tr>")
  }, character(1))

  # Footer rows: Total Impact + Base
  ti_cells <- vapply(sgs, function(sg) {
    sprintf('<td class="ti-cell num-col" data-sg="%s"></td>',
      htmltools::htmlEscape(sg))
  }, character(1))
  base_cells <- vapply(sgs, function(sg) {
    sprintf('<td class="base-cell num-col" data-sg="%s"></td>',
      htmltools::htmlEscape(sg))
  }, character(1))
  n_leading <- length(leading_headers)

  total_row <- paste0(
    '<tr class="ti-row"><td class="txt-col" colspan="', n_leading, '">Total Impact</td>',
    paste(ti_cells, collapse = ""), '</tr>'
  )
  base_row <- paste0(
    '<tr class="base-row"><td class="txt-col" colspan="', n_leading, '">Base</td>',
    paste(base_cells, collapse = ""), '</tr>'
  )

  # Compose
  paste0(
    '<div class="impact-dashboard" data-dashboard-id="', dashboard_id, '">',
    '  <div class="impact-controls">',
    '    <label>Metric:</label>',
    '    <select class="impact-ctrl" data-dim="metric">', metric_options_html, '</select>',
    '    <label>Focus:</label>',
    '    <select class="impact-ctrl" data-dim="focus">', focus_options_html, '</select>',
    '    <span class="impact-warning" data-for="focus"></span>',
    '    ', weight_ctrl,
    '  </div>',
    '  <div class="impact-table-wrap">',
    '    <table class="impact-table">',
    '      <thead>', header_row, '</thead>',
    '      <tbody>', paste(body_rows, collapse = ""), '</tbody>',
    '      <tfoot>', total_row, base_row, '</tfoot>',
    '    </table>',
    '  </div>',
    '  <div class="impact-footer">',
    '    <p class="index-note"></p>',
    '    <p class="muted">Bold italicized index means a negative relationship. ',
    'Black cells mean an insignificant relationship (p &gt; 0.10). ',
    'Lift impacts are not calculated when the base is below ', min_base_for_lift, '.</p>',
    '  </div>',
    '  <script type="application/json" class="impact-data">', data_json, '</script>',
    '  <script>(function(){ initImpactDashboard("', dashboard_id, '"); })();</script>',
    '</div>'
  )
}


# --- internal: render a bn_impacts table as an HTML table ----------------
# Consistent with bn_write's dashboard: grey header fill, bold, centered
# numerics, color-coded p-values (green < 0.05, yellow < 0.10), "Index"
# column bolded. Returns an HTML string.
#' @noRd
.bn_report_render_impacts_table <- function(tbl, is_community = FALSE) {
  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
    return('<div class="extra-empty">No impact results to display.</div>')
  }

  cols <- names(tbl)

  # Order columns consistently: variable, label, community, then metric groups
  first_cols <- intersect(c("Variable", "Community", "Label"), cols)
  metric_cols <- setdiff(cols, first_cols)
  ordered_cols <- c(first_cols, metric_cols)
  tbl <- tbl[, ordered_cols, drop = FALSE]

  # Detect p-value columns (for coloring) — anything ending in _p_val
  pval_cols <- grep("_p_val$|^p_val$", names(tbl), value = TRUE)
  # Detect index columns — anything ending in _index or a bare "index"
  index_cols <- grep("_index$|^index$|^Index$", names(tbl), value = TRUE)
  # Numeric columns (for centering / formatting)
  num_cols <- names(tbl)[vapply(tbl, is.numeric, logical(1))]

  .fmt_cell <- function(col, val) {
    # Flatten list-column entries and normalize to a single scalar for
    # formatting. Some impact tables carry list columns (e.g., bootstrap
    # arrays) that would otherwise trip up is.na() / as.numeric().
    if (is.list(val)) val <- unlist(val, use.names = FALSE)
    if (length(val) == 0) return("")
    if (length(val) > 1) val <- paste(format(val), collapse = ", ")
    if (is.na(val)) return("")
    if (col %in% pval_cols) {
      pv <- suppressWarnings(as.numeric(val))
      if (!is.finite(pv)) return("")
      cls <- if (pv < 0.05) "p-sig" else if (pv < 0.10) "p-marg" else "p-nonsig"
      sprintf('<span class="%s">%s</span>', cls, formatC(pv, format = "f", digits = 3))
    } else if (col %in% index_cols) {
      num <- suppressWarnings(as.numeric(val))
      if (!is.finite(num)) return("")
      sprintf('<strong>%s</strong>', formatC(num, format = "d"))
    } else if (col %in% num_cols) {
      num <- suppressWarnings(as.numeric(val))
      if (!is.finite(num)) return("")
      if (abs(num) < 1) formatC(num, format = "f", digits = 3)
      else formatC(num, format = "f", digits = 2)
    } else {
      htmltools::htmlEscape(as.character(val))
    }
  }

  .col_class <- function(col) {
    if (col %in% num_cols) "num-col" else "txt-col"
  }

  header_html <- paste0(
    "<tr>",
    paste(
      vapply(names(tbl), function(c) {
        sprintf('<th class="%s">%s</th>', .col_class(c), htmltools::htmlEscape(c))
      }, character(1)),
      collapse = ""
    ),
    "</tr>"
  )

  body_rows <- vapply(seq_len(nrow(tbl)), function(i) {
    cells <- vapply(names(tbl), function(c) {
      sprintf('<td class="%s">%s</td>', .col_class(c), .fmt_cell(c, tbl[[c]][[i]]))
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))

  paste0(
    '<div class="extra-wrap">',
    '<table class="extra-table">',
    '<thead>', header_html, '</thead>',
    '<tbody>', paste(body_rows, collapse = ""), '</tbody>',
    '</table>',
    '</div>'
  )
}


# --- internal: full Prioritization dashboard (HTML + inline JS) ------------
# Mirrors the bn_prioritize_write dynamic dashboard: Strategy, Search,
# Subgroup, Focus, Weight dropdowns (only those with multiple values are
# shown); one row per priority step; conditional formatting on p-values
# (green < sig_threshold, orange < marginal_threshold, blackout otherwise)
# and bold-italic for negative marginal gain; Base display + warning next
# to the Focus dropdown.
#' @noRd
.bn_report_render_prioritization_dashboard <- function(
    priort, result_name, dashboard_id,
    sig_threshold = 0.05, marginal_threshold = 0.10
) {
  registry <- tryCatch(.prioritize_build_registry(priort),
    error = function(e) list())
  if (length(registry) == 0) {
    return('<div class="extra-empty">No prioritization results.</div>')
  }

  meta <- priort[["meta"]] %||% list()
  min_base_for_boot <- meta[["min_base_for_boot"]] %||% 100L
  lift_pct <- meta[["lift"]] %||% 0.10

  # --- Collect unique dimension values in a stable order
  strategies <- unique(vapply(registry, function(e) e$strategy %||% "", character(1)))
  searches   <- unique(vapply(registry, function(e) e$search   %||% "", character(1)))
  subgroups  <- unique(vapply(registry, function(e) e$subgroup %||% "", character(1)))
  focuses    <- unique(vapply(registry, function(e) e$focus    %||% "", character(1)))
  weights    <- unique(vapply(registry, function(e) e$weight   %||% "", character(1)))

  # as.list() on each vector prevents jsonlite::toJSON(auto_unbox = TRUE)
  # from collapsing single-value dimensions to scalar strings (which would
  # break data.dims[dim][0] lookups in JS).
  dims <- list(
    strategy = as.list(strategies),
    search   = as.list(searches),
    subgroup = as.list(subgroups),
    focus    = as.list(focuses),
    weight   = as.list(weights)
  )
  # A control is "active" if it has > 1 unique value (show the dropdown)
  active_dims <- names(dims)[vapply(dims, length, integer(1)) > 1]

  # --- Build lookup: key = "strategy|search|subgroup|focus|weight" -> data
  lookup <- list()
  for (e in registry) {
    key <- paste(e$strategy, e$search, e$subgroup, e$focus, e$weight, sep = "|")
    tbl <- e$tbl
    rows <- if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
      list()
    } else {
      lapply(seq_len(nrow(tbl)), function(i) {
        list(
          priority          = if ("priority"          %in% names(tbl)) as.integer(tbl$priority[i])          else i - 1L,
          variable          = if ("variable"          %in% names(tbl)) as.character(tbl$variable[i])        else NA_character_,
          community         = if ("community"         %in% names(tbl)) as.character(tbl$community[i])       else NULL,
          label             = if ("label"             %in% names(tbl)) as.character(tbl$label[i])           else NULL,
          dv_estimate       = if ("dv_estimate"       %in% names(tbl)) as.numeric(tbl$dv_estimate[i])       else NA_real_,
          marginal_gain     = if ("marginal_gain"     %in% names(tbl)) as.numeric(tbl$marginal_gain[i])     else NA_real_,
          marginal_gain_pct = if ("marginal_gain_pct" %in% names(tbl)) as.numeric(tbl$marginal_gain_pct[i]) else NA_real_,
          p_value           = if ("p_value"           %in% names(tbl)) as.numeric(tbl$p_value[i])           else NA_real_
        )
      })
    }
    lookup[[key]] <- list(
      rows  = rows,
      n_obs = if (is.null(e$n_obs)) NA_integer_ else as.integer(e$n_obs)
    )
  }

  # Flag presence of optional columns across any tibble
  has_community <- any(vapply(lookup, function(x) {
    length(x$rows) > 0 && !is.null(x$rows[[1]]$community)
  }, logical(1)))
  has_label <- any(vapply(lookup, function(x) {
    length(x$rows) > 0 && !is.null(x$rows[[1]]$label)
  }, logical(1)))
  # Check ANY row (not just row 1) — the baseline / priority-0 row has
  # p_value = NA by design even when bootstrap p-values were computed.
  has_p <- any(vapply(lookup, function(x) {
    if (length(x$rows) == 0) return(FALSE)
    any(vapply(x$rows, function(r) {
      !is.null(r$p_value) && !is.na(r$p_value)
    }, logical(1)))
  }, logical(1)))

  data_obj <- list(
    dims              = dims,
    active_dims       = as.list(active_dims),
    has_community     = has_community,
    has_label         = has_label,
    has_p             = has_p,
    sig_threshold     = sig_threshold,
    marginal_threshold = marginal_threshold,
    min_base_for_boot = as.integer(min_base_for_boot),
    lift              = lift_pct,
    lookup            = lookup
  )

  data_json <- jsonlite::toJSON(data_obj, auto_unbox = TRUE, null = "null", na = "null")

  # --- HTML scaffold
  dim_labels <- c(
    strategy = "Strategy:",
    search   = "Search:",
    subgroup = "Subgroup:",
    focus    = "Focus:",
    weight   = "Weight:"
  )

  controls <- character(0)
  for (dn in names(dims)) {
    if (!(dn %in% active_dims)) next
    opts <- dims[[dn]]
    opts_html <- paste(
      vapply(opts, function(o) {
        # Keep `value` as the raw token (so the lookup key matches the
        # registry); pretty-print the visible label by replacing
        # underscores with spaces — e.g., "Regular_Google_User" displays
        # as "Regular Google User".
        display <- if (dn == "subgroup") gsub("_", " ", o, fixed = TRUE) else o
        sprintf('<option value="%s">%s</option>',
          htmltools::htmlEscape(o), htmltools::htmlEscape(display))
      }, character(1)),
      collapse = ""
    )
    controls <- c(controls,
      sprintf(
        '<label>%s</label><select class="priort-ctrl" data-dim="%s">%s</select>',
        htmltools::htmlEscape(dim_labels[[dn]]), dn, opts_html
      )
    )
    if (dn == "focus") {
      controls <- c(controls,
        '<span class="priort-warning" data-for="focus"></span>')
    }
  }
  controls_html <- paste(controls, collapse = "")

  # Header columns (same as Excel dashboard)
  headers <- c(
    '<th class="sortable" data-sort="num" data-col="priority">Step</th>',
    '<th class="sortable" data-sort="text" data-col="variable">Variable</th>',
    if (has_community) '<th class="sortable" data-sort="text" data-col="community">Community</th>' else NULL,
    if (has_label)     '<th class="sortable" data-sort="text" data-col="label">Label</th>'         else NULL,
    '<th class="sortable" data-sort="num" data-col="dv_estimate">DV Estimate</th>',
    '<th class="sortable" data-sort="num" data-col="marginal_gain">Marginal Gain</th>',
    '<th class="sortable" data-sort="num" data-col="marginal_gain_pct">Marginal Gain %</th>',
    if (has_p)         '<th class="sortable" data-sort="num" data-col="p_value">p-value</th>'     else NULL
  )
  header_row <- paste0("<tr>", paste(headers, collapse = ""), "</tr>")

  paste0(
    '<div class="priort-dashboard" data-dashboard-id="', dashboard_id, '">',
    '  <div class="priort-controls">', controls_html, '</div>',
    '  <div class="priort-split">',
    '    <div class="priort-table-wrap">',
    '      <table class="priort-table">',
    '        <thead>', header_row, '</thead>',
    '        <tbody></tbody>',
    '      </table>',
    '    </div>',
    '    <div class="priort-chart-wrap">',
    '      <svg class="priort-chart" xmlns="http://www.w3.org/2000/svg"></svg>',
    '      <div class="priort-tooltip" style="display:none;"></div>',
    '    </div>',
    '  </div>',
    '  <div class="priort-footer">',
    '    <p class="priort-footer-base"></p>',
    '    <p class="muted">Bold italicized numbers indicate a negative relationship. ',
    'Green p-values are significant (&lt; ', sig_threshold, '); ',
    'orange are marginal (&lt; ', marginal_threshold, '); ',
    'red are insignificant. ',
    'Lift prioritization uses a ', round(lift_pct * 100, 1), '% distribution shift.</p>',
    '  </div>',
    '  <script type="application/json" class="priort-data">', data_json, '</script>',
    '  <script>(function(){ initPriortDashboard("', dashboard_id, '"); })();</script>',
    '</div>'
  )
}


# --- internal: render a bn_prioritizations result as HTML tables ---------
# Legacy helper kept for backward compat; superseded by the dashboard above.
#' @noRd
.bn_report_render_prioritization <- function(priort) {
  tbl_or_list <- priort[["greedy_lift"]] %||% priort[["greedy_max"]]
  if (is.null(tbl_or_list)) {
    return('<div class="extra-empty">No prioritization results to display.</div>')
  }

  .render_priort_table <- function(tbl) {
    if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
      return('<div class="extra-empty">No data.</div>')
    }

    # Preferred display columns + order
    display_cols <- intersect(
      c("priority", "variable", "community", "label",
        "dv_estimate", "marginal_gain", "marginal_gain_pct", "p_value"),
      names(tbl)
    )
    tbl <- tbl[, display_cols, drop = FALSE]

    # Column labels
    col_labels <- c(
      priority = "Step",
      variable = "Variable",
      community = "Community",
      label = "Label",
      dv_estimate = "DV Estimate",
      marginal_gain = "Marginal Gain",
      marginal_gain_pct = "Marginal Gain %",
      p_value = "p-value"
    )

    # Cell formatter — robust to list columns / zero-length / multi-element
    .fmt <- function(col, val) {
      if (is.list(val)) val <- unlist(val, use.names = FALSE)
      if (length(val) == 0) return("")
      if (length(val) > 1) val <- paste(format(val), collapse = ", ")
      if (is.na(val)) return("")
      if (col == "p_value") {
        pv <- suppressWarnings(as.numeric(val))
        if (!is.finite(pv)) return("")
        cls <- if (pv < 0.05) "p-sig" else if (pv < 0.10) "p-marg" else "p-nonsig"
        sprintf('<span class="%s">%s</span>', cls, formatC(pv, format = "f", digits = 3))
      } else if (col == "priority") {
        formatC(as.integer(val))
      } else if (col %in% c("marginal_gain_pct")) {
        sprintf("%.2f%%", as.numeric(val) * 100)
      } else if (col %in% c("dv_estimate", "marginal_gain")) {
        formatC(as.numeric(val), format = "f", digits = 3)
      } else {
        htmltools::htmlEscape(as.character(val))
      }
    }

    .cls <- function(col) {
      if (col %in% c("variable", "community", "label")) "txt-col" else "num-col"
    }

    header_html <- paste0(
      "<tr>",
      paste(
        vapply(display_cols, function(c) {
          lbl <- col_labels[[c]] %||% c
          sprintf('<th class="%s">%s</th>', .cls(c), htmltools::htmlEscape(lbl))
        }, character(1)),
        collapse = ""
      ),
      "</tr>"
    )

    body_rows <- vapply(seq_len(nrow(tbl)), function(i) {
      cells <- vapply(display_cols, function(c) {
        sprintf('<td class="%s">%s</td>', .cls(c), .fmt(c, tbl[[c]][[i]]))
      }, character(1))
      paste0("<tr>", paste(cells, collapse = ""), "</tr>")
    }, character(1))

    paste0(
      '<table class="extra-table">',
      '<thead>', header_html, '</thead>',
      '<tbody>', paste(body_rows, collapse = ""), '</tbody>',
      '</table>'
    )
  }

  if (is.data.frame(tbl_or_list)) {
    html <- .render_priort_table(tbl_or_list)
    return(paste0('<div class="extra-wrap">', html, '</div>'))
  }

  # Named list: one table per subgroup
  sections <- vapply(names(tbl_or_list), function(sg_name) {
    paste0(
      '<div class="extra-section">',
      '<h4 class="extra-section-title">', htmltools::htmlEscape(sg_name), '</h4>',
      .render_priort_table(tbl_or_list[[sg_name]]),
      '</div>'
    )
  }, character(1))

  paste0(
    '<div class="extra-wrap">',
    paste(sections, collapse = ""),
    '</div>'
  )
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
    '/* extra tabs (impacts / prioritization) — styling matches bn_write */',
    '.extra-wrap { padding: 20px; overflow-x: auto; }',
    '.extra-empty { padding: 40px; text-align: center; color: #999; }',
    '.extra-section { margin-bottom: 24px; }',
    '.extra-section-title {',
    '  margin: 0 0 8px 0;',
    '  font-size: 16px;',
    '  font-weight: 600;',
    '  color: #333;',
    '}',
    '.extra-table {',
    '  width: 100%;',
    '  border-collapse: collapse;',
    '  font-size: 13px;',
    '  background: #fff;',
    '}',
    '.extra-table thead th {',
    '  background: #D9D9D9;',
    '  color: #222;',
    '  font-weight: 700;',
    '  padding: 8px 10px;',
    '  text-align: center;',
    '  border: 1px solid #BFBFBF;',
    '}',
    '.extra-table tbody td {',
    '  padding: 6px 10px;',
    '  border: 1px solid #e0e0e0;',
    '  vertical-align: middle;',
    '}',
    '.extra-table tbody td.num-col { text-align: center; }',
    '.extra-table tbody td.txt-col { text-align: left; }',
    '.extra-table tbody tr:hover { background: #f8f8f8; }',
    '.extra-table .p-sig { color: #2E7D32; font-weight: 700; }',
    '.extra-table .p-marg { color: #E65100; }',
    '.extra-table .p-nonsig { color: #777; }',
    '',
    '/* Attribute Impacts dashboard (mirrors bn_impact_write dynamic) */',
    '.impact-dashboard { padding: 20px; overflow-x: auto; }',
    '.impact-controls {',
    '  display: flex; flex-wrap: wrap; align-items: center; gap: 10px;',
    '  margin-bottom: 16px; padding: 12px;',
    '  background: #fafafa; border: 1px solid #e0e0e0; border-radius: 6px;',
    '}',
    '.impact-controls label {',
    '  font-weight: 600; color: #333; font-size: 13px;',
    '}',
    '.impact-ctrl {',
    '  padding: 4px 8px; font-size: 13px; width: 180px;',
    '  border: 1px solid #bbb; border-radius: 3px; background: #fff;',
    '  transition: background 0.15s, color 0.15s, border-color 0.15s;',
    '}',
    '.impact-ctrl.warn {',
    '  background: #FF0000; color: #FFFFFF; border-color: #FF0000;',
    '}',
    '.impact-warning {',
    '  color: #FF0000; font-weight: 700; font-size: 13px;',
    '}',
    '.impact-table-wrap { overflow-x: auto; }',
    '.impact-table {',
    '  width: 100%; border-collapse: collapse; font-size: 13px;',
    '  background: #fff; table-layout: auto;',
    '}',
    '.impact-table thead th {',
    '  background: #D9D9D9; color: #222; font-weight: 700;',
    '  padding: 8px 10px; text-align: center; vertical-align: middle;',
    '  border: 1px solid #BFBFBF;',
    '  white-space: normal; word-wrap: break-word; overflow-wrap: break-word;',
    '  position: relative;',
    '}',
    '.col-resize-handle {',
    '  position: absolute; top: 0; right: 0;',
    '  width: 6px; height: 100%;',
    '  cursor: col-resize; user-select: none; z-index: 1;',
    '}',
    '.col-resize-handle:hover { background: rgba(0,0,0,0.15); }',
    '.impact-table.resizing { cursor: col-resize; user-select: none; }',
    '.impact-table.resizing * { cursor: col-resize !important; user-select: none !important; }',
    '.impact-table thead th.metric-col {',
    '  width: 100px; min-width: 100px; max-width: 100px;',
    '}',
    '.impact-table thead th.sortable {',
    '  cursor: pointer; user-select: none; position: relative;',
    '  padding-right: 18px;',  # room for the absolutely-positioned triangle
    '}',
    '.impact-table thead th.sortable:hover { background: #CCCCCC; }',
    '.impact-table thead th.sortable::after {',
    '  content: ""; position: absolute; right: 4px; top: 50%;',
    '  transform: translateY(-50%);',
    '  border: 4px solid transparent; opacity: 0.3;',
    '}',
    '.impact-table thead th.sortable.sorted-asc::after {',
    '  border-bottom-color: #333; border-top: 0; opacity: 1;',
    '  transform: translateY(-50%) translateY(-2px);',
    '}',
    '.impact-table thead th.sortable.sorted-desc::after {',
    '  border-top-color: #333; border-bottom: 0; opacity: 1;',
    '  transform: translateY(-50%) translateY(2px);',
    '}',
    '.impact-table tbody td {',
    '  padding: 6px 10px; border: 1px solid #e0e0e0; vertical-align: middle;',
    '}',
    '.impact-table tbody td.num-col { text-align: center; font-variant-numeric: tabular-nums; }',
    '.impact-table tbody td.txt-col { text-align: left; }',
    '.impact-table td.idx-cell.neg { font-weight: 700; font-style: italic; }',
    '.impact-table td.idx-cell.insig {',
    '  background: #000 !important; color: #000; /* blackout */',
    '}',
    '.impact-table tfoot td {',
    '  padding: 8px 10px; border: 1px solid #BFBFBF;',
    '  background: #f5f5f5; font-weight: 600;',
    '}',
    '.impact-table tfoot td.num-col { text-align: center; }',
    '.impact-table tfoot tr.ti-row td { border-top: 2px solid #333; }',
    '.impact-table tfoot tr.base-row td {',
    '  color: #595959; font-weight: 400;',
    '}',
    '.impact-footer { margin-top: 12px; font-size: 12px; color: #555; }',
    '.impact-footer .index-note { margin: 0 0 4px 0; font-style: italic; }',
    '.impact-footer .muted { margin: 0; color: #888; }',
    '',
    '/* Prioritization dashboard (mirrors bn_prioritize_write) */',
    '.priort-dashboard { padding: 20px; overflow-x: auto; }',
    '.priort-controls {',
    '  display: flex; flex-wrap: wrap; align-items: center; gap: 10px;',
    '  margin-bottom: 16px; padding: 12px;',
    '  background: #fafafa; border: 1px solid #e0e0e0; border-radius: 6px;',
    '}',
    '.priort-controls label { font-weight: 600; color: #333; font-size: 13px; }',
    '.priort-ctrl {',
    '  padding: 4px 8px; font-size: 13px; width: 180px;',
    '  border: 1px solid #bbb; border-radius: 3px; background: #fff;',
    '  transition: background 0.15s, color 0.15s, border-color 0.15s;',
    '}',
    '.priort-ctrl.warn {',
    '  background: #FF0000; color: #FFFFFF; border-color: #FF0000;',
    '}',
    '.priort-warning { color: #FF0000; font-weight: 700; font-size: 13px; }',
    '.priort-split {',
    '  display: flex; flex-direction: row; gap: 16px;',
    '  align-items: stretch;',
    '}',
    '/* Column mode is driven entirely by JS (see checkOverflow in priort init).',
    '   It triggers when the viewport is narrow OR when the table\\u2019s natural',
    '   width would overflow its row-mode container — preventing any visual',
    '   overlap between the table and chart. */',
    '.priort-split.force-column { flex-direction: column; }',
    '.priort-split.force-column .priort-table-wrap,',
    '.priort-split.force-column .priort-chart-wrap {',
    '  flex: 0 0 auto; width: 100%;',
    '  box-sizing: border-box;',
    '}',
    '/* In row mode, align-items: stretch made the table inherit the chart',
    '   height; column mode loses that, so a min-height keeps the box balanced',
    '   when there are only a few data rows. */',
    '.priort-split.force-column .priort-table-wrap { min-height: 452px; }',
    '.priort-split.force-column .priort-chart { height: 420px; }',
    '.priort-card {',
    '  background: #fff; border: 1px solid #d8d8d8; border-radius: 8px;',
    '  padding: 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.04);',
    '}',
    '.priort-table-wrap {',
    '  flex: 1 1 0; min-width: 0; overflow-x: auto;',
    '  background: #fff; border: 1px solid #d8d8d8; border-radius: 8px;',
    '  padding: 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.04);',
    '}',
    '.priort-chart-wrap {',
    '  flex: 1 1 0; min-width: 0; position: relative;',
    '  background: #fff; border: 1px solid #d8d8d8; border-radius: 8px;',
    '  padding: 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.04);',
    '}',
    '.priort-tooltip {',
    '  position: fixed; pointer-events: none; z-index: 1000;',
    '  background: rgba(0, 0, 0, 0.55); color: #fff;',
    '  padding: 8px 12px; border-radius: 6px;',
    '  font-size: 12px; line-height: 1.45;',
    '  box-shadow: 0 4px 12px rgba(0,0,0,0.15);',
    '  max-width: 300px; white-space: pre-line;',
    '  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;',
    '  backdrop-filter: blur(2px);',
    '}',
    '.priort-chart { width: 100%; height: 480px; display: block; }',
    '.priort-chart .ax-line { stroke: #888; stroke-width: 1; }',
    '.priort-chart .grid-line { stroke: #e0e0e0; stroke-width: 1; }',
    '.priort-chart .ax-text { fill: #555; font-size: 11px; }',
    # matches bn_prioritize_write colours: light grey base (#D9D9D9), dark grey incremental (#595959)
    '.priort-chart .bar-prev { fill: #D9D9D9; }',
    '.priort-chart .bar-incr { fill: #595959; }',
    '.priort-chart .cum-line { stroke: #595959; stroke-width: 2; fill: none; }',
    '.priort-chart .cum-marker { fill: #595959; stroke: #595959; }',
    '.priort-chart .bar-label { fill: #333; font-size: 11px; text-anchor: middle; }',
    '.priort-chart .x-label { fill: #333; font-size: 11px; }',
    '.priort-table {',
    '  width: 100%; border-collapse: collapse; font-size: 13px;',
    '  background: #fff; table-layout: auto;',
    '}',
    '.priort-table thead th {',
    '  background: #D9D9D9; color: #222; font-weight: 700;',
    '  padding: 8px 10px; text-align: center; vertical-align: middle;',
    '  border: 1px solid #BFBFBF;',
    '  white-space: normal; word-wrap: break-word;',
    '  cursor: pointer; user-select: none; position: relative;',
    '}',
    '.priort-table thead th.sortable {',
    '  padding-right: 18px;',  # room for the absolutely-positioned triangle
    '}',
    '.priort-table thead th.sortable:hover { background: #CCCCCC; }',
    '.priort-table thead th.sortable::after {',
    '  content: ""; position: absolute; right: 4px; top: 50%;',
    '  transform: translateY(-50%);',
    '  border: 4px solid transparent; opacity: 0.3;',
    '}',
    '.priort-table thead th.sortable.sorted-asc::after {',
    '  border-bottom-color: #333; border-top: 0; opacity: 1;',
    '  transform: translateY(-50%) translateY(-2px);',
    '}',
    '.priort-table thead th.sortable.sorted-desc::after {',
    '  border-top-color: #333; border-bottom: 0; opacity: 1;',
    '  transform: translateY(-50%) translateY(2px);',
    '}',
    '.priort-table tbody td {',
    '  padding: 6px 10px; border: 1px solid #e0e0e0; vertical-align: middle;',
    '}',
    '.priort-table tbody td.num-col {',
    '  text-align: center; font-variant-numeric: tabular-nums;',
    '}',
    '.priort-table tbody td.txt-col { text-align: left; }',
    '.priort-table tbody td.neg { font-weight: 700; font-style: italic; }',
    '.priort-table .p-sig   { color: #2E7D32; font-weight: 700; }',
    '.priort-table .p-marg  { color: #E65100; font-weight: 600; }',
    '.priort-table .p-insig { color: #B71C1C; }',
    '.priort-table tfoot td {',
    '  padding: 8px 10px; border: 1px solid #BFBFBF;',
    '  background: #f5f5f5; font-weight: 600; text-align: center;',
    '  color: #595959;',
    '}',
    '.priort-footer { margin-top: 12px; font-size: 12px; color: #888; }',
    '.priort-footer .muted { margin: 0; }',
    '.priort-footer .priort-footer-base {',
    '  margin: 0 0 4px 0; font-weight: 700; color: #222;',
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
    '/* --- Attribute Impacts dashboard --- */',
    'function initImpactDashboard(dashId) {',
    '  var root = document.getElementById(dashId);',
    '  if (!root) return;',
    '  var dataScript = root.querySelector("script.impact-data");',
    '  if (!dataScript) return;',
    '  var data;',
    '  try { data = JSON.parse(dataScript.textContent); } catch (e) { return; }',
    '',
    '  function currentValue(dim) {',
    '    var sel = root.querySelector(\'.impact-ctrl[data-dim="\' + dim + \'"]\');',
    '    return sel ? sel.value : null;',
    '  }',
    '',
    '  function getRows() {',
    '    var weight = currentValue("weight") || "Unweighted";',
    '    if (weight === "Weighted" && data.rows_weighted) return data.rows_weighted;',
    '    return data.rows_unweighted;',
    '  }',
    '',
    '  function metricKey(focus) {',
    '    var key = currentValue("metric");',
    '    if (!key) return null;',
    '    // maxVmin and mi are focus-independent',
    '    if (key === "maxVmin" || key === "mi") return key;',
    '    // lift metrics: brand focuses use suffixed columns',
    '    if (focus && focus !== "Market") return key + "_" + focus;',
    '    return key;',
    '  }',
    '',
    '  function isInsignificant(sgData) {',
    '    if (!sgData) return false;',
    '    var pv = sgData.p_val;',
    '    return (pv != null && pv > 0.10);',
    '  }',
    '',
    '  function getRaw(row, sg, focus) {',
    '    var sgData = row.sg[sg]; if (!sgData) return null;',
    '    var k = metricKey(focus); if (!k) return null;',
    '    var v = sgData[k];',
    '    return (v == null ? null : v);',
    '  }',
    '',
    '  function update() {',
    '    var rows = getRows();',
    '    var focus = currentValue("focus") || "Market";',
    '    var mkey = currentValue("metric");',
    '    var weight = currentValue("weight") || "Unweighted";',
    '',
    '    // 1. Compute per-subgroup mean-absolute-raw (denominator for index)',
    '    data.subgroups.forEach(function(sg) {',
    '      var absVals = rows.map(function(r) {',
    '        var v = getRaw(r, sg, focus);',
    '        return v == null ? 0 : Math.abs(v);',
    '      });',
    '      var sum = absVals.reduce(function(a, b) { return a + b; }, 0);',
    '      var mean = absVals.length > 0 ? (sum / absVals.length) : 0;',
    '',
    '      // 2. Fill index cells + collect for color scaling',
    '      var idxValues = [];',
    '      rows.forEach(function(r, i) {',
    '        var cell = root.querySelector(\'td.idx-cell[data-sg="\' + sg + \'"][data-row="\' + i + \'"]\');',
    '        if (!cell) return;',
    '        var raw = getRaw(r, sg, focus);',
    '        var sgData = r.sg[sg];',
    '        cell.classList.remove("insig", "neg");',
    '        cell.style.background = "";',
    '',
    '        if (raw == null || mean === 0) { cell.textContent = ""; idxValues.push(null); return; }',
    '        var idx = Math.abs(raw) / mean * 100;',
    '        cell.textContent = Math.round(idx);',
    '        idxValues.push(idx);',
    '',
    '        if (raw < 0) cell.classList.add("neg");',
    '        if (isInsignificant(sgData)) cell.classList.add("insig");',
    '      });',
    '',
    '      // 3. Apply 3-color scale across non-null, non-insig cells in this subgroup',
    '      var vals = idxValues.filter(function(v) { return v != null; });',
    '      if (vals.length > 0) {',
    '        var minV = Math.min.apply(null, vals);',
    '        var maxV = Math.max.apply(null, vals);',
    '        var midV = (minV + maxV) / 2;',
    '        rows.forEach(function(r, i) {',
    '          var cell = root.querySelector(\'td.idx-cell[data-sg="\' + sg + \'"][data-row="\' + i + \'"]\');',
    '          if (!cell || cell.classList.contains("insig")) return;',
    '          var v = idxValues[i]; if (v == null) return;',
    '          cell.style.background = interpolate3(v, minV, midV, maxV);',
    '        });',
    '      }',
    '',
    '      // 4. Total Impact = sum(|raw|) / count (only for lift-type metrics)',
    '      var tiCell = root.querySelector(\'td.ti-cell[data-sg="\' + sg + \'"]\');',
    '      if (tiCell) {',
    '        if (mkey && (mkey === "maxVmin" || mkey === "mi")) {',
    '          tiCell.textContent = "";',
    '        } else if (sum === 0 || rows.length === 0) {',
    '          tiCell.textContent = "";',
    '        } else {',
    '          var ti = sum / rows.length;',
    '          tiCell.textContent = (ti * 100).toFixed(1) + "%";',
    '        }',
    '      }',
    '',
    '      // 5. Base cell: base or base_{focus} from the first row',
    '      var baseCell = root.querySelector(\'td.base-cell[data-sg="\' + sg + \'"]\');',
    '      if (baseCell && rows.length > 0) {',
    '        var baseKey = (focus === "Market") ? "base" : ("base_" + focus);',
    '        var b = rows[0].sg[sg] ? rows[0].sg[sg][baseKey] : null;',
    '        baseCell.textContent = (b == null) ? "" : Math.round(b);',
    '      }',
    '    });',
    '',
    '    // 6. Focus warning: red if any subgroup base is below minimum',
    '    var focusWarn = root.querySelector(\'.impact-warning[data-for="focus"]\');',
    '    if (focusWarn) {',
    '      var focusSel = root.querySelector(\'.impact-ctrl[data-dim="focus"]\');',
    '      focusWarn.textContent = "";',
    '      focusSel.classList.remove("warn");',
    '      if (focus !== "Market" && mkey && mkey !== "maxVmin" && mkey !== "mi" && rows.length > 0) {',
    '        var baseKey = "base_" + focus;',
    '        var minBaseAll = null;',
    '        data.subgroups.forEach(function(sg) {',
    '          var b = rows[0].sg[sg] ? rows[0].sg[sg][baseKey] : null;',
    '          if (b != null && (minBaseAll == null || b < minBaseAll)) minBaseAll = b;',
    '        });',
    '        if (minBaseAll != null && minBaseAll < data.min_base_for_lift) {',
    '          focusWarn.textContent = "Results not calculated because base is below " + data.min_base_for_lift;',
    '          focusSel.classList.add("warn");',
    '        }',
    '      }',
    '    }',
    '',
    '    // 7. Weight warning: the weight control is irrelevant for maxVmin / mi',
    '    var weightWarn = root.querySelector(\'.impact-warning[data-for="weight"]\');',
    '    if (weightWarn) {',
    '      if (mkey === "maxVmin" || mkey === "mi") {',
    '        weightWarn.textContent = "Weights don\\u2019t affect this metric";',
    '      } else {',
    '        weightWarn.textContent = "";',
    '      }',
    '    }',
    '',
    '    // 8. Index note below the table',
    '    var note = root.querySelector(".index-note");',
    '    if (note) {',
    '      var desc = metricDescription(mkey);',
    '      note.textContent = desc;',
    '    }',
    '  }',
    '',
    '  function metricDescription(mkey) {',
    '    if (!mkey) return "";',
    '    if (mkey === "lift" || mkey === "lift_0") {',
    '      return "Indexed by average market lift. Average lift measures the overall influence of each attribute on the outcome by shifting each attribute level up by 5% and averaging the resulting changes.";',
    '    }',
    '    if (mkey.indexOf("lift_") === 0) {',
    '      var pct = mkey.replace("lift_", "");',
    '      return "Indexed by " + pct + "% market lift. " + pct + "% lift measures how much the outcome changes when " + pct + "% of respondents for each attribute shift up by one level.";',
    '    }',
    '    if (mkey === "maxVmin") {',
    '      return "Indexed by max vs min impact. Max vs min measures the difference in the outcome between the best-case and worst-case scenario for each attribute.";',
    '    }',
    '    if (mkey === "mi") {',
    '      return "Indexed by mutual information. Mutual information measures the strength of the relationship between each attribute and the outcome.";',
    '    }',
    '    return "Indexed by " + mkey;',
    '  }',
    '',
    '  function interpolate3(v, lo, mid, hi) {',
    '    // 3-stop color scale: red (#f66a6e) -> yellow (#feea8a) -> green (#66bd7d).',
    '    // Matches bn_impact_write dashboard.',
    '    if (hi === lo) return "#feea8a";',
    '    var c1 = [246, 106, 110];  // red',
    '    var c2 = [254, 234, 138];  // yellow',
    '    var c3 = [102, 189, 125];  // green',
    '    function lerp(a, b, t) { return Math.round(a + (b - a) * t); }',
    '    function mix(ca, cb, t) { return [lerp(ca[0], cb[0], t), lerp(ca[1], cb[1], t), lerp(ca[2], cb[2], t)]; }',
    '    var c;',
    '    if (v <= mid) {',
    '      var t = (v - lo) / (mid - lo || 1);',
    '      c = mix(c1, c2, Math.max(0, Math.min(1, t)));',
    '    } else {',
    '      var t2 = (v - mid) / (hi - mid || 1);',
    '      c = mix(c2, c3, Math.max(0, Math.min(1, t2)));',
    '    }',
    '    return "rgb(" + c[0] + "," + c[1] + "," + c[2] + ")";',
    '  }',
    '',
    '  root.querySelectorAll(".impact-ctrl").forEach(function(sel) {',
    '    sel.addEventListener("change", update);',
    '  });',
    '',
    '  // --- Sortable headers ---',
    '  // Click toggles asc -> desc -> original (no sort) on the clicked column.',
    '  // Sorting operates on the <tbody> rows; <tfoot> (Total Impact + Base) is untouched.',
    '  var tbody = root.querySelector(".impact-table tbody");',
    '  var originalOrder = Array.from(tbody.querySelectorAll("tr"));',
    '  root.querySelectorAll(".impact-table thead th.sortable").forEach(function(th, colIdx) {',
    '    th.addEventListener("click", function() {',
    '      var state = th.getAttribute("data-sort-state") || "none";',
    '      var next = state === "none" ? "asc" : (state === "asc" ? "desc" : "none");',
    '      // Reset state on all headers',
    '      root.querySelectorAll(".impact-table thead th.sortable").forEach(function(x) {',
    '        x.classList.remove("sorted-asc", "sorted-desc");',
    '        x.setAttribute("data-sort-state", "none");',
    '      });',
    '      if (next === "none") {',
    '        originalOrder.forEach(function(tr) { tbody.appendChild(tr); });',
    '        return;',
    '      }',
    '      th.classList.add(next === "asc" ? "sorted-asc" : "sorted-desc");',
    '      th.setAttribute("data-sort-state", next);',
    '      var sortType = th.getAttribute("data-sort") || "text";',
    '      var rows = Array.from(tbody.querySelectorAll("tr"));',
    '      // Find actual index of this th among thead cells (stable regardless of col order)',
    '      var headerCells = Array.from(th.parentElement.children);',
    '      var idx = headerCells.indexOf(th);',
    '      rows.sort(function(a, b) {',
    '        var av = a.children[idx] ? a.children[idx].textContent.trim() : "";',
    '        var bv = b.children[idx] ? b.children[idx].textContent.trim() : "";',
    '        if (sortType === "num") {',
    '          var an = parseFloat(av); var bn = parseFloat(bv);',
    '          // Push blanks to the bottom regardless of direction',
    '          if (isNaN(an) && isNaN(bn)) return 0;',
    '          if (isNaN(an)) return 1;',
    '          if (isNaN(bn)) return -1;',
    '          return next === "asc" ? an - bn : bn - an;',
    '        }',
    '        var cmp = av.localeCompare(bv, undefined, { sensitivity: "base" });',
    '        return next === "asc" ? cmp : -cmp;',
    '      });',
    '      rows.forEach(function(tr) { tbody.appendChild(tr); });',
    '    });',
    '  });',
    '',
    '  // --- Column resize ---',
    '  // Drag the right edge of any header to resize. Resizing a metric',
    '  // column resizes ALL metric columns in sync (so they stay uniform).',
    '  var impactTable = root.querySelector(".impact-table");',
    '  root.querySelectorAll(".impact-table thead th").forEach(function(th) {',
    '    var handle = document.createElement("div");',
    '    handle.className = "col-resize-handle";',
    '    th.appendChild(handle);',
    '    // Prevent the handle from firing sort clicks',
    '    handle.addEventListener("click", function(e) { e.stopPropagation(); });',
    '    handle.addEventListener("mousedown", function(e) {',
    '      e.preventDefault(); e.stopPropagation();',
    '      var startX = e.clientX;',
    '      var startW = th.offsetWidth;',
    '      var isMetric = th.classList.contains("metric-col");',
    '      impactTable.classList.add("resizing");',
    '      var suppressClick = function(ev) { ev.stopPropagation(); ev.preventDefault(); };',
    '      function applyWidth(w) {',
    '        var targets = isMetric',
    '          ? root.querySelectorAll(".impact-table thead th.metric-col")',
    '          : [th];',
    '        targets.forEach(function(t) {',
    '          t.style.width = w + "px";',
    '          t.style.minWidth = w + "px";',
    '          t.style.maxWidth = w + "px";',
    '        });',
    '      }',
    '      function onMove(ev) {',
    '        var newW = Math.max(40, startW + (ev.clientX - startX));',
    '        applyWidth(newW);',
    '      }',
    '      function onUp() {',
    '        document.removeEventListener("mousemove", onMove);',
    '        document.removeEventListener("mouseup", onUp);',
    '        impactTable.classList.remove("resizing");',
    '        // Swallow the trailing click so sort doesn\\u2019t fire',
    '        th.addEventListener("click", suppressClick, { once: true, capture: true });',
    '      }',
    '      document.addEventListener("mousemove", onMove);',
    '      document.addEventListener("mouseup", onUp);',
    '    });',
    '  });',
    '',
    '  update();',
    '}',
    '',
    '/* --- Prioritization dashboard --- */',
    'function initPriortDashboard(dashId) {',
    '  var root = document.getElementById(dashId);',
    '  if (!root) return;',
    '  var dataScript = root.querySelector("script.priort-data");',
    '  if (!dataScript) return;',
    '  var data;',
    '  try { data = JSON.parse(dataScript.textContent); } catch (e) { return; }',
    '',
    '  function ctrl(dim) {',
    '    return root.querySelector(\'.priort-ctrl[data-dim="\' + dim + \'"]\');',
    '  }',
    '  function currentValue(dim) {',
    '    var sel = ctrl(dim);',
    '    if (sel) return sel.value;',
    '    // Inactive dim — use the single available value',
    '    return (data.dims[dim] && data.dims[dim][0]) || "";',
    '  }',
    '',
    '  function currentKey() {',
    '    return [currentValue("strategy"), currentValue("search"),',
    '            currentValue("subgroup"), currentValue("focus"),',
    '            currentValue("weight")].join("|");',
    '  }',
    '',
    '  function pvalClass(pv) {',
    '    if (pv == null || isNaN(pv)) return "";',
    '    if (pv < data.sig_threshold) return "p-sig";',
    '    if (pv < data.marginal_threshold) return "p-marg";',
    '    return "p-insig";',
    '  }',
    '',
    '  var tbody = root.querySelector(".priort-table tbody");',
    '  var footerBase = root.querySelector(".priort-footer-base");',
    '  var focusWarn = root.querySelector(\'.priort-warning[data-for="focus"]\');',
    '  var focusSel = root.querySelector(\'.priort-ctrl[data-dim="focus"]\');',
    '',
  '  function whiteToGreen(v, lo, hi) {',
    '    // Linear interpolation white (#FFFFFF) -> green (#66bd7d)',
    '    if (v == null || isNaN(v)) return "";',
    '    if (hi === lo) return "#FFFFFF";',
    '    var t = Math.max(0, Math.min(1, (v - lo) / (hi - lo)));',
    '    var r = Math.round(255 + (102 - 255) * t);',
    '    var g = Math.round(255 + (189 - 255) * t);',
    '    var b = Math.round(255 + (125 - 255) * t);',
    '    return "rgb(" + r + "," + g + "," + b + ")";',
    '  }',
    '',
    '  function render() {',
    '    var key = currentKey();',
    '    var entry = data.lookup[key] || { rows: [], n_obs: null };',
    '',
    '    // Base — bold line below the table',
    '    var nObs = entry.n_obs;',
    '    var baseText = (nObs != null && !isNaN(nObs)) ? ("Base: " + Math.round(nObs)) : "";',
    '    if (footerBase) footerBase.textContent = baseText;',
    '',
    '    // Focus warning',
    '    if (focusSel) focusSel.classList.remove("warn");',
    '    if (focusWarn) focusWarn.textContent = "";',
    '    if (nObs != null && !isNaN(nObs) && nObs < data.min_base_for_boot) {',
    '      if (focusWarn) focusWarn.textContent = "Results not calculated because base is below " + data.min_base_for_boot;',
    '      if (focusSel) focusSel.classList.add("warn");',
    '    }',
    '',
    '    // Precompute min/max for each gradient-scaled metric column',
    '    function rangeOf(key) {',
    '      var vals = entry.rows.map(function(r) { return r[key]; })',
    '        .filter(function(v) { return v != null && !isNaN(v); });',
    '      if (vals.length === 0) return null;',
    '      return { lo: Math.min.apply(null, vals), hi: Math.max.apply(null, vals) };',
    '    }',
    '    var rDV  = rangeOf("dv_estimate");',
    '    var rMG  = rangeOf("marginal_gain");',
    '    var rMGP = rangeOf("marginal_gain_pct");',
    '',
    '    // Rebuild body',
    '    tbody.innerHTML = "";',
    '    entry.rows.forEach(function(r) {',
    '      var tr = document.createElement("tr");',
    '      var cells = [];',
    '      cells.push({cls: \'num-col\', text: r.priority});',
    '      cells.push({cls: \'txt-col\', text: r.variable});',
    '      if (data.has_community) cells.push({cls: \'txt-col\', text: r.community == null ? "" : r.community});',
    '      if (data.has_label)     cells.push({cls: \'txt-col\', text: r.label     == null ? "" : r.label});',
    '      // DV Estimate — white -> green gradient',
    '      cells.push({',
    '        cls: \'num-col\',',
    '        text: r.dv_estimate == null ? "" : r.dv_estimate.toFixed(3),',
    '        bg: rDV ? whiteToGreen(r.dv_estimate, rDV.lo, rDV.hi) : ""',
    '      });',
    '      // Marginal Gain — gradient + bold italic if negative',
    '      cells.push({',
    '        cls: \'num-col\' + (r.marginal_gain != null && r.marginal_gain < 0 ? \' neg\' : \'\'),',
    '        text: r.marginal_gain == null ? "" : r.marginal_gain.toFixed(3),',
    '        bg: rMG ? whiteToGreen(r.marginal_gain, rMG.lo, rMG.hi) : ""',
    '      });',
    '      // Marginal Gain % — gradient + bold italic if negative',
    '      cells.push({',
    '        cls: \'num-col\' + (r.marginal_gain_pct != null && r.marginal_gain_pct < 0 ? \' neg\' : \'\'),',
    '        text: r.marginal_gain_pct == null ? "" : (r.marginal_gain_pct * 100).toFixed(2) + "%",',
    '        bg: rMGP ? whiteToGreen(r.marginal_gain_pct, rMGP.lo, rMGP.hi) : ""',
    '      });',
    '      // p-value — green / orange / red coloring (not blackout)',
    '      if (data.has_p) {',
    '        var pCls = pvalClass(r.p_value);',
    '        cells.push({',
    '          cls: \'num-col priort-pval \' + pCls,',
    '          text: (r.p_value == null || isNaN(r.p_value)) ? "" : r.p_value.toFixed(2)',
    '        });',
    '      }',
    '      cells.forEach(function(c) {',
    '        var td = document.createElement("td");',
    '        td.className = c.cls;',
    '        td.textContent = c.text;',
    '        if (c.bg) td.style.background = c.bg;',
    '        tr.appendChild(td);',
    '      });',
    '      tbody.appendChild(tr);',
    '    });',
    '',
    '    // --- Cumulative-effect chart (waterfall): each step\\u2019s marginal',
    '    // gain stacked on the previous step\\u2019s DV estimate.',
    '    drawChart(entry.rows);',
    '',
    '    // After the table rebuilds, re-evaluate whether it can fit beside the',
    '    // chart in row mode — if not, switch to top/bottom layout so the chart',
    '    // never appears to crowd or overlap the table.',
    '    checkOverflow();',
    '  }',
    '',
    '  // Force column (top/bottom) layout when the viewport is narrow OR when',
    '  // the table\\u2019s natural width would overflow its row-mode container.',
    '  // Implementation: temporarily strip the force-column class so we can',
    '  // measure row-mode dimensions, then re-apply if needed. The temporary',
    '  // toggle costs one synchronous reflow but no visible flicker.',
    '  function checkOverflow() {',
    '    var split = root.querySelector(".priort-split");',
    '    var tableWrap = root.querySelector(".priort-table-wrap");',
    '    var table = root.querySelector(".priort-table");',
    '    if (!split || !tableWrap || !table) return;',
    '',
    '    var wasForced = split.classList.contains("force-column");',
    '',
    '    // Narrow viewport always uses column mode',
    '    if (window.innerWidth < 1100) {',
    '      split.classList.add("force-column");',
    '      if (!wasForced && lastChartRows) drawChart();',
    '      return;',
    '    }',
    '',
    '    // Measure in row mode',
    '    split.classList.remove("force-column");',
    '    // +1 px tolerance for sub-pixel rounding',
    '    var overflows = table.scrollWidth > tableWrap.clientWidth + 1;',
    '    if (overflows) split.classList.add("force-column");',
    '',
    '    var nowForced = split.classList.contains("force-column");',
    '    if (nowForced !== wasForced && lastChartRows) drawChart();',
    '  }',
    '',
    '  // React to viewport resizes (data didn\\u2019t change, but available width did)',
    '  window.addEventListener("resize", checkOverflow);',
    '',
  '  var lastChartRows = null;',
    '  function drawChart(rows) {',
    '    if (rows) lastChartRows = rows;',
    '    var svg = root.querySelector(".priort-chart");',
    '    if (!svg) return;',
    '    while (svg.firstChild) svg.removeChild(svg.firstChild);',
    '',
    '    var W = svg.clientWidth || svg.parentElement.clientWidth || 0;',
    '    var H = svg.clientHeight || 480;',
    '    if (W === 0) return; // tab hidden — re-rendered later by observer',
    '    svg.setAttribute("viewBox", "0 0 " + W + " " + H);',
    '    if (!lastChartRows || lastChartRows.length === 0) return;',
    '    rows = lastChartRows;',
    '',
    '    var pad = { l: 50, r: 16, t: 24, b: 70 };',
    '    var plotW = W - pad.l - pad.r;',
    '    var plotH = H - pad.t - pad.b;',
    '',
    '    // y range: floor at min(0, min DV); ceiling at max DV with padding',
    '    var dvVals = rows.map(function(r) { return r.dv_estimate; })',
    '      .filter(function(v) { return v != null && !isNaN(v); });',
    '    if (dvVals.length === 0) return;',
    '    var yMin = Math.min(0, Math.min.apply(null, dvVals));',
    '    var yMax = Math.max.apply(null, dvVals);',
    '    var yRange = yMax - yMin || 1;',
    '    yMax = yMax + yRange * 0.05;',
    '',
    '    function yScale(v) {',
    '      return pad.t + plotH * (1 - (v - yMin) / (yMax - yMin));',
    '    }',
    '',
    '    var n = rows.length;',
    '    var bandW = plotW / n;',
    '    var barW = Math.max(8, bandW * 0.6);',
    '    var barOffset = (bandW - barW) / 2;',
    '',
    '    var ns = "http://www.w3.org/2000/svg";',
    '    function el(tag, attrs, parent) {',
    '      var e = document.createElementNS(ns, tag);',
    '      for (var k in attrs) e.setAttribute(k, attrs[k]);',
    '      (parent || svg).appendChild(e);',
    '      return e;',
    '    }',
    '',
    '    // gridlines + y-axis labels (5 ticks)',
    '    for (var i = 0; i <= 4; i++) {',
    '      var v = yMin + (yMax - yMin) * i / 4;',
    '      var y = yScale(v);',
    '      el("line", { x1: pad.l, x2: pad.l + plotW, y1: y, y2: y, "class": "grid-line" });',
    '      el("text", { x: pad.l - 6, y: y + 3, "text-anchor": "end", "class": "ax-text" }).textContent = v.toFixed(2);',
    '    }',
    '    // baseline at 0 (only if 0 is within range)',
    '    if (yMin <= 0 && yMax >= 0) {',
    '      var y0 = yScale(0);',
    '      el("line", { x1: pad.l, x2: pad.l + plotW, y1: y0, y2: y0, "class": "ax-line" });',
    '    }',
    '    // x-axis line',
    '    el("line", { x1: pad.l, x2: pad.l + plotW, y1: pad.t + plotH, y2: pad.t + plotH, "class": "ax-line" });',
    '',
    '    // Stacked bars (matches bn_prioritize_write):',
    '    //   light grey "Previous" base from 0 to min(prev, current)',
    '    //   dark grey "Incremental" segment from min(prev, current) to max(prev, current)',
    '    // Plus a cumulative line + circle markers tracing each step\\u2019s DV.',
    '    var prev = 0;',
    '    var linePoints = [];',
    '',
    '    function tooltipText(r, dv, prevVal) {',
    '      var parts = [];',
    '      parts.push("Step " + (r.priority == null ? "?" : r.priority));',
    '      var name = (r.variable == null ? "" : String(r.variable));',
    '      if (r.label != null && r.label !== "" && r.label !== name) {',
    '        parts.push(name + " (" + r.label + ")");',
    '      } else {',
    '        parts.push(name);',
    '      }',
    '      if (r.community != null && r.community !== "") parts.push("Community: " + r.community);',
    '      parts.push("DV: " + dv.toFixed(3));',
    '      var mg = (dv - prevVal);',
    '      parts.push("Marginal Gain: " + mg.toFixed(3));',
    '      if (r.marginal_gain_pct != null && !isNaN(r.marginal_gain_pct)) {',
    '        parts.push("Marginal Gain %: " + (r.marginal_gain_pct * 100).toFixed(2) + "%");',
    '      }',
    '      if (r.p_value != null && !isNaN(r.p_value)) {',
    '        parts.push("p-value: " + r.p_value.toFixed(2));',
    '      }',
    '      return parts.join("\\n");',
    '    }',
    '',
    '    var tooltip = root.querySelector(".priort-tooltip");',
    '    function bindTip(node, text) {',
    '      if (!tooltip) return;',
    '      node.style.cursor = "pointer";',
    '      node.addEventListener("mousemove", function(e) {',
    '        tooltip.textContent = text;',
    '        tooltip.style.display = "block";',
    '        // Position with a small offset; clamp to viewport so it stays on screen',
    '        var px = e.clientX + 12;',
    '        var py = e.clientY + 12;',
    '        var tw = tooltip.offsetWidth;',
    '        var th = tooltip.offsetHeight;',
    '        if (px + tw > window.innerWidth - 8) px = e.clientX - tw - 12;',
    '        if (py + th > window.innerHeight - 8) py = e.clientY - th - 12;',
    '        tooltip.style.left = px + "px";',
    '        tooltip.style.top  = py + "px";',
    '      });',
    '      node.addEventListener("mouseleave", function() {',
    '        tooltip.style.display = "none";',
    '      });',
    '    }',
    '',
    '    rows.forEach(function(r, idx) {',
    '      var dv = r.dv_estimate;',
    '      if (dv == null || isNaN(dv)) return;',
    '      var lo = Math.min(prev, dv);',
    '      var hi = Math.max(prev, dv);',
    '      var x  = pad.l + idx * bandW + barOffset;',
    '      var y0 = yScale(0);',
    '      var yLo = yScale(lo);',
    '      var yHi = yScale(hi);',
    '      var tt = tooltipText(r, dv, prev);',
    '',
    '      // Previous (light grey base): 0 -> lo',
    '      var prevH = Math.max(0, y0 - yLo);',
    '      if (prevH > 0) {',
    '        var rectPrev = el("rect", { x: x, y: yLo, width: barW, height: prevH, "class": "bar-prev" });',
    '        bindTip(rectPrev, tt);',
    '      }',
    '      // Incremental (dark grey): lo -> hi',
    '      var incrH = Math.max(0, yLo - yHi);',
    '      if (incrH > 0) {',
    '        var rectIncr = el("rect", { x: x, y: yHi, width: barW, height: incrH, "class": "bar-incr" });',
    '        bindTip(rectIncr, tt);',
    '      }',
    '',
    '      // Track cumulative-DV line point at the bar centre',
    '      linePoints.push({ x: x + barW / 2, y: yScale(dv), val: dv, tip: tt });',
    '',
    '      // x-axis label (variable name) — rotated -45; hover shows variable + label',
    '      var name = (r.variable == null) ? "" : String(r.variable);',
    '      var labelX = pad.l + idx * bandW + bandW / 2;',
    '      var labelY = pad.t + plotH + 12;',
    '      var t = el("text", {',
    '        x: labelX, y: labelY,',
    '        "text-anchor": "end",',
    '        "transform": "rotate(-45 " + labelX + " " + labelY + ")",',
    '        "class": "x-label"',
    '      });',
    '      t.textContent = name.length > 18 ? name.substring(0, 16) + "\\u2026" : name;',
    '      var hoverName = name;',
    '      if (r.label != null && r.label !== "" && r.label !== name) {',
    '        hoverName = name + " (" + r.label + ")";',
    '      }',
    '      bindTip(t, hoverName + "\\n\\n" + tt);',
    '      prev = dv;',
    '    });',
    '',
    '    // Draw the cumulative line + markers + value labels above each marker',
    '    if (linePoints.length > 0) {',
    '      var d = linePoints.map(function(p, i) {',
    '        return (i === 0 ? "M" : "L") + p.x + " " + p.y;',
    '      }).join(" ");',
    '      el("path", { d: d, "class": "cum-line" });',
    '      linePoints.forEach(function(p) {',
    '        var c = el("circle", { cx: p.x, cy: p.y, r: 3.5, "class": "cum-marker" });',
    '        bindTip(c, p.tip);',
    '        var lab = el("text", { x: p.x, y: p.y - 6, "class": "bar-label" });',
    '        lab.textContent = p.val.toFixed(2);',
    '        bindTip(lab, p.tip);',
    '      });',
    '    }',
    '  }',
    '',
    '  // Wire dropdowns',
    '  root.querySelectorAll(".priort-ctrl").forEach(function(sel) {',
    '    sel.addEventListener("change", render);',
    '  });',
    '',
    '  // Redraw chart when the panel becomes visible (e.g., user clicks the',
    '  // Prioritization tab for the first time after page load — the SVG was',
    '  // zero-width while the tab was hidden, so the initial render was a no-op).',
    '  if (window.ResizeObserver) {',
    '    var chartWrap = root.querySelector(".priort-chart-wrap");',
    '    if (chartWrap) {',
    '      var ro = new ResizeObserver(function(entries) {',
    '        entries.forEach(function(e) {',
    '          if (e.contentRect.width > 0) {',
    '            // Panel just became visible — recheck layout, then redraw',
    '            checkOverflow();',
    '            if (lastChartRows) drawChart();',
    '          }',
    '        });',
    '      });',
    '      ro.observe(chartWrap);',
    '    }',
    '  }',
    '',
    '  // Sortable headers (resets on each render because table rebuilds)',
    '  root.querySelectorAll(".priort-table thead th.sortable").forEach(function(th) {',
    '    th.addEventListener("click", function() {',
    '      var state = th.getAttribute("data-sort-state") || "none";',
    '      var next = state === "none" ? "asc" : (state === "asc" ? "desc" : "none");',
    '      root.querySelectorAll(".priort-table thead th.sortable").forEach(function(x) {',
    '        x.classList.remove("sorted-asc", "sorted-desc");',
    '        x.setAttribute("data-sort-state", "none");',
    '      });',
    '      if (next === "none") { render(); return; }',
    '      th.classList.add(next === "asc" ? "sorted-asc" : "sorted-desc");',
    '      th.setAttribute("data-sort-state", next);',
    '      var sortType = th.getAttribute("data-sort") || "text";',
    '      var tb = root.querySelector(".priort-table tbody");',
    '      var rows = Array.from(tb.querySelectorAll("tr"));',
    '      var headerCells = Array.from(th.parentElement.children);',
    '      var idx = headerCells.indexOf(th);',
    '      rows.sort(function(a, b) {',
    '        var av = a.children[idx] ? a.children[idx].textContent.trim() : "";',
    '        var bv = b.children[idx] ? b.children[idx].textContent.trim() : "";',
    '        if (sortType === "num") {',
    '          // Strip "%" for numeric compare',
    '          var an = parseFloat(av.replace("%", ""));',
    '          var bn = parseFloat(bv.replace("%", ""));',
    '          if (isNaN(an) && isNaN(bn)) return 0;',
    '          if (isNaN(an)) return 1;',
    '          if (isNaN(bn)) return -1;',
    '          return next === "asc" ? an - bn : bn - an;',
    '        }',
    '        var cmp = av.localeCompare(bv, undefined, { sensitivity: "base" });',
    '        return next === "asc" ? cmp : -cmp;',
    '      });',
    '      rows.forEach(function(tr) { tb.appendChild(tr); });',
    '    });',
    '  });',
    '',
    '  render();',
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
