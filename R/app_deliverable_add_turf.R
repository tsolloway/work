
# =============================================================================
# TURF module — returns module definition for app_deliverable()
# =============================================================================

#' app_deliverable_add_turf
#'
#' @description Creates a TURF analysis module for use with
#'   \code{app_deliverable()}. Returns a module definition list containing
#'   two tabs ("TURF" and "TURF - Best Combo"), server logic, CSS, and head
#'   tags.
#'
#' @param best_combo_results Optional. Output from \code{turf_best_combo()}.
#'   If \code{NULL}, the "TURF - Best Combo" tab is hidden.
#' @param raw Data frame. The original respondent-level data.
#' @param vars Character vector. Binary item column names in \code{raw}.
#' @param subgroups Character vector. Subgroup column names (binary 0/1). NULL
#'   creates a "Total" column.
#' @param weight Character. Weight column name in \code{raw}. NULL = unweighted.
#' @param labels Data frame with \code{variable}/\code{label} columns, or a
#'   named character vector.
#' @param project_name Character. Project name written to the Excel workbook
#'   via \code{turf_write()}. Default \code{"Project Name - (#xxxxxxx)"}.
#' @param sig_threshold Numeric. P-value threshold for green significance.
#' @param marginal_threshold Numeric. P-value threshold for orange marginal.
#' @param top Integer or \code{NULL}. Maximum number of combo rows to keep per
#'   subgroup per combo size. Combos are pre-sorted by reach descending, so
#'   truncation drops only the worst-performing combos. Set to \code{NULL} to
#'   keep all. Default \code{5000}.
#'
#' @return A module definition list with elements \code{id}, \code{tabs},
#'   \code{server}, \code{css}, and \code{head_tags}. Pass to
#'   \code{app_deliverable(modules = list(...))}.
#'
#' @examples
#' \dontrun{
#' mod_turf <- app_deliverable_add_turf(
#'   best_combo_results = turf_results,
#'   raw       = example_data_ice_cream,
#'   vars      = example_data_ice_cream_dictionary$variable,
#'   subgroups = c("Total", "Gen_Z", "Millennials", "Gen_X"),
#'   weight    = "weight",
#'   labels    = example_data_ice_cream_dictionary
#' )
#'
#' app_deliverable(
#'   title   = "Ice Cream TURF (#1234567)",
#'   modules = list(mod_turf)
#' )
#' }
#'
#' @export
app_deliverable_add_turf <- function(
    best_combo_results = NULL, raw, vars,
    subgroups = NULL, weight = NULL, labels = NULL,
    project_name = "Project Name - (#xxxxxxx)",
    sig_threshold = 0.10, marginal_threshold = 0.20,
    top = 5000,
    id = NULL
) {

  # ---- Generate unique module ID ----
  if (is.null(id)) {
    id <- paste0("turf_", substr(uuid::UUIDgenerate(), 1, 8))
  }
  ns <- shiny::NS(id)

  # ---- Pre-process (reuse turf_write helpers) ----
  label_lookup <- .turf_build_label_lookup(vars, labels)

  has_combos <- !is.null(best_combo_results)

  if (has_combos) {
    normalized <- .turf_normalize(best_combo_results)
    if (!is.null(top)) {
      for (sg in names(normalized)) {
        for (nk in names(normalized[[sg]])) {
          normalized[[sg]][[nk]] <- utils::head(normalized[[sg]][[nk]], top)
        }
      }
    }
    subgroup_names <- names(normalized)
    n_keys <- names(normalized[[1]])
    n_values <- as.integer(gsub("^n_", "", n_keys))
    first_tbl <- normalized[[1]][[1]]
    col_info <- .turf_detect_columns(first_tbl)
  } else {
    normalized <- NULL
    if (!is.null(subgroups)) {
      subgroup_names <- subgroups
    } else {
      subgroup_names <- "Total"
    }
    n_values <- integer(0)
    col_info <- list(has_labels = FALSE, has_weights = !is.null(weight),
                     item_cols = character(0), label_cols = character(0), n_items = 0)
  }

  if (is.null(weight)) col_info$has_weights <- FALSE

  # Ensure "Total" column exists in raw if needed
  if (is.null(subgroups)) {
    raw$Total <- 1L
    subgroup_names <- "Total"
  }

  base_sizes <- .turf_compute_bases(raw, subgroups, subgroup_names)

  # ---- Trim raw to only the columns the module needs at runtime ----
  keep_cols <- unique(c(vars, subgroup_names))
  if (!is.null(weight)) keep_cols <- c(keep_cols, weight)
  raw <- raw[, keep_cols, drop = FALSE]

  # ---- Coerce binary columns to integer (8 bytes -> 4 bytes per value) ----
  binary_cols <- intersect(c(vars, subgroup_names), names(raw))
  raw[binary_cols] <- lapply(raw[binary_cols], as.integer)

  has_weights <- col_info$has_weights

  # ---- Build UI tabs ----
  tabs <- .turf_module_ui(
    ns = ns,
    has_combos = has_combos,
    has_weights = has_weights,
    subgroup_names = subgroup_names,
    n_values = n_values
  )

  # ---- CSS (namespaced) ----
  ns_prefix <- paste0(id, "-")
  css <- paste(
    paste0(
      "#", ns_prefix, "base_display, #", ns_prefix, "bc_base_display {\n",
      "  pointer-events: none;\n",
      "  background-color: var(--ndr-secondary-bg);\n",
      "}\n",
      # Items-table checkboxes — brand-tokenized so checked state pulls the
      # brand accent (--ndr-accent) instead of Bootstrap's primary alias.
      # `accent-color` is the CSS property that tints native checkboxes.
      "#", ns_prefix, "items_table input[type='checkbox'] {\n",
      "  width: 1.1em; height: 1.1em; cursor: pointer;\n",
      "  accent-color: var(--ndr-accent);\n",
      "}"
    ),
    # Init splash spinner (shared helper) — dropped once the first plotly
    # chart has drawn or a 20s fallback (see head script below).
    .app_deliverable_splash_css(),
    sep = "\n"
  )

  head_tags <- list(
    shiny::tags$style(shiny::HTML(
      ".turf-dl-overlay {",
      "  position: fixed; top: 0; left: 0; width: 100%; height: 100%;",
      "  background: rgba(0,0,0,0.35); z-index: 9999;",
      "  display: flex; align-items: center; justify-content: center;",
      "}",
      ".turf-dl-overlay .turf-dl-box {",
      "  background: var(--bs-body-bg, #fff); border-radius: 8px;",
      "  padding: 2rem 2.5rem; text-align: center;",
      "  box-shadow: 0 4px 24px rgba(0,0,0,0.2);",
      "}",
      ".turf-dl-spinner {",
      "  width: 36px; height: 36px; margin: 0 auto 0.75rem;",
      "  border: 4px solid var(--bs-border-color, #dee2e6);",
      "  border-top-color: var(--bs-primary, #0d6efd);",
      "  border-radius: 50%; animation: turf-spin 0.8s linear infinite;",
      "}",
      "@keyframes turf-spin { to { transform: rotate(360deg); } }"
    )),
    shiny::tags$script(shiny::HTML(paste0(
      "(function() {",
      "  var dlIds = {'", ns_prefix, "dl_workbook': true,",
      "               '", ns_prefix, "dl_workbook_light': true,",
      "               '", ns_prefix, "bc_dl_workbook': true,",
      "               '", ns_prefix, "bc_dl_workbook_light': true};",
      "  function showOverlay() {",
      "    if ($('.turf-dl-overlay').length) return;",
      "    $('<div class=\"turf-dl-overlay\">' +",
      "      '<div class=\"turf-dl-box\">' +",
      "        '<div class=\"turf-dl-spinner\"></div>' +",
      "        '<div>Generating Workbook\\u2026</div>' +",
      "      '</div></div>').appendTo('body');",
      "  }",
      "  function removeOverlay() {",
      "    setTimeout(function() { $('.turf-dl-overlay').remove(); }, 200);",
      "  }",
      "  $(document).on('shiny:filedownload', function(e) {",
      "    if (!dlIds[e.name]) return;",
      "    showOverlay();",
      "    var xhr = new XMLHttpRequest();",
      "    xhr.open('GET', e.href, true);",
      "    xhr.responseType = 'blob';",
      "    xhr.onloadend = function() { removeOverlay(); };",
      "    xhr.send();",
      "  });",
      "})();"
    ))),
    # Init splash spinner (shared CSS in `css` above) — dropped once the first
    # plotly chart has drawn (.js-plotly-plot appears in the DOM) or a 20s
    # fallback so it can never stick.
    shiny::tags$script(shiny::HTML(paste0(
      "(function(){",
      "  function ready(){ if (document.body) document.body.classList.add('rdx-app-ready'); }",
      "  function check(){ if (document.querySelector('.js-plotly-plot')){ ready(); return true; } return false; }",
      "  if (!check()) {",
      "    var obs = new MutationObserver(function(){ if (check()) obs.disconnect(); });",
      "    function start(){ obs.observe(document.body, {childList:true, subtree:true}); }",
      "    if (document.body) start(); else document.addEventListener('DOMContentLoaded', start);",
      "  }",
      "  setTimeout(ready, 20000);",
      "})();"
    ))),
    # Shared resondex floating tooltip — drives every `[data-tip]`
    # element across the app. Used by the reactable column headers
    # (th() helper) instead of Bootstrap\'s data-bs-toggle="tooltip",
    # which depended on bslib auto-init that proved unreliable. The
    # data-tip mechanism is the same one the network-drivers app
    # (network_drivers_impacts.R / network_drivers_prio.R) uses.
    shiny::tags$script(shiny::HTML(resondex_tooltip_js()))
  )

  # ---- Server function ----
  server_fn <- function(input, output, session, dark_mode) {
    .turf_module_server(
      input, output, session,
      dark_mode = dark_mode,
      best_combo_results = normalized,
      raw = raw, vars = vars,
      subgroups = subgroup_names,
      weight = weight,
      label_lookup = label_lookup,
      col_info = col_info,
      base_sizes = base_sizes,
      n_values = n_values,
      project_name = project_name,
      sig_threshold = sig_threshold,
      marginal_threshold = marginal_threshold
    )
  }

  # ---- Return module definition ----
  list(
    id        = id,
    tabs      = tabs,
    server    = server_fn,
    css       = css,
    head_tags = head_tags
  )
}


# =============================================================================
# Module UI — builds namespaced tab panels
# =============================================================================

.turf_module_ui <- function(ns, has_combos, has_weights, subgroup_names, n_values) {

  # Display labels: replace underscores with spaces
  subgroup_choices <- stats::setNames(subgroup_names, gsub("_", " ", subgroup_names))

  # ---- Dashboard tab ----
  dashboard_sidebar <- bslib::sidebar(
    width = 220,
    fillable = TRUE,
    fill = TRUE,
    shiny::selectInput(ns("subgroup"), "Subgroup:",
                        choices = subgroup_choices,
                        selected = subgroup_names[1]),
    shiny::selectInput(ns("optimize"), "Optimize:",
                        choices = c("Reach", "Freq")),
    if (has_weights) {
      shiny::selectInput(ns("weighted"), "Weighted:",
                          choices = c("Yes", "No"))
    },
    shiny::selectInput(ns("chart_label"), "Display Label:",
                        choices = c("Label", "Variable - Label", "Variable"),
                        selected = "Label"),
    shiny::selectInput(ns("chart_theme"), "Chart Theme:",
                        choices = plotly_theme_names(),
                        selected = "Default"),
    shiny::textInput(ns("base_display"), "Base:", value = ""),
    # Workbook downloads pinned to the BOTTOM of the sidebar — same
    # margin-top:auto flex idiom as the network-drivers Download Workbook.
    # bslib::sidebar's content area is a flex column, so margin-top:auto
    # on this wrapper consumes remaining vertical space.
    shiny::div(
      style = "margin-top: auto;",
      shiny::tags$hr(style = "margin: 12px 0;"),
      shiny::downloadButton(ns("dl_workbook"), "Download Excel",
                            icon = NULL,
                            class = "btn-rdx w-100"),
      shiny::downloadButton(ns("dl_workbook_light"), "Download Excel Light",
                            icon = NULL,
                            class = "btn-rdx w-100 mt-1")
    )
  )

  dashboard_tab <- bslib::nav_panel(
    "TURF",
    bslib::layout_sidebar(
      fillable = TRUE,
      fill     = TRUE,
      sidebar = dashboard_sidebar,
      bslib::layout_columns(
        col_widths = bslib::breakpoints(
          xs = c(12, 12, 12), # Stacks elements vertically on small screens
          xl = c(7, 5, 12)    # 2/3 and 1/3 layout on large screens
        ),
        row_heights = bslib::breakpoints(
          xs = c("minmax(500px, auto)", "minmax(500px, auto)", "minmax(500px, auto)"),
          xl = c("minmax(0, 1fr)", "minmax(0, 1fr)")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("TURF Chart"),
          bslib::card_body(fillable = TRUE, fill = TRUE, style = "overflow: hidden;", plotly::plotlyOutput(ns("greedy_chart")))
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            shiny::span("Item Controls"),
            shiny::div(
              shiny::actionButton(ns("items_select_all"), "All",
                                  class = "btn-rdx",
                                  style = "min-width: 60px;"),
              shiny::actionButton(ns("items_deselect_all"), "None",
                                  class = "btn-rdx",
                                  style = "min-width: 60px;")
            )
          ),
          bslib::card_body(
            fillable = TRUE, fill = TRUE,
            reactable::reactableOutput(ns("items_table"))
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            shiny::span("TURF Results"),
            shiny::div(
              shiny::downloadLink(ns("dl_greedy_csv"), "CSV",
                                  class = "btn-rdx",
                                  style = "min-width: 60px;"),
              shiny::downloadLink(ns("dl_greedy_xlsx"), "Excel",
                                  class = "btn-rdx",
                                  style = "min-width: 60px;")
            )
          ),
          bslib::card_body(
            fillable = TRUE, fill = TRUE,
            reactable::reactableOutput(ns("greedy_table"))
          )
        )
      )
    )
  )

  # ---- Best Combos tab ----
  tabs <- list(dashboard_tab)

  if (has_combos) {
    combo_default <- if (2L %in% n_values) 2L else n_values[1]

    combos_sidebar <- bslib::sidebar(
      width = 220,
      fillable = TRUE,
      fill = TRUE,
      shiny::selectInput(ns("bc_subgroup"), "Subgroup:",
                          choices = subgroup_choices,
                          selected = subgroup_names[1]),
      shiny::selectInput(ns("bc_combo_size"), "Combo Size:",
                          choices = n_values,
                          selected = combo_default),
      shiny::numericInput(ns("bc_display"), "Display:", value = 1000, min = 1, max = 50000),
      shiny::selectInput(ns("bc_optimize"), "Optimize:",
                          choices = c("Reach", "Freq")),
      if (has_weights) {
        shiny::selectInput(ns("bc_weighted"), "Weighted:",
                            choices = c("Yes", "No"))
      },
      shiny::selectInput(ns("bc_chart_type"), "Chart:",
                          choices = c("Top Reach", "Reach vs Freq",
                                      "Item Frequency", "None"),
                          selected = "Reach vs Freq"),
      shiny::selectInput(ns("bc_chart_label"), "Display Label:",
                          choices = c("Label", "Variable - Label", "Variable"),
                          selected = "Label"),
      shiny::selectInput(ns("bc_chart_theme"), "Chart Theme:",
                          choices = plotly_theme_names(),
                          selected = "Default"),
      shiny::textInput(ns("bc_base_display"), "Base:", value = ""),
      # Workbook downloads pinned to the BOTTOM of the sidebar — same
      # margin-top:auto flex idiom as the network-drivers Download Workbook.
      shiny::div(
        style = "margin-top: auto;",
        shiny::tags$hr(style = "margin: 12px 0;"),
        shiny::downloadButton(ns("bc_dl_workbook"), "Download Excel",
                              icon = NULL,
                              class = "btn-rdx w-100"),
        shiny::downloadButton(ns("bc_dl_workbook_light"), "Download Excel Light",
                              icon = NULL,
                              class = "btn-rdx w-100 mt-1")
      )
    )

    combos_tab <- bslib::nav_panel(
      "TURF - Best Combo",
      bslib::layout_sidebar(
        fillable = TRUE,
        fill     = TRUE,
        sidebar = combos_sidebar,
        bslib::layout_columns(
          col_widths  = c(12, 12),
          # Second row sizes to its content (NA) so when the chart card
          # is hidden via conditionalPanel (chart_type = "None"), the
          # row collapses away cleanly instead of leaving empty space.
          row_heights = c(1, NA),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center",
              shiny::span("Combo Results"),
              shiny::div(
                shiny::downloadLink(ns("dl_combo_csv"), "CSV",
                                    class = "btn-rdx",
                                    style = "min-width: 60px;"),
                shiny::downloadLink(ns("dl_combo_xlsx"), "Excel",
                                    class = "btn-rdx",
                                    style = "min-width: 60px;")
              )
            ),
            bslib::card_body(
              fillable = TRUE, fill = TRUE,
              reactable::reactableOutput(ns("combo_table"), width = "100%", height = "100%")
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] !== 'None'", ns("bc_chart_type")),
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(shiny::textOutput(ns("bc_chart_title"))),
              bslib::card_body(fillable = TRUE, fill = TRUE, style = "overflow: hidden;", plotly::plotlyOutput(ns("combo_chart")))
            )
          )
        )
      )
    )

    tabs <- c(tabs, list(combos_tab))
  }

  tabs
}


# =============================================================================
# Module Server
# =============================================================================

.turf_module_server <- function(
    input, output, session,
    dark_mode,
    best_combo_results, raw, vars, subgroups,
    weight, label_lookup, col_info, base_sizes,
    n_values, project_name,
    sig_threshold, marginal_threshold
) {

  has_combos <- !is.null(best_combo_results)
  has_weights <- col_info$has_weights
  ns <- session$ns

  # ---- Dark mode override (applies on top of chart theme) ----
  dark_mode_override <- shiny::reactive({
    dark <- dark_mode()
    if (dark) {
      # Match the Network Drivers dark charts — source from the brand's dark
      # viz tokens so this can never drift from resondex_brand() again.
      vd <- resondex_brand("dark")$viz_dark
      list(
        paper_bgcolor = vd$paper_bg,
        plot_bgcolor  = vd$plot_bg,
        font = list(color = vd$font_color),
        xaxis = list(gridcolor = vd$grid),
        yaxis = list(gridcolor = vd$grid)
      )
    } else {
      NULL
    }
  })

  # ---- Reactive values ----
  rv <- shiny::reactiveValues(
    subgroup    = subgroups[1],
    optimize    = "Reach",
    weighted    = "Yes",
    chart_label = "Label",
    chart_theme = "Default",
    combo_size  = if (has_combos) (if (2L %in% n_values) 2L else n_values[1]) else 2L,
    display_n   = 1000L,
    chart_type  = "Reach vs Freq",
    item_include = rep(TRUE, length(vars)),
    syncing     = FALSE
  )


  # ---- Cross-tab sync ----
  shiny::observeEvent(input$subgroup, {
    if (!rv$syncing) {
      rv$syncing <- TRUE
      rv$subgroup <- input$subgroup
      if (has_combos) shiny::updateSelectInput(session, "bc_subgroup", selected = input$subgroup)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$bc_subgroup, {
    if (!rv$syncing) {
      rv$syncing <- TRUE
      rv$subgroup <- input$bc_subgroup
      shiny::updateSelectInput(session, "subgroup", selected = input$bc_subgroup)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$optimize, {
    if (!rv$syncing) {
      rv$syncing <- TRUE
      rv$optimize <- input$optimize
      if (has_combos) shiny::updateSelectInput(session, "bc_optimize", selected = input$optimize)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$bc_optimize, {
    if (!rv$syncing) {
      rv$syncing <- TRUE
      rv$optimize <- input$bc_optimize
      shiny::updateSelectInput(session, "optimize", selected = input$bc_optimize)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  if (has_weights) {
    shiny::observeEvent(input$weighted, {
      if (!rv$syncing) {
        rv$syncing <- TRUE
        rv$weighted <- input$weighted
        if (has_combos) shiny::updateSelectInput(session, "bc_weighted", selected = input$weighted)
        rv$syncing <- FALSE
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$bc_weighted, {
      if (!rv$syncing) {
        rv$syncing <- TRUE
        rv$weighted <- input$bc_weighted
        shiny::updateSelectInput(session, "weighted", selected = input$bc_weighted)
        rv$syncing <- FALSE
      }
    }, ignoreInit = TRUE)
  }

  shiny::observeEvent(input$chart_label, {
    if (!rv$syncing) {
      rv$syncing <- TRUE
      rv$chart_label <- input$chart_label
      if (has_combos) shiny::updateSelectInput(session, "bc_chart_label", selected = input$chart_label)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$bc_chart_label, {
    if (!rv$syncing) {
      rv$syncing <- TRUE
      rv$chart_label <- input$bc_chart_label
      shiny::updateSelectInput(session, "chart_label", selected = input$bc_chart_label)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$chart_theme, {
    if (!rv$syncing) {
      rv$syncing <- TRUE
      rv$chart_theme <- input$chart_theme
      if (has_combos) shiny::updateSelectInput(session, "bc_chart_theme", selected = input$chart_theme)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$bc_chart_theme, {
    if (!rv$syncing) {
      rv$syncing <- TRUE
      rv$chart_theme <- input$bc_chart_theme
      shiny::updateSelectInput(session, "chart_theme", selected = input$bc_chart_theme)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$bc_combo_size, { rv$combo_size <- as.integer(input$bc_combo_size) }, ignoreInit = TRUE)
  shiny::observeEvent(input$bc_display, { rv$display_n <- as.integer(input$bc_display) }, ignoreInit = TRUE)
  shiny::observeEvent(input$bc_chart_type, { rv$chart_type <- input$bc_chart_type }, ignoreInit = TRUE)


  # ---- Filtered matrix (subgroup + included items) ----
  filtered_data <- shiny::reactive({
    sg <- rv$subgroup
    included <- rv$item_include

    if (sg %in% names(raw)) {
      mask <- raw[[sg]] == 1
    } else {
      mask <- rep(TRUE, nrow(raw))
    }

    included_vars <- vars[included]
    if (length(included_vars) == 0) return(NULL)

    mat <- as.matrix(raw[mask, included_vars, drop = FALSE])
    wt <- if (!is.null(weight) && weight %in% names(raw)) {
      raw[[weight]][mask]
    } else {
      rep(1, sum(mask))
    }

    use_weighted <- has_weights && rv$weighted == "Yes"

    list(mat = mat, weights = if (use_weighted) wt else rep(1, length(wt)),
         vars = included_vars, n_resp = sum(mask))
  })


  # ---- Greedy results ----
  greedy_results <- shiny::reactive({
    data <- filtered_data()
    if (is.null(data)) return(NULL)

    .turf_run_greedy(
      mat = data$mat,
      weights = data$weights,
      var_names = data$vars,
      label_lookup = label_lookup,
      optimize_by = tolower(rv$optimize)
    )
  })


  # ---- Base count ----
  base_count <- shiny::reactive({
    unname(base_sizes[rv$subgroup])
  })


  # ---- Filtered combos ----
  filtered_combos <- shiny::reactive({
    if (!has_combos) return(NULL)

    sg <- rv$subgroup
    cs <- rv$combo_size
    nk <- paste0("n_", cs)

    if (is.null(best_combo_results[[sg]])) return(NULL)
    if (is.null(best_combo_results[[sg]][[nk]])) return(NULL)

    tbl <- best_combo_results[[sg]][[nk]]
    included_vars <- vars[rv$item_include]
    use_weighted <- has_weights && rv$weighted == "Yes"

    .turf_filter_combos(
      tbl = tbl,
      included_vars = included_vars,
      use_weighted = use_weighted,
      optimize_by = tolower(rv$optimize),
      display_n = rv$display_n,
      label_lookup = label_lookup
    )
  })


  # ---- Table download handlers (CSV + Excel) ----
  output$dl_greedy_csv <- shiny::downloadHandler(
    filename = function() paste0("TURF Results_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- greedy_results()
      if (!is.null(df)) {
        display <- df %>%
          dplyr::select(step, variable, label,
                        cumul_pct, incr_pct, avg_freq, abs_pct, p_value)
        utils::write.csv(display, file, row.names = FALSE)
      }
    }
  )

  output$dl_greedy_xlsx <- shiny::downloadHandler(
    filename = function() paste0("TURF Results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- greedy_results()
      if (!is.null(df)) {
        display <- df %>%
          dplyr::select(step, variable, label,
                        cumul_pct, incr_pct, avg_freq, abs_pct, p_value)
        openxlsx2::write_xlsx(display, file)
      }
    }
  )

  output$dl_combo_csv <- shiny::downloadHandler(
    filename = function() paste0("TURF Combo Results_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- filtered_combos()
      if (!is.null(df)) {
        display_cols <- grep("^display_\\d+$", names(df), value = TRUE)
        out <- df %>%
          dplyr::select(rank, reach_display, freq_display,
                        dplyr::all_of(display_cols))
        utils::write.csv(out, file, row.names = FALSE)
      }
    }
  )

  output$dl_combo_xlsx <- shiny::downloadHandler(
    filename = function() paste0("TURF Combo Results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- filtered_combos()
      if (!is.null(df)) {
        display_cols <- grep("^display_\\d+$", names(df), value = TRUE)
        out <- df %>%
          dplyr::select(rank, reach_display, freq_display,
                        dplyr::all_of(display_cols))
        openxlsx2::write_xlsx(out, file)
      }
    }
  )

  # ---- Download workbook (turf_write) ----
  .make_workbook_handler <- function(top = NULL) {
    shiny::downloadHandler(
      filename = function() paste0("TURF_Analysis_", Sys.Date(), ".xlsm"),
      content = function(file) {
        tmp_dir <- tempdir()
        args <- list(
          best_combo_results = best_combo_results,
          raw                = raw,
          vars               = vars,
          subgroups          = subgroups,
          weight             = weight,
          labels             = label_lookup,
          file_name          = "TURF_Analysis",
          project_name       = project_name,
          where              = tmp_dir,
          sig_threshold      = sig_threshold,
          marginal_threshold = marginal_threshold
        )
        if (!is.null(top)) args$top <- top
        do.call(turf_write, args)
        # turf_write saves as .xlsm (if template found) or .xlsx
        out_xlsm <- file.path(tmp_dir, "TURF_Analysis.xlsm")
        out_xlsx <- file.path(tmp_dir, "TURF_Analysis.xlsx")
        if (file.exists(out_xlsm)) {
          file.copy(out_xlsm, file, overwrite = TRUE)
        } else if (file.exists(out_xlsx)) {
          file.copy(out_xlsx, file, overwrite = TRUE)
        }
      }
    )
  }
  output$dl_workbook       <- .make_workbook_handler()
  output$bc_dl_workbook    <- .make_workbook_handler()
  output$dl_workbook_light    <- .make_workbook_handler(top = 1000)
  output$bc_dl_workbook_light <- .make_workbook_handler(top = 1000)


  # ---- Outputs: Dashboard ----
  # Update base textInputs (read-only via CSS)
  shiny::observe({
    base_label <- format(base_count(), big.mark = ",")
    shiny::updateTextInput(session, "base_display", value = base_label)
    if (has_combos) {
      shiny::updateTextInput(session, "bc_base_display", value = base_label)
    }
  })

  output$greedy_table <- reactable::renderReactable({
    df <- greedy_results()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    display <- df %>%
      dplyr::select(step, variable, label,
                    cumul_pct, incr_pct, avg_freq, abs_pct, p_value)

    # Per-column max for the white→green scale (uses the showcase's
    # `color-mix(--ndr-success X%, transparent)` pattern, X = frac * 55).
    cumul_max <- 100  # cumulative reach % bounded 0..100
    abs_max   <- 100  # absolute reach % bounded 0..100
    incr_max  <- max(display$incr_pct, na.rm = TRUE)
    freq_max  <- max(display$avg_freq, na.rm = TRUE)
    if (!is.finite(incr_max) || incr_max <= 0) incr_max <- 1
    if (!is.finite(freq_max) || freq_max <= 0) freq_max <- 1

    # White→green background scale (cell style fn). Same color-mix pattern
    # the showcase's Index₂ scale uses, capped at 55% so text stays
    # readable. Brand-tokenized: --ndr-success flips with dark mode.
    scale_bg <- function(max_val) {
      function(value) {
        if (is.na(value) || max_val <= 0) return(NULL)
        frac <- min(1, max(0, value / max_val))
        pct  <- round(frac * 55)
        if (pct == 0) return(NULL)
        list(background = sprintf(
          "color-mix(in srgb, var(--ndr-success) %d%%, transparent)", pct
        ))
      }
    }

    # P-value display: >=1 → "1", <=0.001 → "<.001", <0.01 → "<.01",
    # else 2 decimals. Wrap in <span class="rdx-pval-*"> so the SHARED
    # text-color classes from resondex_css() apply.
    pval_cell <- function(value) {
      if (is.na(value)) return("")
      txt <- if (value >= 1) "1"
        else if (value <= 0.001) "<.001"
        else if (value <  0.01)  "<.01"
        else sprintf("%.2f", value)
      cls <- if (value <  sig_threshold)      "rdx-pval-sig"
        else if (value <  marginal_threshold) "rdx-pval-marg"
        else                                  "rdx-pval-insig"
      htmltools::tags$span(class = cls, txt)
    }

    pct_cell <- function(v) if (is.na(v)) "" else sprintf("%.1f%%", v)
    dec_cell <- function(v) if (is.na(v)) "" else sprintf("%.1f",   v)

    # Reactable column-header tooltip helper. Returns a single <span>
    # with a `data-tip` attribute — the resondex floating tooltip
    # (resondex_tooltip_js, injected via the module head_tags above)
    # catches hover events on any `[data-tip]` element and shows a
    # single shared popup. Same mechanism the network-drivers app
    # uses; replaced Bootstrap's data-bs-toggle="tooltip" path because
    # bslib's auto-init proved unreliable.
    th <- function(label, tip) {
      htmltools::tags$span(
        label,
        `data-tip` = tip,
        style = "border-bottom: 1px dotted var(--ndr-muted); cursor: help;"
      )
    }

    reactable::reactable(
      display,
      pagination = FALSE,
      highlight  = TRUE,
      compact    = TRUE,
      theme      = resondex_reactable_theme(),
      columns = list(
        step      = reactable::colDef(
          name = "#", align = "center",
          header = th("Step", "Order items were optimized")
        ),
        variable  = reactable::colDef(
          name = "Variable", align = "left",
          header = th("Variable", "Name from dataset")
        ),
        label     = reactable::colDef(
          name = "Label", align = "left",
          header = th("Label", "Label for variable")
        ),
        cumul_pct = reactable::colDef(
          name = "Cumul%", align = "center",
          header = th("Cumul%", "Cumulative unduplicated reach (% reached through this step)"),
          cell = pct_cell, style = scale_bg(cumul_max)
        ),
        incr_pct  = reactable::colDef(
          name = "Incr%", align = "center",
          header = th("Incr%", "Incremental reach (% points added by this item beyond previous step)"),
          cell = pct_cell, style = scale_bg(incr_max)
        ),
        avg_freq  = reactable::colDef(
          name = "Avg Freq", align = "center",
          header = th("Avg Freq", "Average frequency among reached respondents (mean items selected per reached respondent)"),
          cell = dec_cell, style = scale_bg(freq_max)
        ),
        abs_pct   = reactable::colDef(
          name = "Abs%", align = "center",
          header = th("Abs%", "Absolute/standalone reach (% who selected this item regardless of other items)"),
          cell = pct_cell, style = scale_bg(abs_max)
        ),
        p_value   = reactable::colDef(
          name = "p-value", align = "center",
          header = th("p-value", "Significance test"),
          cell = pval_cell
        )
      )
    )
  })

  output$greedy_chart <- plotly::renderPlotly({
    df <- greedy_results()
    if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty())
    theme_name <- rv$chart_theme
    colors <- plotly_theme_colors(theme_name)
    pal <- .turf_colors_to_palette(colors, theme_name)
    p <- .turf_chart_greedy(df, rv$chart_label, palette = pal) %>%
      plotly_theme(theme_name)
    dm <- dark_mode_override()
    if (!is.null(dm)) {
      p <- p %>% plotly::layout(
        paper_bgcolor = dm$paper_bgcolor,
        plot_bgcolor  = dm$plot_bgcolor,
        font  = dm$font,
        xaxis = dm$xaxis,
        yaxis = dm$yaxis
      )
    }
    p %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtons = plotly_modebar("TURF Chart")
      )
  })

  # Build items table once — checkbox state managed client-side
  # Use session$ns to build the fully-qualified input name for JS
  chk_input_name <- ns("item_checkbox_change")

  .items_chk <- function(include_vec) {
    purrr::map_chr(seq_along(vars), function(i) {
      checked <- if (include_vec[i]) "checked" else ""
      paste0(
        '<input type="checkbox" ', checked,
        ' onclick="Shiny.setInputValue(\'', chk_input_name, '\', ',
        '{row: ', i, ', checked: this.checked}, {priority: \'event\'})"/>'
      )
    })
  }

  .items_df <- function(include_vec) {
    data.frame(
      Variable = vars,
      Label    = unname(label_lookup[vars]),
      Include  = .items_chk(include_vec),
      stringsAsFactors = FALSE
    )
  }

  output$items_table <- reactable::renderReactable({
    reactable::reactable(
      .items_df(shiny::isolate(rv$item_include)),
      pagination = FALSE,
      highlight  = TRUE,
      compact    = TRUE,
      theme      = resondex_reactable_theme(),
      columns = list(
        Include = reactable::colDef(html = TRUE, align = "center")
      )
    )
  })

  # Individual checkbox clicks — update rv only, no table re-render
  shiny::observeEvent(input$item_checkbox_change, {
    info <- input$item_checkbox_change
    row_idx <- info$row
    checked <- info$checked
    rv$item_include[row_idx] <- checked
  })

  # Select All / Deselect All — update rv + push fresh data via reactable proxy
  shiny::observeEvent(input$items_select_all, {
    rv$item_include <- rep(TRUE, length(vars))
    reactable::updateReactable("items_table", data = .items_df(rv$item_include))
  })

  shiny::observeEvent(input$items_deselect_all, {
    rv$item_include <- rep(FALSE, length(vars))
    reactable::updateReactable("items_table", data = .items_df(rv$item_include))
  })


  # ---- Outputs: Best Combos ----
  if (has_combos) {

    output$bc_chart_title <- shiny::renderText({
      rv$chart_type
    })

    output$combo_table <- reactable::renderReactable({
      df <- filtered_combos()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      # Build display columns
      display_cols <- grep("^display_\\d+$", names(df), value = TRUE)
      item_cols    <- grep("^item_\\d+$",    names(df), value = TRUE)

      # Apply chart_label mode to display columns
      label_mode <- rv$chart_label
      for (i in seq_along(item_cols)) {
        dc <- display_cols[i]
        ic <- item_cols[i]
        df[[dc]] <- switch(label_mode,
          "Variable"         = df[[ic]],
          "Variable - Label" = paste0(df[[ic]], " - ", df[[dc]]),
          df[[dc]]
        )
      }

      out <- df %>% dplyr::select(rank, reach_display, freq_display,
                                   dplyr::all_of(display_cols))

      # Enforce decimal formatting
      out$reach_display <- formatC(round(out$reach_display, 1), format = "f", digits = 1)
      out$freq_display  <- formatC(round(out$freq_display, 1), format = "f", digits = 1)

      # data-tip header helper (same pattern as the greedy table).
      th <- function(label, tip) {
        htmltools::tags$span(
          label,
          `data-tip` = tip,
          style = "border-bottom: 1px dotted var(--ndr-muted); cursor: help;"
        )
      }

      # Dynamic Item N columns (one per display_N column).
      item_col_defs <- stats::setNames(
        lapply(seq_along(display_cols), function(i) {
          reactable::colDef(
            name = paste("Item", i), align = "left",
            header = th(paste("Item", i), "Item in the combo (shown as label)")
          )
        }),
        display_cols
      )

      reactable::reactable(
        out,
        pagination = FALSE,
        highlight  = TRUE,
        compact    = TRUE,
        theme      = resondex_reactable_theme(),
        columns = c(
          list(
            rank          = reactable::colDef(
              name = "Rank", align = "center",
              header = th("Rank", "Combo ranking (1 = best)")
            ),
            reach_display = reactable::colDef(
              name = "Reach%", align = "center",
              header = th("Reach%", "Unduplicated reach (% of respondents selecting at least one item in this combo)")
            ),
            freq_display  = reactable::colDef(
              name = "Freq", align = "center",
              header = th("Frequency", "Average frequency among reached respondents (mean items selected per reached respondent)")
            )
          ),
          item_col_defs
        )
      )
    })

    output$combo_chart <- plotly::renderPlotly({
      df <- filtered_combos()
      if (is.null(df) || nrow(df) == 0 || rv$chart_type == "None") {
        return(plotly::plotly_empty())
      }

      # Apply chart_label mode to display columns for charts
      label_mode <- rv$chart_label
      display_cols <- grep("^display_\\d+$", names(df), value = TRUE)
      item_cols <- grep("^item_\\d+$", names(df), value = TRUE)
      for (i in seq_along(item_cols)) {
        dc <- display_cols[i]
        ic <- item_cols[i]
        df[[dc]] <- switch(label_mode,
          "Variable"         = df[[ic]],
          "Variable - Label" = paste0(df[[ic]], " - ", df[[dc]]),
          df[[dc]]
        )
      }

      theme_name <- rv$chart_theme
      colors <- plotly_theme_colors(theme_name)
      pal <- .turf_colors_to_palette(colors, theme_name)
      p <- switch(rv$chart_type,
        "Top Reach"      = .turf_chart_top_reach(df, palette = pal),
        "Reach vs Freq"  = .turf_chart_reach_vs_freq(df, palette = pal),
        "Item Frequency" = .turf_chart_item_freq(df, rv$combo_size, palette = pal)
      ) %>%
        plotly_theme(theme_name)
      dm <- dark_mode_override()
      if (!is.null(dm)) {
        p <- p %>% plotly::layout(
          paper_bgcolor = dm$paper_bgcolor,
          plot_bgcolor  = dm$plot_bgcolor,
          font  = dm$font,
          xaxis = dm$xaxis,
          yaxis = dm$yaxis
        )
      }
      p %>%
        plotly::config(
          displaylogo = FALSE,
          modeBarButtons = plotly_modebar(
            paste0("TURF Best Combo - ", rv$chart_type)
          )
        )
    })
  }

  # ---- State management ----
  defaults <- list(
    subgroup     = subgroups[1],
    optimize     = "Reach",
    weighted     = "Yes",
    chart_label  = "Label",
    chart_theme  = "Default",
    combo_size   = if (has_combos) (if (2L %in% n_values) 2L else n_values[1]) else 2L,
    display_n    = 1000L,
    chart_type   = "Reach vs Freq",
    item_include = rep(TRUE, length(vars))
  )

  get_state <- function() {
    list(
      subgroup     = rv$subgroup,
      optimize     = rv$optimize,
      weighted     = rv$weighted,
      chart_label  = rv$chart_label,
      chart_theme  = rv$chart_theme,
      combo_size   = rv$combo_size,
      display_n    = rv$display_n,
      chart_type   = rv$chart_type,
      item_include = rv$item_include
    )
  }

  set_state <- function(state) {
    rv$syncing <- TRUE
    on.exit(rv$syncing <- FALSE)

    if (!is.null(state$subgroup) && state$subgroup %in% subgroups) {
      rv$subgroup <- state$subgroup
      shiny::updateSelectInput(session, "subgroup", selected = state$subgroup)
      if (has_combos) shiny::updateSelectInput(session, "bc_subgroup", selected = state$subgroup)
    }
    if (!is.null(state$optimize)) {
      rv$optimize <- state$optimize
      shiny::updateSelectInput(session, "optimize", selected = state$optimize)
      if (has_combos) shiny::updateSelectInput(session, "bc_optimize", selected = state$optimize)
    }
    if (!is.null(state$weighted) && has_weights) {
      rv$weighted <- state$weighted
      shiny::updateSelectInput(session, "weighted", selected = state$weighted)
      if (has_combos) shiny::updateSelectInput(session, "bc_weighted", selected = state$weighted)
    }
    if (!is.null(state$chart_label)) {
      rv$chart_label <- state$chart_label
      shiny::updateSelectInput(session, "chart_label", selected = state$chart_label)
      if (has_combos) shiny::updateSelectInput(session, "bc_chart_label", selected = state$chart_label)
    }
    if (!is.null(state$chart_theme)) {
      rv$chart_theme <- state$chart_theme
      shiny::updateSelectInput(session, "chart_theme", selected = state$chart_theme)
      if (has_combos) shiny::updateSelectInput(session, "bc_chart_theme", selected = state$chart_theme)
    }
    if (!is.null(state$combo_size) && has_combos) {
      rv$combo_size <- as.integer(state$combo_size)
      shiny::updateSelectInput(session, "bc_combo_size", selected = as.character(state$combo_size))
    }
    if (!is.null(state$display_n) && has_combos) {
      rv$display_n <- as.integer(state$display_n)
      shiny::updateNumericInput(session, "bc_display", value = state$display_n)
    }
    if (!is.null(state$chart_type) && has_combos) {
      rv$chart_type <- state$chart_type
      shiny::updateSelectInput(session, "bc_chart_type", selected = state$chart_type)
    }
    if (!is.null(state$item_include) && length(state$item_include) == length(vars)) {
      rv$item_include <- state$item_include
      reactable::updateReactable("items_table", data = .items_df(rv$item_include))
    }
  }

  get_fingerprint <- function() {
    list(vars = vars, subgroups = subgroups, n_values = n_values)
  }

  list(
    get_state       = get_state,
    set_state       = set_state,
    reset           = function() set_state(defaults),
    get_fingerprint = get_fingerprint
  )
}


# =============================================================================
# Greedy algorithm — R port of VBA RunGreedy
# =============================================================================

.turf_run_greedy <- function(mat, weights, var_names, label_lookup,
                              optimize_by = "reach") {

  n_resp <- nrow(mat)
  n_items <- ncol(mat)
  total_weight <- sum(weights)

  if (total_weight == 0 || n_items == 0) {
    return(tibble::tibble(
      step = integer(0), variable = character(0), label = character(0),
      cumul_pct = numeric(0), incr_pct = numeric(0), avg_freq = numeric(0),
      abs_pct = numeric(0), p_value = numeric(0)
    ))
  }

  # Pre-compute per-item weighted sums (constant across iterations)
  item_wt_sums <- as.numeric(crossprod(mat, weights))
  abs_weighted <- item_wt_sums / total_weight * 100
  abs_count <- as.integer(colSums(mat))

  # State vectors
  selected <- logical(n_items)
  reached <- logical(n_resp)
  resp_count <- integer(n_resp)
  cur_reach_wt <- 0
  base_freq_sum <- 0

  # Output accumulators
  out_order <- integer(n_items)
  out_cumul <- numeric(n_items)
  out_incr <- numeric(n_items)
  out_avg_freq <- numeric(n_items)
  out_pval <- numeric(n_items)

  for (k in seq_len(n_items)) {
    # ---- Vectorized candidate evaluation (replaces inner respondent loop) ----
    if (optimize_by == "freq") {
      scores <- base_freq_sum + item_wt_sums
    } else {
      unreached_wt <- weights * (!reached)
      scores <- cur_reach_wt + as.numeric(crossprod(mat, unreached_wt))
    }
    scores[selected] <- -Inf

    best_item <- which.max(scores)
    if (length(best_item) == 0 || scores[best_item] == -Inf) break

    selected[best_item] <- TRUE
    out_order[k] <- best_item

    # ---- P-value (vectorized) ----
    unreached_mask <- !reached
    n_unreached <- sum(unreached_mask)
    n_new <- sum(mat[unreached_mask, best_item])

    if (n_unreached > 0 && n_new > 0) {
      remaining_mask <- !selected
      p0 <- if (any(remaining_mask)) {
        mean(abs_count[remaining_mask]) / n_resp
      } else {
        abs_count[best_item] / n_resp
      }

      if (p0 > 0 && p0 < 1) {
        out_pval[k] <- 1 - stats::pbinom(n_new - 1, n_unreached, p0)
      } else {
        out_pval[k] <- 0
      }
    } else {
      out_pval[k] <- 1
    }

    # ---- Update state (vectorized) ----
    item_col <- mat[, best_item]
    newly_reached <- (item_col == 1L) & (!reached)
    reached <- reached | (item_col == 1L)
    cur_reach_wt <- cur_reach_wt + sum(weights[newly_reached])
    resp_count <- resp_count + item_col
    base_freq_sum <- base_freq_sum + item_wt_sums[best_item]

    out_cumul[k] <- cur_reach_wt / total_weight * 100
    out_incr[k] <- if (k == 1) out_cumul[k] else out_cumul[k] - out_cumul[k - 1]
    out_avg_freq[k] <- if (cur_reach_wt > 0) base_freq_sum / cur_reach_wt else 0
  }

  # Build output tibble
  n_selected <- sum(selected)
  idx <- out_order[seq_len(n_selected)]

  tibble::tibble(
    step      = seq_len(n_selected),
    variable  = var_names[idx],
    label     = unname(label_lookup[var_names[idx]]),
    cumul_pct = round(out_cumul[seq_len(n_selected)], 1),
    incr_pct  = round(out_incr[seq_len(n_selected)], 1),
    avg_freq  = round(out_avg_freq[seq_len(n_selected)], 2),
    abs_pct   = round(abs_weighted[idx], 1),
    p_value   = out_pval[seq_len(n_selected)]
  )
}


# =============================================================================
# Combo filtering
# =============================================================================

.turf_filter_combos <- function(tbl, included_vars, use_weighted,
                                 optimize_by, display_n, label_lookup) {

  item_cols <- grep("^item_\\d+$", names(tbl), value = TRUE)

  if (length(item_cols) == 0 || nrow(tbl) == 0) return(NULL)

  # Filter: keep only rows where ALL items are in included set
  keep <- apply(tbl[, item_cols, drop = FALSE], 1, function(row) {
    all(row %in% included_vars)
  })
  tbl <- tbl[keep, , drop = FALSE]

  if (nrow(tbl) == 0) return(NULL)

  # Select reach/freq columns
  if (use_weighted && "w_reach_pct" %in% names(tbl)) {
    reach_col <- "w_reach_pct"
    freq_col <- "w_freq_avg"
  } else {
    reach_col <- "reach_pct"
    freq_col <- "freq_avg"
  }

  # Sort
  if (optimize_by == "freq") {
    tbl <- tbl[order(-tbl[[freq_col]], -tbl[[reach_col]]), ]
  } else {
    tbl <- tbl[order(-tbl[[reach_col]], -tbl[[freq_col]]), ]
  }

  # Limit
  tbl <- utils::head(tbl, display_n)

  # Add display columns
  tbl$rank <- seq_len(nrow(tbl))
  tbl$reach_display <- tbl[[reach_col]]
  tbl$freq_display <- tbl[[freq_col]]

  # Resolve item labels for display
  for (ic in item_cols) {
    display_col <- gsub("^item_", "display_", ic)
    tbl[[display_col]] <- unname(label_lookup[tbl[[ic]]])
  }

  tbl
}


# =============================================================================
# TURF chart color helper — derives bar/scatter colors from theme colorway
# =============================================================================

.turf_colors_to_palette <- function(colors, theme_name = "Default") {
  # Returns list: bar_base, bar_incr, bar_single, scatter_main, scatter_highlight
  # Derives all colors from the theme's colorway.
  #
  # Empty-colors fallback now pulls from the brand viz palette
  # (resondex_brand()$viz) so even an unknown theme stays on-brand.
  # Previously the "Default" special-case here forced hardcoded
  # `#D9D9D9 / #595959 / #4472C4 / #ED7D31` — that made sense back when
  # "Default" was a no-op; now "Default" IS the branded theme with its
  # own colorway, so it falls through and derives from the brand.
  if (length(colors) == 0) {
    v <- resondex_brand()$viz
    return(list(
      bar_base          = v$bar_base,
      bar_incr          = v$bar_incr,
      bar_single        = v$bar_incr,
      scatter_main      = v$colorway[1],
      scatter_highlight = v$colorway[4]
    ))
  }

  # Detect if dark theme (crude heuristic — covers the paired dark
  # variants of every theme with a dark version).
  is_dark <- grepl("Dark|Unica|Monokai|Dotabuff|Alone|Superheroes|plotly_dark",
                   theme_name, ignore.case = FALSE)

  # Derive bar_base from the primary color — muted version for stacked bar "previous" segment
  bar_base <- .turf_mute_color(colors[1], is_dark)

  bar_incr <- colors[1]
  bar_single <- if (length(colors) >= 2) colors[2] else colors[1]
  scatter_main <- if (length(colors) >= 2) colors[2] else colors[1]
  # Highlight the top-rank marker with a clearly contrasting tone — use
  # the 4th colorway entry when available. For Default this is orange
  # (#C2773C), unmistakably different from the slate-blue scatter_main
  # (#5B7E92) at any size. Falls back through colors[3] → colors[1] for
  # themes with shorter colorways.
  scatter_highlight <- if (length(colors) >= 4) colors[4]
    else if (length(colors) >= 3) colors[3]
    else colors[1]

  list(
    bar_base = bar_base, bar_incr = bar_incr,
    bar_single = bar_single, scatter_main = scatter_main,
    scatter_highlight = scatter_highlight
  )
}


.turf_mute_color <- function(hex, is_dark = FALSE) {
  # Convert hex to RGB, then desaturate and shift toward background
  # Dark themes: darken + desaturate; Light themes: lighten + desaturate
  rgb_val <- grDevices::col2rgb(hex)[, 1] / 255
  # Convert to HSV
  hsv_val <- grDevices::rgb2hsv(rgb_val[1], rgb_val[2], rgb_val[3])
  h <- hsv_val[1, 1]
  s <- hsv_val[2, 1]
  v <- hsv_val[3, 1]

  if (is_dark) {
    # Dark theme: reduce saturation to 30%, pull value down to ~0.35
    s_new <- s * 0.30
    v_new <- 0.30 + (v - 0.30) * 0.15
  } else {
    # Light theme: reduce saturation to 25%, push value up to ~0.88
    s_new <- s * 0.25
    v_new <- 0.88 + (v - 0.88) * 0.10
  }

  s_new <- max(0, min(1, s_new))
  v_new <- max(0, min(1, v_new))
  grDevices::hsv(h, s_new, v_new)
}


# =============================================================================
# Charts
# =============================================================================

.turf_chart_greedy <- function(greedy_df, chart_label_mode, palette = NULL) {
  if (is.null(palette)) palette <- .turf_colors_to_palette(character(0))
  if (is.null(greedy_df) || nrow(greedy_df) == 0) return(plotly::plotly_empty())

  # Build display labels
  display_labels <- switch(chart_label_mode,
    "Variable"         = greedy_df$variable,
    "Variable - Label" = paste0(greedy_df$variable, " - ", greedy_df$label),
    greedy_df$label
  )
  display_labels <- ifelse(nchar(display_labels) > 50,
                           paste0(substr(display_labels, 1, 47), "..."),
                           display_labels)
  display_labels <- paste0(display_labels, "  ")

  # Compute prev_cumul (base portion)
  prev_cumul <- greedy_df$cumul_pct - greedy_df$incr_pct

  # Factor with reversed levels so step 1 is at top
  y_factor <- factor(display_labels, levels = rev(display_labels))

  plotly::plot_ly() %>%
    plotly::add_bars(
      y = y_factor, x = prev_cumul,
      name = "Previous", orientation = "h",
      marker = list(color = palette$bar_base),
      hoverinfo = "skip"
    ) %>%
    plotly::add_bars(
      y = y_factor, x = greedy_df$incr_pct,
      name = "Incremental", orientation = "h",
      marker = list(color = palette$bar_incr),
      text = paste0(round(greedy_df$cumul_pct, 1), "%"),
      textposition = "outside",
      hovertemplate = paste0(
        "Step %{customdata}<br>",
        "Incr: %{x:.1f}%<br>",
        "Cumul: ", round(greedy_df$cumul_pct, 1), "%",
        "<extra></extra>"
      ),
      customdata = greedy_df$step
    ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "", range = c(0, 105), ticksuffix = "%",
                   tickvals = seq(0, 100, 20)),
      yaxis = list(title = ""),
      showlegend = FALSE,
      margin = list(l = 200)
    )
}


.turf_chart_top_reach <- function(combo_df, n_show = 20, palette = NULL) {
  if (is.null(palette)) palette <- .turf_colors_to_palette(character(0))
  if (is.null(combo_df) || nrow(combo_df) == 0) return(plotly::plotly_empty())

  df <- utils::head(combo_df, n_show)
  labels <- paste0("#", df$rank)

  plotly::plot_ly(
    y = factor(labels, levels = rev(labels)),
    x = df$reach_display,
    type = "bar", orientation = "h",
    marker = list(color = palette$bar_single),
    text = paste0(round(df$reach_display, 1), "%"),
    textposition = "outside",
    customdata = df$rank,
    hovertemplate = "Rank %{customdata}<br>Reach: %{x:.1f}%<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "Reach %"),
      yaxis = list(title = ""),
      showlegend = FALSE
    )
}


.turf_chart_reach_vs_freq <- function(combo_df, n_show = 50, palette = NULL) {
  if (is.null(palette)) palette <- .turf_colors_to_palette(character(0))
  if (is.null(combo_df) || nrow(combo_df) == 0) return(plotly::plotly_empty())

  df <- utils::head(combo_df, n_show)

  colors <- c(palette$scatter_highlight, rep(palette$scatter_main, max(0, nrow(df) - 1)))
  sizes <- c(12, rep(6, max(0, nrow(df) - 1)))

  plotly::plot_ly(
    x = df$reach_display, y = df$freq_display,
    type = "scatter", mode = "markers",
    marker = list(color = colors, size = sizes),
    text = as.character(df$rank),
    hovertemplate = "Rank %{text}<br>Reach: %{x:.1f}%<br>Freq: %{y:.2f}<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "Reach %", ticksuffix = "%"),
      yaxis = list(title = "Avg Freq"),
      showlegend = FALSE
    )
}


.turf_chart_item_freq <- function(combo_df, combo_size, n_show = 100, palette = NULL) {
  if (is.null(palette)) palette <- .turf_colors_to_palette(character(0))
  if (is.null(combo_df) || nrow(combo_df) == 0) return(plotly::plotly_empty())

  df <- utils::head(combo_df, n_show)
  display_cols <- grep("^display_\\d+$", names(df), value = TRUE)

  # Count item appearances
  all_items <- unlist(df[, display_cols], use.names = FALSE)
  all_items <- all_items[!is.na(all_items) & nchar(all_items) > 0]
  counts <- sort(table(all_items), decreasing = TRUE)

  if (length(counts) > 20) counts <- counts[1:20]

  labels <- names(counts)
  labels <- ifelse(nchar(labels) > 40,
                   paste0(substr(labels, 1, 37), "..."),
                   labels)

  plotly::plot_ly(
    y = factor(labels, levels = rev(labels)),
    x = as.integer(counts),
    type = "bar", orientation = "h",
    marker = list(color = palette$bar_single),
    text = as.integer(counts),
    textposition = "outside",
    hovertemplate = "%{y}<br>Count: %{x}<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "Appearances in Combos"),
      yaxis = list(title = ""),
      showlegend = FALSE,
      margin = list(l = 200)
    )
}
