#' turf_app
#' @description Launches an interactive Shiny dashboard replicating the TURF
#'   Excel workbook. Takes pre-computed \code{turf_best_combo()} results and
#'   the raw respondent data, then runs the greedy algorithm and combo filtering
#'   reactively in-browser.
#'
#' @param best_combo_results Optional. Output from \code{turf_best_combo()}.
#'   Accepts any of the 4 return shapes (single tibble, named list by n,
#'   named list by subgroup, nested list). If \code{NULL}, the Best Combos
#'   tab is hidden.
#' @param raw Data frame. The original respondent-level data with binary item
#'   columns, weight column, and subgroup columns.
#' @param vars Character vector. Binary item column names in \code{raw}.
#' @param subgroups Character vector. Subgroup column names (binary 0/1). NULL
#'   creates a "Total" column.
#' @param weight Character. Weight column name in \code{raw}. NULL = unweighted.
#' @param labels Data frame with \code{variable}/\code{label} columns, or a
#'   named character vector.
#' @param sig_threshold Numeric. P-value threshold for green significance.
#'   Default \code{0.10}.
#' @param marginal_threshold Numeric. P-value threshold for orange marginal.
#'   Default \code{0.20}.
#'
#' @examples
#' \dontrun{
#' turf_results <- turf_best_combo(
#'   df        = example_data_ice_cream,
#'   vars      = example_data_ice_cream_dictionary$variable,
#'   n         = 1:3,
#'   subgroups = c("Total", "Gen_Z", "Millennials", "Gen_X"),
#'   labels    = example_data_ice_cream_dictionary,
#'   weight    = "weight"
#' )
#'
#' turf_app(
#'   best_combo_results = turf_results,
#'   raw       = example_data_ice_cream,
#'   vars      = example_data_ice_cream_dictionary$variable,
#'   subgroups = c("Total", "Gen_Z", "Millennials", "Gen_X"),
#'   weight    = "weight",
#'   labels    = example_data_ice_cream_dictionary
#' )
#' }
#'
#' @export
turf_app <- function(
    best_combo_results = NULL, raw, vars,
    subgroups = NULL, weight = NULL, labels = NULL,
    sig_threshold = 0.10, marginal_threshold = 0.20
) {

  # ---- Pre-process (reuse turf_write helpers) ----
  label_lookup <- .turf_build_label_lookup(vars, labels)

  has_combos <- !is.null(best_combo_results)

  if(has_combos){
    normalized <- .turf_normalize(best_combo_results)
    subgroup_names <- names(normalized)
    n_keys <- names(normalized[[1]])
    n_values <- as.integer(gsub("^n_", "", n_keys))
    first_tbl <- normalized[[1]][[1]]
    col_info <- .turf_detect_columns(first_tbl)
  } else {
    normalized <- NULL
    if(!is.null(subgroups)){
      subgroup_names <- subgroups
    } else {
      subgroup_names <- "Total"
    }
    n_values <- integer(0)
    col_info <- list(has_labels = FALSE, has_weights = !is.null(weight),
                     item_cols = character(0), label_cols = character(0), n_items = 0)
  }

  if(is.null(weight)) col_info$has_weights <- FALSE

  # Ensure "Total" column exists in raw if needed
  if(is.null(subgroups)){
    raw$Total <- 1L
    subgroup_names <- "Total"
  }

  base_sizes <- .turf_compute_bases(raw, subgroups, subgroup_names)

  # ---- Build and run app ----
  ui <- turf_app_ui(
    has_combos = has_combos,
    has_weights = col_info$has_weights,
    subgroup_names = subgroup_names,
    n_values = n_values
  )

  server <- function(input, output, session){
    turf_app_server(
      input, output, session,
      best_combo_results = normalized,
      raw = raw, vars = vars,
      subgroups = subgroup_names,
      weight = weight,
      label_lookup = label_lookup,
      col_info = col_info,
      base_sizes = base_sizes,
      n_values = n_values,
      sig_threshold = sig_threshold,
      marginal_threshold = marginal_threshold
    )
  }

  shiny::shinyApp(ui, server)
}


# =============================================================================
# UI
# =============================================================================

#' turf_app_ui
#' @description UI component for the TURF Shiny dashboard.
#' @param has_combos Logical. Whether Best Combos tab should be shown.
#' @param has_weights Logical. Whether weighted controls should be shown.
#' @param subgroup_names Character vector. Subgroup choices.
#' @param n_values Integer vector. Available combo sizes.
#' @export
turf_app_ui <- function(has_combos, has_weights, subgroup_names, n_values){

  # ---- Dashboard tab ----
  dashboard_sidebar <- bslib::sidebar(
    width = 220,
    shiny::selectInput("subgroup", "Subgroup:",
                        choices = subgroup_names,
                        selected = subgroup_names[1]),
    shiny::selectInput("optimize", "Optimize:",
                        choices = c("Reach", "Freq")),
    shiny::selectInput("chart_label", "Chart Label:",
                        choices = c("Label", "Variable - Label", "Variable"),
                        selected = "Label"),
    if(has_weights){
      shiny::selectInput("weighted", "Weighted:",
                          choices = c("Yes", "No"))
    },
    shiny::tags$hr(),
    shiny::textInput("base_display", "Base:", value = "")
  )

  dashboard_tab <- bslib::nav_panel(
    "Dashboard",
    bslib::layout_sidebar(
      sidebar = dashboard_sidebar,
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("TURF Chart"),
          bslib::card_body(plotly::plotlyOutput("greedy_chart", height = "500px"))
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              shiny::span("Item Controls"),
              shiny::div(
                shiny::actionButton("items_select_all", "All", class = "btn-sm btn-outline-secondary"),
                shiny::actionButton("items_deselect_all", "None", class = "btn-sm btn-outline-secondary")
              )
            )
          ),
          bslib::card_body(
            fillable = TRUE, fill = TRUE,
            DT::DTOutput("items_table", width = "100%", height = "100%")
          )
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("TURF Results"),
        bslib::card_body(
          fillable = TRUE, fill = TRUE,
          DT::DTOutput("greedy_table", width = "100%", height = "100%")
        )
      )
    )
  )

  # ---- Best Combos tab ----
  if(has_combos){
    combo_default <- if(2L %in% n_values) 2L else n_values[1]

    combos_sidebar <- bslib::sidebar(
      width = 220,
      shiny::selectInput("bc_subgroup", "Subgroup:",
                          choices = subgroup_names,
                          selected = subgroup_names[1]),
      shiny::selectInput("bc_combo_size", "Combo Size:",
                          choices = n_values,
                          selected = combo_default),
      shiny::numericInput("bc_display", "Display:", value = 1000, min = 1, max = 50000),
      shiny::selectInput("bc_optimize", "Optimize:",
                          choices = c("Reach", "Freq")),
      if(has_weights){
        shiny::selectInput("bc_weighted", "Weighted:",
                            choices = c("Yes", "No"))
      },
      shiny::selectInput("bc_chart_type", "Chart:",
                          choices = c("Top Reach", "Reach vs Freq",
                                      "Item Frequency", "None"),
                          selected = "Top Reach"),
      shiny::tags$hr(),
      shiny::textInput("bc_base_display", "Base:", value = "")
    )

    combos_tab <- bslib::nav_panel(
      "Best Combos",
      bslib::layout_sidebar(
        sidebar = combos_sidebar,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Combo Results"),
          bslib::card_body(DT::DTOutput("combo_table"))
        ),
        shiny::conditionalPanel(
          condition = "input.bc_chart_type !== 'None'",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(shiny::textOutput("bc_chart_title")),
            bslib::card_body(plotly::plotlyOutput("combo_chart", height = "500px"))
          )
        )
      )
    )
  }

  # ---- Assemble page ----
  tabs <- list(dashboard_tab)
  if(has_combos) tabs <- c(tabs, list(combos_tab))

  do.call(
    bslib::page_navbar,
    c(
      list(
        title = "TURF Analysis",
        theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
        header = shiny::tags$head(
          shiny::tags$style(shiny::HTML(
            "#base_display, #bc_base_display {",
            "  pointer-events: none;",
            "  background-color: var(--bs-secondary-bg);",
            "}",
            "#dl_greedy_csv, #dl_combo_csv { display: none; }"
          )),
          shiny::downloadLink("dl_greedy_csv", ""),
          shiny::downloadLink("dl_combo_csv", "")
        )
      ),
      tabs,
      list(bslib::nav_spacer(), bslib::nav_item(bslib::input_dark_mode(id = "dark_mode")))
    )
  )
}


# =============================================================================
# Server
# =============================================================================

#' turf_app_server
#' @description Server component for the TURF Shiny dashboard.
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param best_combo_results Normalized combo results (list of lists of tibbles).
#' @param raw Data frame.
#' @param vars Character vector of item variable names.
#' @param subgroups Character vector of subgroup names.
#' @param weight Character weight column name or NULL.
#' @param label_lookup Named character vector (var -> label).
#' @param col_info List with has_labels, has_weights.
#' @param base_sizes Named integer vector.
#' @param n_values Integer vector of combo sizes.
#' @param sig_threshold Numeric.
#' @param marginal_threshold Numeric.
#' @export
turf_app_server <- function(
    input, output, session,
    best_combo_results, raw, vars, subgroups,
    weight, label_lookup, col_info, base_sizes,
    n_values, sig_threshold, marginal_threshold
) {

  has_combos <- !is.null(best_combo_results)
  has_weights <- col_info$has_weights

  # ---- Plotly theme (reacts to dark mode toggle) ----
  plotly_theme <- shiny::reactive({
    dark <- isTRUE(input$dark_mode == "dark")
    if(dark){
      list(
        paper_bgcolor = "#2c3034",
        plot_bgcolor  = "#2c3034",
        font_color    = "#dee2e6",
        grid_color    = "#495057"
      )
    } else {
      list(
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#ffffff",
        font_color    = "#2c3e50",
        grid_color    = "#ecf0f1"
      )
    }
  })

  # ---- Reactive values ----
  rv <- shiny::reactiveValues(
    subgroup    = subgroups[1],
    optimize    = "Reach",
    weighted    = "Yes",
    chart_label = "Label",
    combo_size  = if(has_combos) (if(2L %in% n_values) 2L else n_values[1]) else 2L,
    display_n   = 1000L,
    chart_type  = "Top Reach",
    item_include = rep(TRUE, length(vars)),
    syncing     = FALSE
  )


  # ---- Cross-tab sync ----
  shiny::observeEvent(input$subgroup, {
    if(!rv$syncing){
      rv$syncing <- TRUE
      rv$subgroup <- input$subgroup
      if(has_combos) shiny::updateSelectInput(session, "bc_subgroup", selected = input$subgroup)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$bc_subgroup, {
    if(!rv$syncing){
      rv$syncing <- TRUE
      rv$subgroup <- input$bc_subgroup
      shiny::updateSelectInput(session, "subgroup", selected = input$bc_subgroup)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$optimize, {
    if(!rv$syncing){
      rv$syncing <- TRUE
      rv$optimize <- input$optimize
      if(has_combos) shiny::updateSelectInput(session, "bc_optimize", selected = input$optimize)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$bc_optimize, {
    if(!rv$syncing){
      rv$syncing <- TRUE
      rv$optimize <- input$bc_optimize
      shiny::updateSelectInput(session, "optimize", selected = input$bc_optimize)
      rv$syncing <- FALSE
    }
  }, ignoreInit = TRUE)

  if(has_weights){
    shiny::observeEvent(input$weighted, {
      if(!rv$syncing){
        rv$syncing <- TRUE
        rv$weighted <- input$weighted
        if(has_combos) shiny::updateSelectInput(session, "bc_weighted", selected = input$weighted)
        rv$syncing <- FALSE
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$bc_weighted, {
      if(!rv$syncing){
        rv$syncing <- TRUE
        rv$weighted <- input$bc_weighted
        shiny::updateSelectInput(session, "weighted", selected = input$bc_weighted)
        rv$syncing <- FALSE
      }
    }, ignoreInit = TRUE)
  }

  shiny::observeEvent(input$chart_label, { rv$chart_label <- input$chart_label }, ignoreInit = TRUE)
  shiny::observeEvent(input$bc_combo_size, { rv$combo_size <- as.integer(input$bc_combo_size) }, ignoreInit = TRUE)
  shiny::observeEvent(input$bc_display, { rv$display_n <- as.integer(input$bc_display) }, ignoreInit = TRUE)
  shiny::observeEvent(input$bc_chart_type, { rv$chart_type <- input$bc_chart_type }, ignoreInit = TRUE)


  # ---- Item include/exclude ----
  shiny::observeEvent(input$items_select_all, {
    rv$item_include <- rep(TRUE, length(vars))
  })

  shiny::observeEvent(input$items_deselect_all, {
    rv$item_include <- rep(FALSE, length(vars))
  })

  # Observe checkbox changes from items table (sent via JS callback)
  shiny::observeEvent(input$item_checkbox_change, {
    info <- input$item_checkbox_change
    row_idx <- info$row
    checked <- info$checked
    rv$item_include[row_idx] <- checked
  })


  # ---- Filtered matrix (subgroup + included items) ----
  filtered_data <- shiny::reactive({
    sg <- rv$subgroup
    included <- rv$item_include

    if(sg %in% names(raw)){
      mask <- raw[[sg]] == 1
    } else {
      mask <- rep(TRUE, nrow(raw))
    }

    included_vars <- vars[included]
    if(length(included_vars) == 0) return(NULL)

    mat <- as.matrix(raw[mask, included_vars, drop = FALSE])
    wt <- if(!is.null(weight) && weight %in% names(raw)){
      raw[[weight]][mask]
    } else {
      rep(1, sum(mask))
    }

    use_weighted <- has_weights && rv$weighted == "Yes"

    list(mat = mat, weights = if(use_weighted) wt else rep(1, length(wt)),
         vars = included_vars, n_resp = sum(mask))
  })


  # ---- Greedy results ----
  greedy_results <- shiny::reactive({
    data <- filtered_data()
    if(is.null(data)) return(NULL)

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
    if(!has_combos) return(NULL)

    sg <- rv$subgroup
    cs <- rv$combo_size
    nk <- paste0("n_", cs)

    if(is.null(best_combo_results[[sg]])) return(NULL)
    if(is.null(best_combo_results[[sg]][[nk]])) return(NULL)

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


  # ---- CSV download handlers ----
  output$dl_greedy_csv <- shiny::downloadHandler(
    filename = function() paste0("turf_greedy_", Sys.Date(), ".csv"),
    content = function(file){
      df <- greedy_results()
      if(!is.null(df)) utils::write.csv(df, file, row.names = FALSE)
    }
  )

  output$dl_combo_csv <- shiny::downloadHandler(
    filename = function() paste0("turf_combos_", Sys.Date(), ".csv"),
    content = function(file){
      df <- filtered_combos()
      if(!is.null(df)) utils::write.csv(df, file, row.names = FALSE)
    }
  )


  # ---- Outputs: Dashboard ----
  # Update base textInputs (read-only via CSS)
  shiny::observe({
    base_label <- format(base_count(), big.mark = ",")
    shiny::updateTextInput(session, "base_display", value = base_label)
    if(has_combos){
      shiny::updateTextInput(session, "bc_base_display", value = base_label)
    }
  })

  output$greedy_table <- DT::renderDT({
    df <- greedy_results()
    if(is.null(df) || nrow(df) == 0) return(NULL)

    display <- df %>%
      dplyr::select(step, variable, label,
                    cumul_pct, incr_pct, avg_freq, abs_pct, p_value) %>%
      dplyr::mutate(
        cumul_pct = formatC(round(cumul_pct, 1), format = "f", digits = 1),
        incr_pct  = formatC(round(incr_pct, 1), format = "f", digits = 1),
        abs_pct   = formatC(round(abs_pct, 1), format = "f", digits = 1),
        avg_freq  = formatC(round(avg_freq, 1), format = "f", digits = 1)
      )

    # p-value JS render: >=1 → "1", <=0.001 → ".001", <0.01 → "<.01", else 2 decimals
    pval_render <- DT::JS(
      "function(data, type, row, meta) {",
      "  if (type !== 'display') return data;",
      "  if (data >= 1) return '1';",
      "  if (data <= 0.001) return '<.001';",
      "  if (data < 0.01) return '<.01';",
      "  return data.toFixed(2);",
      "}"
    )

    DT::datatable(
      display,
      colnames = c("#", "Variable", "Label", "Cumul%", "Incr%",
                    "Avg Freq", "Abs%", "p-value"),
      selection = "none",
      options = list(
        pageLength = nrow(display),
        dom = "t",
        scrollY = "100%",
        scrollCollapse = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = c(0, 3, 4, 5, 6, 7)),
          list(targets = 7, render = pval_render)
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        c("step", "cumul_pct", "incr_pct", "avg_freq", "abs_pct", "p_value"),
        `text-align` = "center"
      ) %>%
      DT::formatStyle(
        "p_value",
        color = DT::styleInterval(
          c(sig_threshold, marginal_threshold),
          c("#198754", "#E67E22", "#DC3545")
        ),
        fontWeight = DT::styleInterval(
          c(sig_threshold),
          c("bold", "normal")
        )
      )
  })

  output$greedy_chart <- plotly::renderPlotly({
    df <- greedy_results()
    if(is.null(df) || nrow(df) == 0) return(plotly::plotly_empty())
    th <- plotly_theme()
    .turf_chart_greedy(df, rv$chart_label) %>%
      plotly::layout(
        paper_bgcolor = th$paper_bgcolor,
        plot_bgcolor  = th$plot_bgcolor,
        font = list(color = th$font_color),
        xaxis = list(gridcolor = th$grid_color),
        yaxis = list(gridcolor = th$grid_color)
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtons = list(list(
          list(
            name = "Download PNG",
            icon = list(
              path = paste0(
                "M3 4V1h2v3h3v2H5v3H3V6H0V4h3zm3 6V7h3V4h7l1.83 ",
                "2H21c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H5c-1.1 ",
                "0-2-.9-2-2V10h3zm7 9c2.76 0 5-2.24 5-5s-2.24-5-5-5",
                "-5 2.24-5 5 2.24 5 5 5zm-3.2-5c0 1.77 1.43 3.2 ",
                "3.2 3.2s3.2-1.43 3.2-3.2-1.43-3.2-3.2-3.2-3.2 1.43-3.2 3.2z"
              ),
              width = 24, height = 24
            ),
            click = htmlwidgets::JS(
              "function(gd) {",
              "  Plotly.downloadImage(gd, {format: 'png', filename: 'turf_chart'});",
              "}"
            )
          ),
          list(
            name = "Download SVG",
            icon = list(
              path = "M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z",
              width = 24, height = 24
            ),
            click = htmlwidgets::JS(
              "function(gd) {",
              "  Plotly.downloadImage(gd, {format: 'svg', filename: 'turf_chart'});",
              "}"
            )
          ),
          list(
            name = "Download CSV",
            icon = list(
              path = paste0(
                "M14 2H6c-1.1 0-2 .9-2 2v16c0 1.1.9 2 2 2h12c1.1 0 ",
                "2-.9 2-2V8l-6-6zm4 18H6V4h7v5h5v11zm-5-6v4h-2v-4H9l3-3 3 3h-2z"
              ),
              width = 24, height = 24
            ),
            click = htmlwidgets::JS(
              "function(gd) {",
              "  document.getElementById('dl_greedy_csv').click();",
              "}"
            )
          ),
          "zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d"
        ))
      )
  })

  output$items_table <- DT::renderDT({
    # Build checkbox HTML — plain inputs with onclick sending to Shiny
    chk_html <- purrr::map_chr(seq_along(vars), function(i){
      checked <- if(rv$item_include[i]) "checked" else ""
      paste0(
        '<input type="checkbox" ', checked,
        ' onclick="Shiny.setInputValue(\'item_checkbox_change\', ',
        '{row: ', i, ', checked: this.checked}, {priority: \'event\'})"/>'
      )
    })

    df <- data.frame(
      Variable = vars,
      Label    = unname(label_lookup[vars]),
      Include  = chk_html,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      df,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = length(vars),
        dom = "t",
        scrollY = "100%",
        scrollCollapse = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = 2)
        )
      ),
      rownames = FALSE
    )
  })


  # ---- Outputs: Best Combos ----
  if(has_combos){

    output$bc_chart_title <- shiny::renderText({
      rv$chart_type
    })

    output$combo_table <- DT::renderDT({
      df <- filtered_combos()
      if(is.null(df) || nrow(df) == 0) return(NULL)

      # Build display columns
      display_cols <- grep("^display_\\d+$", names(df), value = TRUE)
      item_cols <- grep("^item_\\d+$", names(df), value = TRUE)
      n_combo <- length(item_cols)

      out <- df %>% dplyr::select(rank, reach_display, freq_display,
                                   dplyr::all_of(display_cols))

      col_names <- c("Rank", "Reach%", "Freq",
                      paste("Item", seq_len(n_combo)))

      # Enforce decimal formatting
      out$reach_display <- formatC(round(out$reach_display, 1), format = "f", digits = 1)
      out$freq_display  <- formatC(round(out$freq_display, 1), format = "f", digits = 1)

      DT::datatable(
        out,
        colnames = col_names,
        selection = "none",
        options = list(
          pageLength = 50,
          scrollY = "500px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = c(0, 1, 2))
          )
        ),
        rownames = FALSE
      )
    })

    output$combo_chart <- plotly::renderPlotly({
      df <- filtered_combos()
      if(is.null(df) || nrow(df) == 0 || rv$chart_type == "None"){
        return(plotly::plotly_empty())
      }

      th <- plotly_theme()
      p <- switch(rv$chart_type,
        "Top Reach"      = .turf_chart_top_reach(df),
        "Reach vs Freq"  = .turf_chart_reach_vs_freq(df),
        "Item Frequency" = .turf_chart_item_freq(df, rv$combo_size)
      )
      p %>%
        plotly::layout(
          paper_bgcolor = th$paper_bgcolor,
          plot_bgcolor  = th$plot_bgcolor,
          font = list(color = th$font_color),
          xaxis = list(gridcolor = th$grid_color),
          yaxis = list(gridcolor = th$grid_color)
        ) %>%
        plotly::config(
          modeBarButtons = list(list(
            list(
              name = "Download PNG",
              icon = list(
                path = paste0(
                  "M3 4V1h2v3h3v2H5v3H3V6H0V4h3zm3 6V7h3V4h7l1.83 ",
                  "2H21c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H5c-1.1 ",
                  "0-2-.9-2-2V10h3zm7 9c2.76 0 5-2.24 5-5s-2.24-5-5-5",
                  "-5 2.24-5 5 2.24 5 5 5zm-3.2-5c0 1.77 1.43 3.2 ",
                  "3.2 3.2s3.2-1.43 3.2-3.2-1.43-3.2-3.2-3.2-3.2 1.43-3.2 3.2z"
                ),
                width = 24, height = 24
              ),
              click = htmlwidgets::JS(
                "function(gd) {",
                "  Plotly.downloadImage(gd, {format: 'png', filename: 'turf_chart'});",
                "}"
              )
            ),
            list(
              name = "Download SVG",
              icon = list(
                path = "M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z",
                width = 24, height = 24
              ),
              click = htmlwidgets::JS(
                "function(gd) {",
                "  Plotly.downloadImage(gd, {format: 'svg', filename: 'turf_chart'});",
                "}"
              )
            ),
            list(
              name = "Download CSV",
              icon = list(
                path = paste0(
                  "M14 2H6c-1.1 0-2 .9-2 2v16c0 1.1.9 2 2 2h12c1.1 0 ",
                  "2-.9 2-2V8l-6-6zm4 18H6V4h7v5h5v11zm-5-6v4h-2v-4H9l3-3 3 3h-2z"
                ),
                width = 24, height = 24
              ),
              click = htmlwidgets::JS(
                "function(gd) {",
                "  document.getElementById('dl_combo_csv').click();",
                "}"
              )
            ),
            "zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d"
          ))
        )
    })
  }
}


# =============================================================================
# Greedy algorithm — R port of VBA RunGreedy
# =============================================================================

.turf_run_greedy <- function(mat, weights, var_names, label_lookup,
                              optimize_by = "reach") {

  n_resp <- nrow(mat)
  n_items <- ncol(mat)
  total_weight <- sum(weights)

  if(total_weight == 0 || n_items == 0){
    return(tibble::tibble(
      step = integer(0), variable = character(0), label = character(0),
      cumul_pct = numeric(0), incr_pct = numeric(0), avg_freq = numeric(0),
      abs_pct = numeric(0), p_value = numeric(0)
    ))
  }

  # Pre-compute standalone weighted reach and unweighted count per item
  abs_weighted <- numeric(n_items)
  abs_count <- integer(n_items)
  for(j in seq_len(n_items)){
    hit <- mat[, j] == 1
    abs_weighted[j] <- sum(weights[hit]) / total_weight * 100
    abs_count[j] <- sum(hit)
  }

  # State vectors
  selected <- logical(n_items)
  reached <- logical(n_resp)
  resp_count <- integer(n_resp)
  cur_reach_wt <- 0

  # Output accumulators
  out_order <- integer(n_items)
  out_cumul <- numeric(n_items)
  out_incr <- numeric(n_items)
  out_avg_freq <- numeric(n_items)
  out_pval <- numeric(n_items)

  for(k in seq_len(n_items)){
    best_item <- 0L
    best_score <- -1

    # Evaluate each unselected item
    for(j in seq_len(n_items)){
      if(selected[j]) next

      candidate_reach_wt <- cur_reach_wt
      candidate_freq_sum <- 0

      for(i in seq_len(n_resp)){
        if(mat[i, j] == 1 && !reached[i]){
          candidate_reach_wt <- candidate_reach_wt + weights[i]
        }
        resp_total <- resp_count[i] + mat[i, j]
        if(resp_total > 0){
          candidate_freq_sum <- candidate_freq_sum + resp_total * weights[i]
        }
      }

      score <- if(optimize_by == "freq"){
        candidate_freq_sum / total_weight
      } else {
        candidate_reach_wt
      }

      if(score > best_score){
        best_score <- score
        best_item <- j
      }
    }

    if(best_item == 0L) break

    selected[best_item] <- TRUE
    out_order[k] <- best_item

    # --- P-value: count unreached and newly reached BEFORE updating reached ---
    n_unreached <- 0L
    n_new <- 0L
    for(i in seq_len(n_resp)){
      if(!reached[i]){
        n_unreached <- n_unreached + 1L
        if(mat[i, best_item] == 1){
          n_new <- n_new + 1L
        }
      }
    }

    if(n_unreached > 0 && n_new > 0){
      # Mean standalone rate of remaining unselected items (excluding best_item)
      remaining_mask <- !selected  # best_item already marked TRUE
      if(any(remaining_mask)){
        p0 <- mean(abs_count[remaining_mask]) / n_resp
      } else {
        p0 <- abs_count[best_item] / n_resp
      }

      if(p0 > 0 && p0 < 1 && n_new >= 1 && n_unreached >= 1){
        out_pval[k] <- 1 - stats::pbinom(n_new - 1, n_unreached, p0)
      } else {
        out_pval[k] <- 0
      }
    } else {
      out_pval[k] <- 1
    }

    # --- Update reached and cur_reach_wt ---
    for(i in seq_len(n_resp)){
      resp_count[i] <- resp_count[i] + mat[i, best_item]
      if(mat[i, best_item] == 1 && !reached[i]){
        reached[i] <- TRUE
        cur_reach_wt <- cur_reach_wt + weights[i]
      }
    }

    out_cumul[k] <- cur_reach_wt / total_weight * 100

    if(k == 1){
      out_incr[k] <- out_cumul[k]
    } else {
      out_incr[k] <- out_cumul[k] - out_cumul[k - 1]
    }

    # Average frequency among reached respondents
    freq_sum <- 0
    reached_wt <- 0
    for(i in seq_len(n_resp)){
      if(resp_count[i] > 0){
        freq_sum <- freq_sum + resp_count[i] * weights[i]
        reached_wt <- reached_wt + weights[i]
      }
    }
    out_avg_freq[k] <- if(reached_wt > 0) freq_sum / reached_wt else 0
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

  if(length(item_cols) == 0 || nrow(tbl) == 0) return(NULL)

  # Filter: keep only rows where ALL items are in included set
  keep <- apply(tbl[, item_cols, drop = FALSE], 1, function(row){
    all(row %in% included_vars)
  })
  tbl <- tbl[keep, , drop = FALSE]

  if(nrow(tbl) == 0) return(NULL)

  # Select reach/freq columns
  if(use_weighted && "w_reach_pct" %in% names(tbl)){
    reach_col <- "w_reach_pct"
    freq_col <- "w_freq_avg"
  } else {
    reach_col <- "reach_pct"
    freq_col <- "freq_avg"
  }

  # Sort
  if(optimize_by == "freq"){
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
  for(ic in item_cols){
    display_col <- gsub("^item_", "display_", ic)
    tbl[[display_col]] <- unname(label_lookup[tbl[[ic]]])
  }

  tbl
}


# =============================================================================
# Charts
# =============================================================================

.turf_chart_greedy <- function(greedy_df, chart_label_mode) {
  if(is.null(greedy_df) || nrow(greedy_df) == 0) return(plotly::plotly_empty())

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
      marker = list(color = "#D9D9D9"),
      hoverinfo = "skip"
    ) %>%
    plotly::add_bars(
      y = y_factor, x = greedy_df$incr_pct,
      name = "Incremental", orientation = "h",
      marker = list(color = "#595959"),
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


.turf_chart_top_reach <- function(combo_df, n_show = 20) {
  if(is.null(combo_df) || nrow(combo_df) == 0) return(plotly::plotly_empty())

  df <- utils::head(combo_df, n_show)
  labels <- paste0("#", df$rank)

  plotly::plot_ly(
    y = factor(labels, levels = rev(labels)),
    x = df$reach_display,
    type = "bar", orientation = "h",
    marker = list(color = "#4472C4"),
    text = paste0(round(df$reach_display, 1), "%"),
    textposition = "outside",
    hovertemplate = "Rank %{y}<br>Reach: %{x:.1f}%<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "Reach %"),
      yaxis = list(title = ""),
      showlegend = FALSE
    )
}


.turf_chart_reach_vs_freq <- function(combo_df, n_show = 50) {
  if(is.null(combo_df) || nrow(combo_df) == 0) return(plotly::plotly_empty())

  df <- utils::head(combo_df, n_show)

  colors <- c("#ED7D31", rep("#4472C4", max(0, nrow(df) - 1)))
  sizes <- c(12, rep(6, max(0, nrow(df) - 1)))

  plotly::plot_ly(
    x = df$reach_display, y = df$freq_display,
    type = "scatter", mode = "markers",
    marker = list(color = colors, size = sizes),
    text = paste0("#", df$rank),
    hovertemplate = "Rank #%{text}<br>Reach: %{x:.1f}%<br>Freq: %{y:.2f}<extra></extra>"
  ) %>%
    plotly::layout(
      xaxis = list(title = "Reach %"),
      yaxis = list(title = "Avg Freq"),
      showlegend = FALSE
    )
}


.turf_chart_item_freq <- function(combo_df, combo_size, n_show = 100) {
  if(is.null(combo_df) || nrow(combo_df) == 0) return(plotly::plotly_empty())

  df <- utils::head(combo_df, n_show)
  display_cols <- grep("^display_\\d+$", names(df), value = TRUE)

  # Count item appearances
  all_items <- unlist(df[, display_cols], use.names = FALSE)
  all_items <- all_items[!is.na(all_items) & nchar(all_items) > 0]
  counts <- sort(table(all_items), decreasing = TRUE)

  if(length(counts) > 20) counts <- counts[1:20]

  labels <- names(counts)
  labels <- ifelse(nchar(labels) > 40,
                   paste0(substr(labels, 1, 37), "..."),
                   labels)

  plotly::plot_ly(
    y = factor(labels, levels = rev(labels)),
    x = as.integer(counts),
    type = "bar", orientation = "h",
    marker = list(color = "#70AD47"),
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
