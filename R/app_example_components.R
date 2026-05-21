#' app_example_components
#' @description
#' A one-page component gallery for the Resondex visual identity — modelled
#' on the Bootswatch theme-preview pages. Renders the \code{resondex_theme()}
#' chrome plus \code{resondex_css()} tokens across every common Bootstrap
#' component (typography, buttons, forms, navs, alerts, badges, progress,
#' list groups, cards, tables) and the custom pieces (the canonical
#' reactable-spec table, the bn_report header/footer treatment, the shared
#' \code{data-tip} tooltip).
#'
#' This is the visual review surface for the brand system: evaluate the look
#' here and iterate \code{\link{resondex_brand}} before any production code
#' (bn_report, the apps, the site) consumes it. It is also the permanent
#' regression check for future brand changes.
#'
#' @return A \code{shiny::shinyApp} object.
#' @export
app_example_components <- function() {

  .bl <- resondex_brand("light")
  .bd <- resondex_brand("dark")

  # Chip uses the live CSS var so it re-resolves when dark mode flips;
  # meta shows both mode hexes for reference.
  swatch <- function(var, light_hex, dark_hex, label) {
    shiny::tags$div(
      class = "rdx-swatch",
      shiny::tags$div(
        class = "rdx-swatch-chip",
        style = sprintf("background: var(%s);", var)
      ),
      shiny::tags$div(
        class = "rdx-swatch-meta",
        shiny::tags$code(var),
        shiny::tags$span(class = "text-muted",
          sprintf("%s — L %s · D %s", label, light_hex, dark_hex))
      )
    )
  }

  btn <- function(variant, label = variant, extra = "") {
    shiny::tags$button(
      type = "button",
      class = sprintf("btn btn-%s%s", variant, extra),
      tools::toTitleCase(label)
    )
  }

  section <- function(title, ...) {
    shiny::tags$section(
      class = "rdx-section",
      shiny::tags$h2(class = "rdx-section-title", title),
      ...
    )
  }

  # Showcase-only CSS: previews the target table look (the canonical
  # reactable spec) and a few gallery affordances. Not wired into
  # bn_report — that is Step 3.
  gallery_css <- paste(
    c(
      "body { padding: 28px 36px; }",
      ".rdx-section { margin: 0 0 40px 0; }",
      ".rdx-section-title {",
      "  font-size: var(--ndr-fs-h3); font-weight: 600;",
      "  color: var(--ndr-text); margin: 0 0 16px 0;",
      "  padding-bottom: 8px; border-bottom: 1px solid var(--ndr-border);",
      "}",
      ".rdx-row { display: flex; flex-wrap: wrap; gap: 10px; align-items: center; }",
      ".rdx-swatch { display: flex; align-items: center; gap: 10px;",
      "  font-size: var(--ndr-fs-sm); margin: 0 22px 10px 0; }",
      ".rdx-swatch-chip { width: 38px; height: 38px; border-radius: 6px;",
      "  border: 1px solid var(--ndr-border); flex: none; }",
      ".rdx-swatch-meta { display: flex; flex-direction: column; line-height: 1.35; }",
      ".rdx-well { padding: 16px; border-radius: var(--ndr-radius);",
      "  background: var(--ndr-tertiary-bg); border: 1px solid var(--ndr-border); }",
      ".rdx-scale-row { display: flex; align-items: baseline; gap: 14px;",
      "  margin: 0 0 6px 0; }",
      ".rdx-scale-row code { color: var(--ndr-muted); font-size: var(--ndr-fs-sm); }",
      # Canonical table = the reactable spec, expressed via --ndr-*
      ".rdx-table { width: 100%; border-collapse: collapse;",
      "  font-size: var(--ndr-fs-md); color: var(--ndr-text); }",
      ".rdx-table thead th {",
      "  font-weight: 600; text-align: left; padding: 8px 10px;",
      "  background: var(--ndr-card-bg);",
      "  border-bottom: 1px solid var(--ndr-border); }",
      ".rdx-table tbody td { padding: 8px 10px; border: 0; }",
      ".rdx-table tbody tr:hover { background: var(--ndr-secondary-bg); }",
      ".rdx-table tfoot td {",
      "  padding: 8px 10px; background: var(--ndr-tertiary-bg);",
      "  border-top: 1px solid var(--ndr-border);",
      "  font-size: var(--ndr-fs-sm); color: var(--ndr-muted); }",
      # bn_report header/footer treatment preview
      ".rdx-card-title { font-size: var(--ndr-fs-lg); font-weight: 600;",
      "  color: var(--ndr-text); padding: 6px 4px 12px 2px;",
      "  border-bottom: 1px solid var(--ndr-border); margin: 0 0 12px 0; }",
      ".rdx-footer-legend { display: flex; gap: 18px; font-size: var(--ndr-fs-sm);",
      "  color: var(--ndr-muted); margin-top: 10px; }",
      ".rdx-legend-item { display: flex; align-items: center; gap: 6px; }",
      ".rdx-legend-chip { width: 14px; height: 14px; border-radius: 3px; }"
    ),
    collapse = "\n"
  )

  # Plain cell — NO text formatting. Optional shared conditional-format
  # class (.rdx-neg additive edge / .rdx-insig blackout) and inline
  # value-driven scale background.
  cell <- function(content, align = "left", cls = NULL, bg = NULL) {
    sty <- sprintf("text-align:%s;", align)
    if (!is.null(bg)) sty <- paste0(sty, sprintf(" background:%s;", bg))
    shiny::tags$td(class = cls, style = sty, content)
  }
  # Index = value-driven diverging red→neutral→green scale (100 = mean).
  # The scale is inline (value-dependent); .rdx-neg can be ADDED on top.
  idx_bg <- function(value) {
    d <- value - 100
    pct <- min(45, round(abs(d) / 45 * 45))
    tok <- if (d >= 0) "--ndr-success" else "--ndr-danger"
    if (pct == 0) "transparent" else sprintf(
      "color-mix(in srgb, var(%s) %s%%, transparent)", tok, pct)
  }
  # Index2 = sequential white→green scale; `frac` is normalized 0..1.
  idx2_bg <- function(frac) {
    pct <- round(min(1, max(0, frac)) * 55)
    if (pct == 0) "transparent" else sprintf(
      "color-mix(in srgb, var(--ndr-success) %s%%, transparent)", pct)
  }
  # P-value uses the SHARED .rdx-pval-* classes (the one place text colour
  # is intentional — it is the p-value column's whole purpose).
  pval <- function(p, cls = NULL) {
    k <- if (p < 0.05) "rdx-pval-sig" else
      if (p < 0.10) "rdx-pval-marg" else "rdx-pval-insig"
    shiny::tags$td(
      class = cls, style = "text-align:right;",
      shiny::tags$span(class = k, sprintf("%.3f", p))
    )
  }

  demo_table <- shiny::tags$table(
    class = "rdx-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("Attribute"),
      shiny::tags$th(
        style = "cursor:pointer;",
        "Impact ", shiny::tags$span(style = "color:var(--ndr-muted);", "▾")),
      shiny::tags$th("Lift"),
      shiny::tags$th(style = "text-align:right;", "Index"),
      shiny::tags$th(style = "text-align:right;", "Index₂"),
      shiny::tags$th(style = "text-align:right;", "P-value")
    )),
    shiny::tags$tbody(
      # Significant positive — plain text; meaning carried by the scale
      # + p-value column, not by styling the metric text.
      shiny::tags$tr(
        cell("Brand trust"), cell("0.182", "left"), cell("+12.4%", "left"),
        cell("141", "right", bg = idx_bg(141)),
        cell("0.92", "right", bg = idx2_bg(0.92)), pval(0.002)
      ),
      shiny::tags$tr(
        cell("Value for money"), cell("0.094", "left"), cell("+6.1%", "left"),
        cell("118", "right", bg = idx_bg(118)),
        cell("0.61", "right", bg = idx2_bg(0.61)), pval(0.041)
      ),
      # Negative — per bn_report, ONLY the numeric index cells carry .rdx-neg
      # (that cell's raw metric < 0). Label / Impact / Lift stay plain.
      shiny::tags$tr(
        cell("Price sensitivity"), cell("-0.061", "left"), cell("-3.4%", "left"),
        cell("86", "right", "rdx-neg", bg = idx_bg(86)),
        cell("0.18", "right", "rdx-neg", bg = idx2_bg(0.18)), pval(0.083)
      ),
      # Insignificant — only the index cells blackout (.rdx-insig).
      shiny::tags$tr(
        cell("Ease of use"), cell("0.009", "left"), cell("+0.4%", "left"),
        cell("101", "right", "rdx-insig"),
        cell("0.05", "right", "rdx-insig"), pval(0.214)
      ),
      # Negative AND insignificant — index cells get both: blackout
      # background with red italic text (.rdx-neg.rdx-insig).
      shiny::tags$tr(
        cell("Switching cost"), cell("-0.045", "left"), cell("-2.1%", "left"),
        cell("78", "right", "rdx-neg rdx-insig"),
        cell("0.12", "right", "rdx-neg rdx-insig"), pval(0.190)
      )
    ),
    shiny::tags$tfoot(shiny::tags$tr(
      shiny::tags$td(colspan = 6,
        paste("Index = diverging red→green (100 = mean);",
              "Index₂ = sequential white→green;",
              "P-value = sig <.05 green / marginal <.10 orange / insig red;",
              "negative = red italic text, insignificant = blackout, both =",
              "blackout + red italic — per-cell on the numeric index columns",
              "ONLY (label / Impact / Lift never formatted). Shared",
              "conditional-format classes (resondex_css)."))
    ))
  )

  ui <- bslib::page_fluid(
    theme = resondex_theme(),
    # One-call brand bundle (useShinyjs + resondex_css + tooltip JS) —
    # dogfooding the helper every app_deliverable_* will use.
    resondex_deps(),
    shiny::tags$head(shiny::tags$style(shiny::HTML(gallery_css))),

    shiny::tags$div(
      style = "display:flex; align-items:flex-start; justify-content:space-between; gap:16px;",
      shiny::tags$div(
        shiny::tags$h1(
          style = "font-weight:600; letter-spacing:-0.01em;",
          "Resondex Component Gallery"
        ),
        shiny::tags$p(
          class = "lead text-muted",
          "Live preview of resondex_theme() + resondex_css(). Toggle dark ",
          "to review both modes; iterate resondex_brand() before wiring."
        )
      ),
      bslib::input_dark_mode(id = "rdx_mode")
    ),

    section(
      "Palette",
      shiny::tags$p(class = "text-muted", style = "margin-bottom:14px;",
        "The raw brand tokens from resondex_brand() — a spec-sheet view, ",
        "independent of how components use them. Auto-updates with the brand."),
      shiny::tags$div(
        class = "rdx-row",
        swatch("--ndr-text", .bl$colors$text, .bd$colors$text, "text"),
        swatch("--ndr-muted", .bl$colors$muted, .bd$colors$muted, "muted"),
        swatch("--ndr-accent", .bl$colors$accent, .bd$colors$accent, "accent / primary"),
        swatch("--ndr-bg", .bl$colors$bg, .bd$colors$bg, "page bg"),
        swatch("--ndr-card-bg", .bl$colors$card_bg, .bd$colors$card_bg, "card"),
        swatch("--ndr-border", .bl$colors$border, .bd$colors$border, "border"),
        swatch("--ndr-secondary-bg", .bl$surfaces$secondary_bg, .bd$surfaces$secondary_bg, "row hover"),
        swatch("--ndr-tertiary-bg", .bl$surfaces$tertiary_bg, .bd$surfaces$tertiary_bg, "footer"),
        swatch("--ndr-sidebar-bg", .bl$colors$sidebar_bg, .bd$colors$sidebar_bg, "sidebar")
      )
    ),

    section(
      "Typography & type scale",
      shiny::tags$h1("Heading 1"), shiny::tags$h2("Heading 2"),
      shiny::tags$h3("Heading 3"), shiny::tags$h4("Heading 4"),
      shiny::tags$h5("Heading 5"), shiny::tags$h6("Heading 6"),
      shiny::tags$p(
        "Body copy in Inter. ",
        shiny::tags$a(href = "#", "An inline link"), ", ",
        shiny::tags$strong("bold"), ", ", shiny::tags$em("italic"), ", and ",
        shiny::tags$span(class = "text-muted", "muted text"), "."
      ),
      shiny::tags$hr(),
      lapply(
        list(
          c("--ndr-fs-h2", "22 — h1/h2"),
          c("--ndr-fs-h3", "18 — section"),
          c("--ndr-fs-lg", "15 — card title"),
          c("--ndr-fs-md", "14 — table body"),
          c("--ndr-fs-base", "13 — body"),
          c("--ndr-fs-sm", "12 — footer/legend"),
          c("--ndr-fs-xs", "11 — fine print")
        ),
        function(p) {
          shiny::tags$div(
            class = "rdx-scale-row",
            shiny::tags$span(
              style = sprintf("font-size: var(%s);", p[1]),
              "The quick brown fox"
            ),
            shiny::tags$code(p[2])
          )
        }
      )
    ),

    section(
      "Buttons",
      shiny::tags$div(
        class = "rdx-row",
        btn("primary"), btn("secondary"), btn("success"), btn("info"),
        btn("warning"), btn("danger"), btn("light"), btn("dark"),
        btn("link")
      ),
      shiny::tags$div(
        class = "rdx-row", style = "margin-top:10px;",
        btn("outline-primary", "outline"), btn("outline-secondary", "outline"),
        btn("primary", "large", " btn-lg"), btn("primary", "small", " btn-sm"),
        shiny::tags$button(
          type = "button", class = "btn btn-primary", disabled = NA, "Disabled"
        )
      ),
      shiny::tags$hr(),
      shiny::tags$p(class = "text-muted", style = "margin-bottom:6px;",
        shiny::tags$strong(".btn-rdx"),
        " — the brand action button (`resondex_css()`). Single source ",
        "of truth for card-overlay / sidebar / download buttons across ",
        "every app and report. Apply via ", shiny::tags$code('class = "btn-rdx"'),
        " on any ", shiny::tags$code("shiny::tags$button"), ", ",
        shiny::tags$code("shiny::actionButton"), ", or ",
        shiny::tags$code("shiny::downloadButton"), "."),
      shiny::tags$div(
        class = "rdx-row",
        shiny::tags$button(type = "button", class = "btn-rdx", "Default"),
        shiny::tags$button(type = "button", class = "btn-rdx", disabled = NA,
          "Disabled"),
        shiny::downloadLink("dl_rdx_demo", "Download", class = "btn-rdx")
      )
    ),

    section(
      "Forms",
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::tagList(
          shiny::textInput("t1", "Text input", placeholder = "Type here"),
          shiny::selectInput("s1", "Select", c("Option A", "Option B", "Option C")),
          shiny::sliderInput("sl1", "Slider", 0, 100, 40)
        ),
        shiny::tagList(
          shiny::textAreaInput("ta1", "Textarea", rows = 3),
          shiny::checkboxGroupInput("c1", "Checkboxes",
            c("First", "Second"), selected = "First"),
          shiny::radioButtons("r1", "Radios", c("Yes", "No"), inline = TRUE)
        )
      ),
      shiny::tags$hr(),
      shiny::tags$div(
        class = "rdx-row",
        shiny::checkboxInput("enable_btn", "Enable the action button",
          value = TRUE, width = "auto"),
        shiny::actionButton("toggle_btn", "Run analysis",
          class = "btn-primary"),
        shiny::tags$span(class = "text-muted", id = "toggle_status",
          "enabled")
      )
    ),

    section(
      "Navigation",
      # Faux navbar — demonstrates the .navbar-brand + .nav-link brand
      # rules in resondex_css(). app_deliverable's page_navbar renders
      # exactly this shape; the brand layer pins font + color so the
      # title and tabs read consistently across every app and report.
      shiny::tags$nav(
        class = "navbar navbar-expand",
        style = paste(
          "background: var(--ndr-header-bg);",
          "border-bottom: 1px solid var(--ndr-border);",
          "padding: 8px 16px; margin-bottom: 14px;"
        ),
        shiny::tags$a(class = "navbar-brand", href = "#", "Brand Title"),
        shiny::tags$ul(
          class = "navbar-nav",
          shiny::tags$li(class = "nav-item",
            shiny::tags$a(class = "nav-link active", href = "#", "Active tab")),
          shiny::tags$li(class = "nav-item",
            shiny::tags$a(class = "nav-link", href = "#", "Second")),
          shiny::tags$li(class = "nav-item",
            shiny::tags$a(class = "nav-link", href = "#", "Third"))
        )
      ),
      bslib::navset_underline(
        bslib::nav_panel("Tab one", shiny::tags$p(class = "pt-2",
          "Underline nav — the app chrome style.")),
        bslib::nav_panel("Tab two", shiny::tags$p(class = "pt-2", "Second panel.")),
        bslib::nav_panel("Tab three", shiny::tags$p(class = "pt-2", "Third panel."))
      ),
      shiny::tags$ul(
        class = "nav nav-pills", style = "margin-top:14px;",
        shiny::tags$li(class = "nav-item",
          shiny::tags$a(class = "nav-link active", href = "#", "Pill active")),
        shiny::tags$li(class = "nav-item",
          shiny::tags$a(class = "nav-link", href = "#", "Pill")),
        shiny::tags$li(class = "nav-item",
          shiny::tags$a(class = "nav-link", href = "#", "Pill"))
      )
    ),

    section(
      "Alerts, badges & progress",
      shiny::tags$div(class = "alert alert-primary", "A primary alert."),
      shiny::tags$div(class = "alert alert-success", "A success alert."),
      shiny::tags$div(class = "alert alert-warning", "A warning alert."),
      shiny::tags$div(class = "alert alert-danger", "A danger alert."),
      shiny::tags$div(
        class = "rdx-row",
        shiny::tags$span(class = "badge bg-primary", "Primary"),
        shiny::tags$span(class = "badge bg-secondary", "Secondary"),
        shiny::tags$span(class = "badge bg-success", "Success"),
        shiny::tags$span(class = "badge bg-danger", "Danger")
      ),
      shiny::tags$div(
        class = "progress", style = "margin-top:14px;",
        shiny::tags$div(
          class = "progress-bar", role = "progressbar",
          style = "width: 65%;", "65%"
        )
      )
    ),

    section(
      "Cards & list groups",
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Card header"),
          bslib::card_body(
            shiny::tags$p("Card body on the themed surface."),
            btn("primary", "Action")
          ),
          bslib::card_footer(shiny::tags$span(class = "text-muted",
            "Card footer"))
        ),
        shiny::tags$ul(
          class = "list-group",
          shiny::tags$li(class = "list-group-item active", "Active item"),
          shiny::tags$li(class = "list-group-item", "A second item"),
          shiny::tags$li(class = "list-group-item", "A third item")
        )
      )
    ),

    section(
      "Accordion",
      shiny::tags$p(class = "text-muted",
        "bn_report's core result container (.result-accordion) — verify ",
        "header weight, border and surface here when Step 3 restyles it."),
      bslib::accordion(
        id = "rdx_acc",
        bslib::accordion_panel(
          "Network — first result",
          shiny::tags$p("Panel body on the card surface.")
        ),
        bslib::accordion_panel(
          "Network — second result",
          shiny::tags$p("A second collapsible section.")
        )
      )
    ),

    section(
      "Layout — sidebar & well/panel",
      shiny::tags$p(class = "text-muted",
        "The app chrome the deliverables actually use: a bslib sidebar ",
        "(app_deliverable page rail / network-drivers controls) and a ",
        "well/panel surface. The sidebar's control spacing (12px between ",
        "form-groups) is centralized in ", shiny::tags$code("resondex_css()"),
        " — no per-app rules. A trailing element with ",
        shiny::tags$code('style = "margin-top: auto;"'),
        " pins itself to the bottom (classic flexbox idiom)."),
      shiny::tags$div(
        style = "border:1px solid var(--ndr-border); border-radius:var(--ndr-radius); overflow:hidden; height:340px;",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Controls",
            shiny::selectInput("ls1", "Focus", c("Market", "Brand A", "Brand B")),
            shiny::radioButtons("ls2", "Shift", c("Proportional", "Fixed step")),
            shiny::tags$button(type = "button", class = "btn-rdx",
              style = "width: 100%;", "Apply"),
            # margin-top: auto pushes this wrapper (and everything after)
            # to the bottom of the sidebar's flex column.
            shiny::tags$div(
              style = "margin-top: auto;",
              shiny::tags$hr(style = "margin: 12px 0;"),
              shiny::downloadLink("dl_workbook_demo", "Download Workbook",
                class = "btn-rdx", style = "width: 100%;")
            )
          ),
          shiny::tags$h5("Main panel"),
          shiny::tags$p(class = "text-muted",
            "Content area beside the themed sidebar.")
        )
      ),
      shiny::tags$div(
        class = "rdx-well", style = "margin-top:16px;",
        shiny::tags$strong("Well / panel"),
        shiny::tags$p(class = "text-muted", style = "margin:6px 0 0 0;",
          "A sunken surface (--ndr-tertiary-bg + border) for grouped ",
          "controls or supplementary notes — the BS5 replacement for .well.")
      )
    ),

    section(
      "Table — canonical reactable spec",
      shiny::tags$p(class = "text-muted",
        "This is the body/header/footer treatment bn_report's tables and ",
        "the reactable tables both converge on."),
      shiny::tags$div(
        class = "rdx-card-title", "Attribute Impacts: ",
        shiny::tags$em("What moves the outcome?")
      ),
      demo_table,
      shiny::tags$div(
        class = "rdx-footer-legend",
        shiny::tags$span(class = "rdx-legend-item",
          shiny::tags$span(class = "rdx-legend-chip",
            style = "background:var(--ndr-success);"), "Significant"),
        shiny::tags$span(class = "rdx-legend-item",
          shiny::tags$span(class = "rdx-legend-chip",
            style = "background:var(--ndr-muted);"), "Below threshold")
      )
    ),

    section(
      "Charts — opt-in Resondex plotly theme",
      shiny::tags$p(class = "text-muted",
        "plotly_theme(p, \"Resondex\") — the brand colorway/fonts/axes. ",
        "\"Default\" stays a passthrough; the 19 ports are untouched; ",
        "this is purely an added option in the existing chart pickers."),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        plotly::plotlyOutput(outputId = "rdx_bar", height = "300px"),
        plotly::plotlyOutput(outputId = "rdx_stacked", height = "300px"),
        plotly::plotlyOutput(outputId = "rdx_scatter", height = "300px")
      )
    ),

    section(
      "Prioritization waterfall",
      shiny::tags$p(class = "text-muted",
        "The standard two-tone grey: light grey accumulated base ",
        "(bar-prev #D9D9D9) + dark grey incremental gain (bar-incr ",
        "#595959) + cumulative line. Matches bn_report's prio chart and ",
        "the Excel bn_prioritize_write — kept as-is."),
      plotly::plotlyOutput(outputId = "rdx_prio_now", height = "340px")
    ),

    section(
      "Shared tooltip",
      shiny::tags$p(
        "Hover the ",
        shiny::tags$span(
          `data-tip` = "This is the shared resondex tooltip — the prio-chart tooltip, generalized. Fixed-position, so it never reflows layout.",
          style = "border-bottom:1px dotted var(--ndr-muted);",
          "underlined term"
        ),
        " or this ",
        shiny::tags$button(
          type = "button", class = "btn btn-outline-secondary btn-sm",
          `data-tip` = "Tooltips attach to any element with a data-tip attribute — buttons, labels, table headers, chart marks.",
          "button"
        ),
        " — one component, used everywhere."
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$enable_btn, {
      if (isTRUE(input$enable_btn)) {
        shinyjs::enable("toggle_btn")
        shinyjs::html("toggle_status", "enabled")
      } else {
        shinyjs::disable("toggle_btn")
        shinyjs::html("toggle_status", "disabled")
      }
    }, ignoreInit = FALSE)

    output$rdx_bar <- plotly::renderPlotly({
      df <- data.frame(
        cat = rep(c("A", "B", "C", "D"), each = 3),
        grp = rep(c("Series 1", "Series 2", "Series 3"), 4),
        val = c(8, 5, 3, 6, 7, 4, 9, 4, 5, 5, 6, 7)
      )
      plotly::plot_ly(
        df, x = ~cat, y = ~val, color = ~grp, type = "bar"
      ) |>
        plotly::layout(barmode = "group",
                       title = list(text = "Grouped bar")) |>
        plotly_theme("Resondex")
    })

    output$rdx_stacked <- plotly::renderPlotly({
      df <- data.frame(
        cat = rep(c("A", "B", "C", "D"), each = 3),
        grp = rep(c("Series 1", "Series 2", "Series 3"), 4),
        val = c(4, 3, 2, 5, 2, 3, 3, 5, 2, 4, 4, 3)
      )
      plotly::plot_ly(
        df, x = ~cat, y = ~val, color = ~grp, type = "bar"
      ) |>
        plotly::layout(barmode = "stack",
                       title = list(text = "Stacked bar")) |>
        plotly_theme("Resondex")
    })

    prio_fig <- function(incr_color) {
      steps <- c("Driver 1", "Driver 2", "Driver 3", "Driver 4", "Driver 5")
      incr  <- c(28, 18, 12, 8, 5)
      base  <- c(0, head(cumsum(incr), -1))      # cumulative before this step
      cum   <- cumsum(incr)
      df <- data.frame(step = factor(steps, levels = steps),
                       base = base, incr = incr, cum = cum)
      plotly::plot_ly(df, x = ~step) |>
        plotly::add_bars(y = ~base, name = "Cumulative so far",
                         marker = list(color = "#D9D9D9")) |>
        plotly::add_bars(y = ~incr, name = "This step's gain",
                         marker = list(color = incr_color)) |>
        plotly::add_trace(y = ~cum, name = "Cumulative",
                          type = "scatter", mode = "lines+markers",
                          line = list(color = "#595959"),
                          marker = list(color = "#595959")) |>
        plotly::layout(barmode = "stack",
                       title = list(text = "Prioritization")) |>
        plotly_theme("Resondex")
    }
    output$rdx_prio_now <- plotly::renderPlotly(prio_fig("#595959"))

    output$rdx_scatter <- plotly::renderPlotly({
      set.seed(1)
      df <- data.frame(
        x = rep(1:12, 3),
        y = c(cumsum(rnorm(12, 1)), cumsum(rnorm(12, 0.6)),
              cumsum(rnorm(12, 1.4))),
        grp = rep(c("Series 1", "Series 2", "Series 3"), each = 12)
      )
      plotly::plot_ly(
        df, x = ~x, y = ~y, color = ~grp,
        type = "scatter", mode = "lines+markers"
      ) |>
        plotly::layout(title = list(text = "Lines + markers")) |>
        plotly_theme("Resondex")
    })
  }

  shiny::shinyApp(ui, server)
}
