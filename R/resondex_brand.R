#' resondex_brand
#' @description
#' Single source of truth for the Resondex visual identity. Returns a plain
#' list of brand values (palette, surfaces, semantics, type scale, spacing,
#' font, radius, shadow, focus) resolved for the requested colour mode.
#' Every renderer derives from this so they never drift:
#' \code{\link{resondex_css}} (literal CSS for standalone reports / iframes /
#' app layers, emitting BOTH modes), and \code{\link{resondex_theme}} (a
#' \code{bslib::bs_theme} for the Shiny chrome). Change a value here and
#' every surface moves together.
#'
#' Light values reproduce bn_report's restyled \code{:root} block; the dark
#' set is the same cool-neutral identity inverted for legibility on dark
#' surfaces. Mode-independent values (type scale, spacing, font, radius) are
#' the same in both modes.
#'
#' @param mode \code{"light"} (default) or \code{"dark"} — which palette to
#'   resolve. Shape is identical for both modes.
#' @return Named list: \code{mode}, \code{colors}, \code{surfaces},
#'   \code{semantic}, \code{focus}, \code{shadow}, \code{type}, \code{space},
#'   \code{font}, \code{font_stack}, \code{font_import_url}, \code{radius}.
#' @export
resondex_brand <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)

  palettes <- list(
    light = list(
      colors = list(
        bg         = "#f8f9fa",
        card_bg    = "#ffffff",
        border     = "#dee2e6",
        muted      = "#6c757d",
        text       = "#212529",
        accent     = "#595959",
        secondary  = "#6c757d",
        header_bg  = "#ffffff",
        sidebar_bg = "#f3f4f6"
      ),
      surfaces = list(
        secondary_bg = "#f0f0f0",  # row hover
        tertiary_bg  = "#f8f9fa"   # table footer / subtle fills
      ),
      semantic = list(
        success = "#5C8A6B",
        info    = "#5B7E92",
        warning = "#C2773C",
        danger  = "#AC6258"
      ),
      focus  = "rgba(89, 89, 89, 0.35)",
      shadow = "0 1px 3px rgba(0,0,0,.055), 0 1px 2px rgba(0,0,0,.04)"
    ),
    dark = list(
      colors = list(
        bg         = "#16181a",
        card_bg    = "#212529",
        border     = "#3a3f44",
        muted      = "#9aa0a6",
        text       = "#e8eaed",
        accent     = "#aeb4ba",
        secondary  = "#5c6166",
        header_bg  = "#212529",
        sidebar_bg = "#1b1e21"
      ),
      surfaces = list(
        secondary_bg = "#2a2e33",  # row hover
        tertiary_bg  = "#1f2226"   # table footer / subtle fills
      ),
      semantic = list(
        success = "#6FA085",
        info    = "#6F93A8",
        warning = "#D68F52",
        danger  = "#C2776C"
      ),
      focus  = "rgba(174, 180, 186, 0.45)",
      shadow = "0 1px 3px rgba(0,0,0,.45), 0 1px 2px rgba(0,0,0,.35)"
    )
  )

  p <- palettes[[mode]]

  c(
    list(mode = mode),
    p,
    list(
      # Mode-independent.
      type = list(
        xs   = "11px",  # axis / fine print
        sm   = "12px",  # footers / legend
        base = "14px",  # body
        md   = "14px",  # table body (matches reactable spec)
        lg   = "15px",  # card title
        h3   = "18px",
        h2   = "22px"
      ),
      space = list(
        s1 = "4px", s2 = "8px", s3 = "12px",
        s4 = "16px", s5 = "24px", s6 = "32px"
      ),
      font = "Inter",
      font_stack = paste(
        "'Inter'", "-apple-system", "BlinkMacSystemFont", "'Segoe UI'",
        "Roboto", "Helvetica", "Arial", "sans-serif",
        sep = ", "
      ),
      font_import_url =
        "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap",
      radius = "0.5rem",
      # Data-viz palette (concrete hexes — charts render light). Feeds the
      # "Default" plotly theme (brand-themed); cohesive with the UI tokens.
      viz = list(
        # Categorical series colours: brand grey + muted semantics + grey
        # extenders — distinguishable but on-identity.
        colorway = c(
          "#595959", "#5B7E92", "#5C8A6B", "#C2773C",
          "#AC6258", "#8C8C8C", "#BFBFBF", "#3F4A55"
        ),
        paper_bg   = "#ffffff",
        plot_bg    = "#ffffff",
        grid       = "#dee2e6",  # = --ndr-border
        axis_text  = "#6c757d",  # = --ndr-muted
        font_color = "#212529",  # = --ndr-text
        bar_base   = "#D9D9D9",  # prio base bar
        bar_incr   = "#595959",  # prio incremental / accent
        line       = "#595959"
      ),
      # Dark mirror for the "Resondex Dark" plotly theme. Surfaces mirror
      # the dark UI tokens (card_bg, border, muted, text) so charts look
      # native in dark mode. Colorway is lifted toward higher luminance
      # so each series stays distinguishable against the dark plot bg.
      viz_dark = list(
        colorway = c(
          "#A8A8A8", "#86A4B5", "#86B098", "#D69A6F",
          "#C2877F", "#B5B5B5", "#D9D9D9", "#6F7A85"
        ),
        paper_bg   = "#212529",  # = dark --ndr-card-bg
        plot_bg    = "#212529",
        grid       = "#3a3f44",  # = dark --ndr-border
        axis_text  = "#9aa0a6",  # = dark --ndr-muted
        font_color = "#e8eaed",  # = dark --ndr-text
        bar_base   = "#3a3f44",
        bar_incr   = "#A8A8A8",
        line       = "#A8A8A8"
      )
    )
  )
}


# Internal: the --ndr-* / --bs-* surface custom-property declarations for one
# mode. The --bs-* aliases keep anything reading Bootstrap surface vars (the
# reactable table theme) in lockstep with --ndr-* across both modes.
.resondex_token_decls <- function(b) {
  cl <- b$colors
  sf <- b$surfaces
  sm <- b$semantic
  paste(
    c(
      sprintf("  --ndr-bg: %s;", cl$bg),
      sprintf("  --ndr-card-bg: %s;", cl$card_bg),
      sprintf("  --ndr-border: %s;", cl$border),
      sprintf("  --ndr-muted: %s;", cl$muted),
      sprintf("  --ndr-text: %s;", cl$text),
      sprintf("  --ndr-accent: %s;", cl$accent),
      sprintf("  --ndr-secondary: %s;", cl$secondary),
      sprintf("  --ndr-success: %s;", sm$success),
      sprintf("  --ndr-info: %s;", sm$info),
      sprintf("  --ndr-warning: %s;", sm$warning),
      sprintf("  --ndr-danger: %s;", sm$danger),
      sprintf("  --ndr-header-bg: %s;", cl$header_bg),
      sprintf("  --ndr-sidebar-bg: %s;", cl$sidebar_bg),
      sprintf("  --ndr-secondary-bg: %s;", sf$secondary_bg),
      sprintf("  --ndr-tertiary-bg: %s;", sf$tertiary_bg),
      sprintf("  --ndr-focus: %s;", b$focus),
      sprintf("  --ndr-shadow: %s;", b$shadow),
      # --bs-* aliases so Bootstrap-surface consumers (reactable) follow.
      sprintf("  --bs-body-bg: %s;", cl$card_bg),
      sprintf("  --bs-border-color: %s;", cl$border),
      sprintf("  --bs-card-border-color: %s;", cl$border),
      sprintf("  --bs-secondary-bg: %s;", sf$secondary_bg),
      sprintf("  --bs-tertiary-bg: %s;", sf$tertiary_bg)
    ),
    collapse = "\n"
  )
}


#' resondex_css
#' @description
#' Emit the brand as literal CSS: the Inter \code{@import}, BOTH colour-mode
#' token blocks (light under \code{:root}/\code{[data-bs-theme=light]}, dark
#' under \code{[data-bs-theme=dark]}), the mode-independent type/spacing
#' tokens, the shared floating-tooltip component, the disabled-control and
#' focus-ring treatments, and a light body font/size base. Embeddable
#' anywhere with no Sass pipeline — standalone \code{bn_report} HTML, the
#' network-map iframes, or as a CSS layer inside the Shiny apps. Components
#' reference \code{var(--ndr-*)}, so flipping \code{data-bs-theme} on a
#' parent re-resolves everything with no per-component work.
#'
#' @param include_import Logical; prepend the Inter \code{@import}. Keep
#'   \code{TRUE} for standalone reports / iframes. Pass \code{FALSE} inside
#'   \code{app_deliverable}, where \code{resondex_theme()} already loads
#'   Inter via \code{bslib::font_google}.
#' @return A single CSS string.
#' @export
resondex_css <- function(include_import = TRUE) {
  light <- resondex_brand("light")
  dark  <- resondex_brand("dark")
  ty <- light$type
  sp <- light$space

  import <- if (isTRUE(include_import)) {
    sprintf("@import url('%s');\n", light$font_import_url)
  } else {
    ""
  }

  # Mode-independent tokens live alongside the light set.
  mode_indep <- paste(
    c(
      sprintf("  --ndr-font: %s;", light$font_stack),
      sprintf("  --ndr-radius: %s;", light$radius),
      sprintf("  --ndr-fs-xs: %s;", ty$xs),
      sprintf("  --ndr-fs-sm: %s;", ty$sm),
      sprintf("  --ndr-fs-base: %s;", ty$base),
      sprintf("  --ndr-fs-md: %s;", ty$md),
      sprintf("  --ndr-fs-lg: %s;", ty$lg),
      sprintf("  --ndr-fs-h3: %s;", ty$h3),
      sprintf("  --ndr-fs-h2: %s;", ty$h2),
      sprintf("  --ndr-space-1: %s;", sp$s1),
      sprintf("  --ndr-space-2: %s;", sp$s2),
      sprintf("  --ndr-space-3: %s;", sp$s3),
      sprintf("  --ndr-space-4: %s;", sp$s4),
      sprintf("  --ndr-space-5: %s;", sp$s5),
      sprintf("  --ndr-space-6: %s;", sp$s6)
    ),
    collapse = "\n"
  )

  root_light <- paste0(
    ":root, [data-bs-theme=\"light\"] {\n",
    .resondex_token_decls(light), "\n",
    mode_indep, "\n}"
  )
  root_dark <- paste0(
    "[data-bs-theme=\"dark\"] {\n",
    .resondex_token_decls(dark), "\n}"
  )

  tip <- paste(
    c(
      ".resondex-tip {",
      "  position: fixed; pointer-events: none; z-index: 100000;",
      "  background: rgba(0,0,0,0.78); color: #fff;",
      "  padding: 8px 12px; border-radius: 6px;",
      "  font-size: var(--ndr-fs-sm, 12px); line-height: 1.45;",
      "  box-shadow: 0 4px 12px rgba(0,0,0,0.30);",
      "  max-width: 300px; white-space: pre-line;",
      "  font-family: var(--ndr-font);",
      "  backdrop-filter: blur(2px);",
      "  opacity: 0; transition: opacity .12s ease;",
      "}",
      ".resondex-tip.is-visible { opacity: 1; }",
      "[data-tip] { cursor: help; }"
    ),
    collapse = "\n"
  )

  base <- "body { font-family: var(--ndr-font); font-size: var(--ndr-fs-base); }"

  disabled <- paste(
    c(
      ".btn:disabled, .btn.disabled, .btn[disabled], fieldset:disabled .btn {",
      "  background-color: var(--ndr-secondary-bg) !important;",
      "  border-color: var(--ndr-border) !important;",
      "  color: var(--ndr-muted) !important;",
      "  opacity: 0.65 !important;",
      "  box-shadow: none !important;",
      "  cursor: not-allowed !important;",
      "}"
    ),
    collapse = "\n"
  )

  focus_ring <- paste(
    c(
      "*:focus-visible {",
      "  outline: none !important;",
      "  box-shadow: 0 0 0 0.2rem var(--ndr-focus) !important;",
      "  border-radius: 2px;",
      "}",
      ".btn:focus-visible, .form-control:focus, .form-select:focus {",
      "  outline: none !important;",
      "  box-shadow: 0 0 0 0.2rem var(--ndr-focus) !important;",
      "}"
    ),
    collapse = "\n"
  )

  # Form controls — uniform sizing to match .btn-sm (Download Workbook +
  # other small-button affordances) with the brand border.
  #
  # Shiny's selectInput() defaults to selectize=TRUE, so the visible
  # dropdown is a selectize.js widget (.selectize-input wrapper), NOT a
  # native <select class="form-select">. We need to target BOTH:
  #   - .form-select / .form-control — native selects + text inputs
  #     (covers selectize=FALSE selects, textInput, numericInput, etc.)
  #   - .selectize-input — the selectize.js widget body
  form_controls <- paste(
    c(
      ".form-select, .form-control {",
      "  font-family: var(--ndr-font) !important;",
      "  border: 1px solid var(--ndr-border) !important;",
      "  border-radius: 6px !important;",
      "  padding: 0.25rem 0.5rem !important;",
      "  font-size: var(--ndr-fs-sm, 12px) !important;",
      "  line-height: 1.5 !important;",
      "  color: var(--ndr-text);",
      "  background-color: var(--ndr-card-bg);",
      "}",
      # .form-select keeps the chevron — reserve right padding for it.
      ".form-select {",
      "  padding-right: 1.75rem !important;",
      "  background-position: right 0.5rem center !important;",
      "  background-size: 12px 12px !important;",
      "}",
      # Selectize.js widget — the actual visible Shiny selectInput body.
      # Match the .btn-sm sizing + brand border. min-height:0 overrides",
      # selectize's default min-height which would force a taller box.
      ".selectize-input {",
      "  font-family: var(--ndr-font) !important;",
      "  border: 1px solid var(--ndr-border) !important;",
      "  border-radius: 6px !important;",
      "  padding: 0.25rem 1.75rem 0.25rem 0.5rem !important;",
      "  font-size: var(--ndr-fs-sm, 12px) !important;",
      "  line-height: 1.5 !important;",
      "  min-height: 0 !important;",
      "  color: var(--ndr-text);",
      "  background-color: var(--ndr-card-bg);",
      "  box-shadow: none !important;",
      "}",
      ".selectize-input input {",
      "  font-family: var(--ndr-font) !important;",
      "  font-size: var(--ndr-fs-sm, 12px) !important;",
      "  line-height: 1.5 !important;",
      "}",
      # Selectize dropdown panel (the open option list) — brand-align too.
      ".selectize-dropdown {",
      "  font-family: var(--ndr-font) !important;",
      "  border: 1px solid var(--ndr-border) !important;",
      "  border-radius: 6px !important;",
      "  font-size: var(--ndr-fs-sm, 12px) !important;",
      "  background-color: var(--ndr-card-bg);",
      "  color: var(--ndr-text);",
      "}",
      ".selectize-dropdown .option.active, .selectize-dropdown .option:hover {",
      "  background: var(--ndr-secondary-bg) !important;",
      "  color: var(--ndr-text) !important;",
      "}"
    ),
    collapse = "\n"
  )

  # Card headers everywhere read brand text color so dark mode is legible
  # without per-callsite span wraps. bslib::card_header("Some Title") and
  # raw .card-header markup both pick this up.
  #
  # Targets both plain `.card-header` and bslib's more-specific
  # `.bslib-card > .card-header` so the brand rule isn't defeated by
  # specificity. !important is the safety net — bn_report's old unscoped
  # `.card-header { color: #333 }` rule used to leak through and
  # overpower this; that rule is now scoped to `.membership-card .card-header`,
  # but other future component CSS could re-introduce the conflict.
  # Sidebar control spacing — applies to every bslib::sidebar so apps
  # don't have to inline their own rules. The default bslib gap (~1rem)
  # PLUS Bootstrap's default `.form-group { margin-bottom: 15px }` add
  # up to ~30px between controls, which feels loose. Override both to
  # 0 and use a 12px margin-bottom on `.shiny-input-container` so each
  # control owns its own rhythm. Single source of truth: edit here and
  # every app's sidebar control spacing changes together.
  sidebar_controls <- paste(
    c(
      ".bslib-sidebar-layout > .sidebar > .sidebar-content {",
      "  gap: 0 !important;",
      "}",
      ".bslib-sidebar-layout .shiny-input-container.form-group {",
      "  margin-bottom: 12px;",
      "}"
    ),
    collapse = "\n"
  )

  # Navbar typography — pin the brand font + color on the navbar brand
  # (app title) and the navbar nav-links (top-level tabs). Bootstrap +
  # bslib don't set font-family on these explicitly, so inheritance from
  # body should work — but this guards against any specific selector
  # winning the cascade, AND against Inter not having loaded yet when
  # the navbar paints. Single source of truth: edits to --ndr-font
  # propagate everywhere automatically.
  navbar <- paste(
    c(
      ".navbar-brand {",
      "  font-family: var(--ndr-font) !important;",
      "  color: var(--ndr-text) !important;",
      "  font-weight: 600 !important;",
      "  font-size: var(--ndr-fs-lg, 15px) !important;",
      "}",
      ".navbar-brand:hover, .navbar-brand:focus {",
      "  color: var(--ndr-text) !important;",
      "}",
      ".navbar .nav-link {",
      "  font-family: var(--ndr-font) !important;",
      "  font-size: var(--ndr-fs-base, 14px) !important;",
      "}"
    ),
    collapse = "\n"
  )

  card_header <- paste(
    c(
      ".card-header, .bslib-card > .card-header {",
      "  color: var(--ndr-text) !important;",
      "  background-color: var(--ndr-card-bg) !important;",
      "  border-bottom: 1px solid var(--ndr-border) !important;",
      "}"
    ),
    collapse = "\n"
  )

  # Table-footer notes block — the small italic / muted text under
  # impact and prioritization tables. Both the network-drivers app
  # AND the bn_report HTML use these classes; centralizing here so
  # editing once updates both surfaces in lockstep.
  # `margin-top: 0 !important` is defensive — Bootstrap reboot, bn_report
  # media queries, and any future stray rule could add top margin and
  # push the footer away from the reactable; this pins it flush.
  table_footer_notes <- paste(
    c(
      ".impact-footer, .priort-footer {",
      "  margin-top: 0 !important;",
      "  padding: 4px 10px 10px 10px;",
      "  font-size: 12px;",
      "  color: var(--ndr-muted);",
      "}"
    ),
    collapse = "\n"
  )

  # Brand button class. SINGLE source of truth for every "outline-on-card"
  # action button across reports and apps — Download Workbook, Toggle View,
  # the visNetwork modebar buttons (when we migrate them off bespoke CSS),
  # and any future per-component action.
  #
  # Apply via:
  #   - class = "btn-rdx" on shiny::tags$button(...)
  #   - class = "btn-sm btn-rdx" on shiny::downloadButton(...) — bypasses
  #     downloadButton's default .btn.btn-default styling.
  # We also style the bare .shiny-download-link (downloadButton's default
  # selector) so any not-yet-migrated downloadButton still inherits the
  # brand surface automatically.
  brand_btn <- paste(
    c(
      ".btn-rdx, a.shiny-download-link {",
      "  background-color: var(--ndr-card-bg) !important;",
      "  border: 1px solid var(--ndr-border) !important;",
      "  color: var(--ndr-text) !important;",
      "  border-radius: 6px !important;",
      "  padding: 4px 10px !important;",
      "  font-size: var(--ndr-fs-sm, 12px) !important;",
      "  line-height: 1.5 !important;",
      "  text-decoration: none !important;",
      "}",
      ".btn-rdx:hover, a.shiny-download-link:hover {",
      "  background-color: var(--ndr-secondary-bg) !important;",
      "  color: var(--ndr-text) !important;",
      "  border-color: var(--ndr-border) !important;",
      "}",
      ".btn-rdx:focus, a.shiny-download-link:focus {",
      "  box-shadow: 0 0 0 0.2rem var(--ndr-focus) !important;",
      "  outline: 0 !important;",
      "}",
      ".btn-rdx:disabled, .btn-rdx.disabled,",
      "a.shiny-download-link:disabled, a.shiny-download-link.disabled {",
      "  opacity: 0.55 !important;",
      "  cursor: not-allowed !important;",
      "}"
    ),
    collapse = "\n"
  )

  # Canonical table conditional-formatting classes. SHARED: the showcase,
  # bn_report (Step 3) and the reactable theme (Step 6) all apply these
  # exact classes, so the rules are defined once here and never drift.
  #
  #  - .rdx-neg   : negative raw metric. ADDITIVE — an inset danger edge
  #                 that layers over the index colour scale; no text styling.
  #  - .rdx-insig : insignificant relationship. Blackout cell (replaces the
  #                 scale on purpose — "black = not meaningful").
  #  - .rdx-pval-*: p-value tier text colour (the one place text colour is
  #                 intended — it is the p-value column's whole purpose).
  #  The per-value index colour SCALE itself is value-driven and stays an
  #  inline background (computed by the shared scale helper at Step 3/6).
  # Conditional formatting, applied to every cell in the row EXCEPT the
  # p-value (including the row label):
  #   .rdx-neg   : negative raw metric → red, italic text (no border, no
  #                background change — layers over the index colour scale).
  #   .rdx-insig : insignificant relationship → blackout cell, white text.
  # A cell can be both: the compound .rdx-neg.rdx-insig rule (two-class
  # specificity, so it beats .rdx-insig regardless of order) keeps the
  # blackout background but restores red italic text.
  table_fmt <- paste(
    c(
      ".rdx-neg { color: var(--ndr-danger) !important; font-style: italic; }",
      ".rdx-insig {",
      "  background: #111 !important; color: #fff !important;",
      "}",
      ".rdx-neg.rdx-insig { color: var(--ndr-danger) !important; }",
      ".rdx-pval-sig   { color: var(--ndr-success) !important; font-weight: 700; }",
      ".rdx-pval-marg  { color: var(--ndr-warning) !important; font-weight: 600; }",
      ".rdx-pval-insig { color: var(--ndr-danger)  !important; font-weight: 600; }"
    ),
    collapse = "\n"
  )

  paste0(
    import, root_light, "\n", root_dark, "\n",
    tip, "\n", base, "\n", disabled, "\n", focus_ring, "\n",
    form_controls, "\n", sidebar_controls, "\n", navbar, "\n",
    card_header, "\n", table_footer_notes, "\n", brand_btn, "\n",
    table_fmt, "\n"
  )
}


#' resondex_tooltip_js
#' @description
#' JavaScript for the single shared tooltip. One floating element is created
#' once and reused; any element carrying a \code{data-tip} attribute shows
#' its text on hover, positioned near the cursor and flipped away from
#' viewport edges. Idempotent — safe to inject more than once per page.
#'
#' @return A single JS string (no \verb{<script>} wrapper).
#' @export
resondex_tooltip_js <- function() {
  paste(
    c(
      "(function () {",
      "  if (window.__resondexTipInit) return;",
      "  window.__resondexTipInit = true;",
      "  function ready(fn) {",
      "    if (document.readyState !== 'loading') fn();",
      "    else document.addEventListener('DOMContentLoaded', fn);",
      "  }",
      "  ready(function () {",
      "    var tip = document.createElement('div');",
      "    tip.className = 'resondex-tip';",
      "    document.body.appendChild(tip);",
      "    var cur = null;",
      "    function place(e) {",
      "      var pad = 14, vw = window.innerWidth, vh = window.innerHeight;",
      "      var r = tip.getBoundingClientRect();",
      "      var x = e.clientX + pad, y = e.clientY + pad;",
      "      if (x + r.width + 4 > vw) x = e.clientX - r.width - pad;",
      "      if (y + r.height + 4 > vh) y = e.clientY - r.height - pad;",
      "      if (x < 4) x = 4;",
      "      if (y < 4) y = 4;",
      "      tip.style.left = x + 'px';",
      "      tip.style.top = y + 'px';",
      "    }",
      "    document.addEventListener('mouseover', function (e) {",
      "      var t = e.target.closest ? e.target.closest('[data-tip]') : null;",
      "      if (!t) return;",
      "      cur = t;",
      "      tip.textContent = t.getAttribute('data-tip') || '';",
      "      if (!tip.textContent) return;",
      "      place(e);",
      "      tip.classList.add('is-visible');",
      "    });",
      "    document.addEventListener('mousemove', function (e) {",
      "      if (cur && tip.classList.contains('is-visible')) place(e);",
      "    });",
      "    document.addEventListener('mouseout', function (e) {",
      "      var t = e.target.closest ? e.target.closest('[data-tip]') : null;",
      "      if (t && t === cur) { cur = null; tip.classList.remove('is-visible'); }",
      "    });",
      "    document.addEventListener('scroll', function () {",
      "      if (cur) { cur = null; tip.classList.remove('is-visible'); }",
      "    }, true);",
      "  });",
      "})();"
    ),
    collapse = "\n"
  )
}


#' resondex_deps
#' @description
#' The complete brand front-end bundle as a single tag list:
#' \code{shinyjs::useShinyjs()}, the \code{resondex_css()} stylesheet, and
#' the \code{resondex_tooltip_js()} script. Drop this into any Shiny UI so
#' every current and future \code{app_deliverable_*} module pulls the brand
#' identically instead of repeating the incantation (prevents drift).
#'
#' @param include_import Passed to \code{\link{resondex_css}}. Default
#'   \code{FALSE} — apps load Inter via \code{resondex_theme()}; set
#'   \code{TRUE} only if the theme is not applied.
#' @return A \code{shiny::tagList}.
#' @export
resondex_deps <- function(include_import = FALSE) {
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$style(shiny::HTML(resondex_css(include_import = include_import))),
    shiny::tags$script(shiny::HTML(resondex_tooltip_js()))
  )
}


#' resondex_theme
#' @description
#' The Resondex \code{bslib::bs_theme} (Bootstrap 5). Sets the brand bg / fg
#' / primary / secondary / muted-semantics and Inter (with system fallback).
#' bslib derives the dark variant of standard components automatically when
#' \code{data-bs-theme} flips (e.g. via \code{bslib::input_dark_mode()}); the
#' brand's own \code{--ndr-*} / \code{--bs-*} surface tokens flip via
#' \code{\link{resondex_css}}, so chrome, dashboards and tables stay in
#' lockstep across both modes.
#'
#' @return A \code{bslib::bs_theme} object.
#' @export
resondex_theme <- function() {
  b  <- resondex_brand("light")
  cl <- b$colors
  sm <- b$semantic

  base_font <- bslib::font_collection(
    bslib::font_google("Inter", wght = c(400, 500, 600, 700), local = FALSE),
    "-apple-system", "BlinkMacSystemFont", "Segoe UI",
    "Roboto", "Helvetica", "Arial", "sans-serif"
  )

  theme <- bslib::bs_theme(
    version   = 5,
    bg        = cl$card_bg,
    fg        = cl$text,
    primary   = cl$accent,
    secondary = cl$secondary,
    success   = sm$success,
    info      = sm$info,
    warning   = sm$warning,
    danger    = sm$danger,
    base_font = base_font
  )

  bslib::bs_add_variables(
    theme,
    "border-radius" = b$radius,
    .where = "declarations"
  )
}


#' resondex_reactable_theme
#'
#' @description
#' Canonical brand-tokenized \code{\link[reactable]{reactableTheme}}. Use this
#' as the \code{theme} argument of any \code{reactable::reactable(...)} call
#' to get the standard Resondex table chrome: body, headers, footers, hover,
#' borders — all reading \code{var(--ndr-*)} so dark mode tracks automatically.
#'
#' Single source of truth: editing here updates every app/report's reactable
#' tables. Mirrors what bn_report HTML tables render so the on-screen Shiny
#' tables and the static report tables read identically.
#'
#' @return A reactable theme object suitable for the \code{theme} arg of
#'   \code{reactable::reactable()}.
#' @export
#' @examples
#' \dontrun{
#' reactable::reactable(
#'   iris,
#'   theme = resondex_reactable_theme()
#' )
#' }
resondex_reactable_theme <- function() {
  reactable::reactableTheme(
    color           = "var(--ndr-text)",
    backgroundColor = "var(--ndr-card-bg)",
    borderColor     = "var(--ndr-border)",
    stripedColor    = "transparent",
    highlightColor  = "var(--ndr-secondary-bg)",
    cellPadding     = "8px 10px",
    style           = list(
      fontFamily = "inherit",
      fontSize   = "14px",
      color      = "var(--ndr-text)"
    ),
    headerStyle     = list(
      fontWeight   = "600",
      color        = "var(--ndr-text)",
      background   = "var(--ndr-card-bg)",
      border       = "none",
      borderBottom = "1px solid var(--ndr-border)"
    ),
    footerStyle     = list(
      fontSize   = "12px",
      color      = "var(--ndr-muted)",
      background = "transparent",
      borderTop  = "1px solid var(--ndr-border)",
      # No bottom padding — the custom footer div sits 12px below the
      # reactable's <tfoot>, so the <tfoot>'s own bottom padding just
      # adds visible whitespace between the totals row and the custom
      # notes. Order: top right bottom left.
      padding    = "8px 10px 0 10px"
    )
  )
}
