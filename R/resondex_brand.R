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
      # opt-in "Resondex" plotly theme; cohesive with the UI tokens.
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
