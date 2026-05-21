#' plotly_theme
#'
#' @description Apply a theme to any plotly object. Pipe-friendly utility that
#'   sets layout-level properties (backgrounds, fonts, axes, legend, colorway)
#'   to match a named theme. Includes faithful ports of 19 highcharter themes
#'   and 10 native plotly templates.
#'
#' @param p A plotly object created by \code{plotly::plot_ly()} or
#'   \code{plotly::ggplotly()}.
#' @param theme Character. Theme name (case-sensitive). Use \code{"Default"} to
#'   leave the plot unchanged. See \code{\link{plotly_theme_names}} for all
#'   available options.
#'
#' @return The plotly object \code{p} with layout properties updated. When
#'   \code{theme = "Default"}, returns \code{p} unmodified.
#'
#' @details
#' Each theme sets: \code{paper_bgcolor}, \code{plot_bgcolor}, \code{font},
#' \code{title}, \code{xaxis} (grid/line/tick colors), \code{yaxis},
#' \code{legend}, and \code{colorway}. These are applied via
#' \code{plotly::layout()} so they can be further overridden by subsequent
#' \code{layout()} calls (e.g., for dark mode toggling in Shiny apps).
#'
#' Themes are organized in two families:
#' \itemize{
#'   \item \strong{Highcharter ports} (19 themes): Faithfully ported from the
#'     \href{https://github.com/jbkunst/highcharter}{highcharter} R package
#'     source. Each preserves the original hex colors, font families (with
#'     web-safe fallbacks), background colors, and grid styling.
#'     \cr Themes: "538", "Economist", "FT", "Google", "Flat", "Flat Dark",
#'     "Monokai", "Dark Unica", "Gridlight", "Sandsignika", "Superheroes",
#'     "Elementary", "Bloom", "ggplot2", "Highcharter", "Dotabuff", "Alone",
#'     "Handdrawn", "Smpl"
#'   \item \strong{Plotly native} (10 templates): Based on plotly.js built-in
#'     templates. R's plotly package does not resolve named template strings
#'     (that is a Python-layer feature), so these are implemented as explicit
#'     layout property lists.
#'     \cr Templates: "plotly", "plotly_white", "plotly_dark", "seaborn",
#'     "simple_white", "presentation", "ggplot2_native", "xgridoff",
#'     "ygridoff", "gridon"
#' }
#'
#' @seealso \code{\link{plotly_theme_names}} for listing all theme names,
#'   \code{\link{plotly_theme_colors}} for extracting a theme's color palette,
#'   \code{\link{plotly_modebar}} for custom toolbar buttons.
#'
#' @examples
#' \dontrun{
#' library(plotly)
#'
#' # Apply a theme to a bar chart
#' plot_ly(x = 1:10, y = rnorm(10), type = "bar") %>%
#'   plotly_theme("Economist")
#'
#' # Dark theme on a scatter plot
#' plot_ly(x = 1:10, y = rnorm(10), type = "scatter", mode = "markers") %>%
#'   plotly_theme("plotly_dark")
#'
#' # Combine with plotly_modebar for a complete config
#' plot_ly(x = 1:10, y = rnorm(10), type = "bar") %>%
#'   plotly_theme("538") %>%
#'   plotly::config(
#'     displaylogo = FALSE,
#'     modeBarButtons = plotly_modebar("my_chart")
#'   )
#' }
#'
#' @export
plotly_theme <- function(p, theme = "Default") {
  th <- .plotly_theme_def(theme)
  if (is.null(th)) return(p)

  # Global rule (applies to every theme): no vertical grid lines. Forced
  # here so per-theme defs don't need to be edited individually.
  if (is.null(th$xaxis)) th$xaxis <- list()
  th$xaxis$showgrid <- FALSE

  p %>%
    plotly::layout(
      paper_bgcolor = th$paper_bgcolor,
      plot_bgcolor  = th$plot_bgcolor,
      font          = th$font,
      title         = th$title,
      xaxis         = th$xaxis,
      yaxis         = th$yaxis,
      legend        = th$legend,
      hoverlabel    = th$hoverlabel,
      colorway      = th$colorway
    )
}


#' plotly_theme_names
#'
#' @description List all available theme names for \code{\link{plotly_theme}}.
#'   Returns a character vector of theme names: "Default" (the Resondex
#'   brand theme — light), "Default Dark" (the brand theme — dark),
#'   and a curated subset of highcharter ports + plotly native templates.
#'
#' @return Character vector of theme names, suitable for use as dropdown
#'   choices in Shiny apps or as the \code{theme} argument to
#'   \code{\link{plotly_theme}}.
#'
#' @seealso \code{\link{plotly_theme}}, \code{\link{plotly_theme_colors}}
#'
#' @examples
#' \dontrun{
#' # List all themes
#' plotly_theme_names()
#'
#' # Use in a Shiny selectInput
#' shiny::selectInput("theme", "Chart Theme:",
#'                    choices = plotly_theme_names())
#' }
#'
#' @export
plotly_theme_names <- function() {
  # Named character vector: names are display labels (shown in the
  # dropdown), values are the underlying theme keys (passed to
  # plotly_theme()). Underscores → spaces, title-case applied,
  # `ggplot2_native` displayed as "Native". Keeping the keys unchanged
  # preserves back-compat with any code that passes the raw key strings.
  c(
    # Resondex brand themes — "Default" is the branded light theme,
    # "Default Dark" the matching dark variant. "Default" also auto-
    # flips to the dark surface when the host app toggles dark mode.
    "Default"      = "Default",
    "Default Dark" = "Default Dark",
    # Highcharter ports (curated — removed: ggplot2, Highcharter)
    "538"         = "538",
    "Economist"   = "Economist",
    "FT"          = "FT",
    "Google"      = "Google",
    "Flat"        = "Flat",
    "Flat Dark"   = "Flat Dark",
    "Monokai"     = "Monokai",
    "Dark Unica"  = "Dark Unica",
    "Gridlight"   = "Gridlight",
    "Sandsignika" = "Sandsignika",
    "Superheroes" = "Superheroes",
    "Elementary"  = "Elementary",
    "Bloom"       = "Bloom",
    "Dotabuff"    = "Dotabuff",
    "Alone"       = "Alone",
    "Handdrawn"   = "Handdrawn",
    "Smpl"        = "Smpl",
    # Plotly native (curated — removed: xgridoff, ygridoff, gridon)
    "Plotly"       = "plotly",
    "Plotly White" = "plotly_white",
    "Plotly Dark"  = "plotly_dark",
    "Seaborn"      = "seaborn",
    "Simple White" = "simple_white",
    "Presentation" = "presentation",
    "Native"       = "ggplot2_native"
  )
}


#' plotly_theme_colors
#'
#' @description Return the colorway (series color palette) for a given theme.
#'   Useful when you need to manually assign trace colors or derive custom
#'   palettes from a theme's color scheme.
#'
#' @param theme Character. Theme name (case-sensitive). See
#'   \code{\link{plotly_theme_names}} for options.
#'
#' @return Character vector of hex color codes. Returns \code{character(0)} for
#'   \code{"Default"} (which applies no theming).
#'
#' @seealso \code{\link{plotly_theme}}, \code{\link{plotly_theme_names}}
#'
#' @examples
#' \dontrun{
#' # Get the Economist color palette
#' plotly_theme_colors("Economist")
#' # [1] "#6794a7" "#014d64" "#76c0c1" ...
#'
#' # Use the first color from a theme for a single trace
#' cols <- plotly_theme_colors("538")
#' plot_ly(x = 1:10, y = rnorm(10), type = "bar",
#'         marker = list(color = cols[1])) %>%
#'   plotly_theme("538")
#' }
#'
#' @export
plotly_theme_colors <- function(theme = "Default") {
  th <- .plotly_theme_def(theme)
  if (is.null(th)) return(character(0))
  th$colorway
}


# =============================================================================
# Internal: Path A dark-mode theme pairing
# =============================================================================
#
# Map a user-selected theme to its dark counterpart when the host app is in
# dark mode. Returns the input unchanged for themes without a paired dark
# variant (Economist, Monokai, etc. — they have opinionated palettes that
# shouldn't auto-flip). Already-dark themes also pass through unchanged.
#
# Used by every chart render reactive so toggling dark mode triggers a clean
# server-side re-render with the correct surface, no flicker.

#' @noRd
.plotly_dark_pair <- function(theme) {
  switch(
    theme,
    "Default"      = "Default Dark",
    "Flat"         = "Flat Dark",
    "plotly"       = "plotly_dark",
    "plotly_white" = "plotly_dark",
    theme
  )
}


# =============================================================================
# Internal: theme definition lookup
# =============================================================================

.plotly_theme_def <- function(theme_name) {
  switch(theme_name,

    # -------------------------------------------------------------------
    # Highcharter ports
    # -------------------------------------------------------------------

    "538" = list(
      paper_bgcolor = "#F0F0F0",
      plot_bgcolor  = "#F0F0F0",
      font = list(family = "Roboto, Arial, sans-serif", color = "#3C3C3C"),
      title = list(font = list(family = "Roboto, Arial, sans-serif",
                                color = "#3C3C3C", size = 18),
                   x = 0),
      xaxis = list(gridcolor = "#D7D7D8", linecolor = "#D7D7D8",
                   tickcolor = "#D7D7D8", zerolinecolor = "#D7D7D8",
                   title = list(font = list(color = "#A0A0A3"))),
      yaxis = list(gridcolor = "#D7D7D8", linecolor = "#D7D7D8",
                   tickcolor = "#D7D7D8", zerolinecolor = "#D7D7D8",
                   title = list(font = list(color = "#A0A0A3"))),
      legend = list(font = list(color = "#3C3C3C")),
      colorway = c("#FF2700", "#008FD5", "#77AB43", "#636464", "#C4C4C4")
    ),

    "Economist" = list(
      paper_bgcolor = "#d5e4eb",
      plot_bgcolor  = "#d5e4eb",
      font = list(family = "Droid Sans, Arial, sans-serif", color = "#3C3C3C"),
      title = list(font = list(family = "Droid Sans, Arial, sans-serif",
                                color = "#3C3C3C", size = 18),
                   x = 0),
      xaxis = list(gridcolor = "#FFFFFF", linecolor = "#FFFFFF",
                   tickcolor = "#D7D7D8", zerolinecolor = "#FFFFFF"),
      yaxis = list(gridcolor = "#FFFFFF", linecolor = "#FFFFFF",
                   tickcolor = "#D7D7D8", zerolinecolor = "#FFFFFF",
                   title = list(font = list(color = "#A0A0A3"))),
      legend = list(font = list(color = "#3C3C3C")),
      colorway = c("#6794a7", "#014d64", "#76c0c1", "#01a2d9", "#7ad2f6",
                   "#00887d", "#adadad", "#7bd3f6", "#7c260b", "#ee8f71",
                   "#76c0c1", "#a18376")
    ),

    "FT" = list(
      paper_bgcolor = "#FFF1E0",
      plot_bgcolor  = "#FFF1E0",
      font = list(family = "Droid Sans, Arial, sans-serif", color = "#777777"),
      title = list(font = list(family = "Droid Serif, Georgia, serif",
                                color = "#000000", size = 18),
                   x = 0),
      xaxis = list(gridcolor = "#CEC6B9", linecolor = "#CEC6B9",
                   tickcolor = "#CEC6B9", zerolinecolor = "#CEC6B9"),
      yaxis = list(gridcolor = "#CEC6B9", linecolor = "#CEC6B9",
                   tickcolor = "#CEC6B9", zerolinecolor = "#CEC6B9",
                   title = list(font = list(color = "#74736c"))),
      legend = list(font = list(color = "#3C3C3C")),
      colorway = c("#89736C", "#43423e", "#2e6e9e", "#FF0000", "#BEDDDE")
    ),

    "Google" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(family = "Roboto, Arial, sans-serif", color = "#444444"),
      title = list(font = list(family = "Roboto, Arial, sans-serif",
                                color = "#444444", size = 18)),
      xaxis = list(gridcolor = "#F3F3F3", linecolor = "#F3F3F3",
                   tickcolor = "#F3F3F3", zerolinecolor = "#F3F3F3",
                   showgrid = TRUE),
      yaxis = list(gridcolor = "#F3F3F3", linecolor = "#F3F3F3",
                   tickcolor = "#F3F3F3", zerolinecolor = "#F3F3F3"),
      legend = list(font = list(color = "#444444")),
      colorway = c("#0266C8", "#F90101", "#F2B50F", "#00933B")
    ),

    "Flat" = list(
      paper_bgcolor = "#ECF0F1",
      plot_bgcolor  = "#ECF0F1",
      font = list(family = "Arial, sans-serif", color = "#34495e"),
      title = list(font = list(color = "#34495e", size = 18)),
      xaxis = list(gridcolor = "#BDC3C7", linecolor = "#BDC3C7",
                   tickcolor = "#BDC3C7", zerolinecolor = "#BDC3C7",
                   griddash = "dash"),
      yaxis = list(gridcolor = "#BDC3C7", linecolor = "#BDC3C7",
                   tickcolor = "#BDC3C7", zerolinecolor = "#BDC3C7",
                   griddash = "dash"),
      legend = list(font = list(color = "#34495e")),
      colorway = c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c", "#34495e",
                   "#3498db", "#1abc9c", "#f39c12", "#d35400")
    ),

    "Flat Dark" = list(
      paper_bgcolor = "#34495e",
      plot_bgcolor  = "#34495e",
      font = list(family = "Arial, sans-serif", color = "#FFFFFF"),
      title = list(font = list(color = "#FFFFFF", size = 18)),
      xaxis = list(gridcolor = "#46627f", linecolor = "#46627f",
                   tickcolor = "#46627f", zerolinecolor = "#46627f",
                   griddash = "dash",
                   title = list(font = list(color = "#FFFFFF"))),
      yaxis = list(gridcolor = "#46627f", linecolor = "#46627f",
                   tickcolor = "#46627f", zerolinecolor = "#46627f",
                   griddash = "dash",
                   title = list(font = list(color = "#FFFFFF"))),
      legend = list(font = list(color = "#C0C0C0")),
      colorway = c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c", "#34495e",
                   "#3498db", "#1abc9c", "#f39c12", "#d35400")
    ),

    "Monokai" = list(
      paper_bgcolor = "#272822",
      plot_bgcolor  = "#272822",
      font = list(family = "Inconsolata, Courier New, monospace",
                  color = "#A2A39C"),
      title = list(font = list(family = "Inconsolata, Courier New, monospace",
                                color = "#A2A39C", size = 18),
                   x = 0),
      xaxis = list(gridcolor = "#A2A39C", linecolor = "#A2A39C",
                   tickcolor = "#A2A39C", zerolinecolor = "#A2A39C",
                   griddash = "dot", showgrid = TRUE),
      yaxis = list(gridcolor = "#A2A39C", linecolor = "#A2A39C",
                   tickcolor = "#A2A39C", zerolinecolor = "#A2A39C",
                   griddash = "dot"),
      legend = list(font = list(color = "#A2A39C")),
      colorway = c("#F92672", "#66D9EF", "#A6E22E", "#A6E22E")
    ),

    "Dark Unica" = list(
      paper_bgcolor = "#2a2a2b",
      plot_bgcolor  = "#2a2a2b",
      font = list(family = "Unica One, Arial, sans-serif", color = "#E0E0E3"),
      title = list(font = list(family = "Unica One, Arial, sans-serif",
                                color = "#E0E0E3", size = 20)),
      xaxis = list(gridcolor = "#707073", linecolor = "#707073",
                   tickcolor = "#707073", zerolinecolor = "#707073",
                   title = list(font = list(color = "#A0A0A3"))),
      yaxis = list(gridcolor = "#707073", linecolor = "#707073",
                   tickcolor = "#707073", zerolinecolor = "#707073",
                   title = list(font = list(color = "#A0A0A3"))),
      legend = list(font = list(color = "#E0E0E3")),
      colorway = c("#2b908f", "#90ee7e", "#f45b5b", "#7798BF", "#aaeeee",
                   "#ff0066", "#eeaaee", "#55BF3B")
    ),

    "Gridlight" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(family = "Dosis, Arial, sans-serif", color = "#333333"),
      title = list(font = list(family = "Dosis, Arial, sans-serif",
                                color = "#333333", size = 16)),
      xaxis = list(gridcolor = "#E0E0E0", linecolor = "#E0E0E0",
                   tickcolor = "#E0E0E0", zerolinecolor = "#E0E0E0",
                   showgrid = TRUE),
      yaxis = list(gridcolor = "#E0E0E0", linecolor = "#E0E0E0",
                   tickcolor = "#E0E0E0", zerolinecolor = "#E0E0E0"),
      legend = list(font = list(color = "#333333", size = 13)),
      colorway = c("#7CB5EC", "#F7A35C", "#90EE7E", "#7798BF", "#AAEEEE",
                   "#FF0066", "#EEAAEE", "#55BF3B")
    ),

    "Sandsignika" = list(
      paper_bgcolor = "#F5E6CC",
      plot_bgcolor  = "#F5E6CC",
      font = list(family = "Signika, Arial, serif", color = "#333333"),
      title = list(font = list(family = "Signika, Arial, serif",
                                color = "#000000", size = 16)),
      xaxis = list(gridcolor = "#D0D0D8", linecolor = "#D0D0D8",
                   tickcolor = "#D0D0D8", zerolinecolor = "#D0D0D8"),
      yaxis = list(gridcolor = "#D0D0D8", linecolor = "#D0D0D8",
                   tickcolor = "#D0D0D8", zerolinecolor = "#D0D0D8"),
      legend = list(font = list(color = "#333333", size = 13)),
      colorway = c("#F45B5B", "#8085E9", "#8D4654", "#7798BF", "#AAEEEE",
                   "#FF0066", "#EEAAEE", "#55BF3B", "#DF5353")
    ),

    "Superheroes" = list(
      paper_bgcolor = "#0B486B",
      plot_bgcolor  = "#0B486B",
      font = list(family = "Oswald, Arial, sans-serif", color = "#FFFFFF"),
      title = list(font = list(family = "Bangers, Impact, sans-serif",
                                color = "#FFFFFF", size = 24)),
      xaxis = list(gridcolor = "#46627f", linecolor = "#46627f",
                   tickcolor = "#46627f", zerolinecolor = "#46627f",
                   griddash = "dash",
                   title = list(font = list(color = "#FFFFFF"))),
      yaxis = list(gridcolor = "#46627f", linecolor = "#46627f",
                   tickcolor = "#46627f", zerolinecolor = "#46627f",
                   griddash = "dash",
                   title = list(font = list(color = "#FFFFFF"))),
      legend = list(font = list(color = "#FFFFFF")),
      colorway = c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c", "#34495e",
                   "#3498db", "#1abc9c", "#f39c12", "#d35400")
    ),

    "Elementary" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(family = "Open Sans, Arial, sans-serif", color = "#333333"),
      title = list(font = list(family = "Raleway, Arial, sans-serif",
                                color = "#333333", size = 18)),
      xaxis = list(gridcolor = "#F3F3F3", linecolor = "#F3F3F3",
                   tickcolor = "#F3F3F3", zerolinecolor = "#F3F3F3",
                   showgrid = TRUE),
      yaxis = list(gridcolor = "#F3F3F3", linecolor = "#F3F3F3",
                   tickcolor = "#F3F3F3", zerolinecolor = "#F3F3F3"),
      legend = list(font = list(color = "#333333")),
      colorway = c("#41B5E9", "#FA8832", "#34393C", "#E46151")
    ),

    "Bloom" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(family = "Roboto, Arial, sans-serif", color = "#000000"),
      title = list(font = list(family = "Roboto, Arial, sans-serif",
                                color = "#000000", size = 18),
                   x = 0),
      xaxis = list(gridcolor = "#F3F3F3", linecolor = "#000000",
                   tickcolor = "#000000", zerolinecolor = "#000000",
                   linewidth = 2),
      yaxis = list(gridcolor = "#F3F3F3", linecolor = "#CEC6B9",
                   tickcolor = "#CEC6B9", zerolinecolor = "#F3F3F3",
                   title = list(font = list(color = "#000000"))),
      legend = list(font = list(color = "#3C3C3C"),
                    orientation = "h", x = 0, y = 1.02,
                    xanchor = "left", yanchor = "bottom"),
      colorway = c("#E10033", "#000000", "#767676", "#E4E4E4")
    ),

    "ggplot2" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#EBEBEB",
      font = list(family = "Arial, sans-serif", color = "#000000"),
      title = list(font = list(color = "#000000", size = 18)),
      xaxis = list(gridcolor = "#FFFFFF", linecolor = "#666666",
                   tickcolor = "#666666", zerolinecolor = "#FFFFFF",
                   gridwidth = 1.5,
                   title = list(font = list(color = "#000000"))),
      yaxis = list(gridcolor = "#FFFFFF", linecolor = "#666666",
                   tickcolor = "#666666", zerolinecolor = "#FFFFFF",
                   gridwidth = 1.5,
                   title = list(font = list(color = "#000000"))),
      legend = list(font = list(color = "#000000")),
      colorway = c("#595959", "#F8766D", "#A3A500", "#00BF7D",
                   "#00B0F6", "#E76BF3")
    ),

    "Highcharter" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(family = "IBM Plex Sans, Arial, sans-serif",
                  color = "#666666"),
      title = list(font = list(family = "Alegreya Sans SC, Arial, sans-serif",
                                color = "#333333", size = 24),
                   x = 0),
      xaxis = list(gridcolor = "#F3F3F3", linecolor = "#F3F3F3",
                   tickcolor = "#F3F3F3", zerolinecolor = "#F3F3F3",
                   showgrid = TRUE),
      yaxis = list(gridcolor = "#F3F3F3", linecolor = "#F3F3F3",
                   tickcolor = "#F3F3F3", zerolinecolor = "#F3F3F3"),
      legend = list(font = list(color = "#A2A39C")),
      colorway = c("#47475c", "#61BC7B", "#508CC8", "#F49952",
                   "#9C9EDB", "#6699a1")
    ),

    "Dotabuff" = list(
      paper_bgcolor = "#242F39",
      plot_bgcolor  = "#242F39",
      font = list(family = "Arial, sans-serif", color = "#FFFFFF"),
      title = list(font = list(color = "#FFFFFF", size = 18)),
      xaxis = list(gridcolor = "#2E3740", linecolor = "#2E3740",
                   tickcolor = "#2E3740", zerolinecolor = "#2E3740",
                   title = list(font = list(color = "#FFFFFF"))),
      yaxis = list(gridcolor = "#2E3740", linecolor = "#2E3740",
                   tickcolor = "#2E3740", zerolinecolor = "#2E3740",
                   title = list(font = list(color = "#FFFFFF"))),
      legend = list(font = list(color = "#C0C0C0")),
      colorway = c("#A9CF54", "#C23C2A", "#FFFFFF", "#979797", "#FBB829")
    ),

    "Alone" = list(
      paper_bgcolor = "#161C20",
      plot_bgcolor  = "#161C20",
      font = list(family = "Roboto, Arial, sans-serif", color = "#666666"),
      title = list(font = list(family = "Roboto Condensed, Arial, sans-serif",
                                color = "#CCCCCC", size = 18),
                   x = 0),
      xaxis = list(gridcolor = "#424242", linecolor = "#424242",
                   tickcolor = "#424242", zerolinecolor = "#424242",
                   showgrid = TRUE),
      yaxis = list(gridcolor = "#424242", linecolor = "#424242",
                   tickcolor = "#424242", zerolinecolor = "#424242"),
      legend = list(font = list(color = "#424242")),
      colorway = c("#d35400", "#2980b9", "#2ecc71", "#f1c40f",
                   "#2c3e50", "#7f8c8d")
    ),

    "Handdrawn" = list(
      paper_bgcolor = "#FFFFEF",
      plot_bgcolor  = "#FFFFEF",
      font = list(family = "Berkshire Swash, cursive, serif",
                  color = "#000000"),
      title = list(font = list(family = "Berkshire Swash, cursive, serif",
                                color = "#000000", size = 24)),
      xaxis = list(gridcolor = "transparent", linecolor = "#000000",
                   tickcolor = "#000000", zerolinecolor = "#000000"),
      yaxis = list(gridcolor = "transparent", linecolor = "#000000",
                   tickcolor = "#000000", zerolinecolor = "#000000"),
      legend = list(font = list(color = "#000000", size = 14)),
      colorway = c("#171314", "#3F3E38", "#68695D", "#888782")
    ),

    "Smpl" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(family = "Roboto, Arial, sans-serif", color = "#666666"),
      title = list(font = list(family = "Roboto Condensed, Arial, sans-serif",
                                color = "#333333", size = 18),
                   x = 0),
      xaxis = list(gridcolor = "#F3F3F3", linecolor = "#F3F3F3",
                   tickcolor = "#F3F3F3", zerolinecolor = "#F3F3F3",
                   showgrid = TRUE),
      yaxis = list(gridcolor = "#F3F3F3", linecolor = "#F3F3F3",
                   tickcolor = "#F3F3F3", zerolinecolor = "#F3F3F3"),
      legend = list(font = list(color = "#666666")),
      colorway = c("#d35400", "#2980b9", "#2ecc71", "#f1c40f",
                   "#2c3e50", "#7f8c8d")
    ),

    # -------------------------------------------------------------------
    # Plotly native templates
    # -------------------------------------------------------------------

    "plotly" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#E5ECF6",
      font = list(family = "Open Sans, verdana, arial, sans-serif",
                  color = "#2a3f5f"),
      title = list(font = list(color = "#2a3f5f", size = 17)),
      xaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8"),
      yaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8"),
      legend = list(font = list(color = "#2a3f5f")),
      colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#FFA15A",
                   "#19d3f3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52")
    ),

    "plotly_white" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(family = "Open Sans, verdana, arial, sans-serif",
                  color = "#2a3f5f"),
      title = list(font = list(color = "#2a3f5f", size = 17)),
      xaxis = list(gridcolor = "#EBF0F8", linecolor = "#EBF0F8",
                   tickcolor = "", zerolinecolor = "#EBF0F8",
                   ticks = ""),
      yaxis = list(gridcolor = "#EBF0F8", linecolor = "#EBF0F8",
                   tickcolor = "", zerolinecolor = "#EBF0F8",
                   ticks = ""),
      legend = list(font = list(color = "#2a3f5f")),
      colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#FFA15A",
                   "#19d3f3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52")
    ),

    "plotly_dark" = list(
      paper_bgcolor = "rgb(17,17,17)",
      plot_bgcolor  = "rgb(17,17,17)",
      font = list(family = "Open Sans, verdana, arial, sans-serif",
                  color = "#f2f5fa"),
      title = list(font = list(color = "#f2f5fa", size = 17)),
      xaxis = list(gridcolor = "#283442", linecolor = "#506784",
                   tickcolor = "#506784", zerolinecolor = "#283442",
                   ticks = "",
                   title = list(font = list(color = "#f2f5fa"))),
      yaxis = list(gridcolor = "#283442", linecolor = "#506784",
                   tickcolor = "#506784", zerolinecolor = "#283442",
                   ticks = "",
                   title = list(font = list(color = "#f2f5fa"))),
      legend = list(font = list(color = "#f2f5fa")),
      colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#FFA15A",
                   "#19d3f3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52")
    ),

    "seaborn" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#EAEAF2",
      font = list(family = "Arial, sans-serif", color = "#2a3f5f"),
      title = list(font = list(color = "#2a3f5f", size = 17)),
      xaxis = list(gridcolor = "#FFFFFF", linecolor = "#FFFFFF",
                   tickcolor = "", zerolinecolor = "#FFFFFF",
                   ticks = ""),
      yaxis = list(gridcolor = "#FFFFFF", linecolor = "#FFFFFF",
                   tickcolor = "", zerolinecolor = "#FFFFFF",
                   ticks = ""),
      legend = list(font = list(color = "#2a3f5f")),
      colorway = c("#4C72B0", "#DD8452", "#55A868", "#C44E52", "#8172B3",
                   "#937860", "#DA8BC3", "#8C8C8C", "#CCB974", "#64B5CD")
    ),

    "simple_white" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(family = "Arial, sans-serif", color = "#2a3f5f"),
      title = list(font = list(color = "#2a3f5f", size = 17)),
      xaxis = list(gridcolor = "", linecolor = "#2a3f5f",
                   tickcolor = "#2a3f5f", zerolinecolor = "#2a3f5f",
                   showgrid = FALSE, ticks = "outside",
                   showline = TRUE, mirror = TRUE),
      yaxis = list(gridcolor = "", linecolor = "#2a3f5f",
                   tickcolor = "#2a3f5f", zerolinecolor = "#2a3f5f",
                   showgrid = FALSE, ticks = "outside",
                   showline = TRUE, mirror = TRUE),
      legend = list(font = list(color = "#2a3f5f")),
      colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#FFA15A",
                   "#19d3f3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52")
    ),

    "presentation" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#E5ECF6",
      font = list(family = "Open Sans, verdana, arial, sans-serif",
                  color = "#2a3f5f", size = 18),
      title = list(font = list(color = "#2a3f5f", size = 24)),
      xaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8",
                   title = list(font = list(size = 20))),
      yaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8",
                   title = list(font = list(size = 20))),
      legend = list(font = list(color = "#2a3f5f", size = 16)),
      colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#FFA15A",
                   "#19d3f3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52")
    ),

    "ggplot2_native" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#E5E5E5",
      font = list(family = "Arial, sans-serif", color = "#000000"),
      title = list(font = list(color = "#000000", size = 17)),
      xaxis = list(gridcolor = "#FFFFFF", linecolor = "#FFFFFF",
                   tickcolor = "", zerolinecolor = "#FFFFFF",
                   ticks = "", showgrid = TRUE),
      yaxis = list(gridcolor = "#FFFFFF", linecolor = "#FFFFFF",
                   tickcolor = "", zerolinecolor = "#FFFFFF",
                   ticks = "", showgrid = TRUE),
      legend = list(font = list(color = "#000000"),
                    bgcolor = "#E5E5E5", bordercolor = "#E5E5E5"),
      colorway = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6",
                   "#E76BF3")
    ),

    "xgridoff" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#E5ECF6",
      font = list(family = "Open Sans, verdana, arial, sans-serif",
                  color = "#2a3f5f"),
      title = list(font = list(color = "#2a3f5f", size = 17)),
      xaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8",
                   showgrid = FALSE),
      yaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8"),
      legend = list(font = list(color = "#2a3f5f")),
      colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#FFA15A",
                   "#19d3f3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52")
    ),

    "ygridoff" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#E5ECF6",
      font = list(family = "Open Sans, verdana, arial, sans-serif",
                  color = "#2a3f5f"),
      title = list(font = list(color = "#2a3f5f", size = 17)),
      xaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8"),
      yaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8",
                   showgrid = FALSE),
      legend = list(font = list(color = "#2a3f5f")),
      colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#FFA15A",
                   "#19d3f3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52")
    ),

    "gridon" = list(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#E5ECF6",
      font = list(family = "Open Sans, verdana, arial, sans-serif",
                  color = "#2a3f5f"),
      title = list(font = list(color = "#2a3f5f", size = 17)),
      xaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8",
                   showgrid = TRUE),
      yaxis = list(gridcolor = "#FFFFFF", linecolor = "#EBF0F8",
                   tickcolor = "#EBF0F8", zerolinecolor = "#EBF0F8",
                   showgrid = TRUE),
      legend = list(font = list(color = "#2a3f5f")),
      colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#FFA15A",
                   "#19d3f3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52")
    ),

    # -------------------------------------------------------------------
    # Default — the Resondex brand theme (built from resondex_brand()$viz).
    # Comprehensive: backgrounds, Inter, axes/grid, legend, hover and a
    # cohesive colorway so it themes bar / line / scatter / hist / box.
    # Previously the no-op pass-through; now the branded default everywhere.
    # -------------------------------------------------------------------
    "Default" = local({
      v  <- resondex_brand()$viz
      ff <- "Inter, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif"
      list(
        paper_bgcolor = v$paper_bg,
        plot_bgcolor  = v$plot_bg,
        font  = list(family = ff, color = v$font_color, size = 13),
        title = list(font = list(family = ff, color = v$font_color,
                                  size = 16), x = 0),
        xaxis = list(gridcolor = v$grid, linecolor = v$grid,
                     tickcolor = v$grid, zerolinecolor = v$grid,
                     tickfont = list(color = v$axis_text),
                     title = list(font = list(color = v$axis_text))),
        yaxis = list(gridcolor = v$grid, linecolor = v$grid,
                     tickcolor = v$grid, zerolinecolor = v$grid,
                     tickfont = list(color = v$axis_text),
                     title = list(font = list(color = v$axis_text))),
        legend = list(font = list(color = v$font_color),
                      bgcolor = "rgba(0,0,0,0)"),
        hoverlabel = list(
          bgcolor = "rgba(0,0,0,0.78)",
          bordercolor = "rgba(0,0,0,0)",
          font = list(family = ff, color = "#ffffff", size = 12)
        ),
        colorway = v$colorway
      )
    }),

    # Back-compat aliases for the previous brand-theme names.
    "Resondex"      = .plotly_theme_def("Default"),
    "Resondex Dark" = .plotly_theme_def("Default Dark"),

    # -------------------------------------------------------------------
    # Default Dark — paired dark variant of the brand theme. Backed by
    # resondex_brand()$viz_dark so surface colors mirror the dark UI tokens.
    # -------------------------------------------------------------------
    "Default Dark" = local({
      v  <- resondex_brand()$viz_dark
      ff <- "Inter, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif"
      list(
        paper_bgcolor = v$paper_bg,
        plot_bgcolor  = v$plot_bg,
        font  = list(family = ff, color = v$font_color, size = 13),
        title = list(font = list(family = ff, color = v$font_color,
                                  size = 16), x = 0),
        xaxis = list(gridcolor = v$grid, linecolor = v$grid,
                     tickcolor = v$grid, zerolinecolor = v$grid,
                     tickfont = list(color = v$axis_text),
                     title = list(font = list(color = v$axis_text))),
        yaxis = list(gridcolor = v$grid, linecolor = v$grid,
                     tickcolor = v$grid, zerolinecolor = v$grid,
                     tickfont = list(color = v$axis_text),
                     title = list(font = list(color = v$axis_text))),
        legend = list(font = list(color = v$font_color),
                      bgcolor = "rgba(0,0,0,0)"),
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0.92)",
          bordercolor = "rgba(0,0,0,0)",
          font = list(family = ff, color = "#212529", size = 12)
        ),
        colorway = v$colorway
      )
    }),

    # Unknown theme name → NULL → plotly_theme() returns plot unchanged
    NULL
  )
}
