#' plotly_modebar
#'
#' @description Build a custom modebar button list for plotly charts. Replaces
#'   the default plotly toolbar with Download PNG (camera icon), Download SVG
#'   (download icon), and optionally Download CSV (file icon) buttons, plus
#'   standard navigation tools (zoom, pan, reset, etc.).
#'
#'   Designed as a standalone utility — pass the result to the
#'   \code{modeBarButtons} argument of \code{plotly::config()}.
#'
#' @param filename Character. Base filename for PNG and SVG image downloads
#'   (without file extension). Defaults to \code{"chart"}.
#' @param csv_id Character or \code{NULL}. HTML element ID to trigger on click
#'   for CSV download (e.g., a hidden \code{shiny::downloadButton} ID). When
#'   \code{NULL} (the default), the CSV button is omitted from the toolbar.
#' @param nav_buttons Character vector of standard plotly modebar button names
#'   to include after the custom buttons. Defaults to \code{c("zoom2d",
#'   "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d")}. Set to
#'   \code{character(0)} to omit all navigation buttons.
#'
#' @return A list (length 1) containing a list of button definitions, suitable
#'   for the \code{modeBarButtons} argument of \code{plotly::config()}.
#'
#' @details
#' Custom button icons use Material Design SVG paths at 24x24 viewbox:
#' \itemize{
#'   \item \strong{Download PNG}: Camera icon. Calls
#'     \code{Plotly.downloadImage()} with \code{format: 'png'}.
#'   \item \strong{Download SVG}: Download arrow icon. Calls
#'     \code{Plotly.downloadImage()} with \code{format: 'svg'}.
#'   \item \strong{Download CSV}: File/document icon. Clicks the HTML element
#'     specified by \code{csv_id} (intended for a Shiny download handler).
#' }
#'
#' @examples
#' \dontrun{
#' library(plotly)
#'
#' # Basic usage — PNG + SVG downloads and nav tools
#' plot_ly(x = 1:10, y = rnorm(10), type = "bar") %>%
#'   plotly::config(
#'     displaylogo = FALSE,
#'     modeBarButtons = plotly_modebar("my_chart")
#'   )
#'
#' # With CSV download button (Shiny apps)
#' plot_ly(x = 1:10, y = rnorm(10), type = "bar") %>%
#'   plotly::config(
#'     displaylogo = FALSE,
#'     modeBarButtons = plotly_modebar("report", csv_id = "dl_csv")
#'   )
#'
#' # Minimal toolbar — downloads only, no nav buttons
#' plot_ly(x = 1:10, y = rnorm(10), type = "bar") %>%
#'   plotly::config(
#'     displaylogo = FALSE,
#'     modeBarButtons = plotly_modebar("clean", nav_buttons = character(0))
#'   )
#' }
#'
#' @export
plotly_modebar <- function(filename = "chart", csv_id = NULL,
                           nav_buttons = c("zoom2d", "pan2d", "zoomIn2d",
                                           "zoomOut2d", "autoScale2d",
                                           "resetScale2d")) {

  png_btn <- list(
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
      paste0("  Plotly.downloadImage(gd, {format: 'png', filename: '",
             filename, "'});"),
      "}"
    )
  )

  svg_btn <- list(
    name = "Download SVG",
    icon = list(
      path = "M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z",
      width = 24, height = 24
    ),
    click = htmlwidgets::JS(
      "function(gd) {",
      paste0("  Plotly.downloadImage(gd, {format: 'svg', filename: '",
             filename, "'});"),
      "}"
    )
  )

  btns <- list(png_btn, svg_btn)

  if (!is.null(csv_id)) {
    csv_btn <- list(
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
        paste0("  document.getElementById('", csv_id, "').click();"),
        "}"
      )
    )
    btns <- c(btns, list(csv_btn))
  }

  btns <- c(btns, as.list(nav_buttons))
  list(btns)
}
