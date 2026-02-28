#' app_deliverable
#'
#' @description Launch a modular Shiny deliverable app that hosts one or more
#'   analysis modules (e.g., TURF, MaxDiff) as tabs in a single navbar. Each
#'   module contributes its own tabs, server logic, CSS, and head tags.
#'
#' @param title Character. Navbar title displayed at the top of the app.
#'   Defaults to \code{"Project Name (#XXXXXXX)"}.
#' @param modules List of module definition lists, each created by an
#'   \code{app_deliverable_add_*()} function (e.g.,
#'   \code{app_deliverable_add_turf()}). Each module must contain:
#'   \describe{
#'     \item{id}{Character. Unique namespace identifier.}
#'     \item{tabs}{List of \code{bslib::nav_panel} objects.}
#'     \item{server}{Function with signature
#'       \code{function(input, output, session, dark_mode)}.}
#'     \item{css}{Character string of CSS or \code{NULL}.}
#'     \item{head_tags}{List of additional \code{<head>} tags or \code{NULL}.}
#'   }
#' @param nested Logical. If \code{FALSE} (default), all module tabs are
#'   flattened into the top-level navbar. If \code{TRUE}, each module gets
#'   its own top-level nav panel (title derived from the list name, with
#'   underscores replaced by spaces), and the module's tabs become nested
#'   card tabs inside that panel.
#' @param theme Character (Bootswatch theme name) or a \code{bslib::bs_theme}
#'   object. Default \code{NULL} uses \code{"flatly"}. See
#'   \url{https://bootswatch.com/} for available themes.
#'
#' @return A \code{shiny::shinyApp} object.
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
#'
#' # Nested layout — each module gets its own top-level tab
#' app_deliverable(
#'   title   = "Multi-Study (#1234567)",
#'   modules = list(Brand_Health = mod1, Ad_Tracker = mod2),
#'   nested  = TRUE
#' )
#' }
#'
#' @export
app_deliverable <- function(
    title = "Project Name (#XXXXXXX)",
    modules = list(),
    nested = FALSE,
    theme = NULL
) {

  if (is.null(theme)) theme <- "flatly"
  if (is.character(theme)) {
    theme <- bslib::bs_theme(version = 5, bootswatch = theme)
  }

  # ---- Collect UI pieces from modules ----
  all_tabs <- list()
  all_css <- character(0)
  all_head_tags <- list()
  mod_names <- names(modules)

  for (i in seq_along(modules)) {
    mod <- modules[[i]]
    if (!is.null(mod$css)) all_css <- c(all_css, mod$css)
    if (!is.null(mod$head_tags)) all_head_tags <- c(all_head_tags, mod$head_tags)

    if (nested) {
      # Derive panel title from list name (replace _ with space)
      panel_title <- if (!is.null(mod_names) && nchar(mod_names[i]) > 0) {
        gsub("_", " ", mod_names[i])
      } else {
        mod$id
      }
      # Wrap module tabs inside a top-level nav_panel with nested card tabs
      nested_panel <- bslib::nav_panel(
        panel_title,
        do.call(bslib::navset_card_tab, mod$tabs)
      )
      all_tabs <- c(all_tabs, list(nested_panel))
    } else {
      all_tabs <- c(all_tabs, mod$tabs)
    }
  }

  # ---- Build header ----
  header_parts <- list()
  if (length(all_css) > 0) {
    header_parts <- c(header_parts, list(
      shiny::tags$style(shiny::HTML(paste(all_css, collapse = "\n")))
    ))
  }
  header_parts <- c(header_parts, all_head_tags)
  header <- if (length(header_parts) > 0) {
    shiny::tags$head(header_parts)
  } else {
    NULL
  }

  # ---- Assemble UI ----
  ui <- do.call(
    bslib::page_navbar,
    c(
      list(title = title, theme = theme, header = header),
      all_tabs,
      list(
        bslib::nav_spacer(),
        bslib::nav_item(bslib::input_dark_mode(id = "dark_mode"))
      )
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {
    # Shared dark mode reactive — passed to each module
    dark_mode <- shiny::reactive({
      isTRUE(input$dark_mode == "dark")
    })

    for (mod in modules) {
      local({
        m <- mod
        shiny::moduleServer(m$id, function(input, output, session) {
          m$server(input, output, session, dark_mode = dark_mode)
        })
      })
    }
  }

  shiny::shinyApp(ui, server)
}
