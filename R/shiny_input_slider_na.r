#' shiny_input_slider_na_ui
#' @description shiny_input_slider_na_ui
#' @export
shiny_input_slider_na_ui <- function(id, label = NULL, min = 1, max = 5, value = 3, step = 1, cb_label = "NA", cb_value = FALSE) {
  ns <- NS(id)

  tagList(
    # Optional label above the row
    if (!is.null(label)) tags$label(label, style = "font-weight: bold; display: block; margin-bottom: 5px;"),

    # Relative container for slider + checkbox
    div(style = "position: relative; display: flex; align-items: center; gap: 10px;",
        # Slider takes 70%
        div(style = "flex: 0 0 70%;",
            sliderInput(
              ns("slider"),
              label = NULL,
              min = min, max = max, value = value, step = step
            )
        ),
        # Checkbox
        div(style = "flex: 0 0 30%; display: flex; align-items: center; height: 60px;",
            checkboxInput(ns("na_check"), label = cb_label, value = cb_value)
        )
    )
  )
}



#' shiny_input_slider_na_server
#' @description shiny_input_slider_na_server
#' @export
shiny_input_slider_na_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Disable/enable slider when NA checkbox changes
    observeEvent(input$na_check, {
      shinyjs::toggleState("slider", condition = !input$na_check)
    }, ignoreInit = TRUE)

    reactive({
      if (input$na_check) NA else input$slider
    })
  })
}



#' shiny_input_slider_na_demo
#' @description shiny_input_slider_na_demo
#' @export
shiny_input_slider_na_demo <- function(){

  library(shiny)
  library(bslib)
  library(shinyjs)

  ui <- fluidPage(
    useShinyjs(),  # initialize shinyjs
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    titlePanel("Modular Slider with NA Option"),

    sidebarLayout(
      sidebarPanel(
        shiny_input_slider_na_ui(
          "example_slider",
          label = "Select a value (1-5):",
          min = 1, max = 5, value = 3, step = 1
        )
      ),

      mainPanel(
        h3("Current Value"),
        verbatimTextOutput("current_value")
      )
    )
  )

  server <- function(input, output, session) {
    slider_val <- shiny_input_slider_na_server("example_slider")

    output$current_value <- renderPrint({
      slider_val()
    })
  }

  shinyApp(ui, server)
}

