#' ui
#' @importFrom highcharter highchartOutput
app_ui <- function(request) {
  fluidPage(
    theme = "css/custom.min.css",

    fluidRow(
      useShinyjs(),

      div(
        id = "content",
        column(
          12,
          style = "height:100vh",
          htmlOutput("title", container = tags$h2),
          highcharter::highchartOutput("trade_bars_aggregated", height = "95%")
        )
      ),

      hidden(
        div(
          id = "controls",
          column(
            4,
            # Controls ----------------------------------------------------------------

            selectInput(
              "y1",
              "Year 1:",
              choices = NULL,
              selected = NULL,
              selectize = FALSE
            ),

            selectInput(
              "y2",
              "Year 2:",
              choices = NULL,
              selected = NULL,
              selectize = FALSE
            )
          ),

          column(
            4,
            selectInput(
              "r",
              "Reporter:",
              choices = NULL, # c("Select", golem::get_golem_options("available_reporters_iso")),
              selected = NULL,
              selectize = FALSE
            )
          ),

          column(
            4,
            selectInput(
              "p",
              "Partner:",
              choices = NULL,
              selected = NULL,
              selectize = FALSE
            )
          ),

          column(
            12,
            align = "center",
            actionButton("go", "Go!")
          )
        )
      )
    )
  )
}

#' @import shiny
#' @importFrom golem activate_js favicon
golem_add_external_resources <- function() {
  addResourcePath(
    "www", system.file("app/www", package = "embedtrade")
  )

  tags$head(
    activate_js(),
    favicon("www/img/favicon.ico"),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.min.css")
  )
}
