## ui.R ##

shinyUI(
  function(request) {
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
            highchartOutput("trade_aggregated", height = "92%"),
            p("Source: Open Trade Statistics.")
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
                choices = available_years_min:available_years_max,
                selected = NULL,
                selectize = FALSE
              ),

              selectInput(
                "y2",
                "Year 2:",
                choices = available_years_min:available_years_max,
                selected = NULL,
                selectize = FALSE
              )
            ),

            column(
              4,
              selectInput(
                "r",
                "Reporter:",
                choices = c("Select", available_reporters_iso),
                selected = NULL,
                selectize = FALSE
              )
            ),

            column(
              4,
              selectInput(
                "p",
                "Partner:",
                choices = c("Select", available_reporters_iso),
                selected = NULL,
                selectize = FALSE
              )
            )
          )
        ),

        tags$footer(
          tags$link(rel = "shortcut icon", href = "https://tradestatistics.io/images/favicon.ico"),
          tags$script(src = "js/copy-url.js")
        )
      )
    )
  }
)
