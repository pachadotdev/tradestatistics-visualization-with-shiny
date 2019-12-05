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
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'embedtrade')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon("www/img/favicon.ico"),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.min.css")
  )
}
