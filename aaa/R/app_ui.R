#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      fluidRow(
        useShinyjs(),
        
        div(
          id = "content",
          column(
            12,
            style = "height:100vh",
            htmlOutput("title", container = tags$h2),
            highchartOutput("exports_treemap_detailed", height = "95%")
          )
        ),
        
        hidden(
          div(
            id = "controls",
            column(
              4,
              # Controls ----------------------------------------------------------------
              
              selectInput(
                "y",
                "Year:",
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
        ),
        
        tags$footer(
          tags$link(rel = "shortcut icon", href = "img/favicon.ico")
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'aaa')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.min.css")
  )
}
