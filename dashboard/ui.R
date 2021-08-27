## ui.R ##

shinyUI(
  function(request) {
    dashboardPage(
      skin = styles$skin_color,
      theme = styles$css_files,
      sidebar_mini = FALSE,

      dashboardHeader(
        title = "OTS beta dashboard"
      ),

      dashboardSidebar(
        sidebarMenu(
          # Controls ----------------------------------------------------------------

          # hr(),
          # HTML("<center><h4>Explore</h4></center>"),
          
          menuItem("Visualize", tabName = "visualize"),
          
          menuItem("Model", tabName = "model", badgeLabel = "new", badgeColor = "green"),
      
          menuItem("Share", tabName = "share"),
          menuItem("Download", tabName = "download"),
          menuItem("Cite", tabName = "cite")
        )
      ),

      dashboardBody(
        column(
          12,
          htmlOutput("title", container = tags$p),
          htmlOutput("title_legend", container = tags$i)
        ),
        
        tabItems(
          #div(
          #  id = "contents",
          tabItem(
            tabName = "visualize",

            column(
              12,
              hr(),
              h2("Filter")
            ),
            
            column(
              4,
              sliderInput(
                "y",
                "Years:",
                # min = available_years_min,
                min = 1990,
                max = available_years_max,
                value = c(available_years_max - 4, 2019),
                sep = "",
                step = 2,
                ticks = FALSE,
                width = "100%"
              )
            ),
              
            column(
              4,
              selectInput(
                "r",
                "Reporter:",
                choices = available_reporters_iso[available_reporters_iso != "all"],
                selected = "usa",
                selectize = TRUE,
                width = "100%"
              )
            ),
             
            column(
              4,
              selectInput(
                "p",
                "Partner:",
                choices = available_reporters_iso,
                selected = "",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            # Trade -------------------------------------------------------------------

            column(
              12,
              hr(),
              h2("Summary")
            ),
            
            column(
              3,
              h3("Exports"),
              htmlOutput("trade_summary_exp", container = tags$p),
              h3("Imports"),
              htmlOutput("trade_summary_imp", container = tags$p)
            ),
            
            column(
              9,
              htmlOutput("trade_subtitle", container = tags$h2),
              br(),
              highchartOutput("trade_exchange_lines_aggregated", height = "500px"),
              htmlOutput("url_trade")
            ),

            # Exports -----------------------------------------------------------------

            column(
              12,
              htmlOutput("exports_subtitle", container = tags$h2),
              br()
            ),

            column(
              6,
              highchartOutput("exports_treemap_detailed_min_year", height = "500px"),
              htmlOutput("url_exports_min_year")
            ),

            column(
              6,
              highchartOutput("exports_treemap_detailed_max_year", height = "500px"),
              htmlOutput("url_exports_max_year")
            ),

            # Imports -----------------------------------------------------------------

            column(
              12,
              htmlOutput("imports_subtitle", container = tags$h2),
              br()
            ),

            column(
              6,
              highchartOutput("imports_treemap_detailed_min_year", height = "500px"),
              htmlOutput("url_imports_min_year")
            ),

            column(
              6,
              highchartOutput("imports_treemap_detailed_max_year", height = "500px"),
              htmlOutput("url_imports_max_year")
            )
          ),
          
          # Share/Download/Cite ----
          
          tabItem(
            tabName = "share",
            column(
              12,
              htmlOutput("share_subtitle", container = tags$h2)
            ),
            
            column(
              12,
              htmlOutput("url")
            )
          ),
          
          tabItem(
            tabName = "download",
            column(
              12,
              htmlOutput("download_subtitle", container = tags$h2)
            ),
            
            column(
              12,
              selectInput(
                "format",
                "Download data as:",
                choices = available_formats,
                selected = NULL,
                selectize = TRUE
              ),
              
              downloadButton("download_aggregated", "Aggregated data"),
              downloadButton("download_detailed", "Detailed data")
            )
          ),
          
          tabItem(
            tabName = "cite",
            column(
              12,
              htmlOutput("cite_subtitle", container = tags$h2)
            ),
            
            column(
              12,
              htmlOutput("cite_chicago_subtitle", container = tags$h3),
              htmlOutput("cite", container = tags$p),
              htmlOutput("cite_bibtex_subtitle", container = tags$h3),
              verbatimTextOutput("cite_bibtex")
            )
          )
        ),
        
        column(
          12,
          hr(),
          htmlOutput("site_footer", container = tags$p)
        ),
        
        # Loading ----------------------------------------------------------------
        
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          div(
            id = "loading",
            img(src = "img/loading_icon.gif", width = "100"),
            p("Loading..."),
            align = "center"
          )
        ),
        
        # Footer ------------------------------------------------------------------
        
        tags$footer(
          tags$link(rel = "shortcut icon", href = "img/favicon.ico"),
          tags$script(src = "js/copy-url.js")
        )
      )
    )
  }
)
