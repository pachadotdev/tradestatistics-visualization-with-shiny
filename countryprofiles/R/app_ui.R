#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import otsshinycommon
#' @importFrom highcharter highchartOutput
#' @importFrom shinyhelper helper
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter useWaitress
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      skin = styles$skin_color,
      theme = styles$css_files,

      dashboardHeader(title = "Open Trade Statistics"),

      dashboardSidebar(
        useShinyjs(),
        useWaitress(),
        sidebarMenu(
          menuItem("Visualize", tabName = "visualize"),
          menuItem("Download", tabName = "download"),
          menuItem("Cite", tabName = "cite")
        )
      ),

      dashboardBody(
        fluidRow(
          col_12(
            HTML("<h1>Country Profiles</h1>"),
            HTML("<p><i>By Open Trade Statistics.</i> The information displayed here is based on
            <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. Please read our
            <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a>
            for a full description of restrictions and applicable licenses. These figures do not
            include services or foreign direct investment.</p>")
          )
        ),

        tabItems(
          tabItem(
            tabName = "visualize",
            fluidRow(
              # Filter -----

              col_12(
                h2("Filter")
              ),

              col_3(
                sliderInput(
                  "y",
                  "Years",
                  min = available_yrs_min(),
                  max = available_yrs_max(),
                  value = c(2016, 2020),
                  sep = "",
                  step = 1,
                  ticks = FALSE,
                  width = "100%"
                )
              ),

              col_3(
                selectInput(
                  "r",
                  "Reporter",
                  choices = available_reporters_iso()[available_reporters_iso() != "all"],
                  selected = "can",
                  selectize = TRUE,
                  width = "100%"
                )
              ),

              col_3(
                selectInput(
                  "p",
                  "Partner",
                  choices = available_reporters_iso(),
                  selected = "all",
                  selectize = TRUE,
                  width = "100%"
                )
              ),

              col_3(
                selectInput(
                  "d",
                  "Convert dollars to a fixed year",
                  choices = c("No", 2002:2019),
                  selected = "",
                  selectize = TRUE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Convert to dollars of the year",
                    content = c("Uses present value and/or future value equations to adjust money value
                              by yearly changes in GDP deflator. The source for the GDP deflator data is The World Bank."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_12(
                align="center",
                actionButton("go", "Give me the country profile",
                             class = "btn-primary")
              ),

              # Trade ----

              col_12(
                htmlOutput("title", container = tags$h2)
              ),

              ## Aggregated trade -----

              div(
                id = "aggregated_trade",

                col_12(
                  htmlOutput("trd_stl", container = tags$h3)
                ),

                col_3(
                  htmlOutput("trd_stl_exp", container = tags$h4),
                  htmlOutput("trd_smr_exp", container = tags$p),
                  htmlOutput("trd_stl_imp", container = tags$h4),
                  htmlOutput("trd_smr_imp", container = tags$p)
                ),

                col_9(
                  highchartOutput("trd_exc_lines_agg", height = "500px")
                )
              ),

              ## Detailed trade ----

              div(
                id = "detailed_trade",

                col_12(
                  htmlOutput("exp_tt_yr", container = tags$h3),
                  highchartOutput("exp_col_dtl_yr", height = "500px")
                ),

                col_6(
                  htmlOutput("exp_tt_min_yr", container = tags$h4),
                  highchartOutput("exp_tm_dtl_min_yr", height = "500px")
                ),

                col_6(
                  htmlOutput("exp_tt_max_yr", container = tags$h4),
                  highchartOutput("exp_tm_dtl_max_yr", height = "500px")
                ),

                col_12(
                  htmlOutput("imp_tt_yr", container = tags$h3),
                  highchartOutput("imp_col_dtl_yr", height = "500px")
                ),

                col_6(
                  htmlOutput("imp_tt_min_yr", container = tags$h4),
                  highchartOutput("imp_tm_dtl_min_yr", height = "500px")
                ),

                col_6(
                  htmlOutput("imp_tt_max_yr", container = tags$h4),
                  highchartOutput("imp_tm_dtl_max_yr", height = "500px")
                )
              )
            )
          ),

          ## Download ----

          tabItem(
            tabName = "download",
            fluidRow(
              col_12(
                htmlOutput("dwn_stl", container = tags$h3),
                htmlOutput("dwn_txt", container = tags$p),
                uiOutput("dwn_fmt"),
                uiOutput("dwn_agg"),
                uiOutput("dwn_dtl")
              )
            )
          ),

          # Cite ----

          tabItem(
            tabName = "cite",
            fluidRow(
              col_12(
                h2("Cite"),
                uiOutput("citation_text"),
                uiOutput("citation_bibtex")
              )
            )
          )
        ),

        # Footer ----

        fluidRow(
          col_12(
            hr(),
            htmlOutput("site_footer", container = tags$p)
          )
        )
      ),

      uiOutput(outputId = "dynamicUI")
    ),

    tags$footer(
      tags$link(rel = "shortcut icon", href = "img/favicon.ico")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Country Profiles - Open Trade Statistics"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
