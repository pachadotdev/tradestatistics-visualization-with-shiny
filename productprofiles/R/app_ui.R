#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import otsshinycommon
#' @importFrom highcharter highchartOutput
#' @importFrom shinyhelper helper
#' @importFrom waiter useWaitress
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      title = "Product Profiles",
      fluidRow(
        useWaitress(),

        col_12(
          HTML("<h1>Product Profile</h1>"),
          htmlOutput("title_legend", container = tags$p)
        ),

        col_12(
          h2("Filter")
        ),

        col_12(
          sliderInput(
            "y",
            "Years",
            min = available_yrs_min(),
            max = available_yrs_max(),
            value = c(2002, 2014),
            sep = "",
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        ),

        col_6(
          selectInput(
            "s",
            "Section/Commodity",
            choices = list(
              "Custom Selections" = available_vaccine(),
              "HS Sections" = available_sections_code(),
              "HS Commodities" = available_commodities_code()
            ),
            selected = "vaccine",
            selectize = TRUE,
            width = "100%",
            multiple = FALSE
          ) %>%
            helper(
              type = "inline",
              title = "Section/Commodity",
              content = c("Subset the data for a custom category (i.e. vaccine inputs is our own subset),
                              or for any official section or commodity in the Harmonised System.",
                          "",
                          "<b>References</b>",
                          "Hossain, K. and Nyirongo, V.<i><a href='https://unstats.un.org/wiki/display/comtrade/HS+2002+Classification+by+Section'>HS 2002 Classification by Section</a></i>. UN Stats Wiki, 2021."),
              buttonLabel = "Got it!",
              easyClose = FALSE,
              fade = TRUE,
              size = "s"
            )
        ),

        col_6(
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
          actionButton("go", "Give me the product profile",
                       class = "btn-primary")
        ),

        col_12(
          htmlOutput("title", container = tags$h1)
        ),

        ## Aggregated trade -----

        col_12(
          htmlOutput("trd_stl", container = tags$h3),
        ),

        col_3(
          htmlOutput("trd_stl_exp", container = tags$h3),
          htmlOutput("trd_smr_exp", container = tags$p),
          htmlOutput("trd_stl_imp", container = tags$h3),
          htmlOutput("trd_smr_imp", container = tags$p)
        ),

        col_9(
          highchartOutput("trd_exc_columns_agg", height = "500px")
        ),

        ## Detailed trade ----

        col_12(
          htmlOutput("exp_tt_yr", container = tags$h2)
        ),

        col_6(
          htmlOutput("exp_tt_min_yr", container = tags$h3),
          highchartOutput("exp_tm_dtl_min_yr", height = "500px")
        ),

        col_6(
          htmlOutput("exp_tt_max_yr", container = tags$h3),
          highchartOutput("exp_tm_dtl_max_yr", height = "500px")
        ),

        col_12(
          htmlOutput("imp_tt_yr", container = tags$h2)
        ),

        col_6(
          htmlOutput("imp_tt_min_yr", container = tags$h3),
          highchartOutput("imp_tm_dtl_min_yr", height = "500px")
        ),

        col_6(
          htmlOutput("imp_tt_max_yr", container = tags$h3),
          highchartOutput("imp_tm_dtl_max_yr", height = "500px")
        ),

        ## Download ----

        col_12(
          htmlOutput("dwn_stl", container = tags$h2),
          htmlOutput("dwn_txt", container = tags$p),
          uiOutput("dwn_fmt"),
          uiOutput("dwn_dtl")
        ),

        # Cite ----

        col_12(
          uiOutput("citation_stl"),
          uiOutput("citation_text"),
          uiOutput("citation_bibtex")
        ),

        # Footer ----

        col_12(
          hr(),
          htmlOutput("site_footer", container = tags$p)
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
