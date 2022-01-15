## ui.R ##

shinyUI(
  function(request) {
    dashboardPage(
      skin = styles$skin_color,
      theme = styles$css_files,

      dashboardHeader(
        title = "OTS beta dashboard"
      ),

      dashboardSidebar(
        sidebarMenu(
          # Tabs ----

          # hr(),
          # HTML("<center><h4>Explore</h4></center>"),
          
          menuItem("Visualize", tabName = "visualize"),
          
          menuItem("Model", tabName = "model", badgeLabel = "new", badgeColor = "green"),
      
          menuItem("Cite", tabName = "cite")
        )
      ),

      dashboardBody(
        tabItems(
          tabItem(
            tabName = "visualize",

            # Visualize ----

            column(
              12,
              htmlOutput("title_visualize", container = tags$p),
              htmlOutput("title_visualize_legend", container = tags$p)
            ),
            
            column(
              12,
              hr(),
              h2("Filter")
            ),
            
            column(
              4,
              sliderInput(
                "vis_y",
                "Years:",
                # min = available_years_min,
                min = 2002,
                max = available_years_max,
                value = c(available_years_max - 4, 2019),
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              ),
              sliderInput(
                "vis_y_sep",
                "Separate years by:",
                # min = available_years_min,
                min = 1,
                max = 5,
                value = 1,
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            ),
            
            column(
              4,
              selectInput(
                "vis_r",
                "Reporter:",
                choices = available_reporters_iso[available_reporters_iso != "all"],
                selected = "can",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              4,
              selectInput(
                "vis_p",
                "Partner:",
                choices = available_reporters_iso,
                selected = "",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              4,
              radioButtons(
                "vis_i",
                "Use imputed data:",
                choiceNames = list("Yes, use gravity-imputed UN COMTRADE data",
                                   "No, use raw UN COMTRADE data"),
                choiceValues = list("yes", "no"),
                selected = c("no"),
                width = "100%",
              )
            ),
            
            column(
              12,
              htmlOutput("trade_subtitle", container = tags$h2)
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
              highchartOutput("trade_exchange_lines_aggregated", height = "500px")
            ),

            ## Exports ----

            column(
              12,
              htmlOutput("exports_subtitle", container = tags$h2),
              htmlOutput("exports_note", container = tags$p),
              br()
            ),

            column(
              6,
              highchartOutput("exports_treemap_detailed_min_year", height = "500px")
            ),

            column(
              6,
              highchartOutput("exports_treemap_detailed_max_year", height = "500px")
            ),

            ## Imports ----

            column(
              12,
              htmlOutput("imports_subtitle", container = tags$h2),
              htmlOutput("imports_note", container = tags$p),
              br()
            ),

            column(
              6,
              highchartOutput("imports_treemap_detailed_min_year", height = "500px")
            ),

            column(
              6,
              highchartOutput("imports_treemap_detailed_max_year", height = "500px")
            ),
            
            ## Download ----
            
            column(
              12,
              htmlOutput("download_visualize_subtitle", container = tags$h2),
              htmlOutput("download_visualize_text", container = tags$p),
              selectInput(
                "vis_f",
                "Download data as:",
                choices = available_formats,
                selected = NULL,
                selectize = TRUE
              ),
              
              downloadButton("download_visualize_aggregated", "Aggregated data"),
              downloadButton("download_visualize_detailed", "Detailed data")
            )
          ),
          
          # Model ----
          
          tabItem(
            tabName = "model",
            
            column(
              12,
              htmlOutput("title_model", container = tags$p)
            ),
            
            ## Model variables ----
            
            column(
              12,
              hr(),
              h2("Filter")
            ),
            
            column(
              4,
              sliderInput(
                "mod_y",
                "Years:",
                # min = available_years_min,
                min = 2002,
                max = available_years_max,
                value = c(2002, 2014),
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              ),
              sliderInput(
                "mod_y_sep",
                "Separate years by:",
                # min = available_years_min,
                min = 1,
                max = 5,
                value = 4,
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            ),
            
            column(
              4,
              selectInput(
                "mod_r",
                "Reporter:",
                # choices = available_reporters_iso,
                choices = available_reporters_iso[available_reporters_iso != "all"],
                selected = "can",
                selectize = TRUE,
                width = "100%",
                multiple = TRUE
              )
            ),
            
            column(
              4,
              selectInput(
                "mod_p",
                "Partner:",
                choices = available_reporters_iso,
                selected = "all",
                selectize = TRUE,
                width = "100%",
                multiple = TRUE
              )
            ),
            
            column(
              4,
              radioButtons(
                "mod_d",
                "Distance for modelling:",
                choiceNames = list("Simple distance between most populated cities in km (dist)",
                                   "Simple distance between capitals in km (distcap)"),
                choiceValues = list("dist", "distcap"),
                selected = c("dist"),
                width = "100%",
              ),
              radioButtons(
                "mod_cl",
                "Use country pairs for clustering:",
                choiceNames = list("Yes",
                                   "No"),
                choiceValues = list("yes", "no"),
                selected = c("no"),
                width = "100%",
              ),
              radioButtons(
                "mod_i",
                "Use imputed data:",
                choiceNames = list("Yes, use gravity-imputed UN COMTRADE data",
                                   "No, use raw UN COMTRADE data"),
                choiceValues = list("yes", "no"),
                selected = c("no"),
                width = "100%",
              )
            ),
            
            column(
              4,
              checkboxGroupInput(
                "mod_ct",
                "Continuous variables for modelling:",
                choiceNames = list("MFN: Most Favoured Nation tariff"),
                choiceValues = list("mfn"),
                selected = NULL,
                width = "100%",
              ),
              checkboxGroupInput(
                "mod_b",
                "Binary variables for modelling:",
                choiceNames = list("Colony: The two countries are/were in a colonial relation",
                                   "Comlang Ethno: The two countries have at least 9% of their population speaking the same language",
                                   "Comlang Off: The two countries share the same official language",
                                   "Contig: The two countries are next to each other",
                                   "RTA: The two countries are in a trade agreement",
                                   "Smctry: The two countries were or are the same country"),
                choiceValues = list("colony", "comlang_ethno", "comlang_off", "contig", "rta", "smctry"),
                selected = c("contig", "comlang_off", "colony"),
                width = "100%",
              )
            ),
            
            column(
              4,
              selectInput(
                "mod_t",
                "Model type:",
                choices = available_models,
                selected = "ppml",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              8,
              selectInput(
                "mod_pf",
                "Product filter (i.e. HS section subset):",
                choice = available_groups,
                selected = "All Products",
                selectize = TRUE,
                width = "100%",
                multiple = TRUE
              )
            ),
            
            column(
              12,
              selectInput(
                "mod_cpf",
                "Custom product filter (optional, overwrites section filter):",
                choices = available_commodities,
                selected = NULL,
                selectize = TRUE,
                width = "100%",
                multiple = TRUE
              )
            ),
            
            column(4,
                   fileInput('mod_own', 'Upload your own data:',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               '.csv',
                               '.tsv',
                               '.xlsx',
                               '.sav',
                               '.dta'
                             ),
                             width = "100%"
                   )
            ),
            
            column(
              8,
              textInput(
                "mod_s",
                "Subset your data (optional, use ';' to separate):",
                width = "100%"
              )
            ),
            
            # column(6,
            #        checkboxInput('fix_custom_header', 'Fix column names (don\'t change this if you are unsure)', TRUE)
            # ),
            
            column(12,
                   p("The max size 100MB and this works with csv, tsv, xlsx, sav or dta only."),
                   HTML("<p>You can download an example of custom variables that work with UN COMTRADE data from <a href='https://github.com/pachadotdev/tradestatistics-visualization-with-shiny/blob/master/custom_variables_for_modelling_demo.csv?raw=true'>here</a>. See the <i>cepiigeodist</i> package documentation for the details.</p>")
            ),
            
            column(
              12,
              h2("Model formula"),
              uiOutput("model_formula_latex")
            ),
            
            column(
              12,
              align="center",
              actionButton("go", "Give me the results for this model",
                           class = "btn-default")
            ),
            
            ## Model results ----
            
            column(
              12,
              h2("Data preview"),
              tableOutput("data_detailed_model_preview"),
              h2("Model summary"),
              p("WIP: some selections create errors messages such as 'contrasts can be applied only to factors with 2 or more levels' but Shiny hides those."),
              verbatimTextOutput("model_summary")
            ),
            
            ## Download ----
            
            column(
              12,
              htmlOutput("download_model_subtitle", container = tags$h2),
              htmlOutput("download_model_text", container = tags$p),
              selectInput(
                "mod_f",
                "Download data as:",
                choices = available_formats,
                selected = NULL,
                selectize = TRUE
              ),
              
              downloadButton("download_model_detailed", "Detailed data")
            )
          ),
          
          # Cite ----
          
          tabItem(
            tabName = "cite",
            column(
              12,
              htmlOutput("cite_subtitle", container = tags$h2),
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
        
        # Loading ----
        
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          div(
            id = "loading",
            img(src = "img/loading_icon.gif", width = "100"),
            p("Loading..."),
            align = "center"
          )
        ),
        
        # Footer ----
        
        tags$footer(
          tags$link(rel = "shortcut icon", href = "img/favicon.ico")
        )
      )
    )
  }
)
