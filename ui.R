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

          menuItem("Country profile", tabName = "country_profile"),
          
          menuItem("Product profile", tabName = "product_profile", badgeLabel = "new", badgeColor = "green"),
          
          menuItem("Model", tabName = "model", badgeLabel = "new", badgeColor = "green"),
      
          menuItem("Cite", tabName = "cite")
        )
      ),

      dashboardBody(
        tabItems(
          tabItem(
            tabName = "country_profile",

            # Country profile ----

            column(
              12,
              htmlOutput("title_cp", container = tags$h1),
              htmlOutput("title_cp_legend", container = tags$p)
            ),
            
            column(
              12,
              h2("Filter")
            ),
            
            column(
              12,
              sliderInput(
                "cp_y",
                "Years:",
                min = available_years_min,
                max = available_years_max,
                value = c(available_years_max - 4, 2019),
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            ),
            
            column(
              4,
              selectInput(
                "cp_r",
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
                "cp_p",
                "Partner:",
                choices = available_reporters_iso,
                selected = "",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              4,
              selectInput(
                "cp_a",
                "Convert to constant dollars of the year:",
                choices = c("No conversion", 2000:2019),
                selected = "",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              12,
              align="center",
              actionButton("cp_go", "Give me the country profile",
                           class = "btn-primary")
            ),
            
            ## Aggregated trade -----
            
            column(
              12,
              htmlOutput("trade_subtitle_cp", container = tags$h2)
            ),
            
            column(
              3,
              htmlOutput("trade_subtitle_exp_cp", container = tags$h3),
              htmlOutput("trade_summary_exp_cp", container = tags$p),
              htmlOutput("trade_subtitle_imp_cp", container = tags$h3),
              htmlOutput("trade_summary_imp_cp", container = tags$p)
            ),
            
            column(
              9,
              highchartOutput("trade_exchange_lines_aggregated_cp", height = "500px")
            ),

            ## Detailed trade ----

            column(
              12,
              htmlOutput("exports_subtitle_cp", container = tags$h2),
              htmlOutput("exports_note_cp", container = tags$p)
            ),

            column(
              6,
              htmlOutput("exports_title_year_cp", container = tags$h3)
            ),
            
            column(
              6,
              htmlOutput("imports_title_year_cp", container = tags$h3)
            ),
            
            column(
              3,
              htmlOutput("exports_title_min_year_cp", container = tags$h4),
              highchartOutput("exports_treemap_detailed_min_year_cp", height = "500px")
            ),

            column(
              3,
              htmlOutput("exports_title_max_year_cp", container = tags$h4),
              highchartOutput("exports_treemap_detailed_max_year_cp", height = "500px")
            ),

            column(
              3,
              htmlOutput("imports_title_min_year_cp", container = tags$h4),
              highchartOutput("imports_treemap_detailed_min_year_cp", height = "500px")
            ),
            
            column(
              3,
              htmlOutput("imports_title_max_year_cp", container = tags$h4),
              highchartOutput("imports_treemap_detailed_max_year_cp", height = "500px")
            ),
            
            ## Origin/Destination ----
            
            column(
              12,
              htmlOutput("trade_partners_title_cp", container = tags$h2),
              htmlOutput("trade_partners_text_cp", container = tags$p)
            ),
            
            column(
              6,
              htmlOutput("trade_exports_year_subtitle_cp", container = tags$h3)
            ),
            
            column(
              6,
              htmlOutput("trade_imports_year_subtitle_cp", container = tags$h3)
            ),
            
            column(
              3,
              htmlOutput("trade_exports_min_year_subtitle_cp", container = tags$h4),
              highchartOutput("exports_treemap_destinations_min_year_cp", height = "500px")
            ),
            
            column(
              3,
              htmlOutput("trade_exports_max_year_subtitle_cp", container = tags$h4),
              highchartOutput("exports_treemap_destinations_max_year_cp", height = "500px")
            ),
            
            column(
              3,
              htmlOutput("trade_imports_min_year_subtitle_cp", container = tags$h4),
              highchartOutput("imports_treemap_origins_min_year_cp", height = "500px")
            ),
            
            column(
              3,
              htmlOutput("trade_imports_max_year_subtitle_cp", container = tags$h4),
              highchartOutput("imports_treemap_origins_max_year_cp", height = "500px")
            ),
            
            ## Download ----
            
            column(
              12,
              htmlOutput("download_cp_subtitle", container = tags$h2),
              htmlOutput("download_cp_text", container = tags$p),
              uiOutput("download_cp_format"),
              uiOutput("download_cp_aggregated"),
              uiOutput("download_cp_detailed")
            )
          ),
          
          tabItem(
            tabName = "product_profile",
          
            # Product profile ----
            
            column(
              12,
              htmlOutput("title_pp", container = tags$h1)
            ),
            
            column(
              12,
              h2("Filter")
            ),
            
            column(
              12,
              sliderInput(
                "pp_y",
                "Years:",
                min = available_years_min,
                max = available_years_max,
                value = c(available_years_max - 4, 2019),
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            ),
            
            column(
              6,
              selectInput(
                "pp_s",
                "Section:",
                choices = list(
                  "All Products" = available_all,
                  "Custom Selections" = available_vaccine,
                  "HS Sections" = available_sections_code
                ),
                selected = "all",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              6,
              selectInput(
                "pp_a",
                "Convert to constant dollars of the year:",
                choices = c("No conversion", 2000:2019),
                selected = "",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              12,
              align="center",
              actionButton("pp_go", "Give me the product profile",
                           class = "btn-primary")
            ),
            
            column(
              12,
              htmlOutput("exports_title_year_pp", container = tags$h2),
              htmlOutput("trade_summary_exp_pp", container = tags$p)
            ),
            column(
              6,
              htmlOutput("exports_title_min_year_pp", container = tags$h3),
              highchartOutput("exports_treemap_origins_min_year_pp", height = "500px")
            ),
            column(
              6,
              htmlOutput("exports_title_max_year_pp", container = tags$h3),
              highchartOutput("exports_treemap_origins_max_year_pp", height = "500px")
            ),
            
            column(
              12,
              htmlOutput("imports_title_year_pp", container = tags$h2),
              htmlOutput("trade_summary_imp_pp", container = tags$p)
            ),
            column(
              6,
              htmlOutput("imports_title_min_year_pp", container = tags$h3),
              highchartOutput("imports_treemap_origins_min_year_pp", height = "500px")
            ),
            column(
              6,
              htmlOutput("imports_title_max_year_pp", container = tags$h3),
              highchartOutput("imports_treemap_origins_max_year_pp", height = "500px")
            ),
            
            ## Download ----
            
            column(
              12,
              htmlOutput("download_pp_subtitle", container = tags$h2),
              htmlOutput("download_pp_text", container = tags$p),
              uiOutput("download_pp_format"),
              uiOutput("download_pp_aggregated"),
              uiOutput("download_pp_detailed")
            )
          ),
          
          tabItem(
            tabName = "model",
            
            # Model ----
            
            column(
              12,
              htmlOutput("title_model", container = tags$h1),
              htmlOutput("title_model_legend", container = tags$p)
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
                min = available_years_min,
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
              ),
              selectInput(
                "mod_a",
                "Convert to constant dollars of the year:",
                choices = c("No conversion", 2000:2019),
                selected = "",
                selectize = TRUE,
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
                "Product filter (i.e., HS section subset):",
                choice = list(
                  "All Products" = available_all,
                  "Custom Selections" = available_vaccine,
                  "HS Sections" = available_sections_code
                  ),
                selected = "All Products",
                selectize = TRUE,
                width = "100%",
                multiple = TRUE
              )
            ),
            
            # column(
            #   12,
            #   selectInput(
            #     "mod_cpf",
            #     "Custom product filter (optional, overwrites section filter):",
            #     choices = NULL,
            #     selected = NULL,
            #     selectize = TRUE,
            #     width = "100%",
            #     multiple = TRUE
            #   )
            # ),
            
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
              actionButton("mod_go", "Give me the results for this model",
                           class = "btn-primary")
            ),
            
            ## Model results ----
            
            column(
              12,
              htmlOutput("model_data_subtitle", container = tags$h2),
              htmlOutput("data_detailed_model_text", container = tags$p),
              tableOutput("data_detailed_model_preview"),
              htmlOutput("model_summary_subtitle", container = tags$h2),
              htmlOutput("model_summary_text", container = tags$p),
              tableOutput("model_summary_tidy"),
              tableOutput("model_summary_glance")
            ),
            
            ## Download ----
            
            column(
              12,
              htmlOutput("download_model_subtitle", container = tags$h2),
              htmlOutput("download_model_text", container = tags$p),
              uiOutput("download_model_format"),
              uiOutput("download_model_detailed"),
              uiOutput("download_model_fit")
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
