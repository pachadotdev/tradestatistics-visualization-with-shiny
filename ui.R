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

          menuItem("Country profile", tabName = "cp"),
          
          menuItem("Compare countries", tabName = "cc"),
          
          menuItem("Product profile", tabName = "pp"),
          
          menuItem("P.E. Simulation", tabName = "ps", badgeLabel = "new", badgeColor = "green"),
          
          # menuItem("Simulate", tabName = "si", badgeLabel = "SKETCH", badgeColor = "red"),
      
          menuItem("Cite", tabName = "cite")
        )
      ),

      dashboardBody(
        tabItems(
          tabItem(
            tabName = "cp",

            # Country profile ----

            useWaitress(),
            
            column(
              12,
              HTML("<h1>Country Profile</h1>"),
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
                "Years",
                min = available_yrs_min,
                max = available_yrs_max,
                value = c(available_yrs_max - 4, 2019),
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
                "Reporter",
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
                "Partner",
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
                "Convert to dollars of the year",
                choices = c("No conversion", 2000:2019),
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
            
            column(
              12,
              align="center",
              actionButton("cp_go", "Give me the country profile",
                           class = "btn-primary")
            ),
            
            column(
              12,
              htmlOutput("title_cp", container = tags$h1)
            ),
            
            ## Aggregated trade -----
            
            column(
              12,
              htmlOutput("trd_stl_cp", container = tags$h2)
            ),
            
            column(
              3,
              htmlOutput("trd_stl_exp_cp", container = tags$h3),
              htmlOutput("trd_smr_exp_cp", container = tags$p),
              htmlOutput("trd_stl_imp_cp", container = tags$h3),
              htmlOutput("trd_smr_imp_cp", container = tags$p)
            ),
            
            column(
              9,
              highchartOutput("trd_exc_lines_agg_cp", height = "500px")
            ),

            ## Detailed trade ----

            column(
              12,
              htmlOutput("exp_tt_yr_cp", container = tags$h2)
            ),
            
            column(
              6,
              htmlOutput("exp_tt_min_yr_cp", container = tags$h3),
              highchartOutput("exp_tm_dtl_min_yr_cp", height = "500px")
            ),

            column(
              6,
              htmlOutput("exp_tt_max_yr_cp", container = tags$h3),
              highchartOutput("exp_tm_dtl_max_yr_cp", height = "500px")
            ),

            column(
              12,
              htmlOutput("imp_tt_yr_cp", container = tags$h2)
            ),
            
            column(
              6,
              htmlOutput("imp_tt_min_yr_cp", container = tags$h3),
              highchartOutput("imp_tm_dtl_min_yr_cp", height = "500px")
            ),
            
            column(
              6,
              htmlOutput("imp_tt_max_yr_cp", container = tags$h3),
              highchartOutput("imp_tm_dtl_max_yr_cp", height = "500px")
            ),
            
            ## Download ----
            
            column(
              12,
              htmlOutput("dwn_cp_stl", container = tags$h2),
              htmlOutput("dwn_cp_txt", container = tags$p),
              uiOutput("dwn_cp_fmt"),
              uiOutput("dwn_cp_agg"),
              uiOutput("dwn_cp_dtl")
            )
          ),
          
          tabItem(
            tabName = "cc",

            # Compare Countries ----

            useWaitress(),

            column(
              12,
              HTML("<h1>Compare Countries</h1>"),
              htmlOutput("title_cc_legend", container = tags$p)
            ),

            column(
              12,
              h2("Filter")
            ),

            column(
              12,
              sliderInput(
                "cc_y",
                "Years:",
                min = available_yrs_min,
                max = available_yrs_max,
                value = 2019,
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            ),

            column(
              3,
              selectInput(
                "cc_r1",
                "Reporter 1",
                choices = available_reporters_iso[available_reporters_iso != "all"],
                selected = "can",
                selectize = TRUE,
                width = "100%"
              )
            ),

            column(
              3,
              selectInput(
                "cc_r2",
                "Reporter 2",
                choices = available_reporters_iso[available_reporters_iso != "all"],
                selected = "nor",
                selectize = TRUE,
                width = "100%"
              )
            ),

            column(
              3,
              selectInput(
                "cc_p",
                "Partner",
                choices = available_reporters_iso,
                selected = "",
                selectize = TRUE,
                width = "100%"
              )
            ),

            column(
              3,
              selectInput(
                "cc_a",
                "Convert to dollars of the year",
                choices = c("No conversion", 2000:2019),
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

            column(
              12,
              align="center",
              actionButton("cc_go", "Give me the country comparison",
                           class = "btn-primary")
            ),

            column(
              12,
              htmlOutput("title_cc", container = tags$h1)
            ),
            
            ## Aggregated trade ----
            
            column(
              12,
              htmlOutput("trd_stl_cc", container = tags$h2)
            ),
            
            column(
              6,
              highchartOutput("trd_exc_cols_agg_r1_cc", height = "500px"),
              htmlOutput("trd_smr_txt_exp_r1_cc", container = tags$p),
              htmlOutput("trd_smr_txt_imp_r1_cc", container = tags$p)
            ),
            
            column(
              6,
              highchartOutput("trd_exc_cols_agg_r2_cc", height = "500px"),
              htmlOutput("trd_smr_txt_exp_r2_cc", container = tags$p),
              htmlOutput("trd_smr_txt_imp_r2_cc", container = tags$p)
            ),

            ## Detailed trade ----

            column(
              12,
              htmlOutput("trd_stl_exp_cc", container = tags$h2)
            ),
            
            column(
              6,
              htmlOutput("exp_tt_yr_r1_cc", container = tags$h3),
              highchartOutput("exp_tm_dtl_yr_r1_cc", height = "500px")
            ),

            column(
              6,
              htmlOutput("exp_tt_yr_r2_cc", container = tags$h3),
              highchartOutput("exp_tm_dtl_yr_r2_cc", height = "500px")
            ),
            
            column(
              12,
              htmlOutput("trd_stl_imp_cc", container = tags$h2)
            ),
            
            column(
              6,
              htmlOutput("imp_tt_yr_r1_cc", container = tags$h3),
              highchartOutput("imp_tm_dtl_yr_r1_cc", height = "500px")
            ),
            
            column(
              6,
              htmlOutput("imp_tt_yr_r2_cc", container = tags$h3),
              highchartOutput("imp_tm_dtl_yr_r2_cc", height = "500px")
            ),

            ## Download ----

            column(
              12,
              htmlOutput("dwn_cc_stl", container = tags$h2),
              htmlOutput("dwn_cc_text", container = tags$p),
              uiOutput("dwn_cc_fmt"),
              uiOutput("dwn_cc_agg"),
              uiOutput("dwn_cc_dtl")
            )
          ),
          
          tabItem(
            tabName = "pp",
          
            # Product profile ----
            
            useWaitress(),
            
            column(
              12,
              HTML("<h1>Product Profile</h1>"),
              htmlOutput("title_pp_legend", container = tags$p)
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
                min = available_yrs_min,
                max = available_yrs_max,
                value = c(available_yrs_max - 4, 2019),
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            ),
            
            column(
              6,
              selectizeInput(
                "pp_s",
                label = NULL,
                choices = NULL,
                width = "100%"
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
            
            column(
              6,
              selectInput(
                "pp_a",
                "Convert to dollars of the year",
                choices = c("No conversion", 2000:2019),
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
            
            column(
              12,
              align="center",
              actionButton("pp_go", "Give me the product profile",
                           class = "btn-primary")
            ),
            
            column(
              12,
              htmlOutput("title_pp", container = tags$h1),
              htmlOutput("trd_stl_pp", container = tags$h2)
            ),
            
            column(
              3,
              htmlOutput("trd_stl_exp_pp", container = tags$h3),
              htmlOutput("trd_smr_exp_pp", container = tags$p),
              htmlOutput("trd_stl_imp_pp", container = tags$h3),
              htmlOutput("trd_smr_imp_pp", container = tags$p)
            ),
            
            column(
              9,
              highchartOutput("trd_exc_columns_pp", height = "500px")
            ),
            
            column(
              12,
              htmlOutput("exp_tt_yr_pp", container = tags$h2)
            ),
            column(
              6,
              htmlOutput("exp_tt_min_yr_pp", container = tags$h3),
              highchartOutput("exp_tm_ori_min_yr_pp", height = "500px")
            ),
            column(
              6,
              htmlOutput("exp_tt_max_yr_pp", container = tags$h3),
              highchartOutput("exp_tm_ori_max_yr_pp", height = "500px")
            ),
            
            column(
              12,
              htmlOutput("imp_tt_yr_pp", container = tags$h2)
            ),
            column(
              6,
              htmlOutput("imp_tt_min_yr_pp", container = tags$h3),
              highchartOutput("imp_tm_ori_min_yr_pp", height = "500px")
            ),
            column(
              6,
              htmlOutput("imp_tt_max_yr_pp", container = tags$h3),
              highchartOutput("imp_tm_ori_max_yr_pp", height = "500px")
            ),
            
            ## Download ----
            
            column(
              12,
              htmlOutput("dwn_pp_stl", container = tags$h2),
              htmlOutput("dwn_pp_txt", container = tags$p),
              uiOutput("dwn_pp_fmt"),
              uiOutput("dwn_pp_agg"),
              uiOutput("dwn_pp_dtl")
            )
          ),
          
          tabItem(
            tabName = "ps",
            
            # Model ----
            
            useWaitress(),
            
            column(
              12,
              HTML("<h1>Partial Equilibrium Simulation</h1>"),
              htmlOutput("title_ps_legend", container = tags$p)
            ),
            
            ## Model variables ----
            
            column(
              12,
              hr(),
              h2("Filter")
            ),
            
            column(
              6,
              sliderInput(
                "ps_y",
                "Years",
                min = available_yrs_min,
                max = available_yrs_max,
                value = c(2002, 2014),
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            ),
            
            column(
              6,
              sliderInput(
                "ps_y_sep",
                "Interval of years",
                # min = available_yrs_min,
                min = 1,
                max = 5,
                value = 4,
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              ) %>% 
                helper(
                  type = "inline",
                  title = "Interval of years",
                  content = c("Yotov et al. (2016) suggest to use intervals of four years in gravity estimation.",
                              "",
                              "<b>References</b>",
                              "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              3,
              selectInput(
                "ps_r",
                "Importer",
                choices = available_reporters_iso,
                # choices = available_reporters_iso[available_reporters_iso != "all"],
                selected = "all",
                selectize = TRUE,
                width = "100%",
                multiple = TRUE
              ) %>% 
                helper(
                  type = "inline",
                  title = "Select importers",
                  content = "You can select more than one importer. For example, to estimate effects for NAFTA, choose the US, Canada and Mexico.",
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              3,
              selectInput(
                "ps_p",
                "Exporter",
                choices = available_reporters_iso,
                selected = "all",
                selectize = TRUE,
                width = "100%",
                multiple = TRUE
              ) %>% 
                helper(
                  type = "inline",
                  title = "Select exporters",
                  content = "You can select more than one exporter.",
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              3,
              selectInput(
                "ps_t",
                "Model type",
                choices = available_models,
                selected = "ppml",
                selectize = TRUE,
                width = "100%"
              ) %>% 
                helper(
                  type = "inline",
                  title = "Available models",
                  content = c("Yotov et al. (2016) propose that PPML is an estimation methods that is consistent with the economic theory.",
                              "",
                              "<b>References</b>",
                              "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              3,
              selectInput(
                "ps_zero",
                "Drop zero flows",
                choices = list("Yes" = "yes", "No" = "no"),
                selected = c("no"),
                selectize = TRUE,
                width = "100%"
              ) %>% 
                helper(
                  type = "inline",
                  title = "Drop zero flows",
                  content = c("You should set this to 'yes' for OLS. For PPML, being it a zero inflated model, can be set to 'no'."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              3,
              selectInput(
                "ps_a",
                "Convert to dollars of the year",
                choices = c("No conversion", 2000:2019),
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
            
            column(
              3,
              selectInput(
                "ps_cl",
                "Use country pairs for clustering",
                choices = list("Yes" = "yes", "No" = "no"),
                selected = c("no"),
                selectize = TRUE,
                width = "100%"
              ) %>% 
                helper(
                  type = "inline",
                  title = "Country pairs for clustering",
                  content = c("Yotov et al. (2016) propose that in the event of violation of assumptions, we should cluster the standard errors to account for an accurate margin of error.",
                              "",
                              "<b>References</b>",
                              "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              3,
              selectInput(
                "ps_pf",
                "Section/Commodity",
                choice = list(
                  "All Products" = available_all,
                  "Custom Selections" = available_vaccine,
                  "HS Sections" = available_sections_code
                ),
                selected = "all",
                selectize = TRUE,
                width = "100%",
                multiple = TRUE
              ) %>% 
                helper(
                  type = "inline",
                  title = "Section/Commodity",
                  content = c("Subset the data for a custom category (i.e. vaccine inputs is our own subset),
                              or for any official section or commodity in the Harmonised System.",
                              "",
                              "<b>References</b>",
                              "Hossain, K. and Nyirongo, V.<i> HS 2002 Classification by Section </i>. UN Stats Wiki, 2021."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              3,
              fileInput(
                'ps_own', 
                'Upload your own data',
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
              ) %>% 
                helper(
                  type = "inline",
                  title = "Upload your own data",
                  content = c("Select any CSV/TSV/XLSX (Excel) or SAV/DTA (SPSS/Stata) file.",
                              "In order to join you own data to the data in our API, you need at least to provide year, reporter
                              and partner. See this
                              <a href='https://raw.githubusercontent.com/pachadotdev/tradestatistics-visualization-with-shiny/master/custom_variables_for_modelling_demo.csv'>example</a> 
                              that uses ISO-3 codes for reporters and partners.",
                              "Any rows that doesn't match will be dropped and the data for the analysis will be the <b>intersection</b> between yours and ours."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              12,
              textInput(
                "ps_fml",
                "Model formula",
                "trade ~ log(dist) + log(gdp_exporter) + rta + colony + comlang_off + contig",
                width = "100%",
                placeholder = "Any valid R formula"
              ) %>% 
                helper(
                  type = "inline",
                  title = "Model formula",
                  content = c("Write a valid formula in the context of the fixest package. The fixed effects (if any) are added automatically, so don't write those.",
                              "",
                              "<h4>Gravity variables in our database</h4>",
                              "<b>Exports and Imports (LHS)</b>
                              <ul>
                               <li>trade: Bilateral trade (exports, reported at destination) in USD of each year</li>
                              </ul>",
                              "<b>Distance for modelling (RHS)</b>
                              <ul>
                               <li>dist: Simple distance between most populated cities in km</li>
                               <li>distcap: Simple distance between capitals in km</li>
                              </ul>",
                              "<b>Additional variables for modelling</b>
                              <ul>
                                <li>gdp_exporter: GDP (in current USD) for the exporters</li>
                                <li>gdp_importer: GDP (in current USD) for the importers</li>
                                <li>gdp_percap_exporter: GDP per capita (in current USD) for the exporters</li>
                                <li>gdp_percap_importer: GDP per capita (in current USD) for the importers</li>
                                <li>colony: The two countries are/were in a colonial relation</li>
                                <li>comlang_ethno: The two countries have at least 9% of their population speaking the same language</li>
                                <li>comlang_off: The two countries share the same official language</li>
                                <li>contig: The two countries are next to each other</li>
                                <li>rta: The two countries are in a trade agreement</li>
                                <li>smctry: The two countries were or are the same country</li>
                                <li>mfn: Most Favoured Nation tariff (weighted average by exports)</li>
                              </ul>",
                              "<b>References</b>",
                              "Berge, L. and McDermott, G.<i><a href='https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html#14_Other_estimation_functions'>Fast Fixed-Effects Estimation: Short introduction</a></i>. CRAN, 2021."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "l"
                )
            ),
  
            column(
              12,
              hr(),
              h2("RTA change simulation")
            ),          

            column(
              4,
              selectInput(
                "ps_sp",
                "Alter RTAs situation for",
                choices = available_reporters_iso,
                selected = c("can", "usa", "mex"),
                selectize = TRUE,
                width = "100%",
                multiple = TRUE
              ) %>% 
                helper(
                  type = "inline",
                  title = "Alter RTAs situation for",
                  content = c("This corresponds to a 'what if' situation, for example, what would have happened (according
                              to the model) if the countries you've chosen dropped or subscribed their RTA starting in
                              a certain year (i.e. what if Chile and Chile would have subscribed their RTA back in 2002 
                              instead of 2006).",
                              "",
                              "<b>References</b>",
                              "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              4,
              selectInput(
                "ps_sra",
                "RTA action",
                choice = list("Drop RTA" = 0L, "Subscribe RTA" = 1L),
                selected = 0L,
                selectize = TRUE,
                width = "100%",
                multiple = FALSE
              ) %>% 
                helper(
                  type = "inline",
                  title = "RTA action",
                  content = c("This corresponds to a 'what if' situation, for example, what would have happened (according
                              to the model) if the countries you've chosen dropped or subscribed their RTA starting in
                              a certain year (i.e. what if Chile and Chile would have subscribed their RTA back in 2002 
                              instead of 2006).",
                              "",
                              "<b>References</b>",
                              "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            column(
              4,
              sliderInput(
                "ps_sy",
                "Since year",
                min = available_yrs_min,
                max = available_yrs_max,
                value = 2002,
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              ) %>% 
                helper(
                  type = "inline",
                  title = "Since year",
                  content = c("This corresponds to a 'what if' situation, for example, what would have happened (according
                              to the model) if the countries you've chosen dropped or subscribed their RTA starting in
                              a certain year (i.e. what if Chile and Chile would have subscribed their RTA back in 2002 
                              instead of 2006).",
                              "",
                              "<b>References</b>",
                              "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                  buttonLabel = "Got it!",
                  easyClose = FALSE,
                  fade = TRUE,
                  size = "s"
                )
            ),
            
            ## Model results ----
            
            column(
              12,
              align="center",
              hr(),
              actionButton(
                "ps_go", 
                "Give me the results for this model",
                class = "btn-primary"
              )
            ),
            
            column(
              12,
              htmlOutput("df_stl_ps", container = tags$h2),
              tableOutput("df_dtl_pre_ps"),
              htmlOutput("fit_stl1_ps", container = tags$h2),
              tableOutput("tidy_ps"),
              tableOutput("glance_ps"),
              htmlOutput("fit_stl2_ps", container = tags$h2),
              verbatimTextOutput("fit_cat_ps"),
              htmlOutput("pred_stl_ps", container = tags$h2),
              # highchartOutput("pred_trade_lines_ps")
              plotOutput("pred_trade_lines_ps")
            ),
            
            ## Download ----
            
            column(
              12,
              htmlOutput("dwn_ps_stl", container = tags$h2),
              htmlOutput("dwn_ps_txt", container = tags$p),
              uiOutput("dwn_ps_fmt"),
              uiOutput("dwn_ps_dtl"),
              uiOutput("dwn_ps_fit")
            )
          ),
          
          # tabItem(
          #   tabName = "si",
          #   
          #   # Simulate ----
          #   
          #   useWaitress(),
          #   
          #   column(
          #     12,
          #     HTML("<h1>SKETCH USING AGTPA BOOK'S DATA - Effects of RTAs</h1>"),
          #     htmlOutput("title_si_legend", container = tags$p)
          #   ),
          #   
          #   ## Simulation variables ----
          #   
          #   column(
          #     12,
          #     hr(),
          #     h2("Filter")
          #   ),
          #   
          #   column(
          #     4,
          #     sliderInput(
          #       "si_y",
          #       "Years",
          #       min = available_yrs_min,
          #       max = available_yrs_max,
          #       value = c(2002, 2018),
          #       # min = 1986,
          #       # max = 2006,
          #       # value = c(1986,2006),
          #       sep = "",
          #       step = 1,
          #       ticks = FALSE,
          #       width = "100%"
          #     )
          #   ),
          #   
          #   column(
          #     4,
          #     sliderInput(
          #       "si_y_sep",
          #       "Interval of years",
          #       # min = available_yrs_min,
          #       min = 1,
          #       max = 5,
          #       value = 4,
          #       sep = "",
          #       step = 1,
          #       ticks = FALSE,
          #       width = "100%"
          #     ) %>% 
          #       helper(
          #         type = "inline",
          #         title = "Interval of years",
          #         content = c("Yotov et al. (2016) suggest to use intervals of four years in gravity estimation.",
          #                     "",
          #                     "<b>References</b>",
          #                     "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
          #         buttonLabel = "Got it!",
          #         easyClose = FALSE,
          #         fade = TRUE,
          #         size = "s"
          #       )
          #   ),
          #   
          #   column(
          #     4,
          #     sliderInput(
          #       "si_y2",
          #       "RTA withdrawal",
          #       min = available_yrs_min,
          #       max = available_yrs_max,
          #       value = 2006,
          #       # min = 1986,
          #       # max = 2006,
          #       # value = 1994,
          #       sep = "",
          #       step = 1,
          #       ticks = FALSE,
          #       width = "100%"
          #     ) %>% 
          #       helper(
          #         type = "inline",
          #         title = "RTA enaction",
          #         content = c("Re-defines the RTA dummy variable, as if the RTA were not in place.",
          #                     "",
          #                     "<b>References</b>",
          #                     "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
          #         buttonLabel = "Got it!",
          #         easyClose = FALSE,
          #         fade = TRUE,
          #         size = "s"
          #       )
          #   ),
          #   
          #   column(
          #     3,
          #     selectInput(
          #       "si_c",
          #       "Countries",
          #       choices = available_reporters_iso[available_reporters_iso != "all"],
          #       selected = c("can","usa","mex"),
          #       selectize = TRUE,
          #       width = "100%",
          #       multiple = TRUE
          #     ) %>% 
          #       helper(
          #         type = "inline",
          #         title = "Select countries",
          #         content = "You can select more than one country. For example, to estimate effects for NAFTA, choose the US, Canada and Mexico.",
          #         buttonLabel = "Got it!",
          #         easyClose = FALSE,
          #         fade = TRUE,
          #         size = "s"
          #       )
          #   ),
          #   
          #   column(
          #     3,
          #     selectInput(
          #       "si_r",
          #       "Reference country",
          #       choices = available_reporters_iso,
          #       selected = "deu",
          #       selectize = TRUE,
          #       width = "100%",
          #       multiple = TRUE
          #     ) %>% 
          #       helper(
          #         type = "inline",
          #         title = "Select reference country",
          #         content = c("Yotov et al. (2016) use Germany as the country of reference for the fixed effects estimation.",
          #                     "",
          #                     "<b>References</b>",
          #                     "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
          #         buttonLabel = "Got it!",
          #         easyClose = FALSE,
          #         fade = TRUE,
          #         size = "s"
          #       )
          #   ),
          #   
          #   column(
          #     3,
          #     selectInput(
          #       "si_a",
          #       "Convert to dollars of the year",
          #       choices = c("No conversion", 2000:2019),
          #       selected = "",
          #       selectize = TRUE,
          #       width = "100%"
          #     ) %>% 
          #       helper(
          #         type = "inline",
          #         title = "Convert to dollars of the year",
          #         content = c("Uses present value and/or future value equations to adjust money value 
          #                     by yearly changes in GDP deflator. The source for the GDP deflator data is The World Bank."),
          #         buttonLabel = "Got it!",
          #         easyClose = FALSE,
          #         fade = TRUE,
          #         size = "s"
          #       )
          #   ),
          #   
          #   column(
          #     3,
          #     sliderInput(
          #       "si_s",
          #       "Elasticity of substitution",
          #       min = 1,
          #       max = 20,
          #       value = 7,
          #       sep = "",
          #       step = 1,
          #       ticks = FALSE,
          #       width = "100%"
          #     ) %>% 
          #       helper(
          #         type = "inline",
          #         title = "Elasticity of substitution",
          #         content = c("Yotov et al. (2016) suggest to use a value of seven.",
          #                     "",
          #                     "<b>References</b>",
          #                     "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
          #         buttonLabel = "Got it!",
          #         easyClose = FALSE,
          #         fade = TRUE,
          #         size = "s"
          #       )
          #   ),
          #   
          #   ## Simulation results ----
          #   
          #   column(
          #     12,
          #     align="center",
          #     actionButton(
          #       "si_go", 
          #       "Give me the results for this simulation",
          #       class = "btn-primary"
          #     )
          #   ),
          #   
          #   column(
          #     12,
          #     htmlOutput("df_stl_si", container = tags$h2),
          #     dataTableOutput("df_fit_si")
          #   )
          # ),
          
          # Cite ----
          
          tabItem(
            tabName = "cite",
            column(
              12,
              htmlOutput("cite_stl", container = tags$h2),
              htmlOutput("cite_chicago_stl", container = tags$h3),
              htmlOutput("cite", container = tags$p),
              htmlOutput("cite_bibtex_stl", container = tags$h3),
              verbatimTextOutput("cite_bibtex")
            )
          ),
          
          uiOutput(outputId = "dynamicUI")
        ),
        
        column(
          12,
          hr(),
          htmlOutput("site_footer", container = tags$p)
        ),
        
        # Footer ----
        
        tags$footer(
          tags$link(rel = "shortcut icon", href = "img/favicon.ico")
        )
      )
    )
  }
)
