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
          
          menuItem("Compare countries", tabName = "cc", badgeLabel = "new", badgeColor = "green"),
          
          menuItem("Product profile", tabName = "pp", badgeLabel = "new", badgeColor = "green"),
          
          # THIS IS NOT READY
          menuItem("Model", tabName = "md", badgeLabel = "new", badgeColor = "green"),
      
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
                "Reporter 1:",
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
                "Reporter 2:",
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
                "Partner:",
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
            tabName = "md",
            
            # Model ----
            
            # useWaitress(),
            
            column(
              12,
              HTML("<h1>Gravity Models</h1>"),
              htmlOutput("title_md_legend", container = tags$p)
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
                "md_y",
                "Years:",
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
                "md_y_sep",
                "Separate years by:",
                # min = available_yrs_min,
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
                "md_r",
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
                "md_p",
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
              selectInput(
                "md_t",
                "Model type:",
                choices = available_models,
                selected = "ppml",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              3,
              selectInput(
                "md_a",
                "Convert to constant dollars of the year:",
                choices = c("No conversion", 2000:2019),
                selected = "",
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              3,
              selectInput(
                "md_cl",
                "Use country pairs for clustering:",
                choices = list("Yes" = "yes", "No" = "no"),
                selected = c("no"),
                selectize = TRUE,
                width = "100%"
              )
            ),
            
            column(
              3,
              selectInput(
                "md_pf",
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
            
            column(
              3,
              fileInput(
                'md_own', 
                'Upload your own data:',
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
            
            ## Model results ----
            
            column(
              12,
              textInput(
                "md_fml",
                "Model formula (i.e., any valid R formula)",
                "trade_value_usd_exp ~ poly(log(dist), 2) + colony + comlang_off + contig",
                width = "100%",
                placeholder = "Any valid R formula"
              )
            ),
            
            column(
              12,
              align="center",
              actionButton(
                "md_go", 
                "Give me the results for this model",
                class = "btn-primary"
              )
            ),
            
            column(
              12,
              htmlOutput("df_stl_md", container = tags$h2),
              tableOutput("df_dtl_pre_md"),
              htmlOutput("fit_stl_md", container = tags$h2),
              tableOutput("tidy_md"),
              tableOutput("glance_md"),
              htmlOutput("fit_res_md", container = tags$h2),
              verbatimTextOutput("fit_cat_md"),
            ),
            
            ## Download ----
            
            column(
              12,
              htmlOutput("dwn_md_stl", container = tags$h2),
              htmlOutput("dwn_md_txt", container = tags$p),
              uiOutput("dwn_md_fmt"),
              uiOutput("dwn_md_dtl"),
              uiOutput("dwn_md_fit")
            )
          ),
          
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
          )
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
