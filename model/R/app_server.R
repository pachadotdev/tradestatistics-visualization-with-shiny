#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2
#' @importFrom broom glance tidy
#' @importFrom dplyr arrange bind_rows case_when collect everything filter
#'     group_by inner_join left_join mutate select summarise tbl ungroup
#' @importFrom fixest feols feglm
#' @importFrom janitor clean_names
#' @importFrom rio import export
#' @importFrom rlang sym
#' @importFrom stats as.formula predict quasipoisson weighted.mean
#' @importFrom shinyhelper observe_helpers
#' @importFrom tidyr pivot_longer
#' @importFrom utils head
#' @importFrom waiter Waitress
#' @noRd
app_server <- function(input, output, session) {
  # Connect to SQL ----

  sql_con <- sql_con()

  # User inputs ----

  observe_helpers()

  inp_y <- reactive({
    y2 <- (min(input$y[1], input$y[2])):(max(input$y[1], input$y[2]))
    y2 <- seq(min(y2), max(y2), by = input$i)
    return(y2)
  })

  inp_r <- reactive({ sort(input$r) }) # reporter
  inp_p <- reactive({ sort(input$p) }) # partner
  inp_t <- reactive({ input$t }) # model type
  inp_z <- reactive({ input$z }) # drop zeros
  inp_d <- reactive({ input$d }) # adjust dollar
  inp_c <- reactive({ input$c }) # cluste
  inp_s <- reactive({ input$s }) # section/commodity

  inp_fml <- reactive({ input$fml })
  inp_fmt <- reactive({ input$fmt })

  inp_rc <- reactive({ input$rc }) # rta action
  inp_rm <- reactive({ input$rm }) # rta modification
  inp_ry <- reactive({ input$ry }) # rta year

  # Model + Simulation ----

  wt <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

  ## 1. upload custom data ----

  custom_data <- eventReactive(input$go, {
    uploaded_file <- input$own

    if(!is.null(uploaded_file)) {
      inp_data <- import(file = uploaded_file$datapath, format = tools::file_ext(uploaded_file$name)) %>%
        clean_names()

      return(inp_data)
    } else {
      data.frame()
    }
  })

  ## 2. define model formula ----

  lhs <- eventReactive(input$go, {
    lhs <- gsub("\\s+", "", gsub("~.*", "", inp_fml()))
    return(lhs)
  })

  rhs <- eventReactive(input$go, {
    rhs <- unlist(strsplit(gsub("\\s+", "", gsub(".*~", "", inp_fml())), "\\+"))
    rhs <- sort(rhs[rhs != "+"])
    return(rhs)
  })

  raw_lhs <- reactive({
    x <- unlist(regmatches(lhs(), gregexpr("(?<=\\().*?(?=\\))", lhs(), perl = T)))
    x <- x[x != ""]
    return(x)
  })

  raw_rhs <- reactive({
    x <- unlist(regmatches(rhs(), gregexpr("(?<=\\().*?(?=\\))", rhs(), perl = T)))
    x <- x[x != ""]
    return(x)
  })

  fml <- eventReactive(input$go, {
    fml <- paste0(lhs(), " ~ ", paste(rhs(), collapse = " + "))
    if (inp_t() == "olsfe") {
      fml <- paste(fml, "| reporter_yr + partner_yr")
    }
    return(fml)
  })

  ## 3. read from SQL ----

  df_dtl <- reactive({
    print("Collecting model data...")
    wt$notify(position = "tr")

    ### 3.1. apply filters ----

    tbl_sql <- if (any(inp_s() != "all")) {
      "yrpc"
    } else {
      "yrp"
    }

    d <- tbl(sql_con, tbl_sql)

    d <- d %>%
      filter(
        !!sym("year") %in% !!inp_y() &
        !!sym("reporter_iso") != !!sym("partner_iso")
      )

    if (any(inp_r() != "all")) {
      d <- d %>%
        filter(
          !!sym("reporter_iso") %in% !!inp_r()
        )
    }

    if (any(inp_p() != "all")) {
      d <- d %>%
        filter(
          !!sym("partner_iso") %in% !!inp_p()
        )
    }

    wt$inc(1)

    if (any(inp_s() %in% "vaccine")) {
      d <- d %>%
        left_join(
          tbl(sql_con, "vaccine_inputs")
        ) %>%
        mutate(
          section_code = case_when(
            !!sym("is_vaccine_input") == 1L ~ "vaccine",
            TRUE ~ !!sym("section_code")
          )
        )
    }

    if (any(inp_s() != "all")) {
      d <- d %>%
        filter(!!sym("section_code") %in% !!inp_s())
    }

    wt$inc(1)

    #### aggregate data ----

    d <- d %>%
      select(!!sym("year"),
             importer = !!sym("reporter_iso"),
             exporter = !!sym("partner_iso"),
             trade = !!sym("trade_value_usd_imp")) %>%
      group_by(!!sym("year"), !!sym("importer"), !!sym("exporter")) %>%
      summarise(trade = sum(!!sym("trade"), na.rm = T)) %>%
      ungroup()

    if (inp_z() == "yes") {
      d <- d %>%
        filter(!!sym("trade") > 0)
    }

    #### collect data ----

    d <- d %>%
      collect() %>%
      arrange(!!sym("year"), !!sym("importer"), !!sym("exporter"))

    wt$inc(1)

    ### 3.2. add geo dist data ----

    d <- d %>%
      mutate(
        country1 = pmin(!!sym("importer"), !!sym("exporter")),
        country2 = pmax(!!sym("importer"), !!sym("exporter"))
      ) %>%
      inner_join(
        tbl(sql_con, "distances") %>% collect(),
        by = c("country1", "country2")
      ) %>%
      select(-!!sym("country1"),-!!sym("country2"))

    wt$inc(1)

    ### 3.3. add RTA data ----

    if (any(rhs() %in% "rta")) {
      d <- d %>%
        mutate(
          country1 = pmin(!!sym("importer"), !!sym("exporter")),
          country2 = pmax(!!sym("importer"), !!sym("exporter"))
        ) %>%
        left_join(
          tbl(sql_con, "rtas") %>%
            filter(!!sym("year") %in% !!inp_y()) %>%
            collect(),
          by = c("year", "country1", "country2")
        ) %>%
        mutate(
          rta = case_when(
            is.na(!!sym("rta")) ~ 0L,
            TRUE ~ !!sym("rta")
          )
        ) %>%
        select(-!!sym("country1"),-!!sym("country2"))
    }

    wt$inc(2)

    ### 3.4. create fixed effects ----

    if (inp_t() == "olsfe") {
      d <- d %>%
        mutate(
          importer_yr = paste0(!!sym("importer"), !!sym("year")),
          exporter_yr = paste0(!!sym("exporter"), !!sym("year"))
        )
    }

    wt$inc(.5)

    ### 3.5. create clustering variable ----

    if (inp_c() == "yes") {
      d <- d %>%
        mutate(imp_exp = paste(!!sym("importer"), !!sym("exporter"), sep = "_"))
    }

    wt$inc(.5)

    ### 3.6. add GDP / GDP percap ----

    if (any(c(raw_rhs(), rhs()) %in%
            c("gdp_importer", "gdp_percap_importer", "gdp_exporter", "gdp_percap_exporter"))) {
      d <- d %>%
        inner_join(
          tbl(sql_con, "gdp") %>%
            filter(!!sym("year") %in% !!inp_y()) %>%
            select(!!sym("country_iso"), !!sym("year"),
                   gdp_importer = !!sym("gdp"),
                   gdp_percap_importer = !!sym("gdp_percap")) %>%
            collect(),
          by = c("importer" = "country_iso", "year")
        )

      d <- d %>%
        inner_join(
          tbl(sql_con, "gdp") %>%
            filter(!!sym("year") %in% !!inp_y()) %>%
            select(!!sym("country_iso"), !!sym("year"),
                   gdp_exporter = !!sym("gdp"),
                   gdp_percap_exporter = !!sym("gdp_percap")) %>%
            collect(),
          by = c("exporter" = "country_iso", "year")
        )
    }

    ### 3.7. convert dollars in time ----

    # CHECK LATER ----

    # if (inp_d() != "No conversion") {
    #   d <- gdp_deflator_adjustment(d, as.integer(inp_d()))
    # }

    ### 3.8. add MFN data ----

    if (any(c(raw_rhs(), rhs()) %in% "mfn")) {
      tar <- tbl(sql_con, "tariffs") %>%
        filter(
          !!sym("year") %in% !!inp_y()
        )

      trd <- tbl(sql_con, "yrpc") %>%
        filter(
          !!sym("year") %in% !!inp_y()
        )

      if (any(inp_p() != "all")) {
        tar <- tar %>%
          filter(
            # here we need the applied tariffs when the product gets to destination
            !!sym("reporter_iso") %in% !!inp_p()
          )

        trd <- tbl(sql_con, "yrpc") %>%
          filter(
            !!sym("partner_iso") %in% !!inp_r()
          )
      }

      if (any(inp_s() %in% "vaccine")) {
        trd <- trd %>%
          left_join(
            tbl(sql_con, "vaccine_inputs")
          ) %>%
          mutate(
            section_code = case_when(
              !!sym("is_vaccine_input") == 1L ~ "vaccine",
              TRUE ~ !!sym("section_code")
            )
          )
      }

      if (any(inp_s() != "all")) {
        trd <- trd %>%
          filter(!!sym("section_code") %in% !!inp_s())
      }

      trd <- trd %>%
        select(!!sym("year"), !!sym("reporter_iso"), !!sym("partner_iso"),
          !!sym("commodity_code"), !!sym("trade_value_usd_imp")) %>%
        inner_join(
          tar %>%
            select(!!sym("year"), !!sym("reporter_iso"),
                   !!sym("commodity_code"), mfn = !!sym("simple_average")),
          by = c("year", "reporter_iso", "commodity_code")
        )

      trd <- trd %>%
        select(!!sym("year"),
               importer = !!sym("reporter_iso"),
               exporter = !!sym("partner_iso"),
               trade = !!sym("trade_value_usd_imp"),
               !!sym("mfn")) %>%
        collect() %>%
        group_by(!!sym("year"), !!sym("importer"), !!sym("exporter")) %>%
        summarise(
          mfn = weighted.mean(!!sym("mfn"), !!sym("trade"), na.rm = T)
        ) %>%
        ungroup()

      rm(tar)

      d <- d %>%
        inner_join(trd)

      rm(trd)
    }

    wt$inc(1)

    gc()

    return(
      # TODO: improve this
      # it's not elegant, but works well with polynomials, logs, etc in formulas
      d[,
        colnames(d) %in%
          c("year", "importer", "exporter",
            lhs(), rhs(), raw_lhs(), raw_rhs(),
            "remoteness_exp", "remoteness_imp", "imp_exp"
          )
      ]
    )
  }) %>%
    bindCache(
      inp_y(), inp_r(), inp_p(), inp_t(), inp_z(),
      inp_d(), inp_c(), inp_s(),
      fml(), lhs(), rhs(), raw_lhs(), raw_rhs()
    ) %>%
    bindEvent(input$go)

  df_dtl_2 <- eventReactive(input$go, {
    ### 3.8. join with custom data ----

    if (nrow(custom_data()) > 0) {
      d <- df_dtl() %>% inner_join(custom_data())
    } else {
      d <- df_dtl()
    }

    d <- d %>% select(!!sym("year"), everything())

    return(d)
  })

  ## 4. Fit model ----

  fit <- eventReactive(input$go, {
    print("Fitting model...")

    fml <- as.formula(fml())

    if (any(inp_t() %in% c("ols", "olsfe"))) {
      if (inp_c() == "yes") {
        m <- tryCatch(
          feols(fml, df_dtl_2(), cluster = ~imp_exp),
          error = function(e) { custom_regression_error() }
        )
      } else {
        m <- tryCatch(
          feols(fml, df_dtl_2()),
          error = function(e) { custom_regression_error() }
        )
      }
    }

    if (inp_t() == "ppml") {
      if (inp_c() == "yes") {
        m <- tryCatch(
          feglm(fml, df_dtl_2(), cluster = ~imp_exp,
                family = quasipoisson(link = "log")),
          error = function(e) { custom_regression_error() }
        )
      } else {
        m <- tryCatch(
          feglm(fml, df_dtl_2(), family = quasipoisson(link = "log")),
          error = function(e) { custom_regression_error() }
        )
      }
    }

    wt$inc(1)
    gc()

    return(m)
  })

  ## 5. Simulate ----

  pred_trade_plot <- eventReactive(input$go, {
    d <- df_dtl_2() %>%
      filter(
        !!sym("exporter") %in% !!inp_rc()
      )

    d <- d %>% mutate(`UNFEASIBLE` = NA_real_, `ESTIMATION` = NA_real_)

    d <- d %>%
      mutate(predicted_trade = predict(fit(), newdata = d)) %>%
      ungroup() %>%
      select(!!sym("year"), !!sym("exporter"), !!sym("trade"),
             !!sym("predicted_trade")) %>%
      group_by(!!sym("year"), !!sym("exporter")) %>%
      summarise(
        trade = sum(!!sym("trade"), na.rm = T),
        predicted_trade = sum(!!sym("predicted_trade"), na.rm = T)
      )

    d <- d %>%
      pivot_longer(!!sym("trade"):!!sym("predicted_trade"),
                   names_to = "variable", values_to = "value")

    d <- d %>%
      mutate(
        variable = case_when(
          !!sym("variable") == "trade" ~ "Observed trade",
          TRUE ~ "Predicted trade"
        )
      )

    d2 <- df_dtl_2() %>%
      filter(
        !!sym("exporter") %in% !!inp_rc()
      ) %>%
      mutate(
        rta = case_when(
          !!sym("year") >= !!inp_ry() ~ as.integer(!!inp_rm()),
          TRUE ~ !!sym("rta")
        )
      )

    d2 <- d2 %>% mutate(`UNFEASIBLE` = NA_real_, `ESTIMATION` = NA_real_)

    d2 <- d2 %>%
      mutate(predicted_trade = predict(fit(), newdata = d2)) %>%
      select(!!sym("year"), !!sym("exporter"), !!sym("predicted_trade")) %>%
      group_by(!!sym("year"), !!sym("exporter")) %>%
      summarise(
        predicted_trade = sum(!!sym("predicted_trade"), na.rm = T)
      )

    d2 <- d2 %>%
      pivot_longer(!!sym("predicted_trade"),
                   names_to = "variable", values_to = "value")

    d2 <- d2 %>%
      mutate(variable = "Predicted trade (altered RTA)")

    d <- d %>% bind_rows(d2); rm(d2)

    d$variable <- factor(d$variable,
      levels = c("Observed trade",
                 "Predicted trade",
                 "Predicted trade (altered RTA)"))

    wt$inc(1)

    g <- ggplot(d) +
      geom_col(aes(x = as.factor(!!sym("year")),
                   y = !!sym("value") / 1000000000,
                   fill = !!sym("variable")),
               position = "dodge2") +
      facet_wrap(~exporter, scales = "free_y") +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(
        x = "Year",
        y = "USD billion",
        title = "Aggregated effect on exports for the selected countries"
      )

    wt$close()
    return(g)
  })

  # Cite ----

  site_url <- "https://shiny.tradestatistics.io"

  cite <- reactive({
    glue(
      "Open Trade Statistics. \"OTS BETA DASHBOARD\". <i>Open Trade Statistics</i>.
        Accessed {months(Sys.Date()) } { lubridate::day(Sys.Date()) }, { lubridate::year(Sys.Date()) }. { site_url }/"
    )
  })

  cite_bibtex <- reactive({
    glue("@misc{{open_trd_statistics_{lubridate::year(Sys.Date())},
      title = {{Open Trade Statistics Beta Dashboard}},
      url = {{{site_url}}},
      author = {{Vargas, Mauricio}},
      doi = {{10.5281/zenodo.3738793}},
      publisher = {{Open Trade Statistics}},
      year = {{2022}},
      month = {{Apr}},
      note = {{Accessed: { months(Sys.Date()) } { lubridate::day(Sys.Date()) }, { lubridate::year(Sys.Date()) }}}}}"
    )
  })

  # Outputs ----

  output$title_legend <- renderText({
    "The information displayed here is based on
    <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. Please read
    our <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a>
    for a full description of restrictions and applicable licenses. These
    figures do not include services or foreign direct investment."
  })

  hdata_stl <- eventReactive(input$go, { "Data preview" })
  fit_stl1 <- eventReactive(input$go, { "Model summary" })
  fit_stl2 <- eventReactive(input$go, { "Model results" })
  pred_stl <- eventReactive(input$go, { "Model simulation" })

  output$hdata_stl <- renderText({ hdata_stl() })
  output$hdata_dtl <- renderTable({ head(df_dtl_2()) })
  output$fit_stl1 <- renderText({ fit_stl1() })
  output$fit_tidy <- renderTable({ tidy(fit()) })
  output$fit_glance <- renderTable({ glance(fit()) })
  output$fit_stl2 <- renderText({ fit_stl2() })
  output$fit_cat <- renderPrint({ fit() })
  output$pred_stl <- renderText({ pred_stl() })
  output$pred_trade_plot <- renderPlot({ pred_trade_plot() })

  ## Download ----

  dwn_stl <- eventReactive(input$go, { "Download model data" })

  dwn_txt <- eventReactive(input$go, {
    "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
  })

  dwn_fmt <- eventReactive(input$go, {
    selectInput(
      "fmt",
      "Download data as:",
      choices = available_formats(),
      selected = NULL,
      selectize = TRUE
    )
  })

  output$dwn_dtl_pre <- downloadHandler(
    filename = function() {
      glue("{ inp_t() }_{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }.{ inp_fmt() }")
    },
    content = function(filename) {
      export(df_dtl(), filename)
    },
    contentType = "application/zip"
  )

  output$dwn_fit_pre <- downloadHandler(
    filename = function() {
      glue("{ inp_t() }_{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }.rds")
    },
    content = function(filename) {
      saveRDS(fit(), filename)
    },
    contentType = "application/zip"
  )

  output$dwn_stl <- renderText({ dwn_stl() })
  output$dwn_txt <- renderText({ dwn_txt() })
  output$dwn_fmt <- renderUI({ dwn_fmt() })

  output$dwn_dtl <- renderUI({
    req(input$go)
    downloadButton('dwn_dtl_pre', label = 'Detailed data')
  })

  output$dwn_fit <- renderUI({
    req(input$go)
    downloadButton('dwn_fit_pre', label = 'Fitted model')
  })

  ## Cite ----

  # output$cite_stl <- eventReactive(input$go, { renderText({"Cite"}) })
  #
  # output$cite_chicago_stl <- eventReactive(input$go, { renderText({ "Chicago citation" }) })
  #
  # output$cite <- eventReactive(input$go, { renderText({ cite() }) })
  #
  # output$cite_bibtex_stl <- eventReactive(input$go, { renderText({"BibTeX entry"}) })
  #
  # output$cite_bibtex <- eventReactive(input$go, { renderText({ cite_bibtex() }) })

  # Footer ----

  output$site_footer <- renderText({
    glue("<center><i>Open Trade Statistics {lubridate::year(Sys.Date())}.</i></center>")
  })

  # Bookmarking ----

  observe({
    # Trigger this observer every time an input changes
    # strip shiny related URL parameters
    rvtl(input)
    setBookmarkExclude(c(
      "shinyhelper-modal_params"
    ))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}
