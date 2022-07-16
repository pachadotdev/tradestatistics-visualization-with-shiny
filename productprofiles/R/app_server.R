#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import otsshinycommon
#' @importFrom dplyr arrange bind_rows case_when collect dense_rank desc
#'     everything filter group_by inner_join left_join mutate select summarise
#'     tbl tibble ungroup
#' @importFrom forcats fct_lump_n
#' @importFrom highcharter hcaes hchart hc_title hc_xAxis hc_yAxis JS
#'     renderHighchart
#' @importFrom glue glue
#' @importFrom lubridate day year
#' @importFrom rio export
#' @importFrom rlang sym
#' @importFrom shinyhelper observe_helpers
#' @importFrom shinyjs hide show
#' @importFrom tidyr pivot_longer
#' @importFrom waiter Waitress
#' @noRd
app_server <- function(input, output, session) {
  # Connect to SQL ----

  con <- sql_con()

  # User inputs ----

  observe_helpers()

  inp_y <- reactive({
    y <- (min(input$y[1], input$y[2])):(max(input$y[1], input$y[2]))
    y <- seq(min(y), max(y), by = ifelse(max(y) - min(y) >= 10, 2, 1))
    return(y)
  })

  inp_s <- reactive({ sort(input$s) })
  inp_d <- reactive({ input$d }) # adjust dollar

  inp_fmt <- reactive({ input$fmt }) # format

  section_name <- eventReactive(input$go, {
    s <- if (nchar(inp_s()) == 2) {
      gsub(".* -", "", names(otsshinycommon::sections_display[
        otsshinycommon::sections_display == inp_s()]))
    } else if (nchar(inp_s()) == 4) {
      gsub(".* - ", "", names(otsshinycommon::commodities_short_display[
        otsshinycommon::commodities_short_display == inp_s()]))
    } else if (inp_s() == "vaccine") {
      "Vaccine Inputs"
    }

    return(s)
  })

  # Titles ----

  title <- eventReactive(input$go, {
    glue("{ section_name() } multilateral trade in { min(inp_y()) } and { max(inp_y()) }")
  })

  # Visualize ----

  wt <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

  ## Data ----

  df_dtl <- reactive({
    wt$notify(position = "tr")

    wt$inc(1)

    d <- tbl(con, "yrpc") %>%
      filter(year %in% !!inp_y())

    wt$inc(1)

    if (inp_s() == "vaccine") {
      d <- d %>%
        left_join(tbl(con, "vaccine_inputs")) %>%
        mutate(
          section_code = case_when(
            is_vaccine_input == 1L ~ "vaccine",
            TRUE ~ !!sym("section_code")
          )
        )
    }

    if (nchar(inp_s()) == 4) {
      d <- d %>%
        filter(substr(!!sym("commodity_code"), 1, 4) == !!inp_s())
    } else {
      d <- d %>%
        filter(!!sym("section_code") == !!inp_s())
    }

    wt$inc(1)

    d <- d %>%
      group_by(!!sym("year"), !!sym("reporter_iso"), !!sym("partner_iso")) %>%
      summarise(
        trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = T),
        trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = T)
      ) %>%
      ungroup() %>%
      collect()

    if (inp_d() != "No") {
      d <- gdp_deflator_adjustment(d, as.integer(inp_d()), sql_con = con)
    }

    wt$inc(2)

    return(d)
  }) %>%
    bindCache(inp_y(), inp_s(), inp_d()) %>%
    bindEvent(input$go)

  ## Trade ----

  ### Tables ----

  exp_val_min_yr <- eventReactive(input$go, {
    df_dtl() %>%
      filter(!!sym("year") == min(inp_y())) %>%
      select(!!sym("trade_value_usd_exp")) %>%
      summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"))) %>%
      as.numeric()
  })

  exp_val_max_yr <- eventReactive(input$go, {
    df_dtl() %>%
      filter(!!sym("year") == max(inp_y())) %>%
      select(!!sym("trade_value_usd_exp")) %>%
      summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"))) %>%
      as.numeric()
  })

  imp_val_min_yr <- eventReactive(input$go, {
    df_dtl() %>%
      filter(!!sym("year") == min(inp_y())) %>%
      select(!!sym("trade_value_usd_imp")) %>%
      summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"))) %>%
      as.numeric()
  })

  imp_val_max_yr <- eventReactive(input$go, {
    df_dtl() %>%
      filter(!!sym("year") == max(inp_y())) %>%
      select(!!sym("trade_value_usd_imp")) %>%
      summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"))) %>%
      as.numeric()
  })

  exp_val_min_yr_2 <- eventReactive(input$go, {
    show_dollars(exp_val_min_yr())
  })

  exp_val_max_yr_2 <- eventReactive(input$go, {
    show_dollars(exp_val_max_yr())
  })

  imp_val_min_yr_2 <- eventReactive(input$go, {
    show_dollars(imp_val_min_yr())
  })

  imp_val_max_yr_2 <- eventReactive(input$go, {
    show_dollars(imp_val_max_yr())
  })

  exports_growth <- eventReactive(input$go, {
    growth_rate(
      exp_val_max_yr(), exp_val_min_yr(), inp_y()
    )
  })

  exports_growth_2 <- eventReactive(input$go, {
    show_percentage(exports_growth())
  })

  exports_growth_increase_decrease <- eventReactive(input$go, {
    ifelse(exports_growth() >= 0, "increased", "decreased")
  })

  exports_growth_increase_decrease_2 <- eventReactive(input$go, {
    ifelse(exports_growth() >= 0, "increase", "decrease")
  })

  imports_growth <- eventReactive(input$go, {
    growth_rate(
      imp_val_max_yr(), imp_val_min_yr(), inp_y()
    )
  })

  imports_growth_2 <- eventReactive(input$go, {
    show_percentage(imports_growth())
  })

  imports_growth_increase_decrease <- eventReactive(input$go, {
    ifelse(imports_growth() >= 0, "increased", "decreased")
  })

  imports_growth_increase_decrease_2 <- eventReactive(input$go, {
    ifelse(imports_growth() >= 0, "increase", "decrease")
  })

  ### Text/Visual elements ----

  trd_smr_txt_exp <- eventReactive(input$go, {
    glue("The exports of { section_name() } { exports_growth_increase_decrease() } from
         { exp_val_min_yr_2() } in { min(inp_y()) } to { exp_val_max_yr_2() } in { max(inp_y()) }
         (annualized { exports_growth_increase_decrease_2() } of { exports_growth_2() }).")
  })

  trd_smr_txt_imp <- eventReactive(input$go, {
    glue("The imports of { section_name() } { imports_growth_increase_decrease() } from
         { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) }
         (annualized { imports_growth_increase_decrease_2() } of { imports_growth_2() }).")
  })

  trd_exc_columns_title <- eventReactive(input$go, {
    glue("{ section_name() } exchange in { min(inp_y()) } and { max(inp_y()) }")
  })

  trd_exc_columns_agg <- reactive({
    d <- df_dtl() %>%
      group_by(!!sym("year")) %>%
      summarise(
        trade_value_usd_exp = sum(!!sym("trade_value_usd_exp")),
        trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"))
      )

    d <- tibble(
      year = d$year,
      trade = d$trade_value_usd_exp,
      flow = "Exports"
    ) %>%
      bind_rows(
        tibble(
          year = d$year,
          trade = d$trade_value_usd_imp,
          flow = "Imports"
        )
      ) %>%
      mutate(year = as.character(!!sym("year")))

    wt$inc(1)

    hchart(d,
           "column",
           hcaes(x = "year", y = "trade", group = "flow"),
           tooltip = list(
             pointFormatter = custom_tooltip_short()
           )) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "USD billion"),
               labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
      hc_title(text = trd_exc_columns_title())
  }) %>%
    bindCache(inp_y(), inp_s(), inp_d()) %>%
    bindEvent(input$go)

  ## Exports ----

  ### Visual elements ----

  exp_tt_yr <- eventReactive(input$go, {
    glue("Exports of { section_name() } in { min(inp_y()) } and { max(inp_y()) }, by country")
  })

  exp_col_dtl_yr <- reactive({
    d <- df_dtl() %>%
      filter(year == min(!!sym("year"))) %>%
      od_order_and_add_continent(col = "trade_value_usd_exp", sql_con = con) %>%
      group_by(!!sym("country_name")) %>%
      summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE)) %>%
      mutate(year = min(inp_y())) %>%
      filter(!!sym("trade_value") > 0) %>%
      mutate(country_name = fct_lump_n(f = !!sym("country_name"),
                                       n = 10,
                                       w = !!sym("trade_value"),
                                       other_level = "Rest of the World")) %>%
      group_by(!!sym("year"), !!sym("country_name")) %>%
      summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE)) %>%
      bind_rows(
        df_dtl() %>%
          filter(year == max(!!sym("year"))) %>%
          od_order_and_add_continent(col = "trade_value_usd_exp", sql_con = con) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE)) %>%
          mutate(year = max(inp_y())) %>%
          filter(!!sym("trade_value") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"),
                                           n = 10,
                                           w = !!sym("trade_value"),
                                           other_level = "Rest of the World")) %>%
          group_by(!!sym("year"), !!sym("country_name")) %>%
          summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE))
      )

    hchart(d,
           "column",
           hcaes(x = "country_name", y = "trade_value", group = "year"),
           tooltip = list(
             pointFormatter = custom_tooltip_short()
           )) %>%
      hc_xAxis(title = list(text = "Country")) %>%
      hc_yAxis(title = list(text = "USD billion"),
               labels = list(
                 formatter = JS("function() { return this.value / 1000000000 }")
               )) %>%
      hc_title(text = glue("Exports in { min(inp_y()) } and { max(inp_y()) }"))
  }) %>%
    bindCache(inp_y(), inp_s(), inp_d()) %>%
    bindEvent(input$go)

  exp_tt_min_yr <- eventReactive(input$go, {
    glue("Exports in { min(inp_y()) }")
  })

  exp_tm_dtl_min_yr <- reactive({
    d <- df_dtl() %>%
      filter(year == min(!!sym("year"))) %>%
      od_order_and_add_continent(col = "trade_value_usd_exp", sql_con = con)

    d2 <- od_colors(d, sql_con = con)

    wt$inc(1)

    od_to_highcharts(d, d2)
  }) %>%
    bindCache(inp_y(), inp_s(), inp_d()) %>%
    bindEvent(input$go)

  exp_tt_max_yr <- eventReactive(input$go, {
    glue("Exports in { max(inp_y()) }")
  })

  exp_tm_dtl_max_yr <- reactive({
    d <- df_dtl() %>%
      filter(year == max(!!sym("year"))) %>%
      od_order_and_add_continent(col = "trade_value_usd_exp", sql_con = con)

    d2 <- od_colors(d, sql_con = con)

    wt$inc(1)

    od_to_highcharts(d, d2)
  }) %>%
    bindCache(inp_y(), inp_s(), inp_d()) %>%
    bindEvent(input$go)

  ## Imports ----

  ### Visual elements ----

  imp_tt_yr <- eventReactive(input$go, {
    glue("Imports of { section_name() } in { min(inp_y()) } and { max(inp_y()) }, by country")
  })

  imp_col_dtl_yr <- reactive({
    d <- df_dtl() %>%
      filter(year == min(!!sym("year"))) %>%
      od_order_and_add_continent(col = "trade_value_usd_imp", sql_con = con) %>%
      group_by(!!sym("country_name")) %>%
      summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE)) %>%
      mutate(year = min(inp_y())) %>%
      filter(!!sym("trade_value") > 0) %>%
      mutate(country_name = fct_lump_n(f = !!sym("country_name"),
                                       n = 10,
                                       w = !!sym("trade_value"),
                                       other_level = "Rest of the World")) %>%
      group_by(!!sym("year"), !!sym("country_name")) %>%
      summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE)) %>%
      bind_rows(
        df_dtl() %>%
          filter(year == max(!!sym("year"))) %>%
          od_order_and_add_continent(col = "trade_value_usd_imp", sql_con = con) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE)) %>%
          mutate(year = max(inp_y())) %>%
          filter(!!sym("trade_value") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"),
                                           n = 10,
                                           w = !!sym("trade_value"),
                                           other_level = "Rest of the World")) %>%
          group_by(!!sym("year"), !!sym("country_name")) %>%
          summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE))
      )

    hchart(d,
           "column",
           hcaes(x = "country_name", y = "trade_value", group = "year"),
           tooltip = list(
             pointFormatter = custom_tooltip_short()
           )) %>%
      hc_xAxis(title = list(text = "Country")) %>%
      hc_yAxis(title = list(text = "USD billion"),
               labels = list(
                 formatter = JS("function() { return this.value / 1000000000 }")
               )) %>%
      hc_title(text = glue("Imports in { min(inp_y()) } and { max(inp_y()) }"))
  }) %>%
    bindCache(inp_y(), inp_s(), inp_d()) %>%
    bindEvent(input$go)

  imp_tt_min_yr <- eventReactive(input$go, {
    glue("Imports in { min(inp_y()) }")
  })

  imp_tm_dtl_min_yr <- reactive({
    d <- df_dtl() %>%
      filter(year == min(!!sym("year"))) %>%
      od_order_and_add_continent(col = "trade_value_usd_imp", sql_con = con)

    d2 <- od_colors(d, sql_con = con)

    wt$inc(1)

    od_to_highcharts(d, d2)
  }) %>%
    bindCache(inp_y(), inp_s(), inp_d()) %>%
    bindEvent(input$go)

  imp_tt_max_yr <- eventReactive(input$go, {
    glue("Imports in { max(inp_y()) }")
  })

  imp_tm_dtl_max_yr <- reactive({
    d <- df_dtl() %>%
      filter(year == max(!!sym("year"))) %>%
      od_order_and_add_continent(col = "trade_value_usd_imp", sql_con = con)

    d2 <- od_colors(d, sql_con = con)

    wt$inc(1)

    out <- od_to_highcharts(d, d2)

    wt$close()
    return(out)
  }) %>%
    bindCache(inp_y(), inp_s(), inp_d()) %>%
    bindEvent(input$go)

  # Cite ----

  site_url <- "https://shiny.tradestatistics.io"

  cite_text <- reactive({
    glue(
      "Open Trade Statistics. \"OTS BETA DASHBOARD\". <i>Open Trade Statistics</i>.
        Accessed {months(Sys.Date()) } { day(Sys.Date()) }, { year(Sys.Date()) }. { site_url }/."
    )
  })

  cite_bibtex <- reactive({
    glue("@misc{{open_trd_statistics_{year(Sys.Date())},
      title = {{Open Trade Statistics Beta Dashboard}},
      url = {{{site_url}}},
      author = {{Vargas, Mauricio}},
      doi = {{10.5281/zenodo.3738793}},
      publisher = {{Open Trade Statistics}},
      year = {{2022}},
      month = {{Apr}},
      note = {{Accessed: { months(Sys.Date()) } { day(Sys.Date()) }, { year(Sys.Date()) }}}}}"
    )
  })

  # Outputs ----

  ## Titles / texts ----

  output$title <- renderText({ title() })

  # put here to avoid repetition in UI
  legend_txt <- "The information displayed here is based on <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. Please read our <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a> for a full description
      of restrictions and applicable licenses. These figures do not include services or foreign direct investment."

  ## Dynamic / server side selectors ----

  updateSelectizeInput(session, "s",
                       choices = list(
                         "All Products" = available_all(),
                         "Vaccine Inputs" = available_vaccine(),
                         "HS Sections" = available_sections_code(),
                         "HS Commodities" = available_commodities_code()
                       ),
                       selected = "all",
                       server = TRUE
  )

  ## Product profile ----

  ### Trade ----

  output$trd_stl <- eventReactive(input$go, { "Total multilateral Exports and Imports" })

  output$trd_stl_exp <- eventReactive(input$go, { "Exports" })
  output$trd_stl_imp <- eventReactive(input$go, { "Imports" })

  output$trd_smr_exp <- renderText(trd_smr_txt_exp())
  output$trd_smr_imp <- renderText(trd_smr_txt_imp())

  output$trd_exc_columns_agg <- renderHighchart({ trd_exc_columns_agg() })

  # ### Exports ----

  output$exp_tt_yr <- renderText(exp_tt_yr())
  output$exp_col_dtl_yr <- renderHighchart({exp_col_dtl_yr()})
  output$exp_tt_min_yr <- renderText(exp_tt_min_yr())
  output$exp_tm_dtl_min_yr <- renderHighchart({exp_tm_dtl_min_yr()})
  output$exp_tt_max_yr <- renderText(exp_tt_max_yr())
  output$exp_tm_dtl_max_yr <- renderHighchart({exp_tm_dtl_max_yr()})

  # ### Imports ----

  output$imp_tt_yr <- renderText(imp_tt_yr())
  output$imp_tt_min_yr <- renderText(imp_tt_min_yr())
  output$imp_col_dtl_yr <- renderHighchart({imp_col_dtl_yr()})
  output$imp_tm_dtl_min_yr <- renderHighchart({imp_tm_dtl_min_yr()})
  output$imp_tt_max_yr <- renderText(imp_tt_max_yr())
  output$imp_tm_dtl_max_yr <- renderHighchart({imp_tm_dtl_max_yr()})

  ## Download ----

  dwn_stl <- eventReactive(input$go, { "Download product data" })

  dwn_txt <- eventReactive(input$go, {
    "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS) and DTA (Stata)."
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
      glue("{ inp_s() }_{ min(inp_y()) }_{ max(inp_y()) }_detailed.{ inp_fmt() }")
    },
    content = function(filename) {
      export(df_dtl(), filename)
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

  ## Citation ----

  output$citation_text <- renderUI({
    req(input$go)
    HTML(cite_text())
  })

  output$citation_bibtex <- renderUI({
    req(input$go)
    pre(cite_bibtex())
  })

  # Hide boxes until viz is ready ----

  ## observe the button being pressed
  observeEvent(input$go, {
    if (input$go > 0) {
      show(id = "aggregated_trade")
      show(id = "detailed_trade")
    } else {
      hide(id = "aggregated_trade")
      hide(id = "detailed_trade")
    }
  })

  # Footer ----

  output$site_footer <- renderText({
    glue("<center><i>Open Trade Statistics {year(Sys.Date())}.</i></center>")
  })

  # Bookmarking ----

  observe({
    # Trigger this observer every time an input changes
    # strip shiny related URL parameters
    rvtl(input)
    setBookmarkExclude(c(
      "shinyhelper-modal_params", "own", "sidebarCollapsed", "sidebarItemExpanded"
    ))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)
