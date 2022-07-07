#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom dplyr arrange bind_rows case_when collect dense_rank desc
#'     everything filter group_by inner_join left_join mutate select summarise
#'     tbl ungroup
#' @importFrom highcharter hcaes hchart hc_title hc_xAxis hc_yAxis
#'     renderHighchart
#' @importFrom lubridate day year
#' @importFrom rio export
#' @importFrom rlang sym
#' @importFrom shinyhelper observe_helpers
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

  # Titles ----

  r_add_the <- eventReactive(input$go, {
    if (substr(rname(), 1, 6) == "United" |
        substr(rname(), 1, 3) == "USA" |
        substr(rname(), 1, 7) == "Russian") {
      "the"
    } else {
      ""
    }
  })

  r_add_upp_the <- eventReactive(input$go, {
    if (substr(rname(), 1, 6) == "United" |
        substr(rname(), 1, 3) == "USA" |
        substr(rname(), 1, 7) == "Russian") {
      "The"
    } else {
      ""
    }
  })

  p_add_the <- eventReactive(input$go, {
    if (substr(pname(), 1, 6) == "United" |
        substr(pname(), 1, 3) == "USA" |
        substr(pname(), 1, 7) == "Russian") {
      "the"
    } else {
      ""
    }
  })

  title <- eventReactive(input$go, {
    switch(
      tbl_dtl(),
      "yrc" = glue("{ r_add_upp_the() } { rname() } multilateral trade between { min(inp_y()) } and { max(inp_y()) }"),
      "yrpc" = glue("{ r_add_upp_the() } { rname() } and { p_add_the() } { pname() } trade between { min(inp_y()) } and { max(inp_y()) }")
    )
  })

  # Visualize ----

  wt <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

  ## Data ----

  df_dtl <- reactive({
    d <- tbl(con, "yrpc") %>%
      filter(year %in% !!inp_y())

    wt_pp$inc(1)

    if (inp_s() != "all") {
      if (inp_s() == "vaccine") {
        d <- d %>%
          left_join(tbl(con, "vaccine_inputs")) %>%
          mutate(
            section_code = case_when(
              is_vaccine_input == 1L ~ "vaccine",
              TRUE ~ section_code
            )
          )
      }

      if (nchar(inp_s()) == 4) {
        d <- d %>%
          filter(substr(commodity_code, 1, 4) == !!inp_s())
      } else {
        d <- d %>%
          filter(section_code == !!inp_s())
      }
    }

    wt_pp$inc(1)

    d <- d %>%
      group_by(year, reporter_iso, partner_iso) %>%
      summarize(
        trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T),
        trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)
      ) %>%
      ungroup() %>%
      collect()

    if (inp_d() != "No") {
      d <- gdp_deflator_adjustment(d, as.integer(inp_d()))
    }

    wt_pp$inc(1)

    return(d)
  }) %>%
    bindCache(inp_y(), inp_s(), inp_d()) %>%
    bindEvent(input$go)

  ## Trade ----

  ### Tables ----

  tr_tbl_agg <- eventReactive(input$go, {
    df_agg() %>%
      select(!!sym("year"), !!sym("trade_value_usd_exp"), !!sym("trade_value_usd_imp"))
  })

  exp_val_min_yr <- eventReactive(input$go, {
    tr_tbl_agg() %>%
      filter(!!sym("year") == min(inp_y())) %>%
      select(!!sym("trade_value_usd_exp")) %>%
      as.numeric()
  })

  exp_val_max_yr <- eventReactive(input$go, {
    tr_tbl_agg() %>%
      filter(!!sym("year") == max(inp_y())) %>%
      select(!!sym("trade_value_usd_exp")) %>%
      as.numeric()
  })

  imp_val_min_yr <- eventReactive(input$go, {
    tr_tbl_agg() %>%
      filter(!!sym("year") == min(inp_y())) %>%
      select(!!sym("trade_value_usd_imp")) %>%
      as.numeric()
  })

  imp_val_max_yr <- eventReactive(input$go, {
    tr_tbl_agg() %>%
      filter(!!sym("year") == max(inp_y())) %>%
      select(!!sym("trade_value_usd_imp")) %>%
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

  trd_rankings <- eventReactive(input$go, {
    min_max_y <- c(min(inp_y()), max(inp_y()))

    d <- tbl(con, "yrp") %>%
      filter(
        !!sym("year") %in% min_max_y &
          !!sym("reporter_iso") == !!inp_r()
      ) %>%
      collect()

    if (inp_d() != "No") {
      d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con)
    }

    d <- d %>%
      # filter(!!sym("partner_iso") != "0-unspecified") %>%
      mutate(
        trd_value_usd_bal = !!sym("trade_value_usd_exp") + !!sym("trade_value_usd_imp")
      ) %>%
      group_by(!!sym("year")) %>%
      mutate(
        bal_rank = dense_rank(desc(!!sym("trd_value_usd_bal"))),
        exp_share = !!sym("trade_value_usd_exp") / sum(!!sym("trade_value_usd_exp")),
        imp_share = !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp"))
      )

    return(d)
  })

  trd_rankings_no_min_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == min(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("bal_rank")) %>%
      as.character()
  })

  trd_rankings_no_max_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == max(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("bal_rank")) %>%
      as.character()
  })

  trd_rankings_remained <- eventReactive(input$go, {
    ifelse(
      trd_rankings_no_min_yr() == trd_rankings_no_max_yr(),
      "remained",
      "moved to"
    )
  })

  trd_rankings_exp_share_min_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == min(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("exp_share")) %>%
      as.numeric()
  })

  trd_rankings_exp_share_min_yr_2 <- eventReactive(input$go, {
    show_percentage(trd_rankings_exp_share_min_yr())
  })

  trd_rankings_exp_share_max_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == max(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("exp_share")) %>%
      as.numeric()
  })

  trd_rankings_exp_share_max_yr_2 <- eventReactive(input$go, {
    show_percentage(trd_rankings_exp_share_max_yr())
  })

  trd_rankings_imp_share_min_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == min(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("imp_share")) %>%
      as.numeric()
  })

  trd_rankings_imp_share_min_yr_2 <- eventReactive(input$go, {
    show_percentage(trd_rankings_imp_share_min_yr())
  })

  trd_rankings_imp_share_max_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == max(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("imp_share")) %>%
      as.numeric()
  })

  trd_rankings_imp_share_max_yr_2 <- eventReactive(input$go, {
    wt$inc(1)

    show_percentage(trd_rankings_imp_share_max_yr())
  })

  ### Text/Visual elements ----

  trd_smr_txt_exp <- eventReactive(input$go, {
    glue("The exports of { section_name_pp() } { exports_growth_increase_decrease_pp() } from
         { exp_val_min_yr_2_pp() } in { min(inp_pp_y()) } to { exp_val_max_yr_2_pp() } in { max(inp_pp_y()) }
         (annualized { exports_growth_increase_decrease_2_pp() } of { exports_growth_2_pp() }).")
  })

  trd_smr_txt_imp <- eventReactive(input$go, {
    glue("The imports of { section_name_pp() } { imports_growth_increase_decrease_pp() } from
         { imp_val_min_yr_2_pp() } in { min(inp_pp_y()) } to { imp_val_max_yr_2_pp() } in { max(inp_pp_y()) }
         (annualized { imports_growth_increase_decrease_2_pp() } of { imports_growth_2_pp() }).")
  })

  trd_exc_columns_title <- eventReactive(input$go, {
    glue("{ section_name() } exchange in { min(inp_y()) } and { max(inp_y()) }")
  })

  trd_exc_columns_agg <- reactive({
    d <- df_dtl() %>%
      group_by(year) %>%
      summarise(
        trade_value_usd_exp = sum(trade_value_usd_exp),
        trade_value_usd_imp = sum(trade_value_usd_imp)
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
      mutate(year = as.character(year))

    wt_pp$inc(1)

    hchart(d,
           "column",
           hcaes(x = year, y = trade, group = flow),
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
    switch(
      tbl_dtl(),
      "yrc" = glue("Exports of { r_add_the() } { rname() } to the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product"),
      "yrpc" = glue("Exports of { r_add_the() } { rname() } to { p_add_the() } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
    )
  })

  exp_tt_min_yr <- eventReactive(input$go, {
    glue("{ min(inp_y()) }")
  })

  exp_tm_dtl_min_yr <- reactive({
    d <- df_dtl() %>%
      filter(!!sym("year") == min(inp_y())) %>%
      p_fix_section_and_aggregate(col = "trade_value_usd_exp", sql_con = con)

    d2 <- p_colors(d, sql_con = con)

    p_to_highcharts(d, d2)
  }) %>%
    bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
    bindEvent(input$go)

  exp_tt_max_yr <- eventReactive(input$go, {
    glue("{ max(inp_y()) }")
  })

  exp_tm_dtl_max_yr <- reactive({
    d <- df_dtl() %>%
      filter(!!sym("year") == max(inp_y())) %>%
      p_fix_section_and_aggregate(col = "trade_value_usd_exp", sql_con = con)

    d2 <- p_colors(d, sql_con = con)

    wt$inc(1)

    p_to_highcharts(d, d2)
  }) %>%
    bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
    bindEvent(input$go)

  ## Imports ----

  ### Visual elements ----

  imp_tt_yr <- eventReactive(input$go, {
    switch(
      tbl_dtl(),
      "yrc" = glue("Imports of { r_add_the() } { rname() } from the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product"),
      "yrpc" = glue("Imports of { r_add_the() } { rname() } from { p_add_the() } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
    )
  })

  imp_tt_min_yr <- eventReactive(input$go, {
    glue("{ min(inp_y()) }")
  })

  imp_tm_dtl_min_yr <- reactive({
    d <- df_dtl() %>%
      filter(!!sym("year") == min(inp_y())) %>%
      p_fix_section_and_aggregate(col = "trade_value_usd_imp", sql_con = con)

    d2 <- p_colors(d, sql_con = con)

    p_to_highcharts(d, d2)
  }) %>%
    bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
    bindEvent(input$go)

  imp_tt_max_yr <- eventReactive(input$go, {
    glue("{ max(inp_y()) }")
  })

  imp_tm_dtl_max_yr <- reactive({
    d <- df_dtl() %>%
      filter(!!sym("year") == max(inp_y())) %>%
      p_fix_section_and_aggregate(col = "trade_value_usd_imp", sql_con = con)

    d2 <- p_colors(d, sql_con = con)

    wt$inc(2)

    out <- p_to_highcharts(d, d2)

    wt$close()
    return(out)
  }) %>%
    bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
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

  ## Titles ----

  output$title <- renderText({ title() })

  # put here to avoid repetition in UI
  legend_txt <- "The information displayed here is based on <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. Please read our <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a> for a full description
      of restrictions and applicable licenses. These figures do not include services or foreign direct investment."

  output$title_legend <- renderText({ legend_txt })

  ## Country profile ----

  ### Trade ----

  output$trd_stl <- eventReactive(input$go, {
    switch(
      tbl_dtl(),
      "yrc" = glue("Total multilateral Exports and Imports"),
      "yrpc" = glue("Total bilateral Exports and Imports")
    )
  })

  output$trd_stl <- eventReactive(input$go, { "Total multilateral Exports and Imports" })
  output$trd_stl_exp <- eventReactive(input$go, { "Exports" })
  output$trd_stl_imp <- eventReactive(input$go, { "Imports" })

  output$trd_smr_exp <- renderText(trd_smr_txt_exp())
  output$trd_smr_imp <- renderText(trd_smr_txt_imp())

  output$trd_exc_lines_agg <- renderHighchart({ trd_exc_lines_agg() })

  ### Exports ----

  output$exp_tt_yr <- renderText(exp_tt_yr())
  output$exp_tt_min_yr <- renderText(exp_tt_min_yr())
  output$exp_tm_dtl_min_yr <- renderHighchart({exp_tm_dtl_min_yr()})
  output$exp_tt_max_yr <- renderText(exp_tt_max_yr())
  output$exp_tm_dtl_max_yr <- renderHighchart({exp_tm_dtl_max_yr()})

  ### Imports ----

  output$imp_tt_yr <- renderText(imp_tt_yr())
  output$imp_tt_min_yr <- renderText(imp_tt_min_yr())
  output$imp_tm_dtl_min_yr <- renderHighchart({imp_tm_dtl_min_yr()})
  output$imp_tt_max_yr <- renderText(imp_tt_max_yr())
  output$imp_tm_dtl_max_yr <- renderHighchart({imp_tm_dtl_max_yr()})

  ## Download ----

  dwn_stl <- eventReactive(input$go, { "Download country data" })

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
      glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_detailed.{ inp_fmt() }")
    },
    content = function(filename) {
      export(df_dtl(), filename)
    },
    contentType = "application/zip"
  )

  output$dwn_agg_pre <- downloadHandler(
    filename = function() {
      glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_aggregated.{ inp_fmt() }")
    },
    content = function(filename) {
      export(df_agg(), filename)
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

  output$dwn_agg <- renderUI({
    req(input$go)
    downloadButton('dwn_agg_pre', label = 'Aggregated data')
  })

  ## Citation ----

  output$citation_stl <- renderUI({
    req(input$go)
    h2("Citation")
  })

  output$citation_text <- renderUI({
    req(input$go)
    HTML(cite_text())
  })

  output$citation_bibtex <- renderUI({
    req(input$go)
    pre(cite_bibtex())
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
      "shinyhelper-modal_params", "own"
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
