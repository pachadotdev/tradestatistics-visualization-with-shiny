## server.R ##

shinyServer(
  function(input, output, session) {
    # Input -------------------------------------------------------------------

    y1 <- reactive({
      input$y1
    })

    y2 <- reactive({
      input$y2
    })
    
    y <- reactive({
      input$y1:input$y2
    })
      
    r_iso <- reactive({
      input$r
    })

    r_name <- reactive({
      reporters_to_display %>%
        filter(available_reporters_iso == input$r) %>%
        select(available_reporters_names) %>%
        as.character()
    })

    p_iso <- reactive({
      input$p
    })

    p_name <- reactive({
      reporters_to_display %>%
        filter(available_reporters_iso == input$p) %>%
        select(available_reporters_names) %>%
        as.character()
    })

    table_aggregated <- reactive({
      if (p_iso() == "all") {
        "yr"
      } else {
        "yrp"
      }
    })

    table_detailed <- reactive({
      if (p_iso() == "all") {
        "yrc"
      } else {
        "yrpc"
      }
    })

    # Data --------------------------------------------------------------------

    text_add_the <- reactive({
      if (substr(p_name(), 1, 6) == "United" | substr(p_name(), 1, 3) == "USA") {
        "the "
      } else {
        ""
      }
    })
    
    title <- eventReactive(input$go, {
      glue::glue("Trade between { text_add_the() } { r_name() } and { text_add_the() } { p_name() } from { y1() } to { y2() }, aggregated")
    })

    data_aggregated <- eventReactive(input$go, {
      ots_create_tidy_data(
        years = y(),
        reporters = r_iso(),
        partners = p_iso(),
        include_shortnames = FALSE,
        include_communities = FALSE,
        table = table_aggregated()
      )
    })

    trade_table_aggregated <- eventReactive(input$go, {
      data_aggregated() %>%
        select(year, export_value_usd, import_value_usd)
    })
    
    trade_exchange_bars_title <- eventReactive(input$go, {
      switch(table_aggregated(),
             "yr" = glue::glue("{ r_name() } multilateral trade between { min(y()) } and { max(y()) }"),
             "yrp" = glue::glue("{ r_name() } and { p_name() } exchange between { min(y()) } and { max(y()) }")
      )
    })

    trade_bars_aggregated <- eventReactive(input$go, {
      d <- trade_table_aggregated() %>%
        gather(key, value, -year) %>%
        mutate(
          key = ifelse(key == "export_value_usd", "Exports", "Imports")
        ) %>%
        rename(
          `Trade Value` = value,
          `Year` = year,
          group = key
        )
      
      hchart(d, "column", hcaes(x = `Year`, y = `Trade Value`, group = group)) %>% 
        hc_colors(c("#4d6fd0", "#bf3251")) %>% 
        hc_title(text = trade_exchange_bars_title()) %>% 
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
    })

    # Output ------------------------------------------------------------------

    output$trade_bars_aggregated <- renderHighchart({
      trade_bars_aggregated()
    })

    # Bookmarking -------------------------------------------------------------

    observe({
      # Trigger this observer every time an input changes
      # strip shiny related URL parameters
      reactiveValuesToList(input)
      session$doBookmark()
    })

    onBookmarked(function(url) {
      updateQueryString(url)
    })
  }
)
