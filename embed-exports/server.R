## server.R ##

shinyServer(
  function(input, output, session) {
    # Input -------------------------------------------------------------------

    y <- reactive({
      input$y
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

    table_detailed <- reactive({
      if (p_iso() == "all") {
        "yrc"
      } else {
        "yrpc"
      }
    })

    # Data --------------------------------------------------------------------

    data_detailed <- reactive({
      ots_create_tidy_data(
        years = y(),
        reporters = r_iso(),
        partners = p_iso(),
        table = table_detailed(),
        use_localhost = use_localhost
      ) %>% 
        select(section_shortname_english, section_color, export_value_usd) %>%
        filter(export_value_usd > 0)
    })
    
    r_add_the <- reactive({
      if (substr(r_name(), 1, 6) == "United" | substr(r_name(), 1, 3) == "USA") {
        "the"
      } else {
        ""
      }
    })
    
    p_add_the <- reactive({
      if (substr(p_name(), 1, 6) == "United" | substr(p_name(), 1, 3) == "USA") {
        "the"
      } else {
        ""
      }
    })
    
    exports_title <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Exports of { r_add_the() } { r_name() } to the rest of the World in { y() }"),
        "yrpc" = glue::glue("Exports of { r_add_the() } { r_name() } to { p_add_the() } { p_name() } in { y() }")
      )
    })

    exports_treemap_detailed <- reactive({
      d <- data_detailed() %>%
        mutate(
          share = export_value_usd / sum(export_value_usd)
          # section_shortname_english = ifelse(share < 0.01, "Others <1% each", section_shortname_english),
          # section_color = ifelse(share < 0.01, "#d3d3d3", section_color)
        ) %>%
        group_by(section_shortname_english, section_color) %>%
        summarise(export_value_usd = sum(export_value_usd, na.rm = T)) %>%
        ungroup() %>%
        mutate(
          share = paste0(round(100 * export_value_usd / sum(export_value_usd), 2), "%"),
          section_shortname_english = paste0(section_shortname_english, "<br>", share)
        ) %>%
        rename(
          value = export_value_usd,
          name = section_shortname_english,
          color = section_color
        )
      
      highchart() %>%
        hc_chart(type = "treemap") %>%
        hc_xAxis(categories = d$name) %>%
        hc_add_series(d,
                      name = "Export Value USD",
                      showInLegend = FALSE,
                      dataLabels = list(
                        verticalAlign = "top",
                        align = "left",
                        style = list(fontSize = "12px", textOutline = FALSE)
                      )
        ) %>%
        hc_title(text = exports_title()) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
    })

    # Output ------------------------------------------------------------------

    output$exports_treemap_detailed <- renderHighchart({
      exports_treemap_detailed()
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
