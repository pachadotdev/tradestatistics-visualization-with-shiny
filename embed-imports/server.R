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
        "yrc-ca"
      } else {
        "yrpc-ca"
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
      glue::glue("Imports of { text_add_the() } { r_name() } from { text_add_the() } { p_name() } in { y() }, grouped by product community")
    })

    data_detailed <- eventReactive(input$go, {
      ots_create_tidy_data(
        years = y(),
        reporters = r_iso(),
        partners = p_iso(),
        include_shortnames = FALSE,
        include_communities = FALSE,
        table = table_detailed()
      ) %>%
        filter(import_value_usd > 0) %>%
        group_by(community_name, community_color) %>%
        summarise(import_value_usd = sum(import_value_usd, na.rm = T)) %>%
        ungroup() %>%
        mutate(
          share = import_value_usd / sum(import_value_usd),
          community_name = ifelse(share < 0.01, "Others >1% each", community_name),
          community_color = ifelse(share < 0.01, "#d3d3d3", community_color)
        ) %>%
        group_by(community_name, community_color) %>%
        summarise(import_value_usd = sum(import_value_usd, na.rm = T)) %>%
        ungroup()
    })

    imports_table_detailed <- eventReactive(input$go, {
      data_detailed() %>%
        filter(year == min(y()), import_value_usd > 0) %>%
        select(community_name, community_code, community_color, product_shortname_english, import_value_usd) %>%
        mutate(
          icon = glue::glue("img/hs/hs_{ community_code }.png")
        )
    })

    imports_title <- eventReactive(input$go, {
      switch(
        table_detailed(),
        "yrc-ca" = glue::glue("Imports of { r_name() } from the rest of the world in { y() }"),
        "yrpc-ca" = glue::glue("Imports of { r_name() } from { p_name() } in { y() }")
      )
    })

    imports_treemap_detailed <- eventReactive(input$go, {
      d <- data_detailed() %>%
        mutate(
          share = paste0(round(100 * import_value_usd / sum(import_value_usd), 2), "%"),
          community_name = paste0(community_name, "<br>", share)
        ) %>%
        rename(
          value = import_value_usd,
          name = community_name,
          color = community_color
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
            style = list(textOutline = FALSE)
          )
        ) %>%
        hc_title(text = imports_title()) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
    })

    # Output ------------------------------------------------------------------

    output$imports_treemap_detailed <- renderHighchart({
      imports_treemap_detailed()
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
