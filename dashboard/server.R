## server.R ##

shinyServer(
  function(input, output, session) {
    # Input -------------------------------------------------------------------
    
    y <- reactive({
      y2 <- (min(input$y[1], input$y[2])):(max(input$y[1], input$y[2]))
      
      if (length(y2) > 5) {
        y2 <- ceiling(seq(min(y2), max(y2), length.out = 5))
      }
      
      return(y2)
    })
    
    # This section removed non-existing countries for the selected years,
    # unfortunately this resets the url and removes the selection
    # therefore I prefer to have a shareable url
    #
    # available_reporters_iso_in_range <- reactive({
    #   available_reporters_iso_min_year <- sprintf("%s/reporters?y=%s", base_url, min(y())) %>%
    #     jsonlite::fromJSON() %>%
    #     purrr::as_vector()
    #
    #   available_reporters_iso_max_year <- sprintf("%s/reporters?y=%s", base_url, max(y())) %>%
    #     jsonlite::fromJSON() %>%
    #     purrr::as_vector()
    #
    #   dplyr::intersect(available_reporters_iso_min_year, available_reporters_iso_max_year)
    # })
    #
    # output$r <- renderUI({
    #   selectInput(
    #     "r",
    #     "Reporter:",
    #     choices = available_reporters_iso[available_reporters_iso != "all" &
    #                                         available_reporters_iso %in% available_reporters_iso_in_range()],
    #     selected = sample(available_reporters_iso[available_reporters_iso != "all" &
    #                                                 available_reporters_iso %in% available_reporters_iso_in_range()],1),
    #     selectize = TRUE
    #   )
    # })
    #
    # output$p <- renderUI({
    #   selectInput(
    #     "p",
    #     "Partner:",
    #     choices = available_reporters_iso[available_reporters_iso %in% c("all", available_reporters_iso_in_range())],
    #     selected = "all",
    #     selectize = TRUE
    #   )
    # })
    
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
        "yr-sa"
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
    
    # Title -------------------------------------------------------------------
    
    r_add_the <- reactive({
      if (substr(r_name(), 1, 6) == "United" | substr(r_name(), 1, 3) == "USA") {
        "the"
      } else {
        ""
      }
    })
    
    r_add_proper_the <- reactive({
      if (substr(r_name(), 1, 6) == "United" | substr(r_name(), 1, 3) == "USA") {
        "The"
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
    
    p_add_proper_the <- reactive({
      if (substr(p_name(), 1, 6) == "United" | substr(p_name(), 1, 3) == "USA") {
        "The"
      } else {
        ""
      }
    })
    
    title <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("<h1>{ r_add_proper_the() } { r_name() } multilateral trade between { min(y()) } and { max(y()) }</h1>"),
        "yrpc" = glue::glue("<h1>{ r_add_proper_the() } { r_name() } and { p_add_the() } { p_name() } between { min(y()) } and { max(y()) }</h1>")
      )
    })
    
    title_legend <- reactive({
      "The information displayed here is based on <a href='https://comtrade.un.org/'>UN COMTRADE</a> datasets. Please read our <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a> for a full description
      of restrictions and applicable licenses."
    })
    
    # Format ------------------------------------------------------------------
    
    format <- reactive({
      input$format
    })
    
    # Data --------------------------------------------------------------------
    
    data_aggregated <- reactive({
      ots_create_tidy_data(
        years = y(),
        reporters = r_iso(),
        partners = p_iso(),
        table = table_aggregated(),
        use_localhost = use_localhost
      )
    })
    
    data_detailed <- reactive({
      ots_create_tidy_data(
        years = y(),
        reporters = r_iso(),
        partners = p_iso(),
        table = table_detailed(),
        use_localhost = use_localhost
      )
    })
    
    trade_rankings <- reactive({
      ots_create_tidy_data(
        years = c(min(y()), max(y())),
        reporters = r_iso(),
        partners = "all",
        table = "yrp",
        use_localhost = use_localhost
      ) %>%
        mutate(
          exp_rank = dense_rank(desc(export_value_usd)),
          imp_rank = dense_rank(desc(import_value_usd))
        )
    })
    
    top_imports_exports_bilateral <- reactive({
      ots_create_tidy_data(
        years = c(min(y()), max(y())),
        reporters = r_iso(),
        partners = p_iso(),
        table = "yrp",
        use_localhost = use_localhost
      ) %>%
        rename(
          export_value_usd_bilateral = export_value_usd,
          import_value_usd_bilateral = import_value_usd
        ) %>%
        inner_join(
          ots_create_tidy_data(
            years = c(min(y()), max(y())),
            reporters = r_iso(),
            table = "yr-sa",
            use_localhost = use_localhost
          )
        ) %>%
        mutate(
          top_export_to_total_exports = top_export_trade_value_usd / export_value_usd,
          top_import_to_total_imports = top_import_trade_value_usd / import_value_usd
        ) %>%
        left_join(
          ots_sections_shortnames %>%
            rename(
              top_export_section_code = section_code,
              top_export_section_name = section_shortname_english
            )
        ) %>%
        left_join(
          ots_sections_shortnames %>%
            rename(
              top_import_section_code = section_code,
              top_import_section_name = section_shortname_english
            )
        )
    })
    
    top_imports_exports_total <- reactive({
      ots_create_tidy_data(
        years = c(min(y()), max(y())),
        reporters = r_iso(),
        table = "yr-sa",
        use_localhost = use_localhost
      ) %>%
        mutate(
          top_export_to_total_exports = top_export_trade_value_usd / export_value_usd,
          top_import_to_total_imports = top_import_trade_value_usd / import_value_usd
        ) %>%
        left_join(
          ots_sections_shortnames %>%
            rename(
              top_export_section_code = section_code,
              top_export_section_name = section_shortname_english
            )
        ) %>%
        left_join(
          ots_sections_shortnames %>%
            rename(
              top_import_section_code = section_code,
              top_import_section_name = section_shortname_english
            )
        )
    })
    
    # Exports elements for texts ----------------------------------------------
    
    exports_value_paragraph_min_year <- reactive({
      trade_rankings() %>%
        filter(
          year == min(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(export_value_usd) %>%
        as.numeric()
    })
    
    exports_value_paragraph_min_year_2 <- reactive({
      show_dollars(exports_value_paragraph_min_year())
    })
    
    exports_value_paragraph_max_year <- reactive({
      trade_rankings() %>%
        filter(
          year == max(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(export_value_usd) %>%
        as.numeric()
    })
    
    exports_value_paragraph_max_year_2 <- reactive({
      show_dollars(exports_value_paragraph_max_year())
    })
    
    exports_total_value_paragraph_max_year <- reactive({
      ifelse(
        p_iso() == "all",
        top_imports_exports_total() %>%
          filter(year == max(y())) %>%
          select(export_value_usd) %>%
          as.numeric(),
        top_imports_exports_bilateral() %>%
          filter(year == max(y())) %>%
          select(export_value_usd) %>%
          as.numeric()
      )
    })
    
    exports_total_value_paragraph_max_year_2 <- reactive({
      show_dollars(exports_total_value_paragraph_max_year())
    })
    
    exports_bilateral_share_paragraph_max_year <- reactive({
      show_percentage(
        exports_value_paragraph_max_year() / exports_total_value_paragraph_max_year()
      )
    })
    
    exports_rank_paragraph_min_year <- reactive({
      trade_rankings() %>%
        filter(
          year == min(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(exp_rank) %>%
        as.character()
    })
    
    exports_total_value_paragraph_min_year <- reactive({
      ifelse(
        p_iso() == "all",
        top_imports_exports_total() %>%
          filter(year == min(y())) %>%
          select(export_value_usd) %>%
          as.numeric(),
        top_imports_exports_bilateral() %>%
          filter(year == min(y())) %>%
          select(export_value_usd) %>%
          as.numeric()
      )
    })
    
    exports_total_value_paragraph_min_year_2 <- reactive({
      show_dollars(exports_total_value_paragraph_min_year())
    })
    
    exports_bilateral_share_paragraph_min_year <- reactive({
      show_percentage(
        exports_value_paragraph_min_year() / exports_total_value_paragraph_min_year()
      )
    })
    
    exports_rank_paragraph_max_year <- reactive({
      trade_rankings() %>%
        filter(
          year == max(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(exp_rank) %>%
        as.character()
    })
    
    top_export_name_paragraph_min_year <- reactive({
      ifelse(
        p_iso() == "all",
        top_imports_exports_total() %>%
          filter(year == min(y())) %>%
          select(top_export_section_name) %>%
          as.character(),
        top_imports_exports_bilateral() %>%
          filter(year == min(y())) %>%
          select(top_export_section_name) %>%
          as.character()
      )
    })
    
    top_export_value_paragraph_min_year <- reactive({
      top_imports_exports_bilateral() %>%
        filter(year == min(y())) %>%
        select(top_export_trade_value_usd) %>%
        as.character()
    })
    
    top_export_value_paragraph_min_year_2 <- reactive({
      show_dollars(stop_export_value_paragraph_min_year())
    })
    
    top_export_bilateral_value_paragraph_min_year <- reactive({
      top_imports_exports_total() %>%
        filter(year == min(y())) %>%
        select(top_export_trade_value_usd) %>%
        as.numeric()
    })
    
    top_export_bilateral_value_paragraph_min_year_2 <- reactive({
      show_dollars(top_export_bilateral_value_paragraph_min_year())
    })
    
    top_export_bilateral_share_paragraph_min_year <- reactive({
      top_export_bilateral_value_paragraph_min_year() /
        sum(
          data_aggregated() %>% filter(year == min(y())) %>% select(export_value_usd) %>% pull(),
          na.rm = T
        )
    })
    
    top_export_bilateral_share_paragraph_min_year_2 <- reactive({
      show_percentage(top_export_bilateral_share_paragraph_min_year())
    })
    
    top_export_total_share_paragraph_min_year <- reactive({
      top_imports_exports_total() %>%
        filter(year == min(y())) %>%
        select(top_export_to_total_exports) %>%
        as.numeric()
    })
    
    top_export_total_share_paragraph_min_year_2 <- reactive({
      show_percentage(top_export_total_share_paragraph_min_year())
    })
    
    top_export_name_paragraph_max_year <- reactive({
      top_imports_exports_total() %>%
        filter(year == max(y())) %>%
        select(top_export_section_name) %>%
        as.character()
    })
    
    top_export_value_paragraph_max_year <- reactive({
      top_imports_exports_total() %>%
        filter(year == min(y())) %>%
        select(top_export_to_total_exports) %>%
        as.numeric()
    })
    
    top_export_value_paragraph_max_year_2 <- reactive({
      show_dollars(top_export_value_paragraph_max_year())
    })
    
    top_export_bilateral_value_paragraph_max_year <- reactive({
      top_imports_exports_total() %>%
        filter(year == max(y())) %>%
        select(top_export_trade_value_usd) %>%
        as.numeric()
    })
    
    top_export_bilateral_value_paragraph_max_year_2 <- reactive({
      show_dollars(top_export_bilateral_value_paragraph_max_year())
    })
    
    top_export_bilateral_share_paragraph_max_year <- reactive({
      top_export_bilateral_value_paragraph_max_year() /
        sum(
          data_aggregated() %>% filter(year == max(y())) %>% select(export_value_usd) %>% pull(),
          na.rm = T
        )
    })
    
    top_export_bilateral_share_paragraph_max_year_2 <- reactive({
      show_percentage(top_export_bilateral_share_paragraph_max_year())
    })
    
    top_export_total_share_paragraph_max_year <- reactive({
      top_imports_exports_total() %>%
        filter(year == max(y())) %>%
        select(top_export_to_total_exports) %>%
        as.numeric()
    })
    
    top_export_total_share_paragraph_max_year_2 <- reactive({
      show_percentage(top_export_total_share_paragraph_max_year())
    })
    
    exports_growth_paragraph <- reactive({
      ifelse(
        p_iso() == "all",
        growth_rate(
          exports_total_value_paragraph_max_year(), exports_total_value_paragraph_min_year(), y()
        ),
        growth_rate(
          exports_value_paragraph_max_year(), exports_value_paragraph_min_year(), y()
        )
      )
    })
    
    exports_growth_in_or_decreased_paragraph <- reactive({
      ifelse(exports_growth_paragraph() >= 0, "increased", "decreased")
    })
    
    exports_growth_paragraph_2 <- reactive({
      show_percentage(exports_growth_paragraph())
    })
    
    # Imports elements for texts ----------------------------------------------
    
    imports_value_paragraph_min_year <- reactive({
      trade_rankings() %>%
        filter(
          year == min(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(import_value_usd) %>%
        as.numeric()
    })
    
    imports_value_paragraph_min_year_2 <- reactive({
      show_dollars(imports_value_paragraph_min_year())
    })
    
    imports_value_paragraph_max_year <- reactive({
      trade_rankings() %>%
        filter(
          year == max(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(import_value_usd) %>%
        as.numeric()
    })
    
    imports_value_paragraph_max_year_2 <- reactive({
      show_dollars(imports_value_paragraph_max_year())
    })
    
    imports_total_value_paragraph_max_year <- reactive({
      ifelse(
        p_iso() == "all",
        top_imports_exports_total() %>%
          filter(year == max(y())) %>%
          select(import_value_usd) %>%
          as.numeric(),
        top_imports_exports_bilateral() %>%
          filter(year == max(y())) %>%
          select(import_value_usd) %>%
          as.numeric()
      )
    })
    
    imports_total_value_paragraph_max_year_2 <- reactive({
      show_dollars(imports_total_value_paragraph_max_year())
    })
    
    imports_bilateral_share_paragraph_max_year <- reactive({
      show_percentage(
        imports_value_paragraph_max_year() / imports_total_value_paragraph_max_year()
      )
    })
    
    imports_rank_paragraph_min_year <- reactive({
      trade_rankings() %>%
        filter(
          year == min(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(imp_rank) %>%
        as.character()
    })
    
    imports_total_value_paragraph_min_year <- reactive({
      ifelse(
        p_iso() == "all",
        top_imports_exports_total() %>%
          filter(year == min(y())) %>%
          select(import_value_usd) %>%
          as.numeric(),
        top_imports_exports_bilateral() %>%
          filter(year == min(y())) %>%
          select(import_value_usd) %>%
          as.numeric()
      )
    })
    
    imports_total_value_paragraph_min_year_2 <- reactive({
      show_dollars(imports_total_value_paragraph_min_year())
    })
    
    imports_bilateral_share_paragraph_min_year <- reactive({
      show_percentage(
        imports_value_paragraph_min_year() / imports_total_value_paragraph_min_year()
      )
    })
    
    imports_rank_paragraph_max_year <- reactive({
      trade_rankings() %>%
        filter(
          year == max(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(imp_rank) %>%
        as.character()
    })
    
    top_import_name_paragraph_min_year <- reactive({
      ifelse(
        p_iso() == "all",
        top_imports_exports_total() %>%
          filter(year == min(y())) %>%
          select(top_import_section_name) %>%
          as.character(),
        top_imports_exports_bilateral() %>%
          filter(year == min(y())) %>%
          select(top_import_section_name) %>%
          as.character()
      )
    })
    
    top_import_value_paragraph_min_year <- reactive({
      top_imports_exports_bilateral() %>%
        filter(year == min(y())) %>%
        select(top_import_trade_value_usd) %>%
        as.character()
    })
    
    top_import_value_paragraph_min_year_2 <- reactive({
      show_dollars(stop_import_value_paragraph_min_year())
    })
    
    top_import_bilateral_value_paragraph_min_year <- reactive({
      top_imports_exports_total() %>%
        filter(year == min(y())) %>%
        select(top_import_trade_value_usd) %>%
        as.numeric()
    })
    
    top_import_bilateral_value_paragraph_min_year_2 <- reactive({
      show_dollars(top_import_bilateral_value_paragraph_min_year())
    })
    
    top_import_bilateral_share_paragraph_min_year <- reactive({
      top_import_bilateral_value_paragraph_min_year() /
        sum(
          data_aggregated() %>% filter(year == min(y())) %>% select(import_value_usd) %>% pull(),
          na.rm = T
        )
    })
    
    top_import_bilateral_share_paragraph_min_year_2 <- reactive({
      show_percentage(top_import_bilateral_share_paragraph_min_year())
    })
    
    top_import_total_share_paragraph_min_year <- reactive({
      ifelse(
        p_iso() == "all",
        top_imports_exports_total() %>%
          filter(year == min(y())) %>%
          select(top_import_to_total_imports) %>%
          as.numeric(),
        top_imports_exports_bilateral() %>%
          filter(year == min(y())) %>%
          select(top_import_to_total_imports) %>%
          as.numeric()
      )
    })
    
    top_import_total_share_paragraph_min_year_2 <- reactive({
      show_percentage(top_import_total_share_paragraph_min_year())
    })
    
    top_import_name_paragraph_max_year <- reactive({
      top_imports_exports_total() %>%
        filter(year == max(y())) %>%
        select(top_import_section_name) %>%
        as.character()
    })
    
    top_import_value_paragraph_max_year <- reactive({
      top_imports_exports_bilateral() %>%
        filter(year == max(y())) %>%
        select(top_import_trade_value_usd) %>%
        as.character()
    })
    
    top_import_value_paragraph_max_year_2 <- reactive({
      show_dollars(top_import_value_paragraph_max_year())
    })
    
    top_import_bilateral_value_paragraph_max_year <- reactive({
      top_imports_exports_total() %>%
        filter(year == max(y())) %>%
        select(top_import_trade_value_usd) %>%
        as.numeric()
    })
    
    top_import_bilateral_value_paragraph_max_year_2 <- reactive({
      show_dollars(top_import_bilateral_value_paragraph_max_year())
    })
    
    top_import_bilateral_share_paragraph_max_year <- reactive({
      top_import_bilateral_value_paragraph_max_year() /
        sum(
          data_aggregated() %>% filter(year == max(y())) %>% select(import_value_usd) %>% pull(),
          na.rm = T
        )
    })
    
    top_import_bilateral_share_paragraph_max_year_2 <- reactive({
      show_percentage(top_import_bilateral_share_paragraph_max_year())
    })
    
    top_import_total_share_paragraph_max_year <- reactive({
      ifelse(
        p_iso() == "all",
        top_imports_exports_total() %>%
          filter(year == max(y())) %>%
          select(top_import_to_total_imports) %>%
          as.numeric(),
        top_imports_exports_bilateral() %>%
          filter(year == max(y())) %>%
          select(top_import_to_total_imports) %>%
          as.numeric()
      )
    })
    
    top_import_total_share_paragraph_max_year_2 <- reactive({
      show_percentage(top_import_total_share_paragraph_max_year())
    })
    
    imports_growth_paragraph <- reactive({
      ifelse(
        p_iso() == "all",
        growth_rate(
          imports_total_value_paragraph_max_year(), imports_total_value_paragraph_min_year(), y()
        ),
        growth_rate(
          imports_value_paragraph_max_year(), imports_value_paragraph_min_year(), y()
        )
      )
    })
    
    imports_growth_in_or_decreased_paragraph <- reactive({
      ifelse(imports_growth_paragraph() >= 0, "increased", "decreased")
    })
    
    imports_growth_paragraph_2 <- reactive({
      show_percentage(imports_growth_paragraph())
    })
    
    # Trade -------------------------------------------------------------------
    
    trade_table_aggregated <- reactive({
      data_aggregated() %>%
        select(year, export_value_usd, import_value_usd)
    })
    
    trade_subtitle <- reactive({
      "<hr/>Exports and Imports, grouped by year"
    })
    
    trade_paragraph_exports_1 <- reactive({
      switch(table_aggregated(),
             "yr-sa" = glue::glue("Exports: From { exports_total_value_paragraph_min_year_2() } in { min(y()) } to { exports_total_value_paragraph_max_year_2() } in { max(y()) }"),
             
             "yrp" = glue::glue("Exports: From { exports_value_paragraph_min_year_2() } in { min(y()) } to { exports_value_paragraph_max_year_2() } in { max(y()) }")
      )
    })
    
    trade_paragraph_exports_2 <- reactive({
      switch(table_aggregated(),
             "yr-sa" = glue::glue("The exports of { r_add_the() } { r_name() } to the World { exports_growth_in_or_decreased_paragraph() } at
                               an annualized rate of { exports_growth_paragraph_2() }."),
             
             "yrp" = glue::glue("The exports of { r_add_the() } { r_name() } to { p_add_the() } { p_name() } { exports_growth_in_or_decreased_paragraph() } at
                                an annualized rate of { exports_growth_paragraph_2() }. { p_add_proper_the() } { p_name() } moved from No. { exports_rank_paragraph_min_year() } 
                                exports destination to No. { exports_rank_paragraph_max_year() }, representing { exports_bilateral_share_paragraph_min_year() }
                                and { exports_bilateral_share_paragraph_max_year() } of the exports of { r_add_the() } { r_name() }.")
      )
    })
    
    trade_paragraph_imports_1 <- reactive({
      switch(table_aggregated(),
             "yr-sa" = glue::glue("Imports: From { imports_total_value_paragraph_min_year_2() } in { min(y()) } to { imports_total_value_paragraph_max_year_2() } in { max(y()) }"),
             
             "yrp" = glue::glue("Imports: From { imports_value_paragraph_min_year_2() } in { min(y()) } to { imports_value_paragraph_max_year_2() } in { max(y()) }")
      )
    })
    
    trade_paragraph_imports_2 <- reactive({
      switch(table_aggregated(),
             "yr-sa" = glue::glue("The imports of { r_add_the() } { r_name() } from the World { imports_growth_in_or_decreased_paragraph() } at
                               an annualized rate of { imports_growth_paragraph_2() }."),
             
             "yrp" = glue::glue("The imports of { r_add_the() } { r_name() } from { p_add_the() } { p_name() } { imports_growth_in_or_decreased_paragraph() } at
                                an annualized rate of { imports_growth_paragraph_2() }. { p_add_proper_the() } { p_name() } moved from No. { imports_rank_paragraph_min_year() } 
                                imports destination to No. { imports_rank_paragraph_max_year() }, representing { imports_bilateral_share_paragraph_min_year() }
                                and { imports_bilateral_share_paragraph_max_year() } of the imports of { r_add_the() } { r_name() }.")
      )
    })
    
    trade_exchange_lines_title <- reactive({
      switch(table_aggregated(),
             "yr-sa" = glue::glue("{ r_add_proper_the() } { r_name() } multilateral trade between { min(y()) } and { max(y()) }"),
             "yrp" = glue::glue("{ r_add_proper_the() } { r_name() } and { p_add_the() } { p_name() } exchange between { min(y()) } and { max(y()) }")
      )
    })
    
    trade_exchange_lines_aggregated <- reactive({
      d <- trade_table_aggregated() %>%
        gather(key, value, -year) %>%
        mutate(
          key = ifelse(key == "export_value_usd", "Exports", "Imports"),
          year = as.Date(paste(year, "01-01", sep = "-"))
        ) %>%
        rename(
          `Trade Value` = value,
          `Year` = year,
          group = key
        )
      
      hchart(d, "line", hcaes(x = `Year`, y = `Trade Value`, group = group)) %>%
        hc_colors(c("#4d6fd0", "#bf3251")) %>%
        hc_title(text = trade_exchange_lines_title()) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
    })
    
    # Exports -----------------------------------------------------------------
    
    exports_table_detailed_min_year <- reactive({
      data_detailed() %>%
        filter(year == min(y())) %>%
        select(section_shortname_english, section_color, export_value_usd) %>%
        filter(export_value_usd > 0)
    })
    
    exports_subtitle <- reactive({
      "<hr/>Exports, grouped by section"
    })
    
    exports_paragraph_min_year <- reactive({
      switch(table_aggregated(),
             "yr-sa" = glue::glue("Represented { top_export_total_share_paragraph_min_year_2() } of the total exports of { r_add_the() } { r_name() } to the World."),
             
             "yrp" = glue::glue("Represented { top_export_total_share_paragraph_min_year_2() } of the total exports of { r_add_the() } { r_name() } to the World,
                                and { top_export_bilateral_share_paragraph_min_year_2() } of the total exports of { r_add_the() } { r_name() } to { p_add_the() } { p_name() }.")
      )
    })
    
    exports_paragraph_max_year <- reactive({
      switch(table_aggregated(),
             "yr-sa" = glue::glue("Represented { top_export_total_share_paragraph_max_year_2() } of the total exports of { r_add_the() } { r_name() } to the World."),
             
             "yrp" = glue::glue("Represented { top_export_total_share_paragraph_max_year_2() } of the total exports of { r_add_the() } { r_name() } to the World,
                                and { top_export_bilateral_share_paragraph_max_year_2() } of the total exports of { r_add_the() } { r_name() } to { p_add_the() } { p_name() }.")
      )
    })
    
    exports_title_min_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Exports of { r_add_the() } { r_name() } to the rest of the World in { min(y()) }"),
        "yrpc" = glue::glue("Exports of { r_add_the() } { r_name() } to { p_add_the() } { p_name() } in { min(y()) }")
      )
    })
    
    exports_treemap_detailed_min_year <- reactive({
      # d <- exports_table_detailed_min_year() %>%
      #   mutate(share = export_value_usd / sum(export_value_usd))
      # 
      # d2 <- d %>%
      #   group_by(section_shortname_english) %>%
      #   summarise(export_value_usd0 = sum(export_value_usd, na.rm = T)) %>%
      #   ungroup() %>%
      #   mutate(share0 = export_value_usd0 / sum(export_value_usd0))
      # 
      # d <- d %>%
      #   left_join(d2) %>%
      #   mutate(
      #     section_shortname_english = paste0(section_shortname_english, "<br>",
      #                             paste0(round(100 * share0, 2), "%")),
      #     section_shortname_english = ifelse(share0 < 0.01, "Others <1% each", section_shortname_english),
      #     section_color = ifelse(share0 < 0.01, "#d3d3d3", section_color)
      #   ) %>%
      # 
      #   group_by(group_name, section_shortname_english, section_color) %>%
      #   summarise(export_value_usd = sum(export_value_usd, na.rm = T)) %>%
      #   ungroup() %>%
      # 
      #   mutate(
      #     share = paste0(round(100 * export_value_usd / sum(export_value_usd), 2), "%"),
      #     group_name = paste0(group_name, "<br>", share)
      #   ) %>%
      # 
      #   rename(
      #     index1 = section_shortname_english,
      #     index2 = group_name,
      #     size = export_value_usd
      #   )
      # 
      # tm <- d %>%
      #   treemap::treemap(
      #     index = c("index1", "index2"),
      #     vSize = "size",
      #     vColor = "index1",
      #     draw = F
      #   )
      # 
      # tm$tm <- tm$tm %>%
      #   tbl_df() %>%
      #   left_join(d %>% select(index1, section_color) %>% distinct(), by = "index1") %>%
      #   mutate(color = section_color) %>%
      #   select(-section_color)
      # 
      # highchart() %>%
      #   hc_add_series_treemap(tm,
      #                         allowDrillToNode = TRUE,
      #                         layoutAlgorithm = "squarified",
      #                         dataLabels = list(enabled = FALSE),
      #                         levelIsConstant = FALSE,
      #                         levels = list(
      #                           list(
      #                             level = 1,
      #                             dataLabels = list(
      #                               enabled = TRUE,
      #                               verticalAlign = "top",
      #                               align = "left",
      #                               style = list(fontSize = "12px", textOutline = FALSE)
      #                             )
      #                           )
      #                         )) %>%
      #   hc_title(text = exports_title_min_year()) %>%
      #   hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
      
      d <- exports_table_detailed_min_year() %>%
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
          name = "Export Value USD", showInLegend = FALSE,
          dataLabels = list(verticalAlign = "top", align = "left", style = list(fontSize = "12px", textOutline = FALSE))
        ) %>%
        hc_title(text = exports_title_min_year()) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
    })
    
    exports_title_max_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Exports of { r_add_the() } { r_name() } to the rest of the World { max(y()) }"),
        "yrpc" = glue::glue("Exports of { r_add_the() } { r_name() } to { p_add_the() } { p_name() } in { max(y()) }")
      )
    })
    
    exports_table_detailed_max_year <- reactive({
      data_detailed() %>%
        filter(year == max(y())) %>%
        select(section_shortname_english, section_color, export_value_usd) %>%
        filter(export_value_usd > 0)
    })
    
    exports_treemap_detailed_max_year <- reactive({
      d <- exports_table_detailed_max_year() %>%
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
                      name = "Export Value USD", showInLegend = FALSE,
                      dataLabels = list(verticalAlign = "top", align = "left", style = list(fontSize = "12px", textOutline = FALSE))
        ) %>%
        hc_title(text = exports_title_min_year()) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
    })
    
    # Imports -----------------------------------------------------------------
    
    imports_subtitle <- reactive({
      "<hr/>Imports, grouped by section"
    })
    
    imports_paragraph_min_year <- reactive({
      switch(table_aggregated(),
             "yr-sa" = glue::glue("Represented { top_import_total_share_paragraph_min_year_2() } of the total imports of { r_add_the() } { r_name() } from the World."),
             
             "yrp" = glue::glue("Represented { top_import_total_share_paragraph_min_year_2() } of the total imports of { r_add_the() } { r_name() } from the World,
                                and { top_import_bilateral_share_paragraph_min_year_2() } of the total imports of { r_add_the() } { r_name() } from { p_add_the() } { p_name() }.")
      )
    })
    
    imports_paragraph_max_year <- reactive({
      switch(table_aggregated(),
             "yr-sa" = glue::glue("Represented { top_import_total_share_paragraph_max_year_2() } of the total imports of { r_add_the() } { r_name() } from the World."),
             
             "yrp" = glue::glue("Represented { top_import_total_share_paragraph_max_year_2() } of the total imports of { r_add_the() } { r_name() } to the World,
                                and { top_import_bilateral_share_paragraph_max_year_2() } of the total imports of { r_add_the() } { r_name() } from { p_add_the() } { p_name() }.")
      )
    })
    
    imports_title_min_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Imports of { r_add_the() } { r_name() } from the rest of the World in { min(y()) }"),
        "yrpc" = glue::glue("Imports of { r_add_the() } { r_name() } from { p_add_the() } { p_name() } in { min(y()) }")
      )
    })
    
    imports_table_detailed_min_year <- reactive({
      data_detailed() %>%
        filter(year == min(y())) %>%
        select(section_shortname_english, section_color, import_value_usd) %>%
        filter(import_value_usd > 0)
    })
    
    imports_treemap_detailed_min_year <- reactive({
      d <- imports_table_detailed_min_year() %>%
        mutate(
          share = import_value_usd / sum(import_value_usd)
          # section_shortname_english = ifelse(share < 0.01, "Others <1% each", section_shortname_english),
          # section_color = ifelse(share < 0.01, "#d3d3d3", section_color)
        ) %>%
        group_by(section_shortname_english, section_color) %>%
        summarise(import_value_usd = sum(import_value_usd, na.rm = T)) %>%
        ungroup() %>%
        mutate(
          share = paste0(round(100 * import_value_usd / sum(import_value_usd), 2), "%"),
          section_shortname_english = paste0(section_shortname_english, "<br>", share)
        ) %>%
        rename(
          value = import_value_usd,
          name = section_shortname_english,
          color = section_color
        )
      
      highchart() %>%
        hc_chart(type = "treemap") %>%
        hc_xAxis(categories = d$name) %>%
        hc_add_series(d,
                      name = "import Value USD",
                      showInLegend = FALSE,
                      dataLabels = list(
                        verticalAlign = "top",
                        align = "left",
                        style = list(fontSize = "12px", textOutline = FALSE)
                      )
        ) %>%
        hc_title(text = imports_title_min_year()) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
    })
    
    imports_title_max_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Imports of { r_add_the() } { r_name() } from the rest of the World in { max(y()) }"),
        "yrpc" = glue::glue("Imports of { r_add_the() } { r_name() } from { p_add_the() } { p_name() } in { max(y()) }")
      )
    })
    
    imports_title_max_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Imports of { r_add_the() } { r_name() } from the rest of the World in { max(y()) }"),
        "yrpc" = glue::glue("Imports of { r_add_the() } { r_name() } from { p_add_the() } { p_name() } in { max(y()) }")
      )
    })
    
    imports_table_detailed_max_year <- reactive({
      data_detailed() %>%
        filter(year == max(y())) %>%
        select(section_shortname_english, section_color, import_value_usd) %>%
        filter(import_value_usd > 0)
    })
    
    imports_treemap_detailed_max_year <- reactive({
      d <- imports_table_detailed_max_year() %>%
        mutate(
          share = import_value_usd / sum(import_value_usd)
          # section_shortname_english = ifelse(share < 0.01, "Others <1% each", section_shortname_english),
          # section_color = ifelse(share < 0.01, "#d3d3d3", section_color)
        ) %>%
        group_by(section_shortname_english, section_color) %>%
        summarise(import_value_usd = sum(import_value_usd, na.rm = T)) %>%
        ungroup() %>%
        mutate(
          share = paste0(round(100 * import_value_usd / sum(import_value_usd), 2), "%"),
          section_shortname_english = paste0(section_shortname_english, "<br>", share)
        ) %>%
        rename(
          value = import_value_usd,
          name = section_shortname_english,
          color = section_color
        )
      
      highchart() %>%
        hc_chart(type = "treemap") %>%
        hc_xAxis(categories = d$name) %>%
        hc_add_series(d,
                      name = "import Value USD",
                      showInLegend = FALSE,
                      dataLabels = list(
                        verticalAlign = "top",
                        align = "left",
                        style = list(fontSize = "12px", textOutline = FALSE)
                      )
        ) %>%
        hc_title(text = imports_title_min_year()) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
    })
    
    # URL ---------------------------------------------------------------------
    
    url_trade <- reactive({
      glue::glue(
        "{ site_url }/embed-trade/?_inputs_&y1={ min(y()) }&y2={ max(y()) }&r=%22{ r_iso() }%22&p=%22{ p_iso() }%22"
      )
    })
    
    url_exports_min_year <- reactive({
      glue::glue(
        "{ site_url }/embed-exports/?_inputs_&y={ min(y()) }&r=%22{ r_iso() }%22&p=%22{ p_iso() }%22"
      )
    })
    
    url_exports_max_year <- reactive({
      glue::glue(
        "{ site_url }/embed-exports/?_inputs_&y={ max(y()) }&r=%22{ r_iso() }%22&p=%22{ p_iso() }%22"
      )
    })
    
    url_imports_min_year <- reactive({
      glue::glue(
        "{ site_url }/embed-imports/?_inputs_&y={ min(y()) }&r=%22{ r_iso() }%22&p=%22{ p_iso() }%22"
      )
    })
    
    url_imports_max_year <- reactive({
      glue::glue(
        "{ site_url }/embed-imports/?_inputs_&y={ max(y()) }&r=%22{ r_iso() }%22&p=%22{ p_iso() }%22"
      )
    })
    
    share_download_cite_subtitle <- reactive({
      "<hr/>Share, download or cite"
    })
    
    url <- reactive({
      glue::glue(
        "{ site_url }/dashboard/?_inputs_&y=[{ min(y()) },{ max(y()) }]&r=%22{ r_iso() }%22&p=%22{ p_iso() }%22"
      )
    })
    
    # Cite --------------------------------------------------------------------
    
    cite_subtitle <- reactive({
      "Chicago citation"
    })
    
    cite <- reactive({
      sprintf(
        "Open Trade Statistics. \"OTS BETA DASHBOARD\". <i>Open Trade Statistics</i>. Accessed %s %s, %s. %s/",
        months(Sys.Date()),
        lubridate::day(Sys.Date()),
        lubridate::year(Sys.Date()),
        site_url
      )
    })
    
    cite_bibtex_subtitle <- reactive({
      "BibTeX entry"
    })
    
    cite_bibtex <- reactive({
      sprintf(
        "@misc{open_trade_statistics_2019,
              title = {OTS BETA DASHBOARD},
              url = {%s/},
              author = {Vargas, Mauricio},
              doi = {10.5281/zenodo.3738793},
              publisher = {Open Trade Statistics},
              year = {2019},
              month = {Apr},
              note = {Accessed: %s %s, %s}
    }",
        site_url,
        months(Sys.Date()),
        lubridate::day(Sys.Date()),
        lubridate::year(Sys.Date())
      )
    })
    
    # Title output ------------------------------------------------------------
    
    output$title <- renderText({
      title()
    })
    
    output$title_legend <- renderText({
      title_legend()
    })
    
    # Trade output ------------------------------------------------------------
    
    output$trade_subtitle <- renderText(trade_subtitle())
    
    output$trade_box_exports <- renderValueBox({
      customValueBox(
        h4(trade_paragraph_exports_1()),
        trade_paragraph_exports_2(),
        icon = icon("chart-line"),
        color = "valueboxgreen1"
      )
    })
    
    output$trade_box_imports <- renderValueBox({
      customValueBox(
        h4(trade_paragraph_imports_1()),
        trade_paragraph_imports_2(),
        icon = icon("chart-line"),
        color = "valueboxred1"
      )
    })
    
    output$trade_title <- renderText(trade_title())
    
    output$trade_exchange_lines_aggregated <- renderHighchart({
      trade_exchange_lines_aggregated()
    })
    
    # Exports output ----------------------------------------------------------
    
    output$exports_subtitle <- renderText(exports_subtitle())
    
    output$exports_box_min_year <- renderValueBox({
      customValueBox(
        h4(glue::glue("Most exported section in { min(y()) }: { top_export_name_paragraph_min_year() } ({ top_export_bilateral_value_paragraph_min_year_2() })")),
        exports_paragraph_min_year(),
        icon = icon("chart-line"),
        color = "valueboxgreen1"
      )
    })
    
    output$exports_box_max_year <- renderValueBox({
      customValueBox(
        h4(glue::glue("Most exported section in { max(y()) }: { top_export_name_paragraph_max_year() } ({ top_export_bilateral_value_paragraph_max_year_2() })")),
        exports_paragraph_max_year(),
        icon = icon("chart-line"),
        color = "valueboxgreen2"
      )
    })
    
    output$exports_title_min_year <- renderText(exports_title_min_year())
    
    output$exports_treemap_detailed_min_year <- renderHighchart({
      exports_treemap_detailed_min_year()
    })
    
    output$exports_title_max_year <- renderText(exports_title_max_year())
    
    output$exports_treemap_detailed_max_year <- renderHighchart({
      exports_treemap_detailed_max_year()
    })
    
    # Imports output ----------------------------------------------------------
    
    output$imports_subtitle <- renderText(imports_subtitle())
    
    output$imports_box_min_year <- renderValueBox({
      customValueBox(
        h4(glue::glue("Most imported section in { min(y()) }: { top_import_name_paragraph_min_year() } ({ top_import_bilateral_value_paragraph_min_year_2() })")),
        imports_paragraph_min_year(),
        icon = icon("chart-line"),
        color = "valueboxred1"
      )
    })
    
    output$imports_box_max_year <- renderValueBox({
      customValueBox(
        h4(glue::glue("Most imported section in { max(y()) }: { top_import_name_paragraph_max_year() } ({ top_import_bilateral_value_paragraph_max_year_2() })")),
        imports_paragraph_max_year(),
        icon = icon("chart-line"),
        color = "valueboxred2"
      )
    })
    
    output$imports_title_min_year <- renderText(imports_title_min_year())
    
    output$imports_treemap_detailed_min_year <- renderHighchart({
      imports_treemap_detailed_min_year()
    })
    
    output$imports_title_max_year <- renderText(imports_title_max_year())
    
    output$imports_treemap_detailed_max_year <- renderHighchart({
      imports_treemap_detailed_max_year()
    })
    
    # URL and downloads output ------------------------------------------------
    
    output$url_trade <- reactive({
      glue::glue(
        "
        <div class='input-group'>
        <input type='text' class='form-control' value='{ url_trade() }' id='UrlTrade'>
        <span class='input-group-btn'>
        <button class='btn btn-default' type='button' onclick='copyUrlTrade()'><i class='fas fa-copy'></i> Copy</button>
        </span>
        </div>
        "
      )
    })
    
    output$url_exports_min_year <- reactive({
      glue::glue(
        "
        <div class='input-group'>
        <input type='text' class='form-control' value='{ url_exports_min_year() }' id='UrlExportsMinYear'>
        <span class='input-group-btn'>
        <button class='btn btn-default' type='button' onclick='copyUrlExportsMinYear()'><i class='fas fa-copy'></i> Copy</button>
        </span>
        </div>
        "
      )
    })
    
    output$url_exports_max_year <- reactive({
      glue::glue(
        "
        <div class='input-group'>
        <input type='text' class='form-control' value='{ url_exports_max_year() }' id='UrlExportsMaxYear'>
        <span class='input-group-btn'>
        <button class='btn btn-default' type='button' onclick='copyUrlExportsMaxYear()'><i class='fas fa-copy'></i> Copy</button>
        </span>
        </div>
        "
      )
    })
    
    output$url_imports_min_year <- reactive({
      glue::glue(
        "
        <div class='input-group'>
        <input type='text' class='form-control' value='{ url_imports_min_year() }' id='UrlImportsMinYear'>
        <span class='input-group-btn'>
        <button class='btn btn-default' type='button' onclick='copyUrlImportsMinYear()'><i class='fas fa-copy'></i> Copy</button>
        </span>
        </div>
        "
      )
    })
    
    output$url_imports_max_year <- reactive({
      glue::glue(
        "
        <div class='input-group'>
        <input type='text' class='form-control' value='{ url_imports_max_year() }' id='UrlImportsMaxYear'>
        <span class='input-group-btn'>
        <button class='btn btn-default' type='button' onclick='copyUrlImportsMaxYear()'><i class='fas fa-copy'></i> Copy</button>
        </span>
        </div>
        "
      )
    })
    
    output$share_download_cite_subtitle <- renderText({
      share_download_cite_subtitle()
    })
    
    output$url <- reactive({
      glue::glue(
        "
        <p><b>Link sharing:</b></p>
        <div class='input-group'>
        <input type='text' class='form-control' value='{ url() }' id='Url'>
        <span class='input-group-btn'>
        <button class='btn btn-default' type='button' onclick='copyUrl()'><i class='fas fa-copy'></i> Copy</button>
        </span>
        </div>
        "
      )
    })
    
    output$download_aggregated <- downloadHandler(
      filename = function() {
        glue::glue("{ table_aggregated() }_{ r_iso() }_{ p_iso() }_{ min(y()) }_{ max(y()) }.{ format() }")
      },
      content = function(filename) {
        if (format() == "csv") {
          data.table::fwrite(data_aggregated(), filename, sep = ",")
        }
        if (format() == "tsv") {
          data.table::fwrite(data_aggregated(), filename, sep = "\t")
        }
        if (format() == "json") {
          jsonlite::write_json(data_aggregated(), filename, pretty = F)
        }
        if (format() %in% c("xlsx", "sav", "dta")) {
          rio::export(data_aggregated(), filename)
        }
      },
      contentType = "application/zip"
    )
    
    output$download_detailed <- downloadHandler(
      filename = function() {
        glue::glue("{ table_detailed() }_{ r_iso() }_{ p_iso() }_{ min(y()) }_{ max(y()) }.{ format() }")
      },
      content = function(filename) {
        if (format() == "csv") {
          data.table::fwrite(data_detailed(), filename, sep = ",")
        }
        if (format() == "tsv") {
          data.table::fwrite(data_detailed(), filename, sep = "\t")
        }
        if (format() == "json") {
          jsonlite::write_json(data_detailed(), filename, pretty = F)
        }
        if (format() %in% c("xlsx", "sav", "dta")) {
          rio::export(data_detailed(), filename)
        }
      },
      contentType = "application/zip"
    )
    
    # Cite output -------------------------------------------------------------
    
    output$cite_subtitle <- renderText({
      cite_subtitle()
    })
    
    output$cite <- renderText({
      cite()
    })
    
    output$cite_bibtex_subtitle <- renderText({
      cite_bibtex_subtitle()
    })
    
    output$cite_bibtex <- renderText({
      cite_bibtex()
    })
    
    # Bookmarking -------------------------------------------------------------
    
    observe({
      # Trigger this observer every time an input changes
      # strip shiny related URL parameters
      reactiveValuesToList(input)
      setBookmarkExclude(c(
        "r-selectized", "p-selectized", "sidebarCollapsed", "sidebarItemExpanded", "Url", "UrlTrade",
        "UrlExportsMaxYear", "UrlExportsMinYear", "UrlImportsMaxYear", "UrlImportsMinYear", "format", "format-selectized"
      ))
      session$doBookmark()
    })
    
    onBookmarked(function(url) {
      updateQueryString(url)
    })
  }
)
