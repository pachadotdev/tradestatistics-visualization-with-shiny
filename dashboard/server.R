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
          exp_rank = dense_rank(desc(trade_value_usd_exp)),
          imp_rank = dense_rank(desc(trade_value_usd_imp))
        )
    })
    
    # Trade -------------------------------------------------------------------
    
    trade_table_aggregated <- reactive({
      data_aggregated() %>%
        select(year, trade_value_usd_exp, trade_value_usd_imp)
    })
    
    trade_subtitle <- reactive({
      "<hr/>Total Exports and Imports"
    })
    
    exports_value_min_year <- reactive({
      trade_table_aggregated() %>%
        filter(year == min(y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    exports_value_max_year <- reactive({
      trade_table_aggregated() %>%
        filter(year == max(y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    imports_value_min_year <- reactive({
      trade_table_aggregated() %>%
        filter(year == min(y())) %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })
    
    imports_value_max_year <- reactive({
      trade_table_aggregated() %>%
        filter(year == max(y())) %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })
    
    exports_value_min_year_2 <- reactive({
      show_dollars(exports_value_min_year())
    })
    
    exports_value_max_year_2 <- reactive({
      show_dollars(exports_value_max_year())
    })
    
    imports_value_min_year_2 <- reactive({
      show_dollars(imports_value_min_year())
    })
    
    imports_value_max_year_2 <- reactive({
      show_dollars(imports_value_max_year())
    })
    
    exports_growth <- reactive({
      growth_rate(
        exports_value_max_year(), exports_value_min_year(), y()
      )
    })
    
    exports_growth_2 <- reactive({
      show_percentage(exports_growth())
    })
    
    exports_growth_increase_decrease <- reactive({
      ifelse(exports_growth() >= 0, "increased", "decreased")
    })
    
    exports_growth_increase_decrease_2 <- reactive({
      ifelse(exports_growth() >= 0, "increase", "decrease")
    })
    
    imports_growth <- reactive({
      growth_rate(
        imports_value_max_year(), imports_value_min_year(), y()
      )
    })
    
    imports_growth_2 <- reactive({
      show_percentage(imports_growth())
    })
    
    imports_growth_increase_decrease <- reactive({
      ifelse(imports_growth() >= 0, "increased", "decreased")
    })
    
    imports_growth_increase_decrease_2 <- reactive({
      ifelse(imports_growth() >= 0, "increase", "decrease")
    })
    
    trade_rankings <- reactive({
      ots_create_tidy_data(
        years = c(min(y()), max(y())),
        reporters = r_iso(),
        partners = "all",
        table = "yrp",
        use_localhost = use_localhost
      ) %>%
        filter(partner_iso != "0-unspecified") %>% 
        mutate(
          trade_value_usd_bal = trade_value_usd_exp + trade_value_usd_imp
        ) %>% 
        group_by(year) %>% 
        mutate(
          bal_rank = dense_rank(desc(trade_value_usd_bal)),
          exp_share = trade_value_usd_exp / sum(trade_value_usd_exp),
          imp_share = trade_value_usd_imp / sum(trade_value_usd_imp)
        )
    })
    
    trade_rankings_no_min_year <- reactive({
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == min(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trade_rankings_no_max_year <- reactive({
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == max(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trade_rankings_remained <- reactive({
      ifelse(
        trade_rankings_no_min_year() == trade_rankings_no_max_year(),
        "remained", 
        "moved to"
      )
    })
    
    trade_rankings_exp_share_min_year <- reactive({
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == min(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trade_rankings_exp_share_min_year_2 <- reactive({
      show_percentage(trade_rankings_exp_share_min_year())
    })
    
    trade_rankings_exp_share_max_year <- reactive({
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == max(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trade_rankings_exp_share_max_year_2 <- reactive({
      show_percentage(trade_rankings_exp_share_max_year())
    })
    
    trade_rankings_imp_share_min_year <- reactive({
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == min(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trade_rankings_imp_share_min_year_2 <- reactive({
      show_percentage(trade_rankings_imp_share_min_year())
    })
    
    trade_rankings_imp_share_max_year <- reactive({
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == max(y()),
          reporter_iso == r_iso(),
          partner_iso == p_iso()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trade_rankings_imp_share_max_year_2 <- reactive({
      show_percentage(trade_rankings_imp_share_max_year())
    })
    
    trade_summary_text_exp <- reactive({
      switch(table_aggregated(),
             "yr" = glue::glue("The exports of { r_add_the() } { r_name() } to the World { exports_growth_increase_decrease() } from 
                               { exports_value_min_year_2() } in { min(y()) } to { exports_value_max_year_2() } in { max(y()) } 
                               (annualized { exports_growth_increase_decrease_2() } of { exports_growth_2() })."),
             
             "yrp" = glue::glue("The exports of { r_add_the() } { r_name() } to { p_add_the() } { p_name() } { exports_growth_increase_decrease() } from 
                                { exports_value_min_year_2() } in { min(y()) } 
                                to { exports_value_max_year_2() } in { max(y()) } (annualized { exports_growth_increase_decrease_2() } of 
                                { exports_growth_2() }). { p_add_the() } { p_name() } was the No. { trade_rankings_no_min_year() } trading partner of 
                                { r_add_the() } { r_name() } in { min(y()) } (represented { trade_rankings_exp_share_min_year_2() } of its exports), and 
                                then { trade_rankings_remained() } No. { trade_rankings_no_max_year() } in { max(y()) } (represented { trade_rankings_exp_share_max_year_2() } 
                                of its exports).")
      )
    })
    
    trade_summary_text_imp <- reactive({
      switch(table_aggregated(),
             "yr" = glue::glue("The imports of { r_add_the() } { r_name() } to the World { imports_growth_increase_decrease() } from 
                               { imports_value_min_year_2() } in { min(y()) } to { imports_value_max_year_2() } in { max(y()) } 
                               (annualized { imports_growth_increase_decrease_2() } of { imports_growth_2() })."),
             
             "yrp" = glue::glue("The imports of { r_add_the() } { r_name() } to { p_add_the() } { p_name() } { imports_growth_increase_decrease() } from 
                                { imports_value_min_year_2() } in { min(y()) } 
                                to { imports_value_max_year_2() } in { max(y()) } (annualized { imports_growth_increase_decrease_2() } of 
                                { imports_growth_2() }). { p_add_the() } { p_name() } was the No. { trade_rankings_no_min_year() } trading partner of 
                                { r_add_the() } { r_name() } in { min(y()) } (represented { trade_rankings_imp_share_min_year_2() } of its imports), and 
                                then { trade_rankings_remained() } No. { trade_rankings_no_max_year() } in { max(y()) } (represented { trade_rankings_imp_share_max_year_2() } 
                                of its imports).")
      )
    })

    trade_exchange_lines_title <- reactive({
      switch(table_aggregated(),
             "yr" = glue::glue("{ r_add_proper_the() } { r_name() } multilateral trade between { min(y()) } and { max(y()) }"),
             "yrp" = glue::glue("{ r_add_proper_the() } { r_name() } and { p_add_the() } { p_name() } exchange between { min(y()) } and { max(y()) }")
      )
    })
    
    trade_exchange_lines_aggregated <- reactive({
      d <- trade_table_aggregated() %>%
        gather(key, value, -year) %>%
        mutate(
          key = ifelse(key == "trade_value_usd_exp", "Exports", "Imports"),
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
        select(community_name, community_color, trade_value_usd_exp) %>%
        filter(trade_value_usd_exp > 0)
    })
    
    exports_subtitle <- reactive({
      "<hr/>Detailed Exports"
    })
    
    exports_title_min_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Exports of { r_add_the() } { r_name() } to the rest of the World in { min(y()) }"),
        "yrpc" = glue::glue("Exports of { r_add_the() } { r_name() } to { p_add_the() } { p_name() } in { min(y()) }")
      )
    })
    
    exports_treemap_detailed_min_year <- reactive({
      d <- exports_table_detailed_min_year() %>%
        group_by(community_name, community_color) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>%
        ungroup() %>% 
        mutate(
          share = trade_value_usd_exp / sum(trade_value_usd_exp),
          community_name = ifelse(share < 0.01, "Others <1% each", community_name),
          community_color = ifelse(share < 0.01, "#d3d3d3", community_color)
        ) %>%
        group_by(community_name, community_color) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>%
        ungroup() %>%
        mutate(
          share = paste0(round(100 * trade_value_usd_exp / sum(trade_value_usd_exp), 2), "%"),
          community_name = paste0(community_name, "<br>", share)
        ) %>%
        rename(
          value = trade_value_usd_exp,
          name = community_name,
          color = community_color
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
        select(community_name, community_color, trade_value_usd_exp) %>%
        filter(trade_value_usd_exp > 0)
    })
    
    exports_treemap_detailed_max_year <- reactive({
      d <- exports_table_detailed_max_year() %>%
        group_by(community_name, community_color) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>%
        ungroup() %>% 
        mutate(
          share = trade_value_usd_exp / sum(trade_value_usd_exp),
          community_name = ifelse(share < 0.01, "Others <1% each", community_name),
          community_color = ifelse(share < 0.01, "#d3d3d3", community_color)
        ) %>%
        group_by(community_name, community_color) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>%
        ungroup() %>%
        mutate(
          share = paste0(round(100 * trade_value_usd_exp / sum(trade_value_usd_exp), 2), "%"),
          community_name = paste0(community_name, "<br>", share)
        ) %>%
        rename(
          value = trade_value_usd_exp,
          name = community_name,
          color = community_color
        )
      
      highchart() %>%
        hc_chart(type = "treemap") %>%
        hc_xAxis(categories = d$name) %>%
        hc_add_series(d,
                      name = "Export Value USD", showInLegend = FALSE,
                      dataLabels = list(verticalAlign = "top", align = "left", style = list(fontSize = "12px", textOutline = FALSE))
        ) %>%
        hc_title(text = exports_title_max_year()) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
    })
    
    # Imports -----------------------------------------------------------------
    
    imports_subtitle <- reactive({
      "<hr/>Detailed Imports"
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
        select(community_name, community_color, trade_value_usd_imp) %>%
        filter(trade_value_usd_imp > 0)
    })
    
    imports_treemap_detailed_min_year <- reactive({
      d <- imports_table_detailed_min_year() %>%
        group_by(community_name, community_color) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>%
        ungroup() %>% 
        mutate(
          share = trade_value_usd_imp / sum(trade_value_usd_imp),
          community_name = ifelse(share < 0.01, "Others <1% each", community_name),
          community_color = ifelse(share < 0.01, "#d3d3d3", community_color)
        ) %>%
        group_by(community_name, community_color) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>%
        ungroup() %>%
        mutate(
          share = paste0(round(100 * trade_value_usd_imp / sum(trade_value_usd_imp), 2), "%"),
          community_name = paste0(community_name, "<br>", share)
        ) %>%
        rename(
          value = trade_value_usd_imp,
          name = community_name,
          color = community_color
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
    
    imports_title_min_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Imports of { r_add_the() } { r_name() } from the rest of the World in { min(y()) }"),
        "yrpc" = glue::glue("Imports of { r_add_the() } { r_name() } from { p_add_the() } { p_name() } in { min(y()) }")
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
        select(community_name, community_color, trade_value_usd_imp) %>%
        filter(trade_value_usd_imp > 0)
    })
    
    imports_treemap_detailed_max_year <- reactive({
      d <- imports_table_detailed_max_year() %>%
        group_by(community_name, community_color) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>%
        ungroup() %>% 
        mutate(
          share = trade_value_usd_imp / sum(trade_value_usd_imp),
          community_name = ifelse(share < 0.01, "Others <1% each", community_name),
          community_color = ifelse(share < 0.01, "#d3d3d3", community_color)
        ) %>%
        group_by(community_name, community_color) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>%
        ungroup() %>%
        mutate(
          share = paste0(round(100 * trade_value_usd_imp / sum(trade_value_usd_imp), 2), "%"),
          community_name = paste0(community_name, "<br>", share)
        ) %>%
        rename(
          value = trade_value_usd_imp,
          name = community_name,
          color = community_color
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
        hc_title(text = imports_title_max_year()) %>%
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
        "@misc{open_trade_statistics_2021,
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
    
    output$trade_summary_exp <- renderText(trade_summary_text_exp())
    output$trade_summary_imp <- renderText(trade_summary_text_imp())
    
    output$trade_title <- renderText(trade_title())
    
    output$trade_exchange_lines_aggregated <- renderHighchart({
      trade_exchange_lines_aggregated()
    })
    
    # Exports output ----------------------------------------------------------
    
    output$exports_subtitle <- renderText(exports_subtitle())
    
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
