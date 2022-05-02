## server.R ##

shinyServer(
  function(input, output, session) {
    # User inputs ----
    
    # updateSelectizeInput(session, 'mod_cpf', choices = available_commodities, server = TRUE)
    
    input_country_profile_y <- reactive({
      y <- (min(input$cp_y[1], input$cp_y[2])):(max(input$cp_y[1], input$cp_y[2]))
      y <- seq(min(y), max(y), by = ifelse(max(y) - min(y) >= 10, 2, 1))
      return(y)
    })
    
    input_model_y <- reactive({
      y2 <- (min(input$mod_y[1], input$mod_y[2])):(max(input$mod_y[1], input$mod_y[2]))
      y2 <- seq(min(y2), max(y2), by = input$mod_y_sep)
      return(y2)
    })
    
    input_country_profile_reporter_iso <- reactive({ input$cp_r })
    input_model_reporter_iso <- reactive({ input$mod_r })
    
    reporter_name <- eventReactive(input$cp_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == input_country_profile_reporter_iso()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    input_country_profile_partner_iso <- reactive({ input$cp_p })
    input_model_partner_iso <- reactive({ input$mod_p })
    
    input_country_profile_convert_dollars <- reactive({ input$cp_a })
    input_model_convert_dollars <- reactive({ input$mod_a })
    
    partner_name <- eventReactive(input$cp_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == input_country_profile_partner_iso()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    input_model_product_filter <- reactive({ input$mod_pf })
    # input_model_custom_product_filter <- reactive({ input$mod_cpf })
    input_model_type <- reactive({ input$mod_t })
    input_model_dist <- reactive({ input$mod_d })
    input_model_bin <- reactive({ input$mod_b })
    input_model_ctn <- reactive({ input$mod_ct })
    input_model_cluster <- reactive({ input$mod_cl })
    input_model_custom_subset <- reactive({ input$mod_s })
    
    input_country_profile_format <- reactive({ input$cp_f })
    input_model_format <- reactive({ input$mod_f })
    
    table_aggregated <- eventReactive(input$cp_go, {
      ifelse(input_country_profile_partner_iso() == "all", "yr", "yrp")
    })
    
    table_detailed_country_profile <- eventReactive(input$cp_go, {
      ifelse(input_country_profile_partner_iso() == "all", "yrc", "yrpc")
    })
    
    table_detailed_model <- eventReactive(input$mod_go, {
      ifelse(input_model_partner_iso() == "all", "yrc", "yrpc")
    })
    
    # Titles ----
    
    reporter_add_the <- eventReactive(input$cp_go, {
      if (substr(reporter_name(), 1, 6) == "United" | 
          substr(reporter_name(), 1, 3) == "USA" |
          substr(reporter_name(), 1, 7) == "Russian") {
        "the"
      } else {
        ""
      }
    })
    
    reporter_add_proper_the <- eventReactive(input$cp_go, {
      if (substr(reporter_name(), 1, 6) == "United" | 
          substr(reporter_name(), 1, 3) == "USA" |
          substr(reporter_name(), 1, 7) == "Russian") {
        "The"
      } else {
        ""
      }
    })
    
    partner_add_the <- eventReactive(input$cp_go, {
      if (substr(partner_name(), 1, 6) == "United" | 
          substr(partner_name(), 1, 3) == "USA" |
          substr(partner_name(), 1, 7) == "Russian") {
        "the"
      } else {
        ""
      }
    })
    
    title_country_profile <- eventReactive(input$cp_go, {
      switch(
        table_detailed_country_profile(),
        "yrc" = glue("{ reporter_add_proper_the() } { reporter_name() } multilateral trade between { min(input_country_profile_y()) } and { max(input_country_profile_y()) }"),
        "yrpc" = glue("{ reporter_add_proper_the() } { reporter_name() } and { partner_add_the() } { partner_name() } between { min(input_country_profile_y()) } and { max(input_country_profile_y()) }"),
      )
    })
    
    # Country profile ----
    
    ## Data ----
    
    data_aggregated <- eventReactive(input$cp_go, {
      d <- tbl(con, table_aggregated())
          
      if (input_country_profile_partner_iso() == "all") {
        d <- d %>% 
          filter(
            year %in% !!input_country_profile_y() &
              reporter_iso == !!input_country_profile_reporter_iso()
          )
      } else {
        d <- d %>% 
          filter(
            year %in% !!input_country_profile_y() &
              reporter_iso == !!input_country_profile_reporter_iso() &
              partner_iso == !!input_country_profile_partner_iso()
          )
      }
      
      d <- d %>% collect()

      if (input_country_profile_convert_dollars() != "No conversion") {
        d <- tradestatistics::ots_gdp_deflator_adjustment(
          data.table::as.data.table(d), as.integer(input_country_profile_convert_dollars())
        )
      }
      
      return(d)
    })
    
    data_detailed <- eventReactive(input$cp_go, {
      d <- tbl(con, table_detailed_country_profile())
          
      if (input_country_profile_partner_iso() == "all") {
        d <- d %>% 
          filter(
            year %in% !!input_country_profile_y() &
              reporter_iso == !!input_country_profile_reporter_iso()
          )
      } else {
        d <- d %>% 
          filter(
            year %in% !!input_country_profile_y() &
              reporter_iso == !!input_country_profile_reporter_iso() &
              partner_iso == !!input_country_profile_partner_iso()
          )
      }
          
      d <- d %>% collect()
      
      if (input_country_profile_convert_dollars() != "No conversion") {
        d <- tradestatistics::ots_gdp_deflator_adjustment(
          data.table::as.data.table(d),
          as.integer(input_country_profile_convert_dollars())
        )
      }
      
      return(d)
    })
    
    ## Trade ----
    
    ### Tables ----
    
    trade_table_aggregated <- eventReactive(input$cp_go, {
      data_aggregated() %>%
        select(year, trade_value_usd_exp, trade_value_usd_imp)
    })
    
    exports_value_min_year <- eventReactive(input$cp_go, {
      trade_table_aggregated() %>%
        filter(year == min(input_country_profile_y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    exports_value_max_year <- eventReactive(input$cp_go, {
      trade_table_aggregated() %>%
        filter(year == max(input_country_profile_y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    imports_value_min_year <- eventReactive(input$cp_go, {
      trade_table_aggregated() %>%
        filter(year == min(input_country_profile_y())) %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })
    
    imports_value_max_year <- eventReactive(input$cp_go, {
      trade_table_aggregated() %>%
        filter(year == max(input_country_profile_y())) %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })
    
    exports_value_min_year_2 <- eventReactive(input$cp_go, {
      show_dollars(exports_value_min_year())
    })
    
    exports_value_max_year_2 <- eventReactive(input$cp_go, {
      show_dollars(exports_value_max_year())
    })
    
    imports_value_min_year_2 <- eventReactive(input$cp_go, {
      show_dollars(imports_value_min_year())
    })
    
    imports_value_max_year_2 <- eventReactive(input$cp_go, {
      show_dollars(imports_value_max_year())
    })
    
    exports_growth <- eventReactive(input$cp_go, {
      growth_rate(
        exports_value_max_year(), exports_value_min_year(), input_country_profile_y()
      )
    })
    
    exports_growth_2 <- eventReactive(input$cp_go, {
      show_percentage(exports_growth())
    })
    
    exports_growth_increase_decrease <- eventReactive(input$cp_go, {
      ifelse(exports_growth() >= 0, "increased", "decreased")
    })
    
    exports_growth_increase_decrease_2 <- eventReactive(input$cp_go, {
      ifelse(exports_growth() >= 0, "increase", "decrease")
    })
    
    imports_growth <- eventReactive(input$cp_go, {
      growth_rate(
        imports_value_max_year(), imports_value_min_year(), input_country_profile_y()
      )
    })
    
    imports_growth_2 <- eventReactive(input$cp_go, {
      show_percentage(imports_growth())
    })
    
    imports_growth_increase_decrease <- eventReactive(input$cp_go, {
      ifelse(imports_growth() >= 0, "increased", "decreased")
    })
    
    imports_growth_increase_decrease_2 <- eventReactive(input$cp_go, {
      ifelse(imports_growth() >= 0, "increase", "decrease")
    })
    
    trade_rankings <- eventReactive(input$cp_go, {
      min_max_y <- c(min(input_country_profile_y()), max(input_country_profile_y()))
      
      d <- tbl(con, "yrp") %>% 
        filter(
          year %in% min_max_y &
            reporter_iso == !!input_country_profile_reporter_iso()
        ) %>% 
        collect()
      
      if (input_country_profile_convert_dollars() != "No conversion") {
        d <- tradestatistics::ots_gdp_deflator_adjustment(
          data.table::as.data.table(d),
          as.integer(input_country_profile_convert_dollars())
        )
      }
      
      d <- d %>% 
        # filter(partner_iso != "0-unspecified") %>% 
        mutate(
          trade_value_usd_bal = trade_value_usd_exp + trade_value_usd_imp
        ) %>% 
        group_by(year) %>% 
        mutate(
          bal_rank = dense_rank(desc(trade_value_usd_bal)),
          exp_share = trade_value_usd_exp / sum(trade_value_usd_exp),
          imp_share = trade_value_usd_imp / sum(trade_value_usd_imp)
        )
      
      return(d)
    })
    
    trade_rankings_no_min_year <- eventReactive(input$cp_go, {
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == min(input_country_profile_y()),
          reporter_iso == input_country_profile_reporter_iso(),
          partner_iso == input_country_profile_partner_iso()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trade_rankings_no_max_year <- eventReactive(input$cp_go, {
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == max(input_country_profile_y()),
          reporter_iso == input_country_profile_reporter_iso(),
          partner_iso == input_country_profile_partner_iso()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trade_rankings_remained <- eventReactive(input$cp_go, {
      ifelse(
        trade_rankings_no_min_year() == trade_rankings_no_max_year(),
        "remained", 
        "moved to"
      )
    })
    
    trade_rankings_exp_share_min_year <- eventReactive(input$cp_go, {
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == min(input_country_profile_y()),
          reporter_iso == input_country_profile_reporter_iso(),
          partner_iso == input_country_profile_partner_iso()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trade_rankings_exp_share_min_year_2 <- eventReactive(input$cp_go, {
      show_percentage(trade_rankings_exp_share_min_year())
    })
    
    trade_rankings_exp_share_max_year <- eventReactive(input$cp_go, {
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == max(input_country_profile_y()),
          reporter_iso == input_country_profile_reporter_iso(),
          partner_iso == input_country_profile_partner_iso()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trade_rankings_exp_share_max_year_2 <- eventReactive(input$cp_go, {
      show_percentage(trade_rankings_exp_share_max_year())
    })
    
    trade_rankings_imp_share_min_year <- eventReactive(input$cp_go, {
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == min(input_country_profile_y()),
          reporter_iso == input_country_profile_reporter_iso(),
          partner_iso == input_country_profile_partner_iso()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trade_rankings_imp_share_min_year_2 <- eventReactive(input$cp_go, {
      show_percentage(trade_rankings_imp_share_min_year())
    })
    
    trade_rankings_imp_share_max_year <- eventReactive(input$cp_go, {
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == max(input_country_profile_y()),
          reporter_iso == input_country_profile_reporter_iso(),
          partner_iso == input_country_profile_partner_iso()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trade_rankings_imp_share_max_year_2 <- eventReactive(input$cp_go, {
      show_percentage(trade_rankings_imp_share_max_year())
    })
    
    ### Text/Visual elements ----
    
    trade_summary_text_exp <- eventReactive(input$cp_go, {
      switch(table_aggregated(),
             "yr" = glue("The exports of { reporter_add_the() } { reporter_name() } to the World { exports_growth_increase_decrease() } from 
                          { exports_value_min_year_2() } in { min(input_country_profile_y()) } to { exports_value_max_year_2() } in { max(input_country_profile_y()) } 
                          (annualized { exports_growth_increase_decrease_2() } of { exports_growth_2() })."),
             
             "yrp" = glue("The exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { exports_growth_increase_decrease() } from 
                          { exports_value_min_year_2() } in { min(input_country_profile_y()) } 
                          to { exports_value_max_year_2() } in { max(input_country_profile_y()) } (annualized { exports_growth_increase_decrease_2() } of 
                          { exports_growth_2() }). { partner_add_the() } { partner_name() } was the No. { trade_rankings_no_min_year() } trading partner of 
                          { reporter_add_the() } { reporter_name() } in { min(input_country_profile_y()) } (represented { trade_rankings_exp_share_min_year_2() } of its exports), and 
                          then { trade_rankings_remained() } No. { trade_rankings_no_max_year() } in { max(input_country_profile_y()) } (represented { trade_rankings_exp_share_max_year_2() } 
                          of its exports).")
      )
    })
    
    trade_summary_text_imp <- eventReactive(input$cp_go, {
      switch(table_aggregated(),
             "yr" = glue("The imports of { reporter_add_the() } { reporter_name() } to the World { imports_growth_increase_decrease() } from 
                         { imports_value_min_year_2() } in { min(input_country_profile_y()) } to { imports_value_max_year_2() } in { max(input_country_profile_y()) } 
                         (annualized { imports_growth_increase_decrease_2() } of { imports_growth_2() })."),
             
             "yrp" = glue("The imports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { imports_growth_increase_decrease() } from 
                          { imports_value_min_year_2() } in { min(input_country_profile_y()) } 
                          to { imports_value_max_year_2() } in { max(input_country_profile_y()) } (annualized { imports_growth_increase_decrease_2() } of 
                          { imports_growth_2() }). { partner_add_the() } { partner_name() } was the No. { trade_rankings_no_min_year() } trading partner of 
                          { reporter_add_the() } { reporter_name() } in { min(input_country_profile_y()) } (represented { trade_rankings_imp_share_min_year_2() } of its imports), and 
                          then { trade_rankings_remained() } No. { trade_rankings_no_max_year() } in { max(input_country_profile_y()) } (represented { trade_rankings_imp_share_max_year_2() } 
                          of its imports).")
      )
    })

    trade_exchange_lines_title <- eventReactive(input$cp_go, {
      switch(table_aggregated(),
             "yr" = glue("{ reporter_add_proper_the() } { reporter_name() } multilateral trade between { min(input_country_profile_y()) } and { max(input_country_profile_y()) }"),
             "yrp" = glue("{ reporter_add_proper_the() } { reporter_name() } and { partner_add_the() } { partner_name() } exchange between { min(input_country_profile_y()) } and { max(input_country_profile_y()) }")
      )
    })
    
    trade_exchange_lines_aggregated <- eventReactive(input$cp_go, {
      d <- trade_table_aggregated()
      
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

      # highchart() %>%
      #   hc_xAxis(title = list(text = "Year"),
      #            categories = d$year) %>%
      #   hc_yAxis(title = list(text = "USD billion (FOB)"),
      #            labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>% 
      #   hc_add_series(name = "Exports", data = d$trade_value_usd_exp) %>% 
      #   hc_add_series(name = "Imports", data = d$trade_value_usd_imp) %>% 
      #   hc_title(text = trade_exchange_lines_title())
      
      hchart(d, 
        "line", 
        hcaes(x = year, y = trade, group = flow),
        tooltip = list(
          pointFormatter = custom_tooltip_short()
        )) %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "USD billion (FOB)"),
                 labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>% 
        hc_title(text = trade_exchange_lines_title())
    })
    
    ## Exports ----
    
    ### Tables ----
    
    exports_imports_table_origin_destination_year <- eventReactive(input$cp_go, {
      min_max_y <- c(min(input_country_profile_y()), max(input_country_profile_y()))
      
      d <- tbl(con, "yrp") %>% 
        filter(
          year %in% min_max_y &
            reporter_iso == !!input_country_profile_reporter_iso()
        ) %>% 
        collect()
      
      if (input_country_profile_convert_dollars() != "No conversion") {
        d <- tradestatistics::ots_gdp_deflator_adjustment(
          data.table::as.data.table(d),
          as.integer(input_country_profile_convert_dollars())
        )
      }
      
      return(d)
    })
    
    ### Visual elements ----
    
    exports_subtitle <- eventReactive(input$cp_go, {
      switch(
        table_detailed_country_profile(),
        "yrc" = glue("Detailed multilateral Exports and Imports { min(input_country_profile_y()) }-{ max(input_country_profile_y()) }"),
        "yrpc" = glue("Detailed bilateral Exports and Imports { min(input_country_profile_y()) }-{ max(input_country_profile_y()) }")
      )
    })
    
    exports_note <- eventReactive(input$cp_go, {
      switch(
        table_detailed_country_profile(),
        "yrc" = glue("Explore the exports and imports of { reporter_add_the() } { reporter_name() } to/from the World at the begining 
          and end of the selected period. The data was grouped by sections for visual clarity, you can click each section to see the finer detail."),
        "yrpc" = glue("Explore the exports and imports of { reporter_add_the() } { reporter_name() } to/from { partner_add_the() } 
          { partner_name() } at the begining and end of the selected period. The data was grouped by sections for visual clarity, you can 
          click each section to see the finer detail.")
      )
    })
    
    exports_title_year <- eventReactive(input$cp_go, {
      switch(
        table_detailed_country_profile(),
        "yrc" = glue("Exports of { reporter_add_the() } { reporter_name() } to the rest of the World in { min(input_country_profile_y()) } and { max(input_country_profile_y()) }, by product"),
        "yrpc" = glue("Exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } in { min(input_country_profile_y()) } and { max(input_country_profile_y()) }, by product")
      )
    })
    
    exports_title_min_year <- eventReactive(input$cp_go, {
      glue("{ min(input_country_profile_y()) }")
    })
    
    exports_treemap_destinations_min_year <- eventReactive(input$cp_go, {
      d <- exports_imports_table_origin_destination_year() %>%
        filter(year == min(year)) %>% 
        od_order_and_add_continent()
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    exports_treemap_detailed_min_year <- eventReactive(input$cp_go, {
      d <- data_detailed() %>%
        filter(year == min(input_country_profile_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
      
      d2 <- pd_colors(d)
      
      pd_to_highcharts(d, d2)
    })
    
    exports_title_max_year <- eventReactive(input$cp_go, {
      glue("{ max(input_country_profile_y()) }")
    })
    
    exports_treemap_destinations_max_year <- eventReactive(input$cp_go, {
      d <- exports_imports_table_origin_destination_year() %>%
        filter(year == max(year)) %>% 
        od_order_and_add_continent()
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    exports_treemap_detailed_max_year <- eventReactive(input$cp_go, {
      d <- data_detailed() %>%
        filter(year == max(input_country_profile_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
      
      d2 <- pd_colors(d)
      
      pd_to_highcharts(d, d2)
    })

    ## Imports ----
    
    ### Visual elements ----
    
    imports_title_year <- eventReactive(input$cp_go, {
      switch(
        table_detailed_country_profile(),
        "yrc" = glue("Imports of { reporter_add_the() } { reporter_name() } from the rest of the World in { min(input_country_profile_y()) } and { max(input_country_profile_y()) }, by product"),
        "yrpc" = glue("Imports of { reporter_add_the() } { reporter_name() } from { partner_add_the() } { partner_name() } in { min(input_country_profile_y()) } and { max(input_country_profile_y()) }, by product")
      )
    })
    
    imports_title_min_year <- eventReactive(input$cp_go, {
      glue("{ min(input_country_profile_y()) }")
    })
    
    imports_treemap_origins_min_year <- eventReactive(input$cp_go, {
      d <- exports_imports_table_origin_destination_year() %>%
        filter(year == min(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    imports_treemap_detailed_min_year <- eventReactive(input$cp_go, {
      d <- data_detailed() %>%
        filter(year == min(input_country_profile_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
      
      d2 <- pd_colors(d)
      
      pd_to_highcharts(d, d2)
    })
    
    imports_title_max_year <- eventReactive(input$cp_go, {
      glue("{ max(input_country_profile_y()) }")
    })
    
    imports_treemap_origins_max_year <- eventReactive(input$cp_go, {
      d <- exports_imports_table_origin_destination_year() %>%
        filter(year == max(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    imports_treemap_detailed_max_year <- eventReactive(input$cp_go, {
      d <- data_detailed() %>%
        filter(year == max(input_country_profile_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
      
      d2 <- pd_colors(d)
      
      pd_to_highcharts(d, d2)
    })
    
    # Model ----
  
    model_custom_data <- eventReactive(input$mod_go, {
      uploaded_file <- input$mod_own
      
      if(!is.null(uploaded_file)) {
        input_data <- rio::import(file = uploaded_file$datapath, format = tools::file_ext(uploaded_file$name)) %>%
          janitor::clean_names()
        
        return(input_data)
      } else {
        data.frame()
      }
    })
    
    data_detailed_model <- eventReactive(input$mod_go, {
      # 1. read from SQL
      
      d <- tbl(con, "yrpc")
          
      if (input_model_partner_iso() == "all") {
        d <- d %>% 
          filter(
            year %in% !!input_model_y() &
              reporter_iso == !!input_model_reporter_iso()
          )
      } else {
        d <- d %>% 
          filter(
            year %in% !!input_model_y() &
              reporter_iso == !!input_model_reporter_iso() &
              partner_iso == !!input_model_partner_iso()
          )
      }
          
      d <- d %>% collect()
      
      if (input_model_convert_dollars() != "No conversion") {
        d <- tradestatistics::ots_gdp_deflator_adjustment(
          data.table::as.data.table(d),
          as.integer(input_model_convert_dollars())
        )
      }
      
      # 2. apply filters
      
      if (!any(input_model_product_filter() %in% c("All Products"))) {
        if (any(input_model_product_filter() %in% "Vaccine Inputs")) {
          vaccine_codes <- read.csv("vaccine_codes.csv")
          d <- d %>% 
            mutate(
              section_name = case_when(
                commodity_code %in% vaccine_codes$commodity_code ~ "Vaccine Inputs",
                TRUE ~ section_name
              )
            )
        }
        
        d <- d %>% 
          filter(section_name %in% input_model_product_filter())
      }
      
      if (any(input_model_ctn() %in% "mfn")) {
        # 3.1 read from SQL
        
        d <- d %>%
          inner_join(
            tbl(con, "tariffs") %>% 
              filter(
                years %in% !!input_model_y(),
                # here we need the applied tariffs when the product gets to destination
                reporters = input_model_partner_iso()
              ) %>% 
              select(year, partner_iso = reporter_iso, commodity_code, mfn = simple_average) %>% 
              collect(),
            by = c("year", "partner_iso", "commodity_code")
          )
        
        # 3.2. summarise
        
        d2 <- d %>% 
          select(year, reporter_iso, partner_iso, trade_value_usd_exp, trade_value_usd_imp) %>% 
          group_by(year, reporter_iso, partner_iso) %>% 
          summarise(
            trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T),
            trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)
          )
        
        d3 <- d %>% 
          select(year, reporter_iso, partner_iso, trade_value_usd_exp, mfn) %>% 
          group_by(year, reporter_iso, partner_iso) %>% 
          summarise(
            log_mfn = log(weighted.mean(mfn, trade_value_usd_exp, na.rm = T))
          ) %>% 
          ungroup()
        
        d <- d2 %>% left_join(d3); rm(d2,d3)
      } else {
        # 3.1. summarise
        
        d <- d %>% 
          select(year, reporter_iso, partner_iso, trade_value_usd_exp, trade_value_usd_imp) %>% 
          group_by(year, reporter_iso, partner_iso) %>% 
          summarise(
            trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T),
            trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)
          ) %>% 
          ungroup()
      }
      
      # 4. add geo dist data
      
      d <- d %>% 
        mutate(
          country1 = pmin(reporter_iso, partner_iso),
          country2 = pmax(reporter_iso, partner_iso)
        ) %>% 
        inner_join(
          tbl(con, "distances") %>% 
            select(country1, country2,
                   c(!!input_model_dist(), !!input_model_bin()[!!input_model_bin() != "rta"])) %>% 
            collect(),
          by = c("country1", "country2")
        ) %>% 
        select(-c(country1,country2))
      
      # 5. add RTA data
      
      if (any(input_model_bin() %in% "rta")) {
        d <- d %>%
          mutate(
            country1 = pmin(reporter_iso, partner_iso),
            country2 = pmax(reporter_iso, partner_iso)
          ) %>%
          left_join(
            tbl(con, "rtas") %>% 
              filter(year %in% !!input_model_y()) %>% 
              collect(),
            by = c("year", "country1", "country2")
          ) %>% 
          mutate(
            rta = case_when(
              is.na(rta) & nchar(country1) == 3 & nchar(country2) == 3 ~ 0L,
              TRUE ~ rta
            )
          ) %>% 
          select(-c(country1,country2))
      }
      
      # 6. create remoteness indexes
      
      if (input_model_type() == "olsrem") {
        d <- d %>% 
          # Replicate total_e
          group_by(reporter_iso, year) %>%
          mutate(total_e = sum(trade_value_usd_exp, na.rm = T)) %>%
          group_by(year) %>%
          mutate(total_e = max(total_e, na.rm = T)) %>%
          # Replicate rem_exp
          group_by(reporter_iso, year) %>%
          mutate(
            remoteness_exp = sum(dist *  total_e / trade_value_usd_exp, na.rm = T),
            log_remoteness_exp = log(remoteness_exp)
          )
        
        d <- d %>% 
            # Replicate total_y
            group_by(partner_iso, year) %>%
            mutate(total_y = sum(trade_value_usd_imp, na.rm = T)) %>%
            group_by(year) %>%
            mutate(total_y = max(total_y, na.rm = T)) %>%
            # Replicate rem_imp
            group_by(partner_iso, year) %>%
            mutate(
              remoteness_imp = sum(dist / (trade_value_usd_imp / total_y), na.rm = T),
              log_remoteness_imp = log(remoteness_imp)
            )
        
        d <- d %>% 
          select(-c(total_e, total_y, remoteness_exp, remoteness_imp))
      }
      
      # 7. create fixed effects
      
      if (input_model_type() == "olsfe") {
        d <- d %>% 
          mutate(
            reporter_year = paste0(reporter_iso, year),
            partner_year = paste0(partner_iso, year)
          )
      }
      
      # 8. log variables
      
      if (input_model_dist() == "dist") {
        d <- d %>% 
          mutate(log_dist = log(dist)) %>% 
          select(-dist)
      } else {
        d <- d %>% 
          mutate(log_distcap = log(distcap)) %>% 
          select(-distcap)
      }
      
      if (input_model_type() != "ppml") {
        d <- d %>% 
          mutate(
            # not needed for PPML!
            log_trade_value_usd_exp = log(trade_value_usd_exp)
          ) %>% 
          select(-trade_value_usd_exp)
      }
      
      # 9. create clustering variable
      
      if (input_model_cluster() == "yes") {
       d <- d %>% 
         mutate(reporter_partner_iso = paste(reporter_iso, partner_iso, sep = "_"))
      }
      
      # 10. join with custom data
      
      if (nrow(model_custom_data()) > 0) {
        d <- d %>% 
          inner_join(model_custom_data())
      }
      
      gc()
      
      return(
        d %>% 
          select(year, matches("trade"), everything()) %>% 
          select(-trade_value_usd_imp)
      )
    })
    
    data_detailed_model_text <- eventReactive(input$mod_go, { 
      glue("The filtered dataset contains { nrow(data_detailed_model()) } rows and
           { ncol(data_detailed_model()) } columns. Here's a preview of the table to use to fit regression models:")
    })
    data_detailed_model_preview <- eventReactive(input$mod_go, { head(data_detailed_model()) })
    
    model_formula <- eventReactive(input$mod_go, {
      custom_variables <- if(nrow(model_custom_data()) > 0) {
        cols <- colnames(model_custom_data())
        cols <- cols[cols %in% unlist(strsplit(input_model_custom_subset(), ";"))]
        if (length(cols) > 0) cols else "1"
      } else {
        "1" # just a trick to avoid complex if else statements
      }
      
      if (input_model_type() == "ols") {
        if (length(input_model_bin()) > 0 & length(input_model_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", input_model_dist()), 
                                                                        paste0("log_", input_model_ctn()), 
                                                                        input_model_bin(),
                                                                        custom_variables), 
                                                                      collapse = " + "))) 
        }
        
        if (length(input_model_bin()) > 0 & length(input_model_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", input_model_dist()), 
                                                                        input_model_bin(),
                                                                        custom_variables), 
                                                                      collapse = " + "))) 
        }
        
        if (length(input_model_bin()) == 0 & length(input_model_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", input_model_dist()), 
                                                                        paste0("log_", input_model_ctn()),
                                                                        custom_variables), 
                                                                      collapse = " + "))) 
        }
        
        if (length(input_model_bin()) == 0 & length(input_model_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", input_model_dist()),
                                                                        custom_variables), 
                                                                      collapse = " + "))) 
        }
      }
      
      if (input_model_type() == "olsrem") {
        if (length(input_model_bin()) > 0 & length(input_model_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", input_model_dist()),
                                                                        paste0("log_", input_model_ctn()),
                                                                        input_model_bin(),
                                                                        custom_variables,
                                                                        "log_remoteness_exp", "log_remoteness_imp"), collapse = " + ")))
        }
        
        if (length(input_model_bin()) > 0 & length(input_model_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", input_model_dist()),
                                                                        input_model_bin(),
                                                                        custom_variables,
                                                                        "log_remoteness_exp", "log_remoteness_imp"), collapse = " + ")))
        }
        
        if (length(input_model_bin()) == 0 & length(input_model_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", input_model_dist()),
                                                                        paste0("log_", input_model_ctn()),
                                                                        custom_variables,
                                                                        "log_remoteness_exp", "log_remoteness_imp"), collapse = " + ")))
        }
        
        if (length(input_model_bin()) == 0 & length(input_model_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", input_model_dist()),
                                                                        custom_variables,
                                                                        "log_remoteness_exp", "log_remoteness_imp"), collapse = " + ")))
        }
      }
      
      if (input_model_type() == "olsfe") {
        if (length(input_model_bin()) > 0 & length(input_model_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(paste(c(paste0("log_", input_model_dist()),
                                                                              paste0("log_", input_model_ctn()),
                                                                              input_model_bin(),
                                                                              custom_variables), collapse = " + "), 
                                                                      "reporter_year + partner_year", sep = " | ")))
        }
        
        if (length(input_model_bin()) > 0 & length(input_model_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(paste(c(paste0("log_", input_model_dist()),
                                                                              input_model_bin(),
                                                                              custom_variables), collapse = " + "), 
                                                                      "reporter_year + partner_year", sep = " | ")))
        }
        
        if (length(input_model_bin()) == 0 & length(input_model_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(paste(c(paste0("log_", input_model_dist()),
                                                                              paste0("log_", input_model_ctn()),
                                                                              custom_variables), collapse = " + "), 
                                                                      "reporter_year + partner_year", sep = " | ")))
        }
        
        if (length(input_model_bin()) == 0 & length(input_model_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(paste(c(paste0("log_", input_model_dist()),
                                                                              custom_variables), collapse = " + "), 
                                                                      "reporter_year + partner_year", sep = " | ")))
        }
      }
      
      if (input_model_type() == "ppml") {
        if (length(input_model_bin()) > 0 & length(input_model_ctn()) > 0) {
          f <- as.formula(paste("trade_value_usd_exp", "~", paste(paste(c(paste0("log_", input_model_dist()),
                                                                          paste0("log_", input_model_ctn()),
                                                                          input_model_bin(),
                                                                          custom_variables), collapse = " + "))))
        }
        
        if (length(input_model_bin()) > 0 & length(input_model_ctn()) == 0) {
          f <- as.formula(paste("trade_value_usd_exp", "~", paste(paste(c(paste0("log_", input_model_dist()),
                                                                          input_model_bin(),
                                                                          custom_variables), collapse = " + "))))
        }
        
        if (length(input_model_bin()) == 0 & length(input_model_ctn()) > 0) {
          f <- as.formula(paste("trade_value_usd_exp", "~", paste(paste(c(paste0("log_", input_model_dist()),
                                                                          paste0("log_", input_model_ctn()),
                                                                          custom_variables), collapse = " + "))))
        }
        
        if (length(input_model_bin()) == 0 & length(input_model_ctn()) == 0) {
          f <- as.formula(paste("trade_value_usd_exp", "~", paste(paste(c(paste0("log_",input_model_dist()),
                                                                          custom_variables), collapse = " + "))))
        }
      }
      
      print(f)
      return(f)
    })
    
    model_output <- eventReactive(input$mod_go, {
      if (any(input_model_type() %in% c("ols", "olsrem", "olsfe"))) {
        if (input_model_cluster() == "yes") {
          m <- feols(model_formula(), data_detailed_model(), cluster = ~reporter_partner_iso)
        } else {
          m <- feols(model_formula(), data_detailed_model())
        }
      }
      
      if (input_model_type() == "ppml") {
        if (input_model_cluster() == "yes") {
          m <- feglm(model_formula(), data_detailed_model(),
                     cluster = ~reporter_partner_iso,
                     family = quasipoisson(link = "log"))      
        } else {
          m <- feglm(model_formula(), data_detailed_model(),
                     family = quasipoisson(link = "log"))
        }
      }
      
      gc()
      return(m)
    })
    
    # Cite ----
    
    cite <- reactive({
      glue(
        "Open Trade Statistics. \"OTS BETA DASHBOARD\". <i>Open Trade Statistics</i>. 
        Accessed {months(Sys.Date()) } { lubridate::day(Sys.Date()) }, { lubridate::year(Sys.Date()) }. { site_url }/"
      )
    })

    cite_bibtex <- reactive({
      glue("@misc{{open_trade_statistics_{lubridate::year(Sys.Date())},
      title = {{Open Trade Statistics Beta Dashboard}},
      url = {{{ site_url }/}},
      author = {{Vargas, Mauricio}},
      doi = {{10.5281/zenodo.3738793}},
      publisher = {{Open Trade Statistics}},
      year = {{2019}},
      month = {{Apr}},
      note = {{Accessed: { months(Sys.Date()) } { lubridate::day(Sys.Date()) }, { lubridate::year(Sys.Date()) }}}}}"
      )
    })
    
    # Titles output -----------------------------------------------------------
    
    output$title_country_profile <- renderText({
      title_country_profile()
    })
    
    output$title_model <- renderText({
      "Gravity Models"
    })
    
    legend_text <- "The information displayed here is based on <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. Please read our <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a> for a full description
      of restrictions and applicable licenses. These figures do not include services or foreign direct investment."
    
    output$title_country_profile_legend <- renderText({ legend_text })
    
    output$title_model_legend <- renderText({ legend_text })
    
    # Country profile output ----
    
    ## Trade output ----
    
    output$trade_subtitle <- eventReactive(input$cp_go, {
      switch(
        table_detailed_country_profile(),
        "yrc" = glue("Total multilateral Exports and Imports { min(input_country_profile_y()) }-{ max(input_country_profile_y()) }"),
        "yrpc" = glue("Total bilateral Exports and Imports { min(input_country_profile_y()) }-{ max(input_country_profile_y()) }")
      )
    })
    
    output$trade_subtitle_exp <- eventReactive(input$cp_go, {
      "Exports"
    })
    
    output$trade_subtitle_imp <- eventReactive(input$cp_go, {
      "Imports"
    })
    
    output$trade_partners_title <- eventReactive(input$cp_go, {
      glue("Trading partners { min(input_country_profile_y()) }-{ max(input_country_profile_y()) }")
    })
    
    output$trade_partners_title <- eventReactive(input$cp_go, {
      glue("Trading partners { min(input_country_profile_y()) }-{ max(input_country_profile_y()) }")
    })
    
    output$trade_partners_text <- eventReactive(input$cp_go, {
      glue("Explore the trade partners of { reporter_add_the() } { reporter_name() } at the begining and end 
           of the selected period. The data was grouped by continent and country for visual clarity, you can click
           each continent/area to see the finer detail.")
    })
    
    output$trade_exports_year_subtitle <- eventReactive(input$cp_go, {
      glue("Exports of { reporter_add_the() } { reporter_name() } to the rest of the World in 
           { min(input_country_profile_y()) } and { max(input_country_profile_y()) }, by destination")
    })
    
    output$trade_exports_min_year_subtitle <- eventReactive(input$cp_go, {
      glue("{ min(input_country_profile_y()) }")
    })

    output$trade_exports_max_year_subtitle <- eventReactive(input$cp_go, {
      glue("{ max(input_country_profile_y()) }")
    })
    
    output$trade_imports_year_subtitle <- eventReactive(input$cp_go, {
      glue("Imports of { reporter_add_the() } { reporter_name() } from the rest of the World in 
           { min(input_country_profile_y()) } and { max(input_country_profile_y()) }, by origin")
    })
    
    output$trade_imports_min_year_subtitle <- eventReactive(input$cp_go, {
      glue("{ min(input_country_profile_y()) }")
    })

    output$trade_imports_max_year_subtitle <- eventReactive(input$cp_go, {
      glue("{ max(input_country_profile_y()) }")
    })
    
    output$trade_summary_exp <- renderText(trade_summary_text_exp())
    output$trade_summary_imp <- renderText(trade_summary_text_imp())
    
    output$trade_title <- renderText(trade_title())
    
    output$trade_exchange_lines_aggregated <- renderHighchart({
      trade_exchange_lines_aggregated()
    })
    
    ## Exports output ----
    
    output$exports_subtitle <- renderText(exports_subtitle())
    output$exports_note <- renderText(exports_note())
    output$exports_title_year <- renderText(exports_title_year())
    output$exports_title_min_year <- renderText(exports_title_min_year())
    output$exports_treemap_destinations_min_year <- renderHighchart({exports_treemap_destinations_min_year()})
    output$exports_treemap_detailed_min_year <- renderHighchart({exports_treemap_detailed_min_year()})
    output$exports_title_max_year <- renderText(exports_title_max_year())
    output$exports_treemap_destinations_max_year <- renderHighchart({exports_treemap_destinations_max_year()})
    output$exports_treemap_detailed_max_year <- renderHighchart({exports_treemap_detailed_max_year()})
    
    ## Imports output ----
    
    output$imports_title_year <- renderText(imports_title_year())
    output$imports_title_min_year <- renderText(imports_title_min_year())
    output$imports_treemap_origins_min_year <- renderHighchart({imports_treemap_origins_min_year()})
    output$imports_treemap_detailed_min_year <- renderHighchart({imports_treemap_detailed_min_year()})
    output$imports_title_max_year <- renderText(imports_title_max_year())
    output$imports_treemap_origins_max_year <- renderHighchart({imports_treemap_origins_max_year()})
    output$imports_treemap_detailed_max_year <- renderHighchart({imports_treemap_detailed_max_year()})
    
    # Model output ----
    
    output$model_data_subtitle <- eventReactive(input$mod_go, { "Data preview" })
    output$data_detailed_model_text <- renderText(data_detailed_model_text())
    output$data_detailed_model_preview <- renderTable(data_detailed_model_preview())
    
    output$model_formula_latex <- renderUI({
      if (input_model_type() == "ppml") {
        lhs <- "\\text{trade value}_{ij}^{t}"
      } else {
        lhs <- "\\text{log trade value}_{ij}^{t}"
      }
      
      if (input_model_type() == "ols") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(input_model_dist(), input_model_ctn(), input_model_bin())
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", input_model_dist()), 
                                                    paste("log", input_model_ctn()), input_model_bin())), "}"), "_{ij}^{t}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\beta_0 +", rhs, "+ \\varepsilon_{ij}^{t}")
      }
      
      if (input_model_type() == "olsrem") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(input_model_dist(), input_model_bin(), "log_remoteness_exp", "log_remoteness_imp")
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", input_model_dist()), 
                                                    input_model_bin(), "log_remoteness_exp", "log_remoteness_imp")), "}"), "_{ij}^{t}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\beta_0 +", rhs, "+ \\varepsilon_{ij}^{t}")
      }
      
      if (input_model_type() == "olsfe") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(input_model_dist(), input_model_bin(), "reporter_year", "partner_year")
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", input_model_dist()), 
                                                    input_model_bin(), "reporter_year", "partner_year")), "}"), "_{ij}^{t}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\beta_0 +", rhs, "+ \\varepsilon_{ij}^{t}")
      }
      
      if (input_model_type() == "ppml") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(input_model_dist(), input_model_bin())
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", input_model_dist()), 
                                                    input_model_bin())), "}"), "_{ij}^{t}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\exp[\\beta_0 +", rhs, "]\\times \\varepsilon_{ij}^{t}")
      }
      
      withMathJax(
        paste0("\\[", lhs, "=", rhs, "\\]")
      )
    })
    
    output$model_summary_subtitle <- eventReactive(input$mod_go, { "Model summary" })
    output$model_summary_text <- eventReactive(input$mod_go, { 
      "WIP: some selections create errors messages such as 'contrasts can be applied only to factors with 2 or more levels' but Shiny hides those."
    })
    output$model_summary_tidy <- renderTable(tidy(model_output()))
    output$model_summary_glance <- renderTable(glance(model_output()))
    
    # Download output ----
    
    ## Country profile ----
    
    download_country_profile_subtitle <- eventReactive(input$cp_go, {
      "Download country profile data"
    })
    
    download_country_profile_text <- eventReactive(input$cp_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    download_country_profile_format <- eventReactive(input$cp_go, {
      selectInput(
        "cp_f",
        "Download data as:",
        choices = available_formats,
        selected = NULL,
        selectize = TRUE
      )
    })
    
    output$download_country_profile_aggregated_pre <- downloadHandler(
        filename = function() {
          glue("{ table_aggregated() }_{ input_country_profile_reporter_iso() }_{ input_country_profile_partner_iso() }_{ min(input_country_profile_y()) }_{ max(input_country_profile_y()) }.{ input_country_profile_format() }")
        },
        content = function(filename) {
          rio::export(data_aggregated(), filename)
        },
        contentType = "application/zip"
      )
    
    output$download_country_profile_detailed_pre <- downloadHandler(
      filename = function() {
        glue("{ table_detailed_country_profile() }_{ input_country_profile_reporter_iso() }_{ input_country_profile_partner_iso() }_{ min(input_country_profile_y()) }_{ max(input_country_profile_y()) }.{ input_country_profile_format() }")
      },
      content = function(filename) {
        rio::export(data_detailed(), filename)
      },
      contentType = "application/zip"
    )
    
    output$download_country_profile_subtitle <- renderText({download_country_profile_subtitle()})
    output$download_country_profile_text <- renderText({download_country_profile_text()})
    output$download_country_profile_format <- renderUI({download_country_profile_format()})
    output$download_country_profile_aggregated <- renderUI({
      req(input$cp_go)
      downloadButton('download_country_profile_aggregated_pre', label = 'Aggregated data')
    })
    output$download_country_profile_detailed <- renderUI({
      req(input$cp_go)
      downloadButton('download_country_profile_detailed_pre', label = 'Detailed data')
    })
    
    ## Model ----
    
    download_model_subtitle <- eventReactive(input$mod_go, { "Download model data" })
    
    download_model_text <- eventReactive(input$mod_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    download_model_format <- eventReactive(input$mod_go, {
      selectInput(
        "mod_f",
        "Download data as:",
        choices = available_formats,
        selected = NULL,
        selectize = TRUE
      )
    })
    
    output$download_model_detailed_pre <- downloadHandler(
      filename = function() {
        glue("{ input_model_type() }_{ input_model_reporter_iso() }_{ input_model_partner_iso() }_{ min(input_model_y()) }_{ max(input_model_y()) }.{ input_model_format() }")
      },
      content = function(filename) {
        rio::export(data_detailed_model(), filename)
      },
      contentType = "application/zip"
    )
    
    output$download_model_fit_pre <- downloadHandler(
      filename = function() {
        glue("{ input_model_type() }_{ input_model_reporter_iso() }_{ input_model_partner_iso() }_{ min(input_model_y()) }_{ max(input_model_y()) }.rds")
      },
      content = function(filename) {
        saveRDS(model_output(), filename)
      },
      contentType = "application/zip"
    )
    
    output$download_model_subtitle <- renderText({download_model_subtitle()})
    output$download_model_text <- renderText({download_model_text()})
    output$download_model_format <- renderUI({download_model_format()})
    output$download_model_detailed <- renderUI({
      req(input$mod_go)
      downloadButton('download_model_detailed_pre', label = 'Detailed data')
    })
    output$download_model_fit <- renderUI({
      req(input$mod_go)
      downloadButton('download_model_fit_pre', label = 'Fitted model')
    })
    
    # Cite output ----
    
    output$cite_subtitle <- renderText({"Cite"})
    
    output$cite_chicago_subtitle <- renderText({
      "Chicago citation"
    })
    
    output$cite <- renderText({
      cite()
    })
    
    output$cite_bibtex_subtitle <- renderText({
      "BibTeX entry"
    })
    
    output$cite_bibtex <- renderText({
      cite_bibtex()
    })
    
    output$site_footer <- renderText({
      glue("<center><i>Open Trade Statistics {lubridate::year(Sys.Date())}.</i></center>")
    })
    
    # Bookmarking -------------------------------------------------------------
    
    observe({
      # Trigger this observer every time an input changes
      # strip shiny related URL parameters
      reactiveValuesToList(input)
      setBookmarkExclude(c(
        "mod_own", "mod_f", "cp_f", "go", "sidebarCollapsed", "sidebarItemExpanded"
      ))
      session$doBookmark()
    })
    
    onBookmarked(function(url) {
      updateQueryString(url)
    })
  }
)
