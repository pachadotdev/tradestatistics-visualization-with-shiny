## server.R ##

shinyServer(
  function(input, output, session) {
    # User inputs ----
    
    ## Country profile ----
    
    input_cp_y <- reactive({
      y <- (min(input$cp_y[1], input$cp_y[2])):(max(input$cp_y[1], input$cp_y[2]))
      y <- seq(min(y), max(y), by = ifelse(max(y) - min(y) >= 10, 2, 1))
      return(y)
    })
    
    input_cp_reporter_iso <- reactive({ input$cp_r })
    input_cp_partner_iso <- reactive({ input$cp_p })
    
    input_cp_format <- reactive({ input$cp_f })
    
    input_cp_convert_dollars <- reactive({ input$cp_a })
    
    table_aggregated_cp <- eventReactive(input$cp_go, {
      ifelse(input_cp_partner_iso() == "all", "yr", "yrp")
    })
    
    table_detailed_cp <- eventReactive(input$cp_go, {
      ifelse(input_cp_partner_iso() == "all", "yrc", "yrpc")
    })
    
    reporter_name <- eventReactive(input$cp_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == input_cp_reporter_iso()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    partner_name <- eventReactive(input$cp_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == input_cp_partner_iso()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    ## Product profile ----
    
    input_pp_y <- reactive({
      y <- (min(input$pp_y[1], input$pp_y[2])):(max(input$pp_y[1], input$pp_y[2]))
      y <- c(min(y), max(y))
      return(y)
    })
    
    input_pp_section_code <- reactive({ input$pp_s })
    
    input_pp_convert_dollars <- reactive({ input$pp_a })
    
    input_pp_format <- reactive({ input$pp_f })
    
    section_name_pp <- eventReactive(input$pp_go, {
      warning(nchar(input_pp_section_code()))
      
      s <- if (nchar(input_pp_section_code()) == 2) {
        sections_to_display %>%
          filter(section_code == input_pp_section_code()) %>%
          select(section_fullname_english) %>%
          as.character()
      } else if (nchar(input_pp_section_code()) == 4) {
        commodities_to_display %>%
          filter(commodity_code == input_pp_section_code()) %>%
          select(commodity_fullname_english) %>%
          as.character()
      } else if (input_pp_section_code() == "vaccine") {
        "Vaccine Inputs"
      } else if (input_pp_section_code() == "all") {
        "All Products"
      }
      
      return(s)
    })
    
    ## Model ----
    
    input_model_y <- reactive({
      y2 <- (min(input$mod_y[1], input$mod_y[2])):(max(input$mod_y[1], input$mod_y[2]))
      y2 <- seq(min(y2), max(y2), by = input$mod_y_sep)
      return(y2)
    })
    
    input_model_reporter_iso <- reactive({ input$mod_r })
    input_model_partner_iso <- reactive({ input$mod_p })
    
    input_model_convert_dollars <- reactive({ input$mod_a })
    
    input_model_product_filter <- reactive({ input$mod_pf })
    input_model_type <- reactive({ input$mod_t })
    input_model_dist <- reactive({ input$mod_d })
    input_model_bin <- reactive({ input$mod_b })
    input_model_ctn <- reactive({ input$mod_ct })
    input_model_cluster <- reactive({ input$mod_cl })
    input_model_custom_subset <- reactive({ input$mod_s })
    
    input_model_format <- reactive({ input$mod_f })
    
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
    
    title_cp <- eventReactive(input$cp_go, {
      switch(
        table_detailed_cp(),
        "yrc" = glue("{ reporter_add_proper_the() } { reporter_name() } multilateral trade between { min(input_cp_y()) } and { max(input_cp_y()) }"),
        "yrpc" = glue("{ reporter_add_proper_the() } { reporter_name() } and { partner_add_the() } { partner_name() } between { min(input_cp_y()) } and { max(input_cp_y()) }")
      )
    })
    
    title_pp <- eventReactive(input$pp_go, {
      glue("{ section_name_pp() } multilateral trade in { min(input_pp_y()) } and { max(input_pp_y()) }")
    })
    
    # Country profile ----
    
    waitress_cp <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
    
    ## Data ----
    
    data_aggregated_cp <- eventReactive(input$cp_go, {
      waitress_cp$start()
      
      d <- tbl(con, table_aggregated_cp())
          
      if (input_cp_partner_iso() == "all") {
        d <- d %>% 
          filter(
            year %in% !!input_cp_y() &
              reporter_iso == !!input_cp_reporter_iso()
          )
      } else {
        d <- d %>% 
          filter(
            year %in% !!input_cp_y() &
              reporter_iso == !!input_cp_reporter_iso() &
              partner_iso == !!input_cp_partner_iso()
          )
      }
      
      d <- d %>% collect()

      if (input_cp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(input_cp_convert_dollars()))
      }
      
      waitress_cp$inc(2)
      
      return(d)
    })
    
    data_detailed_cp <- eventReactive(input$cp_go, {
      d <- tbl(con, table_detailed_cp())
          
      if (input_cp_partner_iso() == "all") {
        d <- d %>% 
          filter(
            year %in% !!input_cp_y() &
              reporter_iso == !!input_cp_reporter_iso()
          )
      } else {
        d <- d %>% 
          filter(
            year %in% !!input_cp_y() &
              reporter_iso == !!input_cp_reporter_iso() &
              partner_iso == !!input_cp_partner_iso()
          )
      }
          
      d <- d %>% collect()
      
      if (input_cp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(input_cp_convert_dollars()))
      }
      
      waitress_cp$inc(2)
      
      return(d)
    })
    
    ## Trade ----
    
    ### Tables ----
    
    trade_table_aggregated_cp <- eventReactive(input$cp_go, {
      data_aggregated_cp() %>%
        select(year, trade_value_usd_exp, trade_value_usd_imp)
    })
    
    exports_value_min_year_cp <- eventReactive(input$cp_go, {
      trade_table_aggregated_cp() %>%
        filter(year == min(input_cp_y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    exports_value_max_year_cp <- eventReactive(input$cp_go, {
      trade_table_aggregated_cp() %>%
        filter(year == max(input_cp_y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    imports_value_min_year_cp <- eventReactive(input$cp_go, {
      trade_table_aggregated_cp() %>%
        filter(year == min(input_cp_y())) %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })
    
    imports_value_max_year_cp <- eventReactive(input$cp_go, {
      trade_table_aggregated_cp() %>%
        filter(year == max(input_cp_y())) %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })
    
    exports_value_min_year_2_cp <- eventReactive(input$cp_go, {
      show_dollars(exports_value_min_year_cp())
    })
    
    exports_value_max_year_2_cp <- eventReactive(input$cp_go, {
      show_dollars(exports_value_max_year_cp())
    })
    
    imports_value_min_year_2_cp <- eventReactive(input$cp_go, {
      show_dollars(imports_value_min_year_cp())
    })
    
    imports_value_max_year_2_cp <- eventReactive(input$cp_go, {
      show_dollars(imports_value_max_year_cp())
    })
    
    exports_growth_cp <- eventReactive(input$cp_go, {
      growth_rate(
        exports_value_max_year_cp(), exports_value_min_year_cp(), input_cp_y()
      )
    })
    
    exports_growth_2_cp <- eventReactive(input$cp_go, {
      show_percentage(exports_growth_cp())
    })
    
    exports_growth_increase_decrease_cp <- eventReactive(input$cp_go, {
      ifelse(exports_growth_cp() >= 0, "increased", "decreased")
    })
    
    exports_growth_increase_decrease_2_cp <- eventReactive(input$cp_go, {
      ifelse(exports_growth_cp() >= 0, "increase", "decrease")
    })
    
    imports_growth_cp <- eventReactive(input$cp_go, {
      growth_rate(
        imports_value_max_year_cp(), imports_value_min_year_cp(), input_cp_y()
      )
    })
    
    imports_growth_2_cp <- eventReactive(input$cp_go, {
      show_percentage(imports_growth_cp())
    })
    
    imports_growth_increase_decrease_cp <- eventReactive(input$cp_go, {
      ifelse(imports_growth_cp() >= 0, "increased", "decreased")
    })
    
    imports_growth_increase_decrease_2_cp <- eventReactive(input$cp_go, {
      ifelse(imports_growth_cp() >= 0, "increase", "decrease")
    })
    
    trade_rankings_cp <- eventReactive(input$cp_go, {
      min_max_y <- c(min(input_cp_y()), max(input_cp_y()))
      
      d <- tbl(con, "yrp") %>% 
        filter(
          year %in% min_max_y &
            reporter_iso == !!input_cp_reporter_iso()
        ) %>% 
        collect()
      
      if (input_cp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(input_cp_convert_dollars()))
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
    
    trade_rankings_no_min_year_cp <- eventReactive(input$cp_go, {
      trade_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == min(input_cp_y()),
          reporter_iso == input_cp_reporter_iso(),
          partner_iso == input_cp_partner_iso()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trade_rankings_no_max_year_cp <- eventReactive(input$cp_go, {
      trade_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == max(input_cp_y()),
          reporter_iso == input_cp_reporter_iso(),
          partner_iso == input_cp_partner_iso()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trade_rankings_remained_cp <- eventReactive(input$cp_go, {
      ifelse(
        trade_rankings_no_min_year_cp() == trade_rankings_no_max_year_cp(),
        "remained", 
        "moved to"
      )
    })
    
    trade_rankings_exp_share_min_year_cp <- eventReactive(input$cp_go, {
      trade_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == min(input_cp_y()),
          reporter_iso == input_cp_reporter_iso(),
          partner_iso == input_cp_partner_iso()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trade_rankings_exp_share_min_year_2_cp <- eventReactive(input$cp_go, {
      show_percentage(trade_rankings_exp_share_min_year_cp())
    })
    
    trade_rankings_exp_share_max_year_cp <- eventReactive(input$cp_go, {
      trade_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == max(input_cp_y()),
          reporter_iso == input_cp_reporter_iso(),
          partner_iso == input_cp_partner_iso()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trade_rankings_exp_share_max_year_2_cp <- eventReactive(input$cp_go, {
      show_percentage(trade_rankings_exp_share_max_year_cp())
    })
    
    trade_rankings_imp_share_min_year_cp <- eventReactive(input$cp_go, {
      trade_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == min(input_cp_y()),
          reporter_iso == input_cp_reporter_iso(),
          partner_iso == input_cp_partner_iso()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trade_rankings_imp_share_min_year_2_cp <- eventReactive(input$cp_go, {
      show_percentage(trade_rankings_imp_share_min_year_cp())
    })
    
    trade_rankings_imp_share_max_year_cp <- eventReactive(input$cp_go, {
      trade_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == max(input_cp_y()),
          reporter_iso == input_cp_reporter_iso(),
          partner_iso == input_cp_partner_iso()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trade_rankings_imp_share_max_year_2_cp <- eventReactive(input$cp_go, {
      waitress_cp$inc(1)
      
      show_percentage(trade_rankings_imp_share_max_year_cp())
    })
    
    ### Text/Visual elements ----
    
    trade_summary_text_exp_cp <- eventReactive(input$cp_go, {
      switch(table_aggregated_cp(),
             "yr" = glue("The exports of { reporter_add_the() } { reporter_name() } to the World { exports_growth_increase_decrease_cp() } from 
                          { exports_value_min_year_2_cp() } in { min(input_cp_y()) } to { exports_value_max_year_2_cp() } in { max(input_cp_y()) } 
                          (annualized { exports_growth_increase_decrease_2_cp() } of { exports_growth_2_cp() })."),
             
             "yrp" = glue("The exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { exports_growth_increase_decrease_cp() } from 
                          { exports_value_min_year_2_cp() } in { min(input_cp_y()) } 
                          to { exports_value_max_year_2_cp() } in { max(input_cp_y()) } (annualized { exports_growth_increase_decrease_2_cp() } of 
                          { exports_growth_2_cp() }). { partner_add_the() } { partner_name() } was the No. { trade_rankings_no_min_year_cp() } trading partner of 
                          { reporter_add_the() } { reporter_name() } in { min(input_cp_y()) } (represented { trade_rankings_exp_share_min_year_2_cp() } of its exports), and 
                          then { trade_rankings_remained_cp() } No. { trade_rankings_no_max_year_cp() } in { max(input_cp_y()) } (represented { trade_rankings_exp_share_max_year_2_cp() } 
                          of its exports).")
      )
    })
    
    trade_summary_text_imp_cp <- eventReactive(input$cp_go, {
      switch(table_aggregated_cp(),
             "yr" = glue("The imports of { reporter_add_the() } { reporter_name() } to the World { imports_growth_increase_decrease_cp() } from 
                         { imports_value_min_year_2_cp() } in { min(input_cp_y()) } to { imports_value_max_year_2_cp() } in { max(input_cp_y()) } 
                         (annualized { imports_growth_increase_decrease_2_cp() } of { imports_growth_2_cp() })."),
             
             "yrp" = glue("The imports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { imports_growth_increase_decrease_cp() } from 
                          { imports_value_min_year_2_cp() } in { min(input_cp_y()) } 
                          to { imports_value_max_year_2_cp() } in { max(input_cp_y()) } (annualized { imports_growth_increase_decrease_2_cp() } of 
                          { imports_growth_2_cp() }). { partner_add_the() } { partner_name() } was the No. { trade_rankings_no_min_year_cp() } trading partner of 
                          { reporter_add_the() } { reporter_name() } in { min(input_cp_y()) } (represented { trade_rankings_imp_share_min_year_2_cp() } of its imports), and 
                          then { trade_rankings_remained_cp() } No. { trade_rankings_no_max_year_cp() } in { max(input_cp_y()) } (represented { trade_rankings_imp_share_max_year_2_cp() } 
                          of its imports).")
      )
    })

    trade_exchange_lines_title_cp <- eventReactive(input$cp_go, {
      switch(table_aggregated_cp(),
             "yr" = glue("{ reporter_add_proper_the() } { reporter_name() } multilateral trade between { min(input_cp_y()) } and { max(input_cp_y()) }"),
             "yrp" = glue("{ reporter_add_proper_the() } { reporter_name() } and { partner_add_the() } { partner_name() } exchange between { min(input_cp_y()) } and { max(input_cp_y()) }")
      )
    })
    
    trade_exchange_lines_aggregated_cp <- eventReactive(input$cp_go, {
      d <- trade_table_aggregated_cp()
      
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

      waitress_cp$inc(1)
      
      hchart(d, 
        "line", 
        hcaes(x = year, y = trade, group = flow),
        tooltip = list(
          pointFormatter = custom_tooltip_short()
        )) %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "USD billion"),
                 labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>% 
        hc_title(text = trade_exchange_lines_title_cp())
    })
    
    ## Exports ----
    
    ### Tables ----
    
    exports_imports_table_origin_destination_year_cp <- eventReactive(input$cp_go, {
      min_max_y <- c(min(input_cp_y()), max(input_cp_y()))
      
      d <- tbl(con, "yrp") %>% 
        filter(
          year %in% min_max_y &
            reporter_iso == !!input_cp_reporter_iso()
        ) %>% 
        collect()
      
      if (input_cp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(input_cp_convert_dollars()))
      }
      
      waitress_cp$inc(1)
      
      return(d)
    })
    
    ### Visual elements ----
    
    exports_title_year_cp <- eventReactive(input$cp_go, {
      switch(
        table_detailed_cp(),
        "yrc" = glue("Exports of { reporter_add_the() } { reporter_name() } to the rest of the World in { min(input_cp_y()) } and { max(input_cp_y()) }, by product"),
        "yrpc" = glue("Exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } in { min(input_cp_y()) } and { max(input_cp_y()) }, by product")
      )
    })
    
    exports_title_min_year_cp <- eventReactive(input$cp_go, {
      glue("{ min(input_cp_y()) }")
    })
    
    exports_treemap_detailed_min_year_cp <- eventReactive(input$cp_go, {
      d <- data_detailed_cp() %>%
        filter(year == min(input_cp_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
      
      d2 <- pd_colors(d)
      
      pd_to_highcharts(d, d2)
    })
    
    exports_title_max_year_cp <- eventReactive(input$cp_go, {
      glue("{ max(input_cp_y()) }")
    })
    
    exports_treemap_detailed_max_year_cp <- eventReactive(input$cp_go, {
      d <- data_detailed_cp() %>%
        filter(year == max(input_cp_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
      
      d2 <- pd_colors(d)
      
      waitress_cp$inc(1)
      
      pd_to_highcharts(d, d2)
    })
    
    ## Imports ----
    
    ### Visual elements ----
    
    imports_title_year_cp <- eventReactive(input$cp_go, {
      switch(
        table_detailed_cp(),
        "yrc" = glue("Imports of { reporter_add_the() } { reporter_name() } from the rest of the World in { min(input_cp_y()) } and { max(input_cp_y()) }, by product"),
        "yrpc" = glue("Imports of { reporter_add_the() } { reporter_name() } from { partner_add_the() } { partner_name() } in { min(input_cp_y()) } and { max(input_cp_y()) }, by product")
      )
    })
    
    imports_title_min_year_cp <- eventReactive(input$cp_go, {
      glue("{ min(input_cp_y()) }")
    })
    
    imports_treemap_origins_min_year_cp <- eventReactive(input$cp_go, {
      d <- exports_imports_table_origin_destination_year_cp() %>%
        filter(year == min(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    imports_treemap_detailed_min_year_cp <- eventReactive(input$cp_go, {
      d <- data_detailed_cp() %>%
        filter(year == min(input_cp_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
      
      d2 <- pd_colors(d)
      
      pd_to_highcharts(d, d2)
    })
    
    imports_title_max_year_cp <- eventReactive(input$cp_go, {
      glue("{ max(input_cp_y()) }")
    })
    
    imports_treemap_origins_max_year_cp <- eventReactive(input$cp_go, {
      d <- exports_imports_table_origin_destination_year_cp() %>%
        filter(year == max(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    imports_treemap_detailed_max_year_cp <- eventReactive(input$cp_go, {
      d <- data_detailed_cp() %>%
        filter(year == max(input_cp_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
      
      d2 <- pd_colors(d)
      
      waitress_cp$inc(2)
      waitress_cp$close() 
      
      pd_to_highcharts(d, d2)
    })
    
    # Product profile ----
    
    waitress_pp <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
    
    ## Data ----
    
    data_detailed_pp <- eventReactive(input$pp_go, {
      waitress_pp$start()
      
      d <- tbl(con, "yrpc") %>% 
        filter(year %in% !!input_pp_y())
      
      waitress_pp$inc(1)
      
      if (input_pp_section_code() != "all") {
        if (input_pp_section_code() == "vaccine") {
          vaccine_codes <- as.character(unlist(read.csv("vaccine_codes.csv")))
          d <- d %>% 
            filter(commodity_code %in% vaccine_codes)
        } else if (nchar(input_pp_section_code()) == 2) {
          d <- d %>% 
            filter(section_code == !!input_pp_section_code()) 
        } else if (nchar(input_pp_section_code()) == 4) {
          d <- d %>% 
            filter(substr(commodity_code, 1, 4) == !!input_pp_section_code()) 
        }
      }
      
      waitress_pp$inc(1)
      
      d <- d %>% 
        group_by(year, reporter_iso, partner_iso) %>% 
        summarize(
          trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T),
          trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)
        ) %>% 
        ungroup() %>% 
        collect()
      
      if (input_pp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(input_pp_convert_dollars()))
      }
      
      waitress_pp$inc(3)
      
      return(d)
    })
    
    ## Trade ----
    
    ### Numbers ----
    
    exports_value_min_year_pp <- eventReactive(input$pp_go, {
      waitress_pp$inc(1)
      
      data_detailed_pp() %>%
        filter(year == min(input_pp_y())) %>%
        select(trade_value_usd_exp) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp)) %>% 
        as.numeric()
    })
    
    exports_value_max_year_pp <- eventReactive(input$pp_go, {
      data_detailed_pp() %>%
        filter(year == max(input_pp_y())) %>%
        select(trade_value_usd_exp) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp)) %>% 
        as.numeric()
    })
    
    imports_value_min_year_pp <- eventReactive(input$pp_go, {
      waitress_pp$inc(1)
      
      data_detailed_pp() %>%
        filter(year == min(input_pp_y())) %>%
        select(trade_value_usd_imp) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp)) %>% 
        as.numeric()
    })
    
    imports_value_max_year_pp <- eventReactive(input$pp_go, {
      data_detailed_pp() %>%
        filter(year == max(input_pp_y())) %>%
        select(trade_value_usd_imp) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp)) %>% 
        as.numeric()
    })
    
    exports_value_min_year_2_pp <- eventReactive(input$pp_go, {
      show_dollars(exports_value_min_year_pp())
    })
    
    exports_value_max_year_2_pp <- eventReactive(input$pp_go, {
      show_dollars(exports_value_max_year_pp())
    })
    
    imports_value_min_year_2_pp <- eventReactive(input$pp_go, {
      show_dollars(imports_value_min_year_pp())
    })
    
    imports_value_max_year_2_pp <- eventReactive(input$pp_go, {
      show_dollars(imports_value_max_year_pp())
    })
    
    exports_growth_pp <- eventReactive(input$pp_go, {
      growth_rate(
        exports_value_max_year_pp(), exports_value_min_year_pp(), input_pp_y()
      )
    })
    
    exports_growth_2_pp <- eventReactive(input$pp_go, {
      show_percentage(exports_growth_pp())
    })
    
    exports_growth_increase_decrease_pp <- eventReactive(input$pp_go, {
      ifelse(exports_growth_pp() >= 0, "increased", "decreased")
    })
    
    exports_growth_increase_decrease_2_pp <- eventReactive(input$pp_go, {
      ifelse(exports_growth_pp() >= 0, "increase", "decrease")
    })
    
    imports_growth_pp <- eventReactive(input$pp_go, {
      growth_rate(
        imports_value_max_year_pp(), imports_value_min_year_pp(), input_pp_y()
      )
    })
    
    imports_growth_2_pp <- eventReactive(input$pp_go, {
      show_percentage(imports_growth_pp())
    })
    
    imports_growth_increase_decrease_pp <- eventReactive(input$pp_go, {
      ifelse(imports_growth_pp() >= 0, "increased", "decreased")
    })
    
    imports_growth_increase_decrease_2_pp <- eventReactive(input$pp_go, {
      ifelse(imports_growth_pp() >= 0, "increase", "decrease")
    })
    
    ### Text/Visual elements ----
    
    trade_summary_text_exp_pp <- eventReactive(input$pp_go, {
      glue("The exports of { section_name_pp() } { exports_growth_increase_decrease_pp() } from 
                          { exports_value_min_year_2_pp() } in { min(input_pp_y()) } to { exports_value_max_year_2_pp() } in { max(input_pp_y()) } 
                          (annualized { exports_growth_increase_decrease_2_pp() } of { exports_growth_2_pp() }).")
    })
    
    trade_summary_text_imp_pp <- eventReactive(input$pp_go, {
      glue("The imports of { section_name_pp() } { imports_growth_increase_decrease_pp() } from 
                          { imports_value_min_year_2_pp() } in { min(input_pp_y()) } to { imports_value_max_year_2_pp() } in { max(input_pp_y()) } 
                          (annualized { imports_growth_increase_decrease_2_pp() } of { imports_growth_2_pp() }).")
    })
    
    trade_exchange_columns_title_pp <- eventReactive(input$pp_go, {
      glue("{ section_name_pp() } exchange in { min(input_cp_y()) } and { max(input_cp_y()) }")
    })
    
    trade_exchange_columns_pp <- eventReactive(input$pp_go, {
      d <- data_detailed_pp() %>% 
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
      
      waitress_pp$inc(3)
      waitress_pp$close()
      
      hchart(d, 
             "column", 
             hcaes(x = year, y = trade, group = flow),
             tooltip = list(
               pointFormatter = custom_tooltip_short()
             )) %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "USD billion"),
                 labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>% 
        hc_title(text = trade_exchange_columns_title_pp())
    })
    
    ## Exports ----
    
    ### Text/Visual elements ----
    
    exports_title_year_pp <- eventReactive(input$pp_go, {
      glue("Exports of { section_name_pp() } in { min(input_pp_y()) } and { max(input_pp_y()) }, by country")
    })
    
    exports_title_min_year_pp <- eventReactive(input$pp_go, {
      glue("{ min(input_pp_y()) }")
    })
    
    exports_treemap_origins_min_year_pp <- eventReactive(input$pp_go, {
      d <- data_detailed_pp() %>%
        filter(year == min(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_exp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    exports_title_max_year_pp <- eventReactive(input$pp_go, {
      glue("{ max(input_pp_y()) }")
    })
    
    exports_treemap_origins_max_year_pp <- eventReactive(input$pp_go, {
      d <- data_detailed_pp() %>%
        filter(year == max(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_exp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    ## Imports ----
    
    ### Text/Visual elements ----
    
    imports_title_year_pp <- eventReactive(input$pp_go, {
      glue("Imports of { section_name_pp() } in { min(input_pp_y()) } and { max(input_pp_y()) }, by country")
    })
    
    imports_title_min_year_pp <- eventReactive(input$pp_go, {
      glue("{ min(input_pp_y()) }")
    })
    
    imports_treemap_origins_min_year_pp <- eventReactive(input$pp_go, {
      d <- data_detailed_pp() %>%
        filter(year == min(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    imports_title_max_year_pp <- eventReactive(input$pp_go, {
      glue("{ max(input_pp_y()) }")
    })
    
    imports_treemap_origins_max_year_pp <- eventReactive(input$pp_go, {
      d <- data_detailed_pp() %>%
        filter(year == max(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    })
    
    # Model ----
  
    waitress_model <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
    
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
      waitress_model$start()
      
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
      
      waitress_model$inc(1)
      
      if (input_model_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(input_model_convert_dollars()))
      }
      
      # 2. apply filters
      
      if (!any(input_model_product_filter() %in% "all")) {
        if (any(input_model_product_filter() %in% "vaccine")) {
          vaccine_codes <- as.character(unlist(read.csv("vaccine_codes.csv")))
          d <- d %>% 
            mutate(
              section_code = case_when(
                commodity_code %in% vaccine_codes ~ "vaccine",
                TRUE ~ section_code
              )
            )
        }
        
        waitress_model$inc(1)
        
        d <- d %>% 
          filter(section_name %in% !!input_model_product_filter())
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
        
        waitress_model$inc(3)
        
        d <- d %>% 
          select(-c(total_e, total_y, remoteness_exp, remoteness_imp))
      }
      
      # 7. create fixed effects
      
      if (input_model_type() == "olsfe") {
        waitress_model$inc(3)
        
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
      
      waitress_model$inc(1)
      
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
      
      waitress_model$inc(1)
      waitress_model$close() 
      
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
    
    output$title_cp <- renderText({
      title_cp()
    })
    
    output$title_pp <- renderText({
      title_pp()
    })
    
    output$title_model <- renderText({
      "Gravity Models"
    })
    
    legend_text <- "The information displayed here is based on <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. Please read our <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a> for a full description
      of restrictions and applicable licenses. These figures do not include services or foreign direct investment."
    
    output$title_cp_legend <- renderText({ legend_text })
    
    output$title_model_legend <- renderText({ legend_text })
    
    # Country profile output ----
    
    ## Trade output ----
    
    output$trade_subtitle_cp <- eventReactive(input$cp_go, {
      switch(
        table_detailed_cp(),
        "yrc" = glue("Total multilateral Exports and Imports { min(input_cp_y()) }-{ max(input_cp_y()) }"),
        "yrpc" = glue("Total bilateral Exports and Imports { min(input_cp_y()) }-{ max(input_cp_y()) }")
      )
    })
    
    output$trade_subtitle_exp_cp <- eventReactive(input$cp_go, {
      "Exports"
    })
    
    output$trade_subtitle_imp_cp <- eventReactive(input$cp_go, {
      "Imports"
    })
    
    output$trade_summary_exp_cp <- renderText(trade_summary_text_exp_cp())
    output$trade_summary_imp_cp <- renderText(trade_summary_text_imp_cp())
    
    output$trade_exchange_lines_aggregated_cp <- renderHighchart({
      trade_exchange_lines_aggregated_cp()
    })
    
    ## Exports output ----
    
    output$exports_title_year_cp <- renderText(exports_title_year_cp())
    output$exports_title_min_year_cp <- renderText(exports_title_min_year_cp())
    output$exports_treemap_detailed_min_year_cp <- renderHighchart({exports_treemap_detailed_min_year_cp()})
    output$exports_title_max_year_cp <- renderText(exports_title_max_year_cp())
    output$exports_treemap_detailed_max_year_cp <- renderHighchart({exports_treemap_detailed_max_year_cp()})
    
    ## Imports output ----
    
    output$imports_title_year_cp <- renderText(imports_title_year_cp())
    output$imports_title_min_year_cp <- renderText(imports_title_min_year_cp())
    output$imports_treemap_detailed_min_year_cp <- renderHighchart({imports_treemap_detailed_min_year_cp()})
    output$imports_title_max_year_cp <- renderText(imports_title_max_year_cp())
    output$imports_treemap_detailed_max_year_cp <- renderHighchart({imports_treemap_detailed_max_year_cp()})
    
    # Product profile output ----
    
    output$trade_subtitle_pp <- eventReactive(input$pp_go, {
      glue("Total multilateral Exports and Imports { min(input_cp_y()) } and { max(input_cp_y()) }")
    })
    
    output$trade_subtitle_exp_pp <- eventReactive(input$pp_go, {
      "Exports"
    })
    
    output$trade_subtitle_imp_pp <- eventReactive(input$pp_go, {
      "Imports"
    })
    
    output$trade_summary_exp_pp <- renderText(trade_summary_text_exp_pp())
    output$trade_summary_imp_pp <- renderText(trade_summary_text_imp_pp())
    
    output$trade_exchange_columns_pp <- renderHighchart({
      trade_exchange_columns_pp()
    })
    
    output$imports_title_year_pp <- renderText(imports_title_year_pp())
    output$imports_title_min_year_pp <- renderText(imports_title_min_year_pp())
    output$imports_title_max_year_pp <- renderText(imports_title_max_year_pp())
    output$imports_treemap_origins_min_year_pp <- renderHighchart({imports_treemap_origins_min_year_pp()})
    output$imports_treemap_origins_max_year_pp <- renderHighchart({imports_treemap_origins_max_year_pp()})
    
    output$exports_title_year_pp <- renderText(exports_title_year_pp())
    output$exports_title_min_year_pp <- renderText(exports_title_min_year_pp())
    output$exports_title_max_year_pp <- renderText(exports_title_max_year_pp())
    output$exports_treemap_origins_min_year_pp <- renderHighchart({exports_treemap_origins_min_year_pp()})
    output$exports_treemap_origins_max_year_pp <- renderHighchart({exports_treemap_origins_max_year_pp()})
    
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
    
    download_cp_subtitle <- eventReactive(input$cp_go, {
      "Download country profile data"
    })
    
    download_cp_text <- eventReactive(input$cp_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    download_cp_format <- eventReactive(input$cp_go, {
      selectInput(
        "cp_f",
        "Download data as:",
        choices = available_formats,
        selected = NULL,
        selectize = TRUE
      )
    })
    
    output$download_cp_aggregated_pre <- downloadHandler(
        filename = function() {
          glue("{ table_aggregated_cp() }_{ input_cp_reporter_iso() }_{ input_cp_partner_iso() }_{ min(input_cp_y()) }_{ max(input_cp_y()) }.{ input_cp_format() }")
        },
        content = function(filename) {
          rio::export(data_aggregated_cp(), filename)
        },
        contentType = "application/zip"
      )
    
    output$download_cp_detailed_pre <- downloadHandler(
      filename = function() {
        glue("{ table_detailed_cp() }_{ input_cp_reporter_iso() }_{ input_cp_partner_iso() }_{ min(input_cp_y()) }_{ max(input_cp_y()) }.{ input_cp_format() }")
      },
      content = function(filename) {
        rio::export(data_detailed_cp(), filename)
      },
      contentType = "application/zip"
    )
    
    output$download_cp_subtitle <- renderText({download_cp_subtitle()})
    output$download_cp_text <- renderText({download_cp_text()})
    output$download_cp_format <- renderUI({download_cp_format()})
    output$download_cp_aggregated <- renderUI({
      req(input$cp_go)
      downloadButton('download_cp_aggregated_pre', label = 'Aggregated data')
    })
    output$download_cp_detailed <- renderUI({
      req(input$cp_go)
      downloadButton('download_cp_detailed_pre', label = 'Detailed data')
    })
    
    ## Product profile ----
    
    download_pp_subtitle <- eventReactive(input$pp_go, {
      "Download product profile data"
    })
    
    download_pp_text <- eventReactive(input$pp_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    download_pp_format <- eventReactive(input$pp_go, {
      selectInput(
        "pp_f",
        "Download data as:",
        choices = available_formats,
        selected = NULL,
        selectize = TRUE
      )
    })
    
    output$download_pp_pre <- downloadHandler(
      filename = function() {
        glue("yrpc_{ input_pp_section_code() }_{ min(input_pp_y()) }_{ max(input_pp_y()) }.{ input_pp_format() }")
      },
      content = function(filename) {
        rio::export(data_detailed_pp(), filename)
      },
      contentType = "application/zip"
    )
    
    output$download_pp_subtitle <- renderText({download_pp_subtitle()})
    output$download_pp_text <- renderText({download_pp_text()})
    output$download_pp_format <- renderUI({download_pp_format()})
    output$download_pp_aggregated <- renderUI({
      req(input$pp_go)
      downloadButton('download_pp_pre', label = 'Aggregated data')
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
