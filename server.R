## server.R ##

shinyServer(
  function(input, output, session) {
    # User inputs ----
    
    ## Country profile ----
    
    inp_cp_y <- reactive({
      y <- (min(input$cp_y[1], input$cp_y[2])):(max(input$cp_y[1], input$cp_y[2]))
      y <- seq(min(y), max(y), by = ifelse(max(y) - min(y) >= 10, 2, 1))
      return(y)
    })
    
    inp_cp_r <- reactive({ input$cp_r })
    inp_cp_p <- reactive({ input$cp_p })
    
    inp_cp_f <- reactive({ input$cp_f })
    
    inp_cp_convert_dollars <- reactive({ input$cp_a })
    
    table_agg_cp <- eventReactive(input$cp_go, {
      ifelse(inp_cp_p() == "all", "yr", "yrp")
    })
    
    tbl_dtl_cp <- eventReactive(input$cp_go, {
      ifelse(inp_cp_p() == "all", "yrc", "yrpc")
    })
    
    reporter_name <- eventReactive(input$cp_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == inp_cp_r()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    partner_name <- eventReactive(input$cp_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == inp_cp_p()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    ## Product profile ----
    
    updateSelectizeInput(
      session,
      inputId = "pp_s",
      choices = list(
        "All Products" = available_all,
        "Custom Selections" = available_vaccine,
        "HS Sections" = available_sections_code,
        "HS Commodities" = available_commodities_code
      ),
      server = TRUE
    )
    
    inp_pp_y <- reactive({
      y <- (min(input$pp_y[1], input$pp_y[2])):(max(input$pp_y[1], input$pp_y[2]))
      y <- c(min(y), max(y))
      return(y)
    })
    
    inp_pp_section_code <- reactive({ input$pp_s })
    
    inp_pp_convert_dollars <- reactive({ input$pp_a })
    
    inp_pp_f <- reactive({ input$pp_f })
    
    section_name_pp <- eventReactive(input$pp_go, {
      s <- if (nchar(inp_pp_section_code()) == 2) {
        sections_to_display %>%
          filter(section_code == inp_pp_section_code()) %>%
          select(section_fullname_english) %>%
          as.character()
      } else if (nchar(inp_pp_section_code()) == 4) {
        commodities_to_display %>%
          filter(commodity_code == inp_pp_section_code()) %>%
          select(commodity_fullname_english) %>%
          as.character()
      } else if (inp_pp_section_code() == "vaccine") {
        "Vaccine Inputs"
      } else if (inp_pp_section_code() == "all") {
        "All Products"
      }
      
      return(s)
    })
    
    ## Model ----
    
    inp_md_y <- reactive({
      y2 <- (min(input$md_y[1], input$md_y[2])):(max(input$md_y[1], input$md_y[2]))
      y2 <- seq(min(y2), max(y2), by = input$md_y_sep)
      return(y2)
    })
    
    inp_md_riso <- reactive({ input$md_r })
    inp_md_piso <- reactive({ input$md_p })
    
    inp_md_convert_dollars <- reactive({ input$md_a })
    
    inp_md_product_filter <- reactive({ input$md_pf })
    inp_md_type <- reactive({ input$md_t })
    inp_md_dist <- reactive({ input$md_d })
    inp_md_bin <- reactive({ input$md_b })
    inp_md_ctn <- reactive({ input$md_ct })
    inp_md_cluster <- reactive({ input$md_cl })
    inp_md_custom_subset <- reactive({ input$md_s })
    
    inp_md_f <- reactive({ input$md_f })
    
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
        tbl_dtl_cp(),
        "yrc" = glue("{ reporter_add_proper_the() } { reporter_name() } multilateral trade between { min(inp_cp_y()) } and { max(inp_cp_y()) }"),
        "yrpc" = glue("{ reporter_add_proper_the() } { reporter_name() } and { partner_add_the() } { partner_name() } between { min(inp_cp_y()) } and { max(inp_cp_y()) }")
      )
    })
    
    title_pp <- eventReactive(input$pp_go, {
      glue("{ section_name_pp() } multilateral trade in { min(inp_pp_y()) } and { max(inp_pp_y()) }")
    })
    
    # Country profile ----
    
    wt_cp <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
    
    ## Data ----
    
    df_agg_cp <- reactive({
      wt_cp$start()
      
      d <- tbl(con, table_agg_cp())
      
      if (inp_cp_p() == "all") {
        d <- d %>% 
          filter(
            year %in% !!inp_cp_y() &
              reporter_iso == !!inp_cp_r()
          )
      } else {
        d <- d %>% 
          filter(
            year %in% !!inp_cp_y() &
              reporter_iso == !!inp_cp_r() &
              partner_iso == !!inp_cp_p()
          )
      }
      
      d <- d %>% collect()

      if (inp_cp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cp_convert_dollars()))
      }
      
      wt_cp$inc(2)
      
      return(d)
    }) %>% 
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars()) %>% 
      bindEvent(input$cp_go)
    
    df_dtl_cp <- reactive({
      d <- tbl(con, tbl_dtl_cp())
          
      if (inp_cp_p() == "all") {
        d <- d %>% 
          filter(
            year %in% !!inp_cp_y() &
              reporter_iso == !!inp_cp_r()
          )
      } else {
        d <- d %>% 
          filter(
            year %in% !!inp_cp_y() &
              reporter_iso == !!inp_cp_r() &
              partner_iso == !!inp_cp_p()
          )
      }
          
      d <- d %>% collect()
      
      if (inp_cp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cp_convert_dollars()))
      }
      
      wt_cp$inc(2)
      
      return(d)
    }) %>% 
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars(),
                tbl_dtl_cp()) %>% 
      bindEvent(input$cp_go)
    
    ## Trade ----
    
    ### Tables ----
    
    tr_tbl_agg_cp <- eventReactive(input$cp_go, {
      df_agg_cp() %>%
        select(year, trade_value_usd_exp, trade_value_usd_imp)
    })
    
    exp_val_min_yr_cp <- eventReactive(input$cp_go, {
      tr_tbl_agg_cp() %>%
        filter(year == min(inp_cp_y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    exp_val_max_yr_cp <- eventReactive(input$cp_go, {
      tr_tbl_agg_cp() %>%
        filter(year == max(inp_cp_y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    imp_val_min_yr_cp <- eventReactive(input$cp_go, {
      tr_tbl_agg_cp() %>%
        filter(year == min(inp_cp_y())) %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })
    
    imp_val_max_yr_cp <- eventReactive(input$cp_go, {
      tr_tbl_agg_cp() %>%
        filter(year == max(inp_cp_y())) %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })
    
    exp_val_min_yr_2_cp <- eventReactive(input$cp_go, {
      show_dollars(exp_val_min_yr_cp())
    })
    
    exp_val_max_yr_2_cp <- eventReactive(input$cp_go, {
      show_dollars(exp_val_max_yr_cp())
    })
    
    imp_val_min_yr_2_cp <- eventReactive(input$cp_go, {
      show_dollars(imp_val_min_yr_cp())
    })
    
    imp_val_max_yr_2_cp <- eventReactive(input$cp_go, {
      show_dollars(imp_val_max_yr_cp())
    })
    
    exports_growth_cp <- eventReactive(input$cp_go, {
      growth_rate(
        exp_val_max_yr_cp(), exp_val_min_yr_cp(), inp_cp_y()
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
        imp_val_max_yr_cp(), imp_val_min_yr_cp(), inp_cp_y()
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
    
    trd_rankings_cp <- eventReactive(input$cp_go, {
      min_max_y <- c(min(inp_cp_y()), max(inp_cp_y()))
      
      d <- tbl(con, "yrp") %>% 
        filter(
          year %in% min_max_y &
            reporter_iso == !!inp_cp_r()
        ) %>% 
        collect()
      
      if (inp_cp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cp_convert_dollars()))
      }
      
      d <- d %>% 
        # filter(partner_iso != "0-unspecified") %>% 
        mutate(
          trd_value_usd_bal = trade_value_usd_exp + trade_value_usd_imp
        ) %>% 
        group_by(year) %>% 
        mutate(
          bal_rank = dense_rank(desc(trd_value_usd_bal)),
          exp_share = trade_value_usd_exp / sum(trade_value_usd_exp),
          imp_share = trade_value_usd_imp / sum(trade_value_usd_imp)
        )
      
      return(d)
    })
    
    trd_rankings_no_min_yr_cp <- eventReactive(input$cp_go, {
      trd_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == min(inp_cp_y()),
          reporter_iso == inp_cp_r(),
          partner_iso == inp_cp_p()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trd_rankings_no_max_yr_cp <- eventReactive(input$cp_go, {
      trd_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == max(inp_cp_y()),
          reporter_iso == inp_cp_r(),
          partner_iso == inp_cp_p()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trd_rankings_remained_cp <- eventReactive(input$cp_go, {
      ifelse(
        trd_rankings_no_min_yr_cp() == trd_rankings_no_max_yr_cp(),
        "remained", 
        "moved to"
      )
    })
    
    trd_rankings_exp_share_min_yr_cp <- eventReactive(input$cp_go, {
      trd_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == min(inp_cp_y()),
          reporter_iso == inp_cp_r(),
          partner_iso == inp_cp_p()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trd_rankings_exp_share_min_yr_2_cp <- eventReactive(input$cp_go, {
      show_percentage(trd_rankings_exp_share_min_yr_cp())
    })
    
    trd_rankings_exp_share_max_yr_cp <- eventReactive(input$cp_go, {
      trd_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == max(inp_cp_y()),
          reporter_iso == inp_cp_r(),
          partner_iso == inp_cp_p()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trd_rankings_exp_share_max_yr_2_cp <- eventReactive(input$cp_go, {
      show_percentage(trd_rankings_exp_share_max_yr_cp())
    })
    
    trd_rankings_imp_share_min_yr_cp <- eventReactive(input$cp_go, {
      trd_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == min(inp_cp_y()),
          reporter_iso == inp_cp_r(),
          partner_iso == inp_cp_p()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trd_rankings_imp_share_min_yr_2_cp <- eventReactive(input$cp_go, {
      show_percentage(trd_rankings_imp_share_min_yr_cp())
    })
    
    trd_rankings_imp_share_max_yr_cp <- eventReactive(input$cp_go, {
      trd_rankings_cp() %>%
        ungroup() %>% 
        filter(
          year == max(inp_cp_y()),
          reporter_iso == inp_cp_r(),
          partner_iso == inp_cp_p()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trd_rankings_imp_share_max_yr_2_cp <- eventReactive(input$cp_go, {
      wt_cp$inc(1)
      
      show_percentage(trd_rankings_imp_share_max_yr_cp())
    })
    
    ### Text/Visual elements ----
    
    trd_smr_text_exp_cp <- eventReactive(input$cp_go, {
      switch(table_agg_cp(),
             "yr" = glue("The exports of { reporter_add_the() } { reporter_name() } to the World { exports_growth_increase_decrease_cp() } from 
                          { exp_val_min_yr_2_cp() } in { min(inp_cp_y()) } to { exp_val_max_yr_2_cp() } in { max(inp_cp_y()) } 
                          (annualized { exports_growth_increase_decrease_2_cp() } of { exports_growth_2_cp() })."),
             
             "yrp" = glue("The exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { exports_growth_increase_decrease_cp() } from 
                          { exp_val_min_yr_2_cp() } in { min(inp_cp_y()) } 
                          to { exp_val_max_yr_2_cp() } in { max(inp_cp_y()) } (annualized { exports_growth_increase_decrease_2_cp() } of 
                          { exports_growth_2_cp() }). { partner_add_the() } { partner_name() } was the No. { trd_rankings_no_min_yr_cp() } trading partner of 
                          { reporter_add_the() } { reporter_name() } in { min(inp_cp_y()) } (represented { trd_rankings_exp_share_min_yr_2_cp() } of its exports), and 
                          then { trd_rankings_remained_cp() } No. { trd_rankings_no_max_yr_cp() } in { max(inp_cp_y()) } (represented { trd_rankings_exp_share_max_yr_2_cp() } 
                          of its exports).")
      )
    })
    
    trd_smr_text_imp_cp <- eventReactive(input$cp_go, {
      switch(table_agg_cp(),
             "yr" = glue("The imports of { reporter_add_the() } { reporter_name() } to the World { imports_growth_increase_decrease_cp() } from 
                         { imp_val_min_yr_2_cp() } in { min(inp_cp_y()) } to { imp_val_max_yr_2_cp() } in { max(inp_cp_y()) } 
                         (annualized { imports_growth_increase_decrease_2_cp() } of { imports_growth_2_cp() })."),
             
             "yrp" = glue("The imports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { imports_growth_increase_decrease_cp() } from 
                          { imp_val_min_yr_2_cp() } in { min(inp_cp_y()) } 
                          to { imp_val_max_yr_2_cp() } in { max(inp_cp_y()) } (annualized { imports_growth_increase_decrease_2_cp() } of 
                          { imports_growth_2_cp() }). { partner_add_the() } { partner_name() } was the No. { trd_rankings_no_min_yr_cp() } trading partner of 
                          { reporter_add_the() } { reporter_name() } in { min(inp_cp_y()) } (represented { trd_rankings_imp_share_min_yr_2_cp() } of its imports), and 
                          then { trd_rankings_remained_cp() } No. { trd_rankings_no_max_yr_cp() } in { max(inp_cp_y()) } (represented { trd_rankings_imp_share_max_yr_2_cp() } 
                          of its imports).")
      )
    })

    trd_exc_lines_title_cp <- eventReactive(input$cp_go, {
      switch(table_agg_cp(),
             "yr" = glue("{ reporter_add_proper_the() } { reporter_name() } multilateral trade between { min(inp_cp_y()) } and { max(inp_cp_y()) }"),
             "yrp" = glue("{ reporter_add_proper_the() } { reporter_name() } and { partner_add_the() } { partner_name() } exchange between { min(inp_cp_y()) } and { max(inp_cp_y()) }")
      )
    })
    
    trd_exc_lines_agg_cp <- reactive({
      d <- tr_tbl_agg_cp()
      
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

      wt_cp$inc(1)
      
      hchart(d, 
        "line", 
        hcaes(x = year, y = trade, group = flow),
        tooltip = list(
          pointFormatter = custom_tooltip_short()
        )) %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "USD billion"),
                 labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>% 
        hc_title(text = trd_exc_lines_title_cp())
    }) %>%
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars()) %>% 
      bindEvent(input$cp_go)
    
    ## Exports ----
    
    ### Tables ----
    
    exports_imports_table_origin_destination_yr_cp <- reactive({
      min_max_y <- c(min(inp_cp_y()), max(inp_cp_y()))
      
      d <- tbl(con, "yrp") %>% 
        filter(
          year %in% min_max_y &
            reporter_iso == !!inp_cp_r()
        ) %>% 
        collect()
      
      if (inp_cp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cp_convert_dollars()))
      }
      
      wt_cp$inc(1)
      
      return(d)
    }) %>% 
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars()) %>% 
      bindEvent(input$cp_go)
    
    ### Visual elements ----
    
    exp_tt_yr_cp <- eventReactive(input$cp_go, {
      switch(
        tbl_dtl_cp(),
        "yrc" = glue("Exports of { reporter_add_the() } { reporter_name() } to the rest of the World in { min(inp_cp_y()) } and { max(inp_cp_y()) }, by product"),
        "yrpc" = glue("Exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } in { min(inp_cp_y()) } and { max(inp_cp_y()) }, by product")
      )
    })
    
    exp_tt_min_yr_cp <- eventReactive(input$cp_go, {
      glue("{ min(inp_cp_y()) }")
    })
    
    exp_tm_dtl_min_yr_cp <- reactive({
      d <- df_dtl_cp() %>%
        filter(year == min(inp_cp_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
      
      d2 <- pd_colors(d)
      
      pd_to_highcharts(d, d2)
    }) %>% 
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars()) %>% 
      bindEvent(input$cp_go)
    
    exp_tt_max_yr_cp <- eventReactive(input$cp_go, {
      glue("{ max(inp_cp_y()) }")
    })
    
    exp_tm_dtl_max_yr_cp <- reactive({
      d <- df_dtl_cp() %>%
        filter(year == max(inp_cp_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
      
      d2 <- pd_colors(d)
      
      wt_cp$inc(1)
      
      pd_to_highcharts(d, d2)
    }) %>% 
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars()) %>% 
      bindEvent(input$cp_go)
    
    ## Imports ----
    
    ### Visual elements ----
    
    imp_tt_yr_cp <- eventReactive(input$cp_go, {
      switch(
        tbl_dtl_cp(),
        "yrc" = glue("Imports of { reporter_add_the() } { reporter_name() } from the rest of the World in { min(inp_cp_y()) } and { max(inp_cp_y()) }, by product"),
        "yrpc" = glue("Imports of { reporter_add_the() } { reporter_name() } from { partner_add_the() } { partner_name() } in { min(inp_cp_y()) } and { max(inp_cp_y()) }, by product")
      )
    })
    
    imp_tt_min_yr_cp <- eventReactive(input$cp_go, {
      glue("{ min(inp_cp_y()) }")
    })
    
    imp_tm_ori_min_yr_cp <- reactive({
      d <- exports_imports_table_origin_destination_yr_cp() %>%
        filter(year == min(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    }) %>% 
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars()) %>% 
      bindEvent(input$cp_go)
    
    imp_tm_dtl_min_yr_cp <- reactive({
      d <- df_dtl_cp() %>%
        filter(year == min(inp_cp_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
      
      d2 <- pd_colors(d)
      
      pd_to_highcharts(d, d2)
    }) %>% 
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars()) %>% 
      bindEvent(input$cp_go)
    
    imp_tt_max_yr_cp <- eventReactive(input$cp_go, {
      glue("{ max(inp_cp_y()) }")
    })
    
    imp_tm_ori_max_yr_cp <- reactive({
      d <- exports_imports_table_origin_destination_yr_cp() %>%
        filter(year == max(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      od_to_highcharts(d, d2)
    }) %>% 
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars()) %>% 
      bindEvent(input$cp_go)
    
    imp_tm_dtl_max_yr_cp <- reactive({
      d <- df_dtl_cp() %>%
        filter(year == max(inp_cp_y())) %>% 
        pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
      
      d2 <- pd_colors(d)
      
      wt_cp$inc(2)
    
      out <- pd_to_highcharts(d, d2)
      
      wt_cp$close()
      return(out)
    }) %>% 
      bindCache(inp_cp_y(), inp_cp_r(), inp_cp_p(), inp_cp_convert_dollars()) %>% 
      bindEvent(input$cp_go)
    
    # Product profile ----
    
    wt_pp <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
    
    ## Data ----
    
    df_dtl_pp <- reactive({
      wt_pp$start()
      
      d <- tbl(con, "yrpc") %>% 
        filter(year %in% !!inp_pp_y())
      
      wt_pp$inc(1)
      
      if (inp_pp_section_code() != "all") {
        if (inp_pp_section_code() == "vaccine") {
          vaccine_codes <- as.character(unlist(read.csv("vaccine_codes.csv")))
          d <- d %>% 
            filter(commodity_code %in% vaccine_codes)
        } else if (nchar(inp_pp_section_code()) == 2) {
          d <- d %>% 
            filter(section_code == !!inp_pp_section_code()) 
        } else if (nchar(inp_pp_section_code()) == 4) {
          d <- d %>% 
            filter(substr(commodity_code, 1, 4) == !!inp_pp_section_code()) 
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
      
      if (inp_pp_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_pp_convert_dollars()))
      }
      
      wt_pp$inc(3)
      
      return(d)
    }) %>% 
      bindCache(inp_pp_y(), inp_pp_section_code(), inp_pp_convert_dollars()) %>% 
      bindEvent(input$pp_go)
    
    ## Trade ----
    
    ### Numbers ----
    
    exp_val_min_yr_pp <- eventReactive(input$pp_go, {
      wt_pp$inc(1)
      
      df_dtl_pp() %>%
        filter(year == min(inp_pp_y())) %>%
        select(trade_value_usd_exp) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp)) %>% 
        as.numeric()
    })
    
    exp_val_max_yr_pp <- eventReactive(input$pp_go, {
      df_dtl_pp() %>%
        filter(year == max(inp_pp_y())) %>%
        select(trade_value_usd_exp) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp)) %>% 
        as.numeric()
    })
    
    imp_val_min_yr_pp <- eventReactive(input$pp_go, {
      wt_pp$inc(1)
      
      df_dtl_pp() %>%
        filter(year == min(inp_pp_y())) %>%
        select(trade_value_usd_imp) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp)) %>% 
        as.numeric()
    })
    
    imp_val_max_yr_pp <- eventReactive(input$pp_go, {
      df_dtl_pp() %>%
        filter(year == max(inp_pp_y())) %>%
        select(trade_value_usd_imp) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp)) %>% 
        as.numeric()
    })
    
    exp_val_min_yr_2_pp <- eventReactive(input$pp_go, {
      show_dollars(exp_val_min_yr_pp())
    })
    
    exp_val_max_yr_2_pp <- eventReactive(input$pp_go, {
      show_dollars(exp_val_max_yr_pp())
    })
    
    imp_val_min_yr_2_pp <- eventReactive(input$pp_go, {
      show_dollars(imp_val_min_yr_pp())
    })
    
    imp_val_max_yr_2_pp <- eventReactive(input$pp_go, {
      show_dollars(imp_val_max_yr_pp())
    })
    
    exports_growth_pp <- eventReactive(input$pp_go, {
      growth_rate(
        exp_val_max_yr_pp(), exp_val_min_yr_pp(), inp_pp_y()
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
        imp_val_max_yr_pp(), imp_val_min_yr_pp(), inp_pp_y()
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
    
    trd_smr_text_exp_pp <- eventReactive(input$pp_go, {
      glue("The exports of { section_name_pp() } { exports_growth_increase_decrease_pp() } from 
                          { exp_val_min_yr_2_pp() } in { min(inp_pp_y()) } to { exp_val_max_yr_2_pp() } in { max(inp_pp_y()) } 
                          (annualized { exports_growth_increase_decrease_2_pp() } of { exports_growth_2_pp() }).")
    })
    
    trd_smr_text_imp_pp <- eventReactive(input$pp_go, {
      glue("The imports of { section_name_pp() } { imports_growth_increase_decrease_pp() } from 
                          { imp_val_min_yr_2_pp() } in { min(inp_pp_y()) } to { imp_val_max_yr_2_pp() } in { max(inp_pp_y()) } 
                          (annualized { imports_growth_increase_decrease_2_pp() } of { imports_growth_2_pp() }).")
    })
    
    trd_exc_columns_title_pp <- eventReactive(input$pp_go, {
      glue("{ section_name_pp() } exchange in { min(inp_cp_y()) } and { max(inp_cp_y()) }")
    })
    
    trd_exc_columns_pp <- reactive({
      d <- df_dtl_pp() %>% 
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
        hc_title(text = trd_exc_columns_title_pp())
    }) %>% 
      bindCache(inp_pp_y(), inp_pp_section_code(), inp_pp_convert_dollars()) %>% 
      bindEvent(input$pp_go)
    
    ## Exports ----
    
    ### Text/Visual elements ----
    
    exp_tt_yr_pp <- eventReactive(input$pp_go, {
      glue("Exports of { section_name_pp() } in { min(inp_pp_y()) } and { max(inp_pp_y()) }, by country")
    })
    
    exp_tt_min_yr_pp <- eventReactive(input$pp_go, {
      glue("{ min(inp_pp_y()) }")
    })
    
    exp_tm_ori_min_yr_pp <- reactive({
      d <- df_dtl_pp() %>%
        filter(year == min(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_exp")
      
      d2 <- od_colors(d)
      
      wt_pp$inc(1)
      
      od_to_highcharts(d, d2)
    }) %>% 
      bindCache(inp_pp_y(), inp_pp_section_code(), inp_pp_convert_dollars()) %>% 
      bindEvent(input$pp_go)
    
    exp_tt_max_yr_pp <- eventReactive(input$pp_go, {
      glue("{ max(inp_pp_y()) }")
    })
    
    exp_tm_ori_max_yr_pp <- reactive({
      d <- df_dtl_pp() %>%
        filter(year == max(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_exp")
      
      d2 <- od_colors(d)
      
      wt_pp$inc(1)
      
      od_to_highcharts(d, d2)
    }) %>% 
      bindCache(inp_pp_y(), inp_pp_section_code(), inp_pp_convert_dollars()) %>% 
      bindEvent(input$pp_go)
    
    ## Imports ----
    
    ### Text/Visual elements ----
    
    imp_tt_yr_pp <- eventReactive(input$pp_go, {
      glue("Imports of { section_name_pp() } in { min(inp_pp_y()) } and { max(inp_pp_y()) }, by country")
    })
    
    imp_tt_min_yr_pp <- eventReactive(input$pp_go, {
      glue("{ min(inp_pp_y()) }")
    })
    
    imp_tm_ori_min_yr_pp <- reactive({
      d <- df_dtl_pp() %>%
        filter(year == min(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      wt_pp$inc(1)
      
      od_to_highcharts(d, d2)
    }) %>% 
      bindCache(inp_pp_y(), inp_pp_section_code(), inp_pp_convert_dollars()) %>% 
      bindEvent(input$pp_go)
    
    imp_tt_max_yr_pp <- eventReactive(input$pp_go, {
      glue("{ max(inp_pp_y()) }")
    })
    
    imp_tm_ori_max_yr_pp <- reactive({
      d <- df_dtl_pp() %>%
        filter(year == max(year)) %>% 
        od_order_and_add_continent(col = "trade_value_usd_imp")
      
      d2 <- od_colors(d)
      
      wt_pp$inc(1)
      
      out <- od_to_highcharts(d, d2)
      
      wt_pp$close()
      return(out)
    }) %>% 
      bindCache(inp_pp_y(), inp_pp_section_code(), inp_pp_convert_dollars()) %>% 
      bindEvent(input$pp_go)
    
    # Model ----
  
    wt_md <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
    
    md_custom_data <- eventReactive(input$md_go, {
      uploaded_file <- input$md_own
      
      if(!is.null(uploaded_file)) {
        inp_data <- rio::import(file = uploaded_file$datapath, format = tools::file_ext(uploaded_file$name)) %>%
          janitor::clean_names()
        
        return(inp_data)
      } else {
        data.frame()
      }
    })
    
    df_dtl_md <- eventReactive(input$md_go, {
      wt_md$start()
      
      ## 1. read from SQL ----
      
      d <- tbl(con, "yrpc")
          
      if (inp_md_piso() == "all") {
        d <- d %>% 
          filter(
            year %in% !!inp_md_y() &
              reporter_iso == !!inp_md_riso()
          )
      } else {
        d <- d %>% 
          filter(
            year %in% !!inp_md_y() &
              reporter_iso == !!inp_md_riso() &
              partner_iso == !!inp_md_piso()
          )
      }
          
      d <- d %>% collect()
      
      wt_md$inc(2)
      
      ## 2. apply filters ----
      
      if (any(inp_md_product_filter() %in% "vaccine")) {
        vaccine_codes <- as.character(unlist(read.csv("vaccine_codes.csv")))
        d <- d %>% 
          mutate(
            section_code = case_when(
              commodity_code %in% vaccine_codes ~ "vaccine",
              TRUE ~ section_code
            )
          )
      }
      
      if (length(inp_md_product_filter()) > 0) {
        d <- d %>% 
          filter(section_code %in% !!inp_md_product_filter())
      }
        
      wt_md$inc(1)
      
      if (any(inp_md_ctn() %in% "mfn")) {
        ## 3.1. If MFN ----
        
        ### read from SQL ----
        
        d <- d %>%
          inner_join(
            tbl(con, "tariffs") %>% 
              filter(
                years %in% !!inp_md_y(),
                # here we need the applied tariffs when the product gets to destination
                reporters = inp_md_piso()
              ) %>% 
              select(year, partner_iso = reporter_iso, commodity_code, mfn = simple_average) %>% 
              collect(),
            by = c("year", "partner_iso", "commodity_code")
          )
        
        ### summarise ----
        
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
        ## 3.2. If no MFN ----
        
        ### summarise ----
        
        d <- d %>% 
          select(year, reporter_iso, partner_iso, trade_value_usd_exp, trade_value_usd_imp) %>% 
          group_by(year, reporter_iso, partner_iso) %>% 
          summarise(
            trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T),
            trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)
          ) %>% 
          ungroup()
      }
      
      ## 4. add geo dist data ----
      
      d <- d %>% 
        mutate(
          country1 = pmin(reporter_iso, partner_iso),
          country2 = pmax(reporter_iso, partner_iso)
        ) %>% 
        inner_join(
          tbl(con, "distances") %>% 
            select(country1, country2,
                   c(!!inp_md_dist(), !!inp_md_bin()[!!inp_md_bin() != "rta"])) %>% 
            collect(),
          by = c("country1", "country2")
        ) %>% 
        select(-c(country1,country2))
      
      ## 5. add RTA data ----
      
      if (any(inp_md_bin() %in% "rta")) {
        d <- d %>%
          mutate(
            country1 = pmin(reporter_iso, partner_iso),
            country2 = pmax(reporter_iso, partner_iso)
          ) %>%
          left_join(
            tbl(con, "rtas") %>% 
              filter(year %in% !!inp_md_y()) %>% 
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
      
      ## 6. create remoteness indexes ----
      
      if (inp_md_type() == "olsrem") {
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
      
      ## 7. create fixed effects ----
      
      if (inp_md_type() == "olsfe") {
        d <- d %>% 
          mutate(
            reporter_yr = paste0(reporter_iso, year),
            partner_yr = paste0(partner_iso, year)
          )
      }
      
      ## 8. log variables ----
      
      if (inp_md_dist() == "dist") {
        d <- d %>% 
          mutate(log_dist = log(dist)) %>% 
          select(-dist)
      } else {
        d <- d %>% 
          mutate(log_distcap = log(distcap)) %>% 
          select(-distcap)
      }
      
      if (inp_md_type() != "ppml") {
        d <- d %>% 
          mutate(
            # not needed for PPML!
            log_trade_value_usd_exp = log(trade_value_usd_exp)
          ) %>% 
          select(-trade_value_usd_exp)
      }
      
      ## 9. create clustering variable ----
      
      if (inp_md_cluster() == "yes") {
       d <- d %>% 
         mutate(reporter_piso = paste(reporter_iso, partner_iso, sep = "_"))
      }
      
      ## 10. join with custom data ----
      
      if (nrow(md_custom_data()) > 0) {
        d <- d %>% 
          inner_join(md_custom_data())
      }
      
      wt_md$inc(2)
      
      ## 11. convert dollars in time ----
      
      if (inp_md_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_md_convert_dollars()))
      }
      
      wt_md$inc(1)
      
      gc()
      
      return(
        d %>% 
          select(year, matches("trade"), everything()) %>% 
          select(-trade_value_usd_imp)
      )
    })
    
    df_dtl_md_text <- eventReactive(input$md_go, { 
      glue("The filtered dataset contains { nrow(df_dtl_md()) } rows and
           { ncol(df_dtl_md()) } columns. Here's a preview of the table to use to fit regression mds:")
    })
    
    df_dtl_md_preview <- eventReactive(input$md_go, { head(df_dtl_md()) })
    
    md_formula <- eventReactive(input$md_go, {
      custom_variables <- if(nrow(md_custom_data()) > 0) {
        cols <- colnames(md_custom_data())
        cols <- cols[cols %in% unlist(strsplit(inp_md_custom_subset(), ";"))]
        if (length(cols) > 0) cols else "1"
      } else {
        "1" # just a trick to avoid complex if else statements
      }
      
      if (inp_md_type() == "ols") {
        if (length(inp_md_bin()) > 0 & length(inp_md_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", inp_md_dist()), 
                                                                        paste0("log_", inp_md_ctn()), 
                                                                        inp_md_bin(),
                                                                        custom_variables), 
                                                                      collapse = " + "))) 
        }
        
        if (length(inp_md_bin()) > 0 & length(inp_md_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", inp_md_dist()), 
                                                                        inp_md_bin(),
                                                                        custom_variables), 
                                                                      collapse = " + "))) 
        }
        
        if (length(inp_md_bin()) == 0 & length(inp_md_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", inp_md_dist()), 
                                                                        paste0("log_", inp_md_ctn()),
                                                                        custom_variables), 
                                                                      collapse = " + "))) 
        }
        
        if (length(inp_md_bin()) == 0 & length(inp_md_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", inp_md_dist()),
                                                                        custom_variables), 
                                                                      collapse = " + "))) 
        }
      }
      
      if (inp_md_type() == "olsrem") {
        if (length(inp_md_bin()) > 0 & length(inp_md_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", inp_md_dist()),
                                                                        paste0("log_", inp_md_ctn()),
                                                                        inp_md_bin(),
                                                                        custom_variables,
                                                                        "log_remoteness_exp", "log_remoteness_imp"), collapse = " + ")))
        }
        
        if (length(inp_md_bin()) > 0 & length(inp_md_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", inp_md_dist()),
                                                                        inp_md_bin(),
                                                                        custom_variables,
                                                                        "log_remoteness_exp", "log_remoteness_imp"), collapse = " + ")))
        }
        
        if (length(inp_md_bin()) == 0 & length(inp_md_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", inp_md_dist()),
                                                                        paste0("log_", inp_md_ctn()),
                                                                        custom_variables,
                                                                        "log_remoteness_exp", "log_remoteness_imp"), collapse = " + ")))
        }
        
        if (length(inp_md_bin()) == 0 & length(inp_md_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(c(paste0("log_", inp_md_dist()),
                                                                        custom_variables,
                                                                        "log_remoteness_exp", "log_remoteness_imp"), collapse = " + ")))
        }
      }
      
      if (inp_md_type() == "olsfe") {
        if (length(inp_md_bin()) > 0 & length(inp_md_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(paste(c(paste0("log_", inp_md_dist()),
                                                                              paste0("log_", inp_md_ctn()),
                                                                              inp_md_bin(),
                                                                              custom_variables), collapse = " + "), 
                                                                      "reporter_yr + partner_yr", sep = " | ")))
        }
        
        if (length(inp_md_bin()) > 0 & length(inp_md_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(paste(c(paste0("log_", inp_md_dist()),
                                                                              inp_md_bin(),
                                                                              custom_variables), collapse = " + "), 
                                                                      "reporter_yr + partner_yr", sep = " | ")))
        }
        
        if (length(inp_md_bin()) == 0 & length(inp_md_ctn()) > 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(paste(c(paste0("log_", inp_md_dist()),
                                                                              paste0("log_", inp_md_ctn()),
                                                                              custom_variables), collapse = " + "), 
                                                                      "reporter_yr + partner_yr", sep = " | ")))
        }
        
        if (length(inp_md_bin()) == 0 & length(inp_md_ctn()) == 0) {
          f <- as.formula(paste("log_trade_value_usd_exp", "~", paste(paste(c(paste0("log_", inp_md_dist()),
                                                                              custom_variables), collapse = " + "), 
                                                                      "reporter_yr + partner_yr", sep = " | ")))
        }
      }
      
      if (inp_md_type() == "ppml") {
        if (length(inp_md_bin()) > 0 & length(inp_md_ctn()) > 0) {
          f <- as.formula(paste("trade_value_usd_exp", "~", paste(paste(c(paste0("log_", inp_md_dist()),
                                                                          paste0("log_", inp_md_ctn()),
                                                                          inp_md_bin(),
                                                                          custom_variables), collapse = " + "))))
        }
        
        if (length(inp_md_bin()) > 0 & length(inp_md_ctn()) == 0) {
          f <- as.formula(paste("trade_value_usd_exp", "~", paste(paste(c(paste0("log_", inp_md_dist()),
                                                                          inp_md_bin(),
                                                                          custom_variables), collapse = " + "))))
        }
        
        if (length(inp_md_bin()) == 0 & length(inp_md_ctn()) > 0) {
          f <- as.formula(paste("trade_value_usd_exp", "~", paste(paste(c(paste0("log_", inp_md_dist()),
                                                                          paste0("log_", inp_md_ctn()),
                                                                          custom_variables), collapse = " + "))))
        }
        
        if (length(inp_md_bin()) == 0 & length(inp_md_ctn()) == 0) {
          f <- as.formula(paste("trade_value_usd_exp", "~", paste(paste(c(paste0("log_",inp_md_dist()),
                                                                          custom_variables), collapse = " + "))))
        }
      }
      
      wt_md$inc(2)
      
      print(f)
      return(f)
    })
    
    md_output <- eventReactive(input$md_go, {
      if (any(inp_md_type() %in% c("ols", "olsrem", "olsfe"))) {
        if (inp_md_cluster() == "yes") {
          m <- feols(md_formula(), df_dtl_md(), cluster = ~reporter_piso)
        } else {
          m <- feols(md_formula(), df_dtl_md())
        }
      }
      
      if (inp_md_type() == "ppml") {
        if (inp_md_cluster() == "yes") {
          m <- feglm(md_formula(), df_dtl_md(),
                     cluster = ~reporter_piso,
                     family = quasipoisson(link = "log"))      
        } else {
          m <- feglm(md_formula(), df_dtl_md(),
                     family = quasipoisson(link = "log"))
        }
      }
      
      wt_md$inc(2)
      gc()
      
      wt_md$close() 
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
      glue("@misc{{open_trd_statistics_{lubridate::year(Sys.Date())},
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
    
    # Outputs ----
    
    ## Titles ----
    
    output$title_cp <- renderText({
      title_cp()
    })
    
    output$title_pp <- renderText({
      title_pp()
    })
    
    # put here to avoid repetition in UI
    legend_text <- "The information displayed here is based on <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. Please read our <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a> for a full description
      of restrictions and applicable licenses. These figures do not include services or foreign direct investment."
    
    output$title_cp_legend <- renderText({ legend_text })
    output$title_pp_legend <- renderText({ legend_text })
    output$title_md_legend <- renderText({ legend_text })
    
    ## Country profile ----
    
    ### Trade ----
    
    output$trd_stl_cp <- eventReactive(input$cp_go, {
      switch(
        tbl_dtl_cp(),
        "yrc" = glue("Total multilateral Exports and Imports { min(inp_cp_y()) }-{ max(inp_cp_y()) }"),
        "yrpc" = glue("Total bilateral Exports and Imports { min(inp_cp_y()) }-{ max(inp_cp_y()) }")
      )
    })
    
    output$trd_stl_exp_cp <- eventReactive(input$cp_go, {
      "Exports"
    })
    
    output$trd_stl_imp_cp <- eventReactive(input$cp_go, {
      "Imports"
    })
    
    output$trd_smr_exp_cp <- renderText(trd_smr_text_exp_cp())
    output$trd_smr_imp_cp <- renderText(trd_smr_text_imp_cp())
    
    output$trd_exc_lines_agg_cp <- renderHighchart({
      trd_exc_lines_agg_cp()
    })
    
    ### Exports ----
    
    output$exp_tt_yr_cp <- renderText(exp_tt_yr_cp())
    output$exp_tt_min_yr_cp <- renderText(exp_tt_min_yr_cp())
    output$exp_tm_dtl_min_yr_cp <- renderHighchart({exp_tm_dtl_min_yr_cp()})
    output$exp_tt_max_yr_cp <- renderText(exp_tt_max_yr_cp())
    output$exp_tm_dtl_max_yr_cp <- renderHighchart({exp_tm_dtl_max_yr_cp()})
    
    ### Imports ----
    
    output$imp_tt_yr_cp <- renderText(imp_tt_yr_cp())
    output$imp_tt_min_yr_cp <- renderText(imp_tt_min_yr_cp())
    output$imp_tm_dtl_min_yr_cp <- renderHighchart({imp_tm_dtl_min_yr_cp()})
    output$imp_tt_max_yr_cp <- renderText(imp_tt_max_yr_cp())
    output$imp_tm_dtl_max_yr_cp <- renderHighchart({imp_tm_dtl_max_yr_cp()})
    
    ## Product profile ----
    
    output$trd_stl_pp <- eventReactive(input$pp_go, {
      glue("Total multilateral Exports and Imports { min(inp_cp_y()) } and { max(inp_cp_y()) }")
    })
    
    output$trd_stl_exp_pp <- eventReactive(input$pp_go, {
      "Exports"
    })
    
    output$trd_stl_imp_pp <- eventReactive(input$pp_go, {
      "Imports"
    })
    
    output$trd_smr_exp_pp <- renderText(trd_smr_text_exp_pp())
    output$trd_smr_imp_pp <- renderText(trd_smr_text_imp_pp())
    
    output$trd_exc_columns_pp <- renderHighchart({
      trd_exc_columns_pp()
    })
    
    output$imp_tt_yr_pp <- renderText(imp_tt_yr_pp())
    output$imp_tt_min_yr_pp <- renderText(imp_tt_min_yr_pp())
    output$imp_tt_max_yr_pp <- renderText(imp_tt_max_yr_pp())
    output$imp_tm_ori_min_yr_pp <- renderHighchart({imp_tm_ori_min_yr_pp()})
    output$imp_tm_ori_max_yr_pp <- renderHighchart({imp_tm_ori_max_yr_pp()})
    
    output$exp_tt_yr_pp <- renderText(exp_tt_yr_pp())
    output$exp_tt_min_yr_pp <- renderText(exp_tt_min_yr_pp())
    output$exp_tt_max_yr_pp <- renderText(exp_tt_max_yr_pp())
    output$exp_tm_ori_min_yr_pp <- renderHighchart({exp_tm_ori_min_yr_pp()})
    output$exp_tm_ori_max_yr_pp <- renderHighchart({exp_tm_ori_max_yr_pp()})
    
    ## Model ----
    
    output$md_df_stl <- eventReactive(input$md_go, { "Data preview" })
    output$df_dtl_md_text <- renderText(df_dtl_md_text())
    output$df_dtl_md_preview <- renderTable(df_dtl_md_preview())
    
    output$md_formula_latex <- renderUI({
      if (inp_md_type() == "ppml") {
        lhs <- "\\text{trade value}_{ij}^{t}"
      } else {
        lhs <- "\\text{log trade value}_{ij}^{t}"
      }
      
      if (inp_md_type() == "ols") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(inp_md_dist(), inp_md_ctn(), inp_md_bin())
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", inp_md_dist()), 
                                                    paste("log", inp_md_ctn()), inp_md_bin())), "}"), "_{ij}^{t}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\beta_0 +", rhs, "+ \\varepsilon_{ij}^{t}")
      }
      
      if (inp_md_type() == "olsrem") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(inp_md_dist(), inp_md_bin(), "log_remoteness_exp", "log_remoteness_imp")
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", inp_md_dist()), 
                                                    inp_md_bin(), "log_remoteness_exp", "log_remoteness_imp")), "}"), "_{ij}^{t}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\beta_0 +", rhs, "+ \\varepsilon_{ij}^{t}")
      }
      
      if (inp_md_type() == "olsfe") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(inp_md_dist(), inp_md_bin(), "reporter_yr", "partner_yr")
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", inp_md_dist()), 
                                                    inp_md_bin(), "reporter_yr", "partner_yr")), "}"), "_{ij}^{t}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\beta_0 +", rhs, "+ \\varepsilon_{ij}^{t}")
      }
      
      if (inp_md_type() == "ppml") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(inp_md_dist(), inp_md_bin())
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", inp_md_dist()), 
                                                    inp_md_bin())), "}"), "_{ij}^{t}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\exp[\\beta_0 +", rhs, "]\\times \\varepsilon_{ij}^{t}")
      }
      
      withMathJax(
        paste0("\\[", lhs, "=", rhs, "\\]")
      )
    })
    
    output$md_smr_stl <- eventReactive(input$md_go, { "Model summary" })
    output$md_smr_text <- eventReactive(input$md_go, { 
      "WIP: some selections create errors messages such as 'contrasts can be applied only to factors with 2 or more levels' but Shiny hides those."
    })
    output$md_smr_tidy <- renderTable(tidy(md_output()))
    output$md_smr_glance <- renderTable(glance(md_output()))
    
    ## Download ----
    
    ### Country profile ----
    
    dwn_cp_stl <- eventReactive(input$cp_go, {
      "Download country profile data"
    })
    
    dwn_cp_text <- eventReactive(input$cp_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    dwn_cp_f <- eventReactive(input$cp_go, {
      selectInput(
        "cp_f",
        "Download data as:",
        choices = available_formats,
        selected = NULL,
        selectize = TRUE
      )
    })
    
    output$dwn_cp_agg_pre <- downloadHandler(
        filename = function() {
          glue("{ table_agg_cp() }_{ inp_cp_r() }_{ inp_cp_p() }_{ min(inp_cp_y()) }_{ max(inp_cp_y()) }.{ inp_cp_f() }")
        },
        content = function(filename) {
          rio::export(df_agg_cp(), filename)
        },
        contentType = "application/zip"
      )
    
    output$dwn_cp_dtl_pre <- downloadHandler(
      filename = function() {
        glue("{ tbl_dtl_cp() }_{ inp_cp_r() }_{ inp_cp_p() }_{ min(inp_cp_y()) }_{ max(inp_cp_y()) }.{ inp_cp_f() }")
      },
      content = function(filename) {
        rio::export(df_dtl_cp(), filename)
      },
      contentType = "application/zip"
    )
    
    output$dwn_cp_stl <- renderText({dwn_cp_stl()})
    output$dwn_cp_text <- renderText({dwn_cp_text()})
    output$dwn_cp_f <- renderUI({dwn_cp_f()})
    output$dwn_cp_aggregated <- renderUI({
      req(input$cp_go)
      downloadButton('dwn_cp_agg_pre', label = 'Aggregated data')
    })
    output$dwn_cp_dtl <- renderUI({
      req(input$cp_go)
      downloadButton('dwn_cp_dtl_pre', label = 'Detailed data')
    })
    
    ### Product profile ----
    
    dwn_pp_stl <- eventReactive(input$pp_go, {
      "Download product profile data"
    })
    
    dwn_pp_text <- eventReactive(input$pp_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    dwn_pp_f <- eventReactive(input$pp_go, {
      selectInput(
        "pp_f",
        "Download data as:",
        choices = available_formats,
        selected = NULL,
        selectize = TRUE
      )
    })
    
    output$dwn_pp_pre <- downloadHandler(
      filename = function() {
        glue("yrpc_{ inp_pp_section_code() }_{ min(inp_pp_y()) }_{ max(inp_pp_y()) }.{ inp_pp_f() }")
      },
      content = function(filename) {
        rio::export(df_dtl_pp(), filename)
      },
      contentType = "application/zip"
    )
    
    output$dwn_pp_stl <- renderText({dwn_pp_stl()})
    output$dwn_pp_text <- renderText({dwn_pp_text()})
    output$dwn_pp_f <- renderUI({dwn_pp_f()})
    output$dwn_pp_aggregated <- renderUI({
      req(input$pp_go)
      downloadButton('dwn_pp_pre', label = 'Aggregated data')
    })
    
    ### Model ----
    
    dwn_md_stl <- eventReactive(input$md_go, { "Download model data" })
    
    dwn_md_text <- eventReactive(input$md_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    dwn_md_f <- eventReactive(input$md_go, {
      selectInput(
        "md_f",
        "Download data as:",
        choices = available_formats,
        selected = NULL,
        selectize = TRUE
      )
    })
    
    output$dwn_md_dtl_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_md_type() }_{ inp_md_riso() }_{ inp_md_piso() }_{ min(inp_md_y()) }_{ max(inp_md_y()) }.{ inp_md_f() }")
      },
      content = function(filename) {
        rio::export(df_dtl_md(), filename)
      },
      contentType = "application/zip"
    )
    
    output$dwn_md_fit_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_md_type() }_{ inp_md_riso() }_{ inp_md_piso() }_{ min(inp_md_y()) }_{ max(inp_md_y()) }.rds")
      },
      content = function(filename) {
        saveRDS(md_output(), filename)
      },
      contentType = "application/zip"
    )
    
    output$dwn_md_stl <- renderText({dwn_md_stl()})
    output$dwn_md_text <- renderText({dwn_md_text()})
    output$dwn_md_f <- renderUI({dwn_md_f()})
    output$dwn_md_dtl <- renderUI({
      req(input$md_go)
      downloadButton('dwn_md_dtl_pre', label = 'Detailed data')
    })
    output$dwn_md_fit <- renderUI({
      req(input$md_go)
      downloadButton('dwn_md_fit_pre', label = 'Fitted model')
    })
    
    # Cite output ----
    
    output$cite_stl <- renderText({"Cite"})
    
    output$cite_chicago_stl <- renderText({
      "Chicago citation"
    })
    
    output$cite <- renderText({
      cite()
    })
    
    output$cite_bibtex_stl <- renderText({
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
        "md_own", "md_f", "cp_f", "go", "sidebarCollapsed", "sidebarItemExpanded"
      ))
      session$doBookmark()
    })
    
    onBookmarked(function(url) {
      updateQueryString(url)
    })
  }
)
