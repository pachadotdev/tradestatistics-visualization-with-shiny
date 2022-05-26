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
    
    tbl_agg_cp <- eventReactive(input$cp_go, {
      ifelse(inp_cp_p() == "all", "yr", "yrp")
    })
    
    tbl_dtl_cp <- eventReactive(input$cp_go, {
      ifelse(inp_cp_p() == "all", "yrc", "yrpc")
    })
    
    rname_cp <- eventReactive(input$cp_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == inp_cp_r()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    pname_cp <- eventReactive(input$cp_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == inp_cp_p()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    ## Compare countries ----
    
    inp_cc_y <- reactive({ input$cc_y })
    
    inp_cc_r1 <- reactive({ input$cc_r1 })
    inp_cc_r2 <- reactive({ input$cc_r2 })
    inp_cc_p <- reactive({ input$cc_p })

    inp_cc_f <- reactive({ input$cc_f })
    
    inp_cc_convert_dollars <- reactive({ input$cc_a })
    
    tbl_agg_cc <- eventReactive(input$cc_go, {
      ifelse(inp_cc_p() == "all", "yr", "yrp")
    })
    
    tbl_dtl_cc <- eventReactive(input$cc_go, {
      ifelse(inp_cc_p() == "all", "yrc", "yrpc")
    })
    
    r1name_cc <- eventReactive(input$cc_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == inp_cc_r1()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    r2name_cc <- eventReactive(input$cc_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == inp_cc_r2()) %>%
        select(available_reporters_names) %>%
        as.character()
    })

    pname_cc <- eventReactive(input$cc_go, {
      reporters_to_display %>%
        filter(available_reporters_iso == inp_cc_p()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    ## Product profile ----
    
    updateSelectizeInput(
      session,
      inputId = "pp_s",
      label = "Section:",
      choices = list(
        c(available_all, available_vaccine),
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
    
    inp_md_cluster <- reactive({ input$md_cl })
    
    inp_md_product_filter <- reactive({ input$md_pf })
    inp_md_type <- reactive({ input$md_t })
    inp_md_fml <- reactive({ input$md_fml })
    inp_md_f <- reactive({ input$md_f })

    # Titles ----
    
    r_add_the <- eventReactive(input$cp_go, {
      if (substr(rname_cp(), 1, 6) == "United" | 
          substr(rname_cp(), 1, 3) == "USA" |
          substr(rname_cp(), 1, 7) == "Russian") {
        "the"
      } else {
        ""
      }
    })
    
    r_add_upp_the <- eventReactive(input$cp_go, {
      if (substr(rname_cp(), 1, 6) == "United" | 
          substr(rname_cp(), 1, 3) == "USA" |
          substr(rname_cp(), 1, 7) == "Russian") {
        "The"
      } else {
        ""
      }
    })
    
    p_add_the <- eventReactive(input$cp_go, {
      if (substr(pname_cp(), 1, 6) == "United" | 
          substr(pname_cp(), 1, 3) == "USA" |
          substr(pname_cp(), 1, 7) == "Russian") {
        "the"
      } else {
        ""
      }
    })
    
    title_cp <- eventReactive(input$cp_go, {
      switch(
        tbl_dtl_cp(),
        "yrc" = glue("{ r_add_upp_the() } { rname_cp() } multilateral trade between { min(inp_cp_y()) } and { max(inp_cp_y()) }"),
        "yrpc" = glue("{ r_add_upp_the() } { rname_cp() } and { p_add_the() } { pname_cp() } trade between { min(inp_cp_y()) } and { max(inp_cp_y()) }")
      )
    })
    
    r1_add_the <- eventReactive(input$cc_go, {
      if (substr(r1name_cc(), 1, 6) == "United" | 
          substr(r1name_cc(), 1, 3) == "USA" |
          substr(r1name_cc(), 1, 7) == "Russian") {
        "the"
      } else {
        ""
      }
    })
    
    r1_add_upp_the <- eventReactive(input$cc_go, {
      if (substr(r1name_cc(), 1, 6) == "United" | 
          substr(r1name_cc(), 1, 3) == "USA" |
          substr(r1name_cc(), 1, 7) == "Russian") {
        "The"
      } else {
        ""
      }
    })
    
    r2_add_the <- eventReactive(input$cc_go, {
      if (substr(r2name_cc(), 1, 6) == "United" | 
          substr(r2name_cc(), 1, 3) == "USA" |
          substr(r2name_cc(), 1, 7) == "Russian") {
        "the"
      } else {
        ""
      }
    })
    
    r2_add_upp_the <- eventReactive(input$cc_go, {
      if (substr(r2name_cc(), 1, 6) == "United" | 
          substr(r2name_cc(), 1, 3) == "USA" |
          substr(r2name_cc(), 1, 7) == "Russian") {
        "The"
      } else {
        ""
      }
    })
    
    p2_add_the <- eventReactive(input$cc_go, {
      if (substr(pname_cc(), 1, 6) == "United" | 
          substr(pname_cc(), 1, 3) == "USA" |
          substr(pname_cc(), 1, 7) == "Russian") {
        "the"
      } else {
        ""
      }
    })
    
    p2_add_upp_the <- eventReactive(input$cc_go, {
      if (substr(pname_cc(), 1, 6) == "United" | 
          substr(pname_cc(), 1, 3) == "USA" |
          substr(pname_cc(), 1, 7) == "Russian") {
        "The"
      } else {
        ""
      }
    })
    
    title_cc <- eventReactive(input$cc_go, {
      switch(
        tbl_dtl_cc(),
        "yrc" = glue("{ r1_add_upp_the() } { r1name_cc() } and { r2_add_the() } { r2name_cc() } multilateral trade in { inp_cc_y() }"),
        "yrpc" = glue("{ r1_add_upp_the() } { r1name_cc() } and { r2_add_the() } { r2name_cc() } bilateral trade with { p2_add_the() } { pname_cc() } in { inp_cc_y() }")
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
      
      d <- tbl(con, tbl_agg_cp())
      
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
      
      wt_cp$inc(2.5)
      
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
      
      wt_cp$inc(2.5)
      
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
    
    trd_smr_txt_exp_cp <- eventReactive(input$cp_go, {
      switch(tbl_agg_cp(),
             "yr" = glue("The exports of { r_add_the() } { rname_cp() } to the World { exports_growth_increase_decrease_cp() } from 
                          { exp_val_min_yr_2_cp() } in { min(inp_cp_y()) } to { exp_val_max_yr_2_cp() } in { max(inp_cp_y()) } 
                          (annualized { exports_growth_increase_decrease_2_cp() } of { exports_growth_2_cp() })."),
             
             "yrp" = glue("The exports of { r_add_the() } { rname_cp() } to { p_add_the() } { pname_cp() } { exports_growth_increase_decrease_cp() } from 
                          { exp_val_min_yr_2_cp() } in { min(inp_cp_y()) } 
                          to { exp_val_max_yr_2_cp() } in { max(inp_cp_y()) } (annualized { exports_growth_increase_decrease_2_cp() } of 
                          { exports_growth_2_cp() }). { p_add_the() } { pname_cp() } was the No. { trd_rankings_no_min_yr_cp() } trading partner of 
                          { r_add_the() } { rname_cp() } in { min(inp_cp_y()) } (represented { trd_rankings_exp_share_min_yr_2_cp() } of its exports), and 
                          then { trd_rankings_remained_cp() } No. { trd_rankings_no_max_yr_cp() } in { max(inp_cp_y()) } (represented { trd_rankings_exp_share_max_yr_2_cp() } 
                          of its exports).")
      )
    })
    
    trd_smr_txt_imp_cp <- eventReactive(input$cp_go, {
      switch(tbl_agg_cp(),
             "yr" = glue("The imports of { r_add_the() } { rname_cp() } to the World { imports_growth_increase_decrease_cp() } from 
                         { imp_val_min_yr_2_cp() } in { min(inp_cp_y()) } to { imp_val_max_yr_2_cp() } in { max(inp_cp_y()) } 
                         (annualized { imports_growth_increase_decrease_2_cp() } of { imports_growth_2_cp() })."),
             
             "yrp" = glue("The imports of { r_add_the() } { rname_cp() } to { p_add_the() } { pname_cp() } { imports_growth_increase_decrease_cp() } from 
                          { imp_val_min_yr_2_cp() } in { min(inp_cp_y()) } 
                          to { imp_val_max_yr_2_cp() } in { max(inp_cp_y()) } (annualized { imports_growth_increase_decrease_2_cp() } of 
                          { imports_growth_2_cp() }). { p_add_the() } { pname_cp() } was the No. { trd_rankings_no_min_yr_cp() } trading partner of 
                          { r_add_the() } { rname_cp() } in { min(inp_cp_y()) } (represented { trd_rankings_imp_share_min_yr_2_cp() } of its imports), and 
                          then { trd_rankings_remained_cp() } No. { trd_rankings_no_max_yr_cp() } in { max(inp_cp_y()) } (represented { trd_rankings_imp_share_max_yr_2_cp() } 
                          of its imports).")
      )
    })

    trd_exc_lines_title_cp <- eventReactive(input$cp_go, {
      switch(tbl_agg_cp(),
             "yr" = glue("{ r_add_upp_the() } { rname_cp() } multilateral trade between { min(inp_cp_y()) } and { max(inp_cp_y()) }"),
             "yrp" = glue("{ r_add_upp_the() } { rname_cp() } and { p_add_the() } { pname_cp() } exchange between { min(inp_cp_y()) } and { max(inp_cp_y()) }")
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
    
    ### Visual elements ----
    
    exp_tt_yr_cp <- eventReactive(input$cp_go, {
      switch(
        tbl_dtl_cp(),
        "yrc" = glue("Exports of { r_add_the() } { rname_cp() } to the rest of the World in { min(inp_cp_y()) } and { max(inp_cp_y()) }, by product"),
        "yrpc" = glue("Exports of { r_add_the() } { rname_cp() } to { p_add_the() } { pname_cp() } in { min(inp_cp_y()) } and { max(inp_cp_y()) }, by product")
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
        "yrc" = glue("Imports of { r_add_the() } { rname_cp() } from the rest of the World in { min(inp_cp_y()) } and { max(inp_cp_y()) }, by product"),
        "yrpc" = glue("Imports of { r_add_the() } { rname_cp() } from { p_add_the() } { pname_cp() } in { min(inp_cp_y()) } and { max(inp_cp_y()) }, by product")
      )
    })
    
    imp_tt_min_yr_cp <- eventReactive(input$cp_go, {
      glue("{ min(inp_cp_y()) }")
    })
    
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
    
    # Compare countries ----
    
    wt_cc <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
    
    ## Data ----
    
    df_agg_r1_cc <- reactive({
      wt_cc$start()

      d <- tbl(con, tbl_agg_cc())

      if (inp_cc_p() == "all") {
        d <- d %>%
          filter(
            year == !!inp_cc_y() &
              reporter_iso == !!inp_cc_r1()
          )
      } else {
        d <- d %>%
          filter(
            year == !!inp_cc_y() &
              reporter_iso == !!inp_cc_r1() &
              partner_iso == !!inp_cc_p()
          )
      }

      d <- d %>% collect()

      if (inp_cc_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cc_convert_dollars()))
      }

      wt_cc$inc(1)

      return(d)
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars()) %>%
      bindEvent(input$cc_go)
    
    df_dtl_r1_cc <- reactive({
      d <- tbl(con, tbl_dtl_cc())

      if (inp_cc_p() == "all") {
        d <- d %>%
          filter(
            year == !!inp_cc_y() &
              reporter_iso == !!inp_cc_r1()
          )
      } else {
        d <- d %>%
          filter(
            year == !!inp_cc_y() &
              reporter_iso == !!inp_cc_r1() &
              partner_iso == !!inp_cc_p()
          )
      }

      d <- d %>% collect()

      if (inp_cc_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cc_convert_dollars()))
      }

      wt_cc$inc(1)

      return(d)
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars(),
                tbl_dtl_cc()) %>%
      bindEvent(input$cc_go)
    
    df_agg_r2_cc <- reactive({
      d <- tbl(con, tbl_agg_cc())
      
      if (inp_cc_p() == "all") {
        d <- d %>%
          filter(
            year == !!inp_cc_y() &
              reporter_iso == !!inp_cc_r2()
          )
      } else {
        d <- d %>%
          filter(
            year == !!inp_cc_y() &
              reporter_iso == !!inp_cc_r2() &
              partner_iso == !!inp_cc_p()
          )
      }
      
      d <- d %>% collect()
      
      if (inp_cc_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cc_convert_dollars()))
      }
      
      wt_cc$inc(1)
      
      return(d)
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r2(), inp_cc_p(), inp_cc_convert_dollars()) %>%
      bindEvent(input$cc_go)
    
    df_dtl_r2_cc <- reactive({
      d <- tbl(con, tbl_dtl_cc())
      
      if (inp_cc_p() == "all") {
        d <- d %>%
          filter(
            year == !!inp_cc_y() &
              reporter_iso == !!inp_cc_r2()
          )
      } else {
        d <- d %>%
          filter(
            year %in% !!inp_cc_y() &
              reporter_iso == !!inp_cc_r2() &
              partner_iso == !!inp_cc_p()
          )
      }
      
      d <- d %>% collect()
      
      if (inp_cc_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cc_convert_dollars()))
      }
      
      wt_cc$inc(1)
      
      return(d)
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r2(), inp_cc_p(), inp_cc_convert_dollars(),
                tbl_dtl_cc()) %>%
      bindEvent(input$cc_go)
    
    ## Trade ----
    
    ### Tables ----
    
    tr_tbl_agg_r1_cc <- eventReactive(input$cc_go, {
      df_agg_r1_cc() %>%
        select(year, trade_value_usd_exp, trade_value_usd_imp)
    })
    
    tr_tbl_agg_r2_cc <- eventReactive(input$cc_go, {
      df_agg_r2_cc() %>%
        select(year, trade_value_usd_exp, trade_value_usd_imp)
    })

    exp_val_yr_r1_cc <- eventReactive(input$cc_go, {
      tr_tbl_agg_r1_cc() %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })

    exp_val_yr_r2_cc <- eventReactive(input$cc_go, {
      tr_tbl_agg_r2_cc() %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })

    imp_val_yr_r1_cc <- eventReactive(input$cc_go, {
      tr_tbl_agg_r1_cc() %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })

    imp_val_yr_r2_cc <- eventReactive(input$cc_go, {
      tr_tbl_agg_r2_cc() %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })

    exp_val_yr_2_r1_cc <- eventReactive(input$cc_go, {
      show_dollars(exp_val_yr_r1_cc())
    })

    exp_val_yr_2_r2_cc <- eventReactive(input$cc_go, {
      show_dollars(exp_val_yr_r2_cc())
    })

    imp_val_yr_2_r1_cc <- eventReactive(input$cc_go, {
      show_dollars(imp_val_yr_r1_cc())
    })

    imp_val_yr_2_r2_cc <- eventReactive(input$cc_go, {
      show_dollars(imp_val_yr_r2_cc())
    })

    trd_rankings_r1_cc <- eventReactive(input$cc_go, {
      d <- tbl(con, "yrp") %>%
        filter(
          year %in% !!inp_cc_y() &
            reporter_iso == !!inp_cc_r1()
        ) %>%
        collect()

      if (inp_cc_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cc_convert_dollars()))
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

    trd_rankings_no_yr_r1_cc <- eventReactive(input$cc_go, {
      trd_rankings_r1_cc() %>%
        ungroup() %>%
        filter(
          reporter_iso == !!inp_cc_r1(),
          partner_iso == !!inp_cc_p()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
     
    trd_rankings_exp_share_yr_r1_cc <- eventReactive(input$cc_go, {
      trd_rankings_r1_cc() %>%
        ungroup() %>%
        filter(
          reporter_iso == !!inp_cc_r1(),
          partner_iso == !!inp_cc_p()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trd_rankings_exp_share_yr_2_r1_cc <- eventReactive(input$cc_go, {
      show_percentage(trd_rankings_exp_share_yr_r1_cc())
    })

    trd_rankings_imp_share_yr_r1_cc <- eventReactive(input$cc_go, {
      trd_rankings_r1_cc() %>%
        ungroup() %>%
        filter(
          year == inp_cc_y(),
          reporter_iso == !!inp_cc_r1(),
          partner_iso == !!inp_cc_p()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })

    trd_rankings_imp_share_yr_2_r1_cc <- eventReactive(input$cc_go, {
      show_percentage(trd_rankings_imp_share_yr_r1_cc())
    })
    
    trd_rankings_r2_cc <- eventReactive(input$cc_go, {
      d <- tbl(con, "yrp") %>%
        filter(
          year %in% !!inp_cc_y() &
            reporter_iso == !!inp_cc_r2()
        ) %>%
        collect()
      
      if (inp_cc_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_cc_convert_dollars()))
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
    
    trd_rankings_no_yr_r2_cc <- eventReactive(input$cc_go, {
      trd_rankings_r2_cc() %>%
        ungroup() %>%
        filter(
          reporter_iso == !!inp_cc_r2(),
          partner_iso == !!inp_cc_p()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trd_rankings_exp_share_yr_r2_cc <- eventReactive(input$cc_go, {
      trd_rankings_r2_cc() %>%
        ungroup() %>%
        filter(
          reporter_iso == !!inp_cc_r2(),
          partner_iso == !!inp_cc_p()
        ) %>%
        select(exp_share) %>%
        as.numeric()
    })
    
    trd_rankings_exp_share_yr_2_r2_cc <- eventReactive(input$cc_go, {
      show_percentage(trd_rankings_exp_share_yr_r2_cc())
    })
    
    trd_rankings_imp_share_yr_r2_cc <- eventReactive(input$cc_go, {
      trd_rankings_r2_cc() %>%
        ungroup() %>%
        filter(
          year == inp_cc_y(),
          reporter_iso == !!inp_cc_r2(),
          partner_iso == !!inp_cc_p()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trd_rankings_imp_share_yr_2_r2_cc <- eventReactive(input$cc_go, {
      show_percentage(trd_rankings_imp_share_yr_r2_cc())
    })
    
    ### Text/Visual elements ----
    
    trd_smr_txt_exp_r1_cc <- eventReactive(input$cc_go, {
      switch(tbl_agg_cc(),
             "yr" = glue("The exports of { r1name_cc() } to the World were { exp_val_yr_2_r1_cc() } in { inp_cc_y() }."),

             "yrp" = glue("The exports of { r1name_cc() } to { pname_cc() } were { exp_val_yr_2_r1_cc() } in { inp_cc_y() }.
                          { p2_add_upp_the() } { pname_cc() } was the No. { trd_rankings_no_yr_r1_cc() } trading partner of
                          { r1name_cc() } in { inp_cc_y() } (represented { trd_rankings_exp_share_yr_2_r1_cc() } of its exports).")
      )
    })

    trd_smr_txt_imp_r1_cc <- eventReactive(input$cc_go, {
      switch(tbl_agg_cc(),
             "yr" = glue("The imports of { r1name_cc() } to the World were { imp_val_yr_2_r1_cc() } in { inp_cc_y() }."),

             "yrp" = glue("The imports of { r1name_cc() } to { pname_cc() } were { imp_val_yr_2_r1_cc() } in { inp_cc_y() }.
                          { p2_add_upp_the() } { pname_cc() } was the No. { trd_rankings_no_yr_r1_cc() } trading partner of
                          { r1name_cc() } in { inp_cc_y() } (represented { trd_rankings_imp_share_yr_2_r1_cc() } of its imports).")
      )
    })

    trd_smr_txt_exp_r2_cc <- eventReactive(input$cc_go, {
      switch(tbl_agg_cc(),
             "yr" = glue("The exports of { r2name_cc() } to the World were { exp_val_yr_2_r2_cc() } in { inp_cc_y() }."),
             
             "yrp" = glue("The exports of { r2name_cc() } to { pname_cc() } were { exp_val_yr_2_r2_cc() } in { inp_cc_y() }.
                          { p2_add_upp_the() } { pname_cc() } was the No. { trd_rankings_no_yr_r2_cc() } trading partner of
                          { r2name_cc() } in { inp_cc_y() } (represented { trd_rankings_exp_share_yr_2_r2_cc() } of its exports).")
      )
    })
    
    trd_smr_txt_imp_r2_cc <- eventReactive(input$cc_go, {
      switch(tbl_agg_cc(),
             "yr" = glue("The imports of { r2name_cc() } to the World were { imp_val_yr_2_r2_cc() } in { inp_cc_y() }."),
             
             "yrp" = glue("The imports of { r2name_cc() } to { pname_cc() } were { imp_val_yr_2_r2_cc() } in { inp_cc_y() }.
                          { p2_add_upp_the() } { pname_cc() } was the No. { trd_rankings_no_yr_r2_cc() } trading partner of
                          { r2name_cc() } in { inp_cc_y() } (represented { trd_rankings_imp_share_yr_2_r2_cc() } of its imports).")
      )
    })
    
    trd_exc_cols_title_r1_cc <- eventReactive(input$cc_go, {
      switch(tbl_agg_cc(),
             "yr" = glue("{ r1_add_upp_the() } { r1name_cc() } multilateral trade in { inp_cc_y() }"),
             "yrp" = glue("{ r1_add_upp_the() } { r1name_cc() } and { p2_add_the() } { pname_cc() } exchange in { inp_c1_y() }")
      )
    })
    
    trd_exc_cols_title_r1_cc <- eventReactive(input$cc_go, {
      switch(tbl_agg_cc(),
             "yr" = glue("{ r1_add_upp_the() } { r1name_cc() } multilateral trade in { inp_cc_y() }"),
             "yrp" = glue("{ r1_add_upp_the() } { r1name_cc() } and { p2_add_the() } { pname_cc() } exchange in { inp_cc_y() }")
      )
    })
    
    trd_exc_cols_title_r2_cc <- eventReactive(input$cc_go, {
      switch(tbl_agg_cc(),
             "yr" = glue("{ r2_add_upp_the() } { r2name_cc() } multilateral trade in { inp_cc_y() }"),
             "yrp" = glue("{ r2_add_upp_the() } { r2name_cc() } and { p2_add_the() } { pname_cc() } exchange in { inp_cc_y() }")
      )
    })
    
    trd_exc_cols_agg_r1_cc <- reactive({
      d <- tr_tbl_agg_r1_cc()
      
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
      
      wt_cc$inc(1)
      
      hchart(d, 
             "column", 
             hcaes(x = year, y = trade, group = flow),
             tooltip = list(
               pointFormatter = custom_tooltip_short()
             )) %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "USD billion"),
                 labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>% 
        hc_title(text = trd_exc_cols_title_r1_cc())
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars()) %>% 
      bindEvent(input$cc_go)
    
    trd_exc_cols_agg_r2_cc <- reactive({
      d <- tr_tbl_agg_r2_cc()
      
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
      
      wt_cc$inc(1)
      
      hchart(d, 
             "column", 
             hcaes(x = year, y = trade, group = flow),
             tooltip = list(
               pointFormatter = custom_tooltip_short()
             )) %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "USD billion"),
                 labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>% 
        hc_title(text = trd_exc_cols_title_r2_cc())
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r2(), inp_cc_p(), inp_cc_convert_dollars()) %>% 
      bindEvent(input$cc_go)
    
    ## Exports ----
    
    ### Visual elements ----
    
    exp_tt_yr_r1_cc <- eventReactive(input$cc_go, { r1name_cc() })

    exp_tm_dtl_yr_r1_cc <- reactive({
      d <- df_dtl_r1_cc() %>%
        pd_fix_section_and_aggregate(col = "trade_value_usd_exp")

      d2 <- pd_colors(d)

      wt_cc$inc(1)
      pd_to_highcharts(d, d2)
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars()) %>%
      bindEvent(input$cc_go)
    
    exp_tt_yr_r2_cc <- eventReactive(input$cc_go, { r2name_cc() })
    
    exp_tm_dtl_yr_r2_cc <- reactive({
      d <- df_dtl_r2_cc() %>%
        pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
      
      d2 <- pd_colors(d)
      
      wt_cc$inc(1)
      pd_to_highcharts(d, d2)
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r2(), inp_cc_p(), inp_cc_convert_dollars()) %>%
      bindEvent(input$cc_go)
    
    ## Imports ----
    
    ### Visual elements ----
    
    imp_tt_yr_r1_cc <- eventReactive(input$cc_go, { r1name_cc() })
    
    imp_tm_dtl_yr_r1_cc <- reactive({
      d <- df_dtl_r1_cc() %>%
        pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
      
      d2 <- pd_colors(d)
      
      wt_cc$inc(1)
      pd_to_highcharts(d, d2)
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars()) %>%
      bindEvent(input$cc_go)
    
    imp_tt_yr_r2_cc <- eventReactive(input$cc_go, { r2name_cc() })
    
    imp_tm_dtl_yr_r2_cc <- reactive({
      d <- df_dtl_r2_cc() %>%
        pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
      
      d2 <- pd_colors(d)
      
      wt_cc$inc(1)
      out <- pd_to_highcharts(d, d2)
      
      wt_cc$close()
      return(out)
    }) %>%
      bindCache(inp_cc_y(), inp_cc_r2(), inp_cc_p(), inp_cc_convert_dollars()) %>%
      bindEvent(input$cc_go)
    
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
          d <- d %>% 
            left_join(tbl(con, "vaccine_inputs")) %>% 
            mutate(
              section_code = case_when(
                is_vaccine_input == 1L ~ "vaccine",
                TRUE ~ section_code
              )
            )
        }
        
        if (nchar(inp_pp_section_code()) == 4) {
          d <- d %>% 
            filter(substr(commodity_code, 1, 4) == !!inp_pp_section_code()) 
        } else {
          d <- d %>% 
            filter(section_code == !!inp_pp_section_code())
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
      
      wt_pp$inc(1)
      
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
    
    trd_smr_txt_exp_pp <- eventReactive(input$pp_go, {
      glue("The exports of { section_name_pp() } { exports_growth_increase_decrease_pp() } from 
                          { exp_val_min_yr_2_pp() } in { min(inp_pp_y()) } to { exp_val_max_yr_2_pp() } in { max(inp_pp_y()) } 
                          (annualized { exports_growth_increase_decrease_2_pp() } of { exports_growth_2_pp() }).")
    })
    
    trd_smr_txt_imp_pp <- eventReactive(input$pp_go, {
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
    
    ## 1. upload custom data ----
    
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
    
    ## 2. define model formula ----
    
    lhs_md <- eventReactive(input$md_go, {
      print(inp_md_fml())
      lhs <- gsub("\\s+", "", gsub("~.*", "", inp_md_fml()))
      print(lhs)
      return(lhs)
    })
    
    rhs_md <- eventReactive(input$md_go, {
      rhs <- unlist(strsplit(gsub("\\s+", "", gsub(".*~", "", inp_md_fml())), "\\+"))
      rhs <- rhs[rhs != "+"]
      print(rhs)
      return(rhs)
    })
    
    fml_md <- eventReactive(input$md_go, {
      fml <- paste0(lhs_md(), " ~ ", paste(rhs_md(), collapse = " + "))
      if (inp_md_type() == "olsrem") {
        fml <- paste(fml, "| log(remoteness_exp) + log(remoteness_imp)")
      }
      if (inp_md_type() == "olsfe") {
        fml <- paste(fml, "| reporter_yr + partner_yr")
      }
      print(fml)
      return(fml)
    })
    
    df_dtl_md <- eventReactive(input$md_go, {
      print("Collecting model data...")
      wt_md$start()

      ## 3. read from SQL ----

      d <- tbl(con, "yrpc")

      d <- d %>%
        filter(
          year %in% !!inp_md_y() & reporter_iso %in% !!inp_md_riso()
        )
      
      # if (any(inp_md_riso() != "all")) {
      #   d <- d %>%
      #     filter(
      #       reporter_iso %in% !!inp_md_riso()
      #     )
      # } 
      
      if (any(inp_md_piso() != "all")) {
        d <- d %>%
          filter(
            partner_iso %in% !!inp_md_piso()
          )
      }

      # d <- d %>% collect()

      wt_md$inc(1)

      ## 4. apply filters ----

      if (any(inp_md_product_filter() %in% "vaccine")) {
        d <- d %>% 
          left_join(
            tbl(con, "vaccine_inputs")
          ) %>% 
          mutate(
            section_code = case_when(
              is_vaccine_input == 1L ~ "vaccine",
              TRUE ~ section_code
            )
          )
      }

      if (length(inp_md_product_filter()) > 0) {
        d <- d %>%
          filter(section_code %in% !!inp_md_product_filter())
      }

      wt_md$inc(1)

      d <- d %>%
        select(year, reporter_iso, partner_iso, trade_value_usd_exp, trade_value_usd_imp) %>%
        group_by(year, reporter_iso, partner_iso) %>%
        summarise(
          trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T),
          trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)
        ) %>%
        ungroup() %>%
        collect()
      
      wt_md$inc(1)
      
      ## 5. add geo dist data ----

      d <- d %>%
        mutate(
          country1 = pmin(reporter_iso, partner_iso),
          country2 = pmax(reporter_iso, partner_iso)
        ) %>%
        inner_join(
          tbl(con, "distances") %>% collect(),
          by = c("country1", "country2")
        ) %>%
        select(-c(country1,country2))

      wt_md$inc(1)
      
      ## 6. add RTA data ----

      if (any(rhs_md() %in% "rta")) {
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
              is.na(rta) ~ 0L,
              TRUE ~ rta
            )
          ) %>%
          select(-c(country1,country2))
      }

      wt_md$inc(1)
      
      ## 7. create remoteness indexes ----

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
            remoteness_exp = sum(dist *  total_e / trade_value_usd_exp, na.rm = T)
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
              remoteness_imp = sum(dist / (trade_value_usd_imp / total_y), na.rm = T)
            )

        d <- d %>%
          select(-c(total_e, total_y))
      }

      wt_md$inc(1)
      
      ## 8. create fixed effects ----

      if (inp_md_type() == "olsfe") {
        d <- d %>%
          mutate(
            reporter_yr = paste0(reporter_iso, year),
            partner_yr = paste0(partner_iso, year)
          )
      }

      wt_md$inc(.5)
      
      ## 9. create clustering variable ----

      if (inp_md_cluster() == "yes") {
       d <- d %>%
         mutate(cluster_pairs = paste(reporter_iso, partner_iso, sep = "_"))
      }
      
      wt_md$inc(.5)

      ## 10. convert dollars in time ----

      if (inp_md_convert_dollars() != "No conversion") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_md_convert_dollars()))
      }
      
      ## 11. add MFN data ----
      
      raw_lhs_md <- reactive({
        gsub(".*\\(", "", regmatches(lhs_md(), gregexpr("(?<=\\().*?(?=\\))", lhs_md(), perl = T)))
      })
      
      raw_rhs_md <- reactive({
        gsub(".*\\(", "", regmatches(rhs_md(), gregexpr("(?<=\\().*?(?=\\))", rhs_md(), perl = T)))
      })
      
      if (any(raw_rhs_md() %in% "mfn")) {
        tar <- tbl(con, "tariffs") %>%
          filter(
            year %in% !!inp_md_y()
          )
        
        trd <- tbl(con, "yrpc") %>% 
          filter(
            year %in% !!inp_md_y() & reporter_iso %in% !!inp_md_riso()
          )
        
        if (any(inp_md_piso() != "all")) {
          tar <- tar %>%
            filter(
              # here we need the applied tariffs when the product gets to destination
              reporter_iso %in% !!inp_md_piso()
            )
          
          trd <- tbl(con, "yrpc") %>% 
            filter(
              partner_iso %in% !!inp_md_riso()
            )
        }
        
        if (any(inp_md_product_filter() %in% "vaccine")) {
          trd <- trd %>% 
            left_join(
              tbl(con, "vaccine_inputs")
            ) %>% 
            mutate(
              section_code = case_when(
                is_vaccine_input == 1L ~ "vaccine",
                TRUE ~ section_code
              )
            )
        }
        
        if (length(inp_md_product_filter()) > 0) {
          trd <- trd %>%
            filter(section_code %in% !!inp_md_product_filter())
        }
        
        trd <- trd %>%
          select(year, reporter_iso, partner_iso, commodity_code, trade_value_usd_exp) %>% 
          inner_join(
            tar %>% 
              select(year, partner_iso = reporter_iso, commodity_code, mfn = simple_average),
            by = c("year", "partner_iso", "commodity_code")
          )

        trd <- trd %>%
          select(year, reporter_iso, partner_iso, trade_value_usd_exp, mfn) %>%
          collect() %>%
          group_by(year, reporter_iso, partner_iso) %>%
          summarise(
            mfn = weighted.mean(mfn, trade_value_usd_exp, na.rm = T)
          ) %>%
          ungroup()
        
        rm(tar)

        d <- d %>% inner_join(trd); rm(trade)
      }

      wt_md$inc(1)

      gc()

      print(raw_lhs_md())
      print(raw_rhs_md())
      
      return(
        # this is not elegant, but works well with polynomials, logs, etc in formulas
        d[,
          colnames(d) %in% 
            c("year", "reporter_iso", "partner_iso",
              lhs_md(), rhs_md(), raw_lhs_md(), raw_rhs_md(),
              "remoteness_exp", "remoteness_imp", "cluster_pairs"
            )
        ]
      )
    })
      # bindCache(
      #   inp_md_y(), inp_md_riso(), inp_md_piso(), inp_md_type(), 
      #   inp_md_cluster(), inp_md_convert_dollars(),
      #   inp_md_product_filter(),
      #   gsub("\\,.*", "", gsub(".*\\(", "", regmatches(lhs_md(), gregexpr("(?<=\\().*?(?=\\))", lhs_md(), perl = T))[[1]])),
      #   gsub("\\,.*", "", gsub(".*\\(", "", regmatches(rhs_md(), gregexpr("(?<=\\().*?(?=\\))", rhs_md(), perl = T))[[1]]))
      # ) %>% 
      # bindEvent(input$md_go)
    
    df_dtl_2_md <- eventReactive(input$md_go, {
      ## 12. join with custom data ----
      
      if (nrow(md_custom_data()) > 0) {
        d <- df_dtl_md() %>% inner_join(md_custom_data())
      } else {
        d <- df_dtl_md()
      }
      
      return(d)
    })
    
    fit_md <- eventReactive(input$md_go, {
      fml <- as.formula(fml_md())
      
      if (any(inp_md_type() %in% c("ols", "olsrem", "olsfe"))) {
        if (inp_md_cluster() == "yes") {
          m <- tryCatch(
            feols(fml, df_dtl_2_md(), cluster = ~cluster_pairs),
            error = function(e) {
              feols(COLLINEAR_ESTIMATION ~ COLLINEAR + ESTIMATION, 
                    data = data.frame(
                      COLLINEAR_ESTIMATION = c(1,0,0), 
                      COLLINEAR = c(0,1,0), 
                      ESTIMATION = c(0,0,1)
                    )
              )
            }
          )
        } else {
          m <- tryCatch(
            feols(fml, df_dtl_2_md()),
            error = function(e) {
              feols(COLLINEAR_ESTIMATION ~ COLLINEAR + ESTIMATION, 
                    data = data.frame(
                      COLLINEAR_ESTIMATION = c(1,0,0), 
                      COLLINEAR = c(0,1,0), 
                      ESTIMATION = c(0,0,1)
                    )
              )
            }
          )
        }
      }
      
      if (inp_md_type() == "ppml") {
        if (inp_md_cluster() == "yes") {
          m <- tryCatch(
            feglm(fml, df_dtl_2_md(), cluster = ~cluster_pairs, family = quasipoisson(link = "log")),
            error = function(e) {
              feols(COLLINEAR_ESTIMATION ~ COLLINEAR + ESTIMATION, 
                    data = data.frame(
                      COLLINEAR_ESTIMATION = c(1,0,0), 
                      COLLINEAR = c(0,1,0), 
                      ESTIMATION = c(0,0,1)
                    )
              )
            }
          )
        } else {
          m <- tryCatch(
            feglm(fml, df_dtl_2_md(), family = quasipoisson(link = "log")),
            error = function(e) {
              feols(COLLINEAR_ESTIMATION ~ COLLINEAR + ESTIMATION, 
                    data = data.frame(
                      COLLINEAR_ESTIMATION = c(1,0,0), 
                      COLLINEAR = c(0,1,0), 
                      ESTIMATION = c(0,0,1)
                    )
              )
            }
          )
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
    
    output$title_cp <- renderText({ title_cp() })
    output$title_cc <- renderText({ title_cc() })
    output$title_pp <- renderText({ title_pp() })
    
    # put here to avoid repetition in UI
    legend_txt <- "The information displayed here is based on <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. Please read our <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a> for a full description
      of restrictions and applicable licenses. These figures do not include services or foreign direct investment."
    
    output$title_cp_legend <- renderText({ legend_txt })
    output$title_cc_legend <- renderText({ legend_txt })
    output$title_pp_legend <- renderText({ legend_txt })
    output$title_md_legend <- renderText({ legend_txt })
    
    ## Country profile ----
    
    ### Trade ----
    
    output$trd_stl_cp <- eventReactive(input$cp_go, {
      switch(
        tbl_dtl_cp(),
        "yrc" = glue("Total multilateral Exports and Imports"),
        "yrpc" = glue("Total bilateral Exports and Imports")
      )
    })
    
    output$trd_stl_exp_cp <- eventReactive(input$cp_go, { "Exports" })
    output$trd_stl_imp_cp <- eventReactive(input$cp_go, { "Imports" })
    
    output$trd_smr_exp_cp <- renderText(trd_smr_txt_exp_cp())
    output$trd_smr_imp_cp <- renderText(trd_smr_txt_imp_cp())
    
    output$trd_exc_lines_agg_cp <- renderHighchart({ trd_exc_lines_agg_cp() })
    
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
    
    ## Compare countries ----
    
    output$trd_stl_cc <- eventReactive(input$cc_go, {
      switch(
        tbl_dtl_cc(),
        "yrc" = glue("Total multilateral Exports and Imports"),
        "yrpc" = glue("Total bilateral Exports and Imports")
      )
    })
    
    output$trd_stl_exp_cc <- eventReactive(input$cc_go, { "Exports" })
    output$trd_stl_imp_cc <- eventReactive(input$cc_go, { "Imports" })
    
    output$trd_smr_txt_exp_r1_cc <- renderText(trd_smr_txt_exp_r1_cc())
    output$trd_smr_txt_imp_r1_cc <- renderText(trd_smr_txt_imp_r1_cc())
    output$trd_smr_txt_exp_r2_cc <- renderText(trd_smr_txt_exp_r2_cc())
    output$trd_smr_txt_imp_r2_cc <- renderText(trd_smr_txt_imp_r2_cc())
    
    ### Trade ----
    
    output$trd_exc_cols_agg_r1_cc <- renderHighchart({ trd_exc_cols_agg_r1_cc() })
    output$trd_exc_cols_agg_r2_cc <- renderHighchart({ trd_exc_cols_agg_r2_cc() })
    
    ### Exports ----
    
    output$exp_tt_yr_r1_cc <- renderText(exp_tt_yr_r1_cc())
    output$exp_tm_dtl_yr_r1_cc <- renderHighchart({exp_tm_dtl_yr_r1_cc()})
    
    output$exp_tt_yr_r2_cc <- renderText(exp_tt_yr_r2_cc())
    output$exp_tm_dtl_yr_r2_cc <- renderHighchart({exp_tm_dtl_yr_r2_cc()})
    
    ### Imports ----
    
    output$imp_tt_yr_r1_cc <- renderText(imp_tt_yr_r1_cc())
    output$imp_tm_dtl_yr_r1_cc <- renderHighchart({imp_tm_dtl_yr_r1_cc()})
    
    output$imp_tt_yr_r2_cc <- renderText(imp_tt_yr_r2_cc())
    output$imp_tm_dtl_yr_r2_cc <- renderHighchart({imp_tm_dtl_yr_r2_cc()})
    
    ## Product profile ----
    
    output$trd_stl_pp <- eventReactive(input$pp_go, { "Total multilateral Exports and Imports" })
    
    output$trd_stl_exp_pp <- eventReactive(input$pp_go, { "Exports" })
    output$trd_stl_imp_pp <- eventReactive(input$pp_go, { "Imports" })
    
    output$trd_smr_exp_pp <- renderText(trd_smr_txt_exp_pp())
    output$trd_smr_imp_pp <- renderText(trd_smr_txt_imp_pp())
    
    output$trd_exc_columns_pp <- renderHighchart({ trd_exc_columns_pp() })
    
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
    
    output$variables_desc_md <- renderText({
      "
      <h2>Gravity variables</h2>
      
      <h3>Exports and Imports (LHS)</h3>
      <ul>
       <li>trade_value_usd_exp: Exports in USD of each year</li>
       <li>trade_value_usd_imp: Imports in USD of each year</li>
      </ul>
      
      <h3>Distance for modelling (RHS)</h3>
      <ul>
       <li>dist: Simple distance between most populated cities in km</li>
       <li>distcap: Simple distance between capitals in km</li>
      </ul>
      
      <h3>Additional variables for modelling</h3>
      <ul>
        <li>colony: The two countries are/were in a colonial relation</li>
        <li>comlang Ethno: The two countries have at least 9% of their population speaking the same language</li>
        <li>comlang_off: The two countries share the same official language</li>
        <li>contig: The two countries are next to each other</li>
        <li>rta: The two countries are in a trade agreement</li>
        <li>smctry: The two countries were or are the same country</li>
        <li>mfn: Most Favoured Nation tariff (weighted average by exports)</li>
      </ul>
      "  
    })
    output$df_stl_md <- eventReactive(input$md_go, { "Data preview" })
    output$df_dtl_pre_md <- renderTable({ head(df_dtl_2_md()) })
    output$fit_stl_md <- eventReactive(input$md_go, { "Model summary" })
    output$tidy_md <- renderTable(tidy(fit_md()))
    output$glance_md <- renderTable(glance(fit_md()))
    output$fit_res_md <- eventReactive(input$md_go, { "Model results" })
    output$fit_cat_md <- renderPrint({ fit_md() })
    
    ## Download ----
    
    ### Country profile ----
    
    dwn_cp_stl <- eventReactive(input$cp_go, {
      "Download country profile data"
    })
    
    dwn_cp_txt <- eventReactive(input$cp_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    dwn_cp_fmt <- eventReactive(input$cp_go, {
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
          glue("{ tbl_agg_cp() }_{ inp_cp_r() }_{ inp_cp_p() }_{ min(inp_cp_y()) }_{ max(inp_cp_y()) }.{ inp_cp_f() }")
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
    output$dwn_cp_txt <- renderText({dwn_cp_txt()})
    output$dwn_cp_fmt <- renderUI({dwn_cp_fmt()})
    output$dwn_cp_agg <- renderUI({
      req(input$cp_go)
      downloadButton('dwn_cp_agg_pre', label = 'Aggregated data')
    })
    output$dwn_cp_dtl <- renderUI({
      req(input$cp_go)
      downloadButton('dwn_cp_dtl_pre', label = 'Detailed data')
    })
    
    ### Compare countries ----
    
    dwn_cc_stl <- eventReactive(input$cc_go, {
      "Download country profile data"
    })
    
    dwn_cc_txt <- eventReactive(input$cc_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    dwn_cc_fmt <- eventReactive(input$cc_go, {
      selectInput(
        "cc_f",
        "Download data as:",
        choices = available_formats,
        selected = NULL,
        selectize = TRUE
      )
    })
    
    output$dwn_cc_agg_pre <- downloadHandler(
      filename = function() {
        glue("{ tbl_agg_cc() }_{ inp_cc_r1() }_{ inp_cc_r2() }_{ inp_cc_p() }_{ inp_cc_y() }.{ inp_cc_f() }")
      },
      content = function(filename) {
        rio::export(dplyr::bind_rows(df_agg_r1_cc(), df_agg_r2_cc()), filename)
      },
      contentType = "application/zip"
    )
    
    output$dwn_cc_dtl_pre <- downloadHandler(
      filename = function() {
        glue("{ tbl_dtl_cc() }_{ inp_cc_r1() }_{ inp_cc_r2() }_{ inp_cc_p() }_{ inp_cc_y() }.{ inp_cc_f() }")
      },
      content = function(filename) {
        rio::export(dplyr::bind_rows(df_dtl_r1_cc(), df_dtl_r2_cc()), filename)
      },
      contentType = "application/zip"
    )
    
    output$dwn_cc_stl <- renderText({dwn_cc_stl()})
    output$dwn_cc_txt <- renderText({dwn_cc_txt()})
    output$dwn_cc_fmt <- renderUI({dwn_cc_fmt()})
    output$dwn_cc_agg <- renderUI({
      req(input$cc_go)
      downloadButton('dwn_cc_agg_pre', label = 'Aggregated data')
    })
    output$dwn_cc_dtl <- renderUI({
      req(input$cc_go)
      downloadButton('dwn_cc_dtl_pre', label = 'Detailed data')
    })
    
    ### Product profile ----
    
    dwn_pp_stl <- eventReactive(input$pp_go, {
      "Download product profile data"
    })
    
    dwn_pp_txt <- eventReactive(input$pp_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    dwn_pp_fmt <- eventReactive(input$pp_go, {
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
    output$dwn_pp_txt <- renderText({dwn_pp_txt()})
    output$dwn_pp_fmt <- renderUI({dwn_pp_fmt()})
    output$dwn_pp_agg <- renderUI({
      req(input$pp_go)
      downloadButton('dwn_pp_pre', label = 'Aggregated data')
    })
    
    ### Model ----
    
    dwn_md_stl <- eventReactive(input$md_go, { "Download model data" })
    
    dwn_md_txt <- eventReactive(input$md_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    dwn_md_fmt <- eventReactive(input$md_go, {
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
        saveRDS(fit_md(), filename)
      },
      contentType = "application/zip"
    )
    
    output$dwn_md_stl <- renderText({dwn_md_stl()})
    output$dwn_md_txt <- renderText({dwn_md_txt()})
    output$dwn_md_fmt <- renderUI({dwn_md_fmt()})
    output$dwn_md_dtl <- renderUI({
      req(input$md_go)
      downloadButton('dwn_md_dtl_pre', label = 'Detailed data')
    })
    output$dwn_md_fit <- renderUI({
      req(input$md_go)
      downloadButton('dwn_md_fit_pre', label = 'Fitted model')
    })
    
    ## Cite ----
    
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
        "md_own", "md_f", "cp_f", "cc_f", "sidebarCollapsed", "sidebarItemExpanded"
      ))
      session$doBookmark()
    })
    
    onBookmarked(function(url) {
      updateQueryString(url)
    })
  }
)
