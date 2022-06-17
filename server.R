## server.R ##

shinyServer(
  function(input, output, session) {
    # User inputs ----
    
    observe_helpers()
    
    ## Country profile ----
    
    inp_cp_y <- reactive({
      y <- (min(input$cp_y[1], input$cp_y[2])):(max(input$cp_y[1], input$cp_y[2]))
      y <- seq(min(y), max(y), by = ifelse(max(y) - min(y) >= 10, 2, 1))
      return(y)
    })
    
    inp_cp_r <- reactive({ sort(input$cp_r) })
    inp_cp_p <- reactive({ sort(input$cp_p) })
    
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
    
    inp_cc_r1 <- reactive({ sort(input$cc_r1) })
    inp_cc_r2 <- reactive({ sort(input$cc_r2) })
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
      label = "Section/Commodity:",
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
    
    inp_pp_section_code <- reactive({ sort(input$pp_s) })
    
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
    
    ## Partial Equilibrium Simulation ----
    
    inp_ps_y <- reactive({
      y2 <- (min(input$ps_y[1], input$ps_y[2])):(max(input$ps_y[1], input$ps_y[2]))
      y2 <- seq(min(y2), max(y2), by = input$ps_y_sep)
      return(y2)
    })
    
    inp_ps_riso <- reactive({ sort(input$ps_r) })
    inp_ps_piso <- reactive({ sort(input$ps_p) })
    
    inp_ps_convert_dollars <- reactive({ input$ps_a })
    
    inp_ps_cluster <- reactive({ input$ps_cl })
    
    inp_ps_product_filter <- reactive({ input$ps_pf })
    inp_ps_type <- reactive({ input$ps_t })
    inp_ps_zero <- reactive({ input$ps_zero })
    inp_ps_fml <- reactive({ input$ps_fml })
    inp_ps_f <- reactive({ input$ps_f })
    
    inp_ps_sp <- reactive({ input$ps_sp })
    inp_ps_sra <- reactive({ input$ps_sra })
    inp_ps_sy <- reactive({ input$ps_sy })
    
    ## Simulate ----
    
    # inp_si_y <- reactive({
    #   y2 <- (min(input$si_y[1], input$si_y[2])):(max(input$si_y[1], input$si_y[2]))
    #   y2 <- seq(min(y2), max(y2), by = input$si_y_sep)
    #   return(y2)
    # })
    # 
    # inp_si_y2 <- reactive({ input$si_y2 })
    # inp_si_countries <- reactive({ input$si_c })
    # inp_si_reference <- reactive({ input$si_r })
    # inp_si_convert_dollars <- reactive({ input$si_a })
    # inp_si_sigma <- reactive({ input$si_s })
    
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
    
    # Partial Equilibrium Simulation ----
    
    wt_ps <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
    
    ## 1. upload custom data ----
    
    ps_custom_data <- eventReactive(input$ps_go, {
      uploaded_file <- input$ps_own
      
      if(!is.null(uploaded_file)) {
        inp_data <- rio::import(file = uploaded_file$datapath, format = tools::file_ext(uploaded_file$name)) %>%
          janitor::clean_names()
        
        return(inp_data)
      } else {
        data.frame()
      }
    })
    
    ## 2. define model formula ----
    
    lhs_ps <- eventReactive(input$ps_go, {
      lhs <- gsub("\\s+", "", gsub("~.*", "", inp_ps_fml()))
      return(lhs)
    })
    
    rhs_ps <- eventReactive(input$ps_go, {
      rhs <- unlist(strsplit(gsub("\\s+", "", gsub(".*~", "", inp_ps_fml())), "\\+"))
      rhs <- sort(rhs[rhs != "+"])
      return(rhs)
    })
    
    raw_lhs_ps <- reactive({
      x <- unlist(regmatches(lhs_ps(), gregexpr("(?<=\\().*?(?=\\))", lhs_ps(), perl = T)))
      x <- x[x != ""]
      return(x)
    })
    
    raw_rhs_ps <- reactive({
      x <- unlist(regmatches(rhs_ps(), gregexpr("(?<=\\().*?(?=\\))", rhs_ps(), perl = T)))
      x <- x[x != ""]
      return(x)
    })
    
    fml_ps <- eventReactive(input$ps_go, {
      fml <- paste0(lhs_ps(), " ~ ", paste(rhs_ps(), collapse = " + "))
      if (inp_ps_type() == "olsfe") {
        fml <- paste(fml, "| reporter_yr + partner_yr")
      }
      # print(fml)
      return(fml)
    })
    
    ## 3. read from SQL ----
    
    df_dtl_ps <- reactive({
      print("Collecting model data...")
      wt_ps$start()
      
      ### 3.1. apply filters ----
      
      tbl_sql <- if (any(inp_ps_product_filter() != "all")) {
        "yrpc"
      } else {
        "yrp"
      }
                 
      d <- tbl(con, tbl_sql)
      
      d <- d %>%
        filter(
          year %in% !!inp_ps_y() & reporter_iso != partner_iso
        )
      
      if (any(inp_ps_riso() != "all")) {
        d <- d %>%
          filter(
            reporter_iso %in% !!inp_ps_riso()
          )
      }
      
      if (any(inp_ps_piso() != "all")) {
        d <- d %>%
          filter(
            partner_iso %in% !!inp_ps_piso()
          )
      }
      
      # d <- d %>% collect()
      
      wt_ps$inc(1)
      
      if (any(inp_ps_product_filter() %in% "vaccine")) {
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
      
      if (any(inp_ps_product_filter() != "all")) {
        d <- d %>%
          filter(section_code %in% !!inp_ps_product_filter())
      }
      
      wt_ps$inc(1)
      
      #### aggregate data ----
      
      d <- d %>%
        select(year, importer = reporter_iso, exporter = partner_iso, 
               trade = trade_value_usd_imp) %>%
        group_by(year, importer, exporter) %>%
        summarise(trade = sum(trade, na.rm = T)) %>%
        ungroup()
      
      if (inp_ps_zero() == "yes") {
        d <- d %>% 
          filter(trade > 0)
      }
      
      #### collect data ----
      
      d <- d %>% 
        collect() %>% 
        arrange(year, importer, exporter)
      
      wt_ps$inc(1)
      
      ### 3.2. add geo dist data ----
      
      d <- d %>%
        mutate(
          country1 = pmin(importer, exporter),
          country2 = pmax(importer, exporter)
        ) %>%
        inner_join(
          tbl(con, "distances") %>% collect(),
          by = c("country1", "country2")
        ) %>%
        select(-c(country1,country2))
      
      wt_ps$inc(1)
      
      ### 3.3. add RTA data ----
      
      if (any(rhs_ps() %in% "rta")) {
        d <- d %>%
          mutate(
            country1 = pmin(importer, exporter),
            country2 = pmax(importer, exporter)
          ) %>%
          left_join(
            tbl(con, "rtas") %>%
              filter(year %in% !!inp_ps_y()) %>%
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
      
      wt_ps$inc(2)
      
      ### 3.4. create fixed effects ----
      
      if (inp_ps_type() == "olsfe") {
        d <- d %>%
          mutate(
            importer_yr = paste0(importer, year),
            exporter_yr = paste0(exporter, year)
          )
      }
      
      wt_ps$inc(.5)
      
      ### 3.5. create clustering variable ----
      
      if (inp_ps_cluster() == "yes") {
        d <- d %>%
          mutate(imp_exp = paste(importer, exporter, sep = "_"))
      }
      
      wt_ps$inc(.5)
      
      ### 3.6. add GDP / GDP percap ----
      
      if (any(c(raw_rhs_ps(), rhs_ps()) %in% 
              c("gdp_importer", "gdp_percap_importer", "gdp_exporter", "gdp_percap_exporter"))) {
        d <- d %>% 
          inner_join(
            tbl(con, "gdp") %>% 
              filter(year %in% !!inp_ps_y()) %>% 
              select(country_iso, year, gdp_importer = gdp, gdp_percap_importer = gdp_percap) %>% 
              collect(),
            by = c("importer" = "country_iso", "year")
          )
        
        d <- d %>% 
          inner_join(
            tbl(con, "gdp") %>% 
              filter(year %in% !!inp_ps_y()) %>% 
              select(country_iso, year, gdp_exporter = gdp, gdp_percap_exporter = gdp_percap) %>% 
              collect(),
            by = c("exporter" = "country_iso", "year")
          )
      }
      
      ### 3.7. convert dollars in time ----
      
      # CHECK LATER ----
      
      # if (inp_ps_convert_dollars() != "No conversion") {
      #   d <- gdp_deflator_adjustment(d, as.integer(inp_ps_convert_dollars()))
      # }
      
      ### 3.8. add MFN data ----
      
      if (any(c(raw_rhs_ps(), rhs_ps()) %in% "mfn")) {
        tar <- tbl(con, "tariffs") %>%
          filter(
            year %in% !!inp_ps_y()
          )
        
        trd <- tbl(con, "yrpc") %>% 
          filter(
            year %in% !!inp_ps_y()
          )
        
        if (any(inp_ps_piso() != "all")) {
          tar <- tar %>%
            filter(
              # here we need the applied tariffs when the product gets to destination
              reporter_iso %in% !!inp_ps_piso()
            )
          
          trd <- tbl(con, "yrpc") %>% 
            filter(
              partner_iso %in% !!inp_ps_riso()
            )
        }
        
        if (any(inp_ps_product_filter() %in% "vaccine")) {
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
        
        if (any(inp_ps_product_filter() != "all")) {
          trd <- trd %>%
            filter(section_code %in% !!inp_ps_product_filter())
        }
        
        trd <- trd %>%
          select(year, reporter_iso, partner_iso, commodity_code, trade_value_usd_imp) %>% 
          inner_join(
            tar %>% 
              select(year, reporter_iso, commodity_code, mfn = simple_average),
            by = c("year", "reporter_iso", "commodity_code")
          )
        
        trd <- trd %>%
          select(year, importer = reporter_iso, exporter = partner_iso,
                 trade = trade_value_usd_imp, mfn) %>%
          collect() %>%
          group_by(year, importer, exporter) %>%
          summarise(
            mfn = weighted.mean(mfn, trade, na.rm = T)
          ) %>%
          ungroup()
        
        rm(tar)
        
        d <- d %>% 
          inner_join(trd)
        
        rm(trade)
      }
      
      wt_ps$inc(1)
      
      gc()
      
      # print(c(raw_lhs_ps(), raw_rhs_ps()))
      print(colnames(d))
      
      return(
        # this is not elegant, but works well with polynomials, logs, etc in formulas
        d[,
          colnames(d) %in% 
            c("year", "importer", "exporter",
              lhs_ps(), rhs_ps(), raw_lhs_ps(), raw_rhs_ps(),
              "remoteness_exp", "remoteness_imp", "imp_exp"
            )
        ]
      )
    }) %>% 
      bindCache(
        inp_ps_y(), inp_ps_riso(), inp_ps_piso(), inp_ps_type(), inp_ps_zero(),
        inp_ps_convert_dollars(), inp_ps_cluster(), inp_ps_product_filter(),
        fml_ps(), lhs_ps(), rhs_ps(), raw_lhs_ps(), raw_rhs_ps()
      ) %>%
      bindEvent(input$ps_go)
    
    df_dtl_2_ps <- eventReactive(input$ps_go, {
      ### 3.8. join with custom data ----
      
      if (nrow(ps_custom_data()) > 0) {
        d <- df_dtl_ps() %>% inner_join(ps_custom_data())
      } else {
        d <- df_dtl_ps()
      }
      
      d <- d %>% select(year, everything())
      
      # print(d)
      return(d)
    })
    
    ## 4. Fit model ----
    
    fit_ps <- eventReactive(input$ps_go, {
      print("Fitting model...")
      
      fml <- as.formula(fml_ps())
      
      if (any(inp_ps_type() %in% c("ols", "olsfe"))) {
        if (inp_ps_cluster() == "yes") {
          m <- tryCatch(
            feols(fml, df_dtl_2_ps(), cluster = ~imp_exp),
            error = function(e) { custom_regression_error() }
          )
        } else {
          m <- tryCatch(
            feols(fml, df_dtl_2_ps()),
            error = function(e) { custom_regression_error() }
          )
        }
      }
      
      if (inp_ps_type() == "ppml") {
        if (inp_ps_cluster() == "yes") {
          m <- tryCatch(
            feglm(fml, df_dtl_2_ps(), cluster = ~imp_exp,
                  family = quasipoisson(link = "log")),
            error = function(e) { custom_regression_error() }
          )
        } else {
          m <- tryCatch(
            feglm(fml, df_dtl_2_ps(), family = quasipoisson(link = "log")),
            error = function(e) { custom_regression_error() }
          )
        }
      }
      
      wt_ps$inc(1)
      gc()
      
      return(m)
    })
    
    ## 5. Simulate ----
    
    pred_trade_lines_ps <- eventReactive(input$ps_go, {
      # print(inp_ps_sy()) # print(inp_ps_sp()) # print(inp_ps_sra())
      
      d <- df_dtl_2_ps() %>%
        filter(
          exporter %in% !!inp_ps_sp()
          # importer %in% !!inp_ps_sp()
        )
      
      d <- d %>% mutate(`UNFEASIBLE` = NA_real_, `ESTIMATION` = NA_real_)
      
      d <- d %>%
        mutate(predicted_trade = predict(fit_ps(), newdata = d)) %>%
        ungroup() %>% 
        select(year, exporter, trade, predicted_trade) %>%
        group_by(year, exporter) %>%
        # summarise_if(is.numeric, sum, na.rm = T)
        summarise(
          trade = sum(trade, na.rm = T),
          predicted_trade = sum(predicted_trade, na.rm = T)
        )
      
      d <- d %>%
        pivot_longer(trade:predicted_trade, names_to = "variable", values_to = "value")
      
      d <- d %>% 
        mutate(
          variable = case_when(
            variable == "trade" ~ "Observed trade",
            TRUE ~ "Predicted trade"
          )
        )
      
      d2 <- df_dtl_2_ps() %>%
        filter(
          exporter %in% !!inp_ps_sp()
          # importer %in% !!inp_ps_sp()
        ) %>% 
        mutate(
          rta = case_when(
            year >= !!inp_ps_sy() ~ as.integer(!!inp_ps_sra()),
            TRUE ~ rta
          )
        )

      d2 <- d2 %>% mutate(`UNFEASIBLE` = NA_real_, `ESTIMATION` = NA_real_)
      
      d2 <- d2 %>%
        mutate(predicted_trade = predict(fit_ps(), newdata = d2)) %>%
        select(year, exporter, predicted_trade) %>%
        group_by(year, exporter) %>%
        summarise(
          predicted_trade = sum(predicted_trade, na.rm = T)
        )
      
      d2 <- d2 %>%
        pivot_longer(predicted_trade, names_to = "variable", values_to = "value")

      d2 <- d2 %>% 
        mutate(variable = "Predicted trade (altered RTA)")
      
      d <- d %>% bind_rows(d2); rm(d2)
      
      # d <- d %>% 
      #   mutate(variable = paste(exporter, variable)) %>% 
      #   select(-exporter)
      
      d$variable <- factor(d$variable,
                           levels = c("Observed trade", "Predicted trade", "Predicted trade (altered RTA)"))
      
      wt_ps$inc(1)

      # d <- d %>% 
      #   pivot_wider(values_from = "value", names_from = "variable")
      
      # g <- hchart(d,
      #        "column",
      #        hcaes(x = year, y = value, group = variable),
      #        tooltip = list(
      #          pointFormatter = custom_tooltip_short()
      #        )) %>%
      #   hc_xAxis(title = list(text = "Year")) %>%
      #   hc_yAxis(title = list(text = "USD billion"),
      #            labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
      #   hc_title(text = "Aggregated effect on exports for the selected countries")

      g <- ggplot(d) +
        geom_col(aes(x = as.factor(year), y = value / 1000000000, fill = variable),
                 position = "dodge2") +
        facet_wrap(~exporter, scales = "free_y") +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(
          x = "Year",
          y = "USD billion",
          title = "Aggregated effect on exports for the selected countries"
        )
      
      wt_ps$close()
      return(g)
    })
    
    # Simulate ----
    
    # wt_si <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
    # 
    # ## 1. read from SQL ----
    # 
    # df_dtl_si <- reactive({
    #   print("Collecting simulation data...")
    #   wt_si$start()
    #   
    #   ### 3.1. apply filters ----
    #   
    #   d <- tbl(con, "yrp") %>%
    #     filter(year %in% !!inp_si_y() & reporter_iso != partner_iso)
    #   
    #   # print(inp_si_y())
    #   # print(inp_si_y2())
    #   
    #   # d <- tradepolicy::agtpa_applications %>% 
    #   #   filter(year %in% !!inp_si_y()) %>% 
    #   #   mutate(
    #   #     exporter = tolower(exporter),
    #   #     importer = tolower(importer)
    #   #   )
    #   
    #   ### 3.2. aggregate and transform data ----
    #   
    #   d <- d %>%
    #     select(year, importer = reporter_iso, exporter = partner_iso,
    #            trade = trade_value_usd_imp)
    # 
    #   # add GRAVITY variables
    #   
    #   d <- d %>%
    #     mutate(
    #       country1 = pmin(importer, exporter, na.rm = T),
    #       country2 = pmax(importer, exporter, na.rm = T)
    #     ) %>%
    #     inner_join(
    #       tbl(con, "distances") %>%
    #         select(country1, country2, dist, contig, comlang_off, colony),
    #       by = c("country1", "country2")
    #     )
    #   
    #   # add RTA data
    #   
    #   d <- d %>%
    #     left_join(
    #       tbl(con, "rtas") %>%
    #         filter(year %in% !!inp_si_y()),
    #       by = c("year", "country1", "country2")
    #     ) %>%
    #     mutate(
    #       rta = case_when(
    #         is.na(rta) ~ 0L,
    #         TRUE ~ rta
    #       )
    #     ) %>%
    #     select(-c(country1,country2))
    #   
    #   # transform factors
    #   
    #   d <- d %>% 
    #     mutate(
    #       intl = ifelse(importer != exporter, 1, 0),
    #       importer = case_when(
    #         importer == !!inp_si_reference() ~ paste0("0-", !!inp_si_reference()),
    #         TRUE ~ importer
    #       ),
    #       exporter = case_when(
    #         exporter == !!inp_si_reference() ~ paste0("0-", !!inp_si_reference()),
    #         TRUE ~ exporter
    #       )
    #     )
    #   
    #   d <- d %>% 
    #     # Create Eit
    #     group_by(importer, year) %>%
    #     mutate(e = sum(trade, na.rm = T)) %>% 
    #     
    #     # Create Yit
    #     group_by(exporter, year) %>%
    #     mutate(y = sum(trade, na.rm = T)) %>%
    #     
    #     # Create Er
    #     group_by(year) %>%
    #     mutate(e_r = max(
    #       case_when(
    #         importer == paste0("0-", !!inp_si_reference()) ~ e,
    #         TRUE ~ NA_real_
    #       ), 
    #       na.rm = T
    #     ))
    #   
    #   d <- d %>% 
    #     # Pairing variable for the internal dyads for the fixed effects
    #     mutate(
    #       exp_year = paste(exporter, year, sep = "_"),
    #       imp_year = paste(importer, year, sep = "_"),
    #       imp_exp = paste(importer, exporter, sep = "_"),
    #       imp_exp_2 = case_when(
    #         exporter == importer ~ "0-intra",
    #         TRUE ~ imp_exp
    #       )
    #     )
    #   
    #   d <- d %>% 
    #     # To filter the cases where the sum by dyad is zero
    #     group_by(imp_exp) %>%
    #     mutate(sum_trade = sum(trade, na.rm = T)) %>% 
    #     ungroup()
    #   
    #   ### 3.3 collect data ----
    #   
    #   d <- d %>%
    #     collect() %>%
    #     arrange(year, importer, exporter)
    #   
    #   wt_si$inc(1)
    #   
    #   ### 3.4. convert dollars in time ----
    #   
    #   if (inp_si_convert_dollars() != "No conversion") {
    #     d <- gdp_deflator_adjustment(d, as.integer(inp_si_convert_dollars()))
    #   }
    #   
    #   wt_si$inc(1)
    #   
    #   gc()
    #   
    #   # print(d)
    #   
    #   return(d)
    # })
    # 
    # ## 2. Fit models ----
    # 
    # df_fit_si <- eventReactive(input$si_go, {
    #   print("Fitting model...")
    #   
    #   ### Step I: Solve the baseline gravity model ----
    #   
    #   d <- df_dtl_si()
    #   
    #   #### Fit baseline ----
    #   
    #   fit_baseline <- fepois(
    #     trade ~ rta | exp_year + imp_year + imp_exp_2,
    #     data = filter(d, sum_trade > 0),
    #     glm.iter = 100
    #   )
    #   
    #   wt_si$inc(1)
    #   
    #   d <- d %>%
    #     mutate(
    #       fe_exporter_bln = fixef(fit_baseline)$exp_year[exp_year],
    #       fe_importer_bln = fixef(fit_baseline)$imp_year[imp_year],
    #       fe_imp_exp_2_bln = fixef(fit_baseline)$imp_exp_2[imp_exp_2]
    #     )
    #   
    #   d <- d %>%
    #     mutate(
    #       tij_bar = exp(fe_imp_exp_2_bln),
    #       tij_bln = exp(fe_imp_exp_2_bln + fit_baseline$coefficients["rta"] * rta)
    #     )
    #   
    #   d_slice <- d %>%
    #     filter(year == !!inp_si_y2(), exporter != importer)
    #   
    #   print(d)
    #   print(d_slice)
    # 
    #   #### Fit costs ----
    #   
    #   fit_costs <- fepois(
    #     tij_bar ~ log(dist) + contig + comlang_off + colony | exporter + importer,
    #     # tij_bar ~ log(dist) + cntg + lang + clny | exporter + importer,
    #     data = d_slice,
    #     glm.iter = 100
    #   )
    #   
    #   wt_si$inc(1)
    #   
    #   d_slice <- d_slice %>%
    #     mutate(tij_no_rta = predict(fit_costs, d_slice)) %>%
    #     select(exporter, importer, tij_no_rta)
    #   
    #   d <- d %>%
    #     filter(year == !!inp_si_y2()) %>%
    #     left_join(d_slice, by = c("importer", "exporter")) %>%
    #     mutate(
    #       tij_bar = case_when(
    #         is.na(tij_bar) ~ tij_no_rta, 
    #         TRUE ~ tij_bar
    #       ),
    #       tij_bln = case_when(
    #         is.na(tij_bln) ~ tij_bar * exp(fit_baseline$coefficients["rta"] * rta),
    #         TRUE ~ tij_bln
    #       )
    #     ) %>%
    #     select(-tij_no_rta)
    #   
    #   #### Fit constrained ----
    #   
    #   fit_constrained <- fepois(
    #     trade ~ 0 | exporter + importer,
    #     data = d,
    #     offset = ~log(tij_bln),
    #     glm.iter = 100
    #   )
    #   
    #   wt_si$inc(1)
    #   
    #   d <- d %>%
    #     mutate(tradehat_bln = predict(fit_constrained, d)) %>%
    #     group_by(exporter) %>%
    #     mutate(xi_bln = sum(tradehat_bln * (exporter != importer))) %>%
    #     ungroup()
    #   
    #   d <- d %>%
    #     mutate(
    #       fe_exporter_cns = fixef(fit_constrained)$exporter[exporter],
    #       fe_importer_cns = fixef(fit_constrained)$importer[importer]
    #     )
    #   
    #   d <- d %>%
    #     mutate(
    #       omr_bln = y * e_r/ exp(fe_exporter_cns),
    #       imr_bln = e / (exp(fe_importer_cns) * e_r)
    #     )
    #   
    #   ### Step II: Define a counterfactual scenario ----
    #   
    #   d <- d %>%
    #     mutate(
    #       no_rta = ifelse(
    #         exporter %in% !!inp_si_countries() & 
    #           importer %in% !!inp_si_countries() &
    #           year >= !!inp_si_y2(), 0, rta
    #       ),
    #       tij_cfl = tij_bar * exp(fit_baseline$coefficients["rta"] * no_rta)
    #     )
    #   
    #   ### Step III: Solve the counterfactual model ----
    #   
    #   fit_counterfactual <- fepois(
    #     trade ~ 0 | exporter + importer,
    #     data = d,
    #     offset = ~log(tij_cfl),
    #     glm.iter = 100
    #   )
    #   
    #   wt_si$inc(1)
    #   
    #   d <- d %>%
    #     mutate(
    #       fe_exporter_cfl = fixef(fit_counterfactual)$exporter[exporter],
    #       fe_importer_cfl = fixef(fit_counterfactual)$importer[importer]
    #     )
    #   
    #   d <- d %>%
    #     mutate(
    #       omr_cfl = y * e_r / exp(fe_exporter_cfl),
    #       imr_cfl = e / (exp(fe_importer_cfl) * e_r)
    #     )
    #   
    #   d <- d %>%
    #     mutate(tradehat_cfl = predict(fit_counterfactual, d)) %>%
    #     group_by(exporter) %>%
    #     mutate(xi_cfl = sum(tradehat_cfl * (exporter != importer))) %>%
    #     ungroup()
    #   
    #   sigma <- inp_si_sigma()
    #   
    #   d <- d %>%
    #     mutate(
    #       change_tij = tij_cfl / tij_bln,
    #       phi = ifelse(importer == exporter, e / y, 0)
    #     ) %>%
    #     group_by(exporter) %>%
    #     mutate(phi = max(phi)) %>%
    #     ungroup()
    #   
    #   d <- d %>%
    #     group_by(exporter) %>%
    #     mutate(change_p_i = ((exp(fe_exporter_cfl) / e_r) / (exp(fe_exporter_cns) / e_r))^(1 /(1 - sigma))) %>%
    #     ungroup() %>%
    #     
    #     group_by(importer) %>%
    #     mutate(
    #       change_p_j = ifelse(importer == exporter, change_p_i, 0),
    #       change_p_j = max(change_p_j)
    #     ) %>%
    #     ungroup()
    #   
    #   d <- d %>%
    #     mutate(trade_cfl = tradehat_cfl * change_p_i * change_p_j)
    #   
    #   d <- d %>%
    #     mutate(
    #       omr_cfl_0 = omr_cfl,
    #       imr_cfl_0 = imr_cfl,
    #       change_imr_full_0 = 1,
    #       change_omr_full_0 = 1,
    #       change_p_i_0 = change_p_i,
    #       change_p_j_0 = change_p_j,
    #       fe_exporter_cfl_0 = fe_exporter_cfl,
    #       fe_importer_cfl_0 = fe_importer_cfl,
    #       tradehat_0 = tradehat_cfl,
    #       e_r_cfl_0 = e_r
    #     )
    #   
    #   #### set parameters ----
    #   max_dif <- 1
    #   sd_dif <- 1
    #   change_price_i_old <- 0
    #   
    #   #### run loop ----
    #   i <- 1
    #   while(sd_dif > 1e-3 | max_dif > 1e-3) {
    #     cat(paste0(i," "))
    #     d <- d %>%
    #       mutate(trade_1 = tradehat_0 * change_p_i_0 * change_p_j_0 / 
    #                (change_omr_full_0 * change_imr_full_0))
    #     
    #     # repeat the counterfactual model
    #     fit_counterfactual_2 <- fepois(
    #       trade_1 ~ 0 | exporter + importer,
    #       data = d,
    #       offset = ~log(tij_cfl),
    #       glm.iter = 100
    #     )
    #     
    #     wt_si$inc(.5)
    #     
    #     d <- d %>%
    #       mutate(
    #         fe_exporter_cfl = fixef(fit_counterfactual_2)$exporter[exporter],
    #         fe_importer_cfl = fixef(fit_counterfactual_2)$importer[importer]
    #       )
    #     
    #     # compute the conditional general equilibrium effects of trade
    #     d <- d %>%
    #       mutate(tradehat_1 = predict(fit_counterfactual_2, d)) %>%
    #       group_by(exporter) %>%
    #       mutate(y_cfl_1 = sum(tradehat_1)) %>%
    #       ungroup() %>%
    #       
    #       mutate(e_cfl_1 = ifelse(importer == exporter, phi * y_cfl_1, 0)) %>%
    #       group_by(importer) %>%
    #       mutate(e_cfl_1 = max(e_cfl_1)) %>%
    #       ungroup() %>%
    #       
    #       mutate(
    #         e_r_cfl_1 = case_when(
    #           importer == paste0("0-", inp_si_reference()) ~ e_cfl_1,
    #           TRUE ~ 0
    #         ),
    #         e_r_cfl_1 = max(e_r_cfl_1)
    #       )
    #     
    #     # compute the change in prices for exporters and importers
    #     d <- d %>%
    #       mutate(change_p_i_1 = ((exp(fe_exporter_cfl) / e_r_cfl_1) /
    #                                (exp(fe_exporter_cfl_0) / e_r_cfl_0))^(1 / (1 - sigma)))
    #     
    #     # compute the change in prices for exporters and importers
    #     d <- d %>%
    #       group_by(importer) %>%
    #       mutate(
    #         change_p_j_1 = ifelse(importer == exporter, change_p_i_1, 0),
    #         change_p_j_1 = max(change_p_j_1)
    #       ) %>%
    #       ungroup()
    #     
    #     # compute both outward and inward multilateral resistance
    #     d <- d %>%
    #       mutate(
    #         omr_cfl_1 = (y_cfl_1 * e_r_cfl_1) / exp(fe_exporter_cfl),
    #         imr_cfl_1 = e_cfl_1 / (exp(fe_importer_cfl) * e_r_cfl_1)
    #       )
    #     
    #     # update the differences
    #     max_dif <- abs(max(d$change_p_i_0 - change_price_i_old))
    #     sd_dif <- sd(d$change_p_i_0 - change_price_i_old)
    #     change_price_i_old <- d$change_p_i_0
    #     
    #     # compute changes in outward and inward multilateral resistance
    #     d <- d %>%
    #       mutate(
    #         change_omr_full_1 = omr_cfl_1 / omr_cfl_0,
    #         change_imr_full_1 = imr_cfl_1 / imr_cfl_0,
    #         omr_cfl_0 = omr_cfl_1,
    #         imr_cfl_0 = imr_cfl_1,
    #         change_omr_full_0 = change_omr_full_1,
    #         change_imr_full_0 = change_imr_full_1,
    #         change_p_i_0 = change_p_i_1,
    #         change_p_j_0 = change_p_j_1,
    #         fe_exporter_cfl_0 = fe_exporter_cfl,
    #         fe_importer_cfl_0 = fe_importer_cfl,
    #         tradehat_0 = tradehat_1,
    #         e_r_cfl_0 = e_r_cfl_1
    #       ) %>%
    #       select(-fe_exporter_cfl, -fe_importer_cfl)
    #     
    #     i <- i + 1
    #   }
    #   
    #   d <- d %>%
    #     mutate(
    #       change_p_i_full = ((exp(fe_exporter_cfl_0) / e_r_cfl_0) /
    #                            (exp(fe_exporter_cns) / e_r))^(1 / (1 - sigma)),
    #       change_p_j_full = change_p_i_full * (exporter == importer)
    #     ) %>%
    #     group_by(importer) %>%
    #     mutate(change_p_j_full = max(change_p_j_full)) %>%
    #     ungroup() %>%
    #     mutate(y_full = change_p_i_full * y)
    #   
    #   d <- d %>%
    #     mutate(e_full = change_p_j_full * e * (exporter == importer)) %>%
    #     group_by(importer) %>%
    #     mutate(e_full = max(e_full, na.rm = TRUE)) %>%
    #     ungroup() %>%
    #     mutate(
    #       e_full_r = e_full * (importer == "0-DEU"),
    #       e_full_r = max(e_full_r)
    #     )
    #   
    #   d <- d %>%
    #     mutate(
    #       omr_full = y_full * e_r_cfl_0 / exp(fe_exporter_cfl_0),
    #       imr_full = e_cfl_1 / (exp(fe_importer_cfl_0) * e_r_cfl_0)
    #     )
    #   
    #   d <- d %>%
    #     mutate(x_full = (y_full * e_full * tij_cfl) / (imr_full * omr_full)) %>%
    #     group_by(exporter) %>%
    #     mutate(xi_full = sum(x_full * (importer != exporter))) %>%
    #     ungroup()
    #   
    #   exporter_indexes <- d %>%
    #     select(
    #       exporter, starts_with("omr_"), change_p_i_full,
    #       starts_with("xi_"), y, y_full
    #     ) %>%
    #     distinct() %>%
    #     # mutate(exporter = ifelse(exporter == "0-DEU", "DEU", exporter)) %>%
    #     arrange(exporter) %>%
    #     mutate(
    #       change_p_i_full = (1 - change_p_i_full) * 100,
    #       change_omr_cfl = ((omr_bln / omr_cfl)^(1 / (1-sigma)) - 1) * 100,
    #       change_omr_full = ((omr_bln / omr_full)^(1 / (1-sigma)) - 1) * 100,
    #       change_xi_cfl = (xi_bln / xi_cfl - 1) * 100,
    #       change_xi_full = (xi_bln / xi_full - 1) * 100
    #     ) %>%
    #     select(exporter, starts_with("change"), starts_with("y"))
    #   
    #   importer_indexes <- d %>%
    #     select(importer, imr_bln, imr_cfl, imr_full) %>%
    #     distinct() %>%
    #     # mutate(importer = ifelse(importer == "0-DEU", "DEU", importer)) %>%
    #     arrange(importer) %>%
    #     mutate(
    #       change_imr_cfl = ((imr_bln / imr_cfl)^(1 / (1 - sigma)) - 1) * 100,
    #       change_imr_full = ((imr_bln / imr_full)^(1 / (1 - sigma)) - 1) * 100
    #     )
    #   
    #   indexes_final <- exporter_indexes %>%
    #     left_join(importer_indexes, by = c("exporter" = "importer")) %>%
    #     mutate(
    #       rgdp_bln = y / (imr_bln^(1 / (1 - sigma))),
    #       rgdp_full = y_full / (imr_full^(1 / (1 - sigma))),
    #       change_rgdp_full = (rgdp_bln / rgdp_full - 1) * 100
    #     ) %>%
    #     select(exporter, change_xi_cfl, change_xi_full,
    #            change_rgdp_full, change_imr_full, change_omr_full, change_p_i_full)
    #   
    #   indexes_final <- indexes_final %>%
    #     mutate_if(is.numeric, function(x) round(x, 2))
    #   
    #   wt_si$close()
    #   return(indexes_final)
    # })
    
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
    output$title_ps_legend <- renderText({ legend_txt })
    output$title_si_legend <- renderText({ legend_txt })
    
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
    
    output$df_stl_ps <- eventReactive(input$ps_go, { "Data preview" })
    output$df_dtl_pre_ps <- renderTable({ head(df_dtl_2_ps()) })
    output$fit_stl1_ps <- eventReactive(input$ps_go, { "Model summary" })
    output$tidy_ps <- renderTable(tidy(fit_ps()))
    output$glance_ps <- renderTable(glance(fit_ps()))
    output$fit_stl2_ps <- eventReactive(input$ps_go, { "Model results" })
    output$fit_cat_ps <- renderPrint({ fit_ps() })
    output$pred_stl_ps <- eventReactive(input$ps_go, { "Model simulation" })
    output$pred_trade_lines_ps <- renderPlot({ pred_trade_lines_ps() })
    
    ## Simulate ----
    
    # output$df_stl_si <- eventReactive(input$sim_go, { "Data preview" })
    # output$df_fit_si <- renderDataTable({ df_fit_si() })
    
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
    
    ### Partial Equilibrium Simulation ----
    
    dwn_ps_stl <- eventReactive(input$ps_go, { "Download model data" })
    
    dwn_ps_txt <- eventReactive(input$ps_go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."
    })
    
    dwn_ps_fmt <- eventReactive(input$ps_go, {
      selectInput(
        "ps_f",
        "Download data as:",
        choices = available_formats,
        selected = NULL,
        selectize = TRUE
      )
    })
    
    output$dwn_ps_dtl_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_ps_type() }_{ inp_ps_riso() }_{ inp_ps_piso() }_{ min(inp_ps_y()) }_{ max(inp_ps_y()) }.{ inp_ps_f() }")
      },
      content = function(filename) {
        rio::export(df_dtl_ps(), filename)
      },
      contentType = "application/zip"
    )
    
    output$dwn_ps_fit_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_ps_type() }_{ inp_ps_riso() }_{ inp_ps_piso() }_{ min(inp_ps_y()) }_{ max(inp_ps_y()) }.rds")
      },
      content = function(filename) {
        saveRDS(fit_ps(), filename)
      },
      contentType = "application/zip"
    )
    
    output$dwn_ps_stl <- renderText({dwn_ps_stl()})
    output$dwn_ps_txt <- renderText({dwn_ps_txt()})
    output$dwn_ps_fmt <- renderUI({dwn_ps_fmt()})
    output$dwn_ps_dtl <- renderUI({
      req(input$ps_go)
      downloadButton('dwn_ps_dtl_pre', label = 'Detailed data')
    })
    output$dwn_ps_fit <- renderUI({
      req(input$ps_go)
      downloadButton('dwn_ps_fit_pre', label = 'Fitted model')
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
        "ps_own", "ps_f", "cp_f", "cc_f", "sidebarCollapsed", "sidebarItemExpanded",
        "shinyhelper-modal_params"
      ))
      session$doBookmark()
    })
    
    onBookmarked(function(url) {
      updateQueryString(url)
    })
  }
)
