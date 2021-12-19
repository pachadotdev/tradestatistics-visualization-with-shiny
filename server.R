## server.R ##

shinyServer(
  function(input, output, session) {
    # Input ----
    
    input_visualize_y <- reactive({
      y <- (min(input$vis_y[1], input$vis_y[2])):(max(input$vis_y[1], input$vis_y[2]))
      y <- seq(min(y), max(y), by = input$vis_y_sep)
      return(y)
    })
    
    input_model_y <- reactive({
      y2 <- (min(input$mod_y[1], input$mod_y[2])):(max(input$mod_y[1], input$mod_y[2]))
      y2 <- seq(min(y2), max(y2), by = input$mod_y_sep)
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
    
    input_visualize_reporter_iso <- reactive({ input$vis_r })
    input_model_reporter_iso <- reactive({ input$mod_r })
    
    reporter_name <- reactive({
      reporters_to_display %>%
        filter(available_reporters_iso == input_visualize_reporter_iso()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    input_visualize_partner_iso <- reactive({ input$vis_p })
    input_model_partner_iso <- reactive({ input$model_partner })
    
    input_visualize_imputed_data <- reactive({ input$vis_i })
    input_model_imputed_data <- reactive({ input$mod_i })
    
    partner_name <- reactive({
      reporters_to_display %>%
        filter(available_reporters_iso == input_visualize_partner_iso()) %>%
        select(available_reporters_names) %>%
        as.character()
    })
    
    input_model_product_filter <- reactive({ input$mod_pf })
    input_model_type <- reactive({ input$mod_t })
    input_model_dist <- reactive({ input$mod_d })
    input_model_bin <- reactive({ input$mod_b })
    input_model_ctn <- reactive({ input$mod_ct })
    input_model_cluster <- reactive({ input$mod_cl })
    input_model_custom_subset <- reactive({ input$mod_s })
    
    input_visualize_format <- reactive({ input$vis_f })
    input_model_format <- reactive({ input$mod_f })
    
    table_aggregated <- reactive({
      if (input_visualize_partner_iso() == "all") { 
        if (input_visualize_imputed_data() == "yes") "yr-imputed" else "yr"
      } else {
        if (input_visualize_imputed_data() == "yes") "yrp-imputed" else "yrp"
      }
    })
    
    table_detailed <- reactive({
      if (input_visualize_partner_iso() == "all") { 
        if (input_visualize_imputed_data() == "yes") "yrc-imputed" else "yrc"
      } else {
        if (input_visualize_imputed_data() == "yes") "yrpc-imputed" else "yrpc"
      }
    })
    
    # Title ----
    
    reporter_add_the <- reactive({
      if (substr(reporter_name(), 1, 6) == "United" | substr(reporter_name(), 1, 3) == "USA") {
        "the"
      } else {
        ""
      }
    })
    
    reporter_add_proper_the <- reactive({
      if (substr(reporter_name(), 1, 6) == "United" | substr(reporter_name(), 1, 3) == "USA") {
        "The"
      } else {
        ""
      }
    })
    
    partner_add_the <- reactive({
      if (substr(partner_name(), 1, 6) == "United" | substr(partner_name(), 1, 3) == "USA") {
        "the"
      } else {
        ""
      }
    })
    
    title_visualize <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("<h1>{ reporter_add_proper_the() } { reporter_name() } multilateral trade between { min(input_visualize_y()) } and { max(input_visualize_y()) }</h1>"),
        "yrpc" = glue::glue("<h1>{ reporter_add_proper_the() } { reporter_name() } and { partner_add_the() } { partner_name() } between { min(input_visualize_y()) } and { max(input_visualize_y()) }</h1>")
      )
    })
    
    # Data ----
    
    data_aggregated <- reactive({
      ots_create_tidy_data(
        years = input_visualize_y(),
        reporters = input_visualize_reporter_iso(),
        partners = input_visualize_partner_iso(),
        table = table_aggregated(),
        use_localhost = use_localhost
      )
    })
    
    data_detailed <- reactive({
      ots_create_tidy_data(
        years = input_visualize_y(),
        reporters = input_visualize_reporter_iso(),
        partners = input_visualize_partner_iso(),
        table = table_detailed(),
        use_localhost = use_localhost
      )
    })
    
    trade_rankings <- reactive({
      ots_create_tidy_data(
        years = c(min(input_visualize_y()), max(input_visualize_y())),
        reporters = input_visualize_reporter_iso(),
        partners = "all",
        table = "yrp",
        use_localhost = use_localhost
      ) %>%
        mutate(
          exp_rank = dense_rank(desc(trade_value_usd_exp)),
          imp_rank = dense_rank(desc(trade_value_usd_imp))
        )
    })
    
    # Visualize ----
    
    ## Trade ----
    
    trade_table_aggregated <- reactive({
      data_aggregated() %>%
        select(year, trade_value_usd_exp, trade_value_usd_imp)
    })
    
    exports_value_min_year <- reactive({
      trade_table_aggregated() %>%
        filter(year == min(input_visualize_y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    exports_value_max_year <- reactive({
      trade_table_aggregated() %>%
        filter(year == max(input_visualize_y())) %>%
        select(trade_value_usd_exp) %>%
        as.numeric()
    })
    
    imports_value_min_year <- reactive({
      trade_table_aggregated() %>%
        filter(year == min(input_visualize_y())) %>%
        select(trade_value_usd_imp) %>%
        as.numeric()
    })
    
    imports_value_max_year <- reactive({
      trade_table_aggregated() %>%
        filter(year == max(input_visualize_y())) %>%
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
        exports_value_max_year(), exports_value_min_year(), input_visualize_y()
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
        imports_value_max_year(), imports_value_min_year(), input_visualize_y()
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
        years = c(min(input_visualize_y()), max(input_visualize_y())),
        reporters = input_visualize_reporter_iso(),
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
          year == min(input_visualize_y()),
          reporter_iso == input_visualize_reporter_iso(),
          partner_iso == input_visualize_partner_iso()
        ) %>%
        select(bal_rank) %>%
        as.character()
    })
    
    trade_rankings_no_max_year <- reactive({
      trade_rankings() %>%
        ungroup() %>% 
        filter(
          year == max(input_visualize_y()),
          reporter_iso == input_visualize_reporter_iso(),
          partner_iso == input_visualize_partner_iso()
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
          year == min(input_visualize_y()),
          reporter_iso == input_visualize_reporter_iso(),
          partner_iso == input_visualize_partner_iso()
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
          year == max(input_visualize_y()),
          reporter_iso == input_visualize_reporter_iso(),
          partner_iso == input_visualize_partner_iso()
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
          year == min(input_visualize_y()),
          reporter_iso == input_visualize_reporter_iso(),
          partner_iso == input_visualize_partner_iso()
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
          year == max(input_visualize_y()),
          reporter_iso == input_visualize_reporter_iso(),
          partner_iso == input_visualize_partner_iso()
        ) %>%
        select(imp_share) %>%
        as.numeric()
    })
    
    trade_rankings_imp_share_max_year_2 <- reactive({
      show_percentage(trade_rankings_imp_share_max_year())
    })
    
    trade_summary_text_exp <- reactive({
      switch(table_aggregated(),
             "yr" = glue::glue("The exports of { reporter_add_the() } { reporter_name() } to the World { exports_growth_increase_decrease() } from 
                               { exports_value_min_year_2() } in { min(input_visualize_y()) } to { exports_value_max_year_2() } in { max(input_visualize_y()) } 
                               (annualized { exports_growth_increase_decrease_2() } of { exports_growth_2() })."),
             
             "yr-imputed" = glue::glue("The exports of { reporter_add_the() } { reporter_name() } to the World { exports_growth_increase_decrease() } from 
                               { exports_value_min_year_2() } in { min(input_visualize_y()) } to { exports_value_max_year_2() } in { max(input_visualize_y()) } 
                               (annualized { exports_growth_increase_decrease_2() } of { exports_growth_2() })."),
             
             "yrp" = glue::glue("The exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { exports_growth_increase_decrease() } from 
                                { exports_value_min_year_2() } in { min(input_visualize_y()) } 
                                to { exports_value_max_year_2() } in { max(input_visualize_y()) } (annualized { exports_growth_increase_decrease_2() } of 
                                { exports_growth_2() }). { partner_add_the() } { partner_name() } was the No. { trade_rankings_no_min_year() } trading partner of 
                                { reporter_add_the() } { reporter_name() } in { min(input_visualize_y()) } (represented { trade_rankings_exp_share_min_year_2() } of its exports), and 
                                then { trade_rankings_remained() } No. { trade_rankings_no_max_year() } in { max(input_visualize_y()) } (represented { trade_rankings_exp_share_max_year_2() } 
                                of its exports)."),
             
             "yrp-imputed" = glue::glue("The exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { exports_growth_increase_decrease() } from 
                                { exports_value_min_year_2() } in { min(input_visualize_y()) } 
                                to { exports_value_max_year_2() } in { max(input_visualize_y()) } (annualized { exports_growth_increase_decrease_2() } of 
                                { exports_growth_2() }). { partner_add_the() } { partner_name() } was the No. { trade_rankings_no_min_year() } trading partner of 
                                { reporter_add_the() } { reporter_name() } in { min(input_visualize_y()) } (represented { trade_rankings_exp_share_min_year_2() } of its exports), and 
                                then { trade_rankings_remained() } No. { trade_rankings_no_max_year() } in { max(input_visualize_y()) } (represented { trade_rankings_exp_share_max_year_2() } 
                                of its exports).")
      )
    })
    
    trade_summary_text_imp <- reactive({
      switch(table_aggregated(),
             "yr" = glue::glue("The imports of { reporter_add_the() } { reporter_name() } to the World { imports_growth_increase_decrease() } from 
                               { imports_value_min_year_2() } in { min(input_visualize_y()) } to { imports_value_max_year_2() } in { max(input_visualize_y()) } 
                               (annualized { imports_growth_increase_decrease_2() } of { imports_growth_2() })."),
             
             "yr-imputed" = glue::glue("The imports of { reporter_add_the() } { reporter_name() } to the World { imports_growth_increase_decrease() } from 
                               { imports_value_min_year_2() } in { min(input_visualize_y()) } to { imports_value_max_year_2() } in { max(input_visualize_y()) } 
                               (annualized { imports_growth_increase_decrease_2() } of { imports_growth_2() })."),
             
             "yrp" = glue::glue("The imports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { imports_growth_increase_decrease() } from 
                                { imports_value_min_year_2() } in { min(input_visualize_y()) } 
                                to { imports_value_max_year_2() } in { max(input_visualize_y()) } (annualized { imports_growth_increase_decrease_2() } of 
                                { imports_growth_2() }). { partner_add_the() } { partner_name() } was the No. { trade_rankings_no_min_year() } trading partner of 
                                { reporter_add_the() } { reporter_name() } in { min(input_visualize_y()) } (represented { trade_rankings_imp_share_min_year_2() } of its imports), and 
                                then { trade_rankings_remained() } No. { trade_rankings_no_max_year() } in { max(input_visualize_y()) } (represented { trade_rankings_imp_share_max_year_2() } 
                                of its imports)."),
             
             "yrp-imputed" = glue::glue("The imports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } { imports_growth_increase_decrease() } from 
                                { imports_value_min_year_2() } in { min(input_visualize_y()) } 
                                to { imports_value_max_year_2() } in { max(input_visualize_y()) } (annualized { imports_growth_increase_decrease_2() } of 
                                { imports_growth_2() }). { partner_add_the() } { partner_name() } was the No. { trade_rankings_no_min_year() } trading partner of 
                                { reporter_add_the() } { reporter_name() } in { min(input_visualize_y()) } (represented { trade_rankings_imp_share_min_year_2() } of its imports), and 
                                then { trade_rankings_remained() } No. { trade_rankings_no_max_year() } in { max(input_visualize_y()) } (represented { trade_rankings_imp_share_max_year_2() } 
                                of its imports).")
      )
    })

    trade_exchange_lines_title <- reactive({
      switch(table_aggregated(),
             "yr" = glue::glue("{ reporter_add_proper_the() } { reporter_name() } multilateral trade between { min(input_visualize_y()) } and { max(input_visualize_y()) }"),
             "yrp" = glue::glue("{ reporter_add_proper_the() } { reporter_name() } and { partner_add_the() } { partner_name() } exchange between { min(input_visualize_y()) } and { max(input_visualize_y()) }")
      )
    })
    
    trade_exchange_lines_aggregated <- reactive({
      d <- trade_table_aggregated()

      highchart() %>%
        hc_xAxis(title = list(text = "Year")) %>% 
        hc_xAxis(categories = d$year) %>%
        hc_add_series(name = "Exports", data = d$trade_value_usd_exp) %>% 
        hc_add_series(name = "Imports", data = d$trade_value_usd_imp) %>% 
        hc_title(text = trade_exchange_lines_title())
    })
    
    ## Exports ----
    
    exports_table_detailed_min_year <- reactive({
      d <- data_detailed() %>%
        filter(year == min(input_visualize_y())) %>%
        select(group_fullname_english, trade_value_usd_exp) %>%
        filter(trade_value_usd_exp > 0) %>% 
        group_by(group_fullname_english) %>% 
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>% 
        arrange(-trade_value_usd_exp)
      
      d <- d %>% 
        mutate(
          group_fullname_english = case_when(
            row_number() > 9 ~ "Others",
            TRUE ~ group_fullname_english
          )) %>% 
        group_by(group_fullname_english) %>% 
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>% 
        left_join(groups_colors)
      
      return(d)
    })
    
    exports_subtitle <- reactive({
      sprintf("<hr/>Detailed Exports %s-%s", min(input_visualize_y()), max(input_visualize_y()))
    })
    
    exports_title_min_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Exports of { reporter_add_the() } { reporter_name() } to the rest of the World in { min(input_visualize_y()) }"),
        "yrpc" = glue::glue("Exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } in { min(input_visualize_y()) }")
      )
    })
    
    exports_treemap_detailed_min_year <- reactive({
      d <- exports_table_detailed_min_year() %>%
        group_by(group_fullname_english, group_color) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>%
        ungroup() %>% 
        group_by(group_fullname_english, group_color) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>%
        ungroup() %>% 
        mutate(
          share = paste0(round(100 * trade_value_usd_exp / sum(trade_value_usd_exp), 2), "%"),
          group_fullname_english = paste0(group_fullname_english, "<br>", share)
        ) %>%
        rename(
          value = trade_value_usd_exp,
          name = group_fullname_english,
          color = group_color
        )
      
      highchart() %>%
        hc_chart(type = "treemap") %>%
        hc_xAxis(categories = d$name) %>%
        hc_add_series(d,
                      name = "Export Value USD", showInLegend = FALSE,
                      dataLabels = list(verticalAlign = "top", align = "left", style = list(fontSize = "12px", textOutline = FALSE))
        ) %>%
        hc_title(text = exports_title_min_year())
    })
    
    exports_title_max_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Exports of { reporter_add_the() } { reporter_name() } to the rest of the World { max(input_visualize_y()) }"),
        "yrpc" = glue::glue("Exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } in { max(input_visualize_y()) }")
      )
    })
    
    exports_table_detailed_max_year <- reactive({
      d <- data_detailed() %>%
        filter(year == max(input_visualize_y())) %>%
        select(group_fullname_english, trade_value_usd_exp) %>%
        filter(trade_value_usd_exp > 0) %>% 
        group_by(group_fullname_english) %>% 
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>% 
        arrange(-trade_value_usd_exp)
      
      d <- d %>% 
        mutate(
          group_fullname_english = case_when(
            row_number() > 9 ~ "Others",
            TRUE ~ group_fullname_english
          )) %>% 
        group_by(group_fullname_english) %>% 
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>% 
        left_join(groups_colors)
      
      return(d)
    })
    
    exports_treemap_detailed_max_year <- reactive({
      d <- exports_table_detailed_max_year() %>%
        group_by(group_fullname_english, group_color) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>%
        ungroup() %>% 
        group_by(group_fullname_english, group_color) %>%
        summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>%
        ungroup() %>% 
        mutate(
          share = paste0(round(100 * trade_value_usd_exp / sum(trade_value_usd_exp), 2), "%"),
          group_fullname_english = paste0(group_fullname_english, "<br>", share)
        ) %>%
        rename(
          value = trade_value_usd_exp,
          name = group_fullname_english,
          color = group_color
        )
      
      highchart() %>%
        hc_chart(type = "treemap") %>%
        hc_xAxis(categories = d$name) %>%
        hc_add_series(d,
                      name = "Export Value USD", showInLegend = FALSE,
                      dataLabels = list(verticalAlign = "top", align = "left", style = list(fontSize = "12px", textOutline = FALSE))
        ) %>%
        hc_title(text = exports_title_max_year())
    })
    
    ## Imports -----------------------------------------------------------------
    
    imports_subtitle <- reactive({
      sprintf("<hr/>Detailed Imports %s-%s", min(input_visualize_y()), max(input_visualize_y()))
    })

    imports_title_min_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Imports of { reporter_add_the() } { reporter_name() } from the rest of the World in { min(input_visualize_y()) }"),
        "yrpc" = glue::glue("Imports of { reporter_add_the() } { reporter_name() } from { partner_add_the() } { partner_name() } in { min(input_visualize_y()) }")
      )
    })
    
    imports_table_detailed_min_year <- reactive({
      d <- data_detailed() %>%
        filter(year == min(input_visualize_y())) %>%
        select(group_fullname_english, trade_value_usd_imp) %>%
        filter(trade_value_usd_imp > 0) %>% 
        group_by(group_fullname_english) %>% 
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>% 
        arrange(-trade_value_usd_imp)
      
      d <- d %>% 
        mutate(
          group_fullname_english = case_when(
            row_number() > 9 ~ "Others",
            TRUE ~ group_fullname_english
          )) %>% 
        group_by(group_fullname_english) %>% 
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>% 
        left_join(groups_colors)
      
      return(d)
    })
    
    imports_treemap_detailed_min_year <- reactive({
      d <- imports_table_detailed_min_year() %>%
        group_by(group_fullname_english, group_color) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>%
        ungroup() %>% 
        group_by(group_fullname_english, group_color) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>%
        ungroup() %>% 
        mutate(
          share = paste0(round(100 * trade_value_usd_imp / sum(trade_value_usd_imp), 2), "%"),
          group_fullname_english = paste0(group_fullname_english, "<br>", share)
        ) %>%
        rename(
          value = trade_value_usd_imp,
          name = group_fullname_english,
          color = group_color
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
        hc_title(text = imports_title_min_year())
    })
    
    imports_title_min_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Imports of { reporter_add_the() } { reporter_name() } from the rest of the World in { min(input_visualize_y()) }"),
        "yrpc" = glue::glue("Imports of { reporter_add_the() } { reporter_name() } from { partner_add_the() } { partner_name() } in { min(input_visualize_y()) }")
      )
    })
    
    imports_title_max_year <- reactive({
      switch(
        table_detailed(),
        "yrc" = glue::glue("Imports of { reporter_add_the() } { reporter_name() } from the rest of the World in { max(input_visualize_y()) }"),
        "yrpc" = glue::glue("Imports of { reporter_add_the() } { reporter_name() } from { partner_add_the() } { partner_name() } in { max(input_visualize_y()) }")
      )
    })
    
    imports_table_detailed_max_year <- reactive({
      d <- data_detailed() %>%
        filter(year == max(input_visualize_y())) %>%
        select(group_fullname_english, trade_value_usd_imp) %>%
        filter(trade_value_usd_imp > 0) %>% 
        group_by(group_fullname_english) %>% 
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>% 
        arrange(-trade_value_usd_imp)
      
      d <- d %>% 
        mutate(
          group_fullname_english = case_when(
            row_number() > 9 ~ "Others",
            TRUE ~ group_fullname_english
          )) %>% 
        group_by(group_fullname_english) %>% 
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>% 
        left_join(groups_colors)
      
      return(d)
    })
    
    imports_treemap_detailed_max_year <- reactive({
      d <- imports_table_detailed_max_year() %>%
        group_by(group_fullname_english, group_color) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>%
        ungroup() %>% 
        group_by(group_fullname_english, group_color) %>%
        summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>%
        ungroup() %>% 
        mutate(
          share = paste0(round(100 * trade_value_usd_imp / sum(trade_value_usd_imp), 2), "%"),
          group_fullname_english = paste0(group_fullname_english, "<br>", share)
        ) %>%
        rename(
          value = trade_value_usd_imp,
          name = group_fullname_english,
          color = group_color
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
        hc_title(text = imports_title_max_year())
    })
    
    # Model ----
  
    model_custom_data <- reactive({
      uploaded_file <- input$mod_f
      
      if(!is.null(uploaded_file)) {
        input_data <- rio::import(file = uploaded_file$datapath, format = tools::file_ext(uploaded_file$name)) %>%
          janitor::clean_names()
        
        return(input_data)
      } else {
        data.frame()
      }
    })
    
    data_detailed_model <- eventReactive(input$go, {
      # 1. read from API
      
      table_model <- reactive({
        if (input_model_imputed_data() == "yes") "yrpc-imputed" else "yrpc"
      })
      
      d <- ots_create_tidy_data(
          years = input_model_y(),
          reporters = input_model_reporter_iso(),
          partners = input_model_partner_iso(),
          table = table_model(),
          use_localhost = use_localhost
        ) %>% 
        filter(
          reporter_iso != "0-unspecified",
          partner_iso != "0-unspecified"
        )
      
      # 2. apply filters
      
      if (!any(input_model_product_filter() %in% c("All Products"))) {
        if (any(input_model_product_filter() %in% "Vaccine Inputs")) {
          d <- d %>% 
            mutate(
              group_fullname_english = case_when(
                commodity_code %in% c("170199", "220710", "220720", "220890", "250100", 
                                      "280610", "281121", "281511", "281512", "282731", 
                                      "283330", "283522", "283524", "285210", "285390", 
                                      "290544", "290613", "291211", "291521", "291529", 
                                      "291814", "291815", "292219", "292249", "292250", 
                                      "292320", "293329", "294190", "300220", "300510", 
                                      "310420", "340213", "350300", "350510", "350790", 
                                      "382100", "382200", "391740", "392310", "392321", 
                                      "392329", "392330", "392690", "401511", "401519", 
                                      "401699", "482110", "482190", "701090", "701710", 
                                      "701720", "701790", "830990", "841830", "841840", 
                                      "841920", "841989", "842129", "842230", "847982", 
                                      "847989", "854370", "901831", "901832", "902720", 
                                      "902790", "903289") ~ "Vaccine Inputs",
                TRUE ~ group_fullname_english
              )
            )
        }
        
        d <- d %>% 
          filter(group_fullname_english %in% input_model_product_filter())
      }
      
      if (any(input_model_ctn() %in% "mfn")) {
        # 3.1 read from API
        
        d <- d %>%
          left_join(
            ots_create_tidy_data(
              years = input_model_y(),
              # here we need the applied tariffs when the product gets to destination
              reporters = input_model_partner_iso(),
              table = "tariffs",
              use_localhost = use_localhost
            ) %>% 
              select(year, partner_iso = reporter_iso, commodity_code, mfn = simple_average)
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
          )
        
        d <- d2 %>% left_join(d3); rm(d2,d3)
      } else {
        # 3.1. summarise
        
        d <- d %>% 
          select(year, reporter_iso, partner_iso, trade_value_usd_exp, trade_value_usd_imp) %>% 
          group_by(year, reporter_iso, partner_iso) %>% 
          summarise(
            trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T),
            trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)
          )
      }
      
      # 4. add geo dist data
      
      d <- d %>% 
        inner_join(
          dist_cepii %>% 
            select(reporter_iso = iso_o, partner_iso = iso_d,
                   c(input_model_dist(), input_model_bin()[input_model_bin() != "rta"])) %>% 
            mutate_if(is.character, tolower)
        )
      
      # 5. add RTA data
      
      if (any(input_model_bin() %in% "rta")) {
        dc <- d %>%
          select(reporter_iso, partner_iso) %>%
          rowwise() %>%
          mutate(
            country1 = min(reporter_iso, partner_iso),
            country2 = max(reporter_iso, partner_iso)
          ) %>%
          ungroup() %>%
          select(country1, country2)
        
        # left join because not all countries sign RTAs!
        d <- d %>%
          bind_cols(dc) %>% 
          left_join(
            ots_create_tidy_data(
              years = input_model_y(),
              table = "rtas",
              use_localhost = use_localhost
            )
          )
        
        rm(dc)
        
        d <- d %>%
          mutate(rta = ifelse(is.na(rta), 0, rta))
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
    
    data_detailed_model_preview <- eventReactive(input$go, { head(data_detailed_model()) })
    
    model_formula <- eventReactive(input$go, {
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
      
      return(f)
    })
    
    model_summary <- eventReactive(input$go, {
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
      return(summary(m))
    })
    
    # Cite ----
    
    cite <- reactive({
      sprintf(
        "Open Trade Statistics. \"OTS BETA DASHBOARD\". <i>Open Trade Statistics</i>. Accessed %s %s, %s. %s/",
        months(Sys.Date()),
        lubridate::day(Sys.Date()),
        lubridate::year(Sys.Date()),
        site_url
      )
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
    
    output$title_visualize <- renderText({
      title_visualize()
    })
    
    output$title_legend <- renderText({
      "The information displayed here is based on <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. Please read our <a href='https://docs.tradestatistics.io/index.html#code-of-conduct'>Code of Conduct</a> for a full description
      of restrictions and applicable licenses."
    })
    
    output$title_model <- renderText({
      "<h1>Gravity Models</h1>"
    })
    
    # Visualize output ----
    
    ## Trade output ----
    
    output$trade_subtitle <- reactive({
      sprintf("<hr/>Total Exports and Imports %s-%s", min(input_visualize_y()), max(input_visualize_y()))
    })
    
    output$trade_summary_exp <- renderText(trade_summary_text_exp())
    output$trade_summary_imp <- renderText(trade_summary_text_imp())
    
    output$trade_title <- renderText(trade_title())
    
    output$trade_exchange_lines_aggregated <- renderHighchart({
      trade_exchange_lines_aggregated()
    })
    
    ## Exports output ----
    
    output$exports_subtitle <- renderText(exports_subtitle())
    output$exports_note <- renderText(
      "The official HS groups were grouped to show the top ten to make the charts clearer. Any exported group below the 9th place was added to 'Others'. You can download the product level data at the bottom of the page. You can download the product level data at the bottom of the page."
    )
    
    output$exports_title_min_year <- renderText(exports_title_min_year())
    
    output$exports_treemap_detailed_min_year <- renderHighchart({
      exports_treemap_detailed_min_year()
    })
    
    output$exports_title_max_year <- renderText(exports_title_max_year())
    
    output$exports_treemap_detailed_max_year <- renderHighchart({
      exports_treemap_detailed_max_year()
    })
    
    ## Imports output ----
    
    output$imports_subtitle <- renderText(imports_subtitle())
    output$imports_note <- renderText(
      "The official HS groups were grouped to show the top ten to make the charts clearer. Any imported group below the 9th place was added to 'Others'. You can download the product level data at the bottom of the page."
    )
    
    output$imports_title_min_year <- renderText(imports_title_min_year())
    
    output$imports_treemap_detailed_min_year <- renderHighchart({
      imports_treemap_detailed_min_year()
    })
    
    output$imports_title_max_year <- renderText(imports_title_max_year())
    
    output$imports_treemap_detailed_max_year <- renderHighchart({
      imports_treemap_detailed_max_year()
    })
    
    # Model output ----
    
    output$data_detailed_model_preview <- renderTable(data_detailed_model_preview())
    
    output$model_formula_latex <- renderUI({
      if (input_model_type() == "ppml") {
        lhs <- "\\text{trade value}_{ij}^{kt}"
      } else {
        lhs <- "\\text{log trade value}_{ij}^{kt}"
      }
      
      if (input_model_type() == "ols") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(input_model_dist(), input_model_ctn(), input_model_bin())
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", input_model_dist()), 
                                                    paste("log", input_model_ctn()), input_model_bin())), "}"), "_{ij}^{kt}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\beta_0 +", rhs, "+ \\varepsilon_{ij}^{kt}")
      }
      
      if (input_model_type() == "olsrem") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(input_model_dist(), input_model_bin(), "log_remoteness_exp", "log_remoteness_imp")
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", input_model_dist()), 
                                                    input_model_bin(), "log_remoteness_exp", "log_remoteness_imp")), "}"), "_{ij}^{kt}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\beta_0 +", rhs, "+ \\varepsilon_{ij}^{kt}")
      }
      
      if (input_model_type() == "olsfe") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(input_model_dist(), input_model_bin(), "reporter_year", "partner_year")
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", input_model_dist()), 
                                                    input_model_bin(), "reporter_year", "partner_year")), "}"), "_{ij}^{kt}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\beta_0 +", rhs, "+ \\varepsilon_{ij}^{kt}")
      }
      
      if (input_model_type() == "ppml") {
        rhs <- paste(
          paste0("\\beta_", seq_len(length(
            c(input_model_dist(), input_model_bin())
          ))),
          paste0(paste0("\\text{", gsub("_", " ", c(paste("log", input_model_dist()), 
                                                    input_model_bin())), "}"), "_{ij}^{kt}"),
          collapse = " + "
        )
        
        rhs <- paste0("\\exp[\\beta_0 +", rhs, "]\\times \\varepsilon_{ij}^{kt}")
      }
      
      withMathJax(
        paste0("\\[", lhs, "=", rhs, "\\]")
      )
    })
    
    output$model_summary <- renderPrint(model_summary())
    
    # Download output ----
    
    ## Visualize ----
    
    output$download_visualize_subtitle <- renderText({"<hr/>Download visualized data"})
    
    output$download_visualize_text <- renderText({"Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."})
    
    output$download_visualize_aggregated <- downloadHandler(
      filename = function() {
        glue::glue("{ table_aggregated() }_{ input_visualize_reporter_iso() }_{ input_visualize_partner_iso() }_{ min(input_visualize_y()) }_{ max(input_visualize_y()) }.{ input_visualize_format() }")
      },
      content = function(filename) {
        rio::export(data_aggregated(), filename)
      },
      contentType = "application/zip"
    )
    
    output$download_visualize_detailed <- downloadHandler(
      filename = function() {
        glue::glue("{ table_detailed() }_{ input_visualize_reporter_iso() }_{ input_visualize_partner_iso() }_{ min(input_visualize_y()) }_{ max(input_visualize_y()) }.{ input_visualize_format() }")
      },
      content = function(filename) {
        rio::export(data_detailed(), filename)
      },
      contentType = "application/zip"
    )
    
    ## Model ----
    
    output$download_model_subtitle <- renderText({"<hr/>Download model data"})
    
    output$download_model_text <- renderText({"Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS), DTA (Stata) and JSON (cross-language)."})
    
    output$download_model_detailed <- downloadHandler(
      filename = function() {
        glue::glue("{ input_model_type() }_{ input_visualize_reporter_iso() }_{ input_visualize_partner_iso() }_{ min(input_visualize_y()) }_{ max(input_visualize_y()) }.{ input_model_format() }")
      },
      content = function(filename) {
        rio::export(data_detailed_model(), filename)
      },
      contentType = "application/zip"
    )
    
    # Cite output ----
    
    output$cite_subtitle <- renderText({"<hr/>Cite"})
    
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
      sprintf(
        "<center><i>Open Trade Statistics %s.</i></center>",
        lubridate::year(Sys.Date())
      )
    })
    
    # Bookmarking -------------------------------------------------------------
    
    observe({
      # Trigger this observer every time an input changes
      # strip shiny related URL parameters
      reactiveValuesToList(input)
      setBookmarkExclude(c(
        "mod_f", "vis_f", "go", "sidebarCollapsed", "sidebarItemExpanded"
      ))
      session$doBookmark()
    })
    
    onBookmarked(function(url) {
      updateQueryString(url)
    })
  }
)
