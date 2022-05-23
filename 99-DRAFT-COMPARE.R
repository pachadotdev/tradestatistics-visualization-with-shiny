# Country profile ----

wt_cc <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

## Data ----

df_agg_r1_cc <- reactive({
  wt_cc$start()
  
  d <- tbl(con, table_agg_cc())
  
  if (inp_cc_p() == "all") {
    d <- d %>% 
      filter(
        year %in% !!inp_cc_y() &
          reporter_iso == !!inp_cc_r1()
      )
  } else {
    d <- d %>% 
      filter(
        year %in% !!inp_cc_y() &
          reporter_iso == !!inp_cc_r1() &
          partner_iso == !!inp_cc_p()
      )
  }
  
  d <- d %>% collect()
  
  if (inp_cc_convert_dollars() != "No conversion") {
    d <- gdp_deflator_adjustment(d, as.integer(inp_cc_convert_dollars()))
  }
  
  wt_cc$inc(2)
  
  return(d)
}) %>% 
  bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars()) %>% 
  bindEvent(input$cc_go)

df_dtl_r1_cc <- reactive({
  d <- tbl(con, tbl_dtl_cc())
  
  if (inp_cc_p() == "all") {
    d <- d %>% 
      filter(
        year %in% !!inp_cc_y() &
          reporter_iso == !!inp_cc_r1()
      )
  } else {
    d <- d %>% 
      filter(
        year %in% !!inp_cc_y() &
          reporter_iso == !!inp_cc_r1() &
          partner_iso == !!inp_cc_p()
      )
  }
  
  d <- d %>% collect()
  
  if (inp_cc_convert_dollars() != "No conversion") {
    d <- gdp_deflator_adjustment(d, as.integer(inp_cc_convert_dollars()))
  }
  
  wt_cc$inc(2)
  
  return(d)
}) %>% 
  bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars(),
            tbl_dtl_cc()) %>% 
  bindEvent(input$cc_go)

## Trade ----

### Tables ----

tr_tbl_agg_r1_cc <- eventReactive(input$cc_go, {
  df_agg_r1_cc() %>%
    select(year, trade_value_usd_exp, trade_value_usd_imp)
})

exp_val_min_yr_r1_cc <- eventReactive(input$cc_go, {
  tr_tbl_agg_r1_cc() %>%
    filter(year == min(inp_cc_y())) %>%
    select(trade_value_usd_exp) %>%
    as.numeric()
})

exp_val_max_yr_r1_cc <- eventReactive(input$cc_go, {
  tr_tbl_agg_r1_cc() %>%
    filter(year == max(inp_cc_y())) %>%
    select(trade_value_usd_exp) %>%
    as.numeric()
})

imp_val_min_yr_r1_cc <- eventReactive(input$cc_go, {
  tr_tbl_agg_r1_cc() %>%
    filter(year == min(inp_cc_y())) %>%
    select(trade_value_usd_imp) %>%
    as.numeric()
})

imp_val_max_yr_r1_cc <- eventReactive(input$cc_go, {
  tr_tbl_agg_r1_cc() %>%
    filter(year == max(inp_cc_y())) %>%
    select(trade_value_usd_imp) %>%
    as.numeric()
})

exp_val_min_yr_2_r1_cc <- eventReactive(input$cc_go, {
  show_dollars(exp_val_min_yr_r1_cc())
})

exp_val_max_yr_2_r1_cc <- eventReactive(input$cc_go, {
  show_dollars(exp_val_max_yr_r1_cc())
})

imp_val_min_yr_2_r1_cc <- eventReactive(input$cc_go, {
  show_dollars(imp_val_min_yr_r1_cc())
})

imp_val_max_yr_2_r1_cc <- eventReactive(input$cc_go, {
  show_dollars(imp_val_max_yr_r1_cc())
})

trd_rankings_r1_cc <- eventReactive(input$cc_go, {
  d <- tbl(con, "yrp") %>% 
    filter(
      year %in% !!inp_cc_y() &
        reporter_iso == inp_cc_r1()
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
      year == inp_cc_y(),
      reporter_iso == inp_cc_r1(),
      partner_iso == inp_cc_p()
    ) %>%
    select(bal_rank) %>%
    as.character()
})

trd_rankings_exp_share_yr_r1_cc <- eventReactive(input$cc_go, {
  trd_rankings_r1_cc() %>%
    ungroup() %>% 
    filter(
      year == inp_cc_y(),
      reporter_iso == inp_cc_r1(),
      partner_iso == inp_cc_p()
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
      reporter_iso == inp_cc_r1(),
      partner_iso == inp_cc_p()
    ) %>%
    select(imp_share) %>%
    as.numeric()
})

trd_rankings_imp_share_yr_2_r1_cc <- eventReactive(input$cc_go, {
  show_percentage(trd_rankings_imp_share_yr_r1_cc())
})

### Text/Visual elements ----

trd_smr_txt_exp_r1_cc <- eventReactive(input$cc_go, {
  switch(table_agg_cc(),
         "yr" = glue("The exports of { r1name_cc() } to the World were { exp_val_yr_2_r1_cc() } in { inp_cc_y() }."),
         
         "yrp" = glue("The exports of { r1name_cc() } to { pname_cc() } were { exp_val_yr_2_r1_cc() } in { inp_cc_y() }. 
                          { pname_cc() } was the No. { trd_rankings_no_yr_r1_cc() } trading partner of 
                          { r1name_cc() } in { inp_cc_y() } (represented { trd_rankings_exp_share_yr_2_r1_cc() } of its exports).")
  )
})

trd_smr_txt_imp_r1_cc <- eventReactive(input$cc_go, {
  switch(table_agg_cc(),
         "yr" = glue("The imports of { r1name_cc() } to the World were { imp_val_yr_2_r1_cc() } in { inp_cc_y() }."),
         
         "yrp" = glue("The imports of { r2name_cc() } to { pname_cc() } were { imp_val_min_yr_2_r1_cc() } in { inp_cc_y() }.
                          { pname_cc() } was the No. { trd_rankings_no_yr_r1_cc() } trading partner of 
                          { r1name_cc() } in { inp_cc_y() } (represented { trd_rankings_imp_share_yr_2_r1_cc() } of its imports).")
  )
})

trd_exc_lines_title_r1_cc <- eventReactive(input$cc_go, {
  switch(table_agg_cc(),
         "yr" = glue("{ r1name_cc() } multilateral trade in { inp_cc_y() }"),
         "yrp" = glue("{ r1name_cc() } and { pname_cc() } exchange in { inp_cc_y() }")
  )
})

trd_exc_lines_agg_r1_cc <- reactive({
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
         "line", 
         hcaes(x = year, y = trade, group = flow),
         tooltip = list(
           pointFormatter = custom_tooltip_short()
         )) %>% 
    hc_xAxis(title = list(text = "Year")) %>%
    hc_yAxis(title = list(text = "USD billion"),
             labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>% 
    hc_title(text = trd_exc_lines_title_r1_cc())
}) %>%
  bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars()) %>% 
  bindEvent(input$cc_go)

## Exports ----

### Tables ----

exp_imp_tbl_od_yr_r1_cc <- reactive({
  d <- tbl(con, "yrp") %>% 
    filter(
      year %in% !!inp_cc_y() &
        reporter_iso == !!inp_cc_r()
    ) %>% 
    collect()
  
  if (inp_cc_convert_dollars() != "No conversion") {
    d <- gdp_deflator_adjustment(d, as.integer(inp_cc_convert_dollars()))
  }
  
  wt_cc$inc(1)
  
  return(d)
}) %>% 
  bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars()) %>% 
  bindEvent(input$cc_go)

### Visual elements ----

exp_tt_yr_r1_cc <- eventReactive(input$cc_go, {
  switch(
    tbl_dtl_r1_cc(),
    "yrc" = glue("Exports of { r1name_cc() } to the rest of the World in { inp_cc_y() }, by product"),
    "yrpc" = glue("Exports of { r1name_cc() } to { pname_cc() } in { inp_cc_y() }, by product")
  )
})

exp_tm_dtl_yr_r1_cc <- reactive({
  d <- df_dtl_r1_cc() %>%
    pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
  
  d2 <- pd_colors(d)
  
  pd_to_highcharts(d, d2)
}) %>% 
  bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars()) %>% 
  bindEvent(input$cc_go)

## Imports ----

### Visual elements ----

imp_tt_yr_r1_cc <- eventReactive(input$cc_go, {
  switch(
    tbl_dtl_r1_cc(),
    "yrc" = glue("Imports of { r1name_cc() } from the rest of the World in { inp_cc_y() }, by product"),
    "yrpc" = glue("Imports of { r1name_cc() } from { pname_cc() } in { inp_cc_y() }, by product")
  )
})

imp_tm_ori_yr_r1_cc <- reactive({
  d <- exp_imp_tbl_od_yr_cc() %>%
    od_order_and_add_continent(col = "trade_value_usd_imp")
  
  d2 <- od_colors(d)
  
  od_to_highcharts(d, d2)
}) %>% 
  bindCache(inp_cc_y(), inp_cc_r1(), inp_cc_p(), inp_cc_convert_dollars()) %>% 
  bindEvent(input$cc_go)
