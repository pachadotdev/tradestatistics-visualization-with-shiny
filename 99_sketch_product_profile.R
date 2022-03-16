# Product profile ----

## Data ----

data_detailed_pp <- eventReactive(input$pp_go, {
  d <- ots_create_tidy_data(
    years = input_product_profile_y(),
    reporters = "all",
    partners = "all",
    sections = "all",
    table = input_product_profile_s(),
    use_localhost = use_localhost
  )
  
  if (input_product_profile_convert_dollars() != "No conversion") {
    d <- ots_inflation_adjustment(d, as.integer(input_product_profile_convert_dollars()))
  }
  
  return(d)
})

## Exports ----

### Visual elements ----

exports_subtitle_pp <- eventReactive(input$pp_go, {
  glue("Detailed multilateral Exports and Imports { min(input_product_profile_y()) }-{ max(input_product_profile_y()) }")
})

exports_note <- eventReactive(input$pp_go, {
  switch(
    table_detailed(),
    "yrc" = glue("Explore the exports and imports of { reporter_add_the() } { reporter_name() } to/from the World at the begining 
          and end of the selected period. The data was grouped by sections for visual clarity, you can click each section to see the finer detail."),
    "yrpc-parquet" = glue("Explore the exports and imports of { reporter_add_the() } { reporter_name() } to/from { partner_add_the() } 
          { partner_name() } at the begining and end of the selected period. The data was grouped by sections for visual clarity, you can 
          click each section to see the finer detail."),
  )
})

exports_title_year <- eventReactive(input$pp_go, {
  switch(
    table_detailed(),
    "yrc" = glue("Exports of { reporter_add_the() } { reporter_name() } to the rest of the World in { min(input_product_profile_y()) } and { max(input_product_profile_y()) }, by product"),
    "yrpc-parquet" = glue("Exports of { reporter_add_the() } { reporter_name() } to { partner_add_the() } { partner_name() } in { min(input_product_profile_y()) } and { max(input_product_profile_y()) }, by product")
  )
})

exports_title_min_year <- eventReactive(input$pp_go, {
  glue("{ min(input_product_profile_y()) }")
})

exports_treemap_destinations_min_year <- eventReactive(input$pp_go, {
  d <- exports_imports_table_origin_destination_year() %>%
    filter(year == min(year)) %>% 
    od_order_and_add_continent()
  
  d2 <- od_colors(d)
  
  od_to_highcharts(d, d2)
})

exports_treemap_detailed_min_year <- eventReactive(input$pp_go, {
  d <- data_detailed() %>%
    filter(year == min(input_product_profile_y())) %>% 
    pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
  
  d2 <- pd_colors(d)
  
  pd_to_highcharts(d, d2)
})

exports_title_max_year <- eventReactive(input$pp_go, {
  glue("{ max(input_product_profile_y()) }")
})

exports_treemap_destinations_max_year <- eventReactive(input$pp_go, {
  d <- exports_imports_table_origin_destination_year() %>%
    filter(year == max(year)) %>% 
    od_order_and_add_continent()
  
  d2 <- od_colors(d)
  
  od_to_highcharts(d, d2)
})

exports_treemap_detailed_max_year <- eventReactive(input$pp_go, {
  d <- data_detailed() %>%
    filter(year == max(input_product_profile_y())) %>% 
    pd_fix_section_and_aggregate(col = "trade_value_usd_exp")
  
  d2 <- pd_colors(d)
  
  pd_to_highcharts(d, d2)
})

## Imports ----

### Visual elements ----

imports_title_year <- eventReactive(input$pp_go, {
  switch(
    table_detailed(),
    "yrc" = glue("Imports of { reporter_add_the() } { reporter_name() } from the rest of the World in { min(input_product_profile_y()) } and { max(input_product_profile_y()) }, by product"),
    "yrpc-parquet" = glue("Imports of { reporter_add_the() } { reporter_name() } from { partner_add_the() } { partner_name() } in { min(input_product_profile_y()) } and { max(input_product_profile_y()) }, by product")
  )
})

imports_title_min_year <- eventReactive(input$pp_go, {
  glue("{ min(input_product_profile_y()) }")
})

imports_treemap_origins_min_year <- eventReactive(input$pp_go, {
  d <- exports_imports_table_origin_destination_year() %>%
    filter(year == min(year)) %>% 
    od_order_and_add_continent(col = "trade_value_usd_imp")
  
  d2 <- od_colors(d)
  
  od_to_highcharts(d, d2)
})

imports_treemap_detailed_min_year <- eventReactive(input$pp_go, {
  d <- data_detailed() %>%
    filter(year == min(input_product_profile_y())) %>% 
    pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
  
  d2 <- pd_colors(d)
  
  pd_to_highcharts(d, d2)
})

imports_title_max_year <- eventReactive(input$pp_go, {
  glue("{ min(input_product_profile_y()) }")
})

imports_treemap_origins_max_year <- eventReactive(input$pp_go, {
  d <- exports_imports_table_origin_destination_year() %>%
    filter(year == max(year)) %>% 
    od_order_and_add_continent(col = "trade_value_usd_imp")
  
  d2 <- od_colors(d)
  
  od_to_highcharts(d, d2)
})

imports_treemap_detailed_max_year <- eventReactive(input$pp_go, {
  d <- data_detailed() %>%
    filter(year == max(input_product_profile_y())) %>% 
    pd_fix_section_and_aggregate(col = "trade_value_usd_imp")
  
  d2 <- pd_colors(d)
  
  pd_to_highcharts(d, d2)
})
