# ORIGIN/DESTINATION TREEMAPS -----

custom_tooltip <- function() {
  JS("function() { return '<b>' + this.name + '</b>' + '<br>' + 
                        'Share: ' + Math.round(this.value / this.series.tree.val * 10000)/100 + '%' + '<br>' +
                        'Value: ' + Highcharts.numberFormat(this.value, 0) + ' USD'
                        }")
}

custom_tooltip_short <- function() {
  JS("function() { return '<b>' + this.series.name + '</b>' + ' ' + 
     Highcharts.numberFormat(this.y, 0) + ' USD (FOB)' }")
}

data_labels <- function() {
  JS("function() { return this.key + '<br>' + Math.round(this.point.value / this.point.series.tree.val * 10000 ) / 100 + '%'}")
}

od_order_and_add_continent <- function(d, col = "trade_value_usd_exp") {
  d <- d %>% 
    select(country_iso = partner_iso, trade_value = !!sym(col))
  
  d <- d %>% 
    
    full_join(
      tbl(con, "countries") %>% 
        select(country_iso, country_name = country_name_english,
               continent_name = continent_name_english) %>% 
        collect() %>% 
        filter(!grepl("c-|all", country_iso))
    ) %>% 
  
    mutate(
      continent_name = case_when(
        is.na(continent_name) ~ country_name,
        TRUE ~ continent_name
      )
    ) %>% 
    
    group_by(country_iso, country_name, continent_name) %>% 
    summarise(trade_value = sum(trade_value, na.rm = T)) %>% 
    ungroup() %>% 
    
    select(-country_iso)
  
  d <- d %>% 
    select(continent_name, country_name, trade_value) %>% 
    group_by(continent_name) %>% 
    mutate(sum_trade_value = sum(trade_value, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(-sum_trade_value) %>% 
    select(-sum_trade_value) 
  
  d <- map_df(
    d %>% 
      select(continent_name) %>% 
      distinct() %>% 
      pull(),
    function(c) {
      d %>% 
        filter(continent_name == c) %>% 
        arrange(-trade_value)
    }
  )
  
  return(d)
}

od_colors <- function(d) {
  d %>% 
    select(continent_name) %>% 
    distinct() %>% 
    
    inner_join(
      tbl(con, "countries") %>% 
        select(country_iso, continent_name = continent_name_english, country_name_english) %>% 
        collect() %>% 
        filter(grepl("c-|e-536|e-837|e-838|e-839|e-899", country_iso)) %>% 
        mutate(continent_name = ifelse(is.na(continent_name), country_name_english, continent_name)) %>% 
        select(-country_name_english) %>% 
        left_join(
          tbl(con, "countries_colors") %>% 
            select(country_iso, country_color) %>% 
            collect()
        )
    ) %>% 
    
    select(-country_iso)
}

od_to_highcharts <- function(d, d2) {
  dd <- d %>% 
    mutate(continent_name = factor(continent_name, levels = d2$continent_name)) %>% 
    arrange(continent_name)
  
  new_lvls <- dd %>% 
    group_by(continent_name, country_name) %>% 
    summarise(trade_value = sum(trade_value, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>% 
    arrange(desc(trade_value)) %>% 
    distinct(continent_name) %>% 
    pull(continent_name)
  
  new_colors <- d2 %>% 
    mutate(continent_name = factor(continent_name, levels = new_lvls)) %>% 
    arrange(continent_name) %>% 
    pull(country_color)
  
  els <- data_to_hierarchical(dd, c(continent_name, country_name), trade_value, color = new_colors)
  
  lopts <- getOption("highcharter.lang")
  lopts$thousandsSep <- ","
  options(highcharter.lang = lopts)
  
  hchart(
    els,
    type = "treemap",
    levelIsConstant = FALSE,
    allowDrillToNode = TRUE,
    levels = lvl_opts,
    tooltip = list(
      pointFormatter = custom_tooltip()
    ),
    dataLabels = list(
      formatter = data_labels()
    )
  )
}

# PRODUCT TREEMAPS ----

pd_fix_section_and_aggregate <- function(d, col = "trade_value_usd_exp") {
  d <- d %>% 
    select(commodity_code, trade_value = !!sym(col)) %>% 
    
    pd_aggregate_products() %>% 
    
    group_by(section_name, commodity_name) %>% 
    summarise(trade_value = sum(trade_value, na.rm = T)) %>% 
    ungroup()
  
  d <- d %>% 
    select(section_name, commodity_name, trade_value) %>% 
    group_by(section_name) %>% 
    mutate(sum_trade_value = sum(trade_value, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(-sum_trade_value) %>% 
    select(-sum_trade_value) 
  
  d <- map_df(
    d %>% 
      select(section_name) %>% 
      distinct() %>% 
      pull(),
    function(s) {
      d %>% 
        filter(section_name == s) %>% 
        arrange(-trade_value)
    }
  )
  
  return(d)
}

pd_aggregate_products <- function(d) {
  d %>% 
    full_join(
      tbl(con, "commodities") %>% 
        select(
          commodity_code,
          section_code, section_name = section_fullname_english
        ) %>% 
        filter(nchar(commodity_code) == 6) %>% 
        collect()
    ) %>% 
  
    mutate(commodity_code = substr(commodity_code, 1, 4)) %>% 
    group_by(commodity_code, section_code, section_name) %>% 
    summarise(trade_value = sum(trade_value, na.rm = T)) %>% 
    ungroup() %>% 
    
    left_join(
      tbl(con, "commodities_short") %>% 
        select(commodity_code, commodity_name = commodity_fullname_english) %>% 
        collect()
    )
}
    
pd_colors <- function(d) {
  d %>% 
    select(section_name) %>% 
    distinct() %>% 
    
    inner_join(
      tbl(con, "sections") %>% 
        select(section_name = section_fullname_english, section_code) %>% 
        collect() %>% 
        
        inner_join(
          tbl(con, "sections_colors") %>% 
            collect()
        ) %>% 
        select(-section_code)
    )
}

pd_to_highcharts <- function(d, d2) {
  dd <- d %>% 
    mutate(section_name = factor(section_name, levels = d2$section_name)) %>% 
    arrange(section_name)
  
  new_lvls <- dd %>% 
    group_by(section_name, commodity_name) %>% 
    summarise(trade_value = sum(trade_value, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>% 
    arrange(desc(trade_value)) %>% 
    distinct(section_name) %>% 
    pull(section_name)
  
  new_colors <- d2 %>% 
    mutate(section_name = factor(section_name, levels = new_lvls)) %>% 
    arrange(section_name) %>% 
    pull(section_color)
  
  els <- data_to_hierarchical(dd, c(section_name, commodity_name), trade_value, color = new_colors)
  
  lopts <- getOption("highcharter.lang")
  lopts$thousandsSep <- ","
  options(highcharter.lang = lopts)
  
  hchart(
    els,
    type = "treemap",
    levelIsConstant = FALSE,
    allowDrillToNode = TRUE,
    levels = lvl_opts,
    tooltip = list(
      pointFormatter = custom_tooltip()
    ),
    dataLabels = list(
      formatter = data_labels()
    )
  )
}
