# ORIGIN/DESTINATION TREEMAPS -----

od_add_share_and_continent <- function(d, col = "trade_value_usd_exp") {
  d %>% 
    select(partner_iso, partner_name, trade_value = !!sym(col)) %>%
    
    mutate(
      share = trade_value / sum(trade_value)
    ) %>% 
    
    left_join(
      ots_countries %>% select(country_iso,
                               continent_name = continent_name_english),
      by = c("partner_iso" = "country_iso")
    )
}

od_colors <- function(d) {
  d %>% 
    group_by(continent_name) %>% 
    summarise(trade_value = sum(trade_value, na.rm = T)) %>% 
    arrange(-trade_value) %>% 
    left_join(
      ots_countries %>% 
        select(country_iso, continent_name = continent_name_english) %>% 
        filter(grepl("c-", country_iso)) 
    ) %>% 
    inner_join(
      ots_countries_colors %>% 
        select(country_iso, country_color)
    ) %>% 
    select(-country_iso) %>% 
    distinct(country_color) %>% 
    pull()
}

od_add_labels <- function(d) {
  d %>% 
    mutate(
      share = paste0(round(100 * share, 2), "%"),
      partner_name = paste0(partner_name, "<br>", share)
    ) %>% 
    
    mutate(
      continent_name = case_when(
        is.na(continent_name) ~ "Unspecified",
        TRUE ~ continent_name
      )
    ) %>% 
    
    group_by(continent_name) %>% 
    mutate(
      share_continent = sum(trade_value, na.rm = T)
    ) %>% 
    ungroup() %>% 
    
    mutate(
      share_continent = share_continent / sum(trade_value, na.rm = T)
    ) %>% 
    mutate(
      share_continent = paste0(round(100 * share_continent, 2), "%"),
      continent_name = paste0(continent_name, "<br>", share_continent)
    )
}

# PRODUCT TREEMAPS ----

pd_add_share_fix_section <- function(d, col = "trade_value_usd_exp") {
  d %>% 
    rename(trade_value = !!sym(col)) %>% 
    
    pd_aggregate_products() %>% 
    
    mutate(
      share = paste0(round(100 * trade_value / sum(trade_value), 2), "%"),
      commodity_name = paste0(commodity_name, "<br>", share)
    ) %>% 
    
    mutate(
      section_name = case_when(
        is.na(section_name) ~ "Unspecified",
        TRUE ~ section_name
      )
    )
}

pd_aggregate_products <- function(d) {
  d %>% 
    select(-commodity_name) %>% 
    left_join(ots_sections_colors) %>% 
    mutate(commodity_code = substr(commodity_code, 1, 4)) %>% 
    left_join(
      ots_commodities_short %>% 
        rename(commodity_name = commodity_fullname_english)
    ) %>% 
    group_by(commodity_name, section_name, section_color) %>% 
    summarise(trade_value = sum(trade_value, na.rm = T)) %>% 
    ungroup()
}
    
pd_colors <- function(d) {
  d %>% 
    group_by(section_color) %>% 
    summarise(trade_value = sum(trade_value, na.rm = T)) %>% 
    arrange(-trade_value) %>% 
    select(section_color) %>% 
    mutate(
      section_color = case_when(
        is.na(section_color) ~ "#d3d3d3",
        TRUE ~ section_color
      )
    ) %>% 
    pull()
}

pd_add_labels <- function(d, col = "trade_value_usd_exp") {
  d %>% 
    group_by(section_name) %>%
    mutate(
      share_section = sum(trade_value, na.rm = T)
    ) %>%
    
    ungroup() %>%
    mutate(
      share_section = share_section / sum(trade_value, na.rm = T)
    ) %>%
    
    mutate(
      share_section = paste0(round(100 * share_section, 2), "%"),
      section_name = paste0(section_name, "<br>", share_section)
    )
}
