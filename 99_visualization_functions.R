# ORIGIN/DESTINATION TREEMAPS -----

od_add_share_and_continent <- function(d, col = "trade_value_usd_exp") {
  d %>% 
    select(partner_iso, trade_value = !!sym(col)) %>%
    
    full_join(
      ots_countries %>% 
        select(partner_iso = country_iso, partner_name = country_name_english,
               continent_name = continent_name_english) %>% 
        filter(!grepl("c-|all", partner_iso))
    ) %>% 
  
    mutate(
      continent_name = case_when(
        is.na(continent_name) ~ "Unspecified",
        TRUE ~ continent_name
      )
    ) %>% 
    group_by(partner_iso, partner_name, continent_name) %>% 
    summarise(trade_value = sum(trade_value, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(continent_name)
}

od_colors <- function(d) {
  d %>% 
    group_by(continent_name) %>% 
    summarise(x = sum(trade_value, na.rm = T)) %>% 
    
    left_join(
      ots_countries %>% 
        select(country_iso, continent_name = continent_name_english) %>% 
        filter(grepl("c-|ata", country_iso)) 
    ) %>% 
    left_join(
      ots_countries_colors %>% 
        select(country_iso, country_color)
    ) %>% 
    mutate(
      country_color = case_when(
        is.na(country_color) ~ "#D3D3D3",
        TRUE ~ country_color
      )
    ) %>% 
    
    arrange(-x) %>% 
    distinct(country_color) %>% 
    pull()
}

od_add_labels <- function(d) {
  d %>% 
    
    group_by(continent_name) %>% 
    mutate(
      share_continent = sum(trade_value, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      share_continent = share_continent / sum(trade_value, na.rm = T)
    ) %>% 
    
    mutate(
      share = trade_value / sum(trade_value, na.rm = T)
    ) %>% 
    
    mutate(
      share = paste0(round(100 * share, 2), "%"),
      partner_name = paste0(partner_name, "<br>", share)
    ) %>% 
    
    mutate(
      share_continent = paste0(round(100 * share_continent, 2), "%"),
      continent_name = paste0(continent_name, "<br>", share_continent)
    )
}

# PRODUCT TREEMAPS ----

pd_fix_section <- function(d, col = "trade_value_usd_exp") {
  d %>% 
    select(commodity_code, trade_value = !!sym(col)) %>% 
    
    pd_aggregate_products() %>% 
    
    mutate(
      section_name = case_when(
        is.na(section_name) ~ "Unspecified",
        TRUE ~ section_name
      ),
      section_code = case_when(
        is.na(section_code) ~ "99",
        TRUE ~ section_code
      )
    ) %>% 
    
    group_by(commodity_code, commodity_name, section_code, section_name) %>% 
    summarise(trade_value = sum(trade_value, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(section_code)
}

pd_aggregate_products <- function(d) {
  d %>% 
    full_join(
      ots_commodities %>% 
        select(
          commodity_code,
          section_code, section_name = section_fullname_english
        ) %>% 
        filter(nchar(commodity_code) == 6)
    ) %>% 
  
    mutate(commodity_code = substr(commodity_code, 1, 4)) %>% 
    group_by(commodity_code, section_code, section_name) %>% 
    summarise(trade_value = sum(trade_value, na.rm = T)) %>% 
    ungroup() %>% 
    
    left_join(
      ots_commodities_short %>% 
        rename(commodity_name = commodity_fullname_english)
    )
}
    
pd_colors <- function(d) {
  d %>% 
    group_by(section_name) %>% 
    summarise(x = sum(trade_value, na.rm = T)) %>% 
    
    left_join(
      inner_join(
        ots_commodities %>% 
          select(section_name = section_fullname_english, section_code) %>% 
          distinct(),
        ots_sections_colors
      )
    ) %>% 
    mutate(
      section_color = case_when(
        is.na(section_color) ~ "#D3D3D3",
        TRUE ~ section_color
      ),
      section_code = case_when(
        is.na(section_code) ~ "99",
        TRUE ~ section_code
      )
    ) %>% 
    
    arrange(-x) %>% 
    distinct(section_color) %>% 
    pull()
}

pd_add_labels <- function(d) {
  d %>% 
    
    mutate(
      share = paste0(round(100 * trade_value / sum(trade_value), 2), "%"),
      commodity_name = paste0(commodity_name, "<br>", share)
    ) %>% 
    
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
