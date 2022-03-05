# DESTINATION TREEMAPS -----

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
    pull(country_color)
}

od_add_labels <- function(d) {
  d %>% 
    mutate(
      share = paste0(round(100 * share, 2), "%"),
      partner_name = paste0(partner_name, "<br>", share)
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
