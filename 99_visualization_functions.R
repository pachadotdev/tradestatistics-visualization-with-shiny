# DESTINATION TREEMAPS -----

## EXPORTS ----

cols_exp <- function(d) {
  d %>% 
    group_by(continent_name) %>% 
    summarise(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = T)) %>% 
    arrange(-trade_value_usd_exp) %>% 
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

add_share_and_continent_exp <- function(d) {
  d %>% 
    select(partner_iso, partner_name, trade_value_usd_exp) %>%
    
    mutate(
      share = trade_value_usd_exp / sum(trade_value_usd_exp)
    ) %>% 
    
    left_join(
      ots_countries %>% select(country_iso,
                               continent_name = continent_name_english),
      by = c("partner_iso" = "country_iso")
    )
}

add_labels_exp <- function(d) {
  d %>% 
    mutate(
      share = paste0(round(100 * share, 2), "%"),
      partner_name = paste0(partner_name, "<br>", share)
    ) %>% 
    
    group_by(continent_name) %>% 
    mutate(
      share_continent = sum(trade_value_usd_exp, na.rm = T)
    ) %>% 
    ungroup() %>% 
    
    mutate(
      share_continent = share_continent / sum(trade_value_usd_exp, na.rm = T)
    ) %>% 
    mutate(
      share_continent = paste0(round(100 * share_continent, 2), "%"),
      continent_name = paste0(continent_name, "<br>", share_continent)
    )
}

## IMPORTS ----

cols_imp <- function(d) {
  d %>% 
    group_by(continent_name) %>% 
    summarise(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = T)) %>% 
    arrange(-trade_value_usd_imp) %>% 
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

add_share_and_continent_imp <- function(d) {
  d %>% 
    select(partner_iso, partner_name, trade_value_usd_imp) %>%
    
    mutate(
      share = trade_value_usd_imp / sum(trade_value_usd_imp)
    ) %>% 
    
    left_join(
      ots_countries %>% select(country_iso,
                               continent_name = continent_name_english),
      by = c("partner_iso" = "country_iso")
    )
}

add_labels_imp <- function(d) {
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
      share_continent = sum(trade_value_usd_imp, na.rm = T)
    ) %>% 
    ungroup() %>% 
    
    mutate(
      share_continent = share_continent / sum(trade_value_usd_imp, na.rm = T)
    ) %>% 
    mutate(
      share_continent = paste0(round(100 * share_continent, 2), "%"),
      continent_name = paste0(continent_name, "<br>", share_continent)
    )
}
