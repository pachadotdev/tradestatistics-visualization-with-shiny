gdp_deflator_adjustment <- function(d, reference_year) {
  # Filter year conversion rates and join data ------------------------------
  years <- d %>% 
    distinct(year) %>% 
    pull()
  
  dd <- map_df(
    years,
    function(year) {
      if (year < reference_year) {
        tbl(con, "gdp_deflator") %>% 
          filter(year_to <= reference_year & year_to > year & country_iso == "wld") %>% 
          collect() %>% 
          summarise(gdp_deflator = last(cumprod(gdp_deflator))) %>% 
          mutate(year = year, conversion_year = reference_year)
      } else if (year > reference_year) {
        tbl(con, "gdp_deflator") %>% 
          filter(year_from >= reference_year & year_from < year & country_iso == "wld") %>% 
          collect() %>% 
          summarise(gdp_deflator = 1 / last(cumprod(gdp_deflator))) %>% 
          mutate(year = year, conversion_year = reference_year)
      } else if (year == reference_year) {
        tibble(
          year = year, conversion_year = year, gdp_deflator = 1
        )
      }
    }
  )
  
  d <- d %>% 
    left_join(dd, by = "year") %>% 
    mutate(
      trade_value_usd_imp = round(trade_value_usd_imp * gdp_deflator, 0),
      trade_value_usd_exp = round(trade_value_usd_exp * gdp_deflator, 0)
    )

  return(d)
}
