get_available_years <- function() {
  base_url <- "https://api.tradestatistics.io"
  # base_url <- "http://localhost:8080"

  sprintf("%s/year_range", base_url) %>%
    jsonlite::fromJSON() %>%
    purrr::as_vector()
}

get_available_reporters_iso <- function() {
  tradestatistics::ots_countries %>%
    dplyr::select(country_iso, country_name_english) %>%
    dplyr::pull(country_iso) %>%
    as.list()
}
