# Packages ----------------------------------------------------------------

library(shiny)
library(shinyjs)
library(dplyr)
library(tradestatistics)
library(highcharter)

# URLs --------------------------------------------------------------------

running_on_server <- T

if (running_on_server == TRUE) {
  base_url <- "http://localhost:8080"
  use_localhost <- TRUE
} else {
  base_url <- "https://api.tradestatistics.io"
  use_localhost <- FALSE
}

# Tables ------------------------------------------------------------------

countries <- ots_countries %>%
  select(country_iso, country_name_english)

products <- ots_products %>%
  arrange(product_code)

# Choices -----------------------------------------------------------------

# choices trick by Andrea Gao
# http://gytcrt.github.io/gytcrt.github.io/2016/08/11/RShiny-easily-passing-a-long-list-of-items-to-selectInput-choices/

available_tables <- as.list(c("select", "yr", "yrp"))
names(available_tables) <- c("Select", "Multilateral trade", "Bilateral trade")

available_years <- sprintf("%s/year_range", base_url) %>%
  jsonlite::fromJSON() %>%
  purrr::as_vector()

available_years_min <- min(available_years)
available_years_max <- max(available_years)

available_reporters_iso <- as.list(countries$country_iso)
available_reporters_iso <- c("all", available_reporters_iso[grep("^c-|all", available_reporters_iso, invert = T)])
names(available_reporters_iso) <- c("the World", as.vector(countries$country_name_english[grep("^Alias", countries$country_name_english, invert = T)]))

reporters_to_display <- tibble(
  available_reporters_iso = purrr::as_vector(available_reporters_iso),
  available_reporters_names = names(available_reporters_iso)
)

# Bookmarking -------------------------------------------------------------

enableBookmarking(store = "url")

# Highcharts --------------------------------------------------------------

hc_export_menu <- list(
  list(
    text = "Download PNG image",
    onclick = JS("function () { 
                  this.exportChart({ type: 'image/png' }); }")
  ),
  list(
    text = "Download JPEG image",
    onclick = JS("function () { 
                  this.exportChart({ type: 'image/jpeg' }); }")
  ),
  list(
    text = "Download SVG vector image",
    onclick = JS("function () { 
                  this.exportChart({ type: 'image/svg+xml' }); }")
  ),
  list(
    text = "Download PDF document",
    onclick = JS("function () { 
                  this.exportChart({ type: 'application/pdf' }); }")
  )
)
