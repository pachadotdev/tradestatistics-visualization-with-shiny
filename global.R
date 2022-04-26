# Packages ----------------------------------------------------------------

source("99_packages.R")

# SQL connection ----------------------------------------------------------

con <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "tradestatistics",
  host = "tradestatistics.io",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

# URLs --------------------------------------------------------------------

site_url <- "https://shiny.tradestatistics.io"

# Tables ------------------------------------------------------------------

countries <- tradestatistics::ots_countries %>%
  select(country_iso, country_name_english)

# Paragraphs format -------------------------------------------------------

show_dollars <- function(x) {
  ifelse(x %/% 10e8 >= 1,
    paste0(round(x / 10e8, 2), "B"),
    paste0(round(x / 10e5, 2), "M")
  )
}

show_percentage <- function(x) {
  paste0(round(100 * x, 2), "%")
}

growth_rate <- function(p, q, t) {
  (p / q)^(1 / (max(t) - min(t))) - 1
}

# Choices -----------------------------------------------------------------

# choices trick by Andrea Gao
# http://gytcrt.github.io/gytcrt.github.io/2016/08/11/RShiny-easily-passing-a-long-list-of-items-to-selectInput-choices/

available_tables <- c("tc", "ntc")
names(available_tables) <- c("Raw data (i.e., with transportation costs)",
                             "Imputed data (i.e., corrected flows and without transportation costs)")

available_years <- 2002:2020

available_years_min <- min(available_years)
available_years_max <- max(available_years)

available_reporters_iso <- as.list(countries$country_iso)
available_reporters_iso <- c("all", available_reporters_iso[grep("^c-|all", available_reporters_iso, invert = T)])
names(available_reporters_iso) <- c("the World", as.vector(countries$country_name_english[grep("^Alias", countries$country_name_english, invert = T)]))

reporters_to_display <- tibble(
  available_reporters_iso = purrr::as_vector(available_reporters_iso),
  available_reporters_names = names(available_reporters_iso)
)

available_commodities <- tradestatistics::ots_commodities$commodity_code
names(available_commodities) <- paste(tradestatistics::ots_commodities$commodity_code,
                                      tradestatistics::ots_commodities$commodity_fullname_english,
                                      sep = " - ")

available_groups <- c("All Products", "Vaccine Inputs", tradestatistics::ots_sections$section_fullname_english)

available_models <- list("ols", "olsrem", "olsfe", "ppml")
names(available_models) <- c("OLS", "OLS (Remoteness Index)", "OLS (Fixed Effects)", "Poisson Pseudo Maximum Likelihood (PPML)")
  
available_formats <- c("csv", "tsv", "json", "xlsx", "sav", "dta")

# Buttons -----------------------------------------------------------------

download_button <- function(outputId, label = "Download", class = NULL, ...) {
  aTag <- tags$a(
    id = outputId, class = paste(
      "btn btn-default action-button",
      class
    ), href = "", target = "_blank", download = NA,
    icon("download"), label, ...
  )
}

# Bookmarking -------------------------------------------------------------

enableBookmarking(store = "url")

# Styles ------------------------------------------------------------------

styles <- list(
  skin_color = "blue",
  css_files = c("css/AdminLTE.min.css", "css/_all-skins.min.css", "css/custom.min.css",
                "css/ion.rangeSlider.min.css")
)

# File upload -------------------------------------------------------------

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 100MB.
options(shiny.maxRequestSize = 100*1024^2)

# Modularized functions ---------------------------------------------------

source("99_highcharts_options.R")
source("99_visualization_functions.R")
