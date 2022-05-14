# Packages ----------------------------------------------------------------

source("99_packages.R")

# SQL connection ----------------------------------------------------------

# Read credentials from file excluded in .gitignore
readRenviron("/tradestatistics/visualization-with-shiny")

con <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "tradestatistics",
  host = "localhost",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

# URLs --------------------------------------------------------------------

site_url <- "https://shiny.tradestatistics.io"

# Tables ------------------------------------------------------------------

commodities <- tbl(con, "commodities") %>%
  select(commodity_code, commodity_fullname_english) %>% 
  collect()

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

reporters_to_display <- tbl(con, "countries") %>%
  select(country_iso, country_name_english) %>% 
  collect()

available_reporters_iso <- reporters_to_display$country_iso
names(available_reporters_iso) <- reporters_to_display$country_name_english

available_reporters_iso <- c("all", sort(available_reporters_iso[grep("^c-|all", available_reporters_iso, invert = T)]))
names(available_reporters_iso)[1] <- "the World"

reporters_to_display <- tibble(
  available_reporters_iso = available_reporters_iso,
  available_reporters_names = names(available_reporters_iso)
)

sections_to_display <- tbl(con, "sections") %>%
  collect()

available_all <- c("All Products" = "all")
available_vaccine <- c("Vaccine Inputs" = "vaccine")

available_sections_code <- sections_to_display$section_code
names(available_sections_code) <- sections_to_display$section_fullname_english
names(available_sections_code) <- glue("{ stringr::str_pad(1:22, 2, 'left', '0') } - { names(available_sections_code) }")

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
source("99_gdp_deflator_adjustment.R")
