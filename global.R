# Packages ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(highcharter)
library(tradestatistics)
library(fixest)
library(cepiigeodist)
# library(rio) # used to import/export data

# URLs --------------------------------------------------------------------

running_on_server <- T

if (running_on_server == TRUE) {
  base_url <- "http://localhost:8080"
  use_localhost <- TRUE
} else {
  base_url <- "https://api.tradestatistics.io"
  use_localhost <- FALSE
}

site_url <- "https://shiny.tradestatistics.io"

# Tables ------------------------------------------------------------------

countries <- ots_countries %>%
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

available_models <- list("ols", "olsrem", "olsfe", "ppml")
names(available_models) <- c("OLS", "OLS (Remoteness Index)", "OLS (Fixed Effects)", "Poisson Pseudo Maximum Likelihood (PPML)")

available_groups <- c("All Products", "Vaccine Inputs", sort(unique(ots_commodities$group_fullname_english)))
  
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

# Colors for group visualization (instead of communities) -----------------

# colors_jbk <- c("#037a7e", "#89bda7", "#c0bda8", "#e6929a", "#429c91",
#                 "#8f9e98", "#c0d6b8", "#f2a6a3", "#67b1a4", "#a3dbbe")

# colors_escap <- c("#1a3668", "#ea1c2d", "#d19f2a", "#2d9a47", "#c22033",
#                   "#ef412a", "#00add8", "#fdb714", "#8f1838", "#8f1838")

colors_jbk <- c("#037a7e", "#e6929a")

# groups_colors <- read.csv("groups_colors.csv")

# sections_colors <- tradestatistics::ots_commodities %>% 
#   select(section_fullname_english) %>% 
#   distinct()
# 
# sections_colors <- sections_colors %>% 
#   mutate(
#     section_fullname_english = case_when(
#       is.na(section_fullname_english) ~ "Unspecified",
#       TRUE ~ section_fullname_english
#     )
#   )
#
# sections_colors <- sections_colors %>% 
#   mutate(
#     section_color = c("#bf616e",
#                      "#7ab93d",
#                      "#8b60da",
#                      "#cd9931",
#                      "#5a7fde",
#                      "#cd5b2f",
#                      "#4eadd3",
#                      "#d53b57",
#                      "#52a667",
#                      "#c251bb",
#                      "#79943f",
#                      "#d53d8b",
#                      "#45ac95",
#                      "#c86eb9",
#                      "#a8844d",
#                      "#7c5db2",
#                      "#ca685a",
#                      "#638bd1",
#                      "#9d446b",
#                      "#9082c0",
#                      "#e17da6",
#                      "#b57aa9")
#   )
# readr::write_csv(sections_colors, "sections_colors.csv")

sections_colors <- read.csv("sections_colors.csv")

# Bookmarking -------------------------------------------------------------

enableBookmarking(store = "url")

# Styles ------------------------------------------------------------------

styles <- list(
  skin_color = "green-light",
  css_files = c("css/AdminLTE.min.css", "css/_all-skins.min.css", "css/custom.min.css",
                "css/ion.rangeSlider.min.css")
)

# File upload -------------------------------------------------------------

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 100MB.
options(shiny.maxRequestSize = 100*1024^2)
