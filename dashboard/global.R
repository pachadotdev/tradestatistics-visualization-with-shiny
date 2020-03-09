# Packages ----------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(highcharter)
library(tradestatistics) # from github.com/tradestatistics/tradestatistics

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
         paste0(round(x / 10e5, 2), "M"))
}

show_percentage <- function(x) {
  paste0(round(100 * x, 2), "%")
}

growth_rate <- function(p, q, t) {
  (p / q)^(1 / (max(t) - min(t))) - 1
}

# Custom value boxes ------------------------------------------------------

customValueBox <- function(value, subtitle, icon = NULL, color = "aqua", width = 4,
                     href = NULL, inputId = NULL)
{
  boxContent <- div(id = inputId, class = paste0("small-box bg-", color),
                    div(class = "inner",
                        h3(class = "value-box-value", value),
                        p(subtitle)
                    ),
                    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href))
    boxContent <- a(href = href, boxContent)
  
  div(class = if (!is.null(width)) paste0("col-sm-", width),
      boxContent
  )
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

# styles ------------------------------------------------------------------

styles <- list(
  skin_color = "blue-light",
  css_files = c("css/AdminLTE.min.css", "css/_all-skins.min.css", "css/custom.min.css")
)

# Highcharts --------------------------------------------------------------

hc_export_menu <- list(
  list(text="Download PNG image",
       onclick=JS("function () { 
                  this.exportChart({ type: 'image/png' }); }")),
  list(text="Download JPEG image",
       onclick=JS("function () { 
                  this.exportChart({ type: 'image/jpeg' }); }")),
  list(text="Download SVG vector image",
       onclick=JS("function () { 
                  this.exportChart({ type: 'image/svg+xml' }); }")),
  list(text="Download PDF document",
       onclick=JS("function () { 
                  this.exportChart({ type: 'application/pdf' }); }"))
       )
