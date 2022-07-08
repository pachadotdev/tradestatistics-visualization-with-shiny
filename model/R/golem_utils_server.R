#' Available Server parameters expressed as functions
#' @importFrom glue glue
#' @noRd
available_formats <- function() { c("csv", "tsv", "xlsx", "sav", "dta") }

#' SQL connection
#' @importFrom pool dbPool
#' @importFrom RPostgres Postgres
#' @noRd
sql_con <- function() {
  readRenviron("/tradestatistics")

  con <- dbPool(
    drv = Postgres(),
    dbname = "tradestatistics",
    host = "localhost",
    user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
    password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
  )

  return(con)
}

#' Custom regression error for models
#' @importFrom fixest feols
#' @noRd
custom_regression_error <- function() {
  feols(ERROR ~ UNFEASIBLE + ESTIMATION,
        data = data.frame(
          ERROR = c(1,0,0),
          UNFEASIBLE = c(0,1,0),
          ESTIMATION = c(0,0,1)
        )
  )
}

#' Convert dollars from year X to year Y
#' @importFrom dplyr distinct last tibble
#' @importFrom purrr map_df
#' @noRd
gdp_deflator_adjustment <- function(d, reference_year, con) {
  # Filter year conversion rates and join data ------------------------------
  years <- d %>%
    distinct(!!sym("year")) %>%
    pull()

  dd <- map_df(
    years,
    function(year) {
      if (year < reference_year) {
        tbl(con, "gdp_deflator") %>%
          filter(!!sym("year_to") <= reference_year &
                   !!sym("year_to") > year &
                   !!sym("country_iso") == "wld") %>%
          collect() %>%
          summarise(gdp_deflator = last(cumprod(!!sym("gdp_deflator")))) %>%
          mutate(year = year, conversion_year = reference_year)
      } else if (year > reference_year) {
        tbl(con, "gdp_deflator") %>%
          filter(!!sym("year_from") >= reference_year &
                   !!sym("year_from") < year &
                   !!sym("country_iso") == "wld") %>%
          collect() %>%
          summarise(gdp_deflator = 1 / last(cumprod(!!sym("gdp_deflator")))) %>%
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
      trade = round(!!sym("trade") * !!sym("gdp_deflator"), 0)
    )

  if (any(colnames(d) %in% c("gdp_exporter"))) {
    d <- d %>%
      mutate(
        gdp_exporter = round(!!sym("gdp_exporter") * !!sym("gdp_deflator"), 0)
      )
  }

  if (any(colnames(d) %in% c("gdp_exporter_percap"))) {
    d <- d %>%
      mutate(
        gdp_exporter_percap = round(!!sym("gdp_exporter_percap") * !!sym("gdp_deflator"), 0)
      )
  }

  if (any(colnames(d) %in% c("gdp_importer"))) {
    d <- d %>%
      mutate(
        gdp_importer = round(!!sym("gdp_importer") * !!sym("gdp_deflator"), 0)
      )
  }

  if (any(colnames(d) %in% c("gdp_importer_percap"))) {
    d <- d %>%
      mutate(
        gdp_importer_percap = round(!!sym("gdp_importer_percap") * !!sym("gdp_deflator"), 0)
      )
  }

  return(d)
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)
