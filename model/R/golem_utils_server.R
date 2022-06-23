#' Available Server parameters expressed as functions
#' @importFrom glue glue
#' @noRd
available_formats <- function() { c("csv", "tsv", "xlsx", "sav", "dta") }

#' SQL connection
#' @importFrom pool dbPool
#' @importFrom RPostgres Postgres
#' @noRd
sql_con <- function() {
  readRenviron("/tradestatistics/visualization-with-shiny")

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

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)
