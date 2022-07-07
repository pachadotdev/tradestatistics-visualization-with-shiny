#' Available UI parameters expressed as functions
#' @noRd
available_all <- function() { c("All Products" = "all") }

available_logicals <- function() {
  c("Yes" = "yes", "No" = "no")
}

available_models <- function() {
  c("OLS" = "ols",
    "OLS (Fixed Effects)" = "olsfe",
    "Poisson Pseudo Maximum Likelihood (PPML)" = "ppml")
}

available_reporters_iso <- function() {
  otsshinyproductprofiles::reporters_to_display
}

available_sections_code <- function() {
  otsshinyproductprofiles::sections_to_display
}

available_commodities_code <- function() {
  otsshinyproductprofiles::commodities_to_display
}

available_vaccine <- function() { c("Vaccine Inputs" = "vaccine") }

available_yrs <- function() { 2002:2020 }

available_yrs_min <- function() { min(available_yrs()) }

available_yrs_max <- function() { max(available_yrs()) }

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' @importFrom shiny tags
enurl <- function(url, text) {
  tags$a(href = url, text)
}

#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...) {
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...) {
  column(10, ...)
}

#' @importFrom shiny column
col_9 <- function(...) {
  column(9, ...)
}

#' @importFrom shiny column
col_8 <- function(...) {
  column(8, ...)
}

#' @importFrom shiny column
col_6 <- function(...) {
  column(6, ...)
}


#' @importFrom shiny column
col_4 <- function(...) {
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...) {
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...) {
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...) {
  column(1, ...)
}
