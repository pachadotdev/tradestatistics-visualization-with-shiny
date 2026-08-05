#' @title Open Trade Statistics Dashboard
#' @description Iteractive dashboard to explore Open Trade Statistics data, which consists in a
#'  curated version of the International Trade and Production Database for Estimation (ITPD-E) and
#'  the International Trade and Production Database for Simulation (ITPD-S).
#' @import tabler
#' @import d3po
#' @importFrom tinycache dcache
#' @importFrom data.table `:=` .N .I .SD copy data.table fifelse frankv rbindlist setDT setnames setorder uniqueN
#' @importFrom DBI dbConnect dbDisconnect dbIsValid dbGetQuery
#' @importFrom glue glue
#' @importFrom htmlwidgets JS
#' @importFrom jsonlite toJSON
#' @importFrom rio export
#' @importFrom RPostgres Postgres
#' @importFrom stats setNames
"_PACKAGE"

# THIS IS KINDA BAD PRACTISE BUT THIS IS AN INTERNAL PKG TO AVOID A VERY LONG 1-SCRIPT APP
utils::globalVariables(c(
  ".", ".data",
  "broad_sector", "broad_sector_id",
  "case_id", "color", "commodity_name", "continent_name", "country", "country_color", "country_name",
  "exp_pct", "exp_rank", "exp_share", "exporter", "exporter_iso3_dynamic",
  "financial", "flow",
  "imp_pct", "imp_rank", "imp_share", "importer", "industry_id",
  "n",
  "region_colour",
  "sanctioning_state_dynamic",
  "sector_color", "sum_trade_value",
  "trade", "trade_exp", "trade_imp", "trade_value", "trd_value_usd_bal",
  "year",
  "outcome", "partner", "priority", "rank"
))

.onLoad <- function(libname, pkgname) {
  tablerOptions(cache = dcache(dir = "/tradestatistics/cache"))
}

#' @title Countries
#' @descriptionInternal dataset for country codes.
#' @docType data
#' @keywords datasets
#' @name countries
"countries"

#' @title Sectors
#' @description Internal dataset for sector codes.
#' @docType data
#' @keywords datasets
#' @name sectors
"sectors"

#' @title Industries
#' @description Internal dataset for industry codes.
#' @docType data
#' @keywords datasets
#' @name industries
"industries"
