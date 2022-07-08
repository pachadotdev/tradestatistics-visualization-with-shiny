library(dplyr)
library(DBI)
library(RPostgres)
library(glue)

readRenviron("/tradestatistics/visualization-with-shiny")

con <- dbConnect(
  drv = Postgres(),
  dbname = "tradestatistics",
  host = "localhost",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

# reporters ----

reporters_to_display <- tbl(con, "countries") %>%
  select(!!sym("country_iso"), !!sym("country_name_english")) %>%
  collect()

reporters_out <- reporters_to_display %>%
  select(!!sym("country_iso")) %>%
  pull()

reporters_names_out <- reporters_to_display %>%
  select(!!sym("country_name_english")) %>%
  pull()

# names(reporters_out) <- glue("{ reporters_out } - { reporters_names_out }")
names(reporters_out) <- reporters_names_out

reporters_to_display <- reporters_out

# sections ----

sections_to_display <- tbl(con, "sections") %>%
  collect()

sections_out <- sections_to_display %>%
  select(!!sym("section_code")) %>%
  pull()

sections_names_out <- sections_to_display %>%
  select(!!sym("section_fullname_english")) %>%
  pull()

names(sections_out) <- glue("{ sections_out } - { sections_names_out }")

sections_to_display <- sections_out

# commodities ----

commodities_to_display <- tbl(con, "commodities_short") %>%
  select(commodity_code, commodity_fullname_english) %>%
  collect()

commodities_out <- commodities_to_display %>%
  select(!!sym("commodity_code")) %>%
  pull()

commodities_names_out <- commodities_to_display %>%
  select(!!sym("commodity_fullname_english")) %>%
  pull()

names(commodities_out) <- glue("{ commodities_out } - { commodities_names_out }")

commodities_to_display <- commodities_out

# output ----

usethis::use_data(reporters_to_display, overwrite = T)

usethis::use_data(sections_to_display, overwrite = T)

usethis::use_data(commodities_to_display, overwrite = T)

DBI::dbDisconnect(con)
