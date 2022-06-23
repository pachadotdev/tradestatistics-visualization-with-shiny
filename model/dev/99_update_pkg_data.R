library(dplyr)
library(DBI)
library(RPostgres)

readRenviron("/tradestatistics/visualization-with-shiny")

con <- dbConnect(
  drv = Postgres(),
  dbname = "tradestatistics",
  host = "localhost",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

reporters_to_display <- tbl(con, "countries") %>%
  select(!!sym("country_iso"), !!sym("country_name_english")) %>%
  collect()

sections_to_display <- tbl(sql_con(), "sections") %>% collect()

usethis::use_data(reporters_to_display, overwrite = T)

usethis::use_data(sections_to_display, overwrite = T)

DBI::dbDisconnect(con)
