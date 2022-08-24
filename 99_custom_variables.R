library(arrow)
library(dplyr)
library(purrr)

d <- open_dataset("../hs12-visualization/yrpc",
                  partitioning = c("year", "reporter_iso"))

d2 <- map_df(
  2002,
  function(t) {
    t2 <- paste0("year=", t)
    set.seed(1234)
    d %>% 
      filter(year == t2) %>% 
      select(year, reporter_iso, partner_iso) %>% 
      collect() %>%
      distinct() %>% 
      mutate(reporter_iso = sub("reporter_iso=", "", reporter_iso),
             year = as.integer(sub("year=", "", year))) %>% 
      filter(reporter_iso != "0-unspecified",
             partner_iso != "0-unspecified") %>% 
      arrange(reporter_iso, partner_iso) %>% 
      left_join(
        cepiigeodist::dist_cepii %>% 
          mutate(reporter_iso = tolower(iso_o), 
                 partner_iso = tolower(iso_d),
                 distw = round(as.numeric(distw), 4)) %>% 
          select(reporter_iso, partner_iso, comcol, distw)
      )
  }
)

fout <- "custom_variables_for_modelling_demo.csv"
data.table::fwrite(d2, fout)
system(paste("7z a", sub("csv", "zip", fout), fout))
# file.remove(fout)
