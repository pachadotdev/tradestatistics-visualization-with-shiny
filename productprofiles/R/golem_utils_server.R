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

#' Convert dollars from year X to year Y
#' @importFrom dplyr distinct last pull tibble
#' @importFrom purrr map_df
#' @noRd
gdp_deflator_adjustment <- function(d, reference_year, sql_con) {
  years <- d %>%
    distinct(!!sym("year")) %>%
    pull()

  dd <- map_df(
    years,
    function(year) {
      if (year < reference_year) {
        tbl(sql_con, "gdp_deflator") %>%
          filter(!!sym("year_to") <= reference_year &
                   !!sym("year_to") > !!sym("year") &
                   !!sym("country_iso") == "wld") %>%
          collect() %>%
          summarise(gdp_deflator = last(cumprod(!!sym("gdp_deflator")))) %>%
          mutate(year = year, conversion_year = reference_year)
      } else if (year > reference_year) {
        tbl(sql_con, "gdp_deflator") %>%
          filter(!!sym("year_from") >= reference_year &
                   !!sym("year_from") < !!sym("year") &
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
      trade_value_usd_imp = round(!!sym("trade_value_usd_imp") * !!sym("gdp_deflator"), 0),
      trade_value_usd_exp = round(!!sym("trade_value_usd_exp") * !!sym("gdp_deflator"), 0)
    )

  return(d)
}

# ORIGIN/DESTINATION TREEMAPS -----

lvl_opts <- list(
  list(
    level = 1,
    borderWidth = 1,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.25),
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(
        fontSize = "12px",
        textOutline = FALSE
      )
    )
  ),
  list(
    level = 2,
    borderWidth = 1,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 1),
    dataLabels = list(enabled = FALSE),
    style = list(
      fontSize = "12px",
      textOutline = FALSE
    )
  )
)

#' Custom Tooltip (For Highcharter Visuals)
#' @importFrom highcharter JS
#' @noRd
custom_tooltip <- function() {
  JS("function() { return '<b>' + this.name + '</b>' + '<br>' +
                        'Share: ' + Math.round(this.value / this.series.tree.val * 10000)/100 + '%' + '<br>' +
                        'Value: ' + Highcharts.numberFormat(this.value, 0) + ' USD'
                        }")
}

#' Custom Short Tooltip (For Highcharter Visuals)
#' @importFrom highcharter JS
#' @noRd
custom_tooltip_short <- function() {
  JS("function() { return '<b>' + this.series.name + '</b>' + ' ' +
     Highcharts.numberFormat(this.y, 0) + ' USD' }")
}

#' Custom Data Labels (For Highcharter Visuals)
#' @importFrom highcharter JS
#' @noRd
data_labels <- function() {
  JS("function() { return this.key + '<br>' + Math.round(this.point.value / this.point.series.tree.val * 10000 ) / 100 + '%'}")
}

#' Origin-Destination Order and Add Continent (For Highcharter Visuals)
#' @importFrom dplyr case_when collect filter full_join group_by mutate select
#'     summarise tbl ungroup
#' @noRd
od_order_and_add_continent <- function(d, col = "trade_value_usd_exp", sql_con) {
  d <- d %>%
    select(country_iso = !!sym("reporter_iso"), trade_value = !!sym(col))

  d <- d %>%

    full_join(
      tbl(sql_con, "countries") %>%
        select(!!sym("country_iso"), country_name = !!sym("country_name_english"),
               continent_name = !!sym("continent_name_english")) %>%
        collect() %>%
        filter(!grepl("c-|all", !!sym("country_iso")))
    ) %>%

    mutate(
      continent_name = case_when(
        is.na(!!sym("continent_name")) ~ !!sym("country_name"),
        TRUE ~ !!sym("continent_name")
      )
    ) %>%

    group_by(!!sym("country_iso"), !!sym("country_name"), !!sym("continent_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup() %>%

    select(-!!sym("country_iso"))

  d <- d %>%
    select(!!sym("continent_name"), !!sym("country_name"), !!sym("trade_value")) %>%
    group_by(!!sym("continent_name")) %>%
    mutate(sum_trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup() %>%
    arrange(-!!sym("sum_trade_value")) %>%
    select(-!!sym("sum_trade_value"))

  d <- map_df(
    d %>%
      select(!!sym("continent_name")) %>%
      distinct() %>%
      pull(),
    function(c) {
      d %>%
        filter(!!sym("continent_name") == c) %>%
        arrange(-!!sym("trade_value"))
    }
  )

  return(d)
}

#' Origin-Destination Colours (For Highcharter Visuals)
#' @importFrom dplyr collect distinct filter left_join mutate select tbl
#' @noRd
od_colors <- function(d, sql_con) {
  d %>%
    select(!!sym("continent_name")) %>%
    distinct() %>%

    inner_join(
      tbl(sql_con, "countries") %>%
        select(!!sym("country_iso"), continent_name = !!sym("continent_name_english"),
               !!sym("country_name_english")) %>%
        collect() %>%
        filter(grepl("c-|e-536|e-837|e-838|e-839|e-899", !!sym("country_iso"))) %>%
        mutate(continent_name = ifelse(is.na(!!sym("continent_name")),
                                       !!sym("country_name_english"), !!sym("continent_name"))) %>%
        select(-!!sym("country_name_english")) %>%
        left_join(
          tbl(sql_con, "countries_colors") %>%
            select(!!("country_iso"), !!("country_color")) %>%
            collect()
        )
    ) %>%

    select(-!!sym("country_iso"))
}

#' Origin-Destination to Highcharter (For Highcharter Visuals)
#' @importFrom dplyr arrange distinct group_by mutate pull summarise ungroup
#' @importFrom highcharter data_to_hierarchical hchart
#' @noRd
od_to_highcharts <- function(d, d2) {
  dd <- d %>%
    mutate(continent_name = factor(!!sym("continent_name"), levels = d2$continent_name)) %>%
    arrange(!!sym("continent_name"))

  new_lvls <- dd %>%
    group_by(!!sym("continent_name"), !!sym("country_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>%
    arrange(desc(!!sym("trade_value"))) %>%
    distinct(!!sym("continent_name")) %>%
    pull(!!sym("continent_name"))

  new_colors <- d2 %>%
    mutate(continent_name = factor(!!sym("continent_name"),
                                   levels = new_lvls)) %>%
    arrange(!!sym("continent_name")) %>%
    pull(!!sym("country_color"))

  els <- data_to_hierarchical(dd, c("continent_name", "country_name"), "trade_value",
                              colors = new_colors)

  lopts <- getOption("highcharter.lang")
  lopts$thousandsSep <- ","
  options(highcharter.lang = lopts)

  hchart(
    els,
    type = "treemap",
    levelIsConstant = FALSE,
    allowDrillToNode = TRUE,
    levels = lvl_opts,
    tooltip = list(
      pointFormatter = custom_tooltip()
    ),
    dataLabels = list(
      formatter = data_labels()
    )
  )
}

# PRODUCT TREEMAPS ----

#' Add Percentages to Sections (For Highcharter Visuals)
#' @importFrom dplyr arrange distinct filter group_by mutate pull rename
#'     select summarise ungroup
#' @importFrom purrr map_df
#' @noRd
p_fix_section_and_aggregate <- function(d, col, sql_con) {
  d <- d %>%
    select(!!sym("commodity_code"), !!sym(col)) %>%
    rename(trade_value = !!sym(col))

  d <- p_aggregate_products(d, sql_con = sql_con)

  d <- d %>%
    group_by(!!sym("section_name"), !!sym("commodity_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup()

  d <- d %>%
    select(!!sym("section_name"), !!sym("commodity_name"), !!sym("trade_value")) %>%
    group_by(!!sym("section_name")) %>%
    mutate(sum_trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup() %>%
    arrange(-!!sym("sum_trade_value")) %>%
    select(-!!sym("sum_trade_value"))

  d <- map_df(
    d %>%
      select(!!sym("section_name")) %>%
      distinct() %>%
      pull(),
    function(s) {
      d %>%
        filter(!!sym("section_name") == s) %>%
        arrange(-!!sym("trade_value"))
    }
  )

  return(d)
}

#' Aggregate Products (For Highcharter Visuals)
#' @importFrom dplyr collect filter full_join group_by left_join mutate select
#'     summarise tbl ungroup
#' @noRd
p_aggregate_products <- function(d, sql_con) {
  d %>%
    full_join(
      tbl(sql_con, "commodities") %>%
        select(
          !!sym("commodity_code"), !!sym("section_code"),
          section_name = !!sym("section_fullname_english")
        ) %>%
        filter(nchar(!!sym("commodity_code")) == 6) %>%
        collect()
    ) %>%

    mutate(commodity_code = substr(!!sym("commodity_code"), 1, 4)) %>%
    group_by(!!sym("commodity_code"), !!sym("section_code"),
             !!sym("section_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup() %>%

    left_join(
      tbl(sql_con, "commodities_short") %>%
        select(!!sym("commodity_code"),
               commodity_name = !!sym("commodity_fullname_english")) %>%
        collect()
    )
}

#' Colorize Products (For Highcharter Visuals)
#' @importFrom dplyr collect distinct inner_join select tbl
#' @noRd
p_colors <- function(d, sql_con) {
  d %>%
    select(!!sym("section_name")) %>%
    distinct() %>%

    inner_join(
      tbl(sql_con, "sections") %>%
        select(section_name = !!sym("section_fullname_english"),
               !!sym("section_code")) %>%
        collect() %>%

        inner_join(
          tbl(sql_con, "sections_colors") %>%
            collect()
        ) %>%
        select(-!!sym("section_code"))
    )
}

#' Product to Highcharter (For Highcharter Visuals)
#' @importFrom dplyr collect distinct inner_join mutate_if select tbl
#' @noRd
p_to_highcharts <- function(d, d2) {
  dd <- d %>%
    mutate(section_name = factor(!!sym("section_name"), levels = d2$section_name)) %>%
    arrange(!!sym("section_name"))

  new_lvls <- dd %>%
    group_by(!!sym("section_name"), !!sym("commodity_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE),
              .groups = "drop") %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>%
    arrange(desc(!!sym("trade_value"))) %>%
    distinct(!!sym("section_name")) %>%
    pull(!!sym("section_name"))

  new_colors <- d2 %>%
    mutate(section_name = factor(!!sym("section_name"), levels = new_lvls)) %>%
    arrange(!!sym("section_name")) %>%
    pull(!!sym("section_color"))

  els <- data_to_hierarchical(dd, c("section_name", "commodity_name"), "trade_value",
                              colors = new_colors)

  lopts <- getOption("highcharter.lang")
  lopts$thousandsSep <- ","
  options(highcharter.lang = lopts)

  hchart(
    els,
    type = "treemap",
    levelIsConstant = FALSE,
    allowDrillToNode = TRUE,
    levels = lvl_opts,
    tooltip = list(
      pointFormatter = custom_tooltip()
    ),
    dataLabels = list(
      formatter = data_labels()
    )
  )
}

# FORMAT TEXTS ----

#' Format for Dollars
#' @noRd
show_dollars <- function(x) {
  ifelse(x %/% 10e8 >= 1,
         paste0(round(x / 10e8, 2), "B"),
         paste0(round(x / 10e5, 2), "M")
  )
}

#' Format for Percentages
#' @noRd
show_percentage <- function(x) {
  paste0(round(100 * x, 2), "%")
}

#' Compute Compound Annualized Growth Rate
#' @noRd
growth_rate <- function(p, q, t) {
  (p / q)^(1 / (max(t) - min(t))) - 1
}
