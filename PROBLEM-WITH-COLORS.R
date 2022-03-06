library(highcharter)

d <- readRDS("d.rds")
d2 <- readRDS("d2.rds")

lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.250),
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
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = FALSE),
    style = list(
      fontSize = "12px",
      textOutline = FALSE
    )
  )
)

hchart(
  data_to_hierarchical(d, c(continent_name, partner_name), trade_value, color = d2$country_color),
  type = "treemap",
  levelIsConstant = FALSE,
  allowDrillToNode = TRUE,
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE)
)