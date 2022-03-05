lvl_opts <-  list(
  list(
    level = 1,
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
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = FALSE),
    style = list(
      fontSize = "12px",
      textOutline = FALSE
    )
  )
)