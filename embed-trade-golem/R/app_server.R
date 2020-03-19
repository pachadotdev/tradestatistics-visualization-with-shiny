#' server
#' @import shiny
#' @import shinyjs
#' @importFrom magrittr %>%
#' @importFrom dplyr filter arrange select rename tibble
#' @importFrom jsonlite fromJSON
#' @importFrom purrr as_vector
#' @importFrom stringr str_sub str_length
#' @importFrom glue glue
#' @importFrom tidyr gather
#' @importFrom tradestatistics ots_create_tidy_data
#' @importFrom highcharter hchart hcaes hc_colors hc_title hc_exporting renderHighchart JS
app_server <- function(input, output, session) {
  # Tables ------------------------------------------------------------------

  countries <- tradestatistics::ots_countries %>%
    select(country_iso, country_name_english)

  products <- tradestatistics::ots_products %>%
    filter(str_length(product_code) %in% c(2, 4)) %>%
    arrange(product_code)

  communities <- tradestatistics::ots_communities

  # Choices -----------------------------------------------------------------

  # choices trick by Andrea Gao
  # http://gytcrt.github.io/gytcrt.github.io/2016/08/11/RShiny-easily-passing-a-long-list-of-items-to-selectInput-choices/

  available_tables <- as.list(c("select", "yr", "yrp"))
  names(available_tables) <- c("Select", "Multilateral trade", "Bilateral trade")

  available_years <- get_available_years()

  # updateSelectInput(
  #   session,
  #   "y1",
  #   choices = available_years
  # )
  # updateSelectInput(
  #   session,
  #   "y2",
  #   choices = available_years
  # )

  available_years_min <- min(available_years)
  available_years_max <- max(available_years)

  available_reporters_iso <- get_available_reporters_iso()
  # updateSelectInput(
  #   session,
  #   "p",
  #   choices = c("Select", available_reporters_iso)
  # )
  # updateSelectInput(
  #   session,
  #   "r",
  #   choices = c("Select", available_reporters_iso)
  # )

  available_reporters_iso <- c("all", available_reporters_iso[grep("^c-|all", available_reporters_iso, invert = T)])
  names(available_reporters_iso) <- c("the World", as.vector(countries$country_name_english[grep("^Alias", countries$country_name_english, invert = T)]))

  reporters_to_display <- tibble(
    available_reporters_iso = as_vector(available_reporters_iso),
    available_reporters_names = names(available_reporters_iso)
  )

  # Bookmarking -------------------------------------------------------------

  enableBookmarking(store = "url")

  # Highcharts --------------------------------------------------------------

  hc_export_menu <- list(
    list(
      text = "Download PNG image",
      onclick = JS("function () { 
                  this.exportChart({ type: 'image/png' }); }")
    ),
    list(
      text = "Download JPEG image",
      onclick = JS("function () { 
                  this.exportChart({ type: 'image/jpeg' }); }")
    ),
    list(
      text = "Download SVG vector image",
      onclick = JS("function () { 
                  this.exportChart({ type: 'image/svg+xml' }); }")
    ),
    list(
      text = "Download PDF document",
      onclick = JS("function () { 
                  this.exportChart({ type: 'application/pdf' }); }")
    )
  )

  # Input -------------------------------------------------------------------

  y1 <- reactive({
    input$y1
  })

  y2 <- reactive({
    input$y2
  })

  y <- reactive({
    input$y1:input$y2
  })

  r_iso <- reactive({
    input$r
  })

  r_name <- reactive({
    reporters_to_display %>%
      filter(available_reporters_iso == input$r) %>%
      select(available_reporters_names) %>%
      as.character()
  })

  p_iso <- reactive({
    input$p
  })

  p_name <- reactive({
    reporters_to_display %>%
      filter(available_reporters_iso == input$p) %>%
      select(available_reporters_names) %>%
      as.character()
  })

  table_aggregated <- reactive({
    if (p_iso() == "all") {
      "yr"
    } else {
      "yrp"
    }
  })

  table_detailed <- reactive({
    if (p_iso() == "all") {
      "yrc"
    } else {
      "yrpc"
    }
  })

  # Data --------------------------------------------------------------------

  text_add_the <- reactive({
    if (str_sub(p_name(), 1, 6) == "United" | str_sub(p_name(), 1, 3) == "USA") {
      "the "
    } else {
      ""
    }
  })

  title <- eventReactive(input$go, {
    glue("Trade between { text_add_the() } { r_name() } and { text_add_the() } { p_name() } from { y1() } to { y2() }, aggregated")
  })

  data_aggregated <- eventReactive(input$go, {
    ots_create_tidy_data(
      years = y(),
      reporters = r_iso(),
      partners = p_iso(),
      include_shortnames = FALSE,
      include_communities = FALSE,
      table = table_aggregated()
    )
  })

  trade_table_aggregated <- eventReactive(input$go, {
    data_aggregated() %>%
      select(year, export_value_usd, import_value_usd)
  })

  trade_exchange_bars_title <- eventReactive(input$go, {
    switch(table_aggregated(),
      "yr" = glue("{ r_name() } multilateral trade between { min(y()) } and { max(y()) }"),
      "yrp" = glue("{ r_name() } and { p_name() } exchange between { min(y()) } and { max(y()) }")
    )
  })

  trade_bars_aggregated <- eventReactive(input$go, {
    d <- trade_table_aggregated() %>%
      gather(key, value, -year) %>%
      mutate(
        key = ifelse(key == "export_value_usd", "Exports", "Imports")
      ) %>%
      rename(
        `Trade Value` = value,
        `Year` = year,
        group = key
      )

    hchart(d, "column", hcaes(x = `Year`, y = `Trade Value`, group = group)) %>%
      hc_colors(c("#4d6fd0", "#bf3251")) %>%
      hc_title(text = trade_exchange_bars_title()) %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_menu)))
  })

  # Output ------------------------------------------------------------------

  output$trade_bars_aggregated <- renderHighchart({
    trade_bars_aggregated()
  })

  # Bookmarking -------------------------------------------------------------

  observe({
    # Trigger this observer every time an input changes
    # strip shiny related URL parameters
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}
