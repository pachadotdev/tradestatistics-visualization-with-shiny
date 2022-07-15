#' @keywords internal
"_PACKAGE"

styles <- list(
  skin_color = "blue",
  css_files = "homogeneous_font.css"
)

shiny::shinyOptions(
  cache = cachem::cache_disk(
    dir = "/tradestatistics/countryprofile_cache"
    # logfile = "/tradestatistics/log/countryprofile_cache.log"
  )
)
