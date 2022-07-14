#' @keywords internal
"_PACKAGE"

styles <- list(
  skin_color = "blue",
  css_files = c("css/AdminLTE.min.css",
                "css/_all-skins.min.css",
                "css/custom.min.css",
                "css/ion.rangeSlider.min.css")
)

shiny::shinyOptions(
  cache = cachem::cache_disk(
    dir = "/tradestatistics/countryprofile_cache"
    # logfile = "/tradestatistics/log/countryprofile_cache.log"
  )
)
