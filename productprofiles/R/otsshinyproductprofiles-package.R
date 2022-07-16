#' @keywords internal
"_PACKAGE"

styles <- list(
  skin_color = "blue",
  css_files = "custom.css"
)

shiny::shinyOptions(
  cache = cachem::cache_disk(
    dir = "/tradestatistics/productprofile_cache"
    # logfile = "/tradestatistics/log/productprofile_cache.log"
  )
)
