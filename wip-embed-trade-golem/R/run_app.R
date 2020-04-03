#' Run the Shiny Application
#'
#' @export
#' @importFrom golem with_golem_options
run_app <- function(...) {
  # Render ------------------------------------------------------------------
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server),
    golem_opts = list(...)
  )
}
