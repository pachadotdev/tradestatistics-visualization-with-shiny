#' Run the Shiny Application
#'
#' @export
#' @importFrom golem with_golem_options
run_app <- function(...) {
  # Render ------------------------------------------------------------------
  available_years <- get_available_years()
  available_reporters_iso <- get_available_reporters_iso()
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list(
      available_years = min(available_years) : max(available_years), 
      available_reporters_iso = available_reporters_iso
    )
  )
}
