#' This function launch the Shiny App
#' @return A Shiny app
#' @export
runApp <- function() {
  app <- system.file("App", package = "bis620.2023")
  shiny::runApp(app)
}
