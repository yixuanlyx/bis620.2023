#' This function launch the Shiny App
#' @return A Shiny app
#' @export
runapp <- function() {
  app <- system.file("App", package = "bis620.2023")
  shiny::runapp(app)
}
