#' This function launch the Shiny App
#' @return A Shiny app
#' @export
runApp <- function(){
  appDir <- system.file("App", package = "bis620.2023")
  shiny::runApp(appDir)
}
