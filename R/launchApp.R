#' Launch HCVisR Shiny App
#'
#' This function starts the HCVisR Shiny application.
#' @export
launchApp <- function() {
    appDir <- system.file("app", package = "HCVisR")
    if (appDir == "") {
        stop("The HCVisR app is not found in the installed package directory.")
    }
    shiny::runApp(appDir)
}
