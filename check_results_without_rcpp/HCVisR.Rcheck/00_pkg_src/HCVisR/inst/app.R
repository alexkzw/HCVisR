library(shiny)
library(ggplot2)
library(statcomp)
library(StatOrdPattHxC)
library(tuneR)

ui <- HCVisR::app_ui()
server <- function(input, output, session) {
    HCVisR::app_server(input, output, session)
}

shiny::shinyApp(ui = ui, server = server)
