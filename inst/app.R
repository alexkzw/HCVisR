library(shiny)
library(shiny)
library(ggplot2)
library(statcomp)
library(StatOrdPattHxC)
library(tuneR)

# During development, manually source the scripts from the R/ folder.
source("R/app_ui.R")
source("R/app_server.R")
source("R/time_series_classes.R")
source("R/mod_time_series.R")
source("R/mod_file_upload.R")
source("R/mod_time_series_selection.R")
source("R/mod_plot.R")

# Once you've sourced all your files, you can run the Shiny app.
shinyApp(ui = app_ui, server = app_server)
