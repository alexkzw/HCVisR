#' UI Setup for Shiny App
#'
#' This function sets up the user interface for the Shiny app, arranging inputs
#' and outputs in a sidebar layout.
#'
#' @param request Internal parameter for `{shiny}`. Do not remove.
#' @import shiny
#' @export
app_ui <- function(request) {
    fluidPage(
        titlePanel("Time Series Generation and H x C Visualisation"),

        sidebarLayout(
            sidebarPanel(
                mod_time_series_ui("timeseries"),
                mod_file_upload_ui("file_upload"),
                mod_time_series_selection_ui("available_series"),
                selectInput("embedding_dimension", "Select Embedding Dimension:",
                            choices = 3:6, selected = 3)
            ),
            mainPanel(
                mod_plot_ui("plot")
            )
        )
    )
}
