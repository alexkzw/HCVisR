#' UI Setup for Shiny App
#'
#' This function sets up the user interface for the Shiny app, arranging inputs
#' and outputs with a fixed resolution and responsive layout.
#'
#' @param request Internal parameter for `{shiny}`. Do not remove.
#' @import shiny
#' @export
app_ui <- function(request) {
    fluidPage(
        # Set a max-width for the entire app to control resolution
        tags$head(
            tags$style(HTML("
                .container-fluid {
                    max-width: 1200px;
                    margin: 0 auto;
                }
                @media only screen and (max-width: 768px) {
                    .container-fluid {
                        max-width: 100%;
                    }
                }
            "))
        ),

        titlePanel("Time Series Generation and H x C Visualisation"),

        # Using fluidRow and column for responsive design
        sidebarLayout(
            sidebarPanel(
                fluidRow(
                    column(12, mod_time_series_ui("timeseries")),
                    column(12, mod_file_upload_ui("file_upload")),
                    column(12, mod_time_series_selection_ui("available_series")),
                    column(12, selectInput("embedding_dimension",
                                           "Select Embedding Dimension:",
                                           choices = 3:6, selected = 3))
                ),
                width = 4  # Setting the width of the sidebar panel (responsive)
            ),

            mainPanel(
                fluidRow(
                    column(12, mod_plot_ui("plot"))
                ),
                width = 8  # Set width of main panel (responsive)
            )
        )
    )
}
