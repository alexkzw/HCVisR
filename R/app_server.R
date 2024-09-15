#' Main Server Function for Shiny App
#'
#' This function handles the server-side logic of the Shiny app, coordinating
#' reactive elements and module server functions.
#'
#' @param input Shiny server input
#' @param output Shiny server output
#' @param session User session information
#' @import shiny
#' @export
app_server <- function(input, output, session) {
    # Reactive list to store available time series
    available_series <- reactiveVal(list())

    # Initialise counters for each type of series
    series_counters <- reactiveVal(list(WN = 0, AR = 0, ARMA = 0, logistic_map = 0,
                                        henon_map = 0, white = 0, pink = 0, red = 0))

    # Automatically select the latest generated series
    selected_series <- reactiveVal(NULL)

    # Reactive to track the selected embedding dimension
    embedding_dimension <- reactive({
        as.numeric(input$embedding_dimension)
    })

    # Call the time series generation module
    mod_time_series_server("timeseries", available_series, series_counters)

    # Call the file upload module
    mod_file_upload_server("file_upload", available_series)

    # Call the available time series selection module
    selected_series_data <- mod_time_series_selection_server("available_series",
                                                             available_series,
                                                             selected_series)

    # Call the plot module for both time series plot and H x C plane plot
    mod_plot_server("plot", selected_series_data, embedding_dimension)
}
