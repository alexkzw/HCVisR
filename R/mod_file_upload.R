# File upload UI module
mod_file_upload_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        actionButton(ns("process_file"), "Process File"),
        br(), br()  # Add spacing below the button
    )
}

mod_file_upload_server <- function(id, available_series) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$process_file, {
            # Limit to 5 time series
            if (length(available_series()) >= 5) {
                showNotification("You can only generate or upload up to 5 time series.", type = "error")
                return()
            }
            req(input$file)
            tryCatch({
                file_data <- read.csv(input$file$datapath, header = TRUE)
                time_series_column <- file_data[[1]]  # Assume the first column contains the time series
                ts_obj <- new_timeseries(series = time_series_column, model = input$file$name)  # Use filename as model name
                available_series(c(available_series(), list(ts_obj)))  # Add to available series
            }, error = function(e) {
                showNotification("Error in processing the file. Ensure it's a valid CSV with numeric data in the first column.", type = "error")
            })
        })
    })
}
