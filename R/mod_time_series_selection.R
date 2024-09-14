# Handling time series selection and operator logic

# UI for selecting time series
mod_time_series_selection_ui <- function(id) {
    ns <- NS(id)
    tagList(
        br(),  # Add spacing above the checkbox buttons
        uiOutput(ns("series_selector")),
        br(),  # Spacing between the series selector and operator UI
        uiOutput(ns("operators_ui"))  # Operator selection UI
    )
}

# Time series selection server with operator selection
mod_time_series_selection_server <- function(id, available_series, selected_series) {
    moduleServer(id, function(input, output, session) {

        # UI for selecting multiple time series
        output$series_selector <- renderUI({
            ts_list <- available_series()
            if (length(ts_list) == 0) return(NULL)

            checkboxGroupInput(session$ns("selected_series"),
                               "Available Time Series:",
                               choices = lapply(ts_list, function(ts) ts$model),
                               selected = selected_series())  # Set the selected series
        })

        # Operator UI that appears when more than one series is selected
        output$operators_ui <- renderUI({
            req(input$selected_series)
            if (length(input$selected_series) > 1) {
                tagList(
                    h4("Operators"),
                    radioButtons(session$ns("operator"), "Choose Operator:",
                                 choices = c("NULL", "+", "*"), selected = "NULL"),
                    sliderInput(session$ns("alpha"), "Alpha (for + and * operations)", min = 0, max = 1, value = 0.5)
                )
            }
        })

        # Reactive function to apply the operation if needed
        selected_series_data <- reactive({
            # Ensure the selected series is available
            selected_series <- input$selected_series
            if (is.null(selected_series) || length(selected_series) < 1) return(NULL)

            # Get the selected time series data
            ts_list <- available_series()
            selected_ts_data <- ts_list[sapply(ts_list, function(ts) ts$model) %in% selected_series]

            # If only one series is selected or operator is "NULL", return the first series
            if (length(selected_ts_data) == 1 || input$operator == "NULL") {
                return(list(series = selected_ts_data[[1]]$series, model = selected_ts_data[[1]]$model))
            }

            # Ensure the operator and alpha are valid
            req(input$operator)
            req(input$alpha)

            alpha <- input$alpha
            combined_series <- selected_ts_data[[1]]$series
            model_name <- paste(input$operator, "combination of", paste(input$selected_series, collapse = ", "))

            # Apply the selected operator
            for (i in 2:length(selected_ts_data)) {
                next_series <- selected_ts_data[[i]]$series

                if (input$operator == "+") {
                    combined_series <- alpha * combined_series + (1 - alpha) * next_series
                } else if (input$operator == "*") {
                    combined_series <- alpha * combined_series * (1 - alpha) * next_series
                }
            }

            return(list(series = combined_series, model = model_name))
        })

        return(selected_series_data)
    })
}
