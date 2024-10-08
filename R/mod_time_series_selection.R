#' Time Series Selection UI Module
#'
#' Creates UI elements for selecting and operating on time series data.
#'
#' @param id A namespace identifier for the selection elements
#' @import shiny
#' @export
mod_time_series_selection_ui <- function(id) {
    ns <- NS(id)
    tagList(
        br(),  # Add spacing above the checkbox buttons
        uiOutput(ns("series_selector")),
        br(),  # Spacing between the series selector and operator UI
        uiOutput(ns("operators_ui"))  # Operator selection UI
    )
}

#' Server Logic for Time Series Selection
#'
#' Manages the selection and operation on time series data from the server side.
#'
#' @param id A namespace identifier for the selection operations
#' @param available_series Reactive list of available series
#' @param selected_series Reactive value of selected series
#' @import shiny
#' @export
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

        # Operator UI that only appears when exactly two series are selected
        output$operators_ui <- renderUI({
            req(input$selected_series)
            if (length(input$selected_series) == 2) {
                tagList(
                    h4("Operators"),
                    radioButtons(session$ns("operator"), "Choose Operator:",
                                 choices = c("+", "*"), selected = "+"),
                    # Conditionally render the alpha slider only when the "+" operator is selected
                    conditionalPanel(
                        condition = paste0("input['", session$ns("operator"), "'] == '+'"),
                        sliderInput(session$ns("alpha"), "Alpha (for addition operations only)", min = 0, max = 1, value = 0.5)
                    )
                )
            }
        })

        # Reactive function to apply the operation if needed
        selected_series_data <- reactive({
            # Ensure that selected series exist and are not NULL
            req(input$selected_series)

            selected_series <- input$selected_series
            if (!isTruthy(selected_series)) return(NULL)

            ts_list <- available_series()
            selected_ts_data <- ts_list[sapply(ts_list, function(ts) ts$model) %in% selected_series]

            # Make sure that we have valid data before proceeding
            req(selected_ts_data)
            if (!isTruthy(selected_ts_data)) return(NULL)

            # Check if there's only one series selected or if 'NULL' operator is selected
            if (length(selected_ts_data) == 1 || !isTruthy(input$operator) || input$operator == "NULL") {
                if (!isTruthy(selected_ts_data[[1]]$series) || !isTruthy(selected_ts_data[[1]]$model)) {
                    return(NULL)  # Return NULL if there is no valid series data
                }
                return(list(series = selected_ts_data[[1]]$series, model = selected_ts_data[[1]]$model))
            }

            # Ensure the operator and alpha are valid if two series are selected
            req(input$operator)

            # Conditionally include alpha only for addition
            if (input$operator == "+") {
                req(input$alpha)
                combined_series <- combine.TimeSeries(selected_ts_data[[1]], selected_ts_data[[2]], method = input$operator, alpha = input$alpha)
            } else {
                combined_series <- combine.TimeSeries(selected_ts_data[[1]], selected_ts_data[[2]], method = input$operator)
            }

            model_name <- paste(input$operator, "combination of", paste(input$selected_series, collapse = ", "))
            return(list(series = combined_series$series, model = model_name))
        })
    })
}

