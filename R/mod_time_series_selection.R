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
#' @import shinyjs
#' @export
mod_time_series_selection_server <- function(id, available_series, selected_series) {
    moduleServer(id, function(input, output, session) {
        shinyjs::useShinyjs()

        # UI for selecting multiple time series (up to five)
        output$series_selector <- renderUI({
            ts_list <- available_series()
            if (length(ts_list) == 0) return(NULL)

            # Get all series model names
            series_choices <- lapply(ts_list, function(ts) ts$model)

            # Render the checkbox group for time series selection
            checkboxGroupInput(session$ns("selected_series"),
                               "Available Time Series:",
                               choices = series_choices,
                               selected = selected_series())  # Set the selected series
        })

        # Observe the selection of time series and control operator visibility
        observe({
            selected <- input$selected_series

            # Disable the operator if more than two series are selected
            if (length(selected) > 2) {
                shinyjs::disable("operator")
                updateRadioButtons(session, "operator", selected = "NULL")  # Revert to NULL
            } else if (length(selected) == 2) {
                shinyjs::enable("operator")
            } else {
                shinyjs::disable("operator")
                updateRadioButtons(session, "operator", selected = "NULL")  # Revert to NULL
            }
        })

        # Operator UI: Show only if exactly 2 series are selected
        output$operators_ui <- renderUI({
            req(isTruthy(input$selected_series))

            if (length(input$selected_series) == 2) {
                tagList(
                    h4("Operators"),
                    radioButtons(session$ns("operator"), "Choose Operator:",
                                 choices = c("NULL", "+", "*"), selected = "NULL",
                                 inline = TRUE),
                    conditionalPanel(
                        condition = paste0("input['", session$ns("operator"), "'] == '+'"),
                        sliderInput(session$ns("alpha"), "Alpha (for addition operations only)", min = 0, max = 1, value = 0.5)
                    )
                )
            }
        })

        # Reactive function to accumulate the selections and apply the operation if needed
        selected_series_data <- reactive({
            req(isTruthy(input$selected_series))  # Ensure selected series exist and are valid
            selected_series <- input$selected_series
            if (is.null(selected_series) || length(selected_series) < 1) return(NULL)

            ts_list <- available_series()
            selected_ts_data <- ts_list[sapply(ts_list, function(ts) ts$model) %in% selected_series]

            # Ensure we have valid data before proceeding
            req(isTruthy(selected_ts_data))
            if (length(selected_ts_data) == 0) return(NULL)

            # Check if there's only one series selected or if 'NULL' operator is selected
            if (length(selected_ts_data) == 1 || input$operator == "NULL" || !isTruthy(input$operator)) {
                # Return each individual selected series separately when no operator is applied
                return(selected_ts_data)
            }

            # If exactly 2 series are selected and an operator is chosen, combine them
            if (length(selected_ts_data) == 2 && isTruthy(input$operator) && input$operator != "NULL") {
                req(input$operator)

                # Perform the combination based on the selected operator
                if (input$operator == "+") {
                    req(isTruthy(input$alpha))
                    combined_series_values <- selected_ts_data[[1]]$series * (1 - input$alpha) +
                        selected_ts_data[[2]]$series * input$alpha
                } else if (input$operator == "*") {
                    combined_series_values <- selected_ts_data[[1]]$series * selected_ts_data[[2]]$series
                }

                # Create the combined series and return as a list with only one entry
                combined_series <- list(
                    series = combined_series_values,
                    model = paste(selected_ts_data[[1]]$model, input$operator, selected_ts_data[[2]]$model)
                )

                return(list(combined_series))  # Return the combined series in a list
            }

            # Return the individually selected series data if no combination is applied
            return(selected_ts_data)
        })

        return(selected_series_data)
    })
}
