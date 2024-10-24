#' Time Series Generation UI Module
#'
#' Provides UI components for configuring the generation of time series.
#'
#' @param id A namespace identifier for the time series elements
#' @import shiny
#' @export
mod_time_series_ui <- function(id) {
    ns <- NS(id)
    tagList(
        selectInput(ns("series_type"), "Choose Series Type:", choices = c("Stochastic", "Deterministic")),

        conditionalPanel(
            condition = paste0("input['", ns("series_type"), "'] == 'Stochastic'"),
            selectInput(ns("stochastic_model"), "Choose Stochastic Model:",
                        choices = c("WN", "AR(1)", "ARMA(1,1)", "Colored Noise")),

            conditionalPanel(
                condition = paste0("input['", ns("stochastic_model"), "'] == 'WN'"),
                numericInput(ns("n_wn"), "Length of Time Series (n):", value = 300, min = 100, max = 1000, step = 100)
            ),

            conditionalPanel(
                condition = paste0("input['", ns("stochastic_model"), "'] == 'AR(1)'"),
                numericInput(ns("n_ar"), "Length of Time Series (n):", value = 300, min = 100, max = 1000, step = 100)
            ),

            conditionalPanel(
                condition = paste0("input['", ns("stochastic_model"), "'] == 'ARMA(1,1)'"),
                numericInput(ns("n_arma"), "Length of Time Series (n):", value = 300, min = 100, max = 1000, step = 100)
            ),
            # Colored noise UI
            conditionalPanel(
                condition = paste0("input['", ns("stochastic_model"), "'] == 'Colored Noise'"),
                selectInput(ns("colored_noise_type"), "Choose Noise Color:", choices = c("white", "pink", "red")),
                numericInput(ns("n_colored_noise"), "Length of Time Series (n):", value = 300, min = 100, max = 1000, step = 100)
            )
        ),
        # Deterministic model selection with new time series options
        conditionalPanel(
            condition = paste0("input['", ns("series_type"), "'] == 'Deterministic'"),
            selectInput(ns("deterministic_model"), "Choose Deterministic Model:",
                        choices = c("logistic", "henon")),

            conditionalPanel(
                condition = paste0("input['", ns("deterministic_model"), "'] == 'logistic'"),
                numericInput(ns("n_logistic"), "Length of Time Series (N):", value = 300, min = 100, max = 1000, step = 100)
            ),

            conditionalPanel(
                condition = paste0("input['", ns("deterministic_model"), "'] == 'henon'"),
                numericInput(ns("n_henon"), "Length of Time Series (N):", value = 300, min = 100, max = 1000, step = 100)
            )
        ),
        actionButton(ns("generate"), "Generate Time Series"),
        br(), br()  # Add spacing below the button
    )
}

#' Server Logic for Time Series Generation
#'
#' Handles the generation of time series based on user inputs.
#'
#' @param id A namespace identifier for the generation logic
#' @param available_series Reactive variable to store the generated series
#' @param series_counters Reactive value tracking the number of each type of series
#' @import shiny
#' @export
mod_time_series_server <- function(id, available_series, series_counters) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$generate, {
            # Limit to 5 time series
            if (length(available_series()) >= 5) {
                showNotification("You can only generate or upload up to 5 time series.", type = "error")
                return()
            }

            selected_series <- reactiveVal(NULL)
            ts_obj <- NULL
            model_name <- NULL
            series_length <- NULL

            # Create a copy of the series_counters to modify
            counters <- series_counters()

            if (input$series_type == "Stochastic") {
                model <- input$stochastic_model
                if (model == "WN") {
                    ts_obj <- new_stochastic_ts(model = model, n = input$n_wn)
                    series_length <- input$n_wn
                } else if (model == "AR(1)") {
                    ts_obj <- new_stochastic_ts(model = "AR", phi = 0.5, n = input$n_ar)
                    series_length <- input$n_ar
                } else if (model == "ARMA(1,1)") {
                    ts_obj <- new_stochastic_ts(model = "ARMA", phi = 0.5, theta = 0.4, n = input$n_arma)
                    series_length <- input$n_arma
                } else if (model == "Colored Noise") {
                    ts_obj <- new_stochastic_ts(model = input$colored_noise_type, n = input$n_colored_noise)
                    series_length <- input$n_colored_noise
                }

                model_name <- paste(model, "(N=", series_length, ")", sep = "")
                counters[[model]] <- counters[[model]] + 1
            } else if (input$series_type == "Deterministic") {
                model <- input$deterministic_model
                if (model == "logistic") {
                    ts_obj <- new_deterministic_ts(model = model, N = input$n_logistic, r = 4)
                    series_length <- input$n_logistic
                } else if (model == "henon") {
                    ts_obj <- new_deterministic_ts(model = model, N = input$n_henon, a = 1.4, b = 0.3,
                                                   x0 = 0.1, y0 = 0.1)
                    series_length <- input$n_henon
                }

                model_name <- paste(model, "(N=", series_length, ")", sep = "")
                counters[[model]] <- counters[[model]] + 1
            }

            # Update the series_counters
            series_counters(counters)

            # Set unique model name with index
            if (!is.null(ts_obj)) {
                ts_obj$model <- model_name
                available_series(c(available_series(), list(ts_obj)))
                selected_series(model_name)  # Automatically select the latest series

                # Call the print and summary methods for diagnostics
                print(ts_obj)
                summary(ts_obj)
            }
        })
    })
}

