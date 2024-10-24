#' Plotting Module UI
#'
#' Constructs UI elements for plotting time series data.
#'
#' @param id A namespace identifier for the plot elements
#' @import shiny
#' @export
mod_plot_ui <- function(id) {
    ns <- NS(id)
    tagList(
        plotOutput(ns("plot_ts"), height = "400px", width = "100%"),
        plotlyOutput(ns("plot_hc"), height = "400px", width = "130%")
    )
}

#' Server Logic for Plotting Module
#'
#' Manages server-side operations for generating plots based on user-selected data.
#'
#' @param id A namespace identifier for the plotting operations
#' @param selected_series_data Data selected for plotting
#' @param embedding_dimension The dimension used for embedding plots
#' @import ggplot2
#' @importFrom StatOrdPattHxC HShannon OPprob StatComplexity
#' @export
mod_plot_server <- function(id, selected_series_data, embedding_dimension) {
    moduleServer(id, function(input, output, session) {

        # Ensure selected_series_data always returns a list of selected series
        series_data_list <- reactive({
            series_data <- selected_series_data()
            if (is.null(series_data)) return(NULL)
            return(series_data)  # No need to wrap single series now since it will return a list
        })

        # Time series plot
        output$plot_ts <- renderPlot({
            series_list <- series_data_list()  # Process multiple series
            req(series_list)

            # Initialize ggplot object
            p <- ggplot()

            # Add each time series as a separate geom_line layer
            for (i in seq_along(series_list)) {
                ts_data <- series_list[[i]]
                p <- p + geom_line(aes(x = seq_along(ts_data$series), y = ts_data$series),
                                   color = "blue", linewidth = 1) +  # Use 'linewidth' for lines
                    labs(title = paste("Time Series Plot:", ts_data$model), x = "Time", y = "Value")
            }

            p + theme_minimal()
        })

        # H × C plane plot with user-selected embedding dimension
        output$plot_hc <- renderPlotly({
            series_list <- series_data_list()
            req(series_list)

            emb <- embedding_dimension()
            validate(
                need(emb >= 3 && emb <= 6, "Embedding dimension must be between 3 and 6.")
            )

            hc_data <- data.frame(H = numeric(), C = numeric(), model = character(), Dimension = integer())

            # Calculate H and C for each selected time series
            for (i in seq_along(series_list)) {
                ts_data <- series_list[[i]]
                H_value <- HShannon(OPprob(ts_data$series, emb = emb))
                C_value <- StatComplexity(OPprob(ts_data$series, emb = emb))

                hc_data <- rbind(hc_data, data.frame(H = H_value, C = C_value, model = ts_data$model, Dimension = emb))
            }

            # Subset the LinfLsup data for plotting the boundaries
            linf_data_lower <- subset(StatOrdPattHxC::LinfLsup, Side == "Lower" & Dimension == emb)
            linf_data_upper <- subset(StatOrdPattHxC::LinfLsup, Side == "Upper" & Dimension == emb)

            # Build the plot using plot_ly
            plot_ly() %>%
                add_lines(data = linf_data_lower, x = ~H, y = ~C, line = list(color = 'red'), hoverinfo = 'skip', showlegend = FALSE) %>%
                add_lines(data = linf_data_upper, x = ~H, y = ~C, line = list(color = 'red'), hoverinfo = 'skip', showlegend = FALSE) %>%
                add_markers(data = hc_data, x = ~H, y = ~C, color = ~model, colors = "Set2", size = 3,
                            hovertemplate = paste(
                                'H: %{x:.3f}<br>',
                                'C: %{y:.3f}<br>',
                                '<extra></extra>'  # Removed the %{text}
                            )) %>%
                layout(
                    title = paste("H x C Plane Visualisation (Embedding Dimension:", emb, ")"),
                    xaxis = list(title = "H"),
                    yaxis = list(title = "C"),
                    showlegend = TRUE
                )
        })
    })
}

