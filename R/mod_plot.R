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
        plotOutput(ns("plot_ts")),
        plotOutput(ns("plot_hc"))
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

        output$plot_ts <- renderPlot({
            ts_data <- selected_series_data()
            req(ts_data)  # Ensure ts_data is not NULL

            # Wrap the data back into a TimeSeries object if it's not already one
            ts_obj <- new_timeseries(series = ts_data$series, model = ts_data$model)

            # Now use the plot method for the TimeSeries object
            plot(ts_obj)
        })

        # H x C plane plot with user-selected embedding dimension
        output$plot_hc <- renderPlot({
            ts_data <- selected_series_data()
            req(ts_data)  # Ensure ts_data is not NULL

            emb <- embedding_dimension()  # Get the selected embedding dimension
            H_value <- HShannon(OPprob(ts_data$series, emb = emb))
            C_value <- StatComplexity(OPprob(ts_data$series, emb = emb))

            ggplot(subset(StatOrdPattHxC::LinfLsup, Side == "Lower" & Dimension == emb), aes(x = H, y = C, col = Dimension, group = Dimension)) +
                geom_line() +
                geom_line(data = subset(StatOrdPattHxC::LinfLsup, Side == "Upper" & Dimension == emb), aes(x = H, y = C, col = Dimension, group = Dimension)) +
                geom_point(aes(x = H_value, y = C_value), color = "blue", size = 3) +
                geom_text(aes(x = H_value, y = C_value, label = paste0("H: ", round(H_value, 3), "\nC: ", round(C_value, 3))),
                          vjust = -1.5, color = "black") +  # Add text labels above the point
                xlab(expression(italic(H))) +
                ylab(expression(italic(C))) +
                theme_minimal() +
                theme(legend.position = "none") +
                ggtitle(paste("H x C Plane Visualisation (Embedding Dimension:", emb, ")"))
        })
    })
}
