# Time series plot module UI
mod_plot_ui <- function(id) {
    ns <- NS(id)
    tagList(
        plotOutput(ns("plot_ts")),
        plotOutput(ns("plot_hc"))
    )
}

# Time series plot module server with dynamic embedding dimension
mod_plot_server <- function(id, selected_series_data, embedding_dimension) {
    moduleServer(id, function(input, output, session) {

        # Time series plot
        output$plot_ts <- renderPlot({
            ts_data <- selected_series_data()

            # Ensure we have valid data to plot
            req(ts_data)

            ggplot(data.frame(x = seq_along(ts_data$series), y = ts_data$series), aes(x = x, y = y)) +
                geom_line(color = "blue") +
                labs(title = ts_data$model, x = "Time", y = "Value") +
                theme_minimal()
        })

        # H x C plane plot with user-selected embedding dimension
        output$plot_hc <- renderPlot({
            ts_data <- selected_series_data()

            # Ensure we have valid data for H x C calculation
            req(ts_data)

            emb <- embedding_dimension()  # Get the selected embedding dimension
            H_value <- HShannon(OPprob(ts_data$series, emb = emb))
            C_value <- StatComplexity(OPprob(ts_data$series, emb = emb))

            ggplot(subset(LinfLsup, Side == "Lower" & Dimension == emb), aes(x = H, y = C, col = Dimension, group = Dimension)) +
                geom_line() +
                geom_line(data = subset(LinfLsup, Side == "Upper" & Dimension == emb), aes(x = H, y = C, col = Dimension, group = Dimension)) +
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
