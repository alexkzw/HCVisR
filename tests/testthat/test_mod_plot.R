test_that("Plots are generated correctly", {
    shiny::testServer(
        mod_plot_server,
        args = list(
            selected_series_data = reactiveVal(list(series = rnorm(100), model = "WN")),
            embedding_dimension = reactiveVal(3)
        ),
        {
            # Trigger reactive updates
            session$flushReact()

            # Test that plot_ts is created without errors
            expect_error(output$plot_ts, NA)

            # Test that plot_hc is created without errors
            expect_error(output$plot_hc, NA)
        }
    )
})
