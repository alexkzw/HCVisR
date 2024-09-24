test_that("Time series selection works correctly", {
    shiny::testServer(
        mod_time_series_selection_server,
        args = list(
            available_series = reactiveVal(list(
                list(series = rnorm(100), model = "AR"),
                list(series = rnorm(100), model = "WN")
            )),
            selected_series = reactiveVal("AR")
        ),
        {
            # Simulate the UI for selecting a time series
            session$setInputs(selected_series = "AR")
            session$flushReact()

            # Check if the series_selector UI is rendered properly
            expect_true(!is.null(output$series_selector))

            # Check if a single series is selected and available
            selected <- selected_series_data()
            expect_equal(selected$model, "AR")

            # Simulate selecting more than one series and setting an operator
            session$setInputs(selected_series = c("AR", "WN"))
            session$setInputs(operator = "+")
            session$setInputs(alpha = 0.5)

            session$flushReact()  # Make sure reactivity happens

            # Check if the operator UI is rendered when more than one series is selected
            expect_true(!is.null(output$operators_ui))

            # Check that the combined series has data
            combined <- selected_series_data()
            expect_true(length(combined$series) > 0)
            expect_true(!is.null(combined$model))
        }
    )
})
