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
            # Simulate the UI for selecting a single time series
            session$setInputs(selected_series = "AR")
            session$flushReact()

            # Check if the series_selector UI is rendered properly
            expect_true(!is.null(output$series_selector))

            # Since only one series is selected, operator UI should not be shown
            expect_true(is.null(output$operators_ui))

            # Simulate selecting two series
            session$setInputs(selected_series = c("AR", "WN"))
            session$flushReact()

            # Operator UI should now appear
            expect_true(!is.null(output$operators_ui))

            # Simulate selecting the operator and setting an alpha value
            session$setInputs(operator = "+")
            session$setInputs(alpha = 0.5)
            session$flushReact()

            # Check that the combined series has data and is properly labeled
            combined <- selected_series_data()
            expect_true(length(combined$series) > 0)
            expect_equal(combined$model, "+ combination of AR, WN")
        }
    )
})

test_that("Rcpp operations work correctly for time series addition and multiplication", {
    shiny::testServer(
        mod_time_series_selection_server,
        args = list(
            available_series = reactiveVal(list(
                list(series = rnorm(100), model = "AR"),
                list(series = rnorm(100), model = "WN")
            )),
            selected_series = reactiveVal(c("AR", "WN"))
        ),
        {
            # Simulate selecting two series and applying addition operator
            session$setInputs(selected_series = c("AR", "WN"))
            session$setInputs(operator = "add")
            session$setInputs(alpha = 0.5)
            session$flushReact()

            combined <- selected_series_data()
            expect_true(length(combined$series) == 100)
            expect_equal(combined$model, "add combination of AR, WN")

            # Simulate applying multiplication operator
            session$setInputs(operator = "multiply")
            session$flushReact()

            combined <- selected_series_data()
            expect_true(length(combined$series) == 100)
            expect_equal(combined$model, "multiply combination of AR, WN")
        }
    )
})
