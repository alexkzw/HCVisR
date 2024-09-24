test_that("Time series generation works correctly", {
    shiny::testServer(
        mod_time_series_server,
        args = list(
            available_series = reactiveVal(list()),
            series_counters = reactiveVal(list(WN = 0, `AR(1)` = 0, ARMA = 0, logistic = 0, henon = 0))
        ),
        {
            # Test Stochastic time series generation for WN
            session$setInputs(series_type = "Stochastic", stochastic_model = "WN", n_wn = 200)
            session$setInputs(generate = 1)  # Simulate clicking the 'Generate' button
            session$flushReact()

            # Ensure available_series is updated
            expect_true(length(available_series()) > 0)

            # Test Stochastic time series generation for AR(1)
            session$setInputs(series_type = "Stochastic", stochastic_model = "AR(1)", n_ar = 200)
            session$setInputs(generate = 1)
            session$flushReact()

            expect_true(length(available_series()) > 1)

            # Test Deterministic time series generation for logistic map
            session$setInputs(series_type = "Deterministic", deterministic_model = "logistic", n_logistic = 300, r_logistic = 4)
            session$setInputs(generate = 1)
            session$flushReact()

            expect_true(length(available_series()) > 2)

            # Test Deterministic time series generation for Henon map
            session$setInputs(series_type = "Deterministic", deterministic_model = "henon", n_henon = 300, a_henon = 1.4, b_henon = 0.3)
            session$setInputs(generate = 1)
            session$flushReact()

            expect_true(length(available_series()) > 3)

            # Ensure that series_counters are updating properly
            counters <- series_counters()
            expect_equal(counters$WN, 1)
            expect_equal(counters$`AR(1)`, 1)  # Use the correct key for AR(1)
            expect_equal(counters$logistic, 1)
            expect_equal(counters$henon, 1)
        }
    )
})
