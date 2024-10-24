test_that("Time series generation works correctly with length in model names", {
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

            # Verify that the model name includes the time series length
            wn_series <- available_series()[[1]]
            expect_equal(wn_series$model, "WN(N=200)")

            # Test Stochastic time series generation for AR(1)
            session$setInputs(series_type = "Stochastic", stochastic_model = "AR(1)", n_ar = 200)
            session$setInputs(generate = 1)
            session$flushReact()

            expect_true(length(available_series()) > 1)

            # Verify that the model name includes the time series length
            ar1_series <- available_series()[[2]]
            expect_equal(ar1_series$model, "AR(1)(N=200)")

            # Test Deterministic time series generation for logistic map
            session$setInputs(series_type = "Deterministic", deterministic_model = "logistic", n_logistic = 300, r_logistic = 4)
            session$setInputs(generate = 1)
            session$flushReact()

            expect_true(length(available_series()) > 2)

            # Verify that the model name includes the time series length
            logistic_series <- available_series()[[3]]
            expect_equal(logistic_series$model, "logistic(N=300)")

            # Test Deterministic time series generation for Henon map with hardcoded x0 and y0
            session$setInputs(series_type = "Deterministic", deterministic_model = "henon", n_henon = 300, a_henon = 1.4, b_henon = 0.3)
            session$setInputs(generate = 1)
            session$flushReact()

            expect_true(length(available_series()) > 3)

            # Verify that the generated Henon map time series has the correct name including length
            henon_series <- available_series()[[4]]
            expect_equal(henon_series$model, "henon(N=300)")

            # Ensure that series_counters are updating properly
            counters <- series_counters()
            expect_equal(counters$WN, 1)
            expect_equal(counters$`AR(1)`, 1)  # Use the correct key for AR(1)
            expect_equal(counters$logistic, 1)
            expect_equal(counters$henon, 1)
        }
    )
})
