test_that("app_server loads and handles inputs", {
    shiny::testServer(app_server, {
        session$setInputs(embedding_dimension = 3)

        # Check initial reactive values
        expect_equal(series_counters(), list(WN = 0, AR = 0, ARMA = 0, logistic_map = 0, henon_map = 0, white = 0, pink = 0, red = 0))

        # Simulate module calls and inputs
        session$setInputs(mod_time_series = "logistic_map")
        expect_true(is.null(selected_series()))
    })
})
