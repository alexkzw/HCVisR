test_that("Time series selection UI works", {
    ui <- mod_time_series_selection_ui("test")
    expect_is(ui, "shiny.tag.list")
})

test_that("mod_time_series_selection_server applies operations correctly", {
    shiny::testServer(mod_time_series_selection_server, {
        available_series <- reactiveVal(list(
            list(series = 1:10, model = "TS1"),
            list(series = 11:20, model = "TS2")
        ))

        session$setInputs(selected_series = c("TS1", "TS2"))
        session$setInputs(operator = "+")
        session$setInputs(alpha = 0.5)

        # Test the combined series
        result <- selected_series_data()
        expect_equal(result$model, "+ combination of TS1, TS2")
        expect_equal(result$series, 0.5 * 1:10 + 0.5 * 11:20)
    })
})
