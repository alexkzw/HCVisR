test_that("Time series generation UI works", {
    ui <- mod_time_series_ui("test")
    expect_is(ui, "shiny.tag.list")
})

test_that("mod_time_series_server generates correct series", {
    shiny::testServer(mod_time_series_server, {
        session$setInputs(series_type = "Stochastic")
        session$setInputs(stochastic_model = "AR(1)")
        session$setInputs(n_ar = 100)
        session$setInputs(generate = 1)

        # Check the series is added to available_series
        expect_length(available_series(), 1)
    })
})
