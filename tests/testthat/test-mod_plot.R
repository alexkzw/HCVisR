test_that("Plot UI works", {
    ui <- mod_plot_ui("test")
    expect_is(ui, "shiny.tag.list")
    expect_true("plotOutput" %in% names(ui[[1]]))
})

test_that("mod_plot_server renders the plot correctly", {
    shiny::testServer(mod_plot_server, {
        ts_data <- list(series = 1:100, model = "Test Model")
        selected_series_data <- reactiveVal(ts_data)

        session$setInputs(embedding_dimension = 3)

        # Check plot rendering
        expect_true(inherits(output$plot_ts(), "gg"))
        expect_true(inherits(output$plot_hc(), "gg"))
    })
})
