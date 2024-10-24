test_that("H x C plot is generated for multiple embedding dimensions with multiple time series", {
    shiny::testServer(
        mod_plot_server,
        args = list(
            selected_series_data = reactiveVal(
                list(
                    list(series = rnorm(100), model = "WN"),
                    list(series = rnorm(100), model = "AR(1)")
                )
            ),
            embedding_dimension = reactiveVal(3)
        ),
        {
            session$flushReact()
            expect_error(output$plot_hc, NA)

            # Iterate over multiple embedding dimensions and check for errors
            for (emb_dim in 3:6) {
                embedding_dimension(emb_dim)
                session$flushReact()
                expect_error(output$plot_hc, NA)
            }
        }
    )
})

test_that("Multiple series are plotted correctly", {
    shiny::testServer(
        mod_plot_server,
        args = list(
            selected_series_data = reactiveVal(
                list(
                    list(series = rnorm(100), model = "WN"),
                    list(series = rnorm(100), model = "AR(1)")
                )
            ),
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
