test_that("app_server works as expected", {
    shiny::testServer(app_server, {
        # Check that available_series is initially empty
        expect_equal(available_series(), list())

        # Test default embedding dimension
        session$setInputs(embedding_dimension = 3)
        expect_equal(embedding_dimension(), 3)
    })
})
