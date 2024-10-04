test_that("File upload works correctly with example CSV", {
    shiny::testServer(mod_file_upload_server, args = list(available_series = reactiveVal(list())), {

        # Access example CSV file from inst/extdata
        temp_csv <- system.file("extdata", "example_timeseries.csv", package = "HCVisR")

        # Simulate a file upload
        session$setInputs(file = list(datapath = temp_csv, name = "example_timeseries.csv"))
        session$setInputs(process_file = 1)

        # Check that the series was added to available_series
        expect_equal(length(available_series()), 1)

        # Check that the uploaded file is processed correctly
        ts_obj <- available_series()[[1]]
        expect_equal(length(ts_obj$series), 300)
        expect_equal(ts_obj$model, "example_timeseries.csv")
    })
})

test_that("Error handling for non-numeric and multi-column file", {
    shiny::testServer(mod_file_upload_server, args = list(available_series = reactiveVal(list())), {

        # Simulate a file upload with non-numeric values
        temp_csv_non_numeric <- tempfile(fileext = ".csv")
        write.csv(c("a", "b", "c"), temp_csv_non_numeric)

        session$setInputs(file = list(datapath = temp_csv_non_numeric, name = "non_numeric.csv"))
        session$setInputs(process_file = 1)

        # Check that no series was added to available_series
        expect_equal(length(available_series()), 0)

        # Simulate a file upload with multiple columns
        temp_csv_multiple <- tempfile(fileext = ".csv")
        write.csv(data.frame(col1 = c(1, 2, 3), col2 = c(4, 5, 6)), temp_csv_multiple)

        session$setInputs(file = list(datapath = temp_csv_multiple, name = "multiple_columns.csv"))
        session$setInputs(process_file = 1)

        # Ensure no series was added when file has more than one column
        expect_equal(length(available_series()), 0)

        # Clean up temporary files
        unlink(temp_csv_non_numeric)
        unlink(temp_csv_multiple)
    })
})

test_that("File upload limit is enforced", {
    shiny::testServer(mod_file_upload_server, args = list(available_series = reactiveVal(list(1, 2, 3, 4, 5))), {

        # Simulate a file upload attempt after reaching the limit
        temp_csv <- system.file("extdata", "example_timeseries.csv", package = "HCVisR")

        session$setInputs(file = list(datapath = temp_csv, name = "example_timeseries.csv"))
        session$setInputs(process_file = 1)

        # Ensure no new series is added after the limit is reached
        expect_equal(length(available_series()), 5)
    })
})
