test_that("C++ time series operations (add and multiply) work correctly", {
    # Generate two random time series
    ts1 <- rnorm(100)
    ts2 <- rnorm(100)

    # Test addition of time series using the C++ operator
    result_add <- time_series_operations(list(ts1, ts2), "add", 0.5)

    # Extract the "result" element from the list
    combined_add <- result_add$result

    # Check that the result has the correct length and is numeric
    expect_equal(length(combined_add), 100)
    expect_true(is.numeric(combined_add))

    # Verify the addition operation: 0.5 * ts1 + 0.5 * ts2
    expect_equal(combined_add, 0.5 * ts1 + 0.5 * ts2, tolerance = 1e-5)

    # Test multiplication of time series using the C++ operator
    result_multiply <- time_series_operations(list(ts1, ts2), "multiply", 0.5)

    # Extract the "result" element from the list
    combined_multiply <- result_multiply$result

    # Check that the result has the correct length and is numeric
    expect_equal(length(combined_multiply), 100)
    expect_true(is.numeric(combined_multiply))

    # Verify the multiplication operation: simple point-by-point multiplication
    expect_equal(combined_multiply, ts1 * ts2, tolerance = 1e-5)
})
