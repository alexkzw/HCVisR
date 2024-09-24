test_that("C++ time series operations (add and multiply) work correctly with tolerance", {
    # Generate two random time series
    ts1 <- rnorm(100)
    ts2 <- rnorm(100)

    # Test addition of time series using the C++ operator
    result_add <- time_series_operations_cpp(list(ts1, ts2), "add", 0.5)

    # Extract the "result" element from the list
    combined_add <- result_add$result

    expect_equal(length(combined_add), 100)  # Check the length is correct
    expect_true(is.numeric(combined_add))  # Ensure the result is numeric

    # Verify the operation with a tolerance of 1e-5 for precision
    expect_equal(combined_add, 0.5 * ts1 + 0.5 * ts2, tolerance = 1e-5)

    # Test multiplication of time series using the C++ operator
    result_multiply <- time_series_operations_cpp(list(ts1, ts2), "multiply", 0.5)

    # Extract the "result" element from the list
    combined_multiply <- result_multiply$result

    expect_equal(length(combined_multiply), 100)  # Check the length is correct
    expect_true(is.numeric(combined_multiply))  # Ensure the result is numeric

    # Verify the multiplication operation with a tolerance
    expect_equal(combined_multiply, 0.5 * ts1 * (1 - 0.5) * ts2, tolerance = 1e-5)
})
