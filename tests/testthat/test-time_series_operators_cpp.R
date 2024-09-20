test_that("time_series_operations_cpp works for addition", {
    ts1 <- 1:10
    ts2 <- 11:20
    result <- time_series_operations_cpp(list(ts1, ts2), "add", 0.5)

    expected <- 0.5 * ts1 + 0.5 * ts2
    expect_equal(result$result, expected)
})

test_that("time_series_operations_cpp works for multiplication", {
    ts1 <- 1:10
    ts2 <- 11:20
    result <- time_series_operations_cpp(list(ts1, ts2), "multiply", 0.5)

    expected <- 0.5 * ts1 * 0.5 * ts2
    expect_equal(result$result, expected)
})
