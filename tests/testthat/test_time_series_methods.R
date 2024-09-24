test_that("Time series methods work correctly", {
    # Create a new TimeSeries object for testing
    ts <- new_timeseries(rnorm(100), "AR")

    # Test the print method
    expect_output(print(ts), "Time Series Model:\\s+AR")  # Allow for extra spaces
    expect_output(print(ts), "First few values of the time series:")

    # Test the summary method
    expect_output(summary(ts), "Model:\\s+AR")  # Allow for extra spaces
    expect_output(summary(ts), "Length of series:")
    expect_output(summary(ts), "Summary statistics:")

    # Test the plot method
    expect_silent({
        plot(ts)  # Ensure no errors occur during plotting
    })

    # Test combining two TimeSeries objects using addition
    ts2 <- new_timeseries(rnorm(100), "WN")
    combined_ts_add <- combine.TimeSeries(ts, ts2, method = "add", alpha = 0.5)
    expect_equal(combined_ts_add$model, "AR + WN")
    expect_equal(length(combined_ts_add$series), 100)

    # Test combining two TimeSeries objects using multiplication
    combined_ts_multiply <- combine.TimeSeries(ts, ts2, method = "multiply", alpha = 0.5)
    expect_equal(combined_ts_multiply$model, "AR + WN")
    expect_equal(length(combined_ts_multiply$series), 100)

    # Ensure proper error handling for mismatched series lengths
    ts_mismatched <- new_timeseries(rnorm(150), "ARMA")
    expect_error(combine.TimeSeries(ts, ts_mismatched, method = "add"),
                 "Time series must be of equal length to combine.")
})
