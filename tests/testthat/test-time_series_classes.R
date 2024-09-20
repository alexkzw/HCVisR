test_that("new_timeseries creates a valid time series object", {
    ts <- new_timeseries(1:10, "AR(1)")
    expect_is(ts, "TimeSeries")
    expect_equal(ts$series, 1:10)
    expect_equal(ts$model, "AR(1)")
})

test_that("new_stochastic_ts creates a stochastic time series", {
    ts <- new_stochastic_ts("AR", n = 100)
    expect_is(ts, "StochasticTS")
    expect_equal(length(ts$series), 100)
})

test_that("new_deterministic_ts creates a deterministic time series", {
    ts <- new_deterministic_ts("logistic", N = 100)
    expect_is(ts, "DeterministicTS")
    expect_equal(length(ts$series), 100)
})
