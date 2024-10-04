test_that("Time series constructors work for all models", {
    # Test White Noise (WN) generation
    ts_wn <- new_stochastic_ts(model = "WN", n = 100)
    expect_equal(ts_wn$model, "WN")
    expect_equal(length(ts_wn$series), 100)

    # Test AR(1) generation
    ts_ar <- new_stochastic_ts(model = "AR", n = 100, phi = 0.5)
    expect_equal(ts_ar$model, "AR")
    expect_equal(length(ts_ar$series), 100)

    # Test ARMA(1,1) generation
    ts_arma <- new_stochastic_ts(model = "ARMA", n = 100, phi = 0.5, theta = 0.4)
    expect_equal(ts_arma$model, "ARMA")
    expect_equal(length(ts_arma$series), 100)

    # Test Colored Noise (White, Pink, Red)
    ts_white <- new_stochastic_ts(model = "white", n = 100)
    expect_equal(ts_white$model, "white")
    expect_equal(length(ts_white$series), 100)

    ts_pink <- new_stochastic_ts(model = "pink", n = 100)
    expect_equal(ts_pink$model, "pink")
    expect_equal(length(ts_pink$series), 100)

    ts_red <- new_stochastic_ts(model = "red", n = 100)
    expect_equal(ts_red$model, "red")
    expect_equal(length(ts_red$series), 100)

    # Test Logistic Map (Deterministic)
    ts_logistic <- new_deterministic_ts(model = "logistic", N = 100, r = 3.9)
    expect_equal(ts_logistic$model, "logistic")
    expect_equal(length(ts_logistic$series), 100)
    # ensure no infinite values
    expect_false(any(is.infinite(ts_logistic$series)), info = "Logistic map should not contain infinite values")

    # Test Henon Map (Deterministic)
    ts_henon <- new_deterministic_ts(model = "henon", N = 100, a = 1.4, b = 0.3, x0 = 0.1, y0 = 0.1)
    expect_equal(ts_henon$model, "henon")
    expect_equal(length(ts_henon$series), 100)
    expect_false(any(is.infinite(ts_henon$series)), info = "Henon map should not contain infinite values")
})
