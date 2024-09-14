# Base class constructor for time series
new_timeseries <- function(series, model, class = character()) {
    structure(
        list(
            series = series,
            model = model
        ),
        class = c(class, "TimeSeries")
    )
}

# StochasticTS constructor (including colored noise)
new_stochastic_ts <- function(model = "WN", phi = NULL, theta = NULL, differencing = 0, n = 200, color = NULL) {
    # Set phi and theta to safe values for stationary AR(1) and ARMA(1) models
    if (model == "AR") {
        phi <- 0.5  # Stationary AR(1) model with phi between -1 and 1
    } else if (model == "ARMA") {
        phi <- 0.5  # Stationary AR(1)
        theta <- 0.4  # MA(1) coefficient within safe range
    }

    # Generate time series based on the selected model
    series <- switch(model,
                     "WN" = arima.sim(model = list(order = c(0, 0, 0)), n = n),
                     "AR" = arima.sim(model = list(ar = phi, order = c(1, 0, 0)), n = n),  # AR(1) with phi = 0.5
                     "ARMA" = arima.sim(model = list(ar = phi, ma = theta, order = c(1, 0, 1)), n = n),  # ARMA(1,1)

                     # Add handling for colored noise
                     "white" = as.numeric(noise(kind = "white", duration = n, pcm = TRUE)@left),  # White noise
                     "pink" = as.numeric(noise(kind = "pink", duration = n, pcm = TRUE)@left),   # Pink noise
                     "red" = as.numeric(noise(kind = "red", duration = n, pcm = TRUE)@left),    # Red noise

                     # Handle unrecognized models
                     stop("Model type not recognized. Please choose from 'WN', 'AR', 'ARMA', 'white', 'pink', 'red'.")
    )

    new_timeseries(series, model, class = "StochasticTS")
}


# DeterministicTS constructor
new_deterministic_ts <- function(model = "logistic", N = 1000, r = 4, a = 1.4, b = 0.3) {
    series <- switch(model,
                     "logistic" = statcomp::logistic_map(N = N, r = r),
                     "henon" = statcomp::henon_map(N = N, a = a, b = b)$x_ts,
                     stop("Model type not recognised. Please choose from 'logistic' or 'henon'.")
    )

    new_timeseries(series, model, class = "DeterministicTS")
}
