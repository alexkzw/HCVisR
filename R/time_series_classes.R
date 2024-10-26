#' Time Series Object Constructor
#'
#' Constructs a new time series object with specified attributes.
#'
#' @param series Numeric vector representing the time series data
#' @param model Character string naming the model used to generate the series
#' @param class Additional classes to assign to the time series object
#' @return An object of class 'TimeSeries' along with any specified additional classes
#' @export
new_timeseries <- function(series, model, class = character()) {
    structure(
        list(
            series = series,
            model = model
        ),
        class = c(class, "TimeSeries")
    )
}

#' StochasticTS Constructor
#'
#' Creates a stochastic time series object based on the specified model and parameters.
#'
#' @param model Character string specifying the stochastic model (e.g., "AR", "ARMA").
#' @param phi Numeric value representing the AR coefficient.
#' @param theta Numeric value representing the MA coefficient.
#' @param differencing Integer for differencing order (default is 0).
#' @param n Integer representing the number of points in the time series.
#' @param color Character string representing the noise color (e.g., "white", "pink", "red").
#' @return A stochastic time series object.
#' @importFrom stats arima.sim
#' @importFrom tuneR noise
#' @export
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
                     "WN" = as.numeric(arima.sim(model = list(order = c(0, 0, 0)), n = n)),  # Convert to numeric
                     "AR" = as.numeric(arima.sim(model = list(ar = phi, order = c(1, 0, 0)), n = n)),  # Convert to numeric
                     "ARMA" = as.numeric(arima.sim(model = list(ar = phi, ma = theta, order = c(1, 0, 1)), n = n)),  # Convert to numeric

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
new_deterministic_ts <- function(model = "logistic", N = 1000, r = 4, a = 1.4,
                                 b = 0.3, x0 = 0.1, y0 = 0.1) {
    series <- switch(model,
                     "logistic" = as.numeric(statcomp::logistic_map(N = N, r = r)),  # Convert to numeric

                     # Henon map uses the DChaos package
                     "henon" = {
                         henon_data <- as.data.frame(DChaos::henon.sim(n = N, a = a, b = b,
                                                                       x0 = x0, y0 = y0))
                         as.numeric(henon_data$x)  # Convert the 'x' component of the Henon map to numeric
                     },

                     stop("Model type not recognised. Please choose from 'logistic' or 'henon'.")
    )

    new_timeseries(series, model, class = "DeterministicTS")
}
