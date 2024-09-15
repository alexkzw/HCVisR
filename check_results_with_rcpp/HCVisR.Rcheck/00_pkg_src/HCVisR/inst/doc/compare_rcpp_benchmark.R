## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(Rcpp)
library(stats)   # For ARMA simulation
library(statcomp)   # For logistic map
library(microbenchmark)
library(ggplot2)

# Define a logistic map function as part of setup in case `chaos` isn't available
logistic_map <- function(N, r, x0 = 0.5) {
  series <- numeric(N)
  series[1] <- x0
  for (i in 2:N) {
    series[i] <- r * series[i - 1] * (1 - series[i - 1])
  }
  return(series)
}

# Load the C++ code
sourceCpp("../src/time_series_operators.cpp")  # Adjusted relative path for vignette

# Define the C++ wrapper function
time_series_operations_cpp <- function(ts_list, op, alpha = 0.5) {
  time_series_operations(ts_list, op, alpha)
}

## -----------------------------------------------------------------------------
set.seed(123)  # for reproducibility
ts1 <- arima.sim(model = list(ar = 0.5, ma = 0.5), n = 1000)  # ARMA(1,1)
ts2 <- statcomp::logistic_map(N = 1000, r = 4)  # Logistic map

## -----------------------------------------------------------------------------
# Helper function to benchmark the R vs C++ performance
benchmark_time_series_operations <- function(ts1, ts2, alpha = 0.5) {
    ts_list <- list(ts1, ts2)

    R_add <- function() { alpha * ts1 + (1 - alpha) * ts2 }
    R_multiply <- function() { alpha * ts1 * (1 - alpha) * ts2 }

    Cpp_add <- function() { time_series_operations_cpp(ts_list, "add", alpha) }
    Cpp_multiply <- function() { time_series_operations_cpp(ts_list, "multiply", alpha) }

    bm <- microbenchmark(
        R_add = R_add(),
        R_multiply = R_multiply(),
        Cpp_add = Cpp_add(),
        Cpp_multiply = Cpp_multiply(),
        times = 100
    )

    autoplot(bm)
}

# Run and plot the benchmark
benchmark_plot <- benchmark_time_series_operations(ts1, ts2, alpha = 0.5)
benchmark_plot

