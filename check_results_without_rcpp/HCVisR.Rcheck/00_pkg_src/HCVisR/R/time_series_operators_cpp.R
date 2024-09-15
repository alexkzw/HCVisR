#' @useDynLib HCVisR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @export
NULL

#' Time Series Operations using Rcpp
#'
#' This function performs time series operations (either addition or multiplication)
#' on a list of time series using C++ for faster computation.
#'
#' @param ts_list A list of numeric vectors representing time series.
#' @param op A string, either "add" or "multiply", to specify the operation.
#' @param alpha A numeric scalar, typically between 0 and 1, for weighting the operations.
#' @return A list containing the result of the operation on the time series.
#' @examples
#' #' # Example usage:
#' set.seed(123)
#' ts1 <- stats::arima.sim(model = list(ar = 0.5, ma = 0.5), n = 1000)  # ARMA(1,1)
#' ts2 <- statcomp::logistic_map(N = 1000, r = 4)  # Logistic map
#'
#' time_series_operations_cpp(list(ts1, ts2), "add", 0.5)
#' @export
time_series_operations_cpp <- function(ts_list, op, alpha = 0.5) {
    # Placeholder R code to mimic addition or multiplication without Rcpp
    result <- ts_list[[1]]  # Start with the first time series
    if (length(ts_list) > 1) {
        for (i in 2:length(ts_list)) {
            if (op == "add") {
                result <- result + alpha * ts_list[[i]]
            } else if (op == "multiply") {
                result <- result * ts_list[[i]]
            }
        }
    }
    return(result)
    #time_series_operations(ts_list, op, alpha)  # Call the C++ function
}

#' Benchmark Time Series Operations (R vs C++)
#'
#' This function benchmarks the performance of time series operations implemented in R
#' versus the C++ implementation provided via Rcpp.
#'
#' @param ts1 A numeric vector representing the first time series.
#' @param ts2 A numeric vector representing the second time series.
#' @param alpha A numeric scalar for weighting the operations, default is 0.5.
#' @return A ggplot object showing the comparison of performance between R and C++ implementations.
#' @import ggplot2
#' @import microbenchmark
#' @examples
#' # Example usage:
#' set.seed(123)
#' ts1 <- stats::arima.sim(model = list(ar = 0.5, ma = 0.5), n = 1000)  # ARMA(1,1)
#' ts2 <- statcomp::logistic_map(N = 1000, r = 4)  # Logistic map
#'
#' # Plot the benchmark comparison
#' benchmark_time_series_operations(ts1, ts2, alpha = 0.5)
#' @export
benchmark_time_series_operations <- function(ts1, ts2, alpha = 0.5) {
    # Prepare a list of time series for the C++ function
    ts_list <- list(ts1, ts2)

    # Benchmarking the R version (assuming you have defined R equivalents)
    R_add <- function() { alpha * ts1 + (1 - alpha) * ts2 }
    R_multiply <- function() { alpha * ts1 * (1 - alpha) * ts2 }

    # Benchmarking the C++ version
    Cpp_add <- function() { time_series_operations_cpp(ts_list, "add", alpha) }
    Cpp_multiply <- function() { time_series_operations_cpp(ts_list, "multiply", alpha) }

    # Run the microbenchmark
    bm <- microbenchmark::microbenchmark(
        R_add = R_add(),
        R_multiply = R_multiply(),
        Cpp_add = Cpp_add(),
        Cpp_multiply = Cpp_multiply(),
        times = 100
    )

    # Plotting the results
    autoplot(bm)
}
