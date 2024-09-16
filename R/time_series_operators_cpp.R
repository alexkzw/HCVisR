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
    time_series_operations(ts_list, op, alpha)  # Call the C++ function
}
