#' Print method for TimeSeries objects
#'
#' Custom print method for TimeSeries objects.
#'
#' @param x An object of class 'TimeSeries'.
#' @param ... Additional arguments for printing.
#' @export
print.TimeSeries <- function(x, ...) {
    cat("Time Series Model: ", x$model, "\n")
    cat("First few values of the time series:\n")
    print(head(x$series, 10))
    cat("...\n")
}

#' Summary method for TimeSeries objects
#'
#' Provides a statistical summary of the time series data.
#'
#' @param object An object of class 'TimeSeries'.
#' @param ... Additional arguments for summary.
#' @return A summary of the time series.
#' @export
summary.TimeSeries <- function(object, ...) {
    series <- object$series
    model <- object$model

    cat("Model: ", model, "\n")
    cat("Length of series: ", length(series), "\n")
    cat("Summary statistics:\n")
    print(summary(series))
}

#' Plot method for TimeSeries objects
#'
#' Visualizes the time series data.
#'
#' @param x An object of class 'TimeSeries'.
#' @param ... Additional arguments for plotting.
#' @export
plot.TimeSeries <- function(x, ...) {
    series <- x$series
    model <- x$model

    plot(seq_along(series), series, type = "l", col = "blue",
         main = paste("Time Series Plot: ", model),
         xlab = "Time", ylab = "Value")
}

#' Combine two TimeSeries objects
#'
#' Combines two TimeSeries objects using addition or multiplication.
#'
#' @param ts1 The first TimeSeries object.
#' @param ts2 The second TimeSeries object.
#' @param method A string indicating "add" or "multiply".
#' @param alpha A numeric scalar for weighting the operations.
#' @return A new TimeSeries object with the combined series.
#' @export
combine.TimeSeries <- function(ts1, ts2, method = "add", alpha = 0.5) {
    if (length(ts1$series) != length(ts2$series)) {
        stop("Time series must be of equal length to combine.")
    }

    series1 <- ts1$series
    series2 <- ts2$series

    combined_series <- switch(method,
                              "add" = alpha * series1 + (1 - alpha) * series2,
                              "multiply" = alpha * series1 * (1 - alpha) * series2,
                              stop("Invalid method: choose 'add' or 'multiply'."))

    new_timeseries(series = combined_series, model = paste(ts1$model, "+", ts2$model))
}
