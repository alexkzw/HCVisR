#' Print method for TimeSeries objects
#'
#' Custom print method for TimeSeries objects.
#'
#' @param x An object of class 'TimeSeries'.
#' @param ... Additional arguments for printing.
#' @importFrom utils head
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
#' Visualizes the time series data using ggplot2.
#'
#' @param x An object of class 'TimeSeries'.
#' @param ... Additional arguments for plotting.
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs
#' @export
plot.TimeSeries <- function(x, ...) {
    # Extract the series and model from the TimeSeries object
    series <- x$series
    model <- x$model

    # Create a dataframe for ggplot2
    data <- data.frame(Time = seq_along(series), Value = series)

    # Generate the plot using ggplot2
    ggplot(data, aes(x = Time, y = Value)) +
        geom_line(color = "blue") +  # Draw the line
        labs(title = paste("Time Series Plot:", model),
             x = "Time",
             y = "Value") +
        theme_minimal()  # Use the minimal theme
}

#' Combine two TimeSeries objects using Rcpp
#'
#' Combines two TimeSeries objects using addition or multiplication,
#' utilizing the Rcpp function for performance.
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

    # Call the Rcpp function to perform the operation
    combined_result <- time_series_operations_cpp(list(ts1$series, ts2$series), method, alpha)

    # Return the new combined time series
    new_timeseries(series = combined_result$result, model = paste(ts1$model, "+", ts2$model))
}

