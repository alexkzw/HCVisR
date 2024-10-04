#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List time_series_operations(List ts_list, std::string op, double alpha) {
    // Extract the time series from the list
    NumericVector ts1 = as<NumericVector>(ts_list[0]);  // First series
    NumericVector ts2 = as<NumericVector>(ts_list[1]);  // Second series
    int n = ts1.size();  // Get the size of the time series
    NumericVector result(n);  // Initialize result vector

    // For addition
    if (op == "add" || op == "+") {
        for (int j = 0; j < n; ++j) {
            // Apply alpha to the first series and (1 - alpha) to the second series
            result[j] = alpha * ts1[j] + (1 - alpha) * ts2[j];
        }
    }
    // For multiplication
    else if (op == "multiply" || op == "*") {
        for (int j = 0; j < n; ++j) {
            // Apply point-by-point multiplication
            result[j] = ts1[j] * ts2[j];
        }
    }
    // Handle unknown operations
    else {
        Rcpp::Rcout << "Unknown operation: " << op << std::endl;
        stop("Unsupported operation.");
    }

    return List::create(Named("result") = result);
}
