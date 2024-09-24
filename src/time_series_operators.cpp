#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List time_series_operations(List ts_list, String op, double alpha) {
    int n = as<NumericVector>(ts_list[0]).size();
    NumericVector result(n, 0.0);  // Initialize result to 0.0

    // For addition
    if (op == "add") {
        NumericVector ts1 = as<NumericVector>(ts_list[0]);  // First series
        NumericVector ts2 = as<NumericVector>(ts_list[1]);  // Second series

        for (int j = 0; j < n; ++j) {
            // Apply alpha to the first series and (1 - alpha) to the second series
            result[j] = alpha * ts1[j] + (1 - alpha) * ts2[j];
        }
    }

    // For multiplication
    if (op == "multiply") {
        for (int j = 0; j < n; ++j) {
            result[j] = as<NumericVector>(ts_list[0])[j];  // Initialize result to first series
        }
        for (int i = 1; i < ts_list.size(); ++i) {
            NumericVector ts = as<NumericVector>(ts_list[i]);
            for (int j = 0; j < n; ++j) {
                result[j] = alpha * result[j] * (1 - alpha) * ts[j];
            }
        }
    }

    return List::create(Named("result") = result);
}
