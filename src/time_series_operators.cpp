#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List time_series_operations(List ts_list, String op, double alpha) {
    int n = as<NumericVector>(ts_list[0]).size();
    NumericVector result(n, 0.0);

    for (int i = 0; i < ts_list.size(); ++i) {
        NumericVector ts = as<NumericVector>(ts_list[i]);
        if (op == "add") {
            for (int j = 0; j < n; ++j) {
                result[j] = alpha * result[j] + (1 - alpha) * ts[j];
            }
        } else if (op == "multiply") {
            for (int j = 0; j < n; ++j) {
                result[j] = alpha * result[j] * (1 - alpha) * ts[j];
            }
        }
    }
    return List::create(Named("result") = result);
}
