pkgname <- "HCVisR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "HCVisR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('HCVisR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("benchmark_time_series_operations")
### * benchmark_time_series_operations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: benchmark_time_series_operations
### Title: Benchmark Time Series Operations (R vs C++)
### Aliases: benchmark_time_series_operations

### ** Examples

# Example usage:
set.seed(123)
ts1 <- stats::arima.sim(model = list(ar = 0.5, ma = 0.5), n = 1000)  # ARMA(1,1)
ts2 <- statcomp::logistic_map(N = 1000, r = 4)  # Logistic map

# Plot the benchmark comparison
benchmark_time_series_operations(ts1, ts2, alpha = 0.5)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("benchmark_time_series_operations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("time_series_operations_cpp")
### * time_series_operations_cpp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: time_series_operations_cpp
### Title: Time Series Operations using Rcpp
### Aliases: time_series_operations_cpp

### ** Examples

#' # Example usage:
set.seed(123)
ts1 <- stats::arima.sim(model = list(ar = 0.5, ma = 0.5), n = 1000)  # ARMA(1,1)
ts2 <- statcomp::logistic_map(N = 1000, r = 4)  # Logistic map

time_series_operations_cpp(list(ts1, ts2), "add", 0.5)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("time_series_operations_cpp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
