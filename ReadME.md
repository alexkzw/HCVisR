
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HCVisR

<!-- badges: start -->

<!-- badges: end -->

## Overview

`HCVisR` is an interactive R Shiny app package that provides a
comprehensive tool for generating, visualizing, and analyzing time
series data within the Entropy-Statistical Complexity (H × C) framework.
Based on the methodology of Bandt and Pompe (2002), this app enables
users to explore the entropy and complexity characteristics of various
types of time series, both stochastic and deterministic, by mapping them
onto the H × C plane.

## Key Features

  - **Time Series Generation**: `HCVisR` supports the generation of
    multiple types of time series, including stochastic (e.g., White
    Noise, AR, ARMA) and deterministic (e.g., Logistic Map, Henon Map)
    models.

  - **H × C Plane Visualization**: The app calculates normalized Shannon
    entropy (H) and statistical complexity (C) for each time series
    using ordinal patterns. This calculation relies on the
    [StatOrdPattHxC](https://github.com/arey1911/StatOrdPattHxC)
    package, which provides the necessary functions for ordinal pattern
    analysis and entropy-complexity mapping. Each time series is
    visualized as a data point on the H × C plane, enabling users to
    identify patterns and gain insights into the behavior of different
    time series types.

  - **Mixing Time Series**: `HCVisR` provides a unique feature for
    “mixing” two time series through addition or multiplication. Users
    can select two generated time series and apply these operations,
    creating a new combined series. The app dynamically updates the H ×
    C visualization, showing how the combination affects entropy and
    complexity.

## Installation

You can install the development version of `HCVisR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alexkzw/HCVisR")
```

## Launching the R Shiny app

The R Shiny app can be launched with:

``` r
library(HCVisR)
HCVisR::launchApp()
```

## Vignette

A vignette is available here:
<https://github.com/alexkzw/HCVisR/blob/master/vignettes/HCVisR-vignette.pdf>

## Reference

1.  C. Bandt and B. Pompe. Permutation entropy: A natural complexity
    measure for time series. Physical Review Letters,
    88:174102–1–174102–4, 2002. doi:
    10.1103/PhysRevLett.88.174102.

2.  <https://github.com/arey1911/StatOrdPattHxC>

3.  <https://CRAN.R-project.org/package=statcomp>

4.  <https://CRAN.R-project.org/package=DChaos>
