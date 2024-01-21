Miguel Salema

<!-- README.md is generated from README.Rmd. Please edit that file -->

# politicR

<!-- badges: start -->
<!-- badges: end -->

This package provides functions for a political analysis of the
Portuguese parlamentary system.

## Installation

You can install the development version of politicRfrom
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Salema-DG/politicR")
```

## Example

This is a basic example of how to build a proximity matrix:

``` r
library(politicR)

## basic example code
data(ar_data)
ar_data |> prox_matrix(legislature = 15,
                       plot_plotly = F)
#> Joining with `by = join_by(partido, partido2)`
```

<img src="man/figures/README-example-1.png" style="width:100.0%" />
