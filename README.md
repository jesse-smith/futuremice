
<!-- README.md is generated from README.Rmd. Please edit that file -->

# futuremice

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/futuremice)](https://CRAN.R-project.org/package=futuremice)
[![Codecov test
coverage](https://codecov.io/gh/jesse-smith/futuremice/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jesse-smith/futuremice?branch=master)
[![R-CMD-check](https://github.com/jesse-smith/futuremice/workflows/R-CMD-check/badge.svg)](https://github.com/jesse-smith/futuremice/actions)
<!-- badges: end -->

`{futuremice}` parallelizes the main functionality of the `{mice}`
package using `{future}` and `{furrr}`. This enables the use of a
progress bar for updates, as well as an early stopping method to save
time spent on unneeded iteration or manual convergence checks (not
quality checks - you still have to assess the results yourself).

## Installation

You can install the development version of futuremice like so:

``` r
# You will need Rtools to install packages from Github on Windows
# `devtools` with throw an informative error if Rtools is not found
if (!"devtools" %in% installed.packages()) install.packages("devtools")
devtools::install_github("jesse-smith/futuremice")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(futuremice)
```

## Code of Conduct

Please note that the futuremice project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.