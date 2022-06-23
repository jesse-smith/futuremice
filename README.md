
<!-- README.md is generated from README.Rmd. Please edit that file -->

# futuremice

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/futuremice)](https://CRAN.R-project.org/package=futuremice)
[![Codecov test
coverage](https://codecov.io/gh/jesse-smith/futuremice/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jesse-smith/futuremice?branch=master)
[![R-CMD-check](https://github.com/jesse-smith/futuremice/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jesse-smith/futuremice/actions/workflows/R-CMD-check.yaml)
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

## Minimal Example

Let’s run the example from the `{mice}` package documentation, but in
parallel.

``` r
# Load {futuremice}
library(futuremice)

# Evaluate futures in parallel - max of two workers to avoid hogging resources
future::plan("multisession", workers = pmin(2L, future::availableCores()))

# Use {progress} package for progress bar - shows diagnostics in real time
progressr::handlers("progress")
```

`{futuremice}` uses the `{future}` package to run imputations in
parallel. By default, `{future}` will run a `"sequential"` plan, which
is no different (and a little less efficient) than calling
`mice::mice()`. To take advantage of multiple CPUs, we can use a
`"multisession"` plan (see the
[vignette](https://future.futureverse.org/articles/future-1-overview.html#controlling-how-futures-are-resolved)
from the `{future}` package for details on different plans).
`future_mice()` also provides a progress bar and real-time convergence
diagnostics using `{progressr}`; however, the default progress bar does
not show messages, so we’ll use the `progress` handler to see our
diagnostics.

Now, let’s impute our missing data:

``` r
# Impute the missing values using defaults
# Use `progressr::with_progress()` to show the progress bar
mids <- progressr::with_progress(future_mice(mice::nhanes))
#> Converged in 63 iterations
#> R-hat: 1.042/1.048/1.044 < 1.05

# Or start with `mice::mice()` and finish with `future_mids()`
mids2 <- mice::mice(mice::nhanes, maxit = 1L, printFlag = FALSE)
mids2 <- progressr::with_progress(future_mids(mids2, maxit = 100L))
#> Converged in 44 iterations
#> R-hat: 1.043/1.029/1.041 < 1.05

# View the resulting `mids` (*m*ultiply *i*mputed *d*ata *s*et) object
mids
#> Class: mids
#> Number of multiple imputations:  5 
#> Imputation methods:
#>   age   bmi   hyp   chl 
#>    "" "pmm" "pmm" "pmm" 
#> PredictorMatrix:
#>     age bmi hyp chl
#> age   0   1   1   1
#> bmi   1   0   1   1
#> hyp   1   1   0   1
#> chl   1   1   1   0

# List the actual imputations for BMI
mids$imp$bmi
#>       1    2    3    4    5
#> 1  33.2 33.2 33.2 33.2 33.2
#> 3  27.2 27.2 27.2 27.2 27.2
#> 4  22.5 22.5 22.5 22.5 22.5
#> 6  27.5 27.5 27.5 27.5 27.5
#> 10 27.4 27.4 27.4 27.4 27.4
#> 11 29.6 29.6 29.6 29.6 29.6
#> 12 22.5 22.5 22.5 22.5 22.5
#> 16 27.2 27.2 27.2 27.2 27.2
#> 21 35.3 35.3 35.3 35.3 35.3
```

Note that `future_mice()` will often run longer than `mice::mice()`’s
default of `5` imputations before convergence is confidently achieved.
Also note that we will only get a progress bar if we wrap the call in
`with_progress()`; this is a feature of the `{progressr}` package.

We can use the resulting `mids` object just like the result of a call to
`mice::mice()`. Let’s inspect the quality of the imputations:

``` r
# Inspect quality of imputations
mice::stripplot(mids, chl, pch = 19, xlab = "Imputation number")
```

<img src="man/figures/README-example-inspect-1.png" width="100%" />

In general, we would like the imputations to be plausible, i.e., values
that could have been observed if they had not been missing. Now let’s
fit a model to the imputed data set and pool the results:

``` r
# Fit complete-data model
fit <- with(mids, lm(chl ~ age + bmi))

# Pool and summarize the results
summary(mice::pool(fit))
#>          term  estimate std.error  statistic       df      p.value
#> 1 (Intercept)  4.210554 50.318871 0.08367742 20.23797 0.9341350824
#> 2         age 31.876011  7.851262 4.05998564 20.23797 0.0005992299
#> 3         bmi  4.975187  1.550839 3.20806096 20.23797 0.0043667973
```

The complete-data model is fit to each imputed data set, and the results
are combined to arrive at estimates that properly account for the
missing data.

We can also compare two `mids` objects using `compare_mids()`:

``` r
compare_mids(mids, mids2, ignore_rng = TRUE)
#> Elements of `x` not in `y`
#> ✔ None
#> Elements of `y` not in `x`
#> ✔ None
#> Shared elements with differences:
#> ✖ iteration
#> ✖ chainMean
#> ✖ chainVar
#> Shared elements without differences:
#> ✔ data
#> ✔ imp
#> ✔ m
#> ✔ where
#> ✔ blocks
#> ✔ nmis
#> ✔ method
#> ✔ predictorMatrix
#> ✔ visitSequence
#> ✔ formulas
#> ✔ post
#> ✔ blots
#> ✔ ignore
#> Shared elements ignored:
#> ℹ call
#> ℹ seed
#> ℹ lastSeedValue
#> ℹ loggedEvents
#> ℹ version
#> ℹ date
```

This will show us where differences occur between the two objects (if
there are any). We’ll ignore attributes that depend on the RNG state
because evaluating imputations in parallel requires a different kind of
random number generation than evaluating sequentially, as we did in the
first iteration of `mice::mice()`.

## Code of Conduct

Please note that the futuremice project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
