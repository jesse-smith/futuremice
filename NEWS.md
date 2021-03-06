<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# futuremice 0.0.0.9004

* Fix progress bar not displaying
* Fix issues with lazy argument evaluation during parallelization


# futuremice 0.0.0.9003

* Use `minit` to control minimum iterations in `future_mice()`
* Add comprehensive tests for small functions
* Add minimal test for `future_mice()`

# futuremice 0.0.0.9002

* Change default `maxit` in `future_mice()` from `50` to `100`
* Change default `maxit` in `future_mids()` from `1` to `100`


# futuremice 0.0.0.9000

* Add a `NEWS.md` file to track changes to the package.
* Add `compare_mids()` to compare two `mids` objects
* Add `future_mids()` to add imputation iterations in parallel
* Add `isplit()` to split a imputations into multiple `mids` objects
* Add `ibindlist()` to bind imputations from multiple `mids` objects
* Add `future_mice()` to run multiple imputations in parallel
* Add `rhat_max()` to assess convergence
