# Exports ----------------------------------------------------------------------

#' Quick Max R-hat Calculation
#'
#' Calculates the largest R-hat statistic across all variables and chain
#' statistics for the `it` most recent iterations
#'
#' @param mids A `mids` object as created by `mice::mice()`
#' @param n The number of recent iterations for which R-hat should be
#'   calculated. If this is larger than the total number of iterations, it is
#'   truncated.
#'
#' @return A scalar `double` containing the maximum R-hat statistic
#'
#' @examples
#' # Create `mids` object
#' mids <- mice::mice(mice::nhanes)
#'
#' # Get max R-hat for most recent 2 iterations
#' rhat <- rhat_max(mids, it = 2L)
#' rhat
#'
#' @export
rhat_max <- function(mids, n = 1L) {
  it <- fm_assert_count(n)
  if (it == 0L) return(numeric())
  params <- fm_prep_diagnostic_params(mids)
  maxit <- mids$iteration
  minit <- pmax(1L, maxit - n + 1L)
  iters_list <- purrr::map(
    seq.int(minit, maxit, by = 1L),
    seq_len
  )
  purrr::map_dbl(iters_list, ~ fm_rhat_max_(params, iters = .x))
}


# Helpers ----------------------------------------------------------------------


#' Check Convergence of `mids` Object
#'
#' @param mids A `mids` object with imputations to check
#' @param n The number of iterations to use when checking. Must be > 0.
#' @param mean The upper bound for the mean/median of the R-hat values being checked
#' @param max The upper bound for R-hat convergence
fm_rhat_converged <- function(mids, n = 1L, max = 1.05) {
  fm_assert_mids(mids)
  n <- fm_assert_count(n, zero_ok = FALSE)
  max <- fm_assert_num(max)
  rhat <- rhat_max(mids, n = n)
  converged <- rlang::is_true(
    length(rhat) >= n && !anyNA(rhat) && all(rhat < max)
  )
  list(rhat = rhat, converged = converged)
}


#' Quick Max R-hat Calculation
#'
#' Calculates the largest R-hat statistic across all variables and chain
#' statistics for the most recent iteration
#'
#' @inheritParams rhat_max
#' @param iters An `integer` vector of iterations to use in the R-hat
#'   R-hat calculation
#'
#' @return A scalar `double` containing the maximum R-hat statistic
#'
#' @keywords internal
fm_rhat_max_ <- function(params, iters = NULL) {
  # Calculate R-hat for each variable and parameter, then get max
  if (is.null(iters)) iters <- seq_len(NROW(params$mean[[1L]]))
  rhat_max <- suppressWarnings(max(
    purrr::map_dbl(c(params$mean, params$var), ~ rstan_rhat(.x[iters])),
    na.rm = TRUE
  ))
  # Replace infinite value w/ NA
  if (is.infinite(rhat_max) || is.nan(rhat_max)) rhat_max <- NA_real_

  rhat_max
}


#' Extract Chain Means and Variances from a `mids` object
#'
#' @inheritParams rhat_max
#'
#' @return A list of 2D matrices (one for each imputed variable) with rows
#'   corresponding to iterations and columns corresponding to chains
#'
#' @keywords internal
fm_prep_diagnostic_params <- function(mids) {
  # Input checks
  fm_assert_mids(mids)
  if (is.null(mids$chainMean) || prod(dim(mids$chainMean)) == 0) {
    rlang::abort("No convergence diagnostics found")
  }
  # Variables
  vrbs <- colnames(mids$where)[colSums(mids$where) > 0L]
  n <- length(vrbs)
  # Reshape chain data
  chain_mean <- aperm(mids$chainMean[vrbs, , , drop = FALSE], c(2L, 3L, 1L))
  chain_var  <- aperm(mids$chainVar[vrbs, , , drop = FALSE], c(2L, 3L, 1L))
  # Extract chains for each variable and parameter
  seq_n <- seq_len(n)
  param_mean <- purrr::map(seq_n, ~ chain_mean[, , .x])
  param_var  <- purrr::map(seq_n, ~ chain_var[, , .x])

  list(mean = param_mean, var = param_var)
}


#' Re-Implement `rstan::Rhat()`
#'
#' Internal implementation of `rstan::Rhat()` to avoid unnecessary dependency.
#'
#' @param params A 2D matrix or array with one row per iteration and one column
#'   per chain. The cells are realized draws for a particular parameter or
#'   function of parameters.
#' @param x Vector, matrix, or array to scale
#'
#' @return `rstan_rhat()` returns a scalar `double` that is the max of the bulk
#'   and tail (folded) R-hat statistic for `params`. `rstan_rhat_()` returns a
#'   scalar `double` R-hat. `rstan_split_chains()` returns an array with each
#'   column split into two columns (top and bottom half). `rstan_z_scale()`
#'   returns the scaled vector/matrix/array.
#'
#' @keywords internal
rstan_rhat <- function(params) {
  bulk_rhat <- rstan_rhat_(rstan_z_scale(rstan_split_chains(params)))
  params_folded <- abs(params - stats::median(params))
  tail_rhat <- rstan_rhat_(rstan_z_scale(rstan_split_chains(params_folded)))
  max(bulk_rhat, tail_rhat)
}


#' @rdname rstan_rhat
#'
#' @keywords internal
rstan_rhat_ <- function(params) {
  if (anyNA(params) || rstan_is_constant(params)) return(NA)
  if (any(!is.finite(params))) return(NaN)

  if (is.vector(params)) dim(params) <- c(length(params), 1L)

  n_samples <- NROW(params)
  chains <- seq_len(NCOL(params))
  chain_mean <- purrr::map_dbl(chains, ~ mean(params[, .x]))
  chain_var  <- purrr::map_dbl(chains, ~ stats::var(params[, .x]))
  var_between <- n_samples * stats::var(chain_mean)
  var_within <- mean(chain_var)
  sqrt((var_between / var_within + n_samples - 1) / n_samples)
}


#' @rdname rstan_rhat
#'
#' @keywords internal
rstan_split_chains <- function(params) {
  if (is.vector(params)) dim(params) <- c(length(params), 1L)
  niter <- NROW(params)
  if (niter == 1L) return(params)
  half <- niter / 2L
  cbind(params[1L:floor(half),], params[ceiling(half + 1L):niter,])
}


#' @rdname rstan_rhat
#'
#' @keywords internal
rstan_z_scale <- function(x) {
  r <- rank(x, ties.method = "average")
  z <- stats::qnorm((r - 0.5) / length(x))
  z[is.na(x)] <- NA_real_
  if (!is.null(dim(x))) z <- array(z, dim = dim(x), dimnames = dimnames(x))
  z
}


#' @rdname rstan_rhat
#'
#' @keywords internal
rstan_is_constant <- function(x, tol = .Machine$double.eps) {
  abs(max(x) - min(x)) < tol
}
