# Exports ----------------------------------------------------------------------

#' Quick Max R-hat Calculation
#'
#' Calculates the largest R-hat statistic across all variables and chain
#' statistics for the `it` most recent iterations
#'
#' @param mids A `mids` object as created by `mice::mice()`
#' @param it The number of recent iterations for which R-hat should be
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
rhat_max <- function(mids, it = 1L) {
  it <- fm_assert_count(it)
  if (it == 0L) return(numeric())
  params <- fm_prep_diagnostic_params(mids)
  maxit <- mids$iteration
  minit <- pmax(1L, maxit - it + 1L)
  iters_list <- purrr::map(
    seq.int(minit, maxit, by = 1L),
    seq_len
  )
  purrr::map_dbl(iters_list, ~ fm_rhat_max_(params, iters = .x))
}


# Helpers ----------------------------------------------------------------------


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
    purrr::map_dbl(c(params$mean, params$var), ~ rstan::Rhat(.x[iters])),
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
