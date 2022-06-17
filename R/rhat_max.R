#' Quick Max R-hat Calculation
#'
#' Calculates the largest R-hat statistic across all variables and chain
#' statistics for the most recent iteration
#'
#' @param data An object of class `mids` as created by `mice::mice()`
#'
#' @return A scalar `double` containing the maximum R-hat statistic
#'
#' @keywords internal
rhat_max <- function(data) {
  params <- prep_diagnostic_params(data)
  # Calculate R-hat for each variable and parameter, then get max
  rhat_max_ <- suppressWarnings(max(
    purrr::map_dbl(params, rstan::Rhat),
    na.rm = TRUE
  ))
  # Replace infinite value w/ NA
  if (is.infinite(rhat_max_) || is.nan(rhat_max_)) rhat_max_ <- NA_real_

  rhat_max_
}


#' Extract Chain Means and Variances from a `mids` object
#'
#' @inheritParams rhat_max
#'
#' @return A list of 2D matrices (one for each imputed variable) with rows
#'   corresponding to iterations and columns corresponding to chains
#'
#' @keywords internal
prep_diagnostic_params <- function(data) {
  # Input checks
  if (!mice::is.mids(data)) rlang::abort("`data` not of class 'mids'")
  if (is.null(data$chainMean)) rlang::abort("No convergence diagnostics found")
  # Variables
  vrbs <- colnames(data$where)[colSums(data$where) > 0L]
  n <- length(vrbs)
  # Reshape chain data
  chain_mean <- aperm(data$chainMean[vrbs, , , drop = FALSE], c(2L, 3L, 1L))
  chain_var  <- aperm(data$chainVar[vrbs, , , drop = FALSE], c(2L, 3L, 1L))
  # Extract chains for each variable and parameter
  seq_n <- seq_len(n)
  param_mean <- purrr::map(seq_n, ~ chain_mean[, , .x])
  param_var  <- purrr::map(seq_n, ~ chain_var[, , .x])

  c(param_mean, param_var)
}
