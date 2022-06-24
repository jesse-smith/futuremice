# Utilities --------------------------------------------------------------------


#' Calculate Parameters for Parallelization of Chains
#'
#' Calculates the number of chains per call to `mice::mice()` or
#' `mice::mice.mids()`, the average number of calls chunked into a future, and
#' the number of futures needed to satisfy the given arguments.
#'
#' @param m The total number of chains (imputations)
#' @param chunk_size The average number of chains per future
#'
#' @return A `list` containing parameters `n_chains` (chains per call),
#'   `n_calls` (number of calls to `mice()`),
#'   `chunk_size` (number of calls per future),
#'   `maxit` (maximum number of iterations), and `seed` (RNG seed)
#'
#' @keywords internal
fm_parallel_params <- function(m, chunk_size, maxit, seed) {
  # Check arguments
  m <- fm_assert_count(m, zero_ok = FALSE)
  chunk_size <- fm_assert_count(chunk_size, zero_ok = FALSE)
  maxit <- fm_assert_count(maxit)
  seed <- fm_assert_seed(seed)

  # Get number of chains per call- greatest common divisor of `chunk_size` & `m`
  n_chains <- fm_assert_count(fm_gcd(chunk_size, m), zero_ok = FALSE)

  # Update chunk_size to reflect (possibly) multiple chains per chunk
  chunk_size <- fm_assert_count(chunk_size %/% n_chains, zero_ok = FALSE)

  # Calculate number of calls
  n_calls <- fm_assert_count(m %/% n_chains, zero_ok = FALSE)

  list(
    n_chains = n_chains,
    n_calls = n_calls,
    chunk_size = chunk_size,
    maxit = maxit,
    seed = seed
  )
}

#' Initialize `progressor` for `future_mice()` and `future_mids()`
#'
#' @param parallel_params List of parameters for parallelization as calculated
#'   by `fm_parallel_params()`
#' @param progressor An optional existing `progressor` object, or `NULL`
#'
#' @return A `progressor` object; if `progressor != NULL`, this is the unchanged
#'   input
#'
#' @keywords internal
fm_progressor <- function(parallel_params, progressor = NULL) {
  if (inherits(progressor, "progressor")) {
    progressor
  } else if (!is.null(progressor)) {
    rlang::abort(
      "`progressor` must be `NULL` or a `progressor` function from {progressr}"
    )
  } else {
    fm_assert_count(parallel_params$n_calls)
    fm_assert_count(parallel_params$maxit)
    progressr::progressor(parallel_params$n_calls * parallel_params$maxit)
  }
}


#' Create `furrr_options()` List from Parallelization Parameters
#'
#' @param parallel_params List of parameters for parallelization as calculated
#'   by `fm_parallel_params()`
#'
#' @return A list of options for `furrr` functions, as created by
#'   `furrr::furrr_options()`
#'
#' @keywords internal
fm_furrr_opts <- function(parallel_params) {
  seed <- fm_rng_seed(parallel_params$seed)
  if (!exists(".Random.seed")) {
    rlang::abort("`.Random.seed` does not exist")
  }
  seed_seq <- rngtools::RNGseq(
    fm_assert_count(parallel_params$n_calls, zero_ok = FALSE),
    seed = seed,
    simplify = FALSE
  )
  furrr::furrr_options(
    seed = seed_seq,
    globals = character(),
    chunk_size = fm_assert_count(parallel_params$chunk_size, zero_ok = FALSE)
  )
}


#' Throw Messages/Warnings at End of `future_mice()` Execution
#'
#' @param i Integer(ish) representing iteration count
#' @param rhat_lt Logical vector of R-hat comparisons. `length(rhat_lt)` must be
#'   less than or equal to `rhat_it`.
#' @param rhat_it Integer(ish) number of iterations used in R-hat comparison
#' @param rhat_msg Contents of `message` displaying R-hat values for last
#'   `rhat_it` iterations
#'
#' @return `NULL`, invisibly
#'
#' @keywords internal
fm_exit_msg <- function(i, rhat_lt, rhat_it, rhat_msg) {
  i <- fm_assert_count(i)
  rhat_it <- fm_assert_count(rhat_it, zero_ok = FALSE)
  if (!(is.logical(rhat_lt) && length(rhat_lt) > 0L && length(rhat_lt) <= i)) {
    rlang::abort(
      "`rhat_lt` must be `logical` where `0 < length(rhat_lt) <= rhat_it`"
    )
  }
  if (all(rhat_lt) && length(rhat_lt) >= rhat_it) {
    iters <- paste(i, if (i == 1L) "iteration" else "iterations")
    rlang::inform(paste0(
      "Converged in ", iters, "\n",
      rhat_msg
    ))
  } else {
    rlang::warn(paste("Sampling did not converge in", i, "iterations"))
    rlang::inform(rhat_msg)
  }
  invisible(NULL)
}

#' Create `{mice}`-Friendly `seed`
#'
#' @param seed A scalar `integer`, `NA`, or `NULL`
#'
#' @return Converts `NULL` to `NA`, otherwise returns input or errors
#'
#' @keywords internal
fm_mice_seed <- function(seed) {
  seed <- fm_assert_seed(seed)
  if (is.null(seed)) NA_integer_ else seed
}


#' Create `{rngtools}`-Friendly `seed`
#'
#' @inheritParams fm_mice_seed
#'
#' @return Converts `NA` to `NULL`, otherwise returns input or errors
#'
#' @keywords internal
fm_rng_seed <- function(seed) {
  seed <- fm_assert_seed(seed)
  if (is.null(seed)) return(seed)
  if (is.na(seed)) NULL else seed
}


#' Helper Function for Setting Row and Column Names
#'
#' @param x An object to set names for. Must have at least 2 dimensions to use
#'   `fm_set_colnames()`.
#' @param names A `character` vector of row or column names
#'
#' @return `x`, with (re-)named rows or columns
#'
#' @keywords internal
#'
#' @name fm_set_names
NULL

#' @rdname fm_set_names
#'
#' @keywords internal
fm_set_colnames <- function(x, names) {
  if (!((is.vector(names) && length(names) == NCOL(x)) || is.null(names))) {
    rlang::abort("`names` must be a vector with length equal to `NCOL(x)`")
  }
  colnames(x) <- names
  x
}

#' @rdname fm_set_names
#'
#' @keywords internal
fm_set_rownames <- function(x, names) {
  if (!((is.vector(names) && length(names) == NROW(x)) || is.null(names))) {
    rlang::abort("`names` must be a vector with length equal to `NROW(x)`")
  }
  rownames(x) <- names
  x
}


#' Calculate Greatest Common Divisor of Positive Integers
#'
#' @param ... Numeric vectors containing integer set for GCD calculation
#'
#' @return A positive scalar `integer` containing the GCD of the inputs
#'
#' @keywords internal
fm_gcd <- function(...) {
  # Check and combine arguments
  x <- vctrs::vec_c(..., .ptype = integer())
  if (anyNA(x)) rlang::abort("Inputs may not contain missing values")
  if (any(x <= 0L)) rlang::abort("All inputs must be integers >= 0")

  # Special case - x is empty
  if (length(x) == 0L) return(integer())

  # Get GCD candidates
  i <- seq_len(min(x))

  # Eliminate sequentially
  for (n in x) {
    i <- i[n %% i == 0L]
    if (length(i) == 1L) break
  }

  # Return largest remaining
  max(i)
}
