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
  m <- fm_assert_count(m)
  chunk_size <- fm_assert_count(chunk_size)
  maxit <- fm_assert_count(maxit)
  seed <- fm_assert_seed(seed)
  # Get number of chains per call
  # largest integer <= `chunk_size` that evenly divides `m`
  n_chains <- seq_len(chunk_size)
  n_chains <- max(n_chains[m %% n_chains == 0L])

  # Update chunk_size to reflect (possibly) multiple chains per chunk
  chunk_size <- as.integer(max(1, round(chunk_size / n_chains)))

  # Calculate number of calls
  n_calls <- m %/% n_chains

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
  if (!(is.null(progressor) || inherits(progressor, "progressor"))) {
    rlang::abort(
      "`progressor` must be `NULL` or a `progressor` function from {progressr}"
    )
  } else {
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
  seed_seq <- rngtools::RNGseq(
    parallel_params$n_calls,
    seed = fm_rng_seed(parallel_params$seed),
    simplify = FALSE
  )
  furrr::furrr_options(
    seed = seed_seq,
    globals = character(),
    chunk_size = parallel_params$chunk_size
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
  if (all(rhat_lt) && NROW(rhat_lt) >= rhat_it) {
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
  colnames(x) <- names
  x
}

#' @rdname fm_set_names
#'
#' @keywords internal
fm_set_rownames <- function(x, names) {
  rownames(x) <- names
  x
}
