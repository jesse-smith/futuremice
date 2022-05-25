#' Parallelize \code{\link[mice::mice]{`mice::mice()`}}  Using `{future}`
#'
#' `future_mice()` parallelizes chains in Multivariate Imputation using Chained
#' Equations (MICE) using the `{furrr}` package to create
#' \code{\link[future]{future}}s for chains. Chains are also assessed for
#' convergence using the R-hat (potential scale reduction factor) statistic
#' computed by \code{\link[rstan::Rhat]{rstan::Rhat()}}; if the largest R-hat is
#' less than `rhat_thresh` for `rhat_it` iterations, the function returns early
#' (without completing `maxit` iterations). This can save a significant amount
#' of computation and manual convergence checking. However, a "good" R-hat is
#' not a substitute for assessing imputation quality.
#'
#' MICE is a method for creating multiple imputations (replacement values) for
#' multivariate missing data. The method is based on Fully Conditional
#' Specification (FCS), where each incomplete varaible is imputed by a separate
#' model. The MICE algorithm can impute mixes of continuous, binary, unordered
#' categorical and ordered categorical data. In addition, MICE can impute
#' continuous two-level data and maintain consistency between imputations by
#' means of passive imputation and post-processing. Many diagnostic plots are
#' implemented to inspect the quality of the imputations. See the
#' \code{\link[mice:mice]{mice::mice()}} function and the vignettes on the
#' \href{https://amices.org/mice/}{`{mice}`} package website for details.
#'
#' @inherit mice::mice params return
#' @param maxit A scalar giving the maximum number of iterations. `future_mice()`
#'   will use less than `maxit` iterations if convergence criteria are met.
#' @param chunk_size The average number of chains per future. Differs from the
#'   usual `{future}` parameter in that multiple chains ("chunks") will be
#'   evaluated in a single call to `mice::mice()` if `chunk_size` and `m` share
#'   a greatest common denominator higher than `1`.
#' @param rhat_thresh The R-hat threshold used to assess convergence.
#'   Convergence is defined as `all(tail(rhat, rhat_it) < rhat_thresh)`.
#' @param rhat_it The number of iterations used to assess convergence.
#'   Convergence is defined as `all(tail(rhat, rhat_it) < rhat_thresh)`.
#' @param seed Seed for random number generation; either a length 1 integer or
#'   `NULL`. This seed is not used directly in `mice::mice()`; instead, it is
#'   used to generate separate RNG streams for each `future` using the
#'   parallel-safe L'Ecuyer-CMRG algorithm.
#' @inheritDotParams mice::mice
#'
#' @export
future_mice <- function(
  data,
  predictorMatrix,
  m = 5L,
  maxit = 100L,
  chunk_size = 1L,
  rhat_thresh = 1.05,
  rhat_it = 3L,
  seed = NULL,
  ...
) {
  # Get number of chains per call: greatest common denominator of chunk size & m
  f_m <- seq_len(chunk_size)
  f_m <- max(f_m[m %% f_m == 0L])
  # Get average number of calls per future
  chunk_size <- as.integer(max(1, round(chunk_size / f_m)))
  # Get number of futures
  map_m <- m %/% f_m

  # Collect mice::mice arguments
  mice_args <- future_mice_args(f_m = f_m, ...)

  # Initialize random seed
  seed_is_num <- rlang::is_true(!is.na(seed) && is.numeric(seed))
  seed <- if (seed_is_num) as.integer(seed) else NULL

  # Initialize mice
  init_args <- mice_args
  init_args$maxit <- 0L
  init_args$seed <- if (is.null(seed)) NA else seed
  mids <- eval(rlang::call2(mice::mice, !!!init_args))

  # Create call and ignore
  mids_call <- mids$call
  mids_call$m <- m
  mids_call$maxit <- 1L
  mids_ignore <- mids$ignore

  # Initialize progress bar
  p <- progressr::progressor(map_m * maxit)

  # Set furrr options - RNG seeds and chunk size
  f_opts <- furrr::furrr_options(
    seed = rngtools::RNGseq(map_m, seed = seed, simplify = FALSE),
    globals = character(),
    chunk_size = chunk_size
  )

  # Run first map iteration to get list of mids objects
  mids_list <- furrr::future_map(
    seq_len(map_m),
    f_mice,
    p = p,
    mice_args = mice_args,
    .options = f_opts
  )

  # Reduce and calculate R-hat
  mids <- purrr::reduce(mids_list, mice::ibind)
  rhat <- rhat_max(mids)
  rhat_lt <- if (is.na(rhat)) FALSE else rhat < rhat_thresh
  rhat_msg <- paste("R-hat:", paste0(round(rhat, 3L), collapse = "/"))
  rhat_msg <- paste(rhat_msg, "<", rhat_thresh)

  # Display progress
  p(message = rhat_msg, amount = 0)

  # Finish if criteria are met
  if ((rhat_lt && rhat_it == 1L) || maxit == 1L) {
    future_mice_complete(1L, rhat_lt, rhat_it, rhat_msg)
    # Update call and ignore
    mids$call <- mids_call
    mids$ignore <- mids_ignore
    # Update imp column names
    for (i in seq_along(mids$imp)) {
      colnames(mids$imp[[i]]) <- seq_len(m)
    }
    return(mids)
  }

  # Remove standard mice arguments from mice_args (don't need to pass again)
  mice_fmls_nms <- rlang::fn_fmls_names(mice::mice)
  mice_args <- mice_args[setdiff(names(mice_args), mice_fmls_nms)]

  # Loop for up to maxit iterations
  for (i in seq.int(2L, maxit, by = 1L)) {
    # Update furrr seeds
    if (is.numeric(seed)) seed <- seed + i
    f_opts <- furrr::furrr_options(
      seed = rngtools::RNGseq(map_m, seed = seed, simplify = FALSE),
      globals = character(),
      chunk_size = chunk_size
    )

    # Update mids list
    mids_list <- furrr::future_map(
      mids_list,
      f_mids,
      p = p,
      mice_args = mice_args,
      .options = f_opts
    )

    # Reduce and update R-hat
    mids <- purrr::reduce(mids_list, mice::ibind)
    mids_call$maxit <- i
    rhat <- tail(c(rhat, rhat_max(mids)), rhat_it)
    rhat_lt <- ifelse(is.na(rhat), FALSE, rhat < rhat_thresh)
    rhat_msg <- paste("R-hat:", paste0(round(rhat, 3L), collapse = "/"))
    rhat_msg <- paste(rhat_msg, "<", rhat_thresh)

    # Update progress
    p(message = rhat_msg, amount = 0)

    # Break if criteria are met
    if (all(rhat_lt) && NROW(rhat_lt) >= rhat_it) break
  }

  # Show ending message
  fm_done_msg(i, rhat_lt, rhat_it, rhat_msg)

  # Update call and ignore
  mids$call <- mids_call
  mids$ignore <- mids_ignore
  # Update imp column names
  for (i in seq_along(mids$imp)) {
    colnames(mids$imp[[i]]) <- seq_len(m)
  }

  # Return combined mids object
  mids
}


#' Combine Arguments in `future_mice()`
#'
#' Helper function to combine and parse named arguments + dots in `future_mice()`
#'
#' @param f_m The number of chains per `mice::mice()` call
#' @param ... Additional arguments from the `future_mice()` call
#' @param .args Arguments from the `future_mice()` call
#'
#' @return A list containing arguments to pass to `mice::mice()`
#'
#' @keywords internal
fm_mice_args <- function(f_m, ..., .args = rlang::fn_fmls_syms()) {
  # Update args
  # Only keep arguments shared by `mice::mice()`
  .args <- .args[intersect(names(.args), rlang::fn_fmls_names(mice::mice))]
  # Exclude dots
  .args <- .args[-which(names(.args) == "...")]
  .args$m <- f_m
  .args$maxit <- 1L
  .args$printFlag <- FALSE

  # Gather dots
  dots <- rlang::list2(...)

  # Remove redundant arguments from dots
  dots <- dots[setdiff(names(dots), names(.args))]
  dots <- dots[setdiff(names(dots), rlang::fn_fmls_names(mice::mice.mids))]

  # Combine
  c(.args, dots)
}


#' `{furrr}`-Friendly `mice::mice()` w/ Progress Updates
#'
#' @param .m Sink for using `{purrr}`-style `map()` functions for iteration.
#'   Unused.
#' @inheritParams future_mice
#' @param p A progress bar object from `{progressr}`
#' @param ... Arguments passed on to `mice::mice`. `seed` is ignored.
#'
#' @return A `mids` object (*m*ultiply *i*mputed *d*ata *s*et)
#'
#' @keywords internal
fm_mice <- function(.m, p, mice_args) {
  # Handle seed
  RNGkind("L'Ecuyer-CMRG")
  mice_args$seed <- sample.int(.Machine$integer.max, size = 1L)
  mids <- do.call(mice::mice, mice_args)
  p()
  mids
}


#' `{furrr}`-Friendly `mice::mice.mids()` w/ Progress Updates
#'
#' @param mids An object of class `mids`, typically produced by a previous call
#'   to `fm_mice()`
#' @param maxit The number of additional Gibbs sampling iterations
#' @inheritParams fm_mice
#'
#' @return A `mids` object (*m*ultiply *i*mputed *d*ata *s*et) with additional
#'   iterations
#'
#' @keywords internal
fm_mids <- function(mids, p, mice_args) {
  mids <- do.call(
    mice::mice.mids,
    c(list(mids = mids, newdata = NULL), mice_args)
  )
  p()
  mids
}


#' Throw Messages/Warnings at End of `future_mice()` Execution
#'
#' @param i Integer(ish) representing iteration count
#' @param rhat_lt Logical vector of R-hat comparisons (see Details).
#'   `length(rhat_lt)` must be less than or equal to `rhat_it`
#' @param rhat_it Integer(ish) number of iterations used in R-hat comparison
#' @param rhat_msg Contents of `message` displaying R-hat values for last
#'   `rhat_it` iterations
#'
#' @return `NULL`, invisibly
#'
#' @keywords internal
fm_done_msg <- function(i, rhat_lt, rhat_it, rhat_msg) {
  if (all(rhat_lt) && NROW(rhat_lt) >= rhat_it) {
    iters <- paste(i, if (i == 1L) "iteration" else "iterations")
    rlang::inform(paste0(
      "Converged in ", iters, "\n",
      rhat_msg
    ), use_cli_format = TRUE)
  } else {
    rlang::warn("Sampling did not converge", use_cli_format = TRUE)
    rlang::inform(rhat_msg, use_cli_format = TRUE)
  }
}
