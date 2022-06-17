#' Parallelize \code{\link[mice:mice]{`mice::mice()`}}  Using `{future}`
#'
#' `future_mice()` parallelizes chains in Multivariate Imputation using Chained
#' Equations (MICE) using the `{furrr}` package to create
#' \code{\link[future]{future}}s for chains. Chains are also assessed for
#' convergence using the R-hat (potential scale reduction factor) statistic
#' computed by \code{\link[rstan:Rhat]{rstan::Rhat()}}; if the largest R-hat is
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
#' `future_mice()` mimics the `mice::mice()` interface as closely as possible;
#' however, some shared parameters have different defaults than their `{mice}`
#' equivalents. Notably, the default `maxit` is much larger than in `{mice}`;
#' this is because `maxit` is an upper bound in `future_mice()`, rather than an
#' exact number of iterations, as in `mice()`. The default of `100` should be
#' more than enough iterations for most problems; if you need more than `100`
#' iterations for convergence, you may want to check your imputation model for
#' circularity or other stability issues.
#'
#' Additionally, `future_mice()` provides `NULL` defaults for all unset
#' arguments; this is a best practice in `R`. Because of this, passing `NULL`
#' to any argument without an explicit default is the same as not passing that
#' argument, which differs from the behavior of `mice()` in some instances.
#'
#' Finally, some output attributes are not identical to their equivalents in
#' `mice()`. In particular, the `call` attribute contains the call to `future_mice()`,
#' rather than a call to `mice()`. The `lastSeedValue` should be equivalent, but
#' does not function identically in subsequent calls to `mice.mids()` and
#' `future_mids()`.
#'
#' @inherit mice::mice params return
#' @param maxit A scalar giving the maximum number of iterations.
#'   `future_mice()` will use less than `maxit` iterations if convergence
#'   criteria are met; because of this, the default is `maxit = 50`, which is
#'   much larger than the `{mice}` default of `maxit = 5` but is large enough to
#'   "just work" in many situations without potentially running for days on end
#'   if convergence is not achieved.
#' @param chunk_size The average number of chains per future. Differs from the
#'   usual `{future}` parameter in that multiple chains ("chunks") will be
#'   evaluated in a single call to `mice::mice()` if there is an integer `i`
#'   such that `1 < i <= chunk_size` and `m %% i == 0`.
#' @param rhat_thresh The R-hat threshold used to assess convergence.
#'   Convergence is defined as `all(tail(rhat, rhat_it) < rhat_thresh)`.
#' @param rhat_it The number of iterations used to assess convergence.
#'   Convergence is defined as `all(tail(rhat, rhat_it) < rhat_thresh)`.
#' @param seed Seed for random number generation; either a scalar `integer`,
#'   `NA`, or `NULL`. This seed is not used directly in `mice::mice()`; instead,
#'   it is used to generate separate RNG streams for each `future` using the
#'   parallel-safe L'Ecuyer-CMRG algorithm.
#' @param progressor An optional \code{\link[progressr]{progressor}}
#'   function to signal progress updates. If supplied, you are responsible for
#'   ensuring that the number of steps in the `progressor` is consistent with
#'   the number of iterations performed in `future_mice()`.
#'
#' @inheritDotParams mice::mice
#'
#' @export
future_mice <- function(
  data,
  m = 5L,
  method = NULL,
  predictorMatrix = NULL,
  ignore = NULL,
  where = NULL,
  blocks = NULL,
  visitSequence = NULL,
  formulas = NULL,
  blots = NULL,
  post = NULL,
  defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
  maxit = 50L,
  seed = NA,
  data.init = NULL,
  chunk_size = 1L,
  rhat_thresh = 1.05,
  rhat_it = 3L,
  progressor = NULL,
  ...
) {
  # Get number of chains per call: largest integer <= `chunk_size` that evenly divides `m`
  f_m <- seq_len(chunk_size)
  f_m <- max(f_m[m %% f_m == 0L])
  # Get average number of calls per future
  chunk_size <- as.integer(max(1, round(chunk_size / f_m)))
  # Get number of futures
  map_m <- m %/% f_m

  # Collect mice::mice arguments
  mice_args <- fm_mice_args(f_m = f_m, .args = caller_args(n = 1L))

  # Initialize mice
  init_args <- mice_args
  init_args$maxit <- 0L
  mids <- eval(rlang::call2(mice::mice, !!!init_args))

  # Create call
  call <- rlang::caller_call(n = 0L)

  # Initialize progress bar
  if (!(is.null(progressor) || inherits(progressor, "progressor"))) {
    rlang::abort(
      "`progressor` must be `NULL` or a `progressor` function from {progressr}"
    )
  } else {
    progressor <- progressr::progressor(map_m * maxit)
  }

  # Set furrr options - RNG seeds and chunk size
  f_opts <- furrr::furrr_options(
    seed = rngtools::RNGseq(map_m, seed = rng_seed(seed), simplify = FALSE),
    globals = character(),
    chunk_size = chunk_size
  )

  # Run first map iteration to get list of mids objects
  mids_list <- furrr::future_map(
    seq_len(map_m),
    fm_mice,
    progressor = progressor,
    mice_args = mice_args,
    .options = f_opts
  )

  # Reduce and calculate R-hat
  mids <- ibindlist(mids_list, call = call, seed = mice_seed(seed), last_seed_value = .Random.seed)
  rhat <- rhat_max(mids)
  rhat_lt <- if (is.na(rhat)) FALSE else rhat < rhat_thresh
  rhat_msg <- paste("R-hat:", round(rhat, 3L))
  rhat_msg <- paste(rhat_msg, "<", rhat_thresh)

  # Display progress
  progressor(message = rhat_msg, amount = 0)

  # Finish if criteria are met
  if ((rhat_lt && rhat_it == 1L) || maxit == 1L) {
    fm_exit_msg(1L, rhat_lt, rhat_it, rhat_msg)
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
      seed = rngtools::RNGseq(map_m, seed = rng_seed(seed), simplify = FALSE),
      globals = character(),
      chunk_size = chunk_size
    )

    # Update mids list
    mids_list <- furrr::future_map(
      mids_list,
      fm_mids,
      progressor = progressor,
      mice_args = mice_args,
      .options = f_opts
    )

    # Reduce and update R-hat
    mids <- ibindlist(mids_list, call = call, seed = mice_seed(seed), last_seed_value = .Random.seed)
    rhat <- utils::tail(c(rhat, rhat_max(mids)), rhat_it)
    rhat_lt <- ifelse(is.na(rhat), FALSE, rhat < rhat_thresh)
    rhat_msg <- paste("R-hat:", paste0(round(rhat, 3L), collapse = "/"))
    rhat_msg <- paste(rhat_msg, "<", rhat_thresh)

    # Update progress
    progressor(message = rhat_msg, amount = 0)

    # Break if criteria are met
    if (all(rhat_lt) && NROW(rhat_lt) >= rhat_it) break
  }

  # Show ending message
  fm_exit_msg(i, rhat_lt, rhat_it, rhat_msg)

  # Return
  mids
}


#' Combine Arguments in `future_mice()`
#'
#' Helper function to combine and parse named arguments + dots in `future_mice()`
#'
#' @param f_m The number of chains per `mice::mice()` call
#' @param .args A named list of arguments from the `future_mice()` call
#'
#' @return A list containing arguments to pass to `mice::mice()`
#'
#' @keywords internal
fm_mice_args <- function(f_m, .args = caller_args(n = 2L)) {
  # Get arg names
  arg_nms <- names(.args)

  # Get formals and names from `mice::mice()`
  mice_fmls <- rlang::fn_fmls(mice::mice)
  mice_fmls_nms <- names(mice_fmls)

  # Get formal names from `future_mice()`
  fm_fmls_nms <- rlang::fn_fmls_names(future_mice)

  # Collect arguments specific to `future_mice()`
  args_fm_only <- setdiff(fm_fmls_nms, mice_fmls_nms)

  # Collect passed `NULL` arguments with no defaults in `mice()`
  mice_no_default <- mice_fmls_nms[purrr::map_lgl(mice_fmls, rlang::is_missing)]
  args_null <- arg_nms[purrr::map_lgl(.args, is.null)]
  args_null_no_default <- intersect(args_null, mice_no_default)

  # Combine and remove collected arguments
  .args[union(args_fm_only, args_null_no_default)] <- NULL

  # Replace `m`
  .args$m <- f_m
  # Do not print updates to stdout
  .args$printFlag <- FALSE

  # Return
  .args
}


#' `{furrr}`-Friendly `mice::mice()` w/ Progress Updates
#'
#' @param .m Sink that allows iteration w/ `{purrr}`-style `map()` functions.
#'   Unused.
#' @inheritParams future_mice
#' @param progressor A `progressor()` from `{progressr}`
#' @param ... Arguments passed on to `mice::mice`. `seed` is ignored.
#'
#' @return A `mids` object (*m*ultiply *i*mputed *d*ata *s*et)
#'
#' @keywords internal
fm_mice <- function(.m, mice_args, progressor) {
  # Handle seed
  RNGkind("L'Ecuyer-CMRG")
  mice_args$seed <- sample.int(.Machine$integer.max, size = 1L)

  mids <- do.call(mice::mice, mice_args)
  progressor()
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
fm_mids <- function(mids, mice_args, progressor) {
  # Remove seed
  mice_args$seed <- NULL

  mids <- do.call(
    mice::mice.mids,
    c(list(obj = mids, newdata = NULL, printFlag = FALSE), mice_args)
  )
  progressor()
  mids
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
    ), use_cli_format = TRUE)
  } else {
    rlang::warn("Sampling did not converge", use_cli_format = TRUE)
    rlang::inform(rhat_msg, use_cli_format = TRUE)
  }
  invisible(NULL)
}

caller_args <- function(n = 1, ..., defaults = FALSE, dots_env = NULL, dots_expand = TRUE) {
  call <- rlang::caller_call(n = n)
  fn <- rlang::call_fn(call)
  rlang::call_args(rlang::call_match(
    call = call, fn = fn,
    ...,
    defaults = defaults,
    dots_env = dots_env,
    dots_expand = dots_expand
  ))
}


mice_seed <- function(seed) {
  if (is.null(seed)) {
    NA
  } else if (is.na(seed) || rlang::is_scalar_integerish(seed, finite = TRUE)) {
    seed
  } else {
    rlang::abort("`seed` must be a scalar integer, `NA`, or `NULL`")
  }
}

rng_seed <- function(seed) {
  if (is.null(seed) || rlang::is_scalar_integerish(seed, finite = TRUE)) {
    seed
  } else if (is.na(seed)) {
    NULL
  } else {
    rlang::abort("`seed` must be a scalar integer, `NA`, or `NULL`")
  }
}
