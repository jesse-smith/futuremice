# Exports ----------------------------------------------------------------------


#' Parallelize \code{\link[mice:mice]{`mice::mice()`}}  Using `{future}`
#'
#' `future_mice()` parallelizes chains in Multivariate Imputation using Chained
#' Equations (MICE) using the `{furrr}` package to create
#' \code{\link[future]{future}}s for chains. Chains are also assessed for
#' convergence using the R-hat (potential scale reduction factor) statistic
#' computed by \code{\link[rstan:Rhat]{rstan::Rhat()}}; if the largest R-hat is
#' less than `rhat_thresh` for `rhat_it` iterations, the function returns early
#' (without completing `maxit` iterations). This can save a significant amount
#' of computation and manual convergence checking, and it often works well in
#' practice. However, a "good" R-hat is neither a necessary nor sufficient
#' condition for MCMC convergence, nor is it a substitute for checking
#' imputation quality once convergence is achieved.
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
#' `mice()`. In particular, the `call` attribute contains the call to
#' `future_mice()`, rather than a call to `mice()`. The `lastSeedValue` should
#' be equivalent, but does not function identically in subsequent calls to
#' `mice.mids()` and `future_mids()`.
#'
#' @inherit mice::mice params return
#' @param maxit A scalar giving the maximum number of iterations.
#'   `future_mice()` will use less than `maxit` iterations if convergence
#'   criteria are met; because of this, the default is `maxit = 50`, which is
#'   much larger than the `{mice}` default of `maxit = 5` but is large enough to
#'   "just work" in many situations without potentially running for days on end
#'   if convergence is not achieved.
#' @param quiet Should convergence messages and warning be suppressed?
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
#' @examples
#' # Run imputations in parallel (just two to avoid hogging resources)
#' # Picking a number of workers that divides `m` evenly can help performance
#' future::plan("multisession", workers = pmin(2L, future::availableCores()))
#'
#' # Use just like `mice::mice()` - examples from {mice} documentation
#' mids <- future_mice(mice::nhanes, m = 2L, maxit = 1L)
#'
#' \dontrun{
#' # Run until convergence (`maxit = 100L` by default)
#' mids <- future_mice(mice::nhanes, m = 2L)
#' }
#'
#' mids
#'
#' # List the actual imputations for BMI
#' mids$imp$bmi
#'
#' # First completed data matrix
#' mice::complete(mids)
#'
#' # Reset future plan
#' future::plan("sequential")
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
  maxit = 100L,
  quiet = FALSE,
  seed = NA,
  data.init = NULL,
  chunk_size = 1L,
  rhat_thresh = 1.05,
  rhat_it = 3L,
  progressor = NULL,
  ...
) {
  # Ensure `.Random.seed` is preserved to mimic `mice::mice()` behavior
  withr::local_preserve_seed()

  # Check arguments
  fm_assert_count(maxit)
  fm_assert_bool(quiet)
  fm_assert_count(chunk_size)
  fm_assert_num(rhat_thresh)
  fm_assert_count(rhat_it)

  # Get parallelization parameters
  pp <- fm_parallel_params(
    m = m, chunk_size = chunk_size, maxit = maxit, seed = seed
  )

  # Collect mice::mice arguments
  mice_args <- fm_mice_args(m = pp$n_chains, .args = fm_caller_args(n = 1L))

  # Create call
  call <- rlang::caller_call(n = 0L)

  # Initialize progress bar
  progressor <- fm_progressor(pp, progressor = progressor)

  # Set furrr options - RNG seeds and chunk size
  f_opts <- fm_furrr_opts(pp)

  # Run first map iteration to get list of mids objects
  init_args <- mice_args
  init_args$maxit <- 1L
  mids_list <- furrr::future_map(
    seq_len(pp$n_calls),
    fm_mice,
    progressor = progressor,
    mice_args = init_args,
    .options = f_opts
  )

  # Reduce and calculate R-hat
  mids <- ibindlist(
    mids_list,
    call = call,
    seed = fm_mice_seed(pp$seed),
    last_seed_value = .Random.seed
  )
  num_rhat_max <- rhat_max(mids)
  rhat_lt <- if (is.na(num_rhat_max)) FALSE else num_rhat_max < rhat_thresh
  rhat_msg <- paste("R-hat:", round(num_rhat_max, 3L))
  rhat_msg <- paste(rhat_msg, "<", rhat_thresh)

  # Display progress
  progressor(message = rhat_msg, amount = 0)

  # Finish if criteria are met
  if ((rhat_lt && rhat_it == 1L) || maxit == 1L) {
    if (!quiet) fm_exit_msg(1L, rhat_lt, rhat_it, rhat_msg)
    return(mids)
  }

  # Remove standard mice arguments from mice_args (don't need to pass again)
  mice_fmls_nms <- rlang::fn_fmls_names(mice::mice)
  mice_args <- mice_args[setdiff(names(mice_args), mice_fmls_nms)]

  # Continue until convergence or `maxit` is reached
  future_mids(
    mids,
    newdata = NULL,
    maxit = maxit - 1L,
    quiet = quiet,
    chunk_size = chunk_size,
    rhat_thresh = rhat_thresh,
    rhat_it = rhat_it,
    progressor = progressor,
    update_call = FALSE,
    !!!mice_args
  )
}


# Helpers ----------------------------------------------------------------------


#' Combine Arguments in `future_mice()`
#'
#' Helper function to combine and parse named arguments + dots in
#' `future_mice()`
#'
#' @param m The number of chains per `mice::mice()` call
#' @param .args A named list of arguments from the `future_mice()` call
#'
#' @return A list containing arguments to pass to `mice::mice()`
#'
#' @keywords internal
fm_mice_args <- function(m, .args = fm_caller_args(n = 2L)) {
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
  .args$m <- m
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


#' Construct List of Arguments from Matched Caller Function
#'
#' @inheritParams rlang::caller_call
#' @inheritParams rlang::call_match
#'
#' @inherit rlang::call_args return
#'
#' @keywords internal
fm_caller_args <- function(
    n = 1,
    ...,
    defaults = FALSE,
    dots_env = NULL,
    dots_expand = TRUE
) {
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
