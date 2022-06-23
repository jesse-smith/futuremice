# Exports ----------------------------------------------------------------------


#' Parallelize \code{\link[mice:mice.mids]{`mice::mice.mids()`}}  Using `{future}`
#'
#' `future_mids()` is analogous to `mice::mice.mids()`, but parallelizes chains
#' using the `{furrr}` package and stops early if convergence is detected using
#' the R-hat statistic
#' (see the \code{\link[futuremice:future_mice]{future_mice()}} documentation
#' for details).
#'
#' @param obj A `mids` object, as created by
#'   \code{\link[mice:mice]{mice::mice()}},
#'   \code{\link[futuremice:future_mice]{future_mice()}},
#'   or `future_mids()` (this function)
#' @param update_call Should `mids$call` be set to new `future_mids()` call or
#'   left unchanged?
#' @inheritParams mice::mice.mids
#' @inherit future_mice params return
#'
#' @examples
#'
#' # Run in parallel (just two cores to avoid hogging resources)
#' # Picking a number of workers that divides `m` evenly can help performance
#' future::plan("multisession", workers = pmin(2L, future::availableCores()))
#'
#' # Run `mice::mice()`
#' # `m` and `maxit` are small here to keep runtime short
#' mids <- mice::mice(mice::nhanes, m = 2L, maxit = 1L)
#'
#' # Run for additional iteration
#' mids <- future_mids(mids, maxit = 1L)
#'
#' \dontrun{
#' # Run until convergence
#' mids <- future_mids(mids, maxit = 100L)
#' mids
#' }
#'
#' # Reset future plan
#' future::plan("sequential")
#'
#' @export
future_mids <- function(
  obj,
  newdata = NULL,
  maxit = 100L,
  quiet = FALSE,
  chunk_size = 1L,
  rhat_thresh = 1.05,
  rhat_it = 3L,
  progressor = NULL,
  update_call = TRUE,
  ...
) {
  # Ensure `.Random.seed` is preserved to mimic `mice::mice.mids()` behavior
  withr::local_preserve_seed()

  # Check arguments
  fm_assert_mids(obj)
  fm_assert_count(maxit)
  fm_assert_bool(quiet)
  fm_assert_count(chunk_size)
  fm_assert_num(rhat_thresh)
  fm_assert_count(rhat_it)
  fm_assert_bool(update_call)


  # Calculate parallelization parameters
  pp <- fm_parallel_params(
    m = obj$m,
    chunk_size = chunk_size,
    maxit = maxit,
    seed = obj$seed
  )

  # Collect additional arguments
  dots <- rlang::list2(...)

  # Create call
  call <- if (update_call) rlang::caller_call(n = 0L) else obj$call

  # Extract current iterations
  it <- obj$iteration

  # Initialize progress bar
  progressor <- fm_progressor(pp, progressor = progressor)

  # Split `mids` object into list
  mids_list <- isplit(obj, chunk_size = chunk_size)

  # Create `{furrr}` options
  f_opts <- fm_furrr_opts(pp)

  # Loop for up to maxit iterations
  for (i in seq.int(it + 1L, it + maxit, by = 1L)) {
    # Update mids list
    mids_list <- furrr::future_map(
      mids_list,
      fm_mids,
      newdata = newdata,
      progressor = progressor,
      mice_args = dots,
      .options = f_opts
    )

    # Reduce and update R-hat
    mids <- ibindlist(
      mids_list,
      call = call,
      seed = fm_mice_seed(pp$seed),
      last_seed_value = .Random.seed
    )
    num_rhat_max <- rhat_max(mids, it = rhat_it)
    rhat_lt <- ifelse(is.na(num_rhat_max), FALSE, num_rhat_max < rhat_thresh)
    rhat_msg <- paste("R-hat:", paste0(round(num_rhat_max, 3L), collapse = "/"))
    rhat_msg <- paste(rhat_msg, "<", rhat_thresh)

    # Update progress
    progressor(message = rhat_msg, amount = 0)

    # Break if criteria are met
    if (all(rhat_lt) && NROW(rhat_lt) >= rhat_it) break
  }

  # Show ending message
  if (!quiet) fm_exit_msg(i, rhat_lt, rhat_it, rhat_msg)

  # Return
  mids
}


# Helpers ----------------------------------------------------------------------


#' `{furrr}`-Friendly `mice::mice.mids()` w/ Progress Updates
#'
#' @param mids An object of class `mids`, typically produced by a previous call
#'   to `fm_mice()`
#' @param maxit The number of additional Gibbs sampling iterations
#' @param newdata An optional `data.frame` for which multiple imputations are
#'   generated according to the model in `obj`
#' @inheritParams fm_mice
#'
#' @return A `mids` object (*m*ultiply *i*mputed *d*ata *s*et) with additional
#'   iterations
#'
#' @keywords internal
fm_mids <- function(mids, mice_args, progressor, newdata = NULL) {
  # Remove seed
  mice_args$seed <- NULL
  # Create args
  mids_args <- list(obj = mids, newdata = newdata, printFlag = FALSE)
  mids_args <- c(mids_args, mice_args)
  # Call `mice.mids`
  mids <- do.call(mice::mice.mids, mids_args)

  progressor()

  mids
}
