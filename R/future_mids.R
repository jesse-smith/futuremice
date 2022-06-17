future_mids <- function(
  obj,
  newdata = NULL,
  maxit = 1L,
  chunk_size = 1L,
  rhat_thresh = 1.05,
  rhat_it = 3L,
  progressor = NULL,
  ...
) {
  # Extract number of chains
  m <- obj$m
  # Get number of chains per call: largest integer <= `chunk_size` that evenly divides `m`
  f_m <- seq_len(chunk_size)
  f_m <- max(f_m[m %% f_m == 0L])
  # Get average number of calls per future
  chunk_size <- as.integer(max(1, round(chunk_size / f_m)))
  # Get number of futures
  map_m <- m %/% f_m

  # Collect additional arguments
  dots <- rlang::list2(...)

  # Extract call
  call <- obj$call

  # Extract seed
  seed <- obj$seed

  # Initialize progress bar
  if (!(is.null(progressor) || inherits(progressor, "progressor"))) {
    rlang::abort(
      "`progressor` must be `NULL` or a `progressor` function from {progressr}"
    )
  } else {
    progressor <- progressr::progressor(map_m * maxit)
  }

  # Split `mids` object into list
  mids_list <- isplit(obj, chunk_size = chunk_size)

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
    rhat <- tail(c(rhat, rhat_max(mids)), rhat_it)
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
