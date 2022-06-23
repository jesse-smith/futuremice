# Exports ----------------------------------------------------------------------


#' Split a `mids` Object into List of `mids`
#'
#' `isplit()` is the inverse of \code{\link[futuremice:ibindlist]{ibindlist()}}
#' and converts a single `mids` object into a list of `mids` objects containing
#' the largest number of chains possible while
#' 1) distributing chains evenly among `mids` objects and
#' 2) keeping the number of chains per object <= `chunk_size`.
#' The resulting number of objects is specified implicitly through `mids$m` and
#' `chunk_size`.
#'
#' @param mids A `mids` object, as created by `mice::mice()`
#' @param chunk_size The number of to allocate to each smaller `mids` object in
#'   the resulting `list`. If `mids$m %% chunk_size != 0`, a smaller
#'   `chunk_size` will be used.
#'
#' @return A `list` of `mids` objects
#'
#' @examples
#' # Create `mids` object
#' mids <- mice::mice(mice::nhanes, m = 2L, maxit = 1L)
#'
#' # Split into two - 1 imputation per object
#' mids_list <- isplit(mids)
#' mids_list
#'
#' @export
isplit <- function(mids, chunk_size = 1L) {
  # Check arguments
  mids <- fm_assert_mids(mids)
  chunk_size <- fm_assert_count(chunk_size)

  # Create parallelization parameters
  pp <- fm_parallel_params(
    m = mids$m,
    chunk_size = chunk_size,
    maxit = mids$iteration,
    seed = mids$seed
  )

  # Create prototype to update for `mids` in list
  mids_ptype <- mids
  # Set chain number to chains per call
  mids_ptype$m <- pp$n_chains
  # Get sequence of chains per call and update chain names
  seq_chains <- seq_len(pp$n_chains)
  chain_dims <- dimnames(mids$chainMean)
  chain_dims[[3L]] <- paste("Chain", seq_chains)
  # Initialize list
  mids_list <- list()
  for (i in seq_len(pp$n_calls)) {
    # Get chain locations
    chains <- i - 1L + seq_chains
    # Subset imp
    mids_ptype$imp <- purrr::map(
      mids$imp,
      ~ fm_set_colnames(.x[, chains, drop = FALSE], seq_chains)
    )
    # Subset chainMean and chainVar
    mids_ptype$chainMean <- mids$chainMean[, , chains, drop = FALSE]
    mids_ptype$chainVar  <- mids$chainVar[, , chains, drop = FALSE]
    dimnames(mids_ptype$chainMean) <- chain_dims
    dimnames(mids_ptype$chainVar) <- chain_dims
    # Subset loggedEvents
    mids_ptype <- fm_isplit_logged_events(mids_ptype, mids$loggedEvents, chains)
    # Add to list
    mids_list[[i]] <- mids_ptype
  }
  mids_list
}


# Helpers ----------------------------------------------------------------------


#' Subset and Insert `mids$loggedEvents` Element
#'
#' @param mids A `mids` object to insert new `loggedEvents` into
#' @param logged_events A `data.frame` containing the event logs from a
#'   `mice::mice()` run
#' @param im The imputations for which the associated logs will be inserted
#'
#' @return `mids` with a new `loggedEvents` element
#'
#' @keywords internal
fm_isplit_logged_events <- function(mids, logged_events, im) {
  # Subset to `im` imputations
  mids$loggedEvents <- logged_events[logged_events$im %in% im,]
  # If no events exist, convert to `NULL`
  if (NROW(mids$loggedEvents) == 0L) {
    # Convert to `list`
    class(mids) <- "list"
    # Remove old `loggedEvents`
    mids$loggedEvents <- NULL
    # Insert `NULL` for `loggedEvents`
    mids <- append(
      mids,
      list(loggedEvents = NULL),
      after = which(names(mids) == "chainVar")
    )
    # Convert back to `mids`
    class(mids) <- "mids"
  }
  mids
}
