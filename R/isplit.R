isplit <- function(mids, chunk_size = 1L) {
  m <- mids$m
  # Get number of chains per call: largest integer <= `chunk_size` that evenly divides `m`
  f_m <- seq_len(chunk_size)
  f_m <- max(f_m[m %% f_m == 0L])
  # Get average number of calls per future
  chunk_size <- as.integer(max(1, round(chunk_size / f_m)))
  # Get number of futures
  map_m <- m %/% f_m

  # Create prototype to update for `mids` in list
  mids_ptype <- mids
  # Set chain number to chains per call
  mids_ptype$m <- f_m
  # Get sequence of chains per call and update chain names
  seq_chains <- seq_len(f_m)
  chain_dims <- dimnames(mids$chainMean)
  chain_dims[[3L]] <- paste("Chain", seq_chains)
  # Initialize list
  mids_list <- list()
  for (i in seq_len(map_m)) {
    # Get chain locations
    chains <- i - 1L + seq_chains
    # Subset imp
    mids_ptype$imp <- purrr::map(
      mids$imp,
      ~ magrittr::set_colnames(.x[, chains, drop = FALSE], seq_chains)
    )
    # Subset chainMean and chainVar
    mids_ptype$chainMean <- mids$chainMean[, , chains, drop = FALSE]
    mids_ptype$chainVar  <- mids$chainVar[, , chains, drop = FALSE]
    dimnames(mids_ptype$chainMean) <- chain_dims
    dimnames(mids_ptype$chainVar) <- chain_dims
    # Add to list
    mids_list[[i]] <- mids_ptype
  }
  mids_list
}
