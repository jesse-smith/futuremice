# Exports ----------------------------------------------------------------------


#' Combine a List of \code{\link[mice]{mids}} Objects
#'
#' Combines a list of \code{\link[mice]{mids}} objects into a single `mids`
#' object. The resulting number of imputed data sets is equal to the sum of the
#' number of imputed data sets in each list element.
#'
#' The `call`, `seed`, and `last_seed_value` arguments are primarily used by
#' \code{\link[futuremice:future_mice]{future_mice()}} and
#' \code{\link[futuremice:future_mids]{future_mids()}}; they allow modification
#' of the `mids` object to match the equivalent
#' \code{\link[mice:mice]{mice::mice()}} output exactly.
#'
#' @param mids_list List of `mids` objects to combine
#' @param call An optional call to use for the `call` attribute of the resulting
#'   `mids` object. The default uses the call in `mids_list[[1]]$call`.
#' @param seed An optional integer to set as the `seed` attribute of the
#'   resulting `mids` object.
#' @param last_seed_value An optional \code{\link[base:Random]{.Random.seed}}
#'   value to set as the `lastSeedValue` attribute of the resulting `mids`
#'   object
#'
#' @return A combined `mids` object
#'
#' @examples
#' # Create individual `mids` objects
#' mids1 <- mice::mice(mice::nhanes, m = 1L, maxit = 1L, seed = 1L)
#' mids2 <- mice::mice(mice::nhanes, m = 2L, maxit = 1L, seed = 2L)
#' mids3 <- mice::mice(mice::nhanes, m = 3L, maxit = 1L, seed = 3L)
#'
#' # Combine imputations
#' mids6 <- ibindlist(list(mids1, mids2, mids3))
#' mids6
#'
#' @export
ibindlist <- function(
  mids_list,
  call = NULL,
  seed = NULL,
  last_seed_value = NULL
) {
  # Reduce to single `mids` object
  mids <- purrr::reduce(mids_list, mice::ibind)

  # Convert `m` to numeric
  mids$m <- as.numeric(mids$m)

  # Set `ignore` attribute
  mids$ignore <- mids_list[[1L]]$ignore

  # Set `seed` attribute
  if (!is.null(seed)) mids$seed <- fm_mice_seed(seed)

  # Set names of imputation data frames
  mids$imp <- purrr::map(
    mids$imp,
    fm_set_colnames,
    names = as.character(seq_len(mids$m))
  )
  mids$imp <- purrr::map(
    mids$imp,
    ~ fm_set_rownames(.x, as.character(rownames(.x)))
  )

  # Set `lastSeedValue` attribute
  if (!is.null(last_seed_value)) {
    mids$lastSeedValue <- fm_assert_vec_int(last_seed_value)
  }

  # Set `call` attribute
  if (is.null(call)) call <- mids_list[[1L]]$call
  mids <- fm_set_call(mids, call = call)

  # Set `loggedEvents` attribute
  mids <- fm_ibind_logged_events(mids, mids_list)

  # Return
  mids
}


# Helpers ----------------------------------------------------------------------


#' Helper Function for Setting `mids$call`
#'
#' @param mids A `mids` object to update
#' @param call A `call` object to use as `mids$call` after standardization. Can
#'   be `NULL`, in which case `mids` is returned unaltered.
#'
#' @return The `mids` object with an updated `call` (if `call` is not `NULL`).
#'   Note that if `call` has arguments `m`, `maxit`, or `seed`, their values
#'   are updated to reflect the corresponding attribute in `mids`.
#'
#' @keywords internal
fm_set_call <- function(mids, call) {
  # Handle special cases
  if (is.null(call)) {
    return(mids)
  } else if (!rlang::is_call(call)) {
    rlang::abort("`call` must be a call object or `NULL`")
  }
  # Updates call arguments
  call_arg_nms <- c(
    rlang::call_args_names(call),
    rlang::fn_fmls_names(rlang::call_fn(call))
  )
  call_arg_nms <- call_arg_nms[nchar(call_arg_nms) > 0L]
  if ("m" %in% call_arg_nms) call$m <- mids$m
  if ("maxit" %in% call_arg_nms) call$maxit <- mids$maxit
  if ("seed" %in% call_arg_nms) call$seed <- mids$seed
  # Set
  mids$call <- call
  # Return
  mids
}


#' Helper Function for Setting `mids$loggedEvents`
#'
#' @param mids A `mids` object to update
#' @param mids_list A list of `mids` objects with `loggedEvents` to extract and
#'   combine
#'
#' @return `mids` with `loggedEvents` from `mids_list`
#'
#' @keywords internal
fm_ibind_logged_events <- function(mids, mids_list) {
  # Combine loggedEvents from `mids_list`
  loggedEvents <- purrr::map_dfr(mids_list, ~ .x$loggedEvents, .id = "i")
  # Update if any exist
  if (NROW(loggedEvents) > 0L) {
    loggedEvents$im <- loggedEvents$im + as.integer(loggedEvents$i) - 1L
    loggedEvents$i  <- NULL
    mids$loggedEvents <- loggedEvents
  }
  # Return
  mids
}
