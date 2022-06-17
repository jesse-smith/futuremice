#' Combine a List of \code{\link[mice]{mids}} Objects
#'
#' Combines a list of \code{\link[mice]{mids}} objects into a single `mids`
#' object. The resulting number of imputed data sets is equal to the sum of the
#' number of imputed data sets in each list element.
#'
#' The `call` and `seed` arguments are primarily used by
#' \code{\link[future_mice]{future_mice()}} and
#' \code{\link[future_mids]{future_mids()}}; they allow modification of the
#' `mids` object to match the equivalent \code{\link[mice:mice]{mice::mice()}}
#' output exactly.
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
#' @export
ibindlist <- function(mids_list, call = NULL, seed = NULL, last_seed_value = NULL) {
  # Combine into single `mids` object
  mids <- purrr::reduce(mids_list, mice::ibind)

  # Set `ignore` attribute
  mids$ignore <- mids_list[[1L]]$ignore

  # Set `seed` attribute
  if (!is.null(seed)) mids$seed <- mice_seed(seed)

  # Set `lastSeedValue` attribute
  if (!is.null(last_seed_value)) mids$lastSeedValue <- last_seed_value

  # Set `call` attribute
  if (is.null(call)) {
    call <- mids_list[[1L]]$call
  } else if (!rlang::is_call(call)) {
    rlang::abort("`call` must be a call object or `NULL`")
  }
  call_arg_nms <- c(
    rlang::call_args_names(call),
    rlang::fn_fmls_names(rlang::call_fn(call))
  )
  call_arg_nms <- call_arg_nms[nchar(call_arg_nms) > 0L]
  if ("m" %in% call_arg_nms) call$m <- mids$m
  if ("maxit" %in% call_arg_nms) call$maxit <- mids$maxit
  if ("seed" %in% call_arg_nms && !is.null(seed)) call$seed <- mids$seed
  mids$call <- call

  # Set names of imputation data frames
  nms <- as.character(seq_len(mids$m))
  mids$imp <- purrr::map(mids$imp, fm_set_colnames, names = nms)

  # Return
  mids
}


#' Helper Function for Setting Column Names
#'
#' @param x An object to set column names for. Must have at least 2 dimensions.
#' @param names A character vector of column names
#'
#' @return `x`, with (re-)named columns
#'
#' @keywords internal
fm_set_colnames <- function(x, names) {
  colnames(x) <- names
  x
}
