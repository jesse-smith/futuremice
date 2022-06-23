#' Compare Two `mids` Objects and Show Summary
#'
#' `compare_mids()` is a combination of
#' \code{\link[base:all.equal]{all.equal()}} and
#' \code{\link[base:identical]{identical()}} for `mids` objects. Like
#' `all.equal()`, it displays a summary of element-wise differences (it also
#' explicitly lists elements that are equal). However, rather than outputting
#' these differences as a `character` vector if found, it always returns either
#' `TRUE` or `FALSE`. If `quiet = FALSE`, the return value is invisible.
#'
#' Proper random number generation requires different methods when performing
#' operations in parallel; thus, is it generally not possible to exactly
#' replicate `mids` objects produced sequentially when imputing chains in
#' parallel. In particular, the imputed values will differ, as well as any
#' functions of those values (`chainMean`, `chainVar`, `loggedEvents`). This
#' function takes the stance that imputations performed by functions in the
#' same package should be consistent, but functions from different packages
#' may handle random number generation differently. Thus, if `compare_mids()`
#' can detect that two functions came from the same package, it will compare
#' the full RNG-related elements by default. Otherwise, it will check that
#' attributes independent of random number generation are the same, but it will
#' allow differences between attributes and data dependent on the RNG state. If
#' RNG-related attributes are ignored, the RNG state itself is also ignored, so
#' differences in `seed` and `lastSeedValue` are also allowed in this case.
#'
#' @param x,y `mids` objects to compare, as created by
#'   \code{\link[mice:mice]{mice::mice()}},
#'   \code{\link[futuremice:future_mice]{future_mice()}},
#'   \code{\link[mice:mice.mids]{mice::mice.mids()}},
#'   \code{\link[futuremice:future_mids]{future_mids()}}, etc.
#' @param show_msg Should the message displaying element-wise comparisons be
#'   shown?
#' @param show_rtn Should the return value be visible? The default is the
#'   opposite of `show_msg`, so that either the message or return value is
#'   shown, but not both.
#' @param ignore_rng Should elements or components of elements affected by the
#'   RNG state be ignored? See Details for more information. The default is to
#'   ignore when different package namespaces are detected in the `mids$call`,
#'   otherwise to check.
#' @param ignore_call,ignore_version,ignore_date Should the corresponding
#'   `mids` element (`mids$call`, `mids$version`, `mids$date`, respectively) be
#'   ignored in equality comparison? These elements do not usually impact
#'   results if all other elements are identical and can thus usually be safely
#'   ignored.
#'
#' @return `TRUE` or `FALSE`
#'
#' @export
compare_mids <- function(
  x,
  y,
  show_msg = TRUE,
  show_rtn = !show_msg,
  ignore_rng = NULL,
  ignore_call = TRUE,
  ignore_version = TRUE,
  ignore_date = TRUE
) {
  # Check arguments
  fm_assert_mids(x)
  fm_assert_mids(y)
  fm_assert_bool(show_msg)
  fm_assert_bool(show_rtn)
  fm_assert_bool(ignore_call)
  fm_assert_bool(ignore_version)
  fm_assert_bool(ignore_date)

  # Check and (possibly) set `ignore_rng`
  if (is.null(ignore_rng)) {
    x_ns_nm <- fm_call_ns_name(x$call, error = FALSE)
    y_ns_nm <- fm_call_ns_name(y$call, error = FALSE)
    ignore_rng <- is.null(x_ns_nm) || is.null(y_ns_nm) || x_ns_nm != y_ns_nm
  } else if (!rlang::is_bool(ignore_rng)) {
    rlang::abort("`ignore_rng` must be `TRUE`, `FALSE`, or `NULL`")
  }

  # Elements in one object but not the other
  el_x_not_y <- setdiff(names(x), names(y))
  el_y_not_x <- setdiff(names(y), names(x))

  # Shared elements
  el_shared <- intersect(names(x), names(y))
  # Ignored elements
  el_ignored <- character()

  # Prepare for equality testing
  if (ignore_call) {
    x$call <- NULL
    y$call <- NULL
    el_ignored <- c(el_ignored, "call")
  }
  if (ignore_rng) {
    if (!"imp" %in% c(el_x_not_y, el_y_not_x)) {
      x$imp <- purrr::map(x$imp, vctrs::vec_ptype)
      y$imp <- purrr::map(y$imp, vctrs::vec_ptype)
    }
    if (!"chainMean" %in% c(el_x_not_y, el_y_not_x)) {
      x$chainMean <- dimnames(x$chainMean)
      y$chainMean <- dimnames(y$chainMean)
    }
    if (!"chainVar" %in% c(el_x_not_y, el_y_not_x)) {
      x$chainVar <- dimnames(x$chainVar)
      y$chainVar <- dimnames(y$chainVar)
    }
    x$seed <- NULL
    y$seed <- NULL
    x$lastSeedValue <- NULL
    y$lastSeedValue <- NULL
    x$loggedEvents <- NULL
    y$loggedEvents <- NULL
    el_ignored <- c(el_ignored, "seed", "lastSeedValue", "loggedEvents")
  }
  if (ignore_version) {
    x$version <- NULL
    y$version <- NULL
    el_ignored <- c(el_ignored, "version")
  }
  if (ignore_date) {
    x$date <- NULL
    y$date <- NULL
    el_ignored <- c(el_ignored, "date")
  }
  if (!"formulas" %in% c(el_x_not_y, el_y_not_x)) {
    x$formulas <- purrr::map(rlang::as_quosures(x$formulas), rlang::quo_squash)
    y$formulas <- purrr::map(rlang::as_quosures(y$formulas), rlang::quo_squash)
  }

  # Handle seed
  if (rlang::is_na(x$seed) || rlang::is_integerish(x$seed)) x$seed <- as.integer(x$seed)
  if (rlang::is_na(y$seed) || rlang::is_integerish(y$seed)) y$seed <- as.integer(y$seed)

  # Remove ignored elements from `el_shared`
  el_shared <- setdiff(el_shared, el_ignored)

  # Remove unshared elements from `el_ignored`
  el_ignored <- setdiff(el_ignored, c(el_x_not_y, el_y_not_x))

  # Test for equality in shared elements
  is_eq <- purrr::map_lgl(
    setdiff(el_shared, el_ignored),
    ~ identical(x[[.x]], y[[.x]])
  )

  el_eq <- el_shared[is_eq]
  el_not_eq <- el_shared[!is_eq]

  # Create and show message if not `quiet`
  if (show_msg) {
    # Name elements
    names(el_x_not_y) <- rep("x", NROW(el_x_not_y))
    names(el_y_not_x) <- rep("x", NROW(el_y_not_x))
    names(el_shared) <- rep("i", NROW(el_shared))
    names(el_eq) <- rep("v", NROW(el_eq))
    names(el_not_eq) <- rep("x", NROW(el_not_eq))
    names(el_ignored) <- rep("i", NROW(el_ignored))

    # Fill in empty sets
    if (NROW(el_x_not_y) == 0L) el_x_not_y <- c(v = "None")
    if (NROW(el_y_not_x) == 0L) el_y_not_x <- c(v = "None")
    if (NROW(el_eq) == 0L) el_eq <- c(x = "None")
    if (NROW(el_not_eq) == 0L) el_not_eq <- c(v = "None")
    if (NROW(el_ignored) == 0L) el_ignored <- c(i = "None")

    rlang::inform(rlang::format_error_bullets(c(
      "Elements of `x` not in `y`",
      el_x_not_y,
      "Elements of `y` not in `x`",
      el_y_not_x,
      "Shared elements with differences:",
      el_not_eq,
      "Shared elements without differences:",
      el_eq,
      "Shared elements ignored:",
      el_ignored
    )))
  }

  # Test for equality
  eq <- identical(x, y)

  if (show_rtn) eq else invisible(eq)
}


#' Detect Namespace of Function Call
#'
#' Tries to detect the namespace of a call automatically. It will first try
#' to extract the namespace using `rlang::call_ns()`; if this fails, it will
#' use the supplied `env` (`rlang::caller_env()`, by default) to attempt to
#' extract the namespace from the call's function. The latter method is similar
#' to `rlang::call_fn()`, which is now deprecated due to inconsistent results.
#' It is not yet clear whether this works well enough to retain the behavior in
#' practice.
#'
#' @param call A `call` object to extract a `namespace` from
#' @param env The environment to use to lookup the function definition from
#'   `call` if the call is not explicitly namespaced
#' @param error Should the function throw an error if `call` is not a `call`
#'   object?
#'
#' @return The name of the associated namespace, if detected; if not, returns
#'   `NULL`
#'
#' @keywords internal
fm_call_ns_name <- function(call, env = rlang::caller_env(), error = TRUE) {
  # Check arguments
  fm_assert_bool(error)
  if (!rlang::is_environment(env)) rlang::abort("`env` must be an environment")
  # Process and check call
  env <- rlang::get_env(call, env = env)
  call <- rlang::expr(!!call)
  if (!rlang::is_call(call)) {
    if (error) rlang::abort("`call` must be a call object") else return(NULL)
  }
  ns_name <- rlang::call_ns(call)
  if (is.null(ns_name)) {
    # Use internals of `rlang::call_fn()` - now deprecated
    ns <- rlang::eval_bare(rlang::node_car(call), env = env)
    ns_name <- rlang::ns_env_name(ns)
  }
  if (is.na(ns_name) || NROW(ns_name) == 0L || nchar(ns_name) == "0L") {
    ns_name <- NULL
  }
  ns_name
}
