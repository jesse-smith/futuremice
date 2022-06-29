#' Make Assertions on Data Types and Arguments
#'
#' @description
#' `fm_assert_mids()` checks for a `mids` object
#' `fm_assert_progressor()` checks for a `progressor` function or `NULL`
#' `fm_assert_bool()` checks for a non-missing, scalar `logical`
#' `fm_assert_count()` checks for a non-missing, finite, scalar integer-ish
#'   value >= 0 and converts its input to `integer`
#' `fm_assert_seed()` checks for a finite, scalar integer-ish value, `NA`,
#'   or `NULL`, and converts non-`NULL` input to `integer`
#' `fm_assert_num()` checks for a non-missing, scalar, finite `numeric` and
#'   converts its input to `double`
#' `fm_assert_vec_int()` checks for non-missing, finite integer-ish values and
#'   converts its input to `integer`
#' `fm_assert_vec_num()` checks for non-missnig, finite values and converts its
#'   input to `double`
#'
#' @param x An object to check
#' @param arg_nm The name of the object to check; can usually be inferred from
#'   the caller environment automatically
#' @param zero_ok Should zero be included in the counting numbers or raise an
#'   error?
#' @param na_ok Should `NA`, `NaN`, or `Inf` values be considered valid?
#'
#' @return If successful, `x`, invisibly; errors if unsuccessful. `x` may be
#'   converted to strictly match the required data type.
#'
#' @keywords internal
#'
#' @name fm_assert
NULL


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_mids <- function(x, arg_nm = rlang::caller_arg(x)) {
  if (mice::is.mids(x)) {
    invisible(x)
  } else {
    rlang::abort(paste0("`", arg_nm, "` must be a `mids` object"))
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_progressor <- function(x, arg_nm = rlang::caller_arg(x)) {
  if (is.null(x) || inherits(x, "progressor")) {
    invisible(x)
  } else {
    rlang::abort(paste0(
      "`", arg_nm, "`",
      " must be `NULL` or a `progressor` function from {progressr}"
    ))
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_bool <- function(x, arg_nm = rlang::caller_arg(x)) {
  if (rlang::is_bool(x)) {
    invisible(x)
  } else {
    rlang::abort(paste0("`", arg_nm, "` must be `TRUE` or `FALSE`"))
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_count <- function(x, zero_ok = TRUE, arg_nm = rlang::caller_arg(x)) {
  zero_ok <- fm_assert_bool(zero_ok)
  if (fm_is_scalar_int(x) && ((zero_ok && x >= 0L) || (!zero_ok && x > 0L))) {
    invisible(as.integer(x))
  } else {
    op <- if (zero_ok) ">=" else ">"
    rlang::abort(paste0("`", arg_nm, "` must be an integer ", op, " 0"))
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_seed <- function(x, arg_nm = rlang::caller_arg(x)) {
  if (is.null(x)) {
    invisible(x)
  } else if (NROW(x) > 0L && (fm_is_scalar_int(x) || is.na(x)[[1L]])) {
    invisible(as.integer(x))
  } else {
    rlang::abort(paste0("`", arg_nm, "` must be an integer, `NA`, or `NULL`"))
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_num <- function(x, arg_nm = rlang::caller_arg(x)) {
  is_num <- rlang::is_bare_numeric(x)
  if (is_num && NROW(x) == 1L && !(is.na(x) || is.infinite(x))) {
    invisible(as.double(x))
  } else {
    rlang::abort(paste0("`", arg_nm, "must be a finite, non-missing number"))
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_vec_int <- function(x, arg_nm = rlang::caller_arg(x)) {
  is_int <- rlang::is_bare_numeric(x) && rlang::is_integerish(x, finite = TRUE)
  if (is_int && NROW(x) > 0L) {
    invisible(as.integer(x))
  } else {
    rlang::abort(
      paste0(
        "`", arg_nm,
        "` must be a finite, non-missing, integerish vector ",
        "with `length(", arg_nm, ") > 0`"
      )
    )
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_vec_num <- function(x, na_ok = FALSE, arg_nm = rlang::caller_arg(x)) {
  fm_assert_bool(na_ok)
  is_num <- rlang::is_bare_numeric(x)
  if (is_num && NROW(x) > 0L && (na_ok || !(anyNA(x) || any(is.infinite(x))))) {
    invisible(as.double(x))
  } else {
    rlang::abort(paste0(
      "`", arg_nm, "` must be a finite, non-missing numeric vector ",
      "with `length(", arg_nm, ") > 0`"
    ))
  }
}


#' Is Object a Scalar Integer-ish Value
#'
#' Always returns `TRUE` or `FALSE` - never errors
#'
#' @param x Object to test
#'
#' @return `TRUE` or `FALSE`
#'
#' @keywords internal
fm_is_scalar_int <- function(x) {
  is_int <- tryCatch(
    rlang::is_bare_numeric(x) && rlang::is_scalar_integerish(x, finite = TRUE),
    error = function(e) FALSE
  )
  tryCatch(rlang::is_true(is_int), error = function(e) FALSE)
}
