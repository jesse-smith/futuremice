#' Make Assertions on Data Types and Arguments
#'
#' @description
#' `fm_assert_mids()` checks for a `mids` object
#' `fm_assert_bool()` checks for a non-missing, scalar `logical`
#' `fm_assert_count()` checks for a non-missing, finite, scalar integer-ish
#'   value >= 0 and converts its input to `integer`
#' `fm_assert_seed()` checks for a finite, scalar integer-ish value, `NA`,
#'   or `NULL`, and converts non-`NULL` input to `integer`
#' `fm_assert_num()` checks for a non-missing, scalar, finite `numeric` and
#'   converts its input to `double`
#' `fm_assert_vec_int()` checks for non-missing, finite integer-ish values and
#'   converts its input to `integer`
#'
#' @param x An object to check
#' @param arg_nm The name of the object to check; can usually be inferred from
#'   the caller environment automatically
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
fm_assert_count <- function(x, arg_nm = rlang::caller_arg(x)) {
  if (rlang::is_scalar_integerish(x, finite = TRUE) && x >= 0L) {
    invisible(as.integer(x))
  } else {
    rlang::abort(paste0("`", arg_nm, "` must be an integer >= 0"))
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_seed <- function(x, arg_nm = rlang::caller_arg(x)) {
  if (is.null(x)) {
    invisible(x)
  } else if (rlang::is_scalar_integerish(x, finite = TRUE) || is.na(x)) {
    invisible(as.integer(x))
  } else {
    rlang::abort(paste0("`", arg_nm, "` must be an integer, `NA`, or `NULL`"))
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_num <- function(x, arg_nm = rlang::caller_arg(x)) {
  if (is.numeric(x) && NROW(x) == 1L && !(is.na(x) || is.infinite(x))) {
    invisible(as.double(x))
  } else {
    rlang::abort(paste0("`", arg_nm, "must be a finite, non-missing number"))
  }
}


#' @rdname fm_assert
#'
#' @keywords internal
fm_assert_vec_int <- function(x, arg_nm = rlang::caller_arg(x)) {
  if (rlang::is_integerish(x, finite = TRUE)) {
    invisible(as.integer(x))
  } else {
    rlang::abort(
      paste0("`", arg_nm, "` must be a non-missing, finite, integerish vector")
    )
  }
}
