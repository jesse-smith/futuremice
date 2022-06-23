test_that("`fm_assert_mids()` checks inheritance", {
  mids <- mice::mice(mice::nhanes, m = 1L, maxit = 0L)
  expect_identical(fm_assert_mids(mids), mids)
  list_mids <- mids
  class(list_mids) <- "list"
  expect_error(fm_assert_mids(list_mids))
})


test_that("`fm_assert_bool() works as expected`", {
  # Scalar, non-missing logicals pass
  expect_identical(fm_assert_bool(TRUE), TRUE)
  expect_identical(fm_assert_bool(FALSE), FALSE)
  # Vector and missing logicals fail
  expect_error(fm_assert_bool(c(TRUE, TRUE)))
  expect_error(fm_assert_bool(NA))
  # Logical-like numerics, characters, and factors fail
  expect_error(fm_assert_bool(1L))
  expect_error(fm_assert_bool(0L))
  expect_error(fm_assert_bool("TRUE"))
  expect_error(fm_assert_bool("FALSE"))
  expect_error(fm_assert_bool(factor(TRUE)))
  # NULL and length 0 logical fail
  expect_error(fm_assert_bool(NULL))
  expect_error(fm_assert_bool(logical()))
})


test_that("`fm_assert_count()` works as expected", {
  # Scalar, non-missing, non-negative integers pass
  expect_identical(fm_assert_count(1L), 1L)
  expect_identical(fm_assert_count(0L), 0L)
  expect_identical(fm_assert_count(.Machine$integer.max), .Machine$integer.max)
  # Scalar, non-missing, non-negative, finite integer-ish values pass
  # Passing doubles are converted to integer
  expect_identical(fm_assert_count(1), 1L)
  expect_identical(fm_assert_count(0), 0L)
  expect_identical(
    fm_assert_count(as.double(.Machine$integer.max)),
    .Machine$integer.max
  )
  # Vector and missing integer-ish values fail
  expect_error(fm_assert_count(c(0L, 1L)))
  expect_error(fm_assert_count(NA_integer_))
  # Negative, finite, or non-integer-ish values fail
  expect_error(fm_assert_count(-1L))
  expect_error(fm_assert_count(Inf))
  expect_error(fm_assert_count(1.01))
  # Integer-like characters, logicals, and factors fail
  expect_error(fm_assert_count("1"))
  expect_error(fm_assert_count(TRUE))
  expect_error(fm_assert_count(factor(1L)))
  # NULL and length 0 numerics fail
  expect_error(fm_assert_count(NULL))
  expect_error(fm_assert_count(integer()))
  expect_error(fm_assert_count(double()))
})


test_that("`fm_assert_seed()` works as expected", {
  # Scalar integer values pass
  expect_identical(fm_assert_seed(1L), 1L)
  expect_identical(fm_assert_seed(0L), 0L)
  expect_identical(fm_assert_seed(.Machine$integer.max), .Machine$integer.max)
  expect_identical(fm_assert_seed(-.Machine$integer.max), -.Machine$integer.max)
  # Scalar integer-ish values pass
  expect_identical(fm_assert_seed(1), 1L)
  expect_identical(fm_assert_seed(0), 0L)
  expect_identical(
    fm_assert_seed(as.double(.Machine$integer.max)),
    .Machine$integer.max
  )
  expect_identical(
    fm_assert_seed(as.double(-.Machine$integer.max)),
    -.Machine$integer.max
  )
  # Scalar missings pass, including `NaN`
  expect_identical(fm_assert_seed(NA_integer_), NA_integer_)
  expect_identical(fm_assert_seed(NA), NA_integer_)
  expect_identical(fm_assert_seed(NA_character_), NA_integer_)
  expect_identical(fm_assert_seed(NA_real_), NA_integer_)
  expect_identical(fm_assert_seed(NA_complex_), NA_integer_)
  expect_identical(fm_assert_seed(NaN), NA_integer_)
  # NULL passes unchanged
  expect_identical(fm_assert_seed(NULL), NULL)
  # Length 0 integer fails
  expect_error(fm_assert_seed(integer()))
  # Vector integer values fail
  expect_error(fm_assert_seed(c(0L, 1L)))
  # Infinite or non-integer-ish values fail
  expect_error(fm_assert_seed(Inf))
  expect_error(fm_assert_seed(1.01))
  # Integer-like characters, logicals, and factors fail
  expect_error(fm_assert_seed("1"))
  expect_error(fm_assert_seed(TRUE))
  expect_error(fm_assert_seed(factor(1L)))
})


test_that("`fm_assert_num()` works as expected", {
  # Scalar, non-missing, finite numerics pass
  # Converts to double
  expect_identical(fm_assert_num(0), 0)
  expect_identical(fm_assert_num(1.5), 1.5)
  expect_identical(fm_assert_num(-1.5), -1.5)
  expect_identical(fm_assert_num(.Machine$double.xmax), .Machine$double.xmax)
  expect_identical(fm_assert_num(.Machine$double.xmin), .Machine$double.xmin)
  expect_identical(fm_assert_num(-.Machine$double.xmax), -.Machine$double.xmax)
  expect_identical(fm_assert_num(-.Machine$double.xmin), -.Machine$double.xmin)
  expect_identical(fm_assert_num(0L), 0)
  expect_identical(
    fm_assert_num(.Machine$integer.max),
    as.double(.Machine$integer.max)
  )
  expect_identical(
    fm_assert_num(-.Machine$integer.max),
    as.double(-.Machine$integer.max)
  )
  # Vector, missing, and infinite numerics fail
  expect_error(fm_assert_num(c(0, 1)))
  expect_error(fm_assert_nume(c(0L, 1L)))
  expect_error(fm_assert_num(NA_real_))
  expect_error(fm_assert_num(NA_integer_))
  expect_error(fm_assert_num(Inf))
  # NULL and length 0 numerics fail
  expect_error(fm_assert_num(NULL))
  expect_error(fm_assert_num(integer()))
  expect_error(fm_assert_num(double()))
  # Non-numerics fail
  expect_error(fm_assert_num(TRUE))
  expect_error(fm_assert_num("1"))
  expect_error(fm_assert_num(factor(1)))
})


test_that("`fm_assert_vec_int()` works as expected", {
  # Non-missing integer-ish values pass
  int <- c(-.Machine$integer.max, 0L, .Machine$integer.max)
  expect_identical(fm_assert_vec_int(int), int)
  expect_identical(fm_assert_vec_int(as.double(int)), int)
  # Doesn't require non-scalar
  expect_identical(fm_assert_vec_int(int[[1L]]), int[[1L]])
  expect_identical(fm_assert_vec_int(0), 0L)
  # Missing, infinite, or non-integerish values fail
  expect_error(fm_assert_vec_int(c(int, NA_integer_)))
  expect_error(fm_assert_vec_int(c(as.double(int), NA_real_)))
  expect_error(fm_assert_vec_int(c(as.double(int), NaN)))
  expect_error(fm_assert_vec_int(c(as.double(int), Inf)))
  expect_error(fm_assert_vec_int(c(as.double(int), 1.01)))
  # Non-numeric values fail
  expect_error(fm_assert_vec_int(c(TRUE, FALSE)))
  expect_error(fm_assert_vec_int(c("0", "1")))
  expect_error(fm_assert_vec_int(factor(c(0L, 1L))))
  # NULL and length 0 numerics fail
  expect_error(fm_assert_vec_int(NULL))
  expect_error(fm_assert_vec_int(integer()))
  expect_error(fm_assert_vec_int(double()))
})
