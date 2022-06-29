test_that("`fm_parallel_params()` returns expected values", {
  # Passing
  expect_equal(
    fm_parallel_params(m = 1L, chunk_size = 1L, maxit = 1L, seed = 1L),
    list(n_chains = 1L, n_calls = 1L, chunk_size = 1L, maxit = 1L, seed = 1L)
  )
  expect_equal(
    fm_parallel_params(m = 1, chunk_size = 1, maxit = 1, seed = 1),
    list(n_chains = 1L, n_calls = 1L, chunk_size = 1L, maxit = 1L, seed = 1L)
  )
  expect_equal(
    fm_parallel_params(m = 1, chunk_size = 1, maxit = 0, seed = 1),
    list(n_chains = 1L, n_calls = 1L, chunk_size = 1L, maxit = 0L, seed = 1L)
  )
  expect_equal(
    fm_parallel_params(m = 1, chunk_size = 1, maxit = 0, seed = -100),
    list(n_chains = 1L, n_calls = 1L, chunk_size = 1L, maxit = 0L, seed = -100L)
  )
  expect_equal(
    fm_parallel_params(m = 10, chunk_size = 2, maxit = 100, seed = 1),
    list(n_chains = 2L, n_calls = 5L, chunk_size = 1L, maxit = 100L, seed = 1L)
  )
  expect_equal(
    fm_parallel_params(m = 10, chunk_size = 3, maxit = 100, seed = 1),
    list(n_chains = 1L, n_calls = 10L, chunk_size = 3L, maxit = 100L, seed = 1L)
  )
  expect_equal(
    fm_parallel_params(m = 1L, chunk_size = 1L, maxit = 1L, seed = NULL),
    list(n_chains = 1L, n_calls = 1L, chunk_size = 1L, maxit = 1L, seed = NULL)
  )
  expect_equal(
    fm_parallel_params(m = 1L, chunk_size = 1L, maxit = 1L, seed = NA),
    list(n_chains = 1L, n_calls = 1L, chunk_size = 1L, maxit = 1L, seed = NA_integer_)
  )
})
test_that("`fm_parallel_params()` errors when expected", {
  # Failing
  expect_error(
    fm_parallel_params(m = 0L, chunk_size = 1L, maxit = 1L, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = 0L, maxit = 1L, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = 1L, maxit = -1L, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1.5, chunk_size = 1L, maxit = 1L, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = 1.5, maxit = 1L, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = 1L, maxit = 1.5, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = 1L, maxit = 1L, seed = 1.5)
  )
  expect_error(
    fm_parallel_params(m = NA_integer_, chunk_size = 1L, maxit = 1L, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = NA_integer_, maxit = 1L, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = 1L, maxit = NA_integer_, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = integer(), chunk_size = 1L, maxit = 1L, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = integer(), maxit = 1L, seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = 1L, maxit = integer(), seed = 1L)
  )
  expect_error(
    fm_parallel_params(m = 1L, chunk_size = 1L, maxit = 1L, seed = integer())
  )
})


test_that("`fm_furrr_opts()` returns expected value", {
  withr::local_seed(1L)
  pp <- fm_parallel_params(m = 5L, chunk_size = 1L, maxit = 10L, seed = 1L)
  pp_na <- fm_parallel_params(m = 5L, chunk_size = 1L, maxit = 10L, seed = NA)
  pp_na$maxit <- NA
  pp_na$n_chains <- NA
  pp_null <- fm_parallel_params(m = 5L, chunk_size = 1L, maxit = 10L, seed = NULL)
  pp_null$maxit <- NULL
  pp_null$n_chains <- NULL
  f_opts <- furrr::furrr_options(
    seed = rngtools::RNGseq(pp$n_calls, seed = pp$seed, simplify = FALSE),
    globals = character(),
    chunk_size = pp$chunk_size
  )
  expect_identical(fm_furrr_opts(pp), f_opts)
  expect_silent(fm_furrr_opts(pp_na))
  expect_silent(fm_furrr_opts(pp_null))
})
test_that("`fm_furr_opts()` returns different seeds when none is passed", {
  withr::local_seed(1L)
  pp <- fm_parallel_params(m = 5L, chunk_size = 1L, maxit = 10L, seed = NULL)
  f_opts1 <- fm_furrr_opts(pp)
  f_opts2 <- fm_furrr_opts(pp)
  expect_false(identical(f_opts1$seed, f_opts2$seed))
})
test_that("`fm_furrr_opts()` errors when expected", {
  # Expect error due to no `.Random.seed`
  pp <- fm_parallel_params(m = 5L, chunk_size = 1L, maxit = 10L, seed = 1L)
  expect_error(fm_furrr_opts(pp))
  # Test other cases
  withr::local_seed(1L)
  pp_n_calls <- fm_parallel_params(m = 5L, chunk_size = 1L, maxit = 10L, seed = 1L)
  pp_n_calls$n_calls <- 0L
  pp_chunk_size <- fm_parallel_params(m = 5L, chunk_size = 1L, maxit = 10L, seed = 1L)
  pp_chunk_size$chunk_size <- 0L
  expect_error(fm_furrr_opts(pp_n_calls))
  expect_error(fm_furrr_opts(pp_chunk_size))
})

test_that("`fm_exit_msg()` produces expected output", {
  suppressMessages(expect_message(
    fm_exit_msg(
      i = 10L,
      rhat = list(rhat = rep(1, 3L), converged = TRUE),
      minit = 3L,
      "test"
    )
  ))
  suppressMessages(expect_warning(
    fm_exit_msg(
      i = 10L,
      rhat = list(rhat = rep(1, 3L), converged = TRUE),
      minit = 5L,
      "test"
    )
  ))
  suppressMessages(expect_warning(
    fm_exit_msg(
      i = 10L,
      rhat = list(rhat = c(1.5, 1, 1), converged = FALSE),
      minit = 3L,
      "test"
    )
  ))
})
test_that("`fm_exit_msg()` errors when expected", {
  expect_error(
    fm_exit_msg(
      i = -1L,
      rhat = list(rhat = rep(1, 3L), converged = TRUE),
      minit = 3L,
      "test"
    )
  )
  expect_error(
    fm_exit_msg(
      i = 1L,
      rhat = list(rhat = rep(1, 3L), converged = TRUE),
      minit = 3L,
      "test"
    )
  )
  expect_error(
    fm_exit_msg(
      i = 10L,
      rhat = list(rhat = rep(TRUE, 3L), converged = TRUE),
      minit = 3L,
      "test"
    )
  )
  expect_error(
    fm_exit_msg(
      i = 10L,
      rhat = list(rhat = rep(1, 3L), converged = NA),
      minit = 3L,
      "test"
    )
  )
  expect_error(
    fm_exit_msg(
      i = 10L,
      rhat = list(rhat = rep(1, 3L), converged = TRUE),
      minit = 0L,
      "test"
    )
  )
})

test_that("`fm_mice_seed()` returns expected values", {
  expect_identical(fm_mice_seed(-.Machine$integer.max), -.Machine$integer.max)
  expect_identical(fm_mice_seed(0L), 0L)
  expect_identical(fm_mice_seed(.Machine$integer.max), .Machine$integer.max)
  expect_identical(
    fm_mice_seed(as.double(-.Machine$integer.max)),
    -.Machine$integer.max
  )
  expect_identical(fm_mice_seed(0), 0L)
  expect_identical(
    fm_mice_seed(as.double(.Machine$integer.max)),
    .Machine$integer.max
  )
  expect_identical(fm_mice_seed(NA_integer_), NA_integer_)
  expect_identical(fm_mice_seed(NA), NA_integer_)
  expect_identical(fm_mice_seed(NaN), NA_integer_)
  expect_identical(fm_mice_seed(NULL), NA_integer_)
})
test_that("`fm_mice_seed()` errors when expected", {
  expect_error(fm_mice_seed(c(1L, 2L)))
  expect_error(fm_mice_seed(integer()))
  expect_error(fm_mice_seed(0.1))
  expect_error(fm_mice_seed(Inf))
  expect_error(fm_mice_seed("1"))
  expect_error(fm_mice_seed(factor(1L)))
})

test_that("`fm_rng_seed()` returns expected values", {
  expect_identical(fm_rng_seed(-.Machine$integer.max), -.Machine$integer.max)
  expect_identical(fm_rng_seed(0L), 0L)
  expect_identical(fm_rng_seed(.Machine$integer.max), .Machine$integer.max)
  expect_identical(
    fm_rng_seed(as.double(-.Machine$integer.max)),
    -.Machine$integer.max
  )
  expect_identical(fm_rng_seed(0), 0L)
  expect_identical(
    fm_rng_seed(as.double(.Machine$integer.max)),
    .Machine$integer.max
  )
  expect_identical(fm_rng_seed(NA_integer_), NULL)
  expect_identical(fm_rng_seed(NA), NULL)
  expect_identical(fm_mice_seed(NaN), NA_integer_)
  expect_identical(fm_rng_seed(NULL), NULL)
})
test_that("`fm_rng_seed()` errors when expected", {
  expect_error(fm_rng_seed(c(1L, 2L)))
  expect_error(fm_rng_seed(integer()))
  expect_error(fm_rng_seed(0.1))
  expect_error(fm_rng_seed(Inf))
  expect_error(fm_rng_seed("1"))
  expect_error(fm_rng_seed(factor(1L)))
})

test_that("`fm_set_colnames()` produces expected output on arrays", {
  a <- array(1:9, dim = rep(3L, 3L))
  a_nm <- a
  colnames(a_nm) <- letters[1:3]
  a_null <- a_nm
  colnames(a_null) <- NULL
  expect_identical(fm_set_colnames(a, letters[1:3]), a_nm)
  expect_identical(fm_set_colnames(a, NULL), a)
  expect_identical(fm_set_colnames(a_nm, NULL), a_null)
  a0 <- array(dim = c(3L, 0L, 3L))
  a0_nm <- a0
  colnames(a0_nm) <- character()
  expect_identical(fm_set_colnames(a0, NULL), a0)
  expect_identical(fm_set_colnames(a0, character()), a0_nm)
})
test_that("`fm_set_colnames()` errors when expected on arrays", {
  a <- array(1:9, dim = rep(3L, 3L))
  a_nm <- a
  colnames(a_nm) <- letters[1:3]
  a_null <- a_nm
  colnames(a_null) <- NULL
  expect_error(fm_set_colnames(a, letters))
  expect_error(fm_set_colnames(a, character()))
  expect_error(fm_set_colnames(a_nm, letters))
  expect_error(fm_set_colnames(a_nm, character()))
  expect_error(fm_set_colnames(a_null, letters))
  expect_error(fm_set_colnames(a_null, character()))
  a0 <- array(dim = c(3L, 0L, 3L))
  a0_nm <- a0
  colnames(a0_nm) <- character()
  expect_error(fm_set_colnames(a0, letters))
  expect_error(fm_set_colnames(a0_nm, letters))
})

test_that("`fm_set_rownames()` produces expected output on arrays", {
  a <- array(1:9, dim = rep(3L, 3L))
  a_nm <- a
  rownames(a_nm) <- letters[1:3]
  a_null <- a_nm
  rownames(a_null) <- NULL
  expect_identical(fm_set_rownames(a, letters[1:3]), a_nm)
  expect_identical(fm_set_rownames(a, NULL), a)
  expect_identical(fm_set_rownames(a_nm, NULL), a_null)
  a0 <- array(dim = c(0L, 3L, 3L))
  a0_nm <- a0
  rownames(a0_nm) <- character()
  expect_identical(fm_set_rownames(a0, NULL), a0)
  expect_identical(fm_set_rownames(a0, character()), a0_nm)
})
test_that("`fm_set_rownames()` errors when expected on arrays", {
  a <- array(1:9, dim = rep(3L, 3L))
  a_nm <- a
  rownames(a_nm) <- letters[1:3]
  a_null <- a_nm
  rownames(a_null) <- NULL
  expect_error(fm_set_rownames(a, letters))
  expect_error(fm_set_rownames(a, character()))
  expect_error(fm_set_rownames(a_nm, letters))
  expect_error(fm_set_rownames(a_nm, character()))
  expect_error(fm_set_rownames(a_null, letters))
  expect_error(fm_set_rownames(a_null, character()))
  a0 <- array(dim = c(3L, 0L, 3L))
  a0_nm <- a0
  rownames(a0_nm) <- character()
  expect_error(fm_set_rownames(a0, letters))
  expect_error(fm_set_rownames(a0_nm, letters))
})

test_that("`fm_gcd()` returns expected output", {
  expect_identical(fm_gcd(), integer())
  expect_identical(fm_gcd(numeric()), integer())
  expect_identical(fm_gcd(10, 5), 5L)
  expect_identical(fm_gcd(10, 1), 1L)
  expect_identical(fm_gcd(11, 5), 1L)
  expect_identical(fm_gcd(14, 4), 2L)
  expect_identical(fm_gcd(36, 2, 10), 2L)
  expect_identical(fm_gcd(c(10, 5)), 5L)
})
test_that("`fm_gcd()` errors when expected", {
  expect_error(fm_gcd(0L))
  expect_error(fm_gcd(0L, 1L))
  expect_error(fm_gcd(-1L, 1L))
  expect_error(fm_gcd(Inf))
  expect_error(fm_gcd(NaN))
})
