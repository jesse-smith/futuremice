test_that("`future_mice()` works", {
  withr::local_seed(1L)
  future::plan("multisession", workers = pmin(2L, future::availableCores()))
  expect_snapshot(future_mice(mice::nhanes2, seed = 1L, maxit = 5L, minit = 1L))
  future::plan("sequential")
})
