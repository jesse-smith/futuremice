test_that("`rhat_max()` output has not changed", {
  mids <- mice::mice(
    mice::nhanes,
    m = 5L,
    maxit = 5L,
    seed = 1L,
    printFlag = FALSE
  )
  expect_snapshot(rhat_max(mids, 5L))
})
test_that("`rhat_max()` errors when expected", {
  mids_it0 <- mice::mice(mice::nhanes, m = 1L, maxit = 0L, seed = 1L)
  expect_error(rhat_max(mids_it0))
  mids_nochain <- mids_it0
  mids_nochain$chainMean <- NULL
  expect_error(rhat_max(mids_nochain))
  mids_nochain <- mids_it0
  mids_nochain$chainVar <- NULL
  expect_error(rhat_max(mids_nochain))
})
