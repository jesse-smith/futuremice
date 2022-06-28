test_that("`future_mice()` works", {
  withr::local_seed(1L)
  future::plan("multisession", workers = pmin(2L, future::availableCores()))
  withr::defer(future::plan("sequential"))
  is_inst <- furrr::future_map_lgl(
    seq_len(4L),
    function(x) rlang::is_installed("futuremice")
  )
  if (!all(is_inst)) {
    tryCatch(
      devtools::install(reload = FALSE, quick = TRUE, quiet = TRUE),
      error = function(e) skip("Could not install `{futuremice}` in test env")
    )
    withr::defer(suppressMessages(remove.packages("futuremice")))
  }
  fmids <- suppressMessages(suppressWarnings(
    future_mice(mice::nhanes, maxit = 5L, seed = 1L)
  ))
  expect_true(mice::is.mids(fmids))
})
