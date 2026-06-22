test_that("auto_window picks sensible defaults from the panel", {
  f <- get("auto_window", envir = asNamespace("nonabsdid"))

  # 50 units x 30 periods, treatment turns on at t=10 for half.
  set.seed(1)
  d <- expand.grid(id = 1:50, t = 1:30)
  d$d <- as.integer(d$id <= 25 & d$t >= 10)

  out <- f(d, treatment = "d", unit = "id", time = "t")
  expect_true(out$lags  >= 2L)
  expect_true(out$lags  <= 6L)
  expect_true(out$leads >= 2L)
  expect_true(out$leads <= 8L)
})

test_that("auto_window passes user values straight through", {
  f <- get("auto_window", envir = asNamespace("nonabsdid"))
  d <- expand.grid(id = 1:5, t = 1:10); d$d <- 0L
  out <- f(d, "d", "id", "t", user_lags = 4, user_leads = 6)
  expect_identical(out$lags,  4L)
  expect_identical(out$leads, 6L)
})

test_that("auto_window warns and falls back when no treated obs", {
  f <- get("auto_window", envir = asNamespace("nonabsdid"))
  d <- expand.grid(id = 1:5, t = 1:10); d$d <- 0L
  out <- expect_warning(f(d, "d", "id", "t"), "No treated")
  expect_identical(out$lags,  6L)
  expect_identical(out$leads, 8L)
})

test_that("nabs_event_study_simple validates inputs before running anything", {
  d <- data.frame(id = 1, t = 1, y = 0)
  expect_error(
    nabs_event_study_simple(d, "y", "d", "id", "t"),
    "not found"
  )
  expect_error(
    nabs_event_study_simple(d, outcome = 1, treatment = "d",
                            unit = "id", time = "t"),
    "is.character"
  )
})

test_that("nabs_event_study_simple errors gracefully when no estimator is available", {
  # In this test environment none of DIDmultiplegtDYN / PanelMatch / fect
  # are installed, so every method is skipped. We expect a clean error
  # rather than a confusing crash.
  estimator_pkgs <- c("DIDmultiplegtDYN", "PanelMatch", "fect")

  is_installed <- function(pkg) {
  nzchar(system.file(package = pkg))
  }

  if (any(vapply(estimator_pkgs, is_installed, logical(1)))) {
    skip("At least one estimator package is installed; skipping no-estimator path test.")
  }
    
  set.seed(2)
  d <- expand.grid(id = 1:10, t = 1:6)
  d$y <- rnorm(nrow(d))
  d$d <- as.integer(d$id <= 5 & d$t >= 3)

  expect_error(
    suppressWarnings(suppressMessages(
      nabs_event_study_simple(d, "y", "d", "id", "t",
                              verbose = FALSE,
                              include_twfe = FALSE)
    )),
    "No estimator succeeded"
  )
})
