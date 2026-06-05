# We test the DCDH tidier with a hand-built mock object that mimics the
# stable surface of did_multiplegt_dyn() output (the embedded ggplot's
# $data slot), so the tests run on CI without needing DIDmultiplegtDYN
# installed.

make_mock_dcdh <- function(times, estimates, lb = NULL, ub = NULL, se = NULL) {
  pd <- data.frame(
    Time = as.integer(times),
    Estimate = as.numeric(estimates),
    LB.CI = if (is.null(lb)) estimates - 1 else lb,
    UB.CI = if (is.null(ub)) estimates + 1 else ub
  )
  if (!is.null(se)) pd$SE <- se
  obj <- list(plot = list(data = pd))
  class(obj) <- "did_multiplegt_dyn"
  obj
}

test_that("DCDH tidier extracts time, estimate, and CI", {
  m <- make_mock_dcdh(times = c(-3, -2, -1, 0, 1, 2),
                      estimates = c(0.1, 0.0, 0.0, 0.4, 0.6, 0.7))
  out <- as_nabs_event_study(m, outcome = "y")
  expect_s3_class(out, "nabs_event_study_tbl")
  expect_equal(out$method, rep("DCDH", 6))
  expect_equal(out$time, c(-4L, -3L, -2L, -1L, 0L, 1L)) # DCDH package adopts a different reference period.
  expect_equal(out$conf.low,  out$estimate - 1)
  expect_equal(out$conf.high, out$estimate + 1)
})

test_that("DCDH tidier passes through SE when present", {
  m <- make_mock_dcdh(times = -1:1, estimates = c(0, 0.5, 1.0),
                      se = c(0.1, 0.2, 0.3))
  out <- as_nabs_event_study(m)
  expect_equal(out$std.error, c(0.1, 0.2, 0.3))
})

test_that("DCDH tidier errors helpfully on missing plot data", {
  bad <- structure(list(plot = NULL), class = "did_multiplegt_dyn")
  expect_error(as_nabs_event_study(bad), "graph_off")
})
