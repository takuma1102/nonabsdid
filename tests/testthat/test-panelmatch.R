make_mock_pe <- function() {
  est <- c("t+0" = 0.20, "t+1" = 0.40, "t+2" = 0.55)
  se  <- c("t+0" = 0.10, "t+1" = 0.12, "t+2" = 0.13)
  obj <- list(estimate = est, standard.error = se)
  class(obj) <- "PanelEstimate"
  obj
}

make_mock_placebo <- function() {
  est <- c("t-3" = 0.05, "t-2" = -0.02)
  se  <- c("t-3" = 0.08, "t-2" = 0.07)
  list(estimates = est, standard.errors = se)
}

test_that("PanelMatch tidier handles post-only case", {
  pe <- make_mock_pe()
  out <- as_nabs_event_study(pe)
  expect_identical(out$time, c(0L, 1L, 2L))
  expect_equal(out$estimate, c(0.20, 0.40, 0.55))
  expect_identical(unique(out$window), "post")
  expect_identical(unique(out$method), "PanelMatch")
})

test_that("PanelMatch tidier joins pre + post and inserts t = -1 reference", {
  pe <- make_mock_pe()
  pl <- make_mock_placebo()
  out <- as_nabs_event_study(pe, pre_obj = pl)
  expect_identical(out$time, c(-3L, -2L, -1L, 0L, 1L, 2L))
  # Reference row at -1 has estimate = 0 and zero CI width.
  ref <- out[out$time == -1L, ]
  expect_equal(ref$estimate,  0)
  expect_equal(ref$conf.low,  0)
  expect_equal(ref$conf.high, 0)
})

test_that("add_reference = FALSE skips the t = -1 anchor", {
  pe <- make_mock_pe()
  pl <- make_mock_placebo()
  out <- as_nabs_event_study(pe, pre_obj = pl, add_reference = FALSE)
  expect_false(any(out$time == -1L))
})

test_that("PanelMatch time-name parser accepts both 't+1' and 't1'", {
  est <- c("t1" = 0.3, "t-2" = 0.1)
  se  <- c("t1" = 0.1, "t-2" = 0.05)
  pe <- structure(list(estimate = est, standard.error = se),
                  class = "PanelEstimate")
  out <- as_nabs_event_study(pe)
  expect_identical(sort(out$time), c(-2L, 1L))
})

test_that("PanelMatch tidier errors when names cannot be parsed", {
  est <- c("foo" = 0.1)
  se  <- c("foo" = 0.05)
  pe <- structure(list(estimate = est, standard.error = se),
                  class = "PanelEstimate")
  expect_error(as_nabs_event_study(pe), "Failed to parse")
})
