# Coverage for as_nabs_event_study.fixest(). Needs fixest (a Suggests), so
# skip when it isn't installed. We fit a small absorbing event study with
# fixest::i() so the coefficient names follow the `time_to_event::<k>` form
# the tidier looks for.

test_that("fixest tidier extracts event-study coefficients and inserts t = -1", {
  skip_if_not_installed("fixest")

  set.seed(1)
  d <- expand.grid(id = 1:20, t = 1:8)
  d$g <- ifelse(d$id <= 10, 4L, 6L)          # two fully-treated cohorts
  d$time_to_event <- d$t - d$g               # finite, bounded leads/lags
  d$treat <- as.integer(d$t >= d$g)
  d$y <- 0.5 * d$treat + stats::rnorm(nrow(d))

  m <- fixest::feols(
    y ~ i(time_to_event, ref = -1) | id + t,
    data = d
  )

  out <- as_nabs_event_study(m, outcome = "y")
  expect_s3_class(out, "nabs_event_study_tbl")
  expect_identical(unique(out$method), "TWFE")          # default label
  expect_true(any(out$time == -1L))                 # reference inserted
  expect_equal(out$estimate[out$time == -1L], 0)
  expect_true(all(diff(out$time) > 0))              # ordered by time
})

test_that("fixest tidier respects an explicit method label", {
  skip_if_not_installed("fixest")
  set.seed(2)
  d <- expand.grid(id = 1:20, t = 1:8)
  d$g <- ifelse(d$id <= 10, 4L, 6L)
  d$time_to_event <- d$t - d$g
  d$treat <- as.integer(d$t >= d$g)
  d$y <- 0.3 * d$treat + stats::rnorm(nrow(d))
  m <- fixest::feols(y ~ i(time_to_event, ref = -1) | id + t, data = d)

  out <- as_nabs_event_study(m, method = "MyTWFE")
  expect_identical(unique(out$method), "MyTWFE")
})

test_that("fixest tidier errors when there are no event-study coefficients", {
  skip_if_not_installed("fixest")
  set.seed(3)
  d <- expand.grid(id = 1:10, t = 1:6)
  d$treat <- as.integer(d$id <= 5 & d$t >= 3)
  d$y <- 0.4 * d$treat + stats::rnorm(nrow(d))

  # A plain TWFE with no `time_to_event::k` terms.
  m <- fixest::feols(y ~ treat | id + t, data = d)
  expect_error(as_nabs_event_study(m), "event-study coefficients")
})
