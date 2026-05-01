test_that("naive_twfe runs end-to-end on a small synthetic panel", {
  skip_if_not_installed("fixest")

  # Synthetic panel: 50 units x 20 periods; treatment turns on at t = 10
  # for half of the units. Effect = 1 + small noise.
  set.seed(42)
  N <- 50; TT <- 20
  d <- expand.grid(id = seq_len(N), t = seq_len(TT))
  d$treated <- d$id <= N / 2
  d$d <- as.integer(d$treated & d$t >= 10)
  d$y <- 0.1 * d$id + 0.05 * d$t + d$d * 1.0 +
         stats::rnorm(nrow(d), sd = 0.2)

  out <- naive_twfe(d, outcome = "y", treatment = "d",
                    unit = "id", time = "t",
                    lags = 5L, leads = 5L)

  expect_s3_class(out, "nabs_event_study_tbl")
  expect_equal(unique(out$method), "TWFE")
  # Reference period should be present at -1 with estimate exactly 0.
  expect_true(any(out$time == -1L))
  expect_equal(out$estimate[out$time == -1L], 0)

  # Post-treatment estimates should be near 1 (sanity, generous tolerance).
  post <- out$estimate[out$time >= 0L]
  expect_true(mean(post) > 0.7)
})

test_that("naive_twfe errors on missing columns (fixest not required)", {
  # Input validation runs before the fixest dependency check, so this
  # test passes even if fixest is not installed -- and the user sees
  # the actually relevant error first.
  d <- data.frame(id = 1, t = 1, y = 0)
  expect_error(
    naive_twfe(d, outcome = "y", treatment = "d", unit = "id", time = "t"),
    "not found"
  )
})

test_that("naive_twfe rejects bad argument types early", {
  d <- data.frame(id = 1, t = 1, y = 0, d = 0)
  expect_error(naive_twfe(d, outcome = 1, treatment = "d",
                          unit = "id", time = "t"))
  expect_error(naive_twfe(d, outcome = "y", treatment = "d",
                          unit = "id", time = "t", lags = -2L))
})

test_that("build_time_to_event computes relative time correctly", {
  # Internal test: never-treated units should get NA, treated units should
  # see t - first_treated_period.
  f <- get("build_time_to_event", envir = asNamespace("nonabsdid"))
  d <- data.frame(
    id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
    t  = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
    d  = c(0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0)
    # id 1: first treated at t=3
    # id 2: first treated at t=2 (then back off, but first switch is what matters)
    # id 3: never treated
  )
  out <- f(d, treatment = "d", unit = "id", time = "t")

  # Never-treated unit gets NA throughout.
  expect_true(all(is.na(out$time_to_event[out$id == 3])))
  # id 1: first switch at t=3, so t=3 -> 0; t=4 -> 1; t=2 -> -1; t=1 -> -2.
  expect_equal(out$time_to_event[out$id == 1 & out$t == 3], 0)
  expect_equal(out$time_to_event[out$id == 1 & out$t == 4], 1)
  expect_equal(out$time_to_event[out$id == 1 & out$t == 2], -1)
  expect_equal(out$time_to_event[out$id == 1 & out$t == 1], -2)
})
