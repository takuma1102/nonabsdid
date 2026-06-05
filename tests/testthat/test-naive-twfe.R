test_that("naive_twfe runs end-to-end on an absorbing panel", {
  skip_if_not_installed("fixest")

  # Synthetic panel: 50 units x 20 periods; treatment turns on at t = 10
  # for half of the units and stays on (absorbing). Effect = 1 + small noise.
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

  fit <- attr(out, "fit")
  expect_equal(stats::nobs(fit), nrow(d))

  # Reference period should be present at -1 with estimate exactly 0.
  expect_true(any(out$time == -1L))
  expect_equal(out$estimate[out$time == -1L], 0)

  # Post-treatment estimates should be near 1 (sanity, generous tolerance).
  post <- out$estimate[out$time >= 0L]
  expect_true(mean(post) > 0.7)
})

test_that("naive_twfe recovers a NON-absorbing (on/off) dynamic path", {
  skip_if_not_installed("fixest")

  # Treatment switches on AND off. The true per-switch impulse increments are
  # m = (0.5, 0.3, 0.2), so the cumulative event-study path is
  # (0.5, 0.8, 1.0, 1.0, ...). The classic "first onset only" event study would
  # mislabel post-switch-off periods and fail to recover this; the
  # distributed-lag formulation should recover it.
  set.seed(7)
  N <- 60; TT <- 24
  m <- c(0.5, 0.3, 0.2)
  ai <- stats::rnorm(N, sd = 1.5)
  lt <- stats::rnorm(TT, sd = 0.5)
  rows <- vector("list", N)
  for (i in seq_len(N)) {
    Di <- integer(TT)
    if (i %% 5 != 0) {                     # one in five units never treated
      on <- sample(7:11, 1); off <- on + sample(3:5, 1)
      Di[on:min(off, TT)] <- 1L
      if (i %% 3 == 0 && off + 3 <= TT) Di[(off + 3):TT] <- 1L  # switch back on
    }
    dD <- c(0, diff(Di))
    y <- numeric(TT)
    for (t in seq_len(TT)) {
      eff <- 0
      for (j in seq_along(m)) if (t - (j - 1) >= 1) eff <- eff + m[j] * dD[t - (j - 1)]
      y[t] <- ai[i] + lt[t] + eff + stats::rnorm(1, sd = 0.05)
    }
    rows[[i]] <- data.frame(id = i, yr = seq_len(TT), d = Di, y = y)
  }
  d <- do.call(rbind, rows)

  out <- naive_twfe(d, outcome = "y", treatment = "d",
                    unit = "id", time = "yr", lags = 4L, leads = 5L)

  # Never-treated units remain in the estimation sample.
  expect_equal(stats::nobs(attr(out, "fit")), nrow(d))

  # Cumulative path recovered within tolerance.
  e <- stats::setNames(out$estimate, out$time)
  expect_equal(unname(e["0"]), 0.5, tolerance = 0.05)
  expect_equal(unname(e["1"]), 0.8, tolerance = 0.05)
  expect_equal(unname(e["2"]), 1.0, tolerance = 0.05)
  expect_equal(unname(e["3"]), 1.0, tolerance = 0.05)

  # No spurious pre-trend.
  pre <- out$estimate[out$time < -1L]
  expect_true(all(abs(pre) < 0.1))
  expect_equal(out$estimate[out$time == -1L], 0)
})

test_that("naive_twfe errors on missing columns (fixest not required)", {
  # Input validation runs before the fixest dependency check, so this
  # test passes even if fixest is not installed.
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

test_that("naive_twfe errors when there are no treated observations", {
  d <- data.frame(id = rep(1:3, each = 3),
                  t = rep(1:3, 3), d = 0L, y = rnorm(9))
  expect_error(
    naive_twfe(d, outcome = "y", treatment = "d", unit = "id", time = "t"),
    "No treated observations"
  )
})

test_that("build_dl_design encodes treatment changes and endpoint bins", {
  f <- get("build_dl_design", envir = asNamespace("nonabsdid"))
  d <- data.frame(
    id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    t  = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    d  = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0)
    # id 1: switches on at t=3, off at t=5 (non-absorbing)
    # id 2: never treated
  )
  res <- f(d, treatment = "d", unit = "id", time = "t", lags = 2L, leads = 2L)
  des <- res$data
  i1 <- des$id == 1

  # Column -> event-time map.
  expect_equal(res$map[["nabs_dl_p0"]], 0L)
  expect_equal(res$map[["nabs_dl_p1"]], 1L)
  expect_equal(res$map[["nabs_dl_p2"]], 2L)   # far post bin
  expect_equal(res$map[["nabs_dl_m2"]], -2L)  # far pre bin
  # Event time -1 (lead 1) is the omitted reference: no column.
  expect_false("nabs_dl_m1" %in% names(res$map))

  # p0 = delta D contemporaneously: +1 at the on-switch (t=3), -1 at off (t=5).
  expect_equal(des$nabs_dl_p0[i1], c(0, 0, 1, 0, -1))
  # p1 = delta D one period earlier.
  expect_equal(des$nabs_dl_p1[i1], c(0, 0, 0, 1, 0))
  # p2 = treatment LEVEL two periods earlier (the binned long-run term).
  expect_equal(des$nabs_dl_p2[i1], c(0, 0, 0, 0, 1))

  # Never-treated unit contributes zero to every change-based column.
  expect_true(all(des$nabs_dl_p0[des$id == 2] == 0))
  expect_true(all(des$nabs_dl_p1[des$id == 2] == 0))
})

test_that("cumulate_dl cumulates coefficients with delta-method SEs", {
  f <- get("cumulate_dl", envir = asNamespace("nonabsdid"))
  map <- c(nabs_dl_p0 = 0L, nabs_dl_p1 = 1L, nabs_dl_p2 = 2L, nabs_dl_m2 = -2L)
  est <- c(nabs_dl_p0 = 0.4, nabs_dl_p1 = 0.3, nabs_dl_p2 = 0.3, nabs_dl_m2 = -0.1)
  V <- diag(c(0.01, 0.04, 0.09, 0.25))
  dimnames(V) <- list(names(est), names(est))

  out <- f(est, V, map, lags = 2L, leads = 2L)
  o <- stats::setNames(out$estimate, out$time)
  s <- stats::setNames(out$std.error, out$time)

  # Cumulative point estimates.
  expect_equal(unname(o["0"]), 0.4)
  expect_equal(unname(o["1"]), 0.7)
  expect_equal(unname(o["2"]), 1.0)
  expect_equal(unname(o["-2"]), 0.1)   # -(-0.1)
  expect_equal(unname(o["-1"]), 0)     # reference

  # Delta-method SEs: sqrt of summed variances (diagonal VCOV).
  expect_equal(unname(s["0"]), sqrt(0.01))
  expect_equal(unname(s["1"]), sqrt(0.01 + 0.04))
  expect_equal(unname(s["2"]), sqrt(0.01 + 0.04 + 0.09))
  expect_equal(unname(s["-2"]), sqrt(0.25))
  expect_equal(unname(s["-1"]), 0)
})
