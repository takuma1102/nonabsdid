test_that("naive_twfe runs end-to-end and recovers level distributed-lag coefs", {
  skip_if_not_installed("fixest")

  # True model: y depends on the treatment LEVEL at lags 0, 1, 2 with known
  # coefficients (0.5, 0.3, 0.2). Treatment is non-absorbing (toggles on/off).
  set.seed(5)
  N <- 150; TT <- 22
  b <- c(0.5, 0.3, 0.2)
  lt <- stats::rnorm(TT, sd = 0.4)
  rows <- vector("list", N)
  for (i in seq_len(N)) {
    D <- integer(TT); st <- 1L
    for (t in 2:TT) { if (stats::runif(1) < 0.15) st <- 1L - st; D[t] <- st }
    ai <- stats::rnorm(1, sd = 2); y <- numeric(TT)
    for (t in seq_len(TT)) {
      e <- 0
      for (k in 0:2) if (t - k >= 1) e <- e + b[k + 1] * D[t - k]
      y[t] <- ai + lt[t] + e + stats::rnorm(1, sd = 0.1)
    }
    rows[[i]] <- data.frame(id = i, yr = seq_len(TT), d = D, y = y)
  }
  d <- do.call(rbind, rows)

  out <- naive_twfe(d, outcome = "y", treatment = "d",
                    unit = "id", time = "yr", lags = 4L, leads = 6L)

  expect_s3_class(out, "nabs_event_study_tbl")
  expect_identical(unique(out$method), "TWFE")
  expect_equal(stats::nobs(attr(out, "fit")), nrow(d))

  e <- stats::setNames(out$estimate, out$time)
  expect_equal(unname(e["0"]), 0.5, tolerance = 0.05)
  expect_equal(unname(e["1"]), 0.3, tolerance = 0.05)
  expect_equal(unname(e["2"]), 0.2, tolerance = 0.05)

  # No spurious pre-trend, reference exactly 0.
  pre <- out$estimate[out$time < -1L]
  expect_true(all(abs(pre) < 0.1))
  expect_equal(out$estimate[out$time == -1L], 0)
})

test_that("naive_twfe errors on missing columns (fixest not required)", {
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

test_that("build_dl_design builds shifted treatment-level columns", {
  f <- get("build_dl_design", envir = asNamespace("nonabsdid"))
  d <- data.frame(
    id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    t  = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    d  = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0)
    # id 1: treated at t = 3, 4 (non-absorbing); id 2: never treated
  )
  res <- f(d, treatment = "d", unit = "id", time = "t", lags = 2L, leads = 2L)
  des <- res$data
  i1 <- des$id == 1

  # Column -> event-time map; event -1 (lead 1) is the omitted reference.
  expect_identical(res$map[["nabs_dl_p0"]], 0L)
  expect_identical(res$map[["nabs_dl_p1"]], 1L)
  expect_identical(res$map[["nabs_dl_p2"]], 2L)
  expect_identical(res$map[["nabs_dl_m2"]], -2L)
  expect_false("nabs_dl_m1" %in% names(res$map))

  # Each column is the treatment level shifted by its event time.
  expect_equal(des$nabs_dl_p0[i1], c(0, 0, 1, 1, 0))  # D_t
  expect_equal(des$nabs_dl_p1[i1], c(0, 0, 0, 1, 1))  # D_{t-1}
  expect_equal(des$nabs_dl_p2[i1], c(0, 0, 0, 0, 1))  # D_{t-2}
  expect_equal(des$nabs_dl_m2[i1], c(1, 1, 0, 0, 0))  # D_{t+2}

  # Never-treated unit contributes zero everywhere.
  expect_true(all(des$nabs_dl_p0[des$id == 2] == 0))
  expect_true(all(des$nabs_dl_m2[des$id == 2] == 0))
})

test_that("collect_dl reads per-period coefficients and adds the reference", {
  f <- get("collect_dl", envir = asNamespace("nonabsdid"))
  map <- c(nabs_dl_p0 = 0L, nabs_dl_p1 = 1L, nabs_dl_m2 = -2L)
  est <- c(nabs_dl_p0 = 0.5, nabs_dl_p1 = 0.3, nabs_dl_m2 = -0.1)
  se  <- c(nabs_dl_p0 = 0.05, nabs_dl_p1 = 0.04, nabs_dl_m2 = 0.06)

  out <- f(est, se, map)
  o <- stats::setNames(out$estimate, out$time)
  s <- stats::setNames(out$std.error, out$time)

  # Each event time is its own coefficient; reference -1 is exactly 0.
  expect_equal(unname(o["0"]), 0.5)
  expect_equal(unname(o["1"]), 0.3)
  expect_equal(unname(o["-2"]), -0.1)
  expect_equal(unname(o["-1"]), 0)
  expect_equal(unname(s["1"]), 0.04)
  expect_equal(unname(s["-1"]), 0)
})
