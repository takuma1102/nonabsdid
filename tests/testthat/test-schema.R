test_that("data.frame method builds a valid nabs_event_study_tbl", {
  df <- data.frame(
    time = c(-3, -2, -1, 0, 1, 2),
    estimate = c(0.1, 0.0, 0, 0.5, 0.8, 1.0),
    std.error = c(0.1, 0.1, 0.0, 0.2, 0.2, 0.3)
  )
  out <- as_nabs_event_study(df, method = "TEST", outcome = "y")

  expect_s3_class(out, "nabs_event_study_tbl")
  expect_named(out, c("time", "estimate", "std.error",
                      "conf.low", "conf.high",
                      "window", "method", "outcome"))
  expect_equal(out$method, rep("TEST", 6))
  expect_true(all(out$window[out$time < 0] == "pre"))
  expect_true(all(out$window[out$time >= 0] == "post"))
  # CIs derived from SE under normal approximation.
  z <- qnorm(0.975)
  expect_equal(out$conf.low,  out$estimate - z * out$std.error)
  expect_equal(out$conf.high, out$estimate + z * out$std.error)
})

test_that("conf.level argument is respected", {
  df <- data.frame(time = 0L, estimate = 1, std.error = 0.5)
  out <- as_nabs_event_study(df, conf.level = 0.90)
  z <- qnorm(0.95)
  expect_equal(out$conf.low,  1 - z * 0.5)
  expect_equal(out$conf.high, 1 + z * 0.5)
  expect_equal(attr(out, "conf.level"), 0.90)
})

test_that("missing required columns errors helpfully", {
  bad <- data.frame(t = 0, e = 1)
  expect_error(as_nabs_event_study(bad), "time")
})

test_that("default method errors on unknown classes", {
  expect_error(as_nabs_event_study(structure(list(), class = "weirdo")),
               "No.*as_nabs_event_study.*method")
})

test_that("idempotent on nabs_event_study_tbl", {
  df <- data.frame(time = 0:1, estimate = c(1, 2), std.error = c(0.1, 0.1))
  t1 <- as_nabs_event_study(df, method = "A")
  t2 <- as_nabs_event_study(t1, method = "B")
  expect_equal(t2$method, c("B", "B"))
  expect_s3_class(t2, "nabs_event_study_tbl")
})
