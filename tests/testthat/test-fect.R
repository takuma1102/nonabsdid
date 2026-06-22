make_mock_fect <- function(with_se = TRUE, method = "ife") {
  t   <- c(-3, -2, -1, 0, 1, 2, 3)
  att <- c(0.0, 0.05, 0, 0.4, 0.6, 0.7, 0.6)
  bnd <- cbind(att - 0.2, att + 0.2)
  ea  <- if (with_se) {
    m <- cbind(att, c(0.10, 0.10, 0.0, 0.15, 0.18, 0.18, 0.20))
    colnames(m) <- c("ATT", "S.E.")
    m
  } else NULL
  obj <- list(time = t, att = att, att.bound = bnd, est.att = ea,
              method = method)
  class(obj) <- "fect"
  obj
}

test_that("fect tidier extracts time, att, and CI bounds", {
  f <- make_mock_fect()
  out <- as_nabs_event_study(f, outcome = "y")
  expect_s3_class(out, "nabs_event_study_tbl")
  expect_identical(out$method, rep("IFE", 7))
  expect_identical(out$time, -3:3)
  expect_equal(out$conf.low,  out$estimate - 0.2)
  expect_equal(out$conf.high, out$estimate + 0.2)
})

test_that("fect tidier picks up SE when est.att has S.E. column", {
  f <- make_mock_fect()
  out <- as_nabs_event_study(f)
  expect_equal(out$std.error,
               c(0.10, 0.10, 0.0, 0.15, 0.18, 0.18, 0.20))
})

test_that("fect tidier handles missing SE gracefully", {
  f <- make_mock_fect(with_se = FALSE)
  out <- as_nabs_event_study(f)
  expect_true(all(is.na(out$std.error)))
  # CIs should still be present from att.bound.
  expect_false(anyNA(out$conf.low))
})

test_that("custom method label is respected", {
  f <- make_mock_fect()
  out <- as_nabs_event_study(f, method = "Imputation")
  expect_identical(unique(out$method), "Imputation")
})

test_that("fect tidier auto-labels FE / IFE / MC from $method", {
  f_fe  <- make_mock_fect(method = "fe")
  f_ife <- make_mock_fect(method = "ife")
  f_mc  <- make_mock_fect(method = "mc")
  expect_identical(unique(as_nabs_event_study(f_fe )$method), "FE")
  expect_identical(unique(as_nabs_event_study(f_ife)$method), "IFE")
  expect_identical(unique(as_nabs_event_study(f_mc )$method), "MC")
})

test_that("fect tidier handles legacy / unknown method strings", {
  # An old fect that called the FE estimator "imputation".
  f_legacy  <- make_mock_fect(method = "imputation")
  expect_identical(unique(as_nabs_event_study(f_legacy)$method), "Imputation")

  # An object built without a method slot (very old version) falls back to "IFE".
  f <- make_mock_fect()
  f$method <- NULL
  expect_identical(unique(as_nabs_event_study(f)$method), "IFE")

  # Unknown future labels fall back to "IFE" rather than erroring out.
  f$method <- "futureproof"
  expect_identical(unique(as_nabs_event_study(f)$method), "IFE")
})
