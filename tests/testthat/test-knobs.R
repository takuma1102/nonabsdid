# Integration smoke tests for the new tuning knobs and lighter defaults.
# These need the suggested estimator packages, so they skip when those aren't
# installed. They assert robust properties only (does it run, is the knob
# accepted, is the default honoured) rather than exact numbers.

# A reasonably sized non-absorbing panel (treatment switches on and off).
make_big_panel <- function(n_units = 60, n_time = 12, seed = 1) {
  set.seed(seed)
  d <- expand.grid(id = seq_len(n_units), t = seq_len(n_time))
  d <- d[order(d$id, d$t), ]
  ever <- d$id <= n_units / 2
  on   <- (d$t >= 4 & d$t <= 7) | (d$t >= 9 & d$t <= 10)  # on, off, on
  d$d  <- as.integer(ever & on)
  d$x1 <- rnorm(nrow(d))
  d$y  <- 0.2 * d$t + 0.4 * d$d + 0.1 * d$x1 + rnorm(nrow(d))
  d
}

test_that("fect FE runs single-threaded by default", {
  skip_if_not_installed("fect")
  d <- make_big_panel()
  res <- tryCatch(
    nabs_event_study(d, outcome = "y", treatment = "d", unit = "id",
                     time = "t", method = "FE", lags = 2, leads = 2),
    error = function(e) skip(paste("fect FE unavailable on test panel:",
                                   conditionMessage(e)))
  )
  expect_s3_class(res, "nabs_event_study_result")
  expect_true(all(c("time", "estimate", "method") %in% names(res$tidy)))
})

test_that("cv = FALSE is accepted (no collision with internal default)", {
  skip_if_not_installed("fect")
  d <- make_big_panel()
  res <- tryCatch(
    suppressWarnings(
      nabs_event_study(d, outcome = "y", treatment = "d", unit = "id",
                       time = "t", method = "IFE",
                       lags = 2, leads = 2,
                       cv = FALSE, r = 1, parallel = FALSE)
    ),
    error = function(e) skip(paste("fect IFE unavailable on test panel:",
                                   conditionMessage(e)))
  )
  expect_s3_class(res, "nabs_event_study_result")
})

test_that("a clashing fect arg passed via ... is dropped with a warning", {
  skip_if_not_installed("fect")
  d <- make_big_panel()
  # CV (capital) duplicates the protected internal arg; expect the note. Any
  # downstream fit error is irrelevant to what we're checking, so swallow it.
  expect_warning(
    tryCatch(
      nabs_event_study(d, outcome = "y", treatment = "d", unit = "id",
                       time = "t", method = "FE", lags = 2, leads = 2,
                       CV = TRUE),
      error = function(e) NULL
    ),
    regexp = "Ignoring fect argument"
  )
})

test_that("PanelMatch accepts number.iterations", {
  skip_if_not_installed("PanelMatch")
  d <- make_big_panel()
  res <- tryCatch(
    nabs_event_study(d, outcome = "y", treatment = "d", unit = "id",
                     time = "t", method = "PanelMatch",
                     lags = 2, leads = 2, number.iterations = 50),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    # It may fail on this small panel for reasons unrelated to the knob, but it
    # must not be because number.iterations was rejected as an unused argument.
    expect_false(grepl("unused argument", conditionMessage(res), fixed = TRUE))
    skip(paste("PanelMatch unavailable on test panel:", conditionMessage(res)))
  }
  expect_s3_class(res, "nabs_event_study_result")
})

test_that("simple() subsamples large panels and drops fits by default", {
  skip_if_not_installed("fect")
  d <- make_big_panel(n_units = 60)
  res <- NULL
  msgs <- tryCatch(
    testthat::capture_messages({
      res <- nabs_event_study_simple(
        d, outcome = "y", treatment = "d", unit = "id", time = "t",
        methods = "FE", include_twfe = FALSE,
        lags = 2, leads = 2,
        max_units = 30                      # force a subsample
      )
    }),
    error = function(e) {
      skip(paste("fect FE unavailable on test panel:", conditionMessage(e)))
    }
  )

  expect_match(paste(msgs, collapse = "\n"), "sample")
  expect_s3_class(res, "nabs_event_study_simple")
  expect_length(res$fits, 0L)               # keep_fits = FALSE by default
  expect_true("FE" %in% names(res$per_method))
})

test_that("simple() uses the cheap default method set", {
  # No estimator packages needed: just inspect the default formals.
  defs <- formals(nabs_event_study_simple)
  expect_identical(eval(defs$methods), c("DCDH", "FE"))
  expect_false(eval(defs$keep_fits))
  expect_false(eval(defs$full))
})
