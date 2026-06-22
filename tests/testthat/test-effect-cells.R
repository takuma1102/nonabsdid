# Tests for the experimental cohort-matrix feature. The schema / plotting paths
# run with no estimator packages (data.frame escape hatch). The fect and DCDH
# adapters are exercised only when their packages are installed.

make_cell_df <- function() {
  d <- expand.grid(cohort = c(4L, 6L, 8L), event_time = -2:4)
  d$estimate  <- with(d, ifelse(event_time < 0, 0, 0.4 + 0.05 * event_time))
  d$std.error <- 0.07
  d
}

test_that("data.frame coerces to the cell schema with derived CIs", {
  cells <- as_nabs_effect_cells(make_cell_df(), method = "FE", outcome = "y")
  expect_s3_class(cells, "nabs_effect_cell_tbl")
  expect_true(all(c("cohort", "event_time", "calendar_time", "estimate",
                    "std.error", "conf.low", "conf.high", "n", "window",
                    "method", "outcome", "se_method") %in% names(cells)))
  expect_identical(unique(cells$method), "FE")
  # calendar_time = cohort + event_time
  expect_equal(cells$calendar_time, cells$cohort + cells$event_time)
  # CIs derived from SE under normality
  z <- stats::qnorm(0.975)
  expect_equal(cells$conf.low,  cells$estimate - z * cells$std.error)
  expect_equal(cells$conf.high, cells$estimate + z * cells$std.error)
  # window split on event_time
  expect_identical(cells$window, ifelse(cells$event_time < 0, "pre", "post"))
})

test_that("missing required columns is an error", {
  expect_error(as_nabs_effect_cells(data.frame(cohort = 1, estimate = 1)),
               "event_time")
})

test_that("plot_effect_matrix returns a ggplot and auto-titles single method", {
  skip_if_not_installed("ggplot2")
  cells <- as_nabs_effect_cells(make_cell_df(), method = "FE")
  p <- plot_effect_matrix(cells, show_estimates = TRUE, show_se = TRUE)
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$title, "Fect FE")
})

test_that("plot_effect_matrix facets multiple methods (no overall title)", {
  skip_if_not_installed("ggplot2")
  fe   <- as_nabs_effect_cells(make_cell_df(), method = "FE")
  dcdh <- as_nabs_effect_cells(make_cell_df(), method = "DCDH")
  p <- plot_effect_matrix(fe, dcdh)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
})

test_that("aggregate_effects collapses cells to an event-study tibble", {
  cells <- as_nabs_effect_cells(make_cell_df(), method = "FE")
  es <- suppressMessages(aggregate_effects(cells, by = "event_time"))
  expect_s3_class(es, "nabs_event_study_tbl")
  # one row per distinct event_time, SE intentionally NA
  expect_identical(sort(unique(es$time)), sort(unique(cells$event_time)))
  expect_true(all(is.na(es$std.error)))
})

test_that("fect adapter builds cohort cells from a real fit", {
  skip_if_not_installed("fect")
  set.seed(1)
  N <- 60; TT <- 12
  panel <- expand.grid(id = 1:N, t = 1:TT)
  onset <- c(`1` = 4L, `2` = 7L)[as.character(panel$id %% 3)]
  panel$d <- as.integer(!is.na(onset) & panel$t >= onset)
  panel$y <- 0.15 * panel$t + ifelse(panel$d == 1, 0.4, 0) + rnorm(nrow(panel))

  res <- nabs_effect_cells(panel, outcome = "y", treatment = "d",
                           unit = "id", time = "t", method = "FE",
                           lags = 3, leads = 4, nboots = 20)
  cells <- res$cells
  expect_s3_class(cells, "nabs_effect_cell_tbl")
  # fect surface is treated-only: onset cohort cells start at event_time 0
  expect_true(min(cells$event_time) >= 0)
  expect_true(any(cells$event_time == 0))
  expect_true(all(cells$method == "FE"))
})

test_that("DCDH loop adapter builds cohort cells from a real fit", {
  skip_if_not_installed("DIDmultiplegtDYN")
  skip_if_not_installed("polars")
  set.seed(1)
  N <- 90; TT <- 12
  panel <- expand.grid(id = 1:N, t = 1:TT)
  onset <- c(`1` = 4L, `2` = 7L)[as.character(panel$id %% 3)]   # %% 3 == 0 -> control
  panel$d <- as.integer(!is.na(onset) & panel$t >= onset)
  panel$y <- 0.15 * panel$t + ifelse(panel$d == 1, 0.4, 0) + rnorm(nrow(panel))

  res <- nabs_effect_cells(panel, outcome = "y", treatment = "d",
                           unit = "id", time = "t", method = "DCDH",
                           lags = 2, leads = 3, dcdh_strategy = "loop")
  cells <- res$cells
  expect_s3_class(cells, "nabs_effect_cell_tbl")
  expect_true(all(cells$method == "DCDH"))
  expect_gt(length(unique(cells$cohort)), 1L)
})
