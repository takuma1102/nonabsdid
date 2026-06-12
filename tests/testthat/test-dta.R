# All tests in this file need haven (Suggests).

make_panel <- function(n_id = 6L, n_t = 4L) {
  panel <- expand.grid(id = seq_len(n_id), t = seq_len(n_t))
  panel$d <- as.integer(panel$id %% 2L == 0L & panel$t >= 3L)
  panel$y <- panel$t + panel$d + seq_len(nrow(panel)) / 100
  panel
}

test_that("nabs_read_dta reads a plain .dta file", {
  skip_if_not_installed("haven")
  tmp <- withr::local_tempfile(fileext = ".dta")
  haven::write_dta(make_panel(), tmp)

  out <- nabs_read_dta(tmp, verbose = FALSE)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("id", "t", "d", "y"))
  expect_equal(nrow(out), 24L)
})

test_that("nabs_read_dta converts labelled columns to factor by default", {
  skip_if_not_installed("haven")
  tmp <- withr::local_tempfile(fileext = ".dta")
  panel <- make_panel()
  panel$d <- haven::labelled(
    as.double(panel$d),
    labels = c(untreated = 0, treated = 1)
  )
  haven::write_dta(panel, tmp)

  out_f <- nabs_read_dta(tmp, verbose = FALSE)
  expect_s3_class(out_f$d, "factor")
  expect_setequal(levels(out_f$d), c("untreated", "treated"))

  out_n <- nabs_read_dta(tmp, labelled = "numeric", verbose = FALSE)
  expect_type(out_n$d, "double")
  expect_false(inherits(out_n$d, "haven_labelled"))
  expect_setequal(unique(out_n$d), c(0, 1))

  out_k <- nabs_read_dta(tmp, labelled = "keep", verbose = FALSE)
  expect_s3_class(out_k$d, "haven_labelled")
})

test_that("nabs_read_dta collapses tagged NAs by default", {
  skip_if_not_installed("haven")
  tmp <- withr::local_tempfile(fileext = ".dta")
  panel <- make_panel()
  panel$y[1] <- haven::tagged_na("a")
  haven::write_dta(panel, tmp)

  out <- nabs_read_dta(tmp, verbose = FALSE)
  expect_true(is.na(out$y[1]))
  expect_false(any(haven::is_tagged_na(out$y)))

  out_keep <- nabs_read_dta(tmp, missings = "keep", verbose = FALSE)
  expect_true(haven::is_tagged_na(out_keep$y[1], "a"))
})

test_that("nabs_read_dta errors on missing files and bad paths", {
  skip_if_not_installed("haven")
  expect_error(nabs_read_dta("no/such/file.dta"), "does not exist")
  expect_error(nabs_read_dta(c("a.dta", "b.dta")), "single file path")
})

test_that("nabs_write_dta writes a tidy table with Stata-valid names", {
  skip_if_not_installed("haven")
  tmp <- withr::local_tempfile(fileext = ".dta")

  tidy <- as_nabs_event_study(
    data.frame(time = -2:2,
               estimate = c(0.1, 0, 0.5, 0.6, 0.7),
               std.error = 0.1),
    method = "TEST", outcome = "y"
  )
  expect_message(nabs_write_dta(tidy, tmp), "Renamed for Stata")

  back <- haven::read_dta(tmp)
  expect_named(back, c("time", "estimate", "std_error",
                       "conf_low", "conf_high",
                       "window", "method", "outcome"))
  expect_equal(nrow(back), 5L)
  expect_equal(back$estimate, tidy$estimate)
})

test_that("nabs_write_dta returns the path invisibly", {
  skip_if_not_installed("haven")
  tmp <- withr::local_tempfile(fileext = ".dta")
  tidy <- as_nabs_event_study(data.frame(time = 0L, estimate = 1))
  out <- withVisible(nabs_write_dta(tidy, tmp, verbose = FALSE))
  expect_false(out$visible)
  expect_identical(out$value, tmp)
  expect_true(file.exists(tmp))
})

test_that("resolve_panel_data dispatches .dta paths, passes frames through", {
  skip_if_not_installed("haven")
  tmp <- withr::local_tempfile(fileext = ".dta")
  haven::write_dta(make_panel(), tmp)

  expect_message(
    out <- nonabsdid:::resolve_panel_data(tmp),
    "nabs_read_dta"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 24L)

  df <- make_panel()
  expect_identical(nonabsdid:::resolve_panel_data(df), df)
})
