# Direct coverage for small internal helpers and a few validation branches
# that the existing integration tests only reach when a suggested estimator
# package is installed. These run everywhere.

test_that("even_breaks anchors on multiples of `by` and keeps 0 on the grid", {
  f <- get("even_breaks", envir = asNamespace("nonabsdid"))
  expect_equal(f(c(-3, 4), by = 2), c(-4, -2, 0, 2, 4))
  expect_equal(f(c(-1, 1), by = 2), c(-2, 0, 2))
  expect_equal(f(c(0, 9), by = 3), c(0, 3, 6, 9))
})

test_that("backtick_name quotes names and escapes embedded backticks", {
  f <- get("backtick_name", envir = asNamespace("nonabsdid"))
  expect_identical(f("a b"), "`a b`")
  expect_identical(f("x`y"), "`x``y`")
  expect_identical(f(c("a", "b")), c("`a`", "`b`"))
})

test_that("recycle_or_pad handles NULL, scalars, full vectors, and bad lengths", {
  f <- get("recycle_or_pad", envir = asNamespace("nonabsdid"))
  expect_equal(f(NULL, 3L), rep(NA_real_, 3L))
  expect_equal(f(5, 3L), c(5, 5, 5))
  expect_equal(f(c(1, 2, 3), 3L), c(1, 2, 3))
  expect_error(f(c(1, 2), 3L), "length 1 or 3")
})

test_that("new_event_study_tbl errors when time and estimate differ in length", {
  f <- get("new_event_study_tbl", envir = asNamespace("nonabsdid"))
  expect_error(f(1:3, 1:2), "same length")
})

test_that("is_nonnegative_integerish recognises valid and invalid inputs", {
  f <- get("is_nonnegative_integerish", envir = asNamespace("nonabsdid"))
  expect_true(f(0))
  expect_true(f(5))
  expect_true(f(5L))
  expect_false(f(-1))
  expect_false(f(2.5))
  expect_false(f(NA_real_))
  expect_false(f(Inf))
  expect_false(f(c(1, 2)))
  expect_false(f("3"))
})

test_that("check_character_scalar returns invisibly and errors on bad input", {
  f <- get("check_character_scalar", envir = asNamespace("nonabsdid"))
  out <- withVisible(f("y", "outcome"))
  expect_false(out$visible)
  expect_identical(out$value, "y")
  expect_error(f(1, "outcome"), "outcome")
  expect_error(f(c("a", "b"), "unit"), "unit")
  expect_error(f(NA_character_, "time"), "time")
})

test_that("parse_panelmatch_times errors on empty / unnamed input", {
  f <- get("parse_panelmatch_times", envir = asNamespace("nonabsdid"))
  expect_error(f(character(0)), "no names")
  expect_error(f(NULL), "no names")
})

test_that("fect_method_label maps known and unknown labels", {
  f <- get("fect_method_label", envir = asNamespace("nonabsdid"))
  expect_identical(f("fe"), "FE")
  expect_identical(f("ife"), "IFE")
  expect_identical(f("mc"), "MC")
  expect_identical(f("polynomial"), "Polynomial")
  expect_identical(f(NULL), "IFE")
  expect_identical(f(character(0)), "IFE")
  expect_identical(f("something-new"), "IFE")
})

test_that("resolve_panel_data passes non-.dta inputs straight through", {
  f <- get("resolve_panel_data", envir = asNamespace("nonabsdid"))
  df <- data.frame(id = 1, t = 1, y = 0)
  expect_identical(f(df), df)
  # A character that is not a .dta path is returned unchanged (no haven call).
  expect_identical(f("not-a-dta-file.csv"), "not-a-dta-file.csv")
})

test_that("collect_event_studies unwraps a single list and coerces frames", {
  f <- get("collect_event_studies", envir = asNamespace("nonabsdid"))
  df <- data.frame(time = -1:1, estimate = c(0, 0, 0.5),
                   std.error = 0.1, method = "DCDH")
  out <- f(list(df))
  expect_length(out, 1L)
  expect_s3_class(out[[1]], "nabs_event_study_tbl")
})

test_that("naive_twfe validates cluster, controls and conf.level before fitting", {
  d <- data.frame(id = rep(1:2, each = 2), t = rep(1:2, 2),
                  d = c(0L, 1L, 0L, 0L), y = rnorm(4))
  expect_error(
    naive_twfe(d, "y", "d", "id", "t", cluster = 1),
    "cluster"
  )
  expect_error(
    naive_twfe(d, "y", "d", "id", "t", controls = 1),
    "controls"
  )
  expect_error(
    naive_twfe(d, "y", "d", "id", "t", conf.level = 1.5),
    "conf.level"
  )
  expect_error(
    naive_twfe(d, "y", "d", "id", "t", conf.level = 0),
    "conf.level"
  )
})

test_that("naive_twfe rejects a non-0/1 treatment column", {
  d <- data.frame(id = rep(1:2, each = 2), t = rep(1:2, 2),
                  d = c(0L, 2L, 0L, 3L), y = rnorm(4))
  expect_error(
    naive_twfe(d, "y", "d", "id", "t"),
    "0/1"
  )
})
