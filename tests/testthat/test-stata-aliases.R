# Tests for translate_stata_dots() and its wiring into the wrappers.
# None of these need the estimator packages: alias resolution happens (and
# alias conflicts error) before any estimator is touched.

no_have <- list(data = FALSE, unit = FALSE, lags = FALSE, leads = FALSE)

test_that("group/df/placebo/effects translate to canonical names", {
  dots <- list(group = "id", df = mtcars, placebo = 6, effects = 8,
               normalized = TRUE)
  st <- suppressMessages(nonabsdid:::translate_stata_dots(dots, no_have))

  expect_identical(st$values$unit, "id")
  expect_identical(st$values$data, mtcars)
  expect_identical(st$values$lags, 6L)
  expect_identical(st$values$leads, 7L)  # effects - 1

  # Untranslated extras survive; consumed aliases are gone.
  expect_named(st$dots, "normalized")
})

test_that("translation is announced, and quiet = TRUE silences it", {
  expect_message(
    nonabsdid:::translate_stata_dots(list(group = "id"), no_have),
    "group"
  )
  expect_silent(
    nonabsdid:::translate_stata_dots(list(group = "id"), no_have,
                                     quiet = TRUE)
  )
})

test_that("supplying both canonical and alias errors", {
  have_unit <- modifyList(no_have, list(unit = TRUE))
  expect_error(
    nonabsdid:::translate_stata_dots(list(group = "id"), have_unit),
    "group"
  )
  have_leads <- modifyList(no_have, list(leads = TRUE))
  expect_error(
    nonabsdid:::translate_stata_dots(list(effects = 8), have_leads),
    "effects"
  )
})

test_that("invalid effects/placebo values error informatively", {
  expect_error(
    nonabsdid:::translate_stata_dots(list(effects = 0), no_have),
    "effects"
  )
  expect_error(
    nonabsdid:::translate_stata_dots(list(placebo = -1), no_have),
    "placebo"
  )
  expect_error(
    nonabsdid:::translate_stata_dots(list(effects = "eight"), no_have),
    "effects"
  )
})

test_that("empty dots pass through untouched, silently", {
  expect_silent(st <- nonabsdid:::translate_stata_dots(list(), no_have))
  expect_length(st$values, 0L)
  expect_length(st$dots, 0L)
})

test_that("nabs_event_study rejects canonical + alias conflicts up front", {
  panel <- expand.grid(id = 1:4, t = 1:4)
  panel$d <- as.integer(panel$id %% 2L == 0L & panel$t >= 3L)
  panel$y <- rnorm(nrow(panel))

  expect_error(
    nabs_event_study(panel, outcome = "y", treatment = "d",
                     unit = "id", time = "t", method = "DCDH",
                     group = "id"),
    "group"
  )
})

test_that("nabs_event_study_simple resolves group= before validation", {
  panel <- expand.grid(id = 1:4, t = 1:4)
  panel$d <- as.integer(panel$id %% 2L == 0L & panel$t >= 3L)
  panel$y <- rnorm(nrow(panel))

  # With a nonexistent column passed via the alias, the error should be the
  # wrapper's own "column not found" check -- proof that `group` was mapped
  # onto `unit` before validation ran.
  expect_error(
    suppressMessages(
      nabs_event_study_simple(panel, outcome = "y", treatment = "d",
                              time = "t", group = "nope", verbose = FALSE)
    ),
    "nope"
  )
})
