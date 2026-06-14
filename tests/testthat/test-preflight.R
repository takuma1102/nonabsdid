# Tests for the guard layer (preflight_panel) and the small internal helpers
# it relies on. These deliberately avoid the suggested estimator packages so
# they run everywhere; the friction these lock in is exactly what used to make
# the package hard to get running on a real panel.

pf      <- get("preflight_panel",  envir = asNamespace("nonabsdid"))
dn      <- get("drop_nulls",       envir = asNamespace("nonabsdid"))
mun     <- get("make_unique_name", envir = asNamespace("nonabsdid"))
wls     <- get("with_local_seed",  envir = asNamespace("nonabsdid"))

# A small, valid non-absorbing panel: character unit + string cluster.
make_panel <- function() {
  d <- expand.grid(ori = c("aa", "bb", "cc", "dd"), year = 1:6,
                   stringsAsFactors = FALSE)
  d <- d[order(d$ori, d$year), ]
  d$state <- ifelse(d$ori %in% c("aa", "bb"), "alaska", "alabama")
  d$d     <- as.integer(d$ori %in% c("aa", "cc") & d$year %in% 3:4) # switches off
  d$y     <- rnorm(nrow(d))
  d$x1    <- rnorm(nrow(d))
  d
}

test_that("character unit id is coerced to an integer code column", {
  d <- make_panel()
  out <- pf(d, outcome = "y", treatment = "d", unit = "ori", time = "year",
            quiet = TRUE)

  expect_false(identical(out$unit, "ori"))          # unit was renamed
  expect_true(out$unit %in% names(out$data))         # new column exists
  expect_true(is.numeric(out$data[[out$unit]]))      # and is numeric
  # Coercion only relabels: same number of distinct ids.
  expect_equal(dplyr::n_distinct(out$data[[out$unit]]),
               dplyr::n_distinct(d$ori))
})

test_that("string cluster is coerced, and a default cluster follows the unit", {
  d <- make_panel()

  # explicit string cluster
  out1 <- pf(d, outcome = "y", treatment = "d", unit = "ori", time = "year",
             cluster = "state", quiet = TRUE)
  expect_true(is.numeric(out1$data[[out1$cluster]]))
  expect_false(identical(out1$cluster, "state"))

  # cluster defaulting to the (character) unit should follow the coerced unit
  out2 <- pf(d, outcome = "y", treatment = "d", unit = "ori", time = "year",
             cluster = "ori", quiet = TRUE)
  expect_identical(out2$cluster, out2$unit)
})

test_that("numeric unit/cluster are left untouched", {
  d <- make_panel()
  d$id <- as.integer(factor(d$ori))
  out <- pf(d, outcome = "y", treatment = "d", unit = "id", time = "year",
            quiet = TRUE)
  expect_identical(out$unit, "id")          # no rename
  expect_false("nabs_unit_id" %in% names(out$data))
})

test_that("a 0/1 treatment is required, but logical and \"0\"/\"1\" are accepted", {
  d <- make_panel()

  bad <- d; bad$d <- bad$d * 2L            # 0/2, not 0/1
  expect_error(
    pf(bad, outcome = "y", treatment = "d", unit = "ori", time = "year",
       quiet = TRUE),
    regexp = "0/1"
  )

  lg <- d; lg$d <- as.logical(lg$d)
  expect_silent(
    pf(lg, outcome = "y", treatment = "d", unit = "ori", time = "year",
       quiet = TRUE)
  )

  ch <- d; ch$d <- as.character(ch$d)
  expect_silent(
    pf(ch, outcome = "y", treatment = "d", unit = "ori", time = "year",
       quiet = TRUE)
  )
})

test_that("missing columns and all-NA controls error clearly", {
  d <- make_panel()

  expect_error(
    pf(d, outcome = "nope", treatment = "d", unit = "ori", time = "year",
       quiet = TRUE),
    regexp = "nope"
  )

  d$bad <- NA_real_
  expect_error(
    pf(d, outcome = "y", treatment = "d", unit = "ori", time = "year",
       controls = c("x1", "bad"), quiet = TRUE),
    regexp = "NA"
  )
})

test_that("partial missingness is reported but not fatal", {
  d <- make_panel()
  d$x1[1:3] <- NA
  expect_message(
    pf(d, outcome = "y", treatment = "d", unit = "ori", time = "year",
       controls = "x1", quiet = FALSE),
    regexp = "missing"
  )
})

test_that("drop_nulls keeps only supplied values", {
  expect_equal(dn(list(a = 1, b = NULL, c = 3)), list(a = 1, c = 3))
  expect_equal(dn(list(a = NULL, b = NULL)), list())
  # the knob pattern used by nabs_event_study(): parallel = FALSE is kept.
  expect_equal(dn(list(cv = NULL, parallel = FALSE)), list(parallel = FALSE))
})

test_that("make_unique_name avoids collisions", {
  expect_equal(mun("nabs_unit_id", c("a", "b")), "nabs_unit_id")
  expect_equal(mun("nabs_unit_id", c("nabs_unit_id")), "nabs_unit_id_1")
  expect_equal(mun("nabs_unit_id", c("nabs_unit_id", "nabs_unit_id_1")),
               "nabs_unit_id_2")
})

test_that("with_local_seed is reproducible and restores global RNG state", {
  set.seed(99)
  before <- .Random.seed
  a <- wls(123, sample(1:1e6, 3))
  b <- wls(123, sample(1:1e6, 3))
  expect_equal(a, b)                       # same seed -> same draw
  expect_identical(.Random.seed, before)   # caller's RNG untouched
})
