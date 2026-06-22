# Coverage for the print methods and the coercion methods that route through
# `$tidy`. None of these need a suggested estimator package.

mk_tbl <- function(method = "DCDH") {
  as_nabs_event_study(
    data.frame(time = -2:2,
               estimate = c(0, 0, 0, 0.5, 0.6),
               std.error = 0.1),
    method = method, outcome = "y"
  )
}

test_that("print.nabs_event_study_tbl runs and returns invisibly", {
  tbl <- mk_tbl()
  expect_no_error(print(tbl))
  expect_output(print(tbl))                 # prints something
  res <- withVisible(print(tbl))
  expect_false(res$visible)
})

test_that("print.nabs_event_study_result runs", {
  res <- structure(
    list(tidy = mk_tbl(), fit = NULL,
         call = quote(nabs_event_study(panel, method = "DCDH"))),
    class = "nabs_event_study_result"
  )
  expect_no_error(print(res))
  out <- withVisible(print(res))
  expect_false(out$visible)
  expect_identical(out$value, res)
})

test_that("print.nabs_event_study_simple covers both the fits and no-fits paths", {
  base <- list(
    plot       = NULL,
    tidy       = mk_tbl(),
    per_method = list(DCDH = mk_tbl("DCDH")),
    fits       = list(),
    twfe       = NULL,
    call       = quote(nabs_event_study_simple(panel))
  )

  no_fits <- structure(base, class = "nabs_event_study_simple")
  expect_no_error(print(no_fits))

  with_fits <- structure(
    modifyList(base, list(fits = list(DCDH = "native-object"),
                          twfe = mk_tbl("TWFE"))),
    class = "nabs_event_study_simple"
  )
  expect_no_error(print(with_fits))
})

test_that("as_nabs_event_study.list binds a list of tibbles", {
  out <- as_nabs_event_study(list(mk_tbl("DCDH"), mk_tbl("IFE")))
  expect_s3_class(out, "nabs_event_study_tbl")
  expect_setequal(unique(out$method), c("DCDH", "IFE"))
})

test_that("as_nabs_event_study dispatches on result and simple objects", {
  res <- structure(
    list(tidy = mk_tbl("DCDH"), fit = NULL, call = quote(f())),
    class = "nabs_event_study_result"
  )
  out_res <- as_nabs_event_study(res)
  expect_s3_class(out_res, "nabs_event_study_tbl")
  expect_identical(unique(out_res$method), "DCDH")

  simp <- structure(
    list(plot = NULL, tidy = mk_tbl("IFE"), per_method = list(),
         fits = list(), twfe = NULL, call = quote(f())),
    class = "nabs_event_study_simple"
  )
  out_simp <- as_nabs_event_study(simp, method = "Relabelled")
  expect_s3_class(out_simp, "nabs_event_study_tbl")
  expect_identical(unique(out_simp$method), "Relabelled")
})

test_that("idempotent method relabels both method and outcome", {
  tbl <- mk_tbl("A")
  out <- as_nabs_event_study(tbl, method = "B", outcome = "z")
  expect_identical(unique(out$method), "B")
  expect_identical(unique(out$outcome), "z")
})
