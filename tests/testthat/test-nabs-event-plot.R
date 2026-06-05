make_tbl <- function(method, times, est, se = 0.1) {
  df <- data.frame(time = times, estimate = est, std.error = se)
  as_nabs_event_study(df, method = method)
}

test_that("nabs_event_plot returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  d <- make_tbl("DCDH",       -3:3, c(0,0,0,0.4,0.6,0.7,0.5))
  p <- make_tbl("PanelMatch", -3:3, c(0.1,0,0,0.5,0.5,0.6,0.55))
  i <- make_tbl("IFE",        -3:3, c(-0.05,0.05,0,0.45,0.55,0.65,0.55))

  g <- nabs_event_plot(d, p, i, xlim = c(-3, 3), ylim = c(-1, 1.5))
  expect_s3_class(g, "ggplot")
})

test_that("nabs_event_plot accepts a single list of tibbles", {
  skip_if_not_installed("ggplot2")
  d <- make_tbl("DCDH", -2:2, c(0, 0, 0, 0.5, 0.6))
  i <- make_tbl("IFE",  -2:2, c(0, 0, 0, 0.4, 0.5))
  g <- nabs_event_plot(list(d, i))
  expect_s3_class(g, "ggplot")
})

test_that("nabs_event_plot includes the reference series when provided", {
  skip_if_not_installed("ggplot2")
  d <- make_tbl("DCDH", -2:2, c(0, 0, 0, 0.5, 0.6))
  ref <- make_tbl("TWFE", -2:2, c(0.1, 0.05, 0, 0.7, 0.9))
  g <- nabs_event_plot(d, reference = ref)
  expect_s3_class(g, "ggplot")
  # The reference is folded into the same position-dodged layers as the main
  # series (so it gets its own horizontal slot instead of sitting on top of
  # the centre series), rather than being drawn as separate layers. Verify
  # its rows are part of the plot data and that the plot still builds.
  expect_true("TWFE" %in% g$data$method)
  expect_no_error(ggplot2::ggplot_build(g))
})

test_that("nabs_event_plot warns and fills in missing palette entries", {
  skip_if_not_installed("ggplot2")
  weird <- make_tbl("MyMethod", -1:1, c(0, 0, 0.5))
  expect_warning(g <- nabs_event_plot(weird), "No palette entry")
  expect_s3_class(g, "ggplot")
})

test_that("nabs_event_plot accepts a named-vector palette override", {
  skip_if_not_installed("ggplot2")
  weird <- make_tbl("Custom", -1:1, c(0, 0, 0.5))
  pal <- c("Custom_pre" = "#000000", "Custom_post" = "#FF0000")
  expect_silent(g <- nabs_event_plot(weird, palette = pal))
  expect_s3_class(g, "ggplot")
})

test_that("nabs_event_plot errors on no input", {
  expect_error(nabs_event_plot(), "at least one")
})

test_that("x-axis uses even integer breaks, not half-integers", {
  skip_if_not_installed("ggplot2")
  d <- make_tbl("DCDH", -4:4, c(0, 0, 0, 0, 0.4, 0.5, 0.6, 0.6, 0.5))
  g <- nabs_event_plot(d, xlim = c(-4, 4))

  b <- ggplot2::ggplot_build(g)
  brks <- b$layout$panel_params[[1]]$x$breaks
  brks <- brks[!is.na(brks)]
  # Event-study time is integer; ggplot2's default can land on 2.5.
  expect_true(all(brks %% 2 == 0))
  # Zero must sit on the grid.
  expect_true(0 %in% brks)
})

test_that("x_break_by controls tick spacing", {
  skip_if_not_installed("ggplot2")
  d <- make_tbl("DCDH", -6:6, c(0, 0, 0, 0, 0, 0, 0, 0.3, 0.4, 0.5, 0.5, 0.4, 0.4))
  g <- nabs_event_plot(d, xlim = c(-6, 6), x_break_by = 3)

  b <- ggplot2::ggplot_build(g)
  brks <- b$layout$panel_params[[1]]$x$breaks
  brks <- brks[!is.na(brks)]
  expect_true(all(brks %% 3 == 0))
})
