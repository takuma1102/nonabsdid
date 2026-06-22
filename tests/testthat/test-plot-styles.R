# Coverage for the plotting branches the existing tests don't reach:
# style = "method_shape", the colorblind palette, connecting lines, the
# reference shape handling, and the palette/shape error paths. All need
# ggplot2 (a Suggests), so skip when it's unavailable.

mk <- function(method, times = -3:3,
               est = c(0, 0, 0, 0.4, 0.6, 0.7, 0.5), se = 0.1) {
  as_nabs_event_study(
    data.frame(time = times, estimate = est, std.error = se),
    method = method
  )
}

test_that("method_shape style with a connecting line builds", {
  skip_if_not_installed("ggplot2")
  d <- mk("DCDH")
  p <- mk("PanelMatch")
  g <- nabs_event_plot(d, p, style = "method_shape", connect = TRUE)
  expect_s3_class(g, "ggplot")
  expect_no_error(ggplot2::ggplot_build(g))
})

test_that("method_shape style folds in a reference series", {
  skip_if_not_installed("ggplot2")
  d <- mk("DCDH")
  ref <- mk("TWFE")
  g <- nabs_event_plot(d, style = "method_shape", reference = ref,
                       connect = TRUE)
  expect_s3_class(g, "ggplot")
  expect_true("TWFE" %in% g$data$method)
  expect_no_error(ggplot2::ggplot_build(g))
})

test_that("colorblind palette works for both styles", {
  skip_if_not_installed("ggplot2")
  d <- mk("DCDH")
  i <- mk("IFE")
  expect_s3_class(
    nabs_event_plot(d, i, palette = "colorblind"),
    "ggplot"
  )
  expect_s3_class(
    nabs_event_plot(d, i, style = "method_shape", palette = "colorblind"),
    "ggplot"
  )
})

test_that("show_pre_post_legend = FALSE collapses the per-window labels", {
  skip_if_not_installed("ggplot2")
  d <- mk("DCDH")
  g <- nabs_event_plot(d, show_pre_post_legend = FALSE)
  expect_s3_class(g, "ggplot")
})

test_that("custom shapes are honoured under method_shape", {
  skip_if_not_installed("ggplot2")
  d <- mk("DCDH")
  p <- mk("PanelMatch")
  g <- nabs_event_plot(d, p, style = "method_shape",
                       shapes = c(DCDH = 1L, PanelMatch = 2L))
  expect_s3_class(g, "ggplot")
})

test_that("unknown method under method_shape warns but still draws (spare shapes)", {
  skip_if_not_installed("ggplot2")
  weird <- mk("Weird", times = -1:1, est = c(0, 0, 0.5))
  expect_warning(
    nabs_event_plot(weird, style = "method_shape"),
    "No palette entry"
  )
  g <- suppressWarnings(nabs_event_plot(weird, style = "method_shape"))
  expect_s3_class(g, "ggplot")
})

test_that("connect = TRUE with an xlim drops off-window points", {
  skip_if_not_installed("ggplot2")
  d <- mk("DCDH")
  g <- nabs_event_plot(d, connect = TRUE, xlim = c(-2, 2))
  expect_s3_class(g, "ggplot")
  expect_true(all(g$data$time >= -2 & g$data$time <= 2))
})

test_that("ylim alone triggers coord_cartesian without an xlim", {
  skip_if_not_installed("ggplot2")
  d <- mk("DCDH")
  g <- nabs_event_plot(d, ylim = c(-1, 1.5))
  expect_s3_class(g, "ggplot")
})

test_that("unknown palette name and malformed palette error clearly", {
  skip_if_not_installed("ggplot2")
  d <- mk("DCDH")
  expect_error(nabs_event_plot(d, palette = "no-such-palette"),
               "Unknown palette")
  expect_error(nabs_event_plot(d, palette = 1:3),
               "name or a named character vector")
})

test_that("a bare data frame is coerced on the way into the plot", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(time = -1:1, estimate = c(0, 0, 0.5),
                   std.error = 0.1, method = "DCDH")
  g <- nabs_event_plot(df)
  expect_s3_class(g, "ggplot")
  expect_true("DCDH" %in% g$data$method)
})
