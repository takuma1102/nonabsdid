#' Plot a cohort-by-time effect matrix as a heatmap
#'
#' Draws one or more `nabs_effect_cell_tbl` objects as cohort (rows) by relative
#' or calendar time (columns) heatmaps, with fill encoding the point estimate on
#' a diverging scale centred at zero.
#'
#' The intended use is **one method per plot**: a single-method call gets the
#' method as its title automatically. Passing several methods facets them with a
#' shared scale, but that side-by-side view gets crowded quickly, so for careful
#' comparison prefer separate per-method heatmaps.
#'
#' @param ... One or more `nabs_effect_cell_tbl` objects (bare args or a single
#'   list), typically from [nabs_effect_cells()] or [as_nabs_effect_cells()].
#' @param axis `"event"` (default) puts `event_time` on the x axis; `"calendar"`
#'   uses `calendar_time`.
#' @param facet Logical; facet by `method` when more than one is present.
#'   Default `TRUE`.
#' @param show_estimates Logical; print the rounded estimate in each tile.
#'   Default `FALSE`.
#' @param show_se Logical; print the standard error in parentheses beneath the
#'   estimate (implies showing the estimate). Cells with `NA` SE show the
#'   estimate alone. Default `FALSE`.
#' @param digits Rounding for the in-tile estimate / SE labels. Default `2`.
#' @param text_size Font size for the in-tile labels. Default `2.6`.
#' @param title Plot title. `NULL` (default) auto-titles a single-method plot
#'   with its display label (the fect family shows as `"Fect FE"` / `"Fect IFE"`
#'   / `"Fect MC"`; `"DCDH"` is unchanged); faceted plots are left untitled (the
#'   strips name the methods). Pass a string to override, or `NA` to suppress.
#' @param caption A short gloss of the axes printed under the plot. `NULL`
#'   (default) auto-writes a one-line note (rows = onset cohort, columns =
#'   time since onset / calendar time). Pass a string to override, or `NA` to
#'   suppress.
#' @param low,mid,high Diverging fill colours for negative / zero / positive
#'   estimates.
#' @param limits Optional length-2 numeric fill limits; `NULL` (default) makes
#'   the scale symmetric around zero from the data range.
#' @param na_color Fill for empty `(cohort, time)` cells. Default `"grey92"`.
#' @param xlab,ylab,legend_title Axis and legend labels.
#' @param base_size Base font size for `theme_minimal()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' raw <- expand.grid(cohort = 3:6, event_time = -2:4)
#' raw$estimate  <- with(raw, ifelse(event_time < 0, 0,
#'                                   0.15 * event_time + 0.05 * (cohort - 4)))
#' raw$std.error <- 0.07
#' cells <- as_nabs_effect_cells(raw, method = "FE", outcome = "y")
#' plot_effect_matrix(cells)                                   # auto title "FE"
#' plot_effect_matrix(cells, show_estimates = TRUE, show_se = TRUE)
#' @export
plot_effect_matrix <- function(...,
                               axis = c("event", "calendar"),
                               facet = TRUE,
                               show_estimates = FALSE,
                               show_se = FALSE,
                               digits = 2,
                               text_size = 2.6,
                               title = NULL,
                               caption = NULL,
                               low = "#3182BD", mid = "#F7F7F7", high = "#DE2D26",
                               limits = NULL,
                               na_color = "grey92",
                               xlab = NULL,
                               ylab = "Onset cohort",
                               legend_title = "Effect",
                               base_size = 11) {
  rlang::check_installed("ggplot2")
  axis <- match.arg(axis)

  cells <- collect_effect_cells(list(...))
  if (!length(cells)) {
    cli::cli_abort("Pass at least one {.cls nabs_effect_cell_tbl} to {.fun plot_effect_matrix}.")
  }
  df <- bind_effect_cells(cells)

  df$xval <- if (axis == "event") df$event_time else df$calendar_time
  if (all(is.na(df$xval))) {
    cli::cli_abort("No {.field {paste0(axis, '_time')}} values available to plot.")
  }
  df <- df[!is.na(df$xval), , drop = FALSE]

  # Cohorts as a reversed ordered factor so the earliest onset sits at the top.
  cohort_levels <- sort(unique(df$cohort))
  df$cohort_f <- factor(df$cohort, levels = rev(cohort_levels))

  if (is.null(limits)) {
    rng <- max(abs(df$estimate), na.rm = TRUE)
    limits <- c(-rng, rng)
  }
  if (is.null(xlab)) {
    xlab <- if (axis == "event") "Time relative to onset" else "Calendar time"
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$xval, y = .data$cohort_f, fill = .data$estimate)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::scale_fill_gradient2(
      name = legend_title,
      low = low, mid = mid, high = high,
      midpoint = 0, limits = limits, na.value = na_color
    ) +
    ggplot2::scale_x_continuous(
      breaks = even_breaks(range(df$xval, na.rm = TRUE), 2),
      expand = ggplot2::expansion(mult = 0.01)
    ) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Solid white canvas so the figure drops straight into a README / paper
      # instead of rendering on a transparent (often black-previewed) panel.
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid       = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.caption     = ggplot2::element_text(hjust = 0, color = "grey45",
                                               size = base_size * 0.78,
                                               margin = ggplot2::margin(t = 6)),
      strip.text       = ggplot2::element_text(face = "bold"),
      legend.position  = "right",
      plot.margin      = ggplot2::margin(8, 10, 8, 8)
    )

  # Mark onset (event_time 0) for the relative-time view.
  if (axis == "event") {
    p <- p + ggplot2::geom_vline(xintercept = -0.5, color = "grey40",
                                 linetype = "dotted", linewidth = 0.4)
  }

  if (isTRUE(show_estimates) || isTRUE(show_se)) {
    est_lab <- formatC(df$estimate, format = "f", digits = digits)
    if (isTRUE(show_se)) {
      se_lab <- ifelse(
        is.na(df$std.error), "",
        paste0("(", formatC(df$std.error, format = "f", digits = digits), ")")
      )
      df$lab <- ifelse(nzchar(se_lab), paste0(est_lab, "\n", se_lab), est_lab)
    } else {
      df$lab <- est_lab
    }
    # Adaptive label colour: white on saturated (dark) tiles, near-black on
    # pale ones, so the numbers stay legible across the whole diverging scale.
    denom <- max(abs(limits))
    frac  <- pmin(abs(df$estimate) / denom, 1)
    df$txt_col <- ifelse(!is.na(frac) & frac > 0.55, "white", "grey15")
    p <- p + ggplot2::geom_text(
      data = df,
      ggplot2::aes(x = .data$xval, y = .data$cohort_f,
                   label = .data$lab, color = .data$txt_col),
      inherit.aes = FALSE, size = text_size, lineheight = 0.85
    ) +
      ggplot2::scale_color_identity(guide = "none")
  }

  n_methods <- length(unique(df$method))
  if (isTRUE(facet) && n_methods > 1L) {
    p <- p + ggplot2::facet_wrap(
      ~ .data$method,
      labeller = ggplot2::as_labeller(method_display_label)
    )
  }

  # Auto-title a single-method plot with its (display) method label; leave
  # faceted plots untitled (the strips already name each method).
  if (is.null(title)) {
    if (n_methods == 1L) {
      p <- p + ggplot2::ggtitle(method_display_label(df$method[1]))
    }
  } else if (!is.na(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  # A one-line axis gloss by default, kept short and grey so it informs without
  # crowding the figure. `caption = NA` removes it.
  if (is.null(caption)) {
    caption <- if (axis == "event") {
      "Rows = onset cohort \u00b7 Columns = time since onset (0 = first treated period)"
    } else {
      "Rows = onset cohort \u00b7 Columns = calendar time"
    }
  }
  if (length(caption) && !is.na(caption)) {
    p <- p + ggplot2::labs(caption = caption)
  }
  p
}

# Display name for plot titles / facet strips. The fect-family labels (FE / IFE
# / MC) are prefixed with "Fect" so the estimator family is explicit; DCDH and
# anything else pass through unchanged. The `method` column itself is left as the
# short code, so the schema and downstream code are unaffected.
method_display_label <- function(m) {
  vapply(as.character(m), function(x) {
    switch(toupper(x),
      "FE"  = "Fect FE",
      "IFE" = "Fect IFE",
      "MC"  = "Fect MC",
      x
    )
  }, character(1), USE.NAMES = FALSE)
}

#' Collapse effect cells back onto an event-study path
#'
#' Aggregates a `nabs_effect_cell_tbl` over cohorts to recover a one-dimensional
#' path, returning a `nabs_event_study_tbl` that plugs straight into
#' [nabs_event_plot()]. This makes explicit that the event study is the
#' cohort-collapsed view of the same cells.
#'
#' Point estimates are averaged across cohorts (weighted by `n` when present).
#' Re-aggregated standard errors are **not** computed here -- collapsing SEs
#' correctly needs the estimator's replicate draws -- so `std.error` and the CI
#' columns are returned as `NA`. Use this for a quick overlay, not for inference.
#'
#' @param cells A `nabs_effect_cell_tbl`.
#' @param by Aggregation axis: `"event_time"` (default) or `"calendar_time"`.
#'
#' @return A `nabs_event_study_tbl` (with `NA` standard errors).
#'
#' @examples
#' raw <- expand.grid(cohort = 3:6, event_time = -2:4)
#' raw$estimate <- with(raw, ifelse(event_time < 0, 0, 0.2 * event_time))
#' cells <- as_nabs_effect_cells(raw, method = "FE")
#' aggregate_effects(cells)
#' @export
aggregate_effects <- function(cells, by = c("event_time", "calendar_time")) {
  if (!inherits(cells, "nabs_effect_cell_tbl")) {
    cells <- as_nabs_effect_cells(cells)
  }
  by <- match.arg(by)

  x <- cells[[by]]
  keep <- !is.na(x)
  x <- x[keep]
  est <- cells$estimate[keep]
  w   <- cells$n[keep]
  if (all(is.na(w))) w <- rep(1, length(est))
  w[is.na(w)] <- 0

  num <- tapply(est * w, x, sum, na.rm = TRUE)
  den <- tapply(w,       x, sum, na.rm = TRUE)
  tt  <- as.integer(names(num))

  cli::cli_inform(c(
    "i" = "Aggregated over cohorts; {.field std.error} is {.val NA} \\
           (re-aggregated SEs need replicate draws)."
  ))

  new_event_study_tbl(
    time      = tt,
    estimate  = as.numeric(num / den),
    std.error = NA_real_,
    method    = cells$method[1],
    outcome   = cells$outcome[1],
    conf.level = attr(cells, "conf.level") %||% 0.95
  )
}
