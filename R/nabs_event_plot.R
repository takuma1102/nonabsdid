#' Plot one or more event-study tibbles on a single panel
#'
#' Overlays event-study estimates from any combination of supported estimators
#' on a single ggplot2 panel. Two visual encodings are available via `style`:
#'
#' * `"prepost_color"` (default) -- each method gets its own color, with
#'   separate shades for pre- and post-treatment periods, mirroring common
#'   conventions in DCDH-style plots. Points are drawn as circles throughout.
#' * `"method_shape"` -- each method gets a single color (pre and post share
#'   it), and the pre/post distinction is carried by the *marker shape*
#'   instead (e.g. hollow circles for pre, filled triangles for post). Useful
#'   for grayscale printing or when color should encode method only.
#'
#' An optional `reference` series -- typically a naive TWFE fit from
#' [naive_twfe()] -- is drawn in a neutral color (default black) so the reader
#' can see what the heterogeneity-robust estimators are correcting against.
#'
#' Set `connect = TRUE` to join each series' point estimates with a thin line,
#' in addition to the points and error bars.
#'
#' @param ... One or more `nabs_event_study_tbl` objects. Bare arguments and a
#'   single list are both accepted.
#' @param style Visual encoding of the pre/post distinction. One of
#'   `"prepost_color"` (default; color differs by pre/post) or
#'   `"method_shape"` (color by method only, pre/post shown by marker shape).
#' @param connect Logical. If `TRUE`, point estimates within each series are
#'   joined by a thin line. Default `FALSE`. The line is split at the
#'   treatment boundary so pre- and post-treatment segments are not joined
#'   across the discontinuity.
#' @param connect_linewidth Width of the connecting line when `connect = TRUE`.
#'   Default `0.4`.
#' @param reference Optional `nabs_event_study_tbl` to draw as a neutral-color
#'   reference layer (typically a naive TWFE estimate). Drawn under the
#'   main series.
#' @param reference_color Color for the reference series. Default `"grey20"`.
#' @param palette Either `"default"` (the package's built-in palette,
#'   patterned after the DCDH/PanelMatch/IFE conventions in the codebase
#'   this package was extracted from), `"colorblind"` (Okabe-Ito), or a
#'   named character vector of colors. For `style = "prepost_color"` the names
#'   are keyed by `"<method>_<window>"`, e.g.
#'   `c("DCDH_pre" = "#DE2D26", "DCDH_post" = "#3182BD", ...)`. For
#'   `style = "method_shape"` the names are keyed by `"<method>"`, e.g.
#'   `c("DCDH" = "#3182BD", ...)`.
#' @param xlim,ylim Numeric length-2 vectors for axis limits. `NULL` lets
#'   ggplot2 choose.
#' @param dodge Width of the position-dodge applied to points, lines, and
#'   error bars. The `reference` series shares this dodge with the main
#'   series, so all series (including the naive TWFE reference) get their own
#'   evenly-spaced horizontal slot and their CIs do not overlap. Default `0.5`.
#' @param point_size,errorbar_width Aesthetic controls for the geom layers.
#' @param show_pre_post_legend Logical. Only relevant for
#'   `style = "prepost_color"`. If `TRUE`, the legend keys are labeled
#'   `"<method>; pre"` / `"<method>; post"`. If `FALSE`, only one key per
#'   method is shown. Default `TRUE`. (For `style = "method_shape"` color is
#'   keyed by method, and a separate shape legend shows pre/post.)
#' @param xlab,ylab Axis labels.
#' @param base_size Base font size passed to `theme_minimal()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' # Build two tidy series with the dependency-free data.frame coercion,
#' # then overlay them. Only ggplot2 (an Imports dependency) is needed,
#' # so this example runs without any of the estimator packages.
#' dcdh <- as_nabs_event_study(
#'   data.frame(time      = -3:4,
#'              estimate  = c(-0.02, 0.01, 0.00, 0.03, 0.28, 0.40, 0.37, 0.46),
#'              std.error = 0.10),
#'   method = "DCDH", outcome = "y"
#' )
#' ife <- as_nabs_event_study(
#'   data.frame(time      = -3:4,
#'              estimate  = c(0.00, -0.01, 0.02, 0.01, 0.33, 0.46, 0.40, 0.52),
#'              std.error = 0.11),
#'   method = "IFE", outcome = "y"
#' )
#' nabs_event_plot(dcdh, ife, xlim = c(-3, 4), ylab = "Effect on y")
#' @export
nabs_event_plot <- function(...,
                       style = c("prepost_color", "method_shape"),
                       connect = FALSE,
                       connect_linewidth = 0.4,
                       reference = NULL,
                       reference_color = "grey20",
                       palette = "default",
                       xlim = NULL,
                       ylim = NULL,
                       dodge = 0.5,
                       point_size = 2.5,
                       errorbar_width = 0.1,
                       show_pre_post_legend = TRUE,
                       xlab = "Relative time to treatment change",
                       ylab = "Estimated effect",
                       base_size = 11) {
  rlang::check_installed("ggplot2")
  style <- match.arg(style)

  series <- collect_event_studies(list(...))
  if (length(series) == 0L) {
    cli::cli_abort("Pass at least one {.cls nabs_event_study_tbl} to {.fun nabs_event_plot}.")
  }

  df <- bind_event_studies(series)
  df$window <- ifelse(df$time < 0, "pre", "post")
  # `cgrp` is the color grouping; `dgrp` the dodge slot (always per method, so
  # every method occupies one evenly-spaced horizontal slot and connecting
  # lines line up with the points). `shp` is the marker shape.
  if (style == "prepost_color") {
    df$cgrp <- paste(df$method, df$window, sep = "_")
    df$shp  <- "pt"                       # single shape (circle) throughout
  } else {                                # method_shape
    df$cgrp <- df$method
    df$shp  <- df$window                  # pre vs post -> different markers
  }
  df$dgrp <- df$method
  df$role <- "main"                       # lets the reference take a lighter look
  # Segment id so connecting lines don't bridge the pre/post discontinuity.
  df$seg  <- paste(df$dgrp, df$window, sep = "@@")

  # Resolve the palette on the main series only (before the reference is
  # folded in), so resolve_palette() doesn't warn about a missing key.
  pal <- resolve_palette(palette, df, style)

  # Fold the reference (e.g. naive TWFE) into the SAME data frame so it shares
  # the position_dodge with the main series. Drawn as its own layer it would
  # only see one group and stay centred on the integer x -- which is exactly
  # why it used to overlap the centre series (IFE). Sharing the dodge gives it
  # its own evenly-spaced slot.
  ref_key <- NULL
  if (!is.null(reference)) {
    if (!inherits(reference, "nabs_event_study_tbl")) {
      reference <- as_nabs_event_study(reference)
    }
    reference$window <- ifelse(reference$time < 0, "pre", "post")
    ref_key <- paste(unique(reference$method), collapse = "/")
    reference$cgrp <- ref_key
    reference$dgrp <- ref_key
    reference$role <- "ref"
    reference$shp  <- if (style == "method_shape") "ref" else "pt"
    reference$seg  <- paste(ref_key, reference$window, sep = "@@")
    pal <- c(pal, stats::setNames(reference_color, ref_key))

    common <- intersect(names(df), names(reference))
    df <- dplyr::bind_rows(df[, common, drop = FALSE],
                           reference[, common, drop = FALSE])
  }

  # Build color-legend labels.
  if (style == "prepost_color") {
    if (isTRUE(show_pre_post_legend)) {
      label_for <- function(k) {
        parts <- strsplit(k, "_", fixed = TRUE)[[1]]
        paste0(parts[1], "; ", parts[2])
      }
    } else {
      label_for <- function(k) strsplit(k, "_", fixed = TRUE)[[1]][1]
    }
  } else {
    label_for <- function(k) k             # already the method name
  }
  lbls <- vapply(names(pal), label_for, character(1))
  if (!is.null(ref_key)) {
    lbls[ref_key] <- paste0(ref_key, " (naive)")
  }

  pos <- ggplot2::position_dodge(width = dodge)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$time, y = .data$estimate,
                 color = .data$cgrp, shape = .data$shp,
                 group = .data$dgrp)   # one dodge slot per method
  )

  # Optional thin connecting line, drawn first so points/bars sit on top.
  if (isTRUE(connect)) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(alpha = .data$role, group = .data$seg),
        linewidth = connect_linewidth,
        position  = pos,
        show.legend = FALSE
      )
  }

  p <- p +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high,
                   alpha = .data$role),
      width = errorbar_width,
      linewidth = 0.8,
      position = pos
    ) +
    ggplot2::geom_point(
      ggplot2::aes(alpha = .data$role, size = .data$role),
      position = pos
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = pal,
      breaks = names(pal),
      labels = lbls
    ) +
    ggplot2::scale_alpha_manual(
      values = c("main" = 1, "ref" = 0.7),
      guide  = "none"
    ) +
    ggplot2::scale_size_manual(
      values = c("main" = point_size, "ref" = point_size * 0.85),
      guide  = "none"
    ) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.key.width = ggplot2::unit(1.5, "lines")
    )

  if (style == "prepost_color") {
    # All one shape (circle); hide the shape legend.
    p <- p +
      ggplot2::scale_shape_manual(values = c("pt" = 16L)) +
      ggplot2::guides(shape = "none")
  } else {
    # Shape carries pre/post; keep its legend.
    p <- p +
      ggplot2::scale_shape_manual(
        name   = NULL,
        values = c("pre" = 1L, "post" = 17L, "ref" = 15L),
        breaks = c("pre", "post"),
        labels = c("pre" = "pre", "post" = "post")
      )
  }

  if (!is.null(xlim) || !is.null(ylim)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  }

  p
}

# ----- internal helpers ------------------------------------------------------

# Accept either bare nabs_event_study_tbl args or a single list of them.
collect_event_studies <- function(dots) {
  if (length(dots) == 1L && is.list(dots[[1]]) &&
      !inherits(dots[[1]], "nabs_event_study_tbl") &&
      !inherits(dots[[1]], "data.frame")) {
    dots <- dots[[1]]
  }
  lapply(dots, function(x) {
    if (inherits(x, "nabs_event_study_tbl")) x else as_nabs_event_study(x)
  })
}

# The default palette mirrors the conventions used in the codebase this
# package was extracted from: red shades for pre-treatment, blue/green for
# post-treatment, with each method getting a distinguishable hue pair.
default_palette <- c(
  "DCDH_pre"        = "#DE2D26",
  "DCDH_post"       = "#3182BD",
  "PanelMatch_pre"  = "#FB6A4A",
  "PanelMatch_post" = "#08519C",
  "IFE_pre"         = "#FC9272",
  "IFE_post"        = "#66C2A5",
  "FE_pre"          = "#A50F15",
  "FE_post"         = "#2C7FB8",
  "MC_pre"          = "#CB181D",
  "MC_post"         = "#41B6C4"
)

# Okabe-Ito-derived colorblind-safe palette.
colorblind_palette <- c(
  "DCDH_pre"        = "#D55E00",
  "DCDH_post"       = "#0072B2",
  "PanelMatch_pre"  = "#E69F00",
  "PanelMatch_post" = "#56B4E9",
  "IFE_pre"         = "#CC79A7",
  "IFE_post"        = "#009E73",
  "FE_pre"          = "#882255",
  "FE_post"         = "#117733",
  "MC_pre"          = "#AA4499",
  "MC_post"         = "#44AA99"
)

# Method-level palettes for style = "method_shape" (one color per method).
# These take the "post" hue from each pair above, which reads well as the
# single method color.
default_method_palette <- c(
  "DCDH"       = "#3182BD",
  "PanelMatch" = "#08519C",
  "IFE"        = "#1B9E77",
  "FE"         = "#2C7FB8",
  "MC"         = "#41B6C4"
)

colorblind_method_palette <- c(
  "DCDH"       = "#0072B2",
  "PanelMatch" = "#E69F00",
  "IFE"        = "#009E73",
  "FE"         = "#CC79A7",
  "MC"         = "#D55E00"
)

resolve_palette <- function(palette, df, style = "prepost_color") {
  needed <- unique(df$cgrp)

  builtin <- if (identical(style, "method_shape")) {
    list(default = default_method_palette, colorblind = colorblind_method_palette)
  } else {
    list(default = default_palette, colorblind = colorblind_palette)
  }

  pal <- if (is.character(palette) && length(palette) == 1L) {
    switch(palette,
           default     = builtin$default,
           colorblind  = builtin$colorblind,
           cli::cli_abort("Unknown palette name {.val {palette}}.")
    )
  } else if (is.character(palette) && !is.null(names(palette))) {
    palette
  } else {
    cli::cli_abort("`palette` must be a name or a named character vector.")
  }

  missing <- setdiff(needed, names(pal))
  if (length(missing)) {
    # Fill with grayscale fallbacks; user can supply a custom palette to fix.
    fillers <- grDevices::gray.colors(length(missing), start = 0.2, end = 0.7)
    names(fillers) <- missing
    pal <- c(pal, fillers)
    cli::cli_warn(c(
      "No palette entry for: {.val {missing}}.",
      "i" = "Filled in with grayscale; supply a custom {.arg palette} \\
             to override."
    ))
  }
  # Keep only used keys, preserving the canonical order where possible.
  ord <- intersect(names(pal), needed)
  pal[ord]
}
