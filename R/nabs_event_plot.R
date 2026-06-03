#' Plot one or more event-study tibbles on a single panel
#'
#' Overlays event-study estimates from any combination of supported estimators
#' on a single ggplot2 panel. Each method gets its own color (with separate
#' shades for pre- and post-treatment periods, mirroring common conventions
#' in DCDH-style plots). An optional `reference` series -- typically a naive
#' TWFE fit from [naive_twfe()] -- is drawn in a neutral color (default
#' black) so the reader can see what the heterogeneity-robust estimators
#' are correcting against.
#'
#' @param ... One or more `nabs_event_study_tbl` objects. Bare arguments and a
#'   single list are both accepted.
#' @param reference Optional `nabs_event_study_tbl` to draw as a neutral-color
#'   reference layer (typically a naive TWFE estimate). Drawn under the
#'   main series.
#' @param reference_color Color for the reference series. Default `"grey20"`.
#' @param palette Either `"default"` (the package's built-in palette,
#'   patterned after the DCDH/PanelMatch/IFE conventions in the codebase
#'   this package was extracted from), `"colorblind"` (Okabe-Ito), or a
#'   named character vector of colors keyed by `"<method>_<window>"`,
#'   e.g. `c("DCDH_pre" = "#DE2D26", "DCDH_post" = "#3182BD", ...)`.
#' @param xlim,ylim Numeric length-2 vectors for axis limits. `NULL` lets
#'   ggplot2 choose.
#' @param dodge Width of the position-dodge applied to points and error
#'   bars. Default `0.5`.
#' @param point_size,errorbar_width Aesthetic controls for the geom layers.
#' @param show_pre_post_legend Logical. If `TRUE`, the legend keys are
#'   labeled `"<method>; pre"` / `"<method>; post"`. If `FALSE`, only one
#'   key per method is shown. Default `TRUE`.
#' @param xlab,ylab Axis labels.
#' @param base_size Base font size passed to `theme_minimal()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#'   p <- nabs_event_plot(dcdh_tidy, panelmatch_tidy, ife_tidy,
#'                   reference = naive_twfe_tidy,
#'                   xlim = c(-6, 6),
#'                   ylim = c(-2, 2),
#'                   ylab = "Effect on logged dollars")
#'   p
#' }
#' @export
nabs_event_plot <- function(...,
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

  series <- collect_event_studies(list(...))
  if (length(series) == 0L) {
    cli::cli_abort("Pass at least one {.cls nabs_event_study_tbl} to {.fun nabs_event_plot}.")
  }

  df <- bind_event_studies(series)
  df$key <- paste(df$method, df$window, sep = "_")

  pal <- resolve_palette(palette, df)

  # Build labels for legend (e.g. "DCDH; pre" or just "DCDH").
  if (isTRUE(show_pre_post_legend)) {
    label_for <- function(k) {
      parts <- strsplit(k, "_", fixed = TRUE)[[1]]
      paste0(parts[1], "; ", parts[2])
    }
  } else {
    label_for <- function(k) strsplit(k, "_", fixed = TRUE)[[1]][1]
  }
  lbls <- vapply(names(pal), label_for, character(1))

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$time, y = .data$estimate,
                 color = .data$key, shape = .data$window)
  )

  # Reference series (drawn first so it sits underneath).
  if (!is.null(reference)) {
    if (!inherits(reference, "nabs_event_study_tbl")) {
      reference <- as_nabs_event_study(reference)
    }
    p <- p +
      ggplot2::geom_errorbar(
        data = reference,
        ggplot2::aes(x = .data$time,
                     ymin = .data$conf.low, ymax = .data$conf.high),
        inherit.aes = FALSE,
        color = reference_color,
        width = errorbar_width,
        alpha = 0.55
      ) +
      ggplot2::geom_line(
        data = reference,
        ggplot2::aes(x = .data$time, y = .data$estimate, group = 1L,
                     linetype = "TWFE (naive)"),
        inherit.aes = FALSE,
        color = reference_color,
        alpha = 0.7
      ) +
      ggplot2::geom_point(
        data = reference,
        ggplot2::aes(x = .data$time, y = .data$estimate),
        inherit.aes = FALSE,
        color = reference_color,
        size = point_size * 0.8,
        alpha = 0.8
      )
  }

  p <- p +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high),
      width = errorbar_width,
      linewidth = 0.8,
      position = ggplot2::position_dodge(width = dodge)
    ) +
    ggplot2::geom_point(
      size = point_size,
      position = ggplot2::position_dodge(width = dodge)
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = pal,
      breaks = names(pal),
      labels = lbls
    ) +
    ggplot2::scale_shape_manual(
      name   = NULL,
      values = c("pre" = 16L, "post" = 17L)
    ) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c("TWFE (naive)" = "dashed")
    ) +
    ggplot2::guides(shape = "none") +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.key.width = ggplot2::unit(1.5, "lines")
    )

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

resolve_palette <- function(palette, df) {
  needed <- unique(df$key)

  pal <- if (is.character(palette) && length(palette) == 1L) {
    switch(palette,
           default     = default_palette,
           colorblind  = colorblind_palette,
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
