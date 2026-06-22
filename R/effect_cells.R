#' Coerce an estimator result to a tidy cohort-by-time effect-cell tibble
#'
#' `as_nabs_effect_cells()` is an S3 generic that converts the native output of a
#' supported estimator into a *cohort x time* effect-cell schema -- the input
#' for [plot_effect_matrix()] heatmaps. It is the two-dimensional companion to
#' [as_nabs_event_study()]: where the event-study schema collapses everything
#' onto a single relative-time axis, this schema keeps the cohort (treatment
#' onset period) as a second dimension so heterogeneity *across* cohorts stays
#' visible.
#'
#' @section Status:
#' This is an **experimental** feature line, separate from the stable
#' event-study API. Only the `fect` family (`IFE` / `FE` / `MC`) and `DCDH`
#' (`DIDmultiplegtDYN`) are supported; `PanelMatch` is deliberately omitted for
#' now because a faithful cohort breakdown there needs the matched-set bootstrap
#' to be re-aggregated by cohort, which is out of scope for this pass.
#'
#' @section Cohort and event-time conventions:
#' * `cohort` is the treatment **onset calendar period** (the first period a
#'   unit is treated). For repeated on/off treatment this is the *first* onset,
#'   so interpret later periods through the estimator's own carryover handling.
#' * `event_time` is the relative period with `0` at onset, matching the
#'   `nabs_event_study_tbl` convention. For `fect` this is computed directly as
#'   `calendar_time - cohort`; for `DCDH` it is the native event-study axis
#'   shifted so onset sits at `0`.
#' * The `fect` surface only covers **treated** cells, so its matrix spans
#'   `event_time >= 0`. `DCDH` run with placebos additionally yields the
#'   pre-period (`event_time < 0`) cells.
#'
#' @param x A supported estimator object, or a data frame with at least
#'   `cohort`, `event_time`, and `estimate` columns.
#' @param method Optional override for the `method` column.
#' @param outcome Optional outcome name recorded in the `outcome` column.
#' @param conf.level Confidence level used to derive `conf.low` / `conf.high`
#'   from `std.error` when explicit bounds are not supplied. Default `0.95`.
#' @param ... Method-specific arguments (e.g. `axis`, `weighted` for the
#'   `fect` method).
#'
#' @return A tibble of class `"nabs_effect_cell_tbl"`, one row per
#'   `(cohort, event_time)` cell, with columns documented in [new_effect_cell_tbl()].
#'
#' @seealso [plot_effect_matrix()] to draw the heatmap, [nabs_effect_cells()]
#'   to fit and tidy in one step, [aggregate_effects()] to collapse cells back
#'   onto an event-study path.
#'
#' @examples
#' # The data.frame escape hatch needs no estimator packages.
#' raw <- expand.grid(cohort = 3:5, event_time = 0:3)
#' raw$estimate  <- with(raw, 0.1 * event_time + 0.05 * (cohort - 4))
#' raw$std.error <- 0.08
#' cells <- as_nabs_effect_cells(raw, method = "DCDH", outcome = "y")
#' cells
#' @export
as_nabs_effect_cells <- function(x, method = NULL, outcome = NA_character_,
                                 conf.level = 0.95, ...) {
  UseMethod("as_nabs_effect_cells")
}

#' @export
as_nabs_effect_cells.default <- function(x, method = NULL, outcome = NA_character_,
                                         conf.level = 0.95, ...) {
  cli::cli_abort(c(
    "No {.fun as_nabs_effect_cells} method for object of class {.cls {class(x)[1]}}.",
    "i" = "Supported: {.cls fect}, {.cls did_multiplegt_dyn} (run with {.arg by}), \\
           and {.cls data.frame}."
  ))
}

#' @export
as_nabs_effect_cells.nabs_effect_cell_tbl <- function(x, method = NULL,
                                                      outcome = NA_character_,
                                                      conf.level = 0.95, ...) {
  if (!is.null(method)) x$method <- method
  if (!is.na(outcome))  x$outcome <- outcome
  x
}

#' @rdname as_nabs_effect_cells
#' @export
as_nabs_effect_cells.data.frame <- function(x, method = NULL, outcome = NA_character_,
                                            conf.level = 0.95, ...) {
  if (!all(c("cohort", "event_time", "estimate") %in% names(x))) {
    cli::cli_abort(
      "A data frame passed to {.fun as_nabs_effect_cells} must have columns \\
       {.field cohort}, {.field event_time}, and {.field estimate}."
    )
  }
  new_effect_cell_tbl(
    cohort        = x$cohort,
    event_time    = x$event_time,
    estimate      = x$estimate,
    std.error     = if ("std.error"     %in% names(x)) x$std.error     else NA_real_,
    conf.low      = if ("conf.low"      %in% names(x)) x$conf.low      else NA_real_,
    conf.high     = if ("conf.high"     %in% names(x)) x$conf.high     else NA_real_,
    calendar_time = if ("calendar_time" %in% names(x)) x$calendar_time else NA_integer_,
    n             = if ("n"             %in% names(x)) x$n             else NA_integer_,
    method        = method  %||% (x$method[1]  %||% NA_character_),
    outcome       = outcome %|na|% (x$outcome[1] %||% NA_character_),
    se_method     = if ("se_method"     %in% names(x)) x$se_method[1]  else NA_character_,
    conf.level    = conf.level
  )
}

# Internal: build the canonical effect-cell tibble from raw vectors. All
# estimator methods (fect, DCDH) funnel through here so the schema is enforced
# in one place, mirroring new_event_study_tbl().
#
# Columns:
#   cohort         int  Treatment onset calendar period (g).
#   event_time     int  Relative period; 0 = onset.
#   calendar_time  int  Calendar period t = cohort + event_time (may be NA).
#   estimate       num  Point estimate for the cell.
#   std.error      num  Standard error (may be NA).
#   conf.low/high  num  CI bounds (derived from SE under normality if absent).
#   n              int  Number of treated cells aggregated (NA when unknown).
#   window         chr  "pre" if event_time < 0, else "post".
#   method         chr  Estimator label.
#   outcome        chr  Outcome variable name.
#   se_method      chr  "bootstrap" / "native" / "ci" / "none" -- how std.error was
#                       produced; lets a future PanelMatch path slot in without
#                       touching the plotting code.
new_effect_cell_tbl <- function(cohort, event_time, estimate,
                                std.error = NA_real_,
                                conf.low = NA_real_, conf.high = NA_real_,
                                calendar_time = NA_integer_,
                                n = NA_integer_,
                                method = NA_character_,
                                outcome = NA_character_,
                                se_method = NA_character_,
                                conf.level = 0.95) {
  k <- length(estimate)
  if (length(cohort) != k || length(event_time) != k) {
    cli::cli_abort("`cohort`, `event_time`, and `estimate` must have equal length.")
  }

  std.error     <- recycle_or_pad(std.error, k)
  conf.low      <- recycle_or_pad(conf.low,  k)
  conf.high     <- recycle_or_pad(conf.high, k)
  calendar_time <- recycle_or_pad(calendar_time, k)
  n             <- recycle_or_pad(n, k)
  need_ct <- is.na(calendar_time) & !is.na(cohort) & !is.na(event_time)
  if (any(need_ct)) calendar_time[need_ct] <- cohort[need_ct] + event_time[need_ct]

  # Derive CI from SE under normality where bounds are missing.
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  derive <- (is.na(conf.low) | is.na(conf.high)) & !is.na(std.error)
  if (any(derive)) {
    conf.low [derive] <- estimate[derive] - z * std.error[derive]
    conf.high[derive] <- estimate[derive] + z * std.error[derive]
  }

  # And the reverse: recover std.error from a symmetric CI when the estimator
  # reports bounds but not a point SE (e.g. DCDH plot data carries LB.CI/UB.CI
  # but no SE column). For symmetric-normal CIs this back-computes the exact SE,
  # so show_se can display DCDH uncertainty too.
  derive_se <- is.na(std.error) & !is.na(conf.low) & !is.na(conf.high)
  if (any(derive_se)) {
    std.error[derive_se] <- (conf.high[derive_se] - conf.low[derive_se]) / (2 * z)
  }

  out <- tibble::tibble(
    cohort        = as.integer(cohort),
    event_time    = as.integer(event_time),
    calendar_time = as.integer(calendar_time),
    estimate      = as.numeric(estimate),
    std.error     = as.numeric(std.error),
    conf.low      = as.numeric(conf.low),
    conf.high     = as.numeric(conf.high),
    n             = as.integer(n),
    window        = ifelse(event_time < 0, "pre", "post"),
    method        = as.character(method),
    outcome       = as.character(outcome),
    se_method     = as.character(se_method)
  )
  out <- out[order(out$method, out$cohort, out$event_time), , drop = FALSE]

  attr(out, "conf.level") <- conf.level
  class(out) <- c("nabs_effect_cell_tbl", class(out))
  out
}

# Row-bind several nabs_effect_cell_tbls and re-stamp the class.
bind_effect_cells <- function(lst) {
  out <- dplyr::bind_rows(lst)
  class(out) <- c("nabs_effect_cell_tbl",
                  setdiff(class(out), "nabs_effect_cell_tbl"))
  out
}

# Accept either bare nabs_effect_cell_tbl args or a single list of them, coercing
# anything else through the generic.
collect_effect_cells <- function(dots) {
  if (length(dots) == 1L && is.list(dots[[1]]) &&
      !inherits(dots[[1]], "nabs_effect_cell_tbl") &&
      !inherits(dots[[1]], "data.frame")) {
    dots <- dots[[1]]
  }
  lapply(dots, function(x) {
    if (inherits(x, "nabs_effect_cell_tbl")) x else as_nabs_effect_cells(x)
  })
}

#' @export
print.nabs_effect_cell_tbl <- function(x, ...) {
  cli::cli_text(
    "# {.cls nabs_effect_cell_tbl}: {nrow(x)} cell{?s}, \\
     {length(unique(x$cohort))} cohort{?s}, method{?s}: {.val {unique(x$method)}}"
  )
  NextMethod()
}
