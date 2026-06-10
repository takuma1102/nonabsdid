#' Coerce an estimator result to a tidy event-study tibble
#'
#' `as_nabs_event_study()` is an S3 generic that converts the native output object
#' of a supported estimator into the unified *nabs_event_study_tbl* schema used
#' by [nabs_event_plot()]. Methods exist for objects of class `"did_multiplegt_dyn"`
#' (from `DIDmultiplegtDYN`), `"PanelEstimate"` (from `PanelMatch`),
#' `"fect"` (from `fect`), and `"fixest"` (from `fixest`, used for the
#' naive TWFE reference series).
#'
#' A `data.frame` method is also provided as an escape hatch: it accepts any
#' frame that already contains `time` and `estimate` columns and fills in
#' the rest of the schema if missing.
#'
#' @param x A supported estimator object.
#' @param method Optional override for the `method` column. If `NULL`, the
#'   default for that estimator is used.
#' @param outcome Optional outcome name to record in the `outcome` column.
#' @param conf.level Confidence level for `conf.low` / `conf.high`. Default
#'   `0.95`. When the underlying object stores its own CI bounds (e.g. `fect`),
#'   those are used as-is and `conf.level` is recorded as metadata only.
#' @param ... Method-specific arguments. See the individual method files
#'   for details (e.g. `pre_obj` for the `PanelEstimate` method).
#'
#' @return A tibble of class `"nabs_event_study_tbl"` with one row per relative
#'   period and the columns documented in the package overview.
#'
#' @examples
#' # The data.frame escape hatch needs no estimator packages: pass a frame
#' # that already has `time` and `estimate`; the remaining schema columns
#' # (including CIs derived from `std.error`) are filled in automatically.
#' raw <- data.frame(
#'   time      = -3:4,
#'   estimate  = c(-0.05, 0.01, 0.00, 0.02, 0.30, 0.42, 0.38, 0.50),
#'   std.error = 0.12
#' )
#' tidy_fit <- as_nabs_event_study(raw, method = "DCDH", outcome = "y")
#'
#' # With the DCDH estimator installed, coerce its native object directly.
#' if (requireNamespace("DIDmultiplegtDYN", quietly = TRUE) &&
#'     requireNamespace("polars", quietly = TRUE)) {
#'   set.seed(1)
#'   panel <- expand.grid(id = 1:60, t = 1:10)
#'   panel$d <- with(panel, as.integer(
#'     (id %% 4 == 1 & t %in% 4:7) |
#'     (id %% 4 == 2 & t %in% 5:8) |
#'     (id %% 4 == 3 & t %in% 6:9)
#'   ))
#'   panel$y <- 0.2 * panel$t + 0.5 * panel$d + rnorm(nrow(panel))
#'
#'   fit <- DIDmultiplegtDYN::did_multiplegt_dyn(
#'     df = panel,
#'     outcome = "y",
#'     group = "id",
#'     time = "t",
#'     treatment = "d",
#'     effects = 3,
#'     placebo = 2
#'   )
#'   as_nabs_event_study(fit, outcome = "y")
#' }
#' @export
as_nabs_event_study <- function(x, method = NULL, outcome = NA_character_,
                           conf.level = 0.95, ...) {
  UseMethod("as_nabs_event_study")
}

#' @export
as_nabs_event_study.default <- function(x, method = NULL, outcome = NA_character_,
                                   conf.level = 0.95, ...) {
  cli::cli_abort(c(
    "No {.fun as_nabs_event_study} method for object of class {.cls {class(x)[1]}}.",
    "i" = "Supported classes: {.cls did_multiplegt_dyn}, {.cls PanelEstimate}, \\
           {.cls fect}, {.cls fixest}, {.cls data.frame}."
  ))
}

#' @export
as_nabs_event_study.nabs_event_study_tbl <- function(x, method = NULL, outcome = NA_character_,
                                           conf.level = 0.95, ...) {
  # Idempotent: passing an already-tidied object back through the generic
  # just optionally relabels method/outcome.
  if (!is.null(method)) x$method <- method
  if (!is.na(outcome)) x$outcome <- outcome
  x
}

# Internal: build the canonical tibble from raw vectors. All callers
# (DCDH, PanelMatch, fect, fixest) funnel through here, so the schema
# is enforced in one place.
new_event_study_tbl <- function(time, estimate, std.error = NA_real_,
                                conf.low = NA_real_, conf.high = NA_real_,
                                method = NA_character_,
                                outcome = NA_character_,
                                conf.level = 0.95) {
  n <- length(time)
  if (length(estimate) != n) {
    cli::cli_abort("`time` and `estimate` must have the same length.")
  }

  std.error <- recycle_or_pad(std.error, n)
  conf.low  <- recycle_or_pad(conf.low,  n)
  conf.high <- recycle_or_pad(conf.high, n)

  # If we have SE but no CI, derive CI from SE under normality.
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  needs_ci <- is.na(conf.low) | is.na(conf.high)
  has_se   <- !is.na(std.error)
  derive   <- needs_ci & has_se
  if (any(derive)) {
    conf.low [derive] <- estimate[derive] - z * std.error[derive]
    conf.high[derive] <- estimate[derive] + z * std.error[derive]
  }

  out <- tibble::tibble(
    time      = as.integer(time),
    estimate  = as.numeric(estimate),
    std.error = as.numeric(std.error),
    conf.low  = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    window    = ifelse(time < 0, "pre", "post"),
    method    = as.character(method),
    outcome   = as.character(outcome)
  )
  out <- out[order(out$time), , drop = FALSE]

  attr(out, "conf.level") <- conf.level
  class(out) <- c("nabs_event_study_tbl", class(out))
  out
}

recycle_or_pad <- function(x, n) {
  if (is.null(x)) return(rep(NA_real_, n))
  if (length(x) == 1L) return(rep(x, n))
  if (length(x) == n)  return(x)
  cli::cli_abort("Expected a vector of length 1 or {n}, got length {length(x)}.")
}

#' @export
as_nabs_event_study.data.frame <- function(x, method = NULL, outcome = NA_character_,
                                      conf.level = 0.95, ...) {
  if (!all(c("time", "estimate") %in% names(x))) {
    cli::cli_abort("A data frame passed to {.fun as_nabs_event_study} must have \\
                    columns {.field time} and {.field estimate}.")
  }
  new_event_study_tbl(
    time      = x$time,
    estimate  = x$estimate,
    std.error = if ("std.error" %in% names(x)) x$std.error else NA_real_,
    conf.low  = if ("conf.low"  %in% names(x)) x$conf.low  else NA_real_,
    conf.high = if ("conf.high" %in% names(x)) x$conf.high else NA_real_,
    method    = method  %||% (x$method[1]  %||% NA_character_),
    outcome   = outcome %|na|% (x$outcome[1] %||% NA_character_),
    conf.level = conf.level
  )
}

# Small infix helpers (NULL- and NA-safe defaults).
`%||%` <- function(a, b) if (is.null(a)) b else a
`%|na|%` <- function(a, b) if (is.na(a))  b else a

#' Combine multiple nabs_event_study_tbl objects
#'
#' Internal: row-bind several `nabs_event_study_tbl`s and re-stamp the class.
#' Used by [nabs_event_plot()] when multiple methods are supplied.
#' @noRd
bind_event_studies <- function(lst) {
  out <- dplyr::bind_rows(lst)
  class(out) <- c("nabs_event_study_tbl", class(out)[!class(out) %in% "nabs_event_study_tbl"])
  out
}

#' @export
print.nabs_event_study_tbl <- function(x, ...) {
  cli::cli_text("# {.cls nabs_event_study_tbl}: {nrow(x)} row{?s}, \\
                  method{?s}: {.val {unique(x$method)}}")
  NextMethod()
}
