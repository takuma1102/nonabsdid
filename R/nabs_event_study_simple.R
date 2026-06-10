#' One-line exploratory front door for non-absorbing event studies
#'
#' `nabs_event_study_simple()` is a deliberately opinionated convenience
#' wrapper for the *first 30 seconds* of an analysis. You give it your data
#' and the four column names that identify outcome / treatment / unit / time,
#' and it tries to give you a sensible event-study figure with as little
#' typing as possible.
#'
#' By default it runs **all three** heterogeneity-robust estimators (DCDH,
#' PanelMatch, IFE) plus a naive TWFE reference, and returns a single
#' overlay plot along with the tidy tibbles and raw fits. Use it to *see
#' the picture quickly*; for a careful, publication-ready result, switch
#' to [nabs_event_study()] and tune options per estimator.
#'
#' @param data A panel data frame.
#' @param outcome,treatment,unit,time Character column names. The treatment
#'   column should be a 0/1 indicator (it is allowed to switch back to 0,
#'   i.e. non-absorbing).
#' @param methods Character vector of estimators to run. Any subset of
#'   `c("DCDH", "PanelMatch", "IFE", "FE", "MC")`. Default `c("DCDH",
#'   "PanelMatch", "IFE")` -- the three classic heterogeneity-robust
#'   estimators.
#' @param include_twfe Logical; if `TRUE` (default), also fit a naive TWFE
#'   reference series via [naive_twfe()] and overlay it in a neutral color.
#' @param lags,leads Integer pre- and post-period lengths. If `NULL`
#'   (default), reasonable values are auto-chosen from the panel: `leads`
#'   is set to roughly one third of the longest post-treatment span
#'   (capped at 8), and `lags` to roughly one quarter of the longest
#'   pre-treatment span (capped at 6). Override either explicitly to be
#'   sure of the window.
#' @param controls Optional character vector of covariate names; passed
#'   straight through to each estimator.
#' @param verbose Logical; if `TRUE` (default), print a brief progress
#'   message before each estimator runs.
#' @param ... Forwarded to [nabs_event_plot()] (e.g. `xlim`, `ylim`,
#'   `palette`, `ylab`, `x_break_by`).
#'
#' @return A list of class `"nabs_event_study_simple"` with elements:
#'   \describe{
#'     \item{`plot`}{A `ggplot` object; the overlay figure.}
#'     \item{`tidy`}{A single combined `nabs_event_study_tbl` with all methods.}
#'     \item{`per_method`}{Named list of per-method tidy tibbles.}
#'     \item{`fits`}{Named list of native estimator objects.}
#'     \item{`twfe`}{The TWFE reference (or `NULL`).}
#'     \item{`call`}{The matched call.}
#'   }
#'
#' @details
#' If a particular estimator's package is not installed, that estimator is
#' silently skipped with a message and the rest are still attempted. This
#' is intentional: the goal of `_simple()` is to give you *something* to
#' look at even if your environment isn't fully provisioned.
#'
#' Errors from a single estimator (for instance, PanelMatch failing because
#' there are too few clean controls in the lag window) are caught, reported
#' as a warning, and the remaining estimators continue.
#'
#' @examples
#' \donttest{
#'   set.seed(1)
#'   panel <- expand.grid(id = 1:40, t = 1:10)
#'   panel$d <- rbinom(nrow(panel), 1, 0.3)
#'   panel$y <- 0.4 * panel$d + rnorm(nrow(panel))
#'
#'   # Restrict to a single estimator for a fast, self-contained example.
#'   res <- nabs_event_study_simple(
#'     panel,
#'     outcome   = "y",
#'     treatment = "d",
#'     unit      = "id",
#'     time      = "t",
#'     methods   = "DCDH",
#'     lags = 2, leads = 3
#'   )
#'   res$plot
#'   res$tidy
#' }
#' @export
nabs_event_study_simple <- function(data, outcome, treatment, unit, time,
                                    methods = c("DCDH", "PanelMatch", "IFE"),
                                    include_twfe = TRUE,
                                    lags = NULL, leads = NULL,
                                    controls = NULL,
                                    verbose = TRUE,
                                    ...) {
  call <- match.call()

  # Basic input check -- catches common mistakes early without requiring
  # any of the suggested packages to be installed.
  stopifnot(
    is.data.frame(data),
    is.character(outcome),  length(outcome)   == 1L,
    is.character(treatment),length(treatment) == 1L,
    is.character(unit),     length(unit)      == 1L,
    is.character(time),     length(time)      == 1L
  )
  for (col in c(outcome, treatment, unit, time)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in {.arg data}.")
    }
  }
  methods <- match.arg(methods,
                       choices = c("DCDH", "PanelMatch", "IFE", "FE", "MC"),
                       several.ok = TRUE)

  # Auto-pick window lengths if the user didn't supply them. We look at the
  # actual treated history per unit to size the window sensibly.
  win <- auto_window(data, treatment = treatment, unit = unit, time = time,
                     user_lags = lags, user_leads = leads)
  lags  <- win$lags
  leads <- win$leads
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Window: {.val {lags}} pre-periods, \\
                         {.val {leads}} post-periods.")
  }

  # Run each requested estimator, gracefully skipping any whose package
  # isn't installed and any that error out at runtime.
  fits       <- list()
  per_method <- list()

  # Run one estimator at a given window, catching runtime errors so a single
  # failure never takes down the whole call.
  run_method <- function(m, lags, leads) {
    tryCatch(
      nabs_event_study(data,
                       outcome = outcome, treatment = treatment,
                       unit = unit, time = time,
                       method = m,
                       lags = lags, leads = leads,
                       controls = controls),
      error = function(e) {
        cli::cli_alert_warning("{.val {m}} failed: {conditionMessage(e)}")
        NULL
      }
    )
  }

  for (m in methods) {
    pkg <- estimator_pkg(m)
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_alert_warning(
        "Skipping {.val {m}}: package {.pkg {pkg}} is not installed."
      )
      next
    }
    if (isTRUE(verbose)) cli::cli_alert("Running {.val {m}}...")

    res <- run_method(m, lags, leads)

    # Some estimators -- notably DCDH under non-absorbing treatment -- reject a
    # window that is wider than the switch horizons in the data actually
    # support (often surfaced as a misleading "positive integer required"
    # error). Retry once with a halved window before giving up.
    if (is.null(res)) {
      small_lags  <- max(2L, lags  %/% 2L)
      small_leads <- max(2L, leads %/% 2L)
      if (small_lags < lags || small_leads < leads) {
        if (isTRUE(verbose)) {
          cli::cli_alert_info(
            "Retrying {.val {m}} with a smaller window \\
             (lags = {small_lags}, leads = {small_leads})."
          )
        }
        res <- run_method(m, small_lags, small_leads)
      }
    }

    if (!is.null(res)) {
      fits[[m]]       <- res$fit
      per_method[[m]] <- res$tidy
    }
  }

  if (length(per_method) == 0L) {
    cli::cli_abort(c(
      "No estimator succeeded.",
      "i" = "Install at least one of {.pkg DIDmultiplegtDYN}, \\
             {.pkg PanelMatch}, or {.pkg fect}, and check that your \\
             data has both treated and control units in the panel."
    ))
  }

  # Optional naive TWFE reference.
  twfe <- NULL
  if (isTRUE(include_twfe) && requireNamespace("fixest", quietly = TRUE)) {
    if (isTRUE(verbose)) cli::cli_alert("Fitting naive TWFE reference...")
    twfe <- tryCatch(
      naive_twfe(data, outcome = outcome, treatment = treatment,
                 unit = unit, time = time,
                 lags = lags, leads = leads, controls = controls),
      error = function(e) {
        cli::cli_alert_warning(
          "TWFE reference failed: {conditionMessage(e)}"
        )
        NULL
      }
    )
  } else if (isTRUE(include_twfe)) {
    cli::cli_alert_warning(
      "TWFE reference skipped: package {.pkg fixest} is not installed."
    )
  }

  combined <- bind_event_studies(per_method)

  plot <- nabs_event_plot(per_method,
                          reference = twfe,
                          ...)

  structure(
    list(
      plot       = plot,
      tidy       = combined,
      per_method = per_method,
      fits       = fits,
      twfe       = twfe,
      call       = call
    ),
    class = "nabs_event_study_simple"
  )
}

#' @export
print.nabs_event_study_simple <- function(x, ...) {
  cli::cli_h1("nabs_event_study_simple")
  cli::cli_text("methods run: {.val {names(x$per_method)}}")
  cli::cli_text("twfe reference: {.val {!is.null(x$twfe)}}")
  cli::cli_text("rows in combined tidy: {nrow(x$tidy)}")
  cli::cli_text("Use {.code $plot} to view, {.code $tidy} to inspect, \\
                  {.code $fits} for native objects.")
  invisible(x)
}

# ----- internal helpers ------------------------------------------------------

estimator_pkg <- function(method) {
  switch(method,
         DCDH       = "DIDmultiplegtDYN",
         PanelMatch = "PanelMatch",
         IFE        = "fect",
         FE         = "fect",
         MC         = "fect")
}

# Auto-choose window sizes from the actual treated histories in the data.
auto_window <- function(data, treatment, unit, time,
                        user_lags = NULL, user_leads = NULL) {
  if (!is.null(user_lags) && !is.null(user_leads)) {
    return(list(lags = as.integer(user_lags), leads = as.integer(user_leads)))
  }

  d <- data.frame(
    unit = data[[unit]],
    t    = as.numeric(data[[time]]),
    z    = as.integer(data[[treatment]] != 0L)
  )
  treated_rows <- d[d$z == 1L & !is.na(d$z), , drop = FALSE]

  if (nrow(treated_rows) == 0L) {
    cli::cli_warn(c(
      "No treated observations found in {.arg data}.",
      "i" = "Falling back to lags = 6, leads = 8."
    ))
    return(list(lags = 6L, leads = 8L))
  }

  # Pre-treatment span: longest stretch from earliest observation to first
  # switch-on, across treated units.
  first_treat <- stats::aggregate(t ~ unit, data = treated_rows, FUN = min)
  names(first_treat) <- c("unit", "first_t")
  unit_min <- stats::aggregate(t ~ unit, data = d, FUN = min)
  names(unit_min) <- c("unit", "min_t")
  unit_max <- stats::aggregate(t ~ unit, data = d, FUN = max)
  names(unit_max) <- c("unit", "max_t")
  m <- merge(merge(first_treat, unit_min, by = "unit"), unit_max, by = "unit")

  pre_span  <- max(m$first_t - m$min_t, na.rm = TRUE)
  post_span <- max(m$max_t   - m$first_t, na.rm = TRUE)

  default_lags  <- if (is.null(user_lags))  {
    max(2L, min(6L, as.integer(round(pre_span  / 4))))
  } else as.integer(user_lags)

  default_leads <- if (is.null(user_leads)) {
    max(2L, min(8L, as.integer(round(post_span / 3))))
  } else as.integer(user_leads)

  list(lags = default_lags, leads = default_leads)
}
