#' Fit an estimator and return cohort-by-time effect cells
#'
#' `nabs_effect_cells()` is the cohort-matrix counterpart to [nabs_event_study()]:
#' it fits one supported estimator and returns the result already tidied into the
#' `nabs_effect_cell_tbl` schema, ready for [plot_effect_matrix()]. It wires up
#' the per-estimator machinery that a cohort breakdown needs -- a unit-level
#' onset cohort for `DCDH`, and `keep.sims = TRUE` for `fect` bootstrap cell SEs
#' -- so you do not have to.
#'
#' @section Status:
#' Experimental, and intentionally limited to `DCDH` and the `fect` family
#' (`IFE` / `FE` / `MC`). `PanelMatch` is not supported here.
#'
#' @inheritParams nabs_event_study
#' @param method One of `"DCDH"`, `"IFE"`, `"FE"`, `"MC"`.
#' @param axis Which axis [plot_effect_matrix()] should default to: `"event"`
#'   (relative time, default) or `"calendar"`. Both columns are populated
#'   regardless.
#' @param dcdh_strategy How to obtain cohort-specific DCDH estimates:
#'   * `"loop"` (default) re-estimates the event study separately for each onset
#'     cohort against the never-treated units (`only_never_switchers = TRUE`).
#'     Robust -- it reuses the stable event-study tidier -- and the control group
#'     (never-treated) is constant and easy to interpret.
#'   * `"by"` runs a single `did_multiplegt_dyn(..., by = cohort)` call and parses
#'     its per-level sublists. One estimation, native DCDH controls, but it
#'     depends on the package's nested-output layout.
#' @param nboots Bootstrap replicates for the `fect` family (default 200).
#'   Bootstrap draws are retained (`keep.sims = TRUE`) so cell SEs can be formed.
#' @param max_cohorts Safety cap on the number of distinct onset cohorts before
#'   `nabs_effect_cells()` refuses to run (default 30); raise it deliberately.
#'
#' @return A list of class `"nabs_effect_cells_result"` with elements `cells`
#'   (an `nabs_effect_cell_tbl`), `fit` (native object, or a list of them for the
#'   DCDH loop), and `call`.
#'
#' @seealso [plot_effect_matrix()], [as_nabs_effect_cells()].
#'
#' @examples
#' if (requireNamespace("fect", quietly = TRUE)) {
#'   set.seed(1)
#'   panel <- expand.grid(id = 1:80, t = 1:12)
#'   onset <- c(`1` = 4, `2` = 6, `3` = 8)[as.character(panel$id %% 4)]
#'   panel$d <- as.integer(!is.na(onset) & panel$t >= onset)
#'   panel$y <- 0.2 * panel$t + 0.4 * panel$d + rnorm(nrow(panel))
#'   res <- nabs_effect_cells(panel, outcome = "y", treatment = "d",
#'                            unit = "id", time = "t", method = "FE",
#'                            nboots = 50)
#'   res$cells
#' }
#' @export
nabs_effect_cells <- function(data, outcome, treatment, unit, time,
                              method = c("DCDH", "IFE", "FE", "MC"),
                              lags = 6L, leads = 8L,
                              controls = NULL, cluster = unit,
                              conf.level = 0.95,
                              axis = c("event", "calendar"),
                              dcdh_strategy = c("loop", "by"),
                              nboots = 200L,
                              max_cohorts = 30L, ...) {
  method <- match.arg(method)
  axis   <- match.arg(axis)
  dcdh_strategy <- match.arg(dcdh_strategy)
  call <- match.call()

  data <- resolve_panel_data(data)
  pf <- preflight_panel(
    data, outcome = outcome, treatment = treatment,
    unit = unit, time = time, controls = controls, cluster = cluster
  )
  data     <- pf$data
  unit     <- pf$unit
  cluster  <- pf$cluster
  controls <- pf$controls

  if (method == "DCDH") {
    out <- run_dcdh_cells(data, outcome, treatment, unit, time,
                          lags, leads, controls, cluster,
                          conf.level = conf.level,
                          strategy = dcdh_strategy,
                          max_cohorts = max_cohorts, ...)
    cells <- out$cells
    fit   <- out$fit
  } else {
    fect_method <- tolower(method)   # ife / fe / mc
    fit <- run_fect(data, outcome, treatment, unit, time, controls,
                    fect_method = fect_method,
                    nboots = as.integer(nboots), se = TRUE,
                    keep.sims = TRUE, ...)
    cells <- as_nabs_effect_cells(fit, method = method, outcome = outcome,
                                  conf.level = conf.level, axis = axis)
  }

  structure(list(cells = cells, fit = fit, call = call),
            class = "nabs_effect_cells_result")
}

# DCDH cohort cells via either the per-cohort loop or a single by-run.
run_dcdh_cells <- function(data, outcome, treatment, unit, time,
                           lags, leads, controls, cluster,
                           conf.level, strategy, max_cohorts, ...) {
  co <- onset_cohorts(data, treatment, unit, time)
  cohorts <- sort(unique(stats::na.omit(co$cohort)))
  if (!length(cohorts)) {
    cli::cli_abort("No treated units with a detectable onset period were found.")
  }
  if (length(cohorts) > max_cohorts) {
    cli::cli_abort(c(
      "Found {length(cohorts)} onset cohorts (cap: {max_cohorts}).",
      "i" = "Raise {.arg max_cohorts} if this is intended; a heatmap with that \\
             many rows is usually a sign the cohort variable is too granular."
    ))
  }

  if (identical(strategy, "by")) {
    # One run with a unit-level cohort column passed to DCDH's `by`.
    by_col <- make_unique_name("nabs_cohort", names(data))
    data[[by_col]] <- co$cohort_by_row
    fit <- run_dcdh(data, outcome, treatment, unit, time,
                    lags, leads, controls, cluster, by = by_col, ...)
    cells <- as_nabs_effect_cells(fit, method = "DCDH", outcome = outcome,
                                  conf.level = conf.level)
    return(list(cells = cells, fit = fit))
  }

  # strategy == "loop": cohort g switchers vs never-treated, one run each.
  never <- co$unit_id[is.na(co$cohort)]
  if (!length(never)) {
    cli::cli_abort(c(
      "The {.val loop} DCDH strategy needs never-treated units as controls, \\
       but none were found.",
      "i" = "Use {.code dcdh_strategy = \"by\"} for not-yet-treated controls."
    ))
  }

  fits  <- vector("list", length(cohorts))
  parts <- vector("list", length(cohorts))
  for (i in seq_along(cohorts)) {
    g <- cohorts[i]
    keep_units <- c(co$unit_id[!is.na(co$cohort) & co$cohort == g], never)
    sub <- data[data[[unit]] %in% keep_units, , drop = FALSE]
    fit <- tryCatch(
      run_dcdh(sub, outcome, treatment, unit, time,
               lags, leads, controls, cluster,
               only_never_switchers = TRUE, ...),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      cli::cli_warn("Cohort {g}: DCDH failed ({conditionMessage(fit)}); skipped.")
      next
    }
    es <- as_nabs_event_study(fit, method = "DCDH", outcome = outcome,
                              conf.level = conf.level)
    fits[[i]]  <- fit
    parts[[i]] <- new_effect_cell_tbl(
      cohort        = rep(as.integer(g), nrow(es)),
      event_time    = es$time,
      estimate      = es$estimate,
      std.error     = es$std.error,
      conf.low      = es$conf.low,
      conf.high     = es$conf.high,
      calendar_time = as.integer(g) + es$time,
      method        = "DCDH",
      outcome       = outcome,
      se_method     = ifelse(!is.na(es$std.error), "native",
                             ifelse(!is.na(es$conf.low) & !is.na(es$conf.high),
                                    "ci", "none")),
      conf.level    = conf.level
    )
  }
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) cli::cli_abort("Every cohort-specific DCDH run failed.")

  cells <- bind_effect_cells(parts)
  attr(cells, "conf.level") <- conf.level
  list(cells = cells, fit = Filter(Negate(is.null), fits))
}

# Per-unit onset cohort = first period the unit is treated. Returns the unit
# ids, their cohort (NA = never treated), and a row-aligned cohort vector for
# the `by` strategy.
onset_cohorts <- function(data, treatment, unit, time) {
  u <- data[[unit]]
  t <- data[[time]]
  d <- as.integer(data[[treatment]] == 1 | data[[treatment]] == TRUE)
  ord <- order(u, t)
  treated_time <- ifelse(d[ord] == 1L, t[ord], NA)
  first_on <- tapply(treated_time, u[ord], function(z) {
    z <- z[!is.na(z)]
    if (length(z)) min(z) else NA_real_
  })
  unit_id <- names(first_on)
  cohort  <- as.integer(round(as.numeric(first_on)))
  # Coerce unit_id back to the column's type for membership tests.
  if (is.numeric(u)) unit_id <- as.numeric(unit_id)
  by_row <- cohort[match(as.character(u), as.character(names(first_on)))]
  list(unit_id = unit_id, cohort = cohort, cohort_by_row = by_row)
}

#' @export
print.nabs_effect_cells_result <- function(x, ...) {
  cli::cli_h1("nabs_effect_cells_result")
  cli::cli_text("method: {.val {unique(x$cells$method)}}")
  cli::cli_text("cohorts: {length(unique(x$cells$cohort))}, \\
                 cells: {nrow(x$cells)}")
  cli::cli_text("se: {.val {unique(x$cells$se_method)}}")
  invisible(x)
}
