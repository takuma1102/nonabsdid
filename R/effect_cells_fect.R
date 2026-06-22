#' @rdname as_nabs_effect_cells
#'
#' @details
#' ## fect method
#'
#' Uses `fect::imputed_outcomes()` (fect >= 2.4.0), the documented long-form
#' accessor that returns one row per treated cell with columns `id`, `time`,
#' `event.time`, `cohort`, `eff`, and `W.agg`. The cell estimate for each
#' `(cohort, event_time)` group is the `W.agg`-weighted mean of the cell-level
#' effects `eff` (set `weighted = FALSE` for an unweighted mean).
#'
#' Standard errors come from the bootstrap surface: when the fit was produced
#' with `se = TRUE` and `keep.sims = TRUE`, `imputed_outcomes(replicates = TRUE)`
#' is re-aggregated within each replicate, and the cell SE is the standard
#' deviation across replicates (with percentile CIs). Without stored sims the
#' SE / CI columns are `NA` and `se_method` is `"none"`.
#'
#' @param axis For the matrix axes only: `"event"` keeps `event_time`
#'   (default), `"calendar"` additionally fills `calendar_time`. Both columns
#'   are always present; this only affects which one [plot_effect_matrix()]
#'   defaults to.
#' @param weighted Logical; weight the within-cell mean of `eff` by `W.agg`.
#'   Default `TRUE`.
#'
#' @export
as_nabs_effect_cells.fect <- function(x, method = NULL, outcome = NA_character_,
                                      conf.level = 0.95,
                                      axis = c("event", "calendar"),
                                      weighted = TRUE, ...) {
  rlang::check_installed("fect", reason = "to extract cohort cells from a fect fit.")
  axis <- match.arg(axis)

  io <- get_fect_imputed_outcomes()
  po <- io(x)
  req <- c("cohort", "time", "eff")
  if (!is.data.frame(po) || !all(req %in% names(po))) {
    cli::cli_abort(c(
      "Unexpected {.fun fect::imputed_outcomes} output; cannot build cohort cells.",
      "i" = "Expected at least columns {.field {req}}."
    ))
  }

  # event_time is computed directly so onset (time == cohort) is 0, matching the
  # package convention. fect's own `event.time` equals this + 1.
  po$.et <- as.integer(po$time) - as.integer(po$cohort)
  po$.w  <- if (isTRUE(weighted) && "W.agg" %in% names(po)) po$W.agg else 1

  pt <- agg_fect_cells(po$eff, po$.w, po$cohort, po$.et)

  # Bootstrap SE: re-aggregate the replicate expansion if available.
  se <- rep(NA_real_, nrow(pt))
  lo <- rep(NA_real_, nrow(pt))
  hi <- rep(NA_real_, nrow(pt))
  se_method <- "none"
  rep_po <- try(io(x, replicates = TRUE), silent = TRUE)
  if (!inherits(rep_po, "try-error") && is.data.frame(rep_po) &&
      "replicate" %in% names(rep_po) &&
      all(c("cohort", "time", "eff") %in% names(rep_po))) {
    rep_po$.et <- as.integer(rep_po$time) - as.integer(rep_po$cohort)
    rep_po$.w  <- if (isTRUE(weighted) && "W.agg" %in% names(rep_po)) rep_po$W.agg else 1
    boot <- agg_fect_replicates(rep_po, conf.level)
    # Align to the point-estimate cell order.
    key  <- paste(pt$cohort, pt$.et)
    bkey <- paste(boot$cohort, boot$.et)
    m    <- match(key, bkey)
    se   <- boot$se[m]
    lo   <- boot$lo[m]
    hi   <- boot$hi[m]
    se_method <- "bootstrap"
  } else {
    cli::cli_inform(c(
      "!" = "No stored bootstrap sims on this {.cls fect} fit; cell SEs are {.val NA}.",
      "i" = "Refit with {.code se = TRUE, keep.sims = TRUE} (or use \\
             {.fun nabs_effect_cells}) for cell-level uncertainty."
    ))
  }

  new_effect_cell_tbl(
    cohort        = pt$cohort,
    event_time    = pt$.et,
    estimate      = pt$est,
    std.error     = se,
    conf.low      = lo,
    conf.high     = hi,
    calendar_time = pt$cohort + pt$.et,
    n             = pt$n,
    method        = method %||% fect_method_label(x[["method"]]),
    outcome       = outcome,
    se_method     = se_method,
    conf.level    = conf.level
  )
}

# Resolve fect::imputed_outcomes(), erroring helpfully on older fect.
get_fect_imputed_outcomes <- function() {
  io <- tryCatch(getExportedValue("fect", "imputed_outcomes"),
                 error = function(e) NULL)
  if (is.null(io) || !is.function(io)) {
    cli::cli_abort(c(
      "This version of {.pkg fect} does not export {.fun imputed_outcomes}.",
      "i" = "Cohort cells need fect >= 2.4.0; please upgrade {.pkg fect}."
    ))
  }
  io
}

# Weighted within-cell mean of eff over (cohort, event_time). Base R only, to
# avoid a hard dplyr grouping dependency on the hot path.
agg_fect_cells <- function(eff, w, cohort, et) {
  g    <- interaction(cohort, et, drop = TRUE, lex.order = TRUE)
  num  <- tapply(eff * w, g, sum, na.rm = TRUE)
  den  <- tapply(w,        g, sum, na.rm = TRUE)
  cnt  <- tapply(eff,      g, function(z) sum(!is.na(z)))
  ck   <- tapply(cohort,   g, function(z) z[1])
  ek   <- tapply(et,       g, function(z) z[1])
  data.frame(
    cohort = as.integer(ck),
    .et    = as.integer(ek),
    est    = as.numeric(num / den),
    n      = as.integer(cnt),
    row.names = NULL
  )
}

# Per-replicate cell means -> SE and percentile CI across replicates.
agg_fect_replicates <- function(rep_po, conf.level) {
  g <- interaction(rep_po$cohort, rep_po$.et, rep_po$replicate,
                   drop = TRUE, lex.order = TRUE)
  num <- tapply(rep_po$eff * rep_po$.w, g, sum, na.rm = TRUE)
  den <- tapply(rep_po$.w,              g, sum, na.rm = TRUE)
  est <- num / den
  ck  <- tapply(rep_po$cohort, g, function(z) z[1])
  ek  <- tapply(rep_po$.et,    g, function(z) z[1])

  cell <- interaction(ck, ek, drop = TRUE, lex.order = TRUE)
  a    <- (1 - conf.level) / 2
  spl  <- split(as.numeric(est), cell)
  se   <- vapply(spl, stats::sd, numeric(1), na.rm = TRUE)
  lo   <- vapply(spl, stats::quantile, numeric(1), probs = a,     na.rm = TRUE, names = FALSE)
  hi   <- vapply(spl, stats::quantile, numeric(1), probs = 1 - a, na.rm = TRUE, names = FALSE)
  ckc  <- tapply(as.integer(ck), cell, function(z) z[1])
  ekc  <- tapply(as.integer(ek), cell, function(z) z[1])
  data.frame(
    cohort = as.integer(ckc),
    .et    = as.integer(ekc),
    se = as.numeric(se), lo = as.numeric(lo), hi = as.numeric(hi),
    row.names = NULL
  )
}
