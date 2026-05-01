#' @rdname as_nabs_event_study
#'
#' @details
#' ## fect method
#'
#' `fect::fect()` returns event-study coordinates in `$time` and `$att`,
#' with confidence-interval bounds in the two-column matrix `$att.bound`.
#' Standard errors are pulled from `$est.att[, "S.E."]` when available; if
#' the object was fit without `se = TRUE`, only the point estimates are
#' returned and SE / CI columns are filled with `NA`.
#'
#' The `method` label is auto-detected from `x$method`, the option that was
#' passed to `fect::fect()`:
#'
#' \itemize{
#'   \item `"fe"`  -> `"FE"`  (two-way fixed-effects imputation; Borusyak-style)
#'   \item `"ife"` -> `"IFE"` (interactive fixed effects; Bai 2009)
#'   \item `"mc"`  -> `"MC"`  (matrix completion; Athey et al. 2021)
#' }
#'
#' Pass an explicit `method` argument to override this auto-detected label.
#'
#' @export
as_nabs_event_study.fect <- function(x, method = NULL, outcome = NA_character_,
                                     conf.level = 0.95, ...) {
  t   <- x[["time"]]
  att <- x[["att"]]
  if (is.null(t) || is.null(att)) {
    cli::cli_abort(c(
      "Could not find {.code $time} / {.code $att} on this {.cls fect} object.",
      "i" = "fect >= 1.0 stores event-study coordinates in those slots."
    ))
  }

  bnd <- x[["att.bound"]]
  conf.low  <- if (!is.null(bnd)) bnd[, 1] else NA_real_
  conf.high <- if (!is.null(bnd)) bnd[, 2] else NA_real_

  # Try to recover SEs; fect places them in est.att with an "S.E." column.
  se <- NA_real_
  ea <- x[["est.att"]]
  if (!is.null(ea) && "S.E." %in% colnames(ea) && nrow(ea) == length(t)) {
    se <- ea[, "S.E."]
  }

  inferred <- fect_method_label(x[["method"]])

  new_event_study_tbl(
    time      = t,
    estimate  = att,
    std.error = se,
    conf.low  = conf.low,
    conf.high = conf.high,
    method    = method %||% inferred,
    outcome   = outcome,
    conf.level = conf.level
  )
}

# Map a fect $method string to a clean label.
fect_method_label <- function(m) {
  if (is.null(m) || length(m) == 0L) return("IFE")  # historical default
  switch(
    tolower(as.character(m)[1]),
    "fe"          = "FE",
    "ife"         = "IFE",
    "mc"          = "MC",
    "imputation"  = "Imputation",   # very old fect synonym for fe
    "polynomial"  = "Polynomial",
    "IFE"
  )
}
