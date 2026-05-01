#' @rdname as_nabs_event_study
#' @export
as_nabs_event_study.did_multiplegt_dyn <- function(x, method = NULL,
                                              outcome = NA_character_,
                                              conf.level = 0.95, ...) {
  # The DIDmultiplegtDYN object stores its event-study coordinates
  # inside the ggplot it builds: `x$plot$data` has columns
  # `Time`, `Estimate`, `LB.CI`, `UB.CI`, plus `SE` in some versions.
  # We deliberately do NOT depend on internal slot names like
  # `results$ATE` / `results$Effects` / `results$Placebos` since their
  # naming has shifted across DIDmultiplegtDYN releases (1.0.x -> 2.x).
  # The plot data is a stable contract because users see it; if a
  # future version moves it, we'll add a fallback path.

  pd <- x[["plot"]][["data"]]
  if (is.null(pd) || !all(c("Time", "Estimate") %in% names(pd))) {
    cli::cli_abort(c(
      "Could not locate event-study coordinates in this {.cls did_multiplegt_dyn} object.",
      "i" = "Was the object built with {.code graph_off = TRUE}? \\
             nonabsdid needs the embedded ggplot to extract estimates."
    ))
  }

  has_se <- "SE" %in% names(pd)
  se     <- if (has_se) pd[["SE"]] else NA_real_

  new_event_study_tbl(
    time      = pd$Time,
    estimate  = pd$Estimate,
    std.error = se,
    conf.low  = pd[["LB.CI"]],
    conf.high = pd[["UB.CI"]],
    method    = method %||% "DCDH",
    outcome   = outcome,
    conf.level = conf.level
  )
}
