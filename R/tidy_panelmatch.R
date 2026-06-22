#' @rdname as_nabs_event_study
#'
#' @details
#' ## PanelMatch method
#'
#' For `PanelMatch::PanelEstimate()` the post-treatment leads are stored as
#' `$estimate` / `$standard.error` (singular). The pre-treatment placebo
#' results from `PanelMatch::placebo_test()` use `$estimates` / `$standard.errors`
#' (plural). To produce a single event-study path, pass the placebo object
#' via `pre_obj`:
#'
#' \preformatted{
#'   pm <- PanelMatch::PanelMatch(...)
#'   pe <- PanelMatch::PanelEstimate(pm, panel.data = pd)
#'   pl <- PanelMatch::placebo_test(pm, panel.data = pd, plot = FALSE)
#'   tidy <- as_nabs_event_study(pe, pre_obj = pl)
#' }
#'
#' A `time = -1` reference point with `estimate = 0` is inserted so that
#' the event-study path is anchored at t = -1, matching common practice
#' and the `did` / `fixest::iplot` convention. Disable with
#' `add_reference = FALSE`.
#'
#' @param pre_obj A `placebo_test` result from `PanelMatch::placebo_test()`,
#'   used to fill in the pre-treatment portion of the path.
#' @param add_reference Logical; if `TRUE` (default) and `pre_obj` is given,
#'   adds a `(time = -1, estimate = 0)` row.
#'
#' @export
as_nabs_event_study.PanelEstimate <- function(x, method = NULL,
                                         outcome = NA_character_,
                                         conf.level = 0.95,
                                         pre_obj = NULL,
                                         add_reference = TRUE,
                                         ...) {
  # Post-treatment portion (always present).
  est_post <- x[["estimate"]]
  se_post  <- x[["standard.error"]]
  if (is.null(est_post) || is.null(se_post)) {
    cli::cli_abort(c(
      "Could not find {.code $estimate} / {.code $standard.error} on this \\
       {.cls PanelEstimate} object.",
      "i" = "PanelMatch >= 2.0 stores post-treatment results in those slots."
    ))
  }

  t_post <- parse_panelmatch_times(names(est_post))
  post_tbl <- new_event_study_tbl(
    time      = t_post,
    estimate  = unname(est_post),
    std.error = unname(se_post),
    method    = method %||% "PanelMatch",
    outcome   = outcome,
    conf.level = conf.level
  )

  if (is.null(pre_obj)) return(post_tbl)

  # Pre-treatment portion from placebo_test().
  est_pre <- pre_obj[["estimates"]]
  se_pre  <- pre_obj[["standard.errors"]]
  if (is.null(est_pre) || is.null(se_pre)) {
    cli::cli_abort(c(
      "{.arg pre_obj} should be a {.fun PanelMatch::placebo_test} return value \\
       with {.code $estimates} and {.code $standard.errors}.",
      "i" = "Note the plural -- PanelEstimate uses singular, placebo_test plural."
    ))
  }

  t_pre <- parse_panelmatch_times(names(est_pre))
  pre_tbl <- new_event_study_tbl(
    time      = t_pre,
    estimate  = unname(est_pre),
    std.error = unname(se_pre),
    method    = method %||% "PanelMatch",
    outcome   = outcome,
    conf.level = conf.level
  )

  out <- dplyr::bind_rows(pre_tbl, post_tbl)

  if (isTRUE(add_reference) && !any(out$time == -1L)) {
    ref <- new_event_study_tbl(
      time      = -1L,
      estimate  = 0,
      std.error = 0,
      conf.low  = 0,
      conf.high = 0,
      method    = method %||% "PanelMatch",
      outcome   = outcome,
      conf.level = conf.level
    )
    out <- dplyr::bind_rows(out, ref)
  }

  out <- out[order(out$time), , drop = FALSE]
  class(out) <- c("nabs_event_study_tbl",
                  setdiff(class(out), "nabs_event_study_tbl"))
  attr(out, "conf.level") <- conf.level
  out
}

# Names from PanelMatch are like "t+0", "t+1", "t-2", or sometimes "t0", "t-1"
# depending on version. Strip the leading "t" and any leading "+" sign.
parse_panelmatch_times <- function(nms) {
  if (is.null(nms) || length(nms) == 0L) {
    cli::cli_abort("PanelMatch estimate vector has no names; cannot recover time.")
  }
  cleaned <- sub("^t", "", nms)
  cleaned <- sub("^\\+", "", cleaned)
  out <- suppressWarnings(as.integer(cleaned))
  if (anyNA(out)) {
    cli::cli_abort(c(
      "Failed to parse relative-time labels from PanelMatch output.",
      "x" = "Names received: {.val {nms}}"
    ))
  }
  out
}
